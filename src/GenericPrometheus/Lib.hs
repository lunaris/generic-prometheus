{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericPrometheus.Lib where

import qualified Control.Lens as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import qualified Data.Generics.Product as G.P
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as Tx
import qualified GHC.Generics as G
import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import qualified System.Metrics.Prometheus.MetricId as Prom.Id
import qualified System.Metrics.Prometheus.Metric.Counter as Prom.M.Ctr
import qualified System.Metrics.Prometheus.Metric.Gauge as Prom.M.Gg
import qualified System.Metrics.Prometheus.Metric.Histogram as Prom.M.Histo
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as Prom.R

type Labels
  = [(Symbol, Symbol)]

type k := v
  = '(k, v)

newtype Counter (name :: Symbol) (labels :: Labels)
  = Counter Prom.M.Ctr.Counter

newtype Gauge (name :: Symbol) (labels :: Labels)
  = Gauge Prom.M.Gg.Gauge

newtype Histogram (name :: Symbol) (labels :: Labels) (upperBounds :: HistogramUpperBounds)
  = Histogram Prom.M.Histo.Histogram

type HistogramUpperBounds
  = [Nat]

--  This is not the ideal interface. Ideally we'd put methods like @incCounter@
--  et. al in here so that e.g. tests could perfectly mock Prometheus
--  interfaces etc. However, doing that (and thus having methods with
--  additional ad-hoc constraints, such as:
--
--  @
--    incCounter :: HasCounter name metrics => ...
--  @
--
--  means that when we deploy @deriving via@, GHC generates code of the form
--
--  @
--    coerce ... (forall name. HasCounter name ConcreteType => ...) ...
--  @
--
--  which is incompatible with @-Wsimplifiable-class-constraints@, since the
--  concrete type causes GHC to argue that the constraint
--
--  @
--    (Generic ConcreteType, GHasCounter name (Rep ConcreteType))
--  @
--
--  should be used instead (since it can see this is how the @HasCounter@
--  constraint will be solved). I've not yet discovered a way to make GHC
--  generate code that obeys its own rule in this instance and so have opted
--  for this less-than-perfect (but functional, as best as I can tell)
--  interface instead.

class MonadPrometheus (metrics :: Type) m | m -> metrics where
  getMetrics :: m metrics

incCounter
  :: forall name metrics m
   . ( MonadIO m
     , MonadPrometheus metrics m
     , HasCounter name metrics
     )
  => m ()
incCounter = do
  ms <- getMetrics
  let c = getCounter @name ms
  liftIO $ Prom.M.Ctr.inc c

newtype PrometheusT metrics m a
  = PrometheusT (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader r)

instance (MonadIO m, MonadReader r m, G.P.HasType metrics r)
      =>  MonadPrometheus metrics (PrometheusT metrics m) where
  getMetrics =
    Lens.view (G.P.typed @metrics)

--------------------------------------------------------------------------------
--  Counters
--------------------------------------------------------------------------------

class HasCounter (name :: Symbol) (metrics :: Type) where
  getCounter :: metrics -> Prom.M.Ctr.Counter

instance {-# OVERLAPPABLE #-}
         ( G.Generic metrics
         , GHasCounter name (G.Rep metrics)
         )
      => HasCounter name metrics where
  getCounter =
    ggetCounter @name . G.from

class GHasCounter (name :: Symbol) (rep :: Type -> Type) where
  ggetCounter :: rep x -> Prom.M.Ctr.Counter

instance GHasCounter name (G.S1 m (G.Rec0 (Counter name labels))) where
  ggetCounter (G.M1 (G.K1 (Counter ctr))) =
    ctr

instance GHasCounter name rep => GHasCounter name (G.D1 m rep) where
  ggetCounter =
    ggetCounter @name . G.unM1

instance GHasCounter name rep => GHasCounter name (G.C1 m rep) where
  ggetCounter =
    ggetCounter @name . G.unM1

instance ( HasMetric (Counter name labels) (l G.:*: r) ~ lr
         , GProductHasCounter lr name (l G.:*: r)
         )
      => GHasCounter name (l G.:*: r) where
  ggetCounter =
    ggetProductCounter @(HasMetric (Counter name labels) (l G.:*: r)) @name

class GProductHasCounter (lr :: Bool) (name :: Symbol) (rep :: Type -> Type) where
  ggetProductCounter :: rep x -> Prom.M.Ctr.Counter

instance GHasCounter name l
      => GProductHasCounter 'True name (l G.:*: r) where
  ggetProductCounter (x G.:*: _) =
    ggetCounter @name x

instance GHasCounter name r
      => GProductHasCounter 'False name (l G.:*: r) where
  ggetProductCounter (_ G.:*: y) =
    ggetCounter @name y

--------------------------------------------------------------------------------
--  Gauges
--------------------------------------------------------------------------------

class HasGauge (name :: Symbol) (metrics :: Type) where
  getGauge :: metrics -> Prom.M.Gg.Gauge

--------------------------------------------------------------------------------
--  Histograms
--------------------------------------------------------------------------------

class HasHistogram (name :: Symbol) (metrics :: Type) where
  getHistogram :: metrics -> Prom.M.Histo.Histogram

--------------------------------------------------------------------------------
--  Construction and registration
--------------------------------------------------------------------------------

mkMetrics
  :: forall a m
   . ( G.Generic a
     , GConstructsMetrics (G.Rep a)
     , MonadIO m
     )
  => Prom.R.RegistryT m a
mkMetrics =
  G.to <$> gmkMetrics @(G.Rep a)

class GConstructsMetrics (rep :: Type -> Type) where
  gmkMetrics :: MonadIO m => Prom.R.RegistryT m (rep x)

instance ( KnownSymbol name
         , KnownLabels labels
         )
      => GConstructsMetrics (G.S1 m (G.Rec0 (Counter name labels))) where
  gmkMetrics =
    coerce <$>
      Prom.R.registerCounter (symbolName @name) (labelsVal @labels)

instance ( KnownSymbol name
         , KnownLabels labels
         )
      => GConstructsMetrics (G.S1 m (G.Rec0 (Gauge name labels))) where
  gmkMetrics =
    coerce <$>
      Prom.R.registerGauge (symbolName @name) (labelsVal @labels)

instance ( KnownSymbol name
         , KnownLabels labels
         , KnownUpperBounds upperBounds
         )
      => GConstructsMetrics
          (G.S1 m (G.Rec0 (Histogram name labels upperBounds))) where
  gmkMetrics =
    coerce <$>
      Prom.R.registerHistogram (symbolName @name) (labelsVal @labels)
        (upperBoundsVal @upperBounds)

instance GConstructsMetrics rep => GConstructsMetrics (G.D1 m rep) where
  gmkMetrics =
    G.M1 <$> gmkMetrics @rep

instance GConstructsMetrics rep => GConstructsMetrics (G.C1 m rep) where
  gmkMetrics =
    G.M1 <$> gmkMetrics @rep

instance (GConstructsMetrics l, GConstructsMetrics r)
      =>  GConstructsMetrics (l G.:*: r) where
  gmkMetrics =
    (G.:*:) <$> gmkMetrics @l <*> gmkMetrics @r

--------------------------------------------------------------------------------
--  Utilities
--------------------------------------------------------------------------------

symbolName :: forall name. KnownSymbol name => Prom.Id.Name
symbolName =
  Prom.Id.Name (symbolText @name)

symbolText :: forall name. KnownSymbol name => Tx.Text
symbolText =
  Tx.pack (symbolVal (Proxy @name))

natNum :: forall n a. (KnownNat n, Num a) => a
natNum =
  fromInteger (natVal (Proxy @n))

labelsVal :: forall labels. KnownLabels labels => Prom.Id.Labels
labelsVal =
  coerce (labelsMap @labels)

class KnownLabels (labels :: Labels) where
  labelsMap :: M.Map Tx.Text Tx.Text

instance KnownLabels '[] where
  labelsMap =
    M.empty

instance ( KnownLabels labels
         , KnownSymbol k
         , KnownSymbol v
         )
      => KnownLabels ( '( k, v ) ': labels ) where
  labelsMap =
    M.insert (symbolText @k) (symbolText @v) (labelsMap @labels)

class KnownUpperBounds (upperBounds :: HistogramUpperBounds) where
  upperBoundsVal :: [Double]

instance KnownUpperBounds '[] where
  upperBoundsVal =
    []

instance ( KnownUpperBounds upperBounds
         , KnownNat ub
         )
      => KnownUpperBounds (ub ': upperBounds) where
  upperBoundsVal =
    natNum @ub : upperBoundsVal @upperBounds

type family HasMetric (metric :: Type) (rep :: Type -> Type) :: Bool where
  HasMetric (Counter name _) (G.S1 _ (G.Rec0 (Counter name _))) =
    'True
  HasMetric (Gauge name _) (G.S1 _ (G.Rec0 (Gauge name _))) =
    'True
  HasMetric (Histogram name _ _) (G.S1 _ (G.Rec0 (Histogram name _ _))) =
    'True
  HasMetric metric (l G.:+: r) =
    HasMetric metric l `Or` HasMetric metric r
  HasMetric metric (l G.:*: r) =
    HasMetric metric l `Or` HasMetric metric r
  HasMetric metric (G.S1 _ _) =
    'False
  HasMetric metric (G.C1 _ rep) =
    HasMetric metric rep
  HasMetric metric (G.D1 _ rep) =
    HasMetric metric rep
  HasMetric metric (G.K1 _ _) =
    'False
  HasMetric metric G.U1 =
    'False
  HasMetric metric G.V1 =
    'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True b = 'True
  Or a     b = b
