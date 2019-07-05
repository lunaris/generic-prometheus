{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericPrometheus
  ( Labels
  , (:=)
  , Counter (..)
  , Gauge (..)
  , Histogram (..)
  , HistogramUpperBounds

  , MonadPrometheus (..)
  , PrometheusT (..)
  , WhenMetrics

  , addCounter
  , incCounter
  , sampleCounter
  , addAndSampleCounter
  , Prom.M.Ctr.CounterSample (..)
  , HasCounter (..)

  , addGauge
  , subGauge
  , incGauge
  , decGauge
  , setGauge
  , sampleGauge
  , modifyAndSampleGauge
  , Prom.M.Gg.GaugeSample (..)
  , HasGauge (..)

  , observeHistogram
  , sampleHistogram
  , observeAndSampleHistogram
  , Prom.M.Histo.HistogramSample (..)
  , HasHistogram (..)

  , forkMetricsServer
  , mkMetrics
  , Prom.R.RegistrySample

  , symbolName
  , labelsVal
  , KnownLabels (..)
  , KnownUpperBounds (..)
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Lens as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Functor (void)
import qualified Data.Generics.Product as G.P
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Text as Tx
import qualified GHC.Generics as G
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import qualified System.Metrics.Prometheus.Http.Scrape as Prom.HTTP
import qualified System.Metrics.Prometheus.MetricId as Prom.Id
import qualified System.Metrics.Prometheus.Metric.Counter as Prom.M.Ctr
import qualified System.Metrics.Prometheus.Metric.Gauge as Prom.M.Gg
import qualified System.Metrics.Prometheus.Metric.Histogram as Prom.M.Histo
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as Prom.RT
import qualified System.Metrics.Prometheus.Registry as Prom.R

-- |A list of @'( key, value )@ pairs, to be used when defining metrics.
type Labels
  = [(Symbol, Symbol)]

-- |A synonym for constructing labels as @'( key, value )@ pairs.
type k := v
  = '(k, v)

-- |A Prometheus counter with the given name and set of labels.
--
--  @
--  data Metrics
--    = Metrics
--        { ...
--        , _mCounter :: Counter "example_counter" '[ "l1" := "v1" ]
--        }
--  @
newtype Counter (name :: Symbol) (labels :: Labels)
  = Counter Prom.M.Ctr.Counter

-- |A Prometheus gauge with the given name and set of labels.
--
--  @
--  data Metrics
--    = Metrics
--        { ...
--        , _mGauge :: Gauge "example_gauge" '[ "l1" := "v1" ]
--        }
--  @
newtype Gauge (name :: Symbol) (labels :: Labels)
  = Gauge Prom.M.Gg.Gauge

-- |A Prometheus histogram with the given name, label set and buckets, defined
--  by their successive upper bounds.
--
--  @
--  data Metrics
--    = Metrics
--        { ...
--        , _mHistogram :: Histogram "example_histogram" '[ "l1" := "v1" ] '[ 10, 20, 30 ]
--        }
--  @
newtype Histogram (name :: Symbol) (labels :: Labels) (upperBounds :: HistogramUpperBounds)
  = Histogram Prom.M.Histo.Histogram

-- |A list of upper bounds, to be used when defining @Histogram@ buckets.
type HistogramUpperBounds
  = [Nat]

--  This is not the ideal interface. Ideally we'd put methods like @incCounter@
--  et. al in a type class so that e.g. tests could perfectly mock Prometheus
--  interfaces etc. However, doing that and thus having methods with
--  additional ad-hoc constraints, such as:
--
--  @
--  class MonadPrometheus (metrics :: Type) m | m -> metrics where
--    incCounter :: HasCounter name metrics => ...
--  @
--
--  means that when we deploy @deriving via@, GHC generates code of the form:
--
--  @
--  coerce ... (forall name. HasCounter name ConcreteType => ...) ...
--  @
--
--  which is incompatible with @-Wsimplifiable-class-constraints@, since the
--  concrete type causes GHC to argue that the constraint:
--
--  @
--  (Generic ConcreteType, GHasCounter name (Rep ConcreteType))
--  @
--
--  should be used instead (since it can see this is how the @HasCounter@
--  constraint will be solved). I've not yet discovered a way to make GHC
--  generate code that obeys its own rule in this instance and so have opted
--  for this less-than-perfect (but functional, as best as I can tell)
--  interface instead.

class MonadPrometheus (metrics :: Type) m | m -> metrics where
  getMetrics :: m metrics

newtype PrometheusT metrics m a
  = PrometheusT (m a)
  deriving (Applicative, Functor, Monad,
            MonadIO, MonadReader r)

instance ( MonadReader r m
         , G.P.HasType metrics r
         )
      => MonadPrometheus metrics (PrometheusT metrics m) where
  getMetrics =
    Lens.view (G.P.typed @metrics)

type WhenMetrics has name metrics m
  = ( MonadIO m
    , MonadPrometheus metrics m
    , has name metrics
    )

withMetric
  :: forall metrics a b m
   . ( MonadIO m
     , MonadPrometheus metrics m
     )
  => (metrics -> a)
  -> (a -> IO b)
  -> m b
withMetric get update = do
  ms <- getMetrics @metrics
  let c = get ms
  liftIO $ update c

--------------------------------------------------------------------------------
--  Counters
--------------------------------------------------------------------------------

-- |Adds a given amount to the specified counter.
--
--  @
--  addCounter @"example_counter" 5
--  @
addCounter
  :: forall name metrics m
   . WhenMetrics HasCounter name metrics m
  => Int
  -> m ()
addCounter =
  withMetric @metrics (getCounter @name) . Prom.M.Ctr.add

-- |Increments the specified counter.
--
--  @
--  incCounter @"example_counter"
--  @
incCounter
  :: forall name metrics m
   . WhenMetrics HasCounter name metrics m
  => m ()
incCounter =
  withMetric @metrics (getCounter @name) Prom.M.Ctr.inc

-- |Samples the specified counter, returning the current count.
--
--  @
--  sampleCounter @"example_counter"
--  @
sampleCounter
  :: forall name metrics m
   . WhenMetrics HasCounter name metrics m
  => m Prom.M.Ctr.CounterSample
sampleCounter =
  withMetric @metrics (getCounter @name) Prom.M.Ctr.sample

-- |Adds the given amount to the specified counter and thereafter samples it,
--  returning the new count.
--
--  @
--  addAndSampleCounter @"example_counter" 5
--  @
addAndSampleCounter
  :: forall name metrics m
   . WhenMetrics HasCounter name metrics m
  => Int
  -> m Prom.M.Ctr.CounterSample
addAndSampleCounter =
  withMetric @metrics (getCounter @name) . Prom.M.Ctr.addAndSample

-- |The class of types which have a @Counter@ metric with the supplied @name@.
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

-- |Adds the given amount to the specified gauge.
--
--  @
--  addGauge @"example_gauge" 5.0
--  @
addGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => Double
  -> m ()
addGauge =
  withMetric @metrics (getGauge @name) . Prom.M.Gg.add

-- |Subtracts the given amount from the specified gauge.
--
--  @
--  subGauge @"example_gauge" 5.0
--  @
subGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => Double
  -> m ()
subGauge =
  withMetric @metrics (getGauge @name) . Prom.M.Gg.sub

-- |Increments the specified gauge.
--
--  @
--  incGauge @"example_gauge"
--  @
incGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => m ()
incGauge =
  withMetric @metrics (getGauge @name) Prom.M.Gg.inc

-- |Decrements the specified gauge.
--
--  @
--  incGauge @"example_gauge"
--  @
decGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => m ()
decGauge =
  withMetric @metrics (getGauge @name) Prom.M.Gg.dec

-- |Sets the specified gauge to the given value.
--
--  @
--  setGauge @"example_gauge" 5.0
--  @
setGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => Double
  -> m ()
setGauge =
  withMetric @metrics (getGauge @name) . Prom.M.Gg.set

-- |Samples the specified gauge, returning the current value.
--
--  @
--  sampleGauge @"example_gauge"
--  @
sampleGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => m Prom.M.Gg.GaugeSample
sampleGauge =
  withMetric @metrics (getGauge @name) Prom.M.Gg.sample

-- |Applies the given function to the specified gauge's current value before
--  sampling the gauge and returning the new value.
--
--  @
--  modifyAndSampleGauge @"example_gauge" (2 *)
--  @
modifyAndSampleGauge
  :: forall name metrics m
   . WhenMetrics HasGauge name metrics m
  => (Double -> Double)
  -> m Prom.M.Gg.GaugeSample
modifyAndSampleGauge =
  withMetric @metrics (getGauge @name) . Prom.M.Gg.modifyAndSample

-- |The class of types which have a @Gauge@ metric with the supplied @name@.
class HasGauge (name :: Symbol) (metrics :: Type) where
  getGauge :: metrics -> Prom.M.Gg.Gauge

instance {-# OVERLAPPABLE #-}
         ( G.Generic metrics
         , GHasGauge name (G.Rep metrics)
         )
      => HasGauge name metrics where
  getGauge =
    ggetGauge @name . G.from

class GHasGauge (name :: Symbol) (rep :: Type -> Type) where
  ggetGauge :: rep x -> Prom.M.Gg.Gauge

instance GHasGauge name (G.S1 m (G.Rec0 (Gauge name labels))) where
  ggetGauge (G.M1 (G.K1 (Gauge ctr))) =
    ctr

instance GHasGauge name rep => GHasGauge name (G.D1 m rep) where
  ggetGauge =
    ggetGauge @name . G.unM1

instance GHasGauge name rep => GHasGauge name (G.C1 m rep) where
  ggetGauge =
    ggetGauge @name . G.unM1

instance ( HasMetric (Gauge name labels) (l G.:*: r) ~ lr
         , GProductHasGauge lr name (l G.:*: r)
         )
      => GHasGauge name (l G.:*: r) where
  ggetGauge =
    ggetProductGauge @(HasMetric (Gauge name labels) (l G.:*: r)) @name

class GProductHasGauge (lr :: Bool) (name :: Symbol) (rep :: Type -> Type) where
  ggetProductGauge :: rep x -> Prom.M.Gg.Gauge

instance GHasGauge name l
      => GProductHasGauge 'True name (l G.:*: r) where
  ggetProductGauge (x G.:*: _) =
    ggetGauge @name x

instance GHasGauge name r
      => GProductHasGauge 'False name (l G.:*: r) where
  ggetProductGauge (_ G.:*: y) =
    ggetGauge @name y

--------------------------------------------------------------------------------
--  Histograms
--------------------------------------------------------------------------------

-- |Records an observation of the given value in the specified histogram.
--
--  @
--  observeHistogram @"example_histogram" 5.0
--  @
observeHistogram
  :: forall name metrics m
   . WhenMetrics HasHistogram name metrics m
  => Double
  -> m ()
observeHistogram =
  withMetric @metrics (getHistogram @name) . Prom.M.Histo.observe

-- |Samples the specified histogram, returning information about the current
--  distribution of observations.
--
--  @
--  sampleHistogram @"example_histogram"
--  @
sampleHistogram
  :: forall name metrics m
   . WhenMetrics HasHistogram name metrics m
  => m Prom.M.Histo.HistogramSample
sampleHistogram =
  withMetric @metrics (getHistogram @name) Prom.M.Histo.sample

-- |Records an observation of the given value in the specified histogram before
--  sampling it and returning information about the new distribution.
--
--  @
--  observeAndSampleHistogram @"example_histogram" 5.0
--  @
observeAndSampleHistogram
  :: forall name metrics m
   . WhenMetrics HasHistogram name metrics m
  => Double
  -> m Prom.M.Histo.HistogramSample
observeAndSampleHistogram =
  withMetric @metrics (getHistogram @name) . Prom.M.Histo.observeAndSample

-- |The class of types which have a @Histogram@ metric with the supplied @name@.
class HasHistogram (name :: Symbol) (metrics :: Type) where
  getHistogram :: metrics -> Prom.M.Histo.Histogram

instance {-# OVERLAPPABLE #-}
         ( G.Generic metrics
         , GHasHistogram name (G.Rep metrics)
         )
      => HasHistogram name metrics where
  getHistogram =
    ggetHistogram @name . G.from

class GHasHistogram (name :: Symbol) (rep :: Type -> Type) where
  ggetHistogram :: rep x -> Prom.M.Histo.Histogram

instance GHasHistogram name (G.S1 m (G.Rec0 (Histogram name labels upperBounds))) where
  ggetHistogram (G.M1 (G.K1 (Histogram ctr))) =
    ctr

instance GHasHistogram name rep => GHasHistogram name (G.D1 m rep) where
  ggetHistogram =
    ggetHistogram @name . G.unM1

instance GHasHistogram name rep => GHasHistogram name (G.C1 m rep) where
  ggetHistogram =
    ggetHistogram @name . G.unM1

instance ( HasMetric (Histogram name labels upperBounds) (l G.:*: r) ~ lr
         , GProductHasHistogram lr name (l G.:*: r)
         )
      => GHasHistogram name (l G.:*: r) where
  ggetHistogram =
    ggetProductHistogram @(HasMetric (Histogram name labels upperBounds) (l G.:*: r)) @name

class GProductHasHistogram (lr :: Bool) (name :: Symbol) (rep :: Type -> Type) where
  ggetProductHistogram :: rep x -> Prom.M.Histo.Histogram

instance GHasHistogram name l
      => GProductHasHistogram 'True name (l G.:*: r) where
  ggetProductHistogram (x G.:*: _) =
    ggetHistogram @name x

instance GHasHistogram name r
      => GProductHasHistogram 'False name (l G.:*: r) where
  ggetProductHistogram (_ G.:*: y) =
    ggetHistogram @name y

--------------------------------------------------------------------------------
--  Construction and registration
--------------------------------------------------------------------------------

-- |Forks a thread to serve metrics from the given @RegistrySample@ in a format
--  compatible with Prometheus' scraper. Metrics will be served at @/metrics@ on
--  the supplied port.
forkMetricsServer
  :: MonadIO m
  => Int
  -> IO Prom.R.RegistrySample
  -> m ()
forkMetricsServer port sample = liftIO $ void $
  Async.async $ Prom.HTTP.serveHttpTextMetrics port ["metrics"] sample

-- |Generically constructs a record of metrics registered with a registry. Both
--  the record of metrics and a @RegistrySample@ suitable for serving (e.g. with
--  @forkMetricsServer@ are returned.
mkMetrics
  :: forall a m
   . ( G.Generic a
     , GConstructsMetrics (G.Rep a)
     , MonadIO m
     )
  => m (a, IO Prom.R.RegistrySample)
mkMetrics = Prom.RT.runRegistryT $ do
  ms <- G.to <$> gmkMetrics @(G.Rep a)
  sample <- Prom.RT.sample
  pure (ms, sample)

class GConstructsMetrics (rep :: Type -> Type) where
  gmkMetrics :: MonadIO m => Prom.RT.RegistryT m (rep x)

instance ( KnownSymbol name
         , KnownLabels labels
         )
      => GConstructsMetrics (G.S1 m (G.Rec0 (Counter name labels))) where
  gmkMetrics =
    coerce <$>
      Prom.RT.registerCounter (symbolName @name) (labelsVal @labels)

instance ( KnownSymbol name
         , KnownLabels labels
         )
      => GConstructsMetrics (G.S1 m (G.Rec0 (Gauge name labels))) where
  gmkMetrics =
    coerce <$>
      Prom.RT.registerGauge (symbolName @name) (labelsVal @labels)

instance ( KnownSymbol name
         , KnownLabels labels
         , KnownUpperBounds upperBounds
         )
      => GConstructsMetrics
          (G.S1 m (G.Rec0 (Histogram name labels upperBounds))) where
  gmkMetrics =
    coerce <$>
      Prom.RT.registerHistogram (symbolName @name) (labelsVal @labels)
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
