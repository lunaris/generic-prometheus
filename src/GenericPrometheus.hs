{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wnot #-}

module GenericPrometheus
  ( module GenericPrometheus
  , Prom.Wai.metricsApp
  )

  {-( Labels
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
  , Prom.GaugeSample (..)
  , HasGauge (..)

  , observeHistogram
  , sampleHistogram
  , observeAndSampleHistogram
  , Prom.HistogramSample (..)
  , HasHistogram (..)

  , forkMetricsServer
  , mkMetrics
  , Prom.R.RegistrySample

  , symbolName
  , labelsVal
  , KnownLabels (..)
  , KnownUpperBounds (..)
  )-} where

import qualified Control.Concurrent.Async as Async
import qualified Control.Lens as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Functor (void)
import qualified Data.Generics.Product as G.P
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.String (IsString (..))
import Data.Proxy (Proxy (..))
import qualified Data.Text as Tx
import qualified GHC.Generics as G
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Prometheus as Prom.Wai
import qualified Prometheus as Prom
import Text.Read (readMaybe)

-- |A list of @'( key, value )@ pairs, to be used when defining metrics.
type Labels
  = [(Symbol, Symbol)]

-- |A synonym for constructing labels as @'( key, value )@ pairs.
type k := v
  = '(k, v)

newtype Vector (labels :: [Symbol]) (metric :: Type)
  = Vector (Prom.Vector (LabelList labels) metric)

withLabel
  :: forall name labels metric promMetric metrics m
   . ( MonadIO m
     , MonadPrometheus metrics m
     , G.Generic metrics
     , GHasMetric (Vector labels) name promMetric (G.Rep metrics)
     , promMetric ~ Prom.Vector (LabelList labels) metric
     , KnownLabels labels
     )
  => LabelList labels
  -> (metric -> IO ())
  -> m ()
withLabel ls k = do
  ms <- getMetrics @metrics
  let m = ggetMetric @_ @(Vector labels) @name @promMetric (G.from ms)
  liftIO $ Prom.withLabel m ls k

class HasVector (name :: Symbol) (labels :: [Symbol]) (metric :: Type) (metrics :: Type) where
  getVector :: metrics -> Prom.Vector (LabelList labels) metric

instance ( G.Generic metrics
         , GHasMetric (Vector labels) name (Prom.Vector (LabelList labels) metric) (G.Rep metrics)
         )
      => HasVector name labels metric metrics where
  getVector =
    ggetMetric @_ @(Vector labels) @name . G.from

data LabelList :: [Symbol] -> Type where
  LNil  :: LabelList '[]
  LCons :: Tx.Text -> LabelList labels -> LabelList (label ': labels)

class KnownLabels (labels :: [Symbol]) where
  labelListVal
    :: LabelList labels
  labelListPairs
    :: LabelList labels
    -> LabelList labels
    -> Prom.LabelPairs

instance KnownLabels '[] where
  labelListVal =
    LNil
  labelListPairs _ _ =
    []

instance ( KnownSymbol label
         , KnownLabels labels
         )
      => KnownLabels (label ': labels) where
  labelListVal =
    LCons (symbolStr @label) (labelListVal @labels)
  labelListPairs (LCons l1 ls1) (LCons l2 ls2) =
    (l1, l2) : labelListPairs ls1 ls2

instance KnownLabels labels => Prom.Label (LabelList labels) where
  labelPairs =
    labelListPairs

deriving instance Eq (LabelList labels)
deriving instance Ord (LabelList labels)

-- |A Prometheus counter with the given name and set of labels.
--
--  @
--  data Metrics
--    = Metrics
--        { ...
--        , _mCounter :: Counter "example_counter" '[ "l1" := "v1" ]
--        }
--  @
newtype Counter (name :: Symbol) (description :: Symbol)
  = Counter Prom.Counter

-- |A Prometheus gauge with the given name and set of labels.
--
--  @
--  data Metrics
--    = Metrics
--        { ...
--        , _mGauge :: Gauge "example_gauge" '[ "l1" := "v1" ]
--        }
--  @
newtype Gauge (name :: Symbol) (description :: Symbol)
  = Gauge Prom.Gauge

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
newtype Histogram (name :: Symbol) (description :: Symbol) (buckets :: [Symbol])
  = Histogram Prom.Histogram

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

with
  :: forall metric name promMetric metrics m a
   . ( MonadIO m
     , MonadPrometheus metrics m
     , G.Generic metrics
     , GHasMetric metric name promMetric (G.Rep metrics)
     )
  => (promMetric -> IO a)
  -> m a
with k = do
  ms <- getMetrics @metrics
  let m = ggetMetric @_ @metric @name (G.from ms)
  liftIO $ k m

--------------------------------------------------------------------------------
--  Counters
--------------------------------------------------------------------------------

-- |Adds a given amount to the specified counter.
--
--  @
--  addCounter @"example_counter" 5
--  @
-- addCounter
--   :: forall name metrics m
--    . WhenMetrics HasCounter name metrics m
--   => Double
--   -> m Bool
-- addCounter =
--   withMetric @metrics (getCounter @name) . flip Prom.addCounter

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
  withMetric @metrics (getCounter @name) Prom.incCounter

incC :: Prom.Counter -> IO ()
incC =
  Prom.incCounter

-- |Samples the specified counter, returning the current count.
--
--  @
--  sampleCounter @"example_counter"
--  @
-- sampleCounter
--   :: forall name metrics m
--    . WhenMetrics HasCounter name metrics m
--   => m Double
-- sampleCounter =
--   withMetric @metrics (getCounter @name) Prom.getCounter

-- |The class of types which have a @Counter@ metric with the supplied @name@.
class HasCounter (name :: Symbol) (metrics :: Type) where
  getCounter :: metrics -> Prom.Counter

instance {-# OVERLAPPABLE #-}
         ( G.Generic metrics
         , GHasMetric Counter name Prom.Counter (G.Rep metrics)
         )
      => HasCounter name metrics where
  getCounter =
    ggetMetric @_ @Counter @name . G.from

--------------------------------------------------------------------------------
--  Gauges
--------------------------------------------------------------------------------

-- |Adds the given amount to the specified gauge.
--
--  @
--  addGauge @"example_gauge" 5.0
--  @
-- addGauge
--   :: forall name metrics m
--    . WhenMetrics HasGauge name metrics m
--   => Double
--   -> m ()
-- addGauge =
--   withMetric @metrics (getGauge @name) . flip Prom.addGauge

-- |Subtracts the given amount from the specified gauge.
--
--  @
--  subGauge @"example_gauge" 5.0
--  @
-- subGauge
--   :: forall name metrics m
--    . WhenMetrics HasGauge name metrics m
--   => Double
--   -> m ()
-- subGauge =
--   withMetric @metrics (getGauge @name) . flip Prom.subGauge

-- |Increments the specified gauge.
--
--  @
--  incGauge @"example_gauge"
--  @
-- incGauge
--   :: forall name metrics m
--    . WhenMetrics HasGauge name metrics m
--   => m ()
-- incGauge =
--   withMetric @metrics (getGauge @name) Prom.incGauge

-- |Decrements the specified gauge.
--
--  @
--  incGauge @"example_gauge"
--  @
-- decGauge
--   :: forall name metrics m
--    . WhenMetrics HasGauge name metrics m
--   => m ()
-- decGauge =
--   withMetric @metrics (getGauge @name) Prom.decGauge

-- |Sets the specified gauge to the given value.
--
--  @
--  setGauge @"example_gauge" 5.0
--  @
-- setGauge
--   :: forall name metrics m
--    . WhenMetrics HasGauge name metrics m
--   => Double
--   -> m ()
-- setGauge =
--   withMetric @metrics (getGauge @name) . flip Prom.setGauge

-- |Samples the specified gauge, returning the current value.
--
--  @
--  sampleGauge @"example_gauge"
--  @
-- sampleGauge
--   :: forall name metrics m
--    . WhenMetrics HasGauge name metrics m
--   => m Double
-- sampleGauge =
--   withMetric @metrics (getGauge @name) Prom.getGauge

-- |The class of types which have a @Gauge@ metric with the supplied @name@.
class HasGauge (name :: Symbol) (metrics :: Type) where
  getGauge :: metrics -> Prom.Gauge

instance {-# OVERLAPPABLE #-}
         ( G.Generic metrics
         , GHasMetric Gauge name Prom.Gauge (G.Rep metrics)
         )
      => HasGauge name metrics where
  getGauge =
    ggetMetric @_ @Gauge @name . G.from

--------------------------------------------------------------------------------
--  Histograms
--------------------------------------------------------------------------------

-- |Records an observation of the given value in the specified histogram.
--
--  @
--  observeHistogram @"example_histogram" 5.0
--  @
-- observeHistogram
--   :: forall name metrics m
--    . WhenMetrics HasHistogram name metrics m
--   => Double
--   -> m ()
-- observeHistogram =
--   withMetric @metrics (getHistogram @name) . flip Prom.observe

-- |Samples the specified histogram, returning information about the current
--  distribution of observations.
--
--  @
--  sampleHistogram @"example_histogram"
--  @
-- sampleHistogram
--   :: forall name metrics m
--    . WhenMetrics HasHistogram name metrics m
--   => m (M.Map Double Int)
-- sampleHistogram =
--   withMetric @metrics (getHistogram @name) Prom.getHistogram

-- |The class of types which have a @Histogram@ metric with the supplied @name@.
class HasHistogram (name :: Symbol) (metrics :: Type) where
  getHistogram :: metrics -> Prom.Histogram

instance {-# OVERLAPPABLE #-}
         ( G.Generic metrics
         , GHasMetric Histogram name Prom.Histogram (G.Rep metrics)
         )
      => HasHistogram name metrics where
  getHistogram =
    ggetMetric @_ @Histogram @name . G.from

--------------------------------------------------------------------------------
--  Construction and registration
--------------------------------------------------------------------------------

-- |Forks a thread to serve metrics from the global metrics registry in a format
--  compatible with Prometheus' scraper. Metrics will be served at any endpoint
--  on the supplied port.
forkMetricsServer
  :: MonadIO m
  => Int
  -> m ()
forkMetricsServer port = liftIO $ void $
  Async.async $ Warp.run port Prom.Wai.metricsApp

-- |Generically constructs a record of metrics registered with a registry. Both
--  the record of metrics and a @RegistrySample@ suitable for serving (e.g. with
--  @forkMetricsServer@ are returned.
registerMetrics
  :: forall a m
   . ( G.Generic a
     , GRegistersMetrics (G.Rep a)
     , MonadIO m
     )
  => m a
registerMetrics = do
  G.to <$> gregisterMetrics @(G.Rep a)

class GRegistersMetrics (rep :: Type -> Type) where
  gregisterMetrics :: MonadIO m => m (rep x)

instance ( KnownLabels labels
         , KnownSymbol name
         , KnownSymbol description
         )
      => GRegistersMetrics (G.Rec0 (Vector labels (Counter name description))) where
  gregisterMetrics =
    fmap coerce $ Prom.register $ Prom.vector (labelListVal @labels) $
      Prom.counter $ Prom.Info (symbolStr @name) (symbolStr @description)

instance ( KnownSymbol name
         , KnownSymbol description
         )
      => GRegistersMetrics (G.Rec0 (Counter name description)) where
  gregisterMetrics =
    fmap coerce $ Prom.register $ Prom.counter $
      Prom.Info (symbolStr @name) (symbolStr @description)

instance ( KnownSymbol name
         , KnownSymbol description
         )
      => GRegistersMetrics (G.Rec0 (Gauge name description)) where
  gregisterMetrics =
    fmap coerce $ Prom.register $ Prom.gauge $
      Prom.Info (symbolStr @name) (symbolStr @description)

instance ( KnownSymbol name
         , KnownSymbol description
         , KnownBuckets buckets
         )
      => GRegistersMetrics (G.Rec0 (Histogram name description buckets)) where
  gregisterMetrics =
    case bucketsVal @buckets of
      Nothing ->
        --  Ideally we'd have type-level doubles/decimals, but in the absence
        --  of those we'll try to parse @Symbol@s and use a runtime error to
        --  notify the user if something went wrong.
        error
          $  "Buckets supplied for histogram '"
          <> symbolStr @name
          <> "' are not all valid Doubles"
      Just bs ->
        fmap coerce $ Prom.register $ Prom.histogram
          (Prom.Info (symbolStr @name) (symbolStr @description)) bs

instance GRegistersMetrics rep => GRegistersMetrics (G.S1 m rep) where
  gregisterMetrics =
    G.M1 <$> gregisterMetrics @rep

instance GRegistersMetrics rep => GRegistersMetrics (G.C1 m rep) where
  gregisterMetrics =
    G.M1 <$> gregisterMetrics @rep

instance GRegistersMetrics rep => GRegistersMetrics (G.D1 m rep) where
  gregisterMetrics =
    G.M1 <$> gregisterMetrics @rep

instance (GRegistersMetrics l, GRegistersMetrics r)
      =>  GRegistersMetrics (l G.:*: r) where
  gregisterMetrics =
    (G.:*:) <$> gregisterMetrics @l <*> gregisterMetrics @r

--------------------------------------------------------------------------------
--  Utilities
--------------------------------------------------------------------------------

symbolStr :: forall name a. (KnownSymbol name, IsString a) => a
symbolStr =
  fromString (symbolVal (Proxy @name))

natNum :: forall n a. (KnownNat n, Num a) => a
natNum =
  fromInteger (natVal (Proxy @n))

--labelsVal :: forall labels. KnownLabels labels => Prom.Id.Labels
--labelsVal =
--  coerce (labelsMap @labels)
--
--class KnownLabels (labels :: Labels) where
--  labelsMap :: M.Map Tx.Text Tx.Text
--
--instance KnownLabels '[] where
--  labelsMap =
--    M.empty
--
--instance ( KnownLabels labels
--         , KnownSymbol k
--         , KnownSymbol v
--         )
--      => KnownLabels ( '( k, v ) ': labels ) where
--  labelsMap =
--    M.insert (symbolStr @k) (symbolStr @v) (labelsMap @labels)

class KnownBuckets (buckets :: [Symbol]) where
  bucketsVal :: Maybe [Double]

instance KnownBuckets '[] where
  bucketsVal =
    Just []

instance ( KnownBuckets buckets
         , KnownSymbol b
         )
      => KnownBuckets (b ': buckets) where
  bucketsVal =
    (:) <$> readMaybe (symbolStr @b) <*> bucketsVal @buckets

class GHasMetric (metric :: k) (name :: Symbol) (promMetric :: Type)
                 (rep :: Type -> Type) where
  ggetMetric :: rep x -> promMetric

instance GHasMetric Counter name Prom.Counter
                    (G.Rec0 (Counter name description)) where
  ggetMetric (G.K1 (Counter c)) =
    c

instance GHasMetric Gauge name Prom.Gauge
                    (G.Rec0 (Gauge name description)) where
  ggetMetric (G.K1 (Gauge g)) =
    g

instance GHasMetric Histogram name Prom.Histogram
                    (G.Rec0 (Histogram name description buckets)) where
  ggetMetric (G.K1 (Histogram h)) =
    h

instance GHasMetric metric name promMetric rep
      => GHasMetric metric name promMetric (G.S1 m rep) where
  ggetMetric =
    ggetMetric @_ @metric @name . G.unM1

instance GHasMetric metric name promMetric rep
      => GHasMetric metric name promMetric (G.C1 m rep) where
  ggetMetric =
    ggetMetric @_ @metric @name . G.unM1

instance GHasMetric metric name promMetric rep
      => GHasMetric metric name promMetric (G.D1 m rep) where
  ggetMetric =
    ggetMetric @_ @metric @name . G.unM1

instance ( FindMetric metric name l ~ lr
         , GProductHasMetric lr metric name promMetric (l G.:*: r)
         )
      => GHasMetric metric name promMetric (l G.:*: r) where
  ggetMetric =
    ggetProductMetric @_ @(FindMetric metric name l) @metric @name

class GProductHasMetric (lr :: Maybe Type)
                        (metric :: k) (name :: Symbol) (promMetric :: Type)
                        (rep :: Type -> Type) where
  ggetProductMetric :: rep x -> promMetric

instance GHasMetric metric name promMetric l
      => GProductHasMetric ('Just z) metric name promMetric (l G.:*: r) where
  ggetProductMetric (x G.:*: _) =
    ggetMetric @_ @metric @name x

instance GHasMetric metric name promMetric r
      => GProductHasMetric 'Nothing metric name promMetric (l G.:*: r) where
  ggetProductMetric (_ G.:*: y) =
    ggetMetric @_ @metric @name y

type family FindMetric (metric :: k) (name :: Symbol) (rep :: Type -> Type) :: Maybe Type where
  FindMetric (Vector labels metric) name
    (G.Rec0 (Vector labels (Counter name _))) =
      'Just (Prom.Vector (LabelList labels) Prom.Counter)
  FindMetric (Vector labels metric) name
    (G.Rec0 (Vector labels (Gauge name _))) =
      'Just (Prom.Vector (LabelList labels) Prom.Gauge)
  FindMetric (Vector labels metric) name
    (G.Rec0 (Vector labels (Histogram name _ _))) =
      'Just (Prom.Vector (LabelList labels) Prom.Histogram)

  FindMetric Counter name (G.Rec0 (Counter name _)) =
    'Just Prom.Counter
  FindMetric Gauge name (G.Rec0 (Gauge name _)) =
    'Just Prom.Gauge
  FindMetric Histogram name (G.Rec0 (Histogram name _ _)) =
    'Just Prom.Histogram

  FindMetric metric name (l G.:*: r) =
    FindMetric metric name l `Alt` FindMetric metric name r
  FindMetric metric name (G.S1 _ rep) =
    FindMetric metric name rep
  FindMetric metric name (G.C1 _ rep) =
    FindMetric metric name rep
  FindMetric metric name (G.D1 _ rep) =
    FindMetric metric name rep
  FindMetric metric name (G.K1 _ _) =
    'Nothing
  FindMetric metric name G.U1 =
    'Nothing
  FindMetric metric name G.V1 =
    'Nothing

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

type family Alt (mx :: Maybe k) (my :: Maybe k) :: Maybe k where
  Alt ('Just x) _ =
    'Just x
  Alt _ y =
    y
