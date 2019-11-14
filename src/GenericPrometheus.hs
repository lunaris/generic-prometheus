{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericPrometheus
  ( -- *Generic Prometheus metrics

    -- $example

    -- **Metric types

    -- $metrics

    AMetric (..)
  , AVector

  , Buckets (..)
  , AHistogram
  , bucketsVal

  , Quantiles (..)
  , ASummary
  , quantilesVal

  , MonadPrometheus (..)
  , PrometheusT (..)

  , withMetric
  , withMetricIO

  , Prom.Counter
  , Prom.incCounter
  , Prom.addCounter
  , Prom.getCounter

  , Prom.Gauge
  , Prom.addGauge
  , Prom.subGauge
  , Prom.incGauge
  , Prom.decGauge
  , Prom.setGauge
  , Prom.getGauge

  , Prom.MonadMonitor (..)
  , Prom.Monitor
  , Prom.runMonitor
  , Prom.MonitorT
  , Prom.runMonitorT

  , Prom.Observer (..)
  , Prom.observeDuration

  , Prom.Histogram
  , Prom.getHistogram

  , Prom.Summary
  , Prom.getSummary

  , Prom.Vector
  , Prom.withLabel
  , Prom.getVectorWith

    -- **Construction and registration

  , registerMetrics
  , GRegistersMetrics (..)
  , ConstructableMetric (..)

  , Prom.register
  , Prom.GHC.GHCMetrics
  , Prom.GHC.ghcMetrics

    -- **Serving metrics
  , forkMetricsServer

    -- **Labelling metrics
  , LabelList (..)

    -- **Utilities
  , KnownSymbols (..)
  , KnownSymbolPairs (..)
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Lens as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Bitraversable (bitraverse)
import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Product as G.P
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import qualified Data.Text as Tx
import qualified GHC.Generics as G
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Prometheus as Prom.Wai
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom.GHC
import Text.Read (readMaybe)

-- $example
--
-- The types and functions in this module are designed for use with @deriving
-- via@ to enable adding Prometheus-compatible metrics to an application
-- (structured as a program running in an environment with access to @IO@).
-- Assuming that you have a @newtype@ representing such an application:
--
-- @
-- newtype App a
--   = App { _runApp :: ReaderT Env IO a }
--   deriving newtype (Applicative, Functor, Monad,
--                     MonadIO, MonadReader Env)
--
-- data Env
--   = Env
--       { ...
--       }
-- @
--
-- You can add support for Prometheus metrics by @deriving@ an instance of
-- 'MonadPrometheus' @via@ this module's 'PrometheusT' type:
--
-- @
-- import qualified GenericPrometheus as Prom
--
-- newtype App a
--   = App { _runApp :: ReaderT Env IO a }
--   ...
--   deriving (Prom.MonadPrometheus Metrics)
--     via (Prom.PrometheusT Metrics App)
-- @
--
-- Doing so requires that your @Env@ type is 'G.Generic' and has a field of the
-- type named in the 'MonadPrometheus' instance (here @Metrics@). This field
-- should also be 'G.Generic' and consist only of metrics which can be generically
-- constructed, as defined by the 'ConstructableMetric' class in this module:
--
-- @
-- data Env
--   = Env
--       { ...
--       , _eMetrics :: Metrics
--       }
--
--   deriving (Generic)
--
-- data Metrics
--   = Metrics
--       { _msCounter
--           :: Prom.AMetric "example_counter" "An example counter" Prom.Counter
--       , _msVCounter
--           :: Prom.AMetric "example_vcounter" "An example vector counter"
--               (Prom.AVector '["method", "status"] Prom.Counter)
--       , _msGauge
--           :: Prom.AMetric "example_gauge" "An example gauge" Prom.Gauge
--       , _msHistogram
--           :: Prom.AMetric "example_histogram" "An example histogram"
--               (Prom.AHistogram '["0.1", "0.25", "0.5", "1.0", "5.0", "10.0"])
--       , _msSummary
--           :: Prom.AMetric "example_summary" "An example summary"
--              (Prom.ASummary '[ '( "0.5", "0.05" ), '( "0.9", "0.01" ), '( "0.99", "0.001" ) ])
--       }
--
--   deriving (Generic)
-- @
--
-- All you need do then is initialise and register such a set of metrics in your
-- @main@ function (or similar) -- the 'registerMetrics' function takes care of
-- just this task:
--
-- @
-- main :: IO ()
-- main = do
--   ...
--   env <- mkEnv ...
--
--   ... (runApp env) ...
--
-- mkEnv :: ... -> IO Env
-- mkEnv ... = do
--   metrics <- Prom.registerMetrics @Metrics
--   ...
--   pure Env
--     { ...
--     , _eMetrics = metrics
--     }
--
-- runApp :: Env -> App a -> IO a
-- runApp env (App m) =
--   runReaderT m env
-- @
--
-- Serving the metrics can be achieved by incorporating the 'Prom.metricsApp' WAI
-- @Application@ into an existing HTTP flow or by using 'forkMetricsServer' to
-- fork a thread running a Warp serve that serves any registered metrics on the
-- given port:
--
-- @
-- main = do
--   ...
--   Prom.forkMetricsServer 9090
--
--   ...
-- @
--
-- You can then record metrics using 'withMetricIO' and standard functions from
-- the @prometheus-client@ library; for example:
--
-- @
-- Prom.withMetricIO _msCounter Prom.incCounter
-- @
--
-- @
-- Prom.withMetricIO _msGauge (Prom.setGauge 3.0)
-- @
--
-- @
-- Prom.withMetricIO _msVCounter $ \v ->
--   Prom.withLabel v ("GET" :> "200" :> LNil) Prom.incCounter
-- @
--
-- To keep track of the time taken by some part of your program, you should use
-- 'observeDuration'. However, the action whose time you want to track might not
-- be in 'IO'. In this case, 'withMetric' allows you to run the action in the
-- same monad.
--
-- @
-- Prom.withMetric _msSummary $ \metric -> Prom.observeDuration metric $ do
--   getStuffFromDB
-- @

-- $metrics
--
-- Broadly speaking, there are four types of Prometheus metrics:
--
-- * Counters
-- * Gauges
-- * Histograms
-- * Summaries
--
-- Documentation on these can be found in the [Prometheus
-- manual](https://prometheus.io/docs/concepts/metric_types/). Each has a type
-- in this module:
--
-- * 'Prom.Counter'
-- * 'Prom.Gauge'
-- * 'Prom.Histogram'
-- * 'Prom.Summary'
--
-- A fifth type, 'Prom.Vector', allows decorating any given metric with a set of
-- labels, which allows parameterising a metric by some additional information
-- (e.g. a counter of requests could have labels for HTTP method and status
-- code, allowing it to count successful @GET@ requests, failed @POST@ requests,
-- etc.).
--
-- When decorated by the 'AMetric' @newtype@, these types are all instances of
-- 'ConstructableMetric', allowing them to be used in 'G.Generic' records which
-- are to be automatically constructed and registered:
--
-- * @AMetric name description Counter@
--
-- * @AMetric name description Gauge@
--
-- * @AMetric name description (Buckets buckets Histogram)@
--
-- * @AMetric name description (Quantiles quantiles Summary)@
--
-- * @AMetric name description (Vector (LabelList labels) metric)@
--
-- Note that histograms, summaries and vectors take additional parameters as
-- part of their construction, represented by the 'Buckets', 'Quantiles' and
-- 'LabelList' types, respectively. A set of synonyms exist to make it slightly
-- easier to work with these common cases:
--
-- * @AHistogram buckets@, equivalent to @Buckets buckets Histogram@.
--
-- * @ASummary quantiles@, equivalent to @Quantiles quantiles Summary@.
--
-- * @AVector labels metric@, equivalent to @Vector (LabelList labels) metric@.

newtype AMetric (name :: Symbol) (description :: Symbol) (metric :: Type)
  = AMetric { _aMetric :: metric }
  deriving stock (Eq, G.Generic, Ord, Show)
  deriving newtype Prom.Observer

type AVector (labels :: [Symbol]) (metric :: Type)
  = Prom.Vector (LabelList labels) metric

newtype Buckets (buckets :: [Symbol]) (metric :: Type)
  = Buckets { _buckets :: metric }
  deriving stock (Eq, G.Generic, Ord, Show)
  deriving newtype Prom.Observer

type AHistogram (buckets :: [Symbol])
  = Buckets buckets Prom.Histogram

bucketsVal :: forall buckets. KnownSymbols buckets => Maybe [Double]
bucketsVal =
  traverse readMaybe (symbolStrs @buckets)

newtype Quantiles (quantiles :: [(Symbol, Symbol)]) (metric :: Type)
  = Quantiles { _quantiles :: metric }
  deriving stock (Eq, G.Generic, Ord, Show)
  deriving newtype Prom.Observer

type ASummary (quantiles :: [(Symbol, Symbol)])
  = Quantiles quantiles Prom.Summary

quantilesVal
  :: forall quantiles
   . KnownSymbolPairs quantiles
  => Maybe [Prom.Quantile]
quantilesVal =
  traverse (bitraverse k k) (symbolStrPairs @quantiles)
  where
    k = fmap toRational . readMaybe @Double

-- |Inside some 'MonadPrometheus' @m@ with access to a record of metrics with
--  type @metrics@, accepts a function to select a metric and a function to
--  modify it and applies the modification appropriately. The metric being
--  selected can be wrapped with arbitrary @newtype@s (e.g. 'Buckets',
--  'Quantiles', etc.).
--
--  @
--  --  Assuming the @Metrics@ record defined in the example:
--  Prom.withMetricIO _msCounter Prom.incCounter
--
--  Prom.withMetricIO _msGauge (Prom.setGauge 3.0)
--
--  Prom.withMetricIO _msHistogram (Prom.observe 5.0)
--
--  Prom.withMetricIO _msVCounter $ \v ->
--    Prom.withLabel v ("GET" :> "200" :> Prom.LNil) Prom.incCounter
--  @
withMetricIO
  :: ( MonadIO m
     , MonadPrometheus metrics m
     , Coercible wrapped metric
     )
  => (metrics -> wrapped)
  -> (metric -> IO a)
  -> m a
withMetricIO get k =
  withMetric get (liftIO . k)

-- |Inside some 'MonadPrometheus' @m@ with access to a record of metrics with
--  type @metrics@, accepts a function to select a metric and a function to
--  modify it and applies the modification appropriately. Contrary to
--  'withMetricIO', it lets you run the modification code in the same monad.
--
--  @
--  Prim.withMetric _msSummary $ \metric -> Prom.observeDuration metric $ do
--    -- We remain in the same monad here.
--    doSomething
--  @
withMetric
  :: ( MonadPrometheus metrics m
     , Coercible wrapped metric
     )
  => (metrics -> wrapped)
  -> (metric -> m a)
  -> m a
withMetric get k =
  getMetrics >>= k . coerce . get

-- |The class of 'Monad's with access to a set of @metrics@.
class Monad m => MonadPrometheus metrics m where
  getMetrics :: m metrics

-- |An identity 'Monad' transformer that can be used with @deriving via@ to
--  derive a 'MonadPrometheus' instance which uses 'G.Generic's to find a
--  @metrics@ record in an environment determined by 'MonadReader'.
newtype PrometheusT (metrics :: Type) (m :: Type -> Type) (a :: Type)
  = PrometheusT (m a)
  deriving (Applicative, Functor, Monad,
            MonadReader r)

instance ( MonadReader r m
         , G.P.HasType metrics r
         )
      => MonadPrometheus metrics (PrometheusT metrics m) where
  getMetrics =
    Lens.view (G.P.typed @metrics)

--------------------------------------------------------------------------------
--  Construction and registration
--------------------------------------------------------------------------------

-- |Constructs and registers a 'G.Generic' structure of 'ConstructableMetric's.
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

instance ( KnownSymbol name
         , KnownSymbol description
         , ConstructableMetric metric
         )
      => GRegistersMetrics (G.Rec0 (AMetric name description metric)) where
  gregisterMetrics =
    fmap coerce $ Prom.register $
      mkMetric @metric
        (Prom.Info (symbolStr @name) (symbolStr @description))

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

-- |The class of metrics which can be constructed from a @name@ and
--  @description@.
class ConstructableMetric (metric :: Type) where
  mkMetric :: Prom.Info -> Prom.Metric metric

instance ( ConstructableMetric metric
         , KnownLabels labels
         )
      => ConstructableMetric (Prom.Vector (LabelList labels) metric) where
  mkMetric =
    Prom.vector (labelListVal @labels) . mkMetric @metric

instance ConstructableMetric Prom.Counter where
  mkMetric =
    Prom.counter

instance ConstructableMetric Prom.Gauge where
  mkMetric =
    Prom.gauge

instance KnownSymbols buckets
      => ConstructableMetric (Buckets buckets Prom.Histogram) where
  mkMetric =
    case bucketsVal @buckets of
      Nothing ->
        --  Ideally we'd have type-level doubles/decimals, but in the absence
        --  of those we'll try to parse @Symbol@s and use a runtime error to
        --  notify the user if something went wrong.
        error
          $  "The buckets "
          <> show (symbolStrs @buckets @String)
          <> " are not all valid Doubles"
      Just bs ->
        coerce (flip Prom.histogram bs)

instance KnownSymbolPairs quantiles
      => ConstructableMetric (Quantiles quantiles Prom.Summary) where
  mkMetric =
    case quantilesVal @quantiles of
      Nothing ->
        --  Ideally we'd have type-level doubles/decimals, but in the absence
        --  of those we'll try to parse @Symbol@s and use a runtime error to
        --  notify the user if something went wrong.
        error
          $  "The quantiles "
          <> show (symbolStrPairs @quantiles @String @String)
          <> " are not all valid (Double, Double) tuples"
      Just qs ->
        coerce (flip Prom.summary qs)

--------------------------------------------------------------------------------
--  Serving metrics
--------------------------------------------------------------------------------

-- |Forks a Warp server on the given port which will serve metrics on any
--  endpoint.
forkMetricsServer
  :: MonadIO m
  => Int
  -> m (Async.Async ())
forkMetricsServer port = liftIO $
  Async.async $ Warp.run port Prom.Wai.metricsApp

--------------------------------------------------------------------------------
--  Labelling metrics
--------------------------------------------------------------------------------

-- |A list of label values indexed by 'Symbol's indicating the keys. For
--  example:
--
--  @
--  ("GET" :> "200" :> LNil) :: LabelList '["method", "status"]
--  @
data LabelList :: [Symbol] -> Type where
  LNil :: LabelList '[]
  (:>) :: Tx.Text -> LabelList labels -> LabelList (label ': labels)

infixr 5 :>

deriving instance Eq (LabelList labels)
deriving instance Ord (LabelList labels)

class KnownLabels (labels :: [Symbol]) where
  labelListVal   :: LabelList labels
  labelListPairs :: LabelList labels -> LabelList labels -> Prom.LabelPairs

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
    symbolStr @label :> labelListVal @labels
  labelListPairs (l1 :> ls1) (l2 :> ls2) =
    (l1, l2) : labelListPairs ls1 ls2

instance KnownLabels labels => Prom.Label (LabelList labels) where
  labelPairs =
    labelListPairs

--------------------------------------------------------------------------------
--  Utilities
--------------------------------------------------------------------------------

symbolStr :: forall name a. (KnownSymbol name, IsString a) => a
symbolStr =
  fromString (symbolVal (Proxy @name))

class KnownSymbols (ss :: [Symbol]) where
  symbolStrs :: IsString a => [a]

instance KnownSymbols '[] where
  symbolStrs =
    []

instance (KnownSymbol s, KnownSymbols ss)
      =>  KnownSymbols (s ': ss) where
  symbolStrs =
    symbolStr @s : symbolStrs @ss

class KnownSymbolPairs (ps :: [(Symbol, Symbol)]) where
  symbolStrPairs :: (IsString a, IsString b) => [(a, b)]

instance KnownSymbolPairs '[] where
  symbolStrPairs =
    []

instance (KnownSymbol s1, KnownSymbol s2, KnownSymbolPairs ps)
      =>  KnownSymbolPairs ( '(s1, s2) ': ps ) where
  symbolStrPairs =
    (symbolStr @s1, symbolStr @s2) : symbolStrPairs @ps
