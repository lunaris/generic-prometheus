{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericPrometheus
  ( AMetric (..)
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

  , Prom.Observer (..)
  , Prom.observeDuration

  , Prom.Histogram
  , Prom.getHistogram

  , Prom.Summary
  , Prom.getSummary

  , Prom.Vector
  , Prom.withLabel
  , Prom.getVectorWith

  , registerMetrics
  , GRegistersMetrics (..)
  , ConstructableMetric (..)

  , Prom.register
  , Prom.GHC.ghcMetrics

  , forkMetricsServer

  , LabelList (..)

  , KnownSymbols (..)
  , KnownSymbolPairs (..)
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Lens as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Bitraversable (bitraverse)
import Data.Coerce (Coercible, coerce)
import Data.Functor (void)
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

newtype AMetric (name :: Symbol) (description :: Symbol) (metric :: Type)
  = AMetric { _aMetric :: metric }
  deriving stock (Eq, G.Generic, Ord, Show)

type AVector (labels :: [Symbol]) (metric :: Type)
  = Prom.Vector (LabelList labels) metric

newtype Buckets (buckets :: [Symbol]) (metric :: Type)
  = Buckets { _buckets :: metric }
  deriving stock (Eq, G.Generic, Ord, Show)

type AHistogram (buckets :: [Symbol])
  = Buckets buckets Prom.Histogram

bucketsVal :: forall buckets. KnownSymbols buckets => Maybe [Double]
bucketsVal =
  traverse readMaybe (symbolStrs @buckets)

newtype Quantiles (quantiles :: [(Symbol, Symbol)]) (metric :: Type)
  = Quantiles { _quantiles :: metric }
  deriving stock (Eq, G.Generic, Ord, Show)

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

withMetric
  :: ( MonadIO m
     , MonadPrometheus metrics m
     , Coercible wrapped metric
     )
  => (metrics -> wrapped)
  -> (metric -> IO a)
  -> m a
withMetric get k =
  getMetrics >>= liftIO . k . coerce . get

class MonadPrometheus metrics m | m -> metrics where
  getMetrics :: m metrics

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

forkMetricsServer
  :: MonadIO m
  => Int
  -> m ()
forkMetricsServer port = liftIO $ void $
  Async.async $ Warp.run port Prom.Wai.metricsApp

--------------------------------------------------------------------------------
--  Labelling metrics
--------------------------------------------------------------------------------

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
