{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericPrometheus.Lib where

import qualified Control.Lens as Lens
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import qualified Data.Generics.Product as G.P
import Data.Kind (Type)
import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits (Nat, Symbol)
import qualified System.Metrics.Prometheus.Metric.Counter as Prom.M.Ctr
import qualified System.Metrics.Prometheus.Metric.Gauge as Prom.M.Gg
import qualified System.Metrics.Prometheus.Metric.Histogram as Prom.M.Histo

type Labels
  = [(Symbol, Symbol)]

type k := v
  = '(k, v)

newtype Counter (name :: Symbol) (labels :: Labels)
  = Counter Prom.M.Ctr.Counter

newtype Gauge (name :: Symbol) (labels :: Labels)
  = Gauge Prom.M.Gg.Gauge

newtype Histogram (name :: Symbol) (labels :: Labels) (buckets :: HistogramBuckets)
  = Histogram Prom.M.Histo.Histogram

type HistogramBuckets
  = [Nat]

class MonadPrometheus (metrics :: Type) m | m -> metrics where
  incCounter' :: HasCounter name metrics => Proxy# name -> m ()

incCounter
  :: forall name metrics m
   . ( MonadPrometheus metrics m
     , HasCounter name metrics
     )
  => m ()
incCounter =
  incCounter' (proxy# :: Proxy# name)

newtype PrometheusT metrics m a
  = PrometheusT (m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader r)

instance (MonadIO m, MonadReader r m, G.P.HasType metrics r)
      =>  MonadPrometheus metrics (PrometheusT metrics m) where
  incCounter' (_ :: Proxy# name) = do
    ms <- Lens.view (G.P.typed @metrics)
    let c = getCounter @name ms
    liftIO $ Prom.M.Ctr.inc c

class HasCounter (name :: Symbol) (metrics :: Type) where
  getCounter :: metrics -> Prom.M.Ctr.Counter

class HasGauge (name :: Symbol) (metrics :: Type) where
  getGauge :: metrics -> Prom.M.Gg.Gauge

class HasHistogram (name :: Symbol) (metrics :: Type) where
  getHistogram :: metrics -> Prom.M.Histo.Histogram
