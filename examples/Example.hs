{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import qualified Data.Text as Tx
import qualified GenericPrometheus as Prom
import GHC.Generics (Generic)
import qualified Control.Lens as Lens
import qualified Data.Generics.Product as G.P

import qualified Control.Monad.Reader as Rdr

main :: IO ()
main = do
  env <- mkEnv
  Prom.forkMetricsServer 9753
  runApp env $ do
    let go :: Int -> App ()
        go x = do
          liftIO $ do
            putStrLn $ "Tick " ++ show x
            threadDelay 2000000
          let method = if even x then "GET" else "POST"
              status = Tx.pack (show x) <> "00"
          Prom.withMetricIO _msCounter Prom.incCounter
          Prom.withMetricIO _msVCounter $ \v ->
            Prom.withLabel v (method Prom.:> status Prom.:> Prom.LNil) Prom.incCounter
          Prom.withMetric (_nsCounter . _msNested) Prom.incCounter
          go (if x == 4 then 1 else x + 1)
    go 1

mkEnv :: IO Env
mkEnv = do
  ms <- Prom.registerMetrics @Metrics
  _ <- Prom.register Prom.ghcMetrics
  pure Env
    { _eMetrics = ms
    }

newtype App a
  = App { _runApp :: ReaderT Env IO a }
  deriving newtype (Applicative, Functor, Monad,
                    MonadIO, MonadReader Env)
  deriving (Prom.MonadPrometheus Metrics)
    via (Prom.PrometheusT Metrics App)

instance Prom.MonadMonitor App where
  doIO = liftIO

instance (Prom.MonadPrometheus NestedMetrics) App where
  getMetrics = do
    env <- Rdr.ask
    pure $ Lens.view (G.P.typed @Metrics . G.P.typed @NestedMetrics) env

runApp :: Env -> App a -> IO a
runApp env (App m) =
  runReaderT m env

data Env
  = Env
      { _eMetrics :: Metrics
      }

  deriving stock (Generic)

data Metrics
  = Metrics
      { _msCounter
          :: Prom.AMetric "example_counter" "An example counter" Prom.Counter
      , _msVCounter
          :: Prom.AMetric "example_vcounter" "An example vector counter"
              (Prom.AVector '["method", "status"] Prom.Counter)
      , _msGauge
          :: Prom.AMetric "example_gauge" "An example gauge" Prom.Gauge
      , _msHistogram
          :: Prom.AMetric "example_histogram" "An example histogram"
              (Prom.AHistogram '["0.1", "0.25", "0.5", "1.0", "5.0", "10.0"])
      , _msSummary
          :: Prom.AMetric "example_summary" "An example summary"
              (Prom.ASummary '[ '( "0.5", "0.05" ), '( "0.9", "0.01" ), '( "0.99", "0.001" ) ])

      , _msNested :: NestedMetrics
      }

  deriving stock (Generic)

-- Nested metric, could be imported from another package or library
data NestedMetrics
  = NestedMetrics
      { _nsCounter
          :: Prom.AMetric "nested_example_counter" "A nested example counter" Prom.Counter
      }
  deriving stock (Generic)
