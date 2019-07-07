{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Example where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import qualified Data.Text as Tx
import qualified GenericPrometheus as Prom
import GHC.Generics (Generic)

main :: IO ()
main = do
  env <- mkEnv
  Prom.forkMetricsServer 9753
  runApp env $ do
    let go x = do
          liftIO $ do
            putStrLn $ "Tick " ++ show x
            threadDelay 2000000
          let method = if even x then "GET" else "POST"
              status = Tx.pack (show x) <> "00"
          Prom.withLabel @"example_vcounter" @'["method", "status"]
            (Prom.LCons method $ Prom.LCons status Prom.LNil) Prom.incC
          go (if x == 4 then 1 else x + 1)
    go 1

mkEnv :: IO Env
mkEnv = do
  ms <- Prom.registerMetrics @Metrics
  pure Env
    { _eMetrics = ms
    }

newtype App a
  = App { _runApp :: ReaderT Env IO a }
  deriving newtype (Applicative, Functor, Monad,
                    MonadIO, MonadReader Env)
  deriving (Prom.MonadPrometheus Metrics)
    via (Prom.PrometheusT Metrics App)

runApp :: Env -> App a -> IO a
runApp env (App m) =
  runReaderT m env

data Env
  = Env
      { _eMetrics :: Metrics
      }

  deriving stock (Generic)

data Metrics
  = Ms
      { _msCounter
          :: Prom.Counter "example_counter" "An example counter"
      , _msVCounter
          :: Prom.Vector '["method", "status"]
              (Prom.Counter "example_vcounter" "An example vector counter")
      , _msGauge
          :: Prom.Gauge "example_gauge" "An example gauge"
      , _msHistogram
          :: Prom.Histogram "example_histogram" "An example histogram"
              '["0.1", "0.25", "0.5", "1.0", "5.0", "10.0"]
      }

  deriving stock (Generic)
