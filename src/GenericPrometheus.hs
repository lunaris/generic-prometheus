{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module GenericPrometheus where

import GenericPrometheus.Lib

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import GHC.Generics (Generic)

newtype App a
  = App (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad,
                    MonadIO, MonadReader Env)
  deriving (MonadPrometheus AppMetrics)
    via (PrometheusT AppMetrics App)

data Env
  = Env
      { _eMetrics :: AppMetrics
      }

  deriving (Generic)

data AppMetrics
  = AppMetrics
      { _amCtr    :: Counter "example_counter" '[]
      , _amGauge1 :: Gauge "example_gauge" '[ "login" := "failure" ]
      , _amGauge2 :: Gauge "example_gauge" '[ "login" := "success" ]
      , _amHisto  :: Histogram "example_histogram" '[] '[10, 20, 30]
      }

  deriving (Generic)
