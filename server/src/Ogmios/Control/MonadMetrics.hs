--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadMetrics
    ( -- * Class
      MonadMetrics (..)

      -- * Types
    , Stats
    , mean
    , min
    , max
    ) where

import Ogmios.Prelude hiding
    ( max
    , min
    )

import System.Metrics.Distribution
    ( Stats
    , max
    , mean
    , min
    )

import qualified System.Metrics.Counter as Ekg.Counter
import qualified System.Metrics.Distribution as Ekg.Distribution
import qualified System.Metrics.Gauge as Ekg.Gauge

class Monad m => MonadMetrics (m :: Type -> Type) where
    type Gauge m :: Type
    increment :: Gauge m -> m ()
    decrement :: Gauge m -> m ()
    readGauge :: Gauge m -> m Integer

    type Counter m :: Type
    count :: Counter m -> m ()
    readCounter :: Counter m -> m Integer

    type Distribution m :: Type
    record  :: Distribution m -> Double -> m ()
    readDistribution :: (Stats -> stats) -> Distribution m -> m stats

instance MonadMetrics IO where
    type Gauge IO = Ekg.Gauge.Gauge
    increment = Ekg.Gauge.inc
    decrement = Ekg.Gauge.dec
    readGauge = fmap toInteger . Ekg.Gauge.read

    type Counter IO = Ekg.Counter.Counter
    count = Ekg.Counter.inc
    readCounter = fmap toInteger . Ekg.Counter.read

    type Distribution IO = Ekg.Distribution.Distribution
    record distr = Ekg.Distribution.add distr
    readDistribution mk = fmap mk . Ekg.Distribution.read

instance MonadMetrics m => MonadMetrics (ReaderT env m) where
    type Gauge (ReaderT env m) = Gauge m
    increment = lift . increment
    decrement = lift . decrement
    readGauge = lift . readGauge

    type Counter (ReaderT env m) = Counter m
    count = lift . count
    readCounter = lift . readCounter

    type Distribution (ReaderT env m) = Distribution m
    record distr = lift . record distr
    readDistribution mk = lift . readDistribution mk
