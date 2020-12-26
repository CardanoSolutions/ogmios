--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadSTM
    ( MonadSTM (..)

    , TQueue
    , newTQueue
    , writeTQueue
    , readTQueue
    , tryReadTQueue

    , TVar
    , newTVar
    , writeTVar
    , readTVar

    , TMVar
    , newTMVar
    , putTMVar
    , takeTMVar
    , tryTakeTMVar
    ) where

import Control.Monad.Class.MonadSTM
    ( MonadSTM (..), MonadSTMTx (..), TMVar, TQueue, TVar )
