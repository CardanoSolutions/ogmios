--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Control where

import Ogmios.Prelude

import Control.Monad.Class.MonadST
    ( MonadST
    )
import Ogmios.Control.Exception
    ( MonadCatch
    , MonadMask
    , MonadThrow
    )
import Ogmios.Control.MonadAsync
    ( MonadAsync (..)
    , MonadFork
    , MonadThread
    )
import Ogmios.Control.MonadClock
    ( MonadClock
    )
import Ogmios.Control.MonadLog
    ( MonadLog (..)
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    )
import Ogmios.Control.MonadWebSocket
    ( MonadWebSocket
    )
import Ouroboros.Consensus.Util.IOLike
    ( PrimMonad
    )

import qualified Control.Monad.Class.MonadSTM.Internal

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TArray as STM
import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TChan as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TSem as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

--
-- App
--

data family Env (f :: Type -> Type) :: Type

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving newtype
        ( Functor, Applicative, Monad
        , PrimMonad
        , MonadIO
        , MonadReader (Env App)
        , MonadLog, MonadMetrics
        , MonadWebSocket
        , MonadClock
        , MonadST
        , MonadThread, MonadFork
        , MonadThrow, MonadCatch, MonadMask
        )

-- NOTE:
--
-- Since `6b83e2b385b3533ccfc336c1099d27f1c6a79a2c` in ouroboros-network,
-- functional dependencies have been introduced between the class type family
-- and the monad argument making that monad injective with the type-family.
--
-- This means it's no longer possible to define instances of MonadSTM which use
-- `STM` as a target monad directly, without conflicting with the existing
-- instance definition for `IO`... this disqualifies `ReaderT` and all kind of
-- transformers, and also, our `App` application monad!

newtype WrappedSTM a = WrappedSTM { unwrapSTM :: STM.STM a }
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow)

instance MonadSTM App where
  type STM App = WrappedSTM
  atomically   = App . lift . STM.atomically . unwrapSTM
  retry        = WrappedSTM STM.retry
  orElse a0 a1 = WrappedSTM (STM.orElse (unwrapSTM a0) (unwrapSTM a1))
  check        = WrappedSTM . STM.check

  type TVar App  = STM.TVar
  newTVar        = WrappedSTM . STM.newTVar
  newTVarIO      = App . lift . STM.newTVarIO
  readTVar       = WrappedSTM . STM.readTVar
  readTVarIO     = App . lift . STM.readTVarIO
  writeTVar a0   = WrappedSTM . STM.writeTVar a0
  modifyTVar a0  = WrappedSTM . STM.modifyTVar a0
  modifyTVar' a0 = WrappedSTM . STM.modifyTVar' a0
  stateTVar a0   = WrappedSTM . STM.stateTVar a0
  swapTVar a0    = WrappedSTM . STM.swapTVar a0

  type TMVar App  = STM.TMVar
  newTMVar        = WrappedSTM . STM.newTMVar
  newTMVarIO      = App . lift . STM.newTMVarIO
  newEmptyTMVar   = WrappedSTM STM.newEmptyTMVar
  newEmptyTMVarIO = App (lift STM.newEmptyTMVarIO)
  takeTMVar       = WrappedSTM . STM.takeTMVar
  tryTakeTMVar    = WrappedSTM . STM.tryTakeTMVar
  putTMVar a0     = WrappedSTM . STM.putTMVar a0
  tryPutTMVar a0  = WrappedSTM . STM.tryPutTMVar a0
  readTMVar       = WrappedSTM . STM.readTMVar
  tryReadTMVar    = WrappedSTM . STM.tryReadTMVar
  swapTMVar a0    = WrappedSTM . STM.swapTMVar a0
  isEmptyTMVar    = WrappedSTM . STM.isEmptyTMVar

  type TQueue App = STM.TQueue
  newTQueue       = WrappedSTM STM.newTQueue
  newTQueueIO     = App (lift STM.newTQueueIO)
  readTQueue      = WrappedSTM . STM.readTQueue
  tryReadTQueue   = WrappedSTM . STM.tryReadTQueue
  peekTQueue      = WrappedSTM . STM.peekTQueue
  tryPeekTQueue   = WrappedSTM . STM.tryPeekTQueue
  flushTBQueue    = WrappedSTM . STM.flushTBQueue
  writeTQueue a0  = WrappedSTM . STM.writeTQueue a0
  isEmptyTQueue   = WrappedSTM . STM.isEmptyTQueue
  flushTQueue     = WrappedSTM . STM.flushTQueue
  unGetTQueue a0  = WrappedSTM . STM.unGetTQueue a0

  type TBQueue App = STM.TBQueue
  newTBQueue       = WrappedSTM . STM.newTBQueue
  newTBQueueIO     = App . lift . STM.newTBQueueIO
  readTBQueue      = WrappedSTM . STM.readTBQueue
  tryReadTBQueue   = WrappedSTM . STM.tryReadTBQueue
  peekTBQueue      = WrappedSTM . STM.peekTBQueue
  tryPeekTBQueue   = WrappedSTM . STM.tryPeekTBQueue
  writeTBQueue a0  = WrappedSTM . STM.writeTBQueue a0
  lengthTBQueue    = WrappedSTM . STM.lengthTBQueue
  isEmptyTBQueue   = WrappedSTM . STM.isEmptyTBQueue
  isFullTBQueue    = WrappedSTM . STM.isFullTBQueue
  unGetTBQueue a0  = WrappedSTM . STM.unGetTBQueue a0

  type TArray App = STM.TArray

  type TSem App  = STM.TSem
  newTSem        = WrappedSTM . STM.newTSem
  waitTSem       = WrappedSTM . STM.waitTSem
  signalTSem     = WrappedSTM . STM.signalTSem
  signalTSemN a0 = WrappedSTM . STM.signalTSemN a0

  type TChan App    = STM.TChan
  newTChan          = WrappedSTM STM.newTChan
  newBroadcastTChan = WrappedSTM STM.newBroadcastTChan
  dupTChan          = WrappedSTM . STM.dupTChan
  cloneTChan        = WrappedSTM . STM.cloneTChan
  readTChan         = WrappedSTM . STM.readTChan
  tryReadTChan      = WrappedSTM . STM.tryReadTChan
  peekTChan         = WrappedSTM . STM.peekTChan
  tryPeekTChan      = WrappedSTM . STM.tryPeekTChan
  writeTChan a0     = WrappedSTM . STM.writeTChan a0
  unGetTChan a0     = WrappedSTM . STM.unGetTChan a0
  isEmptyTChan      = WrappedSTM . STM.isEmptyTChan

newtype WrappedAsync a = WrappedAsync { unwrapAsync :: Async.Async a }
    deriving newtype (Functor)

instance MonadAsync App where
  type Async App  = WrappedAsync
  async (App (ReaderT m))      = App (ReaderT $ \r -> WrappedAsync <$> async (m r))
  asyncBound (App (ReaderT m)) = App (ReaderT $ \r -> WrappedAsync <$> asyncBound (m r))
  asyncOn n (App (ReaderT m))  = App (ReaderT $ \r -> WrappedAsync <$> asyncOn n (m r))
  asyncThreadId                = Async.asyncThreadId . unwrapAsync
  pollSTM                      = WrappedSTM . Async.pollSTM . unwrapAsync
  waitCatchSTM                 = WrappedSTM . Async.waitCatchSTM . unwrapAsync
  cancel                       = App . lift . Async.cancel . unwrapAsync
  cancelWith a0                = App . lift . Async.cancelWith (unwrapAsync a0)
  asyncWithUnmask restore      =
      App $ ReaderT $ \r ->
          fmap WrappedAsync $ Async.asyncWithUnmask $ \unmask ->
            runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> App a -> App a
      liftF g (App (ReaderT f)) = App (ReaderT (g . f))
  asyncOnWithUnmask n restore  =
      App $ ReaderT $ \r ->
          fmap WrappedAsync $ Async.asyncOnWithUnmask n $ \unmask ->
            runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> App a -> App a
      liftF g (App (ReaderT f)) = App (ReaderT (g . f))

