-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.App.Protocol.Util
    ( prop_inIOSim
    , expectRpcResponse
    , expectRpcFault
    , withMockChannel
    , ResponsePredicate (..)

      -- * Exceptions
    , FailedToDecodeMsg (..)
    , PeerTerminatedUnexpectedly (..)
    , UnexpectedResponse (..)
    ) where

import Ogmios.Prelude

import Control.Exception
    ( evaluate
    )
import Control.Monad.Class.MonadTimer
    ( MonadDelay
    )
import Control.Monad.IOSim
    ( IOSim
    , runSimOrThrow
    )
import Ogmios.Control.Exception
    ( MonadCatch (..)
    , MonadThrow (..)
    )
import Ogmios.Control.MonadAsync
    ( race
    )
import Ogmios.Control.MonadDisk
    ( MonadDisk
    )
import Ogmios.Control.MonadLog
    ( MonadLog
    )
import Ogmios.Control.MonadOuroboros
    ( MonadOuroboros
    )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , newTQueue
    , readTQueue
    , writeTQueue
    )
import Ogmios.Data.Json.Prelude
    ( at
    , inefficientEncodingToValue
    )
import Ouroboros.Network.Channel
    ( Channel (..)
    )
import System.Random
    ( StdGen
    , mkStdGen
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    )
import Test.QuickCheck.Monadic
    ( monadicIO
    , pick
    , run
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson as Json
import qualified Text.Show

-- | Run a function in IOSim, with a 'StdGen' input which can used to compute
-- random numbers deterministically within the simulation. The random generator
-- is however randomly seeded for each property run.
prop_inIOSim
    :: (forall s. StdGen -> IOSim s ())
    -> Property
prop_inIOSim action = monadicIO $ do
    seed <- mkStdGen <$> pick arbitrary
    run $ evaluate $ runSimOrThrow $ action seed

-- Run an action concurrently with a peer, and a 'fake' channel connected to
-- TQueue. The peer is expected to consume and produce values from and to the
-- TQueue to drive the channel communication.
withMockChannel
    :: forall m a. (MonadOuroboros m)
    => ((m LByteString, LByteString -> m ()) -> m ())
        -- ^ A mock peer for consuming the data.
    -> (Channel m LByteString -> m a)
        -- ^ Callback using the channel
    -> m a
withMockChannel mockPeer action = do
    (readBuffer, writeBuffer) <- atomically $ (,) <$> newTQueue <*> newTQueue
    let send = atomically . writeTQueue writeBuffer
    let recv = Just <$> atomically (readTQueue readBuffer)
    let channel = Channel{send,recv}
    let sendDual = atomically (readTQueue writeBuffer)
    let recvDual = atomically . writeTQueue readBuffer
    result <- race (mockPeer (sendDual, recvDual)) (action channel)
    either (const $ error "mockNode terminated?") pure result

-- | Assert that a given JSON object is a JSON-Rpc response for the given
-- method.
--
-- >>> expectRpcResponse @"RequestNext" recv Nothing
-- ()
expectRpcResponse
    :: forall m. (MonadThrow m)
    => ResponsePredicate
    -> m Json.Encoding
    -> Json.Value
    -> m ()
expectRpcResponse (ResponsePredicate isExpectedResponse) recv wantMirror = do
    json <- inefficientEncodingToValue <$> recv

    unless (isExpectedResponse json) $
        throwIO $ UnexpectedResponse "unexpected result" json (Just json) json

    let gotMirror = "id" `at` json
    when (gotMirror /= Just wantMirror) $
        throwIO $ UnexpectedResponse "id" json gotMirror wantMirror

newtype ResponsePredicate = ResponsePredicate (Json.Value -> Bool)

instance Show ResponsePredicate where
    show _ = "ResponsePredicate"

expectRpcFault
    :: (MonadThrow m)
    => m Json.Encoding
    -> Rpc.FaultCode
    -> Json.Value
    -> m ()
expectRpcFault recv wantCode wantMirror = do
    json <- inefficientEncodingToValue <$> recv

    let gotCode = ("error" `at` json) >>= at "code"
    let wantCode' = Json.toJSON wantCode
    when (gotCode /= Just wantCode') $
        throwIO $ UnexpectedResponse "error" json gotCode wantCode'

    let gotMirror = "id" `at` json
    when (gotMirror /= Just wantMirror) $
        throwIO $ UnexpectedResponse "id" json gotMirror wantMirror

--
-- Exceptions
--

newtype FailedToDecodeMsg = FailedToDecodeMsg String deriving Show
instance Exception FailedToDecodeMsg

data PeerTerminatedUnexpectedly = PeerTerminatedUnexpectedly deriving Show
instance Exception PeerTerminatedUnexpectedly

data UnexpectedResponse = UnexpectedResponse
    { what :: String
    , rawJson :: Json.Value
    , gotResponse :: Maybe Json.Value
    , expectedResponse :: Json.Value
    } deriving Show
instance Exception UnexpectedResponse
