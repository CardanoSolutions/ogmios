-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ogmios.App.Protocol.ChainSyncSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , cChainSyncCodec
    , chainSyncClientPeerPipelined
    , codecs
    , nodeToClientV_Latest
    , runPipelinedPeer
    )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Network.TypedProtocol.Codec
    ( Codec (..), PeerHasAgency (..), SomeMessage (..), runDecoder )
import Ogmios.App.Options
    ( defaultSlotsPerEpoch )
import Ogmios.App.Protocol.ChainSync
    ( mkChainSyncClient )
import Ogmios.Control.Exception
    ( MonadThrow (..) )
import Ogmios.Control.MonadAsync
    ( race )
import Ogmios.Control.MonadLog
    ( nullTracer )
import Ogmios.Control.MonadOuroboros
    ( MonadOuroboros )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), newTQueue, readTQueue, writeTQueue )
import Ogmios.Data.Json
    ( Json, SerializationMode (..), encodeBlock, encodePoint, encodeTip )
import Ogmios.Data.Protocol.ChainSync
    ( ChainSyncMessage (..)
    , FindIntersect (..)
    , RequestNext (..)
    , mkChainSyncCodecs
    )
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync (..)
    , ClientHasAgency (..)
    , ServerHasAgency (..)
    , TokNextKind (..)
    )
import System.Random
    ( StdGen, random )
import Test.App.Protocol.Util
    ( PeerTerminatedUnexpectedly (..)
    , expectWSPResponse
    , prop_inIOSim
    , withMockChannel
    )
import Test.Generators
    ( genBlock, genPoint, genTip, generateWith )
import Test.Hspec
    ( Spec, context, parallel, specify )
import Test.QuickCheck
    ( Gen, frequency )

import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync

spec :: Spec
spec = parallel $ do
    context "ChainSync" $ do
        specify "Basic scenario" $ prop_inIOSim $ withChainSyncClient $ \send receive -> do
            let mirror = toJSON (14 :: Int)

            send $ MsgRequestNext RequestNext (Wsp.Response Nothing)
            expectWSPResponse @"RequestNext" receive Null

            send $ MsgRequestNext RequestNext (Wsp.Response $ Just mirror)
            expectWSPResponse @"RequestNext" receive mirror

            send $ MsgFindIntersect (FindIntersect []) (Wsp.Response Nothing)
            expectWSPResponse @"FindIntersect" receive Null

            send $ MsgFindIntersect (FindIntersect []) (Wsp.Response $ Just mirror)
            expectWSPResponse @"FindIntersect" receive mirror

type Protocol = ChainSync Block (Point Block) (Tip Block)

withChainSyncClient
    :: (MonadSTM m, MonadOuroboros m)
    => ((ChainSyncMessage Block -> m ()) ->  m Json -> m a)
    -> StdGen
    -> m a
withChainSyncClient action seed = do
    (recvQ, sendQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
    let mode = CompactSerialization
    let innerCodecs = mkChainSyncCodecs (encodeBlock mode) encodePoint encodeTip
    let client = mkChainSyncClient innerCodecs recvQ (atomically . writeTQueue sendQ)
    let codec = codecs defaultSlotsPerEpoch nodeToClientV_Latest & cChainSyncCodec
    withMockChannel (chainSyncMockPeer seed codec) $ \channel -> do
        result <- race
            (runPipelinedPeer nullTracer codec channel (chainSyncClientPeerPipelined client))
            (action (atomically . writeTQueue recvQ) (atomically $ readTQueue sendQ))
        case result of
            Left{}  -> throwIO PeerTerminatedUnexpectedly
            Right a -> pure a

chainSyncMockPeer
    :: forall m failure. (MonadSTM m, Show failure)
    => StdGen
        -- ^ Random generator
    -> Codec Protocol failure m LByteString
        -- ^ Codec for the given protocol
    -> (m LByteString, LByteString -> m ())
        -- ^ Read/Write from/To the channel
    -> m ()
chainSyncMockPeer seed codec (recv, send) = flip evalStateT seed $ forever $ do
    req <- lift recv
    res <- lift (decodeOrThrow req) >>= \case
        SomeMessage ChainSync.MsgRequestNext -> do
            msg <- generateWith genRequestNextResponse <$> state random
            pure $ encode codec (ServerAgency $ TokNext TokCanAwait) msg
        SomeMessage ChainSync.MsgFindIntersect{} -> do
            msg <- generateWith genFindIntersectResponse <$> state random
            pure $ encode codec (ServerAgency TokIntersect) msg
        SomeMessage ChainSync.MsgDone ->
            error "MsgDone"
    lift $ send res
  where
    decodeOrThrow bytes = do
        decoder <- decode codec (ClientAgency TokIdle)
        runDecoder [bytes] decoder >>= \case
            Left failure -> error (show failure)
            Right msg -> pure msg

    genRequestNextResponse
        :: Gen (ChainSync.Message Protocol ('StNext any) 'StIdle)
    genRequestNextResponse = frequency
        [ (10, ChainSync.MsgRollForward <$> genBlock <*> genTip)
        , ( 1, ChainSync.MsgRollBackward <$> genPoint <*> genTip)
        ]

    genFindIntersectResponse
        :: Gen (ChainSync.Message Protocol 'StIntersect 'StIdle)
    genFindIntersectResponse = frequency
        [ (10, ChainSync.MsgIntersectFound <$> genPoint <*> genTip)
        , ( 1, ChainSync.MsgIntersectNotFound <$> genTip)
        ]
