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
    ( ToJSON (..)
    )
import Network.TypedProtocol.Codec
    ( Codec (..)
    , PeerHasAgency (..)
    , SomeMessage (..)
    , runDecoder
    )
import Ogmios.App.Configuration
    ( EpochSlots (..)
    )
import Ogmios.App.Protocol.ChainSync
    ( MaxInFlight
    , mkChainSyncClient
    )
import Ogmios.Control.Exception
    ( MonadThrow (..)
    )
import Ogmios.Control.MonadAsync
    ( race
    )
import Ogmios.Control.MonadLog
    ( nullTracer
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
import Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , encodeBlock
    , encodePoint
    , encodeTip
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Protocol.ChainSync
    ( ChainSyncMessage (..)
    , FindIntersect (..)
    , RequestNext (..)
    , mkChainSyncCodecs
    )
import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync (..)
    , ClientHasAgency (..)
    , ServerHasAgency (..)
    , TokNextKind (..)
    )
import System.Random
    ( StdGen
    , random
    )
import Test.App.Protocol.Util
    ( FailedToDecodeMsg (..)
    , PeerTerminatedUnexpectedly (..)
    , expectWSPFault
    , expectWSPResponse
    , prop_inIOSim
    , withMockChannel
    )
import Test.Generators
    ( genBlock
    , genMirror
    , genPoint
    , genTip
    , generateWith
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Confidence (..)
    , Gen
    , Property
    , checkCoverageWith
    , choose
    , cover
    , forAll
    , frequency
    , oneof
    )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync

spec :: Spec
spec = parallel $ do
    context "ChainSync" $ do
        parallel $ prop "Basic send/recv" prop_basicSendRecv
        parallel $ prop "Saturate in-flight queue" prop_manyInFlight
        parallel $ prop "Interleave findIntersect with requestNext" prop_interleave
  where
    -- We expect that client can submit request (either 'RequestNext' or
    -- 'FindIntersect') and receive a corresponding response, preseving the
    -- reflection value (a.k.a mirror).
    prop_basicSendRecv :: Property
    prop_basicSendRecv = forAll genMirror $ \mirror ->
        cover 50 (isJust mirror) "with mirror" $
        cover 50 (isNothing mirror) "without mirror" $
        checkCoverageWith (confidence 1e6 0.5) (p mirror)
      where
        p mirror = prop_inIOSim $ withChainSyncClient $ \send receive -> do
            send $ requestNext mirror
            expectWSPResponse @"RequestNext" Proxy receive (toJSON mirror)

            send $ findIntersect mirror []
            expectWSPResponse @"FindIntersect" Proxy receive (toJSON mirror)

    -- The chain-sync client will pipeline requests up to a certain point.
    -- Indeed, WebSockets do allow for (theorically) infinite pipelining,
    -- whereas the Ouroboros framework only allow for finite one. This means
    -- that the application client has to keep track of the pipelined requests,
    -- and choose whether to collect response for previously sent requests, or
    -- to continue pipelining new incoming requests.
    --
    -- There's therefore a different behavior depending on the number of requests
    -- in flight, which this property captures.
    prop_manyInFlight :: Property
    prop_manyInFlight = forAll genMaxInFlight $ \nMax ->
        cover 33 (nMax > maxInFlight) "> maxInFlight" $
        cover 33 (nMax >= maxInFlight - 1 && nMax <= maxInFlight + 1) "=~ maxInFlight" $
        cover 33 (nMax < maxInFlight) "< maxInFlight" $
        checkCoverageWith (confidence 1e6 0.5) (p nMax)
      where
        p nMax = prop_inIOSim $ withChainSyncClient $ \send receive -> do
            mirrors <- forM [0 .. nMax] $ \(i :: Int) -> do
                let mirror = Just $ toJSON i
                mirror <$ send (requestNext mirror)
            forM_ mirrors $ \mirror -> do
                expectWSPResponse @"RequestNext" Proxy receive (toJSON mirror)

        genMaxInFlight :: Gen MaxInFlight
        genMaxInFlight = oneof
            [ choose (0, maxInFlight - 1)
            , choose (maxInFlight - 1, maxInFlight + 1)
            , choose (maxInFlight + 1, 2 * maxInFlight)
            ]

    -- The Ouroboros typed protocol follows a strategy of correct-by-construction
    -- and represents protocol as state machines, fully captured at the
    -- type-level. We can't model this through a WebSocket, and clients may
    -- therefore produce invalid transitions. One of them is for instance,
    -- asking for an intersection while they are still requests in flight that
    -- haven't been collected. In this case, Ogmios returns a client error.
    prop_interleave :: Property
    prop_interleave = forAll ((,) <$> genMirror <*> genMirror) $ \(mirror, mirror') ->
        cover 20 (isJust mirror && isNothing mirror') "+mirror -mirror" $
        cover 20 (isJust mirror && isJust mirror') "+mirror +mirror" $
        cover 20 (isNothing mirror && isNothing mirror') "-mirror -mirror" $
        cover 20 (isNothing mirror && isJust mirror') "-mirror +mirror" $
        checkCoverageWith (confidence 1e6 0.5) (p mirror mirror')
      where
        p mirror mirror' = prop_inIOSim $ withChainSyncClient $ \send receive -> do
            send $ requestNext mirror
            send $ findIntersect mirror' []
            expectWSPFault receive Wsp.FaultClient (toJSON mirror')
            expectWSPResponse @"RequestNext" Proxy receive (toJSON mirror)

type Protocol = ChainSync Block (Point Block) (Tip Block)

maxInFlight :: MaxInFlight
maxInFlight = 3

withChainSyncClient
    :: (MonadSTM m, MonadOuroboros m)
    => ((ChainSyncMessage Block -> m ()) ->  m Json -> m a)
    -> StdGen
    -> m a
withChainSyncClient action seed = do
    (recvQ, sendQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
    let mode = CompactSerialization
    let innerCodecs = mkChainSyncCodecs (encodeBlock mode) encodePoint encodeTip
    let client = mkChainSyncClient maxInFlight innerCodecs recvQ (atomically . writeTQueue sendQ)
    let codec = codecs defaultSlotsPerEpoch nodeToClientV_Latest & cChainSyncCodec
    withMockChannel (chainSyncMockPeer seed codec) $ \channel -> do
        result <- race
            (runPipelinedPeer nullTracer codec channel (chainSyncClientPeerPipelined client))
            (action (atomically . writeTQueue recvQ) (atomically $ readTQueue sendQ))
        case result of
            Left{}  -> throwIO PeerTerminatedUnexpectedly
            Right a -> pure a
  where
    defaultSlotsPerEpoch = EpochSlots 432000

chainSyncMockPeer
    :: forall m failure. (MonadSTM m, MonadThrow m, Show failure)
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
            pure $ Just $ encode codec (ServerAgency $ TokNext TokCanAwait) msg
        SomeMessage ChainSync.MsgFindIntersect{} -> do
            msg <- generateWith genFindIntersectResponse <$> state random
            pure $ Just $ encode codec (ServerAgency TokIntersect) msg
        SomeMessage ChainSync.MsgDone ->
            pure Nothing
    lift $ maybe (pure ()) send res
  where
    decodeOrThrow bytes = do
        decoder <- decode codec (ClientAgency TokIdle)
        runDecoder [bytes] decoder >>= \case
            Left failure -> throwIO $ FailedToDecodeMsg (show failure)
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

--
-- Helpers
--

requestNext :: Wsp.Mirror -> ChainSyncMessage Block
requestNext mirror =
    MsgRequestNext RequestNext (Wsp.Response mirror) (Wsp.Fault mirror)

findIntersect :: Wsp.Mirror -> [Point Block] -> ChainSyncMessage Block
findIntersect mirror points =
    MsgFindIntersect (FindIntersect points) (Wsp.Response mirror) (Wsp.Fault mirror)

confidence :: Double -> Double -> Confidence
confidence (round -> certainty) tolerance =
    Confidence{certainty,tolerance}
