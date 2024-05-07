-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Ogmios.App.Protocol.StateQuerySpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , cStateQueryCodec
    , codecs
    , localStateQueryClientPeer
    , nodeToClientV_Latest
    , runPeer
    )
import Data.Aeson
    ( ToJSON (..)
    , object
    , (.=)
    )
import Data.SOP.Strict
    ( NS (..)
    )
import Generics.SOP
    ( K (..)
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
import Ogmios.App.Protocol
    ( defaultWithInternalError
    )
import Ogmios.App.Protocol.StateQuery
    ( mkStateQueryClient
    )
import Ogmios.Control.Exception
    ( MonadCatch (..)
    , MonadThrow (..)
    )
import Ogmios.Control.MonadAsync
    ( race
    )
import Ogmios.Control.MonadLog
    ( MonadLog
    , nullTracer
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
    , encodeAcquireExpired
    , encodeAcquireFailure
    , encodePoint
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Json.Prelude
    ( at
    )
import Ogmios.Data.Json.Query
    ( Query (..)
    , SomeQuery (..)
    , encodeEpochNo
    , encodeMismatchEraInfo
    )
import Ogmios.Data.Protocol.StateQuery
    ( AcquireLedgerState (..)
    , ReleaseLedgerState (..)
    , StateQueryMessage (..)
    , mkStateQueryCodecs
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryHardFork (..)
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock
    )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..)
    )
import Ouroboros.Network.Block
    ( Point (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( ClientHasAgency (..)
    , LocalStateQuery (..)
    , ServerHasAgency (..)
    )
import System.Random
    ( StdGen
    , random
    )
import Test.App.Protocol.Util
    ( FailedToDecodeMsg (..)
    , PeerTerminatedUnexpectedly (..)
    , ResponsePredicate (..)
    , expectRpcResponse
    , prop_inIOSim
    , withMockChannel
    )
import Test.Generators
    ( genAcquireFailure
    , genEpochResult
    , genMirror
    , genPoint
    , generateWith
    , reasonablySized
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
    ( Gen
    , Property
    , checkCoverage
    , cover
    , elements
    , forAll
    , frequency
    , listOf1
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Ogmios.Data.Protocol.StateQuery as StateQuery
import qualified Ouroboros.Consensus.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ

spec :: Spec
spec = parallel $ do
    context "StateQuery" $ do
        parallel $ prop "Random sequence of messages" prop_anyRandomSequence
  where
    -- There isn't much which can go wrong with the local-state query. So, this
    -- property simply hammer it down by trying some random sequences of
    -- messages (Acquire, Query, Release), in any order, making sure that Ogmios
    -- replies with a corresponding response and mirror.
    --
    -- The property also checks for various cases of particular interest, to
    -- make sure they are covered in the generated sequence.
    prop_anyRandomSequence :: Property
    prop_anyRandomSequence = forAll genMessages $ \messages ->
        cover 5 (isAcquireThenQuery False messages) "Acquire then query" $
        cover 5 (isDirectQuery False messages) "Direct query" $
        cover 5 (isManyQueries (0 :: Int) messages) "Many queries" $
        cover 5 (isDoubleRelease False messages) "Double release" $
        cover 5 (isDoubleAcquire False messages) "Double acquire" $
        checkCoverage $ prop_inIOSim $ withStateQueryClient $ \send receive -> do
            forM_ messages $ \(msg, mirror, predicate) -> do
                send msg >> expectRpcResponse predicate receive (toJSON mirror)
      where
        isDirectQuery hasAcquired = \case
            [] -> False
            ((MsgQueryLedgerState{},_,_):q) -> not hasAcquired || isDirectQuery hasAcquired q
            ((MsgReleaseLedgerState{},_,_):q) -> isDirectQuery False q
            ((MsgAcquireLedgerState{},_,_):q) -> isDirectQuery True q

        isManyQueries nQuery = \case
            [] -> False
            ((MsgQueryLedgerState{},_,_):q) -> nQuery >= 3 || isManyQueries (nQuery + 1) q
            ((MsgReleaseLedgerState{},_,_):q) -> isManyQueries 0 q
            ((MsgAcquireLedgerState{},_,_):q) -> isManyQueries 0 q

        isAcquireThenQuery hasAcquired = \case
            [] -> False
            ((MsgQueryLedgerState{},_,_):q) -> hasAcquired || isAcquireThenQuery False q
            ((MsgReleaseLedgerState{},_,_):q) -> isAcquireThenQuery False q
            ((MsgAcquireLedgerState{},_,_):q) -> isAcquireThenQuery True q

        isDoubleRelease hasReleased = \case
            [] -> False
            ((MsgQueryLedgerState{},_,_):q) -> isDoubleRelease False q
            ((MsgReleaseLedgerState{},_,_):q) -> hasReleased || isDoubleRelease True q
            ((MsgAcquireLedgerState{},_,_):q) -> isDoubleRelease False q

        isDoubleAcquire hasAcquired = \case
            [] -> False
            ((MsgQueryLedgerState{},_,_):q) -> isDoubleAcquire False q
            ((MsgReleaseLedgerState{},_,_):q) -> isDoubleAcquire False q
            ((MsgAcquireLedgerState{},_,_):q) -> hasAcquired || isDoubleAcquire True q

type Protocol = LocalStateQuery Block (Point Block) (Ledger.Query Block)

withStateQueryClient
    :: (MonadCatch m, MonadOuroboros m, MonadLog m)
    => ((StateQueryMessage Block -> m ()) ->  m Json -> m a)
    -> StdGen
    -> m a
withStateQueryClient action seed = do
    (recvQ, sendQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
    let innerCodecs = mkStateQueryCodecs Rpc.defaultOptions encodePoint encodeAcquireFailure encodeAcquireExpired
    let getGenesisConfig = let nope = error "unimplemented" in StateQuery.GetGenesisConfig nope nope nope nope
    let catchError = defaultWithInternalError (const $ return ()) Rpc.defaultOptions
    let client = mkStateQueryClient nullTracer catchError innerCodecs getGenesisConfig recvQ (atomically . writeTQueue sendQ)
    let codec = codecs defaultSlotsPerEpoch nodeToClientV_Latest & cStateQueryCodec
    withMockChannel (stateQueryMockPeer seed codec) $ \channel -> do
        result <- race
            (runPeer nullTracer codec channel (localStateQueryClientPeer client))
            (action (atomically . writeTQueue recvQ) (atomically $ readTQueue sendQ))
        case result of
            Left{}  -> throwIO PeerTerminatedUnexpectedly
            Right a -> pure a
  where
    defaultSlotsPerEpoch = EpochSlots 432000

data SomeResponse from =
    forall to. SomeResponse (LSQ.Message Protocol from to)

stateQueryMockPeer
    :: forall m failure. (MonadSTM m, MonadCatch m, Show failure)
    => StdGen
        -- ^ Random generator
    -> Codec Protocol failure m LByteString
        -- ^ Codec for the given protocol
    -> (m LByteString, LByteString -> m ())
        -- ^ Read/Write from/To the channel
    -> m ()
stateQueryMockPeer seed codec (recv, send) = flip evalStateT seed $ forever $ do
    req <- lift recv

    msg <- lift (try @_ @SomeException (decodeOrThrow TokIdle req)) >>= \case
        Right (SomeMessage LSQ.MsgDone) ->
            pure Nothing
        Right (SomeMessage LSQ.MsgAcquire{}) -> do
            SomeResponse msg <- generateWith genAcquireResponse <$> state random
            pure $ Just $ encode codec (ServerAgency TokAcquiring) msg
        Left{} -> lift (decodeOrThrow TokAcquired req) >>= \case
            SomeMessage (LSQ.MsgQuery query) -> do
                SomeResponse msg <- generateWith (genQueryResponse query) <$> state random
                pure $ Just $ encode codec (ServerAgency $ TokQuerying query) msg
            SomeMessage LSQ.MsgReAcquire{} -> do
                SomeResponse msg <- generateWith genAcquireResponse <$> state random
                pure $ Just $ encode codec (ServerAgency TokAcquiring) msg
            SomeMessage LSQ.MsgRelease{} -> do
                pure Nothing

    lift $ maybe (pure ()) send msg
  where
    decodeOrThrow :: forall (st :: Protocol). ClientHasAgency st -> LByteString -> m (SomeMessage st)
    decodeOrThrow agency bytes = do
        decoder <- decode codec (ClientAgency agency)
        runDecoder [bytes] decoder >>= \case
            Left failure -> throwIO $ FailedToDecodeMsg (show failure)
            Right msg -> pure msg

    genAcquireResponse
        :: Gen (SomeResponse 'StAcquiring)
    genAcquireResponse = reasonablySized $ frequency
        [ (10, pure (SomeResponse LSQ.MsgAcquired))
        , ( 1, SomeResponse . LSQ.MsgFailure <$> genAcquireFailure)
        ]

    genQueryResponse
        :: Ledger.Query Block result
        -> Gen (SomeResponse ('StQuerying result))
    genQueryResponse query = reasonablySized $ case query of
        Ledger.BlockQuery (QueryIfCurrentShelley GetEpochNo) ->
            SomeResponse . LSQ.MsgResult query <$> genEpochResult Proxy
        Ledger.BlockQuery (QueryIfCurrentAllegra GetEpochNo) ->
            SomeResponse . LSQ.MsgResult query <$> genEpochResult Proxy
        Ledger.BlockQuery (QueryIfCurrentMary GetEpochNo) ->
            SomeResponse . LSQ.MsgResult query <$> genEpochResult Proxy
        Ledger.BlockQuery (QueryIfCurrentAlonzo GetEpochNo) ->
            SomeResponse . LSQ.MsgResult query <$> genEpochResult Proxy
        Ledger.BlockQuery (QueryHardFork GetCurrentEra) -> elements
            [ SomeResponse $ LSQ.MsgResult query (EraIndex (IxByron   (K ())))
            , SomeResponse $ LSQ.MsgResult query (EraIndex (IxShelley (K ())))
            , SomeResponse $ LSQ.MsgResult query (EraIndex (IxAllegra (K ())))
            , SomeResponse $ LSQ.MsgResult query (EraIndex (IxMary    (K ())))
            , SomeResponse $ LSQ.MsgResult query (EraIndex (IxAlonzo  (K ())))
            ]
        _unsupportedQuery ->
            error $ "No generator for query: " <> show query

--
-- Constructing EraIndex, shameless copied from: Ouroboros.Consensus.Cardano.Block
--

pattern IxByron   :: f ByronBlock                               -> NS f (CardanoEras c)
pattern IxShelley :: f (ShelleyBlock (TPraos c) (ShelleyEra c)) -> NS f (CardanoEras c)
pattern IxAllegra :: f (ShelleyBlock (TPraos c) (AllegraEra c)) -> NS f (CardanoEras c)
pattern IxMary    :: f (ShelleyBlock (TPraos c) (MaryEra    c)) -> NS f (CardanoEras c)
pattern IxAlonzo  :: f (ShelleyBlock (TPraos c) (AlonzoEra  c)) -> NS f (CardanoEras c)

pattern IxByron   x =             Z x
pattern IxShelley x =          S (Z x)
pattern IxAllegra x =       S (S (Z x))
pattern IxMary    x =    S (S (S (Z x)))
pattern IxAlonzo  x = S (S (S (S (Z x))))

--
-- Command Generator
--

genMessages :: Gen [(StateQueryMessage Block, Rpc.Mirror, ResponsePredicate)]
genMessages = do
    mirror <- genMirror
    point  <- genPoint
    listOf1 $ elements
        [ (acquire  mirror point, mirror, isAcquireLedgerStateResponse)
        , (release  mirror      , mirror, isReleaseLedgerStateResponse)
        , (queryAny mirror      , mirror, isQueryLedgerStateResponse)
        ]

--
-- Helpers
--

acquire :: Rpc.Mirror -> Point Block -> StateQueryMessage Block
acquire mirror point =
    MsgAcquireLedgerState AcquireLedgerState{point} (Rpc.Response method mirror)
  where
    method = Just "acquireLedgerState"

isAcquireLedgerStateResponse :: ResponsePredicate
isAcquireLedgerStateResponse = ResponsePredicate $
    \v -> ("method" `at` v) == Just (toJSON @Text "acquireLedgerState")

release :: Rpc.Mirror -> StateQueryMessage Block
release mirror =
    MsgReleaseLedgerState ReleaseLedgerState (Rpc.Response method mirror)
  where
    method = Just "releaseLedgerState"

isReleaseLedgerStateResponse :: ResponsePredicate
isReleaseLedgerStateResponse = ResponsePredicate $
    \v -> ("method" `at` v) == Just (toJSON @Text "releaseLedgerState")

queryAny :: Rpc.Mirror -> StateQueryMessage Block
queryAny mirror =
    MsgQueryLedgerState Query{rawQuery,queryInEra} (Rpc.Response method mirror)
  where
    method = Just "query/*"
    rawQuery = object [ "query" .= ("currentEpoch" :: String) ]
    queryInEra _ = Just $ SomeStandardQuery
        (Ledger.BlockQuery $ QueryIfCurrentAlonzo GetEpochNo)
        (bimap encodeMismatchEraInfo encodeEpochNo)
        (const Proxy)

isQueryLedgerStateResponse :: ResponsePredicate
isQueryLedgerStateResponse = ResponsePredicate $
    \v -> not (_isAcquireLedgerStateResponse v) && not (_isReleaseLedgerStateResponse v)
  where
    ResponsePredicate _isAcquireLedgerStateResponse = isAcquireLedgerStateResponse
    ResponsePredicate _isReleaseLedgerStateResponse = isReleaseLedgerStateResponse
