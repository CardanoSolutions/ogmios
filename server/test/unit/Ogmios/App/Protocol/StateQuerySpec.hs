-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

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
import GHC.TypeLits
    ( KnownSymbol
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
    , encodeAcquireFailure
    , encodePoint
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Json.Query
    ( SomeQuery (..)
    , encodeEpochNo
    , encodeMismatchEraInfo
    )
import Ogmios.Data.Protocol.StateQuery
    ( Acquire (..)
    , Query (..)
    , Release (..)
    , StateQueryMessage (..)
    , mkStateQueryCodecs
    )
import Ouroboros.Consensus.Byron.Ledger.Block
    ( ByronBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    , AlonzoEra
    , BlockQuery (..)
    , CardanoEras
    , MaryEra
    , ShelleyEra
    )
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
    ( EraIndex (..)
    )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryHardFork (..)
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
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
    , expectWSPResponse
    , prop_inIOSim
    , withMockChannel
    )
import Test.Generators
    ( genAcquireFailure
    , genEpochResult
    , genMirror
    , genPoint
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
    ( Gen
    , Property
    , checkCoverage
    , cover
    , elements
    , forAll
    , frequency
    , listOf1
    )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp
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
            forM_ messages $ \(msg, mirror, SomeProxy proxy) -> do
                send msg >> expectWSPResponse proxy receive (toJSON mirror)
      where
        isDirectQuery hasAcquired = \case
            [] -> False
            ((MsgQuery{},_,_):q) -> not hasAcquired || isDirectQuery hasAcquired q
            ((MsgRelease{},_,_):q) -> isDirectQuery False q
            ((MsgAcquire{},_,_):q) -> isDirectQuery True q

        isManyQueries nQuery = \case
            [] -> False
            ((MsgQuery{},_,_):q) -> nQuery >= 3 || isManyQueries (nQuery + 1) q
            ((MsgRelease{},_,_):q) -> isManyQueries 0 q
            ((MsgAcquire{},_,_):q) -> isManyQueries 0 q

        isAcquireThenQuery hasAcquired = \case
            [] -> False
            ((MsgQuery{},_,_):q) -> hasAcquired || isAcquireThenQuery False q
            ((MsgRelease{},_,_):q) -> isAcquireThenQuery False q
            ((MsgAcquire{},_,_):q) -> isAcquireThenQuery True q

        isDoubleRelease hasReleased = \case
            [] -> False
            ((MsgQuery{},_,_):q) -> isDoubleRelease False q
            ((MsgRelease{},_,_):q) -> hasReleased || isDoubleRelease True q
            ((MsgAcquire{},_,_):q) -> isDoubleRelease False q

        isDoubleAcquire hasAcquired = \case
            [] -> False
            ((MsgQuery{},_,_):q) -> isDoubleAcquire False q
            ((MsgRelease{},_,_):q) -> isDoubleAcquire False q
            ((MsgAcquire{},_,_):q) -> hasAcquired || isDoubleAcquire True q

type Protocol = LocalStateQuery Block (Point Block) (Ledger.Query Block)

withStateQueryClient
    :: (MonadSTM m, MonadCatch m, MonadOuroboros m, MonadLog m)
    => ((StateQueryMessage Block -> m ()) ->  m Json -> m a)
    -> StdGen
    -> m a
withStateQueryClient action seed = do
    (recvQ, sendQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
    let innerCodecs = mkStateQueryCodecs encodePoint encodeAcquireFailure
    let client = mkStateQueryClient nullTracer innerCodecs recvQ (atomically . writeTQueue sendQ)
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
        (Right (SomeMessage LSQ.MsgDone)) ->
            pure Nothing
        (Right (SomeMessage LSQ.MsgAcquire{})) -> do
            SomeResponse msg <- generateWith genAcquireResponse <$> state random
            pure $ Just $ encode codec (ServerAgency TokAcquiring) msg
        (Left{}) -> lift (decodeOrThrow TokAcquired req) >>= \case
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
    genAcquireResponse = frequency
        [ (10, pure (SomeResponse LSQ.MsgAcquired))
        , ( 1, SomeResponse . LSQ.MsgFailure <$> genAcquireFailure)
        ]

    genQueryResponse
        :: Ledger.Query Block result
        -> Gen (SomeResponse ('StQuerying result))
    genQueryResponse query = case query of
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
        _ ->
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

data SomeProxy = forall method. KnownSymbol method => SomeProxy (Proxy method)
deriving instance Show SomeProxy

genMessages :: Gen [(StateQueryMessage Block, Wsp.Mirror, SomeProxy)]
genMessages = do
    mirror <- genMirror
    point  <- genPoint
    listOf1 $ elements
        [ (acquire  mirror point, mirror, SomeProxy (Proxy :: Proxy "Acquire"))
        , (release  mirror      , mirror, SomeProxy (Proxy :: Proxy "Release"))
        , (queryAny mirror      , mirror, SomeProxy (Proxy :: Proxy "Query"))
        ]

--
-- Helpers
--

acquire :: Wsp.Mirror -> Point Block -> StateQueryMessage Block
acquire mirror point =
    MsgAcquire Acquire{point} (Wsp.Response mirror) (Wsp.Fault mirror)

release :: Wsp.Mirror -> StateQueryMessage Block
release mirror =
    MsgRelease Release (Wsp.Response mirror) (Wsp.Fault mirror)

queryAny :: Wsp.Mirror -> StateQueryMessage Block
queryAny mirror =
    MsgQuery Query{rawQuery,queryInEra} (Wsp.Response mirror) (Wsp.Fault mirror)
  where
    rawQuery = object [ "query" .= ("currentEpoch" :: String) ]
    queryInEra _ = Just $ SomeQuery
        { query = Ledger.BlockQuery $ QueryIfCurrentAlonzo GetEpochNo
        , genResult = const Proxy
        , encodeResult = const (either encodeMismatchEraInfo encodeEpochNo)
        }
