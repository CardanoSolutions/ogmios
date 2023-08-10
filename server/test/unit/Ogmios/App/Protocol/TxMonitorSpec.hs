-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

module Ogmios.App.Protocol.TxMonitorSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , cTxMonitorCodec
    , codecs
    , localTxMonitorClientPeer
    , nodeToClientV_Latest
    , runPeer
    )
import Data.Aeson
    ( ToJSON (..)
    )
import Data.List
    ( delete
    , (!!)
    )
import Network.TypedProtocol.Codec
    ( Codec (..)
    , PeerHasAgency (..)
    , SomeMessage (..)
    , runDecoder
    )
import Ogmios.App.Configuration
    ( EpochSlots (..)
    , omitOptionalCbor
    )
import Ogmios.App.Protocol.TxMonitor
    ( mkTxMonitorClient
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
    , encodeTx
    , encodeTxId
    )
import Ogmios.Data.Json.Orphans
    ()
import Ogmios.Data.Json.Prelude
    ( at
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AcquireMempool (..)
    , GenTx
    , GenTxId
    , HasTransaction (..)
    , NextTransaction (..)
    , ReleaseMempool (..)
    , SizeOfMempool (..)
    , SlotNo (..)
    , TxMonitorMessage (..)
    , mkTxMonitorCodecs
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
    ( ClientHasAgency (..)
    , LocalTxMonitor (..)
    , ServerHasAgency (..)
    , TokBusyKind (..)
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
    ( genMempoolSizeAndCapacity
    , genMirror
    , genTx
    , genTxId
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
    ( Confidence (..)
    , Gen
    , Property
    , checkCoverageWith
    , choose
    , cover
    , elements
    , forAllShow
    , forAllShrinkShow
    , frequency
    , listOf1
    , shrinkList
    , suchThat
    , vectorOf
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Data.Aeson as Json
import qualified Ouroboros.Network.Protocol.LocalTxMonitor.Type as TxMonitor

spec :: Spec
spec = parallel $ do
    context "TxMonitor" $ do
        parallel $ prop "Any sequence acquire/has/next/size is valid" prop_anySequenceValid
        parallel $ prop "Can and must query after acquiring" prop_queryAfterAcquiring
  where
    prop_anySequenceValid :: Property
    prop_anySequenceValid =
        checkCoverageWith (confidence 1e6 0.5) $ forAllShrinkShow
            (reasonablySized (listOf1 (withoutReleaseMempool genTxMonitorAcquiredMessage)))
            (shrinkList (const []))
            (show . fmap showTxMonitorMessage)
            (\msgs -> p msgs
                & cover 1 (acquireTwice False msgs) "AcquireMempool twice"
                & cover 1 (multipleNextTransaction (0 :: Int) 0 msgs) "Multiple NextTransaction"
                & cover 1 (nextThenHas False msgs) "NextTransaction then HasTransaction"
            )
      where
        p msgs = prop_inIOSim $ withTxMonitorClient $ \send receive -> do
            send $ acquireMempool Nothing
            expectRpcResponse isAcquireMempoolResponse receive Json.Null
            forM_ msgs  $ \(msg, mirror, predicate) -> do
                send msg
                expectRpcResponse predicate receive (toJSON mirror)
            send $ releaseMempool Nothing
            expectRpcResponse isReleaseMempoolResponse receive Json.Null

        multipleNextTransaction nNextTransaction nNextTransactionMax = \case
            [] -> max nNextTransaction nNextTransactionMax > 5
            ((MsgNextTransaction{},_,_):q) -> multipleNextTransaction (nNextTransaction + 1) nNextTransactionMax q
            ((MsgAcquireMempool{},_,_):q) -> multipleNextTransaction 0 (max nNextTransaction nNextTransactionMax) q
            _:q -> multipleNextTransaction nNextTransaction nNextTransactionMax q

        nextThenHas hasNext = \case
            [] -> False
            ((MsgNextTransaction{},_,_):q) -> nextThenHas True q
            ((MsgHasTransaction{},_,_):q) -> hasNext || nextThenHas False q
            _:q -> nextThenHas False q

        acquireTwice hasAcquired = \case
            [] -> False
            ((MsgAcquireMempool{},_,_):q) -> hasAcquired || acquireTwice True q
            _:q -> acquireTwice False q

    prop_queryAfterAcquiring :: Property
    prop_queryAfterAcquiring =
        checkCoverageWith (confidence 1e6 0.5) $ forAllShow
            (withoutAcquireMempool genTxMonitorAcquiredMessage)
            showTxMonitorMessage
            (\arg -> p arg
                & cover 1 ("ReleaseMempool" == showTxMonitorMessage arg) "ReleaseMempool"
                & cover 1 ("HasTransaction" == showTxMonitorMessage arg) "HasTransaction"
                & cover 1 ("NextTransaction" == showTxMonitorMessage arg) "NextTransaction"
                & cover 1 ("SizeOfMempool" == showTxMonitorMessage arg) "SizeOfMempool"
            )
      where
        p (msg, mirror, _) = prop_inIOSim $ withTxMonitorClient $ \send receive -> do
            send msg
            expectRpcResponse isMustAcquireFirst receive (toJSON mirror)

type Protocol = LocalTxMonitor (GenTxId Block) (GenTx Block) SlotNo

withTxMonitorClient
    :: (MonadSTM m, MonadOuroboros m)
    => ((TxMonitorMessage Block -> m ()) ->  m Json -> m a)
    -> StdGen
    -> m a
withTxMonitorClient action seed = do
    (recvQ, sendQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
    let innerCodecs = mkTxMonitorCodecs encodeTxId (encodeTx omitOptionalCbor)
    let client = mkTxMonitorClient innerCodecs recvQ (atomically . writeTQueue sendQ)
    let codec = codecs defaultSlotsPerEpoch nodeToClientV_Latest & cTxMonitorCodec
    withMockChannel (txMonitorMockPeer seed codec) $ \channel -> do
        result <- race
            (runPeer nullTracer codec channel (localTxMonitorClientPeer client))
            (action (atomically . writeTQueue recvQ) (atomically $ readTQueue sendQ))
        case result of
            Left{}  -> throwIO PeerTerminatedUnexpectedly
            Right a -> pure a
  where
    defaultSlotsPerEpoch = EpochSlots 432000

txMonitorMockPeer
    :: forall m failure. (MonadSTM m, MonadThrow m, Show failure)
    => StdGen
        -- ^ Random generator
    -> Codec Protocol failure m LByteString
        -- ^ Codec for the given protocol
    -> (m LByteString, LByteString -> m ())
        -- ^ Read/Write from/To the channel
    -> m ()
txMonitorMockPeer seed codec (recv, send) = flip evalStateT (seed, emptyServerState) $ forever $ do
    doSomething
    ServerState{mempool,snapshot,cursor, slotNo} <- gets snd
    req <- lift recv
    res <- lift (decodeOrThrow (isJust snapshot) req) >>= \case
        Left (SomeMessage TxMonitor.MsgAcquire) -> do
            case snapshot of
                Just{} ->
                    error "Sent acquire twice!"
                Nothing -> do
                    modifyRight (\st -> st { snapshot = Just mempool, cursor = 0 })
                    let msg = TxMonitor.MsgAcquired slotNo
                    pure $ Just $ encode codec (ServerAgency TokAcquiring) msg
        Right (SomeMessage TxMonitor.MsgAwaitAcquire) -> do
            slotNo' <- waitForNewSnapshot
            let msg = TxMonitor.MsgAcquired slotNo'
            pure $ Just $ encode codec (ServerAgency TokAcquiring) msg
        Right (SomeMessage TxMonitor.MsgNextTx) -> do
            case snapshot of
                Nothing ->
                    error "NextTransaction before acquiring."
                Just xs | length xs > cursor -> do
                    modifyRight (\st -> st { cursor = succ cursor })
                    let msg = TxMonitor.MsgReplyNextTx (Just (xs !! cursor))
                    pure $ Just $ encode codec (ServerAgency (TokBusy TokNextTx)) msg
                Just _ -> do
                    let msg = TxMonitor.MsgReplyNextTx Nothing
                    pure $ Just $ encode codec (ServerAgency (TokBusy TokNextTx)) msg
        Right (SomeMessage (TxMonitor.MsgHasTx x)) -> do
            case snapshot of
                Nothing ->
                    error "HasTransaction before acquiring."
                Just (fmap txId -> xs) -> do
                    let msg = TxMonitor.MsgReplyHasTx (x `elem` xs)
                    pure $ Just $ encode codec (ServerAgency (TokBusy TokHasTx)) msg
        Right (SomeMessage TxMonitor.MsgGetSizes) -> do
            case snapshot of
                Nothing ->
                    error "GetSizes before acquiring."
                Just{} -> do
                    sizes <- generateWith genMempoolSizeAndCapacity <$> stateLeft random
                    let msg = TxMonitor.MsgReplyGetSizes sizes
                    pure $ Just $ encode codec (ServerAgency (TokBusy TokGetSizes)) msg
        Right (SomeMessage TxMonitor.MsgRelease) -> do
            case snapshot of
                Nothing ->
                    error "Release before acquiring."
                Just{} -> do
                    modifyRight (\st -> st { snapshot = Nothing })
                    pure Nothing
        Left (SomeMessage TxMonitor.MsgDone) ->
            pure Nothing
    lift $ maybe (pure ()) send res
  where
    decodeOrThrow acquired bytes
        | acquired = do
            decoder <- decode codec (ClientAgency TokAcquired)
            runDecoder [bytes] decoder >>= \case
                Left failure -> throwIO $ FailedToDecodeMsg (show failure)
                Right msg -> pure (Right msg)
        | otherwise = do
            decoder <- decode codec (ClientAgency TokIdle)
            runDecoder [bytes] decoder >>= \case
                Left failure -> throwIO $ FailedToDecodeMsg (show failure)
                Right msg -> pure (Left msg)

data ServerState = ServerState
    { mempool :: [GenTx Block]
    , snapshot :: Maybe [GenTx Block]
    , cursor :: Int
    , slotNo :: SlotNo
    } deriving (Show)

emptyServerState :: ServerState
emptyServerState = ServerState mempty Nothing 0 (SlotNo 0)

data ServerAction
    = ServerDoesNothing
    | ServerRemoveTx
    | ServerAddTx
    deriving (Show)

doSomething :: Monad m => StateT (StdGen, ServerState) m ()
doSomething = do
    n <- generateWith (choose (1, 4)) <$> stateLeft random
    replicateM_ n runServer

waitForNewSnapshot :: Monad m => StateT (StdGen, ServerState) m SlotNo
waitForNewSnapshot = do
    ServerState{mempool,snapshot,slotNo} <- gets snd
    case snapshot of
        Nothing -> do
            error "Await acquire on empty snapshot?"
        Just mempool' | mempool' == mempool ->
            doSomething *> waitForNewSnapshot
        Just mempool' ->
            slotNo <$ modifyRight (\st -> st { snapshot = Just mempool' })

runServer :: Monad m => StateT (StdGen, ServerState) m ()
runServer = do
    ServerState{mempool} <- gets snd
    action <- generateWith (genServerAction mempool) <$> stateLeft random
    case action of
        ServerDoesNothing ->
            modifyRight $ \st -> st
                { slotNo = succ (slotNo st)
                }
        ServerAddTx -> do
            tx <- generateWith (elements plausibleTxs) <$> stateLeft random
            modifyRight $ \st -> st
                { mempool = tx : mempool
                , slotNo = succ (slotNo st)
                }
        ServerRemoveTx -> do
            tx <- generateWith (elements mempool)  <$> stateLeft random
            modifyRight $ \st -> st
                { mempool = delete tx mempool
                , slotNo = succ (slotNo st)
                }

--
-- Generators
--

maxCapacity :: Int
maxCapacity = 10

plausibleTxs :: [GenTx Block]
plausibleTxs = generateWith (vectorOf (2 * maxCapacity) genTx) 42

plausibleTxsIds :: [GenTxId Block]
plausibleTxsIds = txId <$> plausibleTxs

genServerAction :: [tx] -> Gen ServerAction
genServerAction xs = frequency $ mconcat
    [ [ (75, pure ServerDoesNothing ) ]
    , [ (15, pure ServerAddTx ) | length xs < maxCapacity ]
    , [ (10, pure ServerRemoveTx ) | not (null xs) ]
    ]

genTxMonitorAcquiredMessage :: Gen (TxMonitorMessage Block, Rpc.Mirror, ResponsePredicate)
genTxMonitorAcquiredMessage = do
    mirror <- genMirror
    plausible <- elements plausibleTxsIds
    unknown <- genTxId `suchThat` (`notElem` plausibleTxsIds)
    frequency
        [ (20, pure (acquireMempool mirror     , mirror, isAcquireMempoolResponse))
        , (75, pure (nextTx mirror           , mirror, isNextTxResponse))
        , (10, pure (hasTx mirror plausible  , mirror, isHasTxResponse))
        , (10, pure (hasTx mirror unknown    , mirror, isHasTxResponse))
        , ( 5, pure (sizeOfMempool mirror  , mirror, isSizeOfMempoolResponse))
        , ( 5, pure (releaseMempool mirror   , mirror, isReleaseMempoolResponse))
        ]

withoutReleaseMempool
    :: Gen (TxMonitorMessage Block, b0, c0)
    -> Gen (TxMonitorMessage Block, b0, c0)
withoutReleaseMempool = flip suchThat $ \case
    (MsgReleaseMempool{}, _, _) -> False
    _ -> True

withoutAcquireMempool
    :: Gen (TxMonitorMessage Block, b0, c0)
    -> Gen (TxMonitorMessage Block, b0, c0)
withoutAcquireMempool = flip suchThat $ \case
    (MsgAcquireMempool{}, _, _) -> False
    _ -> True

--
-- Messages
--

acquireMempool :: Rpc.Mirror -> TxMonitorMessage Block
acquireMempool mirror =
    MsgAcquireMempool AcquireMempool (Rpc.Response method mirror)
  where
    method = Just "acquireMempool"

isAcquireMempoolResponse :: ResponsePredicate
isAcquireMempoolResponse = ResponsePredicate $
    \v -> ("method" `at` v)  == Just (toJSON @Text "acquireMempool")

nextTx :: Rpc.Mirror -> TxMonitorMessage Block
nextTx mirror =
    MsgNextTransaction (NextTransaction Nothing) (Rpc.Response method mirror)
  where
    method = Just "nextTransaction"

isNextTxResponse :: ResponsePredicate
isNextTxResponse = ResponsePredicate $
    \v -> ("method" `at` v)  == Just (toJSON @Text "nextTransaction")

hasTx :: Rpc.Mirror -> GenTxId Block -> TxMonitorMessage Block
hasTx mirror tx =
    MsgHasTransaction (HasTransaction tx) (Rpc.Response method mirror)
  where
    method = Just "hasTransaction"

isHasTxResponse :: ResponsePredicate
isHasTxResponse = ResponsePredicate $
    \v -> ("method" `at` v)  == Just (toJSON @Text "hasTransaction")

sizeOfMempool :: Rpc.Mirror -> TxMonitorMessage Block
sizeOfMempool mirror =
    MsgSizeOfMempool SizeOfMempool (Rpc.Response method mirror)
  where
    method = Just "sizeOfMempool"

isSizeOfMempoolResponse :: ResponsePredicate
isSizeOfMempoolResponse = ResponsePredicate $
    \v -> ("method" `at` v)  == Just (toJSON @Text "sizeOfMempool")

releaseMempool :: Rpc.Mirror -> TxMonitorMessage Block
releaseMempool mirror =
    MsgReleaseMempool ReleaseMempool (Rpc.Response method mirror)
  where
    method = Just "releaseMempool"

isReleaseMempoolResponse :: ResponsePredicate
isReleaseMempoolResponse = ResponsePredicate $
    \v -> ("method" `at` v)  == Just (toJSON @Text "releaseMempool")

isMustAcquireFirst :: ResponsePredicate
isMustAcquireFirst = ResponsePredicate $
    \v -> isJust $ do
        e <- "error" `at` v
        code <- "code" `at` e
        guard (code == toJSON @Int 4000)

--
-- Helpers
--

showTxMonitorMessage :: (TxMonitorMessage Block, Rpc.Mirror, ResponsePredicate) -> String
showTxMonitorMessage (msg, _, _) =
    case msg of
        MsgAcquireMempool{} -> "AcquireMempool"
        MsgNextTransaction{} -> "NextTransaction"
        MsgHasTransaction{} -> "HasTransaction"
        MsgSizeOfMempool{} -> "SizeOfMempool"
        MsgReleaseMempool{} -> "ReleaseMempool"

stateLeft :: Monad m => (l -> (a, l)) -> StateT (l, r) m a
stateLeft fn = state (\(l, r) -> second (,r) (fn l))

modifyRight :: Monad m => (r -> r) -> StateT (l, r) m ()
modifyRight = modify . second

confidence :: Double -> Double -> Confidence
confidence (round -> certainty) tolerance =
    Confidence{certainty,tolerance}
