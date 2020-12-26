--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Bridge
    (
    -- * Clients
      newClients
    , handleIOException

    -- * Internals
    , FindIntersect (..)
    , FindIntersectResponse (..)
    , RequestNext (..)
    , RequestNextResponse (..)

    , SubmitTx (..)
    , SubmitTxResponse (..)

    , Acquire (..)
    , AcquireResponse (..)
    , Release (..)
    , Query (..)
    , SomeQuery (..)
    , QueryResponse (..)
    ) where

import Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Clients (..) )
import Control.Concurrent.Async
    ( async, link )
import Control.Concurrent.Queue
    ( Queue (..), newQueue )
import Control.Exception
    ( IOException )
import Control.Manufacture
    ( Worker (..), bindWorker, feedWorker, newWorker )
import Control.Monad
    ( guard, void )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), (.=) )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.HashMap.Strict
    ( (!?) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Void
    ( Void, absurd )
import GHC.Generics
    ( Generic, Rep )
import Network.TypedProtocol.Pipelined
    ( Nat (..), natToInt )
import Ogmios.Json
    ( SomeQuery (..) )
import Ogmios.Trace
    ( TraceOgmios (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxClientStIdle (..), LocalTxSubmissionClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

--  _____ _           _         _____
-- /  __ \ |         (_)       /  ___|
-- | /  \/ |__   __ _ _ _ __   \ `--. _   _ _ __   ___
-- | |   | '_ \ / _` | | '_ \   `--. \ | | | '_ \ / __|
-- | \__/\ | | | (_| | | | | | /\__/ / |_| | | | | (__
--  \____/_| |_|\__,_|_|_| |_| \____/ \__, |_| |_|\___|
--                                     __/ |
--                                    |___/

data FindIntersect block
    = FindIntersect { points :: [Point block] }
    deriving (Generic, Show)

data FindIntersectResponse block
    = IntersectionFound { point :: Point block, tip :: Tip block }
    | IntersectionNotFound { tip :: Tip block }
    deriving (Generic, Show)

data RequestNext
    = RequestNext
    deriving (Generic, Show)

data RequestNextResponse block
    = RollForward { block :: block, tip :: Tip block }
    | RollBackward { point :: Point block, tip :: Tip block }
    deriving (Generic, Show)

mkChainSyncClient
    :: forall m block.
        ( FromJSON (Point block)
        , ToJSON block
        , ToJSON (Point block)
        , ToJSON (Tip block)
        , Monad m
        )
    => Queue (RequestNextResponse block -> Wsp.Response (RequestNextResponse block)) m
    -> Worker ByteString Json.Encoding m
    -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkChainSyncClient Queue{push,pop,tryPop} Worker{await,tryAwait,yield,pass} =
    ChainSyncClientPipelined (clientStIdle Zero)
  where
    clientStIdle
        :: forall n. Nat n
        -> m (ClientPipelinedStIdle n block (Point block) (Tip block) m ())
    clientStIdle Zero = await >>= Wsp.handle
        (\bytes -> pass bytes *> clientStIdle Zero)
        [ Wsp.Handler $ \FindIntersect{points} ->
            pure . SendMsgFindIntersect points . clientStIntersect

        , Wsp.Handler $ \RequestNext toResponse -> do
            let collect = CollectResponse
                    (Just $ clientStIdle (Succ Zero))
                    (clientStNext Zero pop)
            push toResponse $> SendMsgRequestNextPipelined collect
        ]
    clientStIdle n@(Succ prev) = tryAwait >>= \case
        -- If there's no immediate incoming message, we take this opportunity to
        -- wait and collect one response.
        Nothing -> tryPop >>= \case
            Nothing ->
                clientStIdle n
            Just toResponse ->
                pure $ CollectResponse Nothing (clientStNext prev $ pure toResponse)

        -- Yet, if we have already received a new message from the client, we
        -- prioritize it and pipeline it right away unless there are already too
        -- many requests in flights.
        Just msg -> Wsp.handle
            (\bytes -> pass bytes *> clientStIdle n)
            [ Wsp.Handler $ \RequestNext toResponse -> do
                let collect = CollectResponse
                        (guard (natToInt n < 1000) $> clientStIdle (Succ n))
                        (clientStNext n pop)
                push toResponse $> SendMsgRequestNextPipelined collect
            ] msg

    clientStIntersect
        :: (FindIntersectResponse block -> Wsp.Response (FindIntersectResponse block))
        -> ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    clientStIntersect toResponse = ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point tip -> do
            yield $ Json.toEncoding $ toResponse $ IntersectionFound point tip
            clientStIdle Zero
        , recvMsgIntersectNotFound = \tip -> do
            yield $ Json.toEncoding $ toResponse $ IntersectionNotFound tip
            clientStIdle Zero
        }

    clientStNext
        :: Nat n
        -> m (RequestNextResponse block -> Wsp.Response (RequestNextResponse block))
        -> ClientStNext n block (Point block) (Tip block) m ()
    clientStNext n pop' = ClientStNext
        { recvMsgRollForward = \block tip -> do
            toResponse <- pop'
            yield $ Json.toEncoding $ toResponse $ RollForward block tip
            clientStIdle n
        , recvMsgRollBackward = \point tip -> do
            toResponse <- pop'
            yield $ Json.toEncoding $ toResponse $ RollBackward point tip
            clientStIdle n
        }

--  _____      _____       _               _         _
-- |_   _|    /  ___|     | |             (_)       (_)
--   | |_  __ \ `--. _   _| |__  _ __ ___  _ ___ ___ _  ___  _ __
--   | \ \/ /  `--. \ | | | '_ \| '_ ` _ \| / __/ __| |/ _ \| '_ \
--   | |>  <  /\__/ / |_| | |_) | | | | | | \__ \__ \ | (_) | | | |
--   \_/_/\_\ \____/ \__,_|_.__/|_| |_| |_|_|___/___/_|\___/|_| |_|

data SubmitTx tx
    = SubmitTx { bytes :: tx }
    deriving (Generic, Show)

data SubmitTxResponse e
    = SubmitTxResponse { error :: SubmitResult e }
    deriving (Generic, Show)

instance Show e => Show (SubmitResult e) where
    show = \case
        SubmitSuccess ->"SubmitSuccess"
        SubmitFail e -> "SubmitFail " <> show e

mkLocalTxSubmissionClient
    :: forall m tx err.
        ( FromJSON tx
        , ToJSON err
        , Monad m
        )
    => Worker ByteString Json.Encoding m
    -> LocalTxSubmissionClient tx err m ()
mkLocalTxSubmissionClient Worker{await,yield,pass} =
     LocalTxSubmissionClient clientStIdle
  where
    clientStIdle
        :: m (LocalTxClientStIdle tx err m ())
    clientStIdle = await >>= Wsp.handle
        (\bytes -> pass bytes *> clientStIdle)
        [ Wsp.Handler $ \SubmitTx{bytes} toResponse ->
            pure $ SendMsgSubmitTx bytes $ \e -> do
                yield $ Json.toEncoding $ toResponse $ SubmitTxResponse e
                clientStIdle
        ]

--  _____ _        _         _____
-- /  ___| |      | |       |  _  |
-- \ `--.| |_ __ _| |_ ___  | | | |_   _  ___ _ __ _   _
--  `--. \ __/ _` | __/ _ \ | | | | | | |/ _ \ '__| | | |
-- /\__/ / || (_| | ||  __/ \ \/' / |_| |  __/ |  | |_| |
-- \____/ \__\__,_|\__\___|  \_/\_\\__,_|\___|_|   \__, |
--                                                  __/ |
--                                                 |___/

data Acquire point
    = Acquire { point :: point }
    deriving (Generic, Show)

data AcquireResponse point
    = AcquireSuccess { acquired :: point }
    | AcquireFailed { failure :: AcquireFailure }
    deriving (Generic, Show)

data Release
    = Release
    deriving (Generic, Show)

data Query block = Query { query :: SomeQuery Maybe block }
    deriving (Generic)

newtype QueryResponse =
    QueryResponse { unQueryResponse :: Json.Value }
    deriving (Generic, Show)

mkLocalStateQueryClient
    :: forall m block point.
        ( MonadThrow m
        , ToJSON point
        , ToJSON AcquireFailure
        , FromJSON point
        , FromJSON (SomeQuery Maybe block)
        , point ~ Point block
        )
    => Worker ByteString Json.Encoding m
    -> LocalStateQueryClient block (Point block) (Ledger.Query block) m ()
mkLocalStateQueryClient Worker{await,yield,pass} =
    LocalStateQueryClient clientStIdle
  where
    clientStIdle
        :: m (LSQ.ClientStIdle block (Point block) (Ledger.Query block) m ())
    clientStIdle = await >>= Wsp.handle
        (\bytes -> pass bytes *> clientStIdle)
        [ Wsp.Handler $ \(Acquire pt) toResponse ->
            pure $ LSQ.SendMsgAcquire pt (clientStAcquiring pt toResponse)
        , Wsp.Handler $ \Release _toResponse ->
            clientStIdle
        ]

    clientStAcquiring
        :: point
        -> (AcquireResponse point -> Wsp.Response (AcquireResponse point))
        -> LSQ.ClientStAcquiring block (Point block) (Ledger.Query block) m ()
    clientStAcquiring pt toResponse = LSQ.ClientStAcquiring
        { recvMsgAcquired = do
            yield $ Json.toEncoding $ toResponse $ AcquireSuccess pt
            clientStAcquired
        , recvMsgFailure = \failure -> do
            yield $ Json.toEncoding $ toResponse $ AcquireFailed failure
            clientStIdle
        }

    clientStAcquired
        :: m (LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ())
    clientStAcquired = await >>= Wsp.handle
        (\bytes -> pass bytes *> clientStAcquired)
        [ Wsp.Handler $ \(Acquire pt) toResponse ->
            pure $ LSQ.SendMsgReAcquire pt (clientStAcquiring pt toResponse)
        , Wsp.Handler $ \Release _toResponse ->
            pure $ LSQ.SendMsgRelease clientStIdle
        , Wsp.Handler $ \(Query (SomeQuery query encodeResult _)) toResponse ->
            pure $ LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                { recvMsgResult = \result -> do
                    yield $ Json.toEncoding $ toResponse $ QueryResponse $ encodeResult result
                    clientStAcquired
                }
        ]

--  _   _      _
-- | | | |    | |
-- | |_| | ___| |_ __   ___ _ __ ___
-- |  _  |/ _ \ | '_ \ / _ \ '__/ __|
-- | | | |  __/ | |_) |  __/ |  \__ \
-- \_| |_/\___|_| .__/ \___|_|  |___/
--              | |
--              |_|

-- | WebSocket 'Connection', connects all Ouroboros clients together from a
-- single WebSocket connections. It works by constructing simple pipes using
-- queues between clients. Messages first flow through the chain sync clients
-- (as it is the one with the main performance constraints and which will likely
-- process the most messages) and, are passed onto the next client if the
-- message were not recognized as a 'chainSync' message. Here's a little
-- diagram:
--
--     *---------------------------------------------------------------------*
--     |                                                                     |
--     v                     pass                  pass                fail  |
-- WebSocket --> Chain Sync ------> Tx Submission ------> State Query ------>*
--     ^             |                    |                   |
--     |             | yield              | yield             | yield
--     |             v                    v                   v
--     *------------<*<------------------<*<------------------*
--
newClients
    :: forall block tx err.
        ( ToJSON block
        , ToJSON (Point block)
        , ToJSON (Tip block)
        , ToJSON err
        , FromJSON (Point block)
        , FromJSON (SomeQuery Maybe block)
        , FromJSON tx
        )
    => WS.Connection
    -> IO (Clients IO block tx err)
newClients conn = do
    -- State Query
    stateQueryWorker <- newWorker yield (onUnmatchedMessage @block @tx conn)
    let localStateQueryClient = mkLocalStateQueryClient stateQueryWorker

    -- Tx Submission
    txSubmissionWorker <- bindWorker stateQueryWorker
    let localTxSubmissionClient = mkLocalTxSubmissionClient txSubmissionWorker

    -- Chain Sync
    chainSyncWorker <- bindWorker txSubmissionWorker
    chainSyncInternalQueue <- newQueue
    let chainSyncClient = mkChainSyncClient chainSyncInternalQueue chainSyncWorker

    -- Receiving loop
    link =<< async (feedWorker (WS.receiveData conn) chainSyncWorker)
    pure Clients
        { chainSyncClient
        , localTxSubmissionClient
        , localStateQueryClient
        }
  where
    -- | Send a value back into the web-socket
    yield :: Json.Encoding -> IO ()
    yield = WS.sendTextData conn . BL.toStrict . Json.encodingToLazyByteString

-- | Provide a handler for exception arising when trying to connect to a node
-- that is down.
handleIOException
    :: Tracer IO TraceOgmios
    -> WS.Connection
    -> IOException
    -> IO ()
handleIOException tr conn e = do
    traceWith tr $ OgmiosFailedToConnect e
    let msg = "Connection with the node lost or failed."
    WS.sendClose conn $ BL.toStrict $ Json.encode $ Wsp.serverFault msg

-- | A default handler for unmatched requests. This is an attempt to provide
-- better error messages without adding too much complexity to the above
-- handlers.
--
-- Each handler is rather isolated and independent. They aren't aware of other
-- handlers and this makes them easy to extend or modify independently. A direct
-- consequence if that, from a single handler it is hard to tell whether a
-- request is totally invalid in the context of Ogmios, or simply invalid for
-- that particular handler.
--
-- When a handler fails to parse a request, it simply passes it to the next
-- handler in the pipeline. Ultimately, once all handlers have passed, we end up
-- with a request that wasn't proceeded successfully but, the reason why that is
-- are unclear at this stage:
--
-- - Is the request just a gibberish input?
-- - Is the request an almost valid 'FindIntersect' but with an error on the
--   argument?
-- - Is the request an almost valid 'SubmitTx' but with a slightly invalid
--   transaction body?
--
-- This is what this function tries to answer and yield back to users. To some
-- extent, it redoes some of the parsing work above, but it only occurs on
-- client errors. This way, base handlers are kept clean and fast for normal
-- processing.
onUnmatchedMessage
    :: forall block tx.
        ( FromJSON (Point block)
        , FromJSON (SomeQuery Maybe block)
        , FromJSON tx
        )
    => WS.Connection
    -> ByteString
    -> IO ()
onUnmatchedMessage conn blob = do
    WS.sendTextData conn $ BL.toStrict $ Json.encode $ Wsp.clientFault $ T.unpack fault
  where
    -- Hopefully, this should not show up too often, but only if a request
    -- really is unknown.
    defaultGenericFault =
        "unknown method in 'methodname' (beware names are case-sensitive)."

    fault =
        "Invalid request: " <> modifyAesonFailure details <> "."
      where
        details = case Json.decode' (BL.fromStrict blob) of
            Just (Json.Object obj) ->
                either T.pack absurd $ Json.parseEither userFriendlyParser obj
            _ ->
                "must be a well-formed JSON object."

    -- A parser that never resolves, yet yield proper error messages based on
    -- the attempted request.
    userFriendlyParser :: Json.Object -> Json.Parser Void
    userFriendlyParser obj = do
        methodName <-
            case obj !? "methodname" of
                Just (Json.String t) -> pure (T.unpack t)
                _ -> fail "field 'methodname' must be present and be a string."

        if | methodName == Wsp.gWSPMethodName (Proxy @(Rep (FindIntersect block) _)) ->
              void $ parseJSON @(Wsp.Request (FindIntersect block)) (Json.Object obj)

           | methodName == Wsp.gWSPMethodName (Proxy @(Rep RequestNext _)) ->
              void $ parseJSON @(Wsp.Request RequestNext) (Json.Object obj)

           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (SubmitTx tx) _)) ->
              void $ parseJSON @(Wsp.Request (SubmitTx tx)) (Json.Object obj)

           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (Acquire (Point block)) _)) ->
              void $ parseJSON @(Wsp.Request (Acquire (Point block))) (Json.Object obj)

           | methodName == Wsp.gWSPMethodName (Proxy @(Rep Release _)) ->
              void $ parseJSON @(Wsp.Request Release) (Json.Object obj)

           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (Query block) _)) ->
              void $ parseJSON @(Wsp.Request (Query block)) (Json.Object obj)

           | otherwise ->
              pure ()

        fail defaultGenericFault

    modifyAesonFailure :: Text -> Text
    modifyAesonFailure
        = T.dropWhileEnd (== '.')
        . T.replace "Error in $["    "invalid item ["
        . T.replace "Error in $: "    ""
        . T.replace "Error in $: key" "field"

--    ___ _____  _____ _   _   _____          _
--   |_  /  ___||  _  | \ | | |_   _|        | |
--     | \ `--. | | | |  \| |   | | _ __  ___| |_ __ _ _ __   ___ ___  ___
--     | |`--. \| | | | . ` |   | || '_ \/ __| __/ _` | '_ \ / __/ _ \/ __|
-- /\__/ /\__/ /\ \_/ / |\  |  _| || | | \__ \ || (_| | | | | (_|  __/\__ \
-- \____/\____/  \___/\_| \_/  \___/_| |_|___/\__\__,_|_| |_|\___\___||___/

type instance Wsp.ServiceName (Wsp.Response _) = "ogmios"

instance
    ( FromJSON (Point block)
    ) => FromJSON (Wsp.Request (FindIntersect block))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance FromJSON (Wsp.Request RequestNext)
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance
    ( ToJSON (Point block)
    , ToJSON (Tip block)
    ) => ToJSON (Wsp.Response (FindIntersectResponse block))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (FindIntersect block))

instance
    ( ToJSON block
    , ToJSON (Tip block)
    , ToJSON (Point block)
    ) => ToJSON (Wsp.Response (RequestNextResponse block))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request RequestNext)

instance
    ( FromJSON tx
    ) => FromJSON (Wsp.Request (SubmitTx tx))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance ToJSON e => ToJSON (Wsp.Response (SubmitTxResponse e)) where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (SubmitTx _))

instance ToJSON e => ToJSON (SubmitResult e) where
    toJSON = \case
        SubmitSuccess -> Json.String "SubmitSuccess"
        SubmitFail e  -> Json.object [ "SubmitFail" .= e ]

instance
    ( FromJSON (Point block)
    ) => FromJSON (Wsp.Request (Acquire (Point block)))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance
    ( ToJSON AcquireFailure
    , ToJSON (Point block)
    ) => ToJSON (Wsp.Response (AcquireResponse (Point block)))
  where
    toJSON = Wsp.genericToJSON Wsp.defaultOptions proxy
      where proxy = Proxy @(Wsp.Request (Acquire _))

instance FromJSON (Wsp.Request Release)
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance
    ( FromJSON (SomeQuery Maybe block)
    ) => FromJSON (Wsp.Request (Query block))
  where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions

instance ToJSON (Wsp.Response QueryResponse)
  where
    toJSON = Wsp.mkResponse Wsp.defaultOptions unQueryResponse proxy
      where proxy = Proxy @(Wsp.Request (Query _))
