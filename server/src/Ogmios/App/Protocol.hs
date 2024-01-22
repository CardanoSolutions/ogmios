--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}

module Ogmios.App.Protocol
    ( onUnmatchedMessage
    , defaultWithInternalError
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( SerializedTransaction
    )
import Data.List
    ( isInfixOf
    )
import GHC.Generics
    ( Rep
    )
import Ogmios.Control.Exception
    ( MonadCatch (..)
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO
    )
import Ogmios.Data.Json
    ( FromJSON
    , Json
    , MultiEraDecoder
    )
import Ogmios.Data.Protocol.ChainSync
    ( FindIntersection
    , NextBlock
    , _decodeFindIntersection
    , _decodeNextBlock
    )
import Ogmios.Data.Protocol.StateQuery
    ( AcquireLedgerState
    , QueryLedgerState
    , ReleaseLedgerState
    , _decodeAcquireLedgerState
    , _decodeQueryLedgerState
    , _decodeReleaseLedgerState
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AcquireMempool
    , GenTxId
    , HasTransaction
    , NextTransaction
    , ReleaseMempool
    , SizeOfMempool
    , _decodeAcquireMempool
    , _decodeHasTransaction
    , _decodeNextTransaction
    , _decodeReleaseMempool
    , _decodeSizeOfMempool
    )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTransaction
    , SubmitTransaction
    , _decodeEvaluateTransaction
    , _decodeSubmitTransaction
    )
import Ouroboros.Network.Block
    ( Point (..)
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T

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
-- - Is the request an almost valid 'FindIntersection' but with an error on the
--   argument?
-- - Is the request an almost valid 'SubmitTransaction' but with a slightly invalid
--   transaction body?
--
-- This is what this function tries to answer and yield back to users. To some
-- extent, it redoes some of the parsing work above, but it only occurs on
-- client errors. This way, base handlers are kept clean and fast for normal
-- processing.
onUnmatchedMessage
    :: forall block.
        ( FromJSON (MultiEraDecoder (SerializedTransaction block))
        , FromJSON (QueryLedgerState block)
        , FromJSON (Point block)
        , FromJSON (GenTxId block)
        , FromJSON (MultiEraUTxO block)
        )
    => Rpc.Options
    -> ByteString
    -> Json
onUnmatchedMessage opts blob = do
    Rpc.ko opts $ Rpc.invalidRequest reflection $ T.unpack fault
  where
    (fault, reflection) =
        ( "Invalid request: " <> modifyAesonFailure details <> "."
        , reflection'
        )
      where
        (details, reflection') = case Json.decode' (fromStrict blob) of
            Just (Json.Object obj) ->
                ( either toText absurd $ Json.parseEither userFriendlyParser obj
                , Json.lookup "id" obj
                )
            _ ->
                ( "must be a well-formed JSON object."
                , Nothing
                )

    modifyAesonFailure :: Text -> Text
    modifyAesonFailure
        = T.dropWhileEnd (== '.')
        . T.replace "Error in $[" "invalid item ["
        . T.replace "Error in $: " ""
        . T.replace "Error in $: key" "field"

    -- A parser that never resolves, yet yield proper error messages based on
    -- the attempted request.
    userFriendlyParser :: Json.Object -> Json.Parser Void
    userFriendlyParser obj = do
        methodName <-
            case Json.lookup "method" obj of
                Just (Json.String t) -> pure (toString t)
                _ -> fail "field 'method' must be present and be a string."
        match methodName (Json.Object obj)

    match :: String -> Json.Value -> Json.Parser Void
    match methodName json = do
        -- Chain-Sync
        if | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep (FindIntersection block) _)) ->
                void $ _decodeFindIntersection @block json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep NextBlock _)) ->
                void $ _decodeNextBlock json
        -- State-Query
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep (AcquireLedgerState block) _)) ->
                void $ _decodeAcquireLedgerState @block json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep ReleaseLedgerState _)) ->
                void $ _decodeReleaseLedgerState json
           | "queryLedgerState" `isInfixOf` methodName || "queryNetwork" `isInfixOf` methodName ->
                void $ _decodeQueryLedgerState @block json
        -- Tx-Submission
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep (SubmitTransaction block) _)) ->
                void $ _decodeSubmitTransaction @block json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep (EvaluateTransaction block) _)) ->
                void $ _decodeEvaluateTransaction @block json
        -- Tx-Monitor
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep AcquireMempool _)) ->
                void $ _decodeAcquireMempool json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep NextTransaction _)) ->
                void $ _decodeNextTransaction json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep (HasTransaction block) _)) ->
                void $ _decodeHasTransaction @block json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep SizeOfMempool _)) ->
                void $ _decodeSizeOfMempool json
           | methodName == Rpc.gRpcMethodName opts (Proxy @(Rep ReleaseMempool _)) ->
                void $ _decodeReleaseMempool json
        -- Fallback
           | otherwise ->
                pure ()
        fail "unknown method in 'method' (beware names are case-sensitive)."

-- | 'catch-all' handler which turns unexpected exception as internal errors.
defaultWithInternalError :: MonadCatch m => Rpc.Options -> m a -> (Json -> m ()) -> Rpc.ToResponse r -> m a -> m a
defaultWithInternalError opts continue yield toResponse = handle $ \(e :: SomeException) -> do
    let (Rpc.Response _ mirror _) = toResponse (error "unused and unevaluated")
    yield $ Rpc.ko opts $ Rpc.internalError mirror (displayException e)
    continue
