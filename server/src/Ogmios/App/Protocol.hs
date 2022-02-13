--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ogmios.App.Protocol
    ( onUnmatchedMessage
    ) where

import Ogmios.Prelude

import Ogmios.Data.Json
    ( FromJSON, Json )
import Ogmios.Data.Protocol
    ( MethodName )
import Ogmios.Data.Protocol.ChainSync
    ( FindIntersect, RequestNext, _decodeFindIntersect, _decodeRequestNext )
import Ogmios.Data.Protocol.StateQuery
    ( Acquire, Query, Release, _decodeAcquire, _decodeQuery, _decodeRelease )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTx, SubmitTx, _decodeEvaluateTx, _decodeSubmitTx )
import Ouroboros.Network.Block
    ( Point (..) )

import Cardano.Network.Protocol.NodeToClient
    ( SerializedTx )
import GHC.Generics
    ( Rep )
import Relude.Extra.Map
    ( lookup )

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp
import qualified Data.Aeson as Json
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
    :: forall block.
        ( FromJSON (SerializedTx block)
        , FromJSON (Query Proxy block)
        , FromJSON (Point block)
        )
    => ByteString
    -> Json
onUnmatchedMessage blob = do
    Wsp.mkFault $ Wsp.clientFault reflection $ T.unpack fault
  where
    (fault, reflection) =
        ( "Invalid request: " <> modifyAesonFailure details <> "."
        , reflection'
        )
      where
        (details, reflection') = case Json.decode' (fromStrict blob) of
            Just (Json.Object obj) ->
                ( either toText absurd $ Json.parseEither userFriendlyParser obj
                , lookup "mirror" obj
                )
            _ ->
                ( "must be a well-formed JSON object."
                , Nothing
                )

    modifyAesonFailure :: Text -> Text
    modifyAesonFailure
        = T.dropWhileEnd (== '.')
        . T.replace
            "field \"submit\" not found"
            "field \"submit\" not found. If you're using the legacy \"bytes\" \
            \field, change it to \"submit\" to get a (hopefully) more \
            \informative error."
        . T.replace "Error in $["    "invalid item ["
        . T.replace "Error in $: "    ""
        . T.replace "Error in $: key" "field"

    -- A parser that never resolves, yet yield proper error messages based on
    -- the attempted request.
    userFriendlyParser :: Json.Object -> Json.Parser Void
    userFriendlyParser obj = do
        methodName <-
            case lookup "methodname" obj of
                Just (Json.String t) -> pure (T.unpack t)
                _ -> fail "field 'methodname' must be present and be a string."
        match methodName (Json.Object obj)

    match :: MethodName -> Json.Value -> Json.Parser Void
    match methodName json = do
        if | methodName == Wsp.gWSPMethodName (Proxy @(Rep (FindIntersect block) _)) ->
                void $ _decodeFindIntersect @block json
           | methodName == Wsp.gWSPMethodName (Proxy @(Rep RequestNext _)) ->
                void $ _decodeRequestNext json
           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (Acquire block) _)) ->
                void $ _decodeAcquire @block json
           | methodName == Wsp.gWSPMethodName (Proxy @(Rep Release _)) ->
                void $ _decodeRelease json
           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (Query Proxy block) _)) ->
                void $ _decodeQuery @block json
           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (SubmitTx block) _)) ->
                void $ _decodeSubmitTx @block json
           | methodName == Wsp.gWSPMethodName (Proxy @(Rep (EvaluateTx block) _)) ->
                void $ _decodeEvaluateTx @block json
           | otherwise ->
                pure ()
        fail "unknown method in 'methodname' (beware names are case-sensitive)."
