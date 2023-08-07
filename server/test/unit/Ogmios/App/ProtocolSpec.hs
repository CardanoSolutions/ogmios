-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}

module Ogmios.App.ProtocolSpec
    ( spec
    ) where

import Ogmios.Prelude

import Cardano.Network.Protocol.NodeToClient
    ( Block
    )
import Data.Aeson
    ( encode
    )
import Data.Aeson.Lens
    ( _String
    , _Value
    , key
    )
import Data.Aeson.QQ.Simple
    ( aesonQQ
    )
import Ogmios.App.Protocol
    ( onUnmatchedMessage
    )
import Ogmios.Data.Json
    ( inefficientEncodingToValue
    )
import Ogmios.Data.Json.Orphans
    ()
import Test.Hspec
    ( Spec
    , context
    , expectationFailure
    , parallel
    , shouldBe
    , shouldSatisfy
    , specify
    )

import qualified Data.Aeson as Json
import qualified Data.Text as T

spec :: Spec
spec = parallel $ do
    context "onUnmatchedMessage" $ do
        let matrix =
                [ ( "%$@#$"
                  , "must be a well-formed JSON object"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{}|]
                  , "'method' must be present"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{ "method": "queryLedgerState" }|]
                  , "missing required field 'jsonrpc'"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{
                      "jsonrpc": "2.0",
                      "method": "??"
                    }|]
                  , "unknown method in 'method'"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{
                      "method": "submitTransaction",
                      "id": { "requestId": 42 }
                    }|]
                  , "missing required field 'jsonrpc'"
                  , Just [aesonQQ|{ "requestId": 42 }|]
                  )
                ]

        forM_ matrix $ \(bytes, msg, mirror) ->
            specify (decodeUtf8 bytes <> " => " <> msg) $ do
                let fault = inefficientEncodingToValue $ onUnmatchedMessage @Block bytes
                fault ^? key "jsonrpc" . _String `shouldBe` Just "2.0"
                case fault ^? key "error" . key "message" . _String of
                    Nothing ->
                        expectationFailure "Error does not contain a 'message' string."
                    Just str ->
                        str `shouldSatisfy` T.isInfixOf (toText msg)
                fault ^? key "id" . _Value `shouldBe` mirror

--
-- Helpers
--

json :: Json.Value -> ByteString
json = toStrict . encode
