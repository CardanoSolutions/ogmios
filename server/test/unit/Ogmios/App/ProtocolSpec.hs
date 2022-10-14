-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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
                  , "'methodname' must be present"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{ "methodname": "Query" }|]
                  , "missing required field 'type'"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{
                      "type": "jsonwsp/request",
                      "version": "1.0",
                      "servicename": "ogmios",
                      "methodname": "??"
                    }|]
                  , "unknown method in 'methodname'"
                  , Just Json.Null
                  )
                , ( json [aesonQQ|{
                      "methodname": "SubmitTx",
                      "mirror": { "requestId": 42 }
                    }|]
                  , "missing required field 'type'"
                  , Just [aesonQQ|{ "requestId": 42 }|]
                  )
                ]

        forM_ matrix $ \(bytes, msg, mirror) ->
            specify (decodeUtf8 bytes <> " => " <> msg) $ do
                let fault = inefficientEncodingToValue $ onUnmatchedMessage @Block bytes
                fault ^? key "type" . _String `shouldBe` Just "jsonwsp/fault"
                fault ^? key "version" . _String `shouldBe` Just "1.0"
                fault ^? key "servicename" . _String `shouldBe` Just "ogmios"
                case fault ^? key "fault" . key "string" . _String of
                    Nothing ->
                        expectationFailure "Fault does not contain a 'fault' string."
                    Just str ->
                        str `shouldSatisfy` T.isInfixOf (toText msg)
                fault ^? key "reflection" . _Value `shouldBe` mirror

--
-- Helpers
--

json :: Json.Value -> ByteString
json = toStrict . encode
