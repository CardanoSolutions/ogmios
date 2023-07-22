--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}

module Codec.Json.RpcSpec
  ( spec,
  )
where

import Prelude

import Data.Aeson
    ( FromJSON (..), Result (..), ToJSON (..), fromJSON )
import Data.Aeson.QQ.Simple
    ( aesonQQ )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, context, shouldBe, specify )

import qualified Codec.Json.Rpc as Rpc
import qualified Codec.Json.Rpc.Handler as Rpc

spec :: Spec
spec = context "gRpcFromJSON" $ do
    specify "can parse Rpc envelope" $ do
        let
            json = [aesonQQ|
                { "jsonrpc": "2.0"
                , "method": "Foo"
                , "params":
                    { "foo": 14
                    , "bar": true
                    }
                }
            |]
         in
            fromJSON json `shouldBe` Success (Rpc.Request Nothing (Foo 14 True))

    specify "can parse Rpc envelope with mirror" $ do
        let
            json = [aesonQQ|
                { "jsonrpc": "2.0"
                , "method": "Foo"
                , "params":
                    { "foo": 14
                    , "bar": true
                    }
                , "id": "whatever"
                }
            |]

            mirror = Just (toJSON ("whatever" :: String))
         in
            fromJSON json `shouldBe` Success (Rpc.Request mirror (Foo 14 True))

data Foo = Foo
    { foo :: Int
    , bar :: Bool
    } deriving (Eq, Show, Generic)

instance FromJSON (Rpc.Request Foo) where
    parseJSON = Rpc.genericFromJSON Rpc.defaultOptions
