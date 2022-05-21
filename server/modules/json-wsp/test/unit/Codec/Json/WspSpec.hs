--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Json.WspSpec
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

import qualified Codec.Json.Wsp as Wsp
import qualified Codec.Json.Wsp.Handler as Wsp

spec :: Spec
spec = context "gWSPFromJSON" $ do
    specify "can parse WSP envelope" $ do
        let
            json = [aesonQQ|
                { "type": "jsonwsp/request"
                , "version": "1.0"
                , "servicename": "any"
                , "methodname": "Foo"
                , "args":
                    { "foo": 14
                    , "bar": true
                    }
                }
            |]
         in
            fromJSON json `shouldBe` Success (Wsp.Request Nothing (Foo 14 True))

    specify "can parse WSP envelope with mirror" $ do
        let
            json = [aesonQQ|
                { "type": "jsonwsp/request"
                , "version": "1.0"
                , "servicename": "any"
                , "methodname": "Foo"
                , "args":
                    { "foo": 14
                    , "bar": true
                    }
                , "mirror": "whatever"
                }
            |]

            mirror = Just (toJSON ("whatever" :: String))
         in
            fromJSON json `shouldBe` Success (Wsp.Request mirror (Foo 14 True))

    specify "fails when given a 'reflection' key" $ do
        let
            json = [aesonQQ|
                { "type": "jsonwsp/request"
                , "version": "1.0"
                , "servicename": "any"
                , "methodname": "Foo"
                , "args":
                    { "foo": 14
                    , "bar": true
                    }
                , "reflection": "whatever"
                }
            |]
         in
            fromJSON @(Wsp.Request Foo) json `shouldBe` Error "invalid key 'reflection'; should be 'mirror' on requests."

data Foo = Foo
    { foo :: Int
    , bar :: Bool
    } deriving (Eq, Show, Generic)

instance FromJSON (Wsp.Request Foo) where
    parseJSON = Wsp.genericFromJSON Wsp.defaultOptions
