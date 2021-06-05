--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Aeson.Via.ShowSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.Aeson
    ( ToJSON (..) )
import Data.Aeson.QQ.Simple
    ( aesonQQ )
import Data.Aeson.Via.Show
    ( ToJSONViaShow (..) )
import Data.Aeson.Via.Show.Internal
    ( Expr (..), parseExpr )
import Data.Map.Strict
    ( Map )
import Data.Time
    ( UTCTime (..) )
import Test.Hspec
    ( Spec, context, shouldBe, specify )

import qualified Data.Map.Strict as Map

data Foo = Foo
    { foo :: Int
    }
    deriving stock Show
    deriving ToJSON via ToJSONViaShow Foo

data Bar = Bar Int Bool
    deriving stock Show
    deriving ToJSON via ToJSONViaShow Bar

data Fizz = Fizz { a :: Foo, b :: Bar }
    deriving stock Show
    deriving ToJSON via ToJSONViaShow Fizz

newtype MyMap = MyMap (Map Char Bool)
    deriving stock Show
    deriving ToJSON via ToJSONViaShow MyMap

data LastUpdate = LastUpdate (Maybe UTCTime)
    deriving stock Show
    deriving ToJSON via ToJSONViaShow LastUpdate

data NetworkParameters = NetworkParameters
    { epoch :: Maybe Int
    , systemStart :: UTCTime
    , networkMagic :: NetworkMagic
    }
    deriving stock Show
    deriving ToJSON via ToJSONViaShow NetworkParameters

newtype NetworkMagic = NetworkMagic
    { unNetworkMagic :: Int
    }
    deriving stock Show

data Some = forall a. (Show a, ToJSON a) => Some a

spec :: Spec
spec = do
    context "parseExpr" $ do
        let matrix =
                [ ( "Foo", Product "Foo" [] )
                , ( "Foo Foo", Product "Foo" [Product "Foo" []] )
                , ( "Foo 14", Product "Foo" [Val "14"] )
                , ( show $ Foo 14 , Record "Foo" [("foo",Val "14")] )
                , ( show $ Bar 42 True, Product "Bar" [Val "42", Product "True" []] )
                , ( show $ Fizz (Foo 14) (Bar 42 True)
                  , Record "Fizz"
                        [ ("a", Record "Foo" [("foo",Val"14")])
                        , ("b", Product "Bar" [Val "42", Product "True" []] )
                        ]
                  )
                , ( "[]", List [] )
                , ( "[1,2,3]", List [Val "1", Val "2", Val "3"] )
                , ( "fromList [('a',True),('b',False)]", Product "Container"
                    [ List
                        [ List [Val "a", Product "True" []]
                        , List [Val "b", Product "False" []]
                        ]
                    ]
                  )
                , ( "Health {time = 2021-06-05 17:17:54.710264188 UTC}"
                  , Record "Health" [("time", Val "2021-06-05 17:17:54.710264188 UTC")]
                  )
                , ( "LastUpdate (Just 2021-06-05 17:17:54.710264188 UTC)"
                  , Product "LastUpdate" [Product "Just" [Val "2021-06-05", Val "17:17:54.710264188", Product "UTC" []]]
                  )
                , ( "OgmiosNetwork {networkParameters = NetworkParameters {networkMagic = NetworkMagic {unNetworkMagic = 764824073}, systemStart = SystemStart 2017-09-23 21:44:51 UTC, slotsPerEpoch = EpochSlots {unEpochSlots = 21600}}}"
                  , Record "OgmiosNetwork"
                        [ ( "networkParameters", Record "NetworkParameters"
                            [ ( "networkMagic", Record "NetworkMagic"
                                [ ( "unNetworkMagic", Val "764824073" )
                                ]
                              )
                            , ( "systemStart", Product "SystemStart"
                                [ Val "2017-09-23", Val "21:44:51", Product "UTC" []
                                ]
                              )
                            , ( "slotsPerEpoch", Record "EpochSlots"
                                [ ( "unEpochSlots", Val "21600" )
                                ]
                              )
                            ]
                          )
                        ]
                  )
                ]
        forM_ matrix $ \(input, expected) -> do
            specify input $ parseExpr False input `shouldBe` (expected, "")

    context "ToJSONViaShow" $ do
        let time = read "2021-06-05 17:17:54.710264188 UTC"
        let matrix =
                [ ( Some (Foo 42)
                  , [aesonQQ|{"Foo":"42"}|]
                  )
                , ( Some (Bar 14 True)
                  , [aesonQQ|{"Bar":["14",true]}|]
                  )
                , ( Some (Fizz (Foo 42) (Bar 14 True))
                  , [aesonQQ|{"Fizz":{"a":"42","b":["14",true]}}|]
                  )
                , ( Some (LastUpdate Nothing)
                  , [aesonQQ|{"LastUpdate": null}|]
                  )
                , ( Some (LastUpdate (Just time))
                  , [aesonQQ|{"LastUpdate": "2021-06-05 17:17:54.710264188 UTC"}|]
                  )
                , ( Some (NetworkParameters (Just 42) time (NetworkMagic 14))
                  , [aesonQQ|{"NetworkParameters": { "epoch": "42", "systemStart": "2021-06-05 17:17:54.710264188 UTC", "networkMagic": "14" } }|]
                  )
                , ( Some (NetworkParameters Nothing time (NetworkMagic 14))
                  , [aesonQQ|{"NetworkParameters": { "epoch": null, "systemStart": "2021-06-05 17:17:54.710264188 UTC", "networkMagic": "14" } }|]
                  )
                , ( Some (MyMap $ Map.fromList [('a', True), ('b', False)])
                  , [aesonQQ|{"MyMap":{"a":true,"b":false}}|]
                  )
                ]
        forM_ matrix $ \(Some input, expected) ->
            specify (show input) $ toJSON input `shouldBe` expected
