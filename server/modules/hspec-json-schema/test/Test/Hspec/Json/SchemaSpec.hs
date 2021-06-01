-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}

module Test.Hspec.Json.SchemaSpec
    ( spec
    ) where

import Prelude

import Data.Aeson
    ( ToJSON (..) )
import Data.Aeson.QQ.Simple
    ( aesonQQ )
import Data.String.Interpolate.Extra
    ( stringQQ )
import Test.Hspec
    ( Spec, SpecWith, context, expectationFailure, specify )
import Test.Hspec.Json.Schema
    ( SchemaRef (..), prop_validateToJSON, unsafeReadSchemaRef )
import Test.QuickCheck
    ( Args (..), Result (..), quickCheckWithResult )

import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Test.QuickCheck as QC

type ExpectedError = String

testFailure
    :: FilePath
    -> Json.Value
    -> ExpectedError
    -> SpecWith ()
testFailure filepath json expectedError = specify filepath $ do
    refs <- unsafeReadSchemaRef $ SchemaRef (T.pack filepath)
    let args = QC.stdArgs { chatty = False }
    result <- quickCheckWithResult args (prop_validateToJSON toJSON refs json)
    case result of
        Failure{output} | trim output == trim expectedError ->
            pure ()

        Failure{output} -> do
            expectationFailure output

        _ -> do
            expectationFailure $ "expected test to fail but didn't: " <> show result
  where
    trim = filter (/= ' ') . filter (/= '\n')

spec :: Spec
spec = context "prop_validateToJSON" $ do
    testFailure "test/schemas/001.json"
        [aesonQQ|
        { "size": "180cm"
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
        JSON:
        {
            "size": "180cm"
        }

        missing required property
          required:  ["size","weight"]
          found:     ["weight"]
        |]

    testFailure "test/schemas/002.json"
        [aesonQQ|
        { "mnemonic_size": 13
        , "amount": 0
        , "index": 42
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
        JSON:
        {
            "amount": 0,
            "mnemonic_size": 13,
            "index": 42
        }

        invalid properties in object
           invalid property 'amount': 0.0 should be greater than 1.0
           invalid property 'mnemonic_size': 13.0 should be a multiple of 3
           invalid property 'index': 42.0 should be lower than 10.0
        |]

    testFailure "test/schemas/003.json"
        [aesonQQ|
        { "firstname": "aa"
        , "lastname": "aaaa"
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
        JSON:
        {
            "lastname": "aaaa",
            "firstname": "aa"
        }

        invalid properties in object
           invalid property 'lastname': 'aaaa' should have at most 3 character(s)
           invalid property 'firstname': 'aa' should have at least 3 character(s)
        |]

    testFailure "test/schemas/004.json"
        [aesonQQ|
        { "words": ["patate"]
        , "indexes": [1,2,3]
        , "keys": { "a": 1, "b": 2 }
        , "props": { "a": 1 }
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
        JSON:
        {
            "props": {
                "a": 1
            },
            "keys": {
                "a": 1,
                "b": 2
            },
            "words": [
                "patate"
            ],
            "indexes": [
                1,
                2,
                3
            ]
        }

        invalid properties in object
            invalid property 'props': should have at least 2 propertie(s)
            invalid property 'keys': should have at most 1 propertie(s)
            invalid property 'words': should have at least 3 item(s)
            invalid property 'indexes': should have at most 2 item(s)
        |]

    testFailure "test/schemas/005.json"
        [aesonQQ|
        { "postcode": "31"
        , "region": null
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
        JSON:
        {
            "postcode": "31",
            "region": null
        }

        invalid properties in object
            invalid property 'postcode': "31" should match /[0-9]{5}/
            invalid property 'region': should *not* match the following schema:
              {
                  "type": "null"
              }
        |]

    testFailure "test/schemas/006.json"
        [aesonQQ|
        { "car_brands": ["renault", "audi", "audi"]
        , "cities": ["a", "b"]
        , "point": [1,2,3]
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
        JSON:
        {
            "car_brands": [
                "renault",
                "audi",
                "audi"
            ],
            "cities": [
                "a",
                "b"
            ],
            "point": [
                1,
                2,
                3
            ]
        }

        invalid properties in object
            invalid property 'car_brands': should have only unique items.
            invalid property 'cities': invalid additional item(s):
                [1] 'b' should have at least 10 character(s)
            invalid property 'point': unexpected additional item(s) at position(s): [2]
        |]

    testFailure "test/schemas/007.json"
        [aesonQQ|
        { "band(s)": 14
        , "genre": "death metal"
        , "country": "Finland"
        }|]
        [stringQQ|
        *** Failed! Assertion failed (after 1 test):
           JSON:
           {
               "country": "Finland",
               "band(s)": 14,
               "genre": "death metal"
           }

           invalid properties in object
               invalid property 'country': expected exactly 1 schema to match. But found 2 matching schemas.
               invalid property 'band(s)': failed to validate any of given schemas:
                   [0] should be of type: string
                   [1] should be of type: array
               invalid property 'genre': failed to validate all given schemas:
                   [0] should be one of the following values: "rock","classic","rap"
                   [1] 'death metal' should have at most 3 character(s)
        |]
