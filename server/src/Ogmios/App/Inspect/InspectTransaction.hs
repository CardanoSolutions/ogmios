--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.App.Inspect.InspectTransaction where

import Ogmios.Prelude

import Data.Aeson
    ( parseJSON
    )
import Ogmios.App.Configuration
    ( omitOptionalCbor
    )
import Ogmios.Data.Json
    ( MultiEraDecoder (..)
    , encodeDeserialisationFailure
    , encodeTx
    , jsonToByteString
    )
import Ogmios.Data.Json.Prelude
    ( MetadataFormat (..)
    , encodeMaybe
    )
import System.Exit
    ( ExitCode (..)
    )

import Ogmios.Data.Json.Orphans
    ()

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Char8 as B8

-- | Interpret a hex-encoded bytestring as a 'Transaction' and display
-- information about it.
inspectTransaction :: Text -> IO ()
inspectTransaction input =
    case Json.parseEither parseJSON (Json.String input) of
        Left e -> do
            B8.putStrLn (encodeUtf8 e)
            exitWith (ExitFailure 1)
        Right (MultiEraDecoderErrors errs) -> do
            B8.putStrLn $ jsonToByteString $ encodeDeserialisationFailure
                (\_ _ -> encodeMaybe identity)
                errs
            exitWith (ExitFailure 1)
        Right (MultiEraDecoderSuccess transaction) ->
            B8.putStrLn $ jsonToByteString $ encodeTx @StandardCrypto (MetadataDetailedSchema, omitOptionalCbor) transaction
