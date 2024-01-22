--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.Data.Protocol.StateQuery
    ( -- * Codecs
      StateQueryCodecs (..)
    , mkStateQueryCodecs

      -- * Genesis Configuration
    , GetGenesisConfig (..)

      -- * Messages
    , StateQueryMessage (..)

      -- ** AcquireLedgerState
    , AcquireLedgerState (..)
    , _encodeAcquireLedgerState
    , _decodeAcquireLedgerState
    , AcquireLedgerStateResponse (..)
    , _encodeAcquireLedgerStateResponse

      -- ** ReleaseLedgerState
    , ReleaseLedgerState (..)
    , _encodeReleaseLedgerState
    , _decodeReleaseLedgerState
    , ReleaseLedgerStateResponse (..)
    , _encodeReleaseLedgerStateResponse

      -- ** QueryLedgerState
    , QueryLedgerState
    , _decodeQueryLedgerState
    , QueryLedgerStateResponse (..)
    , _encodeQueryLedgerStateResponse
    ) where

import Ogmios.Data.Json.Prelude

import Data.Aeson
    ( parseJSON
    , (<?>)
    )
import Ogmios.Data.Json.Query
    ( GenesisConfig
    , Query (..)
    )
import Ouroboros.Network.Block
    ( Point (..)
    , StandardHash
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Codec.Json.Rpc.Handler as Rpc
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T
import qualified Text.Show as T

--
-- Codecs
--

data StateQueryCodecs block = StateQueryCodecs
    { decodeAcquireLedgerState
        :: ByteString
        -> Maybe (Rpc.Request (AcquireLedgerState block))
    , encodeAcquireLedgerStateResponse
        :: Rpc.Response (AcquireLedgerStateResponse block)
        -> Json
    , decodeReleaseLedgerState
        :: ByteString
        -> Maybe (Rpc.Request ReleaseLedgerState)
    , encodeReleaseLedgerStateResponse
        :: Rpc.Response ReleaseLedgerStateResponse
        -> Json
    , decodeQueryLedgerState
        :: ByteString
        -> Maybe (Rpc.Request (Query Proxy block))
    , encodeQueryLedgerStateResponse
        :: Rpc.Response (QueryLedgerStateResponse block)
        -> Json
    }

mkStateQueryCodecs
    :: (FromJSON (Query Proxy block), FromJSON (Point block))
    => Rpc.Options
    -> (Point block -> Json)
    -> (AcquireFailure -> Json)
        -- ^ Failure to acquire
    -> (AcquireFailure -> Json)
        -- ^ Acquire expired
    -> StateQueryCodecs block
mkStateQueryCodecs opts encodePoint encodeAcquireFailure encodeAcquireExpired =
    StateQueryCodecs
        { decodeAcquireLedgerState =
            decodeWith _decodeAcquireLedgerState
        , encodeAcquireLedgerStateResponse =
            _encodeAcquireLedgerStateResponse opts encodePoint encodeAcquireFailure
        , decodeReleaseLedgerState =
            decodeWith _decodeReleaseLedgerState
        , encodeReleaseLedgerStateResponse =
            _encodeReleaseLedgerStateResponse opts
        , decodeQueryLedgerState =
            decodeWith _decodeQueryLedgerState
        , encodeQueryLedgerStateResponse =
            _encodeQueryLedgerStateResponse opts encodeAcquireExpired
        }

--
-- GetGenesisConfig
--

data GetGenesisConfig (m :: Type -> Type) = GetGenesisConfig
    { getByronGenesis :: m (GenesisConfig ByronEra)
    , getShelleyGenesis :: m (GenesisConfig ShelleyEra)
    , getAlonzoGenesis :: m (GenesisConfig AlonzoEra)
    , getConwayGenesis :: m (GenesisConfig ConwayEra)
    }

--
-- Messages
--

data StateQueryMessage block
    = MsgAcquireLedgerState
        (AcquireLedgerState block)
        (Rpc.ToResponse (AcquireLedgerStateResponse block))
    | MsgReleaseLedgerState
        ReleaseLedgerState
        (Rpc.ToResponse ReleaseLedgerStateResponse)
    | MsgQueryLedgerState
        (QueryLedgerState block)
        (Rpc.ToResponse (QueryLedgerStateResponse block))

instance StandardHash block => Show (StateQueryMessage block) where
    showsPrec i = \case
        MsgAcquireLedgerState acquire _ -> T.showParen (i >= 10)
            (T.showString $ "MsgAcquireLedgerState " <> show acquire)
        MsgReleaseLedgerState release _ -> T.showParen (i >= 10)
            (T.showString $ "MsgReleaseLedgerState " <> show release)
        MsgQueryLedgerState{} -> T.showParen (i >= 10)
            (T.showString "MsgQueryLedgerState")

--
-- AcquireLedgerState
--

data AcquireLedgerState block
    = AcquireLedgerState { point :: Point block }
    deriving (Generic, Show, Eq)

_encodeAcquireLedgerState
    :: forall block. ()
    => (Point block -> Json)
    -> Rpc.Request (AcquireLedgerState block)
    -> Json
_encodeAcquireLedgerState encodePoint =
    Rpc.mkRequest $ encodeObject . \case
        AcquireLedgerState{point} ->
            "point" .=
                encodePoint point

_decodeAcquireLedgerState
    :: FromJSON (Point block)
    => Json.Value
    -> Json.Parser (Rpc.Request (AcquireLedgerState block))
_decodeAcquireLedgerState =
    Rpc.genericFromJSON Rpc.defaultOptions

data AcquireLedgerStateResponse block
    = AcquireSuccess { point :: Point block }
    | AcquireFailure { failure :: AcquireFailure }
    deriving (Generic, Show)

_encodeAcquireLedgerStateResponse
    :: forall block. ()
    => Rpc.Options
    -> (Point block -> Json)
    -> (AcquireFailure -> Json)
    -> Rpc.Response (AcquireLedgerStateResponse block)
    -> Json
_encodeAcquireLedgerStateResponse opts encodePoint encodeAcquireFailure =
    Rpc.mkResponse opts $ \resolve reject -> \case
        AcquireSuccess{point} ->
            resolve $ encodeObject
                ( "acquired" .= encodeText "ledgerState" <>
                  "point" .= encodePoint point
                )
        AcquireFailure{failure} ->
            reject (Rpc.FaultCustom 2000)
                "Failed to acquire requested point."
                (Just $ encodeAcquireFailure failure)

--
-- ReleaseLedgerState
--

data ReleaseLedgerState
    = ReleaseLedgerState
    deriving (Generic, Show, Eq)

_encodeReleaseLedgerState
    :: Rpc.Request ReleaseLedgerState
    -> Json
_encodeReleaseLedgerState =
    Rpc.mkRequestNoParams

_decodeReleaseLedgerState
    :: Json.Value
    -> Json.Parser (Rpc.Request ReleaseLedgerState)
_decodeReleaseLedgerState =
    Rpc.genericFromJSON Rpc.defaultOptions

data ReleaseLedgerStateResponse
    = ReleaseLedgerStateResponse
    deriving (Generic, Show)

_encodeReleaseLedgerStateResponse
    :: Rpc.Options
    -> Rpc.Response ReleaseLedgerStateResponse
    -> Json
_encodeReleaseLedgerStateResponse opts =
    Rpc.ok opts $ \case
        ReleaseLedgerStateResponse ->
            encodeObject
                ( "released" .= encodeText "ledgerState"
                )

--
-- Query
--

type QueryLedgerState = Query Proxy

data QueryT = QueryLedgerState
    deriving (Generic)

_decodeQueryLedgerState
    :: FromJSON (QueryLedgerState block)
    => Json.Value
    -> Json.Parser (Rpc.Request (QueryLedgerState block))
_decodeQueryLedgerState json = do
    Rpc.Request method mirror QueryLedgerState <- Rpc.genericFromJSON opts json
    query <- parseJSON json <?> Json.Key "params"
    pure $ Rpc.Request method mirror query
  where
    opts = Rpc.defaultOptions
        { Rpc.methodNamePredicate = const ((==) "query" . T.take 5)
        }

data QueryLedgerStateResponse block
    = QueryResponse { unQueryResponse :: Json }
    | QueryEraMismatch { unEraMismatch :: Json }
    | QueryUnavailableInCurrentEra
    | QueryAcquireFailure { failure :: AcquireFailure }
    deriving (Generic)

instance Show (QueryLedgerStateResponse block) where
    showsPrec i = T.showParen (i >=10) . T.showString . \case
        QueryResponse json ->
            "QueryResponse (" <> str json <> ")"
        QueryEraMismatch hint ->
            "QueryEraMismatch (" <> str hint <> ")"
        QueryUnavailableInCurrentEra ->
            "QueryUnavailableInCurrentEra"
        QueryAcquireFailure failure ->
            "QueryAcquireFailure (" <> show failure <> ")"
      where
        str = decodeUtf8 . jsonToByteString

_encodeQueryLedgerStateResponse
    :: forall block. ()
    => Rpc.Options
    -> (AcquireFailure -> Json)
    -> Rpc.Response (QueryLedgerStateResponse block)
    -> Json
_encodeQueryLedgerStateResponse opts encodeAcquireExpired =
    Rpc.mkResponse opts $ \resolve reject -> \case
        QueryResponse response ->
            resolve response
        QueryEraMismatch hint ->
            reject (Rpc.FaultCustom 2001)
                "Era mismatch between query and ledger."
                (Just hint)
        QueryUnavailableInCurrentEra ->
            reject (Rpc.FaultCustom 2002)
                "Query unavailable in the current era."
                Nothing
        QueryAcquireFailure{failure} ->
            reject (Rpc.FaultCustom 2003)
                "Cannot perform query from previously acquired point."
                (Just $ encodeAcquireExpired failure)
