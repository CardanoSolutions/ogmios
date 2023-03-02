--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
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

      -- ** Acquire
    , Acquire (..)
    , _encodeAcquire
    , _decodeAcquire
    , AcquireResponse (..)
    , _encodeAcquireResponse

      -- ** Release
    , Release (..)
    , _encodeRelease
    , _decodeRelease
    , ReleaseResponse (..)
    , _encodeReleaseResponse

      -- ** Query
    , Query (..)
    , _decodeQuery
    , QueryResponse (..)
    , _encodeQueryResponse
    ) where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Json.Query
    ( ByronEra
    , GenesisConfig
    , Query (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( AlonzoEra
    , ShelleyEra
    )
import Ouroboros.Network.Block
    ( Point (..)
    , StandardHash
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure
    )

import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson.Types as Json
import qualified Text.Show as T

--
-- Codecs
--

data StateQueryCodecs block = StateQueryCodecs
    { decodeAcquire
        :: ByteString
        -> Maybe (Rpc.Request (Acquire block))
    , encodeAcquireResponse
        :: Rpc.Response (AcquireResponse block)
        -> Json
    , decodeRelease
        :: ByteString
        -> Maybe (Rpc.Request Release)
    , encodeReleaseResponse
        :: Rpc.Response ReleaseResponse
        -> Json
    , decodeQuery
        :: ByteString
        -> Maybe (Rpc.Request (Query Proxy block))
    , encodeQueryResponse
        :: Rpc.Response (QueryResponse block)
        -> Json
    }

mkStateQueryCodecs
    :: (FromJSON (Query Proxy block), FromJSON (Point block))
    => (Point block -> Json)
    -> (AcquireFailure -> Json)
    -> StateQueryCodecs block
mkStateQueryCodecs encodePoint encodeAcquireFailure =
    StateQueryCodecs
        { decodeAcquire =
            decodeWith _decodeAcquire
        , encodeAcquireResponse =
            _encodeAcquireResponse encodePoint encodeAcquireFailure
        , decodeRelease =
            decodeWith _decodeRelease
        , encodeReleaseResponse =
            _encodeReleaseResponse
        , decodeQuery =
            decodeWith _decodeQuery
        , encodeQueryResponse =
            _encodeQueryResponse encodeAcquireFailure
        }

--
-- GetGenesisConfig
--

data GetGenesisConfig (m :: Type -> Type) = GetGenesisConfig
    { getByronGenesis :: m (GenesisConfig ByronEra)
    , getShelleyGenesis :: m (GenesisConfig ShelleyEra)
    , getAlonzoGenesis :: m (GenesisConfig AlonzoEra)
    }

--
-- Messages
--

data StateQueryMessage block
    = MsgAcquire
        (Acquire block)
        (Rpc.ToResponse (AcquireResponse block))
        Rpc.ToFault
    | MsgRelease
        Release
        (Rpc.ToResponse ReleaseResponse)
        Rpc.ToFault
    | MsgQuery
        (Query Proxy block)
        (Rpc.ToResponse (QueryResponse block))
        Rpc.ToFault

instance StandardHash block => Show (StateQueryMessage block) where
    showsPrec i = \case
        MsgAcquire acquire _ _ -> T.showParen (i >= 10)
            (T.showString $ "MsgAcquire " <> show acquire)
        MsgRelease release _ _ -> T.showParen (i >= 10)
            (T.showString $ "MsgRelease " <> show release)
        MsgQuery{} -> T.showParen (i >= 10)
            (T.showString "MsgQuery")

--
-- Acquire
--

data Acquire block
    = Acquire { point :: Point block }
    deriving (Generic, Show, Eq)

_encodeAcquire
    :: forall block. ()
    => (Point block -> Json)
    -> Rpc.Request (Acquire block)
    -> Json
_encodeAcquire encodePoint =
    Rpc.mkRequest Rpc.defaultOptions $ encodeObject . \case
        Acquire{point} ->
            "point" .=
                encodePoint point

_decodeAcquire
    :: FromJSON (Point block)
    => Json.Value
    -> Json.Parser (Rpc.Request (Acquire block))
_decodeAcquire =
    Rpc.genericFromJSON Rpc.defaultOptions

data AcquireResponse block
    = AcquireSuccess { point :: Point block }
    | AcquireFailure { failure :: AcquireFailure }
    deriving (Generic, Show)

_encodeAcquireResponse
    :: forall block. ()
    => (Point block -> Json)
    -> (AcquireFailure -> Json)
    -> Rpc.Response (AcquireResponse block)
    -> Json
_encodeAcquireResponse encodePoint encodeAcquireFailure =
    Rpc.mkResponse $ encodeObject . \case
        AcquireSuccess{point} ->
            "AcquireSuccess" .= encodeObject
                ( "point" .=
                    encodePoint point
                )
        AcquireFailure{failure} ->
            "AcquireFailure" .= encodeObject
                ( "failure" .=
                    encodeAcquireFailure failure
                )

--
-- Release
--

data Release
    = Release
    deriving (Generic, Show, Eq)

_encodeRelease
    :: Rpc.Request Release
    -> Json
_encodeRelease =
    Rpc.mkRequest Rpc.defaultOptions $ \case
        Release ->
            encodeObject mempty

_decodeRelease
    :: Json.Value
    -> Json.Parser (Rpc.Request Release)
_decodeRelease =
    Rpc.genericFromJSON Rpc.defaultOptions

data ReleaseResponse
    = Released
    deriving (Generic, Show)

_encodeReleaseResponse
    :: Rpc.Response ReleaseResponse
    -> Json
_encodeReleaseResponse =
    Rpc.mkResponse $ \case
        Released ->
            encodeText "Released"

--
-- Query
--

newtype QueryT block = Query { query :: Query Proxy block }
    deriving (Generic)

_decodeQuery
    :: FromJSON (Query Proxy block)
    => Json.Value
    -> Json.Parser (Rpc.Request (Query Proxy block))
_decodeQuery =
    fmap (fmap query) . Rpc.genericFromJSON Rpc.defaultOptions

data QueryResponse block
    = QueryResponse { unQueryResponse :: Json }
    | QueryUnavailableInCurrentEra
    | QueryAcquireFailure { failure :: AcquireFailure }
    deriving (Generic)

instance Show (QueryResponse block) where
    showsPrec i = T.showParen (i >=10) . T.showString . \case
        QueryResponse json ->
            "QueryResponse (" <> str json <> ")"
        QueryUnavailableInCurrentEra ->
            "QueryUnavailableInCurrentEra"
        QueryAcquireFailure failure ->
            "QueryAcquireFailure (" <> show failure <> ")"
      where
        str = decodeUtf8 . jsonToByteString

_encodeQueryResponse
    :: forall block. ()
    => (AcquireFailure -> Json)
    -> Rpc.Response (QueryResponse block)
    -> Json
_encodeQueryResponse encodeAcquireFailure =
    Rpc.mkResponse $ \case
        QueryResponse json ->
            json
        QueryUnavailableInCurrentEra ->
            encodeText "QueryUnavailableInCurrentEra"
        QueryAcquireFailure{failure} ->
            "AcquireFailure" .= encodeObject
                ( "failure" .=
                    encodeAcquireFailure failure
                )
            & encodeObject
