--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Query
    ( -- * Types
      QueryResult
    , QueryInEra
    , SomeQuery (..)
    , SomeShelleyEra (..)
    , ShelleyBasedEra (..)

      -- * Encoders
    , encodeEraMismatch
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint

      -- * Parsers
    , parseGetEraStart
    , parseGetLedgerTip
    , parseGetEpochNo
    , parseGetNonMyopicMemberRewards
    , parseGetCurrentPParams
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    , parseGetFilteredUTxO
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Api.Eras
    ( ShelleyBasedEra (..) )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Slotting.Slot
    ( EpochNo (..), WithOrigin (..) )
import Data.Aeson
    ( (.:) )
import Data.ByteString.Base16
    ( decodeBase16 )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, CardanoEras, Query (..) )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryAnytime (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( AllegraEra, MaryEra, ShelleyEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( NonMyopicMemberRewards (..), Query (..) )
import Ouroboros.Network.Block
    ( Point (..), castPoint )
import Ouroboros.Network.Point
    ( Block (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Shelley.Spec.Ledger.Address as Sh
import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Coin as Sh
import qualified Shelley.Spec.Ledger.Credential as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.Keys as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Encoders
--

encodeBound
    :: Bound
    -> Json
encodeBound bound = encodeObject
    [ ( "time", encodeRelativeTime (boundTime bound) )
    , ( "slot", encodeSlotNo (boundSlot bound) )
    , ( "epoch", encodeEpochNo (boundEpoch bound) )
    ]

encodeEraMismatch
    :: EraMismatch
    -> Json
encodeEraMismatch x = encodeObject
    [ ( "eraMismatch", encodeObject
        [ ( "ledgerEra"
          , encodeText (ledgerEraName x)
          )
        , ( "queryEra"
          , encodeText (otherEraName x)
          )
        ]
      )
    ]

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeNonMyopicMemberRewards
    :: NonMyopicMemberRewards era
    -> Json
encodeNonMyopicMemberRewards (NonMyopicMemberRewards nonMyopicMemberRewards) =
    encodeMap
        (either Shelley.stringifyCoin Shelley.stringifyCredential)
        (encodeMap Shelley.stringifyPoolId Shelley.encodeCoin)
        nonMyopicMemberRewards

encodeOneEraHash
    :: OneEraHash eras
    -> Json
encodeOneEraHash =
    encodeShortByteString encodeByteStringBase16 . getOneEraHash

encodePoint
    :: Point (CardanoBlock crypto)
    -> Json
encodePoint = \case
    Point Origin -> encodeText "origin"
    Point (At x) -> encodeObject
        [ ( "slot"
          , encodeSlotNo (blockPointSlot x)
          )
        , ( "hash"
          , encodeOneEraHash (blockPointHash x)
          )
        ]

--
-- Parsers
--

parseGetEraStart
    :: forall crypto f. ()
    => (Proxy (Maybe Bound) -> f (Maybe Bound))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetEraStart genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "eraStart") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    encodeMaybe encodeBound
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    QueryAnytimeShelley GetEraStart
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    QueryAnytimeAllegra GetEraStart
                SomeShelleyEra ShelleyBasedEraMary ->
                    QueryAnytimeMary GetEraStart
            )

parseGetLedgerTip
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (ShelleyBlock era)))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetLedgerTip genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentShelley GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentAllegra GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentMary GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }

parseGetEpochNo
    :: forall crypto f. ()
    => GenResult crypto f EpochNo
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetEpochNo genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentEpoch") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo encodeEpochNo
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    QueryIfCurrentShelley GetEpochNo
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    QueryIfCurrentAllegra GetEpochNo
                SomeShelleyEra ShelleyBasedEraMary ->
                    QueryIfCurrentMary GetEpochNo
            )

parseGetNonMyopicMemberRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (NonMyopicMemberRewards crypto)
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetNonMyopicMemberRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        credentials <- parseCredentials obj
        pure $
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo encodeNonMyopicMemberRewards
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    QueryIfCurrentShelley (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    QueryIfCurrentAllegra (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraMary ->
                    QueryIfCurrentMary (GetNonMyopicMemberRewards credentials)
            )
  where
    parseCredentials
        :: Json.Object
        -> Json.Parser (Set (Either Sh.Coin (Sh.Credential 'Sh.Staking crypto)))
    parseCredentials obj = fmap fromList $
        obj .: "nonMyopicMemberRewards" >>= traverse
            (choice "credential"
                [ fmap Left  . parseCoin
                , fmap Right . parseCredential
                ]
            )

parseGetCurrentPParams
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.PParams era))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetCurrentPParams genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentProtocolParameters") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentShelley GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Shelley.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentAllegra GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Allegra.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentMary GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Mary.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }

parseGetProposedPParamsUpdates
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.ProposedPPUpdates era))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetProposedPParamsUpdates genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "proposedProtocolParameters") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentShelley GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentAllegra GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentMary GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }

parseGetStakeDistribution
    :: forall crypto f. ()
    => (GenResult crypto f (Sh.PoolDistr crypto))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetStakeDistribution genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "stakeDistribution") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodePoolDistr
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    QueryIfCurrentShelley GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    QueryIfCurrentAllegra GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraMary ->
                    QueryIfCurrentMary GetStakeDistribution
            )

parseGetUTxO
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxO genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "utxo") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentShelley GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentAllegra GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    QueryIfCurrentMary GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }

parseGetFilteredUTxO
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetFilteredUTxO genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    addrs <- parseAddresses obj
    pure $ \case
        SomeShelleyEra ShelleyBasedEraShelley ->
            Just $ SomeQuery
                { query =
                    QueryIfCurrentShelley (GetFilteredUTxO addrs)
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
        SomeShelleyEra ShelleyBasedEraAllegra ->
            Just $ SomeQuery
                { query =
                    QueryIfCurrentAllegra(GetFilteredUTxO addrs)
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
        SomeShelleyEra ShelleyBasedEraMary ->
            Just $ SomeQuery
                { query =
                    QueryIfCurrentMary (GetFilteredUTxO addrs)
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
  where
    parseAddresses
        :: Json.Object
        -> Json.Parser (Set (Sh.Addr crypto))
    parseAddresses obj = fmap fromList $
        obj .: "utxo" >>= traverse parseAddress

--
-- Internal
--

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

type GenResult crypto f t =
    Proxy (QueryResult crypto t) -> f (QueryResult crypto t)

type QueryInEra f block =
    SomeShelleyEra -> Maybe (SomeQuery f block)

data SomeShelleyEra =
    forall era. SomeShelleyEra (ShelleyBasedEra era)

data SomeQuery (f :: * -> *) block = forall result. SomeQuery
    { query :: Query block result
    , encodeResult :: result -> Json
    , genResult :: Proxy result -> f result
    }

parseAddress
    :: Crypto crypto
    => Json
    -> Json.Parser (Sh.Addr crypto)
parseAddress = Json.withText "Address" $ choice "address"
    [ addressFromBytes fromBech32
    , addressFromBytes fromBase58
    , addressFromBytes fromBase16
    ]
  where
    addressFromBytes decode =
        decode >=> maybe mempty pure . Sh.deserialiseAddr

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe mempty pure $ Bech32.dataPartToBytes dataPart

    fromBase58 =
        maybe mempty pure . decodeBase58 bitcoinAlphabet . encodeUtf8

    fromBase16 =
        either (fail . show) pure . decodeBase16 . encodeUtf8

parseCoin
    :: Json
    -> Json.Parser Sh.Coin
parseCoin =
    fmap Sh.word64ToCoin . Json.parseJSON

-- TODO: Makes it possible to distinguish between KeyHash and ScriptHash
-- credentials. Both are encoded as hex-encoded strings. Encoding them as
-- object is ill-advised because we also need them as key of the non-myopic
-- member rewards map.
--
-- A possible option: encode them as Bech32 strings with different prefixes.
parseCredential
    :: Crypto crypto
    => Json
    -> Json.Parser (Sh.Credential 'Sh.Staking crypto)
parseCredential =
    fmap (Sh.KeyHashObj . Sh.KeyHash) . parseHash

parseHash
    :: CC.HashAlgorithm alg
    => Json
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex
