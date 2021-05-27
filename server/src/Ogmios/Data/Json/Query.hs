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
    , RewardAccounts
    , Delegations

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
    , parseGetFilteredDelegationsAndRewards
    , parseGetCurrentPParams
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    , parseGetFilteredUTxO
    , parseGetGenesisConfig
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Api
    ( ShelleyBasedEra (..) )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Slotting.Slot
    ( EpochNo (..), WithOrigin (..) )
import Data.ByteString.Base16
    ( decodeBase16 )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..), CardanoBlock, CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryAnytime (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( AllegraEra, AlonzoEra, MaryEra, ShelleyEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CompactGenesis, getCompactGenesis )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..), NonMyopicMemberRewards (..) )
import Ouroboros.Network.Block
    ( Point (..), castPoint )
import Ouroboros.Network.Point
    ( Block (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map.Merge.Strict as Map

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Coin as Sh
import qualified Cardano.Ledger.Core as Core
import qualified Shelley.Spec.Ledger.Address as Sh
import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Credential as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.Keys as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
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

encodeDelegationsAndRewards
    :: (Delegations crypto, RewardAccounts crypto)
    -> Json
encodeDelegationsAndRewards (dlg, rwd)
    = encodeMap Shelley.stringifyCredential id
    $ Map.merge whenDlgMissing whenRwdMissing whenBothPresent dlg rwd
  where
    whenDlgMissing = Map.mapMaybeMissing
        (\_ v -> Just $ encodeObject
            [ ( "delegate", Shelley.encodePoolId v )
            ]
        )
    whenRwdMissing = Map.mapMaybeMissing
        (\_ v -> Just $ encodeObject
            [ ( "rewards", encodeCoin v )
            ]
        )
    whenBothPresent = Map.zipWithAMatched
        (\_ x y -> pure $ encodeObject
            [ ( "delegate", Shelley.encodePoolId x )
            , ( "rewards", encodeCoin y )
            ]
        )

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
        (encodeMap Shelley.stringifyPoolId encodeCoin)
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
    -> Json.Value
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
                    BlockQuery $ QueryAnytimeShelley GetEraStart
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    BlockQuery $ QueryAnytimeAllegra GetEraStart
                SomeShelleyEra ShelleyBasedEraMary ->
                    BlockQuery $ QueryAnytimeMary GetEraStart
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    BlockQuery $ QueryAnytimeAlonzo GetEraStart
            )

parseGetLedgerTip
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (ShelleyBlock era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetLedgerTip genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentShelley GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAllegra GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentMary GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAlonzo GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetEpochNo
    :: forall crypto f. ()
    => GenResult crypto f EpochNo
    -> Json.Value
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
                    BlockQuery $ QueryIfCurrentShelley GetEpochNo
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    BlockQuery $ QueryIfCurrentAllegra GetEpochNo
                SomeShelleyEra ShelleyBasedEraMary ->
                    BlockQuery $ QueryIfCurrentMary GetEpochNo
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    BlockQuery $ QueryIfCurrentAlonzo GetEpochNo
            )

parseGetNonMyopicMemberRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (NonMyopicMemberRewards crypto)
    -> Json.Value
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
                    BlockQuery $ QueryIfCurrentShelley (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    BlockQuery $ QueryIfCurrentAllegra (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraMary ->
                    BlockQuery $ QueryIfCurrentMary (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    BlockQuery $ QueryIfCurrentAlonzo (GetNonMyopicMemberRewards credentials)
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

parseGetFilteredDelegationsAndRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Delegations crypto, RewardAccounts crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetFilteredDelegationsAndRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        credentials <- parseCredentials obj
        pure $
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo encodeDelegationsAndRewards
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    BlockQuery $ QueryIfCurrentShelley (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    BlockQuery $ QueryIfCurrentAllegra (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeShelleyEra ShelleyBasedEraMary ->
                    BlockQuery $ QueryIfCurrentMary (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    BlockQuery $ QueryIfCurrentAlonzo (GetFilteredDelegationsAndRewardAccounts credentials)
            )
  where
    parseCredentials
        :: Json.Object
        -> Json.Parser (Set (Sh.Credential 'Sh.Staking crypto))
    parseCredentials obj = fmap fromList $
        obj .: "delegationsAndRewards" >>= traverse parseCredential

parseGetCurrentPParams
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Core.PParams era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetCurrentPParams genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentProtocolParameters") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentShelley GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Shelley.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAllegra GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Allegra.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentMary GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Mary.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAlonzo GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Alonzo.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetProposedPParamsUpdates
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.ProposedPPUpdates era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetProposedPParamsUpdates genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "proposedProtocolParameters") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentShelley GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAllegra GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentMary GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAlonzo GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Alonzo.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetStakeDistribution
    :: forall crypto f. ()
    => (GenResult crypto f (Sh.PoolDistr crypto))
    -> Json.Value
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
                    BlockQuery $ QueryIfCurrentShelley GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    BlockQuery $ QueryIfCurrentAllegra GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraMary ->
                    BlockQuery $ QueryIfCurrentMary GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    BlockQuery $ QueryIfCurrentAlonzo GetStakeDistribution
            )

parseGetUTxO
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxO genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "utxo") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentShelley GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAllegra GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentMary GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAlonzo GetUTxO
                , encodeResult =
                    either encodeMismatchEraInfo Alonzo.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetFilteredUTxO
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetFilteredUTxO genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    addrs <- parseAddresses obj
    pure $ \case
        SomeShelleyEra ShelleyBasedEraShelley ->
            Just $ SomeQuery
            { query =
                BlockQuery $ QueryIfCurrentShelley (GetFilteredUTxO addrs)
            , encodeResult =
                either encodeMismatchEraInfo Shelley.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(ShelleyEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                BlockQuery $ QueryIfCurrentAllegra (GetFilteredUTxO addrs)
            , encodeResult =
                either encodeMismatchEraInfo Allegra.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraMary ->
            Just $ SomeQuery
            { query =
                BlockQuery $ QueryIfCurrentMary (GetFilteredUTxO addrs)
            , encodeResult =
                either encodeMismatchEraInfo Mary.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(MaryEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAlonzo ->
            Just $ SomeQuery
            { query =
                BlockQuery $ QueryIfCurrentAlonzo (GetFilteredUTxO addrs)
            , encodeResult =
                either encodeMismatchEraInfo Alonzo.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(AlonzoEra crypto))
            }
  where
    parseAddresses
        :: Json.Object
        -> Json.Parser (Set (Sh.Addr crypto))
    parseAddresses obj = fmap fromList $
        obj .: "utxo" >>= traverse parseAddress

-- TODO: This query seems to actually always return a compact version of the
-- `ShelleyGenesis`, even when queried from Alonzo. While this is practical
-- (because the return type does not change when crossing eras), there's also no
-- way currently to retrieve an `AlonzoGenesis` ¯\_(ツ)_/¯
--
-- If this query is indeed meant to only return ShelleyGenesis, renaming it to
-- something which suggests it better would make sense. I've asked *the guys*.
parseGetGenesisConfig
    :: forall f crypto. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (CompactGenesis era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetGenesisConfig genResultInEra = do
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "genesisConfig") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentShelley GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Shelley.encodeGenesis . getCompactGenesis
                    in either encodeMismatchEraInfo encodeGenesis
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAllegra GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Shelley.encodeGenesis . getCompactGenesis
                    in either encodeMismatchEraInfo encodeGenesis
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentMary GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Shelley.encodeGenesis . getCompactGenesis
                    in either encodeMismatchEraInfo encodeGenesis
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    BlockQuery $ QueryIfCurrentAlonzo GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Shelley.encodeGenesis . getCompactGenesis
                     in either encodeMismatchEraInfo encodeGenesis
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

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

type Delegations crypto =
    Map (Sh.Credential 'Sh.Staking crypto) (Sh.KeyHash 'Sh.StakePool crypto)

type RewardAccounts crypto =
    Map (Sh.Credential 'Sh.Staking crypto) Sh.Coin

data SomeQuery (f :: Type -> Type) block = forall result. SomeQuery
    { query :: Query block result
    , encodeResult :: result -> Json
    , genResult :: Proxy result -> f result
    }

parseAddress
    :: Crypto crypto
    => Json.Value
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
    :: Json.Value
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
    => Json.Value
    -> Json.Parser (Sh.Credential 'Sh.Staking crypto)
parseCredential =
    fmap (Sh.KeyHashObj . Sh.KeyHash) . parseHash

parseHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex
