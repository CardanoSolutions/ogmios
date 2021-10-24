--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Query
    ( -- * Types
      Query (..)
    , QueryInEra
    , SomeQuery (..)
    , QueryResult

      -- ** Eras
    , ShelleyBasedEra (..)
    , SomeShelleyEra (..)
    , fromEraIndex

      -- ** Types in queries
    , RewardAccounts
    , Delegations
    , RewardProvenance
    , RewardProvenancePool
    , Desirability
    , PoolParams

      -- * Encoders
    , encodeEraMismatch
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint
    , encodeEpochNo

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
    , parseGetUTxOByAddress
    , parseGetUTxOByTxIn
    , parseGetGenesisConfig
    , parseGetRewardProvenance
    , parseGetPoolIds
    , parseGetPoolParameters
    , parseGetPoolsRanking
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Api
    ( ShelleyBasedEra (..) )
import Cardano.Crypto.Hash
    ( hashFromBytes, hashFromTextAsHex )
import Cardano.Ledger.Address
    ( Addr )
import Cardano.Ledger.Credential
    ( Credential )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Keys
    ( KeyRole (..) )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash )
import Cardano.Slotting.Slot
    ( EpochNo (..), WithOrigin (..) )
import Data.Aeson
    ( toJSON )
import Data.SOP.Strict
    ( NS (..) )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..), CardanoBlock, CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..), MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryAnytime (..) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..) )
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
import Shelley.Spec.Ledger.RewardProvenance
    ( Desirability (..), RewardProvenance (..), RewardProvenancePool (..) )
import Shelley.Spec.Ledger.TxBody
    ( PoolParams )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map.Merge.Strict as Map

import qualified Ouroboros.Consensus.Ledger.Query as LSQ

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Address as Address
import qualified Cardano.Ledger.Coin as Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Credential as Credential
import qualified Cardano.Ledger.Crypto as CC
import qualified Cardano.Ledger.Keys as Keys

import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.EpochBoundary as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.TxBody as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Types
--

data Query (f :: Type -> Type) block = Query
    { rawQuery :: Json.Value
    , queryInEra :: QueryInEra f block
    } deriving (Generic)

type QueryInEra f block =
    SomeShelleyEra -> Maybe (SomeQuery f block)

data SomeQuery (f :: Type -> Type) block = forall result. SomeQuery
    { query :: LSQ.Query block result
    , encodeResult :: result -> Json
    , genResult :: Proxy result -> f result
    }

instance Crypto crypto => FromJSON (Query Proxy (CardanoBlock crypto)) where
    parseJSON = choice "query"
        [ \raw -> Query raw <$> parseGetCurrentPParams (const id) raw
        , \raw -> Query raw <$> parseGetEpochNo id raw
        , \raw -> Query raw <$> parseGetEraStart id raw
        , \raw -> Query raw <$> parseGetFilteredDelegationsAndRewards id raw
        , \raw -> Query raw <$> parseGetGenesisConfig (const id) raw
        , \raw -> Query raw <$> parseGetLedgerTip (const id) raw
        , \raw -> Query raw <$> parseGetNonMyopicMemberRewards id raw
        , \raw -> Query raw <$> parseGetPoolIds id raw
        , \raw -> Query raw <$> parseGetPoolParameters id raw
        , \raw -> Query raw <$> parseGetPoolsRanking id raw
        , \raw -> Query raw <$> parseGetProposedPParamsUpdates (const id) raw
        , \raw -> Query raw <$> parseGetRewardProvenance id raw
        , \raw -> Query raw <$> parseGetStakeDistribution id raw
        , \raw -> Query raw <$> parseGetUTxO (const id) raw
        , \raw -> Query raw <$> parseGetUTxOByAddress (const id) raw
        , \raw -> Query raw <$> parseGetUTxOByTxIn (const id) raw
        ]

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

type GenResult crypto f t =
    Proxy (QueryResult crypto t) -> f (QueryResult crypto t)

type Delegations crypto =
    Map (Credential 'Staking crypto) (Keys.KeyHash 'StakePool crypto)

type RewardAccounts crypto =
    Map (Credential 'Staking crypto) Coin

--
-- SomeShelleyEra
--

data SomeShelleyEra =
    forall era. SomeShelleyEra (ShelleyBasedEra era)

deriving instance Show SomeShelleyEra

instance ToJSON SomeShelleyEra where
    toJSON = \case
        SomeShelleyEra ShelleyBasedEraShelley -> toJSON @Text "Shelley"
        SomeShelleyEra ShelleyBasedEraAllegra -> toJSON @Text "Allegra"
        SomeShelleyEra ShelleyBasedEraMary -> toJSON @Text "Mary"
        SomeShelleyEra ShelleyBasedEraAlonzo -> toJSON @Text "Alonzo"

-- | Convert an 'EraIndex' to a Shelley-based era.
fromEraIndex
    :: forall crypto. ()
    => EraIndex (CardanoEras crypto)
    -> Maybe SomeShelleyEra
fromEraIndex = \case
    EraIndex             Z{}     -> Nothing
    EraIndex          (S Z{})    -> Just (SomeShelleyEra ShelleyBasedEraShelley)
    EraIndex       (S (S Z{}))   -> Just (SomeShelleyEra ShelleyBasedEraAllegra)
    EraIndex    (S (S (S Z{})))  -> Just (SomeShelleyEra ShelleyBasedEraMary)
    EraIndex (S (S (S (S Z{})))) -> Just (SomeShelleyEra ShelleyBasedEraAlonzo)

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

encodeRewardProvenance
    :: RewardProvenance crypto
    -> Json
encodeRewardProvenance rp =
    encodeObject
        [ ( "epochLength"
          , encodeWord64 (spe rp)
          )
        , ( "decentralizationParameter"
          , encodeRational (d rp)
          )
        , ( "maxLovelaceSupply"
          , encodeCoin (maxLL rp)
          )
        , ( "mintedBlocks"
          , encodeMap Shelley.stringifyPoolId encodeNatural (Sh.unBlocksMade $ blocks rp)
          )
        , ( "totalMintedBlocks"
          , encodeInteger (blocksCount rp)
          )
        , ( "totalExpectedBlocks"
          , encodeInteger (expBlocks rp)
          )
        , ( "incentive"
          , encodeCoin (deltaR1 rp)
          )
        , ( "rewardsGap"
          , encodeCoin (deltaR2 rp)
          )
        , ( "availableRewards"
          , encodeCoin (r rp)
          )
        , ( "totalRewards"
          , encodeCoin (rPot rp)
          )
        , ( "treasuryTax"
          , encodeCoin (deltaT1 rp)
          )
        , ( "activeStake"
          , encodeCoin (activeStake rp)
          )
        , ( "pools"
          , encodeMap Shelley.stringifyPoolId encodeRewardProvenancePool (pools rp)
          )
        ]

encodeRewardProvenancePool
    :: RewardProvenancePool crypto
    -> Json
encodeRewardProvenancePool rpp =
    encodeObject
        [ ( "totalMintedBlocks"
          , encodeNatural (poolBlocksP rpp)
          )
        , ( "totalStakeShare"
          , encodeRational (sigmaP rpp)
          )
        , ( "activeStakeShare"
          , encodeRational (sigmaAP rpp)
          )
        , ( "ownerStake"
          , encodeCoin (ownerStakeP rpp)
          )
        , ( "parameters"
          , Shelley.encodePoolParams (poolParamsP rpp)
          )
        , ( "pledgeRatio"
          , encodeRational (pledgeRatioP rpp)
          )
        , ( "maxRewards"
          , encodeCoin (maxPP rpp)
          )
        , ( "apparentPerformance"
          , encodeRational (appPerfP rpp)
          )
        , ( "totalRewards"
          , encodeCoin (poolRP rpp)
          )
        , ( "leaderRewards"
          , encodeCoin (lRewardP rpp)
          )
        ]

encodeDesirabilities
    :: RewardProvenance crypto
    -> Json
encodeDesirabilities rp =
    encodeMap Shelley.stringifyPoolId encodeDesirability (desirabilities rp)

encodeDesirability
    :: Desirability
    -> Json
encodeDesirability d =
    encodeObject
        [ ( "score", encodeDouble (desirabilityScore d) )
        , ( "estimatedHitRate", encodeDouble (desirabilityScore d) )
        ]

--
-- Parsers (Queries)
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
                    LSQ.BlockQuery $ QueryAnytimeShelley GetEraStart
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryAnytimeAllegra GetEraStart
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryAnytimeMary GetEraStart
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryAnytimeAlonzo GetEraStart
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetLedgerTip
                , encodeResult =
                    either encodeMismatchEraInfo (encodePoint . castPoint)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetLedgerTip
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetEpochNo
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetEpochNo
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetEpochNo
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetEpochNo
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetNonMyopicMemberRewards credentials)
            )
  where
    parseCredentials
        :: Json.Object
        -> Json.Parser (Set (Either Coin (Credential 'Staking crypto)))
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetFilteredDelegationsAndRewardAccounts credentials)
            )
  where
    parseCredentials
        :: Json.Object
        -> Json.Parser (Set (Credential 'Staking crypto))
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Shelley.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Allegra.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetCurrentPParams
                , encodeResult =
                    either encodeMismatchEraInfo (Mary.encodePParams' id)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetCurrentPParams
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetProposedPParamsUpdates
                , encodeResult =
                    either encodeMismatchEraInfo Alonzo.encodeProposedPPUpdates
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetStakeDistribution
    :: forall crypto f. ()
    => GenResult crypto f (Sh.PoolDistr crypto)
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetStakeDistribution
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetStakeDistribution
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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo Shelley.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo Allegra.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo Mary.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo Alonzo.encodeUtxo
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetUTxOByAddress
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxOByAddress genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    addrs <- parseAddresses obj
    pure $ \case
        SomeShelleyEra ShelleyBasedEraShelley ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentShelley (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo Shelley.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(ShelleyEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo Allegra.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraMary ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentMary (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo Mary.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(MaryEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAlonzo ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo Alonzo.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(AlonzoEra crypto))
            }
  where
    parseAddresses
        :: Json.Object
        -> Json.Parser (Set (Addr crypto))
    parseAddresses obj = fmap fromList $
        obj .: "utxo" >>= traverse parseAddress

parseGetUTxOByTxIn
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxOByTxIn genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    ins <- parseTxIns obj
    pure $ \case
        SomeShelleyEra ShelleyBasedEraShelley ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentShelley (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo Shelley.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(ShelleyEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo Allegra.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraMary ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentMary (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo Mary.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(MaryEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAlonzo ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo Alonzo.encodeUtxo
            , genResult =
                genResultInEra (Proxy @(AlonzoEra crypto))
            }
  where
    parseTxIns
        :: Json.Object
        -> Json.Parser (Set (Sh.TxIn crypto))
    parseTxIns obj = fmap fromList $
        obj .: "utxo" >>= traverse parseTxIn

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
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetGenesisConfig
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
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetGenesisConfig
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
                    LSQ.BlockQuery $ QueryIfCurrentMary GetGenesisConfig
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
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Shelley.encodeGenesis . getCompactGenesis
                     in either encodeMismatchEraInfo encodeGenesis
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetRewardProvenance
    :: forall crypto f. ()
    => GenResult crypto f (RewardProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetRewardProvenance genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "rewardsProvenance") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo encodeRewardProvenance
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetRewardProvenance
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetRewardProvenance
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetRewardProvenance
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetRewardProvenance
            )

parseGetPoolIds
    :: forall crypto f. ()
    => GenResult crypto f (Set (Keys.KeyHash 'StakePool crypto))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolIds genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "poolIds") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo (encodeFoldable Shelley.encodePoolId)
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetStakePools
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetStakePools
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetStakePools
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetStakePools
            )

parseGetPoolParameters
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Map (Keys.KeyHash 'StakePool crypto) (PoolParams crypto))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolParameters genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        ids <- parsePoolIds obj
        pure $
            (\query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either
                        encodeMismatchEraInfo
                        (encodeMap Shelley.stringifyPoolId Shelley.encodePoolParams)
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley (GetStakePoolParams ids)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra (GetStakePoolParams ids)
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary (GetStakePoolParams ids)
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetStakePoolParams ids)
            )
  where
    parsePoolIds
        :: Json.Object
        -> Json.Parser (Set (Keys.KeyHash 'StakePool crypto))
    parsePoolIds obj = fmap fromList $
        obj .: "poolParameters" >>= traverse parsePoolId

parseGetPoolsRanking
    :: forall crypto f. ()
    => GenResult crypto f (RewardProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolsRanking genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "poolsRanking") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo encodeDesirabilities
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetRewardProvenance
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetRewardProvenance
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetRewardProvenance
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetRewardProvenance
            )

--
-- Parsers (Others)
--

parseAddress
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Addr crypto)
parseAddress = Json.withText "Address" $ choice "address"
    [ addressFromBytes fromBech32
    , addressFromBytes fromBase58
    , addressFromBytes fromBase16
    ]
  where
    addressFromBytes decode =
        decode >=> maybe mempty pure . Address.deserialiseAddr

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe mempty pure $ Bech32.dataPartToBytes dataPart

    fromBase58 =
        decodeBase58 . encodeUtf8

    fromBase16 =
        decodeBase16 . encodeUtf8

parseCoin
    :: Json.Value
    -> Json.Parser Coin
parseCoin =
    fmap Coin.word64ToCoin . Json.parseJSON

-- TODO: Makes it possible to distinguish between KeyHash and ScriptHash
-- credentials. Both are encoded as hex-encoded strings. Encoding them as
-- object is ill-advised because we also need them as key of the non-myopic
-- member rewards map.
--
-- A possible option: encode them as Bech32 strings with different prefixes.
parseCredential
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Credential 'Staking crypto)
parseCredential =
    fmap (Credential.KeyHashObj . Keys.KeyHash) . parseHash

parseHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex

parsePoolId
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Keys.KeyHash 'StakePool crypto)
parsePoolId = Json.withText "PoolId" $ choice "poolId"
    [ poolIdFromBytes fromBech32
    , poolIdFromBytes fromBase16
    ]
  where
    poolIdFromBytes decode =
        decode >=> maybe empty (pure . Keys.KeyHash) . hashFromBytes

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe empty pure $ Bech32.dataPartToBytes dataPart

    fromBase16 =
        decodeBase16 . encodeUtf8

parseTxIn
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Sh.TxIn crypto)
parseTxIn = Json.withObject "TxIn" $ \o -> do
    txid <- o .: "txId" >>= fromBase16
    ix <- o .: "index"
    pure $ Sh.TxIn (Sh.TxId txid) ix
  where
    fromBase16 =
        maybe empty (pure . unsafeMakeSafeHash) . hashFromTextAsHex @(CC.HASH crypto)
