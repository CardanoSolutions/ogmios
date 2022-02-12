--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
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
    , Interpreter
    , Sh.RewardProvenance
    , Sh.RewardProvenancePool
    , RewardProvenance'
    , Sh.Api.RewardInfoPool
    , Sh.Api.RewardParams
    , Sh.Desirability
    , Sh.PoolParams

      -- * Encoders
    , encodeBound
    , encodeDelegationsAndRewards
    , encodeDesirabilities
    , encodeEpochNo
    , encodeEraMismatch
    , encodeInterpreter
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint
    , encodePoolDistr
    , encodePoolParameters
    , encodeRewardInfoPool
    , encodeRewardInfoPools
    , encodeRewardProvenance

      -- * Parsers
    , parseGetBlockHeight
    , parseGetChainTip
    , parseGetCurrentPParams
    , parseGetEpochNo
    , parseGetEraStart
    , parseGetFilteredDelegationsAndRewards
    , parseGetGenesisConfig
    , parseGetInterpreter
    , parseGetLedgerTip
    , parseGetNonMyopicMemberRewards
    , parseGetPoolIds
    , parseGetPoolParameters
    , parseGetPoolsRanking
    , parseGetProposedPParamsUpdates
    , parseGetRewardInfoPools
    , parseGetRewardProvenance
    , parseGetStakeDistribution
    , parseGetSystemStart
    , parseGetUTxO
    , parseGetUTxOByAddress
    , parseGetUTxOByTxIn
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Api
    ( ShelleyBasedEra (..) )
import Cardano.Crypto.Hash
    ( pattern UnsafeHash, hashFromBytes, hashFromTextAsHex )
import Cardano.Ledger.Crypto
    ( Crypto, HASH )
import Cardano.Ledger.Keys
    ( KeyRole (..) )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash )
import Cardano.Slotting.Slot
    ( EpochNo (..), WithOrigin (..) )
import Codec.Serialise
    ( deserialise, serialise )
import Data.Aeson
    ( toJSON )
import Data.SOP.Strict
    ( NS (..) )
import Ouroboros.Consensus.BlockchainTime
    ( SystemStart (..) )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..), CardanoBlock, CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..), MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryAnytime (..) )
import Ouroboros.Consensus.HardFork.History.EraParams
    ( EraParams (..), SafeZone (..) )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Interpreter )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..), EraEnd (..), EraSummary (..), Summary (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( AllegraEra, AlonzoEra, MaryEra, ShelleyEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CompactGenesis, getCompactGenesis )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..), NonMyopicMemberRewards (..) )
import Ouroboros.Network.Block
    ( BlockNo, pattern BlockPoint, pattern GenesisPoint, Point (..) )
import Ouroboros.Network.Point
    ( Block (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map.Merge.Strict as Map

import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Query as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as LSQ

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Ledger.Shelley.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.RewardProvenance as Sh
import qualified Cardano.Ledger.Shelley.TxBody as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

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
    , encodeResult :: SerializationMode -> result -> Json
    , genResult :: Proxy result -> f result
    }

instance Crypto crypto => FromJSON (Query Proxy (CardanoBlock crypto)) where
    parseJSON = choice "query"
        [ \raw -> Query raw <$> parseGetBlockHeight id raw
        , \raw -> Query raw <$> parseGetChainTip id raw
        , \raw -> Query raw <$> parseGetCurrentPParams (const id) raw
        , \raw -> Query raw <$> parseGetEpochNo id raw
        , \raw -> Query raw <$> parseGetEraStart id raw
        , \raw -> Query raw <$> parseGetFilteredDelegationsAndRewards id raw
        , \raw -> Query raw <$> parseGetGenesisConfig (const id) raw
        , \raw -> Query raw <$> parseGetInterpreter id raw
        , \raw -> Query raw <$> parseGetLedgerTip (const id) raw
        , \raw -> Query raw <$> parseGetNonMyopicMemberRewards id raw
        , \raw -> Query raw <$> parseGetPoolIds id raw
        , \raw -> Query raw <$> parseGetPoolParameters id raw
        , \raw -> Query raw <$> parseGetPoolsRanking id raw
        , \raw -> Query raw <$> parseGetProposedPParamsUpdates (const id) raw
        , \raw -> Query raw <$> parseGetRewardInfoPools id raw
        , \raw -> Query raw <$> parseGetRewardProvenance id raw
        , \raw -> Query raw <$> parseGetStakeDistribution id raw
        , \raw -> Query raw <$> parseGetSystemStart id raw
        , \raw -> Query raw <$> parseGetUTxO (const id) raw
        , \raw -> Query raw <$> parseGetUTxOByAddress (const id) raw
        , \raw -> Query raw <$> parseGetUTxOByTxIn (const id) raw
        ]

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

type GenResult crypto f t =
    Proxy (QueryResult crypto t) -> f (QueryResult crypto t)

type Delegations crypto =
    Map (Ledger.Credential 'Staking crypto) (Ledger.KeyHash 'StakePool crypto)

type RewardAccounts crypto =
    Map (Ledger.Credential 'Staking crypto) Coin

type RewardProvenance' crypto =
    ( Sh.Api.RewardParams
    , Map (Ledger.KeyHash 'StakePool crypto) (Sh.Api.RewardInfoPool)
    )


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
    :: Crypto crypto
    => SerializationMode
    -> (Delegations crypto, RewardAccounts crypto)
    -> Json
encodeDelegationsAndRewards mode (dlg, rwd) =
    encodeMapWithMode mode Shelley.stringifyCredential id merge
  where
    merge = Map.merge whenDlgMissing whenRwdMissing whenBothPresent dlg rwd

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

encodeDesirabilities
    :: Crypto crypto
    => SerializationMode
    -> Sh.RewardProvenance crypto
    -> Json
encodeDesirabilities mode rp =
    encodeMapWithMode mode Shelley.stringifyPoolId encodeDesirability (Sh.desirabilities rp)
  where
    encodeDesirability
        :: Sh.Desirability
        -> Json
    encodeDesirability d =
        encodeObject
            [ ( "score", encodeDouble (Sh.desirabilityScore d) )
            , ( "estimatedHitRate", encodeDouble (Sh.desirabilityScore d) )
            ]

encodeEraEnd
    :: EraEnd
    -> Json
encodeEraEnd = \case
    EraEnd bound ->
        encodeBound bound
    EraUnbounded ->
        encodeNull

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

encodeEraParams
    :: EraParams
    -> Json
encodeEraParams x = encodeObject
    [ ( "epochLength", encodeEpochSize (eraEpochSize x) )
    , ( "slotLength", encodeSlotLength (eraSlotLength x) )
    , ( "safeZone", encodeSafeZone (eraSafeZone x) )
    ]

encodeEraSummary
    :: EraSummary
    -> Json
encodeEraSummary x = encodeObject
    [ ( "start", encodeBound (eraStart x) )
    , ( "end", encodeEraEnd (eraEnd x) )
    , ( "parameters", encodeEraParams (eraParams x) )
    ]

encodeInterpreter
    :: forall crypto eras. (eras ~ CardanoEras crypto)
    => Interpreter eras
    -> Json
encodeInterpreter (deserialise @(Summary eras). serialise -> Summary eraSummaries) =
    encodeFoldable encodeEraSummary (eraSummaries)

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeNonMyopicMemberRewards
    :: Crypto crypto
    => SerializationMode
    -> NonMyopicMemberRewards crypto
    -> Json
encodeNonMyopicMemberRewards mode (NonMyopicMemberRewards nonMyopicMemberRewards) =
    encodeMapWithMode mode encodeKey encodeVal nonMyopicMemberRewards
  where
    encodeKey = either Shelley.stringifyCoin Shelley.stringifyCredential
    encodeVal = encodeMapWithMode mode Shelley.stringifyPoolId encodeCoin

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

encodePoolDistr
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Ledger.PoolDistr crypto
    -> Json
encodePoolDistr mode
    = encodeMapWithMode mode Shelley.stringifyPoolId encodeIndividualPoolStake
    . Ledger.unPoolDistr
  where
    encodeIndividualPoolStake
        :: Ledger.IndividualPoolStake crypto
        -> Json
    encodeIndividualPoolStake x = encodeObject
        [ ( "stake"
          , encodeRational (Ledger.individualPoolStake x)
          )
        , ( "vrf"
          , Shelley.encodeHash (Ledger.individualPoolStakeVrf x)
          )
        ]

encodePoolParameters
    :: Crypto crypto
    => SerializationMode
    -> Map (Ledger.KeyHash 'StakePool crypto) (Sh.PoolParams crypto)
    -> Json
encodePoolParameters mode =
    encodeMapWithMode mode Shelley.stringifyPoolId Shelley.encodePoolParams

encodeRewardInfoPool
    :: Sh.Api.RewardInfoPool
    -> Json
encodeRewardInfoPool info =
    encodeObject
        [ ( "stake"
          , encodeCoin (Sh.Api.stake info)
          )
        , ( "ownerStake"
          , encodeCoin (Sh.Api.ownerStake info)
          )
        , ( "approximatePerformance"
          , encodeDouble (Sh.Api.performanceEstimate info)
          )
        , ( "poolParameters"
          , encodeObject
            [ ( "cost"
              , encodeCoin (Sh.Api.cost info)
              )
            , ( "margin"
              , encodeUnitInterval (Sh.Api.margin info)
              )
            , ( "pledge"
              , encodeCoin (Sh.Api.ownerPledge info)
              )
            ]
          )
        ]

encodeRewardInfoPools
    :: Crypto crypto
    => RewardProvenance' crypto
    -> Json
encodeRewardInfoPools (rp, pools) =
    encodeObject
        [ ( "desiredNumberOfPools"
          , encodeNatural (Sh.Api.nOpt rp)
          )
        , ( "poolInfluence"
          , encodeNonNegativeInterval (Sh.Api.a0 rp)
          )
        , ( "totalRewards"
          , encodeCoin (Sh.Api.rPot rp)
          )
        , ( "activeStake"
          , encodeCoin (Sh.Api.totalStake rp)
          )
        , ( "pools"
          , encodeMap Shelley.stringifyPoolId encodeRewardInfoPool pools
          )
        ]

encodeRewardProvenance
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Sh.RewardProvenance crypto
    -> Json
encodeRewardProvenance mode rp =
    encodeObjectWithMode mode
        [ ( "epochLength"
          , encodeWord64 (Sh.spe rp)
          )
        , ( "decentralizationParameter"
          , encodeRational (Sh.d rp)
          )
        , ( "maxLovelaceSupply"
          , encodeCoin (Sh.maxLL rp)
          )
        , ( "totalMintedBlocks"
          , encodeInteger (Sh.blocksCount rp)
          )
        , ( "totalExpectedBlocks"
          , encodeInteger (Sh.expBlocks rp)
          )
        , ( "incentive"
          , encodeCoin (Sh.deltaR1 rp)
          )
        , ( "rewardsGap"
          , encodeCoin (Sh.deltaR2 rp)
          )
        , ( "availableRewards"
          , encodeCoin (Sh.r rp)
          )
        , ( "totalRewards"
          , encodeCoin (Sh.rPot rp)
          )
        , ( "treasuryTax"
          , encodeCoin (Sh.deltaT1 rp)
          )
        , ( "activeStake"
          , encodeCoin (Sh.activeStake rp)
          )
        ]
        [ ( "pools"
          , encodeMap Shelley.stringifyPoolId encodeRewardProvenancePool (Sh.pools rp)
          )
        , ( "mintedBlocks"
          , encodeMap Shelley.stringifyPoolId encodeNatural (Ledger.unBlocksMade $ Sh.blocks rp)
          )
        ]
  where
    encodeRewardProvenancePool
        :: Sh.RewardProvenancePool crypto
        -> Json
    encodeRewardProvenancePool rpp =
        encodeObject
            [ ( "totalMintedBlocks"
              , encodeNatural (Sh.poolBlocksP rpp)
              )
            , ( "totalStakeShare"
              , encodeRational (Sh.sigmaP rpp)
              )
            , ( "activeStakeShare"
              , encodeRational (Sh.sigmaAP rpp)
              )
            , ( "ownerStake"
              , encodeCoin (Sh.ownerStakeP rpp)
              )
            , ( "parameters"
              , Shelley.encodePoolParams (Sh.poolParamsP rpp)
              )
            , ( "pledgeRatio"
              , encodeRational (Sh.pledgeRatioP rpp)
              )
            , ( "maxRewards"
              , encodeCoin (Sh.maxPP rpp)
              )
            , ( "apparentPerformance"
              , encodeRational (Sh.appPerfP rpp)
              )
            , ( "totalRewards"
              , encodeCoin (Sh.poolRP rpp)
              )
            , ( "leaderRewards"
              , encodeCoin (Sh.lRewardP rpp)
              )
            ]

encodeSafeZone
    :: SafeZone
    -> Json
encodeSafeZone = \case
    StandardSafeZone k ->
        encodeWord64 k
    UnsafeIndefiniteSafeZone ->
        encodeNull

--
-- Parsers (Queries)
--

parseGetBlockHeight
    :: forall crypto f. ()
    => (Proxy (WithOrigin BlockNo) -> f (WithOrigin BlockNo))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetBlockHeight genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "blockHeight")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.GetChainBlockNo
            , genResult
            , encodeResult = const (encodeWithOrigin encodeBlockNo)
            }

parseGetChainTip
    :: forall crypto f. ()
    => (Proxy (Point (CardanoBlock crypto)) -> f (Point (CardanoBlock crypto)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetChainTip genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "chainTip")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.GetChainPoint
            , genResult
            , encodeResult = const encodePoint
            }

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
                , encodeResult = const (encodeMaybe encodeBound)
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
    :: forall crypto f. (Crypto crypto)
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
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
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
                    const (either encodeMismatchEraInfo encodeEpochNo)
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
                    either encodeMismatchEraInfo . encodeNonMyopicMemberRewards
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
        -> Json.Parser (Set (Either Ledger.Coin (Ledger.Credential 'Staking crypto)))
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
                    either encodeMismatchEraInfo . encodeDelegationsAndRewards
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
        -> Json.Parser (Set (Ledger.Credential 'Staking crypto))
    parseCredentials obj = fmap fromList $
        obj .: "delegationsAndRewards" >>= traverse parseCredential

parseGetCurrentPParams
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Ledger.PParams era))
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
                    const (either encodeMismatchEraInfo (Shelley.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Allegra.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Mary.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Alonzo.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetProposedPParamsUpdates
    :: forall crypto f. (Crypto crypto)
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
                    const (either encodeMismatchEraInfo Shelley.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Allegra.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Mary.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Alonzo.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetStakeDistribution
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Ledger.PoolDistr crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetStakeDistribution genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "stakeDistribution") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodePoolDistr
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

parseGetSystemStart
    :: forall crypto f. ()
    => (Proxy SystemStart -> f SystemStart)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetSystemStart genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "systemStart")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.GetSystemStart
            , genResult
            , encodeResult = const encodeSystemStart
            }

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
                    either encodeMismatchEraInfo . Shelley.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Allegra.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Mary.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Alonzo.encodeUtxoWithMode
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
                either encodeMismatchEraInfo . Shelley.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(ShelleyEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Allegra.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraMary ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentMary (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Mary.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(MaryEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAlonzo ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Alonzo.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AlonzoEra crypto))
            }
  where
    parseAddresses
        :: Json.Object
        -> Json.Parser (Set (Ledger.Addr crypto))
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
                either encodeMismatchEraInfo . Shelley.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(ShelleyEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Allegra.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraMary ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentMary (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Mary.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(MaryEra crypto))
            }
        SomeShelleyEra ShelleyBasedEraAlonzo ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Alonzo.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AlonzoEra crypto))
            }
  where
    parseTxIns
        :: Json.Object
        -> Json.Parser (Set (Ledger.TxIn crypto))
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
                    in const (either encodeMismatchEraInfo encodeGenesis)
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
                    in const (either encodeMismatchEraInfo encodeGenesis)
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
                    in const (either encodeMismatchEraInfo encodeGenesis)
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
                     in const (either encodeMismatchEraInfo encodeGenesis)
                , genResult =
                    genResultInEra (Proxy @(AlonzoEra crypto))
                }

parseGetRewardProvenance
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Sh.RewardProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetRewardProvenance genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "rewardsProvenance") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodeRewardProvenance
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

parseGetRewardInfoPools
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (RewardProvenance' crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetRewardInfoPools genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "rewardsProvenance'") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult = \_ ->
                    either encodeMismatchEraInfo encodeRewardInfoPools
                }
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetRewardInfoPools
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetRewardInfoPools
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetRewardInfoPools
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetRewardInfoPools
            )

parseGetPoolIds
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Set (Ledger.KeyHash 'StakePool crypto))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolIds genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "poolIds") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult = \mode ->
                    either encodeMismatchEraInfo (encodeListWithMode mode Shelley.encodePoolId . toList)
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
    => GenResult crypto f (Map (Ledger.KeyHash 'StakePool crypto) (Sh.PoolParams crypto))
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
                    either encodeMismatchEraInfo . encodePoolParameters
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
        -> Json.Parser (Set (Ledger.KeyHash 'StakePool crypto))
    parsePoolIds obj = fmap fromList $
        obj .: "poolParameters" >>= traverse parsePoolId

parseGetPoolsRanking
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Sh.RewardProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolsRanking genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "poolsRanking") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodeDesirabilities
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

parseGetInterpreter
    :: forall crypto f. ()
    => (Proxy (Interpreter (CardanoEras crypto)) -> f (Interpreter (CardanoEras crypto)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetInterpreter genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "eraSummaries")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.BlockQuery $ LSQ.QueryHardFork LSQ.GetInterpreter
            , genResult
            , encodeResult = const encodeInterpreter
            }

--
-- Parsers (Others)
--

parseAddress
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Addr crypto)
parseAddress = Json.withText "Address" $ choice "address"
    [ addressFromBytes fromBech32
    , addressFromBytes fromBase58
    , addressFromBytes fromBase16
    ]
  where
    addressFromBytes decode =
        decode >=> maybe mempty pure . Ledger.deserialiseAddr

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
    fmap Ledger.word64ToCoin . Json.parseJSON

-- TODO: Makes it possible to distinguish between KeyHash and ScriptHash
-- credentials. Both are encoded as hex-encoded strings. Encoding them as
-- object is ill-advised because we also need them as key of the non-myopic
-- member rewards map.
--
-- A possible option: encode them as Bech32 strings with different prefixes.
parseCredential
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Credential 'Staking crypto)
parseCredential =
    fmap (Ledger.KeyHashObj . Ledger.KeyHash) . parseHash

parseHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex

parsePoolId
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.KeyHash 'StakePool crypto)
parsePoolId = Json.withText "PoolId" $ choice "poolId"
    [ poolIdFromBytes fromBech32
    , poolIdFromBytes fromBase16
    ]
  where
    poolIdFromBytes decode =
        decode >=> maybe empty (pure . Ledger.KeyHash) . hashFromBytes

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
    -> Json.Parser (Ledger.TxIn crypto)
parseTxIn = Json.withObject "TxIn" $ \o -> do
    txid <- o .: "txId" >>= fromBase16
    ix <- o .: "index"
    pure $ Ledger.TxIn (Ledger.TxId txid) ix
  where
    fromBase16 =
        maybe empty (pure . unsafeMakeSafeHash) . hashFromTextAsHex @(HASH crypto)

--
-- Helpers
--

-- NOTE: This is necessary because the constructor of 'Hash' is no longer
-- exposed, and thus, it is not possible to use the 'castPoint' function from
-- Ouroboros.Network.Block anymore! May revisit in future upgrade of the
-- dependencies.
castPoint
    :: forall era crypto. (Ledger.Crypto era ~ crypto, Crypto crypto)
    => Point (ShelleyBlock era)
    -> Point (CardanoBlock crypto)
castPoint = \case
    GenesisPoint -> GenesisPoint
    BlockPoint slot h -> BlockPoint slot (cast h)
  where
    cast (TPraos.unHashHeader . unShelleyHash -> UnsafeHash h) = coerce h
