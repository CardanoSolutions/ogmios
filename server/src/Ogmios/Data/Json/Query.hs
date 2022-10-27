--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
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

      -- * Decoders
    , decodeAddress
    , decodeAssetId
    , decodeAssetName
    , decodeAssets
    , decodeCoin
    , decodeCredential
    , decodeDatumHash
    , decodeHash
    , decodeOneEraHash
    , decodePoint
    , decodePolicyId
    , decodePoolId
    , decodeSerializedTx
    , decodeTip
    , decodeTxId
    , decodeTxIn
    , decodeTxOut
    , decodeUtxo
    , decodeValue

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
    ( ShelleyBasedEra (..)
    )
import Cardano.Binary
    ( Annotator
    , DecoderError
    , FromCBOR (..)
    , decodeAnnotator
    , decodeFull
    , decodeFullDecoder
    )
import Cardano.Crypto.Hash
    ( hashFromBytes
    , hashFromTextAsHex
    , pattern UnsafeHash
    )
import Cardano.Crypto.Hash.Class
    ( Hash
    , HashAlgorithm
    )
import Cardano.Ledger.Babbage
    ()
import Cardano.Ledger.Crypto
    ( Crypto
    , HASH
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
    )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx
    , GenTxId
    , SerializedTx
    )
import Cardano.Slotting.Block
    ( BlockNo (..)
    )
import Cardano.Slotting.Slot
    ( EpochNo (..)
    , SlotNo (..)
    , WithOrigin (..)
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart
    )
import Codec.Serialise
    ( deserialise
    , deserialiseOrFail
    , serialise
    )
import Data.Aeson
    ( toJSON
    )
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.SOP.Strict
    ( NS (..)
    )
import Formatting.Buildable
    ( build
    )
import Ogmios.Data.EraTranslation
    ( MostRecentEra
    , MultiEraTxOut (..)
    , MultiEraUTxO (..)
    , translateTxOut
    )
import Ouroboros.Consensus.BlockchainTime
    ( SystemStart (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoBlock
    , CardanoEras
    , GenTx (..)
    , TxId (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..)
    , MismatchEraInfo
    , OneEraHash (..)
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..)
    , mkEraMismatch
    )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryAnytime (..)
    )
import Ouroboros.Consensus.HardFork.History.EraParams
    ( EraParams (..)
    , SafeZone (..)
    )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Interpreter
    )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..)
    , EraEnd (..)
    , EraSummary (..)
    , Summary (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    , PraosCrypto
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Eras
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , MaryEra
    , ShelleyEra
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    , ShelleyHash (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CompactGenesis
    , getCompactGenesis
    )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( TxId (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( BlockQuery (..)
    , NonMyopicMemberRewards (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ProtoCrypto
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()
import Ouroboros.Network.Block
    ( Point (..)
    , Tip (..)
    , genesisPoint
    , pattern BlockPoint
    , pattern GenesisPoint
    , wrapCBORinCBOR
    )
import Ouroboros.Network.Point
    ( Block (..)
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Text.Read as T

import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Query as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as LSQ

import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Protocol.TPraos.API as TPraos

import qualified Cardano.Ledger.Era as Era

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo.Data as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger.Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as Ledger.Mary
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.RewardProvenance as Sh
import qualified Cardano.Ledger.Shelley.TxBody as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Codec.CBOR.Decoding as Cbor
import qualified Data.Sequence.Strict as StrictSeq
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
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
        , \raw -> Query raw <$> parseGetLedgerTip (const id) (const id) raw
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
    , Map (Ledger.KeyHash 'StakePool crypto) Sh.Api.RewardInfoPool
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
        SomeShelleyEra ShelleyBasedEraBabbage -> toJSON @Text "Babbage"

-- | Convert an 'EraIndex' to a Shelley-based era.
fromEraIndex
    :: forall crypto. ()
    => EraIndex (CardanoEras crypto)
    -> Maybe SomeShelleyEra
fromEraIndex = \case
    EraIndex                Z{}      -> Nothing
    EraIndex             (S Z{})     -> Just (SomeShelleyEra ShelleyBasedEraShelley)
    EraIndex          (S (S Z{}))    -> Just (SomeShelleyEra ShelleyBasedEraAllegra)
    EraIndex       (S (S (S Z{})))   -> Just (SomeShelleyEra ShelleyBasedEraMary)
    EraIndex    (S (S (S (S Z{}))))  -> Just (SomeShelleyEra ShelleyBasedEraAlonzo)
    EraIndex (S (S (S (S (S Z{}))))) -> Just (SomeShelleyEra ShelleyBasedEraBabbage)

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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryAnytimeBabbage GetEraStart
            )

parseGetLedgerTip
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (ShelleyBlock (TPraos crypto) era)))
    -> (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (ShelleyBlock (Praos crypto) era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetLedgerTip genResultInEraTPraos genResultInEraPraos =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEraTPraos (Proxy @(ShelleyEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEraTPraos (Proxy @(AllegraEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentMary GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEraTPraos (Proxy @(MaryEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEraTPraos (Proxy @(AlonzoEra crypto))
                }
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint @(Praos crypto)))
                , genResult =
                    genResultInEraPraos (Proxy @(BabbageEra crypto))
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetEpochNo
            )

parseGetNonMyopicMemberRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (NonMyopicMemberRewards crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetNonMyopicMemberRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        credentials <- decodeCredentials obj
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage (GetNonMyopicMemberRewards credentials)
            )
  where
    decodeCredentials
        :: Json.Object
        -> Json.Parser (Set (Either Ledger.Coin (Ledger.Credential 'Staking crypto)))
    decodeCredentials obj = fmap fromList $
        obj .: "nonMyopicMemberRewards" >>= traverse
            (choice "credential"
                [ fmap Left  . decodeCoin
                , fmap Right . decodeCredential
                ]
            )

parseGetFilteredDelegationsAndRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Delegations crypto, RewardAccounts crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetFilteredDelegationsAndRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        credentials <- decodeCredentials obj
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage (GetFilteredDelegationsAndRewardAccounts credentials)
            )
  where
    decodeCredentials
        :: Json.Object
        -> Json.Parser (Set (Ledger.Credential 'Staking crypto))
    decodeCredentials obj = fmap fromList $
        obj .: "delegationsAndRewards" >>= traverse decodeCredential

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
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Babbage.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(BabbageEra crypto))
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
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Babbage.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(BabbageEra crypto))
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetStakeDistribution
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
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Babbage.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(BabbageEra crypto))
                }

parseGetUTxOByAddress
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxOByAddress genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    addrs <- decodeAddresses obj
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
        SomeShelleyEra ShelleyBasedEraBabbage ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentBabbage (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Babbage.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(BabbageEra crypto))
            }
  where
    decodeAddresses
        :: Json.Object
        -> Json.Parser (Set (Ledger.Addr crypto))
    decodeAddresses obj = fmap fromList $
        obj .: "utxo" >>= traverse decodeAddress

parseGetUTxOByTxIn
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxOByTxIn genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    ins <- decodeTxIns obj
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
        SomeShelleyEra ShelleyBasedEraBabbage ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentBabbage (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Babbage.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(BabbageEra crypto))
            }
  where
    decodeTxIns
        :: Json.Object
        -> Json.Parser (Set (Ledger.TxIn crypto))
    decodeTxIns obj = fmap fromList $
        obj .: "utxo" >>= traverse decodeTxIn

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
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Shelley.encodeGenesis . getCompactGenesis
                     in const (either encodeMismatchEraInfo encodeGenesis)
                , genResult =
                    genResultInEra (Proxy @(BabbageEra crypto))
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetRewardProvenance
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetRewardInfoPools
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetStakePools
            )

parseGetPoolParameters
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Map (Ledger.KeyHash 'StakePool crypto) (Sh.PoolParams crypto))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolParameters genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        ids <- decodePoolIds obj
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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage (GetStakePoolParams ids)
            )
  where
    decodePoolIds
        :: Json.Object
        -> Json.Parser (Set (Ledger.KeyHash 'StakePool crypto))
    decodePoolIds obj = fmap fromList $
        obj .: "poolParameters" >>= traverse decodePoolId

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
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetRewardProvenance
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

decodeAddress
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Addr crypto)
decodeAddress = Json.withText "Address" $ choice "address"
    [ addressFromBytes fromBech32
    , addressFromBytes fromBase58
    , addressFromBytes fromBase16
    ]
  where
    addressFromBytes decode =
        decode >=> maybe empty pure . Ledger.deserialiseAddr

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe empty pure $ Bech32.dataPartToBytes dataPart

    fromBase58 =
        decodeBase58 . encodeUtf8

    fromBase16 =
        decodeBase16 . encodeUtf8

decodeAssets
    :: Crypto crypto
    => Json.Value
    -> Json.Parser [(Ledger.Mary.PolicyID crypto, Ledger.Mary.AssetName, Integer)]
decodeAssets =
    Json.withObject "Assets" $ Json.foldrWithKey fn (pure mempty)
  where
    fn k v p = do
        xs <- p
        (policyId, assetName) <- decodeAssetId (Json.toText k)
        quantity <- Json.parseJSON v
        pure $ (policyId, assetName, quantity) : xs

decodeAssetId
    :: Crypto crypto
    => Text
    -> Json.Parser (Ledger.Mary.PolicyID crypto, Ledger.Mary.AssetName)
decodeAssetId txt =
    case T.splitOn "." txt of
        [rawPolicyId] ->
            let emptyAssetName = Ledger.Mary.AssetName mempty in
            (,) <$> decodePolicyId rawPolicyId <*> pure emptyAssetName
        [rawPolicyId, rawAssetName] ->
            (,) <$> decodePolicyId rawPolicyId <*> decodeAssetName rawAssetName
        _ ->
            fail "invalid asset id, should be a dot-separated policy id and asset name, both base16-encoded."

decodeAssetName
    :: Text
    -> Json.Parser Ledger.Mary.AssetName
decodeAssetName =
    fmap (Ledger.Mary.AssetName . toShort) . decodeBase16 . encodeUtf8

decodeBinaryData
    :: Json.Value
    -> Json.Parser (Ledger.Alonzo.BinaryData era)
decodeBinaryData =
    Json.withText "BinaryData" $ \t -> do
        bytes <- toLazy <$> decodeBase16 (encodeUtf8 t)
        either
            (fail . toString . TL.toLazyText . build)
            (pure . Ledger.Alonzo.dataToBinaryData . Ledger.Alonzo.Data)
            (decodeAnnotator "Data" fromCBOR bytes)

decodeCoin
    :: Json.Value
    -> Json.Parser Coin
decodeCoin =
    fmap Ledger.word64ToCoin . Json.parseJSON

decodeCredential
    :: forall crypto. Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Credential 'Staking crypto)
decodeCredential = Json.withText "Credential" $ \(encodeUtf8 -> str) ->
    asum
        [ decodeBase16 str >>=
            decodeAsKeyHash
        , decodeBech32 str >>=
            whenHrp (`elem` [[humanReadablePart|stake|], [humanReadablePart|stake_test|]])
                decodeAsStakeAddress
        , decodeBech32 str >>=
            whenHrp (== [humanReadablePart|script|])
                decodeAsScriptHash
        , decodeBech32 str >>=
            whenHrp (== [humanReadablePart|stake_vkh|])
                decodeAsKeyHash
        ]
       <|>
        fail "Unable to decode credential. It must be either a base16-encoded \
             \stake key hash or a bech32-encoded stake address, stake key hash \
             \or script hash with one of the following respective prefixes: \
             \stake, stake_vkh or script."
  where
    invalidStakeKeyHash =
        fail "invalid stake key hash"

    invalidStakeAddress =
        fail "invalid stake address"

    whenHrp
        :: (Bech32.HumanReadablePart -> Bool)
        -> (ByteString -> Json.Parser a)
        -> (Bech32.HumanReadablePart, Bech32.DataPart)
        -> Json.Parser a
    whenHrp predicate parser (hrp, bytes) =
        if predicate hrp then
            maybe mempty parser (Bech32.dataPartToBytes bytes)
        else
            mempty

    decodeBech32
        :: ByteString
        -> Json.Parser (Bech32.HumanReadablePart, Bech32.DataPart)
    decodeBech32 =
        either (fail . show) pure . Bech32.decodeLenient . decodeUtf8

    decodeAsKeyHash
        :: ByteString
        -> Json.Parser (Ledger.Credential 'Staking crypto)
    decodeAsKeyHash =
        fmap (Ledger.KeyHashObj . Ledger.KeyHash)
            . maybe invalidStakeKeyHash pure
            . hashFromBytes

    decodeAsScriptHash
        :: ByteString
        -> Json.Parser (Ledger.Credential 'Staking crypto)
    decodeAsScriptHash =
        fmap (Ledger.ScriptHashObj . Ledger.ScriptHash)
            . maybe invalidStakeKeyHash pure
            . hashFromBytes

    decodeAsStakeAddress
        :: ByteString
        -> Json.Parser (Ledger.Credential 'Staking crypto)
    decodeAsStakeAddress =
        fmap Ledger.getRwdCred
            . maybe invalidStakeAddress pure
            . Ledger.deserialiseRewardAcnt

decodeDatumHash
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Alonzo.DataHash crypto)
decodeDatumHash =
    fmap unsafeMakeSafeHash . decodeHash

decodeHash
    :: HashAlgorithm alg
    => Json.Value
    -> Json.Parser (Hash alg a)
decodeHash =
    Json.parseJSON >=> maybe err pure . hashFromTextAsHex
  where
    err = fail "cannot decode given hash digest from base16 text."

decodeOneEraHash
    :: Text
    -> Json.Parser (OneEraHash (CardanoEras crypto))
decodeOneEraHash =
    either (const err) (pure . OneEraHash . toShort . CC.hashToBytes) . CC.decodeHash
  where
    err = fail "cannot decode given hash digest from base16 text."

decodePoint
    :: Json.Value
    -> Json.Parser (Point (CardanoBlock crypto))
decodePoint json =
    parseOrigin json <|> parsePoint json
  where
    parseOrigin = Json.withText "Point" $ \case
        txt | txt == "origin" -> pure genesisPoint
        _ -> empty

    parsePoint = Json.withObject "Point" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        pure $ Point $ At $ Block (SlotNo slot) hash

decodePolicyId
    :: Crypto crypto
    => Text
    -> Json.Parser (Ledger.Mary.PolicyID crypto)
decodePolicyId =
    maybe
        invalidPolicyId
        (pure . Ledger.Mary.PolicyID . Ledger.ScriptHash)
    . hashFromTextAsHex
  where
    invalidPolicyId = fail "failed to decode policy id for a given asset."

decodePoolId
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.KeyHash 'StakePool crypto)
decodePoolId = Json.withText "PoolId" $ choice "poolId"
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

decodeScript
    :: forall era crypto.
        ( crypto ~ Era.Crypto era
        , Ledger.Script era ~ Ledger.Alonzo.Script era
        , Crypto crypto
        , Typeable era
        )
    => Json.Value
    -> Json.Parser (Ledger.Script era)
decodeScript v =
    (Json.withText "Script::CBOR" decodeFromBase16Cbor v)
    <|>
    (Json.withObject "Script::JSON" decodeFromWrappedJson v)
  where
    decodeFromBase16Cbor :: forall a. (FromCBOR (Annotator a)) => Text -> Json.Parser a
    decodeFromBase16Cbor t = do
        taggedScript <- toLazy <$> decodeBase16 (encodeUtf8 t)
        either
            (fail . toString . TL.toLazyText . build)
            pure
            $ do
                annotatedScript <- decodeFullDecoder "Script" decodeTaggedScript taggedScript
                decodeAnnotator "Script" fromCBOR (toLazy annotatedScript)

    decodeFromWrappedJson :: Json.Object -> Json.Parser (Ledger.Script era)
    decodeFromWrappedJson o =
        (Ledger.Alonzo.TimelockScript <$>
            (let lang = "native" in (o .: lang) >>= decodeTimeLock)
        )
        <|>
        (Ledger.Alonzo.PlutusScript Ledger.Alonzo.PlutusV1 <$>
            (let lang = "plutus:v1" in (o .: lang) >>= decodePlutusScript lang)
        )
        <|>
        (Ledger.Alonzo.PlutusScript Ledger.Alonzo.PlutusV2 <$>
            (let lang = "plutus:v2" in (o .: lang) >>= decodePlutusScript lang)
        )
      where
        decodePlutusScript :: Json.Key -> Text -> Json.Parser ShortByteString
        decodePlutusScript (Json.toText -> lang) str = do
            bytes <- decodeBase16 (encodeUtf8 str)
            let lbytes = toLazy bytes
            when (isLeft (deserialiseOrFail @Plutus.Script lbytes)) $ do
                let err = "couldn't decode plutus script"
                let hint =
                        case decodeFullDecoder "Script" decodeRawScript lbytes of
                            Left{} ->
                                case decodeFullDecoder "Script" decodeAnnotatedScript lbytes of
                                    Left{} ->
                                        ""
                                    Right (encodeBase16 -> expected) ->
                                        let suffix = fromMaybe "???" (T.stripSuffix expected str)
                                         in unwords
                                            [ ": when using the explicit JSON notation,"
                                            , "the script must be given raw, without tag."
                                            , "Please drop '" <> suffix <> "' from the"
                                            , "beginning of the script payload."
                                            ]
                            Right (encodeBase16 -> expected) ->
                                let suffix = fromMaybe "???" (T.stripSuffix expected str)
                                 in unwords
                                    [ ": when using the explicit JSON notation,"
                                    , "the script must be given raw, without tag."
                                    , "Please drop '" <> suffix <> "' from the"
                                    , "beginning of the script payload or provide"
                                    , "it without '" <> lang <> "' JSON key."
                                    ]
                fail (toString (err <> hint))
            pure (toShort bytes)

    decodeRawScript :: forall s. Cbor.Decoder s ByteString
    decodeRawScript = do
        bytes <- toLazy <$> decodeTaggedScript
        either
            (fail . show)
            pure
            (decodeFullDecoder "Annotated(Script)" decodeAnnotatedScript bytes)

    decodeTaggedScript :: forall s. Cbor.Decoder s ByteString
    decodeTaggedScript = do
        _tag <- Cbor.decodeTag
        Cbor.decodeBytes

    decodeAnnotatedScript :: forall s. Cbor.Decoder s ByteString
    decodeAnnotatedScript = do
        _len <- Cbor.decodeListLen
        _typ <- Cbor.decodeWord
        Cbor.decodeBytes

decodeSerializedTx
    :: forall crypto.
        ( PraosCrypto crypto
        , TPraos.PraosCrypto crypto
        )
    => Json.Value
    -> Json.Parser (SerializedTx (CardanoBlock crypto))
decodeSerializedTx = Json.withText "Tx" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8 <|> decodeBase64 utf8 <|> invalidEncodingError
    -- NOTE (1):
    -- The order in which we parser matters! Older eras first. formats
    -- are forward-compatible, and near hard-forks, there's a period where the
    -- software can understand the next era but, that era isn't available yet.
    --
    -- Therefore, we need to favor parsing older eras so that existing code keep
    -- working. Transactions are only decoded in the new era when they are using
    -- features not available in older ones.
    --
    -- NOTE (2):
    -- Avoiding 'asum' here because it generates poor errors on failures
    deserialiseCBOR @() GenTxMary (fromStrict bytes)
        <|> deserialiseCBOR @() GenTxMary (wrap bytes)
        <|> deserialiseCBOR @() GenTxAlonzo (fromStrict bytes)
        <|> deserialiseCBOR @() GenTxAlonzo (wrap bytes)
        <|> deserialiseCBOR @(MostRecentEra (CardanoBlock crypto) ~ BabbageEra crypto) GenTxBabbage (wrap bytes)
        <|> deserialiseCBOR @(MostRecentEra (CardanoBlock crypto) ~ BabbageEra crypto) GenTxBabbage (fromStrict bytes)
  where
    invalidEncodingError :: Json.Parser a
    invalidEncodingError =
        fail "failed to decode payload from base64 or base16."

    -- Cardano tools have a tendency to wrap cbor in cbor (e.g cardano-cli).
    -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
    -- `24` and serialized as CBOR bytes `58xx`.
    wrap :: ByteString -> LByteString
    wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

    deserialiseCBOR
        :: forall constraint era.
            ( FromCBOR (GenTx era)
            , constraint
            )
        => (GenTx era -> GenTx (CardanoBlock crypto))
        -> LByteString
        -> Json.Parser (GenTx (CardanoBlock crypto))
    deserialiseCBOR mk =
        either (fail . prettyDecoderError @era) (pure . mk)
        .
        decodeFull
      where
        -- We use this extra constraint when decoding transactions to generate a
        -- compiler warning in case a new era becomes available, to not forget to update
        -- this bit of code. The constraint is purely artificial but will generate a
        -- compilation error which will catch our attention.
        --
        -- Generally speaking, we still want to deserialise previous eras, but we want
        -- to make sure to have support for the latest as well. In case a more recent is
        -- available, this will generate a compiler error looking like:
        --
        --    • Couldn't match type ‘BabbageEra crypto’ with ‘AlonzoEra crypto’ arising from a use of ‘deserialiseCBOR’
        _compilerWarning = keepRedundantConstraint (Proxy @constraint)

    prettyDecoderError
        :: forall era.
            ( FromCBOR (GenTx era)
            )
        => DecoderError
        -> String
    prettyDecoderError =
        toString
            . TL.replace
                (toLazy $ label (Proxy @(GenTx era)))
                "serialised transaction"
            . TL.replace
                "\n"
                " "
            . TL.toLazyText
            . build

decodeTimeLock
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Mary.Timelock crypto)
decodeTimeLock json =
    (decodeRequireSignature json)
    <|>
    (Json.withObject "Timelock::AllOf" decodeAllOf json)
    <|>
    (Json.withObject "Timelock::AnyOf" decodeAnyOf json)
    <|>
    (Json.withObject "Timelocks::MOf" decodeMOf json)
    <|>
    (Json.withObject "Timelocks::TimeExpire" decodeTimeExpire json)
    <|>
    (Json.withObject "Timelocks::TimeStart" decodeTimeStart json)
  where
    decodeRequireSignature t = do
        Ledger.Mary.RequireSignature
            <$> fmap Ledger.KeyHash (decodeHash t)
    decodeAllOf o = do
        xs <- StrictSeq.fromList <$> (o .: "all")
        Ledger.Mary.RequireAllOf <$> traverse decodeTimeLock xs
    decodeAnyOf o = do
        xs <- StrictSeq.fromList <$> (o .: "any")
        Ledger.Mary.RequireAnyOf <$> traverse decodeTimeLock xs
    decodeMOf o =
        case Json.toList o of
            [(k, v)] -> do
                case T.readMaybe (Json.toString k) of
                    Just n -> do
                        xs <- StrictSeq.fromList <$> Json.parseJSON v
                        Ledger.Mary.RequireMOf n <$> traverse decodeTimeLock xs
                    Nothing ->
                        fail "cannot decode MOfN constructor, key isn't a natural."
            _ ->
                fail "cannot decode MOfN, not a list."
    decodeTimeExpire o = do
        Ledger.Mary.RequireTimeExpire . SlotNo <$> (o .: "expiresAt")
    decodeTimeStart o = do
        Ledger.Mary.RequireTimeStart . SlotNo <$> (o .: "startsAt")

decodeTip
    :: Json.Value
    -> Json.Parser (Tip (CardanoBlock crypto))
decodeTip json =
    parseOrigin json <|> parseTip json
  where
    parseOrigin = Json.withText "Tip" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty

    parseTip = Json.withObject "Tip" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeTxId
    :: forall crypto. Crypto crypto
    => Json.Value
    -> Json.Parser (GenTxId (CardanoBlock crypto))
decodeTxId = Json.withText "TxId" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8
    case hashFromBytes bytes of
        Nothing ->
            fail "couldn't interpret bytes as blake2b-256 digest."
        Just h ->
            pure $ GenTxIdAlonzo $ ShelleyTxId $ Ledger.TxId (Ledger.unsafeMakeSafeHash h)

decodeTxIn
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.TxIn crypto)
decodeTxIn = Json.withObject "TxIn" $ \o -> do
    txid <- o .: "txId" >>= fromBase16
    ix <- o .: "index"
    pure $ Ledger.TxIn (Ledger.TxId txid) (Ledger.TxIx ix)
  where
    fromBase16 =
        maybe empty (pure . unsafeMakeSafeHash) . hashFromTextAsHex @(HASH crypto)

decodeTxOut
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (MultiEraTxOut (CardanoBlock crypto))
decodeTxOut = Json.withObject "TxOut" $ \o -> do
    decodeTxOutAlonzo o <|> decodeTxOutBabbage o
  where
    isPlutusData :: Json.Value -> Bool
    isPlutusData x =
        case Json.parse decodeBinaryData x of
            Json.Error{} -> False
            Json.Success{} -> True

    decodeTxOutAlonzo o = do
        address <- o .: "address" >>= decodeAddress
        value <- o .: "value" >>= decodeValue

        -- NOTE: Prior to Babbage, 'datum' was unambiguous in the context of outputs
        -- and always referred to datum hash digests. Babbage introduces inline datums,
        -- which can, from a decoding perspective, be mixed up with datum hashes.
        --
        -- For this matter, we now expect datum hashes to be provided explicitly as
        -- 'datumHash'. Yet, to keep backward-compatibility, it is still possible to
        -- provide a hash digest as 'datum'. The only condition being that, the
        -- assumed hash must not be a valid Plutus data.
        datumHash <- o .:? "datumHash"
        inlineDatum <- o .:? "datum"
        datum <- case (datumHash, inlineDatum) of
            (Nothing, Nothing) ->
                pure SNothing
            (Just Json.Null, Just Json.Null) ->
                pure SNothing
            (Just x, Nothing) | not (isPlutusData x) ->
                SJust <$> decodeDatumHash x
            (Nothing, Just x) | not (isPlutusData x) ->
                SJust <$> decodeDatumHash x
            (Just x, Just y) | x == y ->
                SJust <$> decodeDatumHash x
            (Just{}, Just{}) ->
                fail "specified both 'datumHash' & 'datum'"
            (_, _) ->
                fail "inline-datum in assumed-Alonzo output."

        -- NOTE: Similarly, if a 'script' field is present, we assume the output
        -- to be of the 'Babbage' era, and we make this decoder fail.
        script <- o .:? "script"
        when (isJust @Json.Value script) empty

        pure $ TxOutInAlonzoEra $ Ledger.Alonzo.TxOut address value datum

    decodeTxOutBabbage o = do
        address <- o .: "address" >>= decodeAddress
        value <- o .: "value" >>= decodeValue

        datumHash <- o .:? "datumHash"
        inlineDatum <- o .:? "datum"
        datum <- case (datumHash, inlineDatum) of
            (Nothing, Nothing) ->
                pure Ledger.Babbage.NoDatum
            (Just Json.Null, Just Json.Null) ->
                pure Ledger.Babbage.NoDatum
            (Just x, Nothing) ->
                Ledger.Babbage.DatumHash <$> decodeDatumHash x
            (Nothing, Just x) ->
                Ledger.Babbage.Datum <$> decodeBinaryData x
            (Just{}, Just{}) ->
                fail "specified both 'datumHash' & 'datum'"

        script <-
            o .:? "script" >>= maybe
                (pure SNothing)
                (fmap SJust . decodeScript)

        pure $ TxOutInBabbageEra $ Ledger.Babbage.TxOut address value datum script

decodeUtxo
    :: forall crypto block. (Crypto crypto, block ~ CardanoBlock crypto)
    => Json.Value
    -> Json.Parser (MultiEraUTxO block)
decodeUtxo v = do
    xs <- Json.parseJSONList v
    (UTxOInAlonzoEra . Sh.UTxO . Map.fromList <$> traverse decodeAlonzoUtxoEntry xs) <|>
        (UTxOInBabbageEra . Sh.UTxO . Map.fromList <$> traverse decodeBabbageUtxoEntry xs)
  where
    hint :: String
    hint =
        "Failed to decode utxo entry. Expected an array of length \
        \2 as [output-reference, output]"

    decodeAlonzoUtxoEntry
        :: Json.Value
        -> Json.Parser (Ledger.TxIn crypto, Ledger.Alonzo.TxOut (AlonzoEra crypto))
    decodeAlonzoUtxoEntry =
        Json.parseJSONList >=> \case
            [i, o] ->
                (,) <$> decodeTxIn i <*> (decodeTxOut o >>= \case
                    TxOutInAlonzoEra o' -> pure o'
                    TxOutInBabbageEra{} -> empty
                )
            _ ->
                fail hint

    decodeBabbageUtxoEntry
        :: Json.Value
        -> Json.Parser (Ledger.TxIn crypto, Ledger.Babbage.TxOut (BabbageEra crypto))
    decodeBabbageUtxoEntry =
        Json.parseJSONList >=> \case
            [i, o] ->
                (,) <$> decodeTxIn i <*> (decodeTxOut o >>= \case
                    TxOutInAlonzoEra  o' -> pure (translateTxOut o')
                    TxOutInBabbageEra o' -> pure o'
                )
            _ ->
                fail hint

decodeValue
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.Value (AlonzoEra crypto))
decodeValue = Json.withObject "Value" $ \o -> do
    coins <- o .: "coins" >>= decodeCoin
    assets <- o .:? "assets" >>= maybe (pure mempty) decodeAssets
    pure (Ledger.Mary.valueFromList (Ledger.unCoin coins) assets)

--
-- Helpers
--

-- NOTE: This is necessary because the constructor of 'Hash' is no longer
-- exposed, and thus, it is not possible to use the 'castPoint' function from
-- Ouroboros.Network.Block anymore! May revisit in future upgrade of the
-- dependencies.
castPoint
    :: forall proto era. (Crypto (ProtoCrypto proto))
    => Point (ShelleyBlock proto era)
    -> Point (CardanoBlock (ProtoCrypto proto))
castPoint = \case
    GenesisPoint -> GenesisPoint
    BlockPoint slot h -> BlockPoint slot (cast h)
  where
    cast (unShelleyHash -> UnsafeHash h) = coerce h
