--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

module Ogmios.Data.Json.Query
    ( -- * Types
      Query (..)
    , QueryInEra
    , SomeQuery (..)
    , QueryResult

      -- ** AdHocQuery
    , AdHocQuery (..)

      -- ** Types in queries
    , Delegations
    , GenesisConfig
    , Interpreter
    , RewardAccounts
    , RewardsProvenance
    , Deposits
    , RewardAccountSummaries
    , RewardAccountSummary (..)
    , Sh.Api.RewardInfoPool
    , Sh.Api.RewardParams
    , Sh.Desirability
    , Ledger.PoolParams

      -- * Encoders
    , encodeBound
    , encodeEpochNo
    , encodeEraMismatch
    , encodeInterpreter
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint
    , encodePoolDistr
    , encodeRewardAccountSummaries
    , encodeRewardInfoPool
    , encodeRewardsProvenance
    , encodeStakePools

      -- * Decoders
    , decodeAddress
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
    , decodeScript
    , decodeSerializedTransaction
    , decodeTip
    , decodeTxId
    , decodeTxIn
    , decodeTxOut
    , decodeUtxo
    , decodeValue

      -- * Parsers
    , parseQueryLedgerConstitution
    , parseQueryLedgerConstitutionalCommittee
    , parseQueryLedgerEpoch
    , parseQueryLedgerEraStart
    , parseQueryLedgerEraSummaries
    , parseQueryLedgerLiveStakeDistribution
    , parseQueryLedgerProjectedRewards
    , parseQueryLedgerProposedProtocolParameters
    , parseQueryLedgerProtocolParameters
    , parseQueryLedgerRewardAccountSummaries
    , parseQueryLedgerRewardsProvenance
    , parseQueryLedgerStakePools
    , parseQueryLedgerTip
    , parseQueryLedgerUtxo
    , parseQueryLedgerUtxoByAddress
    , parseQueryLedgerUtxoByOutputReference
    , parseQueryNetworkBlockHeight
    , parseQueryNetworkGenesisConfiguration
    , parseQueryNetworkStartTime
    , parseQueryNetworkTip
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Crypto.Hash
    ( hashFromBytes
    , hashFromTextAsHex
    , pattern UnsafeHash
    )
import Cardano.Crypto.Hash.Class
    ( Hash
    , HashAlgorithm
    )
import Cardano.Ledger.Alonzo.Core
    ( AlonzoEraScript
    )
import Cardano.Ledger.Alonzo.Genesis
    ( AlonzoGenesis
    )
import Cardano.Ledger.Babbage
    ()
import Cardano.Ledger.Binary
    ( Annotator
    , FromCBOR (..)
    , decCBOR
    , decodeFullDecoder
    )
import Cardano.Ledger.Conway.Genesis
    ( ConwayGenesis
    )
import Cardano.Ledger.Crypto
    ( HASH
    )
import Cardano.Ledger.Keys
    ( KeyRole (..)
    )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis
    )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx
    , SerializedTransaction
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
    , serialise
    )
import Data.Aeson
    ( (.!=)
    )
import Data.Reflection
    ( give
    )
import Ogmios.Data.EraTranslation
    ( MostRecentEra
    , MultiEraTxOut (..)
    , MultiEraUTxO (..)
    , Upgrade (..)
    )
import Ouroboros.Consensus.BlockchainTime
    ( SystemStart (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoBlock
    , GenTx (..)
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo
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
    , EraParamsFormat (..)
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
    ( PraosCrypto
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    , ShelleyHash (..)
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

import qualified Cardano.Ledger.Binary.Decoding as Binary
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Decoding as Cbor
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Json
import qualified Data.Aeson.KeyMap as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Protocol.TPraos.API as TPraos
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Query as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as LSQ
import qualified PlutusLedgerApi.Common as Plutus

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.Plutus.Data as Ledger.Plutus
import qualified Cardano.Ledger.Plutus.Language as Ledger.Plutus
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.PoolParams as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.RewardProvenance as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Api.State.Query as Ledger
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Types
--

data Query (f :: Type -> Type) block = Query
    { rawQuery :: !Json.Value
    , queryInEra :: !(QueryInEra f block)
    } deriving (Generic)

type QueryInEra f block =
    SomeShelleyEra -> Maybe (SomeQuery f block)

data SomeQuery (f :: Type -> Type) block where
    SomeStandardQuery
        :: forall f block result. ()
        => LSQ.Query block result
            -- ^ Query definition, bound to a result
        -> (result -> Either Json Json)
            -- ^ Serialize results to JSON encoding.
        -> (Proxy result -> f result)
            -- ^ Yield results in some applicative 'f' from some type definition.
            -- Useful when `f ~ Gen` for testing.
        -> SomeQuery f block

    SomeCompoundQuery
        :: forall f block a b result crypto. (crypto ~ BlockCrypto block)
        => LSQ.Query block (QueryResult crypto a)
            -- ^ First query to run
        -> (a -> LSQ.Query block (QueryResult crypto b))
            -- ^ Serialize results from the first query
        -> (a -> b -> result)
            -- ^ Combine results from first and second queries
        -> (QueryResult crypto result -> Either Json Json)
            -- ^ Serialize results to JSON encoding.
        -> GenResult crypto f result
            -- ^ Yield results in some applicative 'f' from some type definition.
            -- Useful when `f ~ Gen` for testing.
        -> SomeQuery f block

    SomeAdHocQuery
        :: forall f block result. ()
        =>  AdHocQuery result
            -- ^ Query definition, bound to a result
        -> (result -> Json)
            -- ^ Serialize results to JSON encoding.
        -> (Proxy result -> f result)
            -- ^ Yield results in some applicative 'f' from some type definition.
            -- Useful when `f ~ Gen` for testing.
        -> SomeQuery f block

data AdHocQuery result where
    GetByronGenesis   :: AdHocQuery (GenesisConfig ByronEra)
    GetShelleyGenesis :: AdHocQuery (GenesisConfig ShelleyEra)
    GetAlonzoGenesis  :: AdHocQuery (GenesisConfig AlonzoEra)
    GetConwayGenesis  :: AdHocQuery (GenesisConfig ConwayEra)

type family GenesisConfig (era :: Type -> Type) :: Type where
    GenesisConfig ByronEra = Byron.GenesisData
    GenesisConfig ShelleyEra = ShelleyGenesis StandardCrypto
    GenesisConfig AlonzoEra = AlonzoGenesis
    GenesisConfig ConwayEra = ConwayGenesis StandardCrypto

instance Crypto crypto => FromJSON (Query Proxy (CardanoBlock crypto)) where
    parseJSON json = Json.withObject "Query" (\obj -> do
        queryName <- T.drop 5 <$> (obj .: "method")
        queryParams <- obj .:? "params" .!= Json.object []
        Query json <$> case queryName of
            "LedgerState/epoch" ->
                parseQueryLedgerEpoch id queryParams
            "LedgerState/eraStart" ->
                parseQueryLedgerEraStart id queryParams
            "LedgerState/eraSummaries" ->
                parseQueryLedgerEraSummaries id queryParams
            "LedgerState/liveStakeDistribution" ->
                parseQueryLedgerLiveStakeDistribution id queryParams
            "LedgerState/projectedRewards" ->
                parseQueryLedgerProjectedRewards id queryParams
            "LedgerState/protocolParameters" ->
                parseQueryLedgerProtocolParameters (const id) queryParams
            "LedgerState/proposedProtocolParameters" ->
                parseQueryLedgerProposedProtocolParameters (const id) queryParams
            "LedgerState/rewardAccountSummaries" ->
                parseQueryLedgerRewardAccountSummaries id queryParams
            "LedgerState/rewardsProvenance" ->
                parseQueryLedgerRewardsProvenance id queryParams
            "LedgerState/stakePools" ->
                parseQueryLedgerStakePools id queryParams
            "LedgerState/tip" ->
                parseQueryLedgerTip (const id) (const id) queryParams
            "LedgerState/utxo" ->
                parseQueryLedgerUtxoByOutputReference (const id) queryParams
                <|>
                parseQueryLedgerUtxoByAddress (const id) queryParams
                <|>
                parseQueryLedgerUtxo (const id) queryParams
            "LedgerState/constitution" ->
                parseQueryLedgerConstitution (const id) queryParams
            "LedgerState/constitutionalCommittee" ->
                parseQueryLedgerConstitutionalCommittee id queryParams
            "Network/blockHeight" ->
                parseQueryNetworkBlockHeight id queryParams
            "Network/genesisConfiguration" ->
                parseQueryNetworkGenesisConfiguration (Proxy, Proxy, Proxy, Proxy) queryParams
            "Network/startTime" ->
                parseQueryNetworkStartTime id queryParams
            "Network/tip" ->
                parseQueryNetworkTip id queryParams
            _ ->
                fail "unknown query name"
      ) json

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

type GenResult crypto f t =
    Proxy (QueryResult crypto t) -> f (QueryResult crypto t)

type Delegations crypto =
    Map (Ledger.Credential 'Staking crypto) (Ledger.KeyHash 'StakePool crypto)

type Deposits crypto =
    Map (Ledger.Credential 'Staking crypto) Coin

type RewardAccounts crypto =
    Map (Ledger.Credential 'Staking crypto) Coin

type RewardAccountSummaries crypto =
    Map (Ledger.Credential 'Staking crypto) (RewardAccountSummary crypto)

type RewardsProvenance crypto =
    ( Sh.Api.RewardParams
    , Map (Ledger.KeyHash 'StakePool crypto) Sh.Api.RewardInfoPool
    )

data RewardAccountSummary crypto = RewardAccountSummary
    { delegate :: !(Ledger.KeyHash 'StakePool crypto)
    , rewards :: !Coin
    , deposit :: !Coin
    } deriving (Eq, Show, Generic)

--
-- Encoders
--

encodeBound
    :: Bound
    -> Json
encodeBound bound =
    "time" .=
        encodeRelativeTime (boundTime bound) <>
    "slot" .=
        encodeSlotNo (boundSlot bound) <>
    "epoch".=
        encodeEpochNo (boundEpoch bound)
    & encodeObject

encodeRewardAccountSummaries
    :: Crypto crypto
    => RewardAccountSummaries crypto
    -> Json
encodeRewardAccountSummaries =
    encodeMap
        Shelley.stringifyCredential
        (\summary -> encodeObject
            ( "delegate" .=
                encodeSingleton "id" (Shelley.encodePoolId (delegate summary))
           <> "rewards" .=
                encodeCoin (rewards summary)
           <> "deposit" .=
                encodeCoin (deposit summary)
            )
        )

encodeEraMismatch
    :: EraMismatch
    -> Json
encodeEraMismatch x =
    "ledgerEra" .=
        encodeEraName (ledgerEraName x) <>
    "queryEra" .=
        encodeEraName (otherEraName x)
    & encodeObject

encodeEraParams
    :: EraParams
    -> Json
encodeEraParams x =
    "epochLength" .=
        encodeEpochSize (eraEpochSize x) <>
    "slotLength" .=
        encodeSlotLength (eraSlotLength x) <>
    "safeZone" .=
        encodeSafeZone (eraSafeZone x)
    & encodeObject

encodeEraSummary
    :: EraSummary
    -> Json
encodeEraSummary x =
    "start" .=
        encodeBound (eraStart x) <>
    "end" .=? OmitWhenNothing
        encodeBound (eraEndToMaybe (eraEnd x)) <>
    "parameters" .=
        encodeEraParams (eraParams x)
    & encodeObject
  where
    eraEndToMaybe :: EraEnd -> StrictMaybe Bound
    eraEndToMaybe = \case
        EraEnd bound ->
            SJust bound
        EraUnbounded ->
            SNothing

encodeInterpreter
    :: forall crypto eras. (eras ~ CardanoEras crypto)
    => Interpreter eras
    -> Json
encodeInterpreter (coerceInterpreter -> Summary eraSummaries) =
    encodeFoldable encodeEraSummary eraSummaries

coerceInterpreter :: forall crypto eras. (eras ~ CardanoEras crypto) => Interpreter eras -> Summary eras
coerceInterpreter =
    give EraParamsWithoutGenesisWindow (deserialise . serialise . give EraParamsWithoutGenesisWindow)

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeNonMyopicMemberRewards
    :: Crypto crypto
    => NonMyopicMemberRewards crypto
    -> Json
encodeNonMyopicMemberRewards (NonMyopicMemberRewards nonMyopicMemberRewards) =
    encodeMap encodeKey encodeVal nonMyopicMemberRewards
  where
    encodeKey = either Shelley.stringifyCoin Shelley.stringifyCredential
    encodeVal = encodeMap Shelley.stringifyPoolId encodeCoin

encodeOneEraHash
    :: OneEraHash eras
    -> Json
encodeOneEraHash =
    encodeShortByteString encodeByteStringBase16 . getOneEraHash

encodePoint
    :: Point (CardanoBlock crypto)
    -> Json
encodePoint = \case
    Point Origin ->
        encodeText "origin"
    Point (At x) ->
        "slot" .=
            encodeSlotNo (blockPointSlot x) <>
        "id" .=
            encodeOneEraHash (blockPointHash x)
        & encodeObject

encodePoolDistr
    :: forall crypto. Crypto crypto
    => Ledger.PoolDistr crypto
    -> Json
encodePoolDistr =
    encodeMap Shelley.stringifyPoolId encodeIndividualPoolStake . Ledger.unPoolDistr
  where
    encodeIndividualPoolStake
        :: Ledger.IndividualPoolStake crypto
        -> Json
    encodeIndividualPoolStake x =
        "stake" .=
            encodeRational (Ledger.individualPoolStake x) <>
        "vrf" .=
            Shelley.encodeHash (Ledger.individualPoolStakeVrf x)
        & encodeObject

encodeRewardInfoPool
    :: Crypto crypto
    => Ledger.KeyHash 'StakePool crypto
    -> Sh.Api.RewardInfoPool
    -> Json
encodeRewardInfoPool poolId info =
    "id" .=
        Shelley.encodePoolId poolId <>
    "stake" .=
        encodeCoin (Sh.Api.stake info) <>
    "ownerStake" .=
        encodeCoin (Sh.Api.ownerStake info) <>
    "approximatePerformance" .=
        encodeDouble (Sh.Api.performanceEstimate info) <>
    "parameters" .= encodeObject
        ( "cost" .=
            encodeCoin (Sh.Api.cost info) <>
          "margin" .=
              encodeUnitInterval (Sh.Api.margin info) <>
          "pledge" .=
              encodeCoin (Sh.Api.ownerPledge info)
        )
    & encodeObject

encodeRewardsProvenance
    :: Crypto crypto
    => RewardsProvenance crypto
    -> Json
encodeRewardsProvenance (rp, pools) =
    "desiredNumberOfStakePools" .=
        encodeNatural (Sh.Api.nOpt rp) <>
    "stakePoolPledgeInfluence" .=
        encodeNonNegativeInterval (Sh.Api.a0 rp) <>
    "totalRewardsInEpoch" .=
        encodeCoin (Sh.Api.rPot rp) <>
    "activeStakeInEpoch" .=
        encodeCoin (Sh.Api.totalStake rp) <>
    "stakePools" .=
        encodeObject (encodeMapSeries Shelley.stringifyPoolId encodeRewardInfoPool pools)
    & encodeObject

encodeSafeZone
    :: SafeZone
    -> Json
encodeSafeZone = \case
    StandardSafeZone k ->
        encodeWord64 k
    UnsafeIndefiniteSafeZone ->
        encodeNull

encodeStakePools
    :: Crypto crypto
    => Map (Ledger.KeyHash 'StakePool crypto) (Ledger.PoolParams crypto)
    -> Json
encodeStakePools =
    encodeObject . encodeMapSeries Shelley.stringifyPoolId (\k v ->
        encodeObject
            ( "id" .= Shelley.encodePoolId k
           <> Shelley.encodePoolParams v
            )
    )

--
-- Parsers (Queries)
--

eraMismatchOrResult
    :: (a -> b)
    -> Either (MismatchEraInfo (CardanoEras crypto)) a
    -> Either Json b
eraMismatchOrResult =
    bimap encodeMismatchEraInfo
{-# INLINABLE eraMismatchOrResult #-}

parseQueryLedgerConstitution
    :: forall f crypto. (Crypto crypto)
    => (forall era. (Typeable era) => Proxy era -> GenResult crypto f (Ledger.Constitution era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerConstitution genResult =
    Json.withObject "constitution" $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Nothing
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Nothing
            SomeShelleyEra ShelleyBasedEraMary ->
                Nothing
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Nothing
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Nothing
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetConstitution))
                    (eraMismatchOrResult (encodeObject . Conway.encodeConstitution))
                    (genResult $ Proxy @(ConwayEra crypto))

parseQueryLedgerConstitutionalCommittee
    :: forall f crypto. (Crypto crypto)
    => GenResult crypto f (Ledger.CommitteeMembersState crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerConstitutionalCommittee genResult =
    Json.withObject "constitutionalCommittee" $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Nothing
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Nothing
            SomeShelleyEra ShelleyBasedEraMary ->
                Nothing
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Nothing
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Nothing
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway $ GetCommitteeMembersState mempty mempty mempty))
                    (eraMismatchOrResult Conway.encodeCommitteeMembersState)
                    genResult

parseQueryLedgerEpoch
    :: forall crypto f. ()
    => GenResult crypto f EpochNo
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerEpoch genResult =
    Json.withObject "epoch" $ \obj -> do
        guard (null obj) $>
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodeEpochNo)
                genResult
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
                SomeShelleyEra ShelleyBasedEraConway ->
                    LSQ.BlockQuery $ QueryIfCurrentConway GetEpochNo
            )

parseQueryLedgerEraStart
    :: forall crypto f. ()
    => (Proxy (Maybe Bound) -> f (Maybe Bound))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerEraStart genResult =
    Json.withObject "eraStart" $ \obj -> do
        guard (null obj) $>
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (Right . encodeMaybe encodeBound)
                genResult
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
                SomeShelleyEra ShelleyBasedEraConway ->
                    LSQ.BlockQuery $ QueryAnytimeConway GetEraStart
            )

parseQueryLedgerEraSummaries
    :: forall crypto f. ()
    => (Proxy (Interpreter (CardanoEras crypto)) -> f (Interpreter (CardanoEras crypto)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerEraSummaries genResult =
    Json.withObject "eraSummaries" $ \obj -> do
        guard (null obj)
        pure $ const $ Just $ SomeStandardQuery
            (LSQ.BlockQuery (LSQ.QueryHardFork LSQ.GetInterpreter))
            (Right . encodeInterpreter)
            genResult

parseQueryLedgerTip
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (ShelleyBlock (TPraos crypto) era)))
    -> (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (ShelleyBlock (Praos crypto) era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerTip genResultInEraTPraos genResultInEraPraos =
    let query = "tip" in
    Json.withObject (toString @Text query) $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @(ShelleyEra crypto)))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @(AllegraEra crypto)))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @(MaryEra crypto)))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @(AlonzoEra crypto)))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint @(Praos crypto)))
                    (genResultInEraPraos (Proxy @(BabbageEra crypto)))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint @(Praos crypto)))
                    (genResultInEraPraos (Proxy @(ConwayEra crypto)))

parseQueryLedgerProjectedRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (NonMyopicMemberRewards crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerProjectedRewards genResult =
    Json.withObject (toString query) $ \obj -> do
        byStake <- obj .:? "stake" .!= Set.empty
            >>= traverset (fmap Left . decodeCoin)
        byScript <- obj .:? "scripts" .!= Set.empty
            >>= traverset (fmap Right . decodeCredential @crypto decodeAsScriptHash)
        byKey <- obj .:? "keys" .!= Set.empty
            >>= traverset (fmap Right . decodeCredential @crypto decodeAsKeyHash)
        let credentials = byStake <> byScript <> byKey
        pure $
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodeNonMyopicMemberRewards)
                genResult
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley
                        (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra
                        (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary
                        (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo
                        (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage
                        (GetNonMyopicMemberRewards credentials)
                SomeShelleyEra ShelleyBasedEraConway ->
                    LSQ.BlockQuery $ QueryIfCurrentConway
                        (GetNonMyopicMemberRewards credentials)
            )
  where
    query :: Text
    query = "projectedRewards"

parseQueryLedgerRewardAccountSummaries
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (RewardAccountSummaries crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerRewardAccountSummaries genResult =
    let query = "rewardAccountSummaries" in
    Json.withObject (toString @Text query) $ \obj -> do
        byScript <- obj .:? "scripts" .!= Set.empty
            >>= traverset (decodeCredential @crypto decodeAsScriptHash)
        byKey <- obj .:? "keys" .!= Set.empty
            >>= traverset (decodeCredential @crypto decodeAsKeyHash)
        let credentials = byScript <> byKey
        pure $ Just . \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                SomeCompoundQuery
                    (LSQ.BlockQuery
                        (QueryIfCurrentShelley
                            (GetFilteredDelegationsAndRewardAccounts credentials)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentShelley
                            (GetStakeDelegDeposits credentials)
                        )
                    )
                    mergeAll
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult

            SomeShelleyEra ShelleyBasedEraAllegra ->
                SomeCompoundQuery
                    (LSQ.BlockQuery
                        (QueryIfCurrentAllegra
                            (GetFilteredDelegationsAndRewardAccounts credentials)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentAllegra
                            (GetStakeDelegDeposits credentials)
                        )
                    )
                    mergeAll
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult

            SomeShelleyEra ShelleyBasedEraMary ->
                SomeCompoundQuery
                    (LSQ.BlockQuery
                        (QueryIfCurrentMary
                            (GetFilteredDelegationsAndRewardAccounts credentials)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentMary
                            (GetStakeDelegDeposits credentials)
                        )
                    )
                    mergeAll
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult

            SomeShelleyEra ShelleyBasedEraAlonzo ->
                SomeCompoundQuery
                    (LSQ.BlockQuery
                        (QueryIfCurrentAlonzo
                            (GetFilteredDelegationsAndRewardAccounts credentials)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentAlonzo
                            (GetStakeDelegDeposits credentials)
                        )
                    )
                    mergeAll
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult

            SomeShelleyEra ShelleyBasedEraBabbage ->
                SomeCompoundQuery
                    (LSQ.BlockQuery
                        (QueryIfCurrentBabbage
                            (GetFilteredDelegationsAndRewardAccounts credentials)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentBabbage
                            (GetStakeDelegDeposits credentials)
                        )
                    )
                    mergeAll
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult

            SomeShelleyEra ShelleyBasedEraConway ->
                SomeCompoundQuery
                    (LSQ.BlockQuery
                        (QueryIfCurrentConway
                            (GetFilteredDelegationsAndRewardAccounts credentials)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentConway
                            (GetStakeDelegDeposits credentials)
                        )
                    )
                    mergeAll
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult
  where
    mergeAll
        :: (Delegations crypto, RewardAccounts crypto)
        -> Deposits crypto
        -> RewardAccountSummaries crypto
    mergeAll (dlg, rwd) =
        Map.merge
            Map.dropMissing
            Map.dropMissing
            (Map.zipWithMatched (const identity))
            (Map.merge
                Map.dropMissing
                Map.dropMissing
                (Map.zipWithMatched (const RewardAccountSummary))
                dlg
                rwd
            )

parseQueryLedgerProtocolParameters
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Ledger.PParams era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerProtocolParameters genResultInEra =
    let query = "protocolParameters" in
    Json.withObject (toString @Text query) $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley GetCurrentPParams))
                    (eraMismatchOrResult Shelley.encodePParams)
                    (genResultInEra (Proxy @(ShelleyEra crypto)))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetCurrentPParams))
                    (eraMismatchOrResult Shelley.encodePParams)
                    (genResultInEra (Proxy @(AllegraEra crypto)))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetCurrentPParams))
                    (eraMismatchOrResult Shelley.encodePParams)
                    (genResultInEra (Proxy @(MaryEra crypto)))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetCurrentPParams))
                    (eraMismatchOrResult Alonzo.encodePParams)
                    (genResultInEra (Proxy @(AlonzoEra crypto)))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetCurrentPParams))
                    (eraMismatchOrResult Babbage.encodePParams)
                    (genResultInEra (Proxy @(BabbageEra crypto)))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetCurrentPParams))
                    (eraMismatchOrResult Conway.encodePParams)
                    (genResultInEra (Proxy @(ConwayEra crypto)))

parseQueryLedgerProposedProtocolParameters
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.ProposedPPUpdates era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerProposedProtocolParameters genResultInEra =
    let query = "proposedProtocolParameters" in
    Json.withObject (toString @Text query) $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley GetProposedPParamsUpdates))
                    (eraMismatchOrResult Shelley.encodeProposedPPUpdates)
                    (genResultInEra (Proxy @(ShelleyEra crypto)))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetProposedPParamsUpdates))
                    (eraMismatchOrResult Shelley.encodeProposedPPUpdates)
                    (genResultInEra (Proxy @(AllegraEra crypto)))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetProposedPParamsUpdates))
                    (eraMismatchOrResult Shelley.encodeProposedPPUpdates)
                    (genResultInEra (Proxy @(MaryEra crypto)))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetProposedPParamsUpdates))
                    (eraMismatchOrResult Alonzo.encodeProposedPPUpdates)
                    (genResultInEra (Proxy @(AlonzoEra crypto)))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetProposedPParamsUpdates))
                    (eraMismatchOrResult Babbage.encodeProposedPPUpdates)
                    (genResultInEra (Proxy @(BabbageEra crypto)))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetProposedPParamsUpdates))
                    (eraMismatchOrResult Conway.encodeProposedPPUpdates)
                    (genResultInEra (Proxy @(ConwayEra crypto)))

parseQueryLedgerLiveStakeDistribution
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Ledger.PoolDistr crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerLiveStakeDistribution genResult =
    let query = "liveStakeDistribution" in
    Json.withObject (toString @Text query) $ \obj -> do
        guard (null obj) $>
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodePoolDistr)
                genResult
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
                SomeShelleyEra ShelleyBasedEraConway ->
                    LSQ.BlockQuery $ QueryIfCurrentConway GetStakeDistribution
            )

parseQueryLedgerUtxo
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerUtxo genResultInEra =
    let query = "utxo" in
    Json.withObject (toString @Text query) $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley GetUTxOWhole))
                    (eraMismatchOrResult Shelley.encodeUtxo)
                    (genResultInEra (Proxy @(ShelleyEra crypto)))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetUTxOWhole))
                    (eraMismatchOrResult Allegra.encodeUtxo)
                    (genResultInEra (Proxy @(AllegraEra crypto)))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetUTxOWhole))
                    (eraMismatchOrResult Mary.encodeUtxo)
                    (genResultInEra (Proxy @(MaryEra crypto)))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetUTxOWhole))
                    (eraMismatchOrResult Alonzo.encodeUtxo)
                    (genResultInEra (Proxy @(AlonzoEra crypto)))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetUTxOWhole))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @(BabbageEra crypto)))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetUTxOWhole))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @(ConwayEra crypto)))

parseQueryLedgerUtxoByAddress
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerUtxoByAddress genResultInEra =
    let query = "utxo" in
    Json.withObject (toString @Text query) $ \obj -> do
        addrs <- obj .: "addresses" >>= traverset (decodeAddress @crypto)
        pure $ \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Shelley.encodeUtxo)
                    (genResultInEra (Proxy @(ShelleyEra crypto)))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Allegra.encodeUtxo)
                    (genResultInEra (Proxy @(AllegraEra crypto)))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Mary.encodeUtxo)
                    (genResultInEra (Proxy @(MaryEra crypto)))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Alonzo.encodeUtxo)
                    (genResultInEra (Proxy @(AlonzoEra crypto)))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @(BabbageEra crypto)))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @(ConwayEra crypto)))

parseQueryLedgerUtxoByOutputReference
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerUtxoByOutputReference genResultInEra =
    let query = "utxo" in
    Json.withObject (toString @Text query) $ \obj -> do
        ins <- obj .: "outputReferences" >>= traverset (decodeTxIn @crypto)
        pure $ \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentShelley (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Shelley.encodeUtxo)
                    (genResultInEra (Proxy @(ShelleyEra crypto)))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Allegra.encodeUtxo)
                    (genResultInEra (Proxy @(AllegraEra crypto)))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentMary (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Mary.encodeUtxo)
                    (genResultInEra (Proxy @(MaryEra crypto)))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Alonzo.encodeUtxo)
                    (genResultInEra (Proxy @(AlonzoEra crypto)))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentBabbage (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @(BabbageEra crypto)))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentConway (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @(ConwayEra crypto)))

parseQueryNetworkGenesisConfiguration
    :: forall f crypto. ()
    => ( f (GenesisConfig ByronEra)
       , f (GenesisConfig ShelleyEra)
       , f (GenesisConfig AlonzoEra)
       , f (GenesisConfig ConwayEra)
       )
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryNetworkGenesisConfiguration (genByron, genShelley, genAlonzo, genConway) =
    Json.withObject "genesisConfiguration" $ \obj -> do
        era <- obj .: "era"
        case T.toLower era of
            "byron" ->
                pure $ const $ Just $ SomeAdHocQuery
                    GetByronGenesis
                    (Byron.encodeGenesisData)
                    (const genByron)
            "shelley" -> do
                pure $ const $ Just $ SomeAdHocQuery
                    GetShelleyGenesis
                    (Shelley.encodeGenesis)
                    (const genShelley)
            "alonzo" -> do
                pure $ const $ Just $ SomeAdHocQuery
                    GetAlonzoGenesis
                    (Alonzo.encodeGenesis)
                    (const genAlonzo)
            "conway" -> do
                pure $ const $ Just $ SomeAdHocQuery
                    GetConwayGenesis
                    (Conway.encodeGenesis)
                    (const genConway)
            (_unknownEra :: Text) -> do
                fail "Invalid era parameter. Only 'byron', 'shelley', \
                     \'alonzo' and 'conway' have a genesis configuration."

parseQueryLedgerRewardsProvenance
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (RewardsProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerRewardsProvenance genResult =
    Json.withObject "rewardsProvenance" $ \obj -> do
        guard (null obj) $>
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodeRewardsProvenance)
                genResult
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
                SomeShelleyEra ShelleyBasedEraConway ->
                    LSQ.BlockQuery $ QueryIfCurrentConway GetRewardInfoPools
            )

parseQueryLedgerStakePools
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f
        (Map
            (Ledger.KeyHash 'StakePool crypto)
            (Ledger.PoolParams crypto)
        )
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerStakePools genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        obj .:? "stakePools" >>= \case
            Nothing -> pure getAllStakePools
            Just params -> do
                ids <- traverset (decodePoolId @crypto) params
                pure (getFilteredStakePools ids)
  where
    getFilteredStakePools ids =
        (\queryDef -> Just $ SomeStandardQuery
            queryDef
            (eraMismatchOrResult encodeStakePools)
            genResult
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
            SomeShelleyEra ShelleyBasedEraConway ->
                LSQ.BlockQuery $ QueryIfCurrentConway (GetStakePoolParams ids)
        )

    getAllStakePools = Just . \case
        SomeShelleyEra ShelleyBasedEraShelley ->
            SomeCompoundQuery
                (LSQ.BlockQuery (QueryIfCurrentShelley GetStakePools))
                (LSQ.BlockQuery . QueryIfCurrentShelley . GetStakePoolParams)
                (const identity)
                (eraMismatchOrResult encodeStakePools)
                genResult
        SomeShelleyEra ShelleyBasedEraAllegra ->
            SomeCompoundQuery
                (LSQ.BlockQuery (QueryIfCurrentAllegra GetStakePools))
                (LSQ.BlockQuery . QueryIfCurrentAllegra . GetStakePoolParams)
                (const identity)
                (eraMismatchOrResult encodeStakePools)
                genResult
        SomeShelleyEra ShelleyBasedEraMary ->
            SomeCompoundQuery
                (LSQ.BlockQuery (QueryIfCurrentMary GetStakePools))
                (LSQ.BlockQuery . QueryIfCurrentMary . GetStakePoolParams)
                (const identity)
                (eraMismatchOrResult encodeStakePools)
                genResult
        SomeShelleyEra ShelleyBasedEraAlonzo ->
            SomeCompoundQuery
                (LSQ.BlockQuery (QueryIfCurrentAlonzo GetStakePools))
                (LSQ.BlockQuery . QueryIfCurrentAlonzo . GetStakePoolParams)
                (const identity)
                (eraMismatchOrResult encodeStakePools)
                genResult
        SomeShelleyEra ShelleyBasedEraBabbage ->
            SomeCompoundQuery
                (LSQ.BlockQuery (QueryIfCurrentBabbage GetStakePools))
                (LSQ.BlockQuery . QueryIfCurrentBabbage . GetStakePoolParams)
                (const identity)
                (eraMismatchOrResult encodeStakePools)
                genResult
        SomeShelleyEra ShelleyBasedEraConway ->
            SomeCompoundQuery
                (LSQ.BlockQuery (QueryIfCurrentConway GetStakePools))
                (LSQ.BlockQuery . QueryIfCurrentConway . GetStakePoolParams)
                (const identity)
                (eraMismatchOrResult encodeStakePools)
                genResult

parseQueryNetworkBlockHeight
    :: forall crypto f. ()
    => (Proxy (WithOrigin BlockNo) -> f (WithOrigin BlockNo))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryNetworkBlockHeight genResult =
    Json.withObject "blockHeight" $ \obj -> do
        guard (null obj)
        pure $ const $ Just $ SomeStandardQuery
            LSQ.GetChainBlockNo
            (Right . encodeWithOrigin encodeBlockNo)
            genResult

parseQueryNetworkStartTime
    :: forall crypto f. ()
    => (Proxy SystemStart -> f SystemStart)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryNetworkStartTime genResult =
    Json.withObject "startTime" $ \obj -> do
        guard (null obj)
        pure $ const $ Just $ SomeStandardQuery
            LSQ.GetSystemStart
            (Right . encodeSystemStart)
            genResult

parseQueryNetworkTip
    :: forall crypto f. ()
    => (Proxy (Point (CardanoBlock crypto)) -> f (Point (CardanoBlock crypto)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryNetworkTip genResult =
    Json.withObject "tip" $ \obj -> do
        guard (null obj)
        pure $ const $ Just $ SomeStandardQuery
            LSQ.GetChainPoint
            (Right . encodePoint)
            genResult

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
        decode >=> maybe
            (fail "couldn't deserialise address from bytes")
            pure
            . Ledger.decodeAddrLenient

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe
                    (fail "couldn't convert decoded bech32 payload of an address?")
                    pure
                    (Bech32.dataPartToBytes dataPart)

    fromBase58 =
        decodeBase58 . encodeUtf8

    fromBase16 =
        decodeBase16 . encodeUtf8

decodeAssets
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Mary.MultiAsset crypto)
decodeAssets =
    fmap Ledger.Mary.MultiAsset . Json.withObject "Assets" (Json.foldrWithKey collectPolicies (pure mempty))
  where
    collectPolicies (Json.toText -> k) v parser = do
        xs <- parser
        policyId <- decodePolicyId k
        assets <- Json.withObject
            ("Assets<" <> toString k <> ">")
            (Json.foldrWithKey collectAssets (pure mempty))
            v
        pure $ Map.insert policyId assets xs

    collectAssets k v parser = do
        xs <- parser
        assetName <- decodeAssetName (Json.toText k)
        quantity <- Json.parseJSON v
        pure $ Map.insert assetName quantity xs

decodeAssetName
    :: Text
    -> Json.Parser Ledger.Mary.AssetName
decodeAssetName =
    fmap (Ledger.Mary.AssetName . toShort) . decodeBase16 . encodeUtf8

decodeBinaryData
    :: forall era. (Era era)
    => Json.Value
    -> Json.Parser (Ledger.Plutus.BinaryData era)
decodeBinaryData =
    Json.withText "BinaryData" $ \t -> do
        bytes <- toLazy <$> decodeBase16 (encodeUtf8 t)
        Ledger.Plutus.dataToBinaryData <$> decodeCborAnn @era "Data" decCBOR bytes

decodeCoin
    :: Json.Value
    -> Json.Parser Coin
decodeCoin = Json.withObject "Lovelace" $ \o -> do
    lovelace <- o .: "ada" >>= (.: "lovelace")
    pure $ Ledger.word64ToCoin lovelace

decodeCredential
    :: forall crypto. Crypto crypto
    => (ByteString -> Json.Parser (Ledger.Credential 'Staking crypto))
    -> Json.Value
    -> Json.Parser (Ledger.Credential 'Staking crypto)
decodeCredential decodeAsKeyOrScript =
    Json.withText "Credential" $ \(encodeUtf8 -> str) -> asum
        [ decodeBase16 str >>=
            decodeAsKeyOrScript
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

    decodeAsStakeAddress
        :: ByteString
        -> Json.Parser (Ledger.Credential 'Staking crypto)
    decodeAsStakeAddress =
        fmap Ledger.raCredential
            . maybe invalidStakeAddress pure
            . Ledger.deserialiseRewardAccount
      where
        invalidStakeAddress =
            fail "invalid stake address"

decodeAsKeyHash
    :: forall crypto. (Crypto crypto)
    => ByteString
    -> Json.Parser (Ledger.Credential 'Staking crypto)
decodeAsKeyHash =
    fmap (Ledger.KeyHashObj . Ledger.KeyHash)
        . maybe invalidStakeKeyHash pure
        . hashFromBytes
  where
    invalidStakeKeyHash =
        fail "invalid stake key hash"

decodeAsScriptHash
    :: forall crypto. (Crypto crypto)
    => ByteString
    -> Json.Parser (Ledger.Credential 'Staking crypto)
decodeAsScriptHash =
    fmap (Ledger.ScriptHashObj . Ledger.ScriptHash)
        . maybe invalidStakeScriptHash pure
        . hashFromBytes
  where
    invalidStakeScriptHash =
        fail "invalid stake script hash"

decodeDatumHash
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Plutus.DataHash crypto)
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
        _notOrigin -> empty

    parsePoint = Json.withObject "Point" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "id" >>= decodeOneEraHash
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
decodePoolId = Json.withObject "StakePoolId" $ \obj -> do
    txt <- obj .: "id"
    poolIdFromBytes fromBech32 txt <|> poolIdFromBytes fromBase16 txt
  where
    failure =
        fail "couldn't decode stake pool id in neither bech32 nor base16"

    poolIdFromBytes decode =
        decode >=> maybe failure (pure . Ledger.KeyHash) . hashFromBytes

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe failure pure $ Bech32.dataPartToBytes dataPart

    fromBase16 =
        decodeBase16 . encodeUtf8

decodeScript
    :: forall era.
        ( AlonzoEraScript era
        , Ledger.Script era ~ Ledger.Alonzo.AlonzoScript era
        )
    => Json.Value
    -> Json.Parser (Ledger.Script era)
decodeScript v =
    Json.withText "Script::CBOR" decodeFromBase16Cbor v
    <|>
    Json.withObject "Script::JSON" decodeFromWrappedJson v
  where
    decodeFromBase16Cbor :: forall a. (Binary.DecCBOR (Annotator a)) => Text -> Json.Parser a
    decodeFromBase16Cbor t = do
        taggedScript <- toLazy <$> decodeBase16 (encodeUtf8 t)
        annotatedScript <- toLazy <$> decodeCbor @era "Script" decodeTaggedScript taggedScript
        decodeCborAnn @era "Script" decCBOR annotatedScript

    decodeFromWrappedJson :: Json.Object -> Json.Parser (Ledger.Script era)
    decodeFromWrappedJson o = do
        o .:? "language" >>= \case
            Just "native" -> do
                cbor <- o .:? "cbor" >>= traverse (decodeBase16 . encodeUtf8 @Text)
                json <- o .:? "json"
                case (cbor, json) of
                    (Just bytes, _) -> do
                        case decodeCborAnn @era "Script<Native>" Binary.decCBOR (toLazy bytes) of
                            Right script ->
                                pure (Ledger.Alonzo.TimelockScript script)
                            Left (_ :: Text) ->
                                fail "couldn't decode native script"
                    (_, Just script) ->
                        Ledger.Alonzo.TimelockScript <$> decodeTimeLock script
                    (_, _) ->
                        fail "missing field 'cbor' or 'json' to decode native script"
            Just lang@"plutus:v1" -> do
                bytes <- o .: "cbor"
                plutus <- Ledger.Alonzo.mkBinaryPlutusScript Ledger.Plutus.PlutusV1 <$> decodePlutusScript Plutus.PlutusV1 lang bytes
                maybe (fail "unable to instantiate PlutusScript from binary data") (pure . Ledger.Alonzo.PlutusScript) plutus
            Just lang@"plutus:v2" -> do
                bytes <- o .: "cbor"
                plutus <- Ledger.Alonzo.mkBinaryPlutusScript Ledger.Plutus.PlutusV2 <$> decodePlutusScript Plutus.PlutusV2 lang bytes
                maybe (fail "unable to instantiate PlutusScript from binary data") (pure . Ledger.Alonzo.PlutusScript) plutus
            Just lang@"plutus:v3" -> do
                bytes <- o .: "cbor"
                plutus <- Ledger.Alonzo.mkBinaryPlutusScript Ledger.Plutus.PlutusV3 <$> decodePlutusScript Plutus.PlutusV3 lang bytes
                maybe (fail "unable to instantiate PlutusScript from binary data") (pure . Ledger.Alonzo.PlutusScript) plutus
            _ ->
                fail "missing or unknown script language."
      where
        decodePlutusScript :: Plutus.PlutusLedgerLanguage -> Json.Key -> Text -> Json.Parser Ledger.Alonzo.PlutusBinary
        decodePlutusScript ledgerLang (Json.toText -> lang) str = do
            bytes <- decodeBase16 (encodeUtf8 str)
            let lbytes = toLazy bytes
            let protocolVersion = Plutus.ledgerLanguageIntroducedIn ledgerLang
            when (isLeft (Plutus.deserialiseScript ledgerLang protocolVersion (toShort bytes))) $ do
                let err = "couldn't decode plutus script"
                let hint =
                        case decodeCbor @era "Script<Plutus>" decodeRawScript lbytes of
                            Left (_ :: String) ->
                                case decodeCbor @era "Script<Plutus>" decodeAnnotatedScript lbytes of
                                    Left (_ :: String) ->
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
            pure (Ledger.Alonzo.PlutusBinary $ toShort bytes)

    decodeRawScript :: forall s. Binary.Decoder s ByteString
    decodeRawScript = do
        bytes <- toLazy <$> decodeTaggedScript
        version <- Binary.getDecoderVersion
        either
            (fail . show)
            pure
            (decodeFullDecoder version "Annotated(Script)" decodeAnnotatedScript bytes)

    decodeTaggedScript :: forall s. Binary.Decoder s ByteString
    decodeTaggedScript =
        Binary.decodeNestedCborBytes

    decodeAnnotatedScript :: forall s. Binary.Decoder s ByteString
    decodeAnnotatedScript = Binary.fromPlainDecoder $ do
        _len <- Cbor.decodeListLen
        _typ <- Cbor.decodeWord
        Cbor.decodeBytes

decodeSerializedTransaction
    :: forall crypto constraint.
        ( PraosCrypto crypto
        , TPraos.PraosCrypto crypto
        , constraint ~ (MostRecentEra (CardanoBlock crypto) ~ ConwayEra crypto)
        )
    => Json.Value
    -> Json.Parser (MultiEraDecoder (SerializedTransaction (CardanoBlock crypto)))
decodeSerializedTransaction = Json.withText "Transaction" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8 <|> invalidEncodingError
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
    -- Avoiding 'asum' here because it generates poor errors on failures.
    pure $  deserialiseCBOR @(BabbageEra crypto) GenTxBabbage bytes
        <|> deserialiseCBOR @(ConwayEra crypto)  GenTxConway  bytes
        <|> deserialiseCBOR @(AlonzoEra crypto)  GenTxAlonzo  bytes
        <|> deserialiseCBOR @(MaryEra crypto)    GenTxMary    bytes
        <|> deserialiseCBOR @(AllegraEra crypto) GenTxAllegra bytes
        <|> deserialiseCBOR @(ShelleyEra crypto) GenTxShelley bytes
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

    invalidEncodingError :: Json.Parser a
    invalidEncodingError =
        fail "failed to decode base16-encoded payload."

    deserialiseCBOR
        :: forall era sub.
            ( FromCBOR (GenTx sub)
            , IsShelleyBasedEra era
            , Era era
            , sub ~ ShelleyBlock (EraProto era) era
            )
        => (GenTx sub -> GenTx (CardanoBlock crypto))
        -> ByteString
        -> MultiEraDecoder (GenTx (CardanoBlock crypto))
    deserialiseCBOR mk bytes =
        mk <$> decodeCborWith @era "Transaction"
            (\e -> MultiEraDecoderErrors
                [ ( SomeShelleyEra (shelleyBasedEra @era)
                  , e
                  , fromIntegral $ BS.length bytes
                  )
                ]
            )
            (Binary.fromPlainDecoder fromCBOR)
            (if  wrapper `BS.isPrefixOf` bytes
               then fromStrict bytes
               else wrap bytes
            )
      where
        wrapper = BS.pack [216, 24] -- D818...

        -- Cardano tools have a tendency to wrap cbor in cbor (e.g cardano-cli).
        -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
        -- `24` and serialized as CBOR bytes `58xx`.
        wrap :: ByteString -> LByteString
        wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

decodeTimeLock
    :: Era era
    => Json.Value
    -> Json.Parser (Ledger.Allegra.Timelock era)
decodeTimeLock = Json.withObject "Script<Native>" $ \o -> do
    clause <- o .: "clause"
    case clause :: Text of
        "signature" ->
            decodeRequireSignature o
        "all" ->
            decodeAllOf o
        "any" ->
            decodeAnyOf o
        "some" ->
            decodeSomeOf o
        "after" ->
            decodeTimeStart o
        "before" ->
            decodeTimeExpire o
        _ ->
            fail "unknown clause type when decoding native script"
  where
    decodeRequireSignature o = do
        from <- o .: "from" >>= decodeHash
        pure $ Ledger.Allegra.RequireSignature (Ledger.KeyHash from)
    decodeAllOf o = do
        xs <- StrictSeq.fromList <$> (o .: "from")
        Ledger.Allegra.RequireAllOf <$> traverse decodeTimeLock xs
    decodeAnyOf o = do
        xs <- StrictSeq.fromList <$> (o .: "from")
        Ledger.Allegra.RequireAnyOf <$> traverse decodeTimeLock xs
    decodeSomeOf o = do
        n <- o .: "atLeast"
        xs <- StrictSeq.fromList <$> (o .: "from")
        Ledger.Allegra.RequireMOf n <$> traverse decodeTimeLock xs
    decodeTimeExpire o = do
        Ledger.Allegra.RequireTimeExpire . SlotNo <$> (o .: "slot")
    decodeTimeStart o = do
        Ledger.Allegra.RequireTimeStart . SlotNo <$> (o .: "slot")

decodeTip
    :: Json.Value
    -> Json.Parser (Tip (CardanoBlock crypto))
decodeTip json =
    parseOrigin json <|> parseTip json
  where
    parseOrigin = Json.withText "Tip" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _notOrigin -> empty

    parseTip = Json.withObject "Tip" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "id" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeTxId
    :: forall crypto. Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.TxId crypto)
decodeTxId = Json.withText "TxId" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8
    case hashFromBytes bytes of
        Nothing ->
            fail "couldn't interpret bytes as blake2b-256 digest"
        Just h ->
            pure $ Ledger.TxId (Ledger.unsafeMakeSafeHash h)

decodeTxIn
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.TxIn crypto)
decodeTxIn = Json.withObject "TxIn" $ \o -> do
    txid <- o .: "transaction" >>= (.: "id") >>= fromBase16
    ix <- o .: "index"
    pure $ Ledger.TxIn (Ledger.TxId txid) (Ledger.TxIx ix)
  where
    failure =
        fail "couldn't decode transaction id from base16"
    fromBase16 =
        maybe failure (pure . unsafeMakeSafeHash) . hashFromTextAsHex @(HASH crypto)

decodeTxOut
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (MultiEraTxOut (CardanoBlock crypto))
decodeTxOut = Json.withObject "TxOut" $ \o -> do
    decodeTxOutBabbage o <|> decodeTxOutConway o
  where
    decodeTxOutBabbage o = do
        address <- o .: "address" >>= decodeAddress
        value <- o .: "value" >>= decodeValue

        datumHash <- o .:? "datumHash"
        inlineDatum <- o .:? "datum"
        datum <- case (datumHash, inlineDatum) of
            (Nothing, Nothing) ->
                pure Ledger.Plutus.NoDatum
            (Just Json.Null, Just Json.Null) ->
                pure Ledger.Plutus.NoDatum
            (Just x, Nothing) ->
                Ledger.Plutus.DatumHash <$> decodeDatumHash x
            (Nothing, Just x) ->
                Ledger.Plutus.Datum <$> decodeBinaryData @(BabbageEra crypto) x
            (Just{}, Just{}) ->
                fail "specified both 'datumHash' & 'datum'"

        script <-
            o .:? "script" >>= maybe
                (pure SNothing)
                (fmap SJust . decodeScript)

        pure $ TxOutInBabbageEra $ Ledger.Babbage.BabbageTxOut address value datum script

    decodeTxOutConway o = do
        address <- o .: "address" >>= decodeAddress
        value <- o .: "value" >>= decodeValue

        datumHash <- o .:? "datumHash"
        inlineDatum <- o .:? "datum"
        datum <- case (datumHash, inlineDatum) of
            (Nothing, Nothing) ->
                pure Ledger.Plutus.NoDatum
            (Just Json.Null, Just Json.Null) ->
                pure Ledger.Plutus.NoDatum
            (Just x, Nothing) ->
                Ledger.Plutus.DatumHash <$> decodeDatumHash x
            (Nothing, Just x) ->
                Ledger.Plutus.Datum <$> decodeBinaryData @(ConwayEra crypto) x
            (Just{}, Just{}) ->
                fail "specified both 'datumHash' & 'datum'"

        script <-
            o .:? "script" >>= maybe
                (pure SNothing)
                (fmap SJust . decodeScript)

        pure $ TxOutInConwayEra $ Ledger.Babbage.BabbageTxOut address value datum script

decodeUtxo
    :: forall crypto block. (Crypto crypto, block ~ CardanoBlock crypto)
    => Json.Value
    -> Json.Parser (MultiEraUTxO block)
decodeUtxo v = do
    xs <- Json.parseJSONList v
    (UTxOInBabbageEra . Sh.UTxO . Map.fromList <$> traverse decodeBabbageUtxoEntry xs) <|>
        (UTxOInConwayEra . Sh.UTxO . Map.fromList <$> traverse decodeConwayUtxoEntry xs)
  where
    decodeBabbageUtxoEntry
        :: Json.Value
        -> Json.Parser (Ledger.TxIn crypto, Ledger.Babbage.BabbageTxOut (BabbageEra crypto))
    decodeBabbageUtxoEntry o =
        (,) <$> decodeTxIn o
            <*> (decodeTxOut o >>= \case
                    TxOutInBabbageEra o' -> pure o'
                    TxOutInConwayEra{}   -> empty
                )

    decodeConwayUtxoEntry
        :: Json.Value
        -> Json.Parser (Ledger.TxIn crypto, Ledger.Babbage.BabbageTxOut (ConwayEra crypto))
    decodeConwayUtxoEntry o =
        (,) <$> decodeTxIn o
            <*> (decodeTxOut o >>= \case
                    TxOutInBabbageEra o' -> pure (upgrade o')
                    TxOutInConwayEra  o' -> pure o'
                )

decodeValue
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.Value (AlonzoEra crypto))
decodeValue = Json.withObject "Value" $ \o -> do
    coins <- decodeCoin (Json.Object o)
    assets <- decodeAssets (Json.Object $ Json.delete "ada" o)
    pure (Ledger.Mary.MaryValue coins assets)

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
