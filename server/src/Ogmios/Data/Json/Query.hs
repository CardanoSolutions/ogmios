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
    , AccountState (..)
    , DRepSummary (..)
    , Delegations
    , Deposits
    , GenesisConfig
    , Interpreter
    , Ledger.PoolParams
    , PoolRewardsInfo (..)
    , RewardAccountSummaries
    , RewardAccountSummary (..)
    , RewardAccounts
    , RewardsProvenance (..)
    , Sh.Api.RewardParams
    , Sh.Desirability
    , StakePoolsPerformances
    , VoteDelegatees

      -- * Encoders
    , encodeBound
    , encodeEpochNo
    , encodeEraMismatch
    , encodeGovActionState
    , encodeInterpreter
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint
    , encodePoolDistr
    , encodeRewardAccountSummaries
    , encodeRewardsProvenance
    , encodeStakePools
    , encodeStakePoolsPerformances

      -- * Decoders
    , decodeAddress
    , decodeAssetName
    , decodeAssets
    , decodeCoin
    , decodeDRepCredential
    , decodeStakingCredential
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
    , parseQueryLedgerDelegateRepresentatives
    , parseQueryLedgerEpoch
    , parseQueryLedgerEraStart
    , parseQueryLedgerEraSummaries
    , parseQueryLedgerGovernanceProposals
    , parseQueryLedgerGovernanceProposalsByProposalReference
    , parseQueryLedgerLiveStakeDistribution
    , parseQueryLedgerNonces
    , parseQueryLedgerOperationalCertificates
    , parseQueryLedgerProjectedRewards
    , parseQueryLedgerProtocolParameters
    , parseQueryLedgerRewardAccountSummaries
    , parseQueryLedgerRewardsProvenance
    , parseQueryLedgerStakePools
    , parseQueryLedgerStakePoolsPerformances
    , parseQueryLedgerTip
    , parseQueryLedgerTreasuryAndReserves
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
import Cardano.Ledger.Api.State.Query
    ( CommitteeMembersState
    )
import Cardano.Ledger.Babbage
    ()
import Cardano.Ledger.Binary
    ( Annotator
    , FromCBOR (..)
    , decCBOR
    , decodeFullDecoder
    )
import Cardano.Ledger.Binary.Coders
    ( Decode (D, From, RecD)
    , (<!)
    )
import Cardano.Ledger.Conway.Genesis
    ( ConwayGenesis
    )
import Cardano.Ledger.Conway.Governance
    ( GovActionState (..)
    , Voter (..)
    )
import Cardano.Ledger.Hashes
    ( unsafeMakeSafeHash
    )
import Cardano.Ledger.Keys
    ( HasKeyRole (coerceKeyRole)
    , KeyRole (..)
    )
import Cardano.Ledger.Shelley.Genesis
    ( ShelleyGenesis
    )
import Cardano.Ledger.Shelley.LedgerState
    ( AccountState (..)
    )
import Cardano.Ledger.Shelley.Rewards
    ( StakeShare (..)
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
import Data.Generics.Product.Positions
    ( position
    )
import Ogmios.Control.MonadDisk
    ( MonadDisk (..)
    )
import Ogmios.Data.EraTranslation
    ( MostRecentEra
    , MultiEraTxOut (..)
    , MultiEraUTxO (..)
    , Upgrade (..)
    )
import Ogmios.Data.Ledger.Rewards
    ( PoolRewardsInfo (..)
    , RewardsProvenance (..)
    , newRewardsProvenance
    )
import Ouroboros.Consensus.BlockchainTime
    ( SystemStart (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
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
import Ouroboros.Consensus.Protocol.Abstract
    ( ConsensusProtocol (ChainDepState)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( PraosCrypto
    , PraosState (..)
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraosState (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    , ShelleyHash (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( getCompactGenesis
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
    , Serialised (..)
    , Tip (..)
    , genesisPoint
    , pattern BlockPoint
    , pattern GenesisPoint
    , wrapCBORinCBOR
    )
import Ouroboros.Network.Point
    ( Block (..)
    )
import Relude.Extra
    ( lens
    )

import qualified Cardano.Ledger.Binary.Coders as Binary
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
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Protocol.TPraos.API as TPraos
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as TPraos
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Query as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as LSQ
import qualified PlutusLedgerApi.Common as Plutus

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Allegra.Scripts as Ledger.Allegra
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Conway.Governance as Ledger.Conway
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.DRep as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.Plutus.Data as Ledger.Plutus
import qualified Cardano.Ledger.Plutus.Language as Ledger.Plutus
import qualified Cardano.Ledger.PoolParams as Ledger
import qualified Cardano.Ledger.Shelley.Scripts as Ledger.Shelley
import qualified Cardano.Ledger.State as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.LedgerState as Ledger

import qualified Cardano.Ledger.Shelley.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Shelley.RewardProvenance as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Api as Ledger
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Byron as Byron
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

-- FIXME
-- Needed with ouroboros-cardano-consensus==0.18.0.0. Otherwise, use the following:
--
-- import qualified Cardano.Ledger.PoolDistr as Ledger
-- import qualified Cardano.Ledger.Api.State.Query as Ledger
import qualified Cardano.Protocol.TPraos.Rules.Tickn as TPraos
import qualified Ouroboros.Consensus.Shelley.Ledger.Query.Types as Consensus


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

    SomeEffectfullQuery
        :: forall f block result. ()
        => LSQ.Query block result
            -- ^ Query definition, bound to a result
        -> (forall m. MonadDisk m => result -> m (Either Json Json))
            -- ^ Serialize results to JSON encoding.
        -> (Proxy result -> f result)
            -- ^ Yield results in some applicative 'f' from some type definition.
            -- Useful when `f ~ Gen` for testing.
        -> SomeQuery f block

    SomeCompoundQuery
        :: forall f block a b result crypto. (crypto ~ BlockCrypto block)
        => LSQ.Query block (QueryResult crypto a)
            -- ^ First query to run.
        -> (a -> LSQ.Query block (QueryResult crypto b))
            -- ^ Second query to run, using result from the first one.
        -> (a -> b -> result)
            -- ^ Combine results from first and second queries
        -> (QueryResult crypto result -> Either Json Json)
            -- ^ Serialize results to JSON encoding.
        -> GenResult crypto f result
            -- ^ Yield results in some applicative 'f' from some type definition.
            -- Useful when `f ~ Gen` for testing.
        -> SomeQuery f block

    SomeCompound2Query
        :: forall f block a b c result crypto. (crypto ~ BlockCrypto block)
        => LSQ.Query block (QueryResult crypto a)
            -- ^ First query to run.
        -> (a -> LSQ.Query block (QueryResult crypto b))
            -- ^ Second query to run, using result from the first one.
        -> (a -> b -> LSQ.Query block (QueryResult crypto c))
            -- ^ Second query to run, using result from the first and second queries.
        -> (a -> b -> c -> result)
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

type family GenesisConfig (era :: Type) :: Type where
    GenesisConfig ByronEra = Byron.GenesisData
    GenesisConfig ShelleyEra = ShelleyGenesis
    GenesisConfig AlonzoEra = AlonzoGenesis
    GenesisConfig ConwayEra = ConwayGenesis

instance FromJSON (Query Proxy (CardanoBlock StandardCrypto)) where
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
            "LedgerState/rewardAccountSummaries" ->
                parseQueryLedgerRewardAccountSummaries id queryParams
            "LedgerState/rewardsProvenance" ->
                parseQueryLedgerRewardsProvenance id queryParams
            "LedgerState/stakePools" ->
                parseQueryLedgerStakePools id id queryParams
            "LedgerState/stakePoolsPerformances" ->
                parseQueryLedgerStakePoolsPerformances id queryParams
            "LedgerState/tip" ->
                parseQueryLedgerTip (const id) (const id) queryParams
            "LedgerState/utxo" ->
                parseQueryLedgerUtxoByOutputReference (const id) queryParams
                <|>
                parseQueryLedgerUtxoByAddress (const id) queryParams
                <|>
                parseQueryLedgerUtxo (const id) queryParams
            "LedgerState/governanceProposals" ->
                parseQueryLedgerGovernanceProposalsByProposalReference (const id) queryParams
                <|>
                parseQueryLedgerGovernanceProposals (const id) queryParams
            "LedgerState/constitution" ->
                parseQueryLedgerConstitution (const id) queryParams
            "LedgerState/constitutionalCommittee" ->
                parseQueryLedgerConstitutionalCommittee id queryParams
            "LedgerState/treasuryAndReserves" ->
                parseQueryLedgerTreasuryAndReserves id queryParams
            "LedgerState/delegateRepresentatives" ->
                parseQueryLedgerDelegateRepresentatives id queryParams
            "LedgerState/nonces" ->
                parseQueryLedgerNonces (const id) queryParams
            "LedgerState/operationalCertificates" ->
                parseQueryLedgerOperationalCertificates (const id) queryParams
            "LedgerState/dump" ->
                parseQueryLedgerDump (const id) queryParams
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

type Delegations =
    Map (Ledger.Credential 'Staking) (Ledger.KeyHash 'StakePool)

type Deposits =
    Map (Ledger.Credential 'Staking ) Coin

type VoteDelegatees =
    Map (Ledger.Credential 'Staking) Ledger.DRep

type RewardAccounts =
    Map (Ledger.Credential 'Staking) Coin

type RewardAccountSummaries =
    Map (Ledger.Credential 'Staking) RewardAccountSummary

type StakePoolsPerformances =
    ( Sh.Api.RewardParams
    , Map (Ledger.KeyHash 'StakePool) Sh.Api.RewardInfoPool
    )

data RewardAccountSummary = RewardAccountSummary
    { poolDelegate :: !(StrictMaybe (Ledger.KeyHash 'StakePool))
    , drepDelegate :: !(StrictMaybe Ledger.DRep)
    , rewards :: !Coin
    , deposit :: !Coin
    } deriving (Eq, Show, Generic)

data DRepSummary
    = AlwaysAbstain Coin (Set (Ledger.Credential 'Staking))
    | AlwaysNoConfidence Coin (Set (Ledger.Credential 'Staking))
    | Registered Coin RegisteredDRepState
    deriving (Eq, Show, Generic)

data RegisteredDRepState
    = Some Ledger.DRepState
    | Partial (Set (Ledger.Credential 'Staking))
    deriving (Eq, Show, Generic)

--
-- Encoders
--

encodeAccountState
    :: AccountState
    -> Json
encodeAccountState st =
    "treasury" .=
        encodeCoin (asTreasury st) <>
    "reserves" .=
        encodeCoin (asReserves st)
    & encodeObject

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
    :: RewardAccountSummaries
    -> Json
encodeRewardAccountSummaries =
    encodeMapAsList
        (\k summary -> encodeObject
            ( Shelley.encodeCredential "credential" k
           <> "stakePool" .=? OmitWhenNothing
                (encodeSingleton "id" . Shelley.encodePoolId) (poolDelegate summary)
           <> "delegateRepresentative" .=? OmitWhenNothing
               (encodeObject . Conway.encodeDRep) (drepDelegate summary)
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

encodeGovActionState
    :: GovActionState ConwayEra
    -> Json
encodeGovActionState st =
      "proposal" .= Conway.encodeGovActionId (gasId st)
   <> Conway.encodeProposalProcedure (gasProposalProcedure st)
   <> "since" .= (encodeSingleton "epoch" . encodeEpochNo) (gasProposedIn st)
   <> "until" .= (encodeSingleton "epoch" . encodeEpochNo) (gasExpiresAfter st)
   <> "votes" .= encodeMapAsList
        (\issuer vote -> encodeObject
            ( "issuer" .= Conway.encodeVoter issuer
           <> "vote" .= Conway.encodeVote vote
            )
        )
        (mconcat
            [ Map.mapKeys CommitteeVoter (gasCommitteeVotes st)
            , Map.mapKeys DRepVoter (gasDRepVotes st)
            , Map.mapKeys StakePoolVoter (gasStakePoolVotes st)
            ]
        )
    & encodeObject

encodeInterpreter
    :: forall crypto eras. (eras ~ CardanoEras crypto)
    => Interpreter eras
    -> Json
encodeInterpreter (coerceInterpreter -> Summary eraSummaries) =
    encodeFoldable encodeEraSummary eraSummaries

coerceInterpreter :: forall crypto eras. (eras ~ CardanoEras crypto) => Interpreter eras -> Summary eras
coerceInterpreter = deserialise . serialise

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeNonMyopicMemberRewards
    :: NonMyopicMemberRewards
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
    :: Consensus.PoolDistr crypto
    -> Json
encodePoolDistr =
    encodeMap Shelley.stringifyPoolId encodeIndividualPoolStake . Consensus.unPoolDistr
  where
    encodeIndividualPoolStake
        :: Consensus.IndividualPoolStake crypto
        -> Json
    encodeIndividualPoolStake x =
        "stake" .=
            encodeRational (Consensus.individualPoolStake x) <>
        "vrf" .=
            Shelley.encodeHash (Consensus.individualPoolStakeVrf x)
        & encodeObject

encodeRewardInfoPool
    :: Ledger.KeyHash 'StakePool
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

encodeStakePoolsPerformances
     :: StakePoolsPerformances
     -> Json
encodeStakePoolsPerformances (rp, pools) =
    "desiredNumberOfStakePools" .=
        encodeWord16 (Sh.Api.nOpt rp) <>
    "stakePoolPledgeInfluence" .=
        encodeNonNegativeInterval (Sh.Api.a0 rp) <>
    "totalRewardsInEpoch" .=
        encodeCoin (Sh.Api.rPot rp) <>
    "activeStakeInEpoch" .=
        encodeCoin activeStake <>
    "totalStakeInEpoch" .=
        encodeCoin (Sh.Api.totalStake rp) <>
    "stakePools" .=
        encodeObject (encodeMapSeries Shelley.stringifyPoolId encodeRewardInfoPool pools)
    & encodeObject
  where
    activeStake =
        Map.foldr
            (\pool -> (Sh.Api.stake pool <>))
            mempty
            pools

encodeRewardsProvenance
    :: RewardsProvenance
    -> Json
encodeRewardsProvenance rp =
    encodeObject (
           "totalStake" .= encodeCoin (totalStake rp)
        <> "activeStake" .= encodeCoin (activeStake rp)
        <> "fees" .= encodeCoin (fees rp)
        <> "incentives" .= encodeCoin (deltaR1 rp)
        <> "treasuryTax" .= encodeCoin (Coin $ deltaT1 rp)
        <> "totalRewards" .= encodeCoin (Coin $ rPot rp)
        <> "efficiency" .= encodeRational (eta rp)
        <> "stakePools" .= encodeMap Shelley.stringifyPoolId encodePoolRewardInfo (pools rp)
    )
  where
    encodePoolRewardInfo i = encodeObject $
        "relativeStake" .= encodeRational (unStakeShare $ poolRelativeStake i) <>
        "blocksMade" .= encodeNatural (poolBlocks i) <>
        "totalRewards" .= encodeCoin (poolPot i) <>
        "leaderReward" .= encodeCoin (poolLeaderReward i) <>
        "delegators" .= encodeMapAsList
            (\k v -> encodeObject $
                Shelley.encodeCredential "credential" k <>
                "stake" .= encodeCoin v
            ) (poolDelegators i)

-- Partially deserialise the NewEpochState, skipping the UTxO. This should
-- allow to reduce the in-memory footprint of fetching the new epoch state
-- for operations that do not require the entire UTxO.
deserialisePartialNewEpochState
    :: forall era.
        ( Binary.Share (Ledger.CertState era) ~
            ( Binary.Interns (Ledger.Credential Staking)
            , Binary.Interns (Ledger.KeyHash StakePool)
            , Binary.Interns (Ledger.Credential DRepRole)
            , Binary.Interns (Ledger.Credential HotCommitteeRole)
            )
        , Binary.DecCBOR (Ledger.StashedAVVMAddresses era)
        , Ledger.EraTxOut era
        , Ledger.EraGov era, Binary.DecShareCBOR (Ledger.CertState era), Ledger.EraStake era
        )
    => Serialised (Ledger.NewEpochState era)
    -> Ledger.NewEpochState era
deserialisePartialNewEpochState (Serialised bytes) =
    either error identity $ decodeCbor @era "NewEpochState" decodeNewEpochState bytes
  where
    decodeNewEpochState :: forall s. Binary.Decoder s (Ledger.NewEpochState era)
    decodeNewEpochState = Binary.decode $
        RecD Ledger.NewEpochState
          <! From
          <! From
          <! From
          <! D decodePartialEpochState
          <! From
          <! From
          <! From

    decodePartialEpochState :: forall s. Binary.Decoder s (Ledger.EpochState era)
    decodePartialEpochState =
        Binary.decodeRecordNamed "EpochState" (const 4) $
            flip evalStateT mempty $ Ledger.EpochState
                <$> lift decCBOR
                <*> decodePartialLedgerState
                <*> Binary.decSharePlusLensCBOR (
                        lens (\(cs, ks, _, _) -> (cs, ks)) (\(_, _, cd, ch) (cs, ks) -> (cs, ks, cd, ch))
                    )
                <*> Binary.decShareLensCBOR (position @2)

    decodePartialLedgerState
        :: forall s. ()
        => StateT
            ( Binary.Interns (Ledger.Credential 'Staking)
            , Binary.Interns (Ledger.KeyHash 'StakePool)
            , Binary.Interns (Ledger.Credential DRepRole)
            , Binary.Interns (Ledger.Credential HotCommitteeRole)
            )
            (Binary.Decoder s)
            (Ledger.LedgerState era)
    decodePartialLedgerState =
        Binary.decodeRecordNamedT "LedgerState" (const 2) $ do
            cert <- Binary.decSharePlusCBOR
            utxo <- Binary.decSharePlusCBOR
            pure $ Ledger.LedgerState
                Ledger.UTxOState
                    { Ledger.utxosUtxo = mempty
                    , Ledger.utxosDeposited = Ledger.utxosDeposited utxo
                    , Ledger.utxosFees = Ledger.utxosFees utxo
                    , Ledger.utxosGovState = Ledger.utxosGovState utxo
                    , Ledger.utxosInstantStake = Ledger.utxosInstantStake utxo
                    , Ledger.utxosDonation = Ledger.utxosDonation utxo
                    }
                cert

encodeSafeZone
    :: SafeZone
    -> Json
encodeSafeZone = \case
    StandardSafeZone k ->
        encodeWord64 k
    UnsafeIndefiniteSafeZone ->
        encodeNull

encodeStakePools
    :: Map (Ledger.KeyHash 'StakePool) (Ledger.PoolParams, StrictMaybe Coin)
    -> Json
encodeStakePools =
    encodeObject . encodeMapSeries Shelley.stringifyPoolId (\k (params, stake) ->
        encodeObject
            ( "id" .= Shelley.encodePoolId k
           <> Shelley.encodePoolParams params
           <> "stake" .=? OmitWhenNothing encodeCoin stake
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
    :: forall f crypto. ()
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
                    (genResult $ Proxy @ConwayEra)

parseQueryLedgerConstitutionalCommittee
    :: forall f crypto. ()
    => GenResult crypto f CommitteeMembersState
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

parseQueryLedgerDelegateRepresentatives
    :: forall f crypto. ()
    => GenResult crypto f (Map Ledger.DRep DRepSummary)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerDelegateRepresentatives genResult =
    Json.withObject "delegateRepresentatives" $ \obj -> do
        byKeys <- obj .:? "keys" .!= Set.empty
            >>= traverset (decodeDRepCredential decodeAsKeyHash)
        byScripts <- obj .:? "scripts" .!= Set.empty
            >>= traverset (decodeDRepCredential decodeAsScriptHash)
        let credentials =
                byKeys <> byScripts
        let dreps
                | null credentials = Set.empty
                | otherwise =
                    (Set.fromList [Ledger.DRepAlwaysAbstain, Ledger.DRepAlwaysNoConfidence])
                    <>
                    (Set.map Ledger.DRepCredential credentials)
        pure $ \case
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
                Just $ SomeCompound2Query
                    (LSQ.BlockQuery
                        (QueryIfCurrentConway
                            (GetDRepDelegations dreps)
                        )
                    )
                    (\_ -> LSQ.BlockQuery
                        (QueryIfCurrentConway
                            (GetDRepState credentials)
                        )
                    )
                    (\_ _ -> LSQ.BlockQuery
                        (QueryIfCurrentConway
                            (GetDRepStakeDistr dreps)
                        )
                    )
                    (\delegs st distr -> mergeAll delegs (Map.mapKeys Ledger.credToDRep st) distr)
                    (eraMismatchOrResult (encodeMapAsList encodeDRepSummary . withDefaultProtocolDreps))
                    genResult
  where
    withDefaultProtocolDreps
        :: Map Ledger.DRep DRepSummary
        -> Map Ledger.DRep DRepSummary
    withDefaultProtocolDreps
        = Map.alter (<|> Just (AlwaysAbstain mempty Set.empty)) Ledger.DRepAlwaysAbstain
        . Map.alter (<|> Just (AlwaysNoConfidence mempty Set.empty)) Ledger.DRepAlwaysNoConfidence

    mergeAll
        :: Map Ledger.DRep (Set (Ledger.Credential 'Staking))
        -> Map Ledger.DRep Ledger.DRepState
        -> Map Ledger.DRep Coin
        -> Map Ledger.DRep DRepSummary
    mergeAll delegs =
        Map.merge
            -- State but no distr
            --
            -- NOTE:
            -- Somehow, DReps with 0 stake do not figure in the stake
            -- distribution map; so we must account for them here by adding an
            -- empty coin.
            (Map.mapMissing $ \key st ->
                Registered mempty (
                    Some (
                        st { Ledger.drepDelegs = fromMaybe (Ledger.drepDelegs st) (Map.lookup key delegs) }
                    )
                )
            )

            -- Distr but no state
            --
            -- NOTE:
            -- Somehow, the DRepState is not accessible for pre-defined dreps;
            -- Even though it would be pretty interesting to have ways of
            -- querying their delegates.
            --
            -- So we pull them apart into a different constructor variant so we
            -- can easily encode them differently.
            (Map.mapMissing $ \key coin ->
                let delegators = fromMaybe Set.empty (Map.lookup key delegs) in
                case key of
                  Ledger.DRepAlwaysAbstain -> AlwaysAbstain coin delegators
                  Ledger.DRepAlwaysNoConfidence -> AlwaysNoConfidence coin delegators
                  Ledger.DRepCredential{} -> Registered coin (Partial delegators)
            )

            -- Both distr and state (only occurs for registered DReps)
            (Map.zipWithMatched $ \key st coin ->
                Registered coin (
                    Some (
                        st { Ledger.drepDelegs = fromMaybe (Ledger.drepDelegs st) (Map.lookup key delegs) }
                    )
                )
            )

    encodeDRepSummary
        :: Ledger.DRep
        -> DRepSummary
        -> Json
    encodeDRepSummary drep = encodeObject . (Conway.encodeDRep drep <>) . \case
        AlwaysAbstain stake delegators ->
            "stake" .= encodeCoin stake
         <> "delegators" .=
              encodeFoldable
                  (encodeObject . Shelley.encodeCredential "credential")
                  delegators
        AlwaysNoConfidence stake delegators ->
            "stake" .= encodeCoin stake
         <> "delegators" .=
              encodeFoldable
                  (encodeObject . Shelley.encodeCredential "credential")
                  delegators
        Registered stake (Partial delegators) ->
            "stake" .= encodeCoin stake
         <> "delegators" .=
              encodeFoldable
                  (encodeObject . Shelley.encodeCredential "credential")
                  delegators
        Registered stake (Some summary) ->
              "mandate" .=
                (encodeSingleton "epoch" . encodeEpochNo) (Ledger.drepExpiry summary)
           <> "deposit" .=
                encodeCoin (Ledger.drepDeposit summary)
           <> "stake" .=
                encodeCoin stake
           <> "metadata" .=? OmitWhenNothing
                Conway.encodeAnchor (Ledger.drepAnchor summary)
           <> "delegators" .=
                encodeFoldable
                    (encodeObject . Shelley.encodeCredential "credential")
                    (Ledger.drepDelegs summary)

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
    :: forall crypto f. ()
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
                    (genResultInEraTPraos (Proxy @ShelleyEra))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @AllegraEra))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @MaryEra))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint))
                    (genResultInEraTPraos (Proxy @AlonzoEra))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint @(Praos crypto)))
                    (genResultInEraPraos (Proxy @BabbageEra))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetLedgerTip))
                    (eraMismatchOrResult (encodePoint . castPoint @(Praos crypto)))
                    (genResultInEraPraos (Proxy @ConwayEra))

parseQueryLedgerTreasuryAndReserves
    :: forall crypto f. ()
    => GenResult crypto f AccountState
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerTreasuryAndReserves genResult =
    Json.withObject "treasuryAndReserves" $ \obj -> do
        guard (null obj) $>
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodeAccountState)
                genResult
            )
            .
            ( \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    LSQ.BlockQuery $ QueryIfCurrentShelley GetAccountState
                SomeShelleyEra ShelleyBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetAccountState
                SomeShelleyEra ShelleyBasedEraMary ->
                    LSQ.BlockQuery $ QueryIfCurrentMary GetAccountState
                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    LSQ.BlockQuery $ QueryIfCurrentAlonzo GetAccountState
                SomeShelleyEra ShelleyBasedEraBabbage ->
                    LSQ.BlockQuery $ QueryIfCurrentBabbage GetAccountState
                SomeShelleyEra ShelleyBasedEraConway ->
                    LSQ.BlockQuery $ QueryIfCurrentConway GetAccountState
            )

parseQueryLedgerDump
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Serialised (Ledger.NewEpochState era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerDump genResult =
    Json.withObject "dump" $ \obj -> do
        filepath :: FilePath <- obj .: "to"
        pure $ \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeEffectfullQuery
                    (LSQ.BlockQuery $ QueryIfCurrentShelley $ GetCBOR DebugNewEpochState)
                    (saveToDisk filepath . eraMismatchOrResult unSerialised)
                    (genResult $ Proxy @ShelleyEra)

            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeEffectfullQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAllegra $ GetCBOR DebugNewEpochState)
                    (saveToDisk filepath . eraMismatchOrResult unSerialised)
                    (genResult $ Proxy @AllegraEra)

            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeEffectfullQuery
                    (LSQ.BlockQuery $ QueryIfCurrentMary $ GetCBOR DebugNewEpochState)
                    (saveToDisk filepath . eraMismatchOrResult unSerialised)
                    (genResult $ Proxy @MaryEra)

            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeEffectfullQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAlonzo $ GetCBOR DebugNewEpochState)
                    (saveToDisk filepath . eraMismatchOrResult unSerialised)
                    (genResult $ Proxy @AlonzoEra)

            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeEffectfullQuery
                    (LSQ.BlockQuery $ QueryIfCurrentBabbage $ GetCBOR DebugNewEpochState)
                    (saveToDisk filepath . eraMismatchOrResult unSerialised)
                    (genResult $ Proxy @BabbageEra)

            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeEffectfullQuery
                    (LSQ.BlockQuery $ QueryIfCurrentConway $ GetCBOR DebugNewEpochState)
                    (saveToDisk filepath . eraMismatchOrResult unSerialised)
                    (genResult $ Proxy @ConwayEra)
  where
    saveToDisk
        :: forall m. (MonadDisk m)
        => FilePath
        -> Either Json LByteString
        -> m (Either Json Json)
    saveToDisk filepath = \case
        Left err -> pure (Left err)
        Right bytes -> do
            writeLByteString filepath bytes
            pure $ Right encodeNull

parseQueryLedgerNonces
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (ChainDepState (EraProto era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerNonces genResult =
    Json.withObject "nonces" $ \obj -> do
        guard (null obj) $>
            \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentShelley DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @ShelleyEra)

                SomeShelleyEra ShelleyBasedEraAllegra ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentAllegra DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @AllegraEra)

                SomeShelleyEra ShelleyBasedEraMary ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentMary DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @MaryEra)

                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentAlonzo DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @AlonzoEra)

                SomeShelleyEra ShelleyBasedEraBabbage ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentBabbage DebugChainDepState)
                        (eraMismatchOrResult encodePraosState)
                        (genResult $ Proxy @BabbageEra)

                SomeShelleyEra ShelleyBasedEraConway ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentConway DebugChainDepState)
                        (eraMismatchOrResult encodePraosState)
                        (genResult $ Proxy @ConwayEra)
  where
    encodeTPraosState
        :: TPraosState
        -> Json
    encodeTPraosState (TPraosState _ st) =
        encodeObject (
            "epochNonce" .=
                Shelley.encodeNonce (TPraos.ticknStateEpochNonce tickn)
         <> "candidateNonce" .=
                Shelley.encodeNonce candidateNonce
         <> "evolvingNonce" .=
                Shelley.encodeNonce evolvingNonce
         <> "lastEpochLastAncestor" .=
                Shelley.encodeNonce (TPraos.ticknStatePrevHashNonce tickn)
        )
      where
        TPraos.PrtclState _ evolvingNonce candidateNonce = TPraos.csProtocol st
        tickn = TPraos.csTickn st

    encodePraosState
        :: PraosState
        -> Json
    encodePraosState st =
        encodeObject (
            "epochNonce" .=
                Shelley.encodeNonce (praosStateEpochNonce st)
         <> "candidateNonce" .=
                Shelley.encodeNonce (praosStateCandidateNonce st)
         <> "evolvingNonce" .=
                Shelley.encodeNonce (praosStateEvolvingNonce st)
         <> "lastEpochLastAncestor" .=
                Shelley.encodeNonce (praosStateLastEpochBlockNonce st)
        )

parseQueryLedgerOperationalCertificates
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (ChainDepState (EraProto era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerOperationalCertificates genResult =
    Json.withObject "operationalCertificates" $ \obj -> do
        guard (null obj) $>
            \case
                SomeShelleyEra ShelleyBasedEraShelley ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentShelley DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @ShelleyEra)

                SomeShelleyEra ShelleyBasedEraAllegra ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentAllegra DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @AllegraEra)

                SomeShelleyEra ShelleyBasedEraMary ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentMary DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @MaryEra)

                SomeShelleyEra ShelleyBasedEraAlonzo ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentAlonzo DebugChainDepState)
                        (eraMismatchOrResult encodeTPraosState)
                        (genResult $ Proxy @AlonzoEra)

                SomeShelleyEra ShelleyBasedEraBabbage ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentBabbage DebugChainDepState)
                        (eraMismatchOrResult encodePraosState)
                        (genResult $ Proxy @BabbageEra)

                SomeShelleyEra ShelleyBasedEraConway ->
                    Just $ SomeStandardQuery
                        (LSQ.BlockQuery $ QueryIfCurrentConway DebugChainDepState)
                        (eraMismatchOrResult encodePraosState)
                        (genResult $ Proxy @ConwayEra)
  where
    encodeTPraosState
        :: TPraosState
        -> Json
    encodeTPraosState (TPraosState _ st) =
        encodeMap (Shelley.stringifyPoolId . coerceKeyRole) encodeWord64 certificates
      where
        TPraos.PrtclState certificates _ _ = TPraos.csProtocol st

    encodePraosState
        :: PraosState
        -> Json
    encodePraosState =
        encodeMap (Shelley.stringifyPoolId . coerceKeyRole) encodeWord64 . praosStateOCertCounters


parseQueryLedgerProjectedRewards
    :: forall crypto f. ()
    => GenResult crypto f NonMyopicMemberRewards
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerProjectedRewards genResult =
    Json.withObject (toString query) $ \obj -> do
        byStake <- obj .:? "stake" .!= Set.empty
            >>= traverset (fmap Left . decodeCoin)
        byScript <- obj .:? "scripts" .!= Set.empty
            >>= traverset (fmap Right . decodeStakingCredential decodeAsScriptHash)
        byKey <- obj .:? "keys" .!= Set.empty
            >>= traverset (fmap Right . decodeStakingCredential decodeAsKeyHash)
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
    :: forall crypto f. ()
    => GenResult crypto f RewardAccountSummaries
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerRewardAccountSummaries genResult =
    let query = "rewardAccountSummaries" in
    Json.withObject (toString @Text query) $ \obj -> do
        byScript <- obj .:? "scripts" .!= Set.empty
            >>= traverset (decodeStakingCredential decodeAsScriptHash)
        byKey <- obj .:? "keys" .!= Set.empty
            >>= traverset (decodeStakingCredential decodeAsKeyHash)
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
                    (mergeAll mempty)
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
                    (mergeAll mempty)
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
                    (mergeAll mempty)
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
                    (mergeAll mempty)
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
                    (mergeAll mempty)
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult

            SomeShelleyEra ShelleyBasedEraConway ->
                SomeCompound2Query
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
                    (\_ deposits -> LSQ.BlockQuery
                        (QueryIfCurrentConway
                            (GetFilteredVoteDelegatees (Map.keysSet deposits))
                        )
                    )
                    (\a b c -> mergeAll c a b)
                    (eraMismatchOrResult encodeRewardAccountSummaries)
                    genResult
  where
    mergeAll
        :: VoteDelegatees
        -> (Delegations, RewardAccounts)
        -> Deposits
        -> RewardAccountSummaries
    mergeAll dreps (dlg, rwd) =
        Map.merge
            Map.dropMissing
            Map.dropMissing
            (Map.zipWithMatched (const identity))
            (Map.merge
                -- drep, but no pool; use default value for rewards --
                -- although the case is probably impossible?
                (Map.mapMissing $ \_ drep ->
                    RewardAccountSummary SNothing (SJust drep) (Coin 0)
                )
                -- no drep, but a pool
                (Map.mapMissing $ \_ mk -> mk SNothing)
                -- Both drep and pool
                (Map.zipWithMatched $ \_ drep mk -> mk (SJust drep))
                dreps
                (Map.merge
                    Map.dropMissing
                    Map.dropMissing
                    (Map.zipWithMatched
                        (\_ poolDelegate rewards drep ->
                            RewardAccountSummary (SJust poolDelegate) drep rewards
                        )
                    )
                    dlg
                    rwd
                )
            )

parseQueryLedgerProtocolParameters
    :: forall f crypto. ()
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
                    (genResultInEra (Proxy @ShelleyEra))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetCurrentPParams))
                    (eraMismatchOrResult Shelley.encodePParams)
                    (genResultInEra (Proxy @AllegraEra))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetCurrentPParams))
                    (eraMismatchOrResult Shelley.encodePParams)
                    (genResultInEra (Proxy @MaryEra))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetCurrentPParams))
                    (eraMismatchOrResult Alonzo.encodePParams)
                    (genResultInEra (Proxy @AlonzoEra))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetCurrentPParams))
                    (eraMismatchOrResult Babbage.encodePParams)
                    (genResultInEra (Proxy @BabbageEra))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetCurrentPParams))
                    (eraMismatchOrResult Conway.encodePParams)
                    (genResultInEra (Proxy @ConwayEra))

parseQueryLedgerGovernanceProposals
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Ledger.GovState era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerGovernanceProposals genResultInEra =
    let query = "governanceProposals" in
    Json.withObject (toString @Text query) $ \obj -> do
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
                    (LSQ.BlockQuery $ QueryIfCurrentConway GetGovState)
                    (eraMismatchOrResult
                        (encodeFoldable encodeGovActionState
                            . view Ledger.Conway.pPropsL
                            . Ledger.Conway.cgsProposals
                        )
                    )
                    (genResultInEra (Proxy @ConwayEra))

parseQueryLedgerGovernanceProposalsByProposalReference
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Ledger.GovState era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerGovernanceProposalsByProposalReference genResultInEra =
    let query = "governanceProposals" in
    Json.withObject (toString @Text query) $ \obj -> do
        ks <- obj .: "proposals" >>= traverset decodeGovActionId
        pure $ \case
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
                    (LSQ.BlockQuery $ QueryIfCurrentConway GetGovState)
                    (eraMismatchOrResult
                        (encodeFoldable encodeGovActionState
                            . (`Map.restrictKeys` ks)
                            . OMap.toMap
                            . view Ledger.Conway.pPropsL
                            . Ledger.Conway.cgsProposals
                        )
                    )
                    (genResultInEra (Proxy @ConwayEra))


parseQueryLedgerLiveStakeDistribution
    :: forall crypto f. ()
    => GenResult crypto f (Consensus.PoolDistr crypto)
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
    :: forall crypto f. ()
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
                    (genResultInEra (Proxy @ShelleyEra))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetUTxOWhole))
                    (eraMismatchOrResult Allegra.encodeUtxo)
                    (genResultInEra (Proxy @AllegraEra))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetUTxOWhole))
                    (eraMismatchOrResult Mary.encodeUtxo)
                    (genResultInEra (Proxy @MaryEra))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetUTxOWhole))
                    (eraMismatchOrResult Alonzo.encodeUtxo)
                    (genResultInEra (Proxy @AlonzoEra))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetUTxOWhole))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @BabbageEra))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetUTxOWhole))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @ConwayEra))

parseQueryLedgerUtxoByAddress
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerUtxoByAddress genResultInEra =
    let query = "utxo" in
    Json.withObject (toString @Text query) $ \obj -> do
        addrs <- obj .: "addresses" >>= traverset decodeAddress
        pure $ \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Shelley.encodeUtxo)
                    (genResultInEra (Proxy @ShelleyEra))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Allegra.encodeUtxo)
                    (genResultInEra (Proxy @AllegraEra))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Mary.encodeUtxo)
                    (genResultInEra (Proxy @MaryEra))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Alonzo.encodeUtxo)
                    (genResultInEra (Proxy @AlonzoEra))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @BabbageEra))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway (GetUTxOByAddress addrs)))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @ConwayEra))

parseQueryLedgerUtxoByOutputReference
    :: forall crypto f. ()
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerUtxoByOutputReference genResultInEra =
    let query = "utxo" in
    Json.withObject (toString @Text query) $ \obj -> do
        ins <- obj .: "outputReferences" >>= traverset decodeTxIn
        pure $ \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentShelley (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Shelley.encodeUtxo)
                    (genResultInEra (Proxy @ShelleyEra))
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Allegra.encodeUtxo)
                    (genResultInEra (Proxy @AllegraEra))
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentMary (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Mary.encodeUtxo)
                    (genResultInEra (Proxy @MaryEra))
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Alonzo.encodeUtxo)
                    (genResultInEra (Proxy @AlonzoEra))
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentBabbage (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @BabbageEra))
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeStandardQuery
                    (LSQ.BlockQuery $ QueryIfCurrentConway (GetUTxOByTxIn ins))
                    (eraMismatchOrResult Babbage.encodeUtxo)
                    (genResultInEra (Proxy @ConwayEra))

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
                    Byron.encodeGenesisData
                    (const genByron)
            "shelley" -> do
                pure $ const $ Just $ SomeAdHocQuery
                    GetShelleyGenesis
                    Shelley.encodeGenesis
                    (const genShelley)
            "alonzo" -> do
                pure $ const $ Just $ SomeAdHocQuery
                    GetAlonzoGenesis
                    Alonzo.encodeGenesis
                    (const genAlonzo)
            "conway" -> do
                pure $ const $ Just $ SomeAdHocQuery
                    GetConwayGenesis
                    Conway.encodeGenesis
                    (const genConway)
            (_unknownEra :: Text) -> do
                fail "Invalid era parameter. Only 'byron', 'shelley', \
                     \'alonzo' and 'conway' have a genesis configuration."

parseQueryLedgerStakePoolsPerformances
    :: forall crypto f. ()
    => GenResult crypto f StakePoolsPerformances
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerStakePoolsPerformances genResult =
    Json.withObject "stakePoolsPerformances" $ \obj -> do
        guard (null obj) $>
            ( \queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodeStakePoolsPerformances)
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

parseQueryLedgerRewardsProvenance
    :: forall crypto f. ()
    => GenResult crypto f RewardsProvenance
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerRewardsProvenance genResult =
    Json.withObject "rewardsProvenance" $ \obj -> do
        guard (null obj) $> \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery $ QueryIfCurrentShelley GetGenesisConfig)
                    (\_ -> LSQ.BlockQuery $ QueryIfCurrentShelley (GetCBOR DebugNewEpochState))
                    (\(getCompactGenesis -> genesis) -> newRewardsProvenance genesis . deserialisePartialNewEpochState)
                    (eraMismatchOrResult encodeRewardsProvenance)
                    genResult
            SomeShelleyEra ShelleyBasedEraAllegra ->
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAllegra GetGenesisConfig)
                    (\_ -> LSQ.BlockQuery $ QueryIfCurrentAllegra (GetCBOR DebugNewEpochState))
                    (\(getCompactGenesis -> genesis) -> newRewardsProvenance genesis . deserialisePartialNewEpochState)
                    (eraMismatchOrResult encodeRewardsProvenance)
                    genResult
            SomeShelleyEra ShelleyBasedEraMary ->
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery $ QueryIfCurrentMary GetGenesisConfig)
                    (\_ -> LSQ.BlockQuery $ QueryIfCurrentMary (GetCBOR DebugNewEpochState))
                    (\(getCompactGenesis -> genesis) -> newRewardsProvenance genesis . deserialisePartialNewEpochState)
                    (eraMismatchOrResult encodeRewardsProvenance)
                    genResult
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery $ QueryIfCurrentAlonzo GetGenesisConfig)
                    (\_ -> LSQ.BlockQuery $ QueryIfCurrentAlonzo (GetCBOR DebugNewEpochState))
                    (\(getCompactGenesis -> genesis) -> newRewardsProvenance genesis . deserialisePartialNewEpochState)
                    (eraMismatchOrResult encodeRewardsProvenance)
                    genResult
            SomeShelleyEra ShelleyBasedEraBabbage ->
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery $ QueryIfCurrentBabbage GetGenesisConfig)
                    (\_ -> LSQ.BlockQuery $ QueryIfCurrentBabbage (GetCBOR DebugNewEpochState))
                    (\(getCompactGenesis -> genesis) -> newRewardsProvenance genesis . deserialisePartialNewEpochState)
                    (eraMismatchOrResult encodeRewardsProvenance)
                    genResult
            SomeShelleyEra ShelleyBasedEraConway ->
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery $ QueryIfCurrentConway GetGenesisConfig)
                    (\_ -> LSQ.BlockQuery $ QueryIfCurrentConway (GetCBOR DebugNewEpochState))
                    (\(getCompactGenesis -> genesis) -> newRewardsProvenance genesis . deserialisePartialNewEpochState)
                    (eraMismatchOrResult encodeRewardsProvenance)
                    genResult

parseQueryLedgerStakePools
    :: forall crypto f. ()
    => GenResult crypto f (Map (Ledger.KeyHash 'StakePool) (Ledger.PoolParams))
    -> GenResult crypto f (Map (Ledger.KeyHash 'StakePool) (Ledger.PoolParams, StrictMaybe Coin))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseQueryLedgerStakePools genResultNoStake genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        includeStake <- obj .:? "includeStake" .!= False
        obj .:? "stakePools" >>= \case
            Nothing -> pure (getAllStakePools includeStake)
            Just params -> do
                ids <- traverset decodePoolId params
                pure (getFilteredStakePools includeStake ids)
  where
    encodeStakePoolsWithoutStake = encodeStakePools . Map.map (,SNothing)

    mergeDistrAndParams
        :: Map (Ledger.KeyHash 'StakePool) Coin
        -> Map (Ledger.KeyHash 'StakePool) Ledger.PoolParams
        -> Map (Ledger.KeyHash 'StakePool) (Ledger.PoolParams, StrictMaybe Coin)
    mergeDistrAndParams =
        Map.merge
           Map.dropMissing
           (Map.mapMissing $ \_ params -> (params, SJust mempty))
           (Map.zipWithMatched $ \_ distr params -> (params, SJust distr))

    getFilteredStakePools includeStake ids
        | includeStake = \case
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
                Just $ SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway (GetSPOStakeDistr ids)))
                    (\_ -> LSQ.BlockQuery (QueryIfCurrentConway (GetStakePoolParams ids)))
                    mergeDistrAndParams
                    (eraMismatchOrResult encodeStakePools)
                    genResult

        | otherwise =
            (\queryDef -> Just $ SomeStandardQuery
                queryDef
                (eraMismatchOrResult encodeStakePoolsWithoutStake)
                genResultNoStake
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

    getAllStakePools includeStake
        | includeStake = \case
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
                Just $ SomeCompound2Query
                    (LSQ.BlockQuery (QueryIfCurrentConway GetStakePools))
                    (\_ -> LSQ.BlockQuery (QueryIfCurrentConway (GetSPOStakeDistr Set.empty)))
                    -- NOTE: We need the extra 'GetStakePools' query here because:
                    --
                    -- 1. We cannot get all params for all stake pools without explicitly listing them.
                    -- 2. The key set from SPOStakeDistr only contains pools that have delegators; so we're missing quite a lot of keys.
                    (\allPools _ -> LSQ.BlockQuery (QueryIfCurrentConway (GetStakePoolParams allPools)))
                    (const mergeDistrAndParams)
                    (eraMismatchOrResult encodeStakePools)
                    genResult

        | otherwise = Just . \case
            SomeShelleyEra ShelleyBasedEraShelley ->
                SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentShelley GetStakePools))
                    (LSQ.BlockQuery . QueryIfCurrentShelley . GetStakePoolParams)
                    (const identity)
                    (eraMismatchOrResult encodeStakePoolsWithoutStake)
                    genResultNoStake
            SomeShelleyEra ShelleyBasedEraAllegra ->
                SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentAllegra GetStakePools))
                    (LSQ.BlockQuery . QueryIfCurrentAllegra . GetStakePoolParams)
                    (const identity)
                    (eraMismatchOrResult encodeStakePoolsWithoutStake)
                    genResultNoStake
            SomeShelleyEra ShelleyBasedEraMary ->
                SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentMary GetStakePools))
                    (LSQ.BlockQuery . QueryIfCurrentMary . GetStakePoolParams)
                    (const identity)
                    (eraMismatchOrResult encodeStakePoolsWithoutStake)
                    genResultNoStake
            SomeShelleyEra ShelleyBasedEraAlonzo ->
                SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentAlonzo GetStakePools))
                    (LSQ.BlockQuery . QueryIfCurrentAlonzo . GetStakePoolParams)
                    (const identity)
                    (eraMismatchOrResult encodeStakePoolsWithoutStake)
                    genResultNoStake
            SomeShelleyEra ShelleyBasedEraBabbage ->
                SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentBabbage GetStakePools))
                    (LSQ.BlockQuery . QueryIfCurrentBabbage . GetStakePoolParams)
                    (const identity)
                    (eraMismatchOrResult encodeStakePoolsWithoutStake)
                    genResultNoStake
            SomeShelleyEra ShelleyBasedEraConway ->
                SomeCompoundQuery
                    (LSQ.BlockQuery (QueryIfCurrentConway GetStakePools))
                    (LSQ.BlockQuery . QueryIfCurrentConway . GetStakePoolParams)
                    (const identity)
                    (eraMismatchOrResult encodeStakePoolsWithoutStake)
                    genResultNoStake

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
    :: Json.Value
    -> Json.Parser Ledger.Addr
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
    :: Json.Value
    -> Json.Parser Ledger.Mary.MultiAsset
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

decodeStakingCredential
    :: (ByteString -> Json.Parser (Ledger.Credential 'Staking))
    -> Json.Value
    -> Json.Parser (Ledger.Credential 'Staking)
decodeStakingCredential decodeAsKeyOrScript =
    Json.withText "StakingCredential" $ \(encodeUtf8 -> str) -> asum
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
             \stake key hash, stake script hash or a bech32-encoded stake address, \
             \stake key hash or script hash with one of the following respective \
             \prefixes: stake, stake_vkh or script."
  where
    decodeAsStakeAddress
        :: ByteString
        -> Json.Parser (Ledger.Credential 'Staking)
    decodeAsStakeAddress =
        fmap Ledger.raCredential
            . maybe invalidStakeAddress pure
            . Ledger.deserialiseRewardAccount
      where
        invalidStakeAddress =
            fail "invalid stake address"

decodeDRepCredential
    :: (ByteString -> Json.Parser (Ledger.Credential 'DRepRole))
    -> Json.Value
    -> Json.Parser (Ledger.Credential 'DRepRole)
decodeDRepCredential decodeAsKeyOrScript =
    Json.withText "DRepCredential" $ \(encodeUtf8 -> str) -> asum
        [ decodeBase16 str >>=
            decodeAsKeyOrScript
        , decodeBech32 str >>= whenHrp (== [humanReadablePart|drep_vkh|])
            decodeAsKeyHash
        , decodeBech32 str >>= whenHrp (== [humanReadablePart|drep_script|])
            decodeAsScriptHash
        ]
       <|>
        fail "Unable to decode credential. It must be either a base16-encoded \
             \key hash, script hash or a bech32-encoded key hash or script hash \
             \with one of the following respective prefixes: drep_vkh, drep_script."

decodeAsKeyHash
    :: forall keyRole. ()
    => ByteString
    -> Json.Parser (Ledger.Credential keyRole)
decodeAsKeyHash =
    fmap (Ledger.KeyHashObj . Ledger.KeyHash)
        . maybe invalidKeyHash pure
        . hashFromBytes
  where
    invalidKeyHash =
        fail "invalid credential key hash"

decodeAsScriptHash
    :: forall keyRole. ()
    => ByteString
    -> Json.Parser (Ledger.Credential keyRole)
decodeAsScriptHash =
    fmap (Ledger.ScriptHashObj . Ledger.ScriptHash)
        . maybe invalidScriptHash pure
        . hashFromBytes
  where
    invalidScriptHash =
        fail "invalid credential script hash"

decodeDatumHash
    :: Json.Value
    -> Json.Parser Ledger.Plutus.DataHash
decodeDatumHash =
    fmap unsafeMakeSafeHash . decodeHash

decodeGovActionId
    :: Json.Value
    -> Json.Parser Ledger.GovActionId
decodeGovActionId = Json.withObject "GovActionId" $ \o -> do
    txid <- o .: "transaction" >>= (.: "id") >>= fromBase16
    ix <- o .: "index"
    pure $ Ledger.GovActionId (Ledger.TxId txid) (Ledger.GovActionIx ix)
  where
    failure =
        fail "couldn't decode transaction id from base16"
    fromBase16 =
        maybe failure
            (pure . unsafeMakeSafeHash) . hashFromTextAsHex

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
    :: Text
    -> Json.Parser Ledger.Mary.PolicyID
decodePolicyId =
    maybe
        invalidPolicyId
        (pure . Ledger.Mary.PolicyID . Ledger.ScriptHash)
    . hashFromTextAsHex
  where
    invalidPolicyId = fail "failed to decode policy id for a given asset."

decodePoolId
    :: Json.Value
    -> Json.Parser (Ledger.KeyHash 'StakePool)
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
        , Ledger.NativeScript era ~ Ledger.Allegra.Timelock era
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
        , constraint ~ (MostRecentEra (CardanoBlock crypto) ~ ConwayEra)
        , crypto ~ StandardCrypto
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
    pure $  deserialiseCBOR @BabbageEra GenTxBabbage bytes
        <|> deserialiseCBOR @ConwayEra  GenTxConway  bytes
        <|> deserialiseCBOR @AlonzoEra  GenTxAlonzo  bytes
        <|> deserialiseCBOR @MaryEra    GenTxMary    bytes
        <|> deserialiseCBOR @AllegraEra GenTxAllegra bytes
        <|> deserialiseCBOR @ShelleyEra GenTxShelley bytes
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
    :: ( Ledger.Allegra.AllegraEraScript era
       , Ledger.NativeScript era ~ Ledger.Allegra.Timelock era
       )
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
        pure $ Ledger.Shelley.RequireSignature (Ledger.KeyHash from)
    decodeAllOf o = do
        xs <- StrictSeq.fromList <$> (o .: "from")
        Ledger.Shelley.RequireAllOf <$> traverse decodeTimeLock xs
    decodeAnyOf o = do
        xs <- StrictSeq.fromList <$> (o .: "from")
        Ledger.Shelley.RequireAnyOf <$> traverse decodeTimeLock xs
    decodeSomeOf o = do
        n <- o .: "atLeast"
        xs <- StrictSeq.fromList <$> (o .: "from")
        Ledger.Shelley.RequireMOf n <$> traverse decodeTimeLock xs
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
    :: Json.Value
    -> Json.Parser Ledger.TxId
decodeTxId = Json.withText "TxId" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8
    case hashFromBytes bytes of
        Nothing ->
            fail "couldn't interpret bytes as blake2b-256 digest"
        Just h ->
            pure $ Ledger.TxId (Ledger.unsafeMakeSafeHash h)

decodeTxIn
    :: Json.Value
    -> Json.Parser Ledger.TxIn
decodeTxIn = Json.withObject "TxIn" $ \o -> do
    txid <- o .: "transaction" >>= (.: "id") >>= fromBase16
    ix <- o .: "index"
    pure $ Ledger.TxIn (Ledger.TxId txid) (Ledger.TxIx ix)
  where
    failure =
        fail "couldn't decode transaction id from base16"
    fromBase16 =
        maybe failure (pure . unsafeMakeSafeHash) . hashFromTextAsHex

decodeTxOut
    :: forall crypto. ()
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
                Ledger.Plutus.Datum <$> decodeBinaryData @BabbageEra x
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
                Ledger.Plutus.Datum <$> decodeBinaryData @ConwayEra x
            (Just{}, Just{}) ->
                fail "specified both 'datumHash' & 'datum'"

        script <-
            o .:? "script" >>= maybe
                (pure SNothing)
                (fmap SJust . decodeScript)

        pure $ TxOutInConwayEra $ Ledger.Babbage.BabbageTxOut address value datum script

decodeUtxo
    :: forall block. ()
    => Json.Value
    -> Json.Parser (MultiEraUTxO block)
decodeUtxo v = do
    xs <- Json.parseJSONList v
    (UTxOInBabbageEra . Sh.UTxO . Map.fromList <$> traverse decodeBabbageUtxoEntry xs) <|>
        (UTxOInConwayEra . Sh.UTxO . Map.fromList <$> traverse decodeConwayUtxoEntry xs)
  where
    decodeBabbageUtxoEntry
        :: Json.Value
        -> Json.Parser (Ledger.TxIn, Ledger.Babbage.BabbageTxOut BabbageEra)
    decodeBabbageUtxoEntry o =
        (,) <$> decodeTxIn o
            <*> (decodeTxOut o >>= \case
                    TxOutInBabbageEra o' -> pure o'
                    TxOutInConwayEra{}   -> empty
                )

    decodeConwayUtxoEntry
        :: Json.Value
        -> Json.Parser (Ledger.TxIn, Ledger.Babbage.BabbageTxOut ConwayEra)
    decodeConwayUtxoEntry o =
        (,) <$> decodeTxIn o
            <*> (decodeTxOut o >>= \case
                    TxOutInBabbageEra o' -> pure (upgrade o')
                    TxOutInConwayEra  o' -> pure o'
                )

decodeValue
    :: Json.Value
    -> Json.Parser (Ledger.Value AlonzoEra)
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
    :: forall proto era. ()
    => Point (ShelleyBlock proto era)
    -> Point (CardanoBlock (ProtoCrypto proto))
castPoint = \case
    GenesisPoint -> GenesisPoint
    BlockPoint slot h -> BlockPoint slot (cast h)
  where
    cast (unShelleyHash -> UnsafeHash h) = coerce h

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
