--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Data.Json.Shelley where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Ledger.Crypto
    ( Crypto
    )
import Cardano.Ledger.Era
    ( Era
    )
import Cardano.Ledger.Keys
    ( GenDelegPair (..)
    , KeyRole (..)
    )
import Cardano.Ledger.Shelley.Constraints
    ( UsesAuxiliary
    , UsesScript
    , UsesTxBody
    , UsesValue
    )
import Control.State.Transition
    ( STS (..)
    )
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.ByteString.Bech32
    ( HumanReadablePart (..)
    , encodeBech32
    )
import Ouroboros.Consensus.Cardano.Block
    ( ShelleyEra
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    , ShelleyHash (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Ogmios.Data.Json.Byron as Byron

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC

import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Protocol.TPraos.OCert as TPraos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Sh
import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.Delegation.Certificates as Sh
import qualified Cardano.Ledger.Shelley.Genesis as Sh
import qualified Cardano.Ledger.Shelley.LedgerState as Sh
import qualified Cardano.Ledger.Shelley.Metadata as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules.Deleg as Sh
import qualified Cardano.Ledger.Shelley.Rules.Delegs as Sh
import qualified Cardano.Ledger.Shelley.Rules.Delpl as Sh
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Sh
import qualified Cardano.Ledger.Shelley.Rules.Pool as Sh
import qualified Cardano.Ledger.Shelley.Rules.Ppup as Sh
import qualified Cardano.Ledger.Shelley.Rules.Utxo as Sh
import qualified Cardano.Ledger.Shelley.Rules.Utxow as Sh
import qualified Cardano.Ledger.Shelley.Scripts as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxBody as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

type ShelleyBased era =
  ( UsesValue era
  , UsesTxBody era
  , UsesScript era
  , UsesAuxiliary era
  )

--
-- Encoders
--

encodeAddress
    :: Ledger.Addr crypto
    -> Json
encodeAddress = \case
    Ledger.AddrBootstrap addr ->
        Byron.encodeAddress (Ledger.unBootstrapAddress addr)
    addr@(Ledger.Addr network _ _) ->
        encodeByteStringBech32 (hrp network) (Ledger.serialiseAddr addr)
  where
    hrp = \case
        Ledger.Mainnet -> hrpAddrMainnet
        Ledger.Testnet -> hrpAddrTestnet

encodeAuxiliaryDataHash
    :: Crypto crypto
    => Ledger.AuxiliaryDataHash crypto
    -> Json
encodeAuxiliaryDataHash =
    encodeHash . Ledger.extractHash . Ledger.unsafeAuxiliaryDataHash

encodeBHeader
    :: Crypto crypto
    => TPraos.BHeader crypto
    -> Json
encodeBHeader (TPraos.BHeader hBody hSig) =
    "blockHeight" .=
        encodeBlockNo (TPraos.bheaderBlockNo hBody) <>
    "slot" .=
        encodeSlotNo (TPraos.bheaderSlotNo hBody) <>
    "prevHash" .=
        encodePrevHash (TPraos.bheaderPrev hBody) <>
    "issuerVk" .=
        encodeVKey (TPraos.bheaderVk hBody) <>
    "issuerVrf" .=
        encodeVerKeyVRF (TPraos.bheaderVrfVk hBody) <>
    "blockSize" .=
        encodeNatural (TPraos.bsize hBody) <>
    "blockHash" .=
        encodeHash (TPraos.bhash hBody) <>
    "signature" .=
        encodeSignedKES hSig <>
    "nonce" .=
        encodeCertifiedVRF (TPraos.bheaderEta hBody) <>
    "leaderValue" .=
        encodeCertifiedVRF (TPraos.bheaderL hBody) <>
    "opCert" .=
        encodeOCert (TPraos.bheaderOCert hBody) <>
    "protocolVersion" .=
        encodeProtVer (TPraos.bprotver hBody)
    & encodeObject

encodeBlock
    :: Crypto crypto
    => ShelleyBlock (TPraos crypto) (ShelleyEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    "body" .=
        encodeFoldable encodeTx (Sh.txSeqTxns' txs) <>
    "header" .=
        encodeBHeader blkHeader <>
    "headerHash" .=
        encodeShelleyHash headerHash
    & encodeObject

encodeCertVRF
    :: CC.VRFAlgorithm alg
    => CC.CertVRF alg
    -> Json
encodeCertVRF =
    encodeByteStringBase16 . CC.rawSerialiseCertVRF

encodeCertifiedVRF
    :: CC.VRFAlgorithm alg
    => CC.CertifiedVRF alg any
    -> Json
encodeCertifiedVRF x =
    "output" .=
        encodeOutputVRF (CC.certifiedOutput x) <>
    "proof" .=
        encodeCertVRF (CC.certifiedProof x)
    & encodeObject

encodeCredential
    :: forall any crypto. (any :\: 'StakePool, Crypto crypto)
    => Ledger.Credential any crypto
    -> Json
encodeCredential x = case x of
    Ledger.KeyHashObj h -> encodeKeyHash h
    Ledger.ScriptHashObj h -> encodeScriptHash h

encodeDCert
    :: Crypto crypto
    => Sh.DCert crypto
    -> Json
encodeDCert = encodeObject . \case
    Sh.DCertDeleg (Sh.RegKey credential) ->
        "stakeKeyRegistration" .=
            encodeCredential credential
    Sh.DCertDeleg (Sh.DeRegKey credential) ->
        "stakeKeyDeregistration" .=
            encodeCredential credential
    Sh.DCertDeleg (Sh.Delegate delegation) ->
        "stakeDelegation" .=
            encodeDelegation delegation
    Sh.DCertPool (Sh.RegPool params)  ->
        "poolRegistration" .=
            encodePoolParams params
    Sh.DCertPool (Sh.RetirePool keyHash epochNo) ->
        "poolRetirement" .= encodeObject
            ( "poolId" .=
                  encodePoolId keyHash <>
              "retirementEpoch" .=
                  encodeEpochNo epochNo
            )
    Sh.DCertGenesis (Sh.GenesisDelegCert key delegate vrf)  ->
        "genesisDelegation" .= encodeObject
            ( "verificationKeyHash" .=
                encodeKeyHash key <>
              "delegateKeyHash" .=
                  encodeKeyHash delegate <>
              "vrfVerificationKeyHash" .=
                  encodeHash vrf
            )
    Sh.DCertMir (Sh.MIRCert pot target) ->
        "moveInstantaneousRewards" .= encodeObject
            ( "pot" .=
                encodeMIRPot pot <>
              case target of
                Sh.StakeAddressesMIR rewards ->
                    "rewards" .=
                        encodeMap stringifyCredential encodeDeltaCoin rewards
                Sh.SendToOppositePotMIR value ->
                    "value" .=
                        encodeCoin value
            )

encodeDelegation
    :: Crypto crypto
    => Sh.Delegation crypto
    -> Json
encodeDelegation x =
    "delegator" .=
        encodeCredential (Sh._delegator x) <>
    "delegatee" .=
        encodePoolId (Sh._delegatee x)
    & encodeObject

encodeDelegsFailure
    :: PredicateFailure (Ledger.EraRule "DELPL" era) ~ Sh.DelplPredicateFailure era
    => PredicateFailure (Ledger.EraRule "POOL" era)  ~ Sh.PoolPredicateFailure era
    => PredicateFailure (Ledger.EraRule "DELEG" era) ~ Sh.DelegPredicateFailure era
    => Crypto (Ledger.Crypto era)
    => Sh.DelegsPredicateFailure era
    -> Json
encodeDelegsFailure = \case
    Sh.DelegateeNotRegisteredDELEG h ->
        "delegateNotRegistered" .=
            encodePoolId h
        & encodeObject
    Sh.WithdrawalsNotInRewardsDELEGS withdrawals ->
        "unknownOrIncompleteWithdrawals" .=
            encodeWdrl (Sh.Wdrl withdrawals)
        & encodeObject
    Sh.DelplFailure e ->
        encodeDeplFailure e

encodeDelegFailure
    :: Crypto (Ledger.Crypto era)
    => Sh.DelegPredicateFailure era
    -> Json
encodeDelegFailure = encodeObject . \case
    Sh.StakeKeyAlreadyRegisteredDELEG credential ->
        "stakeKeyAlreadyRegistered" .=
            encodeCredential credential
    Sh.StakeKeyInRewardsDELEG credential ->
        "stakeKeyAlreadyRegistered" .=
            encodeCredential credential
    Sh.StakeKeyNotRegisteredDELEG credential ->
        "stakeKeyNotRegistered" .=
            encodeCredential credential
    Sh.StakeDelegationImpossibleDELEG credential ->
        "stakeKeyNotRegistered" .=
            encodeCredential  credential
    Sh.StakeKeyNonZeroAccountBalanceDELEG Nothing ->
        "rewardAccountNotExisting" .=
            encodeNull
    Sh.StakeKeyNonZeroAccountBalanceDELEG (Just balance) ->
        "rewardAccountNotEmpty" .= encodeObject
            ( "balance" .= encodeCoin balance
            )
    Sh.WrongCertificateTypeDELEG ->
        "wrongCertificateType" .=
            encodeNull
    Sh.GenesisKeyNotInMappingDELEG keyHash ->
        "unknownGenesisKey" .=
            encodeKeyHash keyHash
    Sh.DuplicateGenesisDelegateDELEG keyHash ->
        "alreadyDelegating" .=
            encodeKeyHash keyHash
    Sh.InsufficientForInstantaneousRewardsDELEG pot requested size ->
        "insufficientFundsForMir" .= encodeObject
            ( "rewardSource" .= encodeMIRPot pot <>
              "sourceSize" .= encodeCoin size <>
              "requestedAmount" .= encodeCoin requested
            )
    Sh.MIRCertificateTooLateinEpochDELEG currentSlot lastSlot ->
        "tooLateForMir" .= encodeObject
            ( "currentSlot" .= encodeSlotNo currentSlot <>
              "lastAllowedSlot" .= encodeSlotNo lastSlot
            )
    Sh.MIRTransferNotCurrentlyAllowed ->
        "mirTransferNotCurrentlyAllowed" .=
            encodeNull
    Sh.MIRNegativesNotCurrentlyAllowed ->
        "mirNegativeTransferNotCurrentlyAllowed" .=
            encodeNull
    Sh.InsufficientForTransferDELEG pot requested size ->
        "insufficientFundsForMir" .= encodeObject
            ( "rewardSource" .= encodeMIRPot pot <>
              "sourceSize" .= encodeCoin size <>
              "requestedAmount" .= encodeCoin requested
            )
    Sh.MIRProducesNegativeUpdate ->
        "mirProducesNegativeUpdate" .=
            encodeNull
    Sh.MIRNegativeTransfer pot coin ->
        "mirNegativeTransfer" .= encodeObject
            ( "rewardSource" .= encodeMIRPot pot <>
              "attemptedTransfer" .= encodeCoin coin
            )
    Sh.DuplicateGenesisVRFDELEG vrfHash ->
        "duplicateGenesisVrf" .=
            encodeHash vrfHash

encodeDeltaCoin
    :: Ledger.DeltaCoin
    -> Json
encodeDeltaCoin (Ledger.DeltaCoin delta) =
    encodeInteger delta

encodeDeplFailure
    :: PredicateFailure (Ledger.EraRule "POOL" era)  ~ Sh.PoolPredicateFailure era
    => PredicateFailure (Ledger.EraRule "DELEG" era) ~ Sh.DelegPredicateFailure era
    => Crypto (Ledger.Crypto era)
    => Sh.DelplPredicateFailure era
    -> Json
encodeDeplFailure = \case
    Sh.PoolFailure e ->
        encodePoolFailure e
    Sh.DelegFailure e ->
        encodeDelegFailure e

encodeEntities
    :: Foldable f
    => Text
    -> (entity -> Json)
    -> f entity
    -> Json
encodeEntities tag encodeEntity =
    encodeFoldable $ \e -> encodeObject $
        "type" .= encodeText tag <>
        "entity" .= encodeEntity e

encodeGenesis
    :: Era era
    => Sh.ShelleyGenesis era
    -> Json
encodeGenesis x =
    "systemStart" .=
        encodeUtcTime (Sh.sgSystemStart x) <>
    "networkMagic" .=
        encodeWord32 (Sh.sgNetworkMagic x) <>
    "network" .=
        encodeNetwork (Sh.sgNetworkId x) <>
    "activeSlotsCoefficient" .=
        encodePositiveUnitInterval (Sh.sgActiveSlotsCoeff x) <>
    "securityParameter" .=
        encodeWord64 (Sh.sgSecurityParam x) <>
    "epochLength" .=
        encodeEpochSize (Sh.sgEpochLength x) <>
    "slotsPerKesPeriod" .=
        encodeWord64 (Sh.sgSlotsPerKESPeriod x) <>
    "maxKesEvolutions" .=
        encodeWord64 (Sh.sgMaxKESEvolutions x) <>
    "slotLength" .=
        encodeNominalDiffTime (Sh.sgSlotLength x) <>
    "updateQuorum" .=
        encodeWord64 (Sh.sgUpdateQuorum x) <>
    "maxLovelaceSupply" .=
        encodeWord64 (Sh.sgMaxLovelaceSupply x) <>
    "protocolParameters" .=
        encodePParams' (\k encode v -> k .= encode v) (Sh.sgProtocolParams x) <>
    "initialDelegates" .=
        encodeMap stringifyKeyHash encodeGenDelegPair (Sh.sgGenDelegs x) <>
    "initialFunds" .=
        encodeMap stringifyAddress encodeCoin (Sh.sgInitialFunds x) <>
    "initialPools" .=
        encodeShelleyGenesisStaking (Sh.sgStaking x)
    & encodeObject

encodeGenDelegPair :: Crypto crypto => GenDelegPair crypto -> Json
encodeGenDelegPair x =
    "delegate" .=
        encodeKeyHash (genDelegKeyHash x) <>
    "vrf" .=
        encodeHash (genDelegVrfHash x)
    & encodeObject

encodeHash
    :: CC.HashAlgorithm alg
    => CC.Hash alg a
    -> Json
encodeHash (CC.UnsafeHash h) =
    encodeByteStringBase16 (fromShort h)

encodeHashHeader
    :: TPraos.HashHeader crypto
    -> Json
encodeHashHeader =
    encodeByteStringBase16 . CC.hashToBytes . TPraos.unHashHeader

encodeKeyHash
    :: forall any crypto. (any :\: StakePool, Crypto crypto)
    => Ledger.KeyHash any crypto
    -> Json
encodeKeyHash (Ledger.KeyHash h) =
    encodeHash h
  where
    _ = keepRedundantConstraint (Proxy @(any :\: StakePool))

encodeKESPeriod
    :: TPraos.KESPeriod
    -> Json
encodeKESPeriod =
    encodeWord . TPraos.unKESPeriod

encodeLedgerFailure
    :: Crypto crypto
    => Sh.LedgerPredicateFailure (ShelleyEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e ->
        encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeMetadata
    :: Sh.Metadata era
    -> Json
encodeMetadata (Sh.Metadata blob) =
    "blob" .=
        encodeMetadataBlob blob
    & encodeObject

encodeMetadataBlob
    :: Map Word64 Sh.Metadatum
    -> Json
encodeMetadataBlob =
    encodeMap show encodeMetadatum
  where
    encodeMetadatum :: Sh.Metadatum -> Json
    encodeMetadatum = encodeObject . \case
        Sh.I n ->
            "int" .= encodeInteger n
        Sh.B bytes ->
            "bytes" .= encodeByteStringBase16 bytes
        Sh.S txt ->
            "string" .= encodeText txt
        Sh.List xs ->
            "list" .= encodeList encodeMetadatum xs
        Sh.Map xs ->
            "map" .= encodeList encodeKeyPair xs

    encodeKeyPair :: (Sh.Metadatum, Sh.Metadatum) -> Json
    encodeKeyPair (k, v) =
        encodeObject ("k" .= encodeMetadatum k <>  "v" .= encodeMetadatum v)

encodeMIRPot
    :: Sh.MIRPot
    -> Json
encodeMIRPot = \case
    Sh.ReservesMIR ->
        encodeText "reserves"
    Sh.TreasuryMIR ->
        encodeText "treasury"

encodeMultiSig
    :: Crypto crypto
    => Sh.MultiSig crypto
    -> Json
encodeMultiSig = \case
    Sh.RequireSignature sig ->
        encodeKeyHash sig
    Sh.RequireAllOf xs ->
        encodeObject ("all" .= encodeList encodeMultiSig xs)
    Sh.RequireAnyOf xs ->
        encodeObject ("any" .= encodeList encodeMultiSig xs)
    Sh.RequireMOf n xs ->
        encodeObject (show n .= encodeList encodeMultiSig xs)

encodeNetwork
    :: Ledger.Network
    -> Json
encodeNetwork = encodeText . \case
    Ledger.Mainnet -> "mainnet"
    Ledger.Testnet -> "testnet"

encodeNonce
    :: Ledger.Nonce
    -> Json
encodeNonce = \case
    Ledger.NeutralNonce -> encodeText "neutral"
    Ledger.Nonce h -> encodeHash h

encodeOCert
    :: Crypto crypto
    => TPraos.OCert crypto
    -> Json
encodeOCert x =
    "hotVk" .=
        encodeVerKeyKES (TPraos.ocertVkHot x) <>
    "count" .=
        encodeWord64 (TPraos.ocertN x) <>
    "kesPeriod" .=
        encodeKESPeriod (TPraos.ocertKESPeriod x) <>
    "sigma" .=
        encodeSignedDSIGN (TPraos.ocertSigma x)
    & encodeObject

encodeOutputVRF
    :: CC.OutputVRF alg
    -> Json
encodeOutputVRF =
    encodeByteStringBase16 . CC.getOutputVRFBytes

encodePoolId
    :: Crypto crypto
    => Ledger.KeyHash StakePool crypto
    -> Json
encodePoolId =
    encodeText . stringifyPoolId

encodePoolFailure
    :: Crypto (Ledger.Crypto era)
    => Sh.PoolPredicateFailure era
    -> Json
encodePoolFailure = encodeObject . \case
    Sh.StakePoolNotRegisteredOnKeyPOOL keyHash ->
        "stakePoolNotRegistered" .=
            encodePoolId keyHash
    Sh.StakePoolRetirementWrongEpochPOOL current retiring limit ->
        "wrongRetirementEpoch" .= encodeObject
            ( "currentEpoch" .=
                encodeWord64 current <>
              "requestedEpoch" .=
                encodeWord64 retiring <>
              "firstUnreachableEpoch" .=
                encodeWord64 limit
            )
    Sh.WrongCertificateTypePOOL cert ->
        "wrongPoolCertificate" .=
            encodeWord8 cert
    Sh.StakePoolCostTooLowPOOL _cost minimumCost ->
        "poolCostTooSmall" .= encodeObject
            ( "minimumCost" .=
                encodeCoin minimumCost
            )
    Sh.WrongNetworkPOOL _specified expected poolId ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                encodeNetwork expected <>
              "invalidEntities" .=
                encodeEntities "poolRegistration" encodePoolId [poolId]
            )
    Sh.PoolMedataHashTooBig poolId measuredSize ->
        "poolMetadataHashTooBig" .= encodeObject
            ( "poolId" .=
                encodePoolId poolId <>
              "measuredSize" .=
                encodeInteger (fromIntegral measuredSize)
            )

encodePoolMetadata
    :: Sh.PoolMetadata
    -> Json
encodePoolMetadata x =
    "url" .=
        encodeUrl (Sh._poolMDUrl x) <>
    "hash" .=
        encodeByteStringBase16 (Sh._poolMDHash x)
    & encodeObject

encodePoolParams
    :: Crypto crypto
    => Sh.PoolParams crypto
    -> Json
encodePoolParams x =
    "id" .=
        encodePoolId (Sh._poolId x) <>
    "vrf" .=
        encodeHash (Sh._poolVrf x) <>
    "pledge" .=
        encodeCoin (Sh._poolPledge x) <>
    "cost" .=
        encodeCoin (Sh._poolCost x) <>
    "margin" .=
        encodeUnitInterval (Sh._poolMargin x) <>
    "rewardAccount" .=
        encodeRewardAcnt (Sh._poolRAcnt x) <>
    "owners" .=
        encodeFoldable encodeKeyHash (Sh._poolOwners x) <>
    "relays" .=
        encodeFoldable encodeStakePoolRelay (Sh._poolRelays x) <>
    "metadata" .=? OmitWhenNothing
        encodePoolMetadata (Sh._poolMD x)
    & encodeObject

encodePParams'
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> Sh.PParams' f era
    -> Json
encodePParams' encode x =
    encode "minFeeCoefficient"
        encodeNatural (Sh._minfeeA x) <>
    encode "minFeeConstant"
        encodeNatural (Sh._minfeeB x) <>
    encode "maxBlockBodySize"
        encodeNatural (Sh._maxBBSize x) <>
    encode "maxBlockHeaderSize"
        encodeNatural (Sh._maxBHSize x) <>
    encode "maxTxSize"
        encodeNatural (Sh._maxTxSize x) <>
    encode "stakeKeyDeposit"
        encodeCoin (Sh._keyDeposit x) <>
    encode "poolDeposit"
        encodeCoin (Sh._poolDeposit x) <>
    encode "poolRetirementEpochBound"
        encodeEpochNo (Sh._eMax x) <>
    encode "desiredNumberOfPools"
        encodeNatural (Sh._nOpt x) <>
    encode "poolInfluence"
        encodeNonNegativeInterval (Sh._a0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Sh._rho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Sh._tau x) <>
    encode "decentralizationParameter"
        encodeUnitInterval (Sh._d x) <>
    encode "extraEntropy"
        encodeNonce (Sh._extraEntropy x) <>
    encode "protocolVersion"
        encodeProtVer (Sh._protocolVersion x) <>
    encode "minUtxoValue"
        encodeCoin (Sh._minUTxOValue x) <>
    encode "minPoolCost"
        encodeCoin (Sh._minPoolCost x)
    & encodeObject

encodePrevHash
    :: TPraos.PrevHash crypto
    -> Json
encodePrevHash = \case
    TPraos.GenesisHash -> encodeText "genesis"
    TPraos.BlockHash h -> encodeHashHeader h

encodeProposedPPUpdates
    :: Ledger.PParamsDelta era ~ Sh.PParams' StrictMaybe era
    => Crypto (Ledger.Crypto era)
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap
        stringifyKeyHash
        (encodePParams' (\k encode v -> k .=? OmitWhenNothing encode v))
        m

encodeProtVer
    :: Ledger.ProtVer
    -> Json
encodeProtVer x =
    "major" .=
        encodeNatural (Ledger.pvMajor x) <>
    "minor" .=
        encodeNatural (Ledger.pvMinor x)
    & encodeObject

encodeRewardAcnt
    :: Sh.RewardAcnt era
    -> Json
encodeRewardAcnt =
    encodeText . stringifyRewardAcnt

encodeScript
    :: Crypto crypto
    => Sh.MultiSig crypto
    -> Json
encodeScript script =
    "native" .=
        encodeMultiSig script
    & encodeObject

encodeScriptHash
    :: Crypto crypto
    => Sh.ScriptHash crypto
    -> Json
encodeScriptHash (Sh.ScriptHash h) =
    encodeHash h

encodeSignedKES
    :: CC.KESAlgorithm alg
    => CC.SignedKES alg a
    -> Json
encodeSignedKES (CC.SignedKES raw) =
    encodeByteStringBase16 . CC.rawSerialiseSigKES $ raw

encodeShelleyGenesisStaking :: Crypto crypto => Sh.ShelleyGenesisStaking crypto -> Json
encodeShelleyGenesisStaking x =
    "pools" .=
        encodeMap stringifyPoolId encodePoolParams (Sh.sgsPools x) <>
    "delegators" .=
        encodeMap stringifyKeyHash encodePoolId (Sh.sgsStake x)
    & encodeObject

encodeShelleyHash
    :: Crypto crypto
    => ShelleyHash crypto
    -> Json
encodeShelleyHash =
    encodeHash . unShelleyHash

encodeSignedDSIGN
    :: CC.DSIGNAlgorithm alg
    => CC.SignedDSIGN alg a
    -> Json
encodeSignedDSIGN (CC.SignedDSIGN raw) =
    encodeByteStringBase16 . CC.rawSerialiseSigDSIGN $ raw

encodeStakePoolRelay
    :: Sh.StakePoolRelay
    -> Json
encodeStakePoolRelay = encodeObject . \case
    Sh.SingleHostAddr port ipv4 ipv6 ->
        "ipv4" .=? OmitWhenNothing
            encodeIPv4 ipv4 <>
        "ipv6" .=? OmitWhenNothing
            encodeIPv6 ipv6 <>
        "port" .=? OmitWhenNothing
            encodePort port
    Sh.SingleHostName port dns ->
        "hostname" .=
            encodeDnsName dns <>
        "port" .=? OmitWhenNothing
            encodePort port
    Sh.MultiHostName dns ->
        "hostname" .=
            encodeDnsName dns

encodeTx
    :: forall crypto. (Crypto crypto)
    => Sh.Tx (ShelleyEra crypto)
    -> Json
encodeTx x =
    "id" .= encodeTxId (Ledger.txid @(ShelleyEra crypto) (Sh.body x))
        <>
    "inputSource" .= encodeText "inputs"
        <>
    encodeTxBody (Sh.body x)
        <>
    "metadata" .=? OmitWhenNothing identity metadata
        <>
    encodeWitnessSet (Sh.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    metadata = liftA2
        (\hash body -> encodeObject ("hash" .= hash <> "body" .= body))
        (encodeAuxiliaryDataHash <$> Sh._mdHash (Sh.body x))
        (encodeMetadata <$> Sh.auxiliaryData x)

encodeTxBody
    :: Crypto crypto
    => Sh.TxBody (ShelleyEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable encodeTxIn (Sh._inputs x) <>
    "outputs" .=
        encodeFoldable encodeTxOut (Sh._outputs x) <>
    "fee" .=
        encodeCoin (Sh._txfee x) <>
    "validityInterval" .=
        encodeObject ("invalidAfter" .= encodeSlotNo (Sh._ttl x)) <>
    "certificates" .=? OmitWhen null
        (encodeFoldable encodeDCert) (Sh._certs x) <>
    "withdrawals" .=? OmitWhen (null . Sh.unWdrl)
        encodeWdrl (Sh._wdrls x) <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . encodeUpdate)
        (Sh._txUpdate x)

encodeTxId
    :: Crypto crypto
    => Ledger.TxId crypto
    -> Json
encodeTxId =
    encodeHash . Ledger.extractHash . Ledger._unTxId

encodeTxIn
    :: Crypto crypto
    => Ledger.TxIn crypto
    -> Json
encodeTxIn (Ledger.TxIn txid (Ledger.TxIx ix)) =
    "txId" .=
        encodeTxId txid <>
    "index" .=
        encodeWord64 ix
    & encodeObject

encodeTxOut
    :: (ShelleyBased era, Ledger.Value era ~ Coin)
    => Sh.TxOut era
    -> Json
encodeTxOut (Sh.TxOut addr value) =
    "address" .=
        encodeAddress addr <>
    "value" .=
        encodeValue value
    & encodeObject

encodeUpdate
    :: Ledger.PParamsDelta era ~ Sh.PParams' StrictMaybe era
    => Crypto (Ledger.Crypto era)
    => Sh.Update era
    -> Json
encodeUpdate (Sh.Update update epoch) =
    "proposal" .=
        encodeProposedPPUpdates update <>
    "epoch" .=
        encodeEpochNo epoch
    & encodeObject

encodeUpdateFailure
    :: Crypto (Ledger.Crypto era)
    => Sh.PpupPredicateFailure era
    -> Json
encodeUpdateFailure = encodeObject . \case
    Sh.NonGenesisUpdatePPUP voting shouldBeVoting ->
        "nonGenesisVoters" .= encodeObject
            ( "currentlyVoting" .=
                encodeFoldable encodeKeyHash voting <>
              "shouldBeVoting" .=
                encodeFoldable encodeKeyHash shouldBeVoting
            )
    Sh.PPUpdateWrongEpoch currentEpoch updateEpoch votingPeriod ->
        "updateWrongEpoch" .= encodeObject
            ( "currentEpoch" .=
                encodeEpochNo currentEpoch <>
              "requestedEpoch" .=
                encodeEpochNo updateEpoch <>
              "votingPeriod" .=
                encodeVotingPeriod votingPeriod
            )
    Sh.PVCannotFollowPPUP version ->
        "protocolVersionCannotFollow" .=
            encodeProtVer version

encodeUtxo
    :: forall era.
        ( ShelleyBased era
        , Ledger.Value era ~ Coin
        , Ledger.TxOut era ~ Sh.TxOut era
        )
    => Sh.UTxO era
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple encodeTxIn encodeTxOut)

encodeUtxoFailure
    :: Crypto crypto
    => Sh.UtxoPredicateFailure (ShelleyEra crypto)
    -> Json
encodeUtxoFailure = \case
    Sh.BadInputsUTxO inputs ->
        "badInputs" .=
            encodeFoldable encodeTxIn inputs
        & encodeObject
    Sh.ExpiredUTxO ttl currentSlot ->
        "expiredUtxo" .= encodeObject
            ( "transactionTimeToLive" .= encodeSlotNo ttl <>
              "currentSlot" .= encodeSlotNo currentSlot
            )
        & encodeObject
    Sh.MaxTxSizeUTxO actualSize maxSize ->
        "txTooLarge" .= encodeObject
            ( "maximumSize" .= encodeInteger maxSize <>
              "actualSize" .= encodeInteger actualSize
            )
        & encodeObject
    Sh.InputSetEmptyUTxO ->
        "missingAtLeastOneInputUtxo" .=
            encodeNull
        & encodeObject
    Sh.FeeTooSmallUTxO required actual ->
        "feeTooSmall" .= encodeObject
            ( "requiredFee" .=
                encodeCoin required <>
              "actualFee" .= encodeCoin actual
            )
        & encodeObject
    Sh.ValueNotConservedUTxO consumed produced ->
        "valueNotConserved" .= encodeObject
            ( "consumed" .= encodeCoin consumed <>
              "produced" .= encodeCoin produced
            )
        & encodeObject
    Sh.WrongNetwork expected invalidAddrs ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                encodeNetwork expected <>
              "invalidEntities" .=
                    encodeEntities "address" encodeAddress invalidAddrs
            )
        & encodeObject
    Sh.WrongNetworkWithdrawal expected invalidAccts ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                encodeNetwork expected <>
              "invalidEntities" .=
                encodeEntities "rewardAccount" encodeRewardAcnt invalidAccts
            )
        & encodeObject
    Sh.OutputTooSmallUTxO outs ->
        "outputTooSmall" .=
            encodeList encodeTxOut outs
        & encodeObject
    Sh.OutputBootAddrAttrsTooBig outs ->
        "addressAttributesTooLarge" .=
            encodeFoldable encodeAddress ((\(Sh.TxOut addr _) -> addr) <$> outs)
        & encodeObject
    Sh.UpdateFailure e ->
        encodeUpdateFailure e

encodeUtxowFailure
    :: forall era.
        ( Era era
        )
    => (PredicateFailure (Ledger.EraRule "UTXO" era) -> Json)
    -> Sh.UtxowPredicateFailure era
    -> Json
encodeUtxowFailure encodeUtxoFailure_ = \case
    Sh.InvalidWitnessesUTXOW wits ->
        "invalidWitnesses" .=
            encodeList encodeVKey wits
        & encodeObject
    Sh.MissingVKeyWitnessesUTXOW keys ->
        "missingVkWitnesses" .=
            encodeWitHashes keys
        & encodeObject
    Sh.MissingScriptWitnessesUTXOW scripts ->
        "missingScriptWitnesses" .=
            encodeFoldable encodeScriptHash scripts
        & encodeObject
    Sh.ScriptWitnessNotValidatingUTXOW scripts ->
        "scriptWitnessNotValidating" .=
            encodeFoldable encodeScriptHash scripts
        & encodeObject
    Sh.MIRInsufficientGenesisSigsUTXOW keys ->
        "insufficientGenesisSignatures" .=
            encodeFoldable encodeKeyHash keys
        & encodeObject
    Sh.MissingTxBodyMetadataHash hash ->
        "missingTxMetadataHash" .=
            encodeAuxiliaryDataHash hash
        & encodeObject
    Sh.MissingTxMetadata hash ->
        "missingTxMetadata" .=
            encodeAuxiliaryDataHash hash
        & encodeObject
    Sh.ConflictingMetadataHash included expected ->
        "txMetadataHashMismatch" .= encodeObject
            ( "includedHash" .=
                encodeAuxiliaryDataHash included <>
              "expectedHash" .=
                encodeAuxiliaryDataHash expected
            )
        & encodeObject
    Sh.InvalidMetadata ->
        "invalidMetadata" .=
            encodeNull
        & encodeObject
    Sh.ExtraneousScriptWitnessesUTXOW scripts ->
        "extraScriptWitnesses" .=
            encodeFoldable encodeScriptHash scripts
        & encodeObject
    Sh.UtxoFailure e ->
        encodeUtxoFailure_ e

encodeValue
    :: Coin
    -> Json
encodeValue coin =
    encodeObject ("coins" .= encodeCoin coin)

encodeVerKeyDSign
    :: CC.DSIGNAlgorithm alg
    => CC.VerKeyDSIGN alg
    -> Json
encodeVerKeyDSign =
    encodeByteStringBase16 . CC.rawSerialiseVerKeyDSIGN

encodeVerKeyKES
    :: CC.KESAlgorithm alg
    => CC.VerKeyKES alg
    -> Json
encodeVerKeyKES =
    encodeByteStringBase16 . CC.rawSerialiseVerKeyKES

encodeVerKeyVRF
    :: (CC.VRFAlgorithm alg)
    => CC.VerKeyVRF alg
    -> Json
encodeVerKeyVRF =
    encodeByteStringBase16 . CC.rawSerialiseVerKeyVRF

encodeVKey
    :: Crypto crypto
    => Ledger.VKey any crypto
    -> Json
encodeVKey =
    encodeVerKeyDSign . Ledger.unVKey

encodeVotingPeriod
    :: Sh.VotingPeriod
    -> Json
encodeVotingPeriod = \case
    Sh.VoteForThisEpoch ->
        encodeText "voteForThisEpoch"
    Sh.VoteForNextEpoch ->
        encodeText "voteForNextEpoch"

encodeWdrl
    :: Sh.Wdrl era
    -> Json
encodeWdrl =
    encodeMap stringifyRewardAcnt encodeCoin . Sh.unWdrl

encodeWitnessSet
    :: Crypto crypto
    => Sh.WitnessSet (ShelleyEra crypto)
    -> Series
encodeWitnessSet x =
    "signatories" .=
        encodeFoldable2
            encodeBootstrapWitness
            encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap stringifyScriptHash encodeScript) (Sh.scriptWits x)

encodeWitVKey
    :: Crypto crypto
    => Sh.WitVKey Witness crypto
    -> Json
encodeWitVKey (Sh.WitVKey key sig) =
    "key" .=
        (encodeVerKeyDSign . Ledger.unVKey) key <>
    "signature" .=
        encodeSignedDSIGN sig
    & encodeObject

encodeBootstrapWitness
    :: Crypto crypto
    => Sh.BootstrapWitness crypto
    -> Json
encodeBootstrapWitness (Sh.BootstrapWitness key sig cc attr) =
    "key" .=
        encodeVKey key <>
    "signature" .=
        encodeSignedDSIGN sig <>
    "chainCode" .=? OmitWhen BS.null
        encodeByteStringBase16 (Sh.unChainCode cc) <>
    "addressAttributes" .=? OmitWhen BS.null
        encodeByteStringBase16 attr
    & encodeObject

encodeWitHashes
    :: Crypto crypto
    => Sh.WitHashes crypto
    -> Json
encodeWitHashes =
    encodeFoldable encodeKeyHash . Sh.unWitHashes

--
-- Conversion To Text
--

stringifyAddress
    :: Ledger.Addr crypto
    -> Text
stringifyAddress = \case
    Ledger.AddrBootstrap addr ->
        Byron.stringifyAddress (Ledger.unBootstrapAddress addr)
    addr@(Ledger.Addr network _ _) ->
        encodeBech32 (hrp network) (Ledger.serialiseAddr addr)
  where
    hrp = \case
        Ledger.Mainnet -> hrpAddrMainnet
        Ledger.Testnet -> hrpAddrTestnet

stringifyCoin
    :: Coin
    -> Text
stringifyCoin =
    show . unCoin

stringifyCredential
    :: forall any crypto. (any :\: StakePool, Crypto crypto)
    => Ledger.Credential any crypto
    -> Text
stringifyCredential = \case
    Ledger.KeyHashObj h -> stringifyKeyHash h
    Ledger.ScriptHashObj h -> stringifyScriptHash h
  where
    _ = keepRedundantConstraint (Proxy @(any :\: StakePool))

stringifyKeyHash
    :: forall any crypto. (any :\: StakePool, Crypto crypto)
    => Ledger.KeyHash any crypto
    -> Text
stringifyKeyHash (Ledger.KeyHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)
  where
    _ = keepRedundantConstraint (Proxy @(any :\: StakePool))

stringifyPoolId
    :: Crypto crypto
    => Ledger.KeyHash StakePool crypto
    -> Text
stringifyPoolId (Ledger.KeyHash (CC.UnsafeHash h)) =
    encodeBech32 hrpPool (fromShort h)

stringifyRewardAcnt
    :: Sh.RewardAcnt era
    -> Text
stringifyRewardAcnt x@(Sh.RewardAcnt ntwrk _credential) =
    encodeBech32 (hrp ntwrk) (Ledger.serialiseRewardAcnt x)
  where
    hrp = \case
        Ledger.Mainnet -> hrpStakeMainnet
        Ledger.Testnet -> hrpStakeTestnet

stringifyScriptHash
    :: Crypto crypto
    => Sh.ScriptHash crypto
    -> Text
stringifyScriptHash (Sh.ScriptHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyTxId
    :: Crypto crypto
    => Ledger.TxId crypto
    -> Text
stringifyTxId (Ledger.TxId (Ledger.originalBytes -> bytes)) =
    encodeBase16 bytes

stringifyTxIn
    :: Crypto crypto
    => Ledger.TxIn crypto
    -> Text
stringifyTxIn (Ledger.TxIn txid (Ledger.TxIx ix)) =
    stringifyTxId txid <> "#" <> show ix

stringifyVKey
    :: Crypto crypto
    => Ledger.VKey any crypto
    -> Text
stringifyVKey =
    encodeBase16 . CC.rawSerialiseVerKeyDSIGN . Ledger.unVKey

--
-- CIP-0005 Human-Readable Prefixes
--

hrpAddrMainnet :: HumanReadablePart
hrpAddrMainnet = HumanReadablePart "addr"

hrpAddrTestnet :: HumanReadablePart
hrpAddrTestnet = HumanReadablePart "addr_test"

hrpStakeMainnet :: HumanReadablePart
hrpStakeMainnet = HumanReadablePart "stake"

hrpStakeTestnet :: HumanReadablePart
hrpStakeTestnet = HumanReadablePart "stake_test"

hrpPool :: HumanReadablePart
hrpPool = HumanReadablePart "pool"
