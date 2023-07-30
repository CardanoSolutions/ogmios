--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Data.Json.Shelley where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Ledger.Keys
    ( GenDelegPair (..)
    , KeyRole (..)
    )
import Data.ByteString.Base16
    ( encodeBase16
    )
import Data.ByteString.Bech32
    ( HumanReadablePart (..)
    , encodeBech32
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
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Keys.Bootstrap as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.Delegation.Certificates as Sh
import qualified Cardano.Ledger.Shelley.Genesis as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Cardano.Ledger.Shelley.Scripts as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxAuxData as Sh
import qualified Cardano.Ledger.Shelley.TxBody as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Ogmios.Data.Json.Byron as Byron


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
    -> Series
encodeBHeader (TPraos.BHeader hBody _hSig) =
    "ancestor" .=
        encodePrevHash (TPraos.bheaderPrev hBody) <>
    "nonce" .=
        encodeCertifiedVRF (TPraos.bheaderEta hBody) <>
    "height" .=
        encodeBlockNo (TPraos.bheaderBlockNo hBody)  <>
    "slot" .=
        encodeSlotNo (TPraos.bheaderSlotNo hBody) <>
    "issuer" .= encodeObject
        ( "verificationKey" .=
              encodeVKey (TPraos.bheaderVk hBody) <>
          "vrfVerificationKey" .=
              encodeVerKeyVRF (TPraos.bheaderVrfVk hBody) <>
          "leaderValue" .=
              encodeCertifiedVRF (TPraos.bheaderL hBody) <>
          "operationalCertificate" .=
              encodeOCert (TPraos.bheaderOCert hBody)
        ) <>
    "protocol" .= encodeObject
        ( "version" .=
            encodeProtVer (TPraos.bprotver hBody)
        )

encodeBlock
    :: Crypto crypto
    => ShelleyBlock (TPraos crypto) (ShelleyEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "shelley"
        <>
          "header" .= encodeObject
            ( "hash" .= encodeShelleyHash headerHash
            )
        <>
        encodeBHeader blkHeader
        <>
        "size" .= encodeNatural (TPraos.bsize hBody)
        <>
        "transactions" .= encodeFoldable encodeTx (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

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
    Sh.DCertGenesis (Sh.ConstitutionalDelegCert key delegate vrf)  ->
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
        encodeCredential (Sh.dDelegator x) <>
    "delegatee" .=
        encodePoolId (Sh.dDelegatee x)
    & encodeObject

encodeDeltaCoin
    :: Ledger.DeltaCoin
    -> Json
encodeDeltaCoin (Ledger.DeltaCoin delta) =
    encodeInteger delta

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
    :: Crypto crypto
    => Sh.ShelleyGenesis crypto
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
        encodeNominalDiffTimeMicro (Sh.sgSlotLength x) <>
    "updateQuorum" .=
        encodeWord64 (Sh.sgUpdateQuorum x) <>
    "maxLovelaceSupply" .=
        encodeWord64 (Sh.sgMaxLovelaceSupply x) <>
    "protocolParameters" .=
        encodePParams (Sh.sgProtocolParams x) <>
    "initialDelegates" .=
        encodeMap stringifyKeyHash encodeGenDelegPair (Sh.sgGenDelegs x) <>
    "initialFunds" .=
        encodeListMap stringifyAddress encodeCoin (Sh.sgInitialFunds x) <>
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

encodeMetadata
    :: Era era
    => Sh.ShelleyTxAuxData era
    -> Json
encodeMetadata (Sh.ShelleyTxAuxData blob) =
    encodeMetadataBlob blob

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
    :: Era era
    => Sh.MultiSig era
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
    "count" .=
        encodeWord64 (TPraos.ocertN x) <>
    "kes" .= encodeObject
        ( "period" .=
            encodeKESPeriod (TPraos.ocertKESPeriod x) <>
          "verificationKey" .=
              encodeVerKeyKES (TPraos.ocertVkHot x)
        )
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

encodePoolMetadata
    :: Sh.PoolMetadata
    -> Json
encodePoolMetadata x =
    "url" .=
        encodeUrl (Sh.pmUrl x) <>
    "hash" .=
        encodeByteStringBase16 (Sh.pmHash x)
    & encodeObject

encodePoolParams
    :: Crypto crypto
    => Sh.PoolParams crypto
    -> Json
encodePoolParams x =
    "id" .=
        encodePoolId (Sh.ppId x) <>
    "vrf" .=
        encodeHash (Sh.ppVrf x) <>
    "pledge" .=
        encodeCoin (Sh.ppPledge x) <>
    "cost" .=
        encodeCoin (Sh.ppCost x) <>
    "margin" .=
        encodeUnitInterval (Sh.ppMargin x) <>
    "rewardAccount" .=
        encodeRewardAcnt (Sh.ppRewardAcnt x) <>
    "owners" .=
        encodeFoldable encodeKeyHash (Sh.ppOwners x) <>
    "relays" .=
        encodeFoldable encodeStakePoolRelay (Sh.ppRelays x) <>
    "metadata" .=? OmitWhenNothing
        encodePoolMetadata (Sh.ppMetadata x)
    & encodeObject

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Sh.ShelleyPParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) x

encodePParamsUpdate
    :: (Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era)
    => Ledger.PParamsUpdate era
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k encode v -> k .=? OmitWhenNothing encode v) x

encodePParamsHKD
    :: forall f era. (Ledger.PParamsHKD f era ~ Sh.ShelleyPParams f era)
    => (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> Ledger.PParamsHKD f era
    -> Json
encodePParamsHKD encode x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin) (Sh.sppMinFeeA x) <>
    encode "minFeeConstant"
        encodeCoin (Sh.sppMinFeeB x) <>
    encode "maxBlockBodySize"
        encodeNatural (Sh.sppMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        encodeNatural (Sh.sppMaxBHSize x) <>
    encode "maxTxSize"
        encodeNatural (Sh.sppMaxTxSize x) <>
    encode "stakeKeyDeposit"
        encodeCoin (Sh.sppKeyDeposit x) <>
    encode "poolDeposit"
        encodeCoin (Sh.sppPoolDeposit x) <>
    encode "poolRetirementEpochBound"
        encodeEpochNo (Sh.sppEMax x) <>
    encode "desiredNumberOfPools"
        encodeNatural (Sh.sppNOpt x) <>
    encode "poolInfluence"
        encodeNonNegativeInterval (Sh.sppA0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Sh.sppRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Sh.sppTau x) <>
    encode "decentralizationParameter"
        encodeUnitInterval (Sh.sppD x) <>
    encode "extraEntropy"
        encodeNonce (Sh.sppExtraEntropy x) <>
    encode "protocolVersion"
        encodeProtVer (Sh.sppProtocolVersion x) <>
    encode "minUtxoValue"
        encodeCoin (Sh.sppMinUTxOValue x) <>
    encode "minPoolCost"
        encodeCoin (Sh.sppMinPoolCost x)
    & encodeObject

encodePrevHash
    :: TPraos.PrevHash crypto
    -> Json
encodePrevHash = \case
    TPraos.GenesisHash -> encodeText "genesis"
    TPraos.BlockHash h -> encodeHashHeader h

encodeProposedPPUpdates
    :: forall era.
        ( Era era
        , Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era
        )
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap stringifyKeyHash encodePParamsUpdate m

encodeProtVer
    :: Ledger.ProtVer
    -> Json
encodeProtVer x =
    "major" .=
        encodeVersion (Ledger.pvMajor x) <>
    "minor" .=
        encodeNatural (Ledger.pvMinor x)
    & encodeObject

encodeRewardAcnt
    :: Sh.RewardAcnt era
    -> Json
encodeRewardAcnt =
    encodeText . stringifyRewardAcnt

encodeScript
    :: Era era
    => Sh.MultiSig era
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
        encodeListMap stringifyPoolId encodePoolParams (Sh.sgsPools x) <>
    "delegators" .=
        encodeListMap stringifyKeyHash encodePoolId (Sh.sgsStake x)
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
    => Sh.ShelleyTx (ShelleyEra crypto)
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
        (\hash body -> encodeObject ("hash" .= hash <> "labels" .= body))
        (encodeAuxiliaryDataHash <$> Sh.stbMDHash (Sh.body x))
        (encodeMetadata <$> Sh.auxiliaryData x)

encodeTxBody
    :: Crypto crypto
    => Sh.ShelleyTxBody (ShelleyEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable encodeTxIn (Sh.stbInputs x) <>
    "outputs" .=
        encodeFoldable encodeTxOut (Sh.stbOutputs x) <>
    "fee" .=
        encodeCoin (Sh.stbTxFee x) <>
    "validityInterval" .=
        encodeObject ("invalidAfter" .= encodeSlotNo (Sh.stbTTL x)) <>
    "certificates" .=? OmitWhen null
        (encodeFoldable encodeDCert) (Sh.stbCerts x) <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        encodeWdrl (Sh.stbWithdrawals x) <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . encodeUpdate)
        (Sh.stbUpdate x)

encodeTxId
    :: Crypto crypto
    => Ledger.TxId crypto
    -> Json
encodeTxId =
    encodeHash . Ledger.extractHash . Ledger.unTxId

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
    :: (Era era, Ledger.Value era ~ Coin)
    => Sh.ShelleyTxOut era
    -> Json
encodeTxOut (Sh.ShelleyTxOut addr value) =
    "address" .=
        encodeAddress addr <>
    "value" .=
        encodeValue value
    & encodeObject

encodeUpdate
    :: ( Era era
       , Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era
       )
    => Sh.Update era
    -> Json
encodeUpdate (Sh.Update update epoch) =
    "proposal" .=
        encodeProposedPPUpdates update <>
    "epoch" .=
        encodeEpochNo epoch
    & encodeObject

encodeUtxo
    :: forall era.
        ( Era era
        , Ledger.Value era ~ Coin
        , Ledger.TxOut era ~ Sh.ShelleyTxOut era
        )
    => Sh.UTxO era
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple encodeTxIn encodeTxOut)

encodeValue
    :: Coin
    -> Json
encodeValue =
    encodeSingleton "ada" . encodeCoin

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
    :: Ledger.Withdrawals era
    -> Json
encodeWdrl =
    encodeMap stringifyRewardAcnt encodeCoin . Ledger.unWithdrawals

encodeWitnessSet
    :: Crypto crypto
    => Sh.ShelleyTxWits (ShelleyEra crypto)
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
    => Ledger.BootstrapWitness crypto
    -> Json
encodeBootstrapWitness (Ledger.BootstrapWitness key sig cc attr) =
    "key" .=
        encodeVKey key <>
    "signature" .=
        encodeSignedDSIGN sig <>
    "chainCode" .=? OmitWhen BS.null
        encodeByteStringBase16 (Ledger.unChainCode cc) <>
    "addressAttributes" .=? OmitWhen BS.null
        encodeByteStringBase16 attr
    & encodeObject


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
