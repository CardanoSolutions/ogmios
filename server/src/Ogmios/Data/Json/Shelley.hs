--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Data.Json.Shelley where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Era
    ( Era )
import Cardano.Ledger.Shelley.Constraints
    ( ShelleyBased )
import Control.State.Transition
    ( STS (..) )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.ByteString.Bech32
    ( HumanReadablePart (..), encodeBech32 )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )
import Ouroboros.Consensus.Cardano.Block
    ( ShelleyEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )

import qualified Ogmios.Data.Json.Byron as Byron

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC

import qualified Cardano.Ledger.AuxiliaryData as Aux
import qualified Cardano.Ledger.Coin as Coin
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.SafeHash as SafeHash

import qualified Shelley.Spec.Ledger.Address as Sh
import qualified Shelley.Spec.Ledger.Address.Bootstrap as Sh
import qualified Shelley.Spec.Ledger.BaseTypes as Sh
import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Credential as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.Genesis as Sh
import qualified Shelley.Spec.Ledger.Keys as Sh
import qualified Shelley.Spec.Ledger.LedgerState as Sh
import qualified Shelley.Spec.Ledger.Metadata as Sh
import qualified Shelley.Spec.Ledger.OCert as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.Scripts as Sh
import qualified Shelley.Spec.Ledger.STS.Deleg as Sh
import qualified Shelley.Spec.Ledger.STS.Delegs as Sh
import qualified Shelley.Spec.Ledger.STS.Delpl as Sh
import qualified Shelley.Spec.Ledger.STS.Ledger as Sh
import qualified Shelley.Spec.Ledger.STS.Pool as Sh
import qualified Shelley.Spec.Ledger.STS.Ppup as Sh
import qualified Shelley.Spec.Ledger.STS.Utxo as Sh
import qualified Shelley.Spec.Ledger.STS.Utxow as Sh
import qualified Shelley.Spec.Ledger.Tx as Sh
import qualified Shelley.Spec.Ledger.TxBody as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

--
-- Encoders
--

encodeAddress
    :: Sh.Addr era
    -> Json
encodeAddress = \case
    Sh.AddrBootstrap addr ->
        Byron.encodeAddress (Sh.unBootstrapAddress addr)
    addr@(Sh.Addr network _ _) ->
        encodeByteStringBech32 (hrp network) (Sh.serialiseAddr addr)
  where
    hrp = \case
        Sh.Mainnet -> hrpAddrMainnet
        Sh.Testnet -> hrpAddrTestnet

encodeAuxiliaryDataHash
    :: Aux.AuxiliaryDataHash era
    -> Json
encodeAuxiliaryDataHash =
    encodeHash . SafeHash.extractHash . Aux.unsafeAuxiliaryDataHash

encodeBHeader
    :: Crypto crypto
    => SerializationMode
    -> Sh.BHeader crypto
    -> Json
encodeBHeader mode (Sh.BHeader hBody hSig) = encodeObjectWithMode mode
    [ ( "blockHeight"
      , encodeBlockNo (Sh.bheaderBlockNo hBody)
      )
    , ( "slot"
      , encodeSlotNo (Sh.bheaderSlotNo hBody)
      )
    , ( "prevHash"
      , encodePrevHash (Sh.bheaderPrev hBody)
      )
    , ( "issuerVk"
      , encodeVKey (Sh.bheaderVk hBody)
      )
    , ( "issuerVrf"
      , encodeVerKeyVRF (Sh.bheaderVrfVk hBody)
      )
    , ( "blockSize"
      , encodeNatural (Sh.bsize hBody)
      )
    , ( "blockHash"
      , encodeHash (Sh.bhash hBody)
      )
    ]
    [ ( "signature"
      , encodeSignedKES hSig
      )
    , ( "nonce"
      , encodeCertifiedVRF (Sh.bheaderEta hBody)
      )
    , ( "leaderValue"
      , encodeCertifiedVRF (Sh.bheaderL hBody)
      )
    , ( "opCert"
      , encodeOCert (Sh.bheaderOCert hBody)
      )
    , ( "protocolVersion"
      , encodeProtVer (Sh.bprotver hBody)
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (ShelleyEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Sh.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Sh.txSeqTxns' txs)
      )
    , ( "header"
      , encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , encodeShelleyHash headerHash
      )
    ]

encodeBootstrapWitness
    :: Crypto crypto
    => Sh.BootstrapWitness crypto
    -> Json
encodeBootstrapWitness (Sh.BootstrapWitness key sig cc attr) = encodeObject
    [ ( "key"
      , encodeVKey key
      )
    , ( "chainCode"
      , encodeChainCode cc
      )
    , ( "addressAttributes"
      , if BS.null attr then encodeNull else encodeByteStringBase64 attr
      )
    , ( "signature"
      , encodeSignedDSIGN sig
      )
    ]

encodeCertVRF
    :: CC.VRFAlgorithm alg
    => CC.CertVRF alg
    -> Json
encodeCertVRF =
    encodeByteStringBase64 . CC.rawSerialiseCertVRF

encodeCertifiedVRF
    :: CC.VRFAlgorithm alg
    => CC.CertifiedVRF alg any
    -> Json
encodeCertifiedVRF x = encodeObject
    [ ( "output"
      , encodeOutputVRF (CC.certifiedOutput x)
      )
    , ( "proof"
      , encodeCertVRF (CC.certifiedProof x)
      )
    ]

encodeChainCode
    :: Sh.ChainCode
    -> Json
encodeChainCode cc
    | BS.null (Sh.unChainCode cc) = encodeNull
    | otherwise = encodeByteStringBase16 (Sh.unChainCode cc)

encodeCredential
    :: forall any era. (any :\: 'Sh.StakePool)
    => Sh.Credential any era
    -> Json
encodeCredential x = case x of
    Sh.KeyHashObj h -> encodeKeyHash h
    Sh.ScriptHashObj h -> encodeScriptHash h

encodeDCert
    :: Sh.DCert era
    -> Json
encodeDCert = \case
    Sh.DCertDeleg (Sh.RegKey credential) -> encodeObject
        [ ( "stakeKeyRegistration"
          , encodeCredential credential
          )
        ]
    Sh.DCertDeleg (Sh.DeRegKey credential) -> encodeObject
        [ ( "stakeKeyDeregistration"
          , encodeCredential credential
          )
        ]
    Sh.DCertDeleg (Sh.Delegate delegation) -> encodeObject
        [ ( "stakeDelegation"
          , encodeDelegation delegation
          )
        ]
    Sh.DCertPool (Sh.RegPool params)  -> encodeObject
        [ ( "poolRegistration"
          , encodePoolParams params
          )
        ]
    Sh.DCertPool (Sh.RetirePool keyHash epochNo) -> encodeObject
        [ ( "poolRetirement", encodeObject
            [ ( "poolId"
              , encodePoolId keyHash
              )
            , ( "retirementEpoch"
              , encodeEpochNo epochNo
              )
            ]
          )
        ]
    Sh.DCertGenesis (Sh.GenesisDelegCert key delegate vrf)  -> encodeObject
        [ ( "genesisDelegation", encodeObject
            [ ( "verificationKeyHash"
              , encodeKeyHash key
              )
            , ( "delegateKeyHash"
              , encodeKeyHash delegate
              )
            , ( "vrfVerificationKeyHash"
              , encodeHash vrf
              )
            ]
          )
        ]
    Sh.DCertMir (Sh.MIRCert pot target) ->
        encodeObject
            [ ( "moveInstantaneousRewards", encodeObject
                [ ( "pot"
                  , encodeMIRPot pot
                  )
                , case target of
                    Sh.StakeAddressesMIR rewards ->
                        ( "rewards"
                        , encodeMap stringifyCredential encodeDeltaCoin rewards
                        )
                    Sh.SendToOppositePotMIR value ->
                        ( "value"
                        , encodeCoin value
                        )
                ]
              )
            ]

encodeDelegation
    :: Sh.Delegation era
    -> Json
encodeDelegation x = encodeObject
    [ ( "delegator"
      , encodeCredential (Sh._delegator x)
      )
    , ( "delegatee"
      , encodePoolId (Sh._delegatee x)
      )
    ]

encodeDelegsFailure
    :: PredicateFailure (Core.EraRule "DELPL" era) ~ Sh.DelplPredicateFailure era
    => PredicateFailure (Core.EraRule "POOL" era)  ~ Sh.PoolPredicateFailure era
    => PredicateFailure (Core.EraRule "DELEG" era) ~ Sh.DelegPredicateFailure era
    => Sh.DelegsPredicateFailure era
    -> Json
encodeDelegsFailure = \case
    Sh.DelegateeNotRegisteredDELEG h ->
        encodeObject
            [ ( "delegateNotRegistered"
              , encodePoolId h
              )
            ]
    Sh.WithdrawalsNotInRewardsDELEGS withdrawals ->
        encodeObject
            [ ( "unknownOrIncompleteWithdrawals"
              , encodeWdrl (Sh.Wdrl withdrawals)
              )
            ]
    Sh.DelplFailure e ->
        encodeDeplFailure e

encodeDelegFailure
    :: Sh.DelegPredicateFailure era
    -> Json
encodeDelegFailure = \case
    Sh.StakeKeyAlreadyRegisteredDELEG credential ->
        encodeObject
            [ ( "stakeKeyAlreadyRegistered"
              , encodeCredential credential
              )
            ]
    Sh.StakeKeyInRewardsDELEG credential ->
        encodeObject
            [ ( "stakeKeyAlreadyRegistered"
              , encodeCredential credential
              )
            ]
    Sh.StakeKeyNotRegisteredDELEG credential ->
        encodeObject
            [ ( "stakeKeyNotRegistered"
              , encodeCredential credential
              )
            ]
    Sh.StakeDelegationImpossibleDELEG credential ->
        encodeObject
            [ ( "stakeKeyNotRegistered"
              , encodeCredential  credential
              )
            ]
    Sh.StakeKeyNonZeroAccountBalanceDELEG Nothing ->
        encodeText "rewardAccountNotExisting"
    Sh.StakeKeyNonZeroAccountBalanceDELEG (Just balance) ->
        encodeObject
            [ ( "rewardAccountNotEmpty", encodeObject
                [ ( "balance" , encodeCoin  balance )
                ]
              )
            ]
    Sh.WrongCertificateTypeDELEG ->
        encodeString "wrongCertificateType"
    Sh.GenesisKeyNotInMappingDELEG keyHash ->
        encodeObject
            [ ( "unknownGenesisKey"
              , encodeKeyHash keyHash
              )
            ]
    Sh.DuplicateGenesisDelegateDELEG keyHash ->
        encodeObject
            [ ( "alreadyDelegating"
              , encodeKeyHash keyHash
              )
            ]
    Sh.InsufficientForInstantaneousRewardsDELEG pot requested size ->
        encodeObject
            [ ( "insufficientFundsForMir", encodeObject
                [ ( "rewardSource", encodeMIRPot pot )
                , ( "sourceSize", encodeCoin size )
                , ( "requestedAmount", encodeCoin requested )
                ]
              )
            ]

    Sh.MIRCertificateTooLateinEpochDELEG currentSlot lastSlot ->
        encodeObject
            [ ( "tooLateForMir", encodeObject
                [ ( "currentSlot", encodeSlotNo currentSlot )
                , ( "lastAllowedSlot", encodeSlotNo lastSlot )
                ]
              )
            ]
    Sh.MIRTransferNotCurrentlyAllowed ->
        encodeString "mirTransferNotCurrentlyAllowed"

    Sh.MIRNegativesNotCurrentlyAllowed ->
        encodeString "mirNegativeTransferNotCurrentlyAllowed"

    Sh.InsufficientForTransferDELEG pot requested size ->
        encodeObject
            [ ( "insufficientFundsForMir", encodeObject
                [ ( "rewardSource", encodeMIRPot pot )
                , ( "sourceSize", encodeCoin size )
                , ( "requestedAmount", encodeCoin requested )
                ]
              )
            ]

    Sh.MIRProducesNegativeUpdate ->
        encodeString "mirProducesNegativeUpdate"

    Sh.DuplicateGenesisVRFDELEG vrfHash ->
        encodeObject
            [ ( "duplicateGenesisVrf"
              , encodeHash vrfHash
              )
            ]

encodeDeltaCoin
    :: Coin.DeltaCoin
    -> Json
encodeDeltaCoin (Coin.DeltaCoin delta) =
    encodeInteger delta

encodeDeplFailure
    :: PredicateFailure (Core.EraRule "POOL" era)  ~ Sh.PoolPredicateFailure era
    => PredicateFailure (Core.EraRule "DELEG" era) ~ Sh.DelegPredicateFailure era
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
encodeEntities tag encodeEntity = encodeFoldable $ \e -> encodeObject
    [ ( "type", encodeText tag )
    , ( "entity", encodeEntity e )
    ]

encodeGenesis
    :: Sh.ShelleyGenesis era
    -> Json
encodeGenesis x = encodeObject
    [ ( "systemStart"
      , encodeUtcTime (Sh.sgSystemStart x)
      )
    , ( "networkMagic"
      , encodeWord32 (Sh.sgNetworkMagic x)
      )
    , ( "network"
      , encodeNetwork (Sh.sgNetworkId x)
      )
    , ( "activeSlotsCoefficient"
      , encodeRational (Sh.sgActiveSlotsCoeff x)
      )
    , ( "securityParameter"
      , encodeWord64 (Sh.sgSecurityParam x)
      )
    , ( "epochLength"
      , encodeEpochSize (Sh.sgEpochLength x)
      )
    , ( "slotsPerKesPeriod"
      , encodeWord64 (Sh.sgSlotsPerKESPeriod x)
      )
    , ( "maxKesEvolutions"
      , encodeWord64 (Sh.sgMaxKESEvolutions x)
      )
    , ( "slotLength"
      , encodeNominalDiffTime (Sh.sgSlotLength x)
      )
    , ( "updateQuorum"
      , encodeWord64 (Sh.sgUpdateQuorum x)
      )
    , ( "maxLovelaceSupply"
      , encodeWord64 (Sh.sgMaxLovelaceSupply x)
      )
    , ( "protocolParameters"
      , encodePParams' id (Sh.sgProtocolParams x)
      )
    ]

encodeHash
    :: CC.Hash alg a
    -> Json
encodeHash (CC.UnsafeHash h) =
    encodeByteStringBase16 (fromShort h)

encodeHashHeader
    :: Sh.HashHeader crypto
    -> Json
encodeHashHeader =
    encodeByteStringBase16 . CC.hashToBytes . Sh.unHashHeader

encodeKeyHash
    :: forall any crypto. (any :\: Sh.StakePool)
    => Sh.KeyHash any crypto
    -> Json
encodeKeyHash (Sh.KeyHash h) =
    encodeHash h
  where
    _ = keepRedundantConstraint (Proxy @(any :\: Sh.StakePool))

encodeKESPeriod
    :: Sh.KESPeriod
    -> Json
encodeKESPeriod =
    encodeWord . Sh.unKESPeriod

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
encodeMetadata (Sh.Metadata blob) = encodeObject
    [ ( "blob"
      , encodeMetadataBlob blob
      )
    ]

encodeMetadataBlob
    :: Map Word64 Sh.Metadatum
    -> Json
encodeMetadataBlob =
    encodeMap show encodeMetadatum
  where
    encodeMetadatum :: Sh.Metadatum -> Json
    encodeMetadatum = \case
        Sh.I n ->
            encodeObject [("int", encodeInteger n)]
        Sh.B bytes ->
            encodeObject [("bytes", encodeByteStringBase16 bytes)]
        Sh.S txt ->
            encodeObject [("string", encodeText txt)]
        Sh.List xs ->
            encodeObject [("list", encodeList encodeMetadatum xs)]
        Sh.Map xs ->
            encodeObject [("map", encodeList encodeKeyPair xs)]

    encodeKeyPair :: (Sh.Metadatum, Sh.Metadatum) -> Json
    encodeKeyPair (k, v) = encodeObject
        [ ( "k", encodeMetadatum k )
        , ( "v", encodeMetadatum v )
        ]

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
        encodeObject [( "all", encodeList encodeMultiSig xs )]
    Sh.RequireAnyOf xs ->
        encodeObject [( "any", encodeList encodeMultiSig xs )]
    Sh.RequireMOf n xs ->
        encodeObject [( show n, encodeList encodeMultiSig xs)]

encodeNetwork
    :: Sh.Network
    -> Json
encodeNetwork = \case
    Sh.Mainnet -> encodeText "mainnet"
    Sh.Testnet -> encodeText "testnet"

encodeNonce
    :: Sh.Nonce
    -> Json
encodeNonce = \case
    Sh.NeutralNonce -> encodeText "neutral"
    Sh.Nonce h -> encodeHash h

encodeOCert
    :: Crypto crypto
    => Sh.OCert crypto
    -> Json
encodeOCert x = encodeObject
    [ ( "hotVk"
      , encodeVerKeyKES (Sh.ocertVkHot x)
      )
    , ( "count"
      , encodeWord64 (Sh.ocertN x)
      )
    , ( "kesPeriod"
      , encodeKESPeriod (Sh.ocertKESPeriod x)
      )
    , ( "sigma"
      , encodeSignedDSIGN (Sh.ocertSigma x)
      )
    ]

encodeOutputVRF
    :: CC.OutputVRF alg
    -> Json
encodeOutputVRF =
    encodeByteStringBase64 . CC.getOutputVRFBytes

encodePoolDistr
    :: Sh.PoolDistr crypto
    -> Json
encodePoolDistr =
    encodeMap stringifyPoolId encodeIndividualPoolStake . Sh.unPoolDistr

encodeIndividualPoolStake
    :: Sh.IndividualPoolStake crypto
    -> Json
encodeIndividualPoolStake x = encodeObject
    [ ( "stake"
      , encodeRational (Sh.individualPoolStake x)
      )
    , ( "vrf"
      , encodeHash (Sh.individualPoolStakeVrf x)
      )
    ]

encodePoolId
    :: Sh.KeyHash Sh.StakePool crypto
    -> Json
encodePoolId =
    encodeText . stringifyPoolId

encodePoolFailure
    :: Sh.PoolPredicateFailure era
    -> Json
encodePoolFailure = \case
    Sh.StakePoolNotRegisteredOnKeyPOOL keyHash ->
        encodeObject
            [ ( "stakePoolNotRegistered"
              , encodePoolId keyHash
              )
            ]
    Sh.StakePoolRetirementWrongEpochPOOL current retiring limit ->
        encodeObject
            [ ( "wrongRetirementEpoch", encodeObject
                [ ( "currentEpoch", encodeWord64 current )
                , ( "requestedEpoch", encodeWord64 retiring )
                , ( "firstUnreachableEpoch", encodeWord64 limit )
                ]
              )
            ]
    Sh.WrongCertificateTypePOOL cert ->
        encodeObject
            [ ( "wrongPoolCertificate"
              , encodeWord8 cert
              )
            ]
    Sh.StakePoolCostTooLowPOOL _cost minimumCost ->
        encodeObject
            [ ( "poolCostTooSmall", encodeObject
                [ ( "minimumCost", encodeCoin minimumCost )
                ]
              )
            ]
    Sh.WrongNetworkPOOL _specified expected poolId ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , encodeEntities "poolRegistration" encodePoolId [poolId]
                  )
                ]
              )
            ]

encodePoolMetadata
    :: Sh.PoolMetadata
    -> Json
encodePoolMetadata x = encodeObject
    [ ( "url"
      , encodeUrl (Sh._poolMDUrl x)
      )
    , ( "hash"
      , encodeByteStringBase16 (Sh._poolMDHash x)
      )
    ]

encodePoolParams
    :: Sh.PoolParams era
    -> Json
encodePoolParams x = encodeObject
    [ ( "id"
      , encodePoolId (Sh._poolId x)
      )
    , ( "vrf"
      , encodeHash (Sh._poolVrf x)
      )
    , ( "pledge"
      , encodeCoin (Sh._poolPledge x)
      )
    , ( "cost"
      , encodeCoin (Sh._poolCost x)
      )
    , ( "margin"
      , encodeUnitInterval (Sh._poolMargin x)
      )
    , ( "rewardAccount"
      , encodeRewardAcnt (Sh._poolRAcnt x)
      )
    , ( "owners"
      , encodeFoldable encodeKeyHash (Sh._poolOwners x)
      )
    , ( "relays"
      , encodeFoldable encodeStakePoolRelay (Sh._poolRelays x)
      )
    , ( "metadata"
      , encodeStrictMaybe encodePoolMetadata (Sh._poolMD x)
      )
    ]

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Sh.PParams' f era
    -> Json
encodePParams' encodeF x = encodeObject
    [ ( "minFeeCoefficient"
      , encodeF encodeNatural (Sh._minfeeA x)
      )
    , ( "minFeeConstant"
      , encodeF encodeNatural (Sh._minfeeB x)
      )
    , ( "maxBlockBodySize"
      , encodeF encodeNatural (Sh._maxBBSize x)
      )
    , ( "maxBlockHeaderSize"
      , encodeF encodeNatural (Sh._maxBHSize x)
      )
    , ( "maxTxSize"
      , encodeF encodeNatural (Sh._maxTxSize x)
      )
    , ( "stakeKeyDeposit"
      , encodeF encodeCoin (Sh._keyDeposit x)
      )
    , ( "poolDeposit"
      , encodeF encodeCoin (Sh._poolDeposit x)
      )
    , ( "poolRetirementEpochBound"
      , encodeF encodeEpochNo (Sh._eMax x)
      )
    , ( "desiredNumberOfPools"
      , encodeF encodeNatural (Sh._nOpt x)
      )
    , ( "poolInfluence"
      , encodeF encodeRational (Sh._a0 x)
      )
    , ( "monetaryExpansion"
      , encodeF encodeUnitInterval (Sh._rho x)
      )
    , ( "treasuryExpansion"
      , encodeF encodeUnitInterval (Sh._tau x)
      )
    , ( "decentralizationParameter"
      , encodeF encodeUnitInterval (Sh._d x)
      )
    , ( "extraEntropy"
      , encodeF encodeNonce (Sh._extraEntropy x)
      )
    , ( "protocolVersion"
      , encodeF encodeProtVer (Sh._protocolVersion x)
      )
    , ( "minUtxoValue"
      , encodeF encodeCoin (Sh._minUTxOValue x)
      )
    , ( "minPoolCost"
      , encodeF encodeCoin (Sh._minPoolCost x)
      )
    ]

encodePrevHash
    :: Sh.PrevHash crypto
    -> Json
encodePrevHash = \case
    Sh.GenesisHash -> encodeText "genesis"
    Sh.BlockHash h -> encodeHashHeader h

encodeProposedPPUpdates
    :: forall era. (Core.PParamsDelta era ~ Sh.PParams' StrictMaybe era)
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodeProtVer
    :: Sh.ProtVer
    -> Json
encodeProtVer x = encodeObject
    [ ( "major"
      , encodeNatural (Sh.pvMajor x)
      )
    , ( "minor"
      , encodeNatural (Sh.pvMinor x)
      )
    ]

encodeRewardAcnt
    :: Sh.RewardAcnt era
    -> Json
encodeRewardAcnt =
    encodeText . stringifyRewardAcnt

encodeScript
    :: Crypto crypto
    => Sh.MultiSig crypto
    -> Json
encodeScript script = encodeObject
    [ ( "native", encodeMultiSig script ) ]

encodeScriptHash
    :: Sh.ScriptHash era
    -> Json
encodeScriptHash (Sh.ScriptHash h) =
    encodeHash h

encodeSignedKES
    :: CC.KESAlgorithm alg
    => CC.SignedKES alg a
    -> Json
encodeSignedKES (CC.SignedKES raw) =
    encodeByteStringBase64 . CC.rawSerialiseSigKES $ raw

encodeShelleyHash
    :: ShelleyHash crypto
    -> Json
encodeShelleyHash =
    encodeHashHeader . unShelleyHash

encodeSignedDSIGN
    :: CC.DSIGNAlgorithm alg
    => CC.SignedDSIGN alg a
    -> Json
encodeSignedDSIGN (CC.SignedDSIGN raw) =
    encodeByteStringBase64 . CC.rawSerialiseSigDSIGN $ raw

encodeStakePoolRelay
    :: Sh.StakePoolRelay
    -> Json
encodeStakePoolRelay = \case
    Sh.SingleHostAddr port ipv4 ipv6 -> encodeObject
        [ ( "port"
          , encodeStrictMaybe encodePort port
          )
        , ( "ipv4"
          , encodeStrictMaybe encodeIPv4 ipv4
          )
        , ( "ipv6"
          , encodeStrictMaybe encodeIPv6 ipv6
          )
        ]
    Sh.SingleHostName port dns -> encodeObject
        [ ( "hostname"
          , encodeDnsName dns
          )
        , ( "port"
          , encodeStrictMaybe encodePort port
          )
        ]
    Sh.MultiHostName dns -> encodeObject
        [ ( "hostname"
          , encodeDnsName dns
          )
        , ( "port"
          , encodeNull
          )
        ]

encodeTx
    :: forall crypto. (Crypto crypto)
    => SerializationMode
    -> Sh.Tx (ShelleyEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , encodeTxId (Sh.txid @(ShelleyEra crypto) (Sh.body x))
      )
    , ( "body"
      , encodeTxBody (Sh.body x)
      )
      -- NOTE:
      -- We should really return metadata: null when there's no metadata.
      -- Right now, this returns { hash: null, body: null } when null.
      --
      -- The reason for writing it in such a way is slightly _silly_ and because
      -- of the way the generators are currently constructed. Indeed, since the
      -- metadata hash and body are strictly decoupled in the transaction model,
      -- they end up being generated separately and as a result, the generator
      -- may generate actually invalid cases like this.
    , ( "metadata", encodeObject
        [ ( "hash"
          , encodeStrictMaybe encodeAuxiliaryDataHash (Sh._mdHash (Sh.body x))
          )
        , ( "body"
          , encodeStrictMaybe encodeMetadata (Sh.auxiliaryData x)
          )
        ]
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Sh.wits x)
      )
    ]

encodeTxBody
    :: Crypto crypto
    => Sh.TxBody (ShelleyEra crypto)
    -> Json
encodeTxBody x = encodeObject
    [ ( "inputs"
      , encodeFoldable encodeTxIn (Sh._inputs x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (Sh._outputs x)
      )
    , ( "certificates"
      , encodeFoldable encodeDCert (Sh._certs x)
      )
    , ( "withdrawals"
      , encodeWdrl (Sh._wdrls x)
      )
    , ( "fee"
      , encodeCoin (Sh._txfee x)
      )
    , ( "timeToLive"
      , encodeSlotNo (Sh._ttl x)
      )
    , ( "update"
      , encodeStrictMaybe encodeUpdate (Sh._txUpdate x)
      )
    ]

encodeTxId
    :: Sh.TxId crypto
    -> Json
encodeTxId =
    encodeHash . SafeHash.extractHash . Sh._unTxId

encodeTxIn
    :: Crypto crypto
    => Sh.TxIn crypto
    -> Json
encodeTxIn (Sh.TxIn txid ix) = encodeObject
    [ ( "txId"
      , encodeTxId txid
      )
    , ( "index"
      , encodeNatural ix
      )
    ]

encodeTxOut
    :: (ShelleyBased era, Core.Value era ~ Coin)
    => Sh.TxOut era
    -> Json
encodeTxOut (Sh.TxOut addr coin) = encodeObject
    [ ( "address"
      , encodeAddress addr
      )
    , ( "value"
      , encodeCoin coin
      )
    ]

encodeUpdate
    :: forall era. (Core.PParamsDelta era ~ Sh.PParams' StrictMaybe era)
    => Sh.Update era
    -> Json
encodeUpdate (Sh.Update update epoch) = encodeObject
    [ ( "proposal"
      , encodeProposedPPUpdates update
      )
    , ( "epoch"
      , encodeEpochNo epoch
      )
    ]

encodeUpdateFailure
    :: Sh.PpupPredicateFailure era
    -> Json
encodeUpdateFailure = \case
    Sh.NonGenesisUpdatePPUP voting shouldBeVoting ->
        encodeObject
            [ ( "nonGenesisVoters", encodeObject
                [ ( "currentlyVoting", encodeFoldable encodeKeyHash voting )
                , ( "shouldBeVoting", encodeFoldable encodeKeyHash shouldBeVoting )
                ]
              )
            ]
    Sh.PPUpdateWrongEpoch currentEpoch updateEpoch votingPeriod ->
        encodeObject
            [ ( "updateWrongEpoch", encodeObject
                [ ( "currentEpoch", encodeEpochNo currentEpoch )
                , ( "requestedEpoch", encodeEpochNo updateEpoch )
                , ( "votingPeriod", encodeVotingPeriod votingPeriod )
                ]
              )
            ]
    Sh.PVCannotFollowPPUP version ->
        encodeObject
            [ ( "protocolVersionCannotFollow"
              , encodeProtVer version
              )
            ]

encodeUtxo
    :: forall era.
        ( ShelleyBased era
        , Core.Value era ~ Coin
        , Core.TxOut era ~ Sh.TxOut era
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
        encodeObject
            [ ( "badInputs"
              , encodeFoldable encodeTxIn inputs
              )
            ]
    Sh.ExpiredUTxO ttl currentSlot ->
        encodeObject
            [ ( "expiredUtxo", encodeObject
                [ ( "transactionTimeToLive", encodeSlotNo ttl )
                , ( "currentSlot", encodeSlotNo currentSlot )
                ]
              )
            ]
    Sh.MaxTxSizeUTxO actualSize maxSize ->
        encodeObject
            [ ( "txTooLarge", encodeObject
                [ ( "maximumSize", encodeInteger maxSize )
                , ( "actualSize", encodeInteger actualSize )
                ]
              )
            ]
    Sh.InputSetEmptyUTxO ->
        encodeText "missingAtLeastOneInputUtxo"
    Sh.FeeTooSmallUTxO required actual ->
        encodeObject
            [ ( "feeTooSmall", encodeObject
                [ ( "requiredFee", encodeCoin required )
                , ( "actualFee", encodeCoin actual )
                ]
              )
            ]
    Sh.ValueNotConservedUTxO consumed produced ->
        encodeObject
            [ ( "valueNotConserved", encodeObject
                [ ( "consumed", encodeCoin consumed )
                , ( "produced", encodeCoin produced )
                ]
              )
            ]
    Sh.WrongNetwork expected invalidAddrs ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , encodeEntities "address" encodeAddress invalidAddrs
                  )
                ]
              )
            ]
    Sh.WrongNetworkWithdrawal expected invalidAccts ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , encodeEntities "rewardAccount" encodeRewardAcnt invalidAccts
                  )
                ]
              )
            ]
    Sh.OutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeList encodeTxOut outs
              )
            ]
    Sh.OutputBootAddrAttrsTooBig outs ->
        encodeObject
            [ ( "addressAttributesTooLarge"
              , encodeFoldable encodeAddress ((\(Sh.TxOut addr _) -> addr) <$> outs)
              )
            ]
    Sh.UpdateFailure e ->
        encodeUpdateFailure e

encodeUtxowFailure
    :: forall era.
        ( Era era
        )
    => (PredicateFailure (Core.EraRule "UTXO" era) -> Json)
    -> Sh.UtxowPredicateFailure era
    -> Json
encodeUtxowFailure encodeUtxoFailure_ = \case
    Sh.InvalidWitnessesUTXOW wits ->
        encodeObject
            [ ( "invalidWitnesses"
              , encodeList encodeVKey wits
              )
            ]
    Sh.MissingVKeyWitnessesUTXOW keys ->
        encodeObject
            [ ( "missingVkWitnesses"
              , encodeWitHashes keys
              )
            ]
    Sh.MissingScriptWitnessesUTXOW scripts ->
        encodeObject
            [ ( "missingScriptWitnesses"
              , encodeFoldable encodeScriptHash scripts
              )
            ]
    Sh.ScriptWitnessNotValidatingUTXOW scripts ->
        encodeObject
            [ ( "scriptWitnessNotValidating"
              , encodeFoldable encodeScriptHash scripts
              )
            ]
    Sh.MIRInsufficientGenesisSigsUTXOW keys ->
        encodeObject
            [ ( "insufficientGenesisSignatures"
              , encodeFoldable encodeKeyHash keys
              )
            ]
    Sh.MissingTxBodyMetadataHash hash ->
        encodeObject
            [ ( "missingTxMetadataHash"
              , encodeAuxiliaryDataHash hash
              )
            ]
    Sh.MissingTxMetadata hash ->
        encodeObject
            [ ( "missingTxMetadata"
              , encodeAuxiliaryDataHash hash
              )
            ]
    Sh.ConflictingMetadataHash included expected ->
        encodeObject
            [ ( "txMetadataHashMismatch", encodeObject
                [ ( "includedHash" , encodeAuxiliaryDataHash included )
                , ( "expectedHash" , encodeAuxiliaryDataHash expected )
                ]
              )
            ]
    Sh.InvalidMetadata ->
        encodeText "invalidMetadata"
    Sh.UtxoFailure e ->
        encodeUtxoFailure_ e

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
    encodeByteStringBase64 . CC.rawSerialiseVerKeyKES

encodeVerKeyVRF
    :: (CC.VRFAlgorithm alg)
    => CC.VerKeyVRF alg
    -> Json
encodeVerKeyVRF =
    encodeByteStringBase64 . CC.rawSerialiseVerKeyVRF

encodeVKey
    :: Crypto crypto
    => Sh.VKey any crypto
    -> Json
encodeVKey =
    encodeVerKeyDSign . Sh.unVKey

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
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , encodeFoldable encodeWitVKey (Sh.addrWits x)
      )
    , ( "scripts"
      , encodeMap stringifyScriptHash encodeScript (Sh.scriptWits x)
      )
    , ( "bootstrap"
      , encodeFoldable encodeBootstrapWitness (Sh.bootWits x)
      )
    ]

encodeWitHashes
    :: Sh.WitHashes era
    -> Json
encodeWitHashes =
    encodeFoldable encodeKeyHash . Sh.unWitHashes

encodeWitVKey
    :: Crypto crypto
    => Sh.WitVKey Sh.Witness crypto
    -> Json
encodeWitVKey (Sh.WitVKey key sig) =
    encodeObject [(stringifyVKey  key, encodeSignedDSIGN sig)]

--
-- Conversion To Text
--

stringifyCoin
    :: Coin
    -> Text
stringifyCoin =
    show . unCoin

stringifyCredential
    :: forall any era. (any :\: Sh.StakePool)
    => Sh.Credential any era
    -> Text
stringifyCredential = \case
    Sh.KeyHashObj h -> stringifyKeyHash h
    Sh.ScriptHashObj h -> stringifyScriptHash h
  where
    _ = keepRedundantConstraint (Proxy @(any :\: Sh.StakePool))

stringifyKeyHash
    :: forall any era. (any :\: Sh.StakePool)
    => Sh.KeyHash any era
    -> Text
stringifyKeyHash (Sh.KeyHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)
  where
    _ = keepRedundantConstraint (Proxy @(any :\: Sh.StakePool))

stringifyPoolId
    :: Sh.KeyHash Sh.StakePool crypto
    -> Text
stringifyPoolId (Sh.KeyHash (CC.UnsafeHash h)) =
    encodeBech32 hrpPool (fromShort h)

stringifyRewardAcnt
    :: Sh.RewardAcnt era
    -> Text
stringifyRewardAcnt x@(Sh.RewardAcnt ntwrk _credential) =
    encodeBech32 (hrp ntwrk) (Sh.serialiseRewardAcnt x)
  where
    hrp = \case
        Sh.Mainnet -> hrpStakeMainnet
        Sh.Testnet -> hrpStakeTestnet

stringifyScriptHash
    :: Sh.ScriptHash era
    -> Text
stringifyScriptHash (Sh.ScriptHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyVKey
    :: Crypto crypto
    => Sh.VKey any crypto
    -> Text
stringifyVKey =
    encodeBase16 . CC.rawSerialiseVerKeyDSIGN . Sh.unVKey

--
-- Helpers
--

infixr 5 :\:
type family (:\:) (any :: Sh.KeyRole) (excluded :: Sh.KeyRole) :: Constraint where
    excluded :\: excluded = TypeError
        ( 'Text "Cannot use this function for the " :<>: 'ShowType excluded :<>: 'Text " role." :$$:
          'Text "Use a dedicated function instead."
        )
    _ :\: _ = ()

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
