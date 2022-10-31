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
    ( KeyRole (..)
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
    => SerializationMode
    -> TPraos.BHeader crypto
    -> Json
encodeBHeader mode (TPraos.BHeader hBody hSig) = encodeObjectWithMode mode
    [ ( "blockHeight"
      , encodeBlockNo (TPraos.bheaderBlockNo hBody)
      )
    , ( "slot"
      , encodeSlotNo (TPraos.bheaderSlotNo hBody)
      )
    , ( "prevHash"
      , encodePrevHash (TPraos.bheaderPrev hBody)
      )
    , ( "issuerVk"
      , encodeVKey (TPraos.bheaderVk hBody)
      )
    , ( "issuerVrf"
      , encodeVerKeyVRF (TPraos.bheaderVrfVk hBody)
      )
    , ( "blockSize"
      , encodeNatural (TPraos.bsize hBody)
      )
    , ( "blockHash"
      , encodeHash (TPraos.bhash hBody)
      )
    ]
    [ ( "signature"
      , encodeSignedKES hSig
      )
    , ( "nonce"
      , encodeCertifiedVRF (TPraos.bheaderEta hBody)
      )
    , ( "leaderValue"
      , encodeCertifiedVRF (TPraos.bheaderL hBody)
      )
    , ( "opCert"
      , encodeOCert (TPraos.bheaderOCert hBody)
      )
    , ( "protocolVersion"
      , encodeProtVer (TPraos.bprotver hBody)
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (TPraos crypto) (ShelleyEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
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
    :: Crypto crypto
    => Sh.Delegation crypto
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
    :: PredicateFailure (Ledger.EraRule "DELPL" era) ~ Sh.DelplPredicateFailure era
    => PredicateFailure (Ledger.EraRule "POOL" era)  ~ Sh.PoolPredicateFailure era
    => PredicateFailure (Ledger.EraRule "DELEG" era) ~ Sh.DelegPredicateFailure era
    => Crypto (Ledger.Crypto era)
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
    :: Crypto (Ledger.Crypto era)
    => Sh.DelegPredicateFailure era
    -> Json
encodeDelegFailure = \case
    Sh.StakeKeyAlreadyRegisteredDELEG credential ->
        encodeObject
            [ ( "stakeKeyAlreadyRegistered", encodeCredential credential )
            ]
    Sh.StakeKeyInRewardsDELEG credential ->
        encodeObject
            [ ( "stakeKeyAlreadyRegistered", encodeCredential credential )
            ]
    Sh.StakeKeyNotRegisteredDELEG credential ->
        encodeObject
            [ ( "stakeKeyNotRegistered", encodeCredential credential )
            ]
    Sh.StakeDelegationImpossibleDELEG credential ->
        encodeObject
            [ ( "stakeKeyNotRegistered", encodeCredential  credential )
            ]
    Sh.StakeKeyNonZeroAccountBalanceDELEG Nothing ->
        encodeObject
            [ ( "rewardAccountNotExisting", encodeNull )
            ]
    Sh.StakeKeyNonZeroAccountBalanceDELEG (Just balance) ->
        encodeObject
            [ ( "rewardAccountNotEmpty", encodeObject
                [ ( "balance" , encodeCoin  balance )
                ]
              )
            ]
    Sh.WrongCertificateTypeDELEG ->
        encodeObject
            [  ( "wrongCertificateType", encodeNull )
            ]
    Sh.GenesisKeyNotInMappingDELEG keyHash ->
        encodeObject
            [ ( "unknownGenesisKey", encodeKeyHash keyHash )
            ]
    Sh.DuplicateGenesisDelegateDELEG keyHash ->
        encodeObject
            [ ( "alreadyDelegating", encodeKeyHash keyHash )
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
        encodeObject
            [ ( "mirTransferNotCurrentlyAllowed", encodeNull )
            ]
    Sh.MIRNegativesNotCurrentlyAllowed ->
        encodeObject
            [ ( "mirNegativeTransferNotCurrentlyAllowed", encodeNull )
            ]
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
        encodeObject
            [ ( "mirProducesNegativeUpdate", encodeNull )
            ]
    Sh.MIRNegativeTransfer pot coin ->
        encodeObject
            [ ( "mirNegativeTransfer", encodeObject
                [ ( "rewardSource", encodeMIRPot pot )
                , ( "attemptedTransfer", encodeCoin coin )
                ]
              )
            ]
    Sh.DuplicateGenesisVRFDELEG vrfHash ->
        encodeObject
            [ ( "duplicateGenesisVrf", encodeHash vrfHash )
            ]

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
      , encodePositiveUnitInterval (Sh.sgActiveSlotsCoeff x)
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
    :: Ledger.Network
    -> Json
encodeNetwork = \case
    Ledger.Mainnet -> encodeText "mainnet"
    Ledger.Testnet -> encodeText "testnet"

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
encodeOCert x = encodeObject
    [ ( "hotVk"
      , encodeVerKeyKES (TPraos.ocertVkHot x)
      )
    , ( "count"
      , encodeWord64 (TPraos.ocertN x)
      )
    , ( "kesPeriod"
      , encodeKESPeriod (TPraos.ocertKESPeriod x)
      )
    , ( "sigma"
      , encodeSignedDSIGN (TPraos.ocertSigma x)
      )
    ]

encodeOutputVRF
    :: CC.OutputVRF alg
    -> Json
encodeOutputVRF =
    encodeByteStringBase64 . CC.getOutputVRFBytes

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
    Sh.PoolMedataHashTooBig poolId measuredSize ->
        encodeObject
            [ ( "poolMetadataHashTooBig", encodeObject
                [ ( "poolId"
                  , encodePoolId poolId
                  )
                , ( "measuredSize"
                  , encodeInteger (fromIntegral measuredSize)
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
    :: Crypto crypto
    => Sh.PoolParams crypto
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
      , encodeF encodeNonNegativeInterval (Sh._a0 x)
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
    encodeMap stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodeProtVer
    :: Ledger.ProtVer
    -> Json
encodeProtVer x = encodeObject
    [ ( "major"
      , encodeNatural (Ledger.pvMajor x)
      )
    , ( "minor"
      , encodeNatural (Ledger.pvMinor x)
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
    encodeByteStringBase64 . CC.rawSerialiseSigKES $ raw

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
      , encodeTxId (Ledger.txid @(ShelleyEra crypto) (Sh.body x))
      )
    , ( "body"
      , encodeTxBody (Sh.body x)
      )
    , ( "metadata"
      , (,) <$> fmap (("hash",) . encodeAuxiliaryDataHash) (Sh._mdHash (Sh.body x))
            <*> fmap (("body",) . encodeMetadata) (Sh.auxiliaryData x)
        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Sh.wits x)
      )
    , ( "raw"
      , encodeByteStringBase64 (serialize' x)
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
    :: Crypto crypto
    => Ledger.TxId crypto
    -> Json
encodeTxId =
    encodeHash . Ledger.extractHash . Ledger._unTxId

encodeTxIn
    :: Crypto crypto
    => Ledger.TxIn crypto
    -> Json
encodeTxIn (Ledger.TxIn txid (Ledger.TxIx ix)) = encodeObject
    [ ( "txId"
      , encodeTxId txid
      )
    , ( "index"
      , encodeWord64 ix
      )
    ]

encodeTxOut
    :: (ShelleyBased era, Ledger.Value era ~ Coin)
    => Sh.TxOut era
    -> Json
encodeTxOut (Sh.TxOut addr value) = encodeObject
    [ ( "address"
      , encodeAddress addr
      )
    , ( "value"
      , encodeValue value
      )
    ]

encodeUpdate
    :: Ledger.PParamsDelta era ~ Sh.PParams' StrictMaybe era
    => Crypto (Ledger.Crypto era)
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
    :: Crypto (Ledger.Crypto era)
    => Sh.PpupPredicateFailure era
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
        , Ledger.Value era ~ Coin
        , Ledger.TxOut era ~ Sh.TxOut era
        )
    => Sh.UTxO era
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple encodeTxIn encodeTxOut)

encodeUtxoWithMode
    :: forall era.
        ( ShelleyBased era
        , Ledger.Value era ~ Coin
        , Ledger.TxOut era ~ Sh.TxOut era
        )
    => SerializationMode
    -> Sh.UTxO era
    -> Json
encodeUtxoWithMode mode =
    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
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
        encodeObject
            [ ( "missingAtLeastOneInputUtxo", encodeNull )
            ]
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
    => (PredicateFailure (Ledger.EraRule "UTXO" era) -> Json)
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
        encodeObject
            [ ( "invalidMetadata", encodeNull )
            ]
    Sh.ExtraneousScriptWitnessesUTXOW scripts ->
        encodeObject
            [ ( "extraScriptWitnesses"
              , encodeFoldable encodeScriptHash scripts
              )
            ]
    Sh.UtxoFailure e ->
        encodeUtxoFailure_ e

encodeValue
    :: Coin
    -> Json
encodeValue coin = encodeObject
    [ ( "coins", encodeCoin coin )
    ]

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
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , encodeWitVKeys (Sh.addrWits x)
      )
    , ( "scripts"
      , encodeMap stringifyScriptHash encodeScript (Sh.scriptWits x)
      )
    , ( "bootstrap"
      , encodeFoldable encodeBootstrapWitness (Sh.bootWits x)
      )
    ]

encodeWitHashes
    :: Crypto crypto
    => Sh.WitHashes crypto
    -> Json
encodeWitHashes =
    encodeFoldable encodeKeyHash . Sh.unWitHashes

encodeWitVKeys
    :: Crypto crypto
    => Set (Sh.WitVKey Witness crypto)
    -> Json
encodeWitVKeys = encodeFoldable'
    (\(Sh.WitVKey key _) -> stringifyVKey key)
    (\(Sh.WitVKey _ sig) -> encodeSignedDSIGN sig)

--
-- Conversion To Text
--

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
