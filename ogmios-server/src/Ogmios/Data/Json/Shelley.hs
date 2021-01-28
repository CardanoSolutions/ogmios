--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
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
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Control.State.Transition
    ( STS (..) )
import Data.Aeson
    ( (.:) )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.ByteString.Short
    ( fromShort )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, CardanoEras, Query (..), ShelleyEra )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( NonMyopicMemberRewards, Query (..) )
import Ouroboros.Network.Block
    ( Point, castPoint )

import qualified Ogmios.Data.Json.Byron as Byron

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC

import qualified Cardano.Ledger.Core as Sh.Core
import qualified Cardano.Ledger.Shelley.Constraints as Sh
import qualified Shelley.Spec.Ledger.Address as Sh
import qualified Shelley.Spec.Ledger.Address.Bootstrap as Sh
import qualified Shelley.Spec.Ledger.BaseTypes as Sh
import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Coin as Sh
import qualified Shelley.Spec.Ledger.Credential as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.Keys as Sh
import qualified Shelley.Spec.Ledger.LedgerState as Sh
import qualified Shelley.Spec.Ledger.MetaData as Sh
import qualified Shelley.Spec.Ledger.OCert as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.Scripts as Sh
import qualified Shelley.Spec.Ledger.STS.Deleg as Sh
import qualified Shelley.Spec.Ledger.STS.Delegs as Sh
import qualified Shelley.Spec.Ledger.STS.Delpl as Sh
import qualified Shelley.Spec.Ledger.STS.Ledger as Sh
import qualified Shelley.Spec.Ledger.STS.Ledgers as Sh
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
        Sh.Mainnet -> [humanReadablePart|addr|]
        Sh.Testnet -> [humanReadablePart|addr_test|]

encodeBHeader
    :: Crypto crypto
    => Sh.BHeader crypto
    -> Json
encodeBHeader (Sh.BHeader hBody hSig) = encodeObject
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
    , ( "nonce"
      , encodeCertifiedVRF (Sh.bheaderEta hBody)
      )
    , ( "leaderValue"
      , encodeCertifiedVRF (Sh.bheaderL hBody)
      )
    , ( "blockSize"
      , encodeNatural (Sh.bsize hBody)
      )
    , ( "blockHash"
      , encodeHashBody (Sh.bhash hBody)
      )
    , ( "opCert"
      , encodeOCert (Sh.bheaderOCert hBody)
      )
    , ( "protocolVersion"
      , encodeProtVer (Sh.bprotver hBody)
      )
    , ( "signature"
      , encodeSignedKES hSig
      )
    ]

encodeBootstrapWitness
    :: Era era
    => Sh.BootstrapWitness era
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

encodeCoin
    :: Sh.Coin
    -> Json
encodeCoin =
    encodeInteger . Sh.unCoin

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
    Sh.DCertMir (Sh.MIRCert pot rewards) -> encodeObject
        [ ( "moveInstantaneousRewards", encodeObject
            [ ( "pot"
              , encodeMIRPot pot
              )
            , ( "rewards"
              , encodeMap stringifyCredential encodeCoin rewards
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
    :: Sh.DelegsPredicateFailure era
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
    Sh.DuplicateGenesisVRFDELEG vrfHash ->
        encodeObject
            [ ( "duplicateGenesisVrf"
              , encodeHash vrfHash
              )
            ]

encodeDeltaCoin
    :: Sh.DeltaCoin
    -> Json
encodeDeltaCoin (Sh.DeltaCoin delta) =
    encodeInteger delta

encodeDeplFailure
    :: Sh.DelplPredicateFailure era
    -> Json
encodeDeplFailure = \case
    Sh.PoolFailure e ->
        encodePoolFailure e
    Sh.DelegFailure e ->
        encodeDelegFailure e

encodeHash
    :: CC.Hash alg a
    -> Json
encodeHash (CC.UnsafeHash h) =
    encodeByteStringBase16 (fromShort h)

encodeHashBody
    :: Sh.HashBBody crypto
    -> Json
encodeHashBody =
    encodeHash . Sh.unHashBody

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
    => Sh.LedgersPredicateFailure (ShelleyEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.LedgerFailure (Sh.UtxowFailure e) ->
        encodeUtxowFailure encodeUtxoFailure e
    Sh.LedgerFailure (Sh.DelegsFailure e) ->
        encodeDelegsFailure e

encodeMetadata
    :: Sh.MetaData
    -> Json
encodeMetadata (Sh.MetaData blob) = encodeObject
    [ ( "blob"
      , encodeMetadataBlob blob
      )
    ]

encodeMetadataBlob
    :: Map Word64 Sh.MetaDatum
    -> Json
encodeMetadataBlob =
    encodeMap show encodeMetaDatum
  where
    encodeMetaDatum :: Sh.MetaDatum -> Json
    encodeMetaDatum = \case
        Sh.I n ->
            encodeObject [("int", encodeInteger n)]
        Sh.B bytes ->
            encodeObject [("bytes", encodeByteStringBase16 bytes)]
        Sh.S txt ->
            encodeObject [("string", encodeText txt)]
        Sh.List xs ->
            encodeObject [("list", encodeList encodeMetaDatum xs)]
        Sh.Map xs ->
            encodeObject [("map", encodeList encodeKeyPair xs)]

    encodeKeyPair :: (Sh.MetaDatum, Sh.MetaDatum) -> Json
    encodeKeyPair = encode2Tuple
        (encodeObject . pure @[] . ("k",) . encodeMetaDatum)
        (encodeObject . pure @[] . ("v",) . encodeMetaDatum)

encodeMetadataHash
    :: Sh.MetaDataHash era
    -> Json
encodeMetadataHash =
    encodeHash . Sh.unsafeMetaDataHash

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
    => Sh.MultiSig (ShelleyEra crypto)
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

encodePoolMetadata
    :: Sh.PoolMetaData
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
    :: Sh.ProposedPPUpdates era
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

encodeShelleyBlock
    :: Crypto crypto
    => ShelleyBlock (ShelleyEra crypto)
    -> Json
encodeShelleyBlock (ShelleyBlock (Sh.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable encodeTx (Sh.txSeqTxns' txs)
      )
    , ( "header"
      , encodeBHeader blkHeader
      )
    , ( "headerHash"
      , encodeShelleyHash headerHash
      )
    ]

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
    :: Crypto crypto
    => Sh.Tx (ShelleyEra crypto)
    -> Json
encodeTx x = encodeObject
    [ ( "id"
      , encodeTxId (Sh.txid (Sh._body x))
      )
    , ( "body"
      , encodeTxBody (Sh._body x)
      )
    , ( "witness"
      , encodeWitnessSet (Sh._witnessSet x)
      )
    , ( "metadata", encodeObject
        [ ( "hash"
          , encodeStrictMaybe encodeMetadataHash (Sh._mdHash (Sh._body x))
          )
        , ( "body"
          , encodeStrictMaybe encodeMetadata (Sh._metadata x)
          )
        ]
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
    :: Sh.TxId era
    -> Json
encodeTxId =
    encodeHash . Sh._unTxId

encodeTxIn
    :: Era era
    => Sh.TxIn era
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
    :: (Sh.ShelleyBased era, Sh.Core.Value era ~ Sh.Coin)
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
    :: Sh.Update era
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

encodeUTxO
    :: forall era. (Sh.ShelleyBased era, Sh.Core.Value era ~ Sh.Coin)
    => Sh.UTxO era
    -> Json
encodeUTxO =
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
                [ ( "consumed", encodeDeltaCoin consumed )
                , ( "produced", encodeDeltaCoin produced )
                ]
              )
            ]
    Sh.WrongNetwork expected invalidAddrs ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork", encodeNetwork expected )
                , ( "invalidAddresses", encodeFoldable encodeAddress invalidAddrs )
                ]
              )
            ]
    Sh.WrongNetworkWithdrawal expected invalidAccts ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork", encodeNetwork expected )
                , ( "invalidRewardAccounts", encodeFoldable encodeRewardAcnt invalidAccts )
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
    => (PredicateFailure (Sh.UTXO era) -> Json)
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
    Sh.MissingTxBodyMetaDataHash hash ->
        encodeObject
            [ ( "missingTxMetadataHash"
              , encodeMetadataHash hash
              )
            ]
    Sh.MissingTxMetaData hash ->
        encodeObject
            [ ( "missingTxMetadata"
              , encodeMetadataHash hash
              )
            ]
    Sh.ConflictingMetaDataHash included expected ->
        encodeObject
            [ ( "txMetadataHashMismatch", encodeObject
                [ ( "includedHash" , encodeMetadataHash included )
                , ( "expectedHash" , encodeMetadataHash expected )
                ]
              )
            ]
    Sh.InvalidMetaData ->
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
    [ ( "address"
      , encodeFoldable encodeWitVKey (Sh.addrWits x)
      )
    , ( "script"
      , encodeMap stringifyScriptHash encodeMultiSig (Sh.scriptWits x)
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
    :: Era era
    => Sh.WitVKey Sh.Witness era
    -> Json
encodeWitVKey (Sh.WitVKey key sig) = encodeObject
    [ ( "key"
      , encodeVKey key
      )
    , ( "signature"
      , encodeSignedDSIGN sig
      )
    ]

--
-- Conversion To Text
--

stringifyCoin
    :: Sh.Coin
    -> Text
stringifyCoin =
    show . Sh.unCoin

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
    Bech32.encodeLenient hrp (dataPart h)
  where
    dataPart = Bech32.dataPartFromBytes . fromShort
    hrp = [humanReadablePart|pool|]

stringifyRewardAcnt
    :: Sh.RewardAcnt era
    -> Text
stringifyRewardAcnt x@(Sh.RewardAcnt ntwrk _credential) =
    Bech32.encodeLenient (hrp ntwrk) (dataPart x)
  where
    dataPart = Bech32.dataPartFromBytes . Sh.serialiseRewardAcnt
    hrp = \case
        Sh.Mainnet -> [humanReadablePart|stake|]
        Sh.Testnet -> [humanReadablePart|stake_test|]

stringifyScriptHash
    :: Sh.ScriptHash era
    -> Text
stringifyScriptHash (Sh.ScriptHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

--
-- Parsers
--

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

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
    -> Json.Parser (Sh.Credential Sh.Staking (ShelleyEra crypto))
parseCredential =
    fmap (Sh.KeyHashObj . Sh.KeyHash) . parseHash

parseGetLedgerTip
    :: forall crypto f result era.
        ( era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (Point (ShelleyBlock era))
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> (Point (CardanoBlock crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetLedgerTip genResult encodeMismatchEraInfo encodePoint =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> SomeQuery
            { query = QueryIfCurrentShelley GetLedgerTip
            , encodeResult = either encodeMismatchEraInfo (encodePoint . castPoint)
            , genResult
            }

parseGetEpochNo
    :: forall crypto f result.
        ( result ~ QueryResult crypto EpochNo
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetEpochNo genResult encodeMismatchEraInfo =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentEpoch") $> SomeQuery
            { query = QueryIfCurrentShelley GetEpochNo
            , encodeResult = either encodeMismatchEraInfo encodeEpochNo
            , genResult
            }

parseGetNonMyopicMemberRewards
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (NonMyopicMemberRewards era)
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> (NonMyopicMemberRewards era -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetNonMyopicMemberRewards genResult encodeMismatchEraInfo encodeNonMyopicMemberRewards =
    Json.withObject "SomeQuery" $ \obj -> do
        arg <- obj .: "nonMyopicMemberRewards" >>= traverse
            (choice "credential"
                [ fmap Left  . parseCoin
                , fmap Right . parseCredential
                ]
            )
        pure $ SomeQuery
            { query = QueryIfCurrentShelley (GetNonMyopicMemberRewards $ fromList arg)
            , encodeResult = either encodeMismatchEraInfo encodeNonMyopicMemberRewards
            , genResult
            }

parseGetCurrentPParams
    :: forall crypto f result era.
        ( era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (Sh.PParams era)
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetCurrentPParams genResult encodeMismatchEraInfo =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentProtocolParameters") $> SomeQuery
            { query = QueryIfCurrentShelley GetCurrentPParams
            , encodeResult = either encodeMismatchEraInfo (encodePParams' id)
            , genResult
            }

parseGetProposedPParamsUpdates
    :: forall crypto f result era.
        ( era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (Sh.ProposedPPUpdates era)
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetProposedPParamsUpdates genResult encodeMismatchEraInfo =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "proposedProtocolParameters") $> SomeQuery
            { query = QueryIfCurrentShelley GetProposedPParamsUpdates
            , encodeResult = either encodeMismatchEraInfo encodeProposedPPUpdates
            , genResult
            }

parseGetStakeDistribution
    :: forall crypto f result.
        ( result ~ QueryResult crypto (Sh.PoolDistr crypto)
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetStakeDistribution genResult encodeMismatchEraInfo =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "stakeDistribution") $> SomeQuery
            { query = QueryIfCurrentShelley GetStakeDistribution
            , encodeResult = either encodeMismatchEraInfo encodePoolDistr
            , genResult
            }

parseGetUTxO
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (Sh.UTxO era)
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetUTxO genResult encodeMismatchEraInfo =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "utxo") $> SomeQuery
            { query = QueryIfCurrentShelley GetUTxO
            , encodeResult = either encodeMismatchEraInfo encodeUTxO
            , genResult
            }

-- parseGetFilteredUTxO
--     :: forall crypto f result era.
--         ( Crypto crypto
--         , era ~ ShelleyEra crypto
--         , result ~ QueryResult crypto (Sh.UTxO era)
--         )
--     => (Proxy result -> f result)
--     -> Json.Value
--     -> Json.Parser (SomeQuery f Json.Value (CardanoBlock crypto))
-- parseGetFilteredUTxO genResult = Json.withObject "SomeQuery" $ \obj -> do
--     addrs <- obj .: "utxo" >>= traverse parseAddress
--     pure SomeQuery
--         { query = QueryIfCurrentShelley (GetFilteredUTxO $ fromList addrs)
--         , encodeResult = toAltJSON
--         , genResult
--         }
--   where
--     parseAddress :: Json.Value -> Json.Parser (Sh.Addr era)
--     parseAddress = Json.withText "Address" $ choice "address"
--         [ addressFromBytes fromBech32
--         , addressFromBytes fromBase58
--         , addressFromBytes fromBase16
--         ]
--       where
--         addressFromBytes decode =
--             decode >=> maybe mempty pure . Sh.deserialiseAddr
--
--         fromBech32 txt =
--             case Bech32.decodeLenient txt of
--                 Left e ->
--                     fail (show e)
--                 Right (_, dataPart) ->
--                     maybe mempty pure $ Bech32.dataPartToBytes dataPart
--
--         fromBase58 =
--             maybe mempty pure . decodeBase58 bitcoinAlphabet . encodeUtf8
--
--         fromBase16 =
--             either (fail . show) pure . convertFromBase @ByteString Base16 . encodeUtf8

parseHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex

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
