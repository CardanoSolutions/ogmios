--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Data.Json.Shelley where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Era
    ( Era )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..), SlotNo (..) )
import Control.Arrow
    ( right )
import Data.Aeson
    ( (.:) )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.ByteString.Short
    ( fromShort )
import Data.Type.Equality
    ( type (==) )
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
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Ogmios.Data.Json.Byron as Byron

import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC

import qualified Cardano.Api.Typed as Api
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
    :: forall any era. ((any == Sh.StakePool) ~ False)
    => Sh.Credential any era
    -> Json
encodeCredential x = case x of
    Sh.KeyHashObj h -> encodeKeyHash h
    Sh.ScriptHashObj h -> encodeScriptHash h
  where
    _ = keepRedundantConstraint (Proxy @((any == Sh.StakePool) ~ False))

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
    :: forall any crypto. ((any == Sh.StakePool) ~ False)
    => Sh.KeyHash any crypto
    -> Json
encodeKeyHash (Sh.KeyHash h) =
    encodeHash h
  where
    _ = keepRedundantConstraint (Proxy @((any == Sh.StakePool) ~ False))

encodeKESPeriod
    :: Sh.KESPeriod
    -> Json
encodeKESPeriod =
    encodeWord . Sh.unKESPeriod

encodeMetaData
    :: Sh.MetaData
    -> Json
encodeMetaData (Sh.MetaData md) =
    encodeMap show encodeMetaDatum md
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

encodeMetaDataHash
    :: Sh.MetaDataHash era
    -> Json
encodeMetaDataHash =
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

encodePoolId
    :: Sh.KeyHash 'Sh.StakePool crypto
    -> Json
encodePoolId =
    encodeText . stringifyPoolId

encodePoolMetaData
    :: Sh.PoolMetaData
    -> Json
encodePoolMetaData x = encodeObject
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
      , encodeStrictMaybe encodePoolMetaData (Sh._poolMD x)
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
          , encodeStrictMaybe encodeMetaDataHash (Sh._mdHash (Sh._body x))
          )
        , ( "body"
          , encodeStrictMaybe encodeMetaData (Sh._metadata x)
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

encodeVerKeyDSign
    :: CC.DSIGNAlgorithm alg
    => CC.VerKeyDSIGN alg
    -> Json
encodeVerKeyDSign =
    encodeByteStringBase64 . CC.rawSerialiseVerKeyDSIGN

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

encodeWitVKey
    :: Era era
    => Sh.WitVKey 'Sh.Witness era
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
    :: Sh.Credential any era
    -> Text
stringifyCredential = \case
    Sh.KeyHashObj h -> stringifyKeyHash h
    Sh.ScriptHashObj h -> stringifyScriptHash h

stringifyKeyHash
    :: Sh.KeyHash any era
    -> Text
stringifyKeyHash (Sh.KeyHash (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyPoolId
    :: Sh.KeyHash 'Sh.StakePool crypto
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
    -> (EpochNo -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetEpochNo genResult encodeMismatchEraInfo _ =
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
        arg <- obj .: "nonMyopicMemberRewards"
            >>= traverse (choice "credential" [ parseStake, parseCredential ])
        pure $ SomeQuery
            { query = QueryIfCurrentShelley (GetNonMyopicMemberRewards $ fromList arg)
            , encodeResult = either encodeMismatchEraInfo encodeNonMyopicMemberRewards
            , genResult
            }
  where
    parseStake
        :: Json.Value
        -> Json.Parser (Either Sh.Coin b)
    parseStake =
        fmap (Left . Sh.word64ToCoin) . Json.parseJSON

    -- TODO: Makes it possible to distinguish between KeyHash and ScriptHash
    -- credentials. Both are encoded as hex-encoded strings. Encoding them as
    -- object is ill-advised because we also need them as key of the non-myopic
    -- member rewards map.
    --
    -- A possible option: encode them as Bech32 strings with different prefixes.
    parseCredential
        :: Json.Value
        -> Json.Parser (Either a (Sh.Credential 'Sh.Staking (ShelleyEra crypto)))
    parseCredential =
        fmap (Right . Sh.KeyHashObj . Sh.KeyHash) . parseHash

parseHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex
