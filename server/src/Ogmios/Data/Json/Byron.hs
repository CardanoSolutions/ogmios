--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Byron where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( toCBOR
    )
import Codec.CBOR.Encoding
    ( encodeListLen
    )
import Codec.CBOR.Write
    ( toStrictByteString
    )
import Data.ByteString.Base16
    ( encodeBase16
    )

import qualified Cardano.Chain.Block as By hiding
    ( Proof
    , signature
    )
import qualified Cardano.Chain.Block as By.Block
import qualified Cardano.Chain.Byron.API as By
import qualified Cardano.Chain.Common as By
import qualified Cardano.Chain.Delegation as By hiding
    ( APayload
    , signature
    )
import qualified Cardano.Chain.Delegation as By.Dlg
import qualified Cardano.Chain.Genesis as By
import qualified Cardano.Chain.Slotting as By
import qualified Cardano.Chain.UTxO as By
import qualified Cardano.Chain.Update as By hiding
    ( APayload
    , Proof
    )
import qualified Cardano.Chain.Update as By.Upd
import qualified Cardano.Chain.Update.Proposal as By.Upd.Proposal
import qualified Cardano.Chain.Update.Vote as By.Upd.Vote
import qualified Cardano.Crypto.Hashing as By
import qualified Cardano.Crypto.ProtocolMagic as By
import qualified Cardano.Crypto.Signing as By
import qualified Cardano.Crypto.Wallet as CC

encodeAddress
    :: By.Address
    -> Json
encodeAddress =
    encodeText . decodeUtf8 . By.addrToBase58

encodeABlockOrBoundary
    :: By.ABlockOrBoundary ByteString
    -> Json
encodeABlockOrBoundary = encodeObject . \case
    By.ABOBBlock blk ->
        "era" .= encodeText "byron"
        <>
        "header" .= encodeObject
            ( "hash" .= encodeHash (By.blockHashAnnotated blk)
            )
        <>
        "ancestor" .= encodeAnnotated encodeHash (By.aHeaderPrevHash h)
        <>
        "height" .= encodeAnnotated encodeChainDifficulty (By.aHeaderDifficulty h)
        <>
        "slot" .= encodeAnnotated encodeSlotNumber (By.aHeaderSlot h)
        <>
        "size" .= encodeNatural (By.blockLength blk)
        <>
        encodeABody (By.blockBody blk)
        <>
        "protocol" .= encodeObject
            ( "magic" .= encodeAnnotated encodeProtocolMagicId (By.aHeaderProtocolMagicId h)
            <>
              "version" .= encodeProtocolVersion (By.headerProtocolVersion h)
            <>
              "software" .= encodeSoftwareVersion (By.headerSoftwareVersion h)
            )
        <>
        "issuer" .= encodeObject
            ( "verificationKey" .= encodeVerificationKey (By.Dlg.issuerVK c)
            )
        <>
        "delegate" .= encodeObject
            ( "verificationKey" .= encodeVerificationKey (By.Dlg.delegateVK c)
            )
      where
        h = By.blockHeader blk
        s = By.headerSignature h
        c = By.delegationCertificate s

    By.ABOBBoundary blk ->
        "era" .= encodeText "byron"
        <>
        "height" .= encodeChainDifficulty (By.boundaryDifficulty h)
        <>
        "header" .= encodeObject ("hash" .= encodeHash (By.boundaryHashAnnotated blk))
        <>
        "ancestor" .= either encodeGenesisHash encodeHash (By.boundaryPrevHash h)
      where
        h = By.boundaryHeader blk

encodeABody
    :: By.ABody any
    -> Series
encodeABody x =
    "transactions" .=
        encodeATxPayload (By.bodyTxPayload x) <>
    "governanceAction" .=
        encodeAUpdPayload (By.bodyUpdatePayload x) <>
    "operationalCertificates" .=
        encodeADlgPayload (By.bodyDlgPayload x)

encodeACertificate
    :: By.ACertificate any
    -> Json
encodeACertificate x =
    encodeObject
        ( "issuer" .= encodeObject
            ( "verificationKey" .= encodeVerificationKey (By.Dlg.issuerVK x)
            )
        <>
          "delegate" .= encodeObject
            ( "verificationKey" .= encodeVerificationKey (By.Dlg.delegateVK x)
            )
        )

encodeADlgPayload
    :: By.Dlg.APayload any
    -> Json
encodeADlgPayload =
    encodeList encodeACertificate . By.Dlg.getPayload

encodeBlockCount :: By.BlockCount -> Json
encodeBlockCount =
    encodeWord64 . By.unBlockCount

encodeGenesisData
    :: By.GenesisData
    -> Json
encodeGenesisData x =
    "genesisKeyHashes" .=
        encodeFoldable
            encodeKeyHash
            (By.unGenesisKeyHashes (By.gdGenesisKeyHashes x)) <>
    "genesisDelegations" .=
        encodeMap
            stringifyKeyHash
            encodeACertificate
            (By.unGenesisDelegation (By.gdHeavyDelegation x)) <>
    "systemStart" .=
        encodeUtcTime (By.gdStartTime x) <>
    "initialFunds" .=
        encodeMap
            stringifyAddress
            encodeLovelace
            (By.unGenesisNonAvvmBalances (By.gdNonAvvmBalances x)) <>
    "initialCoinOffering" .=
        encodeMap
            (stringifyRedeemVerificationKey . By.fromCompactRedeemVerificationKey)
            encodeLovelace
            (By.unGenesisAvvmBalances (By.gdAvvmDistr x)) <>
    "securityParameter" .=
        encodeBlockCount (By.gdK x) <>
    "networkMagic" .=
        encodeProtocolMagicId (By.gdProtocolMagicId x) <>
    "protocolParameters" .=
        encodeProtocolParameters (By.gdProtocolParameters x)
    & encodeObject

encodeKeyHash :: By.KeyHash -> Json
encodeKeyHash =
    encodeHash . By.unKeyHash

encodeTxIn
    :: By.TxIn
    -> Json
encodeTxIn (By.TxInUtxo txid ix) =
    "txId" .=
        encodeHash txid <>
    "index" .=
        encodeWord16 ix
    & encodeObject

encodeValue
    :: By.Lovelace
    -> Json
encodeValue coins =
    "coins" .= encodeLovelace coins
    & encodeObject

encodeTxOut
    :: By.TxOut
    -> Json
encodeTxOut x =
    "address" .=
        encodeAddress (By.txOutAddress x) <>
    "value" .=
        encodeValue (By.txOutValue x)
    & encodeObject

encodeATxAux
    :: By.ATxAux any
    -> Json
encodeATxAux x =
    "id" .= encodeHash (By.serializeCborHash (By.taTx x))
        <>
    "inputSource" .= encodeText "inputs"
        <>
    encodeAnnotated encodeTx (By.aTaTx x)
        <>
    "signatories" .= encodeAnnotated (encodeFoldable encodeTxInWitness) (By.aTaWitness x)
        <>
    "cbor" .=
        ( let bytes = encodeListLen 2 <> toCBOR (By.taTx x) <> toCBOR (By.taWitness x)
           in encodeByteStringBase16 (toStrictByteString bytes)
        )
    & encodeObject

encodeTx
    :: By.Tx
    -> Series
encodeTx x =
    "inputs" .=
        encodeFoldable encodeTxIn (By.txInputs x) <>
    "outputs" .=
        encodeFoldable encodeTxOut (By.txOutputs x)

encodeATxPayload
    :: By.ATxPayload any
    -> Json
encodeATxPayload =
    encodeList encodeATxAux . By.aUnTxPayload

encodeAUpdPayload
    :: By.Upd.APayload any
    -> Json
encodeAUpdPayload x =
    "proposal" .=? OmitWhenNothing
        encodeAUpdProposal (maybeToStrictMaybe (By.Upd.payloadProposal x)) <>
    "votes" .=
        encodeList encodeAVote (By.Upd.payloadVotes x)
    & encodeObject

encodeAUpdProposal
    :: By.Upd.Proposal.AProposal any
    -> Json
encodeAUpdProposal x =
    encodeObject
        ( encodeAnnotated encodeAUpdProposalBody (By.Upd.Proposal.aBody x)
        <>
        "issuer" .= encodeObject
            ( "verificationKey" .= encodeVerificationKey (By.Upd.Proposal.issuer x)
            )
        )

encodeAUpdProposalBody
    :: By.Upd.Proposal.ProposalBody
    -> Series
encodeAUpdProposalBody x =
    "protocol" .= encodeObject
        ( "version" .= encodeProtocolVersion (By.Upd.Proposal.protocolVersion x)
        <>
          "software" .= encodeSoftwareVersion (By.Upd.Proposal.softwareVersion x)
        )
    <>
    "parameters" .= encodeProtocolParametersUpdate (By.Upd.Proposal.protocolParametersUpdate x)
    <>
    "metadata" .= encodeMap By.Upd.getSystemTag encodeInstallerHash (By.Upd.Proposal.metadata x)

encodeAVote
    :: By.Upd.Vote.AVote any
    -> Json
encodeAVote x = encodeObject
    ( "proposal" .= encodeObject
        ( "hash" .= encodeAnnotated encodeHash (By.Upd.Vote.aProposalId x)
        )
    <> "voter" .= encodeObject
        ( "verificationKey" .=
            encodeVerificationKey (By.Upd.Vote.voterVK x)
        )
    )

encodeApplicationName
    :: By.ApplicationName
    -> Json
encodeApplicationName =
    encodeText . By.unApplicationName

encodeApplyMempoolPayloadErr
    :: By.ApplyMempoolPayloadErr
    -> Json
encodeApplyMempoolPayloadErr = \case
    By.MempoolTxErr e ->
        encodeUTxOValidationError e
    -- NOTE
    -- branches below aren't actually used because we only submit
    -- payment transaction through the protocol.
    By.MempoolDlgErr _e ->
        encodeText "mempoolDelegationError"
    By.MempoolUpdateProposalErr _e ->
        encodeText "mempoolUpdateProposalError"
    By.MempoolUpdateVoteErr _e ->
        encodeText "mempoolUpdateVoteError"

encodeBlockProof
    :: By.Block.Proof
    -> Json
encodeBlockProof x =
    "utxo" .=
        encodeTxProof (By.proofUTxO x) <>
    "delegation" .=
        encodeHash (By.proofDelegation x) <>
    "update" .=
        encodeHash (By.proofUpdate x)
    & encodeObject

encodeChainDifficulty
    :: By.ChainDifficulty
    -> Json
encodeChainDifficulty =
    encodeWord64 . By.unChainDifficulty

encodeEpochNumber
    :: By.EpochNumber
    -> Json
encodeEpochNumber =
    encodeWord64 . By.getEpochNumber

encodeGenesisHash
    :: By.GenesisHash
    -> Json
encodeGenesisHash =
    encodeHash . By.unGenesisHash

encodeHash
    :: By.AbstractHash alg a
    -> Json
encodeHash =
    encodeByteStringBase16 . By.hashToBytes

encodeInstallerHash
    :: By.InstallerHash
    -> Json
encodeInstallerHash =
    encodeHash . By.unInstallerHash

encodeLovelace
    :: By.Lovelace
    -> Json
encodeLovelace =
    encodeInteger . By.lovelaceToInteger

encodeLovelaceError
    :: By.LovelaceError
    -> Json
encodeLovelaceError = \case
    By.LovelaceOverflow x ->
        encodeObject ("overflow" .= encodeWord64 x)
    By.LovelaceTooLarge x ->
        encodeObject ("tooLarge" .= encodeInteger x)
    By.LovelaceTooSmall x ->
        encodeObject ("tooSmall" .= encodeInteger x)
    By.LovelaceUnderflow x y ->
        encodeObject ("underflow" .= encodeList encodeWord64 [x,y])

encodeLovelacePortion
    :: By.LovelacePortion
    -> Json
encodeLovelacePortion =
    encodeRational . By.lovelacePortionToRational

encodeMerkleRoot
    :: By.MerkleRoot any
    -> Json
encodeMerkleRoot =
    encodeHash . By.getMerkleRoot

encodeNetworkMagic
    :: By.NetworkMagic
    -> Json
encodeNetworkMagic = \case
    By.NetworkMainOrStage ->
        encodeText "mainnet"
    By.NetworkTestnet pm ->
        encodeObject ("testnet" .= encodeWord32 pm)

encodeProtocolMagicId
    :: By.ProtocolMagicId
    -> Json
encodeProtocolMagicId =
    encodeWord32 . By.unProtocolMagicId

encodeProtocolParameters
    :: By.ProtocolParameters
    -> Json
encodeProtocolParameters x =
    "scriptVersion" .=
        encodeWord16 (By.ppScriptVersion x) <>
    "slotDuration" .=
        encodeNatural (By.ppSlotDuration x) <>
    "maxBlockSize" .=
        encodeNatural (By.ppMaxBlockSize x) <>
    "maxHeaderSize" .=
        encodeNatural (By.ppMaxHeaderSize x) <>
    "maxTxSize" .=
        encodeNatural (By.ppMaxTxSize x) <>
    "maxProposalSize" .=
        encodeNatural (By.ppMaxProposalSize x) <>
    "mpcThreshold" .=
        encodeLovelacePortion (By.ppMpcThd x) <>
    "heavyDlgThreshold" .=
        encodeLovelacePortion (By.ppHeavyDelThd x) <>
    "updateVoteThreshold" .=
        encodeLovelacePortion (By.ppUpdateVoteThd x) <>
    "updateProposalThreshold" .=
        encodeLovelacePortion (By.ppUpdateProposalThd x) <>
    "updateProposalTimeToLive" .=
        encodeSlotNumber (By.ppUpdateProposalTTL x) <>
    "softforkRule" .=
        encodeSoftforkRule (By.ppSoftforkRule x) <>
    "txFeePolicy" .=
        encodeTxFeePolicy (By.ppTxFeePolicy x) <>
    "unlockStakeEpoch" .=
        encodeEpochNumber (By.ppUnlockStakeEpoch x)
    & encodeObject


encodeProtocolParametersUpdate
    :: By.ProtocolParametersUpdate
    -> Json
encodeProtocolParametersUpdate x =
    "scriptVersion" .=? OmitWhenNothing
        encodeWord16 (maybeToStrictMaybe (By.ppuScriptVersion x)) <>
    "slotDuration" .=? OmitWhenNothing
        encodeNatural (maybeToStrictMaybe (By.ppuSlotDuration x)) <>
    "maxBlockSize" .=? OmitWhenNothing
        encodeNatural (maybeToStrictMaybe (By.ppuMaxBlockSize x)) <>
    "maxHeaderSize" .=? OmitWhenNothing
        encodeNatural (maybeToStrictMaybe (By.ppuMaxHeaderSize x)) <>
    "maxTxSize" .=? OmitWhenNothing
        encodeNatural (maybeToStrictMaybe (By.ppuMaxTxSize x)) <>
    "maxProposalSize" .=? OmitWhenNothing
        encodeNatural (maybeToStrictMaybe (By.ppuMaxProposalSize x)) <>
    "mpcThreshold" .=? OmitWhenNothing
        encodeLovelacePortion (maybeToStrictMaybe (By.ppuMpcThd x)) <>
    "heavyDlgThreshold" .=? OmitWhenNothing
        encodeLovelacePortion (maybeToStrictMaybe (By.ppuHeavyDelThd x)) <>
    "updateVoteThreshold" .=? OmitWhenNothing
        encodeLovelacePortion (maybeToStrictMaybe (By.ppuUpdateVoteThd x)) <>
    "updateProposalThreshold" .=? OmitWhenNothing
        encodeLovelacePortion (maybeToStrictMaybe (By.ppuUpdateProposalThd x)) <>
    "updateProposalTimeToLive" .=? OmitWhenNothing
        encodeSlotNumber (maybeToStrictMaybe (By.ppuUpdateProposalTTL x)) <>
    "softforkRule" .=? OmitWhenNothing
        encodeSoftforkRule (maybeToStrictMaybe (By.ppuSoftforkRule x)) <>
    "txFeePolicy" .=? OmitWhenNothing
        encodeTxFeePolicy (maybeToStrictMaybe (By.ppuTxFeePolicy x)) <>
    "unlockStakeEpoch" .=? OmitWhenNothing
        encodeEpochNumber (maybeToStrictMaybe (By.ppuUnlockStakeEpoch x))
    & encodeObject

encodeProtocolVersion
    :: By.ProtocolVersion
    -> Json
encodeProtocolVersion x =
    "major" .=
        encodeWord16 (By.pvMajor x) <>
    "minor" .=
        encodeWord16 (By.pvMinor x) <>
    "patch" .=
        encodeWord8 (By.pvAlt x)
    & encodeObject

encodeRedeemSignature
    :: By.RedeemSignature any
    -> Json
encodeRedeemSignature (By.RedeemSignature x) =
    encodeByteArray encodeByteStringBase16 x

encodeRedeemVerificationKey
    :: By.RedeemVerificationKey
    -> Json
encodeRedeemVerificationKey (By.RedeemVerificationKey x) =
    encodeByteArray encodeByteStringBase16 x

encodeSignature
    :: By.Signature any
    -> Json
encodeSignature (By.Signature x) =
    encodeByteStringBase16 (CC.unXSignature x)

encodeSlotNumber
    :: By.SlotNumber
    -> Json
encodeSlotNumber =
    encodeWord64 . By.unSlotNumber

encodeSoftforkRule
    :: By.SoftforkRule
    -> Json
encodeSoftforkRule x =
    "initThreshold" .=
        encodeLovelacePortion (By.srInitThd x) <>
    "minThreshold" .=
        encodeLovelacePortion (By.srMinThd x) <>
    "decrementThreshold" .=
        encodeLovelacePortion (By.srThdDecrement x)
    & encodeObject

encodeSoftwareVersion
    :: By.SoftwareVersion
    -> Json
encodeSoftwareVersion x =
    "appName" .=
        encodeApplicationName (By.svAppName x) <>
    "number" .=
        encodeWord32 (By.svNumber x)
    & encodeObject

encodeTxFeePolicy
    :: By.TxFeePolicy
    -> Json
encodeTxFeePolicy (By.TxFeePolicyTxSizeLinear (By.TxSizeLinear cst coeff)) =
    "constant" .=
        encodeLovelace cst <>
    "coefficient" .=
        encodeRational coeff
    & encodeObject

encodeTxInWitness
    :: By.TxInWitness
    -> Json
encodeTxInWitness = encodeObject . \case
    By.VKWitness key sig ->
        "key" .=
            encodeVerificationKey key <>
        "signature" .=
            encodeSignature sig
    By.RedeemWitness key sig ->
        "key" .=
            encodeRedeemVerificationKey key <>
        "signature" .=
            encodeRedeemSignature sig

encodeTxProof
    :: By.TxProof
    -> Json
encodeTxProof x =
    "number" .=
        encodeWord32 (By.txpNumber x) <>
    "root" .=
        encodeMerkleRoot (By.txpRoot x) <>
    "witnessesHash" .=
        encodeHash (By.txpWitnessesHash x)
    & encodeObject

encodeTxValidationError :: By.TxValidationError -> Json
encodeTxValidationError = \case
    By.TxValidationLovelaceError lbl e ->
        "lovelaceError" .=
            ( "label" .= encodeText lbl <>
              "error" .= encodeLovelaceError e
              & encodeObject
            )
        & encodeObject
    By.TxValidationFeeTooSmall _tx required actual ->
        "feeTooSmall" .=
          ( "requiredFee" .= encodeLovelace required <>
            "actualFee" .= encodeLovelace actual
            & encodeObject
          )
        & encodeObject
    By.TxValidationWitnessWrongSignature wit pm _ ->
        "wrongSignature" .=
            ( "witness" .= encodeTxInWitness wit <>
              "protocolMagic" .= encodeProtocolMagicId pm
              & encodeObject
            )
        & encodeObject
    By.TxValidationWitnessWrongKey wit addr ->
        "wrongKey" .=
            ( "witness" .= encodeTxInWitness wit <>
              "address" .= encodeAddress addr
              & encodeObject
            )
        & encodeObject
    By.TxValidationMissingInput txin ->
        "missingInput" .= encodeTxIn txin
        & encodeObject
    By.TxValidationNetworkMagicMismatch expected actual ->
        "networkMismatch" .=
            ( "expectedNetwork" .= encodeNetworkMagic expected <>
              "foundInAddress" .= encodeNetworkMagic actual
              & encodeObject
            )
        & encodeObject
    By.TxValidationTxTooLarge maxSize actualSize ->
        "txTooLarge" .=
            ( "maximumSize" .= encodeNatural maxSize <>
              "actualSize" .= encodeNatural actualSize
              & encodeObject
            )
        & encodeObject
    By.TxValidationUnknownAddressAttributes ->
        encodeText "unknownAddressAttributes"
    By.TxValidationUnknownAttributes ->
        encodeText "unknownAttributes"

encodeUTxOError
    :: By.UTxOError
    -> Json
encodeUTxOError = \case
    By.UTxOOverlappingUnion ->
        encodeText "overlappingUnion"
    By.UTxOMissingInput txin ->
        encodeObject ( "missingInput" .= encodeTxIn txin )

encodeUTxOValidationError
    :: By.UTxOValidationError
    -> Json
encodeUTxOValidationError = \case
    By.UTxOValidationTxValidationError e ->
        encodeObject ( "txValidationError" .= encodeTxValidationError e )
    By.UTxOValidationUTxOError e ->
        encodeObject ( "utxoValidationError" .= encodeUTxOError e )

encodeVerificationKey
    :: By.VerificationKey
    -> Json
encodeVerificationKey =
    encodeByteStringBase16 . CC.unXPub . By.unVerificationKey

stringifyAddress
    :: By.Address
    -> Text
stringifyAddress =
    decodeUtf8 . By.addrToBase58

stringifyKeyHash :: By.KeyHash -> Text
stringifyKeyHash =
    encodeBase16 . By.hashToBytes . By.unKeyHash

stringifyRedeemVerificationKey :: By.RedeemVerificationKey -> Text
stringifyRedeemVerificationKey (By.RedeemVerificationKey x) =
    encodeBase16 (By.fromVerificationKeyToByteString x)
