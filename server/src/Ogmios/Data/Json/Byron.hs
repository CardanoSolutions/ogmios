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
    :: SerializationMode
    -> By.ABlockOrBoundary ByteString
    -> Json
encodeABlockOrBoundary mode = \case
    By.ABOBBlock x -> encodeObject
        [ ( "body"
          , encodeABody mode (By.blockBody x)
          )
        , ( "hash"
          , encodeHash (By.blockHashAnnotated x)
          )
        , ( "header"
          , encodeAHeader mode (By.blockHeader x)
          )
        ]

    By.ABOBBoundary x -> encodeObject
        [ ( "hash"
          , encodeHash (By.boundaryHashAnnotated x)
          )
        , ( "header"
          , encodeABoundaryHeader (By.boundaryHeader x)
          )
        ]

encodeABlockSignature
    :: By.ABlockSignature any
    -> Json
encodeABlockSignature x = encodeObject
    [ ( "dlgCertificate"
      , encodeACertificate (By.delegationCertificate x)
      )
    , ( "signature"
      , encodeSignature (By.Block.signature x)
      )
    ]

encodeABody
    :: SerializationMode
    -> By.ABody any
    -> Json
encodeABody mode x = encodeObjectWithMode mode
    [ ( "txPayload"
      , encodeATxPayload mode (By.bodyTxPayload x)
      )
    , ( "updatePayload"
      , encodeAUpdPayload mode (By.bodyUpdatePayload x)
      )
    ]
    [ ( "dlgPayload"
      , encodeADlgPayload (By.bodyDlgPayload x)
      )
    ]

encodeABoundaryHeader
    :: By.ABoundaryHeader any
    -> Json
encodeABoundaryHeader x = encodeObject
    [ ( "prevHash"
      , either encodeGenesisHash encodeHash (By.boundaryPrevHash x)
      )
    , ( "epoch"
      , encodeWord64 (By.boundaryEpoch x)
      )
    , ( "blockHeight"
      , encodeChainDifficulty (By.boundaryDifficulty x)
      )
    ]

encodeACertificate
    :: By.ACertificate any
    -> Json
encodeACertificate x = encodeObject
    [ ( "epoch"
      , encodeAnnotated encodeEpochNumber (By.Dlg.aEpoch x)
      )
    , ( "issuerVk"
      , encodeVerificationKey (By.Dlg.issuerVK x)
      )
    , ( "delegateVk"
      , encodeVerificationKey (By.Dlg.delegateVK x)
      )
    , ( "signature"
      , encodeSignature (By.Dlg.signature x)
      )
    ]

encodeAHeader
    :: SerializationMode
    -> By.AHeader any
    -> Json
encodeAHeader mode x = encodeObjectWithMode mode
    [ ( "protocolMagicId"
      , encodeAnnotated encodeProtocolMagicId (By.aHeaderProtocolMagicId x)
      )
    , ( "prevHash"
      , encodeAnnotated encodeHash (By.aHeaderPrevHash x)
      )
    , ( "slot"
      , encodeAnnotated encodeSlotNumber (By.aHeaderSlot x)
      )
    , ( "blockHeight"
      , encodeAnnotated encodeChainDifficulty (By.aHeaderDifficulty x)
      )
    , ( "protocolVersion"
      , encodeProtocolVersion (By.headerProtocolVersion x)
      )
    , ( "softwareVersion"
      , encodeSoftwareVersion (By.headerSoftwareVersion x)
      )
    , ( "genesisKey"
      , encodeVerificationKey (By.headerGenesisKey x)
      )
    ]
    [ ( "proof"
      , encodeAnnotated encodeBlockProof (By.aHeaderProof x)
      )
    , ( "signature"
      , encodeABlockSignature (By.headerSignature x)
      )
    ]

encodeADlgPayload
    :: By.Dlg.APayload any
    -> Json
encodeADlgPayload =
    encodeList encodeACertificate . By.Dlg.getPayload

encodeTx
    :: By.Tx
    -> Json
encodeTx x = encodeObject
    [ ( "inputs"
      , encodeFoldable encodeTxIn (By.txInputs x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (By.txOutputs x)
      )
    ]

encodeTxIn
    :: By.TxIn
    -> Json
encodeTxIn (By.TxInUtxo txid ix) = encodeObject
    [ ( "txId"
      , encodeHash txid
      )
    , ( "index"
      , encodeWord16 ix
      )
    ]

encodeValue
    :: By.Lovelace
    -> Json
encodeValue coins = encodeObject
    [ ( "coins", encodeLovelace coins )
    ]

encodeTxOut
    :: By.TxOut
    -> Json
encodeTxOut x = encodeObject
    [ ( "address"
      , encodeAddress (By.txOutAddress x)
      )
    , ( "value"
      , encodeValue (By.txOutValue x)
      )
    ]

encodeATxAux
    :: SerializationMode
    -> By.ATxAux any
    -> Json
encodeATxAux mode x = encodeObjectWithMode mode
    [ ( "id"
      , encodeHash (By.serializeCborHash (By.taTx x))
      )
    , ( "body"
      , encodeAnnotated encodeTx (By.aTaTx x)
      )
    ]
    [ ( "witness"
      , encodeAnnotated encodeTxWitness (By.aTaWitness x)
      )
    , ( "raw"
      , encodeByteStringBase64 $ toStrictByteString $ mconcat
        [ encodeListLen 2
        , toCBOR (By.taTx x)
        , toCBOR (By.taWitness x)
        ]
      )
    ]

encodeATxPayload
    :: SerializationMode
    -> By.ATxPayload any
    -> Json
encodeATxPayload mode =
    encodeList (encodeATxAux mode) . By.aUnTxPayload

encodeAUpdPayload
    :: SerializationMode
    -> By.Upd.APayload any
    -> Json
encodeAUpdPayload mode x = encodeObjectWithMode mode
    [ ( "proposal"
      , encodeMaybe encodeAUpdProposal (By.Upd.payloadProposal x)
      )
    ]
    [ ( "votes"
      , encodeList encodeAVote (By.Upd.payloadVotes x)
      )
    ]

encodeAUpdProposal
    :: By.Upd.Proposal.AProposal any
    -> Json
encodeAUpdProposal x = encodeObject
    [ ( "body"
      , encodeAnnotated encodeAUpdProposalBody (By.Upd.Proposal.aBody x)
      )
    , ( "issuer"
      , encodeVerificationKey (By.Upd.Proposal.issuer x)
      )
    , ( "signature"
      , encodeSignature (By.Upd.Proposal.signature x)
      )
    ]

encodeAUpdProposalBody
    :: By.Upd.Proposal.ProposalBody
    -> Json
encodeAUpdProposalBody x = encodeObject
    [ ( "protocolVersion"
      , encodeProtocolVersion (By.Upd.Proposal.protocolVersion x)
      )
    , ( "parametersUpdate"
      , encodeProtocolParametersUpdate (By.Upd.Proposal.protocolParametersUpdate x)
      )
    , ( "softwareVersion"
      , encodeSoftwareVersion (By.Upd.Proposal.softwareVersion x)
      )
    , ( "metadata"
      , encodeMap By.Upd.getSystemTag encodeInstallerHash (By.Upd.Proposal.metadata x)
      )
    ]

encodeAVote
    :: By.Upd.Vote.AVote any
    -> Json
encodeAVote x = encodeObject
    [ ( "voterVk"
      , encodeVerificationKey (By.Upd.Vote.voterVK x)
      )
    , ( "proposalId"
      , encodeAnnotated encodeHash (By.Upd.Vote.aProposalId x)
      )
    , ( "signature"
      , encodeSignature (By.Upd.Vote.signature x)
      )
    ]

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
encodeBlockProof x = encodeObject
    [ ( "utxo"
      , encodeTxProof (By.proofUTxO x)
      )
    , ( "delegation"
      , encodeHash (By.proofDelegation x)
      )
    , ( "update"
      , encodeHash (By.proofUpdate x)
      )
    ]

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
    :: By.Hash a
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
        encodeObject [ ( "overflow", encodeWord64 x ) ]
    By.LovelaceTooLarge x ->
        encodeObject [ ( "tooLarge", encodeInteger x ) ]
    By.LovelaceTooSmall x ->
        encodeObject [ ( "tooSmall",  encodeInteger x ) ]
    By.LovelaceUnderflow x y ->
        encodeObject [ ( "underflow", encodeList encodeWord64 [x,y] ) ]

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
        encodeObject [ ( "testnet", encodeWord32 pm ) ]

encodeProtocolMagicId
    :: By.ProtocolMagicId
    -> Json
encodeProtocolMagicId =
    encodeWord32 . By.unProtocolMagicId

encodeProtocolParametersUpdate
    :: By.ProtocolParametersUpdate
    -> Json
encodeProtocolParametersUpdate x = encodeObject
    [ ( "scriptVersion"
      , encodeMaybe encodeWord16 (By.ppuScriptVersion x)
      )
    , ( "slotDuration"
      , encodeMaybe encodeNatural (By.ppuSlotDuration x)
      )
    , ( "maxBlockSize"
      , encodeMaybe encodeNatural (By.ppuMaxBlockSize x)
      )
    , ( "maxHeaderSize"
      , encodeMaybe encodeNatural (By.ppuMaxHeaderSize x)
      )
    , ( "maxTxSize"
      , encodeMaybe encodeNatural (By.ppuMaxTxSize x)
      )
    , ( "maxProposalSize"
      , encodeMaybe encodeNatural (By.ppuMaxProposalSize x)
      )
    , ( "mpcThreshold"
      , encodeMaybe encodeLovelacePortion (By.ppuMpcThd x)
      )
    , ( "heavyDlgThreshold"
      , encodeMaybe encodeLovelacePortion (By.ppuHeavyDelThd x)
      )
    , ( "updateVoteThreshold"
      , encodeMaybe encodeLovelacePortion (By.ppuUpdateVoteThd x)
      )
    , ( "updateProposalThreshold"
      , encodeMaybe encodeLovelacePortion (By.ppuUpdateProposalThd x)
      )
    , ( "updateProposalTimeToLive"
      , encodeMaybe encodeSlotNumber (By.ppuUpdateProposalTTL x)
      )
    , ( "softforkRule"
      , encodeMaybe encodeSoftforkRule (By.ppuSoftforkRule x)
      )
    , ( "txFeePolicy"
      , encodeMaybe encodeTxFeePolicy (By.ppuTxFeePolicy x)
      )
    , ( "unlockStakeEpoch"
      , encodeMaybe encodeEpochNumber (By.ppuUnlockStakeEpoch x)
      )
    ]

encodeProtocolVersion
    :: By.ProtocolVersion
    -> Json
encodeProtocolVersion x = encodeObject
    [ ( "major"
      , encodeWord16 (By.pvMajor x)
      )
    , ( "minor"
      , encodeWord16 (By.pvMinor x)
      )
    , ( "patch"
      , encodeWord8 (By.pvAlt x)
      )
    ]

encodeRedeemSignature
    :: By.RedeemSignature any
    -> Json
encodeRedeemSignature (By.RedeemSignature x) =
    encodeByteArray encodeByteStringBase64 x

encodeRedeemVerificationKey
    :: By.RedeemVerificationKey
    -> Json
encodeRedeemVerificationKey (By.RedeemVerificationKey x) =
    encodeByteArray encodeByteStringBase16 x

encodeSignature
    :: By.Signature any
    -> Json
encodeSignature (By.Signature x) =
    encodeByteStringBase64 (CC.unXSignature x)

encodeSlotNumber
    :: By.SlotNumber
    -> Json
encodeSlotNumber =
    encodeWord64 . By.unSlotNumber

encodeSoftforkRule
    :: By.SoftforkRule
    -> Json
encodeSoftforkRule x = encodeObject
    [ ( "initThreshold"
      , encodeLovelacePortion (By.srInitThd x)
      )
    , ( "minThreshold"
      , encodeLovelacePortion (By.srMinThd x)
      )
    , ( "decrementThreshold"
      , encodeLovelacePortion (By.srThdDecrement x)
      )
    ]

encodeSoftwareVersion
    :: By.SoftwareVersion
    -> Json
encodeSoftwareVersion x = encodeObject
    [ ( "appName"
      , encodeApplicationName (By.svAppName x)
      )
    , ( "number"
      , encodeWord32 (By.svNumber x)
      )
    ]

encodeTxFeePolicy
    :: By.TxFeePolicy
    -> Json
encodeTxFeePolicy (By.TxFeePolicyTxSizeLinear (By.TxSizeLinear cst coeff)) = encodeObject
    [ ( "constant"
      , encodeLovelace cst
      )
    , ( "coefficient"
      , encodeRational coeff
      )
    ]

encodeTxProof
    :: By.TxProof
    -> Json
encodeTxProof x = encodeObject
    [ ( "number"
      , encodeWord32 (By.txpNumber x)
      )
    , ( "root"
      , encodeMerkleRoot (By.txpRoot x)
      )
    , ( "witnessesHash"
      , encodeHash (By.txpWitnessesHash x)
      )
    ]

encodeTxWitness
    :: By.TxWitness
    -> Json
encodeTxWitness =
    encodeFoldable encodeTxInWitness

encodeTxInWitness
    :: By.TxInWitness
    -> Json
encodeTxInWitness = \case
    By.VKWitness key sig -> encodeObject
        [ ( "witnessVk", encodeObject
            [ ( "key", encodeVerificationKey key )
            , ( "signature", encodeSignature sig )
            ]
          )
        ]
    By.RedeemWitness key sig -> encodeObject
        [ ( "redeemWitness", encodeObject
            [ ( "key", encodeRedeemVerificationKey key )
            , ( "signature", encodeRedeemSignature sig )
            ]
          )
        ]

encodeTxValidationError :: By.TxValidationError -> Json
encodeTxValidationError = \case
    By.TxValidationLovelaceError lbl e -> encodeObject
        [ ( "lovelaceError", encodeObject
            [ ( "label", encodeText lbl )
            , ( "error", encodeLovelaceError e )
            ]
          )
        ]
    By.TxValidationFeeTooSmall _tx required actual -> encodeObject
        [ ( "feeTooSmall", encodeObject
            [ ( "requiredFee", encodeLovelace required )
            , ( "actualFee", encodeLovelace actual )
            ]
          )
        ]
    By.TxValidationWitnessWrongSignature wit pm _ -> encodeObject
        [ ( "wrongSignature", encodeObject
            [ ( "witness", encodeTxInWitness wit )
            , ( "protocolMagic", encodeProtocolMagicId pm )
            ]
          )
        ]
    By.TxValidationWitnessWrongKey wit addr -> encodeObject
        [ ( "wrongKey", encodeObject
            [ ( "witness", encodeTxInWitness wit )
            , ( "address", encodeAddress addr )
            ]
          )
        ]
    By.TxValidationMissingInput txin -> encodeObject
        [ ( "missingInput", encodeTxIn txin
          )
        ]
    By.TxValidationNetworkMagicMismatch expected actual -> encodeObject
        [ ( "networkMismatch", encodeObject
            [ ( "expectedNetwork", encodeNetworkMagic expected )
            , ( "foundInAddress", encodeNetworkMagic actual )
            ]
          )
        ]
    By.TxValidationTxTooLarge maxSize actualSize -> encodeObject
        [ ( "txTooLarge", encodeObject
            [ ( "maximumSize", encodeNatural maxSize )
            , ( "actualSize", encodeNatural actualSize )
            ]
          )
        ]
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
        encodeObject [ ( "missingInput", encodeTxIn txin ) ]

encodeUTxOValidationError
    :: By.UTxOValidationError
    -> Json
encodeUTxOValidationError = \case
    By.UTxOValidationTxValidationError e ->
        encodeObject [ ( "txValidationError", encodeTxValidationError e ) ]
    By.UTxOValidationUTxOError e ->
        encodeObject [ ( "utxoValidationError", encodeUTxOError e ) ]

encodeVerificationKey
    :: By.VerificationKey
    -> Json
encodeVerificationKey =
    encodeByteStringBase16 . CC.unXPub . By.unVerificationKey
