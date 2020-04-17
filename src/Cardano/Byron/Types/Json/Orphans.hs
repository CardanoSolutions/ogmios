--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Byron.Types.Json.Orphans () where

import Prelude

import Cardano.Binary
    ( Annotated (..), fromCBOR )
import Cardano.Chain.Block
    ( ABlock (..)
    , ABlockOrBoundary (..)
    , ABlockSignature (..)
    , ABody (..)
    , ABoundaryBlock (..)
    , ABoundaryHeader (..)
    , AHeader (..)
    , Proof (..)
    , blockHashAnnotated
    , boundaryHashAnnotated
    )
import Cardano.Chain.Byron.API
    ( ApplyMempoolPayloadErr (..) )
import Cardano.Chain.Common
    ( Address
    , ChainDifficulty (..)
    , Lovelace
    , LovelaceError (..)
    , LovelacePortion
    , MerkleRoot (..)
    , NetworkMagic (..)
    , TxFeePolicy (..)
    , TxSizeLinear (..)
    , addrToBase58
    , lovelacePortionToRational
    , lovelaceToInteger
    )
import Cardano.Chain.Genesis
    ( GenesisHash (..) )
import Cardano.Chain.MempoolPayload
    ( AMempoolPayload (..) )
import Cardano.Chain.Slotting
    ( EpochNumber (..), SlotNumber (..) )
import Cardano.Chain.Update
    ( ApplicationName (..)
    , InstallerHash (..)
    , ProposalBody (..)
    , ProtocolParametersUpdate (..)
    , ProtocolVersion (..)
    , SoftforkRule (..)
    , SoftwareVersion (..)
    , SystemTag (..)
    )
import Cardano.Chain.UTxO
    ( ATxAux (..)
    , ATxPayload (..)
    , Tx (..)
    , TxIn (..)
    , TxInWitness (..)
    , TxOut (..)
    , TxProof (..)
    , TxSigData (..)
    , UTxOError (..)
    , annotateTxAux
    , taTx
    )
import Cardano.Chain.UTxO.Validation
    ( TxValidationError (..), UTxOValidationError (..) )
import Cardano.Crypto.Hashing
    ( Hash, decodeHash, hashToBytes, serializeCborHash )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Cardano.Crypto.Signing
    ( RedeemSignature, RedeemVerificationKey, Signature, VerificationKey )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..), WithOrigin (..) )
import Control.Applicative
    ( empty, (<|>) )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), ToJSONKey (..), (.:), (.=) )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Text
    ( Text )
import Data.Vector
    ( Vector )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), ByronHash (..), GenTx, fromMempoolPayload )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint )
import Ouroboros.Network.Point
    ( Block (..) )

import qualified Cardano.Chain.Delegation as Dlg
import qualified Cardano.Chain.Update as Upd
import qualified Cardano.Chain.Update.Proposal as Upd.Proposal
import qualified Cardano.Chain.Update.Vote as Upd.Vote
import qualified Codec.CBOR.Read as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T

--
-- Required Instances
--

instance ToJSON ByronBlock where
    toJSON = toAltJSON . byronBlockRaw

instance ToJSON (Point ByronBlock) where
    toJSON (Point point) = case point of
        Origin -> Json.String "origin"
        At x   -> Json.object
            [ "slot" .= toAltJSON (blockPointSlot x)
            , "hash" .= toAltJSON (blockPointHash x)
            ]

instance ToJSON (Tip ByronBlock) where
    toJSON = \case
        TipGenesis -> Json.String "origin"
        Tip slot hash blockNo -> Json.object
            [ "slot" .= toAltJSON slot
            , "hash" .= toAltJSON hash
            , "blockNo" .= toAltJSON blockNo
            ]

instance ToJSON ApplyMempoolPayloadErr where
    toJSON = \case
        MempoolTxErr e -> toJSON e
        -- NOTE
        -- branches below aren't actually used because we only submit
        -- payment transaction through the protocol.
        MempoolDlgErr e -> toJSON (show e)
        MempoolUpdateProposalErr e -> toJSON (show e)
        MempoolUpdateVoteErr e -> toJSON (show e)

instance FromJSON (Point ByronBlock) where
    parseJSON json = parseOrigin json <|> parsePoint json
      where
        parsePoint  = Json.withObject "Point" $ \obj -> do
            slot <- obj .: "slot"
            hash <- obj .: "hash" >>= (either (const mempty) pure . decodeHash)
            pure $ Point $ At $ Block (SlotNo slot) (ByronHash hash)

        parseOrigin = Json.withText "Point" $ \case
            txt | txt == "origin" -> pure genesisPoint
            _ -> empty

instance FromJSON (GenTx ByronBlock) where
    parseJSON =
        (fromBase64 . BL.toStrict . Json.encode)
        >=>
        (deserialiseCBOR . BL.fromStrict)
      where
        toGenTx =
            fromMempoolPayload . MempoolTx . annotateTxAux . snd
        deserialiseCBOR =
            either (fail . show) (pure . toGenTx) . Cbor.deserialiseFromBytes fromCBOR
--
-- Collateral Instances
--

instance ToJSON UTxOValidationError where
    toJSON = \case
        UTxOValidationTxValidationError e ->
            Json.object [ "txValidationError" .= toJSON e ]
        UTxOValidationUTxOError e ->
            Json.object [ "utxoValidationError" .= toJSON e ]

instance ToJSON TxValidationError where
    toJSON = \case
        TxValidationLovelaceError lbl e ->
            Json.object
                [ "lovelaceError" .= Json.object
                    [ "label" .= toJSON lbl
                    , "error" .= toJSON e
                    ]
                ]
        TxValidationFeeTooSmall tx required actual ->
            Json.object
                [ "feeTooSmall" .= Json.object
                    [ "tx" .= toAltJSON tx
                    , "requiredFee" .= toAltJSON required
                    , "actualFee" .= toAltJSON actual
                    ]
                ]
        TxValidationWitnessWrongSignature wit pm _ ->
            Json.object
                [ "wrongSignature" .= Json.object
                    [ "witness" .= toAltJSON wit
                    , "protocolMagic" .= toAltJSON pm
                    ]
                ]
        TxValidationWitnessWrongKey wit addr ->
            Json.object
                [ "wrongKey" .= Json.object
                    [ "witness" .= toAltJSON wit
                    , "address" .= toAltJSON addr
                    ]
                ]
        TxValidationMissingInput txin ->
            Json.object
                [ "missingInput" .= toAltJSON txin
                ]
        TxValidationNetworkMagicMismatch expected actual ->
            Json.object
                [ "networkMagicMismatch" .= Json.object
                    [ "expectedMagic" .= toAltJSON expected
                    , "actualMagic" .= toAltJSON actual
                    ]
                ]
        TxValidationTxTooLarge maxSize actualSize ->
            Json.object
                [ "txTooLarge" .= Json.object
                    [ "maximumSize" .= toJSON maxSize
                    , "actualSize" .= toJSON actualSize
                    ]
                ]
        TxValidationUnknownAddressAttributes ->
            Json.String "unknownAddressAttributes"
        TxValidationUnknownAttributes ->
            Json.String "unknownAttributes"

instance ToJSON LovelaceError where
    toJSON = \case
        LovelaceOverflow x ->
            Json.object [ "overflow" .= toJSON x ]
        LovelaceTooLarge x ->
            Json.object [ "tooLarge" .= toJSON x ]
        LovelaceTooSmall x ->
            Json.object [ "tooSmall" .= toJSON x ]
        LovelaceUnderflow x y ->
            Json.object [ "underflow" .= toJSON [x,y] ]

instance ToJSON UTxOError where
    toJSON = \case
        UTxOOverlappingUnion ->
            Json.String "overlappingUnion"
        UTxOMissingInput txin ->
            Json.object [ "missingInput" .= toAltJSON txin ]

-- There are debug JSON instances for many types in cardano-ledger but those are
-- constructed generically and contains too many little discrepency which makes
-- them unsuitable for presenting to an end-user.
-- In particular:
--
--   - Many data-types have an 'ByteString' field which contains raw bytes data.
--     Although this might be useful for serializing back to CBOR, we may not
--     include this in ogmios _yet_ (perhaps under some particular flags).
--
--   - Fields are prefixed to avoid clashes and name collisions in Haskell and
--     as a result, creates awkward JSON values that sweats Haskell.
--
--  - Some data-types like 'AHeader' aren't quite consistent in the prefix they
--    use, so even generic options needs manual adjustment.
--
--   - Several sum-types have constructors with unnamed fields, which makes the
--     resulting JSON instances ugly and hard to inspect.
--
-- Therefore, instances below are 'hand-written', which is quite some
-- boilerplate but has also the nice benefits to make the end-result quite
-- obvious at first glance and is easier to control. Plus, it allows maintaining
-- a public interface without risk that it'll change because an underlying type
-- has changed in a future update.
class ToAltJSON a where
    toAltJSON :: a -> Json.Value

-- * Instances for primitive types

instance ToAltJSON ByteString where
    toAltJSON = toJSON . base16

instance ToAltJSON Text where
    toAltJSON = toJSON

instance ToAltJSON Natural where
    toAltJSON = toJSON

instance ToAltJSON Word8 where
    toAltJSON = toJSON

instance ToAltJSON Word16 where
    toAltJSON = toJSON

instance ToAltJSON Word32 where
    toAltJSON = toJSON

instance ToAltJSON Word64 where
    toAltJSON = toJSON

instance ToAltJSON a => ToAltJSON [a] where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (NonEmpty a) where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (Vector a) where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (Maybe a) where
    toAltJSON = toJSON . fmap toAltJSON

instance (ToJSONKey k, ToAltJSON a) => ToAltJSON (Map k a) where
    toAltJSON = toJSON . fmap toAltJSON

-- * Instances which piggy-back on 'ToJSON'

instance ToAltJSON ApplicationName where
    toAltJSON = toJSON . unApplicationName

instance ToAltJSON BlockNo where
    toAltJSON = toJSON . unBlockNo

instance ToAltJSON ByronHash where
    toAltJSON = toJSON . base16 . hashToBytes .unByronHash

instance ToAltJSON ChainDifficulty where
    toAltJSON = toJSON . unChainDifficulty

instance ToAltJSON EpochNumber where
    toAltJSON = toJSON . getEpochNumber

instance ToAltJSON Lovelace where
    toAltJSON = toJSON . lovelaceToInteger

instance ToAltJSON LovelacePortion where
    toAltJSON = toJSON . lovelacePortionToRational

instance ToAltJSON RedeemVerificationKey where
    toAltJSON = toJSON

instance ToAltJSON (Signature a) where
    toAltJSON = toJSON

instance ToAltJSON (RedeemSignature TxSigData) where
    toAltJSON = toJSON

instance ToAltJSON SlotNo where
    toAltJSON = toJSON . unSlotNo

instance ToAltJSON SlotNumber where
    toAltJSON = toJSON . unSlotNumber

instance ToAltJSON SystemTag where
    toAltJSON = toJSON . getSystemTag

instance ToAltJSON VerificationKey where
    toAltJSON = toJSON

-- * Unary types wrapping some types above

instance ToAltJSON Address where
    toAltJSON = toAltJSON . T.decodeUtf8 . addrToBase58

instance ToAltJSON b => ToAltJSON (Annotated b a) where
    toAltJSON = toAltJSON . unAnnotated

instance ToAltJSON (ATxPayload ByteString) where
    toAltJSON = toAltJSON . aUnTxPayload

instance ToAltJSON (Dlg.APayload ByteString) where
    toAltJSON = toAltJSON . Dlg.getPayload

instance ToAltJSON (Hash a) where
    toAltJSON = toAltJSON . hashToBytes

instance ToAltJSON GenesisHash where
    toAltJSON = toAltJSON . unGenesisHash

instance ToAltJSON InstallerHash where
    toAltJSON = toAltJSON . unInstallerHash

instance ToAltJSON (MerkleRoot a) where
    toAltJSON = toAltJSON . getMerkleRoot

instance ToAltJSON NetworkMagic where
    toAltJSON = \case
        NetworkMainOrStage ->
            Json.String "mainnet"
        NetworkTestnet pm ->
            Json.object [ "testnet" .= pm ]

instance ToAltJSON ProtocolMagicId where
    toAltJSON = toAltJSON . unProtocolMagicId

instance ToAltJSON TxSigData where
    toAltJSON = toAltJSON . txSigTxHash

-- -- * Product & Sum types

instance ToAltJSON (ABlockOrBoundary ByteString) where
    toAltJSON = \case
        ABOBBlock x -> Json.object
            [ "header" .= toAltJSON (blockHeader x)
            , "body" .= toAltJSON (blockBody x)
            , "hash" .= toAltJSON (blockHashAnnotated x)
            ]

        ABOBBoundary x -> Json.object
            [ "header" .= toAltJSON (boundaryHeader x)
            , "hash" .= toAltJSON (boundaryHashAnnotated x)
            ]

instance ToAltJSON (ABoundaryHeader ByteString) where
    toAltJSON x = Json.object
        [ "prevHash" .= either toAltJSON toAltJSON (boundaryPrevHash x)
        , "epoch" .= toAltJSON (boundaryEpoch x)
        , "blockHeight" .= toAltJSON (boundaryDifficulty x)
        ]

instance ToAltJSON (ABlockSignature ByteString) where
    toAltJSON x = Json.object
        [ "dlgCertificate" .= toAltJSON (delegationCertificate x)
        , "signature" .= toAltJSON (signature x)
        ]

instance ToAltJSON (AHeader ByteString) where
    toAltJSON x = Json.object
        [ "protocolMagicId" .= toAltJSON (aHeaderProtocolMagicId x)
        , "prevHash" .= toAltJSON (aHeaderPrevHash x)
        , "slot" .= toAltJSON (aHeaderSlot x)
        , "blockHeight" .= toAltJSON (aHeaderDifficulty x)
        , "protocolVersion" .= toAltJSON (headerProtocolVersion x)
        , "softwareVersion" .= toAltJSON (headerSoftwareVersion x)
        , "proof" .= toAltJSON (aHeaderProof x)
        , "genesisKey" .= toAltJSON (headerGenesisKey x)
        , "signature" .= toAltJSON (headerSignature x)
        ]

instance ToAltJSON Proof where
    toAltJSON x = Json.object
        [ "utxo" .= toAltJSON (proofUTxO x)
        , "delegation" .= toAltJSON (proofDelegation x)
        , "update" .= toAltJSON (proofUpdate x)
        ]
instance ToAltJSON SoftwareVersion where
    toAltJSON x = Json.object
        [ "appName" .= toAltJSON (svAppName x)
        , "number" .= toAltJSON (svNumber x)
        ]

instance ToAltJSON ProtocolVersion where
    toAltJSON x = Json.object
        [ "major" .= toAltJSON (pvMajor x)
        , "minor" .= toAltJSON (pvMinor x)
        , "patch" .= toAltJSON (pvAlt x)
        ]

instance ToAltJSON TxProof where
    toAltJSON x = Json.object
        [ "number" .= toAltJSON (txpNumber x)
        , "root" .= toAltJSON (txpRoot x)
        , "witnessesHash" .= toAltJSON (txpWitnessesHash x)
        ]

instance ToAltJSON (ABody ByteString) where
    toAltJSON x = Json.object
        [ "txPayload" .= toAltJSON (bodyTxPayload x)
        , "dlgPayload" .= toAltJSON (bodyDlgPayload x)
        , "updatePayload" .= toAltJSON (bodyUpdatePayload x)
        ]

instance ToAltJSON (Upd.APayload ByteString) where
    toAltJSON x = Json.object
        [ "proposal" .= toAltJSON (Upd.payloadProposal x)
        , "votes" .= toAltJSON (Upd.payloadVotes x)
        ]

instance ToAltJSON (Upd.AVote ByteString) where
    toAltJSON x = Json.object
        [ "voterVK" .= toAltJSON (Upd.voterVK x)
        , "proposalId" .= toAltJSON (Upd.aProposalId x)
        , "signature" .= toAltJSON (Upd.Vote.signature x)
        ]

instance ToAltJSON (Upd.AProposal ByteString) where
    toAltJSON x = Json.object
        [ "body" .= toAltJSON (Upd.aBody x)
        , "issuer" .= toAltJSON (Upd.issuer x)
        , "signature" .= toAltJSON (Upd.Proposal.signature x)
        ]

instance ToAltJSON ProposalBody where
    toAltJSON x = Json.object
        [ "protocolVersion" .= toAltJSON (protocolVersion x)
        , "parametersUpdate" .= toAltJSON (protocolParametersUpdate x)
        , "softwareVersion" .= toAltJSON (softwareVersion x)
        , "metadata" .= toAltJSON (Map.mapKeys getSystemTag $ metadata x)
        ]

instance ToAltJSON ProtocolParametersUpdate where
    toAltJSON x = Json.object
        [ "scriptVersion" .= toAltJSON (ppuScriptVersion x)
        , "slotDuration" .= toAltJSON (ppuSlotDuration x)
        , "maxBlockSize" .= toAltJSON (ppuMaxBlockSize x)
        , "maxHeaderSize" .= toAltJSON (ppuMaxHeaderSize x)
        , "maxTxSize" .= toAltJSON (ppuMaxTxSize x)
        , "maxProposalSize" .= toAltJSON (ppuMaxProposalSize x)
        , "mpcThreshold"  .= toAltJSON (ppuMpcThd x)
        , "heavyDlgThreshold" .= toAltJSON (ppuHeavyDelThd x)
        , "updateVoteThreshold" .= toAltJSON (ppuUpdateVoteThd x)
        , "updateProposalTheshold" .= toAltJSON (ppuUpdateProposalThd x)
        , "updateProposalTTL" .= toAltJSON (ppuUpdateProposalTTL x)
        , "softforkRule" .= toAltJSON (ppuSoftforkRule x)
        , "txFeePolicy" .= toAltJSON (ppuTxFeePolicy x)
        , "unlockStakeEpoch" .= toAltJSON (ppuUnlockStakeEpoch x)
        ]

instance ToAltJSON TxFeePolicy where
    toAltJSON (TxFeePolicyTxSizeLinear (TxSizeLinear cst coeff)) = Json.object
        [ "constant" .= toAltJSON cst
        , "coefficient" .= toJSON (fromRational coeff :: Double)
        ]

instance ToAltJSON SoftforkRule where
    toAltJSON x = Json.object
        [ "initThreshold" .= toAltJSON (srInitThd x)
        , "minThreshold" .= toAltJSON (srMinThd x)
        , "decrementThreshold" .= toAltJSON (srThdDecrement x)
        ]

instance ToAltJSON (Dlg.ACertificate ByteString) where
    toAltJSON x = Json.object
        [ "epoch" .= toAltJSON (Dlg.aEpoch x)
        , "issuerVK" .= toAltJSON (Dlg.issuerVK x)
        , "delegateVK" .= toAltJSON (Dlg.delegateVK x)
        , "signature" .= toAltJSON (Dlg.signature x)
        ]

instance ToAltJSON (ATxAux ByteString) where
    toAltJSON x = Json.object
        [ "id" .= toAltJSON (serializeCborHash (taTx x))
        , "body" .= toAltJSON (aTaTx x)
        , "witness" .= toAltJSON (aTaWitness x)
        ]

instance ToAltJSON Tx where
    toAltJSON x = Json.object
        [ "inputs" .= toAltJSON (txInputs x)
        , "outputs" .= toAltJSON (txOutputs x)
        ]

instance ToAltJSON TxInWitness where
    toAltJSON = \case
        VKWitness key sig ->
            Json.object
                [ "witnessVK" .= Json.object
                    [ "key" .= toAltJSON key
                    , "signature" .= toAltJSON sig
                    ]
                ]
        RedeemWitness key sig ->
            Json.object
                [ "redeemWitness" .= Json.object
                    [ "key" .= toAltJSON key
                    , "signature" .= toAltJSON sig
                    ]
                ]

instance ToAltJSON TxOut where
    toAltJSON x = Json.object
        [ "address" .= toAltJSON (txOutAddress x)
        , "value" .= toAltJSON (txOutValue x)
        ]

instance ToAltJSON TxIn where
    toAltJSON (TxInUtxo txid ix) = Json.object
        [ "txId" .= toAltJSON txid
        , "index" .= toAltJSON ix
        ]

--
-- Internal / Helpers
--

base16 :: ByteArrayAccess bin => bin -> Text
base16 = T.decodeUtf8 . convertToBase Base16

fromBase64 :: ByteString -> Json.Parser ByteString
fromBase64 = either (fail . show) pure . convertFromBase Base64
