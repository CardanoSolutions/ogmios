--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Types.Json.Orphans
    ( QueryResult
    , parseGetLedgerTip
    , parseGetEpochNo
    , parseGetNonMyopicMemberRewards
    , parseGetCurrentPParams
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    , parseGetFilteredUTxO
    ) where

import Prelude

import Cardano.Api.MetaData
    ( TxMetadataJsonSchema (..), TxMetadataValue (..), metadataToJson )
import Cardano.Api.Typed
    ( TxMetadata, makeTransactionMetadata )
import Cardano.Binary
    ( Annotated (..), FromCBOR (..), serialize )
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
    ( ChainDifficulty (..)
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
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Shelley
    ( Shelley )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( EpochNo (..), SlotNo (..), WithOrigin (..) )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Applicative
    ( Alternative, empty, (<|>) )
import Control.Arrow
    ( right )
import Control.Exception
    ( PatternMatchFail (..), throw )
import Control.Monad
    ( guard, (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), ToJSONKey (..), (.:), (.=) )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray
    ( ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.ByteString.Short
    ( fromShort, toShort )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( asum, toList )
import Data.Functor
    ( ($>) )
import Data.Functor.Contravariant
    ( contramap )
import Data.Functor.Identity
    ( Identity )
import Data.IP
    ( IPv4, IPv6 )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.Ratio
    ( Rational )
import Data.Sequence.Strict
    ( StrictSeq )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Vector
    ( Vector )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Numeric.Natural
    ( Natural )
import Ogmios.Bridge
    ( SomeQuery (..) )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..), ByronHash (..), GenTx )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , CardanoQueryResult
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , Query (..)
    , ShelleyEra
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( LedgerEraInfo (..)
    , MismatchEraInfo (..)
    , OneEraHash (..)
    , SingleEraInfo (..)
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..) )
import Ouroboros.Consensus.HardFork.Combinator.Util.Match
    ( mismatchTwo )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Ledger
    ( NonMyopicMemberRewards (..), Query (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )
import Shelley.Spec.Ledger.API
    ( ApplyTxError (..) )
import Shelley.Spec.Ledger.STS.Ppup
    ( VotingPeriod (..) )

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Delegation as Dlg
import qualified Cardano.Chain.Update as Upd
import qualified Cardano.Chain.Update.Proposal as Upd.Proposal
import qualified Cardano.Chain.Update.Vote as Upd.Vote
import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Read as Cbor
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encoding.Internal as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Ouroboros.Network.Point as Point
import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.Address.Bootstrap as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Credential as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.MetaData as SL
import qualified Shelley.Spec.Ledger.OCert as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Scripts as SL
import qualified Shelley.Spec.Ledger.STS.Deleg as Deleg
import qualified Shelley.Spec.Ledger.STS.Delegs as Delegs
import qualified Shelley.Spec.Ledger.STS.Delpl as Delpl
import qualified Shelley.Spec.Ledger.STS.Ledger as Ledger
import qualified Shelley.Spec.Ledger.STS.Ledgers as Ledgers
import qualified Shelley.Spec.Ledger.STS.Pool as Pool
import qualified Shelley.Spec.Ledger.STS.Ppup as Ppup
import qualified Shelley.Spec.Ledger.STS.Utxo as Utxo
import qualified Shelley.Spec.Ledger.STS.Utxow as Utxow
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxBody as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

--
-- Required Instances
--

instance Crypto crypto => ToJSON (CardanoBlock crypto) where
    toJSON = \case
        BlockByron blk->
            Json.object [ "byron"   .= toAltJSON (byronBlockRaw blk) ]
        BlockShelley blk ->
            Json.object [ "shelley" .= toAltJSON blk ]

instance Crypto crypto => ToJSON (Tip (CardanoBlock crypto)) where
    toJSON = \case
        TipGenesis -> Json.String "origin"
        Tip slot hash blockNo -> Json.object
            [ "slot" .= toAltJSON slot
            , "hash" .= toAltJSON hash
            , "blockNo" .= toAltJSON blockNo
            ]

instance Crypto crypto => ToJSON (Point (CardanoBlock crypto)) where
    toJSON (Point point) = case point of
        Origin -> Json.String "origin"
        At x   -> Json.object
            [ "slot" .= toAltJSON (Point.blockPointSlot x)
            , "hash" .= toAltJSON (Point.blockPointHash x)
            ]

instance Crypto crypto => ToJSON (HardForkApplyTxErr (CardanoEras crypto)) where
    toJSON err = case err of
        ApplyTxErrByron e ->
            toAltJSON e
        ApplyTxErrShelley e ->
            toAltJSON e
        ApplyTxErrWrongEra e ->
            toAltJSON e

instance ToJSON AcquireFailure where
    toJSON = \case
        AcquireFailurePointTooOld ->
            Json.String "pointTooOld"
        AcquireFailurePointNotOnChain ->
            Json.String "pointNotOnChain"

instance Crypto crypto => FromJSON (Point (CardanoBlock crypto)) where
    parseJSON json = parseOrigin json <|> parsePoint json
      where
        parsePoint = Json.withObject "Point" $ \obj -> do
            slot <- obj .: "slot"
            hash <- obj .: "hash" >>= (either (const mempty) pure . decodeHash')
            pure $ Point $ At $ Point.Block (SlotNo slot) hash

        parseOrigin = Json.withText "Point" $ \case
            txt | txt == "origin" -> pure genesisPoint
            _ -> empty

        decodeHash' =
            fmap (OneEraHash . toShort . hashToBytes) . decodeHash

instance Crypto crypto => FromJSON (GenTx (CardanoBlock crypto))
  where
    parseJSON =
        (fromBase64 . BL.toStrict . Json.encode)
        >=>
        (deserialiseCBOR . BL.fromStrict)
      where
        deserialiseCBOR = either (fail . show) (pure . GenTxShelley . snd)
            . Cbor.deserialiseFromBytes fromCBOR

instance Crypto crypto => FromJSON (SomeQuery Maybe (CardanoBlock crypto)) where
    parseJSON = choice
        [ parseGetLedgerTip (const Nothing)
        , parseGetEpochNo (const Nothing)
        , parseGetNonMyopicMemberRewards (const Nothing)
        , parseGetCurrentPParams (const Nothing)
        , parseGetProposedPParamsUpdates (const Nothing)
        , parseGetStakeDistribution (const Nothing)
        , parseGetUTxO (const Nothing)
        , parseGetFilteredUTxO (const Nothing)
        ]

instance (Crypto crypto, ToAltJSON a) => ToAltJSON (CardanoQueryResult crypto a) where
    toAltJSON = \case
        Left  e -> toAltJSON e
        Right a -> toAltJSON a

instance (Crypto crypto) => ToAltJSON (MismatchEraInfo (CardanoEras crypto)) where
    toAltJSON (MismatchEraInfo mismatch) = case mismatchTwo mismatch of
        Left (SingleEraInfo query, LedgerEraInfo (SingleEraInfo ledger)) ->
            toAltJSON $ EraMismatch ledger query
        Right (SingleEraInfo query, LedgerEraInfo (SingleEraInfo ledger)) ->
            toAltJSON $ EraMismatch ledger query

instance ToAltJSON EraMismatch where
    toAltJSON x = Json.object
        [ "eraMismatch" .= Json.object
            [ "ledgerEra" .= toAltJSON (ledgerEraName x)
            , "queryEra" .= toAltJSON (otherEraName x)
            ]
        ]

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

class FromAltJSON a where
    fromAltJSON :: Json.Value -> Json.Parser a

-- * Instances for primitive types

instance ToAltJSON Char where
    toAltJSON = toJSON

instance ToAltJSON Text where
    toAltJSON = toJSON

instance ToAltJSON ByteString where
    toAltJSON = toJSON . base16

instance ToAltJSON BL.ByteString where
    toAltJSON = toAltJSON . BL.toStrict

instance ToAltJSON Natural where
    toAltJSON = toJSON

instance ToAltJSON Word where
    toAltJSON = toJSON

instance ToAltJSON Word8 where
    toAltJSON = toJSON

instance ToAltJSON Word16 where
    toAltJSON = toJSON

instance ToAltJSON Word32 where
    toAltJSON = toJSON

instance ToAltJSON Word64 where
    toAltJSON = toJSON

instance ToAltJSON Integer where
    toAltJSON = toJSON

instance ToAltJSON Rational where
    toAltJSON = toJSON . fromRational @Double

instance ToAltJSON IPv4 where
    toAltJSON = toJSON

instance ToAltJSON IPv6 where
    toAltJSON = toJSON

instance (ToAltJSON a, ToAltJSON b) => ToAltJSON (a, b) where
    toAltJSON (a,b) = toJSON (toAltJSON a, toAltJSON b)

instance ToAltJSON a => ToAltJSON [a] where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (NonEmpty a) where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (Vector a) where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (Set a) where
    toAltJSON = toAltJSON . Set.toList

instance ToAltJSON a => ToAltJSON (StrictSeq a) where
    toAltJSON = toAltJSON . toList

instance (ToJSONKey k, ToAltJSON a) => ToAltJSON (Map k a) where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (Maybe a) where
    toAltJSON = toJSON . fmap toAltJSON

instance ToAltJSON a => ToAltJSON (SL.StrictMaybe a) where
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

instance Crypto crypto => ToAltJSON (Point (CardanoBlock crypto)) where
    toAltJSON = toJSON

-- * Unary types wrapping some types above

instance ToAltJSON SL.ChainCode where
    toAltJSON cc =
        if BS.null (SL.unChainCode cc)
        then Json.Null
        else toAltJSON (SL.unChainCode cc)

instance ToAltJSON SL.DnsName where
    toAltJSON = toAltJSON . SL.dnsToText

instance ToAltJSON SL.Port where
    toAltJSON = toAltJSON . SL.portToWord16

instance ToAltJSON SL.Url where
    toAltJSON = toAltJSON . SL.urlToText

instance ToAltJSON SL.UnitInterval where
    toAltJSON = Json.Number . fromRational . SL.unitIntervalToRational

instance ToAltJSON Byron.Address where
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

instance Crypto crypto => ToAltJSON (SL.HashHeader (Shelley crypto)) where
    toAltJSON = toAltJSON . CC.hashToBytes . SL.unHashHeader

instance ToAltJSON (MerkleRoot a) where
    toAltJSON = toAltJSON . getMerkleRoot

instance ToAltJSON (OneEraHash eras) where
    toAltJSON = toAltJSON . fromShort . getOneEraHash

instance ToAltJSON ProtocolMagicId where
    toAltJSON = toAltJSON . unProtocolMagicId

instance Crypto crypto => ToAltJSON (ShelleyHash (Shelley crypto)) where
    toAltJSON (ShelleyHash h) = toAltJSON h

instance ToAltJSON TxSigData where
    toAltJSON = toAltJSON . txSigTxHash

instance Crypto crypto => ToAltJSON (SL.VKey any (Shelley crypto)) where
    toAltJSON = toAltJSON . SL.unVKey

instance ToAltJSON (CC.Hash alg a) where
    toAltJSON (CC.UnsafeHash h) = toAltJSON (fromShort h)

instance CC.HashAlgorithm alg => FromAltJSON (CC.Hash alg a) where
    fromAltJSON =
        parseJSON >=> maybe empty pure . CC.hashFromTextAsHex

instance CC.DSIGNAlgorithm alg => ToAltJSON (CC.VerKeyDSIGN alg) where
    toAltJSON = toAltJSON . CC.rawSerialiseVerKeyDSIGN

instance CC.DSIGNAlgorithm alg => ToAltJSON (CC.SignedDSIGN alg a) where
    toAltJSON (CC.SignedDSIGN raw) = toAltJSON . CC.rawSerialiseSigDSIGN $ raw

instance CC.VRFAlgorithm alg => ToAltJSON (CC.VerKeyVRF alg) where
    toAltJSON = toAltJSON . CC.rawSerialiseVerKeyVRF

instance CC.KESAlgorithm alg => ToAltJSON (CC.VerKeyKES alg) where
    toAltJSON = toAltJSON . CC.rawSerialiseVerKeyKES

instance CC.KESAlgorithm alg => ToAltJSON (CC.SignedKES alg a) where
    toAltJSON (CC.SignedKES raw) = toAltJSON . base64 . CC.rawSerialiseSigKES $ raw

instance ToAltJSON (CC.OutputVRF alg) where
    toAltJSON = toAltJSON . CC.getOutputVRFBytes

instance CC.VRFAlgorithm alg => ToAltJSON (CC.CertVRF alg) where
    toAltJSON = toAltJSON . CC.rawSerialiseCertVRF

instance ToAltJSON SL.KESPeriod where
    toAltJSON = toAltJSON . SL.unKESPeriod

instance Crypto crypto => ToAltJSON (SL.TxSeq (Shelley crypto)) where
    toAltJSON = toAltJSON . toList . SL.txSeqTxns'

instance Crypto crypto => ToAltJSON (SL.TxId (Shelley crypto)) where
    toAltJSON (SL.TxId h) = toAltJSON h

instance ToAltJSON SL.Coin where
    toAltJSON = toAltJSON . SL.unCoin

instance FromAltJSON SL.Coin where
    fromAltJSON = fmap SL.word64ToCoin . parseJSON

coinToText :: SL.Coin -> Text
coinToText = T.pack . show . SL.unCoin

instance Crypto crypto => ToAltJSON (SL.KeyHash any (Shelley crypto)) where
    toAltJSON (SL.KeyHash hash) = toAltJSON hash

instance {-# OVERLAPS #-} Crypto crypto =>
    ToAltJSON (SL.KeyHash 'SL.StakePool (Shelley crypto))
  where
    toAltJSON (SL.KeyHash (CC.UnsafeHash h)) = toAltJSON . bech32 hrp . fromShort $ h
      where
        hrp = [humanReadablePart|pool|]

instance {-# OVERLAPS #-} Crypto crypto =>
    ToJSONKey (SL.KeyHash 'SL.StakePool (Shelley crypto))
  where
    toJSONKey = Json.ToJSONKeyText
        (unsafeMatchString . toAltJSON)
        (Json.text . unsafeMatchString . toAltJSON)

instance Crypto crypto => ToAltJSON (SL.MetaDataHash (Shelley crypto)) where
    toAltJSON (SL.MetaDataHash hash) = toAltJSON hash

instance Crypto crypto => ToAltJSON (SL.ScriptHash (Shelley crypto)) where
    toAltJSON (SL.ScriptHash hash) = toAltJSON hash

instance Crypto crypto => ToJSONKey (SL.ScriptHash (Shelley crypto)) where
    toJSONKey = contramap scriptHashToText toJSONKey

scriptHashToText
    :: Crypto crypto
    => SL.ScriptHash (Shelley crypto)
    -> Text
scriptHashToText =
    unsafeMatchString . toAltJSON

instance Crypto crypto => ToAltJSON (SL.WitHashes (Shelley crypto)) where
    toAltJSON (SL.WitHashes hash) = toAltJSON hash

instance ToAltJSON EpochNo where
    toAltJSON (EpochNo ep) = toAltJSON ep

instance Crypto crypto => ToAltJSON (SL.Wdrl (Shelley crypto)) where
    toAltJSON = toAltJSON . Map.mapKeys (unsafeMatchString . toAltJSON) . SL.unWdrl

instance Crypto crypto => ToAltJSON (SL.ProposedPPUpdates (Shelley crypto)) where
    toAltJSON (SL.ProposedPPUpdates m) = toAltJSON m

instance Crypto crypto => ToAltJSON (SL.HashBBody (Shelley crypto)) where
    toAltJSON = toAltJSON . BL.drop cborOverhead . serialize
      where
        -- TODO: The constructor of HashBBody isn't exposed.
        -- So, we abuse the cbor instance to get the raw bytes, but need to drop
        -- the '58XX' prefix in front of the payload.
        cborOverhead = 2

-- * Product & Sum types

instance ToAltJSON NetworkMagic where
    toAltJSON = \case
        NetworkMainOrStage ->
            Json.String "mainnet"
        NetworkTestnet pm ->
            Json.object [ "testnet" .= pm ]

instance ToAltJSON VotingPeriod where
    toAltJSON = \case
        VoteForThisEpoch ->
            Json.String "voteForThisEpoch"
        VoteForNextEpoch ->
            Json.String "voteForNextEpoch"

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

instance ToAltJSON (Dlg.ACertificate ByteString) where
    toAltJSON x = Json.object
        [ "epoch" .= toAltJSON (Dlg.aEpoch x)
        , "issuerVk" .= toAltJSON (Dlg.issuerVK x)
        , "delegateVk" .= toAltJSON (Dlg.delegateVK x)
        , "signature" .= toAltJSON (Dlg.signature x)
        ]

instance ToAltJSON Proof where
    toAltJSON x = Json.object
        [ "utxo" .= toAltJSON (proofUTxO x)
        , "delegation" .= toAltJSON (proofDelegation x)
        , "update" .= toAltJSON (proofUpdate x)
        ]

instance ToAltJSON ProtocolVersion where
    toAltJSON x = Json.object
        [ "major" .= toAltJSON (pvMajor x)
        , "minor" .= toAltJSON (pvMinor x)
        , "patch" .= toAltJSON (pvAlt x)
        ]

instance ToAltJSON SL.ProtVer where
    toAltJSON (SL.ProtVer major minor) = Json.object
        [ "major" .= toAltJSON major
        , "minor" .= toAltJSON minor
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
        [ "voterVk" .= toAltJSON (Upd.voterVK x)
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
        , "updateProposalTimeToLive" .= toAltJSON (ppuUpdateProposalTTL x)
        , "softforkRule" .= toAltJSON (ppuSoftforkRule x)
        , "txFeePolicy" .= toAltJSON (ppuTxFeePolicy x)
        , "unlockStakeEpoch" .= toAltJSON (ppuUnlockStakeEpoch x)
        ]

instance ToAltJSON SoftforkRule where
    toAltJSON x = Json.object
        [ "initThreshold" .= toAltJSON (srInitThd x)
        , "minThreshold" .= toAltJSON (srMinThd x)
        , "decrementThreshold" .= toAltJSON (srThdDecrement x)
        ]

instance ToAltJSON SoftwareVersion where
    toAltJSON x = Json.object
        [ "appName" .= toAltJSON (svAppName x)
        , "number" .= toAltJSON (svNumber x)
        ]

instance ToAltJSON TxFeePolicy where
    toAltJSON (TxFeePolicyTxSizeLinear (TxSizeLinear cst coeff)) = Json.object
        [ "constant" .= toAltJSON cst
        , "coefficient" .= toJSON (fromRational coeff :: Double)
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
                [ "witnessVk" .= Json.object
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

instance Crypto crypto => ToAltJSON (SL.PrevHash (Shelley crypto)) where
    toAltJSON = \case
        SL.GenesisHash -> toJSON ("genesis" :: String)
        SL.BlockHash h -> toAltJSON h

instance CC.VRFAlgorithm alg => ToAltJSON (CC.CertifiedVRF alg any) where
    toAltJSON x = Json.object
        [ "output" .= toAltJSON (CC.certifiedOutput x)
        , "proof" .= toAltJSON (CC.certifiedProof x)
        ]

instance Crypto crypto => ToAltJSON (SL.OCert (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "hotVk" .= toAltJSON (SL.ocertVkHot x)
        , "count" .= toAltJSON (SL.ocertN x)
        , "kesPeriod" .= toAltJSON (SL.ocertKESPeriod x)
        , "sigma" .= toAltJSON (SL.ocertSigma x)
        ]

instance Crypto crypto => ToAltJSON (ShelleyBlock (Shelley crypto)) where
    toAltJSON (ShelleyBlock (SL.Block (SL.BHeader hBody hSig) txs) hHash) =
        Json.object
            [ "body" .= toAltJSON txs
            , "header" .= Json.object
                [ "blockHeight" .= toAltJSON (SL.bheaderBlockNo hBody)
                , "slot" .= toAltJSON (SL.bheaderSlotNo hBody)
                , "prevHash" .= toAltJSON (SL.bheaderPrev hBody)
                , "issuerVk" .=  toAltJSON (SL.bheaderVk hBody)
                , "issuerVrf" .= toAltJSON (SL.bheaderVrfVk hBody)
                , "nonce" .= toAltJSON (SL.bheaderEta hBody)
                , "leaderValue" .=  toAltJSON (SL.bheaderL hBody)
                , "blockSize" .= toAltJSON (SL.bsize hBody)
                , "blockHash" .= toAltJSON (SL.bhash hBody)
                , "opCert" .= toAltJSON (SL.bheaderOCert hBody)
                , "protocolVersion" .= toAltJSON (SL.bprotver hBody)
                , "signature" .= toAltJSON hSig
                ]
            , "headerHash" .= toAltJSON hHash
            ]

instance Crypto crypto => ToAltJSON (SL.Tx (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "id" .= toAltJSON (SL.txid (SL._body x))
        , "body" .= toAltJSON (SL._body x)
        , "witness" .= toAltJSON (SL._witnessSet x)
        , "metadata" .= Json.object
            [ "hash" .= toAltJSON (SL._mdHash (SL._body x))
            , "body" .= toAltJSON (SL._metadata x)
            ]
        ]

instance ToAltJSON SL.MetaData where
    toAltJSON = metadataToJson TxMetadataJsonDetailedSchema . fromShelleyMetadata
      where
        fromShelleyMetadata :: SL.MetaData -> TxMetadata
        fromShelleyMetadata (SL.MetaData meta) =
            makeTransactionMetadata (convert <$> meta)
          where
            convert :: SL.MetaDatum -> TxMetadataValue
            convert = \case
                SL.I n -> TxMetaNumber n
                SL.B bs -> TxMetaBytes bs
                SL.S s -> TxMetaText s
                SL.Map as -> TxMetaMap (bimap convert convert <$> as)
                SL.List xs -> TxMetaList (map convert xs)

instance Crypto crypto => ToAltJSON (SL.WitnessSet (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "address" .= toAltJSON (SL.addrWits x)
        , "multisig" .= toAltJSON (SL.msigWits x)
        , "bootstrap" .= toAltJSON (SL.bootWits x)
        ]

instance Crypto crypto => ToAltJSON (SL.TxBody (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "inputs" .= toAltJSON (SL._inputs x)
        , "outputs" .= toAltJSON (SL._outputs x)
        , "certificates" .= toAltJSON (SL._certs x)
        , "withdrawals" .= toAltJSON (SL._wdrls x)
        , "fee" .= toAltJSON (SL._txfee x)
        , "timeToLive" .= toAltJSON (SL._ttl x)
        , "update" .= toAltJSON (SL._txUpdate x)
        ]

instance Crypto crypto => ToAltJSON (SL.DCert (Shelley crypto)) where
    toAltJSON = \case
        SL.DCertDeleg (SL.RegKey credential) ->
            Json.object
                [ "stakeKeyRegistration" .= toAltJSON credential
                ]
        SL.DCertDeleg (SL.DeRegKey credential) ->
            Json.object
                [ "stakeKeyDeregistration" .= toAltJSON credential
                ]
        SL.DCertDeleg (SL.Delegate delegation) ->
            Json.object
                [ "stakeDelegation" .= toAltJSON delegation
                ]
        SL.DCertPool (SL.RegPool params)  ->
            Json.object
                [ "poolRegistration" .= toAltJSON params
                ]
        SL.DCertPool (SL.RetirePool keyHash epochNo) ->
            Json.object
                [ "poolRetirement" .= Json.object
                    [ "poolId" .= toAltJSON keyHash
                    , "retirementEpoch" .= toAltJSON epochNo
                    ]
                ]
        SL.DCertGenesis (SL.GenesisDelegCert key delegate vrf)  ->
            Json.object
                [ "genesisDelegation" .= Json.object
                    [ "verificationKeyHash" .= toAltJSON key
                    , "delegateKeyHash" .= toAltJSON delegate
                    , "vrfVerificationKeyHash" .= toAltJSON vrf
                    ]
                ]
        SL.DCertMir (SL.MIRCert pot rewards) ->
            Json.object
                [ "moveInstantaneousRewards" .= Json.object
                    [ "pot" .= toAltJSON pot
                    , "rewards" .= toAltJSON rewards
                    ]
                ]

instance Crypto crypto => ToAltJSON (SL.Delegation (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "delegator" .= toAltJSON (SL._delegator x)
        , "delegatee" .= toAltJSON (SL._delegatee x)
        ]

instance Crypto crypto => ToAltJSON (SL.PoolParams (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "id" .= toAltJSON (SL._poolPubKey x)
        , "vrf" .= toAltJSON (SL._poolVrf x)
        , "pledge" .= toAltJSON (SL._poolPledge x)
        , "cost" .= toAltJSON (SL._poolCost x)
        , "margin" .= toAltJSON (SL._poolMargin x)
        , "rewardAccount" .= toAltJSON (SL._poolRAcnt x)
        , "owners" .= toAltJSON (SL._poolOwners x)
        , "relays" .= toAltJSON (SL._poolRelays x)
        , "metadata" .= toAltJSON (SL._poolMD x)
        ]

instance ToAltJSON SL.PoolMetaData where
    toAltJSON x = Json.object
        [ "url" .= toAltJSON (SL._poolMDUrl x)
        , "hash" .= toAltJSON (SL._poolMDHash x)
        ]

instance ToAltJSON SL.StakePoolRelay where
    toAltJSON = \case
        SL.SingleHostAddr port ipv4 ipv6 ->
            Json.object
                [ "port" .= toAltJSON port
                , "ipv4" .= toAltJSON ipv4
                , "ipv6" .= toAltJSON ipv6
                ]
        SL.SingleHostName port dns ->
            Json.object
                [ "hostname" .= toAltJSON dns
                , "port" .= toAltJSON port
                ]
        SL.MultiHostName dns ->
            Json.object
                [ "hostname" .= toAltJSON dns
                , "port" .= Json.Null
                ]

instance Crypto crypto => ToAltJSON (SL.TxOut (Shelley crypto)) where
    toAltJSON (SL.TxOut addr coin) = Json.object
        [ "address" .= toAltJSON addr
        , "value" .= toAltJSON coin
        ]

instance Crypto crypto => ToAltJSON (SL.TxIn (Shelley crypto)) where
    toAltJSON (SL.TxIn txid ix) = Json.object
        [ "txId" .= toAltJSON txid
        , "index" .= toAltJSON ix
        ]

instance Crypto crypto => ToAltJSON (SL.RewardAcnt (Shelley crypto)) where
    toAltJSON x = toAltJSON . bech32 hrp . SL.serialiseRewardAcnt $ x
      where
        hrp = case SL.getRwdNetwork x of
            SL.Mainnet -> [humanReadablePart|stake|]
            SL.Testnet -> [humanReadablePart|stake_test|]

instance Crypto crypto => ToAltJSON (SL.Addr (Shelley crypto)) where
    toAltJSON = \case
        SL.AddrBootstrap addr ->
            toAltJSON . SL.unBootstrapAddress $ addr
        addr@(SL.Addr network _ _) ->
            toAltJSON . bech32 (hrp network) . SL.serialiseAddr $ addr
      where
        hrp = \case
            SL.Mainnet -> [humanReadablePart|addr|]
            SL.Testnet -> [humanReadablePart|addr_test|]

instance ToAltJSON SL.Network where
    toAltJSON = \case
        SL.Mainnet -> Json.String "mainnet"
        SL.Testnet -> Json.String "testnet"

instance ToAltJSON SL.MIRPot where
    toAltJSON = \case
        SL.ReservesMIR -> "reserves"
        SL.TreasuryMIR -> "treasury"

instance Crypto crypto => ToAltJSON (ApplyTxError (Shelley crypto)) where
    toAltJSON (ApplyTxError xs) = toJSON (ledgerFailureToJSON <$> xs)
      where
        ledgerFailureToJSON = \case
            Ledgers.LedgerFailure (Ledger.UtxowFailure e)  ->
                utxowFailureToJSON e
            Ledgers.LedgerFailure (Ledger.DelegsFailure e) ->
                delegsFailureToJSON e

        utxowFailureToJSON = \case
            Utxow.InvalidWitnessesUTXOW wits ->
                Json.object
                    [ "invalidWitnesses" .= toAltJSON wits
                    ]
            Utxow.MissingVKeyWitnessesUTXOW keys ->
                Json.object
                    [ "missingVkWitnesses" .= toAltJSON keys
                    ]
            Utxow.MissingScriptWitnessesUTXOW scripts ->
                Json.object
                    [ "missingScriptWitnesses" .= toAltJSON scripts
                    ]
            Utxow.ScriptWitnessNotValidatingUTXOW scripts ->
                Json.object
                    [ "scriptWitnessNotValidating" .= toAltJSON scripts
                    ]
            Utxow.MIRInsufficientGenesisSigsUTXOW keys ->
                Json.object
                    [ "insufficientGenesisSignatures" .= toAltJSON keys
                    ]
            Utxow.MissingTxBodyMetaDataHash hash ->
                Json.object
                    [ "missingTxMetadataHash" .= toAltJSON hash
                    ]
            Utxow.MissingTxMetaData hash ->
                Json.object
                    [ "missingTxMetadata" .= toAltJSON hash
                    ]
            Utxow.ConflictingMetaDataHash included expected ->
                Json.object
                    [ "txMetadataHashMismatch" .= Json.object
                        [ "includedHash" .= toAltJSON included
                        , "expectedHash" .= toAltJSON expected
                        ]
                    ]
            Utxow.UtxoFailure e ->
                utxoFailureToJSON e

        utxoFailureToJSON = \case
            Utxo.BadInputsUTxO inputs ->
                Json.object
                    [ "badInputs" .= toAltJSON inputs ]
            Utxo.ExpiredUTxO ttl currentSlot ->
                Json.object
                    [ "expiredUtxo" .= Json.object
                        [ "transactionTimeToLive" .= toAltJSON ttl
                        , "currentSlot" .= toAltJSON currentSlot
                        ]
                    ]
            Utxo.MaxTxSizeUTxO actualSize maxSize ->
                Json.object
                    [ "txTooLarge" .= Json.object
                        [ "maximumSize" .= maxSize
                        , "actualSize"  .= actualSize
                        ]
                    ]
            Utxo.InputSetEmptyUTxO ->
                Json.String "missingAtLeastOneInputUtxo"
            Utxo.FeeTooSmallUTxO required actual ->
                Json.object
                    [ "feeTooSmall" .= Json.object
                        [ "requiredFee" .= toAltJSON required
                        , "actualFee" .= toAltJSON actual
                        ]
                    ]
            Utxo.ValueNotConservedUTxO consumed produced ->
                Json.object
                    [ "valueNotConserved" .= Json.object
                        [ "consumed" .= toAltJSON consumed
                        , "produced" .= toAltJSON produced
                        ]
                    ]
            Utxo.WrongNetwork expected invalidAddrs ->
                Json.object
                    [ "networkMismatch" .= Json.object
                        [ "expectedNetwork" .= toAltJSON expected
                        , "invalidAddresses" .= toAltJSON invalidAddrs
                        ]
                    ]
            Utxo.WrongNetworkWithdrawal expected invalidAccts ->
                Json.object
                    [ "networkMismatch" .= Json.object
                        [ "expectedNetwork" .= toAltJSON expected
                        , "invalidRewardAccounts" .= toAltJSON invalidAccts
                        ]
                    ]
            Utxo.OutputTooSmallUTxO outs ->
                Json.object
                    [ "outputTooSmall" .= toAltJSON outs
                    ]
            Utxo.OutputBootAddrAttrsTooBig outs ->
                Json.object
                    [ "addressAttributesTooLarge" .=
                        toAltJSON ((\(SL.TxOut addr _) -> addr) <$> outs)
                    ]
            Utxo.UpdateFailure e -> updateFailureToJSON e

        delegsFailureToJSON = \case
            Delegs.DelegateeNotRegisteredDELEG h ->
                Json.object
                    [ "delegateNotRegistered" .= toAltJSON h
                    ]
            Delegs.WithdrawalsNotInRewardsDELEGS withdrawals ->
                Json.object
                    [ "unknownOrIncompleteWithdrawals" .= toAltJSON (SL.Wdrl withdrawals)
                    ]
            Delegs.DelplFailure e ->
                delplFailureToJSON e

        delplFailureToJSON = \case
            Delpl.PoolFailure e -> poolFailureToJSON e
            Delpl.DelegFailure e -> delegFailureToJSON e

        poolFailureToJSON = \case
            Pool.StakePoolNotRegisteredOnKeyPOOL keyHash ->
                Json.object
                    [ "stakePoolNotRegistered" .= toAltJSON keyHash
                    ]
            Pool.StakePoolRetirementWrongEpochPOOL current retiring limit ->
                Json.object
                    [ "wrongRetirementEpoch" .= Json.object
                        [ "currentEpoch" .= toAltJSON current
                        , "requestedEpoch" .= toAltJSON retiring
                        , "firstUnreachableEpoch" .= toAltJSON limit
                        ]
                    ]
            Pool.WrongCertificateTypePOOL cert ->
                Json.object
                    [ "wrongPoolCertificate" .= toAltJSON cert
                    ]
            Pool.StakePoolCostTooLowPOOL _cost minimumCost ->
                Json.object
                    [ "poolCostTooSmall" .= Json.object
                        [ "minimumCost" .= minimumCost
                        ]
                    ]

        delegFailureToJSON = \case
            Deleg.StakeKeyAlreadyRegisteredDELEG credential ->
                Json.object
                    [ "stakeKeyAlreadyRegistered" .= toAltJSON credential
                    ]
            Deleg.StakeKeyInRewardsDELEG credential ->
                Json.object
                    [ "stakeKeyAlreadyRegistered" .= toAltJSON credential
                    ]
            Deleg.StakeKeyNotRegisteredDELEG credential ->
                Json.object
                    [ "stakeKeyNotRegistered" .= toAltJSON credential
                    ]
            Deleg.StakeDelegationImpossibleDELEG credential ->
                Json.object
                    [ "stakeKeyNotRegistered" .= toAltJSON credential
                    ]
            Deleg.StakeKeyNonZeroAccountBalanceDELEG Nothing ->
                Json.String "rewardAccountNotExisting"
            Deleg.StakeKeyNonZeroAccountBalanceDELEG (Just balance) ->
                Json.object
                    [ "rewardAccountNotEmpty" .= Json.object
                        [ "balance" .= toAltJSON  balance
                        ]
                    ]
            Deleg.WrongCertificateTypeDELEG ->
                Json.String "wrongCertificateType"
            Deleg.GenesisKeyNotInpMappingDELEG keyHash ->
                Json.object
                    [ "unknownGenesisKey" .= keyHash
                    ]
            Deleg.DuplicateGenesisDelegateDELEG keyHash ->
                Json.object
                    [ "alreadyDelegating" .= keyHash
                    ]
            Deleg.InsufficientForInstantaneousRewardsDELEG pot requested size ->
                Json.object
                    [ "insufficientFundsForMir" .= Json.object
                        [ "rewardSource" .= toAltJSON pot
                        , "sourceSize" .= toAltJSON size
                        , "requestedAmount" .= toAltJSON requested
                        ]
                    ]
            Deleg.MIRCertificateTooLateinEpochDELEG currentSlot lastSlot ->
                Json.object
                    [ "tooLateForMir" .= Json.object
                        [ "currentSlot" .= toAltJSON currentSlot
                        , "lastAllowedSlot" .= toAltJSON lastSlot
                        ]
                    ]
            Deleg.DuplicateGenesisVRFDELEG vrfHash ->
                Json.object
                    [ "duplicateGenesisVrf" .= toAltJSON vrfHash
                    ]

        updateFailureToJSON = \case
            Ppup.NonGenesisUpdatePPUP voting shouldBeVoting ->
                Json.object
                    [ "nonGenesisVoters" .= Json.object
                        [ "currentlyVoting" .= toAltJSON voting
                        , "shouldBeVoting" .= toAltJSON shouldBeVoting
                        ]
                    ]
            Ppup.PPUpdateWrongEpoch currentEpoch updateEpoch votingPeriod ->
                Json.object
                    [ "updateWrongEpoch" .= Json.object
                        [ "currentEpoch" .= toAltJSON currentEpoch
                        , "requestedEpoch" .= toAltJSON updateEpoch
                        , "votingPeriod" .= toAltJSON votingPeriod
                        ]
                    ]
            Ppup.PVCannotFollowPPUP version ->
                Json.object
                    [ "protocolVersionCannotFollow" .= toAltJSON version
                    ]

instance ToAltJSON ApplyMempoolPayloadErr where
    toAltJSON = \case
        MempoolTxErr e -> toAltJSON e
        -- NOTE
        -- branches below aren't actually used because we only submit
        -- payment transaction through the protocol.
        MempoolDlgErr e -> toAltJSON (show e)
        MempoolUpdateProposalErr e -> toAltJSON (show e)
        MempoolUpdateVoteErr e -> toAltJSON (show e)

instance ToAltJSON UTxOValidationError where
    toAltJSON = \case
        UTxOValidationTxValidationError e ->
            Json.object [ "txValidationError" .= toAltJSON e ]
        UTxOValidationUTxOError e ->
            Json.object [ "utxoValidationError" .= toAltJSON e ]

instance ToAltJSON TxValidationError where
    toAltJSON = \case
        TxValidationLovelaceError lbl e ->
            Json.object
                [ "lovelaceError" .= Json.object
                    [ "label" .= toAltJSON lbl
                    , "error" .= toAltJSON e
                    ]
                ]
        TxValidationFeeTooSmall _tx required actual ->
            Json.object
                [ "feeTooSmall" .= Json.object
                    [ "requiredFee" .= toAltJSON required
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
                [ "networkMismatch" .= Json.object
                    [ "expectedNetwork" .= toAltJSON expected
                    , "foundInAddress" .= toAltJSON actual
                    ]
                ]
        TxValidationTxTooLarge maxSize actualSize ->
            Json.object
                [ "txTooLarge" .= Json.object
                    [ "maximumSize" .= toAltJSON maxSize
                    , "actualSize" .= toAltJSON actualSize
                    ]
                ]
        TxValidationUnknownAddressAttributes ->
            Json.String "unknownAddressAttributes"
        TxValidationUnknownAttributes ->
            Json.String "unknownAttributes"

instance ToAltJSON LovelaceError where
    toAltJSON = \case
        LovelaceOverflow x ->
            Json.object [ "overflow" .= toAltJSON x ]
        LovelaceTooLarge x ->
            Json.object [ "tooLarge" .= toAltJSON x ]
        LovelaceTooSmall x ->
            Json.object [ "tooSmall" .= toAltJSON x ]
        LovelaceUnderflow x y ->
            Json.object [ "underflow" .= toAltJSON [x,y] ]

instance ToAltJSON UTxOError where
    toAltJSON = \case
        UTxOOverlappingUnion ->
            Json.String "overlappingUnion"
        UTxOMissingInput txin ->
            Json.object [ "missingInput" .= toAltJSON txin ]

instance Crypto crypto => ToAltJSON (SL.Credential any (Shelley crypto)) where
    toAltJSON = \case
        SL.KeyHashObj hash -> toAltJSON hash
        SL.ScriptHashObj hash -> toAltJSON hash

instance {-# OVERLAPS #-} Crypto crypto =>
    ToJSONKey (SL.Credential any (Shelley crypto))
  where
    toJSONKey = contramap credentialToText toJSONKey

-- FIXME: Makes it possible to distinguish between KeyHash and ScriptHash
-- credentials. Both are encoded as hex-encoded strings. Encoding them as object
-- is ill-advised because we also need them as key of the non-myopic member
-- rewards map.
--
-- A possible option: encode them as Bech32 strings with different prefixes.
instance Crypto crypto => FromAltJSON (SL.Credential any (Shelley crypto)) where
    fromAltJSON =
        fmap (SL.KeyHashObj . SL.KeyHash) . fromAltJSON

credentialToText
    :: Crypto crypto
    => SL.Credential any (Shelley crypto)
    -> Text
credentialToText = \case
    SL.KeyHashObj (SL.KeyHash hash) -> unsafeMatchString $ toAltJSON hash
    SL.ScriptHashObj hash -> unsafeMatchString $ toAltJSON hash

instance Crypto crypto =>
    ToJSONKey (Either SL.Coin (SL.Credential any (Shelley crypto)))
  where
    toJSONKey = Json.ToJSONKeyText
        (\case
            Left coin -> coinToText coin
            Right cred -> credentialToText cred
        )
        (\case
            Left coin -> Json.text $ coinToText coin
            Right cred -> Json.text $ credentialToText cred
        )

instance Crypto crypto => ToAltJSON (SL.Update (Shelley crypto)) where
    toAltJSON (SL.Update update epoch) = Json.object
        [ "proposal" .= toAltJSON update
        , "epoch" .= toAltJSON epoch
        ]

instance ToAltJSON (SL.PParams' SL.StrictMaybe (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "minFeeCoefficient" .= toAltJSON (SL._minfeeA x)
        , "minFeeConstant" .= toAltJSON (SL._minfeeB x)
        , "maxBlockBodySize" .= toAltJSON (SL._maxBBSize x)
        , "maxBlockHeaderSize" .= toAltJSON (SL._maxBHSize x)
        , "maxTxSize" .= toAltJSON (SL._maxTxSize x)
        , "stakeKeyDeposit" .= toAltJSON (SL._keyDeposit x)
        , "poolDeposit" .= toAltJSON (SL._poolDeposit x)
        , "poolRetirementEpochBound" .= toAltJSON (SL._eMax x)
        , "desiredNumberOfPools" .= toAltJSON (SL._nOpt x)
        , "poolInfluence" .= toAltJSON (SL._a0 x)
        , "monetaryExpansion" .= toAltJSON (SL._rho x)
        , "treasuryExpansion" .= toAltJSON (SL._tau x)
        , "decentralizationParameter" .= toAltJSON (SL._d x)
        , "extraEntropy" .= toAltJSON (SL._extraEntropy x)
        , "protocolVersion" .= toAltJSON (SL._protocolVersion x)
        , "minUtxoValue" .= toAltJSON (SL._minUTxOValue x)
        , "minPoolCost" .= toAltJSON (SL._minPoolCost x)
        ]

instance ToAltJSON (SL.PParams' Identity (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "minFeeCoefficient" .= toAltJSON (SL._minfeeA x)
        , "minFeeConstant" .= toAltJSON (SL._minfeeB x)
        , "maxBlockBodySize" .= toAltJSON (SL._maxBBSize x)
        , "maxBlockHeaderSize" .= toAltJSON (SL._maxBHSize x)
        , "maxTxSize" .= toAltJSON (SL._maxTxSize x)
        , "stakeKeyDeposit" .= toAltJSON (SL._keyDeposit x)
        , "poolDeposit" .= toAltJSON (SL._poolDeposit x)
        , "poolRetirementEpochBound" .= toAltJSON (SL._eMax x)
        , "desiredNumberOfPools" .= toAltJSON (SL._nOpt x)
        , "poolInfluence" .= toAltJSON (SL._a0 x)
        , "monetaryExpansion" .= toAltJSON (SL._rho x)
        , "treasuryExpansion" .= toAltJSON (SL._tau x)
        , "decentralizationParameter" .= toAltJSON (SL._d x)
        , "extraEntropy" .= toAltJSON (SL._extraEntropy x)
        , "protocolVersion" .= toAltJSON (SL._protocolVersion x)
        , "minUtxoValue" .= toAltJSON (SL._minUTxOValue x)
        , "minPoolCost" .= toAltJSON (SL._minPoolCost x)
        ]


instance ToAltJSON SL.Nonce where
    toAltJSON = \case
        SL.NeutralNonce -> Json.String "neutral"
        SL.Nonce h -> toAltJSON h

instance Crypto crypto => ToAltJSON (SL.MultiSig (Shelley crypto)) where
    toAltJSON = \case
        SL.RequireSignature sig ->
            toAltJSON sig
        SL.RequireAllOf xs ->
            Json.object
                [ "all" .= toAltJSON xs
                ]
        SL.RequireAnyOf xs ->
            Json.object
                [ "any" .= toAltJSON xs
                ]
        SL.RequireMOf n xs ->
            Json.object
                [ T.pack (show n) .= toAltJSON xs
                ]

instance Crypto crypto => ToAltJSON (SL.WitVKey (Shelley crypto) 'SL.Witness) where
    toAltJSON (SL.WitVKey key sig) = Json.object
        [ "key" .= toAltJSON key
        , "signature" .= toAltJSON sig
        ]

instance Crypto crypto => ToAltJSON (SL.BootstrapWitness (Shelley crypto)) where
    toAltJSON (SL.BootstrapWitness key sig cc attr) = Json.object
        [ "key" .= toAltJSON key
        , "chainCode" .= toAltJSON cc
        , "addressAttributes" .= if BS.null attr then Json.Null else toAltJSON attr
        , "signature" .= toAltJSON sig
        ]

instance Crypto crypto => ToAltJSON (NonMyopicMemberRewards (Shelley crypto)) where
    toAltJSON (NonMyopicMemberRewards rewards) = toAltJSON rewards

instance Crypto crypto => ToAltJSON (SL.PoolDistr (Shelley crypto)) where
    toAltJSON (SL.PoolDistr m) = toJSON $ Map.map toAltJSON m

instance Crypto crypto => ToAltJSON (SL.IndividualPoolStake (Shelley crypto)) where
    toAltJSON x = Json.object
        [ "stake" .= toAltJSON (SL.individualPoolStake x)
        , "vrf" .= toAltJSON (SL.individualPoolStakeVrf x)
        ]

instance Crypto crypto => ToAltJSON (SL.UTxO (Shelley crypto)) where
    toAltJSON (SL.UTxO m) = toAltJSON (Map.toList m)

--
-- Local State Queries
--

parseGetLedgerTip
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (Point (ShelleyBlock era))
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetLedgerTip genResult = Json.withText "SomeQuery" $ \text -> do
    guard (text == "ledgerTip") $> SomeQuery
        { query = QueryIfCurrentShelley GetLedgerTip
        , encodeResult = toAltJSON. right (coerce @_ @(Point (CardanoBlock crypto)))
        , genResult
        }

parseGetEpochNo
    :: forall crypto f result.
        ( Crypto crypto
        , result ~ QueryResult crypto EpochNo
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetEpochNo genResult = Json.withText "SomeQuery" $ \text -> do
    guard (text == "currentEpoch") $> SomeQuery
        { query = QueryIfCurrentShelley GetEpochNo
        , encodeResult = toAltJSON
        , genResult
        }

parseGetNonMyopicMemberRewards
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (NonMyopicMemberRewards era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetNonMyopicMemberRewards genResult = Json.withObject "SomeQuery" $ \obj -> do
    arg <- obj .: "nonMyopicMemberRewards"
        >>= traverse (choice [ parseStake, parseCredential ])
    pure $ SomeQuery
        { query = QueryIfCurrentShelley (GetNonMyopicMemberRewards $ Set.fromList arg)
        , encodeResult = toAltJSON
        , genResult
        }
  where
    parseStake
        :: Json.Value
        -> Json.Parser (Either SL.Coin b)
    parseStake = fmap Left . fromAltJSON

    parseCredential
        :: Json.Value
        -> Json.Parser (Either a (SL.Credential 'SL.Staking (Shelley crypto)))
    parseCredential = fmap Right . fromAltJSON

parseGetCurrentPParams
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (SL.PParams era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetCurrentPParams genResult = Json.withText "SomeQuery" $ \text -> do
    guard (text == "currentProtocolParameters") $> SomeQuery
        { query = QueryIfCurrentShelley GetCurrentPParams
        , encodeResult = toAltJSON
        , genResult
        }

parseGetProposedPParamsUpdates
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (SL.ProposedPPUpdates era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetProposedPParamsUpdates genResult = Json.withText "SomeQuery" $ \text -> do
    guard (text == "proposedProtocolParameters") $> SomeQuery
        { query = QueryIfCurrentShelley GetProposedPParamsUpdates
        , encodeResult = toAltJSON
        , genResult
        }

parseGetStakeDistribution
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (SL.PoolDistr era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetStakeDistribution genResult = Json.withText "SomeQuery" $ \text -> do
    guard (text == "stakeDistribution") $> SomeQuery
        { query = QueryIfCurrentShelley GetStakeDistribution
        , encodeResult = toAltJSON
        , genResult
        }

parseGetUTxO
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (SL.UTxO era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetUTxO genResult = Json.withText "SomeQuery" $ \text -> do
    guard (text == "utxo") $> SomeQuery
        { query = QueryIfCurrentShelley GetUTxO
        , encodeResult = toAltJSON
        , genResult
        }

parseGetFilteredUTxO
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (SL.UTxO era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetFilteredUTxO genResult = Json.withObject "SomeQuery" $ \obj -> do
    addrs <- obj .: "utxo" >>= traverse parseAddress
    pure SomeQuery
        { query = QueryIfCurrentShelley (GetFilteredUTxO $ Set.fromList addrs)
        , encodeResult = toAltJSON
        , genResult
        }
  where
    parseAddress :: Json.Value -> Json.Parser (SL.Addr era)
    parseAddress = Json.withText "Address" $ choice
        [ addressFromBytes fromBech32
        , addressFromBytes fromBase58
        , addressFromBytes fromBase16
        ]
      where
        addressFromBytes decode =
            decode >=> maybe mempty pure . SL.deserialiseAddr

        fromBech32 txt =
            case Bech32.decodeLenient txt of
                Left e ->
                    fail (show e)
                Right (_, dataPart) ->
                    maybe mempty pure $ Bech32.dataPartToBytes dataPart

        fromBase58 =
            maybe mempty pure . decodeBase58 bitcoinAlphabet . T.encodeUtf8

        fromBase16 =
            either (fail . show) pure . convertFromBase Base16 . T.encodeUtf8

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result
--
-- Internal / Helpers
--

-- | Handy to avoid duplicating the serialization logic. Use only in places
-- where it is actually _safe_ and where the values is known to be a 'Text'
unsafeMatchString :: Json.Value -> Text
unsafeMatchString = \case
    Json.String txt -> txt
    _ -> throw $ PatternMatchFail "expected value to be serialized as a JSON 'String'"


base16 :: ByteArrayAccess bin => bin -> Text
base16 = T.decodeUtf8 . convertToBase Base16

base64 :: ByteArrayAccess bin => bin -> Text
base64 = T.decodeUtf8 . convertToBase Base64

fromBase64 :: ByteString -> Json.Parser ByteString
fromBase64 = either (fail . show) pure . convertFromBase Base64

bech32 :: HumanReadablePart -> ByteString -> Text
bech32 hrp bytes = Bech32.encodeLenient hrp (Bech32.dataPartFromBytes bytes)

choice :: Alternative f => [a -> f b] -> a -> f b
choice xs a = asum (xs <*> pure a)
