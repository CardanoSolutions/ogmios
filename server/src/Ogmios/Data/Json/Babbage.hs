--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Babbage where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Allegra.Scripts
    ( Timelock
    )
import Cardano.Ledger.Alonzo.Plutus.TxInfo
    ( TxOutSource (..)
    )
import Cardano.Ledger.Api
    ( AsIx (..)
    )
import Cardano.Ledger.Binary
    ( sizedValue
    )
import Cardano.Ledger.Compactible
    ( CompactForm
    , Compactible (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )

import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()
import Cardano.Ledger.HKD
    ()

import qualified Data.Map.Strict as Map

import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Plutus.Data as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh

import qualified Cardano.Ledger.Allegra.Scripts as Al

import qualified Cardano.Ledger.Mary.Value as Ma

import qualified Cardano.Ledger.Alonzo.BlockBody as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al

import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Babbage.PParams as Ba
import qualified Cardano.Ledger.Babbage.Tx as Ba
import qualified Cardano.Ledger.Babbage.TxBody as Ba

import qualified Cardano.Ledger.Alonzo.Plutus.TxInfo as Al
import qualified Cardano.Ledger.Babbage.TxInfo as Ba
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley


encodeBlock
    :: (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (Praos StandardCrypto) BabbageEra
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "babbage"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          encodeHeader blkHeader
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (Al.alonzoBlockBodyTxs txs)
        )

encodeContextError
    :: ( Ba.PlutusPurpose AsIx era ~ Al.AlonzoPlutusPurpose AsIx era
       )
    => Ba.BabbageContextError era
    -> Json
encodeContextError err = encodeText $ case err of
    Ba.ByronTxOutInContext TxOutFromInput{} ->
        "Found inputs locked by a (legacy) Byron/Bootstrap address. Don't use those."
    Ba.ByronTxOutInContext TxOutFromOutput{} ->
        "Found outputs to a (legacy) Byron/Bootstrap address. Don't use those."
    Ba.InlineDatumsNotSupported{} ->
       "Inline datums not supported in plutus:v1. Use plutus:v2 or higher."
    Ba.ReferenceScriptsNotSupported{} ->
       "Reference scripts not supported in plutus:v1. Use plutus:v2 or higher."
    Ba.ReferenceInputsNotSupported{} ->
       "Reference inputs not supported in plutus:v1. Use plutus:v2 or higher."
    Ba.RedeemerPointerPointsToNothing purpose ->
        let (title, ptr) =
                case purpose of
                    Al.AlonzoSpending (AsIx ix) -> ("spending input", ix)
                    Al.AlonzoMinting (AsIx ix) -> ("minting policy", ix)
                    Al.AlonzoCertifying (AsIx ix) -> ("publishing certificate", ix)
                    Al.AlonzoRewarding (AsIx ix) -> ("withdrawing from account", ix)
          in "Couldn't find corresponding redeemer for " <> title <> " #" <> show ptr <> ". Verify your transaction's construction."
    Ba.AlonzoContextError (Al.TimeTranslationPastHorizon e) ->
        "Uncomputable slot arithmetic; transaction's validity bounds go beyond the foreseeable end of the current era: " <> e
    Ba.AlonzoContextError (Al.TranslationLogicMissingInput i) ->
        "Unknown transaction input (missing from UTxO set): " <> Shelley.stringifyTxIn i

encodeHeader
    :: Praos.Header StandardCrypto
    -> Series
encodeHeader (Praos.Header hBody _hSig) =
    "size" .=
        encodeSingleton "bytes" (encodeWord32 (Praos.hbBodySize hBody)) <>
    "height" .=
        encodeBlockNo (Praos.hbBlockNo hBody) <>
    "slot" .=
        encodeSlotNo (Praos.hbSlotNo hBody) <>
    "ancestor" .=
        Shelley.encodePrevHash (Praos.hbPrev hBody) <>
    "issuer" .= encodeObject
        ( "verificationKey" .=
            Shelley.encodeVKey (Praos.hbVk hBody) <>
          "vrfVerificationKey" .=
            Shelley.encodeVerKeyVRF (Praos.hbVrfVk hBody) <>
          "operationalCertificate" .=
            Shelley.encodeOCert (Praos.hbOCert hBody) <>
          "leaderValue" .=
            Shelley.encodeCertifiedVRF (Praos.hbVrfRes hBody)
        ) <>
    "protocol" .= encodeObject
        ( "version" .=
            Shelley.encodeProtVer (Praos.hbProtVer hBody)
        )

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Ba.BabbagePParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k enc v -> k .= enc v) (\k enc v -> k .= enc (fromCompact v)) identity x

encodePParamsUpdate
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Ba.BabbagePParams StrictMaybe era
        )
    => Ledger.PParamsUpdate era
    -> [Json]
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    case (Ba.bppProtocolVersion x, x' == Ba.emptyBabbagePParamsUpdate) of
        (SJust version, True) ->
            [ encodeObject
                ( "type" .=
                    encodeText "hardForkInitiation"
               <> "version" .=
                    Shelley.encodeProtVer version
                )
            ]
        (SJust version, False) ->
            [ encodeObject
                ( "type" .=
                    encodeText "hardForkInitiation"
               <> "version" .=
                    Shelley.encodeProtVer version
                )
            , encodeObject
                ( "type" .=
                    encodeText "protocolParametersUpdate"
               <> "parameters" .=
                    encodePParamsHKD
                        (\k enc v -> k .=? OmitWhenNothing enc v)
                        (\k enc v -> k .=? OmitWhenNothing (enc . fromCompact) v)
                        (const SNothing)
                        x'
                )
            ]
        (SNothing, _) ->
            [ encodeObject
                ( "type" .=
                    encodeText "protocolParametersUpdate"
               <> "parameters" .=
                    encodePParamsHKD
                        (\k enc v -> k .=? OmitWhenNothing enc v)
                        (\k enc v -> k .=? OmitWhenNothing (enc . fromCompact) v)
                        (const SNothing)
                        x'
                )
            ]
  where
    x' :: Ledger.PParamsHKD StrictMaybe era
    x' = x { Ba.bppProtocolVersion = SNothing }

encodeProposedPPUpdates
    :: forall era.
        ( Ledger.PParamsHKD StrictMaybe era ~ Ba.BabbagePParams StrictMaybe era
        )
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeFoldable
        (\(Ledger.PParamsUpdate x) ->
            encodePParamsHKD
                (\k enc v -> k .=? OmitWhenNothing enc v)
                (\k enc v -> k .=? OmitWhenNothing (enc . fromCompact) v)
                (const SNothing)
                x
        )
        m

encodePParamsHKD
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> (forall a. Compactible a => Text -> (a -> Json) -> Sh.HKD f (CompactForm a) -> Series)
    -> (Integer -> Sh.HKD f Integer)
    -> Ba.BabbagePParams f era
    -> Json
encodePParamsHKD encode encodeCompact pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (Ba.bppTxFeePerByte x) <>
    encodeCompact "minFeeConstant"
        encodeCoin (Ba.bppTxFeeFixed x) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeWord32) (Ba.bppMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeWord16) (Ba.bppMaxBHSize x) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeWord32) (Ba.bppMaxTxSize x) <>
    encodeCompact "stakeCredentialDeposit"
        encodeCoin (Ba.bppKeyDeposit x) <>
    encodeCompact "stakePoolDeposit"
        encodeCoin (Ba.bppPoolDeposit x) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochInterval (Ba.bppEMax x) <>
    encode "desiredNumberOfStakePools"
        encodeWord16 (Ba.bppNOpt x) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (Ba.bppA0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Ba.bppRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Ba.bppTau x) <>
    encodeCompact "minStakePoolCost"
        encodeCoin (Ba.bppMinPoolCost x) <>
    encode "minUtxoDepositConstant"
        (encodeCoin . Coin) (pure_ 0) <>
    encode "minUtxoDepositCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (Ba.bppCoinsPerUTxOByte x) <>
    encode "plutusCostModels"
        Alonzo.encodeCostModels (Ba.bppCostModels x) <>
    encode "scriptExecutionPrices"
        Alonzo.encodePrices (Ba.bppPrices x) <>
    encode "maxExecutionUnitsPerTransaction"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (Ba.bppMaxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (Ba.bppMaxBlockExUnits x) <>
    encode "maxValueSize"
        (encodeSingleton "bytes" . encodeWord32) (Ba.bppMaxValSize x) <>
    encode "collateralPercentage"
        encodeWord16 (Ba.bppCollateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeWord16 (Ba.bppMaxCollateralInputs x) <>
    encode "version"
        Shelley.encodeProtVer (Ba.bppProtocolVersion x)
    & encodeObject

encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.Tx Ledger.TopTx BabbageEra
    -> Json
encodeTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @BabbageEra (x ^. Ledger.bodyTxL))
       <>
        "spends" .= Alonzo.encodeIsValid (x ^. Ba.isValidTxL)
       <>
        encodeTxBody opts (x ^. Ledger.bodyTxL) (strictMaybe mempty (Map.keys . snd) auxiliary)
       <>
        "metadata" .=? OmitWhenNothing fst auxiliary
       <>
        Alonzo.encodeWitnessSet (snd <$> auxiliary) Alonzo.encodeScriptPurposeIndex (Alonzo.encodeScript opts) (x ^. Ledger.witsTxL)
       <>
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (encodeCbor @BabbageEra x)
        else
           mempty
       )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Ba.btbAuxDataHash (x ^. Ledger.bodyTxL)
        (labels, scripts) <- Alonzo.encodeAuxiliaryData (fmt, opts) <$> x ^. Ledger.auxDataTxL
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: IncludeCbor
    -> Ledger.TxBody Ledger.TopTx BabbageEra
    -> [Ledger.ScriptHash]
    -> Series
encodeTxBody opts x scripts =
    "inputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxIn) (Ba.btbInputs x) <>
    "references" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (Ba.btbReferenceInputs x) <>
    "outputs" .=? OmitWhen null
        (encodeFoldable (encodeObject . encodeTxOut encodeScript . sizedValue)) (Ba.btbOutputs x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (Ba.btbCollateral x) <>
    "collateralReturn" .=? OmitWhenNothing
        (encodeObject . encodeTxOut encodeScript . sizedValue) (Ba.btbCollateralReturn x) <>
    "totalCollateral" .=? OmitWhenNothing
        encodeCoin (Ba.btbTotalCollateral x) <>
    "certificates" .=? OmitWhen null
        (encodeList encodeObject) certs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl (Ba.btbWithdrawals x) <>
    "mint" .=? OmitWhen (== mempty)
        (encodeObject . Mary.encodeMultiAsset) (Ba.btbMint x) <>
    "requiredExtraSignatories" .=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Ba.btbReqSignerHashes x) <>
    "requiredExtraScripts" .=? OmitWhen null
        (encodeFoldable Shelley.encodeScriptHash) scripts <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Ba.btbTxNetworkId x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        Alonzo.encodeScriptIntegrityHash (Ba.btbScriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Ba.btbTxFee x) <>
    "validityInterval" .=? OmitWhen (\it -> isSNothing (Al.invalidBefore it) && isSNothing (Al.invalidHereafter it))
        Allegra.encodeValidityInterval (Ba.btbValidityInterval x) <>
    "proposals" .=? OmitWhen null
        (encodeList (encodeSingleton "action")) actions <>
    "votes" .=? OmitWhen null
        (encodeList Shelley.encodeGenesisVote) votes
  where
    encodeScript =
        Alonzo.encodeScript opts

    (certs, mirs) =
        Shelley.encodeTxCerts (Ba.btbCerts x)

    (votes, actions) = fromSMaybe ([], mirs) $
        Shelley.encodeUpdate encodePParamsUpdate mirs <$> Ba.btbUpdate x

encodeTxOut
    :: forall era.
        ( Ledger.Value era ~ Ma.MaryValue
        , Ledger.EraScript era
        )
    => (Ledger.Script era -> Json)
    -> Ba.BabbageTxOut era
    -> Series
encodeTxOut encodeScript (Ba.BabbageTxOut addr value datum script) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        Mary.encodeValue value <>
    ( case datum of
        Ledger.NoDatum ->
            mempty
        Ledger.DatumHash h ->
            "datumHash" .= Alonzo.encodeDataHash h
        Ledger.Datum bin ->
            "datum" .= Alonzo.encodeBinaryData bin
    ) <>
    "script" .=? OmitWhenNothing
        encodeScript script

encodeUtxo
    :: forall era.
        ( Ba.AlonzoEraScript era
        , Ba.Script era ~ Al.AlonzoScript era
        , Ba.Value era ~ Ma.MaryValue
        , Ba.TxOut era ~ Ba.BabbageTxOut era
        , Ledger.NativeScript era ~ Timelock era
        )
    => Sh.UTxO era
    -> Json
encodeUtxo =
    encodeList id
    . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) []
    . Sh.unUTxO
  where
    encodeIO i o =
        encodeObject (Shelley.encodeTxIn i <> encodeTxOut (Alonzo.encodeScript includeAllCbor) o)
