--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Babbage where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Ledger.Binary
    ( sizedValue
    )
import Cardano.Ledger.Val
    ( Val (..)
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()

import qualified Data.Map.Strict as Map

import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh

import qualified Cardano.Ledger.Mary.Value as Ma

import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Al
import qualified Cardano.Ledger.Alonzo.TxSeq as Al

import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Babbage.PParams as Ba
import qualified Cardano.Ledger.Babbage.Tx as Ba
import qualified Cardano.Ledger.Babbage.TxBody as Ba

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley


encodeBlock
    :: ( Crypto crypto
       )
    => ShelleyBlock (Praos crypto) (BabbageEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "era" .= encodeText "babbage"
        <>
          "header" .= encodeObject
            ( "hash" .= Shelley.encodeShelleyHash headerHash
            )
        <>
        encodeHeader blkHeader
        <>
        "transactions" .= encodeFoldable encodeTx (Al.txSeqTxns txs)
        )

encodeHeader
    :: Crypto crypto
    => Praos.Header crypto
    -> Series
encodeHeader (Praos.Header hBody _hSig) =
    "size" .=
        encodeWord32 (Praos.hbBodySize hBody) <>
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

encodeProposedPPUpdates
    :: Crypto crypto
    => Sh.ProposedPPUpdates (BabbageEra crypto)
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap Shelley.stringifyKeyHash encodePParamsUpdate m

encodePParams
    :: (Ledger.PParamsHKD Identity era ~ Ba.BabbagePParams Identity era)
    => Ledger.PParams era
    -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k encode v -> k .= encode v) x

encodePParamsUpdate
    :: (Ledger.PParamsHKD StrictMaybe era ~ Ba.BabbagePParams StrictMaybe era)
    => Ledger.PParamsUpdate era
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k encode v -> k .=? OmitWhenNothing encode v) x

encodePParamsHKD
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
    -> Ba.BabbagePParams f era
    -> Json
encodePParamsHKD encode x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin) (Ba.bppMinFeeA x) <>
    encode "minFeeConstant"
        encodeCoin (Ba.bppMinFeeB x) <>
    encode "maxBlockBodySize"
        encodeNatural (Ba.bppMaxBBSize x) <>
    encode "maxBlockHeaderSize"
        encodeNatural (Ba.bppMaxBHSize x) <>
    encode "maxTxSize"
        encodeNatural (Ba.bppMaxTxSize x) <>
    encode "stakeKeyDeposit"
        encodeCoin (Ba.bppKeyDeposit x) <>
    encode "poolDeposit"
        encodeCoin (Ba.bppPoolDeposit x) <>
    encode "poolRetirementEpochBound"
        encodeEpochNo (Ba.bppEMax x) <>
    encode "desiredNumberOfPools"
        encodeNatural (Ba.bppNOpt x) <>
    encode "poolInfluence"
        encodeNonNegativeInterval (Ba.bppA0 x) <>
    encode "monetaryExpansion"
        encodeUnitInterval (Ba.bppRho x) <>
    encode "treasuryExpansion"
        encodeUnitInterval (Ba.bppTau x) <>
    encode "protocolVersion"
        Shelley.encodeProtVer (Ba.bppProtocolVersion x) <>
    encode "minPoolCost"
        encodeCoin (Ba.bppMinPoolCost x) <>
    encode "coinsPerUtxoByte"
        (encodeInteger . unCoin . Ba.unCoinPerByte) (Ba.bppCoinsPerUTxOByte x) <>
    encode "costModels"
        Alonzo.encodeCostModels (Ba.bppCostModels x) <>
    encode "prices"
        Alonzo.encodePrices (Ba.bppPrices x) <>
    encode "maxExecutionUnitsPerTransaction"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (Ba.bppMaxTxExUnits x) <>
    encode "maxExecutionUnitsPerBlock"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (Ba.bppMaxBlockExUnits x) <>
    encode "maxValueSize"
        encodeNatural (Ba.bppMaxValSize x) <>
    encode "collateralPercentage"
        encodeNatural (Ba.bppCollateralPercentage x) <>
    encode "maxCollateralInputs"
        encodeNatural (Ba.bppMaxCollateralInputs x)
    & encodeObject

encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => Ba.AlonzoTx (BabbageEra crypto)
    -> Json
encodeTx x =
    "id" .= Shelley.encodeTxId (Ledger.txid @(BabbageEra crypto) (Ba.body x))
        <>
    "inputSource" .= Alonzo.encodeIsValid (Ba.isValid x)
        <>
    encodeTxBody (Ba.body x)
        <>
    "metadata" .=? OmitWhenNothing fst auxiliary
        <>
    Alonzo.encodeWitnessSet (snd <$> auxiliary) (Ba.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Ba.btbAuxDataHash (Ba.body x)
        (labels, scripts) <- Alonzo.encodeAuxiliaryData <$> Ba.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: ( Crypto crypto
       )
    => Ba.BabbageTxBody (BabbageEra crypto)
    -> Series
encodeTxBody x =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn (Ba.btbInputs x) <>
    "references" .=? OmitWhen null
        (encodeFoldable Shelley.encodeTxIn) (Ba.btbReferenceInputs x) <>
    "outputs" .=
        encodeFoldable (encodeTxOut . sizedValue) (Ba.btbOutputs x) <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable Shelley.encodeTxIn) (Ba.btbCollateral x) <>
    "collateralReturn" .=? OmitWhenNothing
        (encodeTxOut . sizedValue) (Ba.btbCollateralReturn x) <>
    "collateral" .=? OmitWhenNothing
        encodeCoin (Ba.btbTotalCollateral x) <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) (Ba.btbCerts x) <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl (Ba.btbWithdrawals x) <>
    "mint" .=? OmitWhen (== mempty)
        (encodeObject . Mary.encodeMultiAsset) (Ba.btbMint x) <>
    "requiredExtraSignatories" .=? OmitWhen null
        (encodeFoldable Shelley.encodeKeyHash) (Ba.btbReqSignerHashes x) <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (Ba.btbTxNetworkId x) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        Alonzo.encodeScriptIntegrityHash (Ba.btbScriptIntegrityHash x) <>
    "fee" .=
        encodeCoin (Ba.btbTxFee x) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (Ba.btbValidityInterval x) <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . encodeUpdate)
        (Ba.btbUpdate x)

encodeTxOut
    :: forall era.
        ( Era era
        , Ba.Script era ~ Al.AlonzoScript era
        , Ba.Value era ~ Ma.MaryValue (Ledger.EraCrypto era)
        , Val (Ba.Value era)
        )
    => Ba.BabbageTxOut era
    -> Json
encodeTxOut (Ba.BabbageTxOut addr value datum script) =
    "address" .=
        Shelley.encodeAddress addr <>
    "value" .=
        Mary.encodeValue value <>
    ( case datum of
        Al.NoDatum ->
            mempty
        Al.DatumHash h ->
            "datumHash" .= Alonzo.encodeDataHash h
        Al.Datum bin ->
            "datum" .= Alonzo.encodeBinaryData bin
    ) <>
    "script" .=? OmitWhenNothing
        Alonzo.encodeScript script
    & encodeObject

encodeUpdate
    :: Crypto crypto
    => Sh.Update (BabbageEra crypto)
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
        , Ba.Script era ~ Al.AlonzoScript era
        , Ba.Value era ~ Ma.MaryValue (Ledger.EraCrypto era)
        , Ba.TxOut era ~ Ba.BabbageTxOut era
        )
    => Sh.UTxO era
    -> Json
encodeUtxo =
    encodeList id
    . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) []
    . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)
