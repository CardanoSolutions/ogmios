--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | Dijkstra is a new era following Conway (protocol version 12).
-- It introduces script guarding, new protocol parameters for reference script
-- costs, and removes pointer addresses.
module Ogmios.Data.Json.Dijkstra where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Address
    ( DirectDeposits (..)
    )
import Cardano.Ledger.Api
    ( AlonzoEraScript (..)
    , AsItem (..)
    , AsIx (..)
    )
import Cardano.Ledger.BaseTypes
    ( BoundedRational (..)
    , Exclusive (..)
    , Inclusive (..)
    , NonNegativeInterval
    , PositiveInterval
    , ProtVer
    )
import Cardano.Ledger.Compactible
    ( fromCompact
    )
import Cardano.Ledger.Conway.PParams
    ( THKD (..)
    )
import Cardano.Ledger.HKD
    ( HKDFunctor (..)
    )
import Cardano.Ledger.MemoBytes
    ( getMemoRawType
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )

import Cardano.Ledger.Babbage.Tx
    ()
import Cardano.Ledger.Conway.Core
    ()
import Cardano.Ledger.Conway.Tx
    ()
import Cardano.Ledger.Dijkstra.Governance
    ()
import Cardano.Ledger.Dijkstra.Tx
    ()
import Ouroboros.Consensus.Shelley.Protocol.Praos
    ()

import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.HKD as Ledger

import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Babbage.Core as Ba
import qualified Cardano.Ledger.Conway.Scripts as Cn
import qualified Cardano.Ledger.Dijkstra.PParams as Di
import qualified Cardano.Ledger.Dijkstra.Scripts as Di
import qualified Cardano.Ledger.Dijkstra.TxBody as Di
import qualified Cardano.Ledger.Dijkstra.TxCert as Di
import qualified Cardano.Ledger.Dijkstra.TxInfo as Di
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Data.Map.Strict as Map

import qualified Cardano.Ledger.Conway.TxCert as Cn
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Babbage as Babbage
import qualified Ogmios.Data.Json.Conway as Conway
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

encodeBlock
    :: (MetadataFormat, IncludeCbor)
    -> ShelleyBlock (Praos StandardCrypto) DijkstraEra
    -> Json
encodeBlock opts (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "type" .= encodeText "praos"
        <>
          "era" .= encodeText "dijkstra"
        <>
          "id" .= Shelley.encodeShelleyHash headerHash
        <>
          Babbage.encodeHeader blkHeader
        <>
          "transactions" .= encodeFoldable (encodeTx opts) (txs ^. Ledger.txSeqBlockBodyL)
        )

encodeContextError
    :: Di.DijkstraContextError DijkstraEra
    -> Json
encodeContextError = \case
    Di.ConwayContextError err ->
        Conway.encodeContextError humanReadablePurpose err
    Di.PointerPresentInOutput{} ->
        encodeText "Found outputs with pointer addresses. Those are no longer supported."

encodeDelegCert
    :: Di.DijkstraDelegCert
    -> NonEmpty Series
encodeDelegCert = Conway.encodeDelegCert . \case
    Di.DijkstraRegCert credential deposit ->
        Cn.ConwayRegCert credential (SJust deposit)
    Di.DijkstraUnRegCert credential deposit ->
        Cn.ConwayUnRegCert credential (SJust deposit)
    Di.DijkstraDelegCert credential delegatee ->
        Cn.ConwayDelegCert credential delegatee
    Di.DijkstraRegDelegCert credential delegatee deposit ->
        Cn.ConwayRegDelegCert credential delegatee deposit

encodeNativeScript
    :: Di.DijkstraNativeScript DijkstraEra
    -> Json
encodeNativeScript ns = encodeObject (go (getMemoRawType ns))
  where
    go = \case
        Di.DijkstraRequireSignature sig ->
            "clause" .= encodeText "signature" <>
            "from" .= Shelley.encodeKeyHash sig
        Di.DijkstraRequireAllOf xs ->
            "clause" .= encodeText "all" <>
            "from" .= encodeFoldable encodeNativeScript xs
        Di.DijkstraRequireAnyOf xs ->
            "clause" .= encodeText "any" <>
            "from" .= encodeFoldable encodeNativeScript xs
        Di.DijkstraRequireMOf n xs ->
            "clause" .= encodeText "some" <>
            "atLeast" .= encodeInteger (toInteger n) <>
            "from" .= encodeFoldable encodeNativeScript xs
        Di.DijkstraTimeStart s ->
            "clause" .= encodeText "after" <>
            "slot" .= encodeSlotNo s
        Di.DijkstraTimeExpire s ->
            "clause" .= encodeText "before" <>
            "slot" .= encodeSlotNo s
        Di.DijkstraRequireGuard cred ->
            "clause" .= encodeText "guard" <>
            "from" .= Shelley.encodeCredentialRaw cred

encodePParams :: Ledger.PParams DijkstraEra -> Json
encodePParams (Ledger.PParams x) =
    encodePParamsHKD (\k enc v -> k .= enc v) identity x

encodePParamsHKD
    :: forall f. (HKDFunctor f)
    => (forall a. Text -> (a -> Json) -> Ledger.HKD f a -> Series)
    -> (forall a. a -> Ledger.HKD f a)
    -> Di.DijkstraPParams f DijkstraEra
    -> Json
encodePParamsHKD encode pure_ x =
    encode "minFeeCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (unTHKD (Di.dppTxFeePerByte x)) <>
    encode "minFeeConstant"
        (encodeCoin . fromCompact) (unTHKD (Di.dppTxFeeFixed x)) <>
    encode "minFeeReferenceScripts"
        (\(base :: NonNegativeInterval) -> encodeObject
            ( "base" .= encodeDouble (fromRational (unboundRational base))
            )
        )
        (unTHKD (Di.dppMinFeeRefScriptCostPerByte x)) <>
    encode "refScriptCostStride"
        (encodeNonZero encodeWord32)
        (unTHKD (Di.dppRefScriptCostStride x)) <>
    encode "refScriptCostMultiplier"
        (\(v :: PositiveInterval) -> encodeDouble (fromRational (unboundRational v)))
        (unTHKD (Di.dppRefScriptCostMultiplier x)) <>
    encode "maxBlockBodySize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxBBSize x)) <>
    encode "maxBlockHeaderSize"
        (encodeSingleton "bytes" . encodeWord16) (unTHKD (Di.dppMaxBHSize x)) <>
    encode "maxTransactionSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxTxSize x)) <>
    encode "maxReferenceScriptsSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxRefScriptSizePerTx x)) <>
    encode "maxReferenceScriptsSizePerBlock"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxRefScriptSizePerBlock x)) <>
    encode "stakeCredentialDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppKeyDeposit x)) <>
    encode "stakePoolDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppPoolDeposit x)) <>
    encode "stakePoolRetirementEpochBound"
        encodeEpochInterval (unTHKD (Di.dppEMax x)) <>
    encode "desiredNumberOfStakePools"
        encodeWord16 (unTHKD (Di.dppNOpt x)) <>
    encode "stakePoolPledgeInfluence"
        encodeNonNegativeInterval (unTHKD (Di.dppA0 x)) <>
    encode "monetaryExpansion"
        encodeUnitInterval (unTHKD (Di.dppRho x)) <>
    encode "treasuryExpansion"
        encodeUnitInterval (unTHKD (Di.dppTau x)) <>
    encode "minStakePoolCost"
        (encodeCoin . fromCompact) (unTHKD (Di.dppMinPoolCost x)) <>
    encode "minUtxoDepositConstant"
        encodeCoin (pure_ (Coin 0)) <>
    encode "minUtxoDepositCoefficient"
        (encodeInteger . unCoin . fromCompact . Ba.unCoinPerByte) (unTHKD (Di.dppCoinsPerUTxOByte x)) <>
    encode "plutusCostModels"
        Alonzo.encodeCostModels (unTHKD (Di.dppCostModels x)) <>
    encode "scriptExecutionPrices"
        Alonzo.encodePrices (unTHKD (Di.dppPrices x)) <>
    encode "maxExecutionUnitsPerTransaction"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (unTHKD (Di.dppMaxTxExUnits x)) <>
    encode "maxExecutionUnitsPerBlock"
        (Alonzo.encodeExUnits . Al.unOrdExUnits) (unTHKD (Di.dppMaxBlockExUnits x)) <>
    encode "maxValueSize"
        (encodeSingleton "bytes" . encodeWord32) (unTHKD (Di.dppMaxValSize x)) <>
    encode "collateralPercentage"
        encodeWord16 (unTHKD (Di.dppCollateralPercentage x)) <>
    encode "maxCollateralInputs"
        encodeWord16 (unTHKD (Di.dppMaxCollateralInputs x)) <>
    encode "version"
        Shelley.encodeProtVer (fromNoUpdate @f @ProtVer (Di.dppProtocolVersion x)) <>
    encode "stakePoolVotingThresholds"
        Conway.encodePoolVotingThresholds (unTHKD (Di.dppPoolVotingThresholds x)) <>
    encode "delegateRepresentativeVotingThresholds"
        Conway.encodeDRepVotingThresholds (unTHKD (Di.dppDRepVotingThresholds x)) <>
    encode "constitutionalCommitteeMinSize"
        encodeWord16 (unTHKD (Di.dppCommitteeMinSize x)) <>
    encode "constitutionalCommitteeMaxTermLength"
        encodeEpochInterval (unTHKD (Di.dppCommitteeMaxTermLength x)) <>
    encode "governanceActionLifetime"
        encodeEpochInterval (unTHKD (Di.dppGovActionLifetime x)) <>
    encode "governanceActionDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppGovActionDeposit x)) <>
    encode "delegateRepresentativeDeposit"
        (encodeCoin . fromCompact) (unTHKD (Di.dppDRepDeposit x)) <>
    encode "delegateRepresentativeMaxIdleTime"
        encodeEpochInterval (unTHKD (Di.dppDRepActivity x))
    & encodeObject

encodePParamsUpdate
    :: Ledger.PParamsUpdate DijkstraEra
    -> Json
encodePParamsUpdate (Ledger.PParamsUpdate x) =
    encodePParamsHKD (\k enc v -> k .=? OmitWhenNothing enc v) (const SNothing) x

encodeScript
    :: IncludeCbor
    -> Al.AlonzoScript DijkstraEra
    -> Json
encodeScript opts = encodeObject . \case
    Al.NativeScript nativeScript ->
        "language" .=
            encodeText "native" <>
        "json" .=
            encodeNativeScript nativeScript <>
        if includeScriptCbor opts then
            "cbor" .=
                encodeByteStringBase16 (Ledger.originalBytes nativeScript)
        else
            mempty
    Al.PlutusScript script ->
        "language" .=
            encodeText (Alonzo.stringifyLanguage (Al.plutusScriptLanguage script)) <>
        "cbor" .=
            encodeByteStringBase16 (Ledger.originalBytes (Al.plutusScriptBinary script))

encodeScriptPurposeIndex
    :: Di.DijkstraPlutusPurpose AsIx DijkstraEra
    -> Json
encodeScriptPurposeIndex = \case
    Di.DijkstraSpending ix ->
        translate (Al.AlonzoSpending ix)
    Di.DijkstraMinting ix ->
        translate (Al.AlonzoMinting ix)
    Di.DijkstraCertifying (AsIx (AsIx -> ix)) ->
        translate (Al.AlonzoCertifying ix)
    Di.DijkstraRewarding ix ->
        translate (Al.AlonzoRewarding ix)
    Di.DijkstraVoting (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "vote"
            )
    Di.DijkstraProposing (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "propose"
            )
    Di.DijkstraGuarding (AsIx ix) ->
        encodeObject
            ( "index" .=
                encodeWord32 ix
           <> "purpose" .=
                encodeText "guard"
            )
  where
    translate = Alonzo.encodeScriptPurposeIndex @AlonzoEra

encodeScriptPurposeItem
    :: Di.DijkstraPlutusPurpose AsItem DijkstraEra
    -> Json
encodeScriptPurposeItem = encodeObject . \case
    Di.DijkstraSpending (Cn.ConwaySpending -> item) -> asPreviousEra item
    Di.DijkstraMinting (Cn.ConwayMinting -> item) -> asPreviousEra item
    Di.DijkstraRewarding (Cn.ConwayRewarding -> item) -> asPreviousEra item
    Di.DijkstraVoting (Cn.ConwayVoting -> item) -> asPreviousEra item
    Di.DijkstraProposing (Cn.ConwayProposing -> item) -> asPreviousEra item
    Di.DijkstraCertifying (Cn.ConwayCertifying -> item) -> asPreviousEra item
    Di.DijkstraGuarding (AsItem scriptHash) ->
        "purpose" .= encodeText "guard" <>
        "script" .= Shelley.encodeScriptHash scriptHash
  where
    asPreviousEra = Conway.encodeScriptPurposeItem encodeTxCert encodePParamsUpdate

encodeTx
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.Tx Ledger.TopTx DijkstraEra
    -> Json
encodeTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @DijkstraEra (x ^. Ledger.bodyTxL))
       <>
        "spends" .= Alonzo.encodeIsValid (x ^. Ledger.isValidTxL)
       <>
        encodeTxBody (fmt, opts) (x ^. Ledger.bodyTxL) (strictMaybe mempty (Map.keys . snd) auxiliary)
       <>
        "metadata" .=? OmitWhenNothing fst auxiliary
       <>
        Alonzo.encodeWitnessSet (snd <$> auxiliary) encodeScriptPurposeIndex (encodeScript opts) (x ^. Ledger.witsTxL)
       <>
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (encodeCbor @ConwayEra x)
        else
           mempty
        )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash . Ledger.hashTxAuxData <$> (x ^. Ledger.auxDataTxL)
        (labels, scripts) <- Alonzo.encodeAuxiliaryData (fmt, opts) <$> x ^. Ledger.auxDataTxL
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.TxBody Ledger.TopTx DijkstraEra
    -> [Ledger.ScriptHash]
    -> Series
encodeTxBody (fmt, opts) x scripts =
    encodeSharedTxBody opts x scripts <>
    "collaterals" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (x ^. Ledger.collateralInputsTxBodyL) <>
    "collateralReturn" .=? OmitWhenNothing
        (encodeObject . Babbage.encodeTxOut (encodeScript opts)) (x ^. Ledger.collateralReturnTxBodyL) <>
    "totalCollateral" .=? OmitWhenNothing
        encodeCoin (x ^. Ledger.totalCollateralTxBodyL) <>
    "fee" .=
        encodeCoin (x ^. Ledger.feeTxBodyL) <>
    "subTransactions" .=? OmitWhen null
        (encodeFoldable (encodeSubTx (fmt, opts)))
        (x ^. Di.subTransactionsTxBodyL)

-- | Encode a sub-transaction. Unlike top-level transactions, sub-transactions
-- have no 'isValid'/'spends' field, no fee, and no collateral.
encodeSubTx
    :: (MetadataFormat, IncludeCbor)
    -> Ledger.Tx Ledger.SubTx DijkstraEra
    -> Json
encodeSubTx (fmt, opts) x =
    encodeObject
        ( Shelley.encodeTxId (Ledger.txIdTxBody @DijkstraEra (x ^. Ledger.bodyTxL))
       <>
        encodeSubTxBody opts (x ^. Ledger.bodyTxL) (strictMaybe mempty (Map.keys . snd) auxiliary)
       <>
        "metadata" .=? OmitWhenNothing fst auxiliary
       <>
        Alonzo.encodeWitnessSet (snd <$> auxiliary) encodeScriptPurposeIndex (encodeScript opts) (x ^. Ledger.witsTxL)
       <>
        -- NOTE: Using @DijkstraEra (not @ConwayEra as in encodeTx) because
        -- sub-transactions are a Dijkstra-only feature.
        if includeTransactionCbor opts then
           "cbor" .= encodeByteStringBase16 (encodeCbor @DijkstraEra x)
        else
           mempty
        )
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash . Ledger.hashTxAuxData <$> (x ^. Ledger.auxDataTxL)
        (labels, scripts) <- Alonzo.encodeAuxiliaryData (fmt, opts) <$> x ^. Ledger.auxDataTxL
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeSubTxBody
    :: IncludeCbor
    -> Ledger.TxBody Ledger.SubTx DijkstraEra
    -> [Ledger.ScriptHash]
    -> Series
encodeSubTxBody opts x scripts =
    encodeSharedTxBody opts x scripts <>
    "requiredTopLevelGuards" .=? OmitWhen null
        (encodeMapAsList encodeGuardEntry)
        (x ^. Di.requiredTopLevelGuardsL)
  where
    encodeGuardEntry credential mData =
        encodeObject
            ( "credential" `Shelley.encodeCredential` credential
           <> "datum" .=? OmitWhenNothing
                (Alonzo.encodeData @DijkstraEra) mData
            )

-- | Encode TxBody fields shared between TopTx and SubTx.
encodeSharedTxBody
    :: IncludeCbor
    -> Ledger.TxBody l DijkstraEra
    -> [Ledger.ScriptHash]
    -> Series
encodeSharedTxBody opts x scripts =
    "inputs" .=
        encodeFoldable (encodeObject . Shelley.encodeTxIn) (x ^. Ledger.inputsTxBodyL) <>
    "references" .=? OmitWhen null
        (encodeFoldable (encodeObject . Shelley.encodeTxIn)) (x ^. Ledger.referenceInputsTxBodyL) <>
    "outputs" .=
        encodeFoldable (encodeObject . Babbage.encodeTxOut (encodeScript opts)) (x ^. Ledger.outputsTxBodyL) <>
    "certificates" .=? OmitWhen null
        (encodeConcatNonEmptyFoldable (fmap encodeObject . encodeTxCert)) (x ^. Ledger.certsTxBodyL) <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
        Shelley.encodeWdrl (x ^. Ledger.withdrawalsTxBodyL) <>
    "mint" .=? OmitWhen (== mempty)
        (encodeObject . Mary.encodeMultiAsset) (x ^. Ledger.mintTxBodyL) <>
    "guards" .=? OmitWhen null
        (encodeFoldable Shelley.encodeCredentialRaw) (x ^. Di.guardsTxBodyL) <>
    "requiredExtraScripts" .=? OmitWhen null
        (encodeFoldable Shelley.encodeScriptHash) scripts <>
    "network" .=? OmitWhenNothing
        Shelley.encodeNetwork (x ^. Ledger.networkIdTxBodyL) <>
    "scriptIntegrityHash" .=? OmitWhenNothing
        Alonzo.encodeScriptIntegrityHash (x ^. Ledger.scriptIntegrityHashTxBodyL) <>
    "validityInterval" .=
        Allegra.encodeValidityInterval (x ^. Ledger.vldtTxBodyL) <>
    "proposals" .=? OmitWhen null
        (encodeFoldable (encodeObject . Conway.encodeProposalProcedure encodePParamsUpdate))
        (x ^. Ledger.proposalProceduresTxBodyL) <>
    "votes" .=? OmitWhen (null . Ledger.unVotingProcedures)
        Conway.encodeVotingProcedures
        (x ^. Ledger.votingProceduresTxBodyL) <>
    "treasury" .=? OmitWhen (\_ -> isSNothing treasuryValue && treasuryDonation == mempty)
        identity
        (encodeObject
            ( "value" .=? OmitWhenNothing
                encodeCoin treasuryValue
           <> "donation" .=? OmitWhen (== mempty)
                encodeCoin treasuryDonation
            )
        ) <>
    "directDeposits" .=? OmitWhen (null . unDirectDeposits)
        (encodeMap Shelley.stringifyRewardAcnt encodeCoin . unDirectDeposits)
        (x ^. Di.directDepositsTxBodyL) <>
    "accountBalanceIntervals" .=? OmitWhen (null . Di.unAccountBalanceIntervals)
        encodeAccountBalanceIntervals
        (x ^. Di.accountBalanceIntervalsTxBodyL)
  where
    treasuryValue =
        x ^. Ledger.currentTreasuryValueTxBodyL

    treasuryDonation =
        x ^. Ledger.treasuryDonationTxBodyL

encodeTxCert
    :: Di.DijkstraTxCert DijkstraEra
    -> NonEmpty Series
encodeTxCert = \case
    Di.DijkstraTxCertDeleg dCert ->
        encodeDelegCert dCert
    Di.DijkstraTxCertPool pCert ->
        Shelley.encodePoolCert pCert :| []
    Di.DijkstraTxCertGov cCert ->
        Conway.encodeConwayGovCert cCert :| []

encodeUtxo :: Sh.UTxO DijkstraEra -> Json
encodeUtxo =
    encodeList id
    . Map.foldrWithKey (\i o -> (:) (encodeObject (Shelley.encodeTxIn i <> encodeTxOut o))) []
    . Sh.unUTxO
  where
    encodeTxOut = Babbage.encodeTxOut (encodeScript includeAllCbor)

encodeAccountBalanceIntervals
    :: Di.AccountBalanceIntervals DijkstraEra
    -> Json
encodeAccountBalanceIntervals =
    encodeMapAsList encodeEntry . Di.unAccountBalanceIntervals
  where
    encodeEntry accountId interval =
        encodeObject
            ( "account" `Shelley.encodeCredential` Ledger.unAccountId accountId
           <> encodeAccountBalanceInterval interval
            )

encodeAccountBalanceInterval
    :: Di.AccountBalanceInterval DijkstraEra
    -> Series
encodeAccountBalanceInterval = \case
    Di.AccountBalanceLowerBound (Inclusive lo) ->
        "lowerBound" .= encodeCoin lo
    Di.AccountBalanceUpperBound (Exclusive hi) ->
        "upperBound" .= encodeCoin hi
    Di.AccountBalanceBothBounds (Inclusive lo) (Exclusive hi) ->
        "lowerBound" .= encodeCoin lo <>
        "upperBound" .= encodeCoin hi

humanReadablePurpose
    :: PlutusPurpose AsIx DijkstraEra
    -> (Text, Word32)
humanReadablePurpose = \case
    Di.DijkstraSpending ix -> Conway.humanReadablePurpose (Cn.ConwaySpending ix)
    Di.DijkstraMinting ix -> Conway.humanReadablePurpose (Cn.ConwayMinting ix)
    Di.DijkstraCertifying (AsIx ix) -> Conway.humanReadablePurpose (Cn.ConwayCertifying (AsIx ix))
    Di.DijkstraRewarding ix -> Conway.humanReadablePurpose (Cn.ConwayRewarding ix)
    Di.DijkstraVoting ix -> Conway.humanReadablePurpose (Cn.ConwayVoting ix)
    Di.DijkstraProposing (AsIx ix) -> Conway.humanReadablePurpose (Cn.ConwayProposing (AsIx ix))
    Di.DijkstraGuarding (AsIx ix) -> ("guarding script", ix)
