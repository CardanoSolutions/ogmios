--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize' )
import Cardano.Ledger.Crypto
    ( Crypto )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.MemoBytes
    ( memobytes )
import GHC.Records
    ( getField )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Prettyprinter
    ( pretty )

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.API as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Sh

import qualified Cardano.Ledger.Alonzo.Data as Al
import qualified Cardano.Ledger.Alonzo.Genesis as Al
import qualified Cardano.Ledger.Alonzo.Language as Al
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Tx as Al
import qualified Cardano.Ledger.Alonzo.TxBody as Al
import qualified Cardano.Ledger.Alonzo.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.TxSeq as Al
import qualified Cardano.Ledger.Alonzo.TxWitness as Al

import qualified Cardano.Ledger.Alonzo.Tools as Al.Tools

--
-- Encoders
--

encodeAlonzoPredFail
    :: Crypto crypto
    => Al.AlonzoPredFail (AlonzoEra crypto)
    -> Json
encodeAlonzoPredFail = \case
    Al.MissingRedeemers missing ->
        encodeObject
            [ ( "missingRequiredRedeemers", encodeObject
                [ ( "missing", encodeFoldable encodeMissingRedeemer missing)
                ]
              )
            ]
    Al.MissingRequiredDatums missing provided ->
        encodeObject
            [ ( "missingRequiredDatums", encodeObject
                [ ( "provided", encodeFoldable encodeDataHash provided )
                , ( "missing", encodeFoldable encodeDataHash missing )
                ]
              )
            ]
    Al.NonOutputSupplimentaryDatums unallowed acceptable ->
        encodeObject
            [ ( "unspendableDatums", encodeObject
                [ ( "nonSpendable", encodeFoldable encodeDataHash unallowed )
                , ( "acceptable", encodeFoldable encodeDataHash acceptable )
                ]
              )
            ]
    Al.PPViewHashesDontMatch provided inferred ->
        encodeObject
            [ ( "extraDataMismatch", encodeObject
                [ ( "provided", encodeStrictMaybe encodeScriptIntegrityHash provided )
                , ( "inferredFromParameters", encodeStrictMaybe encodeScriptIntegrityHash inferred )
                ]
              )
            ]
    Al.MissingRequiredSigners keys ->
        encodeObject
            [ ( "missingRequiredSignatures"
              , encodeFoldable Shelley.encodeKeyHash keys
              )
            ]
    Al.UnspendableUTxONoDatumHash utxos ->
        encodeObject
            [ ( "unspendableScriptInputs"
              , encodeFoldable Shelley.encodeTxIn utxos
              )
            ]
    Al.ExtraRedeemers redeemers ->
        encodeObject
            [ ( "extraRedeemers"
              , encodeFoldable (encodeText . stringifyRdmrPtr) redeemers
              )
            ]
    Al.WrappedShelleyEraFailure e ->
        Shelley.encodeUtxowFailure  encodeUtxoFailure e

encodeAuxiliaryData
    :: Crypto crypto
    => Al.AuxiliaryData (AlonzoEra crypto)
    -> Json
encodeAuxiliaryData (Al.AuxiliaryData blob scripts) = encodeObject
    [ ( "blob"
      , Shelley.encodeMetadataBlob blob
      )
    , ( "scripts"
      , encodeFoldable encodeScript scripts
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (AlonzoEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Al.txSeqTxns txs)
      )
    , ( "header"
      , Shelley.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeCollectError
    :: Crypto crypto
    => Al.CollectError crypto
    -> Json
encodeCollectError = \case
    Al.NoRedeemer purpose ->
        encodeObject [ ( "noRedeemer", encodeScriptPurpose purpose ) ]
    Al.NoWitness hash ->
        encodeObject [ ( "noWitness", Shelley.encodeScriptHash hash ) ]
    Al.NoCostModel lang ->
        encodeObject [ ( "noCostModel", encodeLanguage lang ) ]

encodeCostModel
    :: Al.CostModel
    -> Json
encodeCostModel (Al.CostModel model) =
    encodeMap id encodeInteger model

encodeData
    :: Al.Data era
    -> Json
encodeData (Al.DataConstr datum) =
    encodeShortByteString encodeByteStringBase64 (memobytes datum)

encodeDataHash
    :: Crypto crypto
    => Al.DataHash crypto
    -> Json
encodeDataHash =
    Shelley.encodeHash . Ledger.extractHash

encodeExUnits
    :: Al.ExUnits
    -> Json
encodeExUnits units =  encodeObject
    [ ( "memory", encodeNatural (Al.exUnitsMem units) )
    , ( "steps", encodeNatural (Al.exUnitsSteps units) )
    ]

encodeGenesis
    :: Al.AlonzoGenesis
    -> Json
encodeGenesis x = encodeObject
    [ ( "coinsPerUtxoWord"
      , encodeCoin (Al.coinsPerUTxOWord x)
      )
    , ( "costModels"
      , encodeMap stringifyLanguage encodeCostModel (Al.costmdls x)
      )
    , ( "prices"
      , encodePrices (Al.prices x)
      )
    , ( "maxExecutionUnitsPerTransaction"
      , encodeExUnits (Al.maxTxExUnits x)
      )
    , ( "maxExecutionUnitsPerBlock"
      , encodeExUnits (Al.maxBlockExUnits x)
      )
    , ( "maxValueSize"
      , encodeNatural (Al.maxValSize x)
      )
    , ( "collateralPercentage"
      , encodeNatural (Al.collateralPercentage x)
      )
    , ( "maxCollateralInputs"
      , encodeNatural (Al.maxCollateralInputs x)
      )
    ]

encodeIsValid
    :: Al.IsValid
    -> Json
encodeIsValid = \case
    Al.IsValid True ->
        encodeText "inputs"
    Al.IsValid False ->
        encodeText "collaterals"

encodeLanguage
    :: Al.Language
    -> Json
encodeLanguage =
    encodeText . stringifyLanguage

encodeLedgerFailure
    :: Crypto crypto
    => Sh.LedgerPredicateFailure (AlonzoEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e ->
        encodeAlonzoPredFail e
    Sh.DelegsFailure e ->
        Shelley.encodeDelegsFailure e

encodeMissingRedeemer
    :: Crypto crypto
    => (Al.ScriptPurpose crypto, Sh.ScriptHash crypto)
    -> Json
encodeMissingRedeemer (purpose, hash) =
    encodeObject
        [ ( Shelley.stringifyScriptHash hash
          , encodeScriptPurpose purpose
          )
        ]

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Al.PParams' f era
    -> Json
encodePParams' encodeF x = encodeObject
    [ ( "minFeeCoefficient"
      , encodeF encodeNatural (Al._minfeeA x)
      )
    , ( "minFeeConstant"
      , encodeF encodeNatural (Al._minfeeB x)
      )
    , ( "maxBlockBodySize"
      , encodeF encodeNatural (Al._maxBBSize x)
      )
    , ( "maxBlockHeaderSize"
      , encodeF encodeNatural (Al._maxBHSize x)
      )
    , ( "maxTxSize"
      , encodeF encodeNatural (Al._maxTxSize x)
      )
    , ( "stakeKeyDeposit"
      , encodeF encodeCoin (Al._keyDeposit x)
      )
    , ( "poolDeposit"
      , encodeF encodeCoin (Al._poolDeposit x)
      )
    , ( "poolRetirementEpochBound"
      , encodeF encodeEpochNo (Al._eMax x)
      )
    , ( "desiredNumberOfPools"
      , encodeF encodeNatural (Al._nOpt x)
      )
    , ( "poolInfluence"
      , encodeF encodeNonNegativeInterval (Al._a0 x)
      )
    , ( "monetaryExpansion"
      , encodeF encodeUnitInterval (Al._rho x)
      )
    , ( "treasuryExpansion"
      , encodeF encodeUnitInterval (Al._tau x)
      )
    , ( "decentralizationParameter"
      , encodeF encodeUnitInterval (Al._d x)
      )
    , ( "extraEntropy"
      , encodeF Shelley.encodeNonce (Al._extraEntropy x)
      )
    , ( "protocolVersion"
      , encodeF Shelley.encodeProtVer (Al._protocolVersion x)
      )
    , ( "minPoolCost"
      , encodeF encodeCoin (Al._minPoolCost x)
      )
    , ( "coinsPerUtxoWord"
      , encodeF encodeCoin (Al._coinsPerUTxOWord x)
      )
    , ( "costModels"
      , encodeF (encodeMap stringifyLanguage encodeCostModel) (Al._costmdls x)
      )
    , ( "prices"
      , encodeF encodePrices (Al._prices x)
      )
    , ( "maxExecutionUnitsPerTransaction"
      , encodeF encodeExUnits (Al._maxTxExUnits x)
      )
    , ( "maxExecutionUnitsPerBlock"
      , encodeF encodeExUnits (Al._maxBlockExUnits x)
      )
    , ( "maxValueSize"
      , encodeF encodeNatural (Al._maxValSize x)
      )
    , ( "collateralPercentage"
      , encodeF encodeNatural (Al._collateralPercentage x)
      )
    , ( "maxCollateralInputs"
      , encodeF encodeNatural (Al._maxCollateralInputs x)
      )
    ]

encodePrices
    :: Al.Prices
    -> Json
encodePrices prices =  encodeObject
    [ ( "memory", encodeNonNegativeInterval (Al.prMem prices) )
    , ( "steps", encodeNonNegativeInterval (Al.prSteps prices) )
    ]

encodeProposedPPUpdates
    :: Crypto crypto
    => Sh.ProposedPPUpdates (AlonzoEra crypto)
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap Shelley.stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodeRedeemers
    :: Crypto crypto
    => Al.Redeemers (AlonzoEra crypto)
    -> Json
encodeRedeemers (Al.Redeemers redeemers) =
    encodeMap stringifyRdmrPtr encodeDataAndUnits redeemers
  where
    encodeDataAndUnits
        :: (Al.Data era, Al.ExUnits)
        -> Json
    encodeDataAndUnits (redeemer, units) = encodeObject
        [ ( "redeemer", encodeData redeemer )
        , ( "executionUnits", encodeExUnits units )
        ]

encodeScript
    :: Crypto crypto
    => Al.Script (AlonzoEra crypto)
    -> Json
encodeScript = \case
    Al.TimelockScript nativeScript -> encodeObject
        [ ( "native"
          , Allegra.encodeTimelock nativeScript
          )
        ]
    Al.PlutusScript lang serializedScript -> encodeObject
        [ ( stringifyLanguage lang
          , encodeShortByteString encodeByteStringBase64 serializedScript
          )
        ]

encodeScriptPurpose
    :: Crypto crypto
    => Al.ScriptPurpose crypto
    -> Json
encodeScriptPurpose = \case
    Al.Spending txIn ->
        encodeObject [ ( "spend", Shelley.encodeTxIn txIn ) ]
    Al.Minting policyId ->
        encodeObject [ ( "mint", Mary.encodePolicyId policyId ) ]
    Al.Certifying cert ->
        encodeObject [ ( "certificate", Shelley.encodeDCert cert ) ]
    Al.Rewarding acct ->
        encodeObject [ ( "withdrawal", Shelley.encodeRewardAcnt acct ) ]

encodeTx
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Al.ValidatedTx (AlonzoEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Shelley.encodeTxId (Ledger.txid @(AlonzoEra crypto) (Al.body x))
      )
    , ( "body"
      , encodeTxBody (Al.body x)
      )
    , ( "metadata"
      , (,) <$> fmap (("hash",) . Shelley.encodeAuxiliaryDataHash) (adHash (Al.body x))
            <*> fmap (("body",) . encodeAuxiliaryData) (Al.auxiliaryData x)
        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
      )
    , ( "inputSource"
      , encodeIsValid (Al.isValid x)
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Al.wits x)
      )
    , ( "raw"
      , encodeByteStringBase64 (serialize' x)
      )
    ]
  where
    adHash :: Al.TxBody era -> StrictMaybe (Al.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => Al.TxBody (AlonzoEra crypto)
    -> Json
encodeTxBody x = encodeObject
    [ ( "inputs"
      , encodeFoldable Shelley.encodeTxIn (Al.inputs x)
      )
    , ( "collaterals"
      , encodeFoldable Shelley.encodeTxIn (Al.collateral x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (Al.outputs x)
      )
    , ( "certificates"
      , encodeFoldable Shelley.encodeDCert (Al.txcerts x)
      )
    , ( "withdrawals"
      , Shelley.encodeWdrl (Al.txwdrls x)
      )
    , ( "fee"
      , encodeCoin (Al.txfee x)
      )
    , ( "validityInterval"
      , Allegra.encodeValidityInterval (Al.txvldt x)
      )
    , ( "update"
      , encodeStrictMaybe encodeUpdate (Al.txUpdates x)
      )
    , ( "mint"
      , Mary.encodeValue (Al.mint x)
      )
    , ( "network"
      , encodeStrictMaybe Shelley.encodeNetwork (Al.txnetworkid x)
      )
    , ( "scriptIntegrityHash"
      , encodeStrictMaybe encodeScriptIntegrityHash (Al.scriptIntegrityHash x)
      )
    , ( "requiredExtraSignatures"
      , encodeFoldable Shelley.encodeKeyHash (Al.reqSignerHashes x)
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Al.TxOut (AlonzoEra crypto)
    -> Json
encodeTxOut (Al.TxOut addr value datum) = encodeObject
    [ ( "address"
      , Shelley.encodeAddress addr
      )
    , ( "value"
      , Mary.encodeValue value
      )
    , ( "datum"
      , encodeStrictMaybe encodeDataHash datum
      )
    ]

encodeUpdate
    :: Crypto crypto
    => Sh.Update (AlonzoEra crypto)
    -> Json
encodeUpdate (Sh.Update update epoch) = encodeObject
    [ ( "proposal"
      , encodeProposedPPUpdates update
      )
    , ( "epoch"
      , encodeEpochNo epoch
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AlonzoEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Sh.UTxO (AlonzoEra crypto)
    -> Json
encodeUtxoWithMode mode =
    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)

encodeUtxoFailure
    :: forall crypto. Crypto crypto
    => Al.UtxoPredicateFailure (AlonzoEra crypto)
    -> Json
encodeUtxoFailure = \case
    Al.BadInputsUTxO inputs ->
        encodeObject
            [ ( "badInputs"
              , encodeFoldable Shelley.encodeTxIn inputs
              )
            ]
    Al.OutsideValidityIntervalUTxO itv currentSlot ->
        encodeObject
            [ ( "outsideOfValidityInterval", encodeObject
                [ ( "interval" , Allegra.encodeValidityInterval itv )
                , ( "currentSlot" , encodeSlotNo currentSlot )
                ]
              )
            ]
    Al.MaxTxSizeUTxO actualSize maxSize ->
        encodeObject
            [ ( "txTooLarge", encodeObject
                [ ( "maximumSize", encodeInteger maxSize )
                , ( "actualSize", encodeInteger actualSize )
                ]
              )
            ]
    Al.InputSetEmptyUTxO ->
        encodeObject
            [ ( "missingAtLeastOneInputUtxo", encodeNull )
            ]
    Al.FeeTooSmallUTxO required actual ->
        encodeObject
            [ ( "feeTooSmall", encodeObject
                [ ( "requiredFee", encodeCoin required )
                , ( "actualFee", encodeCoin actual )
                ]
              )
            ]
    Al.ValueNotConservedUTxO consumed produced ->
        encodeObject
            [ ( "valueNotConserved", encodeObject
                [ ( "consumed", Mary.encodeValue consumed )
                , ( "produced", Mary.encodeValue produced )
                ]
              )
            ]
    Al.WrongNetwork expected invalidAddrs ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Shelley.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
                  )
                ]
              )
            ]
    Al.WrongNetworkWithdrawal expected invalidAccts ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Shelley.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Shelley.encodeEntities "rewardAccount" Shelley.encodeRewardAcnt invalidAccts
                  )
                ]
              )
            ]
    Al.WrongNetworkInTxBody expected actual ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Shelley.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Shelley.encodeEntities "transactionBody" Shelley.encodeNetwork [actual]
                  )
                ]
              )
            ]
    Al.OutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeFoldable encodeTxOut outs
              )
            ]
    Al.OutputBootAddrAttrsTooBig outs ->
        encodeObject
            [ ( "addressAttributesTooLarge"
              , encodeFoldable Shelley.encodeAddress ((\(Al.TxOut addr _ _) -> addr) <$> outs)
              )
            ]
    Al.TriesToForgeADA ->
        encodeObject
            [ ( "triesToForgeAda", encodeNull )
            ]
    Al.OutputTooBigUTxO outs ->
        encodeObject
            [ ( "tooManyAssetsInOutput"
              , encodeFoldable (\(_, _, o) -> encodeTxOut o)  outs
              )
            ]
    Al.NoCollateralInputs ->
        encodeObject
            [ ( "missingCollateralInputs", encodeNull )
            ]
    Al.InsufficientCollateral actual required ->
        encodeObject
            [ ( "collateralTooSmall", encodeObject
                [ ( "requiredCollateral", encodeCoin required )
                , ( "actualCollateral", encodeCoin actual )
                ]
              )
            ]
    Al.ScriptsNotPaidUTxO utxo ->
        encodeObject
            [ ( "collateralIsScript", encodeUtxo utxo )
            ]
    Al.CollateralContainsNonADA value ->
        encodeObject
            [ ( "collateralHasNonAdaAssets", Mary.encodeValue value )
            ]
    Al.TooManyCollateralInputs maxInputs actualInputs ->
        encodeObject
            [ ( "tooManyCollateralInputs", encodeObject
                [ ( "maximumCollateralInputs", encodeNatural maxInputs )
                , ( "actualCollateralInputs", encodeNatural actualInputs )
                ]
              )
            ]
    Al.ExUnitsTooBigUTxO maxUnit actualUnit ->
        encodeObject
            [ ( "executionUnitsTooLarge", encodeObject
                [ ( "maximumExecutionUnits", encodeExUnits maxUnit )
                , ( "actualExecutionUnits", encodeExUnits actualUnit )
                ]
              )
            ]
    Al.OutsideForecast slot ->
        encodeObject
            [ ( "outsideForecast", encodeSlotNo slot )
            ]
    Al.UtxosFailure e ->
        encodeUtxosPredicateFailure e

encodeUtxosPredicateFailure
    :: Crypto crypto
    => Al.UtxosPredicateFailure (AlonzoEra crypto)
    -> Json
encodeUtxosPredicateFailure = \case
    Al.ValidationTagMismatch{} ->
        encodeObject
            [ ( "validationTagMismatch", encodeNull )
            ]
    Al.CollectErrors errors ->
        encodeObject
            [ ( "collectErrors", encodeFoldable encodeCollectError errors )
            ]
    Al.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

encodeScriptIntegrityHash
    :: Crypto crypto
    => Al.ScriptIntegrityHash crypto
    -> Json
encodeScriptIntegrityHash =
    Shelley.encodeHash . Ledger.extractHash

encodeScriptFailure
    :: Crypto crypto
    => Al.Tools.ScriptFailure crypto
    -> Json
encodeScriptFailure = \case
    Al.Tools.RedeemerNotNeeded ptr ->
        encodeObject
            [ ( "extraRedeemers"
              , encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
              )
            ]
    Al.Tools.MissingScript ptr ->
        encodeObject
            [ ( "missingRequiredScripts"
              , encodeObject
                  [ ( "missing"
                    , encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
                    )
                  ]
              )
            ]
    Al.Tools.MissingDatum h ->
        encodeObject
            [ ( "missingRequiredDatums"
              , encodeObject
                  [ ( "missing"
                    , encodeFoldable encodeDataHash [h]
                    )
                  ]
              )
            ]
    Al.Tools.ValidationFailedV1 err traces ->
        encodeObject
            [ ( "validatorFailed"
              , encodeObject
                  [ ( "error"
                    , encodeText (show (pretty err))
                    )
                  , ( "traces"
                    , encodeFoldable encodeText traces
                    )
                  ]
              )
            ]
    Al.Tools.ValidationFailedV2 err traces ->
        encodeObject
            [ ( "validatorFailed"
              , encodeObject
                  [ ( "error"
                    , encodeText (show (pretty err))
                    )
                  , ( "traces"
                    , encodeFoldable encodeText traces
                    )
                  ]
              )
            ]
    Al.Tools.UnknownTxIn i ->
        encodeObject
            [ ( "unknownInputReferencedByRedeemer"
              , Shelley.encodeTxIn i
              )
            ]
    Al.Tools.InvalidTxIn i ->
        encodeObject
            [ ( "nonScriptInputReferencedByRedeemer"
              , Shelley.encodeTxIn i
              )
            ]
    Al.Tools.IncompatibleBudget budget ->
        encodeObject
            [ ( "illFormedExecutionBudget"
              , encodeMaybe encodeExUnits (Al.exBudgetToExUnits budget)
              )
            ]
    Al.Tools.NoCostModel lang ->
        encodeObject
            [ ( "noCostModelForLanguage"
              , encodeLanguage lang
              )
            ]

encodeWitnessSet
    :: Crypto crypto
    => Al.TxWitness (AlonzoEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , Shelley.encodeWitVKeys (Al.txwitsVKey x)
      )
    , ( "scripts"
      , encodeMap Shelley.stringifyScriptHash encodeScript (Al.txscripts x)
      )
    , ( "datums"
      , encodeMap stringifyDataHash encodeData (Al.unTxDats $ Al.txdats x)
      )
    , ( "redeemers"
      , encodeRedeemers (Al.txrdmrs x)
      )
    , ( "bootstrap"
      , encodeFoldable Shelley.encodeBootstrapWitness (Al.txwitsBoot x)
      )
    ]

--
-- Conversion To Text
--

stringifyDataHash
    :: Crypto crypto
    => Al.DataHash crypto
    -> Text
stringifyDataHash (Ledger.extractHash -> (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyLanguage
    :: Al.Language
    -> Text
stringifyLanguage = \case
    Al.PlutusV1 -> "plutus:v1"
    Al.PlutusV2 -> "plutus:v2"

stringifyRdmrPtr
    :: Al.RdmrPtr
    -> Text
stringifyRdmrPtr (Al.RdmrPtr tag ptr) =
    stringifyTag tag <> ":" <> show ptr
  where
    stringifyTag
        :: Al.Tag
        -> Text
    stringifyTag = \case
        Al.Spend -> "spend"
        Al.Mint -> "mint"
        Al.Cert -> "certificate"
        Al.Rewrd -> "withdrawal"
