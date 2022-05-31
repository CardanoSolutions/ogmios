--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE:
-- This module uses partial record field accessor to automatically derive
-- JSON instances from the generic data-type structure. The partial fields are
-- otherwise unused.
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

-- NOTE:
-- Needed to derive 'ToJSON' and 'Show' instances for 'SubmitResult'.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Protocol.TxSubmission
    ( -- * Codecs
      TxSubmissionCodecs (..)
    , mkTxSubmissionCodecs

      -- * Messages
    , TxSubmissionMessage (..)

      -- ** BackwardCompatibleSubmitTx
    , BackwardCompatibleSubmitTx (..)
    , _decodeBackwardCompatibleSubmitTx
    , BackwardCompatibleSubmitTxResponse
    , _encodeBackwardCompatibleSubmitTxResponse

      -- ** SubmitTx
    , SubmitTx (..)
    , _decodeSubmitTx
    , SubmitTxResponse (..)
    , _encodeSubmitTxResponse
    , mkSubmitTxResponse

      -- ** EvaluateTx
    , EvaluateTx (..)
    , _decodeEvaluateTx
    , EvaluateTxResponse (..)
    , EvaluateTxError (..)
    , evaluateExecutionUnits
    , incompatibleEra
    , notEnoughSynced
    , _encodeEvaluateTxResponse
    , CanEvaluateScriptsInEra

      -- ** Re-exports
    , AlonzoEra
    , BabbageEra
    , EpochInfo
    , ExUnits
    , GenTxId
    , HasTxId
    , PastHorizonException
    , RdmrPtr
    , ScriptFailure
    , SerializedTx
    , SubmitTxError
    , SystemStart
    , Core.PParams
    , Core.Tx
    , TxIn
    , UTxO (..)
    ) where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Protocol
    ()

import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Alonzo.Data
    ( Data )
import Cardano.Ledger.Alonzo.Scripts
    ( CostModels (..), ExUnits (..), Script )
import Cardano.Ledger.Alonzo.Tools
    ( BasicFailure (..), ScriptFailure, evaluateTransactionExecutionUnits )
import Cardano.Ledger.Alonzo.Tx
    ( DataHash )
import Cardano.Ledger.Alonzo.TxInfo
    ( ExtendedUTxO, TranslationError (..) )
import Cardano.Ledger.Alonzo.TxWitness
    ( RdmrPtr (..), Redeemers, TxDats )
import Cardano.Ledger.Babbage
    ( BabbageEra )
import Cardano.Ledger.BaseTypes
    ( ProtVer )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Era )
import Cardano.Ledger.Shelley.TxBody
    ( DCert, Wdrl )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..) )
import Cardano.Ledger.TxIn
    ( TxIn )
import Cardano.Network.Protocol.NodeToClient
    ( Crypto, GenTxId, SerializedTx, SubmitTxError )
import Cardano.Slotting.EpochInfo
    ( EpochInfo )
import Cardano.Slotting.Time
    ( SystemStart )
import Control.Monad.Trans.Except
    ( Except )
import Data.Sequence.Strict
    ( StrictSeq )
import GHC.Records
    ( HasField (..) )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO )
import Ouroboros.Consensus.HardFork.History
    ( PastHorizonException )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Era

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map

--
-- Codecs
--

data TxSubmissionCodecs block = TxSubmissionCodecs
    { decodeBackwardCompatibleSubmitTx
        :: ByteString
        -> Maybe (Wsp.Request (BackwardCompatibleSubmitTx block))
    , encodeBackwardCompatibleSubmitTxResponse
        :: Wsp.Response (BackwardCompatibleSubmitTxResponse block)
        -> Json
    , decodeSubmitTx
        :: ByteString
        -> Maybe (Wsp.Request (SubmitTx block))
    , encodeSubmitTxResponse
        :: Wsp.Response (SubmitTxResponse block)
        -> Json
    , decodeEvaluateTx
        :: ByteString
        -> Maybe (Wsp.Request (EvaluateTx block))
    , encodeEvaluateTxResponse
        :: Wsp.Response (EvaluateTxResponse block)
        -> Json
    }

mkTxSubmissionCodecs
    :: forall block.
        ( FromJSON (SerializedTx block)
        , FromJSON (MultiEraUTxO block)
        )
    => (GenTxId block -> Json)
    -> (SubmitTxError block -> Json)
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (ScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> (TranslationError -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs encodeTxId encodeSubmitTxError stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn encodeTranslationError =
    TxSubmissionCodecs
        { decodeBackwardCompatibleSubmitTx =
            decodeWith _decodeBackwardCompatibleSubmitTx
        , encodeBackwardCompatibleSubmitTxResponse =
            _encodeBackwardCompatibleSubmitTxResponse (Proxy @block)
                encodeSubmitTxError
        , decodeSubmitTx =
            decodeWith _decodeSubmitTx
        , encodeSubmitTxResponse =
            _encodeSubmitTxResponse (Proxy @block)
                encodeTxId
                encodeSubmitTxError
        , decodeEvaluateTx =
            decodeWith _decodeEvaluateTx
        , encodeEvaluateTxResponse =
            _encodeEvaluateTxResponse (Proxy @block)
                stringifyRdmrPtr
                encodeExUnits
                encodeScriptFailure
                encodeTxIn
                encodeTranslationError
        }

--
-- Messages
--

data TxSubmissionMessage block
    = MsgBackwardCompatibleSubmitTx
        (BackwardCompatibleSubmitTx block)
        (Wsp.ToResponse (BackwardCompatibleSubmitTxResponse block))
        Wsp.ToFault
    | MsgSubmitTx
        (SubmitTx block)
        (Wsp.ToResponse (SubmitTxResponse block))
        Wsp.ToFault
    | MsgEvaluateTx
        (EvaluateTx block)
        (Wsp.ToResponse (EvaluateTxResponse block))
        Wsp.ToFault

--
-- BackwardCompatibleSubmitTx
--

data BackwardCompatibleSubmitTx block
    = BackwardCompatibleSubmitTx { bytes :: SerializedTx block }
    deriving (Generic)
deriving instance Show (SerializedTx block) => Show (BackwardCompatibleSubmitTx block)

_decodeBackwardCompatibleSubmitTx
    :: FromJSON (SerializedTx block)
    => Json.Value
    -> Json.Parser (Wsp.Request (BackwardCompatibleSubmitTx block))
_decodeBackwardCompatibleSubmitTx =
    Wsp.genericFromJSON backwardCompatibleOptions

type BackwardCompatibleSubmitTxResponse block =
    SubmitResult (SubmitTxError block)

_encodeBackwardCompatibleSubmitTxResponse
    :: forall block. ()
    => Proxy block
    -> (SubmitTxError block -> Json)
    -> Wsp.Response (BackwardCompatibleSubmitTxResponse block)
    -> Json
_encodeBackwardCompatibleSubmitTxResponse _proxy encodeSubmitTxError =
    Wsp.mkResponse backwardCompatibleOptions proxy $ \case
        SubmitSuccess ->
            encodeText "SubmitSuccess"
        (SubmitFail e) ->
            encodeObject [ ( "SubmitFail", encodeSubmitTxError e ) ]
  where
    proxy = Proxy @(Wsp.Request (BackwardCompatibleSubmitTx block))

backwardCompatibleOptions :: Wsp.Options
backwardCompatibleOptions =
    Wsp.defaultOptions
        { Wsp.constructorTagModifier =
            drop (length ("BackwardCompatible" :: String))
        }

--
-- SubmitTx
--

data SubmitTx block
    = SubmitTx { submit :: SerializedTx block }
    deriving (Generic)
deriving instance Show (SerializedTx block) => Show (SubmitTx block)

_decodeSubmitTx
    :: FromJSON (SerializedTx block)
    => Json.Value
    -> Json.Parser (Wsp.Request (SubmitTx block))
_decodeSubmitTx =
    Wsp.genericFromJSON Wsp.defaultOptions

data SubmitTxResponse block
    = SubmitTxSuccess (GenTxId block)
    | SubmitTxFail (SubmitTxError block)
    deriving (Generic)
deriving instance
    ( Show (SubmitTxError block)
    , Show (GenTxId block)
    ) => Show (SubmitTxResponse block)

_encodeSubmitTxResponse
    :: forall block. ()
    => Proxy block
    -> (GenTxId block -> Json)
    -> (SubmitTxError block -> Json)
    -> Wsp.Response (SubmitTxResponse block)
    -> Json
_encodeSubmitTxResponse _proxy encodeTxId encodeSubmitTxError =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        SubmitTxSuccess i ->
            encodeObject
                [ ( "SubmitSuccess"
                  , encodeObject [ ( "txId", encodeTxId i) ]
                  )
                ]
        (SubmitTxFail e) ->
            encodeObject
                [ ( "SubmitFail"
                  , encodeSubmitTxError e
                  )
                ]
  where
    proxy = Proxy @(Wsp.Request (SubmitTx block))

-- | Translate an ouroboros-network's 'SubmitResult' into our own
-- 'SubmitTxResponse' which also carries a transaction id.
mkSubmitTxResponse
    :: HasTxId (SerializedTx block)
    => SerializedTx block
    -> SubmitResult (SubmitTxError block)
    -> SubmitTxResponse block
mkSubmitTxResponse tx = \case
    SubmitSuccess ->
        SubmitTxSuccess (txId tx)
    SubmitFail e ->
        SubmitTxFail e

--
-- EvaluateTx
--

data EvaluateTx block
    = EvaluateTx
        { evaluate :: SerializedTx block
        , additionalUtxoSet :: MultiEraUTxO block
        }
    deriving (Generic)
deriving instance
    ( Show (SerializedTx block)
    , Show (MultiEraUTxO block)
    ) => Show (EvaluateTx block)

_decodeEvaluateTx
    :: forall block.
        ( FromJSON (SerializedTx block)
        , FromJSON (MultiEraUTxO block)
        )
    => Json.Value
    -> Json.Parser (Wsp.Request (EvaluateTx block))
_decodeEvaluateTx =
    Wsp.genericFromJSON $ Wsp.defaultOptions
        { Wsp.onMissingField = \fieldName ->
            if fieldName == "additionalUtxoSet" then
                pure (Json.Array mempty)
            else
                Wsp.onMissingField Wsp.defaultOptions fieldName
        }

data EvaluateTxResponse block
    = EvaluationFailure (EvaluateTxError block)
    | EvaluationResult (Map RdmrPtr ExUnits)
    deriving (Show)

data EvaluateTxError block
    = EvaluateTxScriptFailures (Map RdmrPtr [ScriptFailure (Crypto block)])
    | EvaluateTxUnknownInputs (Set (TxIn (Crypto block)))
    | EvaluateTxIncompatibleEra Text
    | EvaluateTxUncomputableSlotArithmetic PastHorizonException
    | EvaluateTxAdditionalUtxoOverlap (Set (TxIn (Crypto block)))
    | EvaluateTxNotEnoughSynced NotEnoughSyncedError
    | EvaluateTxCannotCreateEvaluationContext TranslationError
    deriving (Show)

data NotEnoughSyncedError = NotEnoughSynced
    { currentNodeEra :: Text
    , minimumRequiredEra :: Text
    }
    deriving (Show)

-- | Shorthand constructor for 'EvaluateTxResponse'
incompatibleEra :: Text -> EvaluateTxResponse block
incompatibleEra =
    EvaluationFailure . EvaluateTxIncompatibleEra

-- | Shorthand constructor for 'EvaluateTxResponse'
notEnoughSynced :: Text -> EvaluateTxResponse block
notEnoughSynced currentNodeEra =
    EvaluationFailure (EvaluateTxNotEnoughSynced $
        NotEnoughSynced { currentNodeEra, minimumRequiredEra }
    )
  where
    minimumRequiredEra = "Alonzo"

_encodeEvaluateTxResponse
    :: forall block. ()
    => Proxy block
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (ScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> (TranslationError -> Json)
    -> Wsp.Response (EvaluateTxResponse block)
    -> Json
_encodeEvaluateTxResponse _proxy stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn encodeTranslationError =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        EvaluationResult result -> encodeObject
            [ ( "EvaluationResult"
              , encodeMap stringifyRdmrPtr encodeExUnits result
              )
            ]
        EvaluationFailure (EvaluateTxScriptFailures failures) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "ScriptFailures"
                  , encodeMap stringifyRdmrPtr (encodeList encodeScriptFailure) failures
                  )
                ]
              )
            ]
        EvaluationFailure (EvaluateTxUnknownInputs inputs) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "UnknownInputs"
                  , encodeFoldable encodeTxIn inputs
                  )
                ]
              )
            ]
        EvaluationFailure (EvaluateTxIncompatibleEra era) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "IncompatibleEra"
                  , encodeText era
                  )
                ]
              )
            ]
        EvaluationFailure (EvaluateTxUncomputableSlotArithmetic pastHorizon) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "UncomputableSlotArithmetic"
                  , encodeText (show pastHorizon)
                  )
                ]
              )
            ]
        EvaluationFailure (EvaluateTxAdditionalUtxoOverlap inputs) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "AdditionalUtxoOverlap"
                  , encodeFoldable encodeTxIn inputs
                  )
                ]
              )
            ]
        EvaluationFailure (EvaluateTxNotEnoughSynced err) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "NotEnoughSynced"
                  , encodeObject
                    [ ( "currentNodeEra", encodeText (currentNodeEra err) )
                    , ( "minimumRequiredEra", encodeText (minimumRequiredEra err) )
                    ]
                  )
                ]
              )
            ]
        EvaluationFailure (EvaluateTxCannotCreateEvaluationContext err) -> encodeObject
            [ ( "EvaluationFailure"
              , encodeObject
                [ ( "CannotCreateEvaluationContext"
                  , encodeObject
                    [ ( "reason" , encodeTranslationError err) ]
                  )
                ]
              )
            ]
  where
    proxy = Proxy @(Wsp.Request (EvaluateTx block))

-- | A constraint synonym to bundle together constraints needed to run a script
-- evaluation in any era after Alonzo (incl.).
type CanEvaluateScriptsInEra era =
      ( Era era
      , ExtendedUTxO era
      , HasField "inputs" (Core.TxBody era) (Set (TxIn (Era.Crypto era)))
      , HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Era.Crypto era)))
      , HasField "wdrls" (Core.TxBody era) (Wdrl (Era.Crypto era))
      , HasField "txdats" (Core.Witnesses era) (TxDats era)
      , HasField "txrdmrs" (Core.Witnesses era) (Redeemers era)
      , HasField "datahash" (Core.TxOut era) (StrictMaybe (DataHash (Era.Crypto era)))
      , HasField "datum" (Core.TxOut era) (StrictMaybe (Data era))
      , HasField "_maxTxExUnits" (Core.PParams era) ExUnits
      , HasField "_protocolVersion" (Core.PParams era) ProtVer
      , HasField "_costmdls" (Core.PParams era) CostModels
      , Core.Script era ~ Script era
      , Era.Crypto era ~ StandardCrypto
      )

-- | Evaluate script executions units for the given transaction.
evaluateExecutionUnits
    :: forall era block.
      ( CanEvaluateScriptsInEra era
      , Era.Crypto era ~ Crypto block
      )
    => Core.PParams era
        -- ^ Protocol parameters
    -> SystemStart
        -- ^ Start of the blockchain, for converting slots to UTC times
    -> EpochInfo (Except PastHorizonException)
        -- ^ Information about epoch sizes, for converting slots to UTC times
    -> UTxO era
        -- ^ A UTXO needed to resolve inputs
    -> Core.Tx era
        -- ^ The actual transaction
    -> EvaluateTxResponse block
evaluateExecutionUnits pparams systemStart epochInfo utxo tx = case evaluation of
    Left pastHorizonException ->
        EvaluationFailure (EvaluateTxUncomputableSlotArithmetic pastHorizonException)
    Right (Left (UnknownTxIns inputs)) ->
        EvaluationFailure (EvaluateTxUnknownInputs inputs)
    Right (Left (BadTranslation err)) ->
        EvaluationFailure (EvaluateTxCannotCreateEvaluationContext err)
    Right (Right reports) ->
        let (failures, successes) =
                Map.foldrWithKey aggregateReports (mempty, mempty)  reports
         in if null failures
            then EvaluationResult successes
            else EvaluationFailure $ EvaluateTxScriptFailures failures
  where
    aggregateReports
        :: RdmrPtr
        -> Either (ScriptFailure (Era.Crypto era)) ExUnits
        -> (Map RdmrPtr [ScriptFailure (Era.Crypto era)], Map RdmrPtr ExUnits)
        -> (Map RdmrPtr [ScriptFailure (Era.Crypto era)], Map RdmrPtr ExUnits)
    aggregateReports ptr result (failures, successes) = case result of
        Left scriptFailure ->
            ( Map.unionWith (++) (Map.singleton ptr [scriptFailure]) failures
            , successes
            )
        Right exUnits ->
            ( failures
            , Map.singleton ptr exUnits <> successes
            )

    evaluation
        :: Either
              PastHorizonException
              (Either
                  (BasicFailure (Era.Crypto era))
                  (Map RdmrPtr (Either (ScriptFailure (Era.Crypto era)) ExUnits))
              )
    evaluation =
        runIdentity $
          runExceptT $
            evaluateTransactionExecutionUnits
              pparams
              tx
              utxo
              epochInfo
              systemStart
              (mapToArray (unCostModels (getField @"_costmdls" pparams)))
