--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}
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

      -- ** SubmitTx
    , SubmitTx (..)
    , _decodeSubmitTx
    , SubmitTxResponse
    , _encodeSubmitTxResponse

      -- ** EvaluateTx
    , EvaluateTx (..)
    , _decodeEvaluateTx
    , EvaluateTxResponse (..)
    , EvaluateTxError (..)
    , evaluateExecutionUnits
    , incompatibleEra
    , _encodeEvaluateTxResponse

      -- ** Re-exports
    , AlonzoEra
    , EpochInfo
    , PastHorizonException
    , ExUnits
    , PParams
    , RdmrPtr
    , ScriptFailure
    , SystemStart
    , Tx
    , TxIn
    , UTxO
    ) where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Protocol
    ()

import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Alonzo.PParams
    ( PParams' (_costmdls) )
import Cardano.Ledger.Alonzo.Scripts
    ( ExUnits (..) )
import Cardano.Ledger.Alonzo.Tools
    ( BasicFailure (..), ScriptFailure, evaluateTransactionExecutionUnits )
import Cardano.Ledger.Alonzo.TxWitness
    ( RdmrPtr )
import Cardano.Ledger.Core
    ( PParams, Tx )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..) )
import Cardano.Ledger.TxIn
    ( TxIn )
import Cardano.Network.Protocol.NodeToClient
    ( Crypto, SerializedTx, SubmitTxError )
import Cardano.Slotting.EpochInfo
    ( EpochInfo )
import Cardano.Slotting.Time
    ( SystemStart )
import Control.Monad.Trans.Except
    ( Except )
import Ouroboros.Consensus.HardFork.History
    ( PastHorizonException )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map

--
-- Codecs
--

data TxSubmissionCodecs block = TxSubmissionCodecs
    { decodeSubmitTx
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
    :: forall block. (FromJSON (SerializedTx block))
    => (SubmitTxError block -> Json)
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (ScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs encodeSubmitTxError stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn =
    TxSubmissionCodecs
        { decodeSubmitTx =
            decodeWith _decodeSubmitTx
        , encodeSubmitTxResponse =
            _encodeSubmitTxResponse (Proxy @block)
                encodeSubmitTxError
        , decodeEvaluateTx =
            decodeWith _decodeEvaluateTx
        , encodeEvaluateTxResponse =
            _encodeEvaluateTxResponse (Proxy @block)
                stringifyRdmrPtr
                encodeExUnits
                encodeScriptFailure
                encodeTxIn
        }

--
-- Messages
--

data TxSubmissionMessage block
    = MsgSubmitTx
        (SubmitTx block)
        (Wsp.ToResponse (SubmitTxResponse block))
        Wsp.ToFault
    | MsgEvaluateTx
        (EvaluateTx block)
        (Wsp.ToResponse (EvaluateTxResponse block))
        Wsp.ToFault

--
-- SubmitTx
--

data SubmitTx block
    = SubmitTx { submit :: SerializedTx block }
    deriving (Generic)
deriving instance Show (SerializedTx block) => Show (SubmitTx block)

data BackwardCompatibleSubmitTx block
    = BackwardCompatibleSubmitTx { bytes :: SerializedTx block }
    deriving (Generic)

_decodeSubmitTx
    :: FromJSON (SerializedTx block)
    => Json.Value
    -> Json.Parser (Wsp.Request (SubmitTx block))
_decodeSubmitTx value =
    fmap (fmap backwardCompatible) (Wsp.genericFromJSON Wsp.defaultOptions value)
  <|>
    Wsp.genericFromJSON Wsp.defaultOptions value
  where
    backwardCompatible :: forall block. BackwardCompatibleSubmitTx block -> SubmitTx block
    backwardCompatible (BackwardCompatibleSubmitTx tx) = SubmitTx tx

type SubmitTxResponse block = SubmitResult (SubmitTxError block)

_encodeSubmitTxResponse
    :: forall block. ()
    => Proxy block
    -> (SubmitTxError block -> Json)
    -> Wsp.Response (SubmitTxResponse block)
    -> Json
_encodeSubmitTxResponse _proxy encodeSubmitTxError =
    Wsp.mkResponse Wsp.defaultOptions proxy $ \case
        SubmitSuccess ->
            encodeText "SubmitSuccess"
        (SubmitFail e) ->
            encodeObject [ ( "SubmitFail", encodeSubmitTxError e ) ]
  where
    proxy = Proxy @(Wsp.Request (SubmitTx block))

--
-- EvaluateTx
--

data EvaluateTx block
    = EvaluateTx { evaluate :: SerializedTx block }
    deriving (Generic)
deriving instance Show (SerializedTx block) => Show (EvaluateTx block)

_decodeEvaluateTx
    :: FromJSON (SerializedTx block)
    => Json.Value
    -> Json.Parser (Wsp.Request (EvaluateTx block))
_decodeEvaluateTx =
    Wsp.genericFromJSON Wsp.defaultOptions

data EvaluateTxResponse block
    = EvaluationFailure (EvaluateTxError block)
    | EvaluationResult (Map RdmrPtr ExUnits)
    deriving (Show)

data EvaluateTxError block
    = EvaluateTxScriptFailures (Map RdmrPtr (ScriptFailure (Crypto block)))
    | EvaluateTxUnknownInputs (Set (TxIn (Crypto block)))
    | EvaluateTxIncompatibleEra Text
    | EvaluateTxUncomputableSlotArithmetic PastHorizonException
    deriving (Show)

-- | Shorthand constructor for 'EvaluateTxResponse'
incompatibleEra :: Text -> EvaluateTxResponse block
incompatibleEra = EvaluationFailure . EvaluateTxIncompatibleEra

_encodeEvaluateTxResponse
    :: forall block. ()
    => Proxy block
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (ScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> Wsp.Response (EvaluateTxResponse block)
    -> Json
_encodeEvaluateTxResponse _proxy stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn =
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
                  , encodeMap stringifyRdmrPtr encodeScriptFailure failures
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
  where
    proxy = Proxy @(Wsp.Request (EvaluateTx block))

-- | Evaluate script executions units for the given transaction.
evaluateExecutionUnits
    :: forall block. (Crypto block ~ StandardCrypto)
    => PParams (AlonzoEra (Crypto block))
        -- ^ Protocol parameters
    -> SystemStart
        -- ^ Start of the blockchain, for converting slots to UTC times
    -> EpochInfo (Except PastHorizonException)
        -- ^ Information about epoch sizes, for converting slots to UTC times
    -> UTxO (AlonzoEra (Crypto block))
        -- ^ A UTXO needed to resolve inputs
    -> Tx (AlonzoEra (Crypto block))
        -- ^ The actual transaction
    -> EvaluateTxResponse block
evaluateExecutionUnits pparams systemStart epochInfo utxo tx = case evaluation of
    Left pastHorizonException ->
        EvaluationFailure (EvaluateTxUncomputableSlotArithmetic pastHorizonException)
    Right (Left (UnknownTxIns inputs)) ->
        EvaluationFailure (EvaluateTxUnknownInputs inputs)
    Right (Right reports) ->
        let (failures, successes) =
                Map.foldMapWithKey aggregateReports reports
         in if null failures
            then EvaluationResult successes
            else EvaluationFailure $ EvaluateTxScriptFailures failures
  where
    aggregateReports
        :: RdmrPtr
        -> Either (ScriptFailure (Crypto block)) ExUnits
        -> (Map RdmrPtr (ScriptFailure (Crypto block)), Map RdmrPtr ExUnits)
    aggregateReports ptr = \case
        Left scriptFailure ->
            ( Map.singleton ptr scriptFailure, mempty )
        Right exUnits ->
            ( mempty, Map.singleton ptr exUnits )

    evaluation
        :: Either
              PastHorizonException
              (Either
                  (BasicFailure (Crypto block))
                  (Map RdmrPtr (Either (ScriptFailure (Crypto block)) ExUnits))
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
              (mapToArray (_costmdls pparams))
