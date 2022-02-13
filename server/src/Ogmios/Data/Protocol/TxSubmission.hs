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
    , _encodeEvaluateTxResponse

      -- ** Re-exports
    , AlonzoEra
    , EpochInfo
    , ExUnits
    , GenTxId
    , HasTxId
    , PParams
    , PastHorizonException
    , RdmrPtr
    , ScriptFailure
    , SerializedTx
    , SubmitTxError
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
    ( Crypto, GenTxId, SerializedTx, SubmitTxError )
import Cardano.Slotting.EpochInfo
    ( EpochInfo )
import Cardano.Slotting.Time
    ( SystemStart )
import Control.Monad.Trans.Except
    ( Except )
import Ouroboros.Consensus.HardFork.History
    ( PastHorizonException )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

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
    :: forall block. (FromJSON (SerializedTx block))
    => (GenTxId block -> Json)
    -> (SubmitTxError block -> Json)
    -> (RdmrPtr -> Text)
    -> (ExUnits -> Json)
    -> (ScriptFailure (Crypto block) -> Json)
    -> (TxIn (Crypto block) -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs encodeTxId encodeSubmitTxError stringifyRdmrPtr encodeExUnits encodeScriptFailure encodeTxIn =
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
    = EvaluateTxScriptFailures (Map RdmrPtr [ScriptFailure (Crypto block)])
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
                Map.foldrWithKey aggregateReports (mempty, mempty)  reports
         in if null failures
            then EvaluationResult successes
            else EvaluationFailure $ EvaluateTxScriptFailures failures
  where
    aggregateReports
        :: RdmrPtr
        -> Either (ScriptFailure (Crypto block)) ExUnits
        -> (Map RdmrPtr [ScriptFailure (Crypto block)], Map RdmrPtr ExUnits)
        -> (Map RdmrPtr [ScriptFailure (Crypto block)], Map RdmrPtr ExUnits)
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
