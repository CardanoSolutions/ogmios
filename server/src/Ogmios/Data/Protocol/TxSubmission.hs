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
    , EvaluateTxResponse
    , _encodeEvaluateTxResponse
    , ExUnits
    , RdmrPtr
    , ScriptFailure
    , TxIn
    ) where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Protocol
    ()

import Cardano.Ledger.Alonzo.Scripts
    ( ExUnits )
import Cardano.Ledger.Alonzo.Tools
    ( ScriptFailure )
import Cardano.Ledger.Alonzo.TxWitness
    ( RdmrPtr )
import Cardano.Ledger.TxIn
    ( TxIn )
import Cardano.Network.Protocol.NodeToClient
    ( Crypto, SerializedTx, SubmitTxError )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Data.Aeson.Types as Json

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
    Wsp.genericFromJSON Wsp.defaultOptions value
  <|>
    fmap (fmap backwardCompatible) (Wsp.genericFromJSON Wsp.defaultOptions value)
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
    deriving (Show)

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
  where
    proxy = Proxy @(Wsp.Request (EvaluateTx block))
