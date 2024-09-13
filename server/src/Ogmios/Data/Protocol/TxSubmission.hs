--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DuplicateRecordFields #-}
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

      -- ** SubmitTransaction
    , SubmitTransaction (..)
    , _decodeSubmitTransaction
    , SubmitTransactionResponse (..)
    , _encodeSubmitTransactionResponse
    , mkSubmitTransactionResponse

      -- ** EvaluateTransaction
    , EvaluateTransaction (..)
    , _decodeEvaluateTransaction
    , EvaluateTransactionResponse (..)
    , EvaluateTransactionError (..)
    , NodeTipTooOldError (..)
    , evaluateExecutionUnits
    , incompatibleEra
    , unsupportedEra
    , nodeTipTooOld
    , _encodeEvaluateTransactionResponse
    , CanEvaluateScriptsInEra

      -- ** Mempool / UTxO reconstruction
    , utxoFromMempool
    , mergeUtxo
    , utxoReferences

      -- ** Re-exports
    , AlonzoEra
    , ConwayEra
    , BabbageEra
    , EpochInfo
    , ExUnits
    , GenTxId
    , HasTxId
    , PastHorizonException
    , TransactionScriptFailure
    , SerializedTransaction
    , SubmitTransactionError
    , SystemStart
    , Core.PParams
    , Core.Tx
    , TxIn (..)
    , UTxO (..)
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Alonzo.Plutus.Context
    ( EraPlutusContext
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoScript
    , AsIx
    , ExUnits (..)
    , PlutusPurpose
    , Script
    )
import Cardano.Ledger.Alonzo.Tx
    ( AlonzoEraTx
    )
import Cardano.Ledger.Alonzo.UTxO
    ( AlonzoScriptsNeeded (..)
    )
import Cardano.Ledger.Api
    ( TransactionScriptFailure
    , evalTxExUnits
    )
import Cardano.Ledger.Api.Tx.In
    ( TxId (..)
    )
import Cardano.Ledger.Babbage.TxBody
    ( BabbageEraTxBody
    )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
    )
import Cardano.Ledger.TxIn
    ( TxIn (..)
    )
import Cardano.Ledger.UTxO
    ( EraUTxO (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( GenTxId
    , SerializedTransaction
    , SubmitTransactionError
    )
import Cardano.Slotting.EpochInfo
    ( EpochInfo
    , hoistEpochInfo
    )
import Cardano.Slotting.Time
    ( SystemStart
    )
import Control.Arrow
    ( left
    )
import Control.Monad.Trans.Except
    ( Except
    )
import Ogmios.Data.EraTranslation
    ( MultiEraUTxO (..)
    , upgrade
    )
import Ogmios.Data.Ledger
    ( ScriptPurposeIndexInAnyEra (..)
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( EvaluateTransactionError (..)
    , NodeTipTooOldError (..)
    , TransactionScriptFailureInAnyEra (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock
    , GenTx (..)
    )
import Ouroboros.Consensus.HardFork.History
    ( PastHorizonException
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..)
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..)
    )

import qualified Cardano.Ledger.Alonzo.Core as Ledger
import qualified Cardano.Ledger.Binary as Binary
import qualified Cardano.Ledger.Core as Core
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Consensus

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.Json.Rpc as Rpc
import qualified Data.Aeson.Types as Json
import qualified Data.Map as Map

--
-- Codecs
--

data TxSubmissionCodecs block = TxSubmissionCodecs
    { decodeSubmitTransaction
        :: ByteString
        -> Maybe (Rpc.Request (SubmitTransaction block))
    , encodeSubmitTransactionResponse
        :: Rpc.Response (SubmitTransactionResponse block)
        -> Json
    , decodeEvaluateTransaction
        :: ByteString
        -> Maybe (Rpc.Request (EvaluateTransaction block))
    , encodeEvaluateTransactionResponse
        :: Rpc.Response (EvaluateTransactionResponse block)
        -> Json
    }

mkTxSubmissionCodecs
    :: forall block.
        ( FromJSON (MultiEraDecoder (SerializedTransaction block))
        , FromJSON (MultiEraUTxO block)
        )
    => Rpc.Options
    -> (GenTxId block -> Json)
    -> (ScriptPurposeIndexInAnyEra (BlockCrypto block) -> Json)
    -> (ExUnits -> Json)
    -> (Rpc.EmbedFault -> EvaluateTransactionError (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> SubmitTransactionError block -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> TxSubmissionCodecs block
mkTxSubmissionCodecs
    opts
    encodeTxId
    encodeScriptPurposeIndex
    encodeExUnits
    encodeEvaluationError
    encodeSubmitTransactionError
    encodeDeserialisationFailure
    =
    TxSubmissionCodecs
        { decodeSubmitTransaction =
            decodeWith _decodeSubmitTransaction
        , encodeSubmitTransactionResponse =
            _encodeSubmitTransactionResponse (Proxy @block)
                opts
                encodeTxId
                encodeSubmitTransactionError
                encodeDeserialisationFailure
        , decodeEvaluateTransaction =
            decodeWith _decodeEvaluateTransaction
        , encodeEvaluateTransactionResponse =
            _encodeEvaluateTransactionResponse (Proxy @block)
                opts
                encodeScriptPurposeIndex
                encodeExUnits
                encodeEvaluationError
                encodeDeserialisationFailure
        }

--
-- Messages
--

data TxSubmissionMessage block
    = MsgSubmitTransaction
        (SubmitTransaction block)
        (Rpc.ToResponse (SubmitTransactionResponse block))
    | MsgEvaluateTransaction
        (EvaluateTransaction block)
        (Rpc.ToResponse (EvaluateTransactionResponse block))

--
-- SubmitTransaction
--

data SubmitTransaction block
    = SubmitTransaction { transaction :: MultiEraDecoder (SerializedTransaction block) }
    deriving (Generic)
deriving instance Show (SerializedTransaction block) => Show (SubmitTransaction block)

_decodeSubmitTransaction
    :: FromJSON (MultiEraDecoder (SerializedTransaction block))
    => Json.Value
    -> Json.Parser (Rpc.Request (SubmitTransaction block))
_decodeSubmitTransaction =
    Rpc.genericFromJSON Rpc.defaultOptions

data SubmitTransactionResponse block
    = SubmitTransactionSuccess (GenTxId block)
    | SubmitTransactionFailure (SubmitTransactionError block)
    | SubmitTransactionFailedToUpgrade Text
    | SubmitTransactionDeserialisationFailure [(SomeShelleyEra, Binary.DecoderError, Word)]
    deriving (Generic)
deriving instance
    ( Show (SubmitTransactionError block)
    , Show (GenTxId block)
    ) => Show (SubmitTransactionResponse block)

_encodeSubmitTransactionResponse
    :: forall block. ()
    => Proxy block
    -> Rpc.Options
    -> (GenTxId block -> Json)
    -> (Rpc.EmbedFault -> SubmitTransactionError block -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> Rpc.Response (SubmitTransactionResponse block)
    -> Json
_encodeSubmitTransactionResponse _proxy
    opts
    encodeTransactionId
    encodeSubmitTransactionError
    encodeDeserialisationFailure
    =
    Rpc.mkResponse opts $ \resolve reject -> \case
        SubmitTransactionSuccess i ->
            resolve $ encodeObject ("transaction" .= encodeTransactionId i)
        SubmitTransactionFailure e ->
            encodeSubmitTransactionError reject e
        SubmitTransactionFailedToUpgrade hint ->
            reject Rpc.FaultInvalidParams
                "Non-upgradable transaction; it seems that you're trying to submit a \
                \transaction in a format that presents incompatibility with the current \
                \ledger era. The field \"data.hint\" contains possible useful information \
                \about what went wrong."
                (pure $ encodeObject
                    ( "hint" .= encodeText hint
                    )
                )
        SubmitTransactionDeserialisationFailure errs ->
            encodeDeserialisationFailure reject errs

-- | Translate an ouroboros-network's 'SubmitResult' into our own
-- 'SubmitTransactionResponse' which also carries a transaction id.
mkSubmitTransactionResponse
    :: HasTxId (SerializedTransaction block)
    => SerializedTransaction block
    -> SubmitResult (SubmitTransactionError block)
    -> SubmitTransactionResponse block
mkSubmitTransactionResponse tx = \case
    SubmitSuccess ->
        SubmitTransactionSuccess (txId tx)
    SubmitFail e ->
        SubmitTransactionFailure e

--
-- EvaluateTransaction
--

data EvaluateTransaction block
    = EvaluateTransaction
        { transaction :: MultiEraDecoder (SerializedTransaction block)
        , additionalUtxo :: MultiEraUTxO block
        }
    deriving (Generic)
deriving instance
    ( Show (SerializedTransaction block)
    , Show (MultiEraUTxO block)
    ) => Show (EvaluateTransaction block)

_decodeEvaluateTransaction
    :: forall block.
        ( FromJSON (MultiEraDecoder (SerializedTransaction block))
        , FromJSON (MultiEraUTxO block)
        )
    => Json.Value
    -> Json.Parser (Rpc.Request (EvaluateTransaction block))
_decodeEvaluateTransaction =
    Rpc.genericFromJSON $ Rpc.defaultOptions
        { Rpc.onMissingField = \fieldName ->
            if fieldName == "additionalUtxo" then
                pure (Json.Array mempty)
            else
                Rpc.onMissingField Rpc.defaultOptions fieldName
        }

data EvaluateTransactionResponse block
    = EvaluationFailure (EvaluateTransactionError (BlockCrypto block))
    | EvaluationResult (Map (ScriptPurposeIndexInAnyEra (BlockCrypto block)) ExUnits)
    | EvaluateTransactionDeserialisationFailure [(SomeShelleyEra, Binary.DecoderError, Word)]

deriving instance Crypto (BlockCrypto block) => Show (EvaluateTransactionResponse block)

-- | Shorthand constructor for 'EvaluateTransactionResponse'
unsupportedEra :: Text -> EvaluateTransactionResponse block
unsupportedEra =
    EvaluationFailure . UnsupportedEra

-- | Shorthand constructor for 'EvaluateTransactionResponse'
incompatibleEra :: Text -> EvaluateTransactionResponse block
incompatibleEra =
    EvaluationFailure . IncompatibleEra

-- | Shorthand constructor for 'EvaluateTransactionResponse'
nodeTipTooOld :: Text -> EvaluateTransactionResponse block
nodeTipTooOld currentNodeEra =
    EvaluationFailure (NodeTipTooOldErr $
        NodeTipTooOld { currentNodeEra, minimumRequiredEra }
    )
  where
    minimumRequiredEra = "alonzo"

-- TODO: Move those instances somewhere near other JSON instances.
_encodeEvaluateTransactionResponse
    :: forall block. ()
    => Proxy block
    -> Rpc.Options
    -> (ScriptPurposeIndexInAnyEra (BlockCrypto block) -> Json)
    -> (ExUnits -> Json)
    -> (Rpc.EmbedFault -> EvaluateTransactionError (BlockCrypto block) -> Json)
    -> (Rpc.EmbedFault -> [(SomeShelleyEra, Binary.DecoderError, Word)] -> Json)
    -> Rpc.Response (EvaluateTransactionResponse block)
    -> Json
_encodeEvaluateTransactionResponse _proxy
    opts
    encodeRdmrPtr
    encodeExUnits
    encodeEvaluationError
    encodeDeserialisationFailure
    =
    Rpc.mkResponse opts $ \resolve reject -> \case
        EvaluationResult budgets ->
            resolve $ encodeList identity $ Map.foldrWithKey
                (\ptr result xs ->
                    encodeObject
                        ( "validator" .= encodeRdmrPtr ptr
                       <> "budget" .= encodeExUnits result
                        ) : xs
                ) [] budgets

        EvaluateTransactionDeserialisationFailure errs ->
            encodeDeserialisationFailure reject errs

        EvaluationFailure e ->
            encodeEvaluationError reject e

-- | A constraint synonym to bundle together constraints needed to run a script
-- evaluation in any era after Alonzo (incl.).
type CanEvaluateScriptsInEra era =
      ( AlonzoEraTx era
      , BabbageEraTxBody era
      , EraPlutusContext era
      , EraUTxO era
      , ScriptsNeeded era ~ AlonzoScriptsNeeded era
      , Script era ~ AlonzoScript era
      , EraPlutusContext era
      , IsAlonzoBasedEra era
      , Crypto (EraCrypto era)
      )

-- | Evaluate script executions units for the given transaction.
evaluateExecutionUnits
    :: forall era block ix crypto.
      ( CanEvaluateScriptsInEra (era crypto)
      , ix ~ ScriptPurposeIndexInAnyEra crypto
      , crypto ~ BlockCrypto block
      , crypto ~ EraCrypto (era crypto)
      )
    => Core.PParams (era crypto)
        -- ^ Protocol parameters
    -> SystemStart
        -- ^ Start of the blockchain, for converting slots to UTC times
    -> EpochInfo (Except PastHorizonException)
        -- ^ Information about epoch sizes, for converting slots to UTC times
    -> UTxO (era crypto)
        -- ^ A UTXO needed to resolve inputs
    -> Core.Tx (era crypto)
        -- ^ The actual transaction
    -> EvaluateTransactionResponse block
evaluateExecutionUnits pparams systemStart epochInfo utxo tx =
    let (failures, successes) =
            Map.foldrWithKey aggregateReports (mempty, mempty)  reports
     in if null failures
        then EvaluationResult successes
        else EvaluationFailure $ ScriptExecutionFailures failures
  where
    aggregateReports
        :: PlutusPurpose AsIx (era crypto)
        -> Either (TransactionScriptFailure (era crypto)) ExUnits
        -> (Map ix [TransactionScriptFailureInAnyEra crypto], Map ix ExUnits)
        -> (Map ix [TransactionScriptFailureInAnyEra crypto], Map ix ExUnits)
    aggregateReports ptr result (failures, successes) =
        case result of
            Left scriptFailure ->
                let failureInAnyEra = TransactionScriptFailureInAnyEra @crypto (alonzoBasedEra @(era crypto), scriptFailure)
                 in ( Map.unionWith (++) (Map.singleton ix [failureInAnyEra]) failures
                    , successes
                    )
            Right exUnits ->
                ( failures
                , Map.singleton ix exUnits <> successes
                )
      where
        ix = ScriptPurposeIndexInAnyEra (alonzoBasedEra @(era crypto), ptr)

    reports
        :: Map
            (PlutusPurpose AsIx (era crypto))
            (Either (TransactionScriptFailure (era crypto)) ExUnits)
    reports =
        evalTxExUnits
          pparams
          tx
          utxo
          (hoistEpochInfo (left show . runIdentity . runExceptT) epochInfo)
          systemStart

--
-- Reconstructing UTxO set from the mempool
--

utxoFromMempool
    :: forall block crypto.
        ( Crypto crypto
        , block ~ CardanoBlock crypto
        )
    => [GenTx block]
    -> MultiEraUTxO block
utxoFromMempool =
    go $ UTxOInBabbageEra mempty
  where
    go :: MultiEraUTxO block -> [GenTx block] -> MultiEraUTxO block
    go utxo = \case
        [] -> utxo
        tx:txs -> go
            (utxo
                & withoutKeys (inputs tx)
                & union (outputs tx)
            )
            txs

    withoutKeys :: Set (TxIn crypto) -> MultiEraUTxO block -> MultiEraUTxO block
    withoutKeys ks = \case
        UTxOInBabbageEra (UTxO utxo) ->
            UTxOInBabbageEra (UTxO (Map.withoutKeys utxo ks))
        UTxOInConwayEra (UTxO utxo) ->
            UTxOInConwayEra (UTxO (Map.withoutKeys utxo ks))

    union :: MultiEraUTxO block -> MultiEraUTxO block -> MultiEraUTxO block
    union l r = case (l, r) of
        (UTxOInBabbageEra (UTxO ul), UTxOInBabbageEra (UTxO ur)) ->
            UTxOInBabbageEra (UTxO (Map.union ul ur))
        (UTxOInBabbageEra (upgrade -> (UTxO ul)), UTxOInConwayEra (UTxO ur)) ->
            UTxOInConwayEra (UTxO (Map.union ul ur))
        (UTxOInConwayEra (UTxO ul), UTxOInBabbageEra (upgrade -> (UTxO ur))) ->
            UTxOInConwayEra (UTxO (Map.union ul ur))
        (UTxOInConwayEra (UTxO ul), UTxOInConwayEra (UTxO ur)) ->
            UTxOInConwayEra (UTxO (Map.union ul ur))

    newUtxoFor :: TxId crypto -> [out] -> Map (TxIn crypto) out
    newUtxoFor h outs =
        Map.fromList [ (TxIn h ix, out) | (out, ix) <- zip outs [minBound ..] ]

    inputs :: GenTx block -> Set (TxIn crypto)
    inputs = \case
        GenTxConway (Consensus.ShelleyTx _ tx) ->
            tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
        GenTxBabbage (Consensus.ShelleyTx _ tx) ->
            tx ^. Ledger.bodyTxL . Ledger.inputsTxBodyL
        GenTxAlonzo{} ->
            error "inputs: unsupported era."
        GenTxMary{} ->
            error "inputs: unsupported era."
        GenTxAllegra{} ->
            error "inputs: unsupported era."
        GenTxShelley{} ->
            error "inputs: unsupported era."
        GenTxByron{} ->
            error "inputs: unsupported era."

    outputs :: GenTx block -> MultiEraUTxO block
    outputs = \case
        GenTxConway (Consensus.ShelleyTx h tx) ->
            let
                outs = tx ^. Ledger.bodyTxL . Ledger.outputsTxBodyL
                utxo = newUtxoFor h (toList outs)
             in
                UTxOInConwayEra (UTxO utxo)
        GenTxBabbage (Consensus.ShelleyTx h tx) ->
            let
                outs = tx ^. Ledger.bodyTxL . Ledger.outputsTxBodyL
                utxo = newUtxoFor h (toList outs)
             in
                UTxOInBabbageEra (UTxO utxo)
        GenTxAlonzo{} ->
            error "outputs: unsupported era."
        GenTxMary{} ->
            error "outputs: unsupported era."
        GenTxAllegra{} ->
            error "outputs: unsupported era."
        GenTxShelley{} ->
            error "outputs: unsupported era."
        GenTxByron{} ->
            error "outputs: unsupported era."

mergeUtxo :: Crypto (BlockCrypto block) => MultiEraUTxO block -> MultiEraUTxO block -> MultiEraUTxO block
mergeUtxo a b = case (a, b) of
    (UTxOInBabbageEra (unUTxO -> l), UTxOInBabbageEra (unUTxO -> r)) ->
        UTxOInBabbageEra $ UTxO (Map.union l r)
    (UTxOInBabbageEra (unUTxO -> l), UTxOInConwayEra (unUTxO -> r)) ->
        UTxOInConwayEra $ UTxO (Map.union (upgrade <$> l) r)
    (UTxOInConwayEra (unUTxO -> l), UTxOInBabbageEra (unUTxO -> r)) ->
        UTxOInConwayEra $ UTxO (Map.union l (upgrade <$> r))
    (UTxOInConwayEra (unUTxO -> l), UTxOInConwayEra (unUTxO -> r)) ->
        UTxOInConwayEra $ UTxO (Map.union l r)

utxoReferences :: Crypto crypto => MultiEraUTxO (CardanoBlock crypto) -> [Text]
utxoReferences = fmap txInToText . \case
    UTxOInBabbageEra (unUTxO -> u) -> Map.keys u
    UTxOInConwayEra  (unUTxO -> u) -> Map.keys u
  where
    txInToText (Ledger.TxIn txid (Ledger.TxIx ix)) =
        let (CC.UnsafeHash h) = Ledger.extractHash (Ledger.unTxId txid)
         in encodeBase16 (fromShort h) <> "#" <> show ix
