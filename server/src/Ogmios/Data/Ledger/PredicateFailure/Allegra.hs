--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Allegra where

import Ogmios.Prelude

import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    )
import Ogmios.Data.Ledger.PredicateFailure
    ( DiscriminatedEntities (..)
    , MultiEraPredicateFailure (..)
    , TxOutInAnyEra (..)
    , ValueInAnyEra (..)
    )
import Ogmios.Data.Ledger.PredicateFailure.Shelley
    ( encodeDelegsFailure
    , encodeUtxowFailure
    )

import qualified Cardano.Ledger.Allegra.Rules as Al
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Data.Set.NonEmpty as NESet
import qualified Ogmios.Data.Ledger.PredicateFailure.Shelley as Shelley

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure AllegraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = Shelley.encodeLedgerFailureInEra
    (encodeUtxowFailure (encodeUtxoFailure ShelleyBasedEraAllegra))
    encodeDelegsFailure

encodeUtxoFailure
    :: forall era.
        ( Era era
        )
    => ShelleyBasedEra era
    -> Al.AllegraUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure era = \case
    Al.BadInputsUTxO inputs ->
        UnknownUtxoReference (NESet.toSet inputs)
    Al.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Al.OutputTooBigUTxO outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit (toList culpritOutputs)
    Al.MaxTxSizeUTxO (Mismatch measuredSize maximumSize) ->
        TransactionTooLarge { measuredSize = fromIntegral measuredSize, maximumSize = fromIntegral maximumSize }
    Al.InputSetEmptyUTxO ->
        EmptyInputSet
    Al.FeeTooSmallUTxO (Mismatch suppliedFee minimumRequiredFee) ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Al.ValueNotConservedUTxO (Mismatch consumed produced) ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Al.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses (NESet.toSet invalidAddrs) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts (NESet.toSet invalidAccts) in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs = toList insufficientlyFundedOutputs }
    Al.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs = toList culpritOutputs }
    Al.UpdateFailure{} ->
        InvalidProtocolParametersUpdate
