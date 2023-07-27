--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Ledger.PredicateFailure.Allegra where

import Ogmios.Prelude

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

encodeLedgerFailure
    :: forall crypto.
        ( Crypto crypto
        )
    => Sh.ShelleyLedgerPredFailure (AllegraEra crypto)
    -> MultiEraPredicateFailure crypto
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure (encodeUtxoFailure ShelleyBasedEraAllegra) e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxoFailure
    :: forall era crypto.
        ( EraCrypto (era crypto) ~ crypto
        , Era (era crypto)
        )
    => ShelleyBasedEra (era crypto)
    -> Al.AllegraUtxoPredFailure (era crypto)
    -> MultiEraPredicateFailure crypto
encodeUtxoFailure era = \case
    Al.BadInputsUTxO inputs ->
        UnknownUtxoReference inputs
    Al.OutsideValidityIntervalUTxO validityInterval currentSlot ->
        TransactionOutsideValidityInterval { validityInterval, currentSlot }
    Al.OutputTooBigUTxO outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        ValueSizeAboveLimit culpritOutputs
    Al.MaxTxSizeUTxO measuredSize maximumSize ->
        TransactionTooLarge { measuredSize, maximumSize }
    Al.InputSetEmptyUTxO ->
        EmptyInputSet
    Al.FeeTooSmallUTxO minimumRequiredFee suppliedFee ->
        TransactionFeeTooSmall { minimumRequiredFee, suppliedFee }
    Al.ValueNotConservedUTxO consumed produced ->
        let valueConsumed = ValueInAnyEra (era, consumed) in
        let valueProduced = ValueInAnyEra (era, produced) in
        ValueNotConserved { valueConsumed, valueProduced }
    Al.WrongNetwork expectedNetwork invalidAddrs ->
        let invalidEntities = DiscriminatedAddresses invalidAddrs in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.WrongNetworkWithdrawal expectedNetwork invalidAccts ->
        let invalidEntities = DiscriminatedRewardAccounts invalidAccts in
        NetworkMismatch { expectedNetwork, invalidEntities }
    Al.OutputTooSmallUTxO outs ->
        let insufficientlyFundedOutputs =
                (\out -> (TxOutInAnyEra (era, out), Nothing)) <$> outs
         in InsufficientAdaInOutput { insufficientlyFundedOutputs }
    Al.OutputBootAddrAttrsTooBig outs ->
        let culpritOutputs = (\out -> TxOutInAnyEra (era, out)) <$> outs in
        BootstrapAddressAttributesTooLarge { culpritOutputs }
    Al.TriesToForgeADA ->
        MintingOrBurningAda
    Al.UpdateFailure{} ->
        InvalidProtocolParametersUpdate
