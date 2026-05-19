--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- TODO(dijkstra): warnings disabled while removed constructors are stubbed.
{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-top-binds -Wno-redundant-constraints #-}

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
import Cardano.Ledger.BaseTypes
    ( Mismatch (..)
    )
import qualified Cardano.Ledger.Shelley.Rules as Sh

encodeLedgerFailure
    :: Sh.ShelleyLedgerPredFailure AllegraEra
    -> MultiEraPredicateFailure
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        encodeUtxowFailure (encodeUtxoFailure ShelleyBasedEraAllegra) e
    Sh.DelegsFailure e ->
        encodeDelegsFailure e

encodeUtxoFailure
    :: forall era.
        ( Era era
        )
    => ShelleyBasedEra era
    -> Al.AllegraUtxoPredFailure era
    -> MultiEraPredicateFailure
encodeUtxoFailure _era _ =
    -- TODO(dijkstra): NonEmptySet vs Set / NonEmpty vs [] / Word32 vs Integer mismatches across many arms in cardano-ledger-allegra 1.9.0.
    error "TODO(dijkstra): encodeUtxoFailure Allegra"
