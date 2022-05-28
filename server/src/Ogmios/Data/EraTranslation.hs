--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

-- | Provides translation of types across the current era and the latest era.
module Ogmios.Data.EraTranslation
    ( -- * Type Families
      MostRecentEra
    , BlockEra

      -- * Translations
    , translateUTxO
    , translateTx
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Era
    ( PreviousEra )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock )

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Era

type family MostRecentEra block :: Type where
    MostRecentEra (CardanoBlock crypto) = BlockEra (LastElem (CardanoEras crypto))

type family BlockEra block :: Type where
   BlockEra (ShelleyBlock protocol era) = era

-- Promote a UTxO from the previous era into the current one. This supposes
-- a forward-compatible format, which has been true since Shelley was introduced.
translateUTxO
    :: forall crypto era.
        ( era ~ MostRecentEra (CardanoBlock crypto)
        , Crypto crypto
        )
    => UTxO (PreviousEra era)
    -> UTxO era
translateUTxO =
    undefined

-- Promote a Tx from the previous era into the current one. This supposes
-- a forward-compatible format, which has been true since Shelley was introduced.
translateTx
    :: forall crypto era.
        ( era ~ MostRecentEra (CardanoBlock crypto)
        , Crypto crypto
        )
    => Core.Tx (PreviousEra era)
    -> Core.Tx era
translateTx =
    undefined
