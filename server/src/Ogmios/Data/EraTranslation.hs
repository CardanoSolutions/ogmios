--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

-- | Provides translation of types across the current era and the latest era.
module Ogmios.Data.EraTranslation
    ( -- * Type Families
      MostRecentEra
    , BlockEra

      -- * GADTs
    , MultiEraUTxO (..)
    , MultiEraTxOut  (..)

      -- * Translations
    , translateUtxo
    , translateTxOut
    , translateTx
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Babbage.Tx
    ( ValidatedTx (..)
    )
import Cardano.Ledger.Era
    ( PreviousEra
    )
import Cardano.Ledger.Serialization
    ( translateViaCBORAnn
    )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
    )
import Cardano.Network.Protocol.NodeToClient
    ( Crypto
    )
import Control.Monad.Trans.Except
    ( runExcept
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra
    , BabbageEra
    , CardanoEras
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock
    )

import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Babbage.Translation as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Ledger

type family MostRecentEra block :: Type where
    MostRecentEra (CardanoBlock crypto) = BlockEra (LastElem (CardanoEras crypto))

type family BlockEra block :: Type where
   BlockEra (ShelleyBlock protocol era) = era

translateTxOut
    :: forall crypto era.
        ( era ~ MostRecentEra (CardanoBlock crypto)
        , Ledger.Crypto crypto
        )
    => Alonzo.TxOut (PreviousEra era)
    -> Babbage.TxOut era
translateTxOut =
    Babbage.translateTxOut
{-# INLINABLE translateTxOut #-}

-- Promote a UTxO from the previous era into the current one. This supposes
-- a forward-compatible format, which has been true since Shelley was introduced.
translateUtxo
    :: forall crypto era.
        ( era ~ MostRecentEra (CardanoBlock crypto)
        , Ledger.Crypto crypto
        )
    => UTxO (PreviousEra era)
    -> UTxO era
translateUtxo =
    UTxO . fmap translateTxOut . unUTxO
{-# INLINABLE translateUtxo #-}

-- Promote a Tx from the previous era into the current one. This supposes
-- a forward-compatible format, which has been true since Shelley was introduced.
translateTx
    :: forall crypto era.
        ( era ~ MostRecentEra (CardanoBlock crypto)
        , Ledger.Crypto crypto
        , Core.Script era ~ Alonzo.Script era
        )
    => Core.Tx (PreviousEra era)
    -> Core.Tx era
translateTx tx = unsafeFromRight $ runExcept $ do
    body <- translateViaCBORAnn "body" $ Alonzo.body tx
    wits <- translateViaCBORAnn "wits" $ Alonzo.wits tx
    auxiliaryData <- case Alonzo.auxiliaryData tx of
      SNothing -> pure SNothing
      SJust axd -> SJust <$> translateViaCBORAnn "auxiliarydata" axd
    let isValid = Alonzo.isValid tx
    pure $ ValidatedTx{body,wits,auxiliaryData,isValid}
  where
    unsafeFromRight :: (HasCallStack, Show e) => Either e a -> a
    unsafeFromRight = either (error . show) id
{-# INLINABLE translateTx #-}

-- Era-specific GADTs

data MultiEraUTxO block where
    UTxOInAlonzoEra
        :: UTxO (AlonzoEra (Crypto block))
        -> MultiEraUTxO block

    UTxOInBabbageEra
        :: UTxO (BabbageEra (Crypto block))
        -> MultiEraUTxO block

deriving instance
    ( Eq (UTxO (AlonzoEra (Crypto block)))
    , Eq (UTxO (BabbageEra (Crypto block)))
    ) => Eq (MultiEraUTxO block)

deriving instance
    ( Show (UTxO (AlonzoEra (Crypto block)))
    , Show (UTxO (BabbageEra (Crypto block)))
    ) => Show (MultiEraUTxO block)

data MultiEraTxOut block where
    TxOutInAlonzoEra
        :: Alonzo.TxOut (AlonzoEra (Crypto block))
        -> MultiEraTxOut block

    TxOutInBabbageEra
        :: Babbage.TxOut (BabbageEra (Crypto block))
        -> MultiEraTxOut block

deriving instance
    ( Eq (Alonzo.TxOut (AlonzoEra (Crypto block)))
    , Eq (Babbage.TxOut (BabbageEra (Crypto block)))
    ) => Eq (MultiEraTxOut block)

deriving instance
    ( Show (Alonzo.TxOut (AlonzoEra (Crypto block)))
    , Show (Babbage.TxOut (BabbageEra (Crypto block)))
    ) => Show (MultiEraTxOut block)
