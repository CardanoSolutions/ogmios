--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE UndecidableInstances #-}

-- TODO(dijkstra): warnings disabled while the AlonzoTx Upgrade instance is stubbed
-- (AlonzoTx now has kind `TxLevel -> Type -> Type` in cardano-ledger 1.20).
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-incomplete-patterns #-}

-- | Provides translation of types across the current era and the latest era.
module Ogmios.Data.EraTranslation
    ( -- * Type Families
      MostRecentEra
    , BlockEra

      -- * GADTs
    , MultiEraUTxO (..)
    , MultiEraTxOut  (..)

      -- * Translations
    , Upgrade (..)
    , upgradeGenTx
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Babbage.Tx
    ( AlonzoTx (..)
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut (..)
    )
import Cardano.Ledger.Api.Tx.Body
    ( upgradeTxBody
    )
import Cardano.Ledger.Core
    ( PreviousEra
    , translateEraThroughCBOR
    )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
    )
import Control.Arrow
    ( left
    )
import Control.Monad.Trans.Except
    ( runExcept
    )
import Data.Maybe.Strict
    ( StrictMaybe (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( GenTx (..)
    )
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock
    )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( GenTx (..)
    )

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Conway.Core as Conway

type family MostRecentEra block :: Type where
    MostRecentEra (CardanoBlock crypto) = BlockEra (LastElem (CardanoEras crypto))

type family BlockEra block :: Type where
   BlockEra (ShelleyBlock protocol era) = era

class Upgrade (f :: Type -> Type) era where
    type Upgraded f :: Type -> Type
    upgrade :: f (PreviousEra era) -> Upgraded f era

----------
-- TxOut
----------

instance Upgrade BabbageTxOut ConwayEra where
    type Upgraded BabbageTxOut = BabbageTxOut
    upgrade = force . Conway.upgradeTxOut

----------
-- UTxO
----------

instance Upgrade UTxO ConwayEra where
    type Upgraded UTxO = UTxO
    upgrade = force . UTxO . fmap upgrade . unUTxO

----------
-- Tx
----------

-- TODO(dijkstra): AlonzoTx now has kind `TxLevel -> Type -> Type` in cardano-ledger 1.20;
-- the Upgrade instance head needs to apply a TxLevel (e.g. TopTx).
-- PR #461 replaces this with `instance Upgrade (Tx TopTx) ConwayEra` using the generic
-- ledger `Tx` type instead of `AlonzoTx`. Stubbed out until that refactor lands.
{-
instance Upgrade AlonzoTx ConwayEra where
    type Upgraded AlonzoTx = AlonzoTx
    upgrade tx = force $ unsafeFromRight $ do
        body <- left show $ upgradeTxBody (Alonzo.atBody tx)
        left show $ runExcept $ do
            wits <- translateEraThroughCBOR "witness" $ Alonzo.atWits tx
            auxiliaryData <- case Alonzo.atAuxData tx of
              SNothing -> pure SNothing
              SJust auxData -> SJust <$> translateEraThroughCBOR "auxiliaryData" auxData
            let isValid = Alonzo.atIsValid tx
            pure $ AlonzoTx { atBody = body, atWits = wits, atAuxData = auxiliaryData, atIsValid = isValid }
-}

----------
-- GenTx
----------

upgradeGenTx
    :: GenTx (CardanoBlock StandardCrypto)
    -> Either Text (GenTx (CardanoBlock StandardCrypto))
upgradeGenTx = \case
    GenTxByron _ ->
        Left "cannot upgrade from Byron transaction: too old, use a more recent transaction builder."
    GenTxShelley _ ->
        Left "cannot upgrade from Shelley transaction: too old, use a more recent transaction builder."
    GenTxAllegra _ ->
        Left "cannot upgrade from Allegra transaction: too old, use a more recent transaction builder."
    GenTxMary _ ->
        Left "cannot upgrade from Mary transaction: too old, use a more recent transaction builder."
    GenTxAlonzo (ShelleyTx hash txAlonzo) -> do
        txBabbage <- left show $ runExcept $ translateEraThroughCBOR @BabbageEra "AlonzoTx" txAlonzo
        upgradeGenTx $ GenTxBabbage $ ShelleyTx hash txBabbage
    GenTxBabbage (ShelleyTx hash txBabbage) -> do
        txConway <- left show $ runExcept $ translateEraThroughCBOR @ConwayEra "BabbageTx" txBabbage
        pure $ GenTxConway $ ShelleyTx hash txConway
    latest@(GenTxConway _)->
        Right latest

unsafeFromRight :: (HasCallStack) => Either Text a -> a
unsafeFromRight = either error id

-- Era-specific GADTs

data MultiEraUTxO block where
    UTxOInBabbageEra
        :: UTxO BabbageEra
        -> MultiEraUTxO block

    UTxOInConwayEra
        :: UTxO ConwayEra
        -> MultiEraUTxO block

deriving instance
    ( Eq (UTxO BabbageEra)
    , Eq (UTxO ConwayEra)
    ) => Eq (MultiEraUTxO block)

deriving instance
    ( Show (UTxO BabbageEra)
    , Show (UTxO ConwayEra)
    ) => Show (MultiEraUTxO block)

data MultiEraTxOut block where
    TxOutInBabbageEra
        :: Babbage.BabbageTxOut BabbageEra
        -> MultiEraTxOut block

    TxOutInConwayEra
        :: Babbage.BabbageTxOut ConwayEra
        -> MultiEraTxOut block

deriving instance
    ( Eq (Babbage.BabbageTxOut BabbageEra)
    , Eq (Babbage.BabbageTxOut ConwayEra)
    ) => Eq (MultiEraTxOut block)

deriving instance
    ( Show (Babbage.BabbageTxOut BabbageEra)
    , Show (Babbage.BabbageTxOut ConwayEra)
    ) => Show (MultiEraTxOut block)
