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
    , Upgrade (..)
    , upgradeGenTx
    ) where

import Ogmios.Prelude

import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..)
    )
import Cardano.Ledger.Babbage.Tx
    ( AlonzoTx (..)
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut (..)
    )
import Cardano.Ledger.Core
    ( translateEraThroughCBOR
    , upgradeTxBody
    )
import Cardano.Ledger.Era
    ( PreviousEra
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
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
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
import qualified Cardano.Ledger.Crypto as Ledger

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

instance Ledger.Crypto crypto => Upgrade BabbageTxOut (ConwayEra crypto) where
    type Upgraded BabbageTxOut = BabbageTxOut
    upgrade = force . Conway.upgradeTxOut

----------
-- UTxO
----------

instance Ledger.Crypto crypto => Upgrade UTxO (ConwayEra crypto) where
    type Upgraded UTxO = UTxO
    upgrade = force . UTxO . fmap upgrade . unUTxO

----------
-- Tx
----------

instance
    ( Ledger.Crypto crypto
    , NFData (SigDSIGN (Ledger.DSIGN crypto))
    , NFData (VerKeyDSIGN (Ledger.DSIGN crypto))
    ) => Upgrade AlonzoTx (ConwayEra crypto) where
    type Upgraded AlonzoTx = AlonzoTx
    upgrade tx = force $ unsafeFromRight $ do
        body <- left show $ upgradeTxBody (Alonzo.body tx)
        left show $ runExcept $ do
            wits <- translateEraThroughCBOR "witness" $ Alonzo.wits tx
            auxiliaryData <- case Alonzo.auxiliaryData tx of
              SNothing -> pure SNothing
              SJust auxData -> SJust <$> translateEraThroughCBOR "auxiliaryData" auxData
            let isValid = Alonzo.isValid tx
            pure $ AlonzoTx{body,wits,auxiliaryData,isValid}

----------
-- GenTx
----------

upgradeGenTx
    :: forall crypto.
        ( Crypto crypto
        )
    => GenTx (CardanoBlock crypto)
    -> Either Text (GenTx (CardanoBlock crypto))
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
        txBabbage <- left show $ runExcept $ translateEraThroughCBOR @(BabbageEra crypto) "AlonzoTx" txAlonzo
        upgradeGenTx $ GenTxBabbage $ ShelleyTx hash txBabbage
    GenTxBabbage (ShelleyTx hash txBabbage) -> do
        txConway <- left show $ runExcept $ translateEraThroughCBOR @(ConwayEra crypto) "BabbageTx" txBabbage
        pure $ GenTxConway $ ShelleyTx hash txConway
    latest@(GenTxConway(_))->
        Right latest

unsafeFromRight :: (HasCallStack) => Either Text a -> a
unsafeFromRight = either error id

-- Era-specific GADTs

data MultiEraUTxO block where
    UTxOInBabbageEra
        :: UTxO (BabbageEra (BlockCrypto block))
        -> MultiEraUTxO block

    UTxOInConwayEra
        :: UTxO (ConwayEra (BlockCrypto block))
        -> MultiEraUTxO block

deriving instance
    ( Eq (UTxO (BabbageEra (BlockCrypto block)))
    , Eq (UTxO (ConwayEra (BlockCrypto block)))
    ) => Eq (MultiEraUTxO block)

deriving instance
    ( Show (UTxO (BabbageEra (BlockCrypto block)))
    , Show (UTxO (ConwayEra (BlockCrypto block)))
    ) => Show (MultiEraUTxO block)

data MultiEraTxOut block where
    TxOutInBabbageEra
        :: Babbage.BabbageTxOut (BabbageEra (BlockCrypto block))
        -> MultiEraTxOut block

    TxOutInConwayEra
        :: Babbage.BabbageTxOut (ConwayEra (BlockCrypto block))
        -> MultiEraTxOut block

deriving instance
    ( Eq (Babbage.BabbageTxOut (BabbageEra (BlockCrypto block)))
    , Eq (Babbage.BabbageTxOut (ConwayEra (BlockCrypto block)))
    ) => Eq (MultiEraTxOut block)

deriving instance
    ( Show (Babbage.BabbageTxOut (BabbageEra (BlockCrypto block)))
    , Show (Babbage.BabbageTxOut (ConwayEra (BlockCrypto block)))
    ) => Show (MultiEraTxOut block)
