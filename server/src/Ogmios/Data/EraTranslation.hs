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
    ) where

import Ogmios.Prelude

import Cardano.Ledger.Babbage.Tx
    ( AlonzoTx (..)
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut
    )
import Cardano.Ledger.Core
    ( translateEraThroughCBOR
    )
import Cardano.Ledger.Era
    ( PreviousEra
    )
import Cardano.Ledger.Shelley.UTxO
    ( UTxO (..)
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
import Ouroboros.Consensus.Shelley.Ledger
    ( ShelleyBlock
    )

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Conway.Translation as Conway
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
    upgrade = fromMaybe invariant . Conway.translateTxOut
      where
        -- NOTE: 'translateTxOut' returns 'Nothing' when the ada value in output is
        -- zero, which should be impossible in the context of Ogmios.
        invariant = error "output contains zero Ada; which isn't allowed."

----------
-- UTxO
----------

instance Ledger.Crypto crypto => Upgrade UTxO (ConwayEra crypto) where
    type Upgraded UTxO = UTxO
    upgrade = UTxO . fmap upgrade . unUTxO

----------
-- Tx
----------

instance Ledger.Crypto crypto => Upgrade AlonzoTx (ConwayEra crypto) where
    type Upgraded AlonzoTx = AlonzoTx
    upgrade tx = unsafeFromRight $ runExcept $ do
        body <- translateEraThroughCBOR "body" $ Alonzo.body tx
        wits <- translateEraThroughCBOR "witness" $ Alonzo.body tx
        auxiliaryData <- case Alonzo.auxiliaryData tx of
          SNothing -> pure SNothing
          SJust auxData -> SJust <$> translateEraThroughCBOR "auxiliaryData" auxData
        let isValid = Alonzo.isValid tx
        pure $ AlonzoTx{body,wits,auxiliaryData,isValid}

unsafeFromRight :: (HasCallStack, Show e) => Either e a -> a
unsafeFromRight = either (error . show) id

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
