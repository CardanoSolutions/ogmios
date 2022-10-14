-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Generators.Orphans () where

import Ogmios.Prelude

import Cardano.Ledger.Alonzo.TxInfo
    ( TxOutSource (..)
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Test.QuickCheck
    ( Arbitrary (..)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )
import Test.QuickCheck.Modifiers
    ( PrintableString (..)
    )

import qualified Cardano.Ledger.TxIn as Ledger

instance
    ( Arbitrary (Ledger.TxIn StandardCrypto)
    , Arbitrary Ledger.TxIx
    ) => Arbitrary (TxOutSource StandardCrypto)
  where
    arbitrary = genericArbitrary

instance Arbitrary Text where
    arbitrary = toText . getPrintableString <$> arbitrary

