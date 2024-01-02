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
import Test.QuickCheck
    ( Arbitrary (..)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )

import qualified Cardano.Ledger.Language as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import Test.Consensus.Cardano.Generators
    ()

instance
    ( Arbitrary (Ledger.TxIn StandardCrypto)
    , Arbitrary Ledger.TxIx
    ) => Arbitrary (TxOutSource StandardCrypto)
  where
    arbitrary = genericArbitrary

instance Arbitrary Ledger.Plutus where
    arbitrary = genericArbitrary

instance Arbitrary Ledger.BinaryPlutus where
    arbitrary = pure
        $ Ledger.BinaryPlutus
        $ toShort
        $ unsafeDecodeBase16
            "58f2010000323232323232323222232325333008323232533300b002100114a0\
            \6644646600200200644a66602200229404c8c94ccc040cdc78010028a5113300\
            \40040013014002375c60240026eb0c038c03cc03cc03cc03cc03cc03cc03cc03\
            \cc020c008c020014dd71801180400399b8f375c6002600e00a91010d48656c6c\
            \6f2c20576f726c6421002300d00114984d958c94ccc020cdc3a400000226464a\
            \66601a601e0042930b1bae300d00130060041630060033253330073370e90000\
            \0089919299980618070010a4c2c6eb8c030004c01401058c01400c8c014dd500\
            \0918019baa0015734aae7555cf2ab9f5742ae881"
