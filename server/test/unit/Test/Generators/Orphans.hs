-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Generators.Orphans () where

import Ogmios.Prelude

import Cardano.Ledger.Alonzo.Core
    ( AlonzoEraScript (..)
    )
import Ogmios.Data.Json.Query
    ( RewardAccountSummary (..)
    )
import Ogmios.Data.Ledger
    ( ScriptPurposeIndexInAnyEra (..)
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , elements
    , oneof
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )

import qualified Cardano.Ledger.Alonzo.Core as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Test.Cardano.Ledger.Alonzo.Arbitrary as Ledger

import Test.Cardano.Ledger.Conway.Arbitrary
    ()

instance Arbitrary ScriptPurposeIndexInAnyEra where
    arbitrary = oneof
        [ ScriptPurposeIndexInAnyEra . (AlonzoBasedEraAlonzo,) <$> arbitrary
        , ScriptPurposeIndexInAnyEra . (AlonzoBasedEraBabbage,) <$> arbitrary
        , ScriptPurposeIndexInAnyEra . (AlonzoBasedEraConway,) <$> arbitrary
        ]

instance Arbitrary RewardAccountSummary where
    arbitrary = genericArbitrary

instance (AlonzoEraScript era, Ledger.Script era ~ Ledger.AlonzoScript era) => Arbitrary (Ledger.PlutusScript era) where
  arbitrary = do
    lang <- elements [minBound .. eraMaxLanguage @era]
    script <- Ledger.genPlutusScript lang
    case script of
      Ledger.TimelockScript{} -> error "impossible"
      Ledger.PlutusScript s -> pure s
