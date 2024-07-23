-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.App.Protocol.TxSubmissionSpec
    ( spec
    ) where

import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut (..)
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Mary.Value
    ( MaryValue (..)
    )
import Ogmios.App.Protocol.TxSubmission
    ( SomeEvaluationInAnyEra (..)
    , newEvaluateTransactionResponse
    )
import Ogmios.Data.Ledger.ScriptFailure
    ( EvaluateTransactionError (..)
    )
import Ogmios.Data.Protocol.TxSubmission
    ( Tx
    , TxIn
    , UTxO (..)
    )
import Ogmios.Prelude hiding
    ( Era
    )

import Test.Generators
    ( genUtxoBabbage
    , shrinkUtxo
    )
import Test.Hspec
    ( Spec
    , context
    , parallel
    )
import Test.Hspec.QuickCheck
    ( prop
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , conjoin
    , cover
    , forAllBlind
    , forAllShrinkBlind
    , suchThat
    , (===)
    , (==>)
    )

import qualified Data.Map as Map

type Tx' = Tx (BabbageEra StandardCrypto)
type TxIn' = TxIn StandardCrypto
type TxOut' = BabbageTxOut (BabbageEra StandardCrypto)

spec :: Spec
spec = parallel $ do
    context "newEvaluateTransactionResponse" $ do
        prop "resulting UTxO is the union of network <> user-defined" prop_merge
        prop "succeeds when fully identical" prop_coherent_intersection
        prop "fails on ambiguous intersection" prop_ambiguous_intersection

prop_merge :: Property
prop_merge =
    forAllShrinkBlind genUtxoBabbage shrinkUtxo $ \(UTxO utxoNetwork) ->
    forAllShrinkBlind genUtxoBabbage shrinkUtxo $ \(UTxO utxoUser) ->
    forAllBlind arbitrary $ \tx ->
        (utxoNetwork /= utxoUser || null utxoNetwork) ==>
            property utxoNetwork utxoUser tx
                & cover 1 (null utxoNetwork) "network = ø"
                & cover 1 (null utxoUser) "user = ø"
                & cover 1 (null utxoUser && null utxoNetwork) "both = ø"
                & cover 1 (not (null utxoUser) && not (null utxoNetwork)) "none = ø"
  where
    property utxoNetwork utxoUser tx =
        UTxO (utxoUser <> utxoNetwork) === newEvaluateTransactionResponse
            const
            (\_ -> error "evaluation failed unexpectedly")
            (UTxO utxoNetwork)
            (SomeEvaluationInAnyEra @(BabbageEra StandardCrypto) (UTxO utxoUser) tx)

prop_coherent_intersection :: Property
prop_coherent_intersection =
    forAllShrinkBlind genUtxoBabbage shrinkUtxo $ \(UTxO utxoNetwork) ->
    forAllBlind arbitrary $ \tx ->
        property utxoNetwork utxoNetwork tx
  where
    property utxoNetwork utxoUser tx =
        UTxO (utxoUser <> utxoNetwork) === newEvaluateTransactionResponse
            const
            (\_ -> error "evaluation failed unexpectedly")
            (UTxO utxoNetwork)
            (SomeEvaluationInAnyEra @(BabbageEra StandardCrypto) (UTxO utxoUser) tx)

prop_ambiguous_intersection :: Property
prop_ambiguous_intersection =
    forAllBlind (genUtxoBabbage `suchThat` (\(UTxO u) -> not (null u))) $ \(UTxO utxo) ->
    forAllBlind arbitrary $ \tx ->
    forAllBlind arbitrary $ \coinFlip ->
        let (utxoNetwork, utxoUser)
                | coinFlip  = (utxo, mutateTxOut <$> utxo)
                | otherwise = (mutateTxOut <$> utxo, utxo)
         in
            property utxoNetwork utxoUser tx
  where
    mutateTxOut :: TxOut' -> TxOut'
    mutateTxOut (BabbageTxOut addr (MaryValue coins assets) datum script) =
        BabbageTxOut addr (MaryValue (coins <> (Coin 1)) assets) datum script

    property :: Map TxIn' TxOut' -> Map TxIn' TxOut' -> Tx' -> Property
    property utxoNetwork utxoUser tx =
        newEvaluateTransactionResponse
            (\_u _tx -> error "evaluation succeeded unexpectedly")
            (\case
                OverlappingAdditionalUtxo keys -> conjoin
                    [ Map.keysSet utxoUser === keys
                    , Map.keysSet utxoNetwork === keys
                    ]
                _ -> error "returned unexpected error"
            )
            (UTxO utxoNetwork)
            (SomeEvaluationInAnyEra (UTxO utxoUser) tx)
