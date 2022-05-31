--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Babbage where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Ouroboros.Consensus.Cardano.Block
    ( BabbageEra )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger
import qualified Cardano.Ledger.Babbage.PParams as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.PParams as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (Praos crypto) (BabbageEra crypto)
    -> Json
encodeBlock _mode _block = -- (ShelleyBlock (Ledger.Block _blkHeader _txs) _headerHash) =
    undefined
    -- encodeObject
    -- [ ( "body"
    --   , encodeFoldable (encodeTx mode) (Ledger.txSeqTxns txs)
    --   )
    -- , ( "header"
    --   , Shelley.encodeBHeader mode blkHeader
    --   )
    -- , ( "headerHash"
    --   , Shelley.encodeShelleyHash headerHash
    --   )
    -- ]

encodeLedgerFailure
    :: Crypto crypto
    => Ledger.LedgerPredicateFailure (BabbageEra crypto)
    -> Json
encodeLedgerFailure = \case
    Ledger.UtxowFailure _e ->
        undefined
        -- encodeUtxowPredicateFail e
    Ledger.DelegsFailure _e ->
        undefined
        -- Shelley.encodeDelegsFailure e

encodeProposedPPUpdates
    :: Crypto crypto
    => Ledger.ProposedPPUpdates (BabbageEra crypto)
    -> Json
encodeProposedPPUpdates (Ledger.ProposedPPUpdates _m) =
    undefined
--    encodeMap Shelley.stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodePParams'
    :: (forall a. (a -> Json) -> HKD f a -> Json)
    -> Ledger.Babbage.PParams' f era
    -> Json
encodePParams' _encodeF _x =
    undefined
-- encodeObject
--    [ ( "minFeeCoefficient"
--      , encodeF encodeNatural (Al._minfeeA x)
--      )
--    , ( "minFeeConstant"
--      , encodeF encodeNatural (Al._minfeeB x)
--      )
--    , ( "maxBlockBodySize"
--      , encodeF encodeNatural (Al._maxBBSize x)
--      )
--    , ( "maxBlockHeaderSize"
--      , encodeF encodeNatural (Al._maxBHSize x)
--      )
--    , ( "maxTxSize"
--      , encodeF encodeNatural (Al._maxTxSize x)
--      )
--    , ( "stakeKeyDeposit"
--      , encodeF encodeCoin (Al._keyDeposit x)
--      )
--    , ( "poolDeposit"
--      , encodeF encodeCoin (Al._poolDeposit x)
--      )
--    , ( "poolRetirementEpochBound"
--      , encodeF encodeEpochNo (Al._eMax x)
--      )
--    , ( "desiredNumberOfPools"
--      , encodeF encodeNatural (Al._nOpt x)
--      )
--    , ( "poolInfluence"
--      , encodeF encodeNonNegativeInterval (Al._a0 x)
--      )
--    , ( "monetaryExpansion"
--      , encodeF encodeUnitInterval (Al._rho x)
--      )
--    , ( "treasuryExpansion"
--      , encodeF encodeUnitInterval (Al._tau x)
--      )
--    , ( "decentralizationParameter"
--      , encodeF encodeUnitInterval (Al._d x)
--      )
--    , ( "extraEntropy"
--      , encodeF Shelley.encodeNonce (Al._extraEntropy x)
--      )
--    , ( "protocolVersion"
--      , encodeF Shelley.encodeProtVer (Al._protocolVersion x)
--      )
--    , ( "minPoolCost"
--      , encodeF encodeCoin (Al._minPoolCost x)
--      )
--    , ( "coinsPerUtxoWord"
--      , encodeF encodeCoin (Al._coinsPerUTxOWord x)
--      )
--    , ( "costModels"
--      , encodeF encodeCostModels (Al._costmdls x)
--      )
--    , ( "prices"
--      , encodeF encodePrices (Al._prices x)
--      )
--    , ( "maxExecutionUnitsPerTransaction"
--      , encodeF encodeExUnits (Al._maxTxExUnits x)
--      )
--    , ( "maxExecutionUnitsPerBlock"
--      , encodeF encodeExUnits (Al._maxBlockExUnits x)
--      )
--    , ( "maxValueSize"
--      , encodeF encodeNatural (Al._maxValSize x)
--      )
--    , ( "collateralPercentage"
--      , encodeF encodeNatural (Al._collateralPercentage x)
--      )
--    , ( "maxCollateralInputs"
--      , encodeF encodeNatural (Al._maxCollateralInputs x)
--      )
--    ]

encodeTx
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Ledger.ValidatedTx (BabbageEra crypto)
    -> Json
encodeTx _mode _x =
    undefined
-- encodeObjectWithMode mode
--    [ ( "id"
--      , Shelley.encodeTxId (Ledger.txid @(AlonzoEra crypto) (Al.body x))
--      )
--    , ( "body"
--      , encodeTxBody (Al.body x)
--      )
--    , ( "metadata"
--      , (,) <$> fmap (("hash",) . Shelley.encodeAuxiliaryDataHash) (adHash (Al.body x))
--            <*> fmap (("body",) . encodeAuxiliaryData) (Al.auxiliaryData x)
--        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
--      )
--    , ( "inputSource"
--      , encodeIsValid (Al.isValid x)
--      )
--    ]
--    [ ( "witness"
--      , encodeWitnessSet (Al.wits x)
--      )
--    , ( "raw"
--      , encodeByteStringBase64 (serialize' x)
--      )
--    ]
--  where
--    adHash :: Al.TxBody era -> StrictMaybe (Al.AuxiliaryDataHash (Ledger.Crypto era))
--    adHash = getField @"adHash"

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Ledger.UTxO (BabbageEra crypto)
    -> Json
encodeUtxoWithMode _mode =
    undefined
--    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
--  where
--    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)
