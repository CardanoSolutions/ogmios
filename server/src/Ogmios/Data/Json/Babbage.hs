--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Babbage where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize' )
import Cardano.Ledger.Crypto
    ( Crypto )
import GHC.Records
    ( getField )
import Ouroboros.Consensus.Cardano.Block
    ( BabbageEra )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos )
import Ouroboros.Consensus.Protocol.Praos.Header
    ( Header (..), HeaderBody (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( ShelleyProtocolHeader )

import qualified Data.Map.Strict as Map

import qualified Cardano.Ledger.Alonzo.TxSeq as Ledger
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Babbage.PParams as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.Tx as Ledger.Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Ledger.Babbage
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.PParams as Ledger
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Alonzo as Alonzo
import qualified Ogmios.Data.Json.Mary as Mary
import qualified Ogmios.Data.Json.Shelley as Shelley

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (Praos crypto) (BabbageEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) = encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Ledger.txSeqTxns txs)
      )
    , ( "header"
      , encodeHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeHeader
    :: Crypto crypto
    => SerializationMode
    -> ShelleyProtocolHeader (Praos crypto)
    -> Json
encodeHeader mode (Header hBody _hSig) = encodeObjectWithMode mode
    [ ( "blockHeight"
      , encodeBlockNo (hbBlockNo hBody)
      )
    , ( "slot"
      , encodeSlotNo (hbSlotNo hBody)
      )
    , ( "prevHash"
      , Shelley.encodePrevHash (hbPrev hBody)
      )
    -- , ( "issuerVk"
    --   , encodeVKey (hbVrfVk hBody)
    --   )
    -- , ( "issuerVrf"
    --   , encodeVerKeyVRF (hbVrfVk hBody)
    --   )
    , ( "blockSize"
      , encodeWord32 (hbBodySize hBody)
      )
    , ( "blockHash"
      , Shelley.encodeHash (hbBodyHash hBody)
      )
    ]
    [ ( "protocolVersion"
      , Shelley.encodeProtVer (hbProtVer hBody)
      )
    , ( "opCert"
      , Shelley.encodeOCert (hbOCert hBody)
      )
    -- , ( "nonce" TODO: Review name
    --   , encodeCertifiedVRF (hbVrfRes hBody)
    --   )
    ]


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
    -> Ledger.Babbage.ValidatedTx (BabbageEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
   [ ( "id"
     , Shelley.encodeTxId (Ledger.txid @(BabbageEra crypto) (Ledger.Babbage.body x))
     )
   , ( "body"
     , encodeTxBody (Ledger.Babbage.body x)
     )
   , ( "metadata"
     , (,) <$> fmap (("hash",) . Shelley.encodeAuxiliaryDataHash) (adHash (Ledger.Babbage.body x))
           <*> fmap (("body",) . Alonzo.encodeAuxiliaryData) (Ledger.Babbage.auxiliaryData x)
       & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
     )
   , ( "inputSource"
     , Alonzo.encodeIsValid (Ledger.Babbage.isValid x)
     )
   ]
   [ ( "witness"
     , Alonzo.encodeWitnessSet (Ledger.Babbage.wits x)
     )
   , ( "raw"
     , encodeByteStringBase64 (serialize' x)
     )
   ]
 where
   adHash :: Ledger.Babbage.TxBody era -> StrictMaybe (Ledger.AuxiliaryDataHash (Ledger.Crypto era))
   adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => Ledger.Babbage.TxBody (BabbageEra crypto)
    -> Json
encodeTxBody x = encodeObject
    [ ( "inputs"
      , encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.inputs x)
      )
    , ( "collaterals"
      , encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.collateral x)
      )
    , ( "references"
      , encodeFoldable Shelley.encodeTxIn (Ledger.Babbage.referenceInputs x)
      )
    , ( "collateralReturn"
      , encodeStrictMaybe encodeTxOut (Ledger.Babbage.collateralReturn x)
      )
    , ( "totalCollateral"
      , encodeStrictMaybe encodeCoin (Ledger.Babbage.totalCollateral x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (Ledger.Babbage.outputs x)
      )
    , ( "certificates"
      , encodeFoldable Shelley.encodeDCert (Ledger.Babbage.txcerts x)
      )
    , ( "withdrawals"
      , Shelley.encodeWdrl (Ledger.Babbage.txwdrls x)
      )
    , ( "fee"
      , encodeCoin (Ledger.Babbage.txfee x)
      )
    , ( "validityInterval"
      , Allegra.encodeValidityInterval (Ledger.Babbage.txvldt x)
      )
    , ( "update"
      , encodeStrictMaybe encodeUpdate (Ledger.Babbage.txUpdates x)
      )
    , ( "mint"
      , Mary.encodeValue (Ledger.Babbage.mint x)
      )
    , ( "network"
      , encodeStrictMaybe Shelley.encodeNetwork (Ledger.Babbage.txnetworkid x)
      )
    , ( "scriptIntegrityHash"
      , encodeStrictMaybe Alonzo.encodeScriptIntegrityHash (Ledger.Babbage.scriptIntegrityHash x)
      )
    , ( "requiredExtraSignatures"
      , encodeFoldable Shelley.encodeKeyHash (Ledger.Babbage.reqSignerHashes x)
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Ledger.Babbage.TxOut (BabbageEra crypto)
    -> Json
encodeTxOut x@(Ledger.Babbage.TxOut addr value _datum script) = encodeObject
    [ ( "address"
      , Shelley.encodeAddress addr
      )
    , ( "value"
      , Mary.encodeValue value
      )
    , ( "datumHash"
      , encodeStrictMaybe Alonzo.encodeDataHash (getField @"datahash" x)
      )
    , ( "datum"
      , encodeStrictMaybe Alonzo.encodeData (getField @"datum" x)
      )
    , ( "script"
      , encodeStrictMaybe Alonzo.encodeScript script
      )
    ]

encodeUpdate
    :: Crypto crypto
    => Ledger.Update (BabbageEra crypto)
    -> Json
encodeUpdate (Ledger.Update update epoch) = encodeObject
    [ ( "proposal"
      , encodeProposedPPUpdates update
      )
    , ( "epoch"
      , encodeEpochNo epoch
      )
    ]

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Ledger.UTxO (BabbageEra crypto)
    -> Json
encodeUtxoWithMode mode =
    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Ledger.unUTxO
  where
    encodeIO = curry (encode2Tuple Shelley.encodeTxIn encodeTxOut)
