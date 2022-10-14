--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Allegra where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Cardano.Ledger.Crypto
    ( Crypto
    )
import GHC.Records
    ( getField
    )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules.Ledger as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA

--
-- Encoders
--

encodeAuxiliaryData
    :: Crypto crypto
    => MA.AuxiliaryData (AllegraEra crypto)
    -> Json
encodeAuxiliaryData (MA.AuxiliaryData blob scripts) = encodeObject
    [ ( "blob"
      , Shelley.encodeMetadataBlob blob
      )
    , ( "scripts"
      , encodeFoldable encodeScript scripts
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (TPraos crypto) (AllegraEra crypto)
    -> Json
encodeBlock mode (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Sh.txSeqTxns' txs)
      )
    , ( "header"
      , Shelley.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeLedgerFailure
    :: Crypto crypto
    => Sh.LedgerPredicateFailure (AllegraEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        Shelley.encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        Shelley.encodeDelegsFailure e

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Sh.PParams' f era
    -> Json
encodePParams' =
    Shelley.encodePParams'

encodeProposedPPUpdates
    :: Ledger.PParamsDelta era ~ Sh.PParamsUpdate era
    => Crypto (Ledger.Crypto era)
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates =
    Shelley.encodeProposedPPUpdates

encodeScript
    :: Crypto crypto
    => MA.Timelock crypto
    -> Json
encodeScript timelock = encodeObject
    [ ( "native", encodeTimelock timelock ) ]

encodeTimelock
    :: Crypto crypto
    => MA.Timelock crypto
    -> Json
encodeTimelock = \case
    MA.RequireSignature sig ->
        Shelley.encodeKeyHash sig
    MA.RequireAllOf xs ->
        encodeObject [( "all", encodeFoldable encodeTimelock xs )]
    MA.RequireAnyOf xs ->
        encodeObject [( "any", encodeFoldable encodeTimelock xs )]
    MA.RequireMOf n xs ->
        encodeObject [( show n, encodeFoldable encodeTimelock xs )]
    MA.RequireTimeExpire s ->
        encodeObject [( "expiresAt", encodeSlotNo s )]
    MA.RequireTimeStart s ->
        encodeObject [( "startsAt", encodeSlotNo s )]

encodeTx
    :: forall crypto. (Crypto crypto)
    => SerializationMode
    -> Sh.Tx (AllegraEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Shelley.encodeTxId (Ledger.txid @(AllegraEra crypto) (Sh.body x))
      )
    , ( "body"
      , encodeTxBody (Sh.body x)
      )
    , ( "metadata"
      , (,) <$> fmap (("hash",) . Shelley.encodeAuxiliaryDataHash) (adHash (Sh.body x))
            <*> fmap (("body",) . encodeAuxiliaryData) (Sh.auxiliaryData x)
        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Sh.wits x)
      )
    , ( "raw"
      , encodeByteStringBase64 (serialize' x)
      )
    ]
  where
    adHash :: MA.TxBody era -> StrictMaybe (Ledger.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => MA.TxBody (AllegraEra crypto)
    -> Json
encodeTxBody (MA.TxBody inps outs certs wdrls fee validity updates _ _) = encodeObject
    [ ( "inputs"
      , encodeFoldable Shelley.encodeTxIn inps
      )
    , ( "outputs"
      , encodeFoldable Shelley.encodeTxOut outs
      )
    , ( "certificates"
      , encodeFoldable Shelley.encodeDCert certs
      )
    , ( "withdrawals"
      , Shelley.encodeWdrl wdrls
      )
    , ( "fee"
      , encodeCoin fee
      )
    , ( "validityInterval"
      , encodeValidityInterval validity
      )
    , ( "update"
      , encodeStrictMaybe Shelley.encodeUpdate updates
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AllegraEra crypto)
    -> Json
encodeUtxo =
    Shelley.encodeUtxo

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Sh.UTxO (AllegraEra crypto)
    -> Json
encodeUtxoWithMode =
    Shelley.encodeUtxoWithMode

encodeUtxoFailure
    :: Crypto crypto
    => MA.UtxoPredicateFailure (AllegraEra crypto)
    -> Json
encodeUtxoFailure = \case
    MA.BadInputsUTxO inputs ->
        encodeObject
            [ ( "badInputs"
              , encodeFoldable Shelley.encodeTxIn inputs
              )
            ]
    MA.OutsideValidityIntervalUTxO itv currentSlot ->
        encodeObject
            [ ( "outsideOfValidityInterval", encodeObject
                [ ( "interval" , encodeValidityInterval itv )
                , ( "currentSlot" , encodeSlotNo currentSlot )
                ]
              )
            ]
    MA.OutputTooBigUTxO outs ->
        encodeObject
            [ ( "tooManyAssetsInOutput"
              , encodeFoldable Shelley.encodeTxOut outs
              )
            ]
    MA.MaxTxSizeUTxO actualSize maxSize ->
        encodeObject
            [ ( "txTooLarge", encodeObject
                [ ( "maximumSize", encodeInteger maxSize )
                , ( "actualSize", encodeInteger actualSize )
                ]
              )
            ]
    MA.InputSetEmptyUTxO ->
        encodeObject
            [ ( "missingAtLeastOneInputUtxo", encodeNull )
            ]
    MA.FeeTooSmallUTxO required actual ->
        encodeObject
            [ ( "feeTooSmall", encodeObject
                [ ( "requiredFee", encodeCoin required )
                , ( "actualFee", encodeCoin actual )
                ]
              )
            ]
    MA.ValueNotConservedUTxO consumed produced ->
        encodeObject
            [ ( "valueNotConserved", encodeObject
                [ ( "consumed", encodeCoin consumed )
                , ( "produced", encodeCoin produced )
                ]
              )
            ]
    MA.WrongNetwork expected invalidAddrs ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Shelley.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
                  )
                ]
              )
            ]
    MA.WrongNetworkWithdrawal expected invalidAccts ->
        encodeObject
        [ ( "networkMismatch", encodeObject
            [ ( "expectedNetwork"
              , Shelley.encodeNetwork expected
              )
            , ( "invalidEntities"
              , Shelley.encodeEntities "rewardAccount" Shelley.encodeRewardAcnt invalidAccts
              )
            ]
          )
        ]
    MA.OutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeFoldable Shelley.encodeTxOut outs
              )
            ]
    MA.OutputBootAddrAttrsTooBig outs ->
        encodeObject
            [ ( "addressAttributesTooLarge"
              , encodeFoldable Shelley.encodeAddress ((\(Sh.TxOut addr _) -> addr) <$> outs)
              )
            ]
    MA.TriesToForgeADA ->
        encodeObject
            [ ( "triesToForgeAda", encodeNull )
            ]
    MA.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

encodeValidityInterval
    :: MA.ValidityInterval
    -> Json
encodeValidityInterval x = encodeObject
    [ ( "invalidBefore"
      , encodeStrictMaybe encodeSlotNo (MA.invalidBefore x)
      )
    , ( "invalidHereafter"
      , encodeStrictMaybe encodeSlotNo (MA.invalidHereafter x)
      )
    ]

encodeWitnessSet
    :: Crypto crypto
    => Sh.WitnessSet (AllegraEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , Shelley.encodeWitVKeys (Sh.addrWits x)
      )
    , ( "scripts"
      , encodeMap Shelley.stringifyScriptHash encodeScript (Sh.scriptWits x)
      )
    , ( "bootstrap"
      , encodeFoldable Shelley.encodeBootstrapWitness (Sh.bootWits x)
      )
    ]
