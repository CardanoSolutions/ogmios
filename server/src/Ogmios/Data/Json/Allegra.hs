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
import qualified Cardano.Ledger.Shelley.TxBody as Sh
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
encodeAuxiliaryData (MA.AuxiliaryData blob scripts) =
    "blob" .=? OmitWhen null
        Shelley.encodeMetadataBlob blob <>
    "scripts" .=? OmitWhen null
        (encodeFoldable encodeScript) scripts
    & encodeObject

encodeBlock
    :: Crypto crypto
    => ShelleyBlock (TPraos crypto) (AllegraEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    "body" .=
        encodeFoldable encodeTx (Sh.txSeqTxns' txs) <>
    "header" .=
        Shelley.encodeBHeader blkHeader <>
    "headerHash" .=
        Shelley.encodeShelleyHash headerHash
    & encodeObject

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
    :: (forall a. Text -> (a -> Json) -> Sh.HKD f a -> Series)
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
encodeScript timelock =
    encodeObject ("native" .= encodeTimelock timelock)

encodeTimelock
    :: Crypto crypto
    => MA.Timelock crypto
    -> Json
encodeTimelock = \case
    MA.RequireSignature sig ->
        Shelley.encodeKeyHash sig
    MA.RequireAllOf xs ->
        encodeObject ("all" .= encodeFoldable encodeTimelock xs)
    MA.RequireAnyOf xs ->
        encodeObject ("any" .= encodeFoldable encodeTimelock xs)
    MA.RequireMOf n xs ->
        encodeObject (show n .= encodeFoldable encodeTimelock xs)
    MA.RequireTimeExpire s ->
        encodeObject ("expiresAt" .= encodeSlotNo s)
    MA.RequireTimeStart s ->
        encodeObject ("startsAt" .= encodeSlotNo s)

encodeTx
    :: forall crypto. (Crypto crypto)
    => Sh.Tx (AllegraEra crypto)
    -> Json
encodeTx x =
    "id" .= Shelley.encodeTxId (Ledger.txid @(AllegraEra crypto) (Sh.body x))
        <>
    "inputSource" .= encodeText "inputs"
        <>
    encodeTxBody (Sh.body x)
        <>
    "metadata" .=? OmitWhenNothing identity metadata
        <>
    encodeWitnessSet (Sh.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    adHash :: MA.TxBody era -> StrictMaybe (Ledger.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

    metadata = liftA2
        (\hash body -> encodeObject ("hash" .= hash <> "body" .= body))
        (Shelley.encodeAuxiliaryDataHash <$> adHash (Sh.body x))
        (encodeAuxiliaryData <$> Sh.auxiliaryData x)

encodeTxBody
    :: Crypto crypto
    => MA.TxBody (AllegraEra crypto)
    -> Series
encodeTxBody (MA.TxBody inps outs certs wdrls fee validity updates _ _) =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn inps <>
    "outputs" .=
        encodeFoldable Shelley.encodeTxOut outs <>
    "withdrawals" .=? OmitWhen (null . Sh.unWdrl)
        Shelley.encodeWdrl wdrls <>
    "certificates" .=? OmitWhen null
        (encodeFoldable Shelley.encodeDCert) certs <>
    "fee" .=
        encodeCoin fee <>
    "validityInterval" .=
        encodeValidityInterval validity <>
    "governanceActions" .=? OmitWhenNothing
        (encodeFoldable identity . pure @[] . Shelley.encodeUpdate)
        updates

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AllegraEra crypto)
    -> Json
encodeUtxo =
    Shelley.encodeUtxo

encodeUtxoFailure
    :: Crypto crypto
    => MA.UtxoPredicateFailure (AllegraEra crypto)
    -> Json
encodeUtxoFailure = \case
    MA.BadInputsUTxO inputs ->
        "badInputs" .=
            encodeFoldable Shelley.encodeTxIn inputs
        & encodeObject
    MA.OutsideValidityIntervalUTxO itv currentSlot ->
        "outsideOfValidityInterval" .= encodeObject
            ( "interval" .= encodeValidityInterval itv <>
              "currentSlot" .= encodeSlotNo currentSlot
            )
        & encodeObject
    MA.OutputTooBigUTxO outs ->
        "tooManyAssetsInOutput" .=
            encodeFoldable Shelley.encodeTxOut outs
        & encodeObject
    MA.MaxTxSizeUTxO actualSize maxSize ->
        "txTooLarge" .= encodeObject
            ( "maximumSize" .=
                encodeInteger maxSize <>
              "actualSize" .=
                encodeInteger actualSize
            )
        & encodeObject
    MA.InputSetEmptyUTxO ->
        "missingAtLeastOneInputUtxo" .=
            encodeNull
        & encodeObject
    MA.FeeTooSmallUTxO required actual ->
        "feeTooSmall" .= encodeObject
            ( "requiredFee" .=
                encodeCoin required <>
              "actualFee" .=
                encodeCoin actual
            )
        & encodeObject
    MA.ValueNotConservedUTxO consumed produced ->
        "valueNotConserved" .= encodeObject
            ( "consumed" .=
                encodeCoin consumed <>
              "produced" .=
                encodeCoin produced
            )
        & encodeObject
    MA.WrongNetwork expected invalidAddrs ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
            )
        & encodeObject
    MA.WrongNetworkWithdrawal expected invalidAccts ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities
                    "rewardAccount"
                    Shelley.encodeRewardAcnt invalidAccts
            )
        & encodeObject
    MA.OutputTooSmallUTxO outs ->
        "outputTooSmall" .=
            encodeFoldable Shelley.encodeTxOut outs
        & encodeObject
    MA.OutputBootAddrAttrsTooBig outs ->
        "addressAttributesTooLarge" .=
            encodeFoldable
                Shelley.encodeAddress
                ((\(Sh.TxOut addr _) -> addr) <$> outs)
        & encodeObject
    MA.TriesToForgeADA ->
        "triesToForgeAda" .=
            encodeNull
        & encodeObject
    MA.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

encodeValidityInterval
    :: MA.ValidityInterval
    -> Json
encodeValidityInterval x =
    "invalidBefore" .=? OmitWhenNothing
        encodeSlotNo (MA.invalidBefore x) <>
    "invalidAfter" .=? OmitWhenNothing
        encodeSlotNo (MA.invalidHereafter x)
    & encodeObject

encodeWitnessSet
    :: Crypto crypto
    => Sh.WitnessSet (AllegraEra crypto)
    -> Series
encodeWitnessSet x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash encodeScript) (Sh.scriptWits x)
