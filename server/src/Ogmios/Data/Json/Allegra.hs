--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Allegra where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( serialize'
    )
import Data.Maybe.Strict
    ( fromSMaybe
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..)
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    ()

import qualified Data.Map as Map

import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Protocol.TPraos.BHeader as TPraos

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger

import qualified Cardano.Ledger.Shelley.BlockChain as Sh
import qualified Cardano.Ledger.Shelley.PParams as Sh
import qualified Cardano.Ledger.Shelley.Rules as Sh
import qualified Cardano.Ledger.Shelley.Tx as Sh
import qualified Cardano.Ledger.Shelley.TxWits as Sh
import qualified Cardano.Ledger.Shelley.UTxO as Sh

import qualified Cardano.Ledger.Allegra.Rules as Al
import qualified Cardano.Ledger.Allegra.Scripts as Al
import qualified Cardano.Ledger.Allegra.TxAuxData as Al
import qualified Cardano.Ledger.Allegra.TxBody as Al

type AuxiliaryScripts crypto =
    Map (Ledger.ScriptHash crypto) (Ledger.Script (AllegraEra crypto))

--
-- Encoders
--
encodeAuxiliaryData
    :: forall crypto. Crypto crypto
    => Al.AllegraTxAuxData (AllegraEra crypto)
    -> (Json, AuxiliaryScripts crypto)
encodeAuxiliaryData (Al.AllegraTxAuxData blob scripts) =
    ( Shelley.encodeMetadataBlob blob
    , foldr
        (\script -> Map.insert (Ledger.hashScript @(AllegraEra crypto) script) script)
        mempty
        scripts
    )

encodeBlock
    :: Crypto crypto
    => ShelleyBlock (TPraos crypto) (AllegraEra crypto)
    -> Json
encodeBlock (ShelleyBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
        ( "era" .= encodeText "allegra"
        <>
          "header" .= encodeObject
            ( "hash" .= Shelley.encodeShelleyHash headerHash
            )
        <>
        Shelley.encodeBHeader blkHeader
        <>
        "size" .= encodeNatural (TPraos.bsize hBody)
        <>
        "transactions" .= encodeFoldable encodeTx (Sh.txSeqTxns' txs)
        )
  where
    TPraos.BHeader hBody _ = blkHeader

encodeLedgerFailure
    :: Crypto crypto
    => Sh.ShelleyLedgerPredFailure (AllegraEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        Shelley.encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        Shelley.encodeDelegsFailure e

encodeProposedPPUpdates
    :: forall era.
        ( Era era
        , Ledger.PParamsHKD StrictMaybe era ~ Sh.ShelleyPParams StrictMaybe era
        )
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates =
    Shelley.encodeProposedPPUpdates

encodeScript
    :: Era era
    => Al.Timelock era
    -> Json
encodeScript timelock =
    encodeObject ("native" .= encodeTimelock timelock)

encodeTimelock
    :: Era era
    => Al.Timelock era
    -> Json
encodeTimelock = \case
    Al.RequireSignature sig ->
        Shelley.encodeKeyHash sig
    Al.RequireAllOf xs ->
        encodeObject ("all" .= encodeFoldable encodeTimelock xs)
    Al.RequireAnyOf xs ->
        encodeObject ("any" .= encodeFoldable encodeTimelock xs)
    Al.RequireMOf n xs ->
        encodeObject (show n .= encodeFoldable encodeTimelock xs)
    Al.RequireTimeExpire s ->
        encodeObject ("expiresAt" .= encodeSlotNo s)
    Al.RequireTimeStart s ->
        encodeObject ("startsAt" .= encodeSlotNo s)

encodeTx
    :: forall crypto. (Crypto crypto)
    => Sh.ShelleyTx (AllegraEra crypto)
    -> Json
encodeTx x =
    "id" .= Shelley.encodeTxId (Ledger.txid @(AllegraEra crypto) (Sh.body x))
        <>
    "inputSource" .= encodeText "inputs"
        <>
    encodeTxBody (Sh.body x)
        <>
    "metadata" .=? OmitWhenNothing fst auxiliary
        <>
    encodeWitnessSet (snd <$> auxiliary) (Sh.wits x)
        <>
    "cbor" .= encodeByteStringBase16 (serialize' x)
        & encodeObject
  where
    auxiliary = do
        hash <- Shelley.encodeAuxiliaryDataHash <$> Al.atbAuxDataHash (Sh.body x)
        (labels, scripts) <- encodeAuxiliaryData <$> Sh.auxiliaryData x
        pure
            ( encodeObject ("hash" .= hash <> "labels" .= labels)
            , scripts
            )

encodeTxBody
    :: Crypto crypto
    => Al.AllegraTxBody (AllegraEra crypto)
    -> Series
encodeTxBody (Al.AllegraTxBody inps outs certs wdrls fee validity updates _) =
    "inputs" .=
        encodeFoldable Shelley.encodeTxIn inps <>
    "outputs" .=
        encodeFoldable Shelley.encodeTxOut outs <>
    "withdrawals" .=? OmitWhen (null . Ledger.unWithdrawals)
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
    => Al.AllegraUtxoPredFailure (AllegraEra crypto)
    -> Json
encodeUtxoFailure = \case
    Al.BadInputsUTxO inputs ->
        "badInputs" .=
            encodeFoldable Shelley.encodeTxIn inputs
        & encodeObject
    Al.OutsideValidityIntervalUTxO itv currentSlot ->
        "outsideOfValidityInterval" .= encodeObject
            ( "interval" .= encodeValidityInterval itv <>
              "currentSlot" .= encodeSlotNo currentSlot
            )
        & encodeObject
    Al.OutputTooBigUTxO outs ->
        "tooManyAssetsInOutput" .=
            encodeFoldable Shelley.encodeTxOut outs
        & encodeObject
    Al.MaxTxSizeUTxO actualSize maxSize ->
        "txTooLarge" .= encodeObject
            ( "maximumSize" .=
                encodeInteger maxSize <>
              "actualSize" .=
                encodeInteger actualSize
            )
        & encodeObject
    Al.InputSetEmptyUTxO ->
        "missingAtLeastOneInputUtxo" .=
            encodeNull
        & encodeObject
    Al.FeeTooSmallUTxO required actual ->
        "feeTooSmall" .= encodeObject
            ( "requiredFee" .=
                encodeCoin required <>
              "actualFee" .=
                encodeCoin actual
            )
        & encodeObject
    Al.ValueNotConservedUTxO consumed produced ->
        "valueNotConserved" .= encodeObject
            ( "consumed" .=
                encodeCoin consumed <>
              "produced" .=
                encodeCoin produced
            )
        & encodeObject
    Al.WrongNetwork expected invalidAddrs ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities "address" Shelley.encodeAddress invalidAddrs
            )
        & encodeObject
    Al.WrongNetworkWithdrawal expected invalidAccts ->
        "networkMismatch" .= encodeObject
            ( "expectedNetwork" .=
                Shelley.encodeNetwork expected <>
              "invalidEntities" .=
                Shelley.encodeEntities
                    "rewardAccount"
                    Shelley.encodeRewardAcnt invalidAccts
            )
        & encodeObject
    Al.OutputTooSmallUTxO outs ->
        "outputTooSmall" .=
            encodeFoldable Shelley.encodeTxOut outs
        & encodeObject
    Al.OutputBootAddrAttrsTooBig outs ->
        "addressAttributesTooLarge" .=
            encodeFoldable
                Shelley.encodeAddress
                ((\(Sh.ShelleyTxOut addr _) -> addr) <$> outs)
        & encodeObject
    Al.TriesToForgeADA ->
        "triesToForgeAda" .=
            encodeNull
        & encodeObject
    Al.UpdateFailure e ->
        Shelley.encodeUpdateFailure e

encodeValidityInterval
    :: Al.ValidityInterval
    -> Json
encodeValidityInterval x =
    "invalidBefore" .=? OmitWhenNothing
        encodeSlotNo (Al.invalidBefore x) <>
    "invalidAfter" .=? OmitWhenNothing
        encodeSlotNo (Al.invalidHereafter x)
    & encodeObject

encodeWitnessSet
    :: Crypto crypto
    => StrictMaybe (AuxiliaryScripts crypto)
    -> Sh.ShelleyTxWits (AllegraEra crypto)
    -> Series
encodeWitnessSet (fromSMaybe mempty -> auxScripts) x =
    "signatories" .=
        encodeFoldable2
            Shelley.encodeBootstrapWitness
            Shelley.encodeWitVKey
            (Sh.bootWits x)
            (Sh.addrWits x) <>
    "scripts" .=? OmitWhen null
        (encodeMap Shelley.stringifyScriptHash encodeScript)
        (Sh.scriptWits x <> auxScripts)
