--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Alonzo where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.MemoBytes
    ( memobytes )
import GHC.Records
    ( getField )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( StrictMaybe (..) )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Allegra as Allegra
import qualified Ogmios.Data.Json.Shelley as Shelley

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.SafeHash as SafeHash

import qualified Shelley.Spec.Ledger.API as Spec
import qualified Shelley.Spec.Ledger.BlockChain as Spec
import qualified Shelley.Spec.Ledger.Tx as Spec
import qualified Shelley.Spec.Ledger.UTxO as Spec

import qualified Cardano.Ledger.Alonzo.Data as Al
import qualified Cardano.Ledger.Alonzo.Language as Al
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Al
import qualified Cardano.Ledger.Alonzo.PParams as Al
import qualified Cardano.Ledger.Alonzo.Rules.Bbody as Al
import qualified Cardano.Ledger.Alonzo.Rules.Ledger as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxos as Al
import qualified Cardano.Ledger.Alonzo.Rules.Utxow as Al
import qualified Cardano.Ledger.Alonzo.Scripts as Al
import qualified Cardano.Ledger.Alonzo.Translation as Al
import qualified Cardano.Ledger.Alonzo.Tx as Al
import qualified Cardano.Ledger.Alonzo.TxBody as Al
import qualified Cardano.Ledger.Alonzo.TxInfo as Al
import qualified Cardano.Ledger.Alonzo.TxSeq as Al
import qualified Cardano.Ledger.Alonzo.TxWitness as Al

--
-- Encoders
--

encodeAlonzoBlock
    :: Crypto crypto
    => SerializationMode
    -> ShelleyBlock (AlonzoEra crypto)
    -> Json
encodeAlonzoBlock mode (ShelleyBlock (Spec.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Al.txSeqTxns txs)
      )
    , ( "header"
      , Shelley.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Shelley.encodeShelleyHash headerHash
      )
    ]

encodeAuxiliaryData
    :: Crypto crypto
    => Al.AuxiliaryData (AlonzoEra crypto)
    -> Json
encodeAuxiliaryData (Al.AuxiliaryData blob scripts datums) = encodeObject
    [ ( "blob"
      , Shelley.encodeMetadataBlob blob
      )
    , ( "scripts"
      , encodeFoldable encodeScript scripts
      )
    , ( "datums"
      , encodeFoldable encodeData datums
      )
    ]

encodeData
    :: Al.Data era
    -> Json
encodeData (Al.DataConstr datum) =
    -- TODO: Check whether 'memobytes' really is what we want here. Might be good
    -- to strip away the extra CBOR wrapping this if any.
    encodeShortByteString encodeByteStringBase64 (memobytes datum)

encodeRedeemers
    :: Al.Redeemers era
    -> Json
encodeRedeemers =
    error "FIXME: encodeRedeemers"

encodeScript
    :: Crypto crypto
    => Al.Script (AlonzoEra crypto)
    -> Json
encodeScript = \case
    Al.TimelockScript nativeScript -> encodeObject
        [ ( "native"
          , Allegra.encodeTimelock nativeScript
          )
        ]
    Al.PlutusScript serializedScript -> encodeObject
        [ ( "plutus"
          , encodeShortByteString encodeByteStringBase64 serializedScript
          )
        ]

encodeTx
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Al.ValidatedTx (AlonzoEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Shelley.encodeTxId (Spec.txid @(AlonzoEra crypto) (Al.body x))
      )
      , ( "body"
        , encodeTxBody (Al.body x)
        )
      , ( "metadata", encodeObject
          [ ( "hash"
            , encodeStrictMaybe Shelley.encodeAuxiliaryDataHash (adHash (Al.body x))
            )
          , ( "body"
            , encodeStrictMaybe encodeAuxiliaryData (Al.auxiliaryData x)
            )
          ]
        )
    ]
    [ ( "witness"
      , encodeWitnessSet (Al.wits x)
      )
    ]
  where
    adHash :: Al.TxBody era -> StrictMaybe (Al.AuxiliaryDataHash (Era.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => Al.TxBody (AlonzoEra crypto)
    -> Json
encodeTxBody =
    error "FIXME: encodeTxBody"

encodeWitnessSet
    :: Crypto crypto
    => Al.TxWitness (AlonzoEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , encodeFoldable Shelley.encodeWitVKey (Al.txwitsVKey x)
      )
    , ( "scripts"
      , encodeMap Shelley.stringifyScriptHash encodeScript (Al.txscripts x)
      )
    , ( "datums"
      , encodeMap stringifyDataHash encodeData (Al.txdats x)
      )
    , ( "redeemers"
      , encodeRedeemers (Al.txrdmrs x)
      )
    , ( "bootstrap"
      , encodeFoldable Shelley.encodeBootstrapWitness (Al.txwitsBoot x)
      )
    ]

--
-- Conversion To Text
--

stringifyDataHash
    :: Al.DataHash crypto
    -> Text
stringifyDataHash (SafeHash.extractHash -> (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)
