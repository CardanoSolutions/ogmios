--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Shelley where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Slotting.Slot
    ( EpochNo )
import Control.Arrow
    ( right )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, CardanoEras, Query (..), ShelleyEra )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock, ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( Query (..) )
import Ouroboros.Network.Block
    ( Point, castPoint )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import qualified Cardano.Crypto.DSIGN.Class as CC
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Crypto.KES.Class as CC
import qualified Cardano.Crypto.VRF.Class as CC

import qualified Cardano.Api.Typed as Sh
import qualified Shelley.Spec.Ledger.Address as Sh
import qualified Shelley.Spec.Ledger.Address.Bootstrap as Sh
import qualified Shelley.Spec.Ledger.BaseTypes as Sh
import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Coin as Sh
import qualified Shelley.Spec.Ledger.Credential as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.Keys as Sh
import qualified Shelley.Spec.Ledger.LedgerState as Sh
import qualified Shelley.Spec.Ledger.MetaData as Sh
import qualified Shelley.Spec.Ledger.OCert as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.Scripts as Sh
import qualified Shelley.Spec.Ledger.STS.Deleg as Sh
import qualified Shelley.Spec.Ledger.STS.Delegs as Sh
import qualified Shelley.Spec.Ledger.STS.Delpl as Sh
import qualified Shelley.Spec.Ledger.STS.Ledger as Sh
import qualified Shelley.Spec.Ledger.STS.Ledgers as Sh
import qualified Shelley.Spec.Ledger.STS.Pool as Sh
import qualified Shelley.Spec.Ledger.STS.Ppup as Sh
import qualified Shelley.Spec.Ledger.STS.Utxo as Sh
import qualified Shelley.Spec.Ledger.STS.Utxow as Sh
import qualified Shelley.Spec.Ledger.Tx as Sh
import qualified Shelley.Spec.Ledger.TxBody as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

parseGetLedgerTip
    :: forall crypto f result era.
        ( era ~ ShelleyEra crypto
        , result ~ QueryResult crypto (Point (ShelleyBlock era))
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> (Point (CardanoBlock crypto) -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetLedgerTip genResult encodeMismatchEraInfo encodePoint =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> SomeQuery
            { query = QueryIfCurrentShelley GetLedgerTip
            , encodeResult = either encodeMismatchEraInfo (encodePoint . castPoint)
            , genResult
            }

parseGetEpochNo
    :: forall crypto f result.
        ( Crypto crypto
        , result ~ QueryResult crypto EpochNo
        )
    => (Proxy result -> f result)
    -> (MismatchEraInfo (CardanoEras crypto) -> Json)
    -> (EpochNo -> Json)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetEpochNo genResult encodeMismatchEraInfo encodeEpochNo =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentEpoch") $> SomeQuery
            { query = QueryIfCurrentShelley GetEpochNo
            , encodeResult = either encodeMismatchEraInfo encodeEpochNo
            , genResult
            }

