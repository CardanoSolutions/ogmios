--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Ledger.PredicateFailure where

import Ogmios.Data.Json.Prelude

import Ogmios.Data.Ledger.PredicateFailure
    ( MultiEraPredicateFailure (..)
    )

import Ogmios.Data.Json.Shelley as Shelley

import qualified Codec.Json.Rpc as Rpc

encodePredicateFailure
    :: Crypto crypto
    => (Rpc.FaultCode -> String -> Maybe Json -> Json)
    -> MultiEraPredicateFailure crypto
    -> Json
encodePredicateFailure reject = \case
    InvalidSignatures { culpritVerificationKeys } ->
        reject (Rpc.FaultCustom 3006)
            "Some signatures are invalid. 'data.verificationKeys' contains a list of \
            \keys for which the signature didn't verify."
            (pure $ encodeObject
                ( "verificationKeys" .=
                    encodeFoldable Shelley.encodeVKey culpritVerificationKeys
                )
            )
    _ ->
        error "TODO: encodePredicateFailure"
