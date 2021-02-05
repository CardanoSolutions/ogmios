--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Data.Json.Query
    ( -- * Types
      QueryResult

      -- * Encoders
    , encodeEraMismatch
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint

      -- * Parsers
    , parseGetLedgerTip
    , parseGetEpochNo
    , parseGetNonMyopicMemberRewards
    , parseGetCurrentPParams
    , parseGetProposedPParamsUpdates
    , parseGetStakeDistribution
    , parseGetUTxO
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Slotting.Slot
    ( EpochNo (..), WithOrigin (..) )
import Data.Aeson
    ( (.:) )
import Ouroboros.Consensus.Cardano.Block
    ( AllegraEra, CardanoBlock, CardanoEras, Query (..) )
import Ouroboros.Consensus.HardFork.Combinator
    ( MismatchEraInfo, OneEraHash (..) )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Ouroboros.Consensus.Shelley.Ledger.Block
    ( ShelleyBlock (..), ShelleyHash (..) )
import Ouroboros.Consensus.Shelley.Ledger.Query
    ( NonMyopicMemberRewards (..), Query (..) )
import Ouroboros.Network.Block
    ( Point (..), castPoint )
import Ouroboros.Network.Point
    ( Block (..) )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Shelley.Spec.Ledger.BlockChain as Sh
import qualified Shelley.Spec.Ledger.Coin as Sh
import qualified Shelley.Spec.Ledger.Credential as Sh
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Sh
import qualified Shelley.Spec.Ledger.Keys as Sh
import qualified Shelley.Spec.Ledger.PParams as Sh
import qualified Shelley.Spec.Ledger.UTxO as Sh

import qualified Ogmios.Data.Json.Shelley as Shelley

--
-- Encoders
--

encodeEraMismatch
    :: EraMismatch
    -> Json
encodeEraMismatch x = encodeObject
    [ ( "eraMismatch", encodeObject
        [ ( "ledgerEra"
          , encodeText (ledgerEraName x)
          )
        , ( "queryEra"
          , encodeText (otherEraName x)
          )
        ]
      )
    ]

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeNonMyopicMemberRewards
    :: NonMyopicMemberRewards era
    -> Json
encodeNonMyopicMemberRewards (NonMyopicMemberRewards nonMyopicMemberRewards) =
    encodeMap
        (either Shelley.stringifyCoin Shelley.stringifyCredential)
        (encodeMap Shelley.stringifyPoolId Shelley.encodeCoin)
        nonMyopicMemberRewards

encodeOneEraHash
    :: OneEraHash eras
    -> Json
encodeOneEraHash =
    encodeShortByteString encodeByteStringBase16 . getOneEraHash

encodePoint
    :: Point (CardanoBlock crypto)
    -> Json
encodePoint = \case
    Point Origin -> encodeText "origin"
    Point (At x) -> encodeObject
        [ ( "slot"
          , encodeSlotNo (blockPointSlot x)
          )
        , ( "hash"
          , encodeOneEraHash (blockPointHash x)
          )
        ]

--
-- Parsers
--

parseGetLedgerTip
    :: forall crypto f result era.
        ( era ~ AllegraEra crypto
        , result ~ QueryResult crypto (Point (ShelleyBlock era))
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetLedgerTip genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> SomeQuery
            { query =
                QueryIfCurrentAllegra GetLedgerTip
            , encodeResult =
                either encodeMismatchEraInfo (encodePoint . castPoint)
            , genResult
            }

parseGetEpochNo
    :: forall crypto f result.
        ( result ~ QueryResult crypto EpochNo
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetEpochNo genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentEpoch") $> SomeQuery
            { query =
                QueryIfCurrentAllegra GetEpochNo
            , encodeResult =
                either encodeMismatchEraInfo encodeEpochNo
            , genResult
            }

parseGetNonMyopicMemberRewards
    :: forall crypto f result.
        ( Crypto crypto
        , result ~ QueryResult crypto (NonMyopicMemberRewards crypto)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetNonMyopicMemberRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        arg <- obj .: "nonMyopicMemberRewards" >>= traverse
            (choice "credential"
                [ fmap Left  . parseCoin
                , fmap Right . parseCredential
                ]
            )
        pure $ SomeQuery
            { query =
                QueryIfCurrentAllegra (GetNonMyopicMemberRewards $ fromList arg)
            , encodeResult =
                either encodeMismatchEraInfo encodeNonMyopicMemberRewards
            , genResult
            }

parseGetCurrentPParams
    :: forall crypto f result era.
        ( era ~ AllegraEra crypto
        , result ~ QueryResult crypto (Sh.PParams era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetCurrentPParams genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentProtocolParameters") $> SomeQuery
            { query =
                QueryIfCurrentAllegra GetCurrentPParams
            , encodeResult =
                either encodeMismatchEraInfo (Shelley.encodePParams' id)
            , genResult
            }

parseGetProposedPParamsUpdates
    :: forall crypto f result era.
        ( era ~ AllegraEra crypto
        , result ~ QueryResult crypto (Sh.ProposedPPUpdates era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetProposedPParamsUpdates genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "proposedProtocolParameters") $> SomeQuery
            { query =
                QueryIfCurrentAllegra GetProposedPParamsUpdates
            , encodeResult =
                either encodeMismatchEraInfo Shelley.encodeProposedPPUpdates
            , genResult
            }

parseGetStakeDistribution
    :: forall crypto f result.
        ( result ~ QueryResult crypto (Sh.PoolDistr crypto)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetStakeDistribution genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "stakeDistribution") $> SomeQuery
            { query =
                QueryIfCurrentAllegra GetStakeDistribution
            , encodeResult =
                either encodeMismatchEraInfo Shelley.encodePoolDistr
            , genResult
            }

parseGetUTxO
    :: forall crypto f result era.
        ( Crypto crypto
        , era ~ AllegraEra crypto
        , result ~ QueryResult crypto (Sh.UTxO era)
        )
    => (Proxy result -> f result)
    -> Json.Value
    -> Json.Parser (SomeQuery f (CardanoBlock crypto))
parseGetUTxO genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "utxo") $> SomeQuery
            { query =
                QueryIfCurrentAllegra GetUTxO
            , encodeResult =
                either encodeMismatchEraInfo Shelley.encodeUTxO
            , genResult
            }

-- parseGetFilteredUTxO
--     :: forall crypto f result era.
--         ( Crypto crypto
--         , era ~ ShelleyEra crypto
--         , result ~ QueryResult crypto (Sh.UTxO era)
--         )
--     => (Proxy result -> f result)
--     -> Json.Value
--     -> Json.Parser (SomeQuery f Json.Value (CardanoBlock crypto))
-- parseGetFilteredUTxO genResult = Json.withObject "SomeQuery" $ \obj -> do
--     addrs <- obj .: "utxo" >>= traverse parseAddress
--     pure SomeQuery
--         { query = QueryIfCurrentAllegra (GetFilteredUTxO $ fromList addrs)
--         , encodeResult = toAltJSON
--         , genResult
--         }
--   where
--     parseAddress :: Json.Value -> Json.Parser (Sh.Addr era)
--     parseAddress = Json.withText "Address" $ choice "address"
--         [ addressFromBytes fromBech32
--         , addressFromBytes fromBase58
--         , addressFromBytes fromBase16
--         ]
--       where
--         addressFromBytes decode =
--             decode >=> maybe mempty pure . Sh.deserialiseAddr
--
--         fromBech32 txt =
--             case Bech32.decodeLenient txt of
--                 Left e ->
--                     fail (show e)
--                 Right (_, dataPart) ->
--                     maybe mempty pure $ Bech32.dataPartToBytes dataPart
--
--         fromBase58 =
--             maybe mempty pure . decodeBase58 bitcoinAlphabet . encodeUtf8
--
--         fromBase16 =
--             either (fail . show) pure . convertFromBase @ByteString Base16 . encodeUtf8

--
-- Internal
--

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

parseCoin
    :: Json.Value
    -> Json.Parser Sh.Coin
parseCoin =
    fmap Sh.word64ToCoin . Json.parseJSON

-- TODO: Makes it possible to distinguish between KeyHash and ScriptHash
-- credentials. Both are encoded as hex-encoded strings. Encoding them as
-- object is ill-advised because we also need them as key of the non-myopic
-- member rewards map.
--
-- A possible option: encode them as Bech32 strings with different prefixes.
parseCredential
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Sh.Credential 'Sh.Staking crypto)
parseCredential =
    fmap (Sh.KeyHashObj . Sh.KeyHash) . parseHash


parseHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
parseHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex
