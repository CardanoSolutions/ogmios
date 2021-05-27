--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

-- | The state query protocol is likely the most versatile of the three Ouroboros
-- mini-protocols. As a matter of fact, it allows for querying various types of
-- information directly from the ledger. In essence, it is like a very simpler
-- request/response pattern where the types of questions one can ask are
-- specified by the protocols. Those questions include: information about the
-- chain tip, information about stake pools but also the balance of a particular
-- address.
--
-- In order to run a question by the ledger, one must first acquire a particular
-- position on the chain, so that the node can reliably answer a few questions
-- on a chosen, frozen state while continuing maintaining more recent version of
-- the ledger on the side. It is important to note that:
--
-- 1. The node cannot acquire any arbitrary state. One can only rewind up
--    to a certain point.
--
-- 2. Should a client keep a state acquired for too long, it is likely to become
--    unreachable at some point, forcing clients to re-acquire.
--
-- @
--                     ┌───────────────┐
--             ┌──────▶│     Idle      │⇦ START
--             │       └───┬───────────┘
--             │           │       ▲
--             │   Acquire │       │ Failure
--             │           ▼       │
--             │       ┌───────────┴───┐
--     Release │       │   Acquiring   │◀─────────────────┐
--             │       └───┬───────────┘                  │
--             │           │       ▲                      │ Result
--             │  Acquired │       │ ReAcquire            │
--             │           ▼       │                      │
--             │       ┌───────────┴───┐         ┌────────┴───────┐
--             └───────┤   Acquired    │────────▶│    Querying    │
--                     └───────────────┘         └────────────────┘
-- @
module Ogmios.App.Protocol.StateQuery
    ( mkStateQueryClient
    ) where

import Ogmios.Prelude

import Ogmios.Control.Exception
    ( MonadThrow )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..), TQueue, readTQueue )
import Ogmios.Data.Json
    ( Json )
import Ogmios.Data.Json.Query
    ( QueryInEra, ShelleyBasedEra (..), SomeQuery (..), SomeShelleyEra (..) )
import Ogmios.Data.Protocol.StateQuery
    ( Acquire (..)
    , AcquireResponse (..)
    , Query (..)
    , QueryResponse (..)
    , Release (..)
    , ReleaseResponse (..)
    , StateQueryCodecs (..)
    , StateQueryMessage (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras )
import Ouroboros.Consensus.HardFork.Combinator
    ( HardForkBlock, eraIndexToInt )
import Ouroboros.Network.Block
    ( Point (..) )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )

import qualified Codec.Json.Wsp as Wsp
import qualified Ouroboros.Consensus.HardFork.Combinator as LSQ
import qualified Ouroboros.Consensus.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

-- | A generic state-query client, which receives commands from a queue, and
-- yield results as JSON.
--
-- This client is meant to be driven by another client (e.g. from a WebSocket
-- connection) and simply ensures correct execution of the state-query protocol.
-- In particular, it also makes it easier to run queries _in the current era_.
mkStateQueryClient
    :: forall m crypto block point query.
        ( MonadThrow m
        , MonadSTM m
        , block ~ HardForkBlock (CardanoEras crypto)
        , point ~ Point block
        , query ~ Ledger.Query block
        )
    => StateQueryCodecs block
        -- ^ For encoding Haskell types to JSON
    -> TQueue m (StateQueryMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> LocalStateQueryClient block point query m ()
mkStateQueryClient StateQueryCodecs{..} queue yield =
    LocalStateQueryClient clientStIdle
  where
    await :: m (StateQueryMessage block)
    await = atomically (readTQueue queue)

    clientStIdle
        :: m (LSQ.ClientStIdle block point query m ())
    clientStIdle = await >>= \case
        MsgAcquire (Acquire pt) toResponse ->
            pure $ LSQ.SendMsgAcquire (Just pt) (clientStAcquiring pt toResponse)
        MsgRelease Release toResponse -> do
            yield $ encodeReleaseResponse (toResponse Released)
            clientStIdle
        MsgQuery query toResponse -> do
            pure $ LSQ.SendMsgAcquire Nothing (clientStAcquiringTip query toResponse)

    clientStAcquiring
        :: Point block
        -> Wsp.ToResponse (AcquireResponse block)
        -> LSQ.ClientStAcquiring block point query m ()
    clientStAcquiring pt toResponse =
        LSQ.ClientStAcquiring
            { LSQ.recvMsgAcquired = do
                yield $ encodeAcquireResponse $ toResponse $ AcquireSuccess pt
                clientStAcquired
            , LSQ.recvMsgFailure = \failure -> do
                yield $ encodeAcquireResponse $ toResponse $ AcquireFailure failure
                clientStIdle
            }

    clientStAcquiringTip
        :: Query block
        -> Wsp.ToResponse (QueryResponse block)
        -> LSQ.ClientStAcquiring block point query m ()
    clientStAcquiringTip (Query queryInEra) toResponse =
        LSQ.ClientStAcquiring
            { LSQ.recvMsgAcquired = do
                withCurrentEra queryInEra $ \case
                    Nothing -> do
                        let response = QueryUnavailableInCurrentEra
                        yield $ encodeQueryResponse $ toResponse response
                        pure $ LSQ.SendMsgRelease clientStIdle

                    Just (SomeQuery query encodeResult _) ->
                        pure $ LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                            { LSQ.recvMsgResult = \result -> do
                                let response = QueryResponse $ encodeResult result
                                yield $ encodeQueryResponse $ toResponse response
                                pure $ LSQ.SendMsgRelease clientStIdle
                            }

            , LSQ.recvMsgFailure = \failure -> do
                let response = QueryAcquireFailure failure
                yield $ encodeQueryResponse $ toResponse response
                clientStIdle
            }

    clientStAcquired
        :: m (LSQ.ClientStAcquired block point query m ())
    clientStAcquired = await >>= \case
        MsgAcquire (Acquire pt) toResponse ->
            pure $ LSQ.SendMsgReAcquire (Just pt) (clientStAcquiring pt toResponse)
        MsgRelease Release toResponse -> do
            yield $ encodeReleaseResponse (toResponse Released)
            pure $ LSQ.SendMsgRelease clientStIdle
        MsgQuery (Query queryInEra) toResponse ->
            withCurrentEra queryInEra $ \case
                Nothing -> do
                    let response = QueryUnavailableInCurrentEra
                    yield $ encodeQueryResponse $ toResponse response
                    clientStAcquired

                Just (SomeQuery query encodeResult _) -> pure $
                    LSQ.SendMsgQuery query $ LSQ.ClientStQuerying
                        { LSQ.recvMsgResult = \result -> do
                            let response = QueryResponse $ encodeResult result
                            yield $ encodeQueryResponse $ toResponse response
                            clientStAcquired
                        }

--
-- Helpers
--

-- | Run a query in the context of the current era. As a matter of fact, queries
-- are typed and bound to a particular era. Different era may support small
-- variations of the same queries.
--
-- This is quite cumbersome to handle client-side and usually not desirable. In
-- most cases:
--
-- - Query don't change from an era to another
-- - New eras may add new queries
-- - Clients only care about queries available in the current / latest era
--
-- Thus, Ogmios is doing the "heavy lifting" by sending queries directly in the
-- current era, if they exist / are compatible.
withCurrentEra
    :: forall crypto block point query m f.
        ( block ~ HardForkBlock (CardanoEras crypto)
        , query ~ Ledger.Query block
        , Applicative m
        )
    => QueryInEra f block
    -> (Maybe (SomeQuery f block) -> m (LSQ.ClientStAcquired block point query m ()))
    -> m (LSQ.ClientStAcquired block point query m ())
withCurrentEra queryInEra callback = pure
    $ LSQ.SendMsgQuery (Ledger.BlockQuery $ LSQ.QueryHardFork LSQ.GetCurrentEra)
    $ LSQ.ClientStQuerying
        { LSQ.recvMsgResult = \eraIndex ->
            callback (toSomeShelleyEra eraIndex >>= queryInEra)
        }

-- | Convert an 'EraIndex' to a Shelley-based era.
toSomeShelleyEra
    :: forall crypto. ()
    => LSQ.EraIndex (CardanoEras crypto)
    -> Maybe SomeShelleyEra
toSomeShelleyEra =
    indexToSomeShelleyEra . eraIndexToInt
  where
    indexToSomeShelleyEra = \case
        1 -> Just (SomeShelleyEra ShelleyBasedEraShelley)
        2 -> Just (SomeShelleyEra ShelleyBasedEraAllegra)
        3 -> Just (SomeShelleyEra ShelleyBasedEraMary)
        _ -> Nothing
