--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

-- This is used by local clients (like wallets, explorers and CLI tools) to
-- monitor the transactions passing through the mempool of a local node.
--
-- The protocol is stateful such that the server keeps track of the transactions
-- already sent to the client.
--
-- @
--                    START
--                      ⇓
--                    ┌───────────────┐
--            ┌──────▶│     Idle      │⇒ DONE
--            │       └───┬───────────┘
--            │           │
--            │   Acquire │
--            │           ▼
--            │       ┌───────────────┐
--    Release │       │   Acquiring   │
--            │       └───┬───────────┘
--            │           │       ▲
--            │  Acquired │       │ AwaitAcquire
--            │           ▼       │
--            │       ┌───────────┴───┐
--            └───────┤   Acquired    │
--                    └───┬───────────┘
--                        │       ▲
--  HasTx|NextTx|GetSizes │       │ Reply (HasTx|NextTx|GetSizes)
--                        ▼       │
--                    ┌───────────┴───┐
--                    │      Busy     │
--                    └───────────────┘
-- @
module Ogmios.App.Protocol.TxMonitor
    ( mkTxMonitorClient
    ) where

import Ogmios.Prelude hiding
    ( id
    )

import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    )
import Ogmios.Data.EraTranslation
    ( MostRecentEra
    )
import Ogmios.Data.Json
    ( Json
    )
import Ogmios.Data.Protocol.TxMonitor
    ( AcquireMempool (..)
    , AcquireMempoolResponse (..)
    , GenTx
    , GenTxId
    , HasTransaction (..)
    , HasTransactionResponse (..)
    , NextTransaction (..)
    , NextTransactionFields (..)
    , NextTransactionResponse (..)
    , ReleaseMempool (..)
    , ReleaseMempoolResponse (..)
    , SizeOfMempool (..)
    , SizeOfMempoolResponse (..)
    , SlotNo (..)
    , TxMonitorCodecs (..)
    , TxMonitorMessage (..)
    )
import Ouroboros.Consensus.Cardano.Block
    ( TxId (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( HasTxId (..)
    )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( NodeToClientVersion (..)
    )
import Ouroboros.Consensus.Shelley.Ledger.Mempool
    ( TxId (..)
    )
import Ouroboros.Network.Protocol.LocalTxMonitor.Client
    ( ClientStAcquired (..)
    , ClientStIdle (..)
    , LocalTxMonitorClient (..)
    )

import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.Json.Rpc as Rpc

mkTxMonitorClient
    :: forall m block crypto.
        ( MonadSTM m
        , HasTxId (GenTx block)
        , block ~ CardanoBlock crypto
        )
    => (forall a r. m a -> (Json -> m ()) -> Rpc.ToResponse r -> m a -> m a)
        -- ^ A default response handler to catch errors.
    -> TxMonitorCodecs block
        -- ^ For encoding Haskell types to JSON
    -> TQueue m (TxMonitorMessage block)
        -- ^ Incoming request queue
    -> (Json -> m ())
        -- ^ An emitter for yielding JSON objects
    -> NodeToClientVersion
        -- ^ Node-to-Client protocol version that was negotiated for this client
    -> LocalTxMonitorClient (GenTxId block) (GenTx block) SlotNo m ()
mkTxMonitorClient defaultWithInternalError TxMonitorCodecs{..} queue yield nodeToClientV =
    LocalTxMonitorClient clientStIdle
  where
    await :: m (TxMonitorMessage block)
    await = atomically (readTQueue queue)

    clientStIdle
        :: m (ClientStIdle (GenTxId block) (GenTx block) SlotNo m ())
    clientStIdle = await >>= \case
        MsgAcquireMempool AcquireMempool toResponse ->
            defaultWithInternalError clientStIdle yield toResponse $ do
                pure $ SendMsgAcquire $ \slot -> do
                    yield $ encodeAcquireMempoolResponse $ toResponse $ AcquireMempoolResponse slot
                    clientStAcquired
        MsgNextTransaction NextTransaction{} toResponse -> do
            yield $ encodeNextTransactionResponse $ toResponse NextTransactionMustAcquireFirst
            clientStIdle
        MsgHasTransaction HasTransaction{} toResponse -> do
            yield $ encodeHasTransactionResponse $ toResponse HasTransactionMustAcquireFirst
            clientStIdle
        MsgSizeOfMempool SizeOfMempool toResponse -> do
            yield $ encodeSizeOfMempoolResponse $ toResponse SizeOfMempoolMustAcquireFirst
            clientStIdle
        MsgReleaseMempool ReleaseMempool toResponse -> do
            yield $ encodeReleaseMempoolResponse $ toResponse ReleaseMempoolMustAcquireFirst
            clientStIdle

    clientStAcquired
        :: m (ClientStAcquired (GenTxId block) (GenTx block) SlotNo m ())
    clientStAcquired = await >>= \case
        MsgAcquireMempool AcquireMempool toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgAwaitAcquire $ \slot -> do
                    yield $ encodeAcquireMempoolResponse $ toResponse $ AcquireMempoolResponse slot
                    clientStAcquired
        MsgNextTransaction NextTransaction{fields} toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgNextTx $ \mTx -> do
                    let response = case fields of
                            Nothing ->
                                NextTransactionResponseId (txId <$> mTx)
                            Just NextTransactionAllFields ->
                                NextTransactionResponseTx mTx
                    yield $ encodeNextTransactionResponse $ toResponse response
                    clientStAcquired
        MsgHasTransaction HasTransaction{id} toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                -- NOTE:
                --
                -- Unfortunately here, we can't reliably ask the node for a
                -- transaction id because it performs equality on the GenTxId NS
                -- wrapper instead of the inner transaction id bytes.
                --
                -- As a consequence, sending a transaction id wrapped as a
                -- GenTxIdAlonzo will not match the same transaction id wrapped
                -- as a GenTxIdBabbage.
                --
                -- Yet, we cannot know upfront in what era those ids are wrapped
                -- in the mempool although we do commit to a specific NS summand
                -- when we deserialize it. So we have no other choice than
                -- retrying the request on `False` with ids wrapped in different
                -- eras.
                --
                -- To be removed once the following issue is addressed:
                --
                --   https://github.com/IntersectMBO/ouroboros-consensus/issues/1009
                loop (inMultipleEras nodeToClientV id)
          where
            done has = do
                yield $ encodeHasTransactionResponse $ toResponse $ HasTransactionResponse{has}
                clientStAcquired

            loop = \case
                [] -> done False
                genId:rest -> do
                    pure $ SendMsgHasTx genId $ \case
                        True  -> done True
                        False -> loop rest

        MsgSizeOfMempool SizeOfMempool toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgGetSizes $ \mempool -> do
                    yield $ encodeSizeOfMempoolResponse $ toResponse $ SizeOfMempoolResponse{mempool}
                    clientStAcquired
        MsgReleaseMempool ReleaseMempool toResponse ->
            defaultWithInternalError clientStAcquired yield toResponse $ do
                pure $ SendMsgRelease $ do
                    yield $ encodeReleaseMempoolResponse $ toResponse Released
                    clientStIdle

inMultipleEras
    :: forall crypto constraint.
        ( constraint ~ (MostRecentEra (CardanoBlock crypto) ~ ConwayEra)
        )
    => NodeToClientVersion
    -> Ledger.TxId
    -> [GenTxId (CardanoBlock crypto)]
inMultipleEras nodeToClientV id =
    -- The list is ordered from the "most probable era", down to the least
    -- probable. This hopefully ensures that we do a minimum number of loops
    -- for the happy path.
    GenTxIdBabbage (ShelleyTxId id) :
        if nodeToClientV >= NodeToClientV_16 then
            [ GenTxIdConway (ShelleyTxId id)
            , GenTxIdAlonzo (ShelleyTxId id)
            , GenTxIdMary (ShelleyTxId id)
            ]
        else
            [ GenTxIdAlonzo (ShelleyTxId id)
            , GenTxIdMary (ShelleyTxId id)
            ]
  where
    -- This line exists as a reminder. It will generate a compiler error
    -- when a new era becomes available. From there, one should update
    -- the list above to contain that latest era.
    _compilerWarning = keepRedundantConstraint (Proxy @constraint)
