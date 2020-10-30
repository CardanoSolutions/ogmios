--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Health
    (
    -- * Heath Check
      Health (..)
    , init
    , newClients

    -- * Internal
    , mkHealthCheckClient
    ) where

import Prelude hiding
    ( init )

import Ogmios.Json
    ()

import Cardano.Network.Protocol.NodeToClient
    ( Block, Clients (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_ )
import Control.Monad
    ( forever )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Aeson
    ( ToJSON (..), genericToJSON )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import GHC.Generics
    ( Generic )
import Network.TypedProtocol.Pipelined
    ( N (..) )
import Ogmios.Health.Trace
    ( TraceHealth (..) )
import Ogmios.Metrics
    ( Metrics )
import Ouroboros.Network.Block
    ( Tip (..), genesisPoint, getTipPoint )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined (..)
    , ClientPipelinedStIdle (..)
    , ClientPipelinedStIntersect (..)
    , ClientStNext (..)
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (..) )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..) )
import System.Time.Clock
    ( Debouncer (..), newDebouncer )

import qualified Data.Aeson as Json
import qualified Ogmios.Metrics as Metrics

-- | Capture some health heartbeat of the application. This is populated by two
-- things:
--
-- - A metric store which measure runtime statistics.
-- - An Ourobors local chain-sync client which follows the chain's tip.
data Health block = Health
    { lastKnownTip :: Tip block
    -- ^ Last known tip of the core node.
    , lastTipUpdate :: Maybe UTCTime
    -- ^ Date at which the last update was received.
    , metrics :: Metrics
    -- ^ Application metrics measured at regular interval
    } deriving (Generic, Eq, Show)

init :: Health block
init = Health
    { lastKnownTip = TipGenesis
    , lastTipUpdate = Nothing
    , metrics = Metrics.empty
    }

instance ToJSON (Tip block) => ToJSON (Health block) where
    toJSON = genericToJSON Json.defaultOptions

--
-- Ouroboros Client
--

-- | Instantiate a new set of Ouroboros mini-protocols clients. Note that only
-- the chain-sync client does something here. Others are just idling.
newClients
    :: Tracer IO (TraceHealth (Health Block))
    -> MVar (Health Block)
    -> Metrics.Sampler IO
    -> IO (Clients IO Block tx err)
newClients tr mvar sample = do
    Debouncer{debounce} <- newDebouncer 20
    return $ Clients
        { chainSyncClient = mkHealthCheckClient $ \lastKnownTip ->
            debounce $ modifyMVar_ mvar $ \_ -> do
                health <- Health lastKnownTip
                    <$> (Just <$> getCurrentTime)
                    <*> sample
                health <$ traceWith tr (HealthTick health)

        , localTxSubmissionClient =
            LocalTxSubmissionClient $ forever $ threadDelay 43200

        , localStateQueryClient =
            LocalStateQueryClient $ forever $ threadDelay 43200
        }

-- | Simple client that follows the chain by jumping directly to the tip and
-- notify a consumer for every tip change.
mkHealthCheckClient
    :: forall m block. (Monad m)
    => (Tip block -> m ())
    -> ChainSyncClientPipelined block (Tip block) m ()
mkHealthCheckClient notify =
    ChainSyncClientPipelined stInit
  where
    stInit
        :: m (ClientPipelinedStIdle Z block (Tip block) m ())
    stInit = pure $
        SendMsgFindIntersect [genesisPoint] $ stIntersect $ \tip -> pure $
            SendMsgFindIntersect [getTipPoint tip] $ stIntersect (const stIdle)

    stIntersect
        :: (Tip block -> m (ClientPipelinedStIdle Z block (Tip block) m ()))
        -> ClientPipelinedStIntersect block (Tip block) m ()
    stIntersect stFound = ClientPipelinedStIntersect
        { recvMsgIntersectNotFound = const stInit
        , recvMsgIntersectFound = const stFound
        }

    stIdle
        :: m (ClientPipelinedStIdle Z block (Tip block) m ())
    stIdle = pure $
        SendMsgRequestNext stNext (pure stNext)

    stNext
        :: ClientStNext Z block (Tip block) m ()
    stNext = ClientStNext
        { recvMsgRollForward  = const check
        , recvMsgRollBackward = const check
        }
      where
        check tip = notify tip *> stIdle
