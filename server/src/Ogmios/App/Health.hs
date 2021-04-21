--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module Ogmios.App.Health
    ( -- * Health
      Health (..)
    , emptyHealth
    , healthCheck

      -- * HealthCheckClient
    , HealthCheckClient
    , newHealthCheckClient
    , connectHealthCheckClient

    -- * Logging
    , TraceHealth (..)
    ) where

import Relude hiding
    ( STM
    , TVar
    , atomically
    , newEmptyTMVar
    , putTMVar
    , readTVar
    , takeTMVar
    , writeTVar
    )

import Ogmios.App.Metrics
    ( RuntimeStats, Sampler, Sensors )
import Ogmios.App.Options
    ( NetworkParameters (..), Options (..) )
import Ogmios.Control.Exception
    ( IOException
    , MonadCatch (..)
    , MonadThrow (..)
    , isAsyncException
    , isDoesNotExistError
    , isResourceVanishedError
    , isTryAgainError
    )
import Ogmios.Control.MonadAsync
    ( MonadAsync )
import Ogmios.Control.MonadClock
    ( Debouncer (..), MonadClock (..), idle, _5s )
import Ogmios.Control.MonadLog
    ( HasSeverityAnnotation (..)
    , Logger
    , MonadLog (..)
    , Severity (..)
    , natTracer
    , nullTracer
    )
import Ogmios.Control.MonadMetrics
    ( MonadMetrics )
import Ogmios.Control.MonadOuroboros
    ( MonadOuroboros )
import Ogmios.Control.MonadSTM
    ( MonadSTM (..)
    , TVar
    , newEmptyTMVar
    , putTMVar
    , readTVar
    , takeTMVar
    , writeTVar
    )
import Ogmios.Data.Health
    ( CardanoEra (..)
    , Health (..)
    , NetworkSynchronization
    , emptyHealth
    , mkNetworkSynchronization
    )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras )

import qualified Ogmios.App.Metrics as Metrics

import Cardano.Network.Protocol.NodeToClient
    ( Block
    , Clients (..)
    , SubmitTxError
    , SubmitTxPayload
    , connectClient
    , mkClient
    )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Time.Clock
    ( DiffTime )
import Network.TypedProtocol.Pipelined
    ( N (..) )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime )
import Ouroboros.Consensus.HardFork.Combinator
    ( HardForkBlock, eraIndexToInt )
import Ouroboros.Consensus.HardFork.History.Qry
    ( interpretQuery, slotToWallclock )
import Ouroboros.Network.Block
    ( Point (..), Tip (..), genesisPoint, getTipPoint )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (NodeToClientVersionData) )
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

import qualified Ouroboros.Consensus.HardFork.Combinator as LSQ
import qualified Ouroboros.Consensus.Shelley.Ledger.Query as Ledger
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as LSQ

-- | Construct a health check by sampling all application sensors.
--
-- See also 'Ogmios.App.Protocol.ChainSync#mkHealthCheckClient'
healthCheck
    :: forall m block.
        ( MonadClock m
        , MonadMetrics m
        , MonadSTM m
        )
    => STM m (Tip block, NetworkSynchronization, CardanoEra)
    -> TVar m (Health block)
    -> Sensors m
    -> Sampler RuntimeStats m
    -> m (Health block)
healthCheck readTip tvar sensors sampler = do
    lastTipUpdate <- Just <$> getCurrentTime
    metrics <- Metrics.sample sampler sensors
    atomically $ do
        Health{startTime} <- readTVar tvar
        (lastKnownTip, networkSynchronization, currentEra) <- readTip
        let health = Health
                { startTime
                , lastKnownTip
                , lastTipUpdate
                , networkSynchronization
                , currentEra
                , metrics
                }
        health <$ writeTVar tvar health

--
-- HealthCheck Client
--


-- | A simple wrapper around Ouroboros 'Clients'. A health check client only
-- carries a chain-sync client.
newtype HealthCheckClient m
    = HealthCheckClient (Clients m Block)

-- | Instantiate a new set of Ouroboros mini-protocols clients. Note that only
-- the chain-sync client does something here. Others are just idling.
newHealthCheckClient
    :: forall m env.
        ( MonadAsync m
        , MonadClock m
        , MonadLog m
        , MonadMetrics m
        , MonadReader env m
        , MonadThrow m
        , HasType (TVar m (Health Block)) env
        , HasType (Sensors m) env
        , HasType (Sampler RuntimeStats m) env
        , HasType NetworkParameters env
        )
    => Logger (TraceHealth (Health Block))
    -> Debouncer m
    -> m (HealthCheckClient m)
newHealthCheckClient tr Debouncer{debounce} = do
    NetworkParameters{systemStart} <- asks (view typed)
    (stateQueryClient, getNetworkInformation) <- newTimeInterpreterClient
    pure $ HealthCheckClient $ Clients
        { chainSyncClient = mkHealthCheckClient $ \lastKnownTip -> debounce $ do
            tvar <- asks (view typed)
            sensors <- asks (view typed)
            sampler <- asks (view typed)
            now <- getCurrentTime
            (currentSlotTime, currentEra) <- getNetworkInformation lastKnownTip
            let networkSync = mkNetworkSynchronization systemStart now currentSlotTime
            let tipInfo = (lastKnownTip, networkSync, currentEra)
            health <- healthCheck (pure tipInfo) tvar sensors sampler
            logWith tr (HealthTick health)

        , txSubmissionClient =
            LocalTxSubmissionClient idle

        , stateQueryClient
        }

connectHealthCheckClient
    :: forall m env.
        ( MonadIO m -- Needed by 'connectClient'
        , MonadCatch m
        , MonadClock m
        , MonadLog m
        , MonadOuroboros m
        , MonadReader env m
        , HasType NetworkParameters env
        , HasType Options env
        )
    => Logger (TraceHealth (Health Block))
    -> (forall a. m a -> IO a)
    -> HealthCheckClient m
    -> m ()
connectHealthCheckClient tr embed (HealthCheckClient clients) = do
    NetworkParameters{slotsPerEpoch,networkMagic} <- asks (view typed)
    Options{nodeSocket} <- asks (view typed)
    let trClient = natTracer liftIO $ contramap HealthClient tr
    let client = mkClient embed trClient slotsPerEpoch clients
    connectClient nullTracer client (NodeToClientVersionData networkMagic) nodeSocket
        & onExceptions nodeSocket
        & foreverCalmly
  where
    onExceptions nodeSocket
        = handle onUnknownException
        . handle (onIOException nodeSocket)

    foreverCalmly :: m a -> m a
    foreverCalmly a = do
        let a' = a *> threadDelay _5s *> a' in a'

    onUnknownException :: SomeException -> m ()
    onUnknownException e
        | isAsyncException e = do
            logWith tr $ HealthShutdown e
            throwIO e
        | otherwise =
            logWith tr $ HealthUnknownException e

    onIOException :: FilePath -> IOException -> m ()
    onIOException nodeSocket e
        | isRetryable = do
            logWith tr $ HealthFailedToConnect nodeSocket _5s
        | otherwise = do
            logWith tr $ HealthUnknownException (toException e)
      where
        isRetryable :: Bool
        isRetryable = isResourceVanishedError e || isDoesNotExistError e || isTryAgainError e

--
-- Ouroboros clients
--

-- | Simple client that follows the chain by jumping directly to the tip and
-- notify a consumer for every tip change.
mkHealthCheckClient
    :: forall m block.
        ( Monad m
        )
    => (Tip block -> m ())
    -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkHealthCheckClient notify =
    ChainSyncClientPipelined stInit
  where
    stInit
        :: m (ClientPipelinedStIdle Z block (Point block) (Tip block) m ())
    stInit = pure $
        SendMsgFindIntersect [genesisPoint] $ stIntersect $ \tip -> pure $
            SendMsgFindIntersect [getTipPoint tip] $ stIntersect (const stIdle)

    stIntersect
        :: (Tip block -> m (ClientPipelinedStIdle Z block (Point block) (Tip block) m ()))
        -> ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    stIntersect stFound = ClientPipelinedStIntersect
        { recvMsgIntersectNotFound = const stInit
        , recvMsgIntersectFound = const stFound
        }

    stIdle
        :: m (ClientPipelinedStIdle Z block (Point block) (Tip block) m ())
    stIdle = pure $
        SendMsgRequestNext stNext (pure stNext)

    stNext
        :: ClientStNext Z block (Point block) (Tip block) m ()
    stNext = ClientStNext
        { recvMsgRollForward  = const check
        , recvMsgRollBackward = const check
        }
      where
        check tip = notify tip *> stIdle

-- | A simple client which is used to determine some metrics about the
-- underlying node. In particular, it allows for knowing the network
-- synchronization of the underlying node, as well as the current era of that
-- node.
newTimeInterpreterClient
    :: forall m crypto block.
        ( MonadThrow m
        , MonadSTM m
        , block ~ HardForkBlock (CardanoEras crypto)
        )
    => m ( LocalStateQueryClient block (Point block) (Ledger.Query block) m ()
         , Tip block -> m (RelativeTime, CardanoEra)
         )
newTimeInterpreterClient = do
    notifyTip <- atomically newEmptyTMVar
    getResult <- atomically newEmptyTMVar
    return
        ( LocalStateQueryClient $ clientStIdle
            (atomically $ takeTMVar notifyTip)
            (\a0 a1 -> atomically $ putTMVar getResult (a0, a1))
        , \tip -> do
            atomically $ putTMVar notifyTip tip
            atomically $ takeTMVar getResult
        )
  where
    clientStIdle
        :: m (Tip block)
        -> (RelativeTime -> CardanoEra -> m ())
        -> m (LSQ.ClientStIdle block (Point block) (Ledger.Query block) m ())
    clientStIdle getTip notifyResult =
        pure $ LSQ.SendMsgAcquire Nothing $ LSQ.ClientStAcquiring
            { LSQ.recvMsgAcquired = do
                clientStAcquired getTip notifyResult
            , LSQ.recvMsgFailure = -- Impossible in practice
                const (clientStIdle getTip notifyResult)
            }

    clientStAcquired
        :: m (Tip block)
        -> (RelativeTime -> CardanoEra -> m ())
        -> m (LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ())
    clientStAcquired getTip notifyResult = do
        tip <- getTip
        pure $ LSQ.SendMsgReAcquire Nothing $ LSQ.ClientStAcquiring
            { LSQ.recvMsgAcquired = do
                let continuation = clientStAcquired getTip notifyResult
                pure (clientStQuerySlotTime notifyResult tip continuation)

            , LSQ.recvMsgFailure = -- Impossible in practice
                const (clientStIdle (pure tip) notifyResult)
            }

    clientStQuerySlotTime
        :: (RelativeTime -> CardanoEra -> m ())
        -> (Tip block)
        -> m (LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ())
        -> LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ()
    clientStQuerySlotTime notifyResult tip continue =
        LSQ.SendMsgQuery (LSQ.QueryHardFork LSQ.GetInterpreter) $ LSQ.ClientStQuerying
            { LSQ.recvMsgResult = \interpreter -> do
                let slot = case tip of
                        TipGenesis -> 0
                        Tip sl _ _ -> sl
                case interpreter `interpretQuery` slotToWallclock slot of
                    -- NOTE: This request cannot fail in theory because the tip
                    -- is always known of the interpreter. If that every happens
                    -- because of some weird condition, retrying should do.
                    Left{} ->
                        pure (clientStQuerySlotTime notifyResult tip continue)
                    Right (slotTime, _) -> do
                        pure (clientStQueryCurrentEra (notifyResult slotTime) continue)
            }

    clientStQueryCurrentEra
        :: (CardanoEra -> m ())
        -> m (LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ())
        -> LSQ.ClientStAcquired block (Point block) (Ledger.Query block) m ()
    clientStQueryCurrentEra notifyResult continue =
        LSQ.SendMsgQuery (LSQ.QueryHardFork LSQ.GetCurrentEra) $ LSQ.ClientStQuerying
            { LSQ.recvMsgResult = \eraIndex -> do
                notifyResult $ case eraIndexToInt eraIndex of
                    0 -> Byron
                    1 -> Shelley
                    2 -> Allegra
                    3 -> Mary
                    _ -> Alonzo
                continue
            }

--
-- Logging
--

data TraceHealth s where
    HealthClient
        :: TraceClient (SubmitTxPayload Block) (SubmitTxError Block)
        -> TraceHealth s

    HealthTick
        :: { status :: s }
        -> TraceHealth s

    HealthFailedToConnect
        :: { socket :: FilePath, retryingIn :: DiffTime }
        -> TraceHealth s

    HealthShutdown
        :: { action :: SomeException }
        -> TraceHealth s

    HealthUnknownException
        :: { exception :: SomeException }
        -> TraceHealth s

deriving instance Show s => Show (TraceHealth s)

instance HasSeverityAnnotation (TraceHealth s) where
    getSeverityAnnotation = \case
        HealthClient msg -> getSeverityAnnotation msg
        HealthTick{} -> Info
        HealthFailedToConnect{} -> Warning
        HealthShutdown{} -> Notice
        HealthUnknownException{} -> Error
