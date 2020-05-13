--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ogmios.Trace
    ( TraceOgmios (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Byron.Constants.Trace
    ( TraceLookup )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient )
import Control.Exception
    ( IOException, SomeException )
import Data.ByteString
    ( ByteString )
import Ogmios.Health.Trace
    ( TraceHealth )

data TraceOgmios where
    OgmiosClient
        :: forall tx err. (Show tx, Show err)
        => TraceClient tx err
        -> TraceOgmios

    OgmiosLookupEnv
        :: TraceLookup
        -> TraceOgmios

    OgmiosStarted
        :: { host :: String, port :: Int }
        -> TraceOgmios

    OgmiosHealth
        :: forall s. Show s
        => { health :: TraceHealth s }
        -> TraceOgmios

    OgmiosConnectionAccepted
        :: { userAgent :: ByteString }
        -> TraceOgmios

    OgmiosConnectionEnded
        :: { userAgent :: ByteString }
        -> TraceOgmios

    OgmiosSocketNotFound
        :: { path :: FilePath }
        -> TraceOgmios

    OgmiosFailedToConnect
        :: { ioException :: IOException }
        -> TraceOgmios

    OgmiosUnknownException
        :: { exception :: SomeException }
        -> TraceOgmios

deriving instance Show TraceOgmios

instance HasPrivacyAnnotation TraceOgmios
instance HasSeverityAnnotation TraceOgmios where
    getSeverityAnnotation = \case
        OgmiosClient msg    -> getSeverityAnnotation msg
        OgmiosLookupEnv msg -> getSeverityAnnotation msg
        OgmiosHealth msg    -> getSeverityAnnotation msg
        OgmiosStarted{}            -> Info
        OgmiosConnectionAccepted{} -> Info
        OgmiosConnectionEnded{}    -> Info
        OgmiosSocketNotFound{}     -> Warning
        OgmiosFailedToConnect{}    -> Error
        OgmiosUnknownException{}   -> Error
