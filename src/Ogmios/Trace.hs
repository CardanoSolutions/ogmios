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
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.Byron.Constants.Trace
    ( TraceLookup )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient )
import Data.ByteString
    ( ByteString )
import Ouroboros.Network.Block
    ( StandardHash )

data TraceOgmios where
    OgmiosClient
        :: forall block. (StandardHash block, Show block)
        => TraceClient block
        -> TraceOgmios

    OgmiosLookupEnv
        :: TraceLookup
        -> TraceOgmios

    OgmiosStarted
        :: { host :: String, port :: Int }
        -> TraceOgmios

    OgmiosSocketNotFound
        :: { path :: FilePath }
        -> TraceOgmios

    OgmiosConnectionAccepted
        :: { userAgent :: ByteString }
        -> TraceOgmios

    OgmiosConnectionEnded
        :: { userAgent :: ByteString }
        -> TraceOgmios

deriving instance Show TraceOgmios

instance DefinePrivacyAnnotation TraceOgmios
instance DefineSeverity TraceOgmios where
    defineSeverity = \case
        OgmiosClient msg -> defineSeverity msg
        OgmiosLookupEnv msg -> defineSeverity msg
        OgmiosStarted{} -> Info
        OgmiosSocketNotFound{} -> Warning
        OgmiosConnectionAccepted{} -> Info
        OgmiosConnectionEnded{} -> Info
