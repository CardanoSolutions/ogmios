--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE LambdaCase #-}

module Cardano.Byron.Constants
    (
      NodeVersionData
    , EpochSlots
    , lookupVersionData

      -- * Mainnet
    , mainnetVersionData
    , mainnetEpochSlots

      -- * Testnet
    , testnetVersionData
    , testnetEpochSlots
    ) where

import Prelude

import Cardano.Byron.Constants.Trace
    ( TraceLookup (..) )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Text
    ( Text )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import Ouroboros.Network.Protocol.Handshake.Version
    ( CodecCBORTerm )
import System.Environment
    ( lookupEnv )
import System.Exit
    ( exitFailure )


-- Type alias to lighten signatures below
type NodeVersionData =
    (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)

-- | Lookup environment for a given version data name, default to mainnet.
lookupVersionData
    :: Tracer IO TraceLookup
        -- ^ A tracer for logging messages
    -> String
        -- ^ Variable name
    -> IO (NodeVersionData, EpochSlots)
lookupVersionData tr name = do
    lookupEnv name >>= \case
        Nothing -> do
            traceWith tr $ LookupDefaultNetwork mainnetNetwork
            pure (mainnetVersionData, mainnetEpochSlots)
        Just "mainnet" -> do
            traceWith tr $ LookupUserDefinedNetwork mainnetNetwork
            pure (mainnetVersionData, mainnetEpochSlots)
        Just "testnet" -> do
            traceWith tr $ LookupUserDefinedNetwork testnetNetwork
            pure (testnetVersionData, testnetEpochSlots)
        Just _invalid  -> do
            traceWith tr $ LookupInvalidNetwork ["mainnet", "testnet"]
            exitFailure
  where
    mainnetNetwork :: (String, NetworkMagic)
    mainnetNetwork = ("mainnet", networkMagic $ fst mainnetVersionData)

    testnetNetwork :: (String, NetworkMagic)
    testnetNetwork = ("testnet", networkMagic $ fst testnetVersionData)

--
-- Mainnet
--

-- Hard-coded mainnet version data
mainnetVersionData
    :: NodeVersionData
mainnetVersionData =
    ( NodeToClientVersionData
        { networkMagic = NetworkMagic 764824073
        }
    , nodeToClientCodecCBORTerm
    )

-- Hard-coded mainnet genesis slots per epoch
mainnetEpochSlots
    :: EpochSlots
mainnetEpochSlots =
    EpochSlots 21600

--
-- Testnet
--

-- Hard-coded testnet version data
testnetVersionData
    :: NodeVersionData
testnetVersionData =
    ( NodeToClientVersionData
        { networkMagic = NetworkMagic 1097911063
        }
    , nodeToClientCodecCBORTerm
    )

-- Hard-coded testnet genesis slots per epoch
testnetEpochSlots
    :: EpochSlots
testnetEpochSlots =
    EpochSlots 21600
