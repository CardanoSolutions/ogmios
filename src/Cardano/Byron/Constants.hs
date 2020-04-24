--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Byron.Constants
    (
      NodeVersionData
    , EpochSlots
    , lookupVersionData

      -- * Mainnet
    , mainnetVersionData

      -- * Testnet
    , testnetVersionData

      -- * Staging
    , stagingVersionData
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
import Ouroboros.Network.CodecCBORTerm
    ( CodecCBORTerm )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..), nodeToClientCodecCBORTerm )
import Safe
    ( readMay )
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
            pure (mainnetVersionData, defaultEpochSlots)
        Just "mainnet" -> do
            traceWith tr $ LookupUserDefinedNetwork mainnetNetwork
            pure (mainnetVersionData, defaultEpochSlots)
        Just "testnet" -> do
            traceWith tr $ LookupUserDefinedNetwork testnetNetwork
            pure (testnetVersionData, defaultEpochSlots)
        Just "staging" -> do
            traceWith tr $ LookupUserDefinedNetwork stagingNetwork
            pure (stagingVersionData, defaultEpochSlots)
        Just custom ->
            case readMay custom of
                Just n -> do
                    let magic = NetworkMagic n
                    traceWith tr $ LookupDefaultNetwork ("custom", magic)
                    pure (customVersionData magic, defaultEpochSlots)
                Nothing -> do
                    traceWith tr $ LookupInvalidNetwork ["mainnet", "testnet", "staging"]
                    exitFailure
  where
    mainnetNetwork :: (String, NetworkMagic)
    mainnetNetwork = ("mainnet", networkMagic $ fst mainnetVersionData)

    testnetNetwork :: (String, NetworkMagic)
    testnetNetwork = ("testnet", networkMagic $ fst testnetVersionData)

    stagingNetwork :: (String, NetworkMagic)
    stagingNetwork = ("staging", networkMagic $ fst stagingVersionData)

--
-- Mainnet
--

-- Hard-coded mainnet version data
mainnetVersionData
    :: NodeVersionData
mainnetVersionData =
    customVersionData (NetworkMagic 764824073)

--
-- Testnet
--

-- Hard-coded testnet version data
testnetVersionData
    :: NodeVersionData
testnetVersionData =
    customVersionData (NetworkMagic 1097911063)

--
-- Staging
--

-- Hard-coded staging version data
stagingVersionData
    :: NodeVersionData
stagingVersionData =
    customVersionData (NetworkMagic 633343913)

--
-- Custom
--

-- A custom / unknown version data
customVersionData
    :: NetworkMagic
    -> NodeVersionData
customVersionData networkMagic =
    ( NodeToClientVersionData { networkMagic }
    , nodeToClientCodecCBORTerm
    )

--
-- Epoch-Slots
--

-- Hard-coded genesis slots per epoch
defaultEpochSlots
    :: EpochSlots
defaultEpochSlots =
    EpochSlots 21600
