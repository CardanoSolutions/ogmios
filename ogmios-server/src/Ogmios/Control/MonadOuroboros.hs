--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadOuroboros
    ( MonadOuroboros
    ) where

import qualified Control.Monad.Class.MonadAsync as Ouroboros
import qualified Control.Monad.Class.MonadST as Ouroboros
import qualified Control.Monad.Class.MonadThrow as Ouroboros

-- | A type alias to ease type-signatures with Ouroboros' internal effects.
--
-- These are required when connecting an Ouroboros application client (more
-- exactly, required for a ChainSync client, although TxSubmission and StateQuery
-- clients both require 'MonadThrow')
type MonadOuroboros m =
        ( Ouroboros.MonadThrow m
        , Ouroboros.MonadAsync m
        , Ouroboros.MonadST m
        )
