--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadDisk
    ( -- * Class
      MonadDisk (..)
    ) where

import Ogmios.Prelude

import Control.Monad.IOSim
    ( IOSim
    )

class Monad m => MonadDisk (m :: Type -> Type) where
    writeLByteString :: FilePath -> LByteString -> m ()
    -- ^ Write the given ByteString to disk.

instance MonadDisk IO where
    writeLByteString = writeFileLBS

instance MonadDisk (IOSim s) where
    writeLByteString _ _ = pure ()

instance MonadDisk m => MonadDisk (ReaderT env m) where
    writeLByteString a0 = lift . writeLByteString a0
