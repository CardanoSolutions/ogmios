--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ogmios.Version
    ( version
    ) where

import Ogmios.Prelude

import Data.Git.Revision.TH
    ( gitRevParseHEAD
    )
import Data.Version
    ( makeVersion
    , showVersion
    )

import qualified Paths_ogmios as Pkg

-- | Show the current application revision from git context embedded at
-- compile-time. Shows a tag if compiled on a revision pointing to a tag, or
-- show the revision and the last known tag otherwise.
--
-- This is impure in disguise but safe if 'git' is available in the app context.
-- If not available, we fallback to the version listed in the cabal file.
version :: Text
version
    | Pkg.version == makeVersion [0] =
        toText ("nightly (" <> sha <> ")")
    | otherwise =
        toText ("v" <> showVersion Pkg.version <> " (" <> sha <> ")")
  where
    sha = take 8 $(gitRevParseHEAD)
