--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ogmios.Version
    ( version
    , shortVersion
    , revision
    ) where

import Ogmios.Prelude

import Data.Git.Revision.TH
    ( gitDescribeHEAD, gitRevParseHEAD, unknownRevision )
import Data.Version
    ( showVersion )

import qualified Data.Text as T
import qualified Paths_ogmios as Pkg

-- | Show the current application revision from git context embedded at
-- compile-time. Shows a tag if compiled on a revision pointing to a tag, or
-- show the revision and the last known tag otherwise.
--
-- This is impure in disguise but safe if 'git' is available in the app context.
-- If not available, we fallback to the version listed in the cabal file.
version :: Text
version =
    case $(gitDescribeHEAD) of
        rev | rev == unknownRevision ->
            toText ("v" <> showVersion Pkg.version <> "-??-????????")
        rev ->
            toText rev

-- | Version tag only.
shortVersion :: Text
shortVersion =
    T.takeWhile (/= '-') version

-- | Current application git revision.
revision :: Text
revision =
    case $(gitRevParseHEAD) of
        rev | rev == unknownRevision ->
            "????????"
        rev ->
            toText rev
