--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ogmios.App.Version
    ( version
    ) where

import Relude

import Data.Git.Revision.TH
    ( gitRevParseHEAD, gitTags, unknownRevision )

-- | Show the current application revision from git context embedded at
-- compile-time. Shows a tag if compiled on a revision pointing to a tag, or
-- show the revision and the last known tag otherwise.
--
-- This is impure in disguise but safe if 'git' is available in the app context.
version :: Text
version = do
    case find ((== revHEAD) . snd) tags of
        Just (tag, _) ->
            toText tag
        Nothing -> unwords $ toText <$>
            [ "unreleased (> " <> lastKnownTag tags <> ")"
            , "-"
            , "git revision " <> revHEADShort
            ]
  where
    tags :: [(String, String)]
    tags = $(gitTags)

    revHEAD :: String
    revHEAD = $(gitRevParseHEAD)

    revHEADShort :: String
    revHEADShort = case $(gitRevParseHEAD) of
        s | s == unknownRevision -> s
        s -> take 8 s

    lastKnownTag :: [(String, String)] -> String
    lastKnownTag = \case
        []    -> "unknown"
        (h:_) -> fst h
