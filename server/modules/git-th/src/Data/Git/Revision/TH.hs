--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2020 KtorZ <matthias.benkort@gmail.com>
-- License: MPL-2.0
-- Stability: Stable
-- Portability: Unix
module Data.Git.Revision.TH
    ( gitDescribeHEAD
    , gitRevParseHEAD
    , gitTags
    , gitRemoteGetURL

      -- Magic constants
    , unknownRevision
    , unknownRemote

    -- For Internal Use
    , git
    , git_
    ) where

import Prelude

import Control.Arrow
    ( second )
import Control.Exception
    ( SomeException, try )
import Control.Monad
    ( void )
import Data.List
    ( dropWhileEnd )
import Language.Haskell.TH
    ( Exp (..), Lit (..), Q, runIO )
import System.Exit
    ( ExitCode (..) )
import System.Process
    ( readProcessWithExitCode )

-- | Give a human readable name based of the current HEAD revision
--
-- The command finds the most recent tag that is reachable from a commit. If the
-- tag points to the commit, then only the tag is shown. Otherwise, it suffixes
-- the tag name with the number of additional commits on top of the tagged object
-- and the abbreviated object name of the most recent commit.
--
-- @since 1.1.0
--
-- >>> $(gitDescribeHEAD)
-- "v3.2.0-96-gb5af7eb"
gitDescribeHEAD :: Q Exp
gitDescribeHEAD =
    LitE . StringL <$> runIO runGitDescribe
  where
    runGitDescribe :: IO String
    runGitDescribe = do
        result <- git ["describe", "HEAD"]
        case result of
            Right (ExitSuccess, revision) -> pure revision
            _                             -> pure unknownRevision


-- | Get the current HEAD revision (long format).
--
-- The resulting splice is a 'String'.
--
-- @since 1.0.0
--
-- >>> $(gitRevParseHEAD)
-- "8901897a8883285ceebae66aa806e8ecceb12a48"
gitRevParseHEAD :: Q Exp
gitRevParseHEAD =
    LitE . StringL <$> runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse = do
        result <- git ["rev-parse", "--verify", "HEAD"]
        case result of
            Right (ExitSuccess, revision) -> pure revision
            _                             -> pure unknownRevision

-- | Get a list of tags, ordered by descending date.
--
-- The resulting splice is a @[(String, String)]@ where for each tuple:
--
-- - The first element is a tag name
-- - The second element is the revision for that tag
--
-- @since 1.0.0
--
-- >>> $(gitTags)
-- [("v1.0.0-beta","6b81b9b961068b4919875a8031677f8b2b091b61")
-- ,("2.0.0","b906a8e04f6a58a6079a4f0c75f80cf701a17f94")
-- ]
gitTags :: Q Exp
gitTags =
    ListE . fmap (\(a,b) -> TupE [Just $ LitE $ StringL a, Just $ LitE $ StringL b]) <$> runIO runGitTag
  where
    runGitTag :: IO [(String, String)]
    runGitTag = do
        result <- git ["tag", "-l", "--sort", "-taggerdate", "--format", "%(refname:short) %(object)"]
        case result of
            Right (ExitSuccess, tags) -> pure (mkTags <$> lines tags)
            _                         -> pure []

    mkTags :: String -> (String, String)
    mkTags str =
        (takeWhile (/= separator) str, tail $ dropWhile (/= separator) str)
      where
        separator :: Char
        separator = ' '

-- | Retrieve the repository's upstream remote url 'origin'.
--
-- Resulting splice is a 'String' literal.
--
-- @since 1.0.0
--
-- >>> $(gitRemoteGetURL)
--"git@github.com:owner/repository.git"
gitRemoteGetURL :: Q Exp
gitRemoteGetURL =
    LitE . StringL <$> runIO runGitRemoteGetURL
  where
    runGitRemoteGetURL :: IO String
    runGitRemoteGetURL = do
        result <- git ["remote", "get-url", "origin"]
        case result of
            Right (ExitSuccess, url) -> pure url
            _                        -> pure unknownRemote

--
-- Internals
--

-- Run an arbitrary git command.
git :: [String] -> IO (Either SomeException (ExitCode, String))
git args = do
    result <- try @SomeException $ readProcessWithExitCode "git" args ""
    pure $ second trimNewline . dropLast <$> result
  where
    dropLast :: (a,b,c) -> (a,b)
    dropLast (a,b,_) = (a,b)

    trimNewline :: String -> String
    trimNewline = dropWhileEnd (== '\n') . dropWhileEnd (== '\r')

-- Like 'git', but discard the result.
git_ :: [String] -> IO ()
git_ = void . git

-- Magic constant for when the revision isn't found.
unknownRevision :: String
unknownRevision = "unknown revision"

-- Magic constant used when the remote can't be found
unknownRemote :: String
unknownRemote = "unknown remote"
