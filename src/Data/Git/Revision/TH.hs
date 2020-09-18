--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Data.Git.Revision.TH
    ( gitRevParseHEAD
    , gitRemoteGetURL
    ) where

import Prelude

import Control.Arrow
    ( second )
import Control.Exception
    ( SomeException, try )
import Language.Haskell.TH
    ( Exp (..), Lit (..), Q, runIO )
import System.Exit
    ( ExitCode (..) )
import System.Process
    ( readProcessWithExitCode )

gitRevParseHEAD :: Q Exp
gitRevParseHEAD =
    LitE . StringL <$> runIO runGitRevParse
  where
    runGitRevParse :: IO String
    runGitRevParse = do
        result <- git ["rev-parse", "--verify", "HEAD"]
        case result of
            Right (ExitSuccess, revision) -> pure revision
            _ -> pure "unknown revision"

gitRemoteGetURL :: Q Exp
gitRemoteGetURL =
    LitE . StringL <$> runIO runGitRemoteGetURL
  where
    runGitRemoteGetURL :: IO String
    runGitRemoteGetURL = do
        result <- git ["remote", "get-url", "origin"]
        case result of
            Right (ExitSuccess, url) -> pure url
            _ -> pure "unknown remote"

git :: [String] -> IO (Either SomeException (ExitCode, String))
git args = do
    result <- try @SomeException $ readProcessWithExitCode "git" args ""
    pure $ second trimNewline . dropLast <$> result
  where
    dropLast :: (a,b,c) -> (a,b)
    dropLast (a,b,_) = (a,b)

    trimNewline :: String -> String
    trimNewline = filter (`notElem` ['\n', '\r'])
