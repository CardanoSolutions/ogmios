module Data.Git.Revision.TH
    ( gitRevParseHEAD
    ) where

import Prelude

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
        (exitCode, revision, _) <-
            readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD"] ""
        case exitCode of
            ExitSuccess -> pure revision
            _           -> pure "Unknown"
