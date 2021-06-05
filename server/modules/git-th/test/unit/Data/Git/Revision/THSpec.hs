--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Git.Revision.THSpec
    ( spec
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
import Data.Git.Revision.TH
    ( gitDescribeHEAD
    , gitRemoteGetURL
    , gitRevParseHEAD
    , gitTags
    , git_
    , unknownRemote
    , unknownRevision
    )
import Language.Haskell.TH
    ( Exp (..), Lit (..), runQ )
import System.Environment
    ( setEnv )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( ActionWith
    , Spec
    , around
    , context
    , shouldBe
    , shouldNotBe
    , shouldSatisfy
    , specify
    )

spec :: Spec
spec = around withGitSandbox $ do
    context "gitDescribeHEAD" $ do
        specify "returns 'unknown revision' if there are no past tags" $ \_ -> do
            git_ ["init"]
            git_ ["commit", "--allow-empty", "-m", "a"]
            git_ ["commit", "--allow-empty", "-m", "b"]
            rev <- asString =<< runQ gitDescribeHEAD
            rev `shouldBe` unknownRevision

        specify "returns latest known tag" $ \_ -> do
            git_ ["init"]
            git_ ["commit", "--allow-empty", "-m", "a"]
            git_ ["tag", "-m", "1st", "2.0.0"] *> threadDelay _1s
            revA <- asString =<< runQ gitDescribeHEAD
            git_ ["commit", "--allow-empty", "-m", "b"]
            revB <- asString =<< runQ gitDescribeHEAD
            take 5 revA `shouldBe` take 5 revB

    context "gitRevParseHEAD" $ do
        specify "interleaving givRevParseHEAD with commits yield different refs" $ \_ -> do
            git_ ["init"]
            git_ ["commit", "--allow-empty", "-m", "a"]
            revA <- asString =<< runQ gitRevParseHEAD
            git_ ["commit", "--allow-empty", "-m", "b"]
            revB <- asString =<< runQ gitRevParseHEAD
            revA `shouldNotBe` revB
            revA `shouldSatisfy` isSHA1
            revB `shouldSatisfy` isSHA1

        specify "default to 'unknown revision' when rev can't be parsed" $ \_ -> do
            setEnv "GIT_WORK_TREE" "/dev/null"
            rev <- asString =<< runQ gitRevParseHEAD
            rev `shouldBe` unknownRevision

    context "gitTags" $ do
        specify "tags are listed in descending date of creation" $ \_ -> do
            git_ ["init"]
            git_ ["commit", "--allow-empty", "-m", "a"]
            git_ ["tag", "-m", "1st", "2.0.0"] *> threadDelay _1s
            git_ ["commit", "--allow-empty", "-m", "b"]
            git_ ["tag", "-m", "2nd", "1.0.0"] *> threadDelay _1s
            git_ ["commit", "--allow-empty", "-m", "c"]
            git_ ["tag", "-m", "3rd", "3.0.0"]
            tags <- traverse (\(a,b) -> (,) <$> asString a <*> asString b)
                =<< traverse as2Tuple
                =<< asList
                =<< runQ gitTags
            (fst <$> tags) `shouldBe` ["3.0.0","1.0.0","2.0.0"]
            (snd <$> tags) `shouldSatisfy` all isSHA1

        specify "default to an empty list" $ \_ -> do
            setEnv "GIT_WORK_TREE" "/dev/null"
            tags <- asList =<< runQ gitTags
            tags `shouldBe` mempty

    context "gitRemoteGetURL" $ do
        specify "can get remote URL corresponding to 'origin'" $ \_ -> do
            let origin = "git@github.com:repo/owner.git"
            let someFork = "git@github.com:fork/someone.git"
            git_ ["init"]
            git_ ["remote", "add", "origin"  , origin]
            git_ ["remote", "add", "someFork", someFork]
            remote <- asString =<< runQ gitRemoteGetURL
            remote `shouldBe` origin

        specify "default to 'unknown remote'" $ \_ -> do
            setEnv "GIT_WORK_TREE" "/dev/null"
            remote <- asString =<< runQ gitRemoteGetURL
            remote `shouldBe` unknownRemote

--
-- Helpers
--

-- | Wrap specifications in a git sandbox, so that we can create interesting
-- test scenarios that are explicit in the test themselves.
withGitSandbox :: ActionWith FilePath -> IO ()
withGitSandbox actionWith =
    withSystemTempDirectory "git-th" $ \tmp -> do
        setEnv "GIT_WORK_TREE" tmp
        setEnv "GIT_DIR" (tmp <> "/.git")
        actionWith tmp

-- | Assert whether a string looks structurally like a base16-encoded SHA1 digest
isSHA1 :: String -> Bool
isSHA1 str =
    length str == 40 && all (`elem` (['0'..'9']++['a'..'f'])) str

-- | One second in micro seconds
_1s :: Int
_1s = 1_000_000

-- | Sort of prism to coerce a TH expression to a 'String'
asString :: MonadFail m => Exp -> m String
asString = \case
    LitE (StringL s) -> pure s
    e -> fail $ "cannot convert expression to a String: " <> show e

-- | Sort of prism to coerce a TH expression to a 'List'
asList :: MonadFail m => Exp -> m [Exp]
asList = \case
    ListE xs -> pure xs
    e -> fail $ "cannot convert expression to a List: " <> show e

-- | Sort of prism to coerce a TH expression to a 2-'Tuple'
as2Tuple :: MonadFail m => Exp -> m (Exp, Exp)
as2Tuple = \case
    TupE [Just a, Just b] -> pure (a,b)
    e -> fail $ "cannot convert expression to 2-Tuple: " <> show e
