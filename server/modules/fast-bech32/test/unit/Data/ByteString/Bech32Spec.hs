-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.ByteString.Bech32Spec
    ( spec
    ) where

import Prelude

import Data.ByteString
    ( ByteString )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.ByteString.Bech32
    ( HumanReadablePart (..), encodeBech32 )
import GHC.Stack
    ( HasCallStack )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Gen, Property, choose, elements, forAll, vector, withMaxSuccess, (===) )

import qualified Codec.Binary.Bech32 as Oracle
import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Spec
spec = describe "encodeBech32" $
    prop "oracle with existing bech32's library" $
        withMaxSuccess 5000 $
            forAll genBytes $ \bytes ->
                forAll genHrp $ prop_matchOracle bytes

prop_matchOracle :: Bytes -> HumanReadablePart -> Property
prop_matchOracle (Bytes bytes) (HumanReadablePart hrp) = do
    let hrp' = unsafeFromRight $ Oracle.humanReadablePartFromText hrp
    let oracle = Oracle.encodeLenient hrp' $ Oracle.dataPartFromBytes bytes
    let ours = encodeBech32 (HumanReadablePart hrp) bytes
    oracle === ours

--
-- Generators
--

newtype Bytes = Bytes { unBytes :: ByteString }

instance Show Bytes where
    show = T.unpack . encodeBase16 . unBytes

genBytes :: Gen Bytes
genBytes =
    Bytes . BS.pack <$> (choose (1, 100) >>= vector)

genHrp :: Gen HumanReadablePart
genHrp = elements
    [ HumanReadablePart "test_"
    , HumanReadablePart "addr"
    , HumanReadablePart "?!?"
    ]

--
-- Helpers
--

unsafeFromRight :: (HasCallStack, Show e) => Either e a -> a
unsafeFromRight = either (error . show) id
