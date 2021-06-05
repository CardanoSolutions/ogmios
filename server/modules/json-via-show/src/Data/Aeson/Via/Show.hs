--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Data.Aeson.Via.Show
    ( ToJSONViaShow (..)
    ) where

import Prelude

import Data.Aeson
    ( ToJSON (..) )
import Data.Aeson.Via.Show.Internal
    ( exprToJSON, parseExpr )

-- | DerivingVia wrapper to auto-magically derive JSON instances for data-types
-- that have a 'Show' instance.
--
-- For example:
--
-- @
-- {-# LANGUAGE DerivingVia #-}
--
-- data Foo = Foo
--     { foo :: [Int]
--     , bar :: String
--     }
--     deriving stock Show
--     deriving ToJSON via ToJSONViaShow Foo
--
-- data Log = Log Bool LastUpdate
--     deriving stock Show
--     deriving ToJSON via ToJSONViaShow Log
--
-- newtype LastUpdate = LastUpdate
--     { unLastUpdate :: UTCTime
--     }
--     deriving stock Show
--     deriving ToJSON via ToJSONViaShow LastUpdate
-- @
--
-- >>> encode (Foo [42] "str")
-- {"Foo":{"foo":["42"],"bar":"str"}}
--
-- >>> encode (Log True (LastUpdate now))
-- {"Log":[true,{"LastUpdate":"2021-06-05 17:17:54.710264188 UTC"}]}
newtype ToJSONViaShow a = ToJSONViaShow { unToJSONViaShow :: a }

instance Show a => ToJSON (ToJSONViaShow a) where
    toJSON = exprToJSON . fst . parseExpr False . show . unToJSONViaShow
