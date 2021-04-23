--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Via.Show
    ( GenericToJsonViaShow (..)
    , ViaJson (..)
    ) where

import Prelude

import GHC.Generics

import Data.Aeson
    ( ToJSON (..), Value (..), (.=) )
import Data.Kind
    ( Type )
import Data.Text
    ( Text )

import qualified Data.Aeson as Json
import qualified Data.Text as T

-- | Newtype wrapping any existing type which carries showable terminal values.
--
-- This is meant to be used in conjunction with a deriving via clause, as such:
--
--     data Foo = Foo
--         { bar :: Int
--         }
--         deriving stock (Generic)
--         deriving ToJSON via GenericToJsonViaShow Foo
--
newtype GenericToJsonViaShow a =
    GenericToJsonViaShow { unGenericToJsonViaShow :: a }
    deriving (Generic)

-- | A wrapper to force a particular type to be encoded using its Json instance.
data ViaJson a = ToJSON a => ViaJson a

instance Show a => Show (ViaJson a) where
    show (ViaJson a) = show a

class GToJsonViaShow (f :: Type -> Type) where
    gToJsonViaShow :: f a -> Value

instance
    ( Generic a
    , GToJsonViaShow (Rep a)
    ) => ToJSON (GenericToJsonViaShow a)
  where
    toJSON = gToJsonViaShow . from . unGenericToJsonViaShow

instance GToJsonViaShow f => GToJsonViaShow (D1 c f) where
    gToJsonViaShow = gToJsonViaShow . unM1

instance {-# OVERLAPS #-} (Constructor c) => GToJsonViaShow (C1 c U1) where
    gToJsonViaShow (M1 _) = toJSON fieldName
      where
        fieldName = T.pack $ conName (undefined :: C1 c U1 a)

instance (Constructor c, GToJsonViaShow f) => GToJsonViaShow (C1 c f) where
    gToJsonViaShow (M1 f) = Json.object [ fieldName .= gToJsonViaShow f ]
      where
        fieldName = T.pack $ conName (undefined :: C1 c f a)

instance (GToJsonViaShow f, GToJsonViaShow g) => GToJsonViaShow (f :+: g) where
    gToJsonViaShow = \case
        L1 f -> gToJsonViaShow f
        R1 g -> gToJsonViaShow g

instance (GToJsonViaShow f, GToJsonViaShow g) => GToJsonViaShow (f :*: g) where
    gToJsonViaShow (f :*: g) =
        case (gToJsonViaShow f, gToJsonViaShow g) of
            (Object ff, Object gg) ->
                Object (ff <> gg)
            (ff, gg) ->
                toJSON [ff, gg]

instance (Selector s, GToJsonViaShow f) => GToJsonViaShow (S1 s f) where
    gToJsonViaShow (M1 f) =
        if null fieldName
        then gToJsonViaShow f
        else Json.object [ T.pack fieldName .= gToJsonViaShow f ]
      where
        fieldName = selName (undefined :: S1 s f a)

instance (Show c) => GToJsonViaShow (K1 i c) where
    gToJsonViaShow = toJSON . show . unK1

instance {-# OVERLAPS #-} GToJsonViaShow (K1 i String) where
    gToJsonViaShow = toJSON . unK1

instance {-# OVERLAPS #-} GToJsonViaShow (K1 i Text) where
    gToJsonViaShow = toJSON . unK1

instance {-# OVERLAPS #-} GToJsonViaShow (K1 i (ViaJson a)) where
    gToJsonViaShow (K1 (ViaJson a)) = toJSON a
