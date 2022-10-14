--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is used to define the 'keepRedundantContraint' helper here where it is
-- safe to define, and use it in other Json modules where we do not want to turn
-- -fno-warn-redundant-constraints for the entire module, but still want some
-- redundant constraints in order to enforce some restriction at the type-level
-- to not shoot ourselves in the foot by accident.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Ogmios.Prelude
    ( -- * relude, minus STM
      module Relude

      -- * generic-lens commons
    , HasType
    , view
    , typed
    , (^?)
    , (^.)

      -- * Array
    , Array
    , mapToArray

      -- * type-level helpers
    , keepRedundantConstraint
    , LastElem
    , Elem
    , Or
    , HKD
    , (:\:)
    ) where

import Data.Array
    ( Array
    , array
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.Generics.Product.Typed
    ( HasType
    , typed
    )
import qualified Data.Map as Map
import Data.Profunctor.Unsafe
    ( (#.)
    )
import GHC.Ix
    ( Ix
    )
import GHC.TypeLits
    ( ErrorMessage (..)
    , TypeError
    )
import Relude hiding
    ( MVar
    , Nat
    , Option
    , STM
    , TMVar
    , TVar
    , atomically
    , catchSTM
    , isEmptyTMVar
    , mkWeakTMVar
    , modifyTVar'
    , newEmptyMVar
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newMVar
    , newTMVar
    , newTMVarIO
    , newTVar
    , newTVarIO
    , putMVar
    , putTMVar
    , readMVar
    , readTMVar
    , readTVar
    , readTVarIO
    , swapMVar
    , swapTMVar
    , takeMVar
    , takeTMVar
    , throwSTM
    , traceM
    , tryPutMVar
    , tryPutTMVar
    , tryReadMVar
    , tryReadTMVar
    , tryTakeMVar
    , tryTakeTMVar
    , writeTVar
    )

-- | Copied from: https://hackage.haskell.org/package/generic-lens-1.1.0.0/docs/src/Data.Generics.Internal.VL.Prism.html
infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)

keepRedundantConstraint :: c => Proxy c -> ()
keepRedundantConstraint _ = ()

-- | Access the last element of a type-level list.
type family LastElem xs where
    LastElem ('[])     = TypeError ('Text "LastElem: empty list.")
    LastElem (x : '[]) = x
    LastElem (x : xs)  = LastElem xs

type family Elem e es where
    Elem e ('[]) = TypeError ('Text "Elem: not found.")
    Elem e (x : es) = Or (e ~ x) (Elem e es)

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    Or () b = ()
    Or (x ~ x) b = Or () b
    Or a () = ()
    Or a (x ~ x) = Or a ()

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

infixr 5 :\:
type family (:\:) (any :: k) (excluded :: k) :: Constraint where
    excluded :\: excluded =
        TypeError ( 'Text "Usage of this function forbids the type '" :<>: 'ShowType excluded :<>: 'Text "'." )
    _ :\: _ = ()

mapToArray :: Ix k => Map k v -> Array k v
mapToArray m =
  array
    (fst (Map.findMin m), fst (Map.findMax m))
    (Map.toList m)
