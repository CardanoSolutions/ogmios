--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is used to define the 'keepRedundantContraint' helper here where it is
-- safe to define, and use it in other Json modules where we do not want to turn
-- -fno-warn-redundant-constraints for the entire module, but still want some
-- redundant constraints in order to enforce some restriction at the type-level
-- to not shoot ourselves in the foot by accident.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ogmios.Prelude
    ( -- * relude, minus STM
      module Relude

      -- * generic-lens commons
    , HasType
    , view
    , typed
    , (^?)
    , (^.)

      -- * type-level helpers
    , keepRedundantConstraint
    , LastElem
    , Or
    ) where

import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Profunctor.Unsafe
    ( ( #. ) )
import GHC.TypeLits
    ( ErrorMessage (..), TypeError )
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

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    Or () b = ()
    Or (x ~ x) b = Or () b
    Or a () = ()
    Or a (x ~ x) = Or a ()
