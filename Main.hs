{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding
    ( toInteger )

import Data.Kind
    ( Constraint )
import Data.Proxy
    ( Proxy (..) )
import Data.Type.Bool
    ( If )
import GHC.TypeLits
    ( type (<=?), ErrorMessage (..), KnownNat, Mod, Nat, TypeError, natVal )

main :: IO ()
main = do
    let a = Even (Proxy @2)
    let b = Even (Proxy @3)
    print (a, b)

type family IsEven (n :: Nat) :: Constraint where
    IsEven n =
        If (Mod n 2 <=? 0)
        (() :: Constraint)
        (TypeError (ShowType n :<>: Text " must be an even number."))

data Even where
    Even :: forall n. (IsEven n, KnownNat n) => Proxy n -> Even

toInteger :: Even -> Integer
toInteger (Even proxy) = natVal proxy

instance Show Even where
    show = show . toInteger

