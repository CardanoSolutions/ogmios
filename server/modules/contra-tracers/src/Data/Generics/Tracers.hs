--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_HADDOCK prune #-}

-- | This module provides a way to generically define and configure multiple
-- tracers for multiple logging components.
--
-- This is useful in order to associate separate configuration (e.g. minimum
-- severity) to tracers corresponding to different parts of an application. For
-- example, in a webserver, one may imagine wanting to lower the verbosity of a
-- tracer used for all HTTP requests, but keep the database tracer at a debug
-- level.
--
-- While this is possible to do without generics, it usually requires a bit of
-- boilerplate and repetitions. This module makes it easy to define one-single
-- data object as a record, for representing both severities configurations and
-- concrete instantiated tracers. Type signatures are a bit frightening, but the
-- usage is pretty simple (see 'Control.Tracer' for examples)
--
module Data.Generics.Tracers
    ( -- * Definition
      IsRecordOfTracers
    , TracerDefinition (..)
    , type TracerHKD

    -- * Construction
    , configureTracers
    , defaultTracers

    -- Internal
    , SomeMsg (..)
    ) where

import Prelude

import GHC.Generics

import Control.Tracer
    ( Tracer (..), nullTracer )
import Data.Aeson
    ( ToJSON (..) )
import Data.Functor.Const
    ( Const (..) )
import Data.Functor.Contravariant
    ( contramap )
import Data.Kind
    ( Type )
import Data.Severity
    ( HasSeverityAnnotation (..), Severity )

-- * Definition

-- | A constraint-alias defining generic record of tracers.
type IsRecordOfTracers tracers m =
    ( GConfigureTracers m (Rep (tracers m MinSeverities)) (Rep (tracers m Concrete))
    , Generic (tracers m MinSeverities)
    , Generic (tracers m Concrete)
    )

-- | A data kind to control what's defined in a generic record of tracer.
--
-- Use 'configureTracers' to turn a record of configuration into a record of
-- concrete tracers.
data TracerDefinition = Concrete | MinSeverities

-- | A high-kinded type family to make definitions of tracers more readable. For
-- example, one can define a multi-component set of tracers as such:
--
--     data Tracers m (kind :: TracerDefinition) = Tracers
--         { tracerHttp  :: TracerHKD kind (Tracer m HttpLog)
--         , tracerStart :: TracerHKD kind (Tracer m StartLog)
--         }
type family TracerHKD (definition :: TracerDefinition) (tracer :: Type) :: Type where
    TracerHKD MinSeverities tracer = Const (Maybe Severity) tracer
    TracerHKD Concrete tracer = tracer

-- * Construction

-- | Convert a 'MinSeverities' of tracers into a 'Concrete' record of tracers,
-- using a base opaque 'Tracer'.
configureTracers
    :: ( IsRecordOfTracers tracers m )
    => tracers m MinSeverities
    -> Tracer m SomeMsg
    -> tracers m Concrete
configureTracers f tr =
    to . gConfigureTracers mempty tr . from $ f

class GConfigureTracers m (f :: Type -> Type) (g :: Type -> Type) where
    gConfigureTracers
        :: String
        -> Tracer m SomeMsg
        -> f (Const (Maybe Severity) tr)
        -> g tr

instance GConfigureTracers m f g => GConfigureTracers m (D1 c f) (D1 c g) where
    gConfigureTracers field tr = M1 . gConfigureTracers field tr . unM1

instance GConfigureTracers m f g => GConfigureTracers m (C1 c f) (C1 c g) where
    gConfigureTracers field tr = M1 . gConfigureTracers field tr . unM1

instance (Selector s, GConfigureTracers m f g) => GConfigureTracers m (S1 s f) (S1 s g) where
    gConfigureTracers _ tr = M1 . gConfigureTracers field tr . unM1
      where
        field = selName (undefined :: S1 s f a)

instance (GConfigureTracers m f0 g0, GConfigureTracers m f1 g1)
    => GConfigureTracers m (f0 :*: f1) (g0 :*: g1)
  where
    gConfigureTracers field tr (f0 :*: f1) =
        gConfigureTracers field tr f0
        :*:
        gConfigureTracers field tr f1

instance (ToJSON msg, HasSeverityAnnotation msg, Applicative m)
    => GConfigureTracers m (K1 i (Const (Maybe Severity) (Tracer m msg))) (K1 i (Tracer m msg))
  where
    gConfigureTracers field tr = \case
        K1 (Const (Just minSeverity)) ->
            K1 (contramap (SomeMsg minSeverity field) tr)
        K1 (Const Nothing) ->
            K1 nullTracer

-- | Assign a default value to a record of tracers.
--
-- This is useful when used in conjunction with 'Const' and a record of tracer
-- severities:
--
--     defaultTracers Info == Tracers
--         { tracerHttp = Const (Just Info)
--         , tracerDb = Const (Just Info)
--         }
--
defaultTracers
    :: forall tracers (m :: Type -> Type).
        ( Generic (tracers m MinSeverities)
        , GDefaultRecord (Rep (tracers m MinSeverities))
        , GDefaultRecordType (Rep (tracers m MinSeverities)) ~ Maybe Severity
        )
    => Maybe Severity
    -> tracers m MinSeverities
defaultTracers =
    to . gDefaultRecord

-- | This generic class allows for defining a default value to a record of
-- functors wrapping some type 'a'.
class GDefaultRecord (f :: Type -> Type) where
    type GDefaultRecordType f :: Type
    gDefaultRecord :: GDefaultRecordType f -> f (Const (GDefaultRecordType f) b)

instance GDefaultRecord f => GDefaultRecord (D1 c f) where
    type GDefaultRecordType (D1 c f) = GDefaultRecordType f
    gDefaultRecord a = M1 (gDefaultRecord a)

instance GDefaultRecord f => GDefaultRecord (C1 c f) where
    type GDefaultRecordType (C1 c f) = GDefaultRecordType f
    gDefaultRecord a = M1 (gDefaultRecord a)

instance GDefaultRecord f => GDefaultRecord (S1 c f) where
    type GDefaultRecordType (S1 c f) = GDefaultRecordType f
    gDefaultRecord a = M1 (gDefaultRecord a)

instance (GDefaultRecord f, GDefaultRecord g, GDefaultRecordType f ~ GDefaultRecordType g) => GDefaultRecord (f :*: g) where
    type GDefaultRecordType (f :*: g) = GDefaultRecordType f
    gDefaultRecord a = gDefaultRecord a :*: gDefaultRecord a

instance GDefaultRecord (K1 i (Const a b)) where
    type GDefaultRecordType (K1 i (Const a b)) = a
    gDefaultRecord a = K1 (Const a)

-- Internal

-- A GADT capturing constraints as existential.
data SomeMsg where
    SomeMsg
        :: forall msg. (ToJSON msg, HasSeverityAnnotation msg)
        => Severity
        -> String
        -> msg
        -> SomeMsg
