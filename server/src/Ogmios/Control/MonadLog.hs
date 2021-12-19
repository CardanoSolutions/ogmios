--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ogmios.Control.MonadLog
    ( -- * Class
      MonadLog (..)
    , Logger

      -- * Tracer
    , Tracer
    , natTracer
    , nullTracer
    , contramap
    , traceWith

      -- * Severity
    , Severity (..)
    , HasSeverityAnnotation (..)

      -- * Instantiation
    , mkTracers
    , defaultTracers
    , withStdoutTracer
    ) where

import Ogmios.Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasSeverityAnnotation (..) )
import Control.Monad.IOSim
    ( IOSim )
import Control.Tracer
    ( Tracer (..), natTracer, nullTracer, traceWith )
import Data.Aeson
    ( ToJSON (..) )
import Data.Aeson.Encoding
    ( pair, pairs )
import GHC.Conc
    ( myThreadId )
import Ogmios.Control.MonadClock
    ( getCurrentTime )
import Ogmios.Control.MonadSTM
    ( newTMVarIO, withTMVar )
import System.IO
    ( BufferMode (..), hSetBuffering, hSetEncoding, utf8 )

import GHC.Generics

import qualified Data.Aeson.Encoding as Json
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

class Monad m => MonadLog (m :: Type -> Type) where
    logWith :: Logger msg -> msg -> m ()

type Logger = Tracer IO
type AppVersion = Text

instance MonadLog IO where
    logWith = traceWith

-- TODO: This instance is bonkers, but this is because of the over-specialized
-- Logger = Tracer IO. Ideally, MonadLog should work with _any_
instance MonadLog (IOSim s) where
    logWith _ _ = pure ()

instance MonadLog m => MonadLog (ReaderT env m) where
    logWith tr = lift . logWith tr

-- | Acquire a tracer to use across the app lifecycle.
withStdoutTracer
    :: AppVersion
    -> (Tracer IO SomeMsg -> IO ())
    -> IO ()
withStdoutTracer version action = do
    hSetBuffering stdout LineBuffering
    hSetEncoding stdout utf8
    lock <- newTMVarIO ()
    action (tracer lock)
  where
    tracer lock = Tracer $ \(SomeMsg minSeverity msg) -> do
        let severity = getSeverityAnnotation msg
        when (severity >= minSeverity) $ liftIO $ withTMVar lock $ \() -> do
            mkEnvelop msg severity >>= liftIO . TIO.putStrLn . decodeUtf8 . Json.encodingToLazyByteString

    mkEnvelop :: forall m msg. (ToJSON msg, MonadIO m) => msg -> Severity -> m Json.Encoding
    mkEnvelop msg severity = do
        timestamp <- liftIO getCurrentTime
        threadId <- T.drop 9 . show <$> liftIO myThreadId
        pure $ pairs $ mempty
            <> pair "severity"  (toEncoding severity)
            <> pair "timestamp" (toEncoding timestamp)
            <> pair "thread"    (toEncoding threadId)
            <> pair "message"   (toEncoding msg)
            <> pair "version"   (toEncoding version)

--
-- Declaring Tracers
--

data SomeMsg where
    SomeMsg :: forall msg. (ToJSON msg, HasSeverityAnnotation msg) => Severity -> msg -> SomeMsg

-- | Generically instantiate tracers from a record type. The record is
-- parameterized by a functor which is intended to be one of:
--
-- - Const
-- - Identity
--
-- 'Const' is used to define a record of minimum severities, for configuring the
-- tracers independently. This function then instantiates each field of the
-- record to an actual tracer with a configured minimum severity.
--
--     data Tracers m (f :: Type -> Type) = Tracers
--         { tracerFoo
--             :: HKD f (Tracer m Foo)
--         , tracerBar
--             :: HKD f (Tracer m Bar)
--         } deriving (Generic)
--
--     mkTracersSpecialized
--       :: Tracers m (Const Severity)
--       -> Tracer m SomeMsg
--       -> Tracers m Identity
--
mkTracers
    :: ( Generic (f m Identity)
       , Generic (f m (Const Severity))
       , GMkTracers m (Rep (f m (Const Severity))) (Rep (f m Identity))
       )
    => f m (Const Severity)
    -> Tracer m SomeMsg
    -> f m Identity
mkTracers f tr =
    to . gMkTracers tr . from $ f

defaultTracers
    :: ( Generic (f (Const Severity))
       , GDefaultTracers (Rep (f (Const Severity)))
       )
    => Severity
    -> f (Const Severity)
defaultTracers =
    to . gDefaultTracers


class GMkTracers m (f :: Type -> Type) (g :: Type -> Type) where
    gMkTracers
        :: Tracer m SomeMsg
        -> f (Const Severity tr)
        -> g tr

instance GMkTracers m f g => GMkTracers m (D1 c f) (D1 c g) where
    gMkTracers tr =
        M1 . gMkTracers tr . unM1

instance GMkTracers m f g => GMkTracers m (C1 c f) (C1 c g) where
    gMkTracers tr =
        M1 . gMkTracers tr . unM1

instance GMkTracers m f g => GMkTracers m (S1 c f) (S1 c g) where
    gMkTracers tr =
        M1 . gMkTracers tr . unM1

instance (GMkTracers m f0 g0, GMkTracers m f1 g1) => GMkTracers m (f0 :*: f1) (g0 :*: g1) where
    gMkTracers tr (f0 :*: f1) =
        gMkTracers tr f0 :*: gMkTracers tr f1

instance (ToJSON msg, HasSeverityAnnotation msg)
    => GMkTracers m (K1 i (Const Severity (Tracer m msg))) (K1 i (Tracer m msg))
  where
    gMkTracers tr (K1 msg) =
        K1 (contramap (SomeMsg (getConst msg)) tr :: Tracer m msg)


class GDefaultTracers (f :: Type -> Type) where
    gDefaultTracers
        :: Severity
        -> f (Const Severity b)

instance GDefaultTracers f => GDefaultTracers (D1 c f) where
    gDefaultTracers minSeverity =
        M1 (gDefaultTracers minSeverity)

instance GDefaultTracers f => GDefaultTracers (C1 c f) where
    gDefaultTracers minSeverity =
        M1 (gDefaultTracers minSeverity)

instance GDefaultTracers f => GDefaultTracers (S1 c f) where
    gDefaultTracers minSeverity =
        M1 (gDefaultTracers minSeverity)

instance (GDefaultTracers f, GDefaultTracers g) => GDefaultTracers (f :*: g) where
    gDefaultTracers minSeverity =
        gDefaultTracers minSeverity :*: gDefaultTracers minSeverity

instance GDefaultTracers (K1 i (Const Severity b))  where
    gDefaultTracers minSeverity =
        K1 (Const minSeverity)
