# contra-tracers

## Overview 

An opinionated, simple and easy-to-use logging library leveraging contravariant functors to provide structured multi-component logger capability to real application. 

## Usage

```hs
-- Some HTTP log message represented as Haskell values.

data HttpLog
    = SomeHttpLog
    | SomeHttpWarning
    deriving stock (Generic)
    deriving anyclass (ToJSON)

nstance HasSeverityAnnotation HttpLog
   getSeverityAnnotation = \case
       SomeHttpLog -> Info
       SomeHttpWarning -> Warning

-- Some DB log message represented as Haskell values.

data DbLog = SomeDbLog
    deriving stock (Generic)
    deriving anyclass (ToJSON)

nstance HasSeverityAnnotation HttpLog
   getSeverityAnnotation = \case
       SomeHttpLog -> Info
       SomeHttpWarning -> Warning

-- Applications tracers, defined as a record of higher-kinded types. We
-- have in particular 'TracerHKD Concrete a ~ a', which effectively makes
-- the 'Tracers' record a plain record of tracers once instantiated.

data Tracers m (kind :: TracerDefinition) = Tracers
    { tracerHttp :: TracerHKD kind (Tracer m HttpLog)
    , tracerDb   :: TracerHKD kind (Tracer m DbLog)
    } deriving stock (Generic)

-- The default configuration, setting all tracers to 'Info' min severity.
-- In practice, you may want to obtain the configuration from a config
-- file or from command-line options.
emptyConfiguration :: Tracers m MinSeverities
emptyConfiguration = defaultTracers Info

main :: IO ()
main = do
    withStdoutTracer mempty emptyConfiguration $ \tracers -> do
        concurrently_
            (myHttpApplication (tracerHttp tracers))
            (myDbApplication (tracerDb  tracers))
```

<p align="center">
  <a href="../../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
