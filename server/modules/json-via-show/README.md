# json-via-show

## Overview 

Automatically derive `ToJSON` instances from types that have `Show` and `Generic` instances.

## Usage

```hs
{-# LANGUAGE DerivingVia #-}

import Data.Aeson
    ( ToJSON )
import Data.Aeson.Via.Show
    ( GenericToJsonViaShow(..), ViaJson (..) )

data Foo = Foo
  { foo :: String
  , bar :: ViaJson bar
  } deriving stock (Generic)
    deriving ToJSON via GenericToJsonViaShow Foo

data Bar = Bar Int Bool String
  deriving stock (Generic)
  deriving ToJSON via GenericToJsonViaShow Bar
```


<hr/>

<p align="center">
  <a href="../../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
