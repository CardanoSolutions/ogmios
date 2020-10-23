# time-extra

## Overview 

Provide a few extra helper with regards to time and time measurement in an application. In particular, the modules offer easy ways to time and debounce IO actions. 

## Usage

```hs
{-# LANGUAGE NamedFieldPuns #-}

import System.Time.Clock
    ( newDebouncer, Debouncer(..) )
import Control.Concurrent.MVar
    ( modifyMVar_, readMVar, newMVar )

main :: IO ()
main = do
    counter <- newMVar (0 :: Word)
    let action = modifyMVar_ counter (pure . (+1))
    Debouncer{debounce} <- newDebouncer (1 :: NominalDiffTime)
    replicateM_ 10 (debounce action)
    readMVar counter `shouldReturn` 1
```

<hr/>

<p align="center">
  <a href="https://ktorz.github.io/cardano-ogmios/modules/time-extra">:book: API Reference</a>
  |
  <a href="../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="../../CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center">
  <a href="https://github.com/KtorZ/cardano-ogmios/blob/master/modules/time-extra/LICENSE">
    <img src="https://img.shields.io/github/license/KtorZ/cardano-ogmios.svg?style=for-the-badge"/>
  </a>
</p>
