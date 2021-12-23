# fast-bech32

## Overview 

An optimized implementation of the [bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki) encoding format (checksumed base32 with human-readable prefixes). 

## Usage


```hs
import Data.ByteString.Bech32 
  ( HumanReadablePart(..), encodeBech32 )

encodeAddress :: ByteString -> Text
encodeAddress = encodeBech32 (HumanReadablePart "addr") 
```

<hr/>

## Benchmarks

| Bytestring length (bytes) | [bech32](https://hackage.haskell.org/package/bech32) | [fast-bech32](https://hackage.haskell.org/package/fast-bech32) |
| ---                       | ---                                                  | ---                                                            |
| 10                        | 8.085μs                                              | 0.875μs                                                        |
| 100                       | 60.83μs                                              | 2.181μs                                                        |
| 1000                      | 664.1μs                                              | 33.05μs                                                       |

<p align="center">
  <a href="https://github.com/CardanoSolutions/ogmios/blob/master/CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="https://github.com/CardanoSolutions/ogmios/blob/master/server/modules/fast-bech32/CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
