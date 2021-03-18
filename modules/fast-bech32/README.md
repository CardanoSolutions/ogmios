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

<p align="center">
  <a href="../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
