# hspec-json-schema

## Overview 

A test library built on top of [hjsonschema](https://hackage.haskell.org/package/hjsonschema) and [hspec](https://hackage.haskell.org/package/hspec) with a particular attention to error messages. The main use-case would be for testing that many `ToJSON` instances of a given Haskell data-types match a specific JSON-schema (Draft 4).

## Usage

The library expects the JSON-schema to be provided as a JSON file on the file-system. It is possible to reference a particular definition from within the file by using fragments. 

`validateToJSON` will generate arbitrary values, serialize them to JSON and verify that they
match the given schema. In case of error, it'll show a counter-example in JSON, the failing schema, as well as details about the nature of the error(s).

Have a look at the `test` folder to see examples of outputs and examples of JSON schemas.

```hs
{-# LANGUAGE TypeApplications #-}

module MyModule
    ( spec
    ) where

import Test.Hspec
    ( Spec, SpecWith, describe )
import Test.Hspec.Json.Schema
    ( validateToJSON )

spec :: Spec
spec = do
    describe "MyDataType" $ 
      validateToJSON (arbitrary @MyDataType) "schema.json#/definitions/MyDataType"

instance Arbitrary MyDataType
    arbitrary = {- ... -}
```

<hr/>

<p align="center">
  <a href="../../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
