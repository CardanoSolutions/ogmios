# json-via-show

## Overview 

Automatically derive `ToJSON` instances from types that have `Show` instances. This project was very much an _interesting_ experiment which turned out to be even more hacky than anticipated. As a matter of fact:

- Without any prior knowledge of the target data-type, it may actually not be possible to parse
  a rendered data-type correctly. This is the case for instance of the show instance for `UTCTime`

- Unfortunately, many types in the wild aren't using stock show instances and don't necessarily follow strict rules when it comes to the grammar. For example, while every constructor in Haskell starts with a capital letter, the "shown" constructor for many `containers` like `Set` or `Map` are displayed as `fromList []`. 

- It feels like _a lot_ is going on in order to re-parse each type and re-transform them back into JSON and, all of it is happening at runtime, leaving room for mistakes and possible failures. Since the primary purpose of this module was to get an easy way to quickly write JSON instances for logging; relying on something that may break at any input does not seem like a good idea. 

- On the plus side, there has been some good results when plugged in a real system, in particular with nested types coming from a sub-system for which the JSON structure was auto-magically discovered:


```
[ogmios:Info:5] [2021-06-06 13:44:58.80 UTC] {"OgmiosNetwork":{"NetworkParameters":{"networkMagic":"1097911063","systemStart":["2019-07-24","20:20:16",{"UTC":[]}],"slotsPerEpoch":"21600"}}}

[ogmios:Info:14] [2021-06-06 13:44:58.80 UTC] {"OgmiosServer":{"ServerStarted":{"nodeSocket":"/home/ktorz/Documents/IOHK/networks/testnet/node.socket","dashboardUrl":"http://127.0.0.1:1338/"}}}

[ogmios:Info:22] [2021-06-06 13:44:58.81 UTC] {"OgmiosHealth":{"HealthTick":{"Health":{"metrics":{"totalUnrouted":"0","totalMessages":"0","runtimeStats":{"gcCpuTime":"0","cpuTime":"5970744","maxHeapSize":"0","currentHeapSize":"0"},"totalConnections":"0","sessionDurations":{"max":"0.0","mean":"0.0","min":"0.0"},"activeConnections":"0"},"startTime":"2021-06-06 13:44:58.807426705 UTC","lastTipUpdate":["2021-06-06","13:44:58.810905762",{"UTC":[]}],"lastKnownTip":[{"SlotNo":"28617866"},"4cf6200d4f37f106c173612cb1473d41b93788d000f5450d2996b1e4a4b18315",{"BlockNo":"2648278"}],"networkSynchronization":"0.99999","currentEra":"Mary"}}}}

[ogmios:Info:533] [2021-06-06 13:45:39.80 UTC] {"OgmiosWebSocket":{"WebSocketConnectionEnded":"User-Agent unknown"}}
[ogmios:Info:548] [2021-06-06 13:45:40.31 UTC] {"OgmiosWebSocket":{"WebSocketConnectionAccepted":{"mode":"FullSerialization","userAgent":"User-Agent unknown"}}}

[ogmios:Info:541] [2021-06-06 13:45:39.80 UTC]{"OgmiosWebSocket":{"WebSocketClient":{"TrTxSubmission":{"Send":[{"ClientAgency":{"TokIdle":[]}},{"MsgSubmitTx":{"HardForkGenTx":{"S":{"S":{"S":{"Z":{"TxRaw":{"_wits":{"txWitsBytes":"\\161\\NUL\\129\\130X \\207\\DC4\\209\\200\\&4\\206\\202\\184\\225\\245D{\\222U\\EMF\\128@W3(%\\226nd\\238C\\a\\157\\212\\b5X@$|^`\\146\\DC10\\250\\GS\\248\\NUL\\211\\DLE\\243\\151\\136\\244\\174\\EOT\\131u4\\173\\230rxu\\219\\184r\\CAN\\245\\180^\\150\\204\\209%\\161LE\\DLE\\232\\SYN\\148\\231\\170\\211\\186\\138$E\\138\\175ko\\156O\\SUBH\\SOH\\190\\186\\ENQ","addrWits'":[{"WitVKey'":{"wvkKey'":{"VerKeyEd25519DSIGN":{"PublicKey":"\\207\\DC4\\209\\200\\&4\\206\\202\\184\\225\\245D{\\222U\\EMF\\128@W3(%\\226nd\\238C\\a\\157\\212\\b5"}},"wvkSig'":{"SigEd25519DSIGN":{"Signature":"$|^`\\146\\DC10\\250\\GS\\248\\NUL\\211\\DLE\\243\\151\\136\\244\\174\\EOT\\131u4\\173\\230rxu\\219\\184r\\CAN\\245\\180^\\150\\204\\209%\\161LE\\DLE\\232\\SYN\\148\\231\\170\\211\\186\\138$E\\138\\175ko\\156O\\SUBH\\SOH\\190\\186\\ENQ"}},"wvkKeyHash":"ff7b4521589238cfb9c26870edfa782541e61544474422d849ceb103","wvkBytes":"\\130X \\207\\DC4\\209\\200\\&4\\206\\202\\184\\225\\245D{\\222U\\EMF\\128@W3(%\\226nd\\238C\\a\\157\\212\\b5X@$|^`\\146\\DC10\\250\\GS\\248\\NUL\\211\\DLE\\243\\151\\136\\244\\174\\EOT\\131u4\\173\\230rxu\\219\\184r\\CAN\\245\\180^\\150\\204\\209%\\161LE\\DLE\\232\\SYN\\148\\231\\170\\211\\186\\138$E\\138\\175ko\\156O\\SUBH\\SOH\\190\\186\\ENQ"}}],"bootWits'":[],"scriptWits'":[]},"_body":{"TxBodyRaw":{"adHash":"SNothing","vldt":{"invalidHereafter":{"SlotNo":"100000000"},"invalidBefore":"SNothing"},"inputs":[{"TxInCompact":[{"TxId":{"SafeHash":"e1e86da6446c7f81da8d5e440bb0d4eed0f1530ba15bf77e49c33d6f050d8fb5"}},"0"]}],"certs":{},"wdrls":{},"outputs":[[{"Addr":{"Testnet":[{"KeyHashObj":{"KeyHash":"ff7b4521589238cfb9c26870edfa782541e61544474422d849ceb103"}},{"StakeRefNull":[]}]}},{"Value":["1660110",{}]}]],"txfee":"169945","mint":["0",{}],"update":"SNothing"}},"_auxiliaryData":"SNothing"}}}}}}}}]}}}}

[ogmios:Info:541] [2021-06-06 13:45:39.80 UTC] {"OgmiosWebSocket":{"WebSocketClient":{"TrTxSubmission":{"Recv":[{"ServerAgency":{"TokBusy":[]}},{"MsgRejectTx":{"HardForkApplyTxErrFromEra":{"S":{"S":{"S":{"Z":{"WrapApplyTxErr":{"ApplyTxError":[{"UtxowFailure":{"UtxoFailure":{"ValueNotConservedUTxO":[{"Value":["0",{}]},{"Value":["1830055",{}]}]}}},{"UtxowFailure":{"UtxoFailure":{"BadInputsUTxO":[{"TxInCompact":[{"TxId":{"SafeHash":"e1e86da6446c7f81da8d5e440bb0d4eed0f1530ba15bf77e49c33d6f050d8fb5"}},"0"]}]}}}]}}}}}}}}]}}}}
```

## Usage

```hs
{-# LANGUAGE DerivingVia #-}

data Foo = Foo
    { foo :: [Int]
    , bar :: String
    }
    deriving stock Show
    deriving ToJSON via ToJSONViaShow Foo

data Log = Log Bool LastUpdate
    deriving stock Show
    deriving ToJSON via ToJSONViaShow Log

newtype LastUpdate = LastUpdate
    { unLastUpdate :: UTCTime
    }
    deriving stock Show
    deriving ToJSON via ToJSONViaShow LastUpdate
```

```hs
>>> encode (Foo [42] "str")
{"Foo":{"foo":["42"],"bar":"str"}}

>>> encode (Log True (LastUpdate now))
{"Log":[true,{"LastUpdate":"2021-06-05 17:17:54.710264188 UTC"}]}
```

<hr/>

<p align="center">
  <a href="../../../CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
