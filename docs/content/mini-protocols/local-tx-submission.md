
+++
title = "Local Tx Submission"
chapter = false
weight = 3
+++


{{% ascii-drawing-split %}}
 ┌──────────┐
 │   Busy   │◀══════════════════════════════╗      
 └────┬─────┘            SubmitTx           ║      
      │                                     ║      
      │                                ┌──────────┐
      │                                │          │
      │                                │          │
      │          SubmitTxResponse      │   Idle   │
      └───────────────────────────────▶│          │
                                       │          │⇦ START
                                       └──────────┘
{{% /ascii-drawing-split %}}

## Overview

Transaction submission is pretty simple & works by submitting an already serialized and signed transaction as one single message.

In case of success, Ogmios / the node returns an empty response. Otherwise, it returns an error with some details about what went wrong. Clients must thereby know how to construct valid transactions.

## Disclaimer

The local tx-submission protocol is the simplest one in appearance. It nevertheless a quite extensive knowledge of the on-chain data-types used by Cardano. Indeed, the protocol in itself is straightforward so long as you already know how to produce and sign a transaction. 

This guide doesn't cover the creation and serialization of Cardano transactions. This is a rather vast topic and there is a handful of tools out there to help on the matter already, in particular:

- [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib) which gives JavaScript and WASM bindings for every Cardano on-chain types.

- [cardano-transactions](https://github.com/input-output-hk/cardano-transactions/) which offers a Haskell library and a command-line interface for constructing and signing transactions.

- [cardano-cli](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli) which offers another command-line interface for constructing and signing transactions.

In any case, one can always refer to the source [CDDL specifications](https://github.com/input-output-hk/cardano-ledger-specs/blob/d6179d72c52588460c1d57b932a2fd0724c5db32/shelley/chain-and-ledger/shelley-spec-ledger-test/cddl-files/shelley.cddl) to know how to construct and serialize Cardano transactions.

{{% notice info %}}
Providing a more user-friendly interface with regards to transactions in Ogmios is still under consideration. Yet, since in order to handle and sign transactions, one needs some knowledge about the on-chain binary format anyway, I've made the (effortless) choice to only treat with already serialized blobs in Ogmios. I am open to suggestions about how this could be made better, drop me a message on Github if you have ideas!
{{% /notice %}}

## How to Use

Sending a transaction through the Cardano network requires one message using the method `SubmitTx`, and with a single mandatory arguments with `bytes`, representing a serialized signed transactions with its full witness. 

Note that JSON does not support embedding raw bytes in objects. Bytes needs therefore to be encoded in either `Base16` or `Base64`; Ogmios will try both encoding. 

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "SubmitTx",
    "args": { "bytes": "<base16 or base64>" }
}
```

The response will indicate either a `SubmitSuccess` or `SubmitFail`. In case of failure, Ogmios will return a list of failures reported by the underlying node. Note that, if the transaction fails to parse, Ogmios will reply with a generic error. 

Transactions in Shelley are rather _complicated_ and there is **a lot of** possible validation errors that can be returned. Be sure to have a look at the [API reference](../../api-reference) for an exhaustive list. 

## Full Example

For what it's worth, here's an example of a transaction submission to the Cardano mainnet via Ogmios. This transaction is using dummy data and will obviously fail. It is however structurally valid, so useful to test if an integration works correctly. 

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function wsp(methodname, args) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname,
        args
    }));
}

client.once('open', () => {
    const bytes =
      "g6QAgYJYIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGCglg5AQEBAQEBAQEB"+
      "AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBGgAehICC"+
      "WDkBAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC"+
      "AgICAgIaAHgXXAIaAAH6pAMZHkahAIGCWCABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"+
      "AAAAAFhA169grjPSrzUUEcFEXHlZBSaZC/pzy7NzK1TvMi2qFC5ohAI0EPi+PBbpvVIHbyuz"+
      "a/ON/gNKnwRljp9WGXq4D/Y=";

    wsp("SubmitTx", { bytes });
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);
    console.log(response);
    client.close();
});
```

Ogmios replies negatively to the request, returning 4 errors reported by the ledger. Yet, this was indeed reported by the ledger itself, amazing isn't it?

{{% expand "SubmitFail" %}}
```json
{
  "version": "1.0",
  "servicename": "ogmios",
  "type": "jsonwsp/response",
  "methodname": "SubmitTx",
  "result": {
    "SubmitTxResponse": {
      "error": {
        "SubmitFail": [
          {
            "expiredUtxo": {
              "transactionTimeToLive": 7750,
              "currentSlot": 12588967
            }
          },
          {
            "feeTooSmall": {
              "requiredFee": 168009,
              "actualFee": 129700
            }
          },
          {
            "badInputs": [
              {
                "index": 0,
                "txId": "0000000000000000000000000000000000000000000000000000000000000000"
              }
            ]
          },
          {
            "valueNotConserved": {
              "consumed": 0,
              "produced": 10000000
            }
          }
        ]
      }
    }
  },
  "reflection": null
}
```
{{% /expand %}}
