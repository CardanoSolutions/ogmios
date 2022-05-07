
+++
title = "Local Tx Submission"
chapter = false
weight = 3
+++


{{% ascii-drawing-split %}}
 ┌──────────┐
 │   Busy   │◀═══════════════════════════════════════╗
 └────┬─────┘        SubmitTx / EvaluateTx           ║
      │                                              ║
      │                                         ┌──────────┐
      │                                         │          │
      │                                         │          │
      │  SubmitTxResponse / EvaluateTxResponse  │   Idle   │
      └────────────────────────────────────────▶│          │
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

## SubmitTx 

Sending a transaction through the Cardano network requires one message using the method `SubmitTx`, and with a single mandatory arguments with `bytes`, representing a serialized signed transactions with its full witness. 

Note that JSON does not support embedding raw bytes in objects. Bytes needs therefore to be encoded in either `Base16` or `Base64`; Ogmios will try both encoding. 

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "SubmitTx",
    "args": { "submit": "<base16 or base64>" }
}
```

The response will indicate either a `SubmitSuccess` or `SubmitFail`. In case of failure, Ogmios will return a list of failures reported by the underlying node. Note that, if the transaction fails to parse, Ogmios will reply with a generic error. 

Transactions in Shelley are rather _complicated_ and there is **a lot of** possible validation errors that can be returned. Be sure to have a look at the [API reference](../../api) for an exhaustive list.

## EvaluateTx

Starting from [`5.2.0`](https://github.com/CardanoSolutions/ogmios/releases/tag/v5.2.0), Ogmios supports a modified version of the local-tx-submission protocol that allows to evaluate the execution units of scripts present in a given transaction, **without actually submitting the transaction**. This is useful for DApp developers who wants a quick-and-easy way to measure script execution costs.

The API is purposely similar to the `SubmitTx` command, with a few semantic changes:

- The transaction needs not to be fully authenticated. Key witnesses may be omitted unless they are relevant to the evaluation of scripts themselves!
- The transaction needs not to be balanced; indeed, the evaluation does not perform a full execution of all the ledger rules. So while the transaction must be well-formed, it may be _invalid_ with regards to phase-1 validations.
- Execution budgets assigned to redeemers are expected to be set to zero since the goal of this endpoint is to figure out these very execution budgets.

From there, the endpoint works similarly to `SubmitTx`, but with different method and argument names:

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "EvaluateTx",
    "args": { "evaluate": "<base16 or base64>" }
}
```

Successful responses include a map of [_redeemer pointers_]() with the corresponding execution units. A redeemer pointer is a key composed of two parts: a _redeemer entity tag_ and a 0-based index related to that entity. There exists 4 kinds of redeemer entities: `spend` (for transaction inputs), `certificate` (for transaction certificates), `mint` (for transaction monetary policies) and `withdrawal` (for transaction's rewards withdrawals). The index therefore refers to the position of the script-locked entity within the set of entities in the transaction.

For example `spend:0` points to the first transaction input; `mint:2` would point to the 3rd policy referenced in the minting map... and so forth.  Here below is a JSON example of an evaluation result:

```json
{
  "type": "jsonwsp/response",
  "version": "1.0",
  "servicename": "ogmios",
  "methodname": "EvaluateTx",
  "result": {
    "EvaluationResult": {
      "spend:0": {
        "memory": 1700,
        "steps": 476468
      }
    }
  },
  "reflection": null
}
```

See the [full API reference](/api/modules/_cardano_ogmios_client.TxSubmission.evaluationErrors.html) for details about possible errors returned from this endpoint.

{{% notice warning %}}
If you're using typed Plutus validators (if you don't know what that is, then it is most likely what you're using), keep in mind that **adding or removing elements** to and off your transaction **will change its execution cost**. Indeed, the creation of the script context passed down to on-chain validators is done as part of the on-chain validator execution. Thus, **larger contexts require more execution units!** <br/><br/> This is the case for instance when you add a change output to a transaction or, a script integrity hash. A generally good way to approach this problem is to either: <br/><br/>**1.** make sure that the transaction you evaluate is as close as possible to the final transaction; that is, create dummy change outputs and script integrity hash before evaluating and fill-in their actual value once evaluated;<br/>**2.** keep some safe margin from the evaluated execution units; Execution units are relatively cheap on Cardano so, an extra 5 or 10% isn't much and saves you in most cases a lot of hassle to cope with small differences.
{{% /notice %}}

### Additional UTXO Set

In order to construct the validator script context, Ogmios needs to resolve transaction inputs from the Cardano blockchain. In case where a submitted transaction refers to non-existing inputs, the evaluation will fail with an [UnknownInputs](/api/modules/_cardano_ogmios_client.TxSubmission.evaluationErrors.html#UnknownInputs) error. This can be an impediment during development or, in scenarios where transactions are being prepared ahead of UTXO.

In such scenarios, Ogmios gives way to provide an _additional UTXO set_ to be used during evaluation. Note that it will still try to resolve inputs that are known, but will use the provided UTXO set as a complement for those that are unknown or yet-to-know.

{{% notice tip %}}
The [structure of the additional UTXO set](/api/modules/_cardano_ogmios_schema.html#Utxo) is the same as UTXO sets returned in other part of the Ogmios' API; that is, an array of `[TxIn, TxOut]` tuples.
{{% /notice %}}

For example:

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "EvaluateTx",
    "args": { 
      "evaluate": "<base16 or base64>",
      "additionalUtxoSet": [
        [ 
          { "txId":"97b2af6dfc6a4825e934146f424cdd6ede43ff98c355d2ae3aa95b0f70b63949"
          , "index": 3
          },
          { "address": "addr_test1qp9zjnc775anpndl0jh3w7vyy25syfezf70m7qmleaky0fdu9mqe2tg33xyxlcqcy98w630c82cyzuwyrumn65cv57nqwxm2yd"
          , "value": { "coins": 10000000 }
          } 
        ]
      ]
    }
}
```


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
    const submit =
      "g6QAgYJYIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGCglg5AQEBAQEBAQEB"+
      "AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBGgAehICC"+
      "WDkBAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIC"+
      "AgICAgIaAHgXXAIaAAH6pAMZHkahAIGCWCABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"+
      "AAAAAFhA169grjPSrzUUEcFEXHlZBSaZC/pzy7NzK1TvMi2qFC5ohAI0EPi+PBbpvVIHbyuz"+
      "a/ON/gNKnwRljp9WGXq4D/Y=";

    wsp("SubmitTx", { submit });
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
