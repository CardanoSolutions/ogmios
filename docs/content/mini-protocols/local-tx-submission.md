
+++
title = "Transaction submission"
chapter = false
weight = 3
+++


{{% ascii-drawing-split %}}
                        submitTransaction
                     ╭────────╮
         START       │        │
           ⇓         │        │
        ┌────────────┴───┐    │
 ╭──────┤      Idle      │◀───╯
 │      └────────────────┘
 │        ▲
 │        │
 ╰────────╯
    evaluateTransaction
{{% /ascii-drawing-split %}}

## Overview

Transaction submission is pretty simple & works by submitting an already serialized and signed transaction as one single message.

In case of success, Ogmios / the node returns an empty response. Otherwise, it returns an error with some details about what went wrong. Clients must thereby know how to construct valid transactions.

## Disclaimer

The transaction submission protocol is the simplest one in appearance. It nevertheless requires a quite extensive knowledge of the on-chain data-types used by Cardano. Indeed, the protocol in itself is straightforward so long as you already know how to produce and sign a transaction.

This guide doesn't cover the creation and serialization of Cardano transactions. This is a rather vast topic and there is a handful of tools out there to help on the matter already, in particular:

- [Lucid](https://lucid.spacebudz.io/), a TypeScript / Deno package for building transaction and managing credentials. It has direct integration into Ogmios.

- [Mesh.js](https://meshjs.dev/), a JavaScript library providing numerous tools to easily build powerful dApps on the Cardano blockchain.

- [cardano-cli](https://github.com/intersectMBO/cardano-cli/tree/main/cardano-cli) which offers another command-line interface for constructing and signing transactions.

In any case, one can always refer to the source [CDDL specifications](https://github.com/input-output-hk/cardano-ledger/blob/5af4f68486680bf73ba21a9620a14e128e9fe5f7/eras/babbage/test-suite/cddl-files/babbage.cddl) to know how to construct and serialize Cardano transactions.

{{% notice info %}}
Providing a more user-friendly interface with regards to transactions in Ogmios is still under consideration. Yet, since in order to handle and sign transactions, one needs some knowledge about the on-chain binary format anyway, I've made the (effortless) choice to only treat with already serialized blobs in Ogmios. I am open to suggestions about how this could be made better, drop me a message on Github if you have ideas!
{{% /notice %}}

## Submitting transactions

Sending a transaction through the Cardano network requires one message using the method `SubmitTx`, and with a single mandatory arguments with `bytes`, representing a serialized signed transactions with its full witness.

Note that JSON does not support embedding raw bytes in objects. Bytes needs therefore to be base16-encoded.

```json
{
    "jsonrpc": "2.0",
    "method": "submitTransaction",
    "params": { "transaction": { "cbor": { "<base16>" } } }
}
```

The response will indicate either a success or a failure. In case of failure, Ogmios will return a list of failures reported by the underlying node. Note that, if the transaction fails to parse, Ogmios will reply with a generic error.

Transactions in Cardano are rather _complicated_ and there are **a lot of** possible validation errors that can be returned. Be sure to have a look at the [API reference](../../api) for an exhaustive list.

## Evaluating transactions

Starting from [`5.2.0`](https://github.com/CardanoSolutions/ogmios/releases/tag/v5.2.0), Ogmios supports a modified version of the transaction submission protocol that allows to evaluate the execution units of scripts present in a given transaction, **without actually submitting the transaction**. This is useful for DApp developers who wants a quick-and-easy way to measure script execution costs.

The API is purposely similar to the `submitTransaction` method, with a few semantic changes:

- The transaction needs not to be fully authenticated. Key witnesses may be omitted unless they are relevant to the evaluation of scripts themselves!
- The transaction needs not to be balanced; indeed, the evaluation does not perform a full execution of all the ledger rules. So while the transaction must be well-formed, it may be _invalid_ with regards to phase-1 validations.
- Execution budgets assigned to redeemers are expected to be set to zero since the goal of this endpoint is to figure out these very execution budgets.

From there, the endpoint works similarly to the submission:

```json
{
    "jsonrpc": "2.0",
    "method": "evaluateTransaction",
    "params": { "transaction": { "cbor": "<base16>" } }
}
```

Successful responses include a map of [_redeemer pointers_]() with the corresponding execution units. A redeemer pointer is a key composed of two parts: a _redeemer entity tag_ and a 0-based index related to that entity. There exists 4 kinds of redeemer entities: `spend` (for transaction inputs), `certificate` (for transaction certificates), `mint` (for transaction monetary policies) and `withdrawal` (for transaction's rewards withdrawals). The index therefore refers to the position of the script-locked entity within the set of entities in the transaction.

For example `spend:0` points to the first transaction input; `mint:2` would point to the 3rd policy referenced in the minting map... and so forth.  Here below is a JSON example of an evaluation result:

```json
{
  "jsonrpc": "2.0",
  "method": "evaluateTransaction",
  "result": [{
    "validator": "spend:0",
    "budget": {
      "memory": 1700,
      "cpu": 476468
    }
  }]
}
```

See the [full API reference](/api/modules/_cardano_ogmios_client.TxSubmission.evaluationErrors.html) for details about possible errors returned from this endpoint.

{{% notice warning %}}
If you're using typed Plutus validators (if you don't know what that is, then it is most likely what you're using), keep in mind that **adding or removing elements** to and off your transaction **will change its execution cost**. Indeed, the creation of the script context passed down to on-chain validators is done as part of the on-chain validator execution. Thus, **larger contexts require more execution units!** <br/><br/> This is the case for instance when you add a change output to a transaction or, a script integrity hash. A generally good way to approach this problem is to either: <br/><br/>**1.** make sure that the transaction you evaluate is as close as possible to the final transaction; that is, create dummy change outputs and script integrity hash before evaluating and fill-in their actual value once evaluated;<br/>**2.** keep some safe margin from the evaluated execution units; Execution units are relatively cheap on Cardano so, an extra 5 or 10% isn't much and saves you in most cases a lot of hassle to cope with small differences.
{{% /notice %}}

### Additional UTXO Set

In order to construct the validator script context, Ogmios needs to resolve transaction inputs from the Cardano blockchain. In case where a submitted transaction refers to non-existing inputs, the evaluation will fail with an [UnknownInputs](/api/modules/_cardano_ogmios_client.TxSubmission.evaluationErrors.html#UnknownInputs) error. This can be an obstacle during development or, in scenarios where transactions are being prepared ahead of UTXO.

In such scenarios, Ogmios gives way to provide an _additional UTXO set_ to be used during evaluation. Note that it will still try to resolve inputs that are known, but will use the provided UTXO set as a complement for those that are unknown or yet-to-know.

{{% notice tip %}}
The [structure of the additional UTXO set](/api/modules/_cardano_ogmios_schema.html#Utxo) is the same as UTXO sets returned in other part of the Ogmios' API; that is, an array of `[OutputReference, Output]` tuples.
{{% /notice %}}

For example:

```json
{
    "jsonrpc": "2.0",
    "method": "evaluateTransaction",
    "params": {
      "transaction": {
        "cbor": "<base16>",
      },
      "additionalUtxoSet": [
        [
          {
            "transaction": { "id": "97b2af6dfc6a4825e934146f424cdd6ede43ff98c355d2ae3aa95b0f70b63949" },
            "output": { "index": 3 }
          },
          {
            "address": "addr_test1qp9zjnc775anpndl0jh3w7vyy25syfezf70d",
            "value": { "lovelace": 10000000 }
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

function rpc(method, params) {
    client.send(JSON.stringify({
        jsonrpc: "2.0",
        method,
        params
    }));
}

client.once('open', () => {
    const cbor =
      "83a4008182582000000000000000000000000000000000000000000000000000"+
      "0000000000000000018282583901010101010101010101010101010101010101"+
      "0101010101010101010101010101010101010101010101010101010101010101"+
      "0101010101011a001e8480825839010202020202020202020202020202020202"+
      "0202020202020202020202020202020202020202020202020202020202020202"+
      "020202020202021a0078175c021a0001faa403191e46a1008182582001000000"+
      "000000000000000000000000000000000000000000000000000000005840d7af"+
      "60ae33d2af351411c1445c79590526990bfa73cbb3732b54ef322daa142e6884"+
      "023410f8be3c16e9bd52076f2bb36bf38dfe034a9f04658e9f56197ab80ff6";

    rpc("submitTransaction", { transaction: { cbor } });
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);
    console.log(response);
    client.close();
});
```

## Errors

Errors from the transaction submission protocol are in the range `3000-3999` and are listed below.

{{% embed-async-api %}}
asyncapi: '2.4.0'
info:
  title: ""
  version: '6.0.0'
servers: {}
channels: {}
components:
  schemas:
    3000/IncompatibleEra:
      $ref: "/ogmios.json#/definitions/EvaluateTransactionFailure/oneOf/0"
    3001/UnsupportedEra:
      $ref: "/ogmios.json#/definitions/EvaluateTransactionFailure/oneOf/1"
    3002/OverlappingAdditionalUtxo:
      $ref: "/ogmios.json#/definitions/EvaluateTransactionFailure/oneOf/2"
    3003/NodeTipTooOld:
      $ref: "/ogmios.json#/definitions/EvaluateTransactionFailure/oneOf/3"
    3004/CannotCreateEvaluationContext:
      $ref: "/ogmios.json#/definitions/EvaluateTransactionFailure/oneOf/4"
    3005/EraMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/0"
    3010/ScriptExecutionFailure:
      $ref: "/ogmios.json#/definitions/EvaluateTransactionFailure/oneOf/5"
    3011/InvalidRedeemerPointers:
      $ref: "/ogmios.json#/definitions/ScriptExecutionFailure/oneOf/0"
    3012/ValidationFailure:
      $ref: "/ogmios.json#/definitions/ScriptExecutionFailure/oneOf/1"
    3013/UnsuitableOutputReference:
      $ref: "/ogmios.json#/definitions/ScriptExecutionFailure/oneOf/2"
    3100/InvalidSignatories:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/1"
    3101/MissingSignatories:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/2"
    3102/MissingScripts:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/3"
    3103/FailingNativeScript:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/4"
    3104/ExtraneousScripts:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/5"
    3105/MissingMetadataHash:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/6"
    3106/MissingMetadata:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/7"
    3107/MetadataHashMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/8"
    3108/InvalidMetadata:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/9"
    3109/MissingRedeemers:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/10"
    3110/ExtraneousRedeemers:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/11"
    3111/MissingDatums:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/12"
    3112/ExtraneousDatums:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/13"
    3113/ScriptIntegrityHashMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/14"
    3114/OrphanScriptInputs:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/15"
    3115/MissingCostModels:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/16"
    3116/MalformedScripts:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/17"
    3117/UnknownOutputReference:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/18"
    3118/OutsideOfValidityInterval:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/19"
    3119/TransactionTooLarge:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/20"
    3120/ValueTooLarge:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/21"
    3121/EmptyInputSet:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/22"
    3122/FeeTooSmall:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/23"
    3123/ValueNotConserved:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/24"
    3124/NetworkMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/25"
    3125/InsufficientlyFundedOutputs:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/26"
    3126/BootstrapAttributesTooLarge:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/27"
    3127/MintingOrBurningAda:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/28"
    3128/InsufficientCollateral:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/29"
    3129/CollateralLockedByScript:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/30"
    3130/UnforeseeableSlot:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/31"
    3131/TooManyCollateralInputs:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/32"
    3132/MissingCollateralInputs:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/33"
    3133/NonAdaCollateral:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/34"
    3134/ExecutionUnitsTooLarge:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/35"
    3135/TotalCollateralMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/36"
    3136/SpendsMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/37"
    3137/UnauthorizedVotes:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/38"
    3138/UnknownGovernanceProposals:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/39"
    3139/InvalidProtocolParametersUpdate:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/40"
    3140/UnknownStakePool:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/41"
    3141/IncompleteWithdrawals:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/42"
    3142/RetirementTooLate:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/43"
    3143/StakePoolCostTooLow:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/44"
    3144/MetadataHashTooLarge:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/45"
    3145/CredentialAlreadyRegistered:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/46"
    3146/UnknownCredential:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/47"
    3147/NonEmptyRewardAccount:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/48"
    3148/InvalidGenesisDelegation:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/49"
    3149/InvalidMIRTransfer:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/50"
    3150/ForbiddenWithdrawal:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/51"
    3151/CredentialDepositMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/52"
    3152/DRepAlreadyRegistered:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/53"
    3153/DRepNotRegistered:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/54"
    3154/UnknownConstitutionalCommitteeMember:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/55"
    3155/GovernanceProposalDepositMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/56"
    3156/ConflictingCommitteeUpdate:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/57"
    3157/InvalidCommitteeUpdate:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/58"
    3158/TreasuryWithdrawalMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/59"
    3159/InvalidOrMissingPreviousProposals:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/60"
    3160/VotingOnExpiredActions:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/61"
    3161/ExecutionBudgetOutOfBounds:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/62"
    3162/InvalidHardForkVersionBump:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/63"
    3163/ConstitutionGuardrailsHashMismatch:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/64"
    3998/UnrecognizedCertificateType:
      $ref: "/ogmios.json#/definitions/SubmitTransactionFailure/oneOf/62"
{{% /embed-async-api %}}

## API Reference

The complete description of the mempool monitoring requests and responses can be found in the [API reference](../../api).

Plus, [test vectors](https://github.com/CardanoSolutions/ogmios/tree/master/server/test/vectors) are available on the repository for testing, debugging and to serve as examples.
