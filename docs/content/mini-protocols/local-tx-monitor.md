+++
title = "Mempool monitoring"
chapter = false
weight = 4
+++


{{% ascii-drawing-split %}}
                       START
                         ⇓
 releaseMempool  ┌───────────────┐
          ╭─────▶│     Idle      │⇒ DONE
          │      └───────┬───────┘
          │              │
          │              │ acquireMempool
          │              │
          │              │
          │              │      (re)acquireMempool
          │              │   ╭───────╮
          │              ▼   │       │
          │      ┌───────────┴───┐   │
          ╰──────┤   Acquired    │◀──╯
                 └───┬───────────┘
                     │       ▲
    nextTransaction  │       │
    hasTransaction   │       │
    sizeOfMempool    │       │
                     ╰───────╯
{{% /ascii-drawing-split %}}

## Overview

To inspect the node's local mempool, one may rely on the mempool monitoring mini-protocol. This protocol provides way to list all transactions sitting in the mempool, but also, to query the size of the mempool, the number of transactions currently in the mempool as well as the current capacity (based on network parameters).

As for the other mini-protocols, the mempool monitoring is a stateful protocol with explicit state acquisition driven by the client. That is, clients must first acquire a mempool snapshot for running queries over it. Once acquired, queries are guaranteed to be consistent. In particular, `nextTransaction` will never yield twice the same transaction for the same snapshot and `sizeOfMempool` will remain constant.

`acquireMempool` is a **blocking** call. The server will only reply once a _"new"_ snapshot is available. _"New"_ means different from the currently acquired snapshot. Seemingly, the first `acquireMempool` is instantaneous. This allows for clients to passively wait for changes without active polling. A typical pattern of usage would be to acquire a snapshot, list all transactions from the mempool via `nextTransaction` and then, block on `acquireMempool` for a change; then repeat.

## How To Use

First, client must always acquire a snapshot and hold onto it for subsequent queries. To list all queries, one must call `nextTransaction` repeatedly until it yields `null`. So for instance, if the mempool currently contains three transactions `t0`, `t1` and `t2`, one can list all transactions from the mempool via the following sequence (schematically):

<pre>
1. acquireMempool  → acquireMempoolResponse
2. nextTransaction → nextTransactionResponse t0
3. nextTransaction → nextTransactionResponse t1
4. nextTransaction → nextTransactionResponse t2
5. nextTransaction → nextTransactionResponse null
</pre>

Another option is simply to query for a specific transaction via `hasTransaction`, which yields `True` or `False` depending on whether the transaction is currently in the mempool or not.

<pre>
1. acquireMempool    → acquireMempoolResponse
2. hasTransaction t0 → hasTransactionResponse True
3. hasTransaction t1 → hasTransactionResponse True
4. hasTransaction t5 → hasTransactionResponse False
</pre>

At any moment, it is also possible to interleave a `sizeOfMempool` query to get the acquired snapshot's size (in bytes), number of transactions and capacity (in bytes).

{{% notice tip %}}
The _capacity_ refers to the maximum size of the mempool. It is currently defined as twice the network block size and can be adjusted via protocol updates.
{{% /notice %}}

### Retrieve Full Transactions

Since `5.3.0`, Ogmios can also return full transactions as a result of `nextTransaction`. This must be however explicitly requested from clients by providing an extra (optional) argument to each `nextTransaction` request:


```json
{
    "jsonrpc": "2.0",
    "method": "nextTransaction",
    "params": {
        "fields": "all"
    }
}
```
`"fields"` accept only one value (`"all"`) and can be omitted. When present, the `result` from the response will contain a full transaction. When omitted, `result` will only contains a transaction id.

## Important Notes

Some important notes to keep in mind regarding the management of the mempool:

### About Transaction Locality

This protocol gives access to transactions that are submitted locally, by the connected client via the [transaction submission protocol](../local-tx-submission). In case of block producing nodes (i.e. stake pools), transactions pulled from peers may also be available.

### About Transaction Observability

The protocol **does not** guarantee observability of all transactions passing through the mempool. There's an inherent race condition between the client acquiring snapshots and the node managing it internally. Thus, while a client is holding a snapshot, it may still submit transactions through the [transaction submission protocol](../local-tx-submission), which may be accepted, processed and included in the ledger before the client next's `acquireMempool`. So, it is possible for clients to miss transactions passing through the mempool should they be concurrently submitting them.

### About Transaction Status

Furthermore, while the presence of a transaction in the mempool qualifies it as pending, the absence of transactions in the mempool does not guarantee their inclusion in the ledger (transaction may be discarded from the mempool for various reasons). In particular, a _valid_ transaction may leave the mempool to be included in a block which later result in a lost fork (e.g. because of a lost slot battle) and may never end up in the ledger. The node does not automatically re-insert transactions into the mempool.

## Full Example

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

// Helper function
function rpc(method, params) {
    client.send(JSON.stringify({
        jsonrpc: "2.0",
        method,
        params
    }));
}

client.on('message', e => {
  const next = JSON.parse(e).result;
  if (next.transaction === null) {
    client.close();
  } else {
    console.log(next.transaction);
    rpc("nextTransaction");
  }
});

client.once('open', () => {
    rpc("acquireMempool");
});
```

## Errors

Errors from the mempool monitoring protocol are in the range `4000-4999` and are listed below.

{{% embed-async-api %}}
asyncapi: '2.4.0'
info:
  title: ""
  version: '6.0.0'
servers: {}
channels: {}
components:
  schemas:
    4000/MustAcquireMempoolFirst:
      $ref: "/ogmios.json#/properties/MustAcquireMempoolFirst/properties/error"
{{% /embed-async-api %}}

## API Reference

The complete description of the mempool monitoring requests and responses can be found in the [API reference](../../api).

Plus, [test vectors](https://github.com/CardanoSolutions/ogmios/tree/master/server/test/vectors) are available on the repository for testing, debugging and to serve as examples.
