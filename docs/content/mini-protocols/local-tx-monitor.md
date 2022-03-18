
+++
title = "Local Tx Monitor"
chapter = false
weight = 4
+++


{{% ascii-drawing-split %}}
                                START
                                  ⇓
                          ┌───────────────┐
                 ┌──────▶ │     Idle      │⇒ DONE 
                 │        └───────┬───────┘
                 │                │
                 │   AwaitAcquire │
                 │                │
                 │                ▼
                 │        ┌───────────────┐
  ReleaseMempool │        │   Acquiring   │
                 │        └───┬───────────┘
                 │            │       ▲
                 │   Acquired │       │ 
                 │            │       │ AwaitAcquire
                 │            ▼       │
                 │        ┌───────────┴───┐
                 └────────┤   Acquired    │
                          └───┬───────────┘
                              │       ▲
 HasTx|NextTx|SizeAndCapacity │       │ Reply (HasTx|NextTx|SizeAndCapacity)
                              ▼       │
                          ┌───────────┴───┐
                          │      Busy     │
                          └───────────────┘
{{% /ascii-drawing-split %}}

# Overview

To inspect the node's local mempool, one may rely on the local-tx-monitor mini-protocol. This protocol provides way to list all transactions sitting in the mempool, but also, to query the size of the mempool, the number of transactions currently in the mempool as well as the current capacity (based on network parameters). 

As for the other mini-protocols, the local-tx-monitor is a stateful protocol with explicit state acquisition driven by the client. That is, clients must first acquire a mempool snapshot for running queries over it. Once acquired, queries are guaranteed to be consistent. In particular, `NextTx` will never yield twice the same transaction for the same snapshot and `SizeAndCapacity` will remain constant. 

`AwaitAcquire` is a blocking call. The server will only reply with `Acquired` once a _"new"_ snapshot is available when _"new"_ means different from the currently acquired snapshot. Seemingly, the first `AwaitAcquire` is instantaneous. This allows for clients to passively wait for changes without active polling. A typical pattern of usage would be to acquire a snapshot, list all transactions from the mempool via `NextTx` and then, block on `AwaitAcquire` for a change; then repeat. 

# How To Use 

First, client must always acquire a snapshot and hold onto it for subsequent queries. To list all queries, one must call `NextTx` repeatedly until it yields `null`. So for instance, if the mempool currently contains three transactions `t0`, `t1` and `t2`, one can list all transactions from the mempool via the following sequence (schematically):

<pre>
1. AwaitAcquire → Acquired
2. NextTx → NextTxResponse t0 
3. NextTx → NextTxResponse t1
4. NextTx → NextTxResponse t2 
5. NextTx → NextTxResponse null 
</pre>

Another option is simply to query for a specific transaction via `HasTx`, which yields `True` or `False` depending on whether the transaction is currently in the mempool or not.

<pre>
1. AwaitAcquire → Acquired
2. HasTx t0 → HasTxResponse True
3. HasTx t1 → HasTxResponse True
4. HasTx t5 → HasTxResponse False
</pre>

At any moment, it is also possible to interleave a `SizeAndCapacity` query to get the acquired snapshot's size (in bytes), number of transactions and capacity (in bytes). 

{{% notice tip %}}
The _capacity_ refers to the maximum size of the mempool. It is currently defined as twice the network block size and can be adjusted via protocol updates. 
{{% /notice %}}

## Retrieve Full Transactions

Since `5.3.0`, Ogmios can also return full transactions as a result of `NextTx`. This must be however explicitly requested from clients by providing an extra (optional) argument to each `NextTx` request:


```json
{                                 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "NextTx",
    "args": {
        "fields": "all"
    }
}
```
`"fields"` accept only one value (`"all"`) and can be omitted. When present, `NextTxResponse` will contain a full transaction as `"result"`. When omitted, the latter only contains a transaction id.

# Important Notes 

Some important notes to keep in mind regarding the management of the mempool:

### About Transaction Locality

This protocol gives access to transactions that are submitted locally, by the connected client via the local-tx-submission protocol. In case of block producing nodes (i.e. stake pools), transactions pulled from peers may also be available.

### About Transaction Observability

The protocol **does not** guarantee observability of all transactions passing through the mempool. There's an inherent race condition between the client acquiring snapshots and the node managing it internally. Thus, while a client is holding a snapshot, it may still submit transactions through the local-tx-submission protocol, which may be accepted, processed and included in the ledger before the client next's `AwaitAcquire`. So, it is possible for clients to miss transactions passing through the mempool should they be concurrently submitting them. 

### About Transaction Status

Furthermore, while the presence of a transaction in the mempool qualifies it as pending, the absence of transactions in the mempool does not guarantee their inclusion in the ledger (transaction may be discarded from the mempool for various reasons). In particular, a _valid_ transaction may leave the mempool to be included in a block which later result in a lost fork (e.g. because of a lost slot battle) and may never end up in the ledger. The node does not automatically re-insert transactions into the mempool. 

# Full Example

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

// Helper function
function wsp(methodname, args) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname,
        args
    }));
}

client.on('message', e => {
  const next = JSON.parse(e).result;
  if (next === null) {
    client.close();
  } else {
    console.log(next);
    wsp("NextTx");
  }
});

client.once('open', () => {
    wsp("AwaitAcquire");
});
```

# API Reference

The API reference will be available soon with the TypeScript client support.

Test vectors for [requests](https://github.com/CardanoSolutions/ogmios/tree/master/server/test/vectors/TxMonitor/Request) and [responses](https://github.com/CardanoSolutions/ogmios/tree/master/server/test/vectors/TxMonitor/Response) are available on the repository for testing, debugging and to serve as examples.
