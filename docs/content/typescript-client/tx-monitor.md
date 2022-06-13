+++
title = "TxMonitor Client"
chapter = false
weight = 5
+++

As described in the [Local Tx Monitor](/mini-protocols/local-tx-monitor/) user guide, Ogmios gives ways to inspect the attached node's mempool. A new `TxMonitorClient` can be created using an interaction context and be used to run many queries. 

Note that all queries are blocking on this client and in particular, the `awaitAcquire` query. The latter will only return once a new snapshot (that is, different from the one currently acquired, if any) can be acquired. It can therefore be used as a notification mechanism to automatically 'wake up' when something has changed in the mempool. 

A typical usage of the mempool consist of acquiring a snapshot, get all the transactions in the mempool, and then wait for a new snapshot to become available. The complete API reference can be found [here](/typescript/api/modules/_cardano_ogmios_client.TxMonitor.html).

```ts
import { createTxMonitorClient, TxMonitor } from '@cardano-ogmios/client'

const client = await createTxMonitorClient(context)

let txs;

await client.awaitAcquire() // first 'awaitAcquire' should resolve instantly
txs = await fetchAllTxs(client)
doSomething(txs)

await client.awaitAcquire() // second is blocking, waiting for a change
txs = await fetchAllTxs(client)
doSomething(txs)

// Eventually... once done
await client.release()

// A recursive function to fetch the 'nextTx' until there's no more to fetch.
async function fetchAllTxs(client) {
  const tx = await client.nextTx()

  if (tx === null) {
    return []
  }
    
  return [tx].concat(await fetchAllTxs(client))
}
```
