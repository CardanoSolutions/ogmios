+++
title = "ChainSync Client"
chapter = false
weight = 2
+++

The ChainSync client streams block from the network, one-by-one in the form of events. Creating such a client requires your application to register two callbacks:

- on `rollForward`, to process new blocks coming in and advancing your local chain.
- on `rollBackward`, to process rollback requests to a previous point of your local chain.

{{% notice tip %}}
These callbacks are captured via the [ChainSyncMessageHandlers](/typescript/api/interfaces/_cardano_ogmios_client.ChainSync.ChainSyncMessageHandlers.html) interface.
{{% /notice %}}

For example, you can store blocks into a _database_ (assuming some high-level `db` interface) as follows:


```ts
import { createChainSyncClient } from '@cardano-ogmios/client'

const rollForward = async ({ block }, requestNext) => {
  await db.insert(block)
  requestNext()
}

const rollBackward = async ({ point }, requestNext) => {
  await db.rollback(point)
  requestNext()
}

const client = await createChainSyncClient(context, { rollForward, rollBackward })

await client.startSync()

// ...

await client.shutdown()
```

Notice how the callback also includes a continuation `requestNext` which commands the server to send the next block. For more details about the chain-sync mini-protocol, have a look at [the protocol documentation itself](mini-protocols/local-chain-sync/).

Once created, the client offers two methods `startSync` and `shutdown`. As a matter of fact, the client does not start syncing automatically. This allows you to specify a starting point if necessary (in case your application is resuming from a previous state). This is optional and by default the client will synchronize from the node's tip and onward.

### Concurrent vs sequential

By default, the client will send requests and process requests _sequentially_ for it is more intuitive. However, some applications may not necessarily bother about sequentiality and may process messages in any order. This behavior can hence by changed by providing an extra option to `createChainSyncClient`:

```ts
const client = await createChainSyncClient(context, callbacks, { sequential: false })
```

When `sequential` is set to `false`, the order in which message callbacks are resolved depends on the JavaScript runtime and whether or not your are making concurrent requests.
