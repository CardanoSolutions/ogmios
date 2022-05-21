+++
title = "StateQuery Client"
chapter = false
weight = 3
+++

The StateQuery client allows you to play with the [Local State Query](/mini-protocols/local-state-query) mini protocol; that is, a protocol for querying parts of the ledger state. The complete set of queries is described in [API reference](https://ogmios.dev/typescript/api/modules/_cardano_ogmios_client.StateQuery.html). To use it, create a client from a context and start querying!

```ts
import { createStateQueryClient } from '@cardano-ogmios/client'

const client = await createStateQueryClient(context)

console.log(`ledgerTip: ${(await client.ledgerTip()).tip}`)
// ledgerTip: {"slot":33055551,"hash":"050b05030645fdc4ee10e81f131030049c08f7763355873564540fe5a0533f43"}

console.log(`currentEpoch: ${await client.currentEpoch()}`)
// currentEpoch: 146

await client.shutdown() // Close the connection when done.
```

### Acquire a specific point

By default, the client will acquire the current tip of the blockchain and **run all subsequent queries** from that point. This makes it possible to get somewhat consistent data when running multiple queries. You can specify which point you want to acquire by providing an extra argument when constructing the client.

For example (notice how the `ledgerTip` remains unchanged, and equal to the acquired point!):

```
import delay from 'delay'
import { createStateQueryClient } from '@cardano-ogmios/client'

const point = {
  slot: 33055551,
  hash: '050b05030645fdc4ee10e81f131030049c08f7763355873564540fe5a0533f43'
}
const client = await createStateQueryClient(context, { point })

console.log(`ledgerTip: ${await client.ledgerTip().tip}`)
// ledgerTip: {"slot":33055551,"hash":"050b05030645fdc4ee10e81f131030049c08f7763355873564540fe5a0533f43"}

delay(5000)

console.log(`ledgerTip: ${await client.ledgerTip().tip}`)
// ledgerTip: {"slot":33055551,"hash":"050b05030645fdc4ee10e81f131030049c08f7763355873564540fe5a0533f43"}
```

{{% notice info %}}
You cannot acquire points that are _too old_ on the blockchain. Too old may be a bit tricky to define but a good rule of thumb is: anything older than 64800 slots is too old (i.e. `3k/f`).
{{% /notice %}}
