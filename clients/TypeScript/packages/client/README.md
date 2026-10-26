# Cardano Ogmios TypeScript Client Packages - Client

Provides a natural interface for ECMAScript integrations, including abstraction of Websocket
management and custom error mapping. The [schema] package is imported and
exported as `Schema`.

## Quick Start
```console
yarn add @cardano-ogmios/client
```

## Request Timeouts
Ledger state queries (and `findIntersection`) accept an optional `MethodOptions` argument with
a `timeout`, in milliseconds. When set, the call rejects with a `RequestTimeoutError` if no
response is received in time:

```ts
import { createInteractionContext, createLedgerStateQueryClient, RequestTimeoutError } from '@cardano-ogmios/client'

const context = await createInteractionContext(console.error, () => {})
const client = await createLedgerStateQueryClient(context, {}, { timeout: 5000 })

try {
  await client.ledgerTip()
} catch (e) {
  if (e instanceof RequestTimeoutError) {
    // Give up on this connection; see below.
  }
}
```

A timeout only stops the client from waiting: the server still processes the request, and
since requests on a connection are handled sequentially, the connection is no longer in a known
state afterwards (for instance, a timed-out `acquireLedgerState` may have succeeded server-side).
After a `RequestTimeoutError`, close the connection and create a new `InteractionContext`.

## Build From Source
See instructions in [Workspace root]

[schema]: ../schema
[Workspace root]: ../..
