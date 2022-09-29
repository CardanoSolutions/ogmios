+++
title = "Overview"
chapter = false
weight = 1
+++

## Overview

The TypeScript client for Ogmios is available on [`npm`](https://www.npmjs.com/package/@cardano-ogmios/client):

```console
$ yarn add @cardano-ogmios/client
```

It provides a fully typed interface for interacting with Ogmios as well as many convenient functions to manage WebSocket connections and drive the Ouroboros mini-protocols. 

The package is divided into a few modules: one per Ouroboros mini-protocols and some for managing WebSocket connections or utils. Let's start with the `Connection` module which is a necessary step regardless of which protocol you'll then use.

{{% notice tip %}}
The complete API reference for the TypeScript client is [available here](/typescript/api/modules/_cardano_ogmios_client.html). Have a look!
{{% /notice %}}

### InteractionContext

This module allows you to define an `InteractionContext` which tells the client how to create and manage WebSocket connections. The creation of an `InteractionContext` returns a `Promise` which is rejected if the server isn't reachable _yet_. That can happen when you first start your service stack; Ogmios is only ready when the underlying cardano-node is itself ready. Cardano-node runs a serie of integrity checks on its database on start, which can often takes a few seconds if not minutes. While doing so, the node does not allow any connections. Using the interaction context, you can implement retry mechanism on failures until it finally resolves.

To create an `InteractionContext`, you need to provide three elements:

- The connection configuration (port, host, protocol)
- An error handler
- A close handler

For example:

```ts
import { createInteractionContext, InteractionContext } from '@cardano-ogmios/client'

const context : InteractionContext = await createInteractionContext(
  err => console.error(err),
  () => console.log("Connection closed."),
  { connection: { port: 1337 } }
)
```

Note that the last connection argument is optional and will use default values when omitted. 

{{% notice warning %}}
An `InteractionContext` is a tiny abstraction on top of a WebSocket connection. Sharing `InteractionContext` between multiple clients isn't recommend and may lead to confusing behavior as some clients will close the underlying connections after certain queries. 
{{% /notice %}}

Once you've got your hands on an `InteractionContext`, you can use it to create any of the 3 clients (each of one corresponding to one of the Ouroboros mini-protocols):

- [`ChainSyncClient`](/typescript-client/chain-sync)
- [`StateQueryClient`](/typescript-client/state-query)
- [`TxSubmissionClient`](/typescript-client/tx-submission)
