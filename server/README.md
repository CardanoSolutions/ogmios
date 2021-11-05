# Ogmios (Server)

<img src="../docs/static/dashboard.gif" />

## Synopsis

Ogmios is a webserver which acts as a proxy for talking to a cardano-node. Upon request, it establishes a node-to-client 
connection with a Cardano node and forward messages from a client application to the node (and vice-versa). Ogmios aims to be
_as capable as_ a Cardano node (w.r.t. node-to-client protocols). Yet, since the internals are statically typed, new additions
in the node (e.g. new queries) needs to be mapped correctly internally and, translated as JSON.

Ogmios does a little more than that, as it offers also some monitoring capabilities and embeds an HTTP server. 

## Architecture

Ogmios is constructed as a [three-layer Haskell cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html), which 
aims at being a simple and maintainable architecture for Haskell applications. Effects are separated from the business logic, and the
entire application is stitched together in a very thin layer. 

### Overview

```tree
             ^  ─── Ogmios.hs
Application  |      ├── Prelude.hs
             v      └── Version.hs

             ^  ─── App
             |      ├── Health.hs
             |      ├── Metrics.hs
             |      ├── Options.hs
             |      ├── Protocol.hs
             |      ├── Protocol
      Logic  |      │   ├── ChainSync.hs
             |      │   ├── StateQuery.hs
             |      │   └── TxSubmission.hs
             |      ├── Server.hs
             |      └── Server
             |          ├── Http.hs
             |          └── WebSocket.hs
             v       
 
             ^  ─── Control
             |      ├── Exception.hs
             |      ├── MonadAsync.hs
             |      ├── MonadClock.hs
    Effects  |      ├── MonadLog.hs
             |      ├── MonadMetrics.hs
             |      ├── MonadOuroboros.hs
             |      ├── MonadSTM.hs
             v      └── MonadWebSocket.hs
 
             ^  ─── Data
             |       ├── Health.hs
             |       ├── Json.hs
             |       ├── Json
             |       │   ├── Prelude.hs
             |       │   ├── Orphans.hs
             |       │   ├── Query.hs
             |       │   ├── Byron.hs
             |       │   ├── Shelley.hs
             |       │   ├── Allegra.hs
             |       │   ├── Mary.hs
       Data  |       │   └── Alonzo.hs
             |       ├── Metrics.hs
             |       ├── Protocol.hs
             |       └── Protocol
             |           ├── ChainSync.hs
             |           ├── StateQuery.hs
             v           └── TxSubmission.hs
```
