# Ogmios (Server)

## Synopsis

TODO

## Architecture

### Overview

```tree
Application  |  ─── Ogmios.hs

             ^  ─── App
             |      ├── Health.hs
             |      ├── Metrics.hs
             |      ├── Options.hs
             |      ├── Protocol
             |      │   ├── ChainSync.hs
             |      │   ├── StateQuery.hs
      Logic  |      │   └── TxSubmission.hs
             |      ├── Server.hs
             |      ├── Server
             |      │   ├── Http.hs
             |      │   └── WebSocket.hs
             v      └── Version.hs
 
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
             |       │   ├── Byron.hs
             |       │   ├── MaryAllegra.hs
       Data  |       │   └── Shelley.hs
             |       ├── Metrics.hs
             |       ├── Protocol.hs
             |       └── Protocol
             |           ├── ChainSync.hs
             |           ├── StateQuery.hs
             v           └── TxSubmission.hs
```

### Implementation Decisions

TODO
