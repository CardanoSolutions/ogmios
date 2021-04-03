---
title: "Changelog"
weight: 5
chapter: false
pre: "<b>5. </b>"
---

### [3.1.0] - 2021-04-04

#### Added

- Extend the local-state query protocol with support for 'GetCompactGenesis'.
- Extend the local-state query protocol with support for 'GetFilteredDelegationsAndRewards'.
- Add missing `mint` field to transaction's body (added since mary).
- The documentation is now hosted on https://ogmios.dev.

#### Changed

- Use 'contentEncoding' over 'format' in appropriate part of the JSON schema.
- Fix various errors in the JSON-schema definition & extend test suite coverage in consequence.
- Implement a 'fast-bech32' encoding library, to speed-up Ogmios serialization of blocks beyond the Shelley era.
- Use faster (and recommended) JSON encoding techniques to speed up overall JSON serialization. 
- Improve generated documentation from JSON schema by:
  - Providing titles to 'oneOf' items
  - Adding descriptions to top-level definitions
  - Adding examples to top-level definitions
- Customized API reference's stylesheet to enhance readability.
- Upgrade dependency and code to work with GHC-8.10.4 (from GHC 8.6.5)
- Handle more gracefully unknown exceptions (avoid infinite fast loop of retries on errors).
- Handle more gracefully network mismatches (e.g. connecting Ogmios in testnet mode to a mainnet network)
- Repository reorganization:
  - 'ogmios-server' renamed into 'server'
  - move Haskell-specific dotfiles and configuration files under 'server'
  - move 'modules' under 'server'
  - move 'Dockerfile' and 'snapshot.Dockerfile' under 'server'

#### Removed

- Support for GHC-8.6.5
- The docker image no longer shows git revision / version on '--version'

### [3.0.0] -- 2021-02-26

#### Added

- Support for the Allegra era on the chain-sync, tx submission and state query protocols.
- Support for the Mary era on the chain-sync, tx submission and state query protocols.
- Support for multi-era state queries, or said differently, Ogmios can survive a hard-fork without being restarted or re-compiled. 
- Allow clients to also make state queries based on the node's tip (instead of passing an explicit point to acquire).
- Interactive dashboard leveraging Ogmios health's endpoint and local state query protocol to show metrics in real-time.
- Automated smoke sanity tests executed on a running instance, running queries and chain-syncs across all eras.
- Various internal optimization, in particular with rewards to the chain-sync protocol (~14.000 blocks/s in Byron, ~2500 block/s in Shelley and beyond).
- Additional metrics for monitoring: current heap size, total messages, total unrouted messages and start time.  
- Configurable HTTP server timeout from the command-line, with sensible defaults.

#### Changed

- Improve error responses to invalid clients' requests (instead of generic error messages).
- Fixed various typos and clumsy wording in the user manual.
- Reworked internal architecture as a Three-Layer Haskell Cake.
- Changed internal dependencies for base16 and base64 encoding for better performances.
- Upgraded internal dependencies to the Cardano eco-system working with cardano-node@1.25.1
- Improved error handling of the Ogmios server, in particular in case of connections lost with the underlying node.
- The server now returns an explicit client error when interleaving 'FindIntersect' messages in-between pipelined 'RequestNext'.
- Revised default compilation flags .

#### Removed

ø

### [2.0.0-beta] -- 2020-10-31

#### Added

- Support for the Shelley chain in the local-chain-sync protocol.
- Support for the local-state-query protocol.
- Health / Heartbeat endpoint for monitoring.
- Runtime and application metrics measured and served on endpoint (`/health`).
- Ogmios now includes an HTTP static server hosting both the WSP definition and, a `/benchmark.html` to run some quick benchmark / smoke test. 
- Added additional configuration options via command-line or environment. 
- Revised user manual with detailed step-by-step examples.

#### Changed

- Several JSON fields renamed to increase consistency between Shelley and Byron.
- Improved logging, more messages and with more context.
- Improved error handling with regards to connection of websocket clients.

#### Removed

ø

#### Changed

### [1.0.0-beta] -- 2020-04-04

#### Added

- Initial release and support for:
  - Chain Synchronization (no pipelining between cardano-node & ogmios)
  - Local Transaction Submission

- JSON-WSP version 1.0, full support with reflection.

- Full docker stack via docker-compose.

- Basic command-line and logging.  

#### Changed

ø

#### Removed 

ø
