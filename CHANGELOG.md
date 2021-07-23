---
title: "Changelog"
weight: 5
chapter: false
pre: "<b>5. </b>"
---

### [4.0.0] - 2021-07-XX

#### Added

- Integrated with the Cardano eco-system corresponding to cardano-node@1.28.0. Bumped the docker-compose installation accordingly.
- New possible errors from the transaction submission:
  - `poolMetadataHashTooBig`
  - `missingRequiredDatums`
  - `unspendableDatums`
  - `unspendableScriptInputsWithoutDatum`

#### Changed

- The `memory` and `steps` JSON representations for `prices` are no longer coins, but ratio (represented as strings in the API).
- `lovelacePerUtxoWord` has been renamed into `coinsPerUtxoWord` in the AlonzoGenesis.

#### Removed

- A previous introduced error from the transaction submission has been removed / replaced:
  - `datumsMismatch`

### [4.0.0-beta.4] - 2021-07-07

#### Added

- Integrated with the Cardano eco-system corresponding to cardano-node@1.27.0. Bumped the docker-compose installation accordingly.

- New possible errors from the transaction submission coming with cardano-node@1.27.0:

  - `mirTransferNotCurrentlyAllowed`
  - `mirNegativeTransferNotCurrentlyAllowed`
  - `mirProducesNegativeUpdate`

  These errors are related to transactions issuing MIR certificates which can only be done by
  genesis delegates. So this change should not impact any 'standard' user.

- New possible errors from the transaction submission coming with the Alonzo era:
  - `unredeemableScripts`
  - `datumsMismatch`
  - `extraDataMismatch`
  - `missingRequiredSignatures`
  - `missingDatumHashesForInputs`
  - `collateralTooSmall`
  - `collateralIsScript`
  - `collateralHasNonAdaAssets`
  - `tooManyCollateralInputs`
  - `executionUnitsTooLarge`
  - `outsideForecast`
  - `validationTagMismatch`
  - `collectErrors`

 - The TypeScript `ChainSyncClient` now implements an in-memory queue to ensure `requestNext`
   responses are processed sequentially when there are async operations in the message handlers.
   This behaviour can be bypassed where sequential processsing is not required, by setting the new
   construction option `sequential` to `false`.

- Nested logs are now also structured, in particular those coming from the Handshake or TxSubmission protocols. Before, any message from these layers where actually plain strings with unprocessable gibberish. Now, the submitted transaction payload is shown encoded as hexadecimals and errors are also serialized to json using the same model / codec as the one used for websockets. The handshake is also more verbose now clearly showing what version is being requested and what the node replies / select. All in all, better logs.

- The Dockerfile now includes a build definition for including `cardano-node` and `ogmios` in
  the same image, which is the default and suggested mode of operation. To build an image
  with only Ogmios, use the build target `ogmios`. Docker Hub now hosts two image
  repositories: `cardanosolutions/cardano-node-ogmios` and `cardanosolutions/ogmios`.
- Docker Hub images are now tagged with `-mainnet` to make all network images consistent. This
  more specific tag is optional, and points to the same build as the defaults.

#### Changed

- The `moveInstantaneousRewards` certificates have a new optional field `value` and not only a `rewards` map as before. When `value` is present, it signifies that rewards are moved to the other pot.
- :warning: **Server Breaking Changes** :warning:

  - Auxiliary data's `scriptPreImages` in Allegra & Mary has been replaced with a field `scripts` which has one field `native`. The value of `native` corresponds to what used to be the value of `scriptPreImages`. In Alonzo, `scripts` may also have another field `plutus` with a serialized Plutus script.

  - Transactions witnesses' `address` has been renamed into `signatures`, and the structure of the object has been changed to be a map from public keys to signatures (instead of an object with two field `key` & `signature`).

  - Transactions witnesses' `script` has been renamed into `scripts`.

  - Transaction submission errors' `networkMismatch` now returns an `invalidEntities` list of object in the form of `{ "type": ..., "entity": }` where `type` is a text tag designating the type of entity for which there is a network identifier mismatch. Values can be `address`, `rewardAccount` and since Alonzo `transactionBody`. The `entity` field contains some details specific to the type of entity. Before, it used to be two distinct fields `invalidAddresses` and `invalidRewardAccounts`.

  - Empty transaction metadata are no longer materialized by an object with two null fields (`{ "hash": null, "body": null }`). Empty transaction metadata are now equal to `null`.

  - `map` metadatum in transactions' metadata are no longer materialized as a list of list of singleton objects: `[[{ "k": ... }, { "v": ... }], ...]` but instead, as a list of object with two fields `k` and `v`: `[{ "k": ..., "v": ...}, ...]`. This was an oversight from the encoder which was never intended to end up that way but happened to slip in because the schema for metadatum was not specified / documented (and therefore, also escaped testing). This is now documented properly.

  - The `TxOut` (and thus Utxo) model definitions have been unified and harmonized across all eras. That is, pre-Mary eras now also wrap Ada values in an object with a field `"coins": ...`. This reduces the discrepancy between eras for there's now a single TxOut representation valid across all eras. Some fields are however optional and only present in some eras (e.g. `datum` starting from Alonzo)

- :warning: **Client/TypeScript Breaking Changes** :warning:

  - Type `DelegationsAndRewards` renamed into `DelegationsAndRewardsByAccounts`
  - Type `DelegationsAndRewards1` renamed into `DelegationsAndRewards`
  - Type `NonMyopicMemberRewards1` renamed into `NonMyopicMemberRewards`
  - Type `TxTooLarge1` renamed into `TxTooLarge`
  - Type `FeeTooSmall1` renamed into `FeeTooSmall`
  - Type `NetworkMismatch1` renamed into `NetworkMismatch`
  - Type `Proposal` renamed into `UpdateProposalShelley`
  - Types `Utxo1`, `Utxo2`, `UtxoMary` have been unified into a single `Utxo` type. Refer to server breaking changes for details.
  - Many types `NullX` merged into a single `Null` type
  - Query types have been renamed from `ledgerTip1` to `GetLedgerTip` and so forth for all queries.

  - `ChainSyncClient` no longer exposes a requestNext function. Instead you must invoke the callback provided as the second argument in each of rollBackward and rollForward handlers.
  - `ChainSyncClient` no longer exposes JSON-WSP reflection as there would be unexpected results given the first n messages would all share the same reflected value.
  - The `ChainSyncClientMessageHandlers` methods now must return a promise.

#### Removed

ø

### [3.2.0] - 2021-05-09

#### Added

- New TypeScript client! The client comes in three packages:
  - An interactive REPL to play with Ogmios using the command-line.
  - A generator to derive TypeScript type definitions from the JSON schema.
  - The actual client library providing nice wrapper around the various protocol, in a typed way.
  The TypeScript client also includes a new battery of automated integration tests against the testnet.
- Support for WebSocket sub-protocols, with currently one support sub-protocol: `ogmios.compact.v1`. When enabled,
  Ogmios will omit fields such as witnesses, proofs and signatures from responses to make responses smaller.
- Provide missing documentation / JSON-schema for:
  - JSON-WSP faults
  - Allegra & Mary `SubmitTx` failures:
    - Allegra:
      - `expiredUtxo` is replaced by `outsideOfValidityInterval`
      - new error `triesToForgeAda`

      Mary:
      - `valueNotConserved.consumed` is now a `Value` (instead of a `DeltaCoin`)
      - `valueNotConserved.produced` is now a `Value` (instead of a `DeltaCoin`)
      - `outputTooSmall` items are now of type `TxOut[Mary]`
      - new error `tooManyAssetsInOutput`
- Continuous integration job checking for code style and lint on the server source code.
- The `/health` endpoint now returns two additional pieces of information:
  - A `networkSynchronization` percentage to indicate how far Ogmios / the node is from the network.
  - A `currentEra` value to indicate the corresponding Cardano era Ogmios / the node is currently running in.
- Nix support for building Ogmios (this also include a `cabal.project` to enable cabal support as well).

#### Changed

- Rework Docker setup to not require an external snapshot image. Everything is now built in a single
  `Dockerfile`, but cache from DockerHub can be leveraged to reduce overall build time when building
  from scratch.
- Fixed typo in the JSON-schema w.r.t to the 'Acquire' request (`points` → `point`), and introduce more automated test
  to catch this kind of errors more easily.

#### Removed

ø

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
