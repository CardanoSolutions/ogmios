---
title: "Changelog"
weight: 6
chapter: false
pre: "<b>6. </b>"
---

### [5.5.7] - 2022-10-27

#### Added

- `delegationAndRewards` and `nonMyopicMemberRewards` queries now both accept credentials in the form of bech32 strings as parameters, with the following expected prefixes and semantic (according to [CIP-0005](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0005#specification):

  - `stake` (resp. `stake_test` on test networks) for stake addresses
  - `stake_vkh` for stake key hash digests
  - `script` for stake script hash digests

  See also [#277](https://github.com/CardanoSolutions/ogmios/issues/277).

- ![TypeScript][] Allow `additionalUtxoSet` to be passed as argument in the repl.

#### Changed

- ![TypeScript][] Fixed `additionalUtxoSet` being ignored in the `TxSubmissionClient` of the TypeScript client.

#### Removed

N/A

### [5.5.6] - 2022-10-21

#### Added

- Prometheus metrics exported at `/metrics` endpoint

- Schema definitions are now included in the [📘 API reference](https://ogmios.dev/api/). This makes it a little easier to find a specific schema without having to drill into a messages definitions.

#### Changed

- Fixed network synchronization reporting `0.99999` even when fully synchronized. There was sometimes a possible discrepancy between the ledger internal clock and Ogmios' clock, causing a few seconds of drift time.

- ![TypeScript][] Fixed a couple of data-types with fields parsed as `number` instead of `bigint`. See [#274](https://github.com/CardanoSolutions/ogmios/issues/274)

  > **Warning** This is technically an internal breaking-change, however it actually comes as a bug fix since this does not change the announced _interface_ in the TypeScript schema (which was correctly indicated 'bigint'). Still, this may cause issues with those using `number` where there will now be `bigint`.

- ![TypeScript][] Fixed browser detection for the `IsomorphicWebSocket` abstraction. See [#273](https://github.com/CardanoSolutions/ogmios/issues/274)

#### Removed

N/A

### [5.5.5] - 2022-08-19

#### Added

- ![TypeScript][] Support for the _TxMonitor_ mini-protocol in the REPL.

- Link to a new Java client for Ogmios.

#### Changed

- ![TypeScript][] Fixed parsing of `ScriptFailures` coming out of the `evaluateTx` command. Before this patch, the client would simply throw `undefined` when such an error was encountered. They are now properly transcribed as `EvaluateTxError`.

#### Removed

- `testnet` has been removed from the target networks by the Docker workflow; which means that until further notice, there will be no more Docker images pushed for testnet. However, support for `preprod` and `preview` environment has been added.

### [5.5.4] - 2022-08-11

#### Added

- Missing JSON specification (and therefore, documentation) for `collectErrors`. See [#244](https://github.com/CardanoSolutions/ogmios/issues/244).

#### Changed

- Slot lengths are now encoded as floating numbers (double precision) instead of integers (still representing a number of seconds). However, to maintain backward-compatibility, integers value are encoded without decimal, as they used to. [#245](https://github.com/CardanoSolutions/ogmios/issues/245)

- ![TypeScript][] Blocks' properties (`header`, `headerHash`, `body`) are no longer marked as _optional_ in the JSON specification (and consequently, in the TypeScript SDK). [#238](https://github.com/CardanoSolutions/ogmios/issues/238)

#### Removed

N/A

---
---

### [5.5.3] - 2022-07-31

#### Added

N/A

#### Changed

- Bumped cardano-node's version (continuous integration & docker image) to 1.35.2.

- ⚠️  Fixed Plutus' data / datum serialization function. See [3f614c3c](https://github.com/CardanoSolutions/ogmios/commit/3f614c3cf15b6e418fd461a34853b750a3408c4f) for details. As a consequence, some datums (either inline or in the witness set) that have been reported in the past (since `v5.5.0`) may have been wrong. Note that the datum hashes were however correct, so it is possible to identify the _"corrupted"_ ones by trying to re-hash (blake2b-256) them and see whether they match their associated hash digest.

- Changed the Docker image tagging's scheme of `cardano-node-ogmios` to now include the cardano-node's version. This allows to more easily bundle more recent version of cardano-node with old versions of Ogmios without the need to make a whole new release. This is in effect from `v5.5.2` and onwards.

  | image               | repository                                                                                      | tags               |
  | ---                 | ---                                                                                             | ---                |
  | cardano-node-ogmios | [cardanosolutions/cardano-node-ogmios](https://hub.docker.com/repository/docker/cardanosolutions/cardano-node-ogmios) | `latest`<br/>`latest-{NETWORK}`<br/>`v*.*.*_{CARDANO_NODE_VERSION}`<br/>`v*.*.*_{CARDANO_NODE_VERSION}-{NETWORK}` |
  | ogmios              | [cardanosolutions/ogmios](https://hub.docker.com/repository/docker/cardanosolutions/ogmios)                           | `latest`<br/>`latest-{NETWORK}`<br/>`v*.*.*`<br/>`v*.*.*-{NETWORK}` |

- Fixed incongruous error message from the command-line when failing to parse protocol parameters from genesis files. The error reporting has been slightly improved to give a more fine-grained error per invalid parameter. See [#242](https://github.com/CardanoSolutions/ogmios/issues/242).

#### Removed

N/A

---
---


### [5.5.2] - 2022-07-11

#### Added

N/A

#### Changed

- Bumped cardano-node's version (continuous integration & docker image) to 1.35.1.

- ![TypeScript][] Remove superfluous string concatenation in `UnknownResultError`'s message. See [#236](https://github.com/CardanoSolutions/ogmios/pull/236).

#### Removed

N/A

---
---

### [5.5.1] - 2022-07-05

#### Added

- ![TypeScript][] New `isBabbageProtocolParameters` helper function, and extended support of the existing ones to Babbage. See [#234](https://github.com/CardanoSolutions/ogmios/pull/234).

#### Changed

- Fixed Health endpoint wrongly reporting 'Alonzo' while in the 'Babbage era'. See [#233](https://github.com/CardanoSolutions/ogmios/issues/233).

#### Removed

N/A

---
---

### [5.5.0] - 2022-06-29

#### Added

- Added Vasil/Babbage support, including:
  - A new block type `babbage` with:
    - New (optional) transaction fields `references`, `collateralReturn`, `totalCollateral`;
    - New (optional) transaction output's fields `datum` and `script`;
  - A new `plutus:v2` script language;
- New transaction error submission failures in the Babbage era:
  - `mirNegativeTransfer`: return when attempting to perform a negative MIR transfer from a reward pot to another;
  - `totalCollateralMismatch`: returned when `totalCollateral` is set but does not match what is actually computed by the ledger (i.e. sum of collateral inputs minus collateral return);
  - `malformedReferenceScripts`: returned when the `script` specified in an output isn't actually a well-formed Plutus script;
  - `malformedScriptWitnesses`, occurs when a script witness specified in the transaction does not properly deserialize to a Plutus script.
- New script evaluation failures in the Babbage era:
  - `corruptCostModelForLanguage`: An artifact from a distant past. This is unused but somehow still present in the ledger internal definitions. Should be removed eventually.
- New server evaluation failures:
  - `NotEnoughSynced`: Happens when attempting to evaluate execution units on a node that isn't enough synchronized. This is, if the node is still in an era prior to Alonzo, evaluation of execution units won't be possible.
  - `CannotCreateEvaluationContext`: Happens when the ledger fails to create an evaluation context from a given transaction. This is mostly due to the transaction being malformed (e.g. wrong redeemer pointer, missing UTxO).

<p align="right">See the <a href="https://ogmios.dev/api">📘 API reference</a> for more details.</p>

#### Changed

- Updated [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) to include the `vasil-dev` network and switch to [cardano-world](https://github.com/input-output-hk/cardano-world) as a source instead of Hydra artifacts -- now being deprecated.

- _Partially fixed_ an issue causing websocket connection to be terminated by the server when p2p is enabled on the underlying node. Ogmios now has a workaround which makes the issue _less likely_, but the real fix belongs in the upstream networking stack. See [#230](https://github.com/CardanoSolutions/ogmios/issues/230), [#208](https://github.com/CardanoSolutions/ogmios/issues/208).

- The `missingRequiredScripts` error now contains an extra field `resolved` that is a map of (pointer → script hash) that have been correctly resolved by said pointers.

- The introduction of the Babbage era comes with some minor (albeit possibly breaking) changes and deprecations:
  - ⚠️  `datums`, `redeemerData` and `plutus:v1` scripts are no longer encoded as `base64` strings, but are encoded as `base16` strings. The data payload remains however identical.
       This change is meant for more compatibility across the API since those data-types can now also be submitted to the server when evaluating execution units for transactions. Using
       `base64` for input data here is a bit awkward since most existing interfaces in the ecosystem favor `base16`;

  - ⚠️ When passing transaction outputs to the server (e.g. when providing an additional UTxO for script evaluation), datum hashes in output must now be specified as `datumHash` (instead of `datum`). However, the server does a best-effort for the sake of backward compatibility and should still work if provided with a valid hash under `datum`. However, after the Vasil hard-fork, it'll be possible to also pass inline-datums using `datum`, while datum hash digest are expected to be specified as `datumHash`. Said differently, existing applications relying on this functionality will keep working without a change on this release, but applications willing to make use of the new inline-datum functionality coming in Vasil must abide by the new notation;

  - ⚠️ Similarly, Alonzo transaction outputs will now contain a `datumHash` field, carrying the datum hash digest. However, they will also contain a `datum` field with the exact same value for backward compatibility reason. In Babbage however, transaction outputs will carry either `datum` or `datumHash` depending on the case; and `datum` will only contain inline datums;

  - ⚠️  The `outputTooSmall` errors from transaction submission will slightly change format for transactions submitted during the Babbage era. Instead of an array of outputs, it is an array of objects with `output` and `minimumRequiredValue` fields;

  - ⚠️  A slightly modified block header: `leaderValue` and `nounce` fields are gone and replaced by a single `inputVrf` field;

  - ⚠️ Few protocol parameters changes:

    - A new protocol parameter `coinsPerUTxOByte` comes to replace `coinsPerUtxoWord` with a slightly different semantic. `coinsPerUTxOByte` is meant to compute the minimum Lovelace requirement on transaction outputs, and is simply a coefficient in a linear function of the serialized (CBOR) output:

      ```
      minUTxOValue(output) =  |serialise(output)| * coinsPerUTxOByte
      ```

    - The `decentralizationParameter` no longer exists.<br/>
      The block production is forever decentralized :tada:!

    - The `extraEntropy` no longer exists.

<p align="right">See the <a href="https://ogmios.dev/api">📘 API reference</a> for more details.</p>

#### Removed

- `UnknownInputs` and `UncomputableSlotArithmetic` errors have been removed from the top-level possible cases of `EvaluationFailure`. Instead, those errors are now comprised in the `CannotCreateEvaluationContext` case.

- The `corruptCostModelForLanguage` error has been removed from the top-level possible cases of `ScriptFailure`. This one was effectively dead-code that couldn't be reached and was there for completeness. The code has now been removed upstream.

---
---

### [5.4.0] - 2022-05-22

#### Added

- ![TypeScript][] TypeScript client for the 'TxMonitor' mini-protocol. Documentation available at https://ogmios.dev/typescript-client/tx-monitor/.

#### Changed

- The server now returns slightly better faults when detecting a misuse of the `TxMonitor` protocol (e.g. when sending a `HasTx` before an `AwaitAcquire`).

- The server now fails with an explicit error when given a `Request` containing a `reflection` field; `reflection` are only used in responses, while requests use `mirror`. See [#217](https://github.com/CardanoSolutions/ogmios/issues/217).

---
---

### [5.3.0] - 2022-05-07

#### Added

- In the Local-Tx-Monitor protocol, `NextTx` can now take an (optional) extra argument `{ "fields": "all" }` to instrument the server in returning not only a transaction id in `NextTxResponse`, but a full transaction object. See [#190](https://github.com/CardanoSolutions/ogmios/issues/190).

- Transaction JSON objects from all eras now contains an extra field `raw`, which represents the raw serialized transaction (CBOR) as a base64-encoded text string. This is the case of the chain-sync protocol, but also for the tx-monitor protocol. The field is however absent in the `ogmios.v1:compact` mode. See [#190](https://github.com/CardanoSolutions/ogmios/issues/190).

- Transaction JSON objects from the **Alonzo** era now contains an extra field `inputSource` which a string set to either `inputs` or `collaterals`. This captures the fact that since the introduction of Plutus scripts in Alonzo, some transactions may be recorded as _failed_ transactions in the ledger. Those transactions do not successfully spend their inputs but instead, consume their collaterals as an input source to compensate block validators for their work.

#### Changed

- The complete [API reference][] for the server is available in a new form at: https://ogmios.dev/api/. This should make the various protocol messages easier to explore and provide a less awkward visualization of the server API than the previous TypeScript documentation. The old TypeScript documentation remains however available at: https://ogmios.dev/typescript/api/.

- Upgrade internal dependencies to `cardano-node@1.34.1`

- Fixed the supervisor script for the `cardano-node-ogmios` Docker image, which would wrongly ignore signals sent from the Docker daemon (e.g. `docker container stop ...`). See [#168](https://github.com/CardanoSolutions/ogmios/issues/168)

- ![TypeScript][] The tx-submission client now only creates a single event listener to interact with the server. This solves the Node.js warning "possible memory leak detected" when firing many submission requests at once (and going beyond the internal default `maxNumberOfListeners` set by node.js on event emitters). See [#197](https://github.com/CardanoSolutions/ogmios/issues/197).

- ![TypeScript][] The options passed to the WebSocket constructors are now ignored on the browser, since they aren't supported and were causing the constructor to "crash". See [#194](https://github.com/CardanoSolutions/ogmios/issues/194).

- ![TypeScript][] ⚠️  Some schema type interface renaming:
    - `Tx` → `TxByron`
    - `BlockBodyShelley` → `TxShelley`
    - `BlockBodyAllegra` → `TxAllegra`
    - `BlockBodyMary` → `TxMary`
    - `BlockBodyAlonzo` → `TxAlonzo`

#### Removed

N/A

---
---

### [5.2.0] - 2022-02-15

#### Added

- Extended the local-tx-submission protocol with a [new `EvaluateTx` query](https://ogmios.dev/mini-protocols/local-tx-submission/#evaluatetx) which evaluates execution units of scripts present in a transaction. This effectively piggybacks on the Alonzo's tools from the cardano-ledger while providing a more user-friendly interface regarding network parameters. The API offers well-detailed errors and an interface similar to the `SubmitTx`. See discussion on [#172](https://github.com/CardanoSolutions/ogmios/issues/172).
>
- New `rewardsProvenance'` query coming as a replacement for the now-deprecated `rewardsProvenance` query. See discussion on [#171](https://github.com/CardanoSolutions/ogmios/issues/171).
>
- ![TypeScript][] Support for the new `evaluateTx` query in the `TxSubmissionClient` & repl.

- ![TypeScript][] Support for the new `rewardsProvenance'` query as `rewardsProvenanceNew` in the `StateQueryClient` & repl.

#### Changed

- Added transaction id as part of the successful response to a `SubmitTx`. While this is technically a breaking-change, it was introduced in a backward-compatible way. Existing applications using the existing `SubmitTx` query will see no change and will keep receiving successes as `"SubmitSuccessful"` text responses. However, queries which pass transactions using the `submit` field (instead of the currently expected `bytes` field) will receive, on success, an augmented response which contains a transaction id `"SubmitSuccessful": { "txId": "..." }`. See discussion on [#174](https://github.com/CardanoSolutions/ogmios/issues/174).
>
- Improved error reporting for the `SubmitTx` protocol which should gives a little clearer errors for ill-formed transactions.
>
- ![TypeScript][] ⚠️  Renamed client's `TxSubmission/errors.ts` into `TxSubmission/submissionErrors.ts`. Similarly, the submission are also now nested under a `submissionErrors` field in the `TxSubmission` top-level object.

#### Removed

N/A

---
---

### [5.1.0] - 2022-01-24

#### Added

- New `LocalTxMonitor` support in Ogmios. See [The user guide](https://ogmios.dev/mini-protocols/local-tx-monitor/) for more details.
>
  ⚠️  This new protocol is **NOT** enabled in `cardano-node@1.33.*`. Until its inclusion in a next release, a custom build of cardano-node is required to include a more recent version of `ouroboros-network` which adds support for that protocol to the Ouroboros' mini-protocols; namely: [`32af9168`](https://github.com/input-output-hk/ouroboros-network/commit/32af9168).
>
  A version of `cardano-node@1.33.0` patched with the necessary commits can be found at [CardanoSolutions/cardano-node@1.33.0+local-tx-monitor](https://github.com/CardanoSolutions/cardano-node/releases/tag/1.33.0+local-tx-monitor).
>
- New fields in the health object:
  - `connectionStatus` → `"connected"` or `"disconnected"`, to reflect status with the node. [#154](https://github.com/CardanoSolutions/ogmios/issues/154)
  - `currentEpoch` → which returns the current known epoch of the linked node [#164](https://github.com/CardanoSolutions/ogmios/issues/164)
  - `slotInEpoch` → which returns the relative number of slots elapsed in the current epoch [#164](https://github.com/CardanoSolutions/ogmios/issues/154)
>
- New `ogmios health-check` command, useful to perform simple health check on a running server. For example, to monitor a container via Docker health check mechanism:
  ```Dockerfile
  HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check
  ```
- Bumped internal dependencies to Cardano's `1.33.*` eco-system.

#### Changed

- `networkSynchronization` and `currentEra` can be `null` when the server isn't connected to a node. [#154](https://github.com/CardanoSolutions/ogmios/issues/154)
- The `Metrics` trace is now correctly tagged with `MetricsRuntimeStatsDisabled`.
- Fixed an issue with the Docker monitoring scripts of cardano-node-ogmios, causing issues on restart. [#159](https://github.com/CardanoSolutions/ogmios/pulls/159)
- ![TypeScript][] Relax upper-bound constraint on required node.js engine. This should make it possible to install the TypeScript packages on more recent versions on node.js than the one specified on the repository.

#### Removed

N/A

---
---

### [5.0.0] - 2021-12-20

#### Added

- New state-query `systemStart` to access the blockchain start time (UTC).

- New state-query `chainTip` to access the blockchain current tip (may slightly differ from the `ledgerTip` which may be behind catching up).
>
- New state-query `blockHeight` to access the blockchain current highest block number (or `"origin"` if the chain is just starting).
>
- New state-query `eraSummaries` to access all era bounds and slotting parameters details, required for proper slot arithmetic.
>
- Log-levels can now be configured per-component. For example, one can decrease the min severity for the health component while keeping the state-query logs at another.
>
- Logs can now be shutdown completely via the special keyword `off`.
>
- Static binaries for Linux are now produced by the Nix build and uploaded as build artifacts for the corresponding Github workflow (i.e. Nix).
>
- ![TypeScript][] Add support for `systemStart`, `chainTip`, `blockHeight` & `eraSummaries` in the `client` and `repl`.

#### Changed

##### 🏢 Server

- ⚠️ `RelativeTime` is no-longer serialised as a string (with `s` as suffix) but, as an integer representing the number of seconds.
>
- ⚠️ Serialised Plutus scripts are now labelled either `plutus:v1` or `plutus:v2` (instead of `plutus`).

- ⚠️ Some breaking changes in the SubmitTx errors returned by the server for the sake of consistency. All submission errors are now returned as singleton objects within an array. The key of each object indicates the type of error and the value gives additional details about the errors. This is also true for era-mismatch errors. Some errors used to be returned as plain strings, they are now wrapped as singleton object with `null` as a value; this is the case for:
  - `invalidMetadata`
  - `mirNegativeTransferNotCurrentlyAllowed`
  - `mirProducesNegativeUpdate`
  - `mirTransferNotCurrentlyAllowed`
  - `missingAtLeastOneInputUtxo`
  - `missingCollateralInputs`
  - `triesToForgeAda`
  - `validationTagMismatch`
  - `wrongCertificateType`
>
- Upgraded internal dependencies to Cardano eco-system 1.31.0

- ⚠️  ![TypeScript][] `Lovelace` is now a native BigInt.
>
- ⚠️  ![TypeScript][] `getServerHealth`'s `connection` argument is now wrapped into an object, mapped to the field `connection`. (see [#135](https://github.com/CardanoSolutions/ogmios/issues/135))

- ⚠️  ![TypeScript][] Replaced schema definitions for `Hash16` and `Hash64` with more precise type definitions. For hashes, definitions now follows a convention `Digest[ALGORITHM]::PRE-IMAGE` where `ALGORITHM` and `PRE-IMAGE` points to the corresponding has algorithm used to hash the `PRE-IMAGE`. The length of the digest is given by `minLength` and `maxLength` JSON-schema constraints. Consequently, TypeScript types / interfaces generated from the JSON-schema definitions have been altered.

#### Removed

- ⚠️ Log level severities `critical`, `alert` and `emergency` have been removed. `error` is now the highest severity.

- ⚠️ The Nix setup has been highly simplified, resulting in removal of the NixOS services configuration and probably some other stuff.

---
---

### [4.2.1] - 2021-11-16

#### Added
>
N / A

#### Changed

- Fixed configuration parsing which would wrongly use Shelley's slots per epoch instead of Byron's. This had an impact on the reported slot number in the chain-sync protocol, where slot numbers would be wrongly offset by `432000` per epoch after the first epoch.

#### Removed

N/A

### [4.2.0] - 2021-11-05

#### Added

- Enabled Alonzo transaction in the submission protocol (oversight from previous releases). Doing so, explicit compiler warnings have been added to the relevant code to avoid overseeing this in the next upgrade.

- Added query response and requests to logs trace, large responses are truncated (e.g. querying the entire UTXO or, informations about all stake pools).
>
- Gracefully handled shutdown and process cleanup on `SIGTERM`.

- Documented Mary last point to the _points of interests_ for the chain-sync protocol.
>
- Bumped cardano-node's integration to 1.31.0

- ![TypeScript][] The State-Query client can now `release` acquired points to perform queries against the most recent tip (which was also the default when creating a client with no point).

#### Changed

- ![TypeScript][] Some internal rework and cleanup; mostly chasing dangling promises by avoiding attaching even handlers when not needed.

#### Removed

N/A

---
---

### [4.1.0] - 2021-09-08

#### Added

- Generate and store [test vectors](https://github.com/CardanoSolutions/ogmios/tree/master/server/test/vectors) for various JSON requests and responses. This should ease integration for many clients who seek for good coverage of the server inputs/outputs.

- Documented [example state queries](https://ogmios.dev/mini-protocols/local-state-query/#example-queries) in the user-guide.

- ![TypeScript][] The `ConnectionConfig` has an additional, optional, configuration parameter `maxPayload` to configure the maximum allowed message size in bytes. The default is chosen quite large as the `utxo` query can result in large payloads.

- ![TypeScript][] New helpers `isByronEpochBoundaryBlock` and `isByronStandardBlock`.

#### Changed

- ![TypeScript][] The `StateQueryClient` now wraps every query in a try/catch to cope with malformed queries leading to client `fault` results from the server.
- ![TypeScript][] Type definitions for `QueryResponse[poolIds]` and `QueryResponse[poolParameters]` are no longer marked as "optional".
- ![TypeScript][] Fixed bug #125 where empty results of `delegationAndRewards` would cause the client to throw an exception.
- ![TypeScript][] Handled some floating promises.

#### Removed

N/A

---
---

### [4.0.0] - 2021-09-06

#### Added

- Integrated with the Cardano eco-system corresponding to [cardano-node@1.29.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.29.0) (Alonzo!) & latest testnet(s).
>
- New `alonzo` block type, with various additions related to Alonzo.
>
- New state-queries:

  Query                        | Description
  ---                          | ---
  `poolIds`                    | The list of all pool identifiers currently registered and active.
  `poolParameters`             | Stake pool parameters submitted with registration certificates.
  `poolsRanking`               | Retrieve stake pools ranking (a.k.a desirabilities).
  `rewardsProvenance`          | Get details about rewards calculation for the ongoing epoch.

- Added missing properties in Byron's protocol parameters update. Somehow, an `additionalProperties: true` had slipped through and caused the tests to pass with an incomplete schema.

- Nested logs are now also structured, in particular those coming from the `Handshake` or `TxSubmission` protocols. Before, any message from these layers where actually plain strings with unintelligible gibberish. Now, the submitted transaction payload is shown encoded as hexadecimals and errors are also serialized to json using the same model / codec as the one used for websockets. The handshake is also more verbose now clearly showing what version is being requested and what the node replies / select. All in all, better logs.

- The Dockerfile now includes a build definition for building `cardano-node` and `ogmios` into the **same image**, which is the default and suggested mode of operation. To build an image with only Ogmios, use the build `--target ogmios`. Docker Hub now hosts two image repositories: `cardanosolutions/cardano-node-ogmios` and `cardanosolutions/ogmios`.

- Docker Hub images are now tagged with a network suffix (e.g. `-mainnet`). In the case of mainnet, the `-mainnet` suffix is optional, and points to the same build as the defaults.

- A new repository for hosting Cardano configurations of various services was created and is now used in Ogmios. Configuration for cardano-node (and therefore Ogmios) or, network genesis can be found in [input-output-hk/cardano-configurations](https://github.com/input-output-hk/cardano-configurations). Configurations are updated automatically by a nightly job to be always up-to-date. They can be pulled into projects as git submodules.

- New possible errors from the transaction submission (stemming from the Alonzo integration):
  - `collateralHasNonAdaAssets`
  - `collateralIsScript`
  - `collateralTooSmall`
  - `collectErrors`
  - `datumsMismatch`
  - `executionUnitsTooLarge`
  - `extraDataMismatch`
  - `extraRedeemers`
  - `mirNegativeTransferNotCurrentlyAllowed`
  - `mirProducesNegativeUpdate`
  - `mirTransferNotCurrentlyAllowed`
  - `missingDatumHashesForInputs`
  - `missingRequiredDatums`
  - `missingRequiredRedeemers`
  - `missingRequiredSignatures`
  - `outsideForecast`
  - `poolMetadataHashTooBig`
  - `tooManyCollateralInputs`
  - `unspendableDatums`
  - `unspendableScriptInputs`
  - `validationTagMismatch`

- ![TypeScript][] Compatibility with aforementioned server additions.

- ![TypeScript][] The `ChainSyncClient` now implements an in-memory queue to ensure `requestNext` responses are processed sequentially when there are async operations in the message handlers.  This behaviour can be bypassed where sequential processsing is not required, by setting the new construction option `sequential` to `false`.

- ![TypeScript][] The `StateQueryClient` can now re-acquire new points at will, useful for long-running clients for which previously acquired points may expire.

- ![TypeScript][] The TypeScript client is now [fully documented](https://ogmios.dev/api/modules/_cardano_ogmios_client.html)!

#### Changed

- ⚠️  The `utxo` query can now accept a list `TxIn` as argument, and still supports list of `Address`. Note that lists can't be heterogeneous and it's not possible to mix `TxIn` and `Address`.

- ⚠️  Asset quantities and transaction metadata's integers are now parsed as native `BigInt`.

- The `memory` and `steps` JSON representations for `prices` are no longer coins, but ratio (represented as strings in the API).

- The `moveInstantaneousRewards` certificates have a new optional field `value` and not only a `rewards` map as before. When `value` is present, it signifies that rewards are moved to the other pot.

- Auxiliary data's `scriptPreImages` in Allegra & Mary has been replaced with a field `scripts` which has one field `native`. The value of `native` corresponds to what used to be the value of `scriptPreImages`. In Alonzo, `scripts` may also have another field `plutus` with a serialized Plutus script.

- Transactions witnesses' `address` has been renamed into `signatures`, and the structure of the object has been changed to be a map from public keys to signatures (instead of an object with two field `key` & `signature`).

- Transactions witnesses' `script` has been renamed into `scripts`.

- Transaction submission errors' `networkMismatch` now returns an `invalidEntities` list of object in the form of `{ "type": ..., "entity": }` where `type` is a text tag designating the type of entity for which there is a network identifier mismatch. Values can be `address`, `rewardAccount` and since Alonzo `transactionBody`. The `entity` field contains some details specific to the type of entity. Before, it used to be two distinct fields `invalidAddresses` and `invalidRewardAccounts`.

- Empty transaction metadata are no longer materialized by an object with two null fields (`{ "hash": null, "body": null }`). Empty transaction metadata are now equal to `null`.

- `map` metadatum in transactions' metadata are no longer materialized as a list of list of singleton objects: `[[{ "k": ... }, { "v": ... }], ...]` but instead, as a list of object with two fields `k` and `v`: `[{ "k": ..., "v": ...}, ...]`. This was an oversight from the encoder which was never intended to end up that way but happened to slip in because the schema for metadatum was not specified / documented (and therefore, also escaped testing). This is now documented properly.

- The `TxOut` (and thus Utxo) model definitions have been unified and harmonized across all eras. That is, pre-Mary eras now also wrap Ada values in an object with a field `"coins": ...`. This reduces the discrepancy between eras for there's now a single TxOut representation valid across all eras. Some fields are however optional and only present in some eras (e.g. `datum` starting from Alonzo)

- ⚠️  ![TypeScript][] State queries (resp. the `StateQueryClient`) now automatically runs queries against the last known tip if no explicit point is provided. It used to acquire a point on the first query which would eventually become too old. The behavior is now equivalent to acquiring a new point on **every** query!

- ⚠️  ![TypeScript][] `SubmitTx` no-longer returns Byron errors. Consequently, submit errors are no longer scoped under `errors.byron` or `errors.shelley` but simply `errors`.

- ⚠️  ![TypeScript][] Fixed `proposedProtocolParameters` query. All fields are actually required AND, more importantly, it can now return either Shelley protocol parameters or, Alonzo protocol parameters.

- ![TypeScript][] The `ChainSyncClientMessageHandlers` methods now must return a promise.

- ![TypeScript][] Various reworks and renaming of the TypeScript types
  - `AssetQuantity` is now a native `bigint`
  - Metadatum's `Int` are now native `bigint`
  - Type `DelegationsAndRewards` renamed into `DelegationsAndRewardsByAccounts`
  - Type `DelegationsAndRewards1` renamed into `DelegationsAndRewards`
  - Type `NonMyopicMemberRewards1` renamed into `NonMyopicMemberRewards`
  - Type `TxTooLarge1` renamed into `TxTooLarge`
  - Type `FeeTooSmall1` renamed into `FeeTooSmall`
  - Type `NetworkMismatch1` renamed into `NetworkMismatch`
  - Type `Proposal` renamed into `UpdateProposalShelley`
  - Types `Utxo1`, `Utxo2`, `UtxoMary` have been unified into a single `Utxo` type. Refer to server breaking changes for details.
  - Type `Tip` & `Point` renamed into `TipOrOrigin` and `PointOrOrigin`. As a consequence, `Tip1` and `Point1` are now simply `Tip` and `Point`.
  - Many types `NullX` merged into a single `Null` type
  - Query types have been renamed from `ledgerTip1` to `GetLedgerTip` and so forth for all queries.

#### Removed

- `datumsMismatch`, a previously introduced error from the transaction submission has been removed / replaced.

- `SubmitTx` can no longer return `SubmitTxError[Byron]`. All the child error types have been removed accordingly, namely:
  - `UtxoValidationError`
  - `TxValidationError`
  - `LovelaceError`

- ![TypeScript][] `ChainSyncClient` no longer exposes a requestNext function. Instead you must invoke the callback provided as the second argument in each of rollBackward and rollForward handlers.

- ![TypeScript][] `ChainSyncClient` no longer exposes JSON-WSP reflection as there would be unexpected results given the first n messages would all share the same reflected value.

---
---

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

---
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
- Customized [API reference][]'s stylesheet to enhance readability.
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

---
---

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

N/A

---
---

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

N/A

#### Changed

N/A

---
---

### [1.0.0-beta] -- 2020-04-04

#### Added

- Initial release and support for:
  - Chain Synchronization (no pipelining between cardano-node & ogmios)
  - Local Transaction Submission

- JSON-WSP version 1.0, full support with reflection.

- Full docker stack via docker-compose.

- Basic command-line and logging.

#### Changed

N/A

#### Removed

N/A

[API reference]: https://ogmios.dev/api/

[TypeScript]: https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Typescript_logo_2020.svg/16px-Typescript_logo_2020.svg.png
