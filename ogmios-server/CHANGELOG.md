# Changelog

## [2.0.0-beta] -- 2020-10-31

### Added

- Support for the Shelley chain in the local-chain-sync protocol.
- Support for the local-state-query protocol.
- Health / Heartbeat endpoint for monitoring.
- Runtime and application metrics measured and served on endpoint (`/health`).
- Ogmios now includes an HTTP static server hosting both the WSP definition and, a `/benchmark.html` to run some quick benchmark / smoke test. 
- Added additional configuration options via command-line or environment. 
- Revised user manual with detailed step-by-step examples.

### Changed

- Several JSON fields renamed to increase consistency between Shelley and Byron.
- Improved logging, more messages and with more context.
- Improved error handling with regards to connection of websocket clients.

### Removed

N/A

### Changed

## [1.0.0-beta] -- 2020-04-04

### Added

- Initial release and support for:
  - Chain Synchronization (no pipelining between cardano-node & ogmios)
  - Local Transaction Submission

- JSON-WSP version 1.0, full support with reflection.

- Full docker stack via docker-compose.

- Basic command-line and logging.  

### Changed

N/A

### Removed 

N/A
