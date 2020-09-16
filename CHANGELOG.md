# Changelog

## [1.0.0] -- Coming Soon

### Added

* 42078df - retry connecting to node only after a short delay (87 seconds ago) <KtorZ>
* e78ae8d - remove documentation about --public-url, now obsolete / removed (4 minutes ago) <KtorZ>
* faadbaa - extend Ogmios default webserver with a /health endpoint (8 minutes ago) <KtorZ>
* 851eb00 - handle connection failure with the node and reply with an appropriate message to clients (6 days ago) <KtorZ>
* fe175f4 - catch linked async exceptions and unknown exceptions. (12 days ago) <KtorZ>
* 35b73ee - also compile source with optimization when the production flag is set (12 days ago) <KtorZ>
* 7563d70 - remove ChainSync trace and add TxSubmission tracers (12 days ago) <KtorZ>
* c7e91ae - sends next batch when half of the previous one has been received (12 days ago) <KtorZ>
* cd662c1 - cleanup NodeToClient implementation, upgrade to V2 and add skeleton for localStateQuery (12 days ago) <KtorZ>
* e65f017 - Implement chain sync pipelining to maximise websocket bandwith (2 weeks ago) <KtorZ>
* 9a56d70 - add header hash and tx id to JSON representations (4 weeks ago) <KtorZ>
* a5f6153 - bump dependencies to 1.10.1 (4 weeks ago) <KtorZ>
* d10198e - adjust docker-compose to pin node's version and bump to 1.10.1 (4 weeks ago) <KtorZ>
* 75b9231 - write and expose simple benchmark script from a running ogmios instance (4 weeks ago) <KtorZ>
* 8b5860f - add support for staging (and custom) network magic (5 weeks ago) <KtorZ>
* 951f73b - make the demo looks good on mobile (5 weeks ago) <KtorZ>
* a767681 - mention the demo on the front README (5 weeks ago) <KtorZ>
* 82ab3dd - add little demo fetching continuously the last block (5 weeks ago) <KtorZ>
* f4d125e - rework specification to reflect more clearly wsp structure (5 weeks ago) <KtorZ>
* 82f4695 - give clearer instruction on the landing page (5 weeks ago) <KtorZ>
* e5a6297 - add simple-client.js example & reference to aws deployment guide in the README (5 weeks ago) <KtorZ>
* c2cdb7f - fix protocol resolution in ogmios landing page (6 weeks ago) <KtorZ>
* b669788 - replace default wai-static landing page with a prettier one :) ... (6 weeks ago) <KtorZ>
* fcf89c1 - document steps for generating TLS with Let's Encrypt for AWS EC2 (6 weeks ago) <KtorZ>
* 4d0921a - deploy to AWS and document the process (6 weeks ago) <KtorZ>
* 727a5cb - revert ouroboros-network submodule to known revision (6 weeks ago) <KtorZ>
* baf7779 - add 'git' to the builder Dockerfile image (6 weeks ago) <KtorZ>
* b7e9bdb - add dockerhub badge and dockerhub README instructions (6 weeks ago) <KtorZ>

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
