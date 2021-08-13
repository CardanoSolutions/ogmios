<p align="center">
<a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Continuous Integration?style=for-the-badge&label=build"/></a> <a href="https://hub.docker.com/r/cardanosolutions"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Docker?style=for-the-badge&label=docker"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Nix"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Nix?style=for-the-badge&label=Nix"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/User Guide?style=for-the-badge&label=User guide"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/User Guide?style=for-the-badge&label=Nightly"/></a>
  </table>
</p>

<p align="center">
  <img src="server/static/assets/logo.png" height=266 width=341 alt="ogmios" />
</p>

### Overview

**Ogmios** is a lightweight bridge interface of [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a WebSockets API that enables local clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via JSON/RPC.

### Features

- Fast synchronization of blocks from the Cardano network
- Full ledger state query support:
  - `currentEpoch`
  - `eraStart`
  - `ledgerTip`
  - `nonMyopicMemberRewards`
  - `delegationsAndRewards`
  - `currentProtocolParameters`
  - `proposedProtocolParameters`
  - `stakeDistribution`
  - `utxo`
  - `genesisConfig`
- Transaction submission with enhanced error messages
- TypeScript client & REPL
- Structured JSON logging 
- Health monitoring (with runtime and application statistics)
- Supports for all Cardano networks (mainnet, testnet, staging...)
- Fully documented API with JSON-schema


### Preview

<p align="center">
  <img src=".github/preview.png" alt="Ogmios TypeScript Client Preview">
</p>

<hr/>

<p align="center">
  <a href="https://cardanosolutions.github.io/ogmios">:book: User Manual</a>
  |
  <a href="CONTRIBUTING.md">:triangular_ruler: Contributing</a>
  |
  <a href="SPONSORS.md">:gift_heart: Sponsors</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/ogmios/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>
