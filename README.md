<p align="center">
<a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Continuous%20Integration?style=for-the-badge&label=&logo=GitHub%20Actions&logoColor=FFFFFF"/></a> <a href="https://hub.docker.com/r/cardanosolutions"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Package?style=for-the-badge&label=&logo=Docker&logoColor=FFFFFF" /></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/User Guide?style=for-the-badge&label=&logo=Hugo&logoColor=FFFFFF"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Network Synchronization"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Network Synchronization?style=for-the-badge&label=&logoColor=FFFFFF&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAWCAYAAADafVyIAAABhWlDQ1BJQ0MgcHJvZmlsZQAAKJF9kT1Iw0AYht+mSv2pdLCDiEOG6mRBqoijVqEIFUKt0KqDyaV/0KQhSXFxFFwLDv4sVh1cnHV1cBUEwR8QNzcnRRcp8bu00CLGg7t7eO97X+6+A4R6mWlW1wSg6baZSsTFTHZVDLyiH70I0RqTmWXMSVISnuPrHj6+30V5lnfdn2NAzVkM8InEs8wwbeIN4ulN2+C8TxxmRVklPiceN+mCxI9cV5r8xrngssAzw2Y6NU8cJhYLHax0MCuaGvEUcUTVdMoXMk1WOW9x1spV1ronf2Ewp68sc53mCBJYxBIkiFBQRQll2IjSrpNiIUXncQ//sOuXyKWQqwRGjgVUoEF2/eB/8Lu3Vn4y1kwKxoHuF8f5GAUCu0Cj5jjfx47TOAH8z8CV3vZX6sDMJ+m1thY5AkLbwMV1W1P2gMsdYOjJkE3Zlfw0hXweeD+jb8oCg7dA31qzb61znD4AaepV8gY4OATGCpS97vHuns6+/VvT6t8PXGZynkY9QMIAAAAGYktHRAAAAAAAAPlDu38AAAAJcEhZcwAADdcAAA3XAUIom3gAAAAHdElNRQflCwkPDArHub8lAAACFUlEQVRIx6WVP2yOURTGf7c1oINEUk0HjVRKVBSlDSryaUqqExZzE0sl4m9FRAwkdgYxde5k8i8iRLSNIHRhkQ5qEEoTg/oi9Gdw37iaV99+n7O853nuuSfvee4950KOqXXqthz+ino5h+9Wl+blWkS+NQBdwJM5/DWgnBO/GXgNzFCJqWEBMbXzrdcUJJ9SBxOuS+1M8AXgA9WaekJdn+DP6mSCN6gDC012tihY7VX3FMQcT6tOJXoKjGfyqE3Rb1DvqqPAe2BaHVNvq/UxpkXNco0Cj4qquehv26ie948NqzcSfFrtiP6lSq7pA2AN8A54nvAvgVrgQMTjwAQwBNzJSxQKKjkCNMeSv4UQ7kd+N7AY6AHehhCuzpekP34b1Xvq2oj3J1LczNl3K1k/GLlW9bG6MuIzNUB9IlddlGDuBcirdDbxfyZcGfgR8VSRREeBVcBDoJxI1AMsAXYBEyGE65U22A51SF2u7k2kOBf7JbPeGDOslioZFfuAfmA10J7wW+NgS3ETcAjoLvrrTrUj+jVqS/RXxKYaU7eobepI5BpiTFM2GNXtandeH5SA78CzEMIs8AYghPAR6Iub+4CvIYSdf931ECYTuAlYFnup8CwG1LYEz6ivEtyqnvyfafopjuQMl9T2BA+q00VvQtWPSXZe1SRuzCtdXZdN2Tn8KbW5kmv6BXiRwx8DDufwI/962X4BWCfNDCIa+lYAAAAASUVORK5CYII="/></a>
<br/>
<img src="server/static/assets/logo.png" height=200 width=256 alt="ogmios" align="center">
</p>

**Ogmios** is a lightweight bridge interface for [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a **WebSocket API** that enables local clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via **JSON/RPC**.

## Features

- [Fast synchronization of blocks from the Cardano network(s)](https://ogmios.dev/mini-protocols/local-chain-sync/)
- [Transaction submission with enhanced error messages](https://ogmios.dev/mini-protocols/local-tx-submission/#submittx)
- [Evaluation of Plutus script execution units](https://ogmios.dev/mini-protocols/local-tx-submission/#evaluatetx)
- [Local mempool monitoring](https://ogmios.dev/mini-protocols/local-tx-monitor/)
- [Full ledger state query support:](https://ogmios.dev/mini-protocols/local-state-query/)
  - `blockHeight`
  - `chainTip`
  - `currentEpoch`
  - `currentProtocolParameters`
  - `delegationsAndRewards`
  - `eraStart`
  - `eraSummaries`
  - `genesisConfig`
  - `ledgerTip`
  - `nonMyopicMemberRewards`
  - `poolIds`
  - `poolParameters`
  - `poolsRanking`
  - `proposedProtocolParameters`
  - `rewardsProvenance`
  - `stakeDistribution`
  - `systemStart`
  - `utxo`
- [TypeScript client & REPL](https://github.com/CardanoSolutions/ogmios/tree/master/clients/TypeScript#cardano-ogmios-typescript-client-packages)
- [Structured JSON logging](https://ogmios.dev/getting-started/monitoring/)
- [Health monitoring, with runtime and application statistics](https://ogmios.dev/getting-started/monitoring/)
- [Fully documented API with JSON-schema](https://ogmios.dev/api-reference/)

## Preview

<p align="center">
  <img src=".github/preview.png" alt="Ogmios TypeScript Client Preview">
</p>

## Sponsors :heart:

<p align="center">
  <a href="https://rraayy.com/"><img src="https://avatars.githubusercontent.com/u/65092852?s=55&v=4" width=55 height=55 /></a>
  <a href="https://sundaeswap.finance/"><img src="https://avatars.githubusercontent.com/u/83610786?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/savaki"><img src="https://avatars.githubusercontent.com/u/108710?s=55&v=4" width=55 height=55 /></a>
  <a href="https://blockfrost.io/"><img src="https://avatars.githubusercontent.com/u/70073210?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/jacoblambda"><img src="https://avatars.githubusercontent.com/u/9424043?s=55&v=4" width=55 height=55 /></a>
  <a href="https://ccvault.io/"><img src="https://avatars.githubusercontent.com/u/86010408?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/codybutz"><img src="https://avatars.githubusercontent.com/u/3670430?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/scarmuega"><img src="https://avatars.githubusercontent.com/u/653886?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/minswap"><img src="https://avatars.githubusercontent.com/u/80548193?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/mrbrinker"><img src="https://avatars.githubusercontent.com/u/41247403?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/artemwright"><img src="https://avatars.githubusercontent.com/u/83517471?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/kayandra"><img src="https://avatars.githubusercontent.com/u/5002506?s=55&v=4" width=55 height=55 /></a>
  <a href="https://github.com/tapiocapool"><img src="https://avatars.githubusercontent.com/u/80033713?s=55&v=4" width=55 height=55 /></a>
</p>

<hr/>

<p align="center">
  <a href="https://cardanosolutions.github.io/ogmios">:book: User Manual</a>
  |
  <a href="CONTRIBUTING.md">:triangular_ruler: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/ogmios/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>
