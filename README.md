<p align="center">
<a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Continuous%20Integration?style=for-the-badge&label=&logo=GitHub%20Actions&logoColor=FFFFFF"/></a> <a href="https://hub.docker.com/r/cardanosolutions"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Docker?style=for-the-badge&label=&logo=Docker&logoColor=FFFFFF" /></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Nix"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Nix?style=for-the-badge&label=&logo=NixOS&logoColor=FFFFFF" /></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/User Guide?style=for-the-badge&label=&logo=Hugo&logoColor=FFFFFF"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Network Synchronization"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Network Synchronization?style=for-the-badge&label=&logoColor=FFFFFF&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAWCAYAAADafVyIAAABhWlDQ1BJQ0MgcHJvZmlsZQAAKJF9kT1Iw0AYht+mSv2pdLCDiEOG6mRBqoijVqEIFUKt0KqDyaV/0KQhSXFxFFwLDv4sVh1cnHV1cBUEwR8QNzcnRRcp8bu00CLGg7t7eO97X+6+A4R6mWlW1wSg6baZSsTFTHZVDLyiH70I0RqTmWXMSVISnuPrHj6+30V5lnfdn2NAzVkM8InEs8wwbeIN4ulN2+C8TxxmRVklPiceN+mCxI9cV5r8xrngssAzw2Y6NU8cJhYLHax0MCuaGvEUcUTVdMoXMk1WOW9x1spV1ronf2Ewp68sc53mCBJYxBIkiFBQRQll2IjSrpNiIUXncQ//sOuXyKWQqwRGjgVUoEF2/eB/8Lu3Vn4y1kwKxoHuF8f5GAUCu0Cj5jjfx47TOAH8z8CV3vZX6sDMJ+m1thY5AkLbwMV1W1P2gMsdYOjJkE3Zlfw0hXweeD+jb8oCg7dA31qzb61znD4AaepV8gY4OATGCpS97vHuns6+/VvT6t8PXGZynkY9QMIAAAAGYktHRAAAAAAAAPlDu38AAAAJcEhZcwAADdcAAA3XAUIom3gAAAAHdElNRQflCwkPDArHub8lAAACFUlEQVRIx6WVP2yOURTGf7c1oINEUk0HjVRKVBSlDSryaUqqExZzE0sl4m9FRAwkdgYxde5k8i8iRLSNIHRhkQ5qEEoTg/oi9Gdw37iaV99+n7O853nuuSfvee4950KOqXXqthz+ino5h+9Wl+blWkS+NQBdwJM5/DWgnBO/GXgNzFCJqWEBMbXzrdcUJJ9SBxOuS+1M8AXgA9WaekJdn+DP6mSCN6gDC012tihY7VX3FMQcT6tOJXoKjGfyqE3Rb1DvqqPAe2BaHVNvq/UxpkXNco0Cj4qquehv26ie948NqzcSfFrtiP6lSq7pA2AN8A54nvAvgVrgQMTjwAQwBNzJSxQKKjkCNMeSv4UQ7kd+N7AY6AHehhCuzpekP34b1Xvq2oj3J1LczNl3K1k/GLlW9bG6MuIzNUB9IlddlGDuBcirdDbxfyZcGfgR8VSRREeBVcBDoJxI1AMsAXYBEyGE65U22A51SF2u7k2kOBf7JbPeGDOslioZFfuAfmA10J7wW+NgS3ETcAjoLvrrTrUj+jVqS/RXxKYaU7eobepI5BpiTFM2GNXtandeH5SA78CzEMIs8AYghPAR6Iub+4CvIYSdf931ECYTuAlYFnup8CwG1LYEz6ivEtyqnvyfafopjuQMl9T2BA+q00VvQtWPSXZe1SRuzCtdXZdN2Tn8KbW5kmv6BXiRwx8DDufwI/962X4BWCfNDCIa+lYAAAAASUVORK5CYII="/></a>
<br/>
<img src="server/static/assets/logo.png" height=200 width=256 alt="ogmios" align="center">
</p>

**Ogmios** is a lightweight bridge interface for [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a **WebSocket API** that enables local clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via **JSON/RPC**.

## Features

- Fast synchronization of blocks from the Cardano network(s)
- Transaction submission with enhanced error messages
- Full ledger state query support:
  - `blockHeight`
  - `chainTip`
  - `currentEpoch`
  - `currentProtocolParameters`
  - `delegationsAndRewards`
  - `eraStart`
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
- TypeScript client & REPL
- Structured JSON logging 
- Health monitoring, with runtime and application statistics
- Fully documented API with JSON-schema


## Preview

<p align="center">
  <img src=".github/preview.png" alt="Ogmios TypeScript Client Preview">
</p>

## Sponsors

| [Ray Network][Ray Network] | [ccvault.io][ccvault] | [tapiocapool][tapiocapool] | 
| --- | --- | --- |
| [<img src="https://avatars.githubusercontent.com/u/65092852" height=100 width=100 />][Ray Network] | [<img src="https://avatars.githubusercontent.com/u/86010408" height=100 width=100 />][Ray Network] | [<img src="https://avatars.githubusercontent.com/u/80033713" height=100 width=100 />][tapiocapool] |

[Ray Network]: https://rraayy.com/
[tapiocapool]: https://github.com/tapiocapool
[ccvault]: https://ccvault.io/

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
