<p align="center">
<a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/actions/workflow/status/cardanosolutions/ogmios/continuous-integration.yaml?style=for-the-badge&label=&logo=GitHub%20Actions&logoColor=FFFFFF"/></a> <a href="https://hub.docker.com/r/cardanosolutions"><img src="https://img.shields.io/github/actions/workflow/status/cardanosolutions/ogmios/package.yaml?style=for-the-badge&label=&logo=Docker&logoColor=FFFFFF" /></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/actions/workflow/status/cardanosolutions/ogmios/user-guide.yaml?style=for-the-badge&label=&logo=Hugo&logoColor=FFFFFF"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Network Synchronization"'><img src="https://img.shields.io/github/actions/workflow/status/cardanosolutions/ogmios/network-synchronization.yaml?style=for-the-badge&label=&logoColor=FFFFFF&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABgAAAAWCAYAAADafVyIAAABhWlDQ1BJQ0MgcHJvZmlsZQAAKJF9kT1Iw0AYht+mSv2pdLCDiEOG6mRBqoijVqEIFUKt0KqDyaV/0KQhSXFxFFwLDv4sVh1cnHV1cBUEwR8QNzcnRRcp8bu00CLGg7t7eO97X+6+A4R6mWlW1wSg6baZSsTFTHZVDLyiH70I0RqTmWXMSVISnuPrHj6+30V5lnfdn2NAzVkM8InEs8wwbeIN4ulN2+C8TxxmRVklPiceN+mCxI9cV5r8xrngssAzw2Y6NU8cJhYLHax0MCuaGvEUcUTVdMoXMk1WOW9x1spV1ronf2Ewp68sc53mCBJYxBIkiFBQRQll2IjSrpNiIUXncQ//sOuXyKWQqwRGjgVUoEF2/eB/8Lu3Vn4y1kwKxoHuF8f5GAUCu0Cj5jjfx47TOAH8z8CV3vZX6sDMJ+m1thY5AkLbwMV1W1P2gMsdYOjJkE3Zlfw0hXweeD+jb8oCg7dA31qzb61znD4AaepV8gY4OATGCpS97vHuns6+/VvT6t8PXGZynkY9QMIAAAAGYktHRAAAAAAAAPlDu38AAAAJcEhZcwAADdcAAA3XAUIom3gAAAAHdElNRQflCwkPDArHub8lAAACFUlEQVRIx6WVP2yOURTGf7c1oINEUk0HjVRKVBSlDSryaUqqExZzE0sl4m9FRAwkdgYxde5k8i8iRLSNIHRhkQ5qEEoTg/oi9Gdw37iaV99+n7O853nuuSfvee4950KOqXXqthz+ino5h+9Wl+blWkS+NQBdwJM5/DWgnBO/GXgNzFCJqWEBMbXzrdcUJJ9SBxOuS+1M8AXgA9WaekJdn+DP6mSCN6gDC012tihY7VX3FMQcT6tOJXoKjGfyqE3Rb1DvqqPAe2BaHVNvq/UxpkXNco0Cj4qquehv26ie948NqzcSfFrtiP6lSq7pA2AN8A54nvAvgVrgQMTjwAQwBNzJSxQKKjkCNMeSv4UQ7kd+N7AY6AHehhCuzpekP34b1Xvq2oj3J1LczNl3K1k/GLlW9bG6MuIzNUB9IlddlGDuBcirdDbxfyZcGfgR8VSRREeBVcBDoJxI1AMsAXYBEyGE65U22A51SF2u7k2kOBf7JbPeGDOslioZFfuAfmA10J7wW+NgS3ETcAjoLvrrTrUj+jVqS/RXxKYaU7eobepI5BpiTFM2GNXtandeH5SA78CzEMIs8AYghPAR6Iub+4CvIYSdf931ECYTuAlYFnup8CwG1LYEz6ivEtyqnvyfafopjuQMl9T2BA+q00VvQtWPSXZe1SRuzCtdXZdN2Tn8KbW5kmv6BXiRwx8DDufwI/962X4BWCfNDCIa+lYAAAAASUVORK5CYII="/></a>
<br/>
<img src="server/static/assets/logo.png" height=200 width=256 alt="ogmios" align="center">
</p>

**Ogmios** is a lightweight bridge interface for [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a **WebSocket API** that enables local clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via **JSON/RPC**.

## QuickStart

See our [Ogmios client starter kit](https://github.com/CardanoSolutions/ogmios-ts-client-starter-kit#ogmios-client-starter-kit) or jump right into the [user manual](https://ogmios.dev).

## Features

<table align="center">
  <tr><td>Fast synchronization of blocks from the Cardano network(s)</td><td><a href="https://ogmios.dev/mini-protocols/local-chain-sync/" target="_blank">↗️</a></td></tr>
  <tr><td>Transaction submission with enhanced error messages</td><td><a href="https://ogmios.dev/mini-protocols/local-tx-submission/#submittx" target="_blank">↗️</a></td></tr>
  <tr><td>Evaluation of Plutus script execution units</td><td><a href="https://ogmios.dev/mini-protocols/local-tx-submission/#evaluatetx" target="_blank">↗️</a></td></tr>
  <tr><td>Local mempool monitoring</td><td><a href="https://ogmios.dev/mini-protocols/local-tx-monitor/" target="_blank">↗️</a></td></tr>
  <tr>
  <td>
  <details>
  <summary>Ledger state queries</summary>

  Query                        | Result
  ---                          | ---
  `blockHeight`                | The chain's highest block number.
  `chainTip`                   | The chain's current tip.
  `currentEpoch`               | The current epoch of the ledger.
  `currentProtocolParameters`  | The current protocol parameters.
  `delegationsAndRewards`      | Current delegation settings and rewards of given reward accounts.
  `eraStart`                   | The information regarding the beginning of the current era.
  `eraSummaries`               | Era bounds and slotting parameters details, required for proper slot arithmetic.
  `genesisConfig`              | Get a compact version of the era's genesis configuration.
  `ledgerTip`                  | The most recent block tip known of the ledger.
  `nonMyopicMemberRewards`     | Non-myopic member rewards for each pool. Used in ranking.
  `poolIds`                    | The list of all pool identifiers currently registered and active.
  `poolParameters`             | Stake pool parameters submitted with registration certificates.
  `poolsRanking`               | Retrieve stake pools ranking (a.k.a desirabilities).
  `proposedProtocolParameters` | The last update proposal w.r.t. protocol parameters, if any.
  `rewardsProvenance'`         | Get details about rewards calculation for the ongoing epoch.
  `stakeDistribution`          | Distribution of the stake across all known stake pools.
  `systemStart`                | The chain's start time (UTC).
  `utxo`                       | Current UTXO, possibly filtered by output reference.

  </details>
  </td>
  <td>
    <a href="https://ogmios.dev/mini-protocols/local-state-query/" target="_blank">↗️</a>
  </td>
  </tr>
  <tr><td>Dual WebSocket/HTTP support</td><td><a href="https://ogmios.dev/getting-started/basics" target="_blank">↗️</a></td></tr>
  <tr><td>Structured JSON logging</td><td><a href="https://ogmios.dev/getting-started/monitoring/" target="_blank">↗️</a></td></tr>
  <tr><td>Health monitoring, with runtime and application statistics</td><td><a href="https://ogmios.dev/getting-started/monitoring/" target="_blank">↗️</a></td></tr>
</table>

# Roadmap

### Cutting-edge work

The project keeps a [changelog 💾](./CHANGELOG.md) in which all past changes and _recent-but-not-yet-released_ changes can be seen. Changes in the changelog have been implemented and are available in latest builds from the `master` branch.

### Planned work

The [github issues & milestones 🎯][milestones] list planned tasks that haven't been implemented but have reached enough maturity to be well-defined and scoped. Milestones have end-dates to give a broad estimate of when it is expected.

### Future work

Finally, [discussions 💡][discussions] contains ongoing discussions regarding the future of Ogmios, with design decisions still under consideration.

## Clients / SDKs

Ogmios provides a language-agnostic API which can be implemented using any WebSocket or HTTP client. If you prefer using a SDK in your favorite language, see below the available clients (_maintained by the community_):

<table align="center"><thead><tr>
<td align="center"><img height=72 width=72 src=".github/clients/typescript.png" alt="Logo:TypeScript"><br/><a href="https://ogmios.dev/typescript-client/overview"><strong>TypeScript</strong></a></td>
<td align="center"><img height=72 width=72 src=".github/clients/go.png" alt="Logo:Go"><br/><a href="https://github.com/savaki/ogmigo/#readme"><strong>Go</strong></a></td>
<td align="center"><img height=72 width=72 src=".github/clients/kotlin.png" alt="Logo:Kotlin"><br/><a href="https://github.com/projectNEWM/kogmios#readme"><strong>Kotlin</strong></a></td>
<td align="center"><img height=72 width=72 src=".github/clients/java.png" alt="Logo:Java"><br/><a href="https://github.com/adabox-aio/ogmios-java-client.git#readme"><strong>Java</strong></a></td>
</tr><thead></table>

## Sponsors 💖

<p align="center">
  <a href="https://rraayy.com/"><img src="https://avatars.githubusercontent.com/u/65092852?s=45&v=4" width=45 height=45 /></a>
  <a href="https://sundaeswap.finance/"><img src="https://avatars.githubusercontent.com/u/83610786?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/savaki"><img src="https://avatars.githubusercontent.com/u/108710?s=45&v=4" width=45 height=45 /></a>
  <a href="https://blockfrost.io/"><img src="https://avatars.githubusercontent.com/u/70073210?s=45&v=4" width=45 height=45 /></a>
  <a href="https://jpeg.store/"><img src="https://avatars.githubusercontent.com/u/98781883?s=200&v=4" width=45 height=45 /></a>
  <a href="https://github.com/jacoblambda"><img src="https://avatars.githubusercontent.com/u/9424043?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/minswap"><img src="https://avatars.githubusercontent.com/u/80548193?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/Quantumplation"><img src="https://avatars.githubusercontent.com/u/49870?v=4" width=45 height=45 /></a>
  <a href="https://github.com/codybutz"><img src="https://avatars.githubusercontent.com/u/3670430?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/scarmuega"><img src="https://avatars.githubusercontent.com/u/653886?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/mrbrinker"><img src="https://avatars.githubusercontent.com/u/41247403?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/sacrelege"><img src="https://avatars.githubusercontent.com/u/7289595?v=4" width=45 height=45 /></a>
  <a href="https://ccvault.io/"><img src="https://avatars.githubusercontent.com/u/86010408?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/artemwright"><img src="https://avatars.githubusercontent.com/u/83517471?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/kayandra"><img src="https://avatars.githubusercontent.com/u/5002506?s=45&v=4" width=45 height=45 /></a>
  <a href="https://github.com/tapiocapool"><img src="https://avatars.githubusercontent.com/u/80033713?s=45&v=4" width=45 height=45 /></a>
</p>

<hr/>

<p align="center">
  <a href="https://cardanosolutions.github.io/ogmios">:book: User Manual</a>
  |
  <a href="CONTRIBUTING.md">:triangular_ruler: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
  |
  <a href="https://discord.gg/ZeyDn65t5v"><img src=".github/discord.svg" alt="Discord">Discord (#ogmios)</a>
  |
  <a href="https://twitter.com/_KtorZ_"><img src=".github/twitter.svg" alt="Twitter"> Twitter (@_KtorZ_)</a>
</p>

<p align="center">
  Need more information? Have a look at the <a href="https://ogmios.dev/faq/" alt="F.A.Q">Frequently Asked Questions</a>.
</p>

<p align="center"><a href="https://github.com/cardanosolutions/ogmios/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>

[discussions]: https://github.com/CardanoSolutions/ogmios/discussions/categories/ideas-feature-requests?discussions_q=sort%3Atop+category%3A%22Ideas+%2F+Feature+Requests%22
[milestones]: https://github.com/CardanoSolutions/ogmios/milestones
