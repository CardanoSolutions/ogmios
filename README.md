<p align="center">
  <img src=".github/ogmios.png" height=400 alt="ogmios" />
</p>

<p align="center">
  <a href='https://github.com/ktorz/cardano-ogmios/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/ktorz/cardano-ogmios/Continuous Integration?style=for-the-badge" /></a> <a href="https://hub.docker.com/r/ktorz/ogmios"><img src="https://img.shields.io/docker/image-size/ktorz/ogmios/latest?color=407088&label=%F0%9F%90%B3%20docker&sort=date&style=for-the-badge" alt="DockerHub"/></a> <a href="https://github.com/KtorZ/cardano-ogmios/blob/master/LICENSE"><img src="https://img.shields.io/github/license/KtorZ/cardano-ogmios.svg?style=for-the-badge"/></a>
</p>

**Ogmios** is a translation service written in Haskell running on top of [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a [JSON-WSP](https://en.wikipedia.org/wiki/JSON-WSP) interface through WebSockets and enables clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via remote procedure calls over JSON. Ogmios is very lightweight too and can be deployed alongside relays to create entry points on the Cardano network for various type of applications (e.g. wallets, explorers, chatbots, dashboards...).

<hr/>

<p align="center">
  <a href="https://ktorz.github.io/cardano-ogmios">:book: User Manual</a>
  |
  <a href="CONTRIBUTING.md">:gift: Contributing</a>
  |
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>
