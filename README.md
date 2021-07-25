<p align="center">
<a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Continuous Integration"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Continuous Integration?style=for-the-badge&label=build"/></a> <a href="https://hub.docker.com/r/cardanosolutions"><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Docker?style=for-the-badge&label=docker"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"Nix"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/Nix?style=for-the-badge&label=Nix"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/User Guide?style=for-the-badge&label=User guide"/></a> <a href='https://github.com/cardanosolutions/ogmios/actions?query=workflow%3A"User Guide"'><img src="https://img.shields.io/github/workflow/status/cardanosolutions/ogmios/User Guide?style=for-the-badge&label=Nightly"/></a>
  </table>
</p>

<p align="center">
  <img src="server/static/assets/logo.png" height=266 width=341 alt="ogmios" />
</p>

**Ogmios** is a lightweight translation service written in Haskell running on top of [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a JSON interface through WebSockets and enables clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via remote procedure calls over JSON. 

It comes battery-included with monitoring, logging and a high-performance [server](./server#readme). In addition, a [TypeScript client library](./clients/TypeScript#readme) is available and provide a fully static and type-safe interface for Cardano in TypeScript.

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
  <a href="server/CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center"><a href="https://github.com/cardanosolutions/ogmios/blob/master/LICENSE"><img src=".github/license.svg" alt="license=MPL-2.0" /></a></p>
