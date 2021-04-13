+++
title = "Overview"
+++

## Overview

[**Ogmios**](https://github.com/KtorZ/cardano-ogmios) is a protocol translation service written in Haskell running on top of [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a [JSON-WSP](https://en.wikipedia.org/wiki/JSON-WSP) interface through WebSockets and enables applications to speak [Ouroboros' client mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via remote procedure calls over JSON. Ogmios is very lightweight too and can be deployed alongside relays to create entry points on the Cardano network for various types of applications (e.g. wallets, explorers, chatbots, dashboards...).

## Motivation

All programs offer more or less elaborated interfaces for either user or program interactions. That is, most user-facing programs include a graphical user interface (a.k.a GUI) where users can send signals via buttons and switches, and receive visual or auditive feedback from the program. Some programs are also meant to be used by other programs and are typically called _services_; such services offer application programming interfaces (a.k.a API) in various forms to enable other programs (or services) to interact. The nodes from Cardano belong to the second category, yet they offer a rather exotic interface: ad-hoc protocols designed in-house by IOHK to be extremely efficient and secure. 

In order to interact with a Cardano node, one must therefore speak one of these so-called _Ouroboros mini-protocols_. This is an impediment to mass adoption for the only existing implementation of those protocols is written in _Haskell_ (and therefore, only available to Haskell applications). Ogmios is an attempt to lower down that entry barrier by translating the interface with a set of technologies that is well-known on the Web. In essence, it is very much like putting a power socket adaptor on the wall in order to branch a device with a peculiar plug; Cardano node is the device, Ogmios is the socket adaptor. 

Ogmios is very lightweight and doesn't do much on its own. It is running next to a Cardano node and it acts as an intermediary for other client applications. Ogmios emulates the Ouroboros mini-protocols through WebSockets and translates the binary on-chain data from and to JSON; both WebSockets and JSON are widespread over the Web and in many other applications so much that they offer the ideal mainstream interface that I hope will help getting more and more application developers to build with Cardano!
