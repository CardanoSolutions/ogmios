+++
title = "Overview"
+++

## Overview

[**Ogmios**](https://github.com/KtorZ/cardano-ogmios) is a translation service written in Haskell running on top of [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a [JSON-WSP](https://en.wikipedia.org/wiki/JSON-WSP) interface through WebSockets and enables clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via remote procedure calls over JSON. Ogmios is very lightweight too and can be deployed alongside relays to create entry points on the Cardano network for various type of applications (e.g. wallets, explorers, chatbots, dashboards...).

## Motivation

All programs offer more or less elaborated interfaces for either user or program interactions. That is, most user-facing programs include a graphical user interface (a.k.a GUI) where users can send signals via buttons and switches, and receive visual or auditive feedback from the program. Some programs are also meant to be used by other programs and are typically called _services_; such services offer application programming interfaces in various forms to enable other programs (or services) to interact. The nodes from Cardano belong to the second category, yet the offer a rather exotic interface: ad-hoc protocols designed in-house by IOHK to be extremely efficient and secure. 

In order to deal with a Cardano node, one has therefore to speak one of these so-called _Ouroboros mini-protocols_. This can be an impediment to mass adoption for the only existing implementation of such protocols is written in _Haskell_. Ogmios is an attempt to lower down that entry barrier by translating the interface to a set of technology that is well-known on the Web. In essence, it is very much like putting a power socket adaptor on the wall in order to branch a device; Cardano node is the device, Ogmios is the socket adaptor. 

Ogmios is thereby very lightweight and doesn't do anything on its own. It is running next to a Cardano node and act as an intermediary for another client application. Ogmios emulates the Ouroboros mini-protocols through WebSockets and translates the binary on-chain data into JSON; both WebSockets and JSON are widespread over the Web and in many other applications so much that they offer the ideal mainstream interface that I hope will help getting more and more application developers to build with Cardano!

## About me

My name is Matthias (a.k.a [KtorZ](https://github.com/KtorZ)) and I currently work with the engineering teams of IOHK to build Cardano. My main area of focus within the Cardano project is basically anything that deals with a Cardano node, which means a lot of things! From a very young age, I've been enjoying designing and writing programs. Ogmios is yet another instance of this which I started as a proof-of-concept on my free-time, and rapidly became one of my most sophisticated side-project. 
