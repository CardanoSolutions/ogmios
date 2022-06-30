+++
title = "F.A.Q"
weight = 7
chapter = false
pre = "<b>7. </b>"
+++

#### Can you explain Ogmios in ~~one~~ three sentences?


Ogmios is a lightweight bridge interface for [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a WebSockets API that enables local clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via JSON/RPC. Ogmios is a fast and lightweight solution that can be deployed alongside relays to create entry points on the Cardano network for various types of applications (e.g. wallets, explorers, chatbots, dashboards…)

#### Can you explain Ogmios to me like I'm five? 

To understand what Ogmios is, you must first understand where it fits in Cardano landscape. Cardano is a network of programs (a.k.a nodes) connected to each other and exchanging messages to run the Cardano blockchain. A Cardano node has an interface that allows for other programs to interact with it (very much like buttons on a remote to control the TV). However, that interface relies on novel communication methods, that were designed in-house by the networking team at IOG. To this day, the only tooling that fully implement those unique communication methods is written in Haskell<sup>\*</sup> (as if all the buttons on the remote control were in Chinese, but you only speak French). So to interact with a Cardano node, one has no other choice than to write a Haskell program, which is a bummer for many application developers.  

This is where Ogmios comes into play. Ogmios is written in Haskell, so it can speak with Cardano nodes just fine. But it also  translates all the interfaces provided by the node using communication methods that are more common and accessible to the vast majority of developers (namely, WebSockets & JSON). Ogmios is a sort of translator; instead of speaking to a Cardano node directly, applications can speak to Ogmios using a language they know, and Ogmios translates it to the node and back to the applications.

> (\*) Since 2022, [Pallas](https://github.com/txpipe/pallas#readme) now provides most of the primitives also in Rust. 

#### Where does the name come from?

Ogmios is a [celtic deity of eloquence](https://en.wikipedia.org/wiki/Ogmios), language and learning. This relates to the way this project helps users communicate with Cardano. And while it doesn’t translate languages, it translates protocols to protocols. 

#### Why do I care? 

Well, it depends. In essence, Ogmios doesn't do much more than what the node itself does. It's pretty much as low-level as things can get with the Cardano network. For many applications, this is too low in the abstraction layer and they would be better off using higher-level services like [cardano-graphql](https://github.com/input-output-hk/cardano-graphql), [Rosetta](https://www.rosetta-api.org/), or [Blockfrost](https://blockfrost.io/). 

However, building such services demands to be able to interact with the blockchain using a more direct interface. This interface can be Ogmios. Currently, the choices given to services like these are limited: talk directly to the node using the Haskell client library, or use cardano-db-sync which is a component that talks to the node and dumps blockchain data in a PostgreSQL database. For those who don't write Haskell, the choice is even more limited; down to a single option. Plus, like any solution, it comes with trade-offs. Deploying a cardano-db-sync instance can be quite heavy, requires extra space, and already forces applications to operate in some specific ways. Ogmios gives a lightweight alternative that is also much closer to what the node offers. It would be possible for example to re-implement cardano-db-sync in a different programming language using Ogmios. 

So in the end, if you're writing a DApp or an application that needs to interact with the Cardano blockchain only at a high level using pre-defined abstractions, then you probably don't care. However, if you're doing some low-level work, and need to access every bit of the protocol or, if you're building a service on top of Cardano for which the blockchain itself is the right level of abstraction, then Ogmios is most likely a good fit for you.

#### Can I build X using Ogmios? 

The short answer is: if you can build X with a Cardano node, then yes. Ogmios is as capable as the client interface for Cardano nodes. Can I build a wallet with Ogmios? Yes. Can I build an explorer with Ogmios? Yes. Can I build a smart-contract application backend with Ogmios? Yes. Anything available on-chain is available through Ogmios which has so far also transitioned through the 4 eras of Cardano. Ogmios' first release was a bit before the Shelley hard-fork, and its development followed the on-chain upgrades and protocol updates. 

#### What's the overhead from running Ogmios? 

Almost none. Ogmios runs within a handful of megabytes of memory and its CPU usage is very much driven by whatever application you'll be connecting to it. That is if your application is syncing the entire blockchain and sending thousands of messages per second to the underlying node, then of course your CPU will get pretty busy; not from Ogmios itself, however, but mostly from the underlying node and your client. In between, Ogmios acts as a bridge and passes messages around. Once a message has been passed, Ogmios forgets about it. That makes the memory footprint of Ogmios quite low, and its resource usages tightly linked to whatever application consumes data from it.  

We secretly keep a hope that someday, many operators will deploy Ogmios alongside their relays. Enabling many application developers to interact with the Cardano blockchain seamlessly by connecting to a relay close to them. 

#### Is there any client for Ogmios?

As a matter of fact, there is. A [TypeScript client library and REPL](https://github.com/cardanosolutions/ogmios/tree/master/clients/TypeScript#cardano-ogmios-typescript-client-packages) is available on the same repository. This client is also a first-class citizen within this user-guide so make sure to check out the [TypeScript Client](/typescript-client/) section.

Since then, there has been other clients built by the community: 

- [Kogmios (Kotlin)](https://github.com/projectNEWM/kogmios)
- [Ogmigo (Go)](https://github.com/savaki/ogmigo/)


Besides, it goes without saying that as an open-source project Ogmios welcomes contributions; especially on the client library and/or around tools built on top. Should you be working on a new client, let us know, we're happy to help. 

#### Does Ogmios require the PAB (Plutus Application Backend) to work?

No it does not. The PAB is an application framework which provides DApp developers with an extra interface for running smart-contracts on Cardano; Ogmios is not a DApp, nor does it require any DApp functionality. Ogmios does however require a full cardano-node to work for it is merely an interface on top of it. 

#### Can I use Ogmios in a remote setup? 

Yes. The easiest way is probably by using a reverse-proxy like [NGINX](https://www.nginx.com/) to also promote the WebSocket connection to a secure connection. A more interesting question however is: should you? Ogmios is an interface for the so-called **local** client protocols which are, by design, intended to be used in a local setup: where Ogmios and cardano-node are on the same host. It would be ill-advised to expose the server to many clients without any restriction as each client can drain a quite large amount of resources from the local node. This is however totally acceptable in a controlled environment, where for example, your own stack would leverage a single Ogmios instance to power few remote services.

#### Why do Ogmios returns JSON with integers larger than `MAX_SAFE_INTEGER`?

JSON does support large integers by design. The default JavaScript JSON parser does not however. Thus, it is not a problem which lies on the server-side, but rather on "naive" JavaScript clients. For more details, have a look at this [architectural decision record](https://github.com/CardanoSolutions/ogmios/blob/master/architectural-decisions/accepted/typescript-client-bigint-parsing.md) which explains how we handle large integers in the TypeScript client. 

#### Is Ogmios production ready? 

Ogmios is an open-source project which is being worked on in small steps when time allows. Its development started in 2020 and it has undergone several updates and iterations. We've got a mad passion for software quality and we put extensive efforts into making Ogmios of the highest quality. The server follows a well-known architecture and abides by battle-tested Haskell coding practices. As it should, Ogmios is of course deeply tested at several levels via **continuous integration**<sup>[1](https://github.com/CardanoSolutions/ogmios/actions)</sup>.

Tests for the server include **property-based testing** of the core protocols<sup>[2](https://github.com/CardanoSolutions/ogmios/blob/master/server/test/unit/Ogmios/App/Protocol/StateQuerySpec.hs)</sup> <sup>[3](https://github.com/CardanoSolutions/ogmios/blob/master/server/test/unit/Ogmios/App/Protocol/StateQuerySpec.hs)</sup>, property-based testing of the entire JSON interface validated against a JSON-schema specification<sup>[4](https://github.com/CardanoSolutions/ogmios/blob/master/server/test/unit/Ogmios/Data/JsonSpec.hs)</sup>. Note that property tests all use generators which comes directly from the [ouroboros-network](https://github.com/input-output-hk/ouroboros-network/) and [cardano-ledger-specs](https://github.com/input-output-hk/cardano-ledger-specs) to ensure that Ogmios is **always up-to-date** with the Cardano eco-system. There are also various<sup>[5](https://github.com/CardanoSolutions/ogmios/blob/master/server/test/unit/Ogmios/App/OptionsSpec.hs)</sup> unit<sup>[6](https://github.com/CardanoSolutions/ogmios/blob/master/server/test/unit/Ogmios/Data/MetricsSpec.hs)</sup> tests<sup>[7](https://github.com/CardanoSolutions/ogmios/blob/master/server/test/unit/Ogmios/Data/HealthSpec.hs)</sup> to cover basic functionalities. 

On the other hand, the TypeScript client is used to perform **end-to-end tests** with tests running against the Cardano testnet<sup>[8](https://github.com/CardanoSolutions/ogmios/tree/master/clients/TypeScript/packages/client/test)</sup>. The tests are executed both in a Node.js and browser context and the synchronization with the network is done via a [Github action which leverages Ogmios' server](https://github.com/CardanoSolutions/gh-action-cardano-node-ogmios-docker-sync).

Beside, Ogmios also comes with **structured logging and monitoring** out of the box. Putting any monitoring solution on top like Prometheus is trivial. 

Finally, if you ventured through this page and user-guide, you have also noticed that the project is **well-documented**. And this includes the [API reference](/api), the [ChangeLog](/changelog) as well as the [architectural decisions](https://github.com/CardanoSolutions/ogmios/tree/master/architectural-decisions) going over rationales for decisions we made along the way. 

Thus, is Ogmios production-ready? **Yes**. At least, this is as good as it gets for an open-source project. We've been incorporating feedback from various users over the past year which has been great so far. For the rest, everything is open-source licensed under [MPL-2.0](https://choosealicense.com/licenses/mpl-2.0/) and **you're the best judge.**

#### Are there any projects/companies using it?

We've heard of a handful happy users! And it keeps growing! To name a few...:

- https://blockfrost.io/
- https://eternl.io/
- https://www.sundaeswap.finance/
- https://jpeg.store/ 
- https://spacebudz.io/
- https://www.tangocrypto.com/
- https://projectnewm.io/
- https://github.com/input-output-hk/cardano-graphql 
- https://mlabs.city/
- https://gimbalabs.com/
- https://www.f2lb.org/

{{% notice note %}}
Are you using Ogmios for a project? [Let us know on Github!](https://github.com/CardanoSolutions/ogmios/issues/new?assignees=&labels=&template=project.md)
{{% /notice %}}
