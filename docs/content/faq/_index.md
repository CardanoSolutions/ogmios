+++
title = "F.A.Q"
weight = 6
chapter = false
pre = "<b>6. </b>"
+++

#### Can you explain Ogmios in ~~one~~ three sentences?

Ogmios is a protocol translation service written in Haskell running on top of cardano-node. It offers a JSON interface through WebSockets and enables applications to speak Ouroboros' client mini-protocols via remote procedure calls. Ogmios is a fast and lightweight solution that can be deployed alongside relays to create entry points on the Cardano network for various types of applications (e.g. wallets, explorers, chatbots, dashboards…).

#### Can you explain Ogmios to me like I'm five? 

To understand what Ogmios is, you must first understand where it fits in Cardano landscape. Cardano is a network of programs (a.k.a nodes) connected to each other and exchanging messages to run the Cardano blockchain. A Cardano node has an interface that allows for other programs to interact with it (very much like buttons on a remote to control the TV). However, that interface relies on novel communication methods, that were designed in-house by the networking team at IOG. To this day, the only tooling that can implement those unique communication methods is written in Haskell (as if all the buttons on the remote control were in Chinese, but you only speak French). So to interact with a Cardano node, one has no other choice than to write a Haskell program, which is a bummer for many application developers.  

This is where Ogmios comes into play. Ogmios is written in Haskell, so it can speak with Cardano nodes just fine. But it also  translates all the interfaces provided by the node using communication methods that are more common and accessible to the vast majority of developers (namely, WebSockets & JSON). Ogmios is a sort of translator; instead of speaking to a Cardano node directly, applications can speak to Ogmios using a language they know, and Ogmios translates it to the node and back to the applications.

#### Where does the name come from?

Ogmios is a [celtic deity of eloquence](https://en.wikipedia.org/wiki/Ogmios) and language, as well as eloquence and learning. This relates to the way this project helps users communicate with Cardano. And while it doesn’t translate languages, it translates protocols to protocols. 

#### Why do I care? 

Well, it depends. In essence, Ogmios doesn't do much more than what the node itself does. It's pretty much as low-level as things can get with the Cardano network. For many applications, this is too low in the abstraction layer and they would be better off using higher-level services like [cardano-graphql](https://github.com/input-output-hk/cardano-graphql), [Rosetta](https://www.rosetta-api.org/), or [Blockfrost](https://blockfrost.io/). 

However, building such services demands to be able to interact with the blockchain using a more direct interface. This interface can be Ogmios. Currently, the choices given to services like these are limited: talk directly to the node using the Haskell client library, or use cardano-db-sync which is a component that talks to the node and dumps blockchain data in a PostgreSQL database. For those who don't write Haskell, the choice is even more limited; down to a single option. Plus, like any solution, it comes with trade-offs. Deploying a cardano-db-sync instance can be quite heavy, requires extra space, and already forces applications to operate in some specific ways. Ogmios gives a lightweight alternative that is also much closer to what the node offers. It would be possible for example to re-implement cardano-db-sync in a different programming language using Ogmios. 

So in the end, if you're writing a DApp or an application that needs to interact with the Cardano blockchain only at a high level using pre-defined abstractions, then you probably don't care. However, if you're doing some low-level work, and need to access every bit of the protocol or, if you're building a service on top of Cardano for which the blockchain itself is the right level of abstraction, then Ogmios is most likely a good fit for you.

#### Can I build X using Ogmios? 

The short answer is: if you can build X with a Cardano node, then yes. Ogmios is as capable as the client interface for Cardano nodes. Can I build a wallet with Ogmios? Yes. Can I build an explorer with Ogmios? Yes. Can I build a smart-contract application backend with Ogmios? Yes. Anything available on-chain is available through Ogmios which has so far also transitioned through the 4 eras of Cardano. Ogmios' first release was a bit before the Shelley hard-fork, and its development followed the on-chain upgrades and protocol updates. 

#### What's the overhead from running Ogmios? 

Almost none. Ogmios runs within a handful of megabytes of memory and its CPU usage is very much driven by whatever application you'll be connecting to it. That is if your application is syncing the entire blockchain and sending thousands of messages per second to the underlying node, then of course your CPU will get pretty busy; not from Ogmios itself, however, but mostly from the underlying node and your client. In between, Ogmios acts as a bridge and passes messages around. Once a message has been passed, Ogmios forgets about it. That makes the memory footprint of Ogmios quite low, and its resource usages tightly linked to whatever application consumes data from it.  

I secretly keep a hope that someday, many operators will deploy Ogmios alongside their relays. Enabling many application developers to interact with the Cardano blockchain seamlessly by connecting to a relay close to them. 

#### Is there any client for Ogmios?

As a matter of fact, there is. A [TypeScript client library and REPL](https://github.com/KtorZ/cardano-ogmios/tree/master/clients/TypeScript#cardano-ogmios-typescript-client-packages) have been developed by a colleague of mine (thanks again Rhys, you're awesome!). This is huge and comparable to what [web3.js](https://web3js.readthedocs.io/en/v1.3.4/) is for Ethereum. And this is only a beginning. We'll keep working on this to grow the client into a more capable SDK for Cardano. In a future where Ogmios is deployed alongside relays, it could become dead simple to get started with web applications development for Cardano. Simply import the TypeScript client, connect to a relay nearby and start hacking. 

Besides, it goes without saying that as an open-source project Ogmios welcomes contributions; especially on the client library and/or around tools built on top. 

#### Is Ogmios production ready? 

Probably, yes. But I'd like to point that I am obviously biased on this topic. I am myself a professional and seasoned software engineer and I've helped to put various systems in production throughout my career. Ogmios is nevertheless an open-source project I've been working on during my free time, I've got a mad passion for software quality and I have put quite a lot of effort into making Ogmios of the highest quality. 

I've been testing various parts of Ogmios using in particular property testing and generators from the Cardano consensus and ledger codebases. The interfaces are well-documented, and the code itself follows a strict organization while abiding by Haskell battle-tested coding practices and code architecture. The application also comes with logging and metrics for monitoring and has already undergone a few rounds of optimizations. 

Now, this is as good as it gets for an open-source project. I've been incorporating feedback from various users over the past year which has been great so far. For the rest, everything is open-source licensed under [MPL-2.0](https://choosealicense.com/licenses/mpl-2.0/) and you're the best judge.

#### Are there any projects using it?

I've got some positive feedback from a handful of stake pool operators using it for various projects, for example:

- https://gimbalabs.com/dandelionapis/ogmios-api
- https://www.f2lb.org/

There are also projects in preparation which looked into Ogmios as an alternative to cardano-db-sync for it better suited their need. Ogmios only reached a good enough feature-set and stability recently, thus most projects currently using it are still in the hoven. With the Ogmios TypeScript client now available, I have also good hope that more and more projects will see the light. This goes hand-and-hand with adoption from stake pools relays of course: the more relays are avaiable and provide entry points to the network, the more applications can rely on it.  
