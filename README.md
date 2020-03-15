<p align="center">
  <a href="https://app.circleci.com/pipelines/github/KtorZ/cardano-ogmios"><img src="https://img.shields.io/circleci/build/github/KtorZ/cardano-ogmios/master?color=%23cc3898&style=for-the-badge&token=544a094c627f593d00aa74b5a099e81b5cc6c66f" /></a>
</p>

<p align="center">
  <img src=".github/ogmios.png" height=400 alt="ogmios" />
</p>

# Overview

**Ogmios** is a translation service written in Haskell running on top of [cardano-node](https://github.com/input-output-hk/cardano-node/). It offers a [JSON-WSP](https://en.wikipedia.org/wiki/JSON-WSP) interface through WebSockets and enables clients to speak [Ouroboros' mini-protocols](https://hydra.iohk.io/build/1070091/download/1/network.pdf#chapter.3) via remote procedure calls over JSON.

```
            CARDANO RELAY                                     OGMIOS SERVER   
                                                                           
                  ◦                                                _=_       
<-------->    ◦   ○   ◦                                          q(-_-)p     
               ○ ◯ ◯ ○                                            _\=/_      
<--------> ◦ ○ ◯     ◯ ○ ◦  <----Mini-Protocols / CBOR---->      /     \     <----WebSocket / JSON---->
               ○ ◯ ◯ ○                                         _(<\   />)_   
<-------->    ◦   ○   ◦                                       (__\_\_/_/__)  
                  ◦
```

Ouroboros' mini-protocols are described as state-machines. **Ogmios** currently supports the following mini protocols:

<details>
  <summary>Chain Synchronisation Protocol</summary>

```
 *-----------*                                              | Clients wishing to synchronise blocks from
 | Intersect |◀══════════════════════════════╗              | the chain can use the Chain Sync protocol.
 *-----------*         FindIntersect         ║              | 
       │                                     ║              | The protocol is stateful, which means that
       │                                *---------*         | each connection between clients and ogmios
       │ Intersect.{Found,NotFound}     |         |         | has a state: a  cursor locating a point on
       └───────────────────────────────╼|         |         | the chain.
                                        |   Idle  |         | 
    ╔═══════════════════════════════════|         |         | Typically, a client will  start by looking
    ║            RequestNext            |         |⇦ START  | for an intersection  between its own local
    ║                                   *---------*         | chain and  the one from the  node / ogmios.
    ▼                                        ╿              |  
 *------*       Roll.{Backward,Forward}      │              | Then, it'll simply request the next action
 | Next |────────────────────────────────────┘              | to take: either rolling forward and adding
 *------*                                                   | new blocks, or rolling backward.
```
</details>

<br/>

<details>

  <summary>Local Transaction Submission Protocol</summary>

```
 *----------*                                                | Transaction submission is pretty simple &
 |   Busy   |◀══════════════════════════════╗                | works by submitting an already serialized 
 *----------*            SubmitTx           ║                | and signed UTxO transaction as one single
      │                                     ║                | message. 
      │                                *---------*           | 
      │                                |         |           | In case of  success, the server returns an
      │                                |         |           | empty  response. Otherwise, it  returns an
      │          SubmitTxResponse      |   Idle  |           | error  with some details  about what  went 
      └───────────────────────────────╼|         |           | wrong.
                                       |         |⇦ START    |
                                       *---------*           | Clients must thereby know how to construct
                                                             | valid transactions.
```
</details>

<p align="right">See also <a href="https://github.com/KtorZ/cardano-ogmios/blob/master/ogmios.wsp.json">ogmios.wsp.json</a> for a full description of the service.</p>

### Why Bother?

Ogmios brings the Ouroboros mini-protocols to the Web, effectively allowing light clients to interact with 
a relay node in a very simple and efficient way. Ogmios is very lightweight too and can be deployed alongside 
relays to create entry points on the Cardano network for various type of applications (e.g. wallets, explorers,
chatbots, dashboards...).

# Getting Started

```
🐳 docker-compose up
```

:point_up: This will run and connect together:

- An OBFT Byron cardano-node, connected to `MainNet`.
- An Ogmios server, listening to `localhost` on port `:1337`.

And here-under are a few JavaScript snippets of client code interacting with a local Ogmios server.

<details>
  <summary><strong>skeleton.js</strong></summary>

  ```js
  const socket = new WebSocket("ws://127.0.0.1:1337");

  const method = {
    FindIntersect: "FindIntersect",
    RequestNext: "RequestNext",
    SubmitTx: "SubmitTx"
  };

  socket.onopen = function (event) {
      socket.wsp(method.FindIntersect, { points: ["origin"] })
  };

  socket.onmessage = function (event) {
    let msg = JSON.parse(event.data);

    switch (msg.methodname) {
      case method.FindIntersect:
        // do something
      break;

      case method.RequestNext:
        // do something
      break;

      case method.SubmitTx: 
        // do something
      break;
    }

    socket.wsp(method.RequestNext);
  };

  // ---------------- See also `WebSocket-polyfill.js`
  ```
</details>

<br/>

<details>
  <summary><strong>WebSocket-polyfill.js</strong></summary>

```js
  /* A simple helper to facilitate JSON-WSP requests through a browser WebSocket. 
  */
  WebSocket.prototype.wsp = function wsp(methodname, args = {}, mirror = null) {
    this.send(JSON.stringify({
      type: "jsonwsp/request",
      version: "1.0",
      methodname,
      args,
      mirror
    }));
  }
```
</details>



<hr/>

<p align="center">
  <a href="https://ktorz.github.io/cardano-ogmios/api-reference">:book: API Reference</a> 
  |
  <a href="docs/COMMAND_LINE.md">:computer: Command-Line Interface</a>
  |
  <a href="CONTRIBUTING.md">:gift: Contributing</a>
  | 
  <a href="CHANGELOG.md">:floppy_disk: Changelog</a>
</p>

<p align="center">
  <a href="https://github.com/KtorZ/cardano-ogmios/blob/master/LICENSE"><img src="https://img.shields.io/github/license/KtorZ/cardano-ogmios.svg?style=for-the-badge"/></a>
</p>
