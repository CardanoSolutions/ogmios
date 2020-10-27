+++
title = "Local Chain Sync"
chapter = false
weight = 1
+++


{{% ascii-drawing %}}
*-----------*                                              
| Intersect |◀══════════════════════════════╗              
*-----------*         FindIntersect         ║              
      │                                     ║              
      │                                *---------*         
      │ Intersect.{Found,NotFound}     |         |         
      └───────────────────────────────╼|         |         
                                       |   Idle  |         
   ╔═══════════════════════════════════|         |         
   ║            RequestNext            |         |⇦ START  
   ║                                   *---------*         
   ▼                                        ╿              
*------*       Roll.{Backward,Forward}      │              
| Next |────────────────────────────────────┘              
*------*                                                   
{{% /ascii-drawing %}}

## Overview

Clients that wish to synchronise blocks from the Cardano chain can use the Local Chain Sync protocol.

The protocol is stateful, which means that each connection between clients and Ogmios has a state: a  cursor locating a point on the chain. Typically, a client will  start by looking for an intersection between its own local chain and the one from the node / Ogmios. Then, it'll simply request the next action to take: either rolling forward and adding new blocks, or rolling backward.

## How To Use

When you open a connection with Ogmios, you automatically begin a local chain-sync session with the underlying cardano-node; This starts in the `Idle` state. There's an implicit state maintained by the node which you can imagine as a cursor, pointing to a point on the Cardano chain. Initially, this cursor starts at a special point called: _origin_. After each request, the node will move the cursor according to the request and remembers its location for the next request. From there, the protocol is in fact quite small and gives you only two choices: 

- Ask the node for the "next" block, where "next" implicitly refers to point after the cursor. 
- Ask the node to set the cursor somewhere else, i.e. find a new intersection.

```none
┌────┬────┬────┬────┬────┬────┬────┬────┬─  
│ 00 │ 01 │ 02 │ 03 │ 04 │ 05 │ 06 │ 07 │ .. Cardano chain
└────┴────┴────┴────┴────┴────┴────┴────┴─  
  ^
  |
origin
```

## RequestNext

To request the next block, send a message with `RequestNext` as a method name. This request does not accept any argument. 

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "RequestNext",
    "args": {}
}
```

As a response, Ogmios will send you back a response which can be either a `RollForward` or a `RollBackward`. Rolling forward is pretty straightforward and is the main type of response you can expect; such response will include the next block, which itself includes a header, transactions, certificates, metadata and all sort of information. 

Rolling backward however may occurs when, since your last request, the underlying node decided to switch to a different fork of the chain to the extent that your cursor is longer pointing to a block that exists on this fork. The node therefore asks you (kindly) to roll backward to previously known point, that is the earliest ancestor that is common between your version of the chain and the one that it just adopted. 

```none                            
                              * -- * -- * -- * (node's chain)
                             /
                            *
                           /
* -- * -- * -- * -- * -- * -- * -- * -- * -- * (local chain)
                         ^
                         |
                         |

                  Point of rollback     
```

When rolling backward, the node will not provide a block but instead, a point which is made of a block header hash and a slot. 

{{% notice info %}} 
As a client, it is therefore crucial to be able to rollback to a previous point of the chain. In practice, Ouroboros guarantees that forks cannot be longer than a certain length. This maximum length is called `k` in the Ouroboros protocol, and also known as _the security parameter_.
{{% /notice %}}

## FindIntersect

On your first connection with the node, you may probably want to synchronize from the _origin_. Yet, on subsequent connections you may want to resume syncing to a point that is much more recent than the _origin_. The local chain-sync protocol lets you do that via the `FindIntersect` message. This message accepts one argument which is a list of header hashes (or the special keyword `"origin"`). Those header hashes identify blocks that are known by your client that the node will compare with its own chain in order to find a common intersection point. 


```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "FindIntersect",
    "args": { 
        "points": [ 
            "9e871633f7aa356ef11cdcabb6fdd6d8f4b00bc919c57aed71a91af8f86df590",
            "d184f428159290bf3558b4d1d139e6a07ec6589738c28a0925a7ab776bde4d62",
            "origin" 
        ]
    },
}
```

If an intersection is found, great, the node will set the cursor to that point and let you know. If not, the cursor will remain where it was and you'll be be notified. As we've seen in the previous section, a node may switch to forks based on different strategies. The more points you give, the better. In practice, you may want to give many recent points and a few more that are more sparse and go back at least `k` blocks in the past (your past). 

For example, imagine the following scenario:

```none
               P01  P02  P03  P04  P05    P98  P99A  P100A
Local chain:    * -- * -- * -- * -- *  ... * -- * -- *  

               P01  P02  P03  P04  P05    P98  P99B  P100B
Node's chain:   * -- * -- * -- * -- *  ... * -- * --  *  
```

As a client, providing any point before or at `P98` will result in finding an intersection. Yet, if you only provide `[P99A, P100A]`, the node will not be able to figure out where to continue the protocol and will remain at the _origin_.

{{% notice tip %}}
The order of the list matters! The node will intersect with the best match, considering that your preferred points are first in the list. If you provide `origin` as a first point, you are guaranteed to always find a match, and always at origin (and that is quite useless!).
{{% /notice %}}

## Full Example

Let's see a full example that is synchronizing the first 14 blocks of the **Shelley** chain and printing them to the console.

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function findIntersect(points) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname: "FindIntersect",
        args: { points }
    }));
}

function requestNext(mirror) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname: "RequestNext",
        mirror,
    }));
}

client.once('open', () => {
    const lastByronBlock = {
        slot: 4492799,
        hash: "f8084c61b6a238acec985b59310b6ecec49c0ab8352249afd7268da5cff2a457"
    };
    findIntersect([lastByronBlock]);
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);

    switch (response.methodname) {
        case "FindIntersect":
            if (!response.result.IntersectionFound) { throw "Whoops? Last Byron block disappeared?" }
            requestNext({ n: 14 });
            break;


        case "RequestNext":
            if (response.result.RollForward) {
                console.log(response.result);
            }

            if (response.reflection.n > 0) {
                requestNext({ n: response.reflection.n - 1 });
            } else {
                client.close();
            }
            break;
    }
});
```

A few important takes from this excerpt: 

- The node continues to stream blocks from intersection point. So if we want the first 14 Shelley blocks, we need to set the intersection at the last Byron block! 

- After successfully finding an intersection, the node will **always** ask us to roll backward to that intersection point. This is because it is possible to provide many points when looking for intersection and the protocol makes sure that both the node and the client are in sync. 

- In this schema, we are sending each request one-by-one, using the `mirror` field as counter. An alternative could have been:

  ```js
  case "FindIntersect":
      if (!response.result.IntersectionFound) { throw "Whoops? First Shelley block disappeared?" }
      for (let i = 14; i > 0; i += 1) {
          requestNext()
      }
      break;
  ```

  We need not to wait for replies to send requests and can collect all responses at a later stage. 
