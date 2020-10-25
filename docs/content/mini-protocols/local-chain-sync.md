+++
title = "Local Chain Sync"
chapter = false
weight = 1
+++


## Overview

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


Clients that wish to synchronise blocks from the Cardano chain can use the Local Chain Sync protocol.

The protocol is stateful, which means that each connection between clients and Ogmios has a state: a  cursor locating a point on the chain. Typically, a client will  start by looking for an intersection between its own local chain and the one from the node / Ogmios. Then, it'll simply request the next action to take: either rolling forward and adding new blocks, or rolling backward.


🚧 Coming Soon: JavaScript Examples with Ogmios 🚧
