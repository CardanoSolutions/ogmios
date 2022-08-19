+++
title = "Local State Query"
chapter = false
weight = 2
+++


{{% ascii-drawing-split %}}
                ┌───────────────┐
        ┌──────▶│     Idle      │⇦ START
        │       └───┬───────────┘
        │           │       ▲ 
        │   Acquire │       │ Failure 
        │           ▼       │
        │       ┌───────────┴───┐                   
Release │       │   Acquiring   │◀─────────────────┐
        │       └───┬───────────┘                  │
        │           │       ▲                      │ Result 
        │  Acquired │       │ ReAcquire            │
        │           ▼       │                      │
        │       ┌───────────┴───┐         ┌────────┴───────┐
        └───────┤   Acquired    │────────▶│    Querying    │
                └───────────────┘  Query  └────────────────┘
{{% /ascii-drawing-split %}}

## Overview

The state query protocol is likely the most versatile of the three Ouroboros mini-protocols. As a matter of fact, it allows for querying various types of information directly from the ledger. In essence, it is like a very simpler request/response pattern where the types of questions one can ask are specified by the protocols. Those questions include: information about the chain tip, information about stake pools but also the balance of a particular address. 

In order to run a question by the ledger, one must first acquire a particular position on the chain, so that the node can reliably answer a few questions on a chosen, frozen state while continuing maintaining more recent version of the ledger on the side. It is important to note that:

1. The node cannot acquire any arbitrary state. One can only rewind up to a certain point.

2. Should a client keep a state acquired for too long, it is likely to become unreachable at some point, forcing clients to re-acquire.

## How To Use

Ogmios uses a simplified version of the above state-machine. Or more exactly, it exposes a simplified version and handles some of the complexity behind the scene for you. As clients, Ogmios will give you 3 possible requests: Acquire, Query, Release. A typical sequence would be to start by Acquiring a state on a given point and then make a few queries, and then release. The release step is optional although it is a bit more polite to say goodbye at the end of a conversation. 

It is also possible to submit queries directly without acquiring. As a consequence, Ogmios will acquire the tip of the chain, run the query and release it for you. This is the easiest way to send 
queries if you don't care about capturing a particular state. Note however that this may create race conditions if you send multiple queries via this method. Indeed, the tip is changing quite often on the network, and two subsequent queries may actually run on two different points of the chain. While this is generally safe for most queries, it may also put your application in an unexpected state when crossing epoch boundaries or hard-forks. 

## Acquire

The `Acquire` request expect one argument named `point`. The point has the same format as points in the local-chain-sync protocol. That is, they can be block header hashes or the special keyword `"origin"` (though there's very little chance that one will be able to acquire the origin!).

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "Acquire",
    "args": { "point": "9e871633f7aa356ef11cdcabb6fdd6d8f4b00bc919c57aed71a91af8f86df590" }
}
```

One thing that doesn't strike as obvious is that, as clients, you need points to query any information. There are many ways to get those hashes but in the context of Ogmios, the most logical way is via the [local-chain-sync](../local-chain-sync/) protocol. 

{{% notice tip %}}
You can acquire multiple times, the last one will prevail. If you need to re-acquire, simply send another `Acquire` request.
{{% /notice %}}

## Query

There are many queries that can be sent to the ledger, and the list is growing days after days as the Cardano team implements new ones. With Ogmios, all queries follow the same pattern and use the method name `Query`. All of them also take one argument named `query` which specifies the query to run and, optionally some extra argument given to the query. For example:

```
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "Query",
    "args": { "query": "ledgerTip" }
}
```

At the moment of writing this guide, the following queries are available: 

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

{{% notice warning %}}
Deprecated queries. Queries or functionalities listed below will be removed in the next **major** release of Ogmios.

Query               | Notice | Deprecated Since
---                 | --- | ---
`rewardsProvenance` | Supports for this query is not longer guaranteed. The `pools` field in the result is no longer populated. Use `rewardsProvenance'` instead | [cardano-node@1.33.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.33.0)
`utxo`              | Filtering UTXO **by address** is no longer recommended with the introduction of on-disk ledger state storage. Filtering by output reference is preferred. | [cardano-node@1.33.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.33.0)
{{% /notice %}}

To know more about arguments and results of each query, have a look at the [API reference](../../api).

## Simplified Example

In this example, we'll consider a simple direct query on the network tip to fetch the latest protocol parameters. The next section gives a more elaborate example which shows how to acquire a specific point on chain.

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function wsp(methodname, args) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname,
        args
    }));
}

client.once('open', () => {
    wsp("Query", { query: "currentProtocolParameters" } );
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);
    console.log(JSON.stringify(response.result, null, 4));
    client.close();
});
```

This little excerpt outputs the most recent protocol parameters in a nice JSON:

```json
{
    "poolDeposit": 500000000,
    "protocolVersion": {
        "minor": 0,
        "major": 3
    },
    "minUtxoValue": 1000000,
    "minFeeConstant": 155381,
    "maxTxSize": 16384,
    "minPoolCost": 340000000,
    "maxBlockBodySize": 65536,
    "extraEntropy": "neutral",
    "minFeeCoefficient": 44,
    "poolInfluence": "3/10",
    "maxBlockHeaderSize": 1100,
    "stakeKeyDeposit": 2000000,
    "decentralizationParameter": "1/5",
    "desiredNumberOfPools": 500,
    "poolRetirementEpochBound": 18,
    "monetaryExpansion": "3/1000",
    "treasuryExpansion": "1/5"
}
```

## Full Example

Let's see a full example getting the stake distribution of all stake pools of the Cardano mainnet. In the example, we'll also use the `FindIntersect` method from the [local-chain-sync](../local-chain-sync) protocol to get an easy point to acquire.  

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function wsp(methodname, args) {
    client.send(JSON.stringify({
        type: "jsonwsp/request",
        version: "1.0",
        servicename: "ogmios",
        methodname,
        args
    }));
}

client.once('open', () => {
    wsp("FindIntersect", { points: ["origin"] });
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);

    switch (response.methodname) {
        case "FindIntersect":
            const point = response.result.IntersectionFound.tip;
            wsp("Acquire", { point });
            break;

        case "Acquire":
            wsp("Query", { query: "stakeDistribution" });
            break;

        case "Query":
            console.log(response.result);
            client.close();
            break;
    }
});
```

Here's a walk-though describing what happens when running the above script:

1. An initial request ask to `FindIntersect` that is guaranteed to succeed is sent. This is a little trick in order to access the ledger tip easily. As a response, Ogmios replies with:

    {{% expand "IntersectionFound" %}}
```json
{
    "version": "1.0",
    "servicename": "ogmios",
    "type": "jsonwsp/response",
    "methodname": "FindIntersect",
    "result": {
        "IntersectionFound": {
            "point": "origin",
            "tip": {
                "hash": "dbafebb0146b2ec45186dfba6c287ad69c83d3fd9a186b39d99ab955631539e0",
                "blockNo": 4887546,
                "slot": 12526684
            }
        }
    },
    "reflection": null
}
```
    {{% /expand %}}

2. Using the `tip` from the previous response, we can now safely `Acquire` a state on that particular tip which we know exists and is not too old. Ogmios replies successfully with:

    {{% expand "AcquireSuccess" %}}
```json
{
    "version": "1.0",
    "servicename": "ogmios",
    "type": "jsonwsp/response",
    "methodname": "Acquire",
    "result": {
        "AcquireSuccess": {
            "acquired": {
                "hash": "dbafebb0146b2ec45186dfba6c287ad69c83d3fd9a186b39d99ab955631539e0",
                "slot": 12526684
            }
        }
    },
    "reflection": null
}
```
    {{% /expand %}}

3. Now in a position to make an actual `Query`, we do it and ask for the stake distribution across all stake pools. The (truncated) response from the server looks like:

    {{% expand "QueryResponse" %}}
```json
{
    "version": "1.0",
    "servicename": "ogmios",
    "type": "jsonwsp/response",
    "methodname": "Query",
    "result": {
        "pool1w3s6gk83y2g3670emy3yfjw9myz3u4whph7peah653rmsfegyj3": {
            "stake": 0,
            "vrf": "29c1a293c550beea756bc0c01416bacd7030ae8992e13ca242d4d6c2aebaac0d"
        },
        "pool1n5shd9xdt4s2gm27fxcnuejaqhhmpepn6chw2c82kqnuzdtpsem": {
            "stake": 0.00003058882418046271,
            "vrf": "7e363eb8bfd8fef018da4c397d6a6ec25998363434e92276e40ee6c706da3ae5"
        },
        "..."
    },
    "reflection": null
}
```
    {{% /expand %}}


{{% notice warning %}}
Be aware that it is possible for an `Acquire` request to fail even if (and in particular if) made immediately after finding the ledger tip. In Ouroboros Praos frequent small rollbacks of the chain are not rare and the few last blocks of the chain can be a bit volatile. A real application may require more elaborate error handling than the toy example above. 
{{% /notice %}}

## Example Queries

#### currentEpoch

```json
{
  "query": "currentEpoch"
}
```

#### currentProtocolParameters

```json
{
  "query": "currentProtocolParameters"
}
```

#### delegationsAndRewards   

```json
{
  "query": {
    "delegationsAndRewards": [
      "7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8"
    ]
  }
}
```

#### eraStart

```json
{
  "query": "eraStart"
}
```

#### eraSummaries

```json
{
  "query": "eraSummaries"
}
```

#### genesisConfig

```json
{
  "query": "genesisConfig"
}
```

#### ledgerTip

```json
{
  "query": "ledgerTip"
}
```


#### nonMyopicMemberRewards (by credentials)

```json
{
  "query": {
    "nonMyopicMemberRewards": [
      "7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8"
    ]
  }
}
```

#### nonMyopicMemberRewards (by amounts)

```json
{
  "query": {
    "nonMyopicMemberRewards": [
      42000000
    ]
  }
}
```

#### poolIds

```json
{
  "query": "poolIds"
}
```

#### poolParameters

```json
{
  "query": {
    "poolParameters": [
      "pool1pk2wzarn9mu64eel89dtg3g8h75c84jsy0q349glpsewgd7sdls",
      "4acf2773917c7b547c576a7ff110d2ba5733c1f1ca9cdc659aea3a56"
    ]
  }
}
```

#### poolsRanking


```json
{
  "query": "poolsRanking"
}
```

#### proposedProtocolParameters


```json
{
  "query": "proposedProtocolParameters"
}
```

#### rewardsProvenance


```json
{
  "query": "rewardsProvenance"
}
```


#### rewardsProvenance'


```json
{
  "query": "rewardsProvenance'"
}
```

#### stakeDistribution

```json
{
  "query": "stakeDistribution"
}
```

#### utxo (by Address)

```json
{
  "query": {
    "utxo": [
      "addr1wx66ue36465w2qq40005h2hadad6pnjht8mu6sgplsfj74qhpnf3s",
      "addr1xyxefct5wvh0n2h88uu44dz9q7l6nq7k2q3uzx54ruxr9e93ddt0tmqxf0n2c09tvq67lt5xkdnvc0wy5r2hzcpawrjsjk6m63"
    ]
  }
}
```

#### utxo (by TxIn)

```json
{
  "query": {
    "utxo": [
      {
        "txId": "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25",
        "index": 2
      }
    ]
  }
}
```
