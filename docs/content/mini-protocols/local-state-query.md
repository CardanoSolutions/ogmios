+++
title = "Ledger state queries"
chapter = false
weight = 2
+++


{{% ascii-drawing-split %}}
                                   queryLedgerState/*
                         START     queryNetwork/*
                           ⇓      ╭────╮
releaseLedgerState ┌──────────────┴┐   │
            ╭─────▶│     Idle      │◀──╯
            │      └───────┬───────┘
            │              │
            │              │ acquireLedgerState
            │              │
            │              │   (re)acquireLedgerState
            │              ▼  ╭────────╮
            │      ┌──────────┴────┐   │
            ╰──────┤   Acquired    │◀──╯
                   └───┬───────────┘
                       │       ▲
    queryLedgerState/* │       │
        queryNetwork/* │       │
                       ╰───────╯
{{% /ascii-drawing-split %}}

## Overview

The state query protocol is likely the most versatile of the three Ouroboros mini-protocols. As a matter of fact, it allows for querying various types of information directly from the ledger. In essence, it is like a very simpler request/response pattern where the types of questions one can ask are specified by the protocols. Those questions include: information about the chain tip, information about stake pools but also the balance of a particular address.

In order to run a question by the ledger, one must first acquire a particular position on the chain, so that the node can reliably answer a few questions on a chosen, frozen state while continuing maintaining more recent version of the ledger on the side. It is important to note that:

1. The node cannot acquire any arbitrary state. One can only rewind up to a certain point.

2. Should a client keep a state acquired for too long, it is likely to become unreachable at some point, forcing clients to re-acquire.

## How to use

Ogmios uses a simplified version of the above state-machine. Or more exactly, it exposes a simplified version and handles some of the complexity behind the scene for you. As clients, Ogmios will give you method to acquire a state, query that state and release the state. A typical sequence would be to start by acquiring a state on a given point and then make a few queries, and then release. The release step is optional although it is a bit more polite to say goodbye at the end of a conversation.

It is also possible to submit queries directly without acquiring. As a consequence, Ogmios will acquire the tip of the chain, run the query and release it for you. This is the easiest way to send queries if you don't care about capturing a particular state. Note however that this may create race conditions if you send multiple queries via this method. Indeed, the tip is changing quite often on the network, and two subsequent queries may actually run on two different points of the chain. While this is generally safe for most queries, it may also put your application in an unexpected state when crossing epoch boundaries or hard-forks.

## Acquiring a state

The `acquireLedgerState` method expects one argument named `point`. The point has the same format as points in the [chain synchronization protocol](../local-chain-sync). That is, they can be block header hashes or the special keyword `"origin"` (though there's very little chance that one will be able to acquire the origin!).

```json
{
    "jsonrpc": "2.0",
    "method": "acquireLedgerState",
    "params": {
        "point": {
            "slot": 1234,
            "hash": "9e871633f7aa356ef11cdcabb6fdd6d8f4b00bc919c57aed71a91af8f86df590"
        }
    }
}
```

One thing that doesn't strike as obvious is that, as clients, you need points to query any information. There are many ways to get those hashes but in the context of Ogmios, the most logical way is via the [chain synchronization](../local-chain-sync/) protocol.

{{% notice tip %}}
You can acquire multiple times, the last one will prevail. If you need to re-acquire, simply send another `acquire` request.
{{% /notice %}}

{{% notice warning %}}
You can skip acquiring a state should you want to run a query on the current state of the chain. This is good for one-off queries, but if you need to chain multiple queries together it is **highly recommended** to acquire a state first to preserve data-consistency between queries!
{{% /notice %}}

## Querying

There are many queries that can be sent to the ledger, and the list is growing days after days as the Cardano team implements new ones. With Ogmios, all queries follow the same pattern and are identified by a method. There exists two types of queries: ledger-state queries and network queries. The former is performed on the ledger state and are era-dependent. The latter are always available (even when the node is synchronizing) and are era-independent. In both cases, queries are constructed in a similar fashion:

- `queryLedgerState/*`, where `*` has to be replaced with an actual ledger-state query name (see below);
- `queryNetwork/*`, where `*` has to be replaced with an actual network query name (see below)

For example, to query the ongoing epoch of the ledger:

```
{
    "jsonrpc": "2.0",
    "method": "queryLedgerState/epoch",
}
```

#### Network

At the moment of writing this guide, the following queries are available:

queryNetwork                 | Information
---                          | ---
`blockHeight`                | The chain's highest block number.
`genesisConfiguration`       | Get the genesis configuration of a specific era.
`startTime`                  | The chain's start time (UTC).
`tip`                        | The network's current tip.

#### Ledger-state

queryLedgerState             | Information
---                          | ---
`epoch`                      | The current epoch of the ledger.
`eraStart`                   | The information regarding the beginning of the current ledger era.
`eraSummaries`               | Era bounds and slot parameters details, required for proper slotting arithmetic.
`liveStakeDistribution`      | Distribution of the stake across all known stake pools, relative to the **total** stake in the network.
`projectedRewards`           | The projected rewards of an account in a context where the top stake pools are fully saturated. This projection gives, in principle, a ranking of stake pools that maximizes delegator rewards.
`protocolParameters`         | The current protocol parameters.
`proposedProtocolParameters` | The last update proposal w.r.t. protocol parameters, if any.
`rewardAccountSummaries`     | Current delegation settings and rewards of chosen reward accounts.
`rewardsProvenance`          | Get details about rewards calculation for the ongoing epoch.
`stakePools`                 | The list of all currently registered and active stake pools with their current parameters.
`tip`                        | The current tip the ledger is at. Said differently, the slot number and header hash of the last block that has been processed by the ledger.
`utxo`                       | Current UTXO, possibly filtered by output reference.

{{% notice tip %}}
To know more about arguments and results of each query, have a look at the [API reference](../../api).
{{% /notice %}}

## Simplified example

In this example, we'll consider a simple direct query on the network tip to fetch the latest protocol parameters. The next section gives a more elaborate example which shows how to acquire a specific point on chain.

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function rpc(method, params = {}, id) {
    client.send(JSON.stringify({
        jsonrpc: "2.0",
        method,
        params,
        id
    }));
}

client.once('open', () => {
    rpc("queryLedgerState/protocolParameters");
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);
    console.log(JSON.stringify(response.result.protocolParameters, null, 4));
    client.close();
});
```

This little excerpt outputs the most recent protocol parameters in a nice JSON:

```json
{
  "minFeeCoefficient": 44,
  "minFeeConstant": { "lovelace": 155381 },
  "maxBlockBodySize": { "bytes": 90112 },
  "maxBlockHeaderSize": { "bytes": 1100 },
  "maxTransactionSize": { "bytes": 16384 },
  "stakeCredentialDeposit": { "lovelace": 2000000 },
  "stakePoolDeposit": { "lovelace": 500000000 },
  "stakePoolRetirementEpochBound": 18,
  "desiredNumberOfStakePools": 500,
  "stakePoolPledgeInfluence": "3/10",
  "monetaryExpansion": "3/1000",
  "treasuryExpansion": "1/5",
  "minStakePoolCost": { "lovelace": 340000000 },
  "minUtxoDepositConstant": 0,
  "minUtxoDepositCoefficient": 4310,
  "plutusCostModels": {
    "plutus:v1": [
      205665,    812,      1,      1,   1000,    571,      0,       1,
        1000,  24177,      4,      1,   1000,     32, 117366,   10475,
           4,  23000,    100,  23000,    100,  23000,    100,   23000,
         100,  23000,    100,  23000,    100,    100,    100,   23000,
         100,  19537,     32, 175354,     32,  46417,      4,  221973,
         511,      0,      1,  89141,     32, 497525,  14068,       4,
           2, 196500, 453240,    220,      0,      1,      1,    1000,
       28662,      4,      2, 245000, 216773,     62,      1, 1060367,
       12586,      1, 208512,    421,      1, 187000,   1000,   52998,
           1,  80436,     32,  43249,     32,   1000,     32,   80556,
           1,  57667,      4,   1000,     10, 197145,    156,       1,
      197145,    156,      1, 204924,    473,      1, 208896,     511,
           1,  52467,     32,  64832
    ],
    "plutus:v2": [
      205665,    812,      1,      1,   1000,    571,      0,       1,
        1000,  24177,      4,      1,   1000,     32, 117366,   10475,
           4,  23000,    100,  23000,    100,  23000,    100,   23000,
         100,  23000,    100,  23000,    100,    100,    100,   23000,
         100,  19537,     32, 175354,     32,  46417,      4,  221973,
         511,      0,      1,  89141,     32, 497525,  14068,       4,
           2, 196500, 453240,    220,      0,      1,      1,    1000,
       28662,      4,      2, 245000, 216773,     62,      1, 1060367,
       12586,      1, 208512,    421,      1, 187000,   1000,   52998,
           1,  80436,     32,  43249,     32,   1000,     32,   80556,
           1,  57667,      4,   1000,     10, 197145,    156,       1,
      197145,    156,      1, 204924,    473,      1, 208896,     511,
           1,  52467,     32,  64832
    ]
  },
  "scriptExecutionPrices": {
    "memory": '577/10000',
    "cpu": '721/10000000'
  },
  "maxExecutionUnitsPerTransaction": {
    "memory": 14000000,
    "cpu": 10000000000
  },
  "maxExecutionUnitsPerBlock": {
    "memory": 62000000,
    "cpu": 20000000000
  },
  "maxValueSize": { "bytes": 5000 },
  "collateralPercentage": 150,
  "maxCollateralInputs": 3,
  "version": {
    "major": 8,
    "minor": 0
  }
}
```

## Full example

Let's see a full example getting the stake distribution of all stake pools of the Cardano mainnet. In the example, we'll also use a network query to find the current chain tip, and then try to acquire it for subsequent queries.

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

function rpc(method, params = {}, id) {
    client.send(JSON.stringify({
        jsonrpc: "2.0",
        method,
        params,
        id
    }));
}

client.once('open', () => {
    rpc("queryNetwork/tip", {})
});

client.on('message', function(msg) {
    const response = JSON.parse(msg);

    switch (response.method) {
        case "queryNetwork/tip":
            const point = response.result.tip;
            rpc("acquireLedgerState", { point });
            break;

        case "acquireLedgerState":
            rpc("queryLedgerState/liveStakeDistribution");
            break;

        default:
            console.log(response.result.liveStakeDistribution);
            client.close();
            break;
    }
});
```

Here's a walk-though describing what happens when running the above script:

1. An initial request ask the network tip. That is guaranteed to succeed and is a little trick in order to access the ledger tip easily. As a response, Ogmios replies with:

    {{% expand "Got network tip" %}}
```json
{
    "jsonrpc": "2.0",
    "method": "queryNetwork/tip",
    "result": {
        "tip": {
            "hash": "dbafebb0146b2ec45186dfba6c287ad69c83d3fd9a186b39d99ab955631539e0",
            "slot": 12526684
        }
    },
    "id": "get-network-tip"
}
```
    {{% /expand %}}

2. Using the `tip` from the previous response, we can now safely aquire a state on that particular tip which we know exists and is not too old. Ogmios replies successfully with:

    {{% expand "Acquired ledger state" %}}
```json
{
    "jsonrpc": "2.0",
    "method": "acquireLedgerState",
    "result": {
      "acquired": "ledgerState",
      "point": {
          "id": "dbafebb0146b2ec45186dfba6c287ad69c83d3fd9a186b39d99ab955631539e0",
          "slot": 12526684
      }
    },
    "id": "acquire-network-tip"
}
```
    {{% /expand %}}

3. Now in a position to make an actual query, we do it and ask for the stake distribution across all stake pools. The (truncated) response from the server looks like:

    {{% expand "Query response" %}}
```json
{
    "jsonrpc": "2.0",
    "method": "queryLedgerState/liveStakeDistribution",
    "result": {
        "liveStakeDistribution": {
            "pool1w3s6gk83y2g3670emy3yfjw9myz3u4whph7peah653rmsfegyj3": {
                "stake": 0,
                "vrf": "29c1a293c550beea756bc0c01416bacd7030ae8992e13ca242d4d6c2aebaac0d"
            },
            "pool1n5shd9xdt4s2gm27fxcnuejaqhhmpepn6chw2c82kqnuzdtpsem": {
                "stake": 0.00003058882418046271,
                "vrf": "7e363eb8bfd8fef018da4c397d6a6ec25998363434e92276e40ee6c706da3ae5"
            },
            "..."
        }
    }
}
```
    {{% /expand %}}


{{% notice warning %}}
Be aware that it is possible for an acquire request to fail even if (and in particular if) made immediately after finding the ledger tip. In Ouroboros Praos frequent small rollbacks of the chain are not rare and the few last blocks of the chain can be a bit volatile. A real application may require more elaborate error handling than the toy example above.
{{% /notice %}}

## Example queries

### Network

#### blockHeight

```json
{
  "jsonrpc": "2.0",
  "method": "queryNetwork/blockHeight"
}
```

#### genesisConfiguration

```json
{
  "jsonrpc": "2.0",
  "method": "queryNetwork/genesisConfiguration",
  "params": {
    "era": "shelley"
  }
}
```

```json
{
  "jsonrpc": "2.0",
  "method": "queryNetwork/genesisConfiguration",
  "params": {
    "era": "alonzo"
  }
}
```

#### startTime

```json
{
  "jsonrpc": "2.0",
  "method": "queryNetwork/startTime"
}
```

#### tip

```json
{
  "jsonrpc": "2.0",
  "method": "queryNetwork/tip"
}
```

### Ledger-state

#### epoch

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/epoch"
}
```

#### eraStart

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/eraStart"
}
```

#### eraSummaries

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/eraSummaries"
}
```

#### liveStakeDistribution

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/liveStakeDistribution"
}
```

#### projectedRewards

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/projectedRewards",
  "params": {
    "stake": [
      1000000
    ]
  }
}
```

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/projectedRewards",
  "params": {
    "keys": [
      "7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8",
      "stake_vkh10stzgpc5ag8p9dq6j98jj3tcftzffwce2ulsefs6pzh6s39tk6l"
    ]
  }
}
```

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/projectedRewards",
  "params": {
    "scripts": [
      "7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8",
      "script10stzgpc5ag8p9dq6j98jj3tcftzffwce2ulsefs6pzh6snywdma"
    ]
  }
}
```

#### protocolParameters

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/protocolParameters"
}
```

#### proposedProtocolParameters

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/proposedProtocolParameters"
}
```

#### rewardAccountSummaries

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/rewardAccountSummaries",
  "params": {
    "keys": [
      "7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8",
      "stake_vkh10stzgpc5ag8p9dq6j98jj3tcftzffwce2ulsefs6pzh6s39tk6l"
    ]
  }
}
```

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/rewardAccountSummaries",
  "params": {
    "scripts": [
      "7c16240714ea0e12b41a914f2945784ac494bb19573f0ca61a08afa8",
      "script10stzgpc5ag8p9dq6j98jj3tcftzffwce2ulsefs6pzh6snywdma"
    ]
  }
}
```

#### rewardsProvenance

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/rewardsProvenance"
}
```

#### stakePools

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/stakePools"
}
```

#### stakePools (filtered)

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/stakePools",
  "params": {
    "stakePools": [
      { "id": "pool1pk2wzarn9mu64eel89dtg3g8h75c84jsy0q349glpsewgd7sdls" },
      { "id": "4acf2773917c7b547c576a7ff110d2ba5733c1f1ca9cdc659aea3a56" }
    ]
  }
}
```

#### ledger tip

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/tip"
}
```

#### network tip

```json
{
  "jsonrpc": "2.0",
  "method": "queryNetwork/tip"
}
```


#### utxo

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/utxo"
}
```

```json
{
  "jsonrpc": "2.0",
  "method": "queryLedgerState/utxo",
  "params": {
    "outputReferences": [
      {
        "transaction": { "id": "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25" },
        "index": 2
      }
    ]

  }
}
```

## Errors

Errors from the chain synchronization protocol are in the range `2000-2999` and are listed below.

{{% embed-async-api %}}
asyncapi: '2.4.0'
info:
  title: ""
  version: '6.0.0'
servers: {}
channels: {}
components:
  schemas:
    2000:
      $ref: "/ogmios.json#/properties/AcquireLedgerStateFailure/properties/error"
    2001:
      $ref: "/ogmios.json#/properties/QueryLedgerStateEraMismatch/properties/error"
    2002:
      $ref: "/ogmios.json#/properties/QueryLedgerStateUnavailableInCurrentEra/properties/error"
    2003:
      $ref: "/ogmios.json#/properties/QueryLedgerStateAcquiredExpire/properties/error"
{{% /embed-async-api %}}

## API Reference

The complete description of the mempool monitoring requests and responses can be found in the [API reference](../../api).

Plus, [test vectors](https://github.com/CardanoSolutions/ogmios/tree/master/server/test/vectors) are available on the repository for testing, debugging and to serve as examples.
