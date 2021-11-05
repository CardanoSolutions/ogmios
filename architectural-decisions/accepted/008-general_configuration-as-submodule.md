---
Number: 8
Title: Configuration as sub-module
Category: General
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Ogmios works hand-and-hand with the [Cardano node](https://github.com/input-output-hk/cardano-node). Without a node, there isn't much going on with the Ogmios server. It therefore makes sense to not view them separately, but rather think about cardano-node-ogmios as a single container which happens to be a cardano-node with a WebSocket interface. While Ogmios is very lightweight and can be configured fully from a few command-line flags, cardano-node requires more complex configuration files (one for the node, multiple genesis files, keys, etc..). 

This is actually the case for many projects which directly depends on a node, like [cardano-rosetta](https://github.com/input-output-hk/cardano-rosetta), [cardano-graphql](https://github.com/input-output-hk/cardano-graphql) or [cardano-wallet](https://github.com/input-output-hk/cardano-wallet). 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

We'll provide, as part of the repository architecture the necessary configuration files for cardano-node. Since this is a concern which can be found in other projects as well, we have decided to host the configurations in a separate repository, and depends on this repository as sub-module in order to:

1. Share a common structure across various projects
2. Provide an easy access to latest configuration 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- [cardano-configurations](https://github.com/input-output-hk/cardano-configurations) was created, including a nightly job to automatically synchronize configurations from https://hydra.iohk.io
- A git sub-module has been added to the Ogmios repository, which makes the latest configurations easy to reach in a single pull. 
