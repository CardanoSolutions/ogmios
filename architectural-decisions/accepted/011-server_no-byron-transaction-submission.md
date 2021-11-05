---
Number: 11
Title: No Byron Transaction Submission 
Category: Server
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Cardano is divided into Eras (Byron, Shelley, Allegra, Mary & Alonzo at the moment or writing this ADR). Each Era brings its own set of new functionality. For example, Allegra introduces timelock scripts over Shelley, and Mary introduces multi-assets UTXO over Allegra. Yet, the transition between Byron and Shelley is the most significant. While any Era after Shelley can be seen as a superset of the previous one, the transition from Byron to Shelley is more of a complete rewrite, with added features along the way. 

Furthermore, one can submit transactions to a Cardano node via the _local tx submission_ mini-protocol. This protocol is dependent of the current node's era. While this sounds intuitive when considering a node fully synchronized, it can lead to surprises for a syncing node. Indeed, even if the _network_ is in era E<sub>n</sub>, while syncing, the local node will go through E<sub>1</sub>, E<sub>2</sub>, ..., E<sub>n-1</sub>. Thus, while syncing, clients have to adjust the transaction submission format to the node's format. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Initially, the choice was made to stick with the node's API and to support all eras, including Byron in the transaction submission. This requires adequate testing and documentation, for something which is only valid during a transient period. Instead, we decide to fully drop Byron transactions from the API and return instead a "EraMismatch" error if any Byron transaction is submitted through the API.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- The documentation, testing and code can be simplified to only cover Shelley and onwards.
- Clients can no longer submit transactions in the Byron era, meaning that it's not possible to use Ogmios on private clusters of Byron nodes (is this really an issue?)
