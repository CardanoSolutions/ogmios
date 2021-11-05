---
Number: 12
Title: Split Rewards Provenance Query
Category: Server
Status: proposed 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

The local state-query Ouroboros mini-protocol provides a slew of queries to perform read operations on the ledger state. One of them, the `GetRewardsProvenance` query is rather complex and returns details about intermediary steps of the rewards calculation for pools at each epoch.

The query also returns current pool parameters for that pool. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

In Ogmios' API, we will artificially split the `GetRewardsProvenance` query in two queries: 

- `rewardsProvenance`, which returns the same result as the Cardano-node's one minus pool parameters.
- `poolParameters`, which returns only pool parameters for the given pool.

This is because the pool parameters feel rather ad-hoc at this level. They aren't strictly related to other calculations involved in the rewards provenance and there's a clear use-case for fetching most recent pool parameters without bothering about the fuzz around rewards.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- This creates a little discrepancy between Ogmios' API and cardano-node's API, although both approaches are equivalent (the same information ends up being exposed).

- The `poolParameters` query is documented separately which is also clearer and well-scoped. 
