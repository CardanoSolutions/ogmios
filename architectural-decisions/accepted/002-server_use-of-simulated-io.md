---
Number: 2
Title: Use Of Simulated IO
Category: Server
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

One of the hard, albeit core part, of programming is to control and contain side-effects. Indeed, side-effects tend to be harder to test by essence, and typical functional architecture will push side-effects at the edge of the system in order to keep most of the core functionality _pure_. Ogmios is no different and share the same concerns. 

The IOG team has developed a Haskell library which model the most common side-effects needed in a system: time management, exceptions, asynchronous operations and concurrent utilities. This library is particularly useful because it can run programs in either the Haskell classic runtime (`IO`), or, a simulated version of it where time is instantaneous. Simulated runs generate a trace of the programs which can be analyzed.

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Ogmios effects will be modelled using the principles exposed in [`io-sim`](https://github.com/input-output-hk/ouroboros-network/tree/22d9b995e2c302fb14532aa459bc6b81627267bc/io-sim) and provides monads for capturing side-effects:

- `MonadAsync`
- `MonadClock`
- `MonadLog`
- `MonadMetrics`
- `MonadOuroboros`
- `MonadSTM`
- `MonadWebSocket`

The use of IO will only pushed at the very edge of the system, such that any bit of logic inside Ogmios can be run in simulated IO. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- The use of `io-sim` plays very well together with [The Three Layer Cake Architecture](./organize-as-three-layer-cake.md). 

- Using mtl-style signatures makes it very clear to know what effects is required by a function, and helps to understand what to expect. For example:

  ```hs
  newHealthCheckClient
    :: forall m env.
        ( MonadAsync m
        , MonadClock m
        , MonadLog m
        , MonadMetrics m
        , MonadReader env m
        , MonadThrow m
        )
  ```

- It is possible to run complex execution of the mini-protocols in fully simulated IO, leveraging property-based testing to find tricky bugs. For examples, we've been able to write extensive tests for the [chain-sync](https://github.com/CardanoSolutions/ogmios/blob/b0d242ab1d602f185fa5ef3ce088f6ec25008df7/server/test/unit/Ogmios/App/Protocol/ChainSyncSpec.hs#L88-L93) protocol and the [state-query](https://github.com/CardanoSolutions/ogmios/blob/b0d242ab1d602f185fa5ef3ce088f6ec25008df7/server/test/unit/Ogmios/App/Protocol/StateQuerySpec.hs#L113-L114) protocol in simulated IO which would have been much more cumbersome to write in plain IO. 
