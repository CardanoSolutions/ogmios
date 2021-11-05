---
Number: 1
Title: Organize as a three-layer Haskell cake
Category: Server
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Choosing _the right architecture_ for a Haskell project is a thought task. Every other months there's a new trend and experts disagree about which is better. Over the past, I have explored managed effects with free monads which turned out to be quite complex, with a significant amount of boilerplate and plumbing for in the end very little practical benefits. Doing everything in plain IO is also not doing a project any good and is merely imperative programming in disguise. So what?

## Decision

<!-- What is the change that we're proposing and/or doing? -->

In his blog post [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html) Matt Parsons goes into some details about how to structure Haskell applications around a `ReaderT env m` monad and a structure in three layers:

1. A top-level which defines how the application is stitched together, how effects are interpreted and also cover operational concerns such as logging, resource acquisition and monitoring. 
2. A second layer for defining control effects needed for writing business logic. This boils down to MTL-style classes which captures side effects as DSL. It provides instances for the first layer, and possible of mocks that can be used for testing. 
3. A third layer where the business logic is located. All functions in this layer are pure and rely on the effect DSL from the second layer.

All-in-all, this architecture was also discussed and encouraged as part of the boring Haskell movement and ultimately led to the creation of the [RIO](https://github.com/commercialhaskell/rio#the-rio-library) which is a really a `ReaderT` around `IO`. 

We'll adopt this architecture in Ogmios, and leverage the `IOSim` library from Input Output to define the second layer of control. This way, we need not to re-implement any of the effect layer and can also benefits from simulated IO where needed.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- The architecture really helped structuring the code better. It is both simple and feels very idiomatic in the Haskell landscape. There's a single entry point to the application which can be explored in a very top-down approach. 
- It fits well with the ouroboros-network approach and the typed protocols. All the business logic of Ogmios is pure and free of noise which reduces maintenance efforts and decrease risk of bugs. 
