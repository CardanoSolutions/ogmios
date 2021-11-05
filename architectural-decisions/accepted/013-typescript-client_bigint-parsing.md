---
Number: 13
Title: BigInt Parsing
Category: TypeScript Client
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Unlike popular opinions, the JSON specification does support arbitrary large integers. However, default parsers, in particular in JavaScript, tends to fail to parse big numbers (in JavaScript, numbers are stored as double and the max safe integers is `2^53 - 1`). The Ogmios server via the underlying Haskell JSON library (`aeson`) it uses for encoding and parsing does also support large integers. 

Most integer values on Cardano are bounded, either because their binary representation is bounded or because a value is constrained by the system so much that in a lifetime, it will remain bounded. Some values however may be arbitrarily large. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Many values fall in the first category (bounded by definition) and their treatment is unequivocal. We will however consider some _potentially unbounded_ value as _empirically bounded_ if their value is constrained by the system (i.e. they aren't user-defined) and may not realistically exceed 2^53-1 over a lifetime. This is the case of the slot length or the block height for instance. This decision may however be reconsidered on future hard forks if their definitions (and thus constraints) would change. 

We will however recommend clients to treat:

- Native assets quantities
- Metadata `int` 

as potentially arbitrarily large and parse them using a type that can handle such types. In particular, the TypeScript client for Ogmios will parse those quantities as native `BigInt` and parse any other quantities as plain `number`.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- We used the `json-bigint` npm library as the client parser instead of the native `JSON.parse`. However, the library isn't maintained anymore and contained a few bugs we had to fix via a fork. We do therefore now maintain a fork of `json-bigint`. 

- There's no way to perform reflection at runtime in TypeScript. Once compiled, any type information actually disappear at runtime (and that is the point of TypeScript!). Thus, that means it is not possible to write a parser that can assert whether a certain value should be parsed to a number or a BigInt and the heuristic to determine which one to choose is wobbly. In particular, the json-bigint library will either: parse every integers as BigInt (which is undesirable) or, only parse numbers that are too large as BigInt. However, for consistency and ease of consumption downstream, it is much better to work with `BigInt` only instead of types `BigInt | number`. We therefore sanitize the JSON results with a second pass, using the surrounding context to determine whether a number should be cast as `BigInt`. 
