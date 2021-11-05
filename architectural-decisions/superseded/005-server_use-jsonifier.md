---
Number: 5
Title: Use jsonifier
Category: Server
Status: superseded by 006
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Ogmios heavily relies on JSON serialization in its core part. Using an efficient serialization library is a must and has a direct impact on performances as measured using profiling tools. In Haskell, there aren't much contestants to [`aeson`](https://hackage.haskell.org/package/aeson) but a case was recently made about [`jsonifier`](http://hackage.haskell.org/package/jsonifier).

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Consider using `jsonifier` instead of `aeson` to write JSON encoders and speed up performances. This drastically changes the approach taken so far in Ogmios which heavily relies on Type-classes. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- `aeson` remained necessary for decoding values since `jsonifier` only cover the encoding part. This lead to quite awkward moment where we would have two separate abstractions for manipulating JSON with a frontier often very unclear. 

- After comparing the encoding API from `aeson` with `jsonifier` (which both use direct encoding instead of constructing intermediate `Value` types like old-school aeson), it turned out that `aeson` performed generally better for complex structures and large lists like we often have in Ogmios. 

- On the plus side and because I had doubts in the first place that `jsonifier` would really be the final choice, I ended up writing the Ogmios' JSON prelude to provide primitives and a small EDSL for writing other encoders (Byron, Shelley, ...). As a result, switching from jsonifier to something else was easy since only the primitives had to change. 
