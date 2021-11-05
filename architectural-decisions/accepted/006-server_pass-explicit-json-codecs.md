---
Number: 6
Title: Pass Explicit JSON Codecs
Category: Server
Status: accepted
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

The typical way to define JSON instances for data-types in Haskell is to use the battle-tested [`aeson`](https://hackage.haskell.org/package/aeson) package with type-classes `FromJSON` / `ToJSON`. While this works okay for many projects, it can also get rapidly out of hands for larger projects because of the way type-classes work in Haskell. In particular, we encounter the following problems:

1. There's only one class instance per data-type; for widely used type-classes like `FromJSON` / `ToJSON` and a project like Ogmios which re-uses data-types from other packages (e.g. the `cardano-ledger-specs`) it may cause unfortunate clashes.

2. Class instances must be ideally co-located with either the class definition, or the data-type definition. In case where one wants to define an instance for a data-type of a foreign package, the only solutions are to either define an _Orphan instance_ (bringing its own set of complications) or to re-wrap the foreign data-type into a newtype. The latter isn't always possible especially for deeply nested data-types.

3. Mostly as a consequence of (1), it is unpractical to provide different instances for a single data-types. This can often be achieved by using newtype modifiers, but present problems for either nested data-types or for foreign types altogether. There are however good use-cases (e.g. the compact serialization mode in Ogmios) which demands being able to provide different implementations based on different settings. 

4. The type-class approach often encourages to use polymorphic class methods instead of explicit functions. That is arguably the main benefits of using type-classes for one doesn't have to worry about the type being transformed so long as it has an instance. But it can also be its biggest source of issue on a large codebase with frequent changes in the definitions of data-types. Maintaining such polymorphic code is tricky and it makes it harder to keep track of JSON transformations happening. For a project like Ogmios which wants to maintain a backward-compatible API on changes, relying on auto-magic and opaque `toJSON` can have disastrous consequences if an underlying data-type changes. Beside, error messages can be quite bad and too generic to help debugging efficiently. 

5. There's a performance cost coming from the use of type-classes. Although it is negligible for many projects, most of the work done by Ogmios is about serializing some data-types to JSON. The serialization being a central part of the project, using efficient serialization methods is a must. Various profiling analysis done using [`profiteur`](https://github.com/jaspervdj/profiteur) have furthermore shown that a significant amount of time was indeed spent serializing data-types. 

## Decision(s)

<!-- What is the change that we're proposing and/or doing? -->

- We'll use the [`Encoding` API](https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson-Encoding.html) from `aeson` for it is now the recommended approach from the package's maintainers:

  > In older versions of this library, encoding a Haskell value involved converting to an intermediate Value, then encoding that. A "direct" encoder converts straight from a source Haskell value to a ByteString without constructing an intermediate Value. This approach is faster than toJSON, and allocates less memory. The toEncoding method makes it possible to implement direct encoding with low memory overhead.

- We'll define record-like types to be passed around at runtime and provide direct encoding of data-types. And similarly, we'll define every data-types encoder as plain functions built on top of handy primitives and avoid the use of type-classes for defining JSON encoders at all cost. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- It has been possible to implement the "compact" mode for Ogmios on top of WebSocket's sub-protocols (that is, allowing clients to select their own encoding flavor)!
- JSON encoders have become much easier to maintain and manage. Compilation errors are very explicit and allow for pin-pointing problems very fast on dependency upgrades.
- Encoder definitions do not clash with existing class instances from `cardano-api` or `cardano-ledger-specs` and are fully under control in the project. 
- There has been a net performance increase (+50%) after switching to direct JSON encoding. 
