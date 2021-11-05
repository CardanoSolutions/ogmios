---
Number: 14
Title: TypeScript Isomorphic WebSockets
Category: TypeScript Client
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

TypeScript is a language which can now be target mainly two runtime environments: the browser or node.js. While the language is identical in both environment, the primitives, APIs and base libraries it has access to is different. In particular, WebSockets follow two distinct interfaces.

Some libraries such as [isomorphic-ws](https://github.com/heineiuo/isomorphic-ws/) provides handy ways to construct WebSocket objects using one common interface in both environment. However, the methods, properties and behavior of each object differs depending on the environment. For example, the event target API on node.js (`on`, `once`, `off`...) is vastly different from the one on browsers (`addEventListener`, `removeEventListener`). 

This is somewhat unpractical to work with in an isomorphic way.

## Decision

<!-- What is the change that we're proposing and/or doing? -->

We will provide a TypeScript interface for constructing WebSocket objects which are unchanged on node.js but, partially polyfilled in browsers just enough to cover the set of functionality that we require from node.js' API. 

This means in particular that we will provide implementation for:

- `on`
- `once`
- `removeListener`
- `removeAllListener` 

for browser WebSockets mimicking the node.js' API. The construction and use of WebSocket will therefore be made fully isomorphic within the scope of the Ogmios' TypeScript client.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- We created [IsomorphicWebSocket](https://github.com/CardanoSolutions/ogmios/blob/master/clients/TypeScript/packages/client/src/IsomorphicWebSocket.ts) as a façade interface for WebSockets which works isomorphically between node.js and browsers.

- This (alongside some other libraries for similar concerns) makes is possible to write fully isomorphic code for the client which can work seamlessly in both environment.

- We still piggyback on the `isomorphic-ws` library for the isomorphic construction of the WebSocket object, which has different options in both environment. 
