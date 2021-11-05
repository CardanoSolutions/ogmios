---
Number: 9
Title: Compact Serialization Mode
Category: Server
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Ogmios provides an access to the Ouroboros mini-protocols. Among them, the chain-sync protocols which enables clients to fetch blocks from a local node. These are full blocks, with headers, signatures and many information. Most of the time, clients do not care about this extra stuff and serializing / deserializing everything takes needless time and CPU resources. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

The server should provide a _"compact mode"_ which strips away block authentication information such as signatures or keys from headers. To do so, it should leverage a feature of WebSockets called
`sub-protocol`. The compact mode is initiated by the client, and the server should default to full mode. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- The synchronization speed was increased by about 20% when running in compact mode, and the data transferred reduced by half (at least on Byron blocks and the first blocks of Shelley; this is probably less true nowadays with larger blocks from the Mary era).

- Clients can now provide a WebSocket sub-protocol when creating the socket connection, to which the server will reply accordingly. 

- This has been documented in the user-guide about the chain-sync protocol (since this is where it is the most useful).

- It is however a little bit hard to properly document _which_ fields are omitted when running in compact mode. We've resorted to a vendor extension of JSON-schema, but this is only visible from the schema itself (and not rendered by the documentation generator at the moment).
