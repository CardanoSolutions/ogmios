---
Number: 7
Title: cardano-node-ogmios Docker
Category: General
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Ogmios provides docker images for the server as an easy way to spin up a server. An Ogmios server on itself does nothing: it needs to be connected to a local cardano-node to operate. Since Ogmios provides an interface for the node, from an external point of view, it may be seen as the node itself. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

In the spirit of a blackbox, we'll provide a `cardano-node-ogmios` docker image to spin up containers running both cardano-node and ogmios side-by-side, already configured and connected. From an external point of view, the container acts as a single unit, which exposes several ports and interfaces for the cardano-node. It is a strawman cardano-node's websocket support. 

However, we'll also still provide an Ogmios standalone image for those who prefer running Ogmios in orchestration. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- We had to rework a bit the Dockerfile to avoid having to maintain two Dockerfiles. Leveraging Docker multi-stages, we've been able to provide a single build definition to build both images, defaulting to cardano-node-ogmios.

- We have also adjusted the documentation to reflect this and, put the spotlight on cardano-node-ogmios (which remains the recommended approach).

- To run both images in a container, we need to do _some level_ of process monitoring. Following Docker's official guides, we have resorted to a simple bash script with `-m` set and both processes executed in background. If either process stops, the other is also terminated.
