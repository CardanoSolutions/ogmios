---
Number: 3
Title: Dual WebSocket / HTTP Server
Category: Server
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Web services typically works over WebSocket or HTTP, through a given TCP port. Different services are usually provided over different ports. Ogmios provides functionalities over both HTTP and WebSockets. The rationale for WebSockets is rather straightforward and the core of Ogmios' added value. The HTTP side is used mainly for monitoring and for serving static files such as, the specification or a landing page which is user-friendly. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

While we could use two different ports for both the WebSocket and the HTTP services, the server will mounts both applications under the same port, and route packets dynamically to either its internal HTTP handler or WebSocket handler, based on the shape of the message. This is trivially achieved thanks to `wai-websockets`. 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- Ogmios can provide both interfaces for WebSockets and HTTP over a same port and that is neat for documentation. 

- The use of `wai-websockets` makes it trivial, and causes no visible performance hit on heavy WebSocket usages. 
