---
Number: 10
Title: Simplified Logging Backend
Category: Server
Status: accepted
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Like any production-ready application, Ogmios needs good logging. Inside the application, Ogmios uses contra-tracers which works great to decouple the production of the logs from their structure and severity. Behind the scene, tracers do ultimately require a backend to realize actual logging. Following the [12-factor](https://12factor.net/) principles, we want the logging to be an event stream done on stdout and be structured to be easily processable by any other log consumer. 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

While Ogmios started using the [`iohk-monitoring-framework`](https://github.com/input-output-hk/iohk-monitoring-framework/) as an application backend, we have decided to let it go in favor of a custom, simpler, backend. Dropping the iohk-monitoring-framework came after many sessions of struggle with the library APIs, undesirable quirks in the generated logs and overall ease of use / heaviness of the library. In the end, we have captured the essence of the stdout concurrent logger, though not yet with buffering. This is okay since Ogmios does not generate many logs at the moment but may be revisited later.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- We have replaced the iohk-monitoring-framework with a much simpler logger, outputting log to stdout and protected by an MVar to be thread-safe. 

- We have been able to greatly simplify the logging code and most traces definitions (no longer requiring `FromJSON` instances for them). 

- No visible application performance impact. 
