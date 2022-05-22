---
Number: 14
Title: Event Emitter to Generator
Category: TypeScript CLient
Status: accepted
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Ogmios uses WebSockets as a transport mechanism. WebSockets are inherently asynchronous, which makes request/response patterns a bit awkward. 
Indeed, the choice of WebSockets for Ogmios was mainly driven by the chain-sync protocol for which an asynchronous setup makes sense (and we 
can also leverage WebSockets' pipelining for 'free'). However, other protocols such as the tx-submission or state-query protocols are more adapted
to a synchronous handling. 

We have written TypeScript clients which gives more convenient access to those protocols. In their initial design however, some of those clients 
have issues when it comes to concurrent request handling. Indeed, an application using a single client to submit many (long) requests may end up 
facing a warning from the node.js runtime `MaxListenersExceededWarning: Possible EventEmitter memory leak detected`. It appears that in node.js, 
the runtime watches every event emitter and warns application as soon as more than `10` listeners have been installed for the same message. 
This limit can be increased, though it is a good sign that something is off in the design: a single tx-submission client, for example, should not 
require more than 10 listeners! Even if the client application is firing requests at a high rate.

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Behind the scene, the server connected to the WebSocket handles transactions sequentially and in order. Thus, it would be sufficient for a client to use
a single WebSocket connection and look at response messages in order. We can therefore define a [generator](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Generator) from an event stream and yield events one by one. To achieve this, we need two queues because we can't control who comes first: the event or generator 'next' call. 

```ts
export function eventEmitterToGenerator (eventEmitter: EventEmitter, eventName: string) {
  const events = [] as Event[]
  const listeners = [] as ((e: Event) => void)[]

  eventEmitter.on(eventName, async (e: Event) => {
    if (listeners.length > 0) {
      listeners.shift()(e)
    } else {
      events.push(e)
    }
  })

  return async function * generator () {
    while (true) {
      yield new Promise((resolve) => {
        if (events.length > 0) {
          resolve(events.shift())
        } else {
          listeners.push(resolve)
        }
      })
    }
  }
}
```

From the consumer side, it becomes a lot more synchronous and can resort to `async/await` to consume responses from the server in a straightforward way:

```ts
const responses = eventEmitterToGenerator(socket, 'message')

socket.send('foo')
await responses.next()

socket.send('bar')
await responses.next()
```

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- With this pattern, we only set **one** event listener per given client interaction. Many requests can be enqueued and their responses yielded in order from a single source.

- This works:
  - (a) Because the server handles requests sequentially and in order behind the scene. (that is, if A reaches the server before B, then necessarily, the response for A will be sent before B's);
  - (b) If and only if the call to `next` is made within the same thread execution context than the call to `send`. Indeed, if we give the runtime the opportunity to switch to a different task (e.g. by using a `setTimeout` or `setInterval`), then we may likely end up in a situation where we await on a different response than the one we just sent. When both calls happen within the same
  task context, then because of the two-queue implementation, `next` is guaranteed to yield the right event.
