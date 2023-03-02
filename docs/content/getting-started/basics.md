+++
title = "Basics"
chapter = false
weight = 5
+++

## JSON-RPC

Ogmios' interface is built on top of [JSON-RPC]() which is a tiny standard to give some structure to the various messages that can be exchanged with Ogmios. The standard specifies a top-level JSON envelope by which messages must abide, as well as a semantic for some of the fields. It well suited for request/response types of protocols, such as any of the Ouroboros mini-protocols spoken by Ogmios.

We won't be covering the JSON-RPC standard in this guide but gives a few extra insights about how it relates to Ogmios and some good takes from it. The standard is however relatively small so we encourage you to spare 5 minutes and quickly go through it when you find an opportunity.

Each request in Ogmios yields exactly one response, might it be a _success_ response or an _error_ response. Furthermore, requests which relate to a particular protocol are guaranteed to yield responses in the same order.

### Requests

Requests messages are sent by client applications (you) to Ogmios. Requests have 3 fields of interest: `method`, `params` and `id`. The `method` identifies the request itself and translates to a corresponding message of one of the 3 Ouroboros mini-protocols. Possible values for `method` are described in [section 3 - Ouroboros mini-protocols](../../mini-protocols) and the complete reference is available in [section 4 - API Reference](../../api). The `params` specify extra parameters that apply to the given `method`.

Here is an example of valid request message:

```json
{
    "jsonrpc": "2.0",
    "method": "FindIntersect",
    "params": { "points": [ "origin" ] },
    "id": "init-1234-5678"
}
```

As you can see, the `method` specifies the method `FindIntersect` which relates to the _local chain-sync_ Ouroboros mini-protocol. This particular request expects one argument named `points`, which contains a list of points we want to intersect with (don't panic, this is explained in further details in the next section!). Another interesting, albeit optional, field is the `id` field. This is completely free-form and will be spit back identically by Ogmios in the response exactly as provided. This can be useful to keep track of states on the client application or pass in extra context to each request/response. Be careful though that anything you send for a request will come back in the response; send something big, get something big.

### Responses

Let's start with a possible response to the request above:

```
{
    "jsonrpc": "1.0",
    "result": {
        "IntersectionFound": {
            "point": "origin",
            "tip": {
              "hash": "d184f428159290bf3558b4d1d139e6a07ec6589738c28a0925a7ab776bde4d62",
              "blockNo": 4870185,
              "slot": 12176171
            }
        }
    },
    "id": "init-1234-5678"
}
```

The response kindly indicates which requests it corresponds to and has the same `method` as the matching request. It also gives a result under a specific field. Responses can have various results which are fully specified in [section 4. - API Reference](../../api). Also, notice the `id` field in the reponse which reflects exactly the `id` field that was set for the request.

{{% notice info %}}
Ogmios' responses may correspond to possible errors that are part of the Ouroboros mini-protocols. For example, if you submit an invalid transaction, you'll get back a success response with a result because it is a valid message in the context of the mini-protocols. Yet, Ogmios will yield an error message (with an `error` field) if you submit an invalid Ogmios message (for instance, an unknown request constructor, or something that is not a valid JSON-RPC message).
{{% /notice %}}

## WebSocket vs HTTP

Ogmios defaults transport protocol is WebSocket. Yet some mini-protocols (e.g. the local-tx-submission or the local-state-query) can also work over HTTP under some circumstances.

### WebSocket

The WebSocket protocol is full-duplex, which means that Ogmios and clients can send messages to each other all at the same time. That is very convenient for clients who can pipeline many requests at once and process responses later on as they arrive.

Therefore, to send a message to ogmios, you only need a WebSocket client! For example, using the well-established [ws](https://www.npmjs.com/package/ws) Node.js package and assuming a local instance of Ogmios started through Docker, you can interact with Ogmios as simply as:

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

client.once('open', () => {
    const request = {
        "jsonrpc": "2.0",
        "method: "FindIntersect",
        "params": { "points": [ "origin" ] }
    };
    client.send(JSON.stringify(request));
});

client.on('message', function(msg) {
  const response = JSON.parse(msg);

  // do something with 'response'
});
```

### HTTP

Some requests may also simply be sent over HTTP as `POST` requests, using the same JSON payload as data. This works well for (part of) the protocols that are stateless. For example, a one-off transaction submission is likely easier to send as an HTTP request than via an asynchronous WebSocket. Similarly, local state queries that are presented later in this user manual are likely candidate too, provided that you need not to hook on a particular point on chain. For example:

```js
const fetch = require('fetch');

fetch("http://localhost:1337", {
  method: "POST",
  data: {
    "jsonrpc": "2.0",
    "method: "SubmitTx",
    "params": { "transaction": "..." }
  }
}).then(async response => {
  const json = await response.json();

  // do something with 'response'
});

That's all for the basics. The rest isn't really specific to Ogmios, but is about the Ouroboros mini-protocols themselves. See the next sections for a deep dive!
