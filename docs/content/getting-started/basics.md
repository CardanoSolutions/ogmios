+++
title = "Basics"
chapter = false
weight = 5
+++

## JSON-WSP 

Ogmios' interface is built on top of [JSON-WSP](https://en.wikipedia.org/wiki/JSON-WSP) which is a tiny standard to give some structure to the various messages that can be exchanged with Ogmios. The standard specifies a top-level JSON envelope by which messages must abide, as well as a semantic for some of the fields. In particular, there are 4 types of messages possible in JSON-WSP:

- descriptions
- requests
- responses
- faults (which are a special kind of responses).

We won't be covering the JSON-WSP standard in this guide but gives a few extra insights about how it relates to Ogmios and some good takes from it. Each request in Ogmios yields exactly one response, might it be a _normal_ response or a fault. Furthermore, requests which relate to a particular protocol are guaranteed to yield responses in the same order. 

### Requests

Requests messages are sent by client applications (you) to Ogmios. Requests have 3 fields of interest: `methodname`, `args` and `mirror`. The `methodname` identifies the request itself and translates to a corresponding message of one of the 3 Ouroboros mini-protocols. Possible values for the `methodname` are described in [section 3 - Ouroboros mini-protocols](../../mini-protocols) and the complete reference is available in [section 4 - API Reference](../../api).

Here is an example of valid request message:

```json
{ 
    "type": "jsonwsp/request",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "FindIntersect",
    "args": { "points": [ "origin" ] },
    "mirror": { "step": "INIT" }
}
```

As you can see, the `methodname` specifies the method `FindIntersect` which relates to the _local chain-sync_ Ouroboros mini-protocol. This particular request expects one argument named `points`, which contains a list of points we want to intersect with (don't panic, this is explained in further details in the next section!). Another interesting, albeit optional, field is the `mirror` field. This is completely free-form and will be spit back identically by Ogmios in the response. This can be useful to keep track of states on the client application or pass in extra context to each request/response. Be careful though that anything you send for a request will come back in the response; send something big, get something big. 

### Responses

Let's start with a possible response to the request above:

```
{
    "type": "jsonwsp/response",
    "version": "1.0",
    "servicename": "ogmios",
    "methodname": "FindIntersect",
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
    "reflection": { "step": "INIT" }
}
```

The response kindly indicates which requests it corresponds to and has the same `methodname` as the matching request. It also gives a result under a specific field. Responses can have various results which are fully specified in [section 4. - API Reference](../../api). Also, notice the `reflection` field which reflects exactly the `mirror` field that was set for the request.

{{% notice info %}}
Ogmios' responses may correspond to possible errors that are part of the Ouroboros mini-protocols. For example, if you submit an invalid transaction, you'll get back a message of type `jsonwsp/response` because it is a valid message in the context of the mini-protocols. Yet, Ogmios will yield a message of type `jsonwsp/fault` if you submit an invalid Ogmios message (for instance, an unknown request constructor, or something that is not a valid JSON-WSP message).
{{% /notice %}}

## WebSocket

Ogmios uses WebSocket as a communication protocol. The WebSocket protocol is full-duplex, which means that Ogmios and clients can send messages to each other all at the same time. That is very convenient for clients who can pipeline many requests at once and process responses later on as they arrive. 

Therefore, to send a message to ogmios, you only need a WebSocket client! For example, using the well-established [ws](https://www.npmjs.com/package/ws) Node.js package and assuming a local instance of Ogmios started through Docker, you can interact with Ogmios as simply as:

```js
const WebSocket = require('ws');
const client = new WebSocket("ws://localhost:1337");

client.once('open', () => {
    const request = { 
        "type": "jsonwsp/request",
        "version": "1.0",
        "servicename": "ogmios",
        "methodname": "FindIntersect",
        "args": { "points": [ "origin" ] }
    };
    client.send(JSON.stringify(request));
});

client.on('message', function(msg) {
  const response = JSON.parse(msg);

  // do something with 'response'
});
```

That's all for the basics. The rest isn't really specific to Ogmios, but is about the Ouroboros mini-protocols themselves. See the next sections for a deep dive!
