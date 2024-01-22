+++
title = "Configuring"
chapter = false
weight = 2
+++

Ogmios comes with a few options that are all configurable via the command-line. Default values for those options are meant to provide sensible behaviors that are expected in _most cases_. However, some cases may not fit your particular use-case so we recommend skimming through this short manual to understand what is configurable and the effect it has on the server.

## Logging & minimum severity levels

Ogmios comes with a multi-component tracing system, which lets you configure the minimum logging severity of various internal components:

Each component can be configured separately with a minimum severity level amongst the following values:

- **Debug**: enable all the component's logs, sometimes useful for troubleshooting a transient issue;
- **Info**: enable only most important logs, helpful for monitoring the application;
- **Notice**: enable only special logs for uncommon, albeit normal, operations;
- **Warning**: enable only warnings, which often indicates that something is about to go wrong in the system;
- **Error**: enable only error logs, when something has actually gone wrong;
- **Off**: simply turn logging off for that component.

{{% notice tip %}}
Usually, you'd want to use `Debug` or `Info` as a minimal severity since more information is always good when troubleshooting problems. Ogmios uses `Info` by default for all components.
{{% /notice %}}

Here's a table summarizing the logging options:

| Option                           | Description                                                                                                                     |
| ---                              | ---                                                                                                                             |
| `--log-level SEVERITY`           | A shorthand that configures the severity levels of ALL components at once.                                                      |
| `--log-level-health SEVERITY`    | Configure min severity level for the health sub-system that is periodically printing information about the state of the server. |
| `--log-level-metrics SEVERITY`   | Configure min severity level for the system metrics, mostly only used to print warnings & notices on start-up.                  |
| `--log-level-websocket SEVERITY` | Configure min severity level for the component managing the incoming websocket connections to the server.                       |
| `--log-level-server SEVERITY`    | Configure min severity level for the HTTP server managing direct connections via HTTP.                                          |
| `--log-level-options SEVERITY`   | Configure min severity level for anything regarding the server's configuration and options.                                     |

For example, if you want to completely disable the health logging and increase the websocket minimum severity you can use the following options:

```console
ogmios --log-level-health Off \
       --log-level-websocket Debug \
       ...
```

## CBOR outputs in responses

For some objects, Ogmios may conditionally include their CBOR-serialized representation in the response, encoded as a base16 text string. This is particularly useful when there exists multiple possible binary representations of those objects, and knowing the original bytes may be necessary to carry out specific operations (such as calculating a hash digest). There are currently three objects that can be configured to always include their CBOR representation: transaction, (native) scripts and metadatums.

| Flag                         | Description                                                                           |
| ---                          | ---                                                                                   |
| `--include-cbor`             | A shorthand to turn all three other flags on at once.                                 |
| `--include-transaction-cbor` | Always include a `cbor` field on transaction objects                                  |
| `--include-metadata-cbor`    | Always include a `cbor` field on metadatum objects                                    |
| `--include-script-cbor`      | Always include a `cbor` field on native scripts (already included for Plutus scripts) |


## Metadata schema

To keep things _simple_, Cardano tools oscillate between 2 (or actually 3, but let's not dwell on that right now) metadata formats. One is called the metadata "no-schema" and the other one is called the "detailed schema". In fact, metadata are encoded on-chain as a collection of CBOR objects abiding by a specific schema. Like JSON, CBOR is a self-describing structured serialization format which shares some overlaps with JSON. Thus, a subset of all the representable CBOR metadata can be _translated_ as JSON metadata. For example, a plain CBOR text string can easily be turned into a JSON string, and a CBOR list of integers can also easily become a JSON list of integers.

<table>
<tr>
<th>Default (no schema)</th>
<th>--metadata-detailed-schema</th>
</tr>
<tr>
<td>

```json
[1, 2, 3]
```

</td>
<td>

```json
{
  "list":
    [ { "int": 1 }
    , { "int": 2 }
    , { "int": 3 }
    ]
}
```

</td>
</tr>
</table>

This type of direct conversions is referred to as the "no-schema" conversion. Yet, it only works for compatible objects. Indeed, some CBOR-serialized data cannot be directly mapped into JSON. For example, CBOR can use plain maps or lists as objects keys; whereas JSON only ever supports text strings as object keys. To ensure that any CBOR value could be represented in JSON, another transformation was defined as the detailed schema. In a detailed schema, we no longer try to map the CBOR representation into JSON, but we use JSON to _describe_ the shape of the CBOR data using a small domain-specific language designed for that purpose.

While this approach is more verbose, it allows for representing _any_ binary metadata as JSON. However, since many DApp developers coming from JavaScript/TypeScript will usually produce JSON-compatible metadata, the no-schema format is applicable in many cases. Ogmios therefore uses no-schema as a default (falling back to plain CBOR when the conversion isn't possible). The behavior can be altered to use the detailed schema by passing the flag `--metadata-detailed-schema`.

## Strict JSON-RPC

The specification for [JSON-RPC 2.0](https://www.jsonrpc.org/specification) is relatively lenient/ambiguous regarding the presence of extra fields in the request / response payloads. Ogmios in particular adds an extra `method` to the responses so that they can easily be traced back to the responses. However, some libraries that implement JSON-RPC clients are quite strict and do not expect any fields other than those mentioned in the specs (thus preventing useful extensions!).

Ogmios, therefore, provides a flag `--strict-rpc` to disable the extra field in responses thus ensuring compliance with those stricter libraries.
