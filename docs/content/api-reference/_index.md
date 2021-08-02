+++
title = "API Reference"
date = 2020-10-25T16:33:22+01:00
weight = 5
chapter = false
pre = "<b>5. </b>"
+++

Ogmios as a JSON-WSP service is entirely described using [JSON Schema - Draft 7](https://json-schema.org/). This can be fed into various tools to generate code, data-types or definitions in many languages. In particular, the server schema can be visualized [here](/api/interfaces/_cardano_ogmios_schema.Ogmios.html).

{{% notice tip %}}
The schema is large (200+ type definitions!) for it covers everything there's to cover in Cardano. If you get lost, look for the `Ogmios` object in the search bar to get back to the root of the schema!
{{% /notice %}}

If you're using TypeScript, take a look at the [API reference for the TypeScript client](/api/modules/_cardano_ogmios_client.html) or more specifically, documentation for one of the Ouroboros mini-protocols clients:

- [ChainSync](/api/modules/_cardano_ogmios_client.ChainSync.html)
- [StateQuery](/api/modules/_cardano_ogmios_client.StateQuery.html)
- [TxSubmission](/api/modules/_cardano_ogmios_client.TxSubmission.html)

{{% notice info %}}
Ogmios is tested against this JSON schema to make sure that it remains up-to-date as new features are added.
{{% /notice %}}

<p align="right">
  {{% button href="/ogmios.wsp.json" icon="fas fa-download" %}}Download JSON Schema{{% /button %}}
</p>
