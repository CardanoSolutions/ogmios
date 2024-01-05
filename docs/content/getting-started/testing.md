+++
title = "Testing"
chapter = false
weight = 3
+++

{{% notice tip %}}
You may **skip this section** if you're not interested in contributing to Ogmios.
{{% /notice %}}

## 🔧 Unit Tests

First, make sure to pull and update git submodules:

```console
$ git submodule update --init
```

Then, simply use cabal as follows:

```console
$ cabal test all
```

## 💨 End-to-end Tests

### Pre-requisites

- A synchronized node running on the **preview** network.
- Ogmios running and listening on `:1337`.

### Running the tests

From the project root, run:

```console
$ cd clients/TypeScript
$ yarn
$ yarn test
```
