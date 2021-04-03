+++
title = "Building"
chapter = false
weight = 2
+++

{{% notice tip %}}
You may **skip this section** if you're using **Docker 🐳.**
{{% /notice %}}

## Pre-requisites 

Ogmios is built using the great Haskell build tool [stack](https://docs.haskellstack.org/en/stable/README/). You'll also need [git](https://git-scm.com/) to clone the source code, that is:
- `git 2.11.*`
- `stack 2.*.*`

Ogmios in itself is a rather small project, yet it's using library directly from the ouroboros-network, cardano-ledger-specs and cardano-node projects. This is handy for re-using existing logic, but comes at the cost of several system dependencies that are required for building everything. Some may already be installed on your system, but the complete list is: 

- `libsodium-dev 1.0.*`
- `libgmp-dev 6.1.*`
- `libssl-dev 1.1.*`
- `libpcre3-dev 2.8.*`
- `libsystemd-dev` 
- `zlib1g-dev 1.2.*`

## 🔨  Ogmios

Clone the git repository from Github:

```console
$ git clone git@github.com:KtorZ/cardano-ogmios.git && cd cardano-ogmios/server
```

Then, use Stack to compile the project source code from the 'server' repository:

```console
$ stack build ogmios
```

The first time, this may take a while as Stack needs to setup a compilation environment and to download a lot of dependencies. Subsequent executions are much faster.

From there, you can run Ogmios via stack using the `exec` command:

```console
$ stack exec -- ogmios --help
```

Alternatively, you can instrument Stack to copy the compiled executable elsewhere so that you can run Ogmios all by itself:

```console
$ stack install ogmios
$ ogmios --help
```

## 📚 Documentation

### API Reference

Install [json-schema-for-humans](https://github.com/coveooss/json-schema-for-humans) using `pip3`:

```console
$ pip3 install json-schema-for-humans
```

And then, run:

```console
$ cd docs && generate-schema-doc --no-link-to-reused-ref static/ogmios.wsp.json static/api-reference.html
```

The documentation will be generated to `static/api-reference.html`.
