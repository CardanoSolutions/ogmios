+++
title = "Building"
chapter = false
weight = 2
+++

{{% notice tip %}}
You may **skip this section** if you're using **Docker 🐳.**
{{% /notice %}}

## Pre-requisites (Server)

Ogmios is built using the great Haskell build tool [cabal](https://cabal.readthedocs.io/en). You'll also need [git](https://git-scm.com/) to clone the source code, that is:
- `git 2.11.*`
- `cabal 3.6.*.*` and higher

Ogmios in itself is a rather small project, yet it's using library directly from the ouroboros-network, cardano-ledger-specs and cardano-node projects. This is handy for re-using existing logic, but comes at the cost of several system dependencies that are required for building everything. Some may already be installed on your system, but the complete list is:

- `libsodium-dev 1.0.*`
- `libgmp-dev 6.1.*`
- `libssl-dev 1.1.*`
- `libpcre3-dev 2.8.*`
- `libsystemd-dev`
- `zlib1g-dev 1.2.*`
- A custom revision of bitcoin-core's secp256k1, with Schnorr signature support enabled:
  ```console
  git clone https://github.com/bitcoin-core/secp256k1.git
  cd secp256k1
  git reset --hard ac83be33d0956faf6b7f61a60ab524ef7d6a473a
  ./autogen.sh
  ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental
  make
  make check
  sudo make install
  ```

## 🔨 Server

Clone the git repository from Github:

```console
$ git clone --depth 1 --recursive --shallow-submodules git@github.com:cardanosolutions/ogmios.git
$ cd cardano-ogmios/server
```

Then, use cabal to compile the project source code from the `server` directory:

```console
$ cabal build all
```

The first time, this may take a while as cabal needs to setup a compilation environment and to download a lot of dependencies. Subsequent executions are much faster.

From there, you can run Ogmios via cabal using the `run` command:

```console
$ cabal run ogmios:exe:ogmios -- --help
```

Alternatively, you can instrument cabal to copy the compiled executable elsewhere so that you can run Ogmios all by itself:

```console
$ cabal install ogmios:exe:ogmios --install-method=copy --overwrite-policy=always
$ ogmios --help
```

## 🔨 TypeScript Client

Clone the git repository from Github:

```console
$ git clone --depth 1 --recursive --shallow-submodules git@github.com:cardanosolutions/ogmios.git
$ cd cardano-ogmios/clients/TypeScript
```

Then, use Yarn to install dependencies and compile the project source code from the
`client/TypeScript` directory:

```console
$ yarn && yarn build
```

## 📚 Documentation

The documentation is built using [Hugo](https://gohugo.io/documentation/). You'll need an `-extended` version of hugo with version `>= 0.96.0`. Then, simply run:

```
hugo

# or, alternatively for an development setup on http://localhost:1313

hugo serve
```

in the `./docs` folder.


In addition, the TypeScript API reference can be generated using `yarn docs` from within the `client/TypeScript` folder. This will automatically create and copy files in the right folder for `hugo` to pick up.
