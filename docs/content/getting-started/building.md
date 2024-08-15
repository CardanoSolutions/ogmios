+++
title = "Building / Installation"
chapter = false
weight = 1
+++

{{% notice tip %}}
You may **skip this section** if you're using **Docker 🐳.**
{{% /notice %}}

## With Homebrew

Ogmios is readily available and distributed through Homebrew. Simply do:

```
brew tap CardanoSolutions/formulas
brew install ogmios
```

## Pre-compiled static executable

All [Ogmios releases](https://github.com/CardanoSolutions/ogmios/releases) comes with downloadable **pre-compiled static executables** for Linux (amd64 & arm64). If you like living on the edge and wants to access a not-yet-released executable, see the [_continuous integration_ workflow](https://github.com/CardanoSolutions/ogmios/actions/workflows/continuous-integration.yaml) on Github. Build artifacts are indeed continuously produced though this automated workflow. Find them at the bottom of any workflow run:

![](/build-artifacts.png)

## Building manually

### Pre-requisites (Server)

Ogmios is built using the great Haskell build tool [cabal](https://cabal.readthedocs.io/en). You'll also need [git](https://git-scm.com/) to clone the source code, that is:

- `git 2.11.*`
- `cabal 3.6.*.*` and higher
- `ghc >=9.4.8 && <9.6`

{{% notice tip %}}
We recommend using [GHCup](https://www.haskell.org/ghcup/) to install cabal & the Haskell platform.
{{% /notice %}}

Ogmios is using library directly from the ouroboros-network, cardano-ledger and cardano-node projects. This is handy for re-using existing logic, but comes at the cost of several system dependencies that are required for building everything. Some may already be installed on your system, but the complete list is:

- `libffi-dev`
- `libgmp-dev 6.1.*`
- `libnuma-dev`
- `libpcre3-dev 2.8.*` (for Mac OS, see below)
- `libssl-dev 1.1.*`
- `libsystemd-dev`
- `llvm 1.11.*`
- `llvm-11-dev`
- `pkg-config 0.29.*`
- `zlib1g-dev 1.2.*`
- `libsodium-dev 1.0.*`  with VRF support (see below)
- `secp256k1` (see below)
- `blst` (see below)

#### `libsodium` with VRF support

A special patched version of libsodium to include VRF support. Note that, while this is necessary to run a validator node; it is not necessary for a simple application node serving data to your client. So here, it suffices to install `libsodium-dev 1.0.*`, and to add/create the following to your `cabal.project.local` (to be placed next to the current `cabal.project`:

  ```cabal
  package cardano-crypto-praos
    flags: -external-libsodium-vrf
  ```

#### `secp256k1`

A custom revision of bitcoin-core's secp256k1, with Schnorr signature support enabled:

- `autoconf 2.*`
- `libtool 2.*.*`

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

#### `blst`

A fancy crypto library for BLS12-381, a.k.a BLST:

```console
git clone https://github.com/supranational/blst
cd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

#### `pcre` (MacOS)

On **MacOS** only, you might need to specify extra `lib` & `include` directories for `pcre` depending on your installation. For example, if you used Homebrew with defaults, add the followings to your `cabal.project.local`:

```cabal
package pcre-light
  extra-include-dirs: /opt/homebrew/opt/pcre/include
  extra-lib-dirs: /opt/homebrew/opt/pcre/lib
```

### 🔨 Server

Clone the git repository from Github:

```console
$ git clone --depth 1 --recursive --shallow-submodules git@github.com:cardanosolutions/ogmios.git
$ cd cardano-ogmios/server
```

Then, use cabal to compile the project source code from the `server` directory:

```console
$ cabal update
$ cabal build ogmios:exe:ogmios
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

### 🔨 TypeScript Client

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

### 📚 Documentation

The documentation is built using [Hugo](https://gohugo.io/documentation/). You'll need an `-extended` version of hugo with version `>= 0.96.0`. Then, simply run:

```
hugo

# or, alternatively for a development setup on http://localhost:1313

hugo serve
```

in the `./docs` folder.


In addition, the TypeScript API reference can be generated using `yarn docs` from within the `client/TypeScript` folder. This will automatically create and copy files in the right folder for `hugo` to pick up.

### Testing

{{% notice tip %}}
You may **skip this section** if you're not interested in contributing to Ogmios.
{{% /notice %}}

#### 🔧 Unit Tests

First, make sure to pull and update git submodules:

```console
$ git submodule update --init
```

Then, simply use cabal as follows:

```console
$ cabal test all
```

#### 💨 End-to-end Tests

##### Pre-requisites

- A synchronized node running on the **preview** network.
- Ogmios running and listening on `:1337`.

##### Running the tests

From the project root, run:

```console
$ cd clients/TypeScript
$ yarn
$ yarn test
```
