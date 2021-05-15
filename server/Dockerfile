#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

ARG GHC_VERSION=8.10.4
ARG CARDANO_NODE_VERSION=1.27.0
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1

#                                                                              #
# ------------------------------- SETUP  ------------------------------------- #
#                                                                              #

FROM haskell:${GHC_VERSION} as build-foundation
WORKDIR /build
RUN apt-get update && apt-get install --no-install-recommends -y \
  automake=1:1.16.* \
  build-essential=12.6 \
  g++=4:8.3.* \
  git=1:2.20.* \
  libffi-dev=3.* \
  libgmp-dev=2:6.1.* \
  libpcre3-dev=2:8.* \
  libncursesw5=6.* \
  libssl-dev=1.1.* \
  libsystemd-dev=241-* \
  libtool=2.4.* \
  make=4.2.* \
  pkg-config=0.29-* \
  zlib1g-dev=1:1.2.*

RUN cabal update
RUN stack upgrade --binary-version 2.5.1
RUN stack setup
RUN stack update --snapshot-location-base \
  https://github.com/KtorZ/cardano-ogmios/blob/ca0671b46bb90be1fed189e8c7d194617c26c280/server/resolver.yaml

WORKDIR /app/src
RUN git clone https://github.com/input-output-hk/libsodium.git &&\
  cd libsodium &&\
  git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV}
WORKDIR /app/src/libsodium
RUN ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

#                                                                              #
# ----------------------- BUILD (cardano-ogmios) ----------------------------- #
#                                                                              #

FROM build-foundation as build-ogmios

WORKDIR /app/src/ogmios

COPY .hpack.config.yaml .hpack.config.yaml
COPY .stylish-haskell.yaml .stylish-haskell.yaml

COPY modules/cardano-client/package.yaml \
     modules/cardano-client/package.yaml

COPY modules/fast-bech32/package.yaml \
     modules/fast-bech32/package.yaml

COPY modules/git-th/package.yaml \
     modules/git-th/package.yaml

COPY modules/hspec-json-schema/package.yaml \
     modules/hspec-json-schema/package.yaml

COPY modules/json-wsp/package.yaml \
     modules/json-wsp/package.yaml

COPY modules/json-via-show/package.yaml \
     modules/json-via-show/package.yaml

COPY resolver.yaml resolver.yaml
COPY stack.yaml stack.yaml
COPY package.yaml package.yaml

RUN stack build --only-snapshot

COPY . .

RUN stack build --only-dependencies
RUN stack install ogmios --flag ogmios:production

#                                                                              #
# --------------------------- RUN (ogmios-only) ------------------------------ #
#                                                                              #

FROM debian:buster-slim as ogmios

LABEL name=ogmios
LABEL description="A JSON-WSP WebSocket client for cardano-node"

COPY --from=build-ogmios /root/.local/bin/ogmios /bin/ogmios
COPY --from=build-ogmios /usr/local/lib/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so.23

RUN mkdir -p /etc/bash_completion.d
RUN ogmios --bash-completion-script ogmios > /etc/bash_completion.d/ogmios
RUN echo "source /etc/bash_completion.d/ogmios" >> ~/.bashrc

EXPOSE 1337/tcp
ENTRYPOINT ["ogmios"]

#                                                                              #
# ------------------------ BUILD (cardano-node) ------------------------------ #
#                                                                              #

FROM build-foundation as build-cardano-node

WORKDIR /app/src
RUN git clone https://github.com/input-output-hk/cardano-node.git &&\
  cd cardano-node &&\
  git fetch --all --tags &&\
  git checkout ${CARDANO_NODE_VERSION}
WORKDIR /app/src/cardano-node
RUN cabal install cardano-node \
  --install-method=copy \
  --installdir=/root/.local/bin \
  -f -systemd

#                                                                              #
# --------------------- RUN (cardano-node & ogmios) -------------------------- #
#                                                                              #

FROM debian:buster-slim as cardano-node-ogmios

LABEL name=cardano-node-ogmios
LABEL description="A JSON-WSP WebSocket client for cardano-node w/ a cardano-node"

COPY --from=build-foundation /usr/local/lib/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so.23
COPY --from=build-ogmios /root/.local/bin/ogmios /bin/ogmios
COPY --from=build-cardano-node /root/.local/bin/cardano-node /bin/cardano-node

WORKDIR /config

RUN wget https://hydra.iohk.io/build/6198010/download/1/mainnet-config.json
RUN wget https://hydra.iohk.io/build/6198010/download/1/mainnet-byron-genesis.json
RUN wget https://hydra.iohk.io/build/6198010/download/1/mainnet-shelley-genesis.json
RUN wget https://hydra.iohk.io/build/6198010/download/1/mainnet-topology.json

RUN wget https://hydra.iohk.io/build/6198010/download/1/testnet-config.json
RUN wget https://hydra.iohk.io/build/6198010/download/1/testnet-byron-genesis.json
RUN wget https://hydra.iohk.io/build/6198010/download/1/testnet-shelley-genesis.json
RUN wget https://hydra.iohk.io/build/6198010/download/1/testnet-topology.json

WORKDIR /root
COPY scripts/entrypoint.sh entrypoint.sh
CMD ./entrypoint.sh
