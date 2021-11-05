#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.
#                                                                              #
# ------------------------------- SETUP  ------------------------------------- #
#                                                                              #

FROM haskell:8.10.4 as setup
ARG CARDANO_NODE_REV=1.31.0
# https://github.com/input-output-hk/iohk-nix/blob/master/overlays/crypto/libsodium.nix
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
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
  zlib1g-dev=1:1.2.* \
  && rm -rf /var/lib/apt/lists/*

RUN cabal update

# Build IOHK's libsodium fork, needed by cardano-node
WORKDIR /app/src/libsodium
RUN git clone https://github.com/input-output-hk/libsodium.git /app/src/libsodium &&\
  git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV}
WORKDIR /app/src/libsodium
RUN ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# Build cardano-node.
WORKDIR /app/src/cardano-node
RUN git clone https://github.com/input-output-hk/cardano-node.git /app/src/cardano-node &&\
  git fetch --all --tags &&\
  git checkout ${CARDANO_NODE_REV}
WORKDIR /app/src/cardano-node
RUN cabal install cardano-node \
  --overwrite-policy=always \
  --install-method=copy \
  --installdir=/app/bin

#                                                                              #
# --------------------------- BUILD (ogmios) --------------------------------- #
#                                                                              #

FROM setup as build

WORKDIR /app/src/ogmios/server

COPY server/ .

RUN cabal install exe:ogmios \
  --overwrite-policy=always \
  --install-method=copy \
  --installdir=/app/bin

WORKDIR /app
RUN git clone --depth 1 https://github.com/input-output-hk/cardano-configurations.git

#                                                                              #
# --------------------------- RUN (ogmios-only) ------------------------------ #
#                                                                              #

FROM debian:buster-slim as ogmios

LABEL name=ogmios
LABEL description="A JSON WebSocket bridge for cardano-node."

COPY --from=build /usr/local/lib/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so.23
COPY --from=build /app/bin/ogmios /bin/ogmios

RUN mkdir -p /etc/bash_completion.d
RUN ogmios --bash-completion-script ogmios > /etc/bash_completion.d/ogmios
RUN echo "source /etc/bash_completion.d/ogmios" >> ~/.bashrc

EXPOSE 1337/tcp
ENTRYPOINT ["ogmios"]

#                                                                              #
# --------------------- RUN (cardano-node & ogmios) -------------------------- #
#                                                                              #

FROM debian:buster-slim as cardano-node-ogmios

ARG NETWORK=mainnet

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

LABEL name=cardano-node-ogmios
LABEL description="A JSON WebSocket bridge for cardano-node w/ a cardano-node."

# Ogmios, cardano-node, ekg, prometheus
EXPOSE 1337/tcp 3000/tcp 12788/tcp 12798/tcp

RUN apt-get update && apt-get install --no-install-recommends -y \
  wget=1.20.1-* \
  netbase=5.6 \
  ca-certificates=20200601* \
  && rm -rf /var/lib/apt/lists/*
RUN apt-get -y purge && apt-get -y clean && apt-get -y autoremove

COPY --from=setup /usr/local/lib/libsodium.so.23 /usr/lib/x86_64-linux-gnu/libsodium.so.23
COPY --from=setup /app/bin/cardano-node /bin/cardano-node
COPY --from=build /app/bin/ogmios /bin/ogmios
COPY --from=build /app/cardano-configurations/network/${NETWORK} /config

RUN mkdir /ipc

WORKDIR /root
COPY scripts/cardano-node-ogmios.sh cardano-node-ogmios.sh
CMD ["bash", "cardano-node-ogmios.sh" ]
