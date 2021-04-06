#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

#                                                                              #
# -------------------------------- SNAPSHOT ---------------------------------- #
#                                                                              #

FROM haskell:8.10.4-stretch as snapshot
WORKDIR /build
RUN apt-get update && apt-get install --no-install-recommends -y \
  build-essential=12.3 \
  git=1:2.11.* \
  libgmp-dev=2:6.1.* \
  libssl-dev=1.1.* \
  libpcre3-dev=2:8.* \
  libsystemd-dev=232-* \
  libsodium-dev=1.0.* \
  zlib1g-dev=1:1.2.*

RUN stack upgrade --binary-version 2.5.1

COPY modules/cardano-client/package.yaml     modules/cardano-client/package.yaml
COPY modules/fast-bech32/package.yaml        modules/fast-bech32/package.yaml
COPY modules/git-th/package.yaml             modules/git-th/package.yaml
COPY modules/hspec-json-schema/package.yaml  modules/hspec-json-schema/package.yaml
COPY modules/json-wsp/package.yaml           modules/json-wsp/package.yaml

COPY package.yaml                            package.yaml
COPY stack.yaml                              stack.yaml
COPY resolver.yaml                           resolver.yaml
COPY .hpack.config.yaml                      .hpack.config.yaml
COPY .stylish-haskell.yaml                   .stylish-haskell.yaml

RUN stack setup
RUN stack build --only-snapshot

#                                                                              #
# --------------------------------- BUILD ------------------------------------ #
#                                                                              #

FROM snapshot as build
RUN stack build --only-dependencies
COPY . .
RUN stack install --flag "ogmios:production"

#                                                                              #
# ---------------------------------- RUN ------------------------------------- #
#                                                                              #

FROM debian:buster-slim

LABEL name=ogmios
LABEL description="A JSON-WSP WebSocket client for cardano-node"

COPY --from=build /root/.local/bin /bin
COPY --from=build /usr/lib/x86_64-linux-gnu/libsodium.so.18 /usr/lib/x86_64-linux-gnu/libsodium.so.18

RUN mkdir -p /etc/bash_completion.d
RUN ogmios --bash-completion-script ogmios > /etc/bash_completion.d/ogmios
RUN echo "source /etc/bash_completion.d/ogmios" >> ~/.bashrc

RUN ogmios --version

EXPOSE 1337/tcp
ENTRYPOINT ["ogmios"]
