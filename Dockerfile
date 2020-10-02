#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

#                                                                              #
# --------------------------------- BUILD ------------------------------------ #
#                                                                              #

FROM haskell:8.6.5 as build
WORKDIR /build
RUN apt-get update && apt-get install --no-install-recommends -y \
  build-essential=12.3 \
  git=1:2.11.* \
  libgmp-dev=2:6.1.* \
  libssl-dev=1.1.* \
  libsystemd-dev=232-* \
  libsodium-dev=1.0.* \
  zlib1g-dev=1:1.2.*

RUN stack upgrade --binary-version 2.1.3

RUN mkdir -p \
  modules/cardano-client \
  modules/git-th \
  modules/json-wsp \
  modules/time-extra \
  ogmios-server
COPY modules/cardano-client/package.yaml modules/cardano-client
COPY modules/git-th/package.yaml modules/git-th
COPY modules/json-wsp/package.yaml modules/json-wsp
COPY modules/time-extra/package.yaml modules/time-extra
COPY ogmios-server/package.yaml ogmios-server
COPY stack.yaml snapshot.yaml .

RUN stack setup
RUN stack build --only-snapshot
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
