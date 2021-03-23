#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

FROM haskell:8.10.2-stretch as build
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

COPY ogmios-server/package.yaml              ogmios-server/package.yaml
COPY stack.yaml                              stack.yaml
COPY resolver.yaml                           resolver.yaml
COPY .hpack.config.yaml                      .hpack.config.yaml
COPY .stylish-haskell.yaml                   .stylish-haskell.yaml

RUN stack setup
RUN stack build --only-snapshot
