#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

FROM haskell:8.6.5
RUN apt-get update && apt-get install -y \
  build-essential \
  git \
  libgmp-dev \
  libssl-dev \
  libtinfo-dev \
  libsystemd-dev \
  zlib1g-dev
RUN stack upgrade --binary-version 2.1.3

# Build in stages, using the given snapshot
WORKDIR /build
COPY snapshot.yaml stack.yaml Setup.hs /build/
COPY .docker/package.yaml /build/package.yaml
RUN stack setup
RUN stack build --only-snapshot

# Cleanup some heavy useless files to lighten the cache image
RUN rm -rf /root/.stack/pantry/hackage/00-index.tar*
