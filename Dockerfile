#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Builder
FROM nixos/nix:2.5.1 as builder

ARG CACHIX_KEY
ARG CACHIX_CACHE

RUN echo -e "\
experimental-features = nix-command flakes \n\
substituters = https://hydra.iohk.io https://cache.nixos.org/ \n\
trusted-public-keys = \
    hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= \
    cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= \
" > /etc/nix/nix.conf

WORKDIR /ogmios
RUN nix shell nixpkgs\#git -c \
  git clone --depth 1 https://github.com/input-output-hk/cardano-configurations.git

# See https://github.com/NixOS/nix/issues/5797, should be fixed in next
# release
RUN mkdir -p /etc/ssl/certs/ && ln -s $NIX_SSL_CERT_FILE /etc/ssl/certs/

WORKDIR /ogmios/server
COPY . .
RUN chmod +x scripts/cachix.sh && scripts/cachix.sh $CACHIX_CACHE $CACHIX_KEY
RUN nix build -L .\#packages.x86_64-linux.ogmios-static
RUN cp result/bin/ogmios .

# Run ogmios & cardano-node
FROM inputoutput/cardano-node:1.31.0 as cardano-node-ogmios

ARG NETWORK=mainnet

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

COPY --from=builder /ogmios/server/ogmios /bin/ogmios
COPY --from=builder /ogmios/cardano-configurations/network/${NETWORK} /config

RUN mkdir /ipc

WORKDIR /root
COPY scripts/cardano-node-ogmios.sh cardano-node-ogmios.sh
# Ogmios, cardano-node, ekg, prometheus
EXPOSE 1337/tcp 3000/tcp 12788/tcp 12798/tcp
STOPSIGNAL SIGINT
ENTRYPOINT ["/root/cardano-node-ogmios.sh"]
