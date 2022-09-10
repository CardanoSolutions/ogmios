#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

#                                                                              #
# ------------------------------- SETUP  ------------------------------------- #
#                                                                              #

ARG CARDANO_NODE_VERSION=1.35.3

FROM --platform=${TARGETPLATFORM:-linux/amd64} nixos/nix:2.11.0 as build

ARG CARDANO_CONFIG_REV=f1263df513f4cc5666bc49245e07fd3055097fee

RUN echo "substituters = https://cache.nixos.org https://hydra.iohk.io" >> /etc/nix/nix.conf &&\
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf

WORKDIR /app
RUN nix-shell -p git --command "git clone https://github.com/input-output-hk/cardano-configurations.git"

WORKDIR /app/ogmios
RUN nix-env -iA cachix -f https://cachix.org/api/v1/install && cachix use cardano-ogmios
COPY default.nix default.nix
COPY server server
RUN nix-build -A platform.amd64 -o dist
RUN cp -r dist/* . && chmod +w dist/bin && chmod +x dist/bin/ogmios
COPY scripts scripts

WORKDIR /app/cardano-configurations
RUN nix-shell -p git --command "git fetch origin && git reset --hard ${CARDANO_CONFIG_REV}"

#                                                                              #
# --------------------------- BUILD (ogmios) --------------------------------- #
#                                                                              #

FROM --platform=${TARGETPLATFORM:-linux/amd64} busybox:1.35 as ogmios

ARG NETWORK=mainnet

LABEL name=ogmios
LABEL description="A JSON WebSocket bridge for cardano-node."

COPY --from=build /app/ogmios/bin/ogmios /bin/ogmios
COPY --from=build /app/cardano-configurations/network/${NETWORK} /config

EXPOSE 1337/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check

STOPSIGNAL SIGINT
ENTRYPOINT ["/bin/ogmios"]

#                                                                              #
# --------------------- RUN (cardano-node & ogmios) -------------------------- #
#                                                                              #

FROM inputoutput/cardano-node:${CARDANO_NODE_VERSION} as cardano-node-ogmios

ARG NETWORK=mainnet
ENV TINI_VERSION v0.19.0

LABEL name=cardano-node-ogmios
LABEL description="A Cardano node, side-by-side with its JSON WebSocket bridge."

COPY --from=build /app/ogmios/bin/ogmios /bin/ogmios
COPY --from=build /app/cardano-configurations/network/${NETWORK} /config
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini
RUN chmod +x /tini && mkdir -p /ipc

WORKDIR /root

 # Ogmios, cardano-node, ekg, prometheus
EXPOSE 1337/tcp 3000/tcp 12788/tcp 12798/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check

STOPSIGNAL SIGINT
COPY scripts/cardano-node-ogmios.sh cardano-node-ogmios.sh
ENTRYPOINT ["/tini", "-g", "--", "/root/cardano-node-ogmios.sh" ]
