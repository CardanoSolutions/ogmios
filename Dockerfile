#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

#                                                                              #
# ------------------------------- SETUP  ------------------------------------- #
#                                                                              #

FROM nixos/nix:2.3.11 as build

RUN echo "substituters = https://cache.nixos.org https://hydra.iohk.io" >> /etc/nix/nix.conf &&\
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf

WORKDIR /app
RUN nix-shell -p git --command "git clone --depth 1 https://github.com/input-output-hk/cardano-configurations.git"

WORKDIR /app/ogmios
RUN nix-env -iA cachix -f https://cachix.org/api/v1/install && cachix use cardano-ogmios
COPY . .
RUN nix-build -A ogmios.components.exes.ogmios -o dist
RUN cp -r dist/* . && chmod +w dist/bin && chmod +x dist/bin/ogmios

#                                                                              #
# --------------------------- BUILD (ogmios) --------------------------------- #
#                                                                              #

FROM busybox as ogmios

ARG NETWORK=mainnet

LABEL name=ogmios
LABEL description="A JSON WebSocket bridge for cardano-node."

COPY --from=build /app/ogmios/bin/ogmios /bin/ogmios
COPY --from=build /app/cardano-configurations/network/${NETWORK} /config

EXPOSE 1337/tcp
STOPSIGNAL SIGINT
ENTRYPOINT ["/bin/ogmios"]

#                                                                              #
# --------------------- RUN (cardano-node & ogmios) -------------------------- #
#                                                                              #

FROM inputoutput/cardano-node:1.31.0 as cardano-node-ogmios

ARG NETWORK=mainnet

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

LABEL name=cardano-node-ogmios
LABEL description="A Cardano node, side-by-side with its JSON WebSocket bridge."

COPY --from=build /app/ogmios/bin/ogmios /bin/ogmios
COPY --from=build /app/cardano-configurations/network/${NETWORK} /config

RUN mkdir -p /ipc

WORKDIR /root
COPY scripts/cardano-node-ogmios.sh cardano-node-ogmios.sh
 # Ogmios, cardano-node, ekg, prometheus
EXPOSE 1337/tcp 3000/tcp 12788/tcp 12798/tcp
STOPSIGNAL SIGINT
CMD ["bash", "cardano-node-ogmios.sh" ]
