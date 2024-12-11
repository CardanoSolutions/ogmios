#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

ARG CARDANO_NODE_IMAGE=ghcr.io/intersectmbo/cardano-node:10.1.3

#                                                                              #
# --------------------------- BUILD (ogmios) --------------------------------- #
#                                                                              #

FROM --platform=${TARGETPLATFORM:-linux/amd64} busybox:1.35 as ogmios

LABEL name=ogmios
LABEL description="A JSON WebSocket bridge for cardano-node."

COPY ./server/bin/ogmios /bin/ogmios
COPY ./server/config/network /config

RUN chmod +x /bin/ogmios

EXPOSE 1337/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check

STOPSIGNAL SIGINT
ENTRYPOINT ["/bin/ogmios"]

#                                                                              #
# --------------------- RUN (cardano-node & ogmios) -------------------------- #
#                                                                              #

FROM ${CARDANO_NODE_IMAGE} as cardano-node-ogmios

ARG NETWORK=mainnet
ENV TINI_VERSION v0.19.0

LABEL name=cardano-node-ogmios
LABEL description="A Cardano node, side-by-side with its JSON WebSocket bridge."

COPY ./server/bin/ogmios /bin/ogmios
COPY ./server/config/network/${NETWORK} /config
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini

RUN chmod +x /bin/ogmios
RUN chmod +x /tini && mkdir -p /ipc

WORKDIR /root

 # Ogmios, cardano-node, ekg, prometheus
EXPOSE 1337/tcp 3000/tcp 12788/tcp 12798/tcp
HEALTHCHECK --interval=10s --timeout=5s --retries=1 CMD /bin/ogmios health-check

STOPSIGNAL SIGINT
COPY scripts/cardano-node-ogmios.sh cardano-node-ogmios.sh
ENTRYPOINT ["/tini", "-g", "--", "/root/cardano-node-ogmios.sh" ]
