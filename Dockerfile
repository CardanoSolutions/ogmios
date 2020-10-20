#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

#                                                                              #
# --------------------------------- BUILD ------------------------------------ #
#                                                                              #

# To build the snapshot locally, see 'snapshot.Dockerfile'.
#
#     docker build . -f snapshot.Dockerfile -t ktorz/ogmios:snapshot
#
# Beware, it takes some time.
FROM ktorz/ogmios:snapshot as build
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
