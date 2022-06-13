#!/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# cardano-node-ogmios.sh
#
#   Runs a cardano-node and an ogmios instance side-by-side, and 'monitor' both processes. If one dies, exits.
#   This script is meant to be used within a container to provide a cardano-node+ogmios as a single service.
#
# Usage: ./cardano-node-ogmios.sh

ogmios \
  --host 0.0.0.0 \
  --node-config /config/cardano-node/config.json \
  --node-socket /ipc/node.socket &
ogmios_status=$?

if [ $ogmios_status -ne 0 ]; then
  echo "Failed to start ogmios: $ogmios_status"
  exit $ogmios_status
fi

cardano-node run \
  --topology /config/cardano-node/topology.json \
  --database-path /db \
  --port 3000 \
  --host-addr 0.0.0.0 \
  --config /config/cardano-node/config.json \
  --socket-path /ipc/node.socket
