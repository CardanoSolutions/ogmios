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
#
# Available ENV vars:
#   NETWORK                 Target network, either 'mainnet' or 'testnet'

set -m

if [ -z "$NETWORK" ]; then
  echo "'NETWORK' environment variable must be set."
  exit 1
fi

cardano-node run\
  --topology /config/$NETWORK-topology.json\
  --database-path db/$NETWORK\
  --port 3000\
  --host-addr 0.0.0.0\
  --config /config/$NETWORK-config.json\
  --socket-path /ipc/node.socket&
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start cardano-node: $status"
  exit $status
fi

OGMIOS_NETWORK=$NETWORK ogmios \
  --host 0.0.0.0\
  --node-socket ./ipc/node.socket &
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start ogmios: $status"
  exit $status
fi

fg %1
