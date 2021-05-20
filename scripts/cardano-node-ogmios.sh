#!/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
  --socket-path ./node.socket&
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start cardano-node: $status"
  exit $status
fi

OGMIOS_NETWORK=$NETWORK ogmios \
  --host 0.0.0.0\
  --node-socket ./node.socket &
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start ogmios: $status"
  exit $status
fi

fg %1
