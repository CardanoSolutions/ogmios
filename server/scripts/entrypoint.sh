#!/bin/bash

if [ -z "$NETWORK" ]; then
  echo "'NETWORK' environment variable must be set."
  exit 1
fi

cardano-node run\
  --topology config/$NETWORK-topology.json\
  --database-path db/$NETWORK\
  --port 3000\
  --config config/$NETWORK-config.json\
  --socket-path ./node.socket &
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start cardano-node: $status"
  exit $status
fi

OGMIOS_NETWORK=$NETWORK ogmios --node-socket ./node.socket &
status=$?
if [ $status -ne 0 ]; then
  echo "Failed to start ogmios: $status"
  exit $status
fi

fg %1
