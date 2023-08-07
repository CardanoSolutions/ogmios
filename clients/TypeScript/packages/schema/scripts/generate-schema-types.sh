#!/usr/bin/env bash

cat ../../../../server/ogmios.json | \
  sed "s/ogmios.json#/#/g" | \
  sed "s/cardano.json#/..\/..\/..\/..\/docs\/static\/cardano.json#/g" | \
  sed 's/"description": "An arbitrary JSON value that will be mirrored back in the response."//g' | \
  sed 's/"description": "Any value that was set by a client request in the .id. field."//g' | \
  json2ts -o src/index.ts --enableBigInt
