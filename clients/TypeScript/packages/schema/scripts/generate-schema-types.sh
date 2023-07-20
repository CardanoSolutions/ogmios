#!/usr/bin/env bash

cat ../../../../server/ogmios.json | \
  sed "s/ogmios.json#/#/g" | \
  sed "s/cardano.json#/..\/..\/..\/..\/docs\/static\/cardano.json#/g" | \
  json2ts -o src/index.ts --enableBigInt
