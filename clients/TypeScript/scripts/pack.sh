#!/usr/bin/env bash

set -euo pipefail

yarn build

npm pack --cwd ./packages/schema  && \
npm pack --cwd ./packages/client  && \
npm pack --cwd ./packages/repl
