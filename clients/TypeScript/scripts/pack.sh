#!/usr/bin/env bash

set -euo pipefail

npm pack --cwd ./packages/schema  && \
npm pack --cwd ./packages/client  && \
npm pack --cwd ./packages/repl
