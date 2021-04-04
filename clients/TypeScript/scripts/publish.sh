#!/usr/bin/env bash

set -euo pipefail

npm publish --cwd ./packages/schema && \
npm publish --cwd ./packages/client && \
npm publish --cwd ./packages/repl
