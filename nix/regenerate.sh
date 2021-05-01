#!/usr/bin/env bash

set -euo pipefail
nix="$(dirname "$0")/default.nix"
$(nix-build "$nix" -A stackNixRegenerate --no-out-link) "$@"
