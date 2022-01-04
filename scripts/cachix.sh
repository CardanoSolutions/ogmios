#!/usr/bin/env bash

if [[ -n "$1" ]]; then
    # `nix profile install` doesn't work??
    nix-env -iA nixpkgs.cachix
    if [[ -n "$2" ]]; then cachix authtoken "$2"; fi
    cachix use "$1"
fi
