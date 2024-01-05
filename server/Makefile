# Change via `make NETWORK=... <cmd>`
NETWORK := mainnet

NODE_CFG := ./config/network/$(NETWORK)
NODE_DB := ${HOME}/.cache/cardano-node/$(NETWORK).db
NODE_RTS := -T -I0 -N2 -A16m -qb -qg --disable-delayed-os-memory-return
NODE_SOCKET := /tmp/node.socket

.PHONY: dev node lint

lint: # Apply linters & file formatters
	@echo "Running $(shell stylish-haskell --version)..."
	@stylish-haskell -i -c .stylish-haskell.yaml $(shell find src test app -type f -name '*.hs')
	@echo "Running $(shell hlint --version)..."
	@hlint src test app -h .hlint.yaml

dev: # Run local ogmios through cabal
	cabal run ogmios -- \
		--node-config $(NODE_CFG)/cardano-node/config.json \
		--node-socket $(NODE_SOCKET) $(filter-out $@,$(MAKECMDGOALS))

node: # Run a local node at the specified version.
	cardano-node$(NODE_VERSION:-) \
		run \
		--port 3000 \
		--database-path $(NODE_DB) \
		--topology $(NODE_CFG)/cardano-node/topology.json \
		--config $(NODE_CFG)/cardano-node/config.json \
		--socket-path $(NODE_SOCKET) +RTS $(NODE_RTS) -RTS

help:
	@echo "\033[1;4mTargets:\033[00m"
	@grep -E '^[a-zA-Z0-9 -]+:.*#'  Makefile | sort | while read -r l; do printf "  \033[1;32m$$(echo $$l | cut -f 1 -d':')\033[00m:$$(echo $$l | cut -f 2- -d'#')\n"; done
	@echo ""
	@echo "\033[1;4mConfiguration:\033[00m"
	@grep -E '^[a-zA-Z0-9_]+ := '  Makefile | sort | while read -r l; do printf "  \033[36m$$(echo $$l | cut -f 1 -d':')\033[00m:=$$(echo $$l | cut -f 2- -d'=')\n"; done
