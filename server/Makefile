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

shell: # Start a nix-shell with appropriate context
	@nix develop --no-write-lock-file --refresh "github:CardanoSolutions/devx#ghc94-static-minimal-iog"

dev: # Run local ogmios through cabal
	cabal run ogmios -- \
		--node-config $(NODE_CFG)/cardano-node/config.json \
		--node-socket $(NODE_SOCKET) $(filter-out $@,$(MAKECMDGOALS))

formula: # Re-generate Homebrew formula
ifeq ($(VERSION),)
	@echo "Missing \033[1;33mVERSION\033[00m argument."
else
ifneq ($(filter v%,$(VERSION)),)
	@echo "Provide \033[1;33mVERSION\033[00m without \033[1;33m\"v\"\033[00m prefix."
else
	@LINUX_AARCH64="$(shell find v$(VERSION) -type f -name "*aarch64-linux.zip" | sha256sum | sed "s/[^0-9a-f]*//g")"; \
	LINUX_X86_64="$(shell find v$(VERSION) -type f -name "*x86_64-linux.zip" | sha256sum | sed "s/[^0-9a-f]*//g")"; \
	MACOS_AARCH64="$(shell find v$(VERSION) -type f -name "*aarch64-macos.zip" | sha256sum | sed "s/[^0-9a-f]*//g")"; \
	cat ../.github/formula.rb.template | sed \
	  -e "s/%VERSION%/$(VERSION)/g" \
		-e "s/%LINUX_AARCH64%/$${LINUX_AARCH64}/g" \
		-e "s/%LINUX_X86_64%/$${LINUX_X86_64}/g" \
		-e "s/%MACOS_AARCH64%/$${MACOS_AARCH64}/g"
endif
endif

bump: # Bump server & TypeScript client versions.
ifeq ($(VERSION),)
	@echo "Missing \033[1;33mVERSION\033[00m argument."
else
	@echo "Bumping versions to $(VERSION)"
	@sed -i '' 's/^version:\([^0-9]*\).*/version:\1$(VERSION)/' package.yaml
	@sed -i '' 's/^version:\([^0-9]*\).*/version:\1$(VERSION)/' ogmios.cabal
	@sed -i '' 's/"version": "[0-9]\.[0-9]\.[0-9]",/"version": "$(VERSION)",/' ../clients/TypeScript/package.json
	@sed -i '' 's/"version": "[0-9]\.[0-9]\.[0-9]",/"version": "$(VERSION)",/' ../clients/TypeScript/packages/**/package.json
	@sed -i '' 's/\("@cardano-ogmios\/.*":\) "[0-9]\.[0-9]\.[0-9]",/\1 "$(VERSION)",/' ../clients/TypeScript/packages/**/package.json
	@echo "All set. Don't forget to ALSO update the asset files for the documentation!"
endif

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
