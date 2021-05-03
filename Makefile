#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

.PHONY: clean server api-reference docker
default: server

# Configuration
#
# INSTALL_DIR:
#	Installation directory for executables. Default to $HOME/.local/bin
#
INSTALL_DIR ?= $(HOME)/.local/bin

SHELL = bash
GIT_REV = $(shell git rev-parse --verify --short HEAD)

#
# .PHONY
#

server: $(INSTALL_DIR)/ogmios-$(GIT_REV)

api-reference:
	@cd docs && generate-schema-doc --no-link-to-reused-ref static/ogmios.wsp.json static/api-reference.html
	@sed -i 's@</head>@<link rel="stylesheet" href="css/api-reference.css"/></head>@' docs/static/api-reference.html

docker:
	docker build server

clean:
	@cd server && stack clean

#
# .TARGETS
#

$(INSTALL_DIR)/ogmios-$(GIT_REV):
	@cd server && stack install ogmios --flag ogmios:production
	@mv $(shell stack path --local-bin)/ogmios $(INSTALL_DIR)/ogmios-$(GIT_REV)
	@ln -s $(INSTALL_DIR)/ogmios-$(GIT_REV) $(INSTALL_DIR)/ogmios
	@find $(INSTALL_DIR) -type f -name 'ogmios-*' ! -name 'ogmios-$(GIT_REV)' -delete
