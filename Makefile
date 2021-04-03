#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

.PHONY: clean server docker
default: server

# Configuration
#
# INSTALL_DIR:
#	Installation directory for executables. Default to $HOME/.local/bin
#
INSTALL_DIR ?= $(shell stack path --local-bin)

SHELL = bash
GIT_REV = $(shell git rev-parse --verify --short HEAD)

#
# .PHONY
#

server: $(INSTALL_DIR)/ogmios-$(GIT_REV)

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
