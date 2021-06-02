#!/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# generate-api-reference.sh
#
#   Re-generate the api-reference in the documentation. Used by the CI automated jobs.
#
#
# Usage: ./generate-api-reference.sh

SRC_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd $SRC_DIR/../docs && generate-schema-doc --no-link-to-reused-ref static/ogmios.wsp.json static/api-reference.html
sed -i 's@</head>@<link rel="stylesheet" href="css/api-reference.css"/></head>@' $SRC_DIR/../docs/static/api-reference.html
