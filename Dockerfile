#  This Source Code Form is subject to the terms of the Mozilla Public
#  License, v. 2.0. If a copy of the MPL was not distributed with this
#  file, You can obtain one at http://mozilla.org/MPL/2.0/.

FROM ktorz/ogmios:circleci as build
COPY package.yaml .
RUN stack build --only-dependencies
COPY . /build/
RUN stack install

FROM frolvlad/alpine-glibc:alpine-3.11_glibc-2.30
RUN apk add gmp
COPY --from=build /root/.local/bin /root
ENTRYPOINT ["/root/ogmios"]
