#!/bin/bash
# Adapted from https://raw.githubusercontent.com/ndmitchell/neil/master/misc/travis.sh

set -e

if [[ -z "${PACKAGE}" ]]; then
  echo "PACKAGE must be provided as ENV var."
  exit 1
fi

if [[ -z "${REPOSITORY}" ]]; then
  echo "REPOSITORY must be provided as ENV var."
  exit 1
fi

if [[ -z "${PLATFORM}" ]]; then
  echo "PLATFORM must be provided as ENV var."
  exit 1
fi

if [[ -z "${VERSION}" ]]; then
  echo "VERSION must be provided as ENV var."
  exit 1
fi

if [[ -z "${RELEASE}" ]]; then
  echo "RELEASE must be provided as ENV var."
  exit 1
fi

URL=https://github.com/$REPOSITORY/releases/download/$RELEASE/$PACKAGE-$VERSION-$PLATFORM.tar.gz
TEMP=$(mktemp --directory .$PACKAGE-XXXXX)

echo Downloading and running $PACKAGE-$VERSION FROM $URL

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP

$TEMP/$PACKAGE-$VERSION*/$PACKAGE $*

