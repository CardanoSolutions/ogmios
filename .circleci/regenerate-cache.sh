#!/usr/bin/env bash
#
# Usage: regenerate-cache [options]
#
# Options: []

TAG=ktorz/ogmios:circleci
CWD=$(dirname "$0")

docker build -t $TAG -f $CWD/cache.Dockerfile .
docker push $TAG
