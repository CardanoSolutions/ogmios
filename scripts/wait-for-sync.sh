#!/usr/bin/env bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# wait-for-sync.sh
#
#   Wait for an ogmios / cardano-node to be synchronized with the network, up to a given threshold.
#
# Usage: ./wait-for-sync.sh OGMIOS_PORT THRESHOLD
#
# Examples:
#   ./wait-for-sync.sh 1337 1
#   ./wait-for-sync.sh 1338 0.95


set -eo pipefail

exitWithUsage () {
  echo -e "Error: missing argument(s)!\n"
  echo -e "Usage: $0 OGMIOS_PORT THRESHOLD"
  echo -e "    Wait until a running Ogmios server at OGMIOS_PORT reaches THRESHOLD network synchronization.\n"
  echo -e "Example: \n    $0 1338 0.95"
  exit 1
}

OGMIOS_PORT=$1
if [ -z "$OGMIOS_PORT" ]; then
  exitWithUsage
fi

THRESHOLD=$2
if [ -z "$THRESHOLD" ]; then
  exitWithUsage
fi

URL=http://localhost:$OGMIOS_PORT/health

showProgress () {
  N="$1"
  PER=$(printf "%.3f\n" "$(bc <<< "$N * 100")")
  LEN=$(printf "%.0f\n" "$(bc <<< "$N * 50")")

  BAR=""
  for ((i=1; i<=$LEN; i++))
  do
    BAR="$BAR▣"
  done
  for ((i=$LEN; i<=50; i++))
  do
    BAR="$BAR "
  done

  echo -en "Network synchronization: [$BAR] $PER%\r"
}

for (( ;; ))
do
  HEALTH=$(curl -sS $URL)

  CONNECTION_STATUS=$(sed 's/.*"connectionStatus":"\([a-z]\+\)".*/\1/' <<< $HEALTH)
  if ! [[ $CONNECTION_STATUS = "connected" ]] ; then
    echo "Waiting for node.socket..."
    sleep 5
  else
    NETWORK_SYNCHRONIZATION=$(sed 's/.*"networkSynchronization":\([0-9]\+\.\?[0-9]*\).*/\1/' <<< $HEALTH)

    RE='^[0-9]+\.?[0-9]*$'
    if ! [[ $NETWORK_SYNCHRONIZATION =~ $RE ]] ; then
       echo "error: unexpected response from /health endpoint: $HEALTH"
       exit 1
    fi

    showProgress $NETWORK_SYNCHRONIZATION
    PREDICATE=$(bc <<< "$NETWORK_SYNCHRONIZATION >= $THRESHOLD")

    if [ "$PREDICATE" -eq 1 ]; then
      exit 0
    else
      sleep 5
    fi
  fi
done
