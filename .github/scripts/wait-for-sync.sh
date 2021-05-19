#!/usr/bin/env bash

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
done
