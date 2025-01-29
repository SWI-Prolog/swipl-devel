#!/bin/bash
#
# Run a specific test in a loop until it fails. Often used together with
# taskset(1) to lock it (together with similar   loops)  on one or a few
# cores. Having more active threads than  available cores causes threads
# to preempt at arbitrary places.  For example:
#
#    taskset 1 ../script/looptest.sh thread

run=1

runtest()
{ echo "### Run $run ###"
  src/swipl "-f" "none" "--no-packs" "--on-error=status" "-q" "../tests/test.pl" "--no-core" "$1"
}

while runtest $1; do run=$(($run+1)); done
