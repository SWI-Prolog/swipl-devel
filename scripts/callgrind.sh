#!/bin/bash
#
# This  script is  intended to  run  Prolog goals  under valgrind  for
# performance analysis.  This is to  be combined with valgrind.pl from
# https://github.com/JanWielemaker/my-prolog-lib.  For example:
#
# ```
# ../scripts/callgrind.sh src/swipl [option ...] [arg ...]
# ?- cg(Goal).
# ?- halt.
# ```

tool=--tool=callgrind
opts="--dump-instr=yes --collect-jumps=yes"

done=false
while [ $done = false ]; do
    case "$1" in
	--tool=cachegrind)
	    tool="$1"
	    opts=" --cache-sim=yes"
	    shift
	    ;;
	*)
	    done=true
	    ;;
    esac
done

echo "Running valgrind $tool $opts --instr-atstart=no $*"
valgrind $tool $opts --instr-atstart=no $*
kcachegrind $(ls -tr callgrind.out.* | tail -1)
