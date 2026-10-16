#!/bin/bash
# usage: vmi-hot.sh <tail-call build dir> [N]
# Profile bench/ and a selection of the tabling benchmarks with a
# tail-calling VMI build and print the N (default 80) VMI/VMH functions
# with the most cycles as VMI_HOT(name) lines for src/pl-vmi-hot.h.
# Each workload counts equally (sum of the percentages).
B=$(realpath $1); N=${2-80}
T=$(mktemp -d)
BENCH=${BENCH-$HOME/src/prolog/tabling_benchmarks}
(cd $(dirname $0)/../../../bench &&
 perf record -e cycles:u -F 5000 -o $T/bench.data $B/src/swipl -O -g "run(0.5),halt" run.pl >/dev/null 2>&1)
for b in 50000shuttle 50krecognize 200pDoubleFirst testJoins mondial accVariants \
	 1000naiveReverse pyramid500 10kfib accManyComp; do
  (cd $BENCH && perf record -e cycles:u -F 5000 -o $T/$b.data $B/src/swipl -f none -O \
     --stack_limit=8g -g "entry(G),forall(between(1,3,_),(abolish_all_tables,G)),halt" \
     $b-swi.pl tabling.pl >/dev/null 2>&1)
done
for f in $T/*.data; do
  perf report -i $f --no-children --sort sym --stdio -q 2>/dev/null |
    awk '{gsub("%","",$1); print $1, $3}'
done | awk '{s[$2]+=$1} END{for(k in s) print s[k], k}' | sort -rn |
  awk -v n=$N '$2 ~ /^(instr|helper)_/ && ++c<=n {print "VMI_HOT(" $2 ")"}'
rm -rf $T
