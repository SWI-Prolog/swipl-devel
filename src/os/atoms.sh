#!/bin/bash

# This script filters the ATOM_declarations that are actually used from
# a set of C source-files, copying their string-value from ../ATOMS
#
# Usage:
# 	./atoms.sh *.[ch]

inputs="$*"

astring()
{ grep "^A $1\>" ../ATOMS | awk '{print $3}'
}

atoms=$(grep '\<ATOM_' $inputs | sed 's/\<ATOM/\nATOM/g' | \
		grep '^ATOM_' | sed 's/^ATOM_\([a-zA-Z0-9_]*\).*/\1/' | sort -u)

for a in $atoms; do
  echo "A $a $(astring $a)"
done

