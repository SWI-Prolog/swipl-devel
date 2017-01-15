#!/bin/bash

preview=false
done=false

while [ done = "false" ]; do
  case "$1" in
    -p)  preview=true
         shift
         ;;
    *)	 done=true
	 ;;
  esac
done

if [ $preview = true ]; then

for f in $*; do
  clang-format $f | sed ':a;N;$!ba;s/{[\ ]*\n\s*/{ /g' | less
done

else

for f in $*; do
  cp $f $f.orig
  clang-format $f.orig | sed ':a;N;$!ba;s/{[\ ]*\n\s*/{ /g' > $f
done

fi
