#!/bin/sh -f
#
# Start script for SWI-Prolog that can  start the appropriate executable
# and make sure that the name of $argv[0]   is  not changed and thus the
# xpce binding through swipl.rc remains working.
#
# Of course it is possible to start the desired executable directly.

prog=`which "$0"`
me=`readlink -f "$prog"`
medir=`dirname "$me"`

os=`uname | tr A-Z a-z`
mach=`uname -m`
arch=$mach-$os
if [ ! -d $medir/$arch ]; then
  case "$mach" in
    x86_64|i?86)
      if [ -d $medir/i686-$os ]; then
	arch=i686-$os
      fi
      ;;
    *)
      echo "ERROR: swipl: Cannot find a matching executable"
      exit 1
  esac
fi

exec $medir/$arch/swipl "$@"
