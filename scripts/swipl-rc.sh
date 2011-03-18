#!/bin/sh -f
#
# Start script for SWI-Prolog's swipl-ld utility
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
      echo "ERROR: swipl-rc: Cannot find a matching executable"
      exit 1
  esac
fi

exec $medir/$arch/swipl-rc "$@"
