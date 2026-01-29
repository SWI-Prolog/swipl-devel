#!/bin/zsh

# This script merges to single architecture builds into a MacOS universal (fat)
# build.   The idea is to
#
#  - Build a version using a gcc cross compiler for x68_64 in e.g. build.bundle-gcc-x86_64
#  - Build a version with native gcc in e.g. build.bundle-gcc
#  - In `build.bundle-gcc`, run
#    - `../scripts/macos-import-x86_64.sh ../build.bundle-gcc-x86_64
#    - `cpack`
#
# This script performs the following steps:
#
#  - For each MacOS executable or shared object
#    - Copy corresponding file from x86_64 to <file>.I
#    - Modify rpath pointing at x86_64 build dir to current build dir
#    - Remove rpath for libgcc
#    - Use `lipo` to merge the two files to a universal binary

from=$1

usage()
{
cat <<_EOF_
Usage: $0 dir
_EOF_
exit 1
}

if [ ! -d $from ]; then
    usage
fi

lsrpath()
{ otool -l "$@" | LC_ALL=C awk 'gsub(/^ +path | \(offset [0-9]+)$/, "") == 2'
}

delgccrpath()
{ rpath=$(lsrpath $1 | grep libgcc)
  if [ ! -z "$rpath" ]; then
      install_name_tool -delete_rpath $rpath $1 || exit 1
  fi
}

rpathfrom=$(realpath $from)/src
rpathto=$(realpath .)/src

files=( $(find . -type f \( -perm -0700 -o -name '*.dylib' -o -name '*.so' \) -a \! -name '*.pl' -a \! -name '*.la') )

for f in $files; do
    if file $f | grep Mach-O > /dev/null; then
	if [ -f $f -a -f $from/$f ]; then
	    echo -n "Joining $f and $from/$f ... "
	    in=$from/$f
	    dorm=
	    if [ $rpathfrom != $rpathto ]; then
		if lsrpath $from/$f | grep $rpathfrom > /dev/null; then
		    cp $from/$f $f.I
		    install_name_tool -rpath $rpathfrom $rpathto $f.I || exit 1
		    delgccrpath $f.I
		    in=$f.I
		    dorm=$in
		fi
	    fi
	    cp $f $f.amd64
	    delgccrpath $f
	    if lipo -create $f $in -output $f.U; then
		if [ ! -z "$dorm" ]; then
		    rm $dorm
		fi
		mv $f.U $f
		echo ok
	    else
		echo "FAILED"
	    fi
	else
	    echo "WARNING: Cannot find $from/$f"
	fi
    else
	echo "WARNING: $f is not a native executable"
    fi
done


