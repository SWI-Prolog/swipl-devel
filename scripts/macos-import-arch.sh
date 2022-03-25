#!/bin/zsh

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

files=( $(find . -type f \( -perm -0700 -o -name '*.dylib' -o -name '*.so' \) -a \! -name '*.pl' -a \! -name '*.la') )

for f in $files; do
    if file $f | grep Mach-O > /dev/null; then
	if [ -f $f -a -f $from/$f ]; then
	    if lipo -info $f | grep -q x86_64; then
		echo "$f already contains x86_64"
	    else
		echo -n "Joining $f and $from/$f ... "
		if lipo -create $f $from/$f -output $f.U; then
		    mv $f.U $f
		    echo ok
		else
		    echo "FAILED"
		fi
	    fi
	else
	    echo "WARNING: Cannot find $from/$f"
	fi
    else
	echo "WARNING: $f is not a native executable"
    fi
done

