#!/bin/sh

# This script is a simple copy replacement for rsync.  If you are actively
# developing, I strongly suggest to get rsync from
# http://rsync.samba.org/rsync/
#
# This script will do fine for one-time installation, given the usage
# enforced by the Makefile

exclude=""
tmpexcl=exclude$$

doopts=true
while $doopts = true; do
    case "$1" in
	-a)
	    shift
	    ;;
	-C)
	    shift
	    ;;
	--exclude-from=*)
	    exclude=`echo $1 | awk -F = '{print $2}'`
	    shift;
	    ;;
	*)  doopts=false
	    ;;
    esac
done

src="$1"
dest="$2"

if [ ! -r $src ]; then
    echo "ERROR: Cannot read $src"
    exit 1;
fi
if [ ! -d $dest ]; then
    echo "ERROR: $dest is not a directory"
    exit 1;
fi

srcd=`dirname $src`
srcf=`basename $src`
(cd $srcd; find $srcf \( -name '*~' -o \
			 -name '*.qlf' -o \
			 -name CVS \) ) > $tmpexcl

tar cf - -C $srcd -X $exclude -X $tmpexcl $srcf | \
	(cd $dest && tar xfBp -)
rm -f $tmpexcl
