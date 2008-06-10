#!/bin/sh

cat >> version.h.new <<_EOM_
/*  Generate file that contains the version identifier from
    the "git describe" command.

    DO NOTE EDIT.   Check Makefile and mkversion.sh in src.
*/

_EOM_

gitversion=''
if v=`git describe 2>/dev/null`; then
    if [ ! -z $v ]; then
	gitversion=`echo $v | sed 's/^V//'`
	echo "#define GIT_VERSION "'"'$gitversion'"' >> version.h.new
    fi
fi

if [ ! -r version.h ]; then
    mv version.h.new version.h
elif cmp version.h version.h.new >/dev/null; then
    echo "No GIT version change"
    rm version.h.new
else
    echo "Updated GIT version"
    mv version.h.new version.h
fi
