#!/bin/sh
################################################################
# plld -- link C and Prolog files to a stand-alone executable
#
# Author: Jan Wielemaker
# E-mail: jan@swi.psy.uva.nl
#
# Copyright 1996 University of Amsterdam, all rights reserved
#
# For documentation, see the SWI-Prolog manual and man plld
################################################################

program="$0"

PL=pl
LD=
clibs=
clibdirs=
ldflags=
cfiles=
cflags="-D__SWI_PROLOG__ -D__SWI_EMBEDDED__"
plfiles=
plout="pl.out$$"			# temporary file for saved-state
cout="c.out$$"				# temporary file for emulator
out="a.out"				# the real output file
plgoal='$welcome'			# initial goal (banner)
pltoplevel=prolog			# toplevel goal
plinitfile=none				# file to load at start
makestate=true
verbose=

PLLD=true				# for sub-programs
export PLLD

usage()
{ cat << _EOM_
usage:  $program -help"
	$program [options] c-files ... pl-files ..."

options:
	-o out		final output file (default = a.out)
	-goal goal	use 'goal' as the (Prolog) entry-point
	-toplevel goal	use 'goal' as the (Prolog) toplevel (default = prolog)
	-pl prolog	SWI-Prolog executable (default = pl)
	-ld linker	linker to be used (default = <CC used for prolog>)
	-nostate	Do not create a saved state: just link the foreign
			code to the Prolog kernel.
	-initfile file	use 'file' as profile file (default = none)
	-v		verbose: print commands executed

Dispatching:
	-g | -I* | -D* | -l* | -L* | *.o | *.c | *.C | *.cpp | *.cxx:
			arguments matching these patterns are passed to
			the C-compiler
	*.pl | *.qlf:
			arguments matching these patterns are passed to
			Prolog
_EOM_
}


while [ -n "$1" ]; do
  case "$1" in
    -help)
	usage;
	exit 0;;
    -nostate)
	makestate=false;;
    -goal)
	plgoal="$2";
	shift;;
    -toplevel)
	pltoplevel="$2";
	shift;;
    -initfile)
	plinitfile="$2";
	shift;;
    -o)
	out="$2";
	shift;;
    -pl)
	PL="$2";
	shift;;
    -ld)
	LD="$2";
	shift;;
    -v)
        verbose=true;;
    -l*)
	clibs="$clibs $1";;
    -L*)
        clibdirs="$clibdirs $1";;
    -g|-I*|-D*)
	cflags="$cflags $1";;
    *.o|*.c|*.C|*.cxx|*.cpp)
	cfiles="$cfiles $1";;
    *.pl|*.qlf)
        if [ -z "$plfiles" ]; then
	  plfiles="'$1'"
	else
	  plfiles="$plfiles, '$1'"
	fi;;
    *)	
	ldflags="$ldflags $1";;
  esac
  shift
done

if [ "$verbose" = true ]; then echo "%% $PL -dump-runtime-variables"; fi
if eval `$PL -dump-runtime-variables`; then
  true
else
  echo "plld: failed to get runtime variables from $PL"
  exit 1
fi

pllibdir="$PLBASE/runtime/$PLARCH"
ldflags="$PLLDFLAGS $ldflags"

cleanup ()
{ if [ "$verbose" = true ]; then echo "%% rm -f $cout $plout"; fi
  rm -f $cout $plout;
}

error()
{ cleanup;
  exit 1;
}

trap error 1 2 15

if [ "$makestate" = false ]; then cout="$out"; fi
if [ -z "$LD" ]; then LD="$CC"; fi

LIBS="-L$pllibdir $clibdirs $clibs -lpl $PLLIBS"
ccmd="$LD -o $cout -I$PLBASE/include $cflags $cfiles $ldflags $LIBS"
plflags="-O -f none -F none -g true"

if [ "$verbose" = true ]; then echo "%% $ccmd"; fi
if $ccmd; then true; else error; fi

if [ "$makestate" = false ]; then
  exit 0;
fi

if [ "$verbose" = true ]; then
  echo "%% $PL $plflags -t "'"'"consult([$plfiles]),qsave_program('$plout',[goal='$plgoal',toplevel='$pltoplevel',init_file='$plinitfile'])"'"';
fi
if $PL $plflags -t "consult([$plfiles]),qsave_program('$plout',[goal='$plgoal',toplevel='$pltoplevel',init_file='$plinitfile'])"; then
  true
else
  error
fi
if [ "$verbose" = true ]; then echo "%% cat $cout $plout > $out"; fi
if cat $cout $plout > $out; then true; else error; fi
if [ "$verbose" = true ]; then echo "%% chmod +x $out"; fi
if chmod +x $out; then true; else error; fi
cleanup
exit 0
