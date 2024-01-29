#!/bin/sh

deployqt=

usage()
{ echo "Usage: $0 [--deployqt=prog] app"
  exit 1
}

done=no
while [ $done = no ]; do
    case "$1" in
	--deployqt=*)
	    deployqt="$(echo $1 | sed 's/--deployqt=//')"
	    shift
	    ;;
	--*)
	    usage
	    ;;
	*)
	    done=yes
	    ;;
    esac
done

app=$1

if [ -z "$app" ]; then
    usage
fi

printf "Fixing app bundle in $app\n"

ARCH=$($app/Contents/MacOS/swipl --arch)
moduledir=$app/Contents/swipl/lib/$ARCH
frameworkdir=$app/Contents/Frameworks

if [ ! -z "$deployqt" ]; then
    dest=$(dirname $app)
    printf "Moving modules to avoid deployqt from interfering ..."
    mv $moduledir $dest
    printf "ok\n"
    printf "Running $(basename $deployqt) ...\n"
    $deployqt $app
    printf "Restoring modules\n"
    mv $dest/$(basename $moduledir) $(dirname $moduledir)
fi

printf "Adding Macport dylibs to modules\n"

changeset="$(echo $moduledir/*) $app/Contents/MacOS/swipl"
while [ ! -z "$changeset" ]; do
  newchanges=
  for f in $changeset; do
    case "$(file $f)" in
      *Mach-O*)
        opt_dep=$(otool -L $f |
		  grep '\(/opt/local\|/deps/\)' |
		  grep -v libjvm |
		  awk '{print $1}')
        if [ ! -z "$opt_dep" ]; then
          change=""
          for dep in $opt_dep; do
            file="$frameworkdir/$(basename $dep)"
            if [ ! -f $file ]; then
              printf "   Adding $dep ... "
              cp $dep $frameworkdir
              chmod 755 $file
              newchanges="$newchanges $file"
              printf 'ok\n'
            fi
            change="$change -change $dep @rpath/$(basename $dep)"
          done
          if [ ! -z "$change" ]; then
            install_name_tool $change $f
          fi
        fi
        ;;
      *)
    esac
  done
  changeset="$newchanges"
done
