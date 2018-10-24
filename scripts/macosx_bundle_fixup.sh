app=$1

if [ -z "$app" ]; then
  echo "Usage: $0 app"
  exit 1
fi

ARCH=$($app/Contents/MacOS/swipl --arch)
moduledir=$app/Contents/swipl/lib/$ARCH
frameworkdir=$app/Contents/Frameworks

printf "Adding Macport dylibs to modules in $moduledir\n"

changeset="$(echo $moduledir/*)"
while [ ! -z "$changeset" ]; do
  newchanges=
  for f in $changeset; do
    case "$(file $f)" in
      *Mach-O*)
        opt_dep=$(otool -L $f | grep /opt/local | awk '{print $1}')
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
