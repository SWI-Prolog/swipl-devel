#!/bin/sh

deployqt=

usage()
{ echo "Usage: $0 app"
  exit 1
}

done=no
while [ $done = no ]; do
    case "$1" in
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

# Bundle layout (since the framework refactor):
#
#   swipl-win.app/Contents/
#     MacOS/swipl-win              (EPILOG GUI)
#     MacOS/swipl, swipl-ld        (CLI tools)
#     Frameworks/swipl.framework/Versions/A/
#       swipl                       (libswipl)
#       PlugIns/swipl/*.so          (foreign extensions)
#       Frameworks/*.dylib          (bundled third-party libs)
#       Resources/swipl/            (Prolog home)

fwroot=$app/Contents/Frameworks/swipl.framework/Versions/A
moduledir=$fwroot/PlugIns/swipl
frameworkdir=$fwroot/Frameworks

if [ ! -d "$frameworkdir" ]; then
    mkdir -p "$frameworkdir"
fi

printf "Bundling Macports/Homebrew dylibs into $frameworkdir\n"

fixup_files()
{
    # Ad-hoc re-sign any Mach-O we modify.  install_name_tool
    # invalidates the existing signature, and on Apple Silicon the
    # loader refuses to map unsigned/invalidated code, so the dylib
    # has to carry at least an ad-hoc signature.  A proper Developer
    # ID signature can be applied over the top later.
    resign() {
        for f in "$@"; do
            codesign --force --sign - "$f" >/dev/null 2>&1
        done
    }

    changeset="$*"
    while [ ! -z "$changeset" ]; do
	newchanges=
	for f in $changeset; do
	    case "$(file $f)" in
		*Mach-O*)
		    opt_dep=$(otool -L $f |
				  grep '\(/opt/local\|/deps/\|/opt/homebrew\)' |
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
				install_name_tool -id \
				    "@rpath/swipl.framework/Versions/A/Frameworks/$(basename $dep)" \
				    "$file"
				newchanges="$newchanges $file"
				printf 'ok\n'
			    fi
			    change="$change -change $dep @rpath/swipl.framework/Versions/A/Frameworks/$(basename $dep)"
			done
			if [ ! -z "$change" ]; then
			    install_name_tool $change $f
			    resign "$f"
			fi
		    fi
		    ;;
		*)
	    esac
	done
	# Re-sign the newly added libraries (their -id rewrite invalidated
	# the original signature).
	if [ ! -z "$newchanges" ]; then
	    resign $newchanges
	fi
	changeset="$newchanges"
    done
}

fixup_files $(echo $moduledir/*) $app/Contents/MacOS/swipl $fwroot/swipl
fixup_files $(echo $frameworkdir/*)

# Code signing (Developer ID Application).  Sign inner-most first.
# The codesign in this script is the legacy ad-hoc path; proper
# notarization is handled outside this script.

if [ ! -z "$CODESIGN_ID" ]; then
    loginkeychain="$(security login-keychain | tr -d ' "')"

    sign()
    { xcrun codesign -f -s $CODESIGN_ID --timestamp --options=runtime $* || return 1
    }

    printf "Code signing using $CODESIGN_ID ...\n"
    security unlock-keychain $loginkeychain || exit 1

    sign $(find $app \( -name '*.dylib' -o -name '*.so' \) ) || exit 1
    sign $fwroot/swipl                          || exit 1
    sign $app/Contents/Frameworks/swipl.framework || exit 1
    sign $app/Contents/MacOS/swipl-ld           || exit 1
    sign $app/Contents/MacOS/swipl              || exit 1
    sign $app/Contents/MacOS/swipl-win          || exit 1
    sign $app                                   || exit 1

    security lock-keychain $loginkeychain
fi
