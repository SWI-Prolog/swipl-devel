#!/bin/sh
# Post-install fixup for the macOS build.
#
# Usage: macosx_bundle_fixup.sh <install-prefix>
#
# <install-prefix> is the directory containing the split layout:
#
#   <prefix>/Applications/swipl-win.app/
#     Contents/MacOS/{swipl-win,swipl,swipl-ld}
#     Contents/Resources/
#
#   <prefix>/Library/Frameworks/swipl.framework/Versions/A/
#     swipl                       (libswipl)
#     PlugIns/swipl/*.so          (foreign extensions)
#     Frameworks/*.dylib          (bundled Homebrew/Macports deps)
#     Resources/swipl/            (Prolog home)
#
# The script copies third-party dylibs referenced via /opt/local,
# /opt/homebrew or $HOME/deps into the framework's Frameworks/
# directory, rewrites all install names to @rpath/swipl.framework/...,
# and ad-hoc resigns every Mach-O it touches so dyld accepts them on
# Apple Silicon.  A proper Developer-ID signing pass runs afterwards
# if $CODESIGN_ID is set.

usage()
{ echo "Usage: $0 <install-prefix>"
  exit 1
}

prefix=$1
if [ -z "$prefix" ] || [ ! -d "$prefix" ]; then
    usage
fi

app=$prefix/Applications/swipl-win.app
fwroot=$prefix/Library/Frameworks/swipl.framework/Versions/A
moduledir=$fwroot/PlugIns/swipl
frameworkdir=$fwroot/Frameworks

# Operate in best-effort mode: a `ninja install' run delivers the whole
# tree (both the app and the framework), while CPack productbuild stages
# each CMake component into a separate prefix and runs install() once
# per component, so only part of the tree is visible at a time.  Skip
# whatever is absent in this invocation instead of failing.

if [ ! -d "$app" ] && [ ! -d "$fwroot" ]; then
    # Nothing of ours in this prefix — silently nothing to do.
    exit 0
fi

printf "Fixing macOS bundle in %s\n" "$prefix"

if [ ! -d "$frameworkdir" ]; then
    mkdir -p "$frameworkdir"
fi

printf "Bundling Macports/Homebrew dylibs into %s\n" "$frameworkdir"

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
			    case "$dep" in
				*Python.framework/*)
				    # Don't bundle Python.framework.  It is
				    # large, signed as its own framework by
				    # python.org / MacPorts / Homebrew, and
				    # the loose `Python' binary that ends up
				    # next to our dylibs trips notarization
				    # ("not signed with a valid Developer ID
				    # certificate") because it is not a
				    # well-formed framework on disk.  Rewrite
				    # the reference to the canonical Python.org
				    # install location instead --- users who
				    # want janus install Python.org Python to
				    # /Library/Frameworks/Python.framework.
				    sys_dep=$(echo "$dep" |
					      sed 's|^.*/Python.framework|/Library/Frameworks/Python.framework|')
				    change="$change -change $dep $sys_dep"
				    ;;
				*)
				    # Rewrite to @rpath/<basename> --- shorter
				    # than the original /opt/local/lib/<basename>
				    # so it always fits in the upstream dylib's
				    # headerpad.  Consumers (libswipl, plugins,
				    # app exes) ship with rpaths that include
				    # swipl.framework/Versions/A/Frameworks/
				    # (set in cmake/Install.cmake); dyld walks
				    # the full call-chain rpath list so the
				    # copied dylibs themselves do not need any
				    # rpath of their own.
				    file="$frameworkdir/$(basename $dep)"
				    if [ ! -f $file ]; then
					printf "   Adding $dep ... "
					cp $dep $frameworkdir
					chmod 755 $file
					install_name_tool -id \
					    "@rpath/$(basename $dep)" \
					    "$file"
					newchanges="$newchanges $file"
					printf 'ok\n'
				    fi
				    change="$change -change $dep @rpath/$(basename $dep)"
				    ;;
			    esac
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

targets=
# Foreign extensions (each component pkg owns its own set)
if [ -d "$moduledir" ]; then
    for f in "$moduledir"/*; do
	[ -f "$f" ] && targets="$targets $f"
    done
fi
# Framework binary (only present in the Core_system component pkg)
[ -f "$fwroot/swipl" ] && targets="$targets $fwroot/swipl"
# App executables (only present in Core_system)
for exe in swipl swipl-ld swipl-win; do
    [ -f "$app/Contents/MacOS/$exe" ] && \
	targets="$targets $app/Contents/MacOS/$exe"
done
if [ ! -z "$targets" ]; then
    fixup_files $targets
fi
# Second pass: any dylibs newly placed in the bundled-deps dir may
# themselves reference further system libraries.
if [ -d "$frameworkdir" ]; then
    deps=
    for f in "$frameworkdir"/*; do
	[ -f "$f" ] && deps="$deps $f"
    done
    [ ! -z "$deps" ] && fixup_files $deps
fi

# Code signing (Developer ID Application).  Sign inner-most first.
# The codesign in this script is the legacy ad-hoc path; proper
# notarization is handled outside this script.

if [ ! -z "$CODESIGN_ID" ]; then
    loginkeychain="$(security login-keychain | tr -d ' "')"

    # Hardened runtime is required for notarization.  Pass the
    # entitlements plist (allow-jit, disable-library-validation, ...)
    # if one was supplied via the $ENTITLEMENTS env var.
    ent_opt=
    [ -n "$ENTITLEMENTS" ] && ent_opt="--entitlements $ENTITLEMENTS"

    sign()
    { xcrun codesign -f -s "$CODESIGN_ID" \
           --timestamp --options=runtime $ent_opt $* || return 1
    }

    printf "Code signing using $CODESIGN_ID ...\n"
    # Re-unlock as a safety net: a long build can exceed the keychain
    # idle-lock timeout between the initial unlock in
    # build_macos_gcc_universal and this codesign pass.  Prefer the
    # non-interactive form when $KEYCHAIN_PASSWORD was forwarded
    # (cmake -E env inherits the parent env); fall back to the
    # interactive form for standalone `ninja install' invocations.
    if [ -n "$KEYCHAIN_PASSWORD" ]; then
        security unlock-keychain -p "$KEYCHAIN_PASSWORD" $loginkeychain || exit 1
    else
        security unlock-keychain $loginkeychain || exit 1
    fi

    sign $(find $fwroot $app \( -name '*.dylib' -o -name '*.so' \) ) || exit 1
    sign $fwroot/swipl                                          || exit 1
    sign $prefix/Library/Frameworks/swipl.framework             || exit 1
    sign $app/Contents/MacOS/swipl-ld                           || exit 1
    sign $app/Contents/MacOS/swipl                              || exit 1
    sign $app/Contents/MacOS/swipl-win                          || exit 1
    sign $app                                                   || exit 1

    # Deliberately do NOT lock the keychain here: productbuild runs
    # after this script (in the `pkg' custom target) and also needs
    # the Installer private key.  Re-locking would force the user to
    # confirm access a second time, which fails outside an Aqua
    # session.  The OS auto-locks on its own timeout.
fi
