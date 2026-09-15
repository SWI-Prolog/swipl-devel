#!/bin/bash
#
# Build the third-party libraries that the SWI-Prolog macOS bundle links
# against.  The result is a tree of _universal_ (x86_64 + arm64) libraries
# in $HOME/deps, which is handed to CMake as
#
#     cmake -DMACOSX_DEPENDENCIES_FROM=$HOME/deps ...
#
# ---------------------------------------------------------------------------
# Usage
# ---------------------------------------------------------------------------
#
# Run this from the directory holding the source tarballs (typically
# ~/src/deps, where this script is symlinked).  Source it to get the
# functions into your shell:
#
#     . macos-deps.sh
#     download_prerequisites       # fetch + unpack every source tarball
#     build_prerequisites          # everything: both arches, merge, install
#
# `build_prerequisites` runs these five steps, which can also be run
# individually:
#
#     build_arch arm64             # -> ./stage-arm64
#     build_arch x86_64            # -> ./stage-x86_64
#     build_universal              # CMake deps, fat -> ./stage-universal
#     merge_universal              # compose the three -> $HOME/deps
#     check_prerequisites          # verify the result
#
# Everything is staged; $HOME/deps is emptied and rebuilt only by
# merge_universal, as the very last step.
#
# ---------------------------------------------------------------------------
# How the universal build works
# ---------------------------------------------------------------------------
#
# Two strategies, by build system:
#
#  - CMake projects (utf8proc, SDL3, SDL3_image) accept
#    CMAKE_OSX_ARCHITECTURES and produce a fat binary in a single pass, so
#    they are built once (`build_universal`).
#
#  - autotools and meson projects cannot be configured for two CPUs at
#    once (OpenSSL picks a single target, gmp picks an ABI and assembly
#    path, pcre2 picks a JIT backend, config.h is per-CPU, ...).  They are
#    built twice -- once per arch -- and merged with `lipo`
#    (`build_arch` + `merge_universal`).
#
# Every pass configures with --prefix=$HOME/deps but installs with a
# DESTDIR of ./stage-<arch>, so install names and pkg-config files already
# name the final location and need no rewriting when the trees are merged.
# Each pass only ever looks inside its own staging tree for headers and
# libraries, so the passes cannot contaminate each other -- nor can the
# previous release still sitting in $HOME/deps.
#
# The x86_64 pass runs under `arch -x86_64`.  Rosetta makes uname(1) and
# any binary the build generates behave like a real Intel Mac, so configure
# scripts are not cross-compiling and their run-time feature tests give the
# right answers.  $CC carries the -arch flag because clang keeps defaulting
# to arm64 even inside a Rosetta shell.
#
# ---------------------------------------------------------------------------
# Notes
# ---------------------------------------------------------------------------
#
#  - Macports and Homebrew must not leak into the result.  Rather than
#    hiding them (this script used to `sudo chmod 0 /opt/local`), nothing
#    ever adds them to the include/library/pkg-config search paths, and
#    `check_prerequisites` verifies that no installed binary refers to
#    them.  Tools *are* taken from Macports: meson, ninja, cmake, pkg-config.
#
#  - cairo and pango declare their dependencies as meson `wrap`s that track
#    git *main* (glib, harfbuzz, fribidi, cairo).  Building those would pin
#    the bundle to a random upstream snapshot, so glib, harfbuzz, fribidi
#    and pixman are built here from release tarballs and both are
#    configured with --wrap-mode=nofallback: a missing dependency is an
#    error, never a silent download.
#
#  - Berkeley DB is deliberately held at 5.3: 6.x is AGPL, which would
#    conflict with distributing the bundle (e.g. as a Homebrew cask); 5.3
#    is under the Sleepycat licence.  Do not "update" it.
#
#  - OSSP uuid is no longer downloadable (the OSSP FTP host is gone).  The
#    tarball in the source directory is the only copy -- do not delete it.

################################################################
# Configuration
################################################################

GMP_VERSION=6.3.0
SSL_VERSION=3.6.4
ZLIB_VERSION=1.3.2
ARCHIVE_VERSION=3.8.9
UUID_VERSION=1.6.2
BDB_VERSION=5.3.28
ODBC_VERSION=2.3.14
PCRE2_VERSION=10.48
FFI_VERSION=3.8.0
YAML_VERSION=0.2.5
UTF8PROC_VERSION=2.11.3
SDL3_VERSION=3.4.16
SDL3_IMAGE_VERSION=3.4.6
PIXMAN_VERSION=0.46.4
GLIB_VERSION=2.88.3
FRIBIDI_VERSION=1.0.16
HARFBUZZ_VERSION=14.4.0
CAIRO_VERSION=1.18.4
PANGO_VERSION=1.58.2

# Where the finished universal libraries go, and where the per-arch builds
# are staged.  DEPS must not contain spaces.  The staging trees are plain
# DESTDIR images of $DEPS -- so $DEPS_ROOT is $DEPS_STAGE with $DEPS
# appended -- and live next to the sources, being build scratch.

DEPS="${DEPS:-$HOME/deps}"
DEPS_SRC="$(pwd)"
DEPS_ARCH="$(uname -m)"			# arm64, or x86_64 under `arch`
DEPS_STAGE="$DEPS_SRC/stage-$DEPS_ARCH"	# DESTDIR for this pass
DEPS_ROOT="$DEPS_STAGE$DEPS"		# where this pass's files really are

# The CMake libraries are fat already and so have no per-architecture pass,
# but they are staged the same way.  Nothing writes into $DEPS until
# merge_universal composes it out of these three trees.

DEPS_STAGE_FAT="$DEPS_SRC/stage-universal"
DEPS_ROOT_FAT="$DEPS_STAGE_FAT$DEPS"

export MACOSX_DEPLOYMENT_TARGET=10.15

# Build against the Command Line Tools rather than Xcode.app.  /usr/bin's
# libtool, otool, lipo etc. are shims that ask xcrun where the real tool
# is, and xcrun refuses to answer while the Xcode license has not been
# accepted.  The CLT toolchain has no such gate, and pins the SDK the
# bundle is built against independently of which Xcode is installed.

export DEVELOPER_DIR="${DEVELOPER_DIR:-/Library/Developer/CommandLineTools}"

# The CLT clang is the real compiler rather than the /usr/bin shim, and
# does not locate the SDK by itself; without this it compiles against a
# non-existent /usr/include.

export SDKROOT="${SDKROOT:-$DEVELOPER_DIR/SDKs/MacOSX.sdk}"

# Tools come from Macports (meson, ninja, cmake, pkg-config); libraries
# never do.  Note that $DEPS/bin is deliberately *not* on $PATH: nothing we
# build is needed to build the rest.

export PATH="$DEVELOPER_DIR/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/bin"

# The -arch flag lives in $CC rather than $CFLAGS so that every configure
# test, every libtool link and every meson probe agrees on the target,
# whatever the project does with $CFLAGS.

# OBJC/OBJCXX matter as much as CC here: glib (gosxutils.m) and other
# macOS backends compile Objective-C, and a build system that picks up
# $CC but defaults the Objective-C compiler to plain `clang' produces
# arm64 objects in the middle of an x86_64 build.  That surfaces much
# later as "ld: symbol(s) not found for architecture arm64".

export CC="clang -arch $DEPS_ARCH"
export CXX="clang++ -arch $DEPS_ARCH"
export OBJC="$CC"
export OBJCXX="$CXX"
export CFLAGS="-O2 -Wno-nullability-completeness"
export CXXFLAGS="$CFLAGS"
export CPPFLAGS="-I$DEPS_ROOT/include"
export LDFLAGS="-L$DEPS_ROOT/lib"

# CPATH and LIBRARY_PATH are honoured by the compiler itself, so they reach
# build systems that drop $CPPFLAGS/$LDFLAGS.  They must be overridden
# rather than left alone: a development shell typically points them at the
# finished $DEPS, which would feed the *other* architecture's libraries
# into this pass.

export CPATH="$DEPS_ROOT/include"
export LIBRARY_PATH="$DEPS_ROOT/lib"

# Keep Macports and Homebrew out of every dependency search.
#
# PKG_CONFIG_SYSROOT_DIR is essential, not cosmetic.  The staged .pc files
# describe the *final* location, so without it `pkg-config --cflags glib-2.0'
# answers -I$DEPS/include/glib-2.0 and every library that depends on another
# one silently compiles against whatever $DEPS happens to hold from the
# previous release -- the wrong version, and the wrong architecture.  The
# sysroot makes pkg-config prepend the staging tree to those paths, while the
# install names recorded in the libraries stay absolute and final.

export PKG_CONFIG_LIBDIR="$DEPS_ROOT/lib/pkgconfig:/usr/lib/pkgconfig"
export PKG_CONFIG_SYSROOT_DIR="$DEPS_STAGE"
export CMAKE_PREFIX_PATH="$DEPS_ROOT"
export CMAKE_IGNORE_PREFIX_PATH="/opt/local;/usr/local;/opt/homebrew"
unset PKG_CONFIG_PATH

NPROC=$(sysctl -n hw.ncpu)

################################################################
# Build helpers
#
# Each takes the source directory, then extra configure arguments.  All
# build out of tree in <srcdir>/build-<arch> so the two arch passes cannot
# see each other's object files or config.h.
################################################################

# autotools: ../configure && make && make install DESTDIR=...

autotools_build()
{ local dir="$1"; shift
  ( set -e
    cd "$DEPS_SRC/$dir"
    rm -rf "build-$DEPS_ARCH"
    mkdir -p "build-$DEPS_ARCH"
    cd "build-$DEPS_ARCH"
    ../configure --prefix="$DEPS" "$@"
    make -j$NPROC
    make install DESTDIR="$DEPS_STAGE"
  )
}

# meson: always out of tree, --wrap-mode=nofallback so that a missing
# dependency is an error rather than a download of some git branch.

meson_build()
{ local dir="$1"; shift
  ( set -e
    cd "$DEPS_SRC/$dir"
    rm -rf "build-$DEPS_ARCH"
    meson setup "build-$DEPS_ARCH" \
	  --prefix="$DEPS" \
	  --buildtype=release \
	  --default-library=shared \
	  --wrap-mode=nofallback \
	  -Dcmake_prefix_path="$DEPS_ROOT" \
	  "$@"
    meson compile -C "build-$DEPS_ARCH" -j $NPROC
    DESTDIR="$DEPS_STAGE" meson install -C "build-$DEPS_ARCH"
  )
}

# CMake: builds a fat binary in one pass and installs straight into $DEPS.
# CC/CXX must not carry -arch here; CMAKE_OSX_ARCHITECTURES drives it.

cmake_build()
{ local dir="$1"; shift
  ( set -e
    cd "$DEPS_SRC/$dir"
    rm -rf build-universal

    # Exported for the build as well as the configure step.  CMake does
    # not always give Objective-C sources the target's include
    # directories (SDL3_image's IMG_ImageIO.m gets no -I at all), so the
    # compiler's own search path has to carry them.
    export CC=clang CXX=clang++ OBJC=clang OBJCXX=clang++
    export CFLAGS="-O2 -Wno-nullability-completeness"
    export CXXFLAGS="$CFLAGS"
    export CPPFLAGS= LDFLAGS=
    export CPATH="$DEPS_ROOT_FAT/include"
    export LIBRARY_PATH="$DEPS_ROOT_FAT/lib"
    export PKG_CONFIG_LIBDIR="$DEPS_ROOT_FAT/lib/pkgconfig:/usr/lib/pkgconfig"
    export PKG_CONFIG_SYSROOT_DIR="$DEPS_STAGE_FAT"
    export CMAKE_PREFIX_PATH="$DEPS_ROOT_FAT"

    cmake -S . -B build-universal -G Ninja \
	  -DCMAKE_BUILD_TYPE=Release \
	  -DCMAKE_INSTALL_PREFIX="$DEPS" \
	  -DCMAKE_OSX_ARCHITECTURES="x86_64;arm64" \
	  -DCMAKE_OSX_DEPLOYMENT_TARGET="$MACOSX_DEPLOYMENT_TARGET" \
	  -DBUILD_SHARED_LIBS=ON \
	  "$@"
    cmake --build build-universal -j $NPROC
    DESTDIR="$DEPS_STAGE_FAT" cmake --install build-universal
  )
}

################################################################
# Downloading
################################################################

fetch()					# fetch url file
{ if [ -f "$2" ]; then
    echo "Already have $2"
  else
    curl -sSL --fail -o "$2.part" "$1" && mv "$2.part" "$2" || \
      { rm -f "$2.part"; echo "FAILED to download $2 from $1"; return 1; }
  fi
}

unpack()				# unpack file [dir]
{ local dir="${2-}"
  [ -n "$dir" ] && [ -d "$dir" ] && return 0
  tar xf "$1"
}

download_gmp()
{ fetch https://ftp.gnu.org/gnu/gmp/gmp-$GMP_VERSION.tar.bz2 \
	gmp-$GMP_VERSION.tar.bz2 &&
  unpack gmp-$GMP_VERSION.tar.bz2 gmp-$GMP_VERSION
}

download_ssl()
{ fetch https://github.com/openssl/openssl/releases/download/openssl-$SSL_VERSION/openssl-$SSL_VERSION.tar.gz \
	openssl-$SSL_VERSION.tar.gz &&
  unpack openssl-$SSL_VERSION.tar.gz openssl-$SSL_VERSION
}

download_zlib()
{ fetch https://zlib.net/zlib-$ZLIB_VERSION.tar.gz zlib-$ZLIB_VERSION.tar.gz &&
  unpack zlib-$ZLIB_VERSION.tar.gz zlib-$ZLIB_VERSION
}

download_libarchive()
{ fetch https://github.com/libarchive/libarchive/releases/download/v$ARCHIVE_VERSION/libarchive-$ARCHIVE_VERSION.tar.gz \
	libarchive-$ARCHIVE_VERSION.tar.gz &&
  unpack libarchive-$ARCHIVE_VERSION.tar.gz libarchive-$ARCHIVE_VERSION
}

# Berkeley DB 5.3.28 is still freely downloadable (unlike 6.x, which needs
# an Oracle account).  sha256 e0a992d740709892e81f9d93f06daf305cf73fb81b545afe72478043172c3628

download_libdb()
{ fetch https://download.oracle.com/berkeley-db/db-$BDB_VERSION.tar.gz \
	db-$BDB_VERSION.tar.gz &&
  unpack db-$BDB_VERSION.tar.gz db-$BDB_VERSION
}

# The OSSP FTP site is gone; the tarball in the source directory is the
# only copy, so we only unpack.

download_libuuid()
{ unpack uuid-$UUID_VERSION.tar.gz uuid-$UUID_VERSION
}

download_odbc()
{ fetch https://www.unixodbc.org/unixODBC-$ODBC_VERSION.tar.gz \
	unixODBC-$ODBC_VERSION.tar.gz &&
  unpack unixODBC-$ODBC_VERSION.tar.gz unixODBC-$ODBC_VERSION
}

download_libpcre2()
{ fetch https://github.com/PCRE2Project/pcre2/releases/download/pcre2-$PCRE2_VERSION/pcre2-$PCRE2_VERSION.tar.gz \
	pcre2-$PCRE2_VERSION.tar.gz &&
  unpack pcre2-$PCRE2_VERSION.tar.gz pcre2-$PCRE2_VERSION
}

download_libffi()
{ fetch https://github.com/libffi/libffi/releases/download/v$FFI_VERSION/libffi-$FFI_VERSION.tar.gz \
	libffi-$FFI_VERSION.tar.gz &&
  unpack libffi-$FFI_VERSION.tar.gz libffi-$FFI_VERSION
}

download_libyaml()
{ fetch https://github.com/yaml/libyaml/releases/download/$YAML_VERSION/yaml-$YAML_VERSION.tar.gz \
	yaml-$YAML_VERSION.tar.gz &&
  unpack yaml-$YAML_VERSION.tar.gz yaml-$YAML_VERSION
}

download_utf8proc()
{ fetch https://github.com/JuliaStrings/utf8proc/archive/refs/tags/v$UTF8PROC_VERSION.tar.gz \
	utf8proc-$UTF8PROC_VERSION.tar.gz &&
  unpack utf8proc-$UTF8PROC_VERSION.tar.gz utf8proc-$UTF8PROC_VERSION
}

download_sdl3()
{ fetch https://github.com/libsdl-org/SDL/releases/download/release-$SDL3_VERSION/SDL3-$SDL3_VERSION.tar.gz \
	SDL3-$SDL3_VERSION.tar.gz &&
  unpack SDL3-$SDL3_VERSION.tar.gz SDL3-$SDL3_VERSION
}

download_sdl3_image()
{ fetch https://github.com/libsdl-org/SDL_image/releases/download/release-$SDL3_IMAGE_VERSION/SDL3_image-$SDL3_IMAGE_VERSION.tar.gz \
	SDL3_image-$SDL3_IMAGE_VERSION.tar.gz &&
  unpack SDL3_image-$SDL3_IMAGE_VERSION.tar.gz SDL3_image-$SDL3_IMAGE_VERSION
}

download_pixman()
{ fetch https://cairographics.org/releases/pixman-$PIXMAN_VERSION.tar.gz \
	pixman-$PIXMAN_VERSION.tar.gz &&
  unpack pixman-$PIXMAN_VERSION.tar.gz pixman-$PIXMAN_VERSION
}

# glib always links libintl ("We require gettext to always be present"),
# which macOS does not provide, and -Dnls=disabled does not change that.
# proxy-libintl supplies the stubs.  Unlike the wraps cairo and pango use
# for their major dependencies, this one is a hash-checked release
# tarball, so it is safe to use -- fetch it here so that the build itself
# needs no network.

download_glib()
{ fetch https://download.gnome.org/sources/glib/${GLIB_VERSION%.*}/glib-$GLIB_VERSION.tar.xz \
	glib-$GLIB_VERSION.tar.xz &&
  unpack glib-$GLIB_VERSION.tar.xz glib-$GLIB_VERSION &&
  ( cd glib-$GLIB_VERSION && meson subprojects download proxy-libintl )
}

download_fribidi()
{ fetch https://github.com/fribidi/fribidi/releases/download/v$FRIBIDI_VERSION/fribidi-$FRIBIDI_VERSION.tar.xz \
	fribidi-$FRIBIDI_VERSION.tar.xz &&
  unpack fribidi-$FRIBIDI_VERSION.tar.xz fribidi-$FRIBIDI_VERSION
}

download_harfbuzz()
{ fetch https://github.com/harfbuzz/harfbuzz/releases/download/$HARFBUZZ_VERSION/harfbuzz-$HARFBUZZ_VERSION.tar.xz \
	harfbuzz-$HARFBUZZ_VERSION.tar.xz &&
  unpack harfbuzz-$HARFBUZZ_VERSION.tar.xz harfbuzz-$HARFBUZZ_VERSION
}

download_cairo()
{ fetch https://cairographics.org/releases/cairo-$CAIRO_VERSION.tar.xz \
	cairo-$CAIRO_VERSION.tar.xz &&
  unpack cairo-$CAIRO_VERSION.tar.xz cairo-$CAIRO_VERSION
}

download_pango()
{ fetch https://download.gnome.org/sources/pango/${PANGO_VERSION%.*}/pango-$PANGO_VERSION.tar.xz \
	pango-$PANGO_VERSION.tar.xz &&
  unpack pango-$PANGO_VERSION.tar.xz pango-$PANGO_VERSION
}

download_prerequisites()
{ for d in "${DEPS_LIBS[@]}" "${DEPS_LIBS_UNIVERSAL[@]}"; do
      echo "*** downloading $d"
      download_$d || return 1
  done
}

################################################################
# Building, per library
################################################################

build_zlib()
{ autotools_build zlib-$ZLIB_VERSION
}

build_libffi()
{ autotools_build libffi-$FFI_VERSION --disable-static --disable-docs
}

build_libpcre2()
{ autotools_build pcre2-$PCRE2_VERSION \
      --disable-static --enable-jit --enable-unicode
}

build_gmp()
{ autotools_build gmp-$GMP_VERSION --enable-shared --disable-static
}

# OpenSSL has its own configuration system.  `install_sw` skips the docs;
# `install_ssldirs` installs $DEPS/ssl/openssl.cnf, which the bundle needs.

build_ssl()
{ local target
  case $DEPS_ARCH in
      arm64)  target=darwin64-arm64-cc ;;
      x86_64) target=darwin64-x86_64-cc ;;
      *)      echo "Unknown architecture $DEPS_ARCH"; return 1 ;;
  esac
  ( set -e
    cd "$DEPS_SRC/openssl-$SSL_VERSION"
    rm -rf "build-$DEPS_ARCH"
    mkdir -p "build-$DEPS_ARCH"
    cd "build-$DEPS_ARCH"
    ../Configure --prefix="$DEPS" --openssldir="$DEPS/ssl" \
		 shared threads no-docs $target
    make -j$NPROC
    make install_sw install_ssldirs DESTDIR="$DEPS_STAGE"
  )
}

build_libarchive()
{ autotools_build libarchive-$ARCHIVE_VERSION \
      --disable-static --with-pic --with-zlib \
      --without-iconv --without-openssl --without-nettle --without-xml2 \
      --without-expat --without-libregex --without-bz2lib \
      --without-lzmadec --without-lzma --without-lzo2 \
      --without-libb2 --without-zstd --without-lz4
}

# Old libtool (Berkeley DB, OSSP uuid) decides how to link a dylib by
# matching $MACOSX_DEPLOYMENT_TARGET against `10.[012]*' -- which also
# matches 10.15, giving -flat_namespace -undefined suppress.  Anchor the
# pattern so only 10.0-10.2 take that branch.  Idempotent.

fix_libtool_darwin()			# fix_libtool_darwin configure
{ sed -i '' 's/^\([[:space:]]*\)10\.\[012\]\*)/\110.[012],*)/' "$1"
}

# Berkeley DB configures from dist/ and installs only the library and the
# headers.  Use a per-architecture build directory like everything else:
# its conventional build_unix/ is shared, and because the second pass's
# configure does not invalidate the first pass's object files, make would
# quietly relink the *previous* architecture's objects and install those.
#
# 5.3.28 predates modern clang: atomic_init() and
# __atomic_compare_exchange() collide with compiler builtins (renamed as
# in Homebrew's berkeley-db@5), and some configure probes rely on implicit
# function declarations.

build_libdb()
{ ( set -e
    cd "$DEPS_SRC/db-$BDB_VERSION"
    sed -i '' -e 's/atomic_init(/atomic_init_db(/g' \
	      -e 's/__atomic_compare_exchange(/__atomic_compare_exchange_db(/g' \
	src/dbinc/atomic.h src/mp/mp_fget.c src/mp/mp_mvcc.c \
	src/mp/mp_region.c src/mutex/mut_method.c src/mutex/mut_tas.c
    fix_libtool_darwin dist/configure
    rm -rf "build-$DEPS_ARCH"
    mkdir -p "build-$DEPS_ARCH"
    cd "build-$DEPS_ARCH"
    CFLAGS="$CFLAGS -Wno-implicit-function-declaration" \
      ../dist/configure --prefix="$DEPS" --enable-shared --disable-static
    make -j$NPROC library_build
    make install_lib install_include DESTDIR="$DEPS_STAGE"
  )
}

build_odbc()
{ autotools_build unixODBC-$ODBC_VERSION \
      --disable-static --enable-gui=no --enable-iconv=no --with-included-ltdl
}

# OSSP uuid 1.6.2 (2008) is the one package here that cannot be built out
# of tree: its install rule copies uuid.pc from the source directory while
# configure writes it into the build directory, and @UUID_VERSION_RAW@
# comes out as 0.1.0 instead of 1.6.2.  Build it in tree instead, cleaning
# up first so the second architecture does not reuse the first one's
# objects.  clib's FindLibUUID.cmake locates the library through uuid.pc,
# so it has to be correct.

build_libuuid()
{ ( set -e
    cd "$DEPS_SRC/uuid-$UUID_VERSION"
    make distclean >/dev/null 2>&1 || true
    fix_libtool_darwin configure
    # uuid-config records $LDFLAGS verbatim and is installed; uuid depends
    # on nothing, so keep the staging -L out of it.
    LDFLAGS= ./configure --prefix="$DEPS" --disable-static
    make -j$NPROC
    make install DESTDIR="$DEPS_STAGE"
  )
}

build_libyaml()
{ autotools_build yaml-$YAML_VERSION --disable-static
}

build_pixman()
{ meson_build pixman-$PIXMAN_VERSION \
      -Dtests=disabled -Ddemos=disabled -Dgtk=disabled -Dlibpng=disabled
}

# glib needs pcre2, libffi and zlib from $DEPS.  gvdb is a pinned subproject
# shipped in the tarball, so the wrap mode has to allow fallbacks here.

build_glib()
{ ( set -e
    cd "$DEPS_SRC/glib-$GLIB_VERSION"
    rm -rf "build-$DEPS_ARCH"
    meson setup "build-$DEPS_ARCH" \
	  --prefix="$DEPS" \
	  --buildtype=release \
	  --default-library=shared \
	  --wrap-mode=nodownload \
	  -Dcmake_prefix_path="$DEPS_ROOT" \
	  -Dnls=disabled \
	  -Dtests=false \
	  -Dintrospection=disabled \
	  -Ddocumentation=false \
	  -Dman-pages=disabled
    meson compile -C "build-$DEPS_ARCH" -j $NPROC
    DESTDIR="$DEPS_STAGE" meson install -C "build-$DEPS_ARCH"
  )
}

build_fribidi()
{ meson_build fribidi-$FRIBIDI_VERSION \
      -Ddocs=false -Dbin=false -Dtests=false
}

# CoreText is the font backend on macOS; FreeType and ICU are not needed.

build_harfbuzz()
{ meson_build harfbuzz-$HARFBUZZ_VERSION \
      -Dglib=enabled -Dgobject=enabled -Dcoretext=enabled \
      -Dfreetype=disabled -Dicu=disabled -Dcairo=disabled -Dchafa=disabled \
      -Dtests=disabled -Ddocs=disabled -Dintrospection=disabled \
      -Dutilities=disabled
}

# Quartz only: XPCE draws through pangocairo with the CoreText font map, so
# the FreeType/fontconfig font backends and libpng are dead weight.  zlib is
# needed for the PDF surface (cairo_pdf_surface_create).

build_cairo()
{ meson_build cairo-$CAIRO_VERSION \
      -Dquartz=enabled -Dzlib=enabled -Dglib=enabled \
      -Dfreetype=disabled -Dfontconfig=disabled -Dpng=disabled \
      -Dxlib=disabled -Dxcb=disabled -Dxlib-xcb=disabled -Ddwrite=disabled \
      -Dtee=disabled -Dtests=disabled -Dspectre=disabled -Dgtk_doc=false
}

build_pango()
{ meson_build pango-$PANGO_VERSION \
      -Dcairo=enabled -Dfontconfig=disabled -Dfreetype=disabled \
      -Dintrospection=disabled -Ddocumentation=false \
      -Dbuild-testsuite=false -Dbuild-examples=false
}

################################################################
# Building, universal in one pass (CMake projects)
################################################################

build_utf8proc()
{ cmake_build utf8proc-$UTF8PROC_VERSION \
      -DUTF8PROC_ENABLE_TESTING=OFF
}

build_sdl3()
{ cmake_build SDL3-$SDL3_VERSION \
      -DSDL_SHARED=ON -DSDL_STATIC=OFF -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF \
      -DSDL_COCOA=ON -DSDL_METAL=ON -DSDL_RENDER=ON \
      -DSDL_VIDEO=ON -DSDL_AUDIO=ON \
      -DSDL_X11=OFF -DSDL_WAYLAND=OFF -DSDL_VULKAN=OFF
}

# PNG and JPEG are decoded by ImageIO (CoreGraphics) and stb, so no
# libpng or libjpeg is needed.  SDLIMAGE_PNG_LIBPNG must be turned off
# explicitly: it defaults on whenever SDLIMAGE_PNG is set and would drag in
# libpng for the sake of animated PNGs, which is the one thing the
# libpng-free IMG_png.c path gives up.  The release tarball's external/
# holds only download scripts, so vendoring is not an option either.

build_sdl3_image()
{ cmake_build SDL3_image-$SDL3_IMAGE_VERSION \
      -DSDLIMAGE_SAMPLES=OFF -DSDLIMAGE_TESTS=OFF -DSDLIMAGE_DEPS_SHARED=OFF \
      -DSDLIMAGE_VENDORED=OFF \
      -DSDLIMAGE_BACKEND_IMAGEIO=ON -DSDLIMAGE_BACKEND_STB=ON \
      -DSDLIMAGE_JPG=ON -DSDLIMAGE_PNG=ON -DSDLIMAGE_PNG_LIBPNG=OFF \
      -DSDLIMAGE_WEBP=OFF -DSDLIMAGE_AVIF=OFF -DSDLIMAGE_JXL=OFF \
      -DSDLIMAGE_TIF=OFF
}

# The `sweep' package (Emacs interface) needs emacs-module.h, which is not
# part of any of the above.  Macports' Emacs is the only place we have it.

build_emacs_header()
{ if [ -f /opt/local/include/emacs-module.h ]; then
    mkdir -p "$DEPS_ROOT_FAT/include"
    cp /opt/local/include/emacs-module.h "$DEPS_ROOT_FAT/include"
  else
    echo "No /opt/local/include/emacs-module.h; skipping (the sweep package needs it)"
  fi
}

################################################################
# Order.  zlib, libffi and pcre2 come first because glib uses all three;
# glib, pixman, fribidi and harfbuzz come before cairo and pango.
################################################################

DEPS_LIBS=(zlib libffi libpcre2 gmp ssl libarchive libdb odbc libuuid
	   libyaml pixman glib fribidi harfbuzz cairo pango)
DEPS_LIBS_UNIVERSAL=(utf8proc sdl3 sdl3_image)

################################################################
# Driving the whole thing
################################################################

# Build every autotools/meson library for one architecture into its own
# staging tree.  For an architecture other than the one we are running on,
# re-enter this script under `arch`.

build_arch()
{ local arch="$1"

  if [ "$arch" != "$(uname -m)" ]; then
    arch -$arch /bin/bash -c \
	 "cd '$DEPS_SRC' && . ./macos-deps.sh && build_arch $arch"
    return $?
  fi

  echo "=== Building for $arch in $DEPS_STAGE ==="
  rm -rf "$DEPS_STAGE"
  for d in "${DEPS_LIBS[@]}"; do
      echo "*** $arch: $d"
      if ! build_$d; then
	  echo "*** FAILED: $d ($arch)"
	  return 1
      fi
  done
}

# Merge the two staging trees into $DEPS.  The arm64 tree provides the
# headers, pkg-config files and scripts; every Mach-O file is replaced by a
# fat version of both.  lipo and install_name_tool invalidate code
# signatures, so everything we touch is re-signed ad hoc -- arm64 refuses to
# load a dylib whose signature does not match.

merge_universal()
{ local arm="$DEPS_SRC/stage-arm64$DEPS"
  local x86="$DEPS_SRC/stage-x86_64$DEPS"
  local fat="$DEPS_ROOT_FAT"
  local d f rel

  for d in "$arm" "$x86" "$fat"; do
      if [ ! -d "$d" ]; then
	  echo "No such staging tree: $d"
	  echo "(run build_arch arm64, build_arch x86_64 and build_universal)"
	  return 1
      fi
  done

  echo "=== Composing $DEPS ==="
  rm -rf "$DEPS"
  mkdir -p "$DEPS" || return 1

  # The arm64 tree provides the headers, pkg-config files and scripts;
  # every Mach-O file in it is then replaced by a fat version of both
  # architectures.  The CMake libraries are fat already and are copied in
  # afterwards.

  ( cd "$arm" && tar cf - . ) | ( cd "$DEPS" && tar xf - )

  ( cd "$DEPS"
    find . -type f -print | while read -r f; do
	case "$(file -b "$f")" in
	    *Mach-O*|*"current ar archive"*)
		rel="${f#./}"
		if [ ! -f "$x86/$rel" ]; then
		    echo "WARNING: no x86_64 counterpart for $rel"
		elif lipo -create "$f" "$x86/$rel" -output "$f.fat"; then
		    mv "$f.fat" "$f"
		    codesign --force --sign - "$f" 2>/dev/null
		else
		    rm -f "$f.fat"
		    echo "WARNING: cannot lipo $rel"
		fi
		;;
	esac
    done
  )

  ( cd "$fat" && tar cf - . ) | ( cd "$DEPS" && tar xf - )

  # libtool .la files record this pass's staging -L in dependency_libs.
  # Nothing downstream uses libtool -- swipl is built with CMake against
  # pkg-config -- so they can only mislead.  Drop them.

  find "$DEPS" -name '*.la' -delete
}

# CMake installs libraries with an @rpath install name.  The bundle rewrites
# install names later (scripts/macosx_bundle_fixup.sh) and expects to find
# absolute paths here, so turn every @rpath reference into $DEPS/lib/...

absolute_install_names()
{ local root="${1:-$DEPS}"
  local f id dep base

  for f in $(find "$root/lib" "$root/bin" -type f 2>/dev/null); do
      case "$(file -b "$f")" in
	  *Mach-O*) ;;
	  *) continue ;;
      esac

      id=$(otool -D "$f" | sed -n '2p')
      case "$id" in
	  @rpath/*) install_name_tool -id "$DEPS/lib/${id#@rpath/}" "$f" ;;
      esac

      for dep in $(otool -L "$f" | awk 'NR>1 {print $1}' | grep '^@rpath/'); do
	  base="${dep#@rpath/}"
	  if [ -f "$DEPS/lib/$base" ]; then
	      install_name_tool -change "$dep" "$DEPS/lib/$base" "$f"
	  fi
      done

      codesign --force --sign - "$f" 2>/dev/null
  done
}

build_universal()
{ echo "=== Building universal (CMake) libraries in $DEPS_STAGE_FAT ==="
  rm -rf "$DEPS_STAGE_FAT"
  for d in "${DEPS_LIBS_UNIVERSAL[@]}"; do
      echo "*** universal: $d"
      if ! build_$d; then
	  echo "*** FAILED: $d"
	  return 1
      fi
  done
  build_emacs_header
  absolute_install_names "$DEPS_ROOT_FAT"
}

build_prerequisites()
{ build_arch arm64	&&
  build_arch x86_64	&&
  build_universal	&&
  merge_universal	&&
  check_prerequisites
}

################################################################
# Verification
################################################################

# Three things can quietly go wrong: a library ends up single-architecture,
# a library refers to Macports/Homebrew, or a library refers to one of the
# staging trees instead of $DEPS.

check_prerequisites()
{ local f leaks bad=0 thin=0 ports=0 stage=0 flat=0

  echo "=== Checking $DEPS ==="
  for f in $(find "$DEPS/lib" "$DEPS/bin" -type f 2>/dev/null); do
      case "$(file -b "$f")" in
	  *Mach-O*) ;;
	  *) continue ;;
      esac

      case "$(lipo -info "$f" 2>/dev/null)" in
	  *"x86_64 arm64"*|*"arm64 x86_64"*) ;;
	  *) echo "NOT UNIVERSAL: ${f#$DEPS/}"; thin=$((thin+1)) ;;
      esac

      if otool -L "$f" | grep -q '/opt/local\|/usr/local\|/opt/homebrew'; then
	  echo "LINKS TO PORTS: ${f#$DEPS/}"
	  otool -L "$f" | grep '/opt/local\|/usr/local\|/opt/homebrew'
	  ports=$((ports+1))
      fi

      # Only linked images have a namespace; the members of a static
      # archive are MH_OBJECT files, which never carry TWOLEVEL.
      case "$(otool -arch arm64 -hv "$f" 2>/dev/null | tail -1)" in
	  *DYLIB*TWOLEVEL*|*EXECUTE*TWOLEVEL*) ;;
	  *DYLIB*|*EXECUTE*)
	      echo "FLAT NAMESPACE: ${f#$DEPS/}"; flat=$((flat+1)) ;;
      esac

      if otool -L "$f" | grep -q "$DEPS_SRC/stage-"; then
	  echo "LINKS TO STAGING: ${f#$DEPS/}"
	  stage=$((stage+1))
      fi
  done

  leaks=$(grep -rls "$DEPS_SRC/stage-" "$DEPS" 2>/dev/null)
  if [ -n "$leaks" ]; then
      echo "$leaks" | head
      echo "(above files mention a staging directory)"
      bad=1
  fi

  echo "--- installed versions"
  for f in "$DEPS"/lib/pkgconfig/*.pc; do
      [ -f "$f" ] || continue
      printf "%-26s %s\n" "$(basename "$f" .pc)" \
	     "$(sed -n 's/^Version: *//p' "$f")"
  done

  echo "--- $thin thin, $ports port-linked, $stage staging-linked, $flat flat-namespace"
  [ $thin -eq 0 -a $ports -eq 0 -a $stage -eq 0 -a $flat -eq 0 -a $bad -eq 0 ]
}

################################################################
# Cleaning
################################################################

# Remove the per-architecture build directories, but keep the unpacked
# sources (and any local patches in them).

clean_builds()
{ ( cd "$DEPS_SRC" && rm -rf */build-arm64 */build-x86_64 */build-universal \
			   stage-arm64 stage-x86_64 stage-universal
    cd "$DEPS_SRC/uuid-$UUID_VERSION" 2>/dev/null && make distclean >/dev/null 2>&1
    true )
}

# Throw away the unpacked sources and start over from the tarballs.

clean_prerequisites()
{ ( cd "$DEPS_SRC"
    rm -rf gmp-$GMP_VERSION openssl-$SSL_VERSION zlib-$ZLIB_VERSION \
	   libarchive-$ARCHIVE_VERSION uuid-$UUID_VERSION db-$BDB_VERSION \
	   unixODBC-$ODBC_VERSION pcre2-$PCRE2_VERSION libffi-$FFI_VERSION \
	   yaml-$YAML_VERSION utf8proc-$UTF8PROC_VERSION \
	   SDL3-$SDL3_VERSION SDL3_image-$SDL3_IMAGE_VERSION \
	   pixman-$PIXMAN_VERSION glib-$GLIB_VERSION \
	   fribidi-$FRIBIDI_VERSION harfbuzz-$HARFBUZZ_VERSION \
	   cairo-$CAIRO_VERSION pango-$PANGO_VERSION
    clean_builds
  )
}
