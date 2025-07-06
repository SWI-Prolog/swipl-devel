#!/bin/bash
#
# This script downloads and builds the   dependencies  of SWI-Prolog. It
# was designed to build the dependencies  in   a  controlled way for the
# MacOS binary bundle, but  should  be   easily  adapted  to install the
# dependencies on other Unix-like platforms.
#
# To use this, run `. macos-deps.sh`  to   get  the various functions in
# your shell. Note that the BDB download   is no longer easily available
# and requires registering an account with Oracle.

# Set PREFIX to point at the prefix for installing the dependencies.
# Configure using cmake -DMACOSX_DEPENDENCIES_FROM=$PREFIX
#
# Ideally, we build _universal_  libraries, but this seems complicated
# because the  configuration of some  of the libraries depends  on the
# CPU.  E.g., OpenSSL  does not build on the M1  using `-arch x86_64`.
# pcre   does  not   include  the   JIT  compiler,   etc.   Therefore,
# unfortunately, we must build the  libraries on a real x86_64 machine
# and  combine  them  using  the  `macos-import-arch.sh`  script  into
# universal binaries.

PREFIX="$HOME/deps"
export MACOSX_DEPLOYMENT_TARGET=10.15

GMP_VERSION=6.3.0
SSL_VERSION=3.5.1
JPEG_VERSION=9f
ZLIB_VERSION=1.3.1
ARCHIVE_VERSION=3.8.1
UUID_VERSION=1.6.2
BDB_VERSION=6.1.26
ODBC_VERSION=2.3.12
PCRE2_VERSION=10.45
FFI_VERSION=3.5.1
YAML_VERSION=0.2.5
READLINE_VERSION=8.2
SDL3_VERSION=3.2.16
SDL3_IMAGE_VERSION=3.2.4
CAIRO_VERSION=1.18.4
PANGO_VERSION=1.56.4

# installation prefix.  This path should not have spaces in one of the
# directory names.

src="$(pwd)"
################
# LDFLAGS allows for running autoconf from a directory

export PYTHON_BIN="/Library/Frameworks/Python.framework/Versions/3.11/bin/"
export PATH="$PREFIX/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PYTHON_BIN:/opt/local/bin"
export LDFLAGS=-L$PREFIX/lib
export CUFLAGS="-arch x86_64"
export CMFLAGS="-mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET -O2"
export CWFLAGS="-Wno-nullability-completeness"
export CIFLAGS="-I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
export CFLAGS="$CIFLAGS $CWFLAGS $CMFLAGS"
# export CFLAGS="$CFLAGS $CUFLAGS"
export PKG_CONFIG_LIBDIR="/usr/lib/pkgconfig:$PREFIX/lib/pkgconfig"
unset PKG_CONFIG_PATH
export CMAKE_PREFIX_PATH="$PREFIX;/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr"
#Not accepted by CMake :(
export CMAKE_IGNORE_PREFIX_PATH="/usr/local:/opt/local"

config()
{ if [ -r ./configure ]; then
    ./configure --prefix=$PREFIX
  elif  [ -r ../src/configure ]; then
    ./configure --prefix=$PREFIX
  fi
}


# Hide Homebrew and Macports to avoid importing dependencies for cairo
# or pango.  We do need meson  and ninja.  We copy ninja from Macports
# as it is a simple executable.   We install meson using MacOS Python.
# Unfortunately MacOS certificates are too  old, so we need to install
# these as well.

hide_ports()
{ mkdir -p $PREFIX/bin
  if [ ! -x $PREFIX/bin/ninja ]; then
      cp /opt/local/bin/ninja $PREFIX/bin/ninja
  fi

  sudo chmod 0 /opt/local /usr/local/lib /usr/local/bin /usr/local/share
  hash -r
  if [ ! -x $PYTHON_BIN/meson ]; then
      python3 -m pip install meson
      python3 -m pip install --user certifi
  fi
  export SSL_CERT_FILE=$(python3 -m certifi)
}

restore_ports()
{ sudo chmod 755 /opt/local /usr/local/lib /usr/local/bin /usr/local/share
  hash -r
}

###########################
# Download and install the GMP library.

download_gmp()
{ GMP_FILE=gmp-$GMP_VERSION.tar.bz2

  [ -f $GMP_FILE ] || \
    wget https://ftp.gnu.org/gnu/gmp/$GMP_FILE
  tar jxf $GMP_FILE
}

build_gmp()
{ ( cd gmp-$GMP_VERSION
    ./configure --prefix=$PREFIX \
       --enable-shared --disable-static --enable-fat
    make
    make install
  )
}

###########################
# Download and install ssl

download_ssl()
{ SSL_FILE=openssl-$SSL_VERSION.tar.gz
  [ -f $SSL_FILE ] || wget http://www.openssl.org/source/$SSL_FILE
  tar xzf $SSL_FILE
}

build_ssl()
{ ( cd openssl-$SSL_VERSION
    case "$(uname -a)" in
	*arm*) target=darwin64-arm64-cc
	       ;;
	*x86_64*) target=darwin64-x86_64-cc
		  ;;
	*) echo "Unknown Darmin target"
	   return 1
	   ;;
    esac
    export CFLAGS="$CMFLAGS"	# cannot build universal binary
    ./Configure --prefix=$PREFIX shared threads $target
    make depend
    make
    make install_sw
    make install_ssldirs	# installs deps/ssl/openssl.cnf
  )
}

###########################
# Download and install BerkeleyDB
# http://www.oracle.com/technetwork/database/database-technologies/berkeleydb/overview/index.html

download_libdb()
{ BDB_FILE=db-$BDB_VERSION.tar.gz

  [ -f $BDB_FILE ] || \
  curl http://download.oracle.com/otn/berkeley-db/$BDB_FILE > $BDB_FILE
  tar zxvf $BDB_FILE
}

build_libdb()
{ ( cd db-$BDB_VERSION/build_unix
    ../dist/configure --prefix=$PREFIX \
       --enable-shared --disable-static
    make library_build
    make install_lib install_include
  )
}


###########################
# Download and install BerkeleyDB
# http://www.oracle.com/technetwork/database/database-technologies/berkeleydb/overview/index.html

download_odbc()
{ ODBC_FILE=unixODBC-$ODBC_VERSION.tar.gz

  [ -f $ODBC_FILE ] || \
  curl https://www.unixodbc.org/$ODBC_FILE > $ODBC_FILE
  tar zxvf $ODBC_FILE
}

build_odbc()
{ ( cd unixODBC-$ODBC_VERSION
    ./configure --prefix=$PREFIX --enable-gui=no --enable-iconv=no --with-included-ltdl
    make
    make install
  )
}


###########################
# Download and install jpeg

download_jpeg()
{ JPEG_FILE=jpegsrc.v$JPEG_VERSION.tar.gz

  [ -f $JPEG_FILE ] || wget http://www.ijg.org/files/$JPEG_FILE
  tar xzf $JPEG_FILE
}

build_jpeg()
{ ( cd jpeg-$JPEG_VERSION
    ./configure --prefix=$PREFIX --enable-shared
    make
    make install
  )
}

###########################
# Download and install zlib

download_zlib()
{ ZLIB_FILE=zlib-$ZLIB_VERSION.tar.gz

  [ -f $ZLIB_FILE ] || wget http://zlib.net/$ZLIB_FILE
  tar xzf $ZLIB_FILE
}

build_zlib()
{ ( cd zlib-$ZLIB_VERSION
    ./configure --prefix=$PREFIX
    make
    make install
  )
}

###########################
# Download and install libreadline

download_readline()
{ READLINE_FILE=readline-$READLINE_VERSION.tar.gz

  [ -f $READLINE_FILE ] || wget https://ftp.gnu.org/gnu/readline/$READLINE_FILE
  tar xzf $READLINE_FILE
}

build_readline()
{ ( cd readline-$READLINE_VERSION
    ./configure --prefix=$PREFIX
    make
    make install
  )
}

#################################
# Download and install libarchive

download_libarchive()
{ ARCHIVE_FILE=libarchive-$ARCHIVE_VERSION.tar.gz

  [ -f $ARCHIVE_FILE ] || \
    wget http://www.libarchive.org/downloads/$ARCHIVE_FILE
  tar xzf $ARCHIVE_FILE
}

# lt_cv_deplibs_check_method=pass_all works around a bug in libtool
# causing: "linker path does not have real file for library" error on MinGW
# See http://lists.cairographics.org/archives/cairo/2009-July/017686.html

build_libarchive()
{ ( cd libarchive-$ARCHIVE_VERSION
    ./configure --prefix=$PREFIX --with-pic \
    --without-iconv --without-openssl --without-nettle --without-xml2 \
    --without-expat --without-libregex --without-bz2lib \
    --without-lzmadec --without-lzma --without-lzo2 \
    --without-libb2 --without-zstd --without-lz4 
    make
    make install
  )
}


#################################
# Download and install libpcre

download_libpcre2()
{ PCRE2_FILE=pcre2-$PCRE2_VERSION.tar.gz

  [ -f $PCRE2_FILE ] || \
    wget https://github.com/PhilipHazel/pcre2/releases/download/pcre2-$PCRE2_VERSION/pcre2-$PCRE2_VERSION.tar.gz
  tar xzf $PCRE2_FILE
}


build_libpcre2()
{ ( cd pcre2-$PCRE2_VERSION
    ./configure --prefix=$PREFIX \
	--disable-static --disable-cpp --enable-utf8 --enable-unicode-properties
    make pcre2.dll
    make install
  )
}


#################################
# Download and install libuuid

download_libuuid()
{ UUID_FILE=uuid-$UUID_VERSION.tar.gz

  [ -f $UUID_FILE ] || \
  curl ftp://ftp.ossp.org/pkg/lib/uuid/$UUID_FILE > $UUID_FILE
  tar zxvf $UUID_FILE
}

build_libuuid()
{ ( cd uuid-$UUID_VERSION
    ./configure --prefix=$PREFIX
    make
    make install
  )
}

################################
# Download and install libffi

download_libffi()
{ FFI_FILE=libffi-$FFI_VERSION.tar.gz
  [ -f $FFI_FILE ] || \
  wget https://github.com/libffi/libffi/releases/download/v$FFI_VERSION/$FFI_FILE
  tar zxvf $FFI_FILE
}

build_libffi()
{ ( cd libffi-$FFI_VERSION
    ./configure --prefix=$PREFIX
    make
    make install
    # Bit strange location for the headers
    cp $PREFIX/lib/libffi-$FFI_VERSION/include/*.h $PREFIX/include
  )
}


################################
# Download and install libyaml

download_libyaml()
{ #tested 01f3a8786127748b5bbd4614880c4484570bbd44
  if [ -d libyaml ]; then
    git -C libyaml pull
  else
    git clone https://github.com/yaml/libyaml
  fi
}

build_libyaml()
{ ( cd libyaml
    ./bootstrap
    ./configure --prefix=$PREFIX
    make
    make install
  )
}

download_sdl3()
{ SDL3_FILE=SDL3-$SDL3_VERSION.tar.gz

  [ -f $SDL3_FILE ] || \
    curl -L -o $SDL3_FILE https://github.com/libsdl-org/SDL/releases/download/release-$SDL3_VERSION/$SDL3_FILE
  tar xzf $SDL3_FILE
}

build_sdl3()
{ ( cd SDL3-$SDL3_VERSION
    mkdir -p build && cd build
    cmake .. \
      -G Ninja \
      -DCMAKE_INSTALL_PREFIX=$PREFIX \
      -DCMAKE_BUILD_TYPE=Release \
      -DSDL_VIDEO=ON \
      -DSDL_AUDIO=ON \
      -DSDL_RENDER=ON \
      -DSDL_EVENTS=ON \
      -DSDL_COCOA=ON \
      -DSDL_X11=OFF \
      -DSDL_WAYLAND=OFF \
      -DSDL_DIRECTFB=OFF \
      -DSDL_VULKAN=OFF \
      -DSDL_METAL=ON \
      -DSDL_TEST=OFF \
      -DSDL_SHARED=ON \
      -DCMAKE_OSX_DEPLOYMENT_TARGET=$MACOSX_DEPLOYMENT_TARGET 

    ninja
    ninja install

    # Do not use @rpath.   We'll fixup while building the bundle.
    dylib=$(echo $PREFIX/lib/libSDL3.*.dylib)
    install_name_tool -id $dylib $dylib
  )
}

download_sdl3_image()
{ SDL3_IMAGE_FILE=SDL_image-release-$SDL3_IMAGE_VERSION.tar.gz

  [ -f $SDL3_IMAGE_FILE ] || \
    curl -o $SDL3_IMAGE_FILE https://github.com/libsdl-org/SDL_image/archive/refs/tags/release-$SDL3_IMAGE_VERSION.tar.gz
  tar xzf $SDL3_IMAGE_FILE
}

build_sdl3_image()
{ ( cd SDL_image-release-$SDL3_IMAGE_VERSION
    mkdir -p build && cd build
    cmake .. \
      -G Ninja \
      -DCMAKE_INSTALL_PREFIX=$PREFIX \
      -DCMAKE_BUILD_TYPE=Release \
      -DSDLIMAGE_BACKEND_IMAGEIO=ON \
      -DSDLIMAGE_BACKEND_STB=ON \
      -DSDLIMAGE_JPG=ON \
      -DSDLIMAGE_PNG=ON \
      -DSDLIMAGE_WEBP=OFF \
      -DCMAKE_OSX_DEPLOYMENT_TARGET=$MACOSX_DEPLOYMENT_TARGET

    ninja
    ninja install

    # Do not use @rpath.   We'll fixup while building the bundle.
    dylib=$(echo $PREFIX/lib/libSDL3_image.*.*.*.dylib)
    sdl3_dylib=$(echo $PREFIX/lib/libSDL3.*.dylib) 
    sdl3_base=$(basename "$sdl3_dylib")
    install_name_tool -id $dylib $dylib
    install_name_tool -change "@rpath/$sdl3_base" "$sdl3_dylib" "$dylib"
  )
}


download_cairo()
{ CAIRO_FILE=cairo-$CAIRO_VERSION.tar.xz

  [ -f $CAIRO_FILE ] || \
    curl -L -o $CAIRO_FILE https://cairographics.org/releases/$CAIRO_FILE
  tar xf $CAIRO_FILE
}

# cairo-1.18.4/subprojects/glib-2.74.0/meson.build:505 needs to be patched
# Comment #'-Werror=declaration-after-statement'
build_cairo()
{ hide_ports
  ( cd cairo-$CAIRO_VERSION
    export CFLAGS="$CFLAGS -std=gnu99 -DHAVE_CTIME_R=1 -Wno-error=declaration-after-statement"

    meson setup build \
      --prefix=$PREFIX \
      --default-library=shared \
      -Dtests=disabled \
      -Dgtk_doc=false \
      -Dxlib=disabled \
      -Dxcb=disabled \
      -Dquartz=enabled \
      -Dtee=disabled \
      -Ddwrite=disabled \
      -Dzlib=enabled \
      -Dfreetype=enabled

    meson compile -C build
    meson install -C build
  )
  restore_ports
}

download_pango()
{ PANGO_FILE=pango-$PANGO_VERSION.tar.xz

  [ -f $PANGO_FILE ] || \
    curl -L -o $PANGO_FILE https://download.gnome.org/sources/pango/1.56/$PANGO_FILE
  tar xf $PANGO_FILE
}

build_pango()
{ hide_ports		# Make Macports and Homebrew invisible
  ( cd pango-$PANGO_VERSION

    meson setup build \
      --prefix=$PREFIX \
      --default-library=shared \
      -Dintrospection=disabled \
      -Dcairo=enabled \
      -Dharfbuzz:icu=disabled \
      -Ddocumentation=false

    meson compile -C build
    meson install -C build
  )
  restore_ports
}


build_emacs()
{ cp /opt/local/include/emacs-module.h $PREFIX/include
}


###########################
# Do the whole lot for all prerequisites

clean_prerequisites()
{ rm -rf jpeg-9f
  for f in *.tar.*; do
      dir=$(echo $f | sed 's/\.tar\..*//')
      echo "Cleaning $dir"
      rm -rf $dir
      tar zxf $f
  done

  ( cd libyaml && git clean -xfd )
}

download_prerequisites()
{ download_gmp
  download_ssl
  download_jpeg
  download_zlib
  download_readline
  download_libarchive
  download_libuuid
  download_libdb
  download_odbc
  download_libpcre2
  download_libffi
  download_libyaml
  download_sdl3
  download_cairo
  download_pango
}

build_prerequisites()
{ build_zlib
  build_libffi
  build_libuuid
  build_gmp
  build_ssl
  build_jpeg
  build_readline
  build_libarchive
  build_libdb
  build_odbc
  build_libpcre2
  build_libyaml
  build_emacs
  build_sdl3
  build_cairo
  build_pango
}
