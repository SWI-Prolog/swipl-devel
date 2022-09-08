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

PREFIX="$HOME/deps"
export MACOSX_DEPLOYMENT_TARGET=10.14

GMP_VERSION=6.2.1
SSL_VERSION=1.1.1m
JPEG_VERSION=9b
ZLIB_VERSION=1.2.11
ARCHIVE_VERSION=3.5.2
UUID_VERSION=1.6.2
BDB_VERSION=6.1.26
ODBC_VERSION=2.3.9
PCRE_VERSION=8.45
PCRE2_VERSION=10.39
FFI_VERSION=3.4.2
YAML_VERSION=0.1.7
READLINE_VERSION=8.0

# installation prefix.  This path should not have spaces in one of the
# directory names.

src="$(pwd)"
################
# Handy for running autoconf from a directory

export LDFLAGS=-L$PREFIX/lib
export CFLAGS="-mmacosx-version-min=10.14 -O2"

config()
{ if [ -r ./configure ]; then
    ./configure --prefix=$PREFIX
  elif  [ -r ../src/configure ]; then
    ./configure --prefix=$PREFIX
  fi
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
  curl http://www.unixodbc.org/$ODBC_FILE > $ODBC_FILE
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
    --without-lzmadec --without-lzma --without-lzo2
    make
    make install
  )
}


#################################
# Download and install libpcre

download_libpcre()
{ PCRE_FILE=pcre-$PCRE_VERSION.tar.gz

  [ -f $PCRE_FILE ] || \
    wget https://ftp.pcre.org/pub/pcre/$PCRE_FILE
  tar xzf $PCRE_FILE
}


build_libpcre()
{ ( cd pcre-$PCRE_VERSION
    ./configure --prefix=$PREFIX \
	--disable-static --disable-cpp --enable-utf8 --enable-unicode-properties
    make pcre.dll
    make install
  )
}


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


###########################
# Do the whole lot for all prerequisites

clean_prerequisites()
{ ( cd gmp-$GMP_VERSION && make distclean )
  ( cd openssl-$SSL_VERSION && make distclean )
  ( cd jpeg-$JPEG_VERSION && make distclean )
  ( cd zlib-$ZLIB_VERSION && make distclean )
  ( cd libarchive-$ARCHIVE_VERSION && make distclean )
  ( cd uuid-$UUID_VERSION && make distclean )
  ( cd ffi-$FFI_VERSION && make distclean )
  ( cd libyaml && make distclean )
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
  download_libpcre
  download_libffi
  download_libyaml
}

build_prerequisites()
{ build_gmp
  build_ssl
  build_jpeg
  build_zlib
  build_readline
  build_libarchive
  build_libuuid
  build_libdb
  build_odbc
  build_libpcre
  build_libffi
  build_libyaml
}
