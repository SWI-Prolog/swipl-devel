# This is supposed to be a TOOLCHAIN file for compiling a 32-bit version
# on 64-bit Linux, but that doesn't work  somehow. The work around is to
# configure using
#
#     cmake -DSWIPL_M32=ON ...
#
# Note that all packages, gcc and g++ need to be installed for i386,
# e.g.
#
#     apt install gcc-multilib g++-multilib

# apt install \
#	libarchive-dev:i386 libc6-dev:i386 libc6-dev-i386 \
#	libdb-dev:i386 libedit-dev:i386 libgmp-dev:i386 \
#	libice-dev:i386 libjpeg-dev:i386 libjpeg8-dev:i386 \
#	libncurses5-dev:i386 libossp-uuid-dev:i386 \
#	libpcre2-dev:i386 libssl1.0-dev:i386 \
#	libstdc++-7-dev:i386 libxext-dev:i386 libxft-dev:i386 \
#	libxinerama-dev:i386 libxpm-dev:i386 libxt-dev:i386 \
#	libyaml-dev:i386 zlib1g-dev:i386

# the name of the target operating system (if TOOLCHAIN works)
# set(CMAKE_SYSTEM_NAME Linux)

# Tell gcc and g++ to generate i386 binaries
set(CMAKE_C_FLAGS -m32)
set(CMAKE_CXX_FLAGS -m32)

# Get this right
set(CMAKE_SIZEOF_VOID_P 4)

# Search in /usr/lib/i386-linux-gnu for the multi archive
# libraries
set(CMAKE_LIBRARY_ARCHITECTURE i386-linux-gnu)

# Make pkg-config search in /usr/lib/i386-linux-gnu/pkgconfig
set(CMAKE_PREFIX_PATH /usr)
