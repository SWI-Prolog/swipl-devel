# Building SWI-Prolog using cmake

Currently SWI-Prolog has cmake CMakeLists.txt   configuration files that
cover almost the entire project and builds   on Ubuntu 18.04, Fedore 28,
MacOSX and cross compilation for Win32 as well as Win64 on Ubuntu 18.04

It has been tested with  the   "Unix  Makefiles" and "Ninja" generators.
Ninja performs notably better. It can be  selected using "cmake -G Ninja
...", after which the usual `make` _target_   can be replaced by `ninja`
_target_.

## Native build

```{bash}
mkdir build
cd build
cmake ..
make -j 8
```

## Build types

The default build type is `RelWithDebInfo`.  Alternatives may be selected
using e.g.,

    cmake -DCMAKE_BUILD_TYPE=Debug
    cmake -DCMAKE_BUILD_TYPE=Release

## Customizing SWI-Prolog

By default the system configures all   features. Several cmake _options_
allow for restricting the system.

  | `-DMULTI_THREADED=OFF`       | Drop support for Prolog threads  |
  | `-DUSE_GMP=OFF`              | Drop bignum and rational numbers |
  | `-DSWIPL_PACKAGES=OFF`       | Only build the core system       |
  | `-DSWIPL_PACKAGES_BASIC=OFF` | Drop all basic packages          |
  | `-DSWIPL_PACKAGES_ODBC=OFF`  | Drop ODBC and CQL packages       |
  | `-DSWIPL_PACKAGES_JAVA=OFF`  | Drop JPL Java interface          |
  | `-DSWIPL_PACKAGES_X=OFF`     | Drop graphics (xpce)             |

Note that packages for  which  the   prerequisites  cannot  be found are
dropped automatically, as are packages  for   which  the sources are not
installed.

## Cross build

Cross building for Windows using (Ubuntu)   linux. Use `README.mingw` to
download  and  build  the  dependencies.   Nex,  set  these  environment
variables:

  - `MINGW64_ROOT` must point at the prefix where the dependencies
    are for 64-bit Windows.
  - `MINGW32_ROOT` must point at the prefix where the dependencies
    are for 32-bit Windows.

The cmake toolchain  config  files  (see   below)  search  for  Java  in
`$HOME/.wine`. Please check these locations.

### 64 bit Windows from Linux

```{bash}
mkdir win64
cd win64
cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win64.cmake ..
```

### 32 bit Windows from Linux

```{bash}
mkdir win32
cd win32
cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win32.cmake ..
```

## Issues

  - Build the library INDEX.pl somehow in the build directory.
    - Can be dome if we create a shadow Prolog home in the build dir
      - This is rather expensive though
      - Alternatively we need a make_library_directory/1 alternative
        that is passed a list of files that will be in the library
	and create the INDEX.pl in the build tree.  In addition we
	need something that loads the INDEX.pl from an alternative
	location.
  - Install the Prolog files as links to the sources for development.
    - Better: create a shadow Prolog home in the build dir using links.
  - Build documention
    - Currently requires `make install`, `make doc`, i.e., system needs
      to be installed before compiling the documentation.
    - By default we can only build the HTML as we need no dependencies
      for that.
  - Create installers
  - Install pkg-config files
