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

  - Activate options, such as selecting packages, single threaded
    build, exclude GMP, etc.
  - The source tree can still not be read-only.  Notably the tests
    run in the source tree and may write temporary files and the SSL
    test certificates are created in the source tree.
  - Install the Prolog files as links to the sources for development.
  - Build documention
    - We will no longer do the LaTeX --> plain text route, but instead
      either display from the HTML on the terminal or create a plain text
      document from HTML at build time.
  - Create installers
  - Install pkg-config files
