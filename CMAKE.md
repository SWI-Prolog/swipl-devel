# Building SWI-Prolog using cmake

As of version 7.7.20,  SWI-Prolog   ships  with `cmake` `CMakeLists.txt`
configuration files that cover the  entire   project.  Builds  have been
tested on Ubuntu 16.04,  18.04,  18.10,   Fedore  28,  MacOSX  and cross
compilation for Win32 as well  as  Win64   using  Ubuntu  18.04  as host
system.

The build has  been  tested  with   the  "Unix  Makefiles"  and  "Ninja"
generators.  We  use  [Ninja](https://ninja-build.org/)   as  it  builds
faster, avoids warning  from  being   cluttered  and  better facilitates
debugging dependency issues. It can be   selected  using `cmake -G Ninja
...`, after which the usual `make` _target_   can be replaced by `ninja`
_target_. The examples below all  use  Ninja.   Drop  `-G  Ninja` to use
classical Unix make.

## Getting cmake

Building SWI-Prolog requires cmake version 3.5  or later (*). Many Linux
systems ship with a cmake package. On  MacOS we use the Macport version.
If the shipped cmake version is too old   you may wish to download cmake
from https://cmake.org/download/ On  Linux   systems,  installing  e.g.,
cmake 3.12 (please pick the latest stable version) is as simple as:

    wget https://cmake.org/files/v3.12/cmake-3.12.0-Linux-x86_64.sh
    sudo sh cmake-3.12.0-Linux-x86_64.sh --prefix=/usr/local --exclude-subdir

(*) Tested with 3.5 (Ubuntu Xenial), 3.10 (Ubuntu Bionic), 3.14 (MacOS).


## Native build

### Getting the source

The   source   may   be    downloaded    as     a    tar    ball    from
http://www.swi-prolog.org or downloaded using git. The git sequence is:

    git clone https://github.com/SWI-Prolog/swipl-devel.git
    cd swipl-devel
    git submodule update --init

If not all modules are needed, one can clone/update particular ones as follows:

    git submodule update --init packages/jpl packages/clib packages/sgml


### Building from source

The typical sequence to build SWI-Prolog  and install in `/usr/local` is
as follows:

    cd swipl-devel
    mkdir build
    cd build
    cmake -G Ninja ..
    ninja
    ctest -j 8
    ninja install

### Upgrading

In most cases the following should  update   an  installed system to the
latest version:

    git pull
    git submodule update --init
    cd build
    cmake ..
    ninja
    ctest -j 8
    ninja install

If this fails, one of these measures may be appropriate:

  1. run `ninja clean` before `ninja`
  2. remove the entire `build` directory and re-create it as above.
     Note that the build process makes no modifications outside the
     `build` directory.

## Build types

The default build type is `RelWithDebInfo`.  Alternatives may be selected
using e.g.,

    cmake -DCMAKE_BUILD_TYPE=Debug -G Ninja ..
    cmake -DCMAKE_BUILD_TYPE=Release -G Ninja ..

## Install location

To install in a particular   location, use `-DCMAKE_INSTALL_PREFIX`. For
example, this will build SWI to be installed in `/usr/local/swipl-git`:

    cmake -DCMAKE_INSTALL_PREFIX=/usr/local/swipl-git -G Ninja ..

After    `sudo    ninja    install`,     `swipl`      will     be     in
`/usr/local/swipl-git/bin/swipl`.


## Customizing SWI-Prolog

By default the system configures all   features. Several cmake _options_
allow for restricting the system.

  | Option			  | Description                         |
  | ----------------------------- | ----------------------------------- |
  | `-DMULTI_THREADED=OFF`        | Drop support for Prolog threads     |
  | `-DUSE_SIGNALS=OFF`           | Drop signal support                 |
  | `-DUSE_GMP=OFF`               | Drop bignum and rational numbers    |
  | `-DUSE_TCMALLOC=OFF`          | Do not link against `-ltcmalloc`    |
  | `-DSWIPL_SHARED_LIB=OFF`      | Build Prolog kernel as static lib   |
  | `-DSWIPL_INSTALL_IN_LIB=ON`   | Install libswipl.so in <prefix>/lib |
  | `-DSWIPL_INSTALL_IN_SHARE=ON` | Install docs in <prefix>/share      |
  | `-DSWIPL_M32=ON`		  | Make 32-bit version on 64-bit Linux |
  | `-DSWIPL_PACKAGES=OFF`        | Only build the core system          |
  | `-DSWIPL_PACKAGES_BASIC=OFF`  | Drop all basic packages             |
  | `-DSWIPL_PACKAGES_ODBC=OFF`   | Drop ODBC and CQL packages          |
  | `-DSWIPL_PACKAGES_JAVA=OFF`   | Drop JPL Java interface             |
  | `-DSWIPL_PACKAGES_X=OFF`      | Drop graphics (xpce)                |
  | `-DBUILD_TESTING=OFF`         | Do not setup for ctest unit tests   |
  | `-DINSTALL_TESTS=ON`          | Add tests to installed system       |
  | `-DINSTALL_DOCUMENTATION=OFF` | Drop generating the HTML docs       |

Note that packages for  which  the   prerequisites  cannot  be found are
dropped automatically, as are packages  for   which  the sources are not
installed.

## Embedding SWI-Prolog in Java, C, C++, etc.

If SWI-Prolog is to be embedded in another executable it must be able to
find its home directory and the main   application  must be able to find
the SWI-Prolog shared library `libswipl.so`   (extension  depends on the
platform).  The following environment variables are commonly used:

    - `SWI_HOME_DIR` should point at SWI-Prolog's main directory, e.g.
      ``${CMAKE_INSTALL_PREFIX}/lib/swipl``
    - The shared object search path should include the directory where
      `libswipl.{so,dll,...}` resides.  The variable depends on the
      platform.  Some popular names:

      - `LD_LIBRARY_PATH` (ELF based systems such as Linux)
      - `DYLD_LIBRARY_PATH` (MacOS)
      - `PATH` (Windows)

If you build SWI-Prolog  you  must   __remove  these  variables from the
environment when building__. Failure  to  do   so  may  cause  the build
process to use parts  of  an   incompatible  installed  system.  Running
`cmake` warns if  such  an  environment   variable  is  found,  but  the
environment must be cleaned when running `ninja` or `make`.

## Profile Guided Optimization

When using Ninja and GCC, the system  may be build using _Profile Guided
Optimization_ (PGO). This  first  builds   the  system  instrumented  to
collect profile information, then runs  a   benchmark  suite and finally
recompiles it using the benchmark suite  output to help (notably) branch
prediction.  The  performance  improvement   is    about   10   to  20%.
Unfortunately the process is a little hard   and the entire system needs
to be recompiled on any change made to it. To build the PGO version, run
the commands below. The `../scripts/pgo-compile.sh`   performs the three
steps above using  the  core  system.   The  final  `ninja`  builds  the
packages and documentation.

    cmake -DCMAKE_BUILD_TYPE=Release -G Ninja ..
    ../scripts/pgo-compile.sh
    ninja

To stop using PGO builds in the current build directory run the commands
below. The `pgo-compile.sh  -off`  restores   the  normal  build  flags,
deletes the PGO data and cleans the core system files.

    ../scripts/pgo-compile.sh --off
    ninja


## Cross build

Cross building for Windows using (Ubuntu)   linux. Use `README.mingw` to
download  and  build  the  dependencies.  Next,  set  these  environment
variables:

  - `MINGW64_ROOT` must point at the prefix where the dependencies
    are for 64-bit Windows.
  - `MINGW32_ROOT` must point at the prefix where the dependencies
    are for 32-bit Windows.

The cmake toolchain  config  files  (see   below)  search  for  Java  in
`$HOME/.wine`. Please check these locations.

### 64 bit Windows from Linux

    mkdir win64
    cd win64
    cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win64.cmake -G Ninja ..
    ninja

### 32 bit Windows from Linux

    mkdir win32
    cd win32
    cmake -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win32.cmake -G Ninja ..

### WASM (Emscripten)

__Note__: due to a  bug  in   the  current  Emscripten  directory access
functions we need the _native friend_   mechanism  to create the library
index. The flags below   include `-DSWIPL_NATIVE_FRIEND=build`, assuming
you built a  native  executable  in   the  directory  `build`  below the
sources. Adjust as necessary.

    [Assumes native Prolog in `build`.  See note above]

    mkdir build.wasm
    cd build.wasm
    source ~/emsdk/emsdk_env.sh
    cmake -DCMAKE_TOOLCHAIN_FILE=$EMSCRIPTEN/cmake/Modules/Platform/Emscripten.cmake \
          -DCMAKE_BUILD_TYPE=Release \
	  -DZLIB_LIBRARY=$HOME/zlib-1.2.11/libz.a \
	  -DZLIB_INCLUDE_DIR=$HOME/zlib-1.2.11 \
	  -DMULTI_THREADED=OFF \
	  -DUSE_SIGNALS=OFF \
	  -DUSE_GMP=OFF \
	  -DBUILD_SWIPL_LD=OFF \
	  -DSWIPL_PACKAGES=OFF \
	  -DINSTALL_DOCUMENTATION=OFF \
	  -DSWIPL_NATIVE_FRIEND=build \
	  -G Ninja ..

### Building a 32-bit version on 64-bit Debian based Linux

Building the 32-bit version on  a  64   bit  platform  can be useful for
testing and creating  32-bit  .qlf  files   or  saved  states.  A fairly
complete system is created using the configuration command below.

    cmake -DSWIPL_M32=ON \
	  -DSWIPL_PACKAGES_JAVA=OFF -DSWIPL_PACKAGES_QT=OFF \
          -G Ninja ..

### Cross-building for targets without an emulator

In the above scenarios we have an  emulator (Wine, Node.js) that can run
the compiled Prolog system  so  we  can   do  the  Prolog  steps  of the
installation such as building  the  boot   file,  building  .qlf  files,
library indexes and the documentation. On some  systems we do not have a
suitable emulator. Experimental support is  provided using the following
steps:

  - Build a native Prolog system in a directory, say `native`.  This
    version must have the same _word-size_ (32 or 64-bits) as the
    cross-compiled target.  One the core Prolog system (no packages)
    is required and the system only needs to be build, i.e., the
    _install_ step is allowed but not needed.  See above.

  - Specify `-DSWIPL_NATIVE_FRIEND=native` for the cross-compilation.
    This will cause the above system to be used for the cross
    compilation steps.

## Development

When building SWI-Prolog using cmake a  complete installation is created
in the cmake _build_ directory. If possible,   the files from the source
tree that do not need modification are   created  as _symbolic links_ to
the real sources. This  implies  that  `src/swipl`   can  be  used  as a
complete development environment and library   and system predicates can
be edited using edit/1 and friends.

The script `scripts/swi-activate` may be used   to  create symlinks from
$HOME/bin to the version in the  current   working  directory. It may be
used to activate the system  in  the   build  directory  or  where it is
installed. It is called from the build directory as one of:

    ../scripts/swipl-activate
    ../scripts/swipl-activate --installed

Developers    may    wish    to    set    the    environment    variable
`SWIPL_INSTALL_PREFIX`,   which   is   used   as     the   default   for
`CMAKE_INSTALL_PREFIX`. Moreover, if this variable includes `@builddir@`
this  string  is  replaced  with  the  basename  of  the  current  build
directory. This aims at the following scenario:

  1. Set e.g. `export SWIPL_INSTALL_PREFIX=$HOME/cmake/@builddir@`
  2. Use multiple build directories  for   debug,  different  targets or
     different configurations.  Typically these are called `build.<config>`,
     for example `build.single-threaded`.
  3. Configure without specifying a `CMAKE_INSTALL_PREFIX`
  4. Build, test and install.  Optionally use `swipl-activate` to
     use this version as default.

When  developing  on  the  core  system  one  often  does  not  want  to
re-generate documentation and possible package dependencies. This can be
achieved using the target `core`, which   builds `swipl`, `libswipl` and
`boot.prc`:

    ninja core


### Testing

Tests are registered for use with `ctest`.  To run all tests, simply run
this in the build directory.  Tests  can   be  run  concurrently (`-j 8`
below).

    % ctest -j 8

Note that there seem to be  few   tests.  This is misleading. Each ctest
test loads a Prolog file that  may  run   hundreds  of  tests. If a test
fails,  run  the  command  below  to    get  details.  Tests  are  named
_package_:_name_, so `ctest -V -R clib:` runs   the tests for the `clib`
package.

    % ctest -V -R name

Note that all tests can be executed   interactively  by loading the test
file and calling the entry point  as   illustrated.  The  entry point is
always the base  name  of  the   file  (without  directory  and  without
extension).

    % src/swipl ../src/Tests/core/test_arith.pl
    ?- test_arith.
    % PL-Unit: div ... done
    ...

### Trapping memory issues using AddressSanitizer

[AddressSanitizer](https://en.wikipedia.org/wiki/AddressSanitizer) is an
extension to Clang and GCC to  instrument executables for finding common
memory    management    issues.    It    traps     similar    bugs    as
[Valgrind](http://valgrind.org/), but if a suspected   bug does not show
up using one tool it might  be  worthwhile   to  try  the  other. A nice
property of Valgrind is that it can   be used directly on the executable
without recompilation. The downside is that   Valgrind makes the program
run about 20 times slower. The slowdown   by AddressSanitizer is about a
factor two. To compile for using with AddressSanitizer, do e.g.,

    % mkdir build.sanitize
    % cd build.sanitize
    % cmake -DCMAKE_BUILD_TYPE=Sanitize -G Ninja ..
    % ninja

See also `cmake/BuildType.cmake` and `PL_halt()` in `src/pl-fli.c`.

You can run the tests normally using   `ctest`. Note that the `swipl:GC`
test requires more stack than the   default when using AddressSanitizer.
To fix this run (bash) `ulimit  -s   20000`  before running `ctest`. The
test   `jpl:prolog_in_java`   because   Java   is    not   loaded   with
AddressSanitizer preloaded.   All other tests should pass (about 4 times
slower than normal).


## Packaging

### Windows

The windows installer is created from the cross-compiled version using a
Linux native port of the NSIS  installer generator. Ensure `makensis` is
installed (`apt-get install nsis`) and run the  commands below to in the
build directory create the installer:

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win64.cmake -G Ninja ..
    ninja
    cpack

And, for the 32-bit version:

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win32.cmake -G Ninja ..
    ninja
    cpack


### Debian based Linux systems (.deb or .rpm)

The following commands create   swipl-<version>-<nr>.<cpu>.deb/rpm  file
with SWI-Prolog to be installed in /usr. The process creates a monolitic
installer for a particular configuration of   SWI-Prolog.  This is *not*
what is typically used to  create   packages  for  distributions. Distro
package maintainers are referred to _Modular  packages for Linux_ below.
The prodedure here is intended  to   create  custom  Debian packages for
in-house deployment.

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr -G Ninja ..
    ninja
    cpack -G DEB

for generating an RPM, use  `cpack  -G   RPM`.  The  cmake configure run
selects a default packager depending on  the availability of the package
installer `apt` (assuming DEB) or `dnf` (assuming RPM).


#### Modular packages for Linux

Most Linux distributions  with  to   install  SWI-Prolog  using multiple
packages, notably to reduce dependencies. For  example, the xpce package
os normally provided  by  a  package   `swi-prolog-x`  and  the  core of
SWI-Prolog as `swi-prolog-nox`. This  allows installing `swi-prolog-nox`
on headless servers without installing X11.

Modular installation can be based on cmake _COMPONENTS_. The files for a
particular component can be installed using, for  example (not this is a
one-line command):

    DESTDIR=$(pwd)/<component> \
        cmake -DCMAKE_INSTALL_COMPONENT=<component> \
	      -P cmake_install.cmake

The defined components are:

  | Component            | Description                          |
  | -------------------- | ------------------------------------ |
  | Core_system		 | Compiler and core libraries          |
  | Core_packages	 | Packages with few dependencies       |
  | Archive_interface	 | Libarchive binding                   |
  | Commandline_editors	 | Readline and libedit interfaces      |
  | ODBC_interface	 | ODBC binding                         |
  | BerkeleyDB_interface | BDB interface                        |
  | Perl_regex		 | PCRE library binding                 |
  | YAML_support	 | Libyaml binding                      |
  | Java_interface	 | Java interface (JPL)                 |
  | OpenSSL_interface    | Binding to OpenSSL/LibreSSL          |
  | TIPC_networking      | Linux TIPC network support           |
  | Qt_console		 | Qt windowed interface                |
  | Graphics_subsystem	 | The xpce graphics system (needs X11) |
  | Documentation	 | System HTML documentation            |
  | Examples		 | Example files		        |

See the `debian` subdirectory for the complete   set  of rules we use to
generate the Ubuntu PPA releases.

### Create a MacOSX Bundle

    cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_MACOS_BUNDLE=ON -G Ninja ..
    ninja
    cpack

## Issues

- Provide a FindSWIPL.cmake?
- Problem compiling SWI when another SWI is installed already and you
  have environment variables set to facilitate e.g., embedding in Java.
  The variable names and possibly conflicting values depend on the OS.
  See [issue](https://github.com/SWI-Prolog/swipl-devel/issues/435)





