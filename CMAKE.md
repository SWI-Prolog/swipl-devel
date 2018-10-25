# Building SWI-Prolog using cmake

Currently SWI-Prolog has cmake CMakeLists.txt   configuration files that
cover almost the entire project and builds   on Ubuntu 18.04, Fedore 28,
MacOSX and cross compilation for Win32 as well as Win64 on Ubuntu 18.04

It has been tested with  the   "Unix  Makefiles" and "Ninja" generators.
Ninja performs notably better. It can be  selected using "cmake -G Ninja
...", after which the usual `make` _target_   can be replaced by `ninja`
_target_. As Ninja gives a better   build  experience the examples below
all use Ninja.

## Getting cmake

Building SWI-Prolog requires cmake version ??.   Many Linux systems ship
with a cmake package. On  MacOS  we   use  the  Macport  version. If the
shipped cmake version is too old  you   may  wish to download cmake from
https://cmake.org/download/


## Native build

### Getting the source

The   source   imay   be    downloaded    as     a    tar    ball   from
http://www.swi-prolog.org or downloaded using git.  The git sequencen is:

    git clone https://github.com/SWI-Prolog/swipl-devel.git
    git submodule update --init


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

    cmake -DCMAKE_BUILD_TYPE=Debug
    cmake -DCMAKE_BUILD_TYPE=Release

## Customizing SWI-Prolog

By default the system configures all   features. Several cmake _options_
allow for restricting the system.

  | `-DMULTI_THREADED=OFF`        | Drop support for Prolog threads   |
  | `-DUSE_GMP=OFF`               | Drop bignum and rational numbers  |
  | `-DSWIPL_SHARED_LIB=OFF`      | Build Prolog kernel as static lib |
  | `-DSWIPL_PACKAGES=OFF`        | Only build the core system        |
  | `-DSWIPL_PACKAGES_BASIC=OFF`  | Drop all basic packages           |
  | `-DSWIPL_PACKAGES_ODBC=OFF`   | Drop ODBC and CQL packages        |
  | `-DSWIPL_PACKAGES_JAVA=OFF`   | Drop JPL Java interface           |
  | `-DSWIPL_PACKAGES_X=OFF`      | Drop graphics (xpce)              |
  | `-DINSTALL_DOCUMENTATION=OFF` | Drop generating the HTML docs     |

Note that packages for  which  the   prerequisites  cannot  be found are
dropped automatically, as are packages  for   which  the sources are not
installed.

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
    ../scripts/swipl-activate --intalled

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

  - Build documention
    - See whether we can eliminate Perl dependency (doc2tex)
  - Use PGO (profile guided optimization)
    - See https://gist.github.com/daniel-j-h/c4b109bff0b717fc9b24
  - Provide a FindSWIPL.cmake?
