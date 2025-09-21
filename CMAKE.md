# Building SWI-Prolog using cmake

SWI-Prolog moved to CMake for configuration   with  version 7.7.20. Soon
thereafter support for GNU autoconf and GNU make has been dropped.

The build has been tested with the  "Unix Makefiles", "Ninja" as well as
"NMake Makefiles" or "Visual Studio  17   2022"  for  Windows when using
VS2022.   Except   for   the    _Visual     Studio_    build,   we   use
[Ninja](https://ninja-build.org/) as it builds   faster,  avoids warning
from being cluttered and better facilitates debugging dependency issues.
It can be selected using `cmake  -G   Ninja  ..`,  after which the usual
`make` _target_ can be replaced by  `ninja` _target_. The examples below
all use Ninja. Drop `-G Ninja` to use classical Unix make.

## Getting cmake

Building SWI-Prolog requires cmake version  3.10   or  later. Many Linux
systems ship with a cmake package. On  MacOS we use the Macport version.
If the shipped cmake version is too old   you may wish to download cmake
from https://cmake.org/download/

## Native build

The [build SWI-Prolog from source](https://www.swi-prolog.org/build/)
page has information one how to get and build SWI-Prolog for various
platforms. Check that page also for the prerequisites depending on the
platform.

### Getting the source

The   source   may   be    downloaded    as     a    tar    ball    from
https://www.swi-prolog.org or downloaded using git. The git sequence is:

    git clone --recursive https://github.com/SWI-Prolog/swipl-devel.git

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

If the build fails, one could try to remove the entire `build` directory
and re-create it as  above.  Note  that   the  build  process  makes  no
modifications outside the `build` directory.


## Build types

The default build type is `RelWithDebInfo`. Alternatives may be selected
using e.g., See [Profile Guided Optimization](#PGO)   for details on the
PGO build for maximum performance.

    cmake -DCMAKE_BUILD_TYPE=Debug -G Ninja ..
    cmake -DCMAKE_BUILD_TYPE=Release -G Ninja ..
    cmake -DCMAKE_BUILD_TYPE=PGO -G Ninja ..

## Install location

To install in a particular   location, use `-DCMAKE_INSTALL_PREFIX`. For
example, this will build SWI to be installed in ``~/bin``.

    cmake -DCMAKE_INSTALL_PREFIX=$HOME -G Ninja ..

After `ninja install`, `swipl` will be in `~/bin/swipl`.


## Customizing SWI-Prolog

By default the system configures all   features. Several cmake _options_
allow for restricting the system, define   the  layout of the filesystem
and libraries that are built.

  | Option                        | Description                           |
  | ----------------------------- | -------------------------------------- |
  | `-DMULTI_THREADED=OFF`        | Drop support for Prolog threads        |
  | `-DENGINES=OFF`               | Drop support for Prolog engines        |
  | `-DUSE_SIGNALS=OFF`           | Drop signal support                    |
  | `-DUSE_GMP=OFF`               | Use bundled LibBF instead of GMP       |
  | `-DUSE_TCMALLOC=OFF`          | Do not link against `-ltcmalloc`       |
  | `-DVMI_FUNCTIONS=ON`          | Use functions for the VM instructions  |
  | `-DSWIPL_SHARED_LIB=OFF`      | Build Prolog kernel as static lib      |
  | `-DSWIPL_STATIC_LIB=ON`       | Also build `libswipl_static.a`         |
  | `-DSTATIC_EXTENSIONS=ON`      | Include packages into the main system  |
  | `-DSWIPL_INSTALL_IN_LIB=ON`   | Install libswipl.so in `<prefix>/lib`  |
  | `-DSWIPL_INSTALL_IN_SHARE=ON` | Install docs in `<prefix>/share`       |
  | `-DSWIPL_CC=<string>`         | Default for `c_cc` flag                |
  | `-DSWIPL_CXX=<string>`        | Default for `c_cxx` flag               |
  | `-DSWIPL_PACKAGES=OFF`        | Only build the core system             |
  | `-DSWIPL_PACKAGES_BASIC=OFF`  | Drop all basic packages                |
  | `-DSWIPL_PACKAGES_ODBC=OFF`   | Drop ODBC and CQL packages             |
  | `-DSWIPL_PACKAGES_JAVA=OFF`   | Drop JPL Java interface                |
  | `-DSWIPL_PACKAGES_GUI=OFF`    | Drop graphics (xpce)                   |
  | `-DSWIPL_PACKAGE_LIST=List`   | ;-separated list of packages           |
  | `-DBUILD_TESTING=OFF`         | Do not setup for ctest unit tests      |
  | `-DINSTALL_TESTS=ON`          | Add tests to installed system          |
  | `-DINSTALL_DOCUMENTATION=OFF` | Drop generating the HTML docs          |
  | `-DINSTALL_QLF=ON`            | Compile and install library .qlf files |
  | `-DINSTALL_PROLOG_SRC=OFF`    | Do not install library .pl files       |

Note that packages for  which  the   prerequisites  cannot  be found are
dropped automatically, as are packages  for   which  the sources are not
installed.

Note  that  many  combinations  of  these  options  are  not  properly
supported.  You are strongly encouraged to install the full system for
desktop usage. When installing  in lightweight and server environments
one    may   drop    one    or    more   of    ``SWIPL_PACKAGES_GUI``,
``SWIPL_PACKAGES_JAVA``,          ``SWIPL_PACKAGES_ODBC``          and
``INSTALL_DOCUMENTATION``.

A   specific   list   of    packages     can    be    requestion   using
`DSWIPL_PACKAGE_LIST` set to a list of package.  The list is checked for
missing dependencies, which  are  automatically   added.  Typically  the
documentation should be disabled in this   scenario because including it
includes many packages. For example:

    cmake -DINSTALL_DOCUMENTATION=OFF -DSWIPL_PACKAGE_LIST="clib;plunit"

### Customizing GUI fonts

The        GUI        (xpce)         renders        fonts        using
[Pango](https://www.gtk.org/docs/architecture/pango).   XPCE specifies
its default  named fonts from  using abstract families  `mono`, `sans`
and `serif`.   The mapping may be  specified by the user.   There is a
default  mapping for  Windows, MacOS  and others  (Linux, *BSD,  ...).
This mapping can be overruled using, e.g., the following CMake option:

	-DSANS_FAMILY='"DejaVu Sans,sans"'

Similarly,  there  are  the ``-DMONO_FAMILY``  and  ``-DSERIF_FAMILY``
options.   Note that  the argument  is  a C  string that  needs to  be
protected  against  the  shell.   A  font  specification  is  a  comma
separated list of  font names that are processed in  order, i.e., if a
character must be written it uses the first font that provides a glyph
for this  character.  These CMake  options are primarily  intended for
creating a port for a particular environment.


## Finding requirements

Finding  requirements  is   the   task    of   CMake.   Typically,   our
`CMakeLists.txt`  files  call  `find_package(SomePackage,  ...)`,  which
implies it loads `FindSomePackage.cmake`. As far  as possible we rely on
the "finders" that come bundled with CMake.   Others can be found in the
various `cmake` directories. These are either copied from other projects
or home brewed. Please consult the   CMake documentation on the specific
"finder" as well as `find_package()` if   you  have trouble finding some
requirement or selecting the right version if you have multiple versions
of the requirement installed on your system.

In                            particular,                            see
[FindPython.cmake](https://cmake.org/cmake/help/latest/module/FindPython.html)
to control the Python version used by the Janus interface to Python. For
example, if you want to use  a   particular  Python library (for example
from   a   (`conda`)   environment),   you     can    use   the   option
`-DPython_LIBRARY:FILEPATH=/path/to/your/library`                    and
`-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE` to make sure   the library is
found also at runtime.


## Embedding SWI-Prolog in Java, C, C++, etc.

If SWI-Prolog is to be embedded in another executable it must be able to
find its home directory and the main   application  must be able to find
the SWI-Prolog shared library `libswipl.so`   (extension  depends on the
platform).  The following environment variables are commonly used:

- `SWI_HOME_DIR` should point at SWI-Prolog's main directory, e.g.
  ``${CMAKE_INSTALL_PREFIX}/lib/swipl``
- The shared object search path should include the directory where
  `libswipl.{so,dll,...}` resides.  The variable depends on the platform.
  Some popular names:

  - `LD_LIBRARY_PATH` (ELF based systems such as Linux)
  - `DYLD_LIBRARY_PATH` (MacOS)
  - `PATH` (Windows)

If you build SWI-Prolog  you  must   __remove  these  variables from the
environment when building__. Failure  to  do   so  may  cause  the build
process to use parts  of  an   incompatible  installed  system.  Running
`cmake` warns if  such  an  environment   variable  is  found,  but  the
environment must be cleaned when running `ninja` or `make`.

## Profile Guided Optimization {#PGO}

When using Ninja and GCC, the system  may be built using _Profile Guided
Optimization_ (PGO). This  first  builds   the  system  instrumented  to
collect profile information, then runs  a   benchmark  suite and finally
recompiles it using the benchmark suite  output to help (notably) branch
prediction. The performance improvement  using   modern  GCC versions is
about 30-40%

    cmake -DCMAKE_BUILD_TYPE=PGO -G Ninja ..
    ninja

Older versions provided a  helper   script  in `scripts/pgo-compile.sh`.
This is now a dummy script  that   runs  `cmake --build`. Note that this
simply builds the  system.  The  build   type  must  be  set  beforehand
explicitly as indicated above.


## Cross build

Cross  building  for  Windows  is  supported    by  means  of  a  Docker
specification that can be found at the  location below. We advice to use
the docker. Of course you can  use   the  recipies  in the Dockerfile to
perform the process on your host Linux system.

    https://github.com/SWI-Prolog/docker-swipl-build-mingw


### WASM (Emscripten)

See https://www.swi-prolog.org/build/WebAssembly.html for details.

For   latest    news   on   the    WASM   version   see    the   [Wiki
page](https://swi-prolog.discourse.group/t/swi-prolog-in-the-browser-using-wasm).
This page also discusses how to  use the WASM version with Node.js and in
a browser.


### Cross-building for targets without an emulator

In the above scenarios we have an  emulator (Wine, Node.js) that can run
the compiled Prolog system  so  we  can   do  the  Prolog  steps  of the
installation such as building  the  boot   file,  building  .qlf  files,
library indexes and the documentation. On some  systems we do not have a
suitable emulator. Experimental support is  provided using the following
steps:

  - Build a native Prolog system in a directory, say `native`.  This
    version must have the same _word-size_ (32 or 64-bits) as the
    cross-compiled target.  One the core Prolog system (no packages) is
    required and the system only needs to be build, i.e., the _install_
    step is allowed but not needed.  See above.

  - Specify `-DSWIPL_NATIVE_FRIEND=native` for the cross-compilation.
    This will cause the above system to be used for the cross compilation
    steps.

## Development

When building SWI-Prolog using cmake a  complete installation is created
in the cmake _build_ directory. If possible,   the files from the source
tree that do not need modification are   created  as _symbolic links_ to
the real sources. This  implies  that  `src/swipl`   can  be  used  as a
complete development environment and library   and system predicates can
be edited using edit/1 and  friends.   Note  that current cmake supports
``-DCMAKE_INSTALL_MODE=ABS_SYMLINK``,  installing  the    system   using
symbolic links to provide  a  similar   result.  The  advantage of using
``-DCMAKE_INSTALL_MODE=ABS_SYMLINK`` is that all files are in the target
position while this is  not  the  case   when  running  from  the  build
directory. The disadvantage is that  ``ninja   install``  must  still be
executed on changes such as adding new files to the library.

The script `scripts/swi-activate` may be used   to  create symlinks from
`$HOME/bin` to the version in the  current   working  directory. It may
be used to activate the system  in  the   build  directory  or  where it
is installed. It is called from the build directory as one of:

    ../scripts/swipl-activate
    ../scripts/swipl-activate --installed

Developers    may    wish    to    set    the    environment    variable
`SWIPL_INSTALL_PREFIX`,   which   is   used   as     the   default   for
`CMAKE_INSTALL_PREFIX`. Moreover, if this variable includes `@builddir@`
this  string  is  replaced  with  the  basename  of  the  current  build
directory. This aims at the following scenario:

  1. Set e.g. `export SWIPL_INSTALL_PREFIX=$HOME/cmake/@builddir@`
  2. Use multiple build directories  for   debug,  different  targets or
     different configurations.  Typically these are called
     `build.<config>`, for example `build.single-threaded`.
  3. Configure without specifying a `CMAKE_INSTALL_PREFIX`
  4. Build, test and install.  Optionally use `swipl-activate` to use
     this version as default.

When  developing  on  the  core  system  one  often  does  not  want  to
re-generate documentation and possible package dependencies. This can be
achieved using the target `core`, which   builds `swipl`, `libswipl` and
`boot.prc`:

    ninja core

Note that when using the `swi-activate` script, any system-wide version
installed (e.g., from a Linux distribution) may be occluded, since the
symbolic link created at `$HOME/bin/` will have precedence over, e.g.,
`/usr/bin/swipl`. Delete the created symbolic link if you would like to
come back to the distribution-based installed version.

### Multiple configurations using scripts/configure

As building in a subdirectory does not  modify the sources, you may have
multiple  build  directories  holding   built    systems   in  different
configurations. Each of these may be executed using `src/swipl` from the
build directory. This is supported   by  the script `scripts/configure`.
This script is executed in a  clean   build  directory. It assembles the
command line options and environment for   running  `cmake` and building
the system based on the name of the build directory. The general name of
the build directory is as below, where   each _feat_ enables or disables
some feature or aspect of the  build.   Check  the script for recognised
features.

    build.feat1-feat2-...

For example, to build  a  system  using   the  `clang`  C  compiler with
AddressSanitizer, use

    mkdir build.clang.asan
    cd build.clang.asan
    ../script/configure
    (direnv allow)
    ninja

The script writes a  script  `configure`   to  the  build directory that
allows you to inspect or re-run   the  configuration and, if environment
variables are required, a file  `.envrc`   for  the  `direnv` utility to
manage the environment when running a shell in the build directory.

A typical set of versions for development is

  - `build` for a clean default Release build
  - `build.pgo` for a PGO optimized Release build
  - `build.debug` for a Debug build
  - `build.asan` for using AddressSanitizer (see below)

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

    % src/swipl ../tests/core/test_arith.pl
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

    % mkdir build.asan
    % cd build.asan
    % cmake -DCMAKE_BUILD_TYPE=Sanitize -G Ninja ..
    % ninja

See also `cmake/BuildType.cmake` and `PL_halt()` in `src/pl-fli.c`.

You can run the tests normally using   `ctest`. Note that the `swipl:GC`
test requires more stack than the   default when using AddressSanitizer.
To fix this run (bash) `ulimit -s unlimited` before running `ctest`. The
test `jpl:prolog_in_java` will fail because  Java   is  not  loaded with
AddressSanitizer preloaded. All other tests should   pass (about 4 times
slower than normal).

By   default,   memory   leak   checking   is   disabled   by   defining
`__asan_default_options()` in `pl-main.c`. Leak checking  may be enabled
by setting `ASAN_OPTIONS`:

    % ASAN_OPTIONS=detect_leaks=1 src/swipl ...

This option also causes Prolog  __not__   to  unload foreign extensions,
which is needed to  make  ASAN   properly  report  locations  in foreign
extensions.

AddressSanitizer is reported (by Alessandro Bartolucci)   not to work on
Apple using the xCode AppleClang. It should  work with a non-Apple Clang
or GCC version.


## Packaging

### Windows

The windows installer is created from the cross-compiled version using a
Linux native port of the NSIS  installer generator. Ensure `makensis` is
installed  (`apt-get install nsis`)  and  run the commands below  in the
build directory to create the installer:

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win64.cmake -G Ninja ..
    ninja
    cpack

And, for the 32-bit version:

    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=../cmake/cross/linux_win32.cmake -G Ninja ..
    ninja
    cpack


### Debian based Linux systems (`.deb` or `.rpm`)

The  following  commands  create  `swipl-<version>-<nr>.<cpu>.deb/rpm`
file with SWI-Prolog to be installed in `/usr`.  The process creates a
monolithic  installer for  a particular  configuration of  SWI-Prolog.
This  is  *not*  what  is   typically  used  to  create  packages  for
distributions.  Distro  package maintainers  are referred  to _Modular
packages for Linux_  below.  The procedure here is  intended to create
custom packages for in-house deployment.

    cmake -DCMAKE_BUILD_TYPE=PGO -DCMAKE_INSTALL_PREFIX=/usr -G Ninja ..
    ninja
    cpack

The `cmake` configure run selects  a default packager depending on the
availability of  the package installer  `apt` (assuming DEB)  or `dnf`
(assuming RPM).  The packager can  be selected explicitly using `cpack
-G DEB` or `cpack -G RPM`


#### Modular packages for Linux

Most Linux distributions  with  to   install  SWI-Prolog  using multiple
packages, notably to reduce dependencies. For  example, the xpce package
os normally provided  by  a  package   `swi-prolog-x`  and  the  core of
SWI-Prolog as `swi-prolog-nox`. This  allows installing `swi-prolog-nox`
on headless servers without installing X11.

Modular installation can be based on cmake _COMPONENTS_. The files for a
particular component can be installed using, for  example  (note this is
a one-line command):

    DESTDIR=$(pwd)/<component> \
        cmake -DCMAKE_INSTALL_COMPONENT=<component> \
              -P cmake_install.cmake

The defined components are:

  | Component            | Description                          |
  | -------------------- | ------------------------------------ |
  | Core_system          | Compiler and core libraries          |
  | Core_packages        | Packages with few dependencies       |
  | Archive_interface    | Libarchive binding                   |
  | Commandline_editors  | Readline and libedit interfaces      |
  | ODBC_interface       | ODBC binding                         |
  | BerkeleyDB_interface | BDB interface                        |
  | Perl_regex           | PCRE2 library binding                |
  | YAML_support         | Libyaml binding                      |
  | Java_interface       | Java interface (JPL)                 |
  | Python_interface     | Python interface (Janus)             |
  | OpenSSL_interface    | Binding to OpenSSL/LibreSSL          |
  | TIPC_networking      | Linux TIPC network support           |
  | Graphics_subsystem   | The xpce graphics system (needs X11) |
  | Documentation        | System HTML documentation            |
  | Examples             | Example files                        |

See the `debian` subdirectory for the complete   set  of rules we use to
generate the Ubuntu PPA releases.

### Create a MacOSX Bundle

    cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_MACOS_BUNDLE=ON -G Ninja ..
    ninja
    cpack

## Issues

- Problem compiling SWIPL when another SWIPL is installed already and you
  have environment variables set to facilitate e.g., embedding in Java.
  The variable names and possibly conflicting values depend on the OS.
  See [issue](https://github.com/SWI-Prolog/swipl-devel/issues/435).
- Potential problems when having multiple parallel installations of SWIPL
  (e.g., distribution-based, build, manually installed), and environment
  variables `SWI_HOME_DIR` or `SWIPL` set to specific SWIPL's home
  directory. Read above.
