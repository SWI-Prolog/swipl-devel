Installing SWI-Prolog
=====================

The guide for installing SWI-Prolog  from   source  is maintained on the
SWI-Prolog website at the following location.  Here you notably find how
to download the system, prepare the sources and get the dependencies for
your environment.

    https://www.swi-prolog.org/build/

Hints for using CMake to configure SWI-Prolog are in CMAKE.md.

Installing from source is typically  a  good   idea  on  Linux  and *BSD
systems. On MacOS it is adviced to   use the binary installer or install
from  source  using  Macports  or  Homebrew.   The  Windows  version  is
cross-compiled on a Linux machine  using   MinGW.  We  provide a [Docker
recipe                                                               for
building](https://github.com/SWI-Prolog/docker-swipl-build-mingw).

## Bluffer's guide

First, get the dependencies. We maintain   dependencies  per platform at
https://www.swi-prolog.org/build/ and linked pages.

With the dependencies installed, the basic installation is:

```
git clone --recurse-submodules https://github.com/SWI-Prolog/swipl-devel.git
cd swipl-devel
mkdir build
cd build
../scripts/configure
ninja
```

The ``../scripts/configure`` script looks at   extensions to the `build`
directory name to  setup  specific   configurations.  The  example below
compiles the system with _Profile  Guided Optimization_ (recommended for
gcc). Multiple features can be  used,   separated  using dashes or dots,
e.g. ``build.pgo-libbf`` or ``build.single-pgo``. The   script  does not
know about conflicting features. Use   ``../script/configure --help`` to
see all supported features.

```
mkdir build.pgo
cd build.pgo
../scripts/configure
ninja
```


Now you may run SWI-Prolog as `src/swipl` or `src/swipl-win` (gui) from the
installation directory.   Alternatively you may

  1. Link the build system from your personal binary directory using

         ../scripts/swipl-activate

  2. Install the system using ``ninja install``

