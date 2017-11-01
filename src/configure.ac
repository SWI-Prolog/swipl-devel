dnl Process this file with autoconf to produce a configure script.

AC_INIT(pl-wam.c)
AC_PREREQ([2.66])

AC_CONFIG_HEADER(config.h)

m4_include([ac/ax_c_float_words_bigendian.m4])

AC_SUBST(COFLAGS)
AC_SUBST(CWFLAGS)
AC_SUBST(CMFLAGS)
AC_SUBST(CIFLAGS)
AC_SUBST(AR)
AC_SUBST(MKDIR)
AC_SUBST(ETAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LDOFLAGS)
AC_SUBST(PROGLDFLAGS)
AC_SUBST(STACKFLAGS)
AC_SUBST(SUBSYSTEMFLAGS)
AC_SUBST(PLBUILD)
AC_SUBST(PLBASE)
AC_SUBST(PLARCH)
AC_SUBST(INSTALL_PLARCH)
AC_SUBST(PLSHARED)
AC_SUBST(PLTHREADS)
AC_SUBST(PL)
AC_SUBST(PLVERSION)
AC_SUBST(PLNVERSION)
AC_SUBST(PLLIBDIR)		dnl either @PL@-@PLVERSION@ or @PL@
AC_SUBST(VPATH)
AC_SUBST(COUTOPT)
AC_SUBST(INSTALLTARGET)
AC_SUBST(EXTRAOBJ)
AC_SUBST(BOOTFILE)
AC_SUBST(PLLIB)			dnl libswipl.a
AC_SUBST(PLSO)			dnl libswipl.so
AC_SUBST(SONAMEOPT)		dnl ld options for linking shared objects
AC_SUBST(SONAME)		dnl libswipl SONAME
AC_SUBST(USEPLLIB)		dnl -lswipl
AC_SUBST(LNLIBS)		dnl libs to link with Prolog
AC_SUBST(XLIBS)                 dnl libs to link with plterm
AC_SUBST(DBGLIBS)		dnl extra libs for debugging
AC_SUBST(LDEXE)
AC_SUBST(EXTRAHDR)
AC_SUBST(SO_EXT)		dnl e.g., "so"
AC_SUBST(SO_LDFLAGS)		dnl e.g., "-shared"
AC_SUBST(SOLIB)
AC_SUBST(TXTEXT)
AC_SUBST(MKLIBS)		dnl system libraries to make
AC_SUBST(BUILDDIR)
AC_SUBST(CNFG)
AC_SUBST(DLLIBVAR)
AC_SUBST(RLPREFIX)
AC_SUBST(OPTIMISE)
AC_SUBST(MINGW)
AC_SUBST(MINGWPTHREAD)
AC_SUBST(WINDRES)
AC_SUBST(WINE)
AC_SUBST(MSLIBEXE)			dnl how to call lib.exe
AC_SUBST(WINEPATHEXE)
AC_SUBST(HOST_TOOLCHAIN_FLAG)
AC_SUBST(HOST_OS_FLAG)
AC_SUBST(DEFSTARTUP)
AC_SUBST(EXEFORMAT)

if test -z "$CIFLAGS"; then
    CIFLAGS="$CPPFLAGS"
else
    CFLAGS="$CIFLAGS $CFLAGS"
    CPPFLAGS="$CIFLAGS $CPPFLAGS"
fi

if test ! -z "$COFLAGS"; then
    CFLAGS="$COFLAGS $CFLAGS"
    CPPFLAGS="$COFLAGS $CPPFLAGS"
fi

if test -z "$CIFLAGS" -a -z "$COFLAGS"; then
  if test ! -z "$CFLAGS"; then
    CIFLAGS="$CFLAGS"
  fi
fi

EXTRAOBJ=""
EXTRAHDR=""
INSTALLTARGET=dv-install
if test -z "$PL"; then PL=swipl; fi
PLLIB=libswipl.a
MKLIBS=""
PLVERSION=`cat ../VERSION`
CNFG=""				dnl Package configuration options
DEFSTARTUP=.swiplrc

AC_ARG_ENABLE(mt, [  --disable-mt                Disable multi-threading],
             [case "$enableval" in
                  yes) MT=yes
                       ;;
                  *)  ;;
              esac],
              MT=yes)

dnl ****************************************************************
dnl Some systems don't appear to have true.  Hope all like this ...
dnl ****************************************************************

true()
{ test a = a
}

dnl ****************************************************************
dnl Get pure numeric version (needed for MacOS -current-version)
dnl ****************************************************************

case "$PLVERSION" in
  *-*)
    pversion=$(echo $PLVERSION | sed 's/-.*//')
    vtag=$(echo $PLVERSION | sed 's/.*-//')
    case "$vtag" in
      rc*)   rev=$(echo $vtag | sed 's/rc//')
             ;;
      alpha) rev=1
	     ;;
      beta)  rev=2
	     ;;
      *)     rev=0
	     ;;
    esac
    PLNVERSION="$pversion.$rev"
    ;;
  *)
    PLNVERSION="$PLVERSION"
    ;;
esac


dnl ****************************************************************
dnl Find the build directory, needed for building the packages
dnl ****************************************************************

AC_MSG_CHECKING(builddir)
wd=`pwd`
BUILDDIR=`basename $wd`
touch conftest.unique
if test -r ../$BUILDDIR/conftest.unique; then
    AC_MSG_RESULT($BUILDDIR)
else
    AC_MSG_RESULT(no)
    BUILDDIR=src
fi
rm -f conftest.unique

AC_PROG_CC
AC_PROG_CXX		dnl used for packages
LDEXE="$CC"
AC_CANONICAL_HOST
changequote(,)dnl
if test "x$PLARCH" = "x"; then
  case "$host" in
    *linux*)
      PLARCH=`echo $host | sed 's/-.*/-linux/'`
      ;;
    i?86-*mingw32*)
      # Make PLARCH match the definition in src/config/win32.h
      PLARCH=i386-win32
      ;;
    x86_64-*mingw32*)
      # Make PLARCH match the definition in src/config/win64.h
      PLARCH=x64-win64
      ;;
    *-gnu)
      PLARCH=`echo $host | sed 's/\([^-]*\)-[^-]*-\([^-]*\)-gnu/\1-\2/'`
      ;;
    *)
      PLARCH=`echo $host | sed 's/\([^-]*\)-[^-]*-\([^-]*\)/\1-\2/'`
      ;;
  esac
fi
changequote([,])dnl

changequote(,)dnl
if test "x$PLBUILD" = "x"; then
  case "$build" in
    *linux*)
      PLBUILD=`echo $build | sed 's/-.*/-linux/'`
      ;;
    i?86-*mingw32*)
      # Make PLBUILD match the definition of PLARCH in src/config/win32.h
      PLBUILD=i386-win32
      ;;
    x86_64-*mingw32*)
      # Make PLBUILD match the definition of PLARCH in src/config/win64.h
      PLBUILD=x64-win64
      ;;
    *-gnu)
      PLBUILD=`echo $build | sed 's/\([^-]*\)-[^-]*-\([^-]*\)-gnu/\1-\2/'`
      ;;
    *)
      PLBUILD=`echo $build | sed 's/\([^-]*\)-[^-]*-\([^-]*\)/\1-\2/'`
      ;;
  esac
fi
changequote([,])dnl

# Determine the C compiler and executable suffix on the build system.
SWI_PROG_CC_FOR_BUILD
SWI_PROG_EXEEXT_FOR_BUILD

AC_ARG_ENABLE(shared, [  --enable-shared	  Prolog kernel as shared obj],
	      [case "$enableval" in
		   yes) MKSHARED=yes
			;;
		   no)  MKSHARED=no
			;;
		   *)   ;;
	       esac])

AC_ARG_ENABLE(static, [  --disable-static	  Do not install static library],
	      [case "$enableval" in
		   yes) MKSTATIC=yes
			;;
		   no)  MKSTATIC=no
			;;
		   *)   ;;
	       esac],
               MKSTATIC=yes)

AC_ARG_WITH(gpl, [  --without-gpl		  Disable GPL components],
	      INCLUDE_GPL="$withval",
	      INCLUDE_GPL=yes)
AC_ARG_WITH(lgpl, [  --without-lgpl		  Disable LGPL components],
	      INCLUDE_LGPL="$withval",
	      INCLUDE_LGPL=yes)

if test "$INCLUDE_LGPL" = no; then
  INCLUDE_GPL=no
fi

if test "$MKSHARED" != yes -a "$MKSTATIC" != yes; then
  AC_MSG_ERROR("--disable-static selected without explicitly selecting --enable-shared")
fi

if test "$MKSTATIC" = yes; then
  MKLIBS=staticlib
fi

AC_ARG_ENABLE(gmp, [  --disable-gmp		  Disable Multi-precision arithmetic],
	      configgmp="$enableval",
	      configgmp=$INCLUDE_LGPL)

AC_ARG_ENABLE(dmalloc, [  --enable-dmalloc	  Enable DMALLOC debugging],
	      dmalloc="$enableval",
	      dmalloc=no)

AC_ARG_ENABLE(useprofile, [  --enable-useprofile	  GCC: recompile with profile],
	      useprofile="$enableval",
	      useprofile=no)
AC_ARG_ENABLE(libdirversion, [  --disable-libdirversion	Do not version installation dir],
	      libdirversion="$enableval",
	      libdirversion=yes)

if test "$libdirversion" = no; then
   PLLIBDIR="$PL"
else
   PLLIBDIR="$PL-$PLVERSION"
fi

OPTIMISE=noprof


dnl ****************************************************************
dnl Setup PLBASE.  Need to expand prefix first as this isn't yet set. Yek
dnl ****************************************************************

test "x$prefix" = xNONE && prefix=$ac_default_prefix

dnl Set directory structure parameters
case "$PLARCH" in
    *-win32|*-win64)
        PLBASE=$prefix/$PLLIBDIR
        SOLIB=bin
        TXTEXT=.TXT
        ;;
    *)
        PLBASE=$libdir/$PLLIBDIR
        INSTALL_PLARCH=$PLARCH
        SOLIB=lib
        ;;
esac

dnl ****************************************************************
dnl Checking for GNU make vpath construct
dnl ****************************************************************

AC_CHECK_PROGS(MAKE, gmake make, "make")
AC_MSG_CHECKING("whether make is GNU-make")
if ($MAKE -v 2>&1) | grep GNU > /dev/null; then
    AC_MSG_RESULT(yes)
    VPATH="vpath %.c"
else
    VPATH="VPATH = "
    AC_MSG_RESULT(no)
fi

dnl ****************************************************************
dnl Checking mkdir -p
dnl ****************************************************************

AC_MSG_CHECKING("whether mkdir accepts -p")
if (mkdir -p config.tmpdir/p && mkdir -p config.tmpdir/p 2>&1) > /dev/null; then
    AC_MSG_RESULT(yes)
    MKDIR="mkdir -p"
else
    AC_MSG_RESULT(no)
    MKDIR=mkdir
fi
rm -rf config.tmpdir

dnl ****************************************************************
dnl Finding various programs
dnl ****************************************************************

AC_CHECK_TOOLS(AR, ar, ar)
AC_CHECK_TOOLS(RANLIB, ranlib, ranlib)
AC_CHECK_TOOLS(WINDRES, windres, ":")
AC_CHECK_PROG(CHRPATH, chrpath, chrpath, ":")
AC_CHECK_PROGS(ETAGS, emacs-etags etags ctags, ":")
AC_PROG_AWK
AC_PATH_PROG(SED, sed, /bin/sed)
AC_PATH_PROG(PERL, perl, /usr/bin/perl)
AC_PROG_INSTALL
dnl Make all paths absolute as we are running some of these scripts
dnl from different locations.  Note this can also be ../src/install-sh
case "$INSTALL" in
    .*) INSTALL="`pwd`/$INSTALL" ;;
esac
case "$INSTALL_PROGRAM" in
    .*) INSTALL_PROGRAM="`pwd`/$INSTALL_PROGRAM" ;;
esac
case "$INSTALL_SCRIPT" in
    .*) INSTALL_SCRIPT="`pwd`/$INSTALL_SCRIPT" ;;
esac
case "$INSTALL_DATA" in
    .*) INSTALL_DATA="`pwd`/$INSTALL_DATA" ;;
esac
AC_PROG_CPP
AC_DEFINE_UNQUOTED(PROG_CPP, "$CPP",
		   [Program to run the C preprocessor])
AC_ISC_POSIX
AC_HEADER_STDC
AC_SYS_LARGEFILE

dnl ****************************************************************
dnl * Test for ELF
dnl ****************************************************************

AC_RUN_IFELSE(
    [ AC_LANG_PROGRAM(
	  [],
	  [
#ifdef __ELF__
return 0;
#else
return 1;
#endif
	  ])
    ],
    [EXEFORMAT=ELF],
    [EXEFORMAT=unknown],
    [EXEFORMAT=unknown])

if test "x$useprofile" = "xyes"; then
  AC_MSG_CHECKING("whether to use GCC -fprofile-use")
  if test "$GCC" = "yes"; then
    AC_MSG_RESULT(yes)
    OPTIMISE=prof
  else
    AC_MSG_RESULT(no)
  fi
fi

AC_ARG_ENABLE(custom-flags, [  --disable-custom-flags  Do not add custom build optimization flags],
	custom_flags=$enableval,
	custom_flags=yes)
if test "x$custom_flags" = "xyes" ; then
	if test "$GCC" = "yes"; then
		COFLAGS="${COFLAGS--O2 -fno-strict-aliasing}"
		LDOFLAGS="${LDFLAGS--O2}"
		CWFLAGS="${CWFLAGS--Wall}"
	else
		COFLAGS="${COFLAGS--O}"

		case "$PLARCH" in
			*mips-irix*)
				COFLAGS="${COFLAGS--O -OPT:Olimit_opt=on}"
			;;
		esac
	fi
else
	COFLAGS=$CFLAGS
fi

PROGLDFLAGS="$LDFLAGS"

dnl ****************************************************************
dnl Other special flags
dnl
dnl On SCO Unix, there are ELF and COFF binaries.  We want elf to
dnl provide for dlopen() and friends.  With help from Tamas Laufer!
dnl
dnl On Alpha machines we need to enable IEEE float arithmetic to avoid
dnl FPE signals on float underflow/overflow
dnl ****************************************************************

case "$PLARCH" in
    *sco*|*SCO*)
	AC_MSG_CHECKING("SCO-Unix.  See if we need -belf")
	old_cflags="$CFLAGS"
	CFLAGS="$CFLAGS -belf"
	AC_TRY_COMPILE(
	[
	],
	[
	],
	CMFLAGS="$CMFLAGS -belf"
	AC_MSG_RESULT(yes),
	AC_MSG_RESULT(no)
	CFLAGS="$old_cflags")
	;;
    *alpha*)
	if test "$GCC" = "yes"; then
	  CMFLAGS="$CMFLAGS -mieee"
	else
	  CMFLAGS="$CMFLAGS -ieee"
	fi
        ;;
    *-win32)
	CMFLAGS="$CMFLAGS -D__WINDOWS__ -DWIN32"
	;;
    *-win64)
	CMFLAGS="$CMFLAGS -D__WINDOWS__ -DWIN64"
	;;
esac

dnl Set swipl-ld host parameters
case "$PLARCH" in
    *-win32|*-win64)
        HOST_TOOLCHAIN_FLAG=-DHOST_TOOLCHAIN_MINGW
        HOST_OS_FLAG=-DHOST_OS_WINDOWS
       ;;
esac

AC_C_INLINE
AC_FUNC_ALLOCA
AC_C_BIGENDIAN
AX_C_FLOAT_WORDS_BIGENDIAN

dnl This fragment is from the glibc configure script
dnl with modification to fit different platforms

if test "$GCC" = "yes"; then
  AC_CACHE_CHECK(whether __attribute__((visibility())) is supported,
		 libc_cv_visibility_attribute,
                 [cat > conftest.c <<EOF
                  int foo __attribute__ ((visibility ("hidden"))) = 1;
EOF
                  libc_cv_visibility_attribute=no
                  if ${CC-cc} -Werror -S conftest.c -o conftest.s >/dev/null 2>&1; then
                    if grep -E "\.(private_extern|hidden).*foo" conftest.s >/dev/null; then
                      libc_cv_visibility_attribute=yes
                    fi
                  fi
                  rm -f conftest.[cs]
                 ])
  if test $libc_cv_visibility_attribute = yes; then
    AC_DEFINE(HAVE_VISIBILITY_ATTRIBUTE, 1,
	      [Define if __attribute__ visibility is supported])
  fi
fi

case "$PLARCH" in
    *aix*)
	EXTRAHDR=exports
	;;
esac


dnl ****************************************************************
dnl How script args are handled
dnl ****************************************************************

AC_MSG_CHECKING(Script arguments)
cat > testconf.args << EOS
#!$SED -e s/foo/bar/ -f

s/x/y/
EOS
chmod +x testconf.args

if ( echo foo | ./testconf.args ) >/dev/null 2>&1; then
    AC_DEFINE(SCRIPT_BREAKDOWN_ARGS, 1,
	      [breaks arguments ])
    AC_MSG_RESULT(breaks down arguments)
else
    AC_MSG_RESULT(does not breakdown arguments)
fi
rm -f testconf.args

dnl ****************************************************************
dnl See whether we can use git versions
dnl ****************************************************************

AC_MSG_CHECKING([Use git-based version stamps])
if "$srcdir/mkversion.sh" && \
   test -r version.h && \
   grep GIT_VERSION version.h > /dev/null; then
  AC_MSG_RESULT(yes)
  AC_DEFINE(USE_GIT_VERSION_H, 1,
	    [Define to use include actual git version])
else
  AC_MSG_RESULT(no)
fi

dnl ****************************************************************
dnl Setup shared library and dynamic linking
dnl ****************************************************************

PLSHARED=no
AC_CHECK_HEADERS(dlfcn.h)
AC_CHECK_LIB(dl, dlopen)
if test "$ac_cv_lib_dl_dlopen" = "yes"; then
  AC_CHECK_FUNCS(dlopen)
  AC_CHECK_FUNCS(dladdr)
else
  AC_CHECK_LIB(dld, shl_load)
  AC_CHECK_FUNCS(shl_load)
fi

dnl On SCO, dlopen is in libc.so and requires dlfcn.h to compile.  Thanks
dnl to Tamas Laufer.

if test ! "$ac_cv_func_dlopen" = "yes"; then
AC_MSG_CHECKING("dlopen in libc")
AC_TRY_COMPILE(
[
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif
#ifndef RTLD_NOW
#define RTLD_NOW 0
#endif
],
[ dlopen("foobar", RTLD_NOW);
], ac_cv_func_dlopen=yes
   AC_DEFINE(HAVE_DLOPEN, 1,
	     [Define if you have a working dlopen()])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
fi

if test "$ac_cv_func_dlopen" = yes -o "$ac_cv_func_shl_load" = yes; then
  PLSHARED=yes
fi

if test "$PLSHARED" = "yes"; then

dnl Some versions of (g)cc need to export the symbols from the main program
dnl using -rdynamic.  See whether this gcc understands this flag.

case "$PLARCH" in
  *darwin*|*-win32|*-win64)
	rdynamic=done
	;;
esac

if test "$rdynamic" != "done"; then
AC_MSG_CHECKING("whether LDFLAGS need -rdynamic")
  old_ldflags="$LDFLAGS"
  LDFLAGS="-rdynamic $LDFLAGS"
  AC_TRY_RUN(
main()
{ exit(0);
},
     if grep 'unrecognized option `-rdynamic'"'" config.log >/dev/null; then
       AC_MSG_RESULT(no)
       LDFLAGS="$old_ldflags"
     else
       AC_MSG_RESULT(yes)
     fi,
     AC_MSG_RESULT(no)
     LDFLAGS="$old_ldflags",
     AC_MSG_RESULT(no)
     LDFLAGS="$old_ldflags")
fi

dnl Some specific OS stuff

RPATH_EXTRA=
if test "x$RPATH_RESERVE" != "x"; then
  RPATH_EXTRA=":"
  n=$RPATH_RESERVE
  while test $n -gt 0; do
    RPATH_EXTRA="${RPATH_EXTRA}x"
    n=$(($n-1))
  done
fi

SO_LDFLAGS="-shared"
SO_pic="-fpic"
SO_PIC="-fPIC"
SO_LD="$CC"
SO_EXT="so"
SO_RPATH="-Wl,-rpath=$PLBASE/lib/$PLARCH$RPATH_EXTRA"
DLLIBVAR=LD_LIBRARY_PATH
MINGW="false"

case "$PLARCH" in
  *solaris* | *sunos*)
	SO_LDFLAGS="-G"
	if test "$GCC" != "yes"; then
	    SO_pic="-Kpic"
	    SO_PIC="-KPIC"
	fi
	;;
  *hpux*)
	SO_EXT="sl"
	LDFLAGS="$LDFLAGS -Wl,-E"
	if test "$GCC" != "yes"; then
	    SO_LDFLAGS="-Wl,-b"
	fi
	;;
  *osf*)
	SO_LD=ld
	SO_LDFLAGS="-shared -expect_unresolved \*"
	SO_pic="-std"
	if test "$MT" = yes; then
	  AC_CHECK_LIB(exc, main)
	fi
	;;
  *aix*)
	SO_EXT="o"
	DLLIBVAR=LIBPATH
	if test "$GCC" = "yes"; then
	    LDFLAGS="$LDFLAGS -Wl,-bE:../include/SWI-Exports"
	    SO_LDFLAGS="-Wl,-bI:$PLBASE/include/SWI-Exports"
	else
	    LDFLAGS="$LDFLAGS -bE:../include/SWI-Exports"
	    SO_LDFLAGS="-bI:$PLBASE/include/SWI-Exports"
	fi
	;;
  *x86_64*linux|*hppa*linux|*arm*linux|*aarch*linux|*power*linux|x86_64*openbsd*)
	if test "x$MKSHARED" = "x"; then
	    MKSHARED=yes
	fi
	;;
  *darwin*)
	if test "x$MKSHARED" = "x"; then
	    MKSHARED=yes
	fi
	SO_EXT=dylib
	SO_pic="-fno-common"
	SO_PIC="-fno-common"
	SO_LDFLAGS="-bundle -dynamic -flat_namespace -undefined suppress"
	SO_RPATH=""
	DLLIBVAR=DYLD_LIBRARY_PATH
	AC_DEFINE(USE_SEM_OPEN, 1,
		  [Define if we must use sem_open() instead of the preferred sem_init()])
	AC_DEFINE(LD_SYMBOL_PREFIX, "_",
		  [String used to prefix all symbols requested through dlsym()])
	;;
  *cygwin*)
	SO_EXT="dll"
	SO_pic=""
	SO_PIC=""
	DLLIBVAR=PATH
	;;
  *-win32|*-win64)
	SO_EXT="dll"
	SO_pic=""
	SO_PIC=""
	SO_PATH=PATH
	case $(uname) in
	    MINGW*)
		DLLIBVAR=PATH
		AC_CHECK_PROG(MSLIBEXE, lib.exe, lib.exe, ":")
		;;
	    *)
		DLLIBVAR=WINEPATH
		AC_CHECK_PROG(WINE, wine, wine, ":")
		AC_MSG_CHECKING(How to call MS LIB.EXE)
		if $WINE lib.exe 2>/dev/null | grep MACHINE > /dev/null; then
		  MSLIBEXE="$WINE lib.exe"
		else
		  MSLIBEXE=":"
		fi
		AC_MSG_RESULT($MSLIBEXE)
		;;
	esac
	if test "$MSLIBEXE" != ":"; then
	  AC_MSG_CHECKING(Machine id for lib.exe)
	  case "$PLARCH" in
	    *-win32)
	      MSLIBEXE="$MSLIBEXE /machine:X86"
	      ;;
	    *-win64)
	      MSLIBEXE="$MSLIBEXE /machine:X64"
	      ;;
	  esac
	  AC_MSG_RESULT($MSLIBEXE)
	fi
	AC_CHECK_PROG(WINEPATHEXE, winepath, winepath, ":")
	DEFSTARTUP=swipl.ini
	CHRPATH=:
        AC_MSG_CHECKING(whether linker supports --stack)
        oldflags="$LDFLAGS"
        LDFLAGS="$LDFLAGS -Wl,--stack,4000000"
        AC_TRY_LINK([], [],
                    [AC_MSG_RESULT(yes)
                     STACKFLAGS=-Wl,--stack,4000000],
                    [AC_MSG_RESULT(no)])
        LDFLAGS="$oldflags"
        AC_MSG_CHECKING(whether linker supports --subsystem)
        oldflags="$LDFLAGS"
        LDFLAGS="$LDFLAGS -Wl,--subsystem,windows"
        AC_TRY_LINK([], [],
                    [AC_MSG_RESULT(yes)
                     SUBSYSTEMFLAGS=-Wl,--subsystem,windows],
                    [AC_MSG_RESULT(no)])
        LDFLAGS="$oldflags"
	MINGW=true
	;;
  *)
	;;
esac

if test "x$SO_PATH"     = "x"; then SO_PATH="$DLLIBVAR"; fi
if test "x$WINEPATHEXE" = "x"; then WINEPATHEXE=":"; fi
if test "x$WINE"        = "x"; then WINE=":"; fi
if test "x$MSLIBEXE"    = "x"; then MSLIBEXE=":"; fi

AC_DEFINE_UNQUOTED(SO_EXT,     "$SO_EXT",
		   [Define to the extension of shared objects (often .so)])
AC_DEFINE_UNQUOTED(SO_PATH,    "$SO_PATH",
		   [Search path for shared objects (often LD_LIBRARY_PATH)])
AC_DEFINE_UNQUOTED(SO_LD,      "$SO_LD",
		   [Program to link shared objects])
AC_DEFINE_UNQUOTED(SO_LDFLAGS, "$SO_LDFLAGS",
		   [Flags to use for linking shared objects])
AC_DEFINE_UNQUOTED(SO_pic,     "$SO_pic",
		   [Flags for compiling position-independent small object])
AC_DEFINE_UNQUOTED(SO_PIC,     "$SO_PIC",
		   [Flags for compiling position-independent BIG object])
AC_DEFINE_UNQUOTED(PROG_PL,    "$PL$EXEEXT",
		   [Name of the SWI-Prolog executable (normally swipl[.exe])])
SONAMEV=`$as_echo "$PLVERSION" | sed 's/\(@<:@0-9@:>@@<:@0-9@:>@*\.@<:@0-9@:>@@<:@0-9@:>@*\)\.@<:@0-9@:>@@<:@0-9@:>@*/\1/'`
SONAME='$(SHARED).'$SONAMEV
case "$PLARCH" in
  *-win32|*-win64|*-cygwin*)
	PLSO=libswipl.$SO_EXT
	SONAMEOPT='-Wl,-soname=$(SONAME) -Wl,--out-implib,$(RUNTIMEDIR)/libswipl.$(SOEXT).a -Wl,--output-def,$(RUNTIMEDIR)/libswipl.def'
	;;
  *)
	PLSO=libswipl.$SO_EXT
	SONAMEOPT='-Wl,-soname=$(SONAME)'
	;;
esac

fi	dnl PLSHARED


dnl ****************************************************************
dnl Finding libraries
dnl ****************************************************************

changequote(,)dnl
USEPLLIB=`echo $PLLIB | sed 's/lib\([a-z]*\).a/-l\1/'`
changequote([,])dnl

case "$PLARCH" in
    *-win32|*-win64)
	AC_CHECK_LIB(shell32, main)
	AC_CHECK_LIB(winmm, main)
	AC_CHECK_LIB(wsock32, main)
	AC_CHECK_LIB(gdi32, main, [XLIBS="$XLIBS -lgdi32"])
	AC_CHECK_LIB(comdlg32, main, [XLIBS="$XLIBS -lcomdlg32"])
	EXTRAOBJ="pl-nt.o pl-ntconsole.o pl-dde.o"
	;;
esac


PLTHREADS=no
if test "$MT" = "yes"; then
pthread_ok=false

case "$PLARCH" in
    *freebsd*)
	AC_DEFINE(_THREAD_SAFE, 1,
		  [Required in FreeBSD for compiling thread-safe code])
	;;
    *-win32|*-win64)
        pthread_ok=true
	AC_CHECK_LIB(pthreadGC2, pthread_create)
        if test "$ac_cv_lib_pthreadGC2_pthread_create" = "yes"; then
	  MINGWPTHREAD=pthreadGC2
	else
          AC_CHECK_LIB(pthreadGC, pthread_create)
          if test "$ac_cv_lib_pthreadGC_pthread_create" = "yes"; then
	    MINGWPTHREAD=pthreadGC
	  else
            AC_CHECK_LIB(pthread, pthread_create)
	    dnl Find real name of this MinGW thread library
	    MINGWPTHREAD=libwinpthread-1
          fi
        fi
	;;
esac

  if test "$GCC" = "yes"; then
  for PTHREAD in -pthread -pthreads; do
    if test "$pthread_ok" = "false"; then
      AC_MSG_CHECKING("whether gcc supports $PTHREAD")
      ocflags="$CFLAGS"
      CFLAGS="$CFLAGS $PTHREAD"
      AC_TRY_LINK([
#include <pthread.h>
		  ],
		  [ static pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;
		    pthread_mutex_lock(&m);
		    pthread_mutex_unlock(&m);
#if !(defined(_REENTRANT) || defined(_THREAD_SAFE) || defined(_POSIX_THREADS))
	this should not be compiled
#endif
		  ],
		  pthread_ok=true
		  CMFLAGS="$CMFLAGS $PTHREAD"
		  LDFLAGS="$LDFLAGS $PTHREAD"
		  AC_MSG_RESULT(yes),
		  CFLAGS="$ocflags"
		  AC_MSG_RESULT(no))
      fi
    done
  fi

  if test "$pthread_ok" = "false"; then
  AC_CHECK_LIB(pthread, main)
    if test "$ac_cv_lib_pthread_main" = "yes"; then
      pthread_ok=true
      CMFLAGS="$CMFLAGS -D_REENTRANT"
    fi
  fi

  if test "$pthread_ok" = "true"; then
    AC_DEFINE(O_PLMT, 1,
	      [Define to include support for multi-threading])
    PLTHREADS=yes
  fi

  AC_CHECK_LIB(rt, sem_init)
  AC_CHECK_FUNCS(sem_init)
  if test ! "$ac_cv_lib_rt_sem_init" = "yes"; then
	AC_CHECK_LIB(thread,sema_init)
	AC_CHECK_FUNCS(sema_init)
  fi
  AC_CHECK_FUNCS(pthread_mutexattr_setkind_np pthread_mutexattr_settype)
  AC_CHECK_FUNCS(pthread_kill pthread_mutex_timedlock pthread_timedjoin_np)
  AC_CHECK_FUNCS(pthread_getname_np pthread_setname_np)
  AC_MSG_CHECKING(for pthread_setname_np(const char*))
  AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
	   [#define _GNU_SOURCE 1
	    #include <pthread.h>
           ],
	   [pthread_setname_np("example")])],
      [AC_MSG_RESULT(yes)
       AC_DEFINE(HAVE_PTHREAD_SETNAME_NP_WITHOUT_TID,1,
		 [Have function pthread_setname_np(const char*)])],
      [AC_MSG_RESULT(no)])

  AC_MSG_CHECKING([whether mutex support recursive locking])
  case "$PLARCH" in
    *-win32|*-win64)
      AC_MSG_RESULT(yes)
      AC_DEFINE(RECURSIVE_MUTEXES, 1,
        [Define to make use of standard (UNIX98) pthread recursive mutexes])
      ;;
    *)
      echo '#include "confdefs.h"' > conftest.c
      cat $srcdir/test/recmutex.c >> conftest.c
      (eval $ac_link) 2>&5
      if test -s conftest && ./conftest; then
        AC_MSG_RESULT(yes)
        AC_DEFINE(RECURSIVE_MUTEXES, 1,
          [Define to make use of standard (UNIX98) pthread recursive mutexes])
      else
        AC_MSG_RESULT(no)
      fi
      ;;
  esac
  AC_CHECK_FUNCS(pthread_sigmask pthread_setconcurrency)
  AC_CHECK_FUNCS(pthread_getw32threadhandle_np)

AC_CHECK_FUNCS(sysconf)

AC_MSG_CHECKING("copy stack size")
AC_TRY_RUN(
[
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <pthread.h>

#define ROUND(p, n)		((((p) + (n) - 1) & ~((n) - 1)))

static void *
dummy(void *closure)
{ return closure;
}

static size_t
round_pages(size_t n)
{ size_t psize;

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
  if ( (psize = sysconf(_SC_PAGESIZE)) == (size_t)-1 )
    psize = 8192;
#else
  psize = 8192;
#endif

  return ROUND(n, psize);
}

int
main()
{ pthread_attr_t attr;
  struct rlimit rlim;
  size_t stack;
  pthread_t tid;
  void *r;

  pthread_attr_init(&attr);
  if ( getrlimit(RLIMIT_STACK, &rlim) != 0 )
    exit(1);
  if ( rlim.rlim_cur != RLIM_INFINITY )
    stack = rlim.rlim_cur;
  stack = round_pages(stack);
  if ( pthread_attr_setstacksize(&attr, stack) != 0 )
    exit(1);
  if ( pthread_create(&tid, &attr, dummy, NULL) != 0 )
    exit(1);
  if ( pthread_join(tid, &r) != 0 )
    exit(1);

  return 0;
}
],
AC_MSG_RESULT(yes)
AC_DEFINE(USE_COPY_STACK_SIZE, 1,
	  [Define if to copy thread stack size from main]),
AC_MSG_RESULT(no),
AC_MSG_RESULT(assuming no))


fi	dnl End of MT configuration


AC_CHECK_LIB(m, main)
AC_CHECK_LIB(ncursesw, main)
if test ! "$ac_cv_lib_ncursesw_main" = "yes"; then
  AC_CHECK_LIB(ncurses, main)
  if test ! "$ac_cv_lib_ncurses_main" = "yes"; then
    AC_CHECK_LIB(curses, main)
    if test ! "$ac_cv_lib_curses_main" = "yes"; then
      AC_CHECK_LIB(termcap, main)
      if test "$ac_cv_lib_termcap_main" = "yes"; then
	AC_MSG_CHECKING("whether -ltermcap needs -lucb")
	AC_TRY_LINK([],
		    [ tgetent();
		    ],
		    AC_MSG_RESULT(no)
		    true,
		    AC_MSG_RESULT(maybe ...)
		    checkucb=yes)
      fi
    fi
  fi
fi

AC_CHECK_HEADERS(term.h ncurses/term.h curses.h ncurses/curses.h)

if test "$checkucb" = "yes"; then
    AC_CHECK_LIB(ucb, rindex)
fi


dnl ================================================================

AC_MSG_CHECKING(for cc -c -o)
  cat > conftest.c << _EOP_
main()
{ exit(0);
}
_EOP_
  if ${CC-cc} $CFLAGS -c conftest.c -o conftest.o 1>&5 2>&5 && test -r conftest.o; then
    COUTOPT='-o $@'
    AC_MSG_RESULT(yes)
  else
    COUTOPT=
    AC_MSG_RESULT(no)
  fi
  rm -rf conftest.*

AC_CHECK_HEADERS(unistd.h string.h memory.h sys/time.h sys/stat.h pwd.h)
AC_CHECK_HEADERS(sys/select.h sys/param.h sys/file.h sys/resource.h)
AC_CHECK_HEADERS(malloc.h sys/stropts.h wchar.h execinfo.h)
AC_CHECK_HEADERS(sys/termios.h sys/termio.h bstring.h sys/mman.h)
AC_CHECK_HEADERS(mach-o/rld.h mach/thread_act.h locale.h)
AC_CHECK_HEADERS(float.h floatingpoint.h ieeefp.h ieee754.h SupportDefs.h)
AC_CHECK_HEADERS(crtdbg.h shlobj.h winsock2.h poll.h)
AC_CHECK_HEADERS(valgrind/valgrind.h)

AC_CHECK_FUNCS(access fchmod chmod dossleep fstat readlink getwd getcwd)
AC_CHECK_FUNCS(getpagesize getpwnam getrlimit gettimeofday opendir sysconf)
AC_CHECK_FUNCS(popen putenv random remove rename select fcntl getrusage)
AC_CHECK_FUNCS(signal sigset sigprocmask sleep usleep srand srandom stat)
AC_CHECK_FUNCS(memmove stricmp strlwr sigaction sigsetmask siggetmask sigblock)
AC_CHECK_FUNCS(rint aint floor ceil isnan isinf tcsetattr cfmakeraw getpid)
AC_CHECK_FUNCS(strerror tgetent times unsetenv grantpt nanosleep waitpid)
AC_CHECK_FUNCS(setlocale fpgetmask fpresetsticky mtrace localtime_r asctime_r)
AC_CHECK_FUNCS(ctime_r mmap confstr strcasecmp mbscoll mbscasecoll fpclass)
AC_CHECK_FUNCS(setenv posix_openpt wcsxfrm sysctlbyname mbsnrtowcs wcsdup)
AC_CHECK_FUNCS(ftruncate localtime_s localeconv poll WSAPoll qsort_r qsort_s)
AC_CHECK_DECLS([mbsnrtowcs])
AC_HEADER_TIME
AC_HEADER_DIRENT
AC_HEADER_SYS_WAIT
AC_FUNC_VFORK

dnl AC_CHECK_FUNCS() doesn't work due to type errors.  Seems there is some
dnl discussion for a better solution, but for now this should do.

AC_MSG_CHECKING(for __builtin_clz)
AC_TRY_LINK([],
	    [ __builtin_clz(0); ],
	    [ AC_MSG_RESULT(yes)
	      AC_DEFINE(HAVE__BUILTIN_CLZ, 1,
			[Define if you have __builtin_clz])
	    ],
	    [ AC_MSG_RESULT(no) ])
AC_MSG_CHECKING(for __builtin_popcount)
AC_TRY_LINK([],
	    [ __builtin_popcount(0); ],
	    [ AC_MSG_RESULT(yes)
	      AC_DEFINE(HAVE__BUILTIN_POPCOUNT, 1,
			[Define if you have __builtin_popcount])
	    ],
	    [ AC_MSG_RESULT(no) ])
AC_MSG_CHECKING(for __sync_synchronize)
AC_TRY_LINK([],
	    [ __sync_synchronize(); ],
	    [ AC_MSG_RESULT(yes)
	      AC_DEFINE(HAVE__SYNC_SYNCHRONIZE, 1,
			[Define if you have __sync_synchronize])
	    ],
	    [ AC_MSG_RESULT(no) ])
AC_MSG_CHECKING(for signbit)
AC_TRY_LINK([#include <math.h>],
	    [ signbit(0.0); ],
	    [ AC_MSG_RESULT(yes)
	      AC_DEFINE(HAVE_SIGNBIT, 1,
			[Define if you have signbit])
	    ],
	    [ AC_MSG_RESULT(no) ])


if test "x$ac_cv_func_qsort_r" = xno -a "x$ac_cv_func_qsort_s" = xno; then
  AC_CHECKING("Use nested functions to emulate qsort_r()")
  nested_rsort=no
  if test "$GCC" = "yes"; then
    ac_oldcflags="$CFLAGS"
    CFLAGS="$CFLAGS -Werror"

dnl NOTE: We put the function in the include section as we have to define
dnl NOTE: a function rather than provide a body.

AC_TRY_COMPILE(
[
#include <stdlib.h>

void sort_r(void *base, size_t nel, size_t width,
            int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)
{ int nested_cmp(const void *a, const void *b)
  {
    return compar(a, b, arg);
  }

  qsort(base, nel, width, nested_cmp);
}
],
[ ;
],
AC_DEFINE(QSORT_R_WITH_NESTED_FUNCTIONS, 1,
	  [Emulate qsort_r() with nested functions])
nested_rsort=yes,
true)
    CFLAGS="$ac_oldcflags"
  fi
  AC_MSG_RESULT($nested_rsort)
fi

if test "x$ac_cv_func_qsort_r" = xyes; then
  AC_CHECKING("GNU style qsort_r()")
  qsort_r_gnu=no

  AC_TRY_RUN(
[
#define _GNU_SOURCE
#include <stdlib.h>

static int
mycompare(const void *p1, const void *p2, void *c)
{ const int *s1 = p1;
  const int *s2 = p2;
  int *ip = c;

  if ( *ip != 1 )
    exit(1);

  return *s1 - *s2;
}

int
main(int argc, char**argv)
{ int data[] = {0,1,2,3,4,5,6,7,8,9};
  int ctx = 1;

  qsort_r(data, 10, sizeof(int), mycompare, (void*)&ctx);
  return 0;
}
],
AC_DEFINE(QSORT_R_GNU, 1,
	  [If qsort_r() is has closure before context])
qsort_r_gnu=yes,
true)
AC_MSG_RESULT($qsort_r_gnu)
fi

case "$PLARCH" in
    *-win32|*-win64)
      if test "x$ac_cv_func_select" = xno; then
        # Under MinGW, select() is not in the default libraries.
        for select_lib in ws2_32; do
          AC_MSG_CHECKING([for select in $select_lib])
          ac_save_LIBS="$LIBS"
          LIBS="-l$select_lib $LIBS"
          AC_LINK_IFELSE(
            [AC_LANG_PROGRAM([[#ifdef HAVE_WINSOCK2_H
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <winsock2.h>
#endif]],
                             [[select(0,
                                      (fd_set *) NULL, (fd_set *) NULL,
                                      (fd_set *) NULL,
                                      (struct timeval *) NULL);]])
            ],
            [
              AC_MSG_RESULT([yes])
              AC_DEFINE([HAVE_SELECT])
              break
            ],
            [
              AC_MSG_RESULT([no])
              LIBS="$ac_save_LIBS"
            ]
          )
        done
      fi
      AC_CHECK_LIB(dbghelp, StackWalk64,
		   check_dbghelp_h=yes)
      if test "$check_dbghelp_h" = yes; then
	AC_CHECK_HEADERS(dbghelp.h,
			 LIBS="-ldbghelp $LIBS",
			 [],
[#include <windows.h>
])
      fi
      AC_CHECK_LIB(imagehlp, ImageLoad,
		   check_imagehlp_h=yes)
      if test "$check_imagehlp_h" = yes; then
	AC_CHECK_HEADERS(imagehlp.h,
			 LIBS="-limagehlp $LIBS",
			 [],
[#include <windows.h>
])
      fi
      AC_CHECK_LIB(dwarf, main, check_dwarf_h=yes)
      if test "x$check_dwarf_h" = "xyes"; then
        AC_CHECK_HEADER(dwarf.h,
		        AC_DEFINE(HAVE_LIBDWARF, 1,
			          [ Define to 1 if you have libdwarf. ])
		          LIBS="-ldwarf $LIBS"
		          COFLAGS+=" -gdwarf-2")
      fi
      ;;
esac

AC_CHECK_TYPE(intptr_t)

if test "x$ac_cv_func_unsetenv" = "xyes"; then
  AC_MSG_CHECKING("whether unsetenv is void")
  AC_TRY_COMPILE(
[
#include <stdlib.h>
],
		 [ int i = unsetenv("test");
		 ],
		 AC_MSG_RESULT(no),
		 AC_DEFINE(VOID_UNSETENV, 1,
			   [Define if unsetenv() is void])
		 AC_MSG_RESULT(yes))
fi

if test "$MT" = "yes"; then
if test "$MINGW" = false; then

dnl The tests below are Unix only.  There is no point doing this for MinGW.
dnl In particular, MinGW 4.8 claims clock_gettime() to be present, but fetching
dnl CLOCK_REALTIME on Win32 returns bogus.

AC_CHECK_HEADERS(sys/syscall.h)
AC_CHECK_FUNCS(syscall)

AC_MSG_CHECKING(whether <sys/syscall.h> has __NR_gettid)
AC_TRY_COMPILE(
[ #include <sys/syscall.h> ],
[ int i = __NR_gettid;
], AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_MSG_CHECKING(for syscall support for gettid)
AC_TRY_COMPILE(
[ #include <unistd.h>
  #include <sys/syscall.h> ],
[ syscall(__NR_gettid);
], ac_syscall_gettid="yes"
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

if test "x$ac_syscall_gettid" = "xyes"; then
   AC_DEFINE(HAVE_GETTID_SYSCALL, 1,
             [Define to 1 if you have syscall support for gettid()])
else dnl ! test "x$ac_syscall_gettid" = "xyes"

AC_MSG_CHECKING("linux gettid _syscall0 macro")
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <linux/unistd.h>
_syscall0(pid_t,gettid)
],
[ pid_t t = gettid();
], AC_DEFINE(HAVE_GETTID_MACRO, 1,
	     [Define to 1 if you have the Linux gettid() _syscall0 macro])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
fi      dnl ! test "x$ac_syscall_gettid" = "xyes"

AC_CHECK_LIB(rt, clock_gettime)
AC_CHECK_FUNCS(clock_gettime)

AC_MSG_CHECKING(for pthread support for cpu clocks)
AC_TRY_RUN(
[
#include <stdio.h>
#include <pthread.h>
#include <time.h>
#include <stdlib.h>
#define ts2d(ts) \
	((double)(ts).tv_sec + (double)(ts).tv_nsec/(double)1000000000.0)
main()
{
  clockid_t clock_id;
  struct timespec ts = {1,1};
  if ( pthread_getcpuclockid(pthread_self(), &clock_id) != 0 )
  { perror("pthread_getcpuclockid");
    exit(1);
  }
  sleep(1);
  if ( clock_gettime(clock_id, &ts) != 0 )
  { perror("clock_gettime");
    exit(1);
  }
  fprintf(stderr, "Used %f sec\n", ts2d(ts));
  exit (ts.tv_sec == 0 ? 0 : 1);
}], ac_pthread_cpuclocks="yes"
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(no))

if test "x$ac_pthread_cpuclocks" = "xyes"; then
   AC_DEFINE(PTHREAD_CPUCLOCKS, 1,
             [Define if you have pthread cpu clocks (glibc 2.4 and greater)])
else

AC_MSG_CHECKING(whether <sys/syscall.h> has __NR_clock_gettime)
AC_TRY_COMPILE(
[ #include <sys/syscall.h> ],
[ int i = __NR_clock_gettime;
], AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_MSG_CHECKING(for syscall support for cpu clocks)
AC_TRY_RUN(
[
#include <stdio.h>
#include <time.h>
#include <sys/syscall.h>
#include <stdlib.h>
#define ts2d(ts) \
	((double)(ts).tv_sec + (double)(ts).tv_nsec/(double)1000000000.0)
main()
{ struct timespec ts0;
  struct timespec ts1;
  double t;
  if ( syscall(__NR_clock_gettime, CLOCK_THREAD_CPUTIME_ID, &ts0) )
    exit(1);
  sleep(1);
  syscall(__NR_clock_gettime, CLOCK_THREAD_CPUTIME_ID, &ts1);
  t = ts2d(ts1) - ts2d(ts0);
  fprintf(stderr, "sleep(1) used %f sec\n", t);
  if ( t > 0.5 )
  { exit(1);
  }
  exit(0);
}], ac_linux_cpuclocks="yes"
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(no))

if test "x$ac_linux_cpuclocks" = "xyes"; then
   AC_DEFINE(LINUX_CPUCLOCKS, 1,
             [Define if you have Linux cpu clocks (2.6.12 and greater)])
else

AC_MSG_CHECKING(linux 2.6 /proc)
if { test -f /proc/$$/task/$$/stat && \
     test `cat /proc/$$/task/$$/stat | wc -w` -gt 41; }; then
   AC_MSG_RESULT(yes)
   AC_DEFINE(LINUX_PROCFS, 26,
	     [Define if you have Linux 2.6 compatible /proc])
else
   AC_MSG_RESULT(no)
fi
fi dnl test "x$ac_linux_cpuclocks" = "xyes"
fi dnl test "$MINGW" = false
fi dnl if test "$MT" = "yes"

AC_MSG_CHECKING(for sysconf support for _SC_NPROCESSORS_CONF)
AC_TRY_COMPILE(
[ #include <unistd.h> ],
[ long n = sysconf(_SC_NPROCESSORS_CONF);
], ac_sc_nprocessors_conf="yes"
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

if test "x$ac_sc_nprocessors_conf" = "xyes"; then
   AC_DEFINE(HAVE_SC_NPROCESSORS_CONF, 1,
             [Define if you have sysconf support for _SC_NPROCESSORS_CONF])
else

AC_MSG_CHECKING(linux /proc/cpuinfo)
if { test -f /proc/cpuinfo && \
     grep 'processor[	 ]*: 0' /proc/cpuinfo 2>&1 > /dev/null; }; then
   AC_MSG_RESULT(yes)
   AC_DEFINE(PROCFS_CPUINFO, 1,
	     [Define if you have Linux compatible /proc/cpuinfo])
else
   AC_MSG_RESULT(no)
fi dnl /proc
fi dnl sc_nprocessors_conf
fi dnl "$MINGW" = false

dnl Used in OpenBSD
AC_CHECK_LIB(execinfo, backtrace_symbols)

if test "$ac_cv_header_execinfo_h" = "yes"; then
AC_MSG_CHECKING("whether backtrace() works")
AC_TRY_RUN(
[
#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

#define BT_MAX_DEPTH 10

int
print_backtrace(void)
{ void *bt[BT_MAX_DEPTH];
  char **strings;
  int i, d;

  d = backtrace(bt, BT_MAX_DEPTH);
  if ( d == -1 )
    return 1;

  strings = backtrace_symbols(bt, d);

  if (strings == NULL)
    return 1;

  free(strings);

  return 0;
}

int
fac(int n)
{ if ( n <= 1 )
  { if ( print_backtrace() )
      exit(1);
    return n;
  }
  int m = fac(n-1)*n;
  return m;
}

int
main(int argc, char**argv)
{ fac(10);

  return 0;
}
],
AC_MSG_RESULT(yes)
AC_DEFINE(HAVE_BACKTRACE, 1,
	  [Define if backtrace is provided and works]),
AC_MSG_RESULT(no),
AC_MSG_RESULT(assuming no))
fi

AC_CHECK_FUNCS(dladdr)
if test "$ac_cv_func_dladdr" != "yes"; then
  AC_CHECK_LIB(unwind, main,
	       check_unwind_h=yes)
  if test "$check_unwind_h" = yes; then
    AC_CHECK_HEADER(libunwind.h,
		    AC_DEFINE(HAVE_LIBUNWIND, 1,
			      [ Define if you have libunwind and libunwind.h ])
		    LIBS="-lunwind $LIBS")
  fi
fi

AC_CHECK_SIZEOF(void*, 4)
AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF(int,  4)
AC_CHECK_SIZEOF(long long, 8)
AC_CHECK_SIZEOF(wchar_t, 4)

if test "$configgmp" = "yes"; then
  AC_CHECK_LIB(gmp, main,
	       check_gmp_h=yes,
	       WARN_NO_GMP=yes)
  if test "$check_gmp_h" = "yes"; then
    AC_CHECK_HEADER(gmp.h,
		    AC_DEFINE(HAVE_GMP_H, 1,
			      [ Define to 1 if you have the <gmp.h> header file. ])
		    LIBS="-lgmp $LIBS",
		    WARN_NO_GMP=yes)
  fi

  AC_MSG_CHECKING("For gmp_randinit_mt()");
  AC_TRY_LINK(
      [#include <gmp.h>
      ],
      [ gmp_randstate_t state;
	gmp_randinit_mt(&state);
      ],
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_GMP_RANDINIT_MT, 1,
		[ Define you you have gmp_randinit_mt (gmp > 4.2.0) ]),
      AC_MSG_RESULT(no))

  AC_CHECK_TYPES([mp_bitcnt_t], [], [], [[#include <gmp.h>]])
fi

dnl Setup DMALLOC

if test "$dmalloc" = "yes"; then
  AC_CHECK_LIB(dmallocth, main,
	       AC_DEFINE(DMALLOC, 1,
			 [Define if you want to use dmalloc])
	       DBGLIBS="$DBGLIBS -ldmallocth",
	       true)
fi

dnl tszet and timezone trouble

AC_MSG_CHECKING("variable timezone in tzset")
AC_TRY_RUN(
[ #include <time.h>
  #include <stdlib.h>
  #if defined(__CYGWIN__) || defined(__MINGW32__)
  #define timezone _timezone
  #else
  extern long timezone;
  #endif

  main()
  { tzset();

    if ( timezone > -15*3600 &&
	 timezone < 15*3600 &&
	 timezone % 60 == 0 )
      return 0;

    exit(1);
  }
],
AC_MSG_RESULT(yes)
AC_DEFINE(HAVE_VAR_TIMEZONE, 1,
	  [Define if tzset sets timezone variable]),
AC_MSG_RESULT(no),
AC_MSG_RESULT(assuming no))

AC_CHECK_MEMBER(struct tm.tm_gmtoff,
		AC_DEFINE(HAVE_STRUCT_TIME_TM_GMTOFF, [],
			  [Define is struct tm has tm_gmtoff]),
		[],
		[#include <time.h>])

AC_MSG_CHECKING("Boot filename");
BOOTFILE=boot`expr ${ac_cv_sizeof_voidp} \* 8`.prc
AC_DEFINE_UNQUOTED(BOOTFILE, "$BOOTFILE",
		   [Name of the file to boot from])
AC_MSG_RESULT($BOOTFILE)

if test "$ac_cv_func_fcntl" = yes; then
  AC_MSG_CHECKING("fcntl based locking");
  AC_TRY_COMPILE(
[
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
],
[ struct flock buf;
  fcntl(0, F_SETLKW, &buf);
], AC_DEFINE(FCNTL_LOCKS, 1,
	     [we have fcntl() and it supports F_SETLKW])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
fi


AC_CHECK_ALIGNOF(void*)
AC_CHECK_ALIGNOF(double)
AC_CHECK_ALIGNOF(int64_t)


dnl See whether we can access intptr_t values over non-aligned pointers.  This
dnl should also check whether it is actually faster to do so.  Note that on
dnl the ARM processor it works, but the pointer is truncated, overwriting
dnl what is in front of it.  The current check validates this.
dnl
dnl Seems we can better play safe and set this to yes on for the Intel
dnl processors and no otherwise.  On some systems it works, but is sloooow,
dnl on others it works kind-of, but not relyable, etc.  I've left the
dnl old test for reference.

AC_MSG_CHECKING("unaligned access")
AC_TRY_RUN(
[
#include <inttypes.h>
  main()
  { char l[sizeof(intptr_t)*2];
    intptr_t *p;

#if defined(__alpha__) || defined(__alpha) || defined(__ia64__) || defined(__sparc__)
    exit(1);
#endif

    l[0] = 10;
    p = (intptr_t *)(l+1);
    *p = 0x20406080;

    if ( l[0] == 10 && *p == 0x20406080 )
    {
#if defined(__i386__)
      exit(0);
#endif
    }

    exit(1);
  }
],
AC_MSG_RESULT(allowed)
AC_DEFINE(NON_ALIGNED_ACCESS, 1,
	  [Define if it is allowed to access intptr_t integers with non-aligned pointers]),
AC_MSG_RESULT(not allowed),
AC_MSG_RESULT(assuming not allowed))

AC_MSG_CHECKING("__sync_add_and_fetch_8")
AC_LINK_IFELSE([AC_LANG_PROGRAM(
     [[#include <inttypes.h>]],
     [ int64_t i = 1;
       __sync_add_and_fetch(&i, 1);
     ])],
     [ AC_DEFINE(HAVE___SYNC_ADD_AND_FETCH_8, 1,
		 [Define if __sync_add_and_fetch is supported for 8 byte objects])
       AC_MSG_RESULT(yes)
     ],
     [ AC_MSG_RESULT(no)
     ])

if test "$ac_cv_func_getrlimit" = "yes"; then
AC_MSG_CHECKING("type rlim_t")
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
],
[ rlim_t datasize;
], AC_DEFINE(HAVE_RLIM_T, 1,
	     [Define if the type rlim_t is defined by <sys/resource.h>])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
fi

if test "$ac_cv_func_getrusage" = "yes"; then
AC_MSG_CHECKING("ru_idrss in struct rusage")
AC_TRY_COMPILE(
[
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
],
[ static struct rusage usage;
  unsigned long rss;

  rss = usage.ru_idrss;
], AC_DEFINE(HAVE_RU_IDRSS, 1,
	     [Define if struct rusage contains the field ru_idrss])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
fi

AC_MSG_CHECKING("whether assert.h needs stdio.h")
AC_TRY_COMPILE(
[
#include <assert.h>
],
[ assert(1);
], AC_MSG_RESULT(no),
   AC_MSG_RESULT(yes);
   AC_DEFINE(ASSERT_H_REQUIRES_STDIO_H, 1,
	     [Define if <assert.h> requires <stdio.h>]))

AC_CHECKING("for including sys/ioctl.h after sys/termios.h")
if test "$GCC" = "yes"; then
    ac_oldcflags="$CFLAGS"
    CFLAGS="$CFLAGS -Werror"
fi
AC_TRY_COMPILE(
[
#include <stdio.h>
#include <sys/types.h>
#include <sys/termios.h>
#include <sys/ioctl.h>
],
[;
], true, AC_DEFINE(NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H, 1,
		   [Define if <sys/ioctl> should *not* be included after <sys/termios.h>]))
if test "$GCC" = "yes"; then CFLAGS="$ac_oldcflags"; fi

if test -z "$ac_cv_header_sys_termio_h" -a -z "$ac_cv_header_sys_termios_h"; then
AC_MSG_CHECKING("struct sgttyb")
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <sys/ioctl.h>
],
[ struct sgttyb buf;
  ;
], AC_DEFINE(HAVE_SGTTYB, 1,
	     [Define if you don't have termio(s), but struct sgttyb])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
fi


AC_MSG_CHECKING("for type uchar")
AC_TRY_COMPILE(
[
#include <sys/types.h>
typedef unsigned char uchar;
],
[;], AC_DEFINE(NEED_UCHAR, 1, Define if uchar is not defined in <sys/types.h>)
     AC_MSG_RESULT("need typedef uchar"),
     AC_MSG_RESULT("uchar predefined"))

AC_MSG_CHECKING("whether we need extern int errno")
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
],
[ return errno;
], AC_MSG_RESULT(no),
   AC_DEFINE(NEED_DECL_ERRNO, 1,
	     [Define if, in addition to <errno.h>, extern int errno; is needed])
   AC_MSG_RESULT(yes))

AC_MSG_CHECKING("for symbolic links")
if test "$MINGW" = false && touch conftest.x && ln -s conftest.x conftest.lns && cmp conftest.x conftest.lns; then
    AC_DEFINE(HAVE_SYMLINKS, 1,
	      [Define if the ln command supports -s])
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)
fi

AC_TYPE_SIGNAL

dnl Do not enable profiling unless MT is also enabled
if test "$MT" = "yes"; then

AC_MSG_CHECKING("for BSD signal handlers")
AC_TRY_RUN(
[
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
static int signalled;
static RETSIGTYPE catch (s) int s; { signalled = 1; }
main()
{ signal(SIGINT, catch);
  kill(getpid(), SIGINT);
  while(!signalled)
    sleep(1);
  if ( signal(SIGINT, catch) == catch )
    exit(0);
  exit(1);
}], AC_DEFINE(BSD_SIGNALS, 1,
	      [Define if BSD compatible signals (i.e. no reset when fired)])
    AC_MSG_RESULT("BSD signals"),
    AC_MSG_RESULT("Posix signals"),
    AC_MSG_RESULT("assuming Posix signals"))

AC_MSG_CHECKING("for SIGPROF profiler support")
AC_TRY_RUN([
#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif
#include <signal.h>
int n; /*global to avoid optimizing too much */
int done;
typedef RETSIGTYPE (*handler_t)(int);

static RETSIGTYPE
prof(int s)
{ done++;

#if !defined(BSD_SIGNALS) && !defined(HAVE_SIGACTION)
  signal(SIGPROF, prof);
#endif
}

handler_t
set_sighandler(int sig, handler_t func)
{
#ifdef HAVE_SIGACTION
  struct sigaction old;
  struct sigaction new;

  memset(&new, 0, sizeof(new));	/* deal with other fields */
  new.sa_handler = func;

  if ( sigaction(sig, &new, &old) == 0 )
    return old.sa_handler;
  else
    return SIG_DFL;
#else
  return signal(sig, func);
#endif
}

main()
{ struct itimerval value, ovalue;
  time_t t0 = time(NULL);
  int i;
  value.it_interval.tv_sec  = 0;
  value.it_interval.tv_usec = 1000;
  value.it_value.tv_sec  = 0;
  value.it_value.tv_usec = 1000;

  set_sighandler(SIGPROF, prof);

  if (setitimer(ITIMER_PROF, &value, &ovalue) != 0)
    exit(1);

  for(;;)
  { for(n=0; n<50000000; n++)
      ;
    if ( time(NULL) > t0+1 )
      break;
  }
  if ( done )
  { printf("(%d interrupts) ", done);
    exit(0);
  } else
    exit(1);
}], AC_DEFINE(O_SIGPROF_PROFILE, 1, Define if SIGPROF and setitimer() are available)
    AC_MSG_RESULT(yes),
    AC_MSG_RESULT(no),
    true)

fi	dnl End of conditional disabling profiling unless MT is enabled

AC_MSG_CHECKING("for union wait")
AC_TRY_COMPILE(
[
#include <sys/wait.h>
],
[ union wait status;
  waitpid(1, &status, WNOHANG);
], AC_MSG_RESULT(yes)
   AC_DEFINE(UNION_WAIT, 1,
	     [Define if wait() uses union wait]),
   AC_MSG_RESULT(no))

AC_MSG_CHECKING("for tagged lvalues")
AC_TRY_COMPILE(
[
],
[ int x;
  *(char *)&x = 'a';
  exit(0);
], AC_DEFINE(TAGGED_LVALUE, 1, [Define if (type)var = value is allowed])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_MSG_CHECKING("for label addresses")
AC_TRY_COMPILE(
[
],
[ int x=0;
  void *addr;

#ifdef __llvm__
  ERROR: Label addresses compile on LLVM, but they are *very* slow.
#endif

  addr = &&label2;
  goto *addr;

  label1:
	x = 1;
	goto label3;
  label2:
	x = 2;

  label3:
	if ( x == 2 )
	  exit(0);

        exit(1);
], AC_DEFINE(O_LABEL_ADDRESSES, 1,
	     [Define to 1 if &&label and goto *var is supported (GCC-2)])
   AC_MSG_RESULT(yes),
   AC_DEFINE(O_LABEL_ADDRESSES, 0)
   AC_MSG_RESULT(no))


dnl ****************************************************************
dnl Very specific SWI-Prolog tests.
dnl ****************************************************************

dnl ****************************************************************
dnl Test whether we can use asm("nop") to separate instructions.
dnl Needed for gcc version 2 (see pl-wam.c)
dnl ****************************************************************

AC_MSG_CHECKING("for nop");
echo '#include "confdefs.h"' > conftest.c
cat $srcdir/test/nop.c >> conftest.c
(eval $ac_link) 2>&5
if test -s conftest && eval `./conftest`; then
    AC_MSG_RESULT(yes)
else
    AC_DEFINE(NO_ASM_NOP, 1,
	      "Define if you can't use asm("nop") to separate two labels")
    AC_MSG_RESULT(no)
fi
rm -fr conftest*


if test "$ac_cv_func_mmap" = "yes"; then
  AC_MSG_CHECKING("whether mmap maps regular files")
  echo "dummy file" >> conftest.dummy
  AC_TRY_RUN(
  [
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifndef MAP_FAILED
#define MAP_FAILED ((void *)-1)
#endif
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif

    main()
    { int fd = open("conftest.dummy", O_RDONLY);
      void *ptr;

      ptr = mmap(NULL, 100,
		 PROT_READ,
		 MAP_SHARED|MAP_NORESERVE,
		 fd,
		 0);

      if ( ptr == MAP_FAILED )
        exit(1);
      exit(0);
    }
  ],
  AC_MSG_RESULT(yes)
  AC_DEFINE(CAN_MMAP_FILES, 1,
	    Define if regular files can be mapped),
  AC_MSG_RESULT(no),
  AC_DEFINE(CAN_MMAP_FILES)
  AC_MSG_RESULT(assuming yes))
rm -f conftest.dummy
fi


dnl ****************************************************************
dnl Are our signal handler is POSIX.1b?
dnl ****************************************************************

AC_CHECK_HEADERS(siginfo.h)

AC_MSG_CHECKING("for SA_SIGINFO");
AC_TRY_COMPILE(
[ #include <signal.h>
  #ifdef HAVE_SIGINFO_H
  #include <siginfo.h>
  #endif
  extern void handler(int sig, siginfo_t *info, void *);
],
[ struct sigaction act;

  act.sa_sigaction = handler;
  act.sa_flags = SA_SIGINFO;

  sigaction(SIGSEGV, (void *)0, &act);
], AC_DEFINE(HAVE_SIGINFO, 1,
	     [Define if signal handler is compliant to POSIX.1b])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))


dnl ================================================================
dnl LNLIBS is the set of libraries you need for linking with the SWI-Prolog
dnl kernel.  If this kernel is a shared object, this is typically empty.
dnl If the kernel is a static library it is the set of libraries needed
dnl by the static library.

if test "$MKSHARED" = yes; then
  MKLIBS="$MKLIBS sharedlib"
  CMFLAGS="$CMFLAGS $SO_PIC"
  LDFLAGS="$LDFLAGS $SO_RPATH"
  LNLIBS=""
  AC_DEFINE(O_SHARED_KERNEL, 1,
	    "Define if Prolog kernel is in shared object")
else
  CHRPATH=":"
  LNLIBS="$LIBS"
fi dnl MKSHARED

dnl ================================================================

echo "	CFLAGS=$CWFLAGS $COFLAGS $CIFLAGS $CMFLAGS"
echo "	LDFLAGS=$LDFLAGS"
echo "	LIBS=$LIBS"

OutputFiles="Makefile rc/Makefile libtai/Makefile ../man/Makefile ../man/correctindex swipl.1 swipl.sh swipl.pc"

case "$PLARCH" in
    i386-win32|x64-win64)
        OutputFiles="$OutputFiles os/windows/Makefile win32/console/Makefile"
	;;
esac

# Prepare the package configuration if present

if test -r ../packages/Makefile.in; then
  OutputFiles="$OutputFiles ../packages/Makefile ../packages/swipl-ld.sh ../packages/swipl.sh"
fi

if test -r ../packages/configure.in; then
  OutputFiles="$OutputFiles ../packages/configure ../packages/Makefile.defs ../packages/Dialect.defs"
fi

AC_CONFIG_COMMANDS([exe],
		   [ if test -r ../packages/configure; then
		       chmod +x ../packages/configure
		       chmod +x ../packages/swipl-ld.sh
		       chmod +x ../packages/swipl.sh
		       chmod +x ../man/correctindex
		     fi
		     chmod +x swipl.sh
		   ])
AC_OUTPUT($OutputFiles)

if test "$WARN_NO_GMP" = yes; then
cat << _EOM_
****************************************************************
* Could not find libgmp or gmp.h.  GMP enables SWI-Prolog
* support for unbounded integer arithmetic and rational numbers.
*
* See README.gmp for details.
****************************************************************

_EOM_
fi

cat << _EOM_
****************************************************************
* Now type the following to make and install the basic environment
*
*	% make
*	% make install
*
* To complete the environment, goto the packages directory and check
* the README file for further instructions.
****************************************************************

_EOM_

