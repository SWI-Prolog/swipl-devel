dnl Common autoconf handling SWI-Prolog packages that require C/C++

AC_SUBST(PL)
AC_SUBST(PLLIBS)
AC_SUBST(PLBASE)
AC_SUBST(PLARCH)
AC_SUBST(PLINCL)
AC_SUBST(INSTALL_PLARCH)
AC_SUBST(COFLAGS)
AC_SUBST(CIFLAGS)
AC_SUBST(CWFLAGS)
AC_SUBST(CMFLAGS)
AC_SUBST(ETAGS)
AC_SUBST(LD)
AC_SUBST(SO)				dnl shared-object extension (e.g., so)
AC_SUBST(SO_PATH)			dnl variable name for SO search path
AC_SUBST(LDSOFLAGS)			dnl pass -shared to swipl-ld
AC_SUBST(SOLIB)
AC_SUBST(TXTEXT)

PLPKGDIR=${PLPKGDIR-..}

if test -z "$PLINCL"; then
plcandidates="swipl swi-prolog pl"
AC_CHECK_PROGS(PL, $plcandidates, "none")
AC_CHECK_PROGS(PLLD, swipl-ld plld, "none")
if test $PLLD = "none"; then
   AC_ERROR("Cannot find SWI-Prolog swipl-ld utility. SWI-Prolog must be installed first")
fi
if test $PL = "none"; then
   AC_ERROR("Cannot find SWI-Prolog. SWI-Prolog must be installed first")
else
   AC_CHECKING("Running $PL -dump-runtime-variables")
   eval `$PL -dump-runtime-variables`
fi
PLINCL=$PLBASE/include
AC_MSG_RESULT("		PLBASE=$PLBASE")
AC_MSG_RESULT("		PLARCH=$PLARCH")
AC_MSG_RESULT("		PLLIBS=$PLLIBS")
AC_MSG_RESULT("		PLLDFLAGS=$PLLDFLAGS")
AC_MSG_RESULT("		PLSHARED=$PLSHARED")
AC_MSG_RESULT("		PLSOEXT=$PLSOEXT")
AC_MSG_RESULT("		PLSOPATH=$PLSOPATH")
if test "$PLTHREADS" = "yes"; then MT=yes; fi
CC=$PLLD
LD=$PLLD
LDSOFLAGS=-shared
CMFLAGS=-fPIC
SO="$PLSOEXT"

else					# Called in build-tree

dialectvar()
{ grep "^$1=" $PLPKGDIR/Dialect.defs | sed "s/^$1=//"
}

PL="$PLPKGDIR/swipl.sh"
CC="$(dialectvar CC)"
CXX="$(dialectvar CXX)"
LD="$(dialectvar LD)"
SO="$(dialectvar SO)"
COFLAGS="$(dialectvar COFLAGS)"
CWFLAGS="$(dialectvar CWFLAGS)"
CMFLAGS="$(dialectvar CMFLAGS)"
CIFLAGS="$(dialectvar CIFLAGS)"

CPPFLAGS="$CIFLAGS $CPPFLAGS"
CFLAGS="$COFLAGS $CWFLAGS $CMFLAGS $CIFLAGS"

# Assume thread support
MT=yes
fi

case "$PLARCH" in
    *-win32|*-win64)
        SOLIB=bin
        TXTEXT=.TXT
        ;;
    *)
        SOLIB=lib
        INSTALL_PLARCH=$PLARCH
        ;;
esac

AC_CHECK_PROGS(MAKE, gmake make, "make")
AC_CHECK_PROGS(ETAGS, etags ctags, ":")
AC_PROG_INSTALL
AC_ISC_POSIX
AC_HEADER_STDC
CFLAGS="$CMFLAGS"
AC_FUNC_ALLOCA
AC_C_BIGENDIAN

if test ! -z "$GCC"; then
    COFLAGS="${COFLAGS--O2 -fno-strict-aliasing}"
    CWFLAGS="${CWFLAGS--Wall}"
else
    COFLAGS="${COFLAGS--O}"
fi

if test "x$MT" = "xyes"; then
dnl Get MinGW thread support.  Note that this may change if we move to
dnl winpthreads.h.  We could also consider handling this through $PLLIBS,
dnl but in theory it should be possible to compile external packages with
dnl different thread libraries.

AC_DEFINE(_REENTRANT, 1, "Define for multi-threaded support (deprecated)")
AC_DEFINE(O_PLMT, 1, "Define for multi-threaded support")

case "$PLARCH" in
     *-win32|*-win64)
        AC_CHECK_LIB(pthreadGC2, pthread_create)
        if test ! "$ac_cv_lib_pthreadGC2_pthread_create" = "yes"; then
          AC_CHECK_LIB(pthreadGC, pthread_create)
          if test ! "$ac_cv_lib_pthreadGC_pthread_create" = "yes"; then
            AC_CHECK_LIB(pthread, pthread_create)
          fi
        fi
        ;;
esac

else dnl MT=yes

AC_MSG_WARN([Seems that SWI-Prolog is not compiled with threads enabled])

fi dnl MT=yes

AC_DEFINE(PL_ARITY_AS_SIZE, 1, "Represent arity as size_t")
