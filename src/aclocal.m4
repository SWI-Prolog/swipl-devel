dnl  SWI-Prolog specific autoconf macros

dnl  Copyright 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2009 Free Software
dnl  Foundation, Inc.
dnl  Copyright 2011, Roberto Bagnara, BUGSENG srl.
dnl
dnl  This file is part of SWI-Prolog.
dnl
dnl  This library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 3 of the License, or (at
dnl  your option) any later version.
dnl
dnl  This library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with this library.  If not, see http://www.gnu.org/licenses/.


dnl  All the "FOR_BUILD" macros have been copied from the GNU MP Library
dnl  and then slightly modified.


dnl  SWI_PROG_CC_FOR_BUILD
dnl  ---------------------
dnl  Establish CC_FOR_BUILD, a C compiler for the build system.
dnl
dnl  If CC_FOR_BUILD is set then it's expected to work, otherwise some
dnl  likely candidates are tried.

AC_DEFUN([SWI_PROG_CC_FOR_BUILD],
[AC_REQUIRE([AC_PROG_CC])
if test -n "$CC_FOR_BUILD"; then
  SWI_PROG_CC_FOR_BUILD_WORKS($CC_FOR_BUILD,,
    [AC_MSG_ERROR([Specified CC_FOR_BUILD doesn't seem to work])])
else
  for i in cc gcc c89 c99 "$CC" "$CC $CFLAGS $CPPFLAGS"; do
    SWI_PROG_CC_FOR_BUILD_WORKS($i,
      [CC_FOR_BUILD=$i
       break])
  done
  if test -z "$CC_FOR_BUILD"; then
    AC_MSG_ERROR([Cannot find a build system compiler])
  fi
fi

AC_ARG_VAR(CC_FOR_BUILD,[build system C compiler])
AC_SUBST(CC_FOR_BUILD)
])


dnl  SWI_PROG_CC_FOR_BUILD_WORKS(cc/cflags[,[action-if-good][,action-if-bad]])
dnl  -------------------------------------------------------------------------
dnl  See if the given cc/cflags works on the build system.
dnl
dnl  It seems easiest to just use the default compiler output, rather than
dnl  figuring out the .exe or whatever at this stage.

AC_DEFUN([SWI_PROG_CC_FOR_BUILD_WORKS],
[AC_MSG_CHECKING([build system compiler $1])
# remove anything that might look like compiler output to our "||" expression
rm -f conftest* a.out b.out a.exe a_out.exe
cat >conftest.c <<EOF
int
main ()
{
  exit(0);
}
EOF
swi_compile="$1 conftest.c"
cc_for_build_works=no
if AC_TRY_EVAL(swi_compile); then
  if (./a.out || ./b.out || ./a.exe || ./a_out.exe || ./conftest) >&AC_FD_CC 2>&1; then
    cc_for_build_works=yes
  fi
fi
rm -f conftest* a.out b.out a.exe a_out.exe
AC_MSG_RESULT($cc_for_build_works)
if test "$cc_for_build_works" = yes; then
  ifelse([$2],,:,[$2])
else
  ifelse([$3],,:,[$3])
fi
])


dnl  SWI_PROG_EXEEXT_FOR_BUILD
dnl  -------------------------
dnl  Determine EXEEXT_FOR_BUILD, the build system executable suffix.
dnl
dnl  The idea is to find what "-o conftest$foo" will make it possible to run
dnl  the program with ./conftest.  On Unix-like systems this is of course
dnl  nothing, for DOS it's ".exe", or for a strange RISC OS foreign file
dnl  system cross compile it can be ",ff8" apparently.  Not sure if the
dnl  latter actually applies to a build-system executable, maybe it doesn't,
dnl  but it won't hurt to try.

AC_DEFUN([SWI_PROG_EXEEXT_FOR_BUILD],
[AC_REQUIRE([SWI_PROG_CC_FOR_BUILD])
AC_CACHE_CHECK([for build system executable suffix],
               swi_cv_prog_exeext_for_build,
[cat >conftest.c <<EOF
int
main ()
{
  exit (0);
}
EOF
for i in .exe ,ff8 ""; do
  swi_compile="$CC_FOR_BUILD conftest.c -o conftest$i"
  if AC_TRY_EVAL(swi_compile); then
    if (./conftest) 2>&AC_FD_CC; then
      swi_cv_prog_exeext_for_build=$i
      break
    fi
  fi
done
rm -f conftest*
if test "${swi_cv_prog_exeext_for_build+set}" != set; then
  AC_MSG_ERROR([Cannot determine executable suffix])
fi
])
AC_SUBST(EXEEXT_FOR_BUILD,$swi_cv_prog_exeext_for_build)
])
