dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_java_cc.html
dnl
# AC_PROG_JAVA_CC([COMPILER ...])
# --------------------------
# COMPILER ... is a space separated list of java compilers to search for.
# This just gives the user an opportunity to specify an alternative
# search list for the java compiler.
AC_DEFUN([AC_PROG_JAVA_CC],
[AC_ARG_VAR([JAVA_CC],                [java compiler command])dnl
AC_ARG_VAR([JAVA_CC_FLAGS],           [java compiler flags])dnl
m4_ifval([$1],
      [AC_CHECK_TOOLS(JAVA_CC, [$1])],
[AC_CHECK_TOOL(JAVA_CC, gcj)
if test -z "$JAVA_CC"; then
  AC_CHECK_TOOL(JAVA_CC, javac)
fi
if test -z "$JAVA_CC"; then
  AC_CHECK_TOOL(JAVA_CC, jikes)
fi
])

if test "$JAVA_CC" = "gcj"; then
   if test "$GCJ_OPTS" = ""; then
      AC_SUBST(GCJ_OPTS,-C)
   fi
   AC_SUBST(JAVA_CC_OPTS, @GCJ_OPTS@,
        [Define the compilation options for GCJ])
fi
test -z "$JAVA_CC" && AC_MSG_ERROR([no acceptable java compiler found in \$PATH])
])# AC_PROG_JAVA_CC
