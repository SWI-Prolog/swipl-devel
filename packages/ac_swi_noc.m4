dnl Common autoconf handling SWI-Prolog packages that require no C/C++

AC_SUBST(PL)
AC_SUBST(PLBASE)

plcandidates="swipl swi-prolog pl"
AC_CHECK_PROGS(PL, $plcandidates, "none")
if test $PL = "none"; then
   AC_ERROR("Cannot find SWI-Prolog. SWI-Prolog must be installed first")
else
   AC_CHECKING("Running $PL -dump-runtime-variables")
   eval `$PL -dump-runtime-variables`
fi
PLINCL=$PLBASE/include
AC_MSG_RESULT("		PLBASE=$PLBASE")

AC_PROG_INSTALL
