dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

m4_include([../ac_swi_c.m4])

AC_CHECK_SIZEOF(long, 4)

AC_CHECK_HEADERS(malloc.h unistd.h sys/time.h fcntl.h floatingpoint.h)
AC_CHECK_FUNCS(snprintf strerror strtoll)

AC_OUTPUT(Makefile)
