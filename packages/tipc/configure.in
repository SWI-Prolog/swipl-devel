dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

AC_SUBST(TARGETS)
AC_SUBST(PLTARGETS)
AC_SUBST(LIBTXT)

m4_include([../ac_swi_c.m4])

AC_SUBST(NETLIBS)
AC_CHECK_FUNC(socket, [], [
             AC_CHECK_LIB(socket, socket,
                      [NETLIBS="$NETLIBS -lsocket"; LIBS="$LIBS -lsocket"]
                      AC_DEFINE(HAVE_LIBSOCKET, 1,
                      "Define if you have the socket library (-lsocket)."))])

AC_MSG_CHECKING(h_errno)
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
],
[ int x = h_errno;
], AC_DEFINE(HAVE_H_ERRNO, 1,
	     [Define of h_errno is provided])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_CHECK_HEADERS(sys/types.h)
AC_CHECK_HEADERS(linux/tipc.h)
AC_CHECK_HEADER(linux/tipc.h, [ tpic=yes ])

AC_CHECK_TYPES(socklen_t, [], [],
[
#include <sys/types.h>
#include <sys/socket.h>
])

if test "$tpic" = "yes"; then
  TARGETS="$TARGETS tipc.$SO"
  LIBTXT="$LIBTXT tipc_overview.txt"
fi

PLTARGETS=`echo $TARGETS | sed -e "s/\.$SO/.pl/g" -e "s/files.pl/filesex.pl/"`
if echo "$PLTARGETS" | grep tipc.pl 2>&1 >/dev/null; then
  PLTARGETS="$PLTARGETS tipc_broadcast.pl tipc_paxos.pl tipc_linda.pl"
fi

AC_OUTPUT(Makefile)



