# $Id$
#
# Copyright 1998 - 1999 Double Precision, Inc.  See COPYING for
# distribution information.

noinst_LIBRARIES=librfc822.a
librfc822_a_SOURCES=rfc822.c rfc822.h rfc822_getaddr.c rfc822_getaddrs.c \
                  rfc822_mkdate.c rfc822_parsedt.c rfc2047u.c \
                  rfc2047.c rfc2047.h imapsubj.c imaprefs.h imaprefs.c

noinst_PROGRAMS=testsuite reftest
testsuite_SOURCES=testsuite.c
testsuite_DEPENDENCIES=librfc822.a
testsuite_LDADD=librfc822.a
reftest_SOURCES=reftest.c imaprefs.h
reftest_DEPENDENCIES=librfc822.a
reftest_LDADD=librfc822.a

EXTRA_DIST=testsuite.txt reftest.txt rfc822.html

check:
	./testsuite | cmp -s - $(srcdir)/testsuite.txt
	./reftest | cmp -s - $(srcdir)/reftest.txt
