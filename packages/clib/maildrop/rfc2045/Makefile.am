#$Id$
#
# Copyright 1998 - 1999 Double Precision, Inc.  See COPYING for
# distribution information.

noinst_LIBRARIES=librfc2045.a
noinst_PROGRAMS=reformime makemime

EXTRA_DIST=reformime.html makemime.html rfc2045.html
librfc2045_a_SOURCES=rfc2045.c rfc2045.h rfc2045acchk.c rfc2045acprep.c \
		     rfc2045appendurl.c rfc2045cdecode.c rfc2045decode.c \
		     rfc2045enomem.c \
                     rfc2045_base64encode.c rfc2045_fromfd.c \
                     rfc2045find.c rfc2045mkboundary.c rfc2045rewrite.c \
                     rfc2045tryboundary.c rfc2045xdump.c

reformime_SOURCES=reformime.c
reformime_LDADD = librfc2045.a ../rfc822/librfc822.a ../numlib/libnumlib.a
reformime_DEPENDENCIES = $(reformime_LDADD)

makemime_SOURCES=makemime.c
makemime_LDADD = librfc2045.a ../numlib/libnumlib.a
makemime_DEPENDENCIES=$(makemime_LDADD)
