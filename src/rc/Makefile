# Generated automatically from Makefile.in by configure.
################################################################
# Makefile for the SWI-Prolog based latex2html converter
#
# Author: Jan Wielemaker
# E-mail: jan@swi.psy.uva.nl
#
# Free for personal usage and usage by academic institutions.
# This package may be distributed freely.
# This package may be modified if it is clearly indicated is concerns a
# modified version, and the copyright and authorship notices are left
# in place.
#
# Copyright (c) 1996, SWI, University of Amsterdam, all rights reserved
################################################################

prefix=/usr/local
exec_prefix=${prefix}
srcdir=.
vpath %.c .
bindir=$(exec_prefix)/bin
PLBASE=$(prefix)/lib/pl-$(PLVERSION)
man_prefix=$(prefix)/man
mansec=1
lmansec=3
mandir=$(man_prefix)/man$(mansec)
lmandir=$(man_prefix)/man$(lmansec)
plmandir=$(PLBASE)/man

CC=gcc
AR=ar
RANLIB=ranlib
COFLAGS=-O2
CWFLAGS=-Wall
CIFLAGS=
CMFLAGS=
CFLAGS=	$(CWFLAGS) $(COFLAGS) $(CIFLAGS) $(CMFLAGS)

INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

LIBOBJ=	html.o access.o build.o util.o

all:	plrc

plrc:	librc.a rc.o
	$(CC) -o $@ rc.o -L. -lrc

librc.a:	$(LIBOBJ)
	rm -f $@
	$(AR) cq $@ $(LIBOBJ)
	$(RANLIB) $@

.c.o:
	$(CC) -c -I.. $(CFLAGS) $< -o $@

################################################################
# Documentation
################################################################

html:	rc.tex
	latex2html rc

dvi:	rc.dvi

rc.dvi:	rc.tex
	./latex_if_needed rc

rc.tex:	rc.doc
	../xpce/bin/doc2tex rc.doc > rc.tex

################################################################
# Cleanup
################################################################

clean:
	rm -f *% *~ $(LIBOBJ) rc.o rcswi.o

distclean:	clean
	rm -f rc rcswi.so

