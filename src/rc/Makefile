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

CC=gcc
LD=$(CC)
LDSO=$(CC)
LDSOFlAGS=-shared
AR=ar
RANLIB=ranlib
COFLAGS=-g
CWFLAGS=-Wall
CDFLAGS=-I. -fpic
CFLAGS=$(CWFLAGS) $(COFLAGS) $(CDFLAGS)
INSTALL=install -c
INSTALL_PROGRAM=$(INSTALL) -m 755
INSTALL_DATA=$(INSTALL) -m 644

LIBOBJ=	html.o access.o build.o util.o

all:	rc

rc:	librc.a rc.o
	$(LD) -o $@ rc.o -L. -lrc

librc.a:	$(LIBOBJ)
	rm -f $@
	$(AR) cq $@ $(LIBOBJ)
	$(RANLIB) $@

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

