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

MAKE=gmake

PL=pl
PLARCH=i686-linux-gnu
PLHOME=@PLHOME@
prefix=/staff/jan
exec_prefix=${prefix}
bindir=${exec_prefix}/bin

CC=plld
COFLAGS=-O2
CWFLAGS=-Wall
CMFLAGS=

LD=plld
LDFLAGS=-shared

CDFLAGS=-D__SWI_PROLOG__
CFLAGS=$(COFLAGS) $(CWFLAGS) $(CMFLAGS) $(CIFLAGS) -DHAVE_CONFIG_H $(CDFLAGS)
INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

# Don't change, this is also in latex2html.in!
LIBDIR=$(prefix)/lib/latex2html

OBJ=	tex.o psfile.o
LIB=	latex2html.pl latex.cmd pl.pl pl.cmd xpce.pl xpce.cmd

all:
	@echo "==============================================================="
	@echo "Usage:"
	@echo ""
	@echo "$(MAKE) install        Install the package"
	@echo "$(MAKE) html           Translate the documentation into HTML"
	@echo "$(MAKE) dvi            Translate the documentation into DVI"
	@echo "==============================================================="

install:	tex.so
	mkdir -p $(LIBDIR)/lib/$(ARCH)
	$(INSTALL_DATA) tex.so $(LIBDIR)/lib/$(ARCH)
	$(INSTALL_PROGRAM) latex2html $(bindir)
	for f in $(LIB); do $(INSTALL_DATA) $$f $(LIBDIR); done
	mkdir -p $(LIBDIR)/icons
	for f in icons/*.gif; do $(INSTALL_DATA) $$f $(LIBDIR)/icons; done

tex.so:		$(OBJ)
	$(LD) $(LDSOFLAGS) -o $@ $(OBJ)

################################################################
# Documentation
################################################################

html:	manual.tex
	latex2html manual

dvi:	manual.tex
	latex manual
	latex manual

manual.tex:	manual.doc
	./doc2tex manual.doc > manual.tex

################################################################
# Cleanup
################################################################

clean:
	rm -f *% *~ $(OBJ) tex.so latex2html

