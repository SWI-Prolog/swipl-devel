################################################################
# Makefile for the SWI-Prolog based latex2html converter
#
# Author: Jan Wielemaker
# E-mail: jkan@swi.psy.uva.nl
#
# Free for personal usage and usage by academic institutions.
# This package may be distributed freely.
# This package may be modified if it is clearly indicated is concerns a
# modified version, and the copyright and authorship notices are left
# in place.
#
# Copyright (c) 1996, SWI, University of Amsterdam, all rights reserved
################################################################

PL=pl
prefix=/oorlam/jan
LIBDIR=$(prefix)/lib/latex2html
BINDIR=$(prefix)/bin

CC=gcc
LDSO=gcc
LDSOFLAGS=-shared
COFLAGS=-O2
CDFLAGS=-D__SWI_PROLOG__
CFLAGS=$(COFLAGS) $(CDFLAGS) -fpic
INSTALL=install -c
INSTALL_PROGRAM=$(INSTALL) -m 755
INSTALL_DATA=$(INSTALL) -m 644

OBJ=	tex.o psfile.o
LIB=	latex2html.pl latex.cmd pl.pl pl.cmd

all:
	@echo "==============================================================="
	@echo "Usage:"
	@echo ""
	@echo "$(MAKE) install        Install the package"
	@echo "$(MAKE) html           Translate the documentation into HTML"
	@echo "$(MAKE) dvi            Translate the documentation into DVI"
	@echo "==============================================================="

install:
	eval `$(PL) -dump-runtime-variables` && $(MAKE) ARCH=$$PLARCH HOME=$$PLBASE xinstall

xinstall:	tex.so latex2html
	mkdir -p $(LIBDIR)/lib/$(ARCH)
	$(INSTALL_DATA) tex.so	      $(LIBDIR)/lib/$(ARCH)
	$(INSTALL_PROGRAM) latex2html $(BINDIR)
	for f in $(LIB); do $(INSTALL_DATA) $$f $(LIBDIR); done

latex2html:	Makefile latex2html.in
	sed -e 's%@PL@%$(PL)%' -e 's%@LIBDIR@%$(LIBDIR)%' latex2html.in > $@
	chmod 755 $@

tex.so:		$(OBJ)
	$(LDSO) $(LDSOFLAGS) -o $@ $(OBJ)

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

