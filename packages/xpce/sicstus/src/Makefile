# Generated automatically from Makefile.in by configure.
################################################################
# Makefile for SICSTus binding to XPCE
#
# Created for SICStus 3.7.1 and XPCE 5.0.5
# Author: Jan Wielemaker
# Date:   Tue Jun  8 1999
#
# Makefile.in is used by ../../src/configure to create Makefile.
#
# Targets:
#	make			Creates pce.so and libraries
#	make install-libs	Copies XPCE library to public location
#	make ql			Creates .ql files in public location
#	make xpce		Creates xpce program as a saved state
#	make install		Install everything
#
# Copyright (C) University of Amsterdam
################################################################

SP_PATH=/usr/local/lib/sicstus-3.7.1
PCEHOME=$(SP_PATH)/xpce
XPCE=xpce

RSYNC=rsync -aC --exclude-from=Exclude
ARCH=linux
ARCHDIR=../../$(ARCH)
CC=gcc
COFLAGS=-O2
CWFLAGS=-Wall
CIFLAGS=-I. -I$(ARCHDIR)
CMFLAGS= -fPIC -funsigned-char -DHAVE_CONFIG_H
RANLIB=ranlib
XLIBS=-lXt -lX11 -lSM -lICE -ljpeg -lXpm
XLIB=/usr/X11R6/lib

CFLAGS=-DSICSTUS -DHAVE_CONFIG_H $(CIFLAGS) $(CWFLAGS) $(COFLAGS)
LIBS=-L. -L$(ARCHDIR) -L$(XLIB) -lswi -lXPCE $(XLIBS)

LIBOBJ=		swi.o swi-stream.o

all:		pce.so .libindex
		(cd lib && $(MAKE))

################################################################
# SICSTUS/XPCE Interface
################################################################

pce.so:		libswi.a interface.o pce_sicstus.pl
		splfr pce pce_sicstus.pl +o interface.o +l $(LIBS)

.libindex:	../../prolog/lib
		sicstus -l mkindex.pl -a ../../prolog/lib
		touch $@

################################################################
# SICSTUS/XPCE resources
################################################################

XPCELIB=	$(PCEHOME)/prolog/lib
LIBINDEX=	$(XPCELIB)/INDEX.pl

SPBOOT=		pce_expansion.pl \
		pce_goal_expansion.pl \
		pce_principal.pl \
		pce_sicstus.pl \
		pce.so

SPLIB=		pce.pl \
		sp_compat.pl \
		sp_compat.c \
		sp_compat.so \
		sp_make.pl \
		sp_report.pl \
		sp_fcompile.pl

install-lib:	sicsrc spfiles $(LIBINDEX)

sicsrc:		
		mkdir -p $(PCEHOME)/prolog
		mkdir -p $(PCEHOME)/man/reference
		mkdir -p $(PCEHOME)/bitmaps
		$(RSYNC) ../../prolog $(PCEHOME)
		$(RSYNC) ../../man/reference $(PCEHOME)/man
		$(RSYNC) ../../man/xpce.1 $(PCEHOME)/man/xpce.1
		$(RSYNC) ../../man/xpce-client.1 $(PCEHOME)/man/xpce-client.1
		$(RSYNC) ../../bitmaps $(PCEHOME)

$(PCEHOME)/prolog/boot/%:	%
		cp -p $< $@
$(PCEHOME)/prolog/lib/%:	lib/%
		cp -p $< $@
$(PCEHOME)/mkxpce.pl:
		cp -p mkxpce.pl $@
$(PCEHOME)/README:
		cp -p README $@

spfiles:	$(addprefix $(PCEHOME)/prolog/boot/, $(SPBOOT)) \
		$(addprefix $(PCEHOME)/prolog/lib/, $(SPLIB)) \
		$(PCEHOME)/mkxpce.pl \
		$(PCEHOME)/README

$(LIBINDEX):	$(XPCELIB)
		sicstus -l mkindex.pl -a $(XPCELIB)

################################################################
# QL
################################################################

ql:		install-lib bootql libql

bootql:
		sicstus -l ql.pl -a boot
libql:
		sicstus -l ql.pl -a library

################################################################
# SWI-Prolog emulation library
################################################################

libswi.a:	$(LIBOBJ)
		rm -f $@
		$(AR) r $@ $(LIBOBJ)
		$(RANLIB) $@

################################################################
# Dependencies
################################################################

swi.o:		swi.h
swi-stream.o:	SWI-Stream.h

################################################################
# Dump the state
################################################################

$(XPCE):
		sicstus -l mkxpce.pl -a $(PCEHOME)/prolog/lib $@

################################################################
# Cleanup
################################################################

clean:
	rm -f *~ *.o 
	(cd lib && $(MAKE) clean)

distclean: clean
	rm -f libswi.a pce.so
	(cd lib && $(MAKE) distclean)

uninstall:
	rm -fr $(PCEHOME)/*

qclean:
	rm -f `find $(PCEHOME)/prolog -name '*.ql'`
