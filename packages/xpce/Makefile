# $Id$
#
# Makefile for XPCE-4.8
#
# NOTE NOTE NOTE: This Makefile only works with GNU-Make!!
#
# The XPCE project was started by Anjo Anjewierden, who developed version
# 1 and 2.  Version 3 is result of joint effort from Anjo Anjewierden and
# Jan Wielemaker.  Version 4 is written by Jan Wielemaker.
#
# Addresses:	Jan Wielemaker:		jan@swi.psy.uva.nl
#		Anjo Anjewierden:	anjo@swi.psy.uva.nl
#
# 		SWI
#		University of Amsterdam
#		Roetersstraat 15
#		1018 WB  Amsterdam
#		The Netherlands
#		FAX: (+31) 20 5256896
#		Phone: (+31) 20 5256121
#
# Copyright (C) 1994, University of Amsterdam
#

.EXPORT_ALL_VARIABLES:
SHELL=/bin/sh

################################################################
# CONFIGURATION
################################################################
#
# PCEHOME:		Absolute path to this directory
#
# XBASE:		Find X include and lib on
#				$(XBASE)/include/X11
#				$(XBASE)/lib
#			For OpenWindows, normally use
#				XBASE=/usr/openwin
#
# ARCH:			Configuration architecture.  There should be
#			a file src/md/md-$(ARCH).h.
################################################################

################################################################
# Places of things.  These *must* be absolute paths!
################################################################

PCEHOME=/staff/jan/src/xpce
PLBASE=/staff/jan/src/pl
SICSHOME=/staff/jan/src/sicstus2.1
SICSTARGET=$(PCEHOME)/bin/xpce-sicstus

################################################################
# X11 (OpenWindows, Motif, ...) place.  For OpenWindows compilation
# you are likely to need XBASE=/usr/openwin.
################################################################

XBASE=/usr
#(openwindows)#XBASE=/usr/openwin

################################################################
# Determine the ARCH parameter from the tcsh(1) value of the
# variable HOSTTYPE.  Allows you to simply type `make' if
# your shell is tcsh (only for poor people like myself, who
# types `make' quite often down here :-)
################################################################

ifndef ARCH
ifeq ($(HOSTTYPE),i386-linux)
ARCH=i486-linux
else
ifeq ($(HOSTTYPE),sun4)
ARCH=sparc-sunos-4
endif
endif
endif

################################################################
# Set libraries and machine-subdirectory for linking with
# SWI-Prolog, based on the ARCH variable
################################################################

ifeq ($(ARCH),sparc-sunos-4)
	PLARCH=sun4
	LDFLAGS=-L$(XLIB) $(COFLAGS) -static
	STATICLIBS=/usr/lib/libc.a -Wl,-Bdynamic -ldl
else
ifeq ($(ARCH),sparc-sunos-5)
	PLARCH=solaris
	LDFLAGS=-L$(XLIB) -lsocket -ldl -lnsl -lelf
	STATICLIBS=/usr/lib/libc.a
else
ifeq ($(ARCH),i486-linux)
	PLARCH=i386
	LDFLAGS=-L$(XLIB)
	STATICLIBS=/usr/lib/libc.a
else
	PLARCH=unknown
	LDFLAGS=-L$(XLIB)
	STATICLIBS=
endif
endif
endif

PLLIBS=-ltermcap -lreadline

# Install XPCE/SWI-Prolog here.  See install-pl-bins for details

PUBLIC_AREA=/usr/local

################################################################
# THINGS YOU MIGHT NEED TO CHANGE, NOTABLY IF YOU DON'T USE GCC
#
# CC:			C-compiler.  On most machines you will only
#			be able to build XPCE using gcc.
#
# CMFLAGS:		Miscelaneous C-flags
#
# COFLAGS:		Optimisation options for $(CC)
#
# CWFLAGS:		Warning level for $(CC)
# 
# CIFLAGS:		Include directories.  Use $(XBASE) to get the
#			X11 includes right.
################################################################

CC=gcc
CPlusPlus=g++
CMFLAGS=-funsigned-char
COFLAGS=-O2
CWFLAGS=-Wall
CIFLAGS=-I..

################################################################
# THIS SHOULD BE OK ON MOST UNIX MACHINES
#
# LN: if you don't have ln, simply replace by `cp'.
################################################################

LN=ln -s
AR=ar
ARFLAGS=ru
ETAGS=etags
SED=sed

XINCLUDES=$(XBASE)/include
XLIB=$(XBASE)/lib

VERSION=4.8.2, October 1994
RESOURCE_CLASS=Pce

PLRUNTIME=$(PLBASE)/runtime/$(PLARCH)
PLINCLUDE=$(PLBASE)/include

PCELIBS=-lXt -lX11 -lm

TARGET=$(PCEHOME)/bin/xpce
LIBS=$(PCELIBS) $(PLLIBS)

################################################################
# MAIN TARGETS
################################################################

all:		check xpce-pl

everything:	check proto tags xpce-pl

################################################################
# TARGETS
################################################################

tags:
	cd src; $(MAKE) tags

proto:
	cd src; $(MAKE) proto

xpce:	$(RESOURCE_CLASS) check
	cd src; $(MAKE) WST=x11 xpcelib

xpce-pl: xpce xpce-client
	cd pl/src; \
	CANONICAL_PATHS=$(PCEHOME); export CANONICAL_PATHS; \
	$(MAKE) TARGET=$(TARGET)

xpce-sicstus: xpce xpce-client
	cd sicstus/src; \
	$(MAKE) TARGET=$(SICSTARGET)

xpce-client:	bin/xpce-client

bin/xpce-client:	src/unx/client.c
	cd src/unx ; $(MAKE) xpce-client
	mv src/unx/xpce-client $@

check:
    ifndef ARCH
	@echo "*** ERROR: No value for Makefile variable ARCH."
	@echo "Invoke as:"
	@echo ""
	@echo "	\"make ARCH=<architecture>\""
	@echo ""
	@echo "Where <architecture> is one of:"
	@echo ""
	@ls src/md/md-*.h | sed 's/.*md-\(.*\)\.h/	\1/'
	@echo ""
	@exit 1
    endif


ifneq ($(RESOURCE_CLASS),Pce)
$(RESOURCE_CLASS):	Makefile Pce
	$(SED) "s/\<Pce\./$(RESOURCE_CLASS)./" Pce > .Pce
	mv .Pce $@
endif

################################################################
# INSTALLATION
################################################################

install-pl: xpce-pl
	./install-pl-bins $(PUBLIC_AREA)


################################################################
# PREPARE BINARY DISTRIBUTION
################################################################

bin/xpce.qfl:
	bin/xpce.base \
		-g pce_host:pce_reinitialise \
		-b ../pl/boot/init.pl \
		-c src/load.pl


################################################################
# Cleanup.  
################################################################

clean:
	for d in src pl/src; do (cd $$d; $(MAKE) clean); done

realclean:
	for d in src pl/src; do (cd $$d; $(MAKE) realclean); done


