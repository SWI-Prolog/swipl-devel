# $Id$
#
# Makefile for XPCE-4.8
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
#
# CC:			C-compiler.  On most machines you will only
#			be able to build XPCE using gcc.
#
# COFLAGS:		Optimisation options for $(CC)
#
# CWFLAGS:		Warning level for $(CC)
# 
# CIFLAGS:		Include directories.  Use $(XBASE) to get the
#			X11 includes right.
#
################################################################

################################################################
# Settings you propably must change.
# Default setup is for SunOs 4.1.x.  Other tested settings are
# flagged using #(condition)#
################################################################

PCEHOME=/staff/jan/src/xpce

XBASE=/usr
#(openwindows)#XBASE=/usr/openwin

ARCH=sparc-sunos-4
#(solaris)# ARCH=sparc-sunos-5
#(linux)#ARCH=i486-linux

PLBASE=/staff/jan/src/pl

PLARCH=sun4
#(solaris)# PLARCH=solaris
#(linux)#PLARCH=i386

LDFLAGS=-L$(XLIB) -static $(COFLAGS)
#(solaris)# LDFLAGS=-L$(XLIB) -lsocket -ldl -lnsl -lelf
#(linux)#LDFLAGS=-L$(XLIB)

STATICLIBS=/usr/lib/libc.a
PLLIBS=-ltermcap -lreadline

# SICStus connection stuff

SICSHOME=	/staff/jan/src/sicstus2.1
SICSTARGET=	$(PCEHOME)/bin/xpce-sicstus

# Install XPCE/SWI-Prolog here.  See install-pl-bins for details

PUBLIC_AREA=/usr/local

################################################################
# THINGS YOU MIGHT NEED TO CHANGE, NOTABLY IF YOU DON'T USE GCC
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

VERSION=4.8.1, October 1994
RESOURCE_CLASS=Pce

PLRUNTIME=$(PLBASE)/runtime/$(PLARCH)
PLINCLUDE=$(PLBASE)/include

PCELIBS=-lXt -lX11 -lm

TARGET=$(PCEHOME)/bin/xpce
LIBS=$(PCELIBS) $(PLLIBS)

################################################################
# MAIN TARGETS
################################################################

all:		xpce-pl

everything:	proto tags xpce-pl

################################################################
# TARGETS
################################################################

tags:
	cd src; $(MAKE) tags

proto:
	cd src; $(MAKE) proto

xpce:	$(RESOURCE_CLASS)
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

$(RESOURCE_CLASS):	Makefile Pce
	$(SED) "s/\<Pce\./$(RESOURCE_CLASS)./" Pce > .Pce
	mv .Pce $@


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


