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
#		Roetersstraat 15
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
# Settings you propably must change
################################################################

PCEHOME=/staff/jan/exp/xpce-2
XBASE=/usr
ARCH=sparc-sunos-4
#ARCH=i486-linux
PLBASE=/staff/jan/src/pl
PLARCH=sun4
#PLARCH=i386

################################################################
# LINKING STUFF
################################################################

LDFLAGS=-L$(XLIB) -static $(COFLAGS)
#LDFLAGS=-L$(XLIB) $(COFLAGS)
#STATICLIBS=/usr/lib/libc.a
PLLIBS=-ltermcap -lreadline

################################################################
# THINGS YOU MIGHT NEED TO CHANGE, NOTABLY IF YOU DON'T USE GCC
################################################################

CC=gcc
CPlusPlus=g++
COFLAGS=-O2
#COFLAGS=-g
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

XINCLUDES=$(XBASE)/include
XLIB=$(XBASE)/lib

VERSION=4.8.0, June 1994

PLRUNTIME=$(PLBASE)/runtime/$(PLARCH)
PLINCLUDE=$(PLBASE)/include

PCELIBS=-lXt -lX11 -lm

TARGET=$(PCEHOME)/bin/xpce
LIBS=$(PCELIBS) $(PLLIBS)

################################################################
# PLAIN LIBRARY TARGET
################################################################

all:
	cd src; $(MAKE) WST=x11 xpcelib
	cd pl/src; \
	CANONICAL_PATHS=$(PCEHOME); export CANONICAL_PATHS; \
	$(MAKE) TARGET=$(TARGET)

################################################################
# Cleanup.  
################################################################

clean:
	for d in src pl/src; do (cd $$d; $(MAKE) clean); done

realclean:
	for d in src pl/src; do (cd $$d; $(MAKE) realclean); done

