# Generated automatically from Makefile.in by configure.
################################################################
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
################################################################

.EXPORT_ALL_VARIABLES:
SHELL=/bin/sh

prefix=/usr/local
exec_prefix=${prefix}
PCEHOME=/staff/jan/src/xpce
XINCLUDES=/usr/include
XLIB=/usr/lib

# PROGRAMS

CC=gcc
CXX=g++
RANLIB=ranlib
PL=pl

# LIBRARIES

NETLIBS=
PLLIBS=-ldl -lreadline -ltermcap -lm 
XLIBS=-lXt -lX11
STATICLIBS=/usr/lib/libc.a
LDFLAGS=
GCCLIB=/usr/local/lib/gcc-lib/sparc-sun-sunos4.1.3/2.6.3/libgcc.a
LDSOFLAGS=
SOEXTRAOBJ=Initialize.o
SO=so

LIBS=	$(XLIBS) $(PLLIBS) $(NETLIBS) $(STATICLIBS)
COFLAGS=-O2
CWFLAGS=
CIFLAGS=-I..
CMFLAGS= -fPIC -funsigned-char -DHAVE_CONFIG_H

################################################################
# Paths for host-languages
################################################################

PLBASE=/staff/jan/lib/pl-2.1.3
PLARCH=sparc-sunos4.1.3

SICSHOME=/staff/jan/src/sicstus2.1
SICSTARGET=$(PCEHOME)/bin/xpce-sicstus

################################################################
# THIS SHOULD BE OK ON MOST UNIX MACHINES
################################################################

LN=ln -s
AR=ar
ARFLAGS=ru
ETAGS=etags
SED=sed
RM=rm

VERSION=4.8.11, June. 1995
RTSUFFIX=
ARCH=sparc
OS=sunos4.1.3
RESOURCE_CLASS=Pce

PLRUNTIME=$(PLBASE)/runtime/$(PLARCH)
PLOBJ=pl$(RTSUFFIX).o
PLINCLUDE=$(PLBASE)/include

PLTARGET=soall

################################################################
# MAIN TARGETS
################################################################

all:		banner xpce-pl

everything:	proto tags xpce-pl

################################################################
# TARGETS
################################################################

tags:
	cd src; $(MAKE) tags

proto:
	cd src; $(MAKE) proto

banner:
	@echo "****************"
	@echo "Making XPCE $(VERSION) for $(ARCH)-$(OS)"
	@echo "****************"


xpce:	$(RESOURCE_CLASS)
	cd src; $(MAKE) WST=x11 xpcelib

xpce-pl: xpce xpce-client
	cd pl/src; \
	CANONICAL_PATHS=$(PCEHOME); export CANONICAL_PATHS; \
	$(MAKE) $(PLTARGET)

so:	
	cd pl/src; \
	$(MAKE) soall

state:
	cd pl/src; \
	CANONICAL_PATHS=$(PCEHOME); export CANONICAL_PATHS; \
	$(MAKE) nosoall

xpce-sicstus: xpce xpce-client
	cd sicstus/src; \
	$(MAKE) TARGET=$(SICSTARGET)

xpce-client:	bin/xpce-client

bin/xpce-client:	src/unx/client.c
	cd src; $(MAKE) xpce-client
	mv src/unx/xpce-client $@

$(RESOURCE_CLASS):	Makefile Pce.in
	$(SED) "s/\<Pce\./$(RESOURCE_CLASS)./" Pce.in > $@

################################################################
# INSTALLATION
################################################################

install-pl: xpce-pl
	./install-pl-bins $(prefix)


################################################################
# PREPARE BINARY DISTRIBUTION
################################################################

bin/xpce.qlf:
	bin/xpce.base \
		-g pce_host:pce_banner \
		-b $(PLBASE)/boot/init.pl \
		-c $(PLBASE)/boot/load.pl \
		   pl/load.pl


################################################################
# Cleanup.  
################################################################

clean:
	for d in src pl/src; do (cd $$d; $(MAKE) clean); done
	$(RM) -f *~ *% a.out core

realclean:
	for d in src pl/src; do (cd $$d; $(MAKE) realclean); done
	$(RM) -f bin/xpce bin/xpce-client bin/xpce-sicstus bin/xpce.base
	$(RM) -f config.log config.cache Pce

distclean:	realclean

