#   Makefile for SWI-prolog version 1.6
#   $Id$
#
#   Jan Wielemaker December 6, 1989
#
#   Copyright in the whole and every part of this software is owned  by  the
#   University  of  Amsterdam.   No reproduction of the whole or any part of
#   this software is to be made without the authority of the  University  of
#   Amsterdam.
#
#   Copyright (c) 1989, University of Amsterdam
#

#
# This file contains changes which are part of a port to HPUX 8.0
# T. Kielmann, 01 Jun 92
#

#include "md.h"

# The following parameters are loaded from md.h (using cpp)
# (delete-quotes) requests to delete double-quotes from the line

CC=M_CC
OPTIMIZE=M_OPTIMIZE
CFLAGS=${OPTIMIZE} M_CFLAGS
LDFLAGS=M_LDFLAGS
LIBS=M_LIBS
#ifdef UNEXEC_SOURCE
UNEXEC=UNEXEC_SOURCE (delete-quotes)
#else
UNEXEC=gnu/unexec.c
#endif
INSTALL=cp
#ifdef FOREIGN_PL
FOREIGN=FOREIGN_PL
#else
FOREIGN=foreign.pl
#endif

# paths and file names for installation

#include "../config.h"
SYSTEMHOME=M_SYSTEMHOME
DEFSTARTUP=M_DEFSTARTUP
PROLOG=M_PROLOG
BINDIR=M_BINDIR

SYS=pl
STARTUPDIR=../startup
STARTUPPATH=$(STARTUPDIR)/startup.MACHINE (delete-quotes)
PB=../boot
BINLIB=libPL.a
CPP=/lib/cpp
EXPORTS=../include/SWI-Exports
CINCLUDE=../include/SWI-Prolog.h

# NOTE: pl-wam.o MUST be first to get it into low addresses when using the
# O_ASM_SWITCH flag

OBJ=	pl-wam.o pl-save.o \
	pl-atom.o pl-alloc.o pl-arith.o pl-bag.o pl-comp.o pl-dump.o \
	pl-dwim.o pl-ext.o pl-file.o pl-flag.o pl-fmt.o pl-funct.o \
	pl-gc.o pl-glob.o pl-index.o pl-itf.o pl-list.o pl-load.o \
	pl-modul.o pl-op.o pl-os.o pl-prims.o pl-pro.o pl-proc.o \
	pl-prof.o pl-read.o pl-rec.o pl-setup.o pl-sys.o pl-table.o \
	pl-trace.o pl-util.o pl-wic.o pl-write.o pl-term.o
DEPOBJ=	pl-main.o

PLINIT=	$(PB)/init.pl
PLBOOT=	$(PB)/syspred.pl $(PB)/toplevel.pl $(PB)/listing.pl \
	$(PB)/edit.pl $(PB)/sort.pl $(PB)/bags.pl $(PB)/apply.pl \
	$(PB)/list.pl $(PB)/writef.pl $(PB)/history.pl $(PB)/portray.pl \
	$(PB)/profile.pl $(PB)/dwim.pl $(PB)/$(FOREIGN) $(PB)/parms.pl \
	$(PB)/autoload.pl

SRC=	$(OBJ:.o=.c) $(DEPOBJ:.o=.c)

PARMS=	-DSYSTEMHOME=\"$(SYSTEMHOME)\" \
	-DDEFSTARTUP=\"$(DEFSTARTUP)\"

TARGETS=	$(EXPORTS) $(CINCLUDE) $(SYS) startup pl-bite

all:		xmakefile
		make -f xmakefile ${TARGETS}

startup:	$(STARTUPDIR) $(STARTUPPATH)


xmakefile:	Makefile md.h ../config.h
		@-rm -f junk.c
		@sed -f make.sed Makefile > junk.c
		@$(CPP) junk.c | \
		sed -e 's/^[ 	]*$$//' -e 's/^ /	/' \
		    -e 's/\(.*\)"\(.*\)"\(.*\)[ 	][ 	]*(delete-quotes)/\1\2\3/' | \
		sed -n -e '/^..*$$/p' > xmakefile
		@rm -f junk.c

$(SYS):		$(OBJ) $(DEPOBJ)
		$(CC) $(LDFLAGS) -o $(SYS) $(OBJ) $(DEPOBJ) $(LIBS)

$(STARTUPDIR):	
		-mkdir $@

$(STARTUPPATH):	$(PLBOOT) $(PLINIT) pl-wam.o
		./$(SYS) -O -o $(STARTUPPATH) -b $(PLINIT) -c $(PLBOOT)

$(EXPORTS):	xmakefile pl-export
		echo \#!$(BINDIR)/$(PROLOG) > $(EXPORTS)
		cat pl-export >> $(EXPORTS)

$(CINCLUDE):	pl-itf.h
		cp pl-itf.h $(CINCLUDE)


pl-main.o:	xmakefile pl-main.c pl-main.h
		$(CC) -c $(CFLAGS) $(PARMS) pl-main.c

pl-funct.o:	pl-funct.ih
pl-atom.o:	pl-funct.ih
pl-dump.o:	gnu/getpagesize.h $(UNEXEC)
pl-main.o:	Makefile pl-itf.h

pl-funct.ih:	ATOMS
		./defatom ATOMS

pl-atom.ih:	ATOMS
		./defatom ATOMS

pl-bite:	pl-bite.c
		$(CC) -o pl-bite pl-bite.c

install:	xmakefile
		make -f xmakefile xinstall

xinstall:
		rm -f $(BINDIR)/$(PROLOG)
#if O_STORE_PROGRAM && !O_INSTALL_DONOT_STORE
		./dump ./$(SYS) $(BINDIR)/$(PROLOG)
#else
#if O_INSTALL_USING_LN
		(me=`pwd`/$(SYS); cd $(BINDIR); ln -s $$me $(PROLOG))
#else
		$(INSTALL) $(SYS) $(BINDIR)/$(PROLOG)
#endif
#endif
		$(INSTALL) pl-bite $(BINDIR)/pl-bite
		chmod 755 $(BINDIR)/$(PROLOG) $(BINDIR)/pl-bite

tags:		TAGS

TAGS:		$(SRC)
		etags $(SRC)

clean:		xmakefile
		make -f xmakefile xclean

xclean:
		rm -f *.o a.out core makefile pl-bite
#if !O_INSTALL_USING_LN
		rm $(SYS)
#endif

# GCC is buggy on this one if you haven't ran `fixincludes' (see gcc
# distribution

#pl-os.o:	pl-os.c
#		cc -c -O pl-os.c

.c.s:
		$(CC) -S $(CFLAGS) $*.c

.c.o:
		$(CC) -c $(CFLAGS) $*.c

saber_src:
		#load $(OBJ) $(DEPOBJ) $(CFLAGS) $(LIBS)

# XPCE linking (incomplete; refer to the xpce directory)

XPCEBASE=	../../xpce
XBIN=		$(XPCEBASE)/MACHINE (delete-quotes)
ITF=		$(XPCEBASE)/pl/MACHINE/pl-pce.o (delete-quotes)
XLIBS=		$(LIBS) -lX11 -lXt

xpce:		xmakefile
		make -f xmakefile xxpce

pce-main.c:	pl-main.c
		ln -s pl-main.c pce-main.c

pce-main.o:	xmakefile pl-main.c pl-main.h
		$(CC) -c $(CFLAGS) $(PARMS) -DO_LINK_PCE pce-main.c

xxpce:		$(OBJ) pce-main.o
		(cd $(XPCEBASE)/src; ./make)
		cc -o $@ pce-main.o $(OBJ) $(ITF) \
		`awk '{print "$(XBIN)/" $$0}' $(XPCEBASE)/src/files` \
		$(XLIBS)
