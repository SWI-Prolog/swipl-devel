#   Makefile for SWI-Prolog <-> PCE interface
#
#   Copyright in the whole and every part of this software is owned  by  the
#   University  of  Amsterdam.   No reproduction of the whole or any part of
#   this software is to be made without the authority of the  University  of
#   Amsterdam.
#
#   Copyright (c) 1991, University of Amsterdam
# 

XPCEOBJ=$(PCEHOME)/src/XPCE.a

OBJ=	$(PLRUNTIME)/pl.o interface.o link.o $(XPCEOBJ)
BINOBJ= ../../pl/runtime/$(ARCH)/pl.o interface.o link.o $(XPCEOBJ)
TARGET=	xpce

CFLAGS= 	$(COFLAGS) -I$(PLINCLUDE) -I$(PCEHOME)/src
ITF=		../../prolog/c/interface.c ../../prolog/c/interface.h

all:		$(TARGET) pl-crt0.o 

interface.o:	$(ITF) interface.c

xpce.base:	$(OBJ)
		$(CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS) $(STATICLIBS)

printcmd:
		@echo $(CC) $(LDFLAGS) -o xpce.base \
			$(BINOBJ) $(LIBS) $(STATICLIBS)

$(TARGET):	xpce.base domangle
		./dump ./xpce.base $@

mangle:		mangle.c
		$(CC) -O -o mangle mangle.c

domangle:	mangle mlist xpce.base
		./mangle mlist xpce.base
		touch domangle

clean:		
		rm -f *.o core a.out *~

realclean:	clean
		rm -f xpce.base mangle domangle ctr0.o
		
