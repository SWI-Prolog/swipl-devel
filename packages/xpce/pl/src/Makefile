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

OBJ=	$(PLRUNTIME)/$(PLOBJ) interface.o link.o $(XPCEOBJ)
BINOBJ= ../../pl/runtime/$(ARCH)/pl.o interface.o link.o $(XPCEOBJ)
TARGET=	xpce
BASE=xpce$(RTSUFFIX).base

CFLAGS= 	$(COFLAGS) -I$(PLINCLUDE) -I$(PCEHOME)/src
ITF=		../../prolog/c/interface.c ../../prolog/c/interface.h

all:		$(TARGET) pl-crt0.o 

interface.o:	$(ITF) interface.c

$(BASE):	$(OBJ)
		$(CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

printcmd:
		@echo $(CC) $(LDFLAGS) -o xpce.base \
			$(BINOBJ) $(LIBS) $(STATICLIBS)

$(TARGET):	$(BASE) domangle
		./dump ./$(BASE) $@

mangle:		mangle.c
		$(CC) -O -o mangle mangle.c

domangle:	mangle mlist xpce.base
		./mangle mlist xpce.base
		touch domangle

clean:		
		$(RM) -f *.o core a.out *~

realclean:	clean
		$(RM) -f xpce$(RTSUFFIX).base mangle domangle ctr0.o
		
