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
XPCE=	xpce$(RTSUFFIX)
QLF=    xpce$(RTSUFFIX).qlf
XLIBS=	-lXt -lX11

CFLAGS= 	$(COFLAGS) $(CMFLAGS) -I$(PLINCLUDE) -I$(PCEHOME)/src
ITF=		../../prolog/c/interface.c ../../prolog/c/interface.h

PLBOOT=		init.pl syspred.pl toplevel.pl listing.pl edit.pl sort.pl \
		bags.pl apply.pl list.pl writef.pl history.pl portray.pl \
		profile.pl dwim.pl foreign.pl parms.pl autoload.pl qlf.pl \
		topvars.pl
PLLIB=		quintus.pl
PCEBOOT=	pce_expand.pl english/pce_messages.pl pce_pl.pl \
		pce_principal.pl pce_error.pl pce_autoload.pl pe_global.pl \
		pce_operator.pl pce_compile.pl pce_editor.pl pce_boot.pl
PCELIB=		pce.pl
PLLOAD=		load.pl xpce.pl

all:		$(QLF) pl-crt0.o $(SHOBJ)

interface.o:	$(ITF) interface.c

$(XPCE):	$(OBJ)
		$(CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

$(QLF):		$(XPCE)
		./$(XPCE) -O -o $(QLF) \
			  -g pce_welcome \
			  -b $(PLBASE)/boot/init.pl \
			  -c load.pl

shitf.o:	interface.c
		cp interface.c shitf.c
		$(CC) -DO_SHAREDLIBRARY $(CFLAGS) -c shitf.c
		rm -f shitf.c
		
Initialize.o:	
		ar x $(XLIB)/libXt.a $@

xpce4pl.$(SO):	shitf.o $(XPCEOBJ) $(SOEXTRAOBJ)
		$(LD) shitf.o $(XPCEOBJ) $(SOEXTRAOBJ) $(GCCLIB) $(XLIBS) -o $@

mangle:		mangle.c
		$(CC) -O -o mangle mangle.c

domangle:	mangle mlist xpce.base
		./mangle mlist xpce.base
		touch domangle

clean:		
		$(RM) -f *.o core a.out shitf.c *~

realclean:	clean
		$(RM) -f xpce$(RTSUFFIX) *.qlf mangle domangle ctr0.o xpce4pl.*
		
