#   Makefile for SWI-Prolog <-> PCE interface
#
#   Copyright in the whole and every part of this software is owned  by  the
#   University  of  Amsterdam.   No reproduction of the whole or any part of
#   this software is to be made without the authority of the  University  of
#   Amsterdam.
#
#   Copyright (c) 1991, University of Amsterdam
# 

.SUFFIXES: .qlf .pl

XPCEOBJ=$(PCEHOME)/src/XPCE.a

PL=     pl
OBJ=	$(PLRUNTIME)/$(PLOBJ) interface.o link.o $(XPCEOBJ)
XPCE=	xpce$(RTSUFFIX)
QLF=    xpce$(RTSUFFIX).qlf
XLIBS=	-lXt -lX11
XPCESO= xpce4pl.$(SO)

CFLAGS= 	$(COFLAGS) $(CMFLAGS) -I$(PLINCLUDE) -I$(PCEHOME)/src
ITF=		../../prolog/c/interface.c ../../prolog/c/interface.h

PCEPLS=		pce_expand.pl english/pce_messages.pl pce_pl.pl \
		pce_principal.pl pce_error.pl pce_autoload.pl pce_global.pl \
		pce_operator.pl pce_compile.pl pce_editor.pl
BOOTQLF=	pce.qlf
LIBQLFS=	pce_manual.qlf dialog/dialog.qlf emacs/emacs.qlf pcedraw.qlf

nosoall:	$(QLF) pl-crt0.o

soall:		$(PLBASE)/plrc $(PLBASE)/xpce ../$(PLARCH)/$(XPCESO) \
		../$(PLARCH)/pl-crt0.o bootqlf libqlfs

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

$(XPCESO):	shitf.o $(XPCEOBJ) $(SOEXTRAOBJ)
		$(LD) $(LDSOFLAGS) shitf.o $(XPCEOBJ) $(SOEXTRAOBJ) $(GCCLIB) $(XLIBS) -o $@

libqlfs:	$(addprefix ../../prolog/lib/, $(LIBQLFS))
bootqlf:	$(addprefix ../../prolog/lib/, $(BOOTQLF))

../../prolog/lib/pce.qlf: $(addprefix ../../prolog/boot/, $(PCEPLS))
		$(PL) -g "qcompile('../../prolog/lib/pce.pl')" -t halt
		
.pl.qlf:
		$(PL) -g "pce_welcome,qcompile('$*')" -t halt

../$(PLARCH):
		mkdir $@

../$(PLARCH)/$(XPCESO): ../$(PLARCH) $(XPCESO)
		(cd ../$(PLARCH); rm -f $(XPCESO); ln -s ../src/$(XPCESO) .)

../$(PLARCH)/pl-crt0.o: ../$(PLARCH) pl-crt0.o
		(cd ../$(PLARCH); rm -f pl-crt0.o; ln -s ../src/pl-crt0.o .)

$(PLBASE)/plrc:	plrc
		cp plrc $@

$(PLBASE)/xpce:
		(cd $(PLBASE); rm -f xpce; ln -s $(PCEHOME) xpce)

mangle:		mangle.c
		$(CC) -O -o mangle mangle.c

domangle:	mangle mlist xpce.base
		./mangle mlist xpce.base
		touch domangle

clean:		
		$(RM) -f *.o core a.out shitf.c *~

realclean:	clean
		$(RM) -f xpce$(RTSUFFIX) *.qlf mangle domangle ctr0.o xpce4pl.*
		
