#   Makefile for SWI-Prolog <-> PCE interface
#
#   Copyright in the whole and every part of this software is owned  by  the
#   University  of  Amsterdam.   No reproduction of the whole or any part of
#   this software is to be made without the authority of the  University  of
#   Amsterdam.
#
#   Copyright (c) 1991, University of Amsterdam
# 

################################################################
# NOTE NOTE NOTE
#
# This Makefile is normally started from the Makefile in the build-directory.
# The following parameters are passed into it:
#
# $(PL)		SWI-Prolog executable
# $(PLBASE)	SWI-Prolog's home directory
# $(PLARCH)	SWI-Prolog's architecture identifier
# $(builddir)	Path from XPCE home to build directory
# $(C?FLAGS)	The various C-compiler flags
# $(SOEXTRAOBJ)	Possibly non-standard .o files for creating xpce4pl.so
# $(GCCLIB)	The path to -lgcc
# $(SO)		Extension for shared libraries
################################################################

.SUFFIXES: .qlf .pl

libXPCE=	../../$(builddir)/libXPCE.a
LIBDIR=		../../$(builddir)/lib
BINDIR=		../../$(builddir)/bin
OBJDIR=		../../$(builddir)/pl

PL=     	pl
ITFOBJ= 	$(addprefix $(OBJDIR)/, interface.o link.o)
SOITF=		$(OBJDIR)/so-interface.o
OBJ=		$(PLRUNTIME)/$(PLOBJ) $(ITFOBJ) $(libXPCE)
XPCE=		$(BINDIR)/xpce$(RTSUFFIX)
QLF=    	$(BINDIR)/xpce$(RTSUFFIX).qlf
#XLIBS=		-lXt -lX11
XPCESO= 	$(LIBDIR)/xpce4pl.$(SO)
CRT0=   	$(LIBDIR)/pl-crt0.o
SOEXTR=		$(addprefix $(OBJDIR)/, $(SOEXTRAOBJ))

CFLAGS= 	$(COFLAGS) $(CMFLAGS) -I$(PLINCLUDE) -I../../src
ITF=		../../prolog/c/interface.c ../../prolog/c/interface.h
SOOBJ=		$(SOITF) $(libXPCE) $(SOEXTR) $(GCCLIB)

PCEPLS=		pce_expand.pl english/pce_messages.pl pce_pl.pl \
		pce_principal.pl pce_error.pl pce_autoload.pl pce_global.pl \
		pce_operator.pl pce_expansion.pl pce_realise.pl pce_editor.pl
BOOTQLF=	pce.qlf
LIBQLFS=	pce_manual.qlf dialog/dialog.qlf emacs/emacs.qlf pcedraw.qlf

################################################################
# The main targets
################################################################

nosoall:	$(QLF) $(CRT0)

soall:		$(PLBASE)/plrc $(PLBASE)/xpce $(XPCESO) $(CRT0) bootqlf

################################################################
# Objects for $(OBJDIR)
################################################################

$(OBJDIR)/interface.o:	interface.c $(ITF) 
		$(CC) -c $(CFLAGS) $< -o $@
$(OBJDIR)/link.o:	link.c
		$(CC) -c $(CFLAGS) $< -o $@
$(CRT0):	pl-crt0.c
		$(CC) -c $(CFLAGS) $< -o $@

################################################################
# Creating statically linked xpce and its xpce.qlf bootfile
################################################################

$(XPCE):	$(OBJ)
		$(CC) $(LDFLAGS) -o $@ $(OBJ) $(LIBS)

$(QLF):		$(XPCE)
		$(XPCE) -O -o $(QLF) \
			-g pce_welcome \
			-b $(PLBASE)/boot/init.pl \
			-c load.pl

################################################################$
# Creating xpce4pl.so
################################################################

$(SOITF):	interface.c
		$(CC) -DO_SHAREDLIBRARY $(CFLAGS) -c $< -o $@
		
$(OBJDIR)/Initialize.o:	
		ar x $(XLIB)/libXt.a $@

$(XPCESO):	$(SOITF) $(libXPCE) $(SOEXTR)
		$(LD) $(LDSOFLAGS) $(SOOBJ) $(XLIBS) -o $@

################################################################$
# QLF generation
################################################################

libqlfs:	$(addprefix ../../prolog/lib/, $(LIBQLFS))
bootqlf:	$(addprefix ../../prolog/lib/, $(BOOTQLF))

../../prolog/lib/pce.qlf: $(addprefix ../../prolog/boot/, $(PCEPLS))
		$(PL) -g "qcompile('../../prolog/lib/pce.pl')" -t halt
		
.pl.qlf:
		$(PL) -g "pce_welcome,qcompile('$*')" -t halt

################################################################
# Link xpce to the SWI-Prolog library directory
################################################################

$(PLBASE)/plrc:	plrc
		cp plrc $@

$(PLBASE)/xpce:
		(cd $(PLBASE); rm -f xpce; ln -s $(PCEHOME) xpce)

################################################################
# C++ Name mangling.  Not used right now
################################################################

mangle:		mangle.c
		$(CC) -O -o mangle mangle.c

domangle:	mangle mlist xpce.base
		./mangle mlist xpce.base
		touch domangle

################################################################
# Cleanup
################################################################

clean:		
		$(RM) -f *.o core a.out shitf.c *~

distclean:	clean
		$(RM) -f xpce$(RTSUFFIX) *.qlf mangle domangle ctr0.o xpce4pl.*
