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
# $(SOEXTRAOBJ)	Possibly non-standard .o files for creating pl2xpce.so
# $(GCCLIB)	The path to -lgcc
# $(SO)		Extension for shared libraries
################################################################

.SUFFIXES: .qlf .pl

libXPCE=	../../$(builddir)/libXPCE.a
LIBDIR=		../../$(builddir)
BINDIR=		../../$(builddir)
OBJDIR=		../../$(builddir)/pl

CPLIFLAGS=	-I$(PLINCLUDE) -I../../$(builddir) -I../../src
PL=     	pl
ITFOBJ= 	$(addprefix $(OBJDIR)/, interface.o link.o)
SOITF=		$(OBJDIR)/so-interface.o
OBJ=		$(ITFOBJ) -L$(PLRUNTIME) -lpl
XPCE=		$(BINDIR)/xpce
QLF=    	$(BINDIR)/xpce.qlf
#XLIBS=		-lXt -lX11
XPCESO= 	$(LIBDIR)/pl2xpce.$(SO)
SOEXTR=		$(addprefix $(OBJDIR)/, $(SOEXTRAOBJ))

CFLAGS= 	$(COFLAGS) $(CMFLAGS) $(CPLIFLAGS)

PCEPLS=		pce_expand.pl ../lib/english/pce_messages.pl pce_pl.pl \
		pce_principal.pl pce_error.pl pce_autoload.pl pce_global.pl \
		pce_operator.pl pce_expansion.pl pce_realise.pl pce_editor.pl
BOOTQLF=	pce.qlf
LIBQLFS=	pce_manual.qlf dialog/dialog.qlf emacs/emacs.qlf pcedraw.qlf

################################################################
# The main targets
################################################################

nosoall:	$(PLBASE)/pl.rc $(PLBASE)/xpce.rc $(PLBASE)/xpce \
		$(PLBASE)/Makefile $(XPCE)

soall:		$(PLBASE)/pl.rc $(PLBASE)/xpce.rc $(PLBASE)/xpce \
		$(PLBASE)/Makefile $(PLBASE)/xpce/lib/$(PLARCH) \
		$(XPCESO) bootqlf

rt-soall:	$(XPCESO)
rt-nosoall:	$(XPCE)

################################################################
# Objects for $(OBJDIR)
################################################################

$(OBJDIR)/interface.o:	interface.c
		$(CC) -c $(CFLAGS) -DSWI $< -o $@
$(OBJDIR)/link.o:	link.c
		$(CC) -c $(CFLAGS) $< -o $@

################################################################
# Creating statically linked xpce and its xpce.qlf bootfile
################################################################

$(XPCE):	$(ITFOBJ) $(libXPCE)
		$(CC) $(LDFLAGS) -o $@ $(OBJ) -L$(LIBDIR) -lXPCE $(LIBS)
		echo $(PLBASE) > ../../swipl

$(QLF):		$(XPCE)
		$(XPCE) -O -o $(QLF) \
			-g pce_welcome \
			-b $(PLBASE)/boot/init.pl \
			-c load.pl

################################################################$
# Creating pl2xpce.so
################################################################

$(SOITF):	interface.c
		$(CC) -DO_SHAREDLIBRARY $(CFLAGS) -DSWI -c $< -o $@
		
$(OBJDIR)/Initialize.o:	
		ar x $(XLIB)/libXt.a $@

$(XPCESO):	$(SOITF) $(libXPCE) $(SOEXTR)
		@if [ -r $(LIBDIR)/libXPCE.$(SO) ]; then \
		    $(MAKE) sopce$(XPCESO); \
		else \
		    $(MAKE) axpce$(XPCESO); \
		fi

sopce$(XPCESO):
		LD_RUN_PATH="$(libdir):$$LD_RUN_PATH"; \
		export LD_RUN_PATH; \
		$(LD) $(LDSOFLAGS) $(SOITF) -L$(LIBDIR) -lXPCE -o $(XPCESO);
axpce$(XPCESO):
		LD_RUN_PATH="$(libdir):$$LD_RUN_PATH"; \
		export LD_RUN_PATH; \
		$(LD) $(LDSOFLAGS) $(SOITF) $(SOEXTR) -L$(LIBDIR) \
		-lXPCE $(GCCLIB) $(XLIBS) $(NETLIBS) -o $(XPCESO);

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

$(PLBASE)/pl.rc:	plrc
	cp plrc $@

$(PLBASE)/xpce.rc:	plrc
	cp plrc $@

$(PLBASE)/xpce:
	(cd $(PLBASE); rm -f xpce; ln -s $(PCEHOME) xpce)

$(PLBASE)/xpce/lib/$(PLARCH):
	(cd ../../lib; rm -f $(PLARCH); ln -s ../$(builddir) $(PLARCH))

$(PLBASE)/Makefile: Makefile.bin
	-cp Makefile.bin $@

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
		$(RM) -f xpce *.qlf mangle domangle ctr0.o *.so
