# Generated automatically from Makefile.in by configure.
################################################################
# Makefile for SWI-Prolog 1.9
# $Id$
#
# Author:	Jan Wielemaker
#		jan@swi.psy.uva.nl
#		SWI
#		University of Amsterdam
#    		Roetersstraat 15
#		1018 WB  Amsterdam
#		The Netherlands
#
# @configure-input@
#
################################################################

PLVERSION=2.1.2

PL=pl
prefix=/staff/jan
exec_prefix=${prefix}
bindir=$(exec_prefix)/bin
PLBASE=$(prefix)/lib/pl-$(PLVERSION)
mansec=1
mandir=$(prefix)/man/man$(mansec)

CC=gcc
LIBS=-ldl -lreadline -ltermcap -lm 
STATICLIBS=/usr/lib/libc.a
AWK=nawk
SED=sed
LD_R=ld -r
ETAGS=etags
ARCH=sparc-sunos4.1.3
INSTALL=/usr/gnu/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644

COFLAGS=-O2
CWFLAGS=
CIFLAGS=
CMFLAGS=-funsigned-char
CFLAGS=	$(CWFLAGS) $(COFLAGS) $(CIFLAGS) $(CMFLAGS)
LDFLAGS=

PLFOREIGN=foreign.pl

DEFSTARTUP=.plrc

PB=../boot
INCLUDEDIR=../include
EXPORTS=$(INCLUDEDIR)/SWI-Exports
CINCLUDE=$(INCLUDEDIR)/SWI-Prolog.h
STREAMH=$(INCLUDEDIR)/SWI-Stream.h
STUBC=$(INCLUDEDIR)/stub.c
STARTUPPATH=$(PL).qlf
RUNTIMEDIR=../runtime/$(ARCH)
RUNTIME=$(RUNTIMEDIR)/$(PL).o
LIBRARYDIR=${PLBASE}/library


OBJ=	pl-wam.o pl-save.o pl-stream.o \
	pl-atom.o pl-arith.o pl-bag.o pl-comp.o pl-dump.o \
	pl-dwim.o pl-ext.o pl-file.o pl-flag.o pl-fmt.o pl-funct.o \
	pl-gc.o pl-glob.o pl-itf.o pl-list.o pl-load.o \
	pl-modul.o pl-op.o pl-os.o pl-prims.o pl-pro.o pl-proc.o \
	pl-prof.o pl-read.o pl-rec.o pl-setup.o pl-sys.o pl-table.o \
	pl-trace.o pl-util.o pl-wic.o pl-write.o pl-term.o pl-buffer.o
DEPOBJ=	pl-main.o
EXT=	pl-extend.o
ALLOBJ= $(OBJ) $(DEPOBJ) $(EXT)

PLINIT=	$(PB)/init.pl

SRC=	$(OBJ:.o=.c) $(DEPOBJ:.o=.c) $(EXT:.o=.c) pl-index.c pl-alloc.c
HDR=	config.h parms.h pl-buffer.h pl-ctype.h pl-incl.h pl-itf.h pl-main.h \
	pl-os.h pl-save.h

all:	banner headers system startup index runtime pl-bite

system:		$(PL)
startup:	$(STARTUPPATH)
headers:	parms.h $(EXPORTS) $(CINCLUDE) $(STREAMH) $(STUBC)
runtime:	$(RUNTIME)

banner:
		@echo "****************"
		@echo "Making SWI-Prolog $(PLVERSION) for $(ARCH)"
		@echo "To be installed in $(prefix)/bin/$(PL)"
		@echo "Home (libraries) in $(PLBASE)"
		@echo "****************"

$(PL):		$(ALLOBJ)
		$(CC) $(LDFLAGS) -o $(PL) $(ALLOBJ) $(LIBS) $(STATICLIBS) 


$(STARTUPPATH):	$(PLINIT) pl-wam.o pl-wic.o
		./$(PL) -O -o $(STARTUPPATH) -b $(PLINIT)


$(RUNTIME):	$(OBJ) $(DEPOBJ)
		if [ ! -d ../runtime ]; then mkdir ../runtime; fi
		if [ ! -d $(RUNTIMEDIR) ]; then mkdir $(RUNTIMEDIR); fi
		$(LD_R) -o $@ $(OBJ) $(DEPOBJ)

index:
		./$(PL) -x $(STARTUPPATH) \
			-f none \
			-g make_library_index\(\'../library\'\) \
			-t halt

$(INCLUDEDIR):
		if [ ! -d $@ ]; then mkdir $@; fi

$(EXPORTS):	Makefile pl-itf.h $(INCLUDEDIR)
		echo \#\!$(bindir)/$(PL) > $(EXPORTS)
		grep '^__pl_export' pl-itf.h | \
			$(SED) 's/.*\(PL_[a-zA-Z0-9_]*\).*/\1/' | \
			grep -v '_t$$' | \
			sort -u >> $(EXPORTS)

$(CINCLUDE):	pl-itf.h $(INCLUDEDIR)
		cp pl-itf.h $(CINCLUDE)

$(STREAMH):	pl-stream.h $(INCLUDEDIR)
		cp pl-stream.h $@

$(STUBC):	pl-extend.c $(INCLUDEDIR)
		$(SED) 's/"pl-itf.h"/<SWI-Prolog.h>/' pl-extend.c > $@
		

parms.h:	Makefile
		@echo "#define PLHOME       \"$(PLBASE)\""      > %$@%
		@echo "#define DEFSTARTUP   \"$(DEFSTARTUP)\"" >> %$@%
		@echo "#define PLVERSION    \"$(PLVERSION)\""  >> %$@%
		@echo "#define ARCH	    \"$(ARCH)\""       >> %$@%
		@echo "#define C_LIBS	    \"$(LIBS)\""       >> %$@%
		@echo "#define C_STATICLIBS \"$(STATICLIBS)\"" >> %$@%
		@echo "#define C_CC	    \"$(CC)\""	       >> %$@%
		@echo "#define C_LDFLAGS    \"$(LDFLAGS)\""    >> %$@%
		@if cmp -s %$@% $@; then rm %$@%; else mv %$@% $@; fi

pl-main.o:	parms.h
pl-setup.o:	parms.h
pl-funct.o:	pl-funct.ih
pl-atom.o:	pl-funct.ih
pl-wam.o:	pl-alloc.c pl-index.c

pl-funct.ih:	ATOMS
		$(AWK) -f defatom.awk < ATOMS

pl-atom.ih:	ATOMS
		$(AWK) -f defatom.awk < ATOMS

pl-bite:	pl-bite.c
		$(CC) $(CFLAGS) -o pl-bite pl-bite.c

tags:		TAGS

TAGS:		$(SRC)
		$(ETAGS) $(SRC) $(HDR)
		
pl-funcs.h:	$(SRC)
		mkproto -p $(SRC) > $@

install:	idirs iboot ilib iinclude iruntime iman
		$(INSTALL_DATA) $(STARTUPPATH) $(PLBASE)/startup/startup
		echo "." > swipl
		$(INSTALL_DATA) swipl $(PLBASE)
		$(INSTALL_DATA) README.bin $(PLBASE)
		$(INSTALL_DATA) ../LICENCE $(PLBASE)
		if [ -r $(bindir)/$(PL) ]; then \
			mv $(bindir)/$(PL) $(bindir)/$(PL).old; \
		fi
		$(INSTALL_PROGRAM) $(PL) $(bindir)
		$(INSTALL_PROGRAM) pl-bite $(bindir)
		cd $(PLBASE)/library; \
		   $(bindir)/$(PL) \
			-f none \
			-g make_library_index\('.'\) \
			-t halt

installdirs=	boot library startup include runtime runtime/$(ARCH)

iruntime:
		if [ ! -d $(PLBASE)/runtime/$(ARCH) ]; then \
			mkdir $(PLBASE)/runtime/$(ARCH); \
		fi
		if [ -f ../runtime/$(ARCH)/$(PL).o ]; then \
			$(INSTALL_DATA) ../runtime/$(ARCH)/$(PL).o \
					$(PLBASE)/runtime/$(ARCH); \
		fi
		$(INSTALL_DATA) pl-extend.c $(PLBASE)/runtime

idirs:
		-mkdir $(PLBASE)
		for d in $(installdirs); do \
			if [ ! -d $(PLBASE)/$$d ]; then \
				mkdir $(PLBASE)/$$d; \
			fi; \
		done

	
iboot:		
		cd ../boot; \
		for f in *.pl; do $(INSTALL_DATA) $$f $(PLBASE)/boot; done
ilib:		
		cd ../library; \
		for f in *.pl MANUAL; do \
			$(INSTALL_DATA) $$f $(PLBASE)/library; \
		done

iinclude:       
		cd ../include; \
		for f in SWI-Prolog.h SWI-Exports SWI-Stream.h stub.c; do \
		    if [ -f $$f ]; then \
			$(INSTALL_DATA) $$f $(PLBASE)/include; \
		    fi; \
		done
		$(INSTALL_DATA) ../include/SWI-Prolog.h $(prefix)/include

iman:
		-$(SED) 's/PL/$(PL)/' pl.1 > $(mandir)/$(PL).$(mansec)
 
#
# Cleanup
#

clean:
		rm -f *.o a.out core *~ #* parms.h

distclean:	clean
		rm -f config.log config.cache

realclean:	clean
		rm -f $(STARTUPPATH)
		rm -f config.log config.cache
		rm -rf $(PL) pl-bite ../startup ../include ../runtime
