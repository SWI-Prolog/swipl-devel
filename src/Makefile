# Generated automatically from Makefile.in by configure.
################################################################
# Makefile for SWI-Prolog 3.x
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
# Public targets:
# 
#	* make			Simply makes all programs in the current tree
#	* make install		Installs the libraries and public executables
#	* make install-bins	Link executables from $(exec_prefix)/bin
#	* make install-arch	Install machine dependent files
#	* make install-libs	Install machine independent files
#
# Copyright (C) University of Amsterdam, all rights reserved
# 
# Copyright policy:
#	
#	* Free for personal, academic and internal prototyping purposes
#
#	* A licence is required for distributing commercial applications
#	  based on SWI-Prolog.  Contact prolog-request@swi.psy.uva.nl.
################################################################

PLVERSION=3.2.8

PL=pl
prefix=/staff/jan
exec_prefix=${prefix}
srcdir=.
vpath %.c .
bindir=$(exec_prefix)/bin
PLBASE=$(prefix)/lib/pl-$(PLVERSION)
man_prefix=$(prefix)/man
mansec=1
lmansec=3
mandir=$(man_prefix)/man$(mansec)
lmandir=$(man_prefix)/man$(lmansec)
plmandir=$(PLBASE)/man

CC=gcc
LIBS=-ldl -lreadline -lncurses -lm 
AWK=gawk
SED=sed
ETAGS=etags
AR=ar
RANLIB=ranlib
ARCH=i686-linux
INSTALL=/usr/bin/install -c
INSTALL_PROGRAM=${INSTALL}
INSTALL_DATA=${INSTALL} -m 644
MKDIR=mkdir -p

COFLAGS=-O2 -g
CWFLAGS=-Wall
CIFLAGS=
CMFLAGS=
CFLAGS=	$(CWFLAGS) $(COFLAGS) $(CIFLAGS) $(CMFLAGS)
LDFLAGS=-export-dynamic 

PLFOREIGN=

DEFSTARTUP=.plrc

PB=../boot
INCLUDEDIR=../include
EXPORTS=$(INCLUDEDIR)/SWI-Exports
CINCLUDE=$(INCLUDEDIR)/SWI-Prolog.h
STREAMH=$(INCLUDEDIR)/SWI-Stream.h
STUBC=$(INCLUDEDIR)/stub.c
STARTUPPATH=$(PL).prc
RUNTIMEDIR=../runtime/$(ARCH)
PLLIB=libpl.a
RUNTIME=$(RUNTIMEDIR)/$(PLLIB)
LIBRARYDIR=${PLBASE}/library

OBJ=	pl-atom.o pl-wam.o pl-stream.o pl-error.o pl-arith.o pl-bag.o \
	pl-comp.o pl-rc.o pl-dwim.o pl-ext.o pl-file.o pl-flag.o \
	pl-fmt.o pl-funct.o pl-gc.o pl-glob.o pl-itf.o pl-list.o \
	pl-load.o pl-modul.o pl-op.o pl-os.o pl-prims.o pl-pro.o \
	pl-proc.o pl-prof.o pl-read.o pl-rec.o pl-rl.o pl-setup.o \
	pl-sys.o pl-table.o pl-trace.o pl-util.o pl-wic.o pl-write.o \
	pl-term.o pl-buffer.o pl-thread.o   frame.o
RCOBJ=	rc/access.o rc/build.o rc/html.o rc/rc.o rc/util.o

DEPOBJ=	pl-main.o
EXT=	pl-extend.o
ALLOBJ= $(OBJ) $(DEPOBJ) $(EXT)

PLINIT=	$(PB)/init.pl

SRC=	$(OBJ:.o=.c) $(DEPOBJ:.o=.c) $(EXT:.o=.c) pl-index.c pl-alloc.c
HDR=	config.h parms.h pl-buffer.h pl-ctype.h pl-incl.h pl-itf.h pl-main.h \
	pl-os.h pl-data.h

PLSRC=	../boot/syspred.pl ../boot/toplevel.pl ../boot/listing.pl \
	../boot/make.pl ../boot/sort.pl ../boot/bags.pl ../boot/apply.pl \
	../boot/list.pl ../boot/writef.pl ../boot/history.pl \
	../boot/profile.pl ../boot/dwim.pl ../boot/rc.pl \
	../boot/foreign.pl ../boot/parms.pl ../boot/autoload.pl \
	../boot/qlf.pl ../boot/topvars.pl ../boot/messages.pl

all:	banner \
	headers rc-lib system startup index runtime \
	pl-bite plld \
	swipl swiplbin

.c.o:
	$(CC) -c -I. -I$(srcdir) -I$(srcdir)/rc $(CFLAGS) $< -o $@

system:		$(PL)
startup:	$(STARTUPPATH)
headers:	parms.h $(EXPORTS) $(CINCLUDE) $(STREAMH) $(STUBC)
runtime:	$(RUNTIME)

banner:
		@echo "****************"
		@echo "Making SWI-Prolog $(PLVERSION) for $(ARCH)"
		@echo "To be installed in $(exec_prefix)/bin/$(PL)"
		@echo "Home (libraries) in $(PLBASE)"
		@echo "****************"


$(PL):		$(ALLOBJ)
		$(CC) $(LDFLAGS) -o $(PL) -Lrc $(ALLOBJ) -lrc $(LIBS)


$(STARTUPPATH):	$(PLINIT) $(PLSRC) pl-wam.o pl-wic.o
		./$(PL) -O -o $(STARTUPPATH) -b $(PLINIT)


$(RUNTIME):	$(OBJ) $(RCOBJ) $(DEPOBJ)
		if [ ! -d ../runtime ]; then $(MKDIR) ../runtime; fi
		if [ ! -d $(RUNTIMEDIR) ]; then $(MKDIR) $(RUNTIMEDIR); fi
		rm -f $@
		$(AR) r $@ $(OBJ) $(RCOBJ) $(DEPOBJ)
		$(RANLIB) $@

rc-lib:
		@(cd rc && $(MAKE))

index:
		./$(PL) -x $(STARTUPPATH) \
			-f none -F none \
			-g make_library_index\(\'../library\'\) \
			-t halt

$(INCLUDEDIR):
		if [ ! -d $@ ]; then $(MKDIR) $@; fi

$(EXPORTS):	Makefile $(srcdir)/pl-itf.h $(INCLUDEDIR)
		echo \#\!$(bindir)/$(PL) > $(EXPORTS)
		grep '^__pl_export' $(srcdir)/pl-itf.h | \
			$(SED) 's/.*\(PL_[a-zA-Z0-9_]*\).*/\1/' | \
			grep -v '_t$$' | \
			sort -u >> $(EXPORTS)

$(CINCLUDE):	$(srcdir)/pl-itf.h $(INCLUDEDIR)
		cp $(srcdir)/pl-itf.h $(CINCLUDE)

$(STREAMH):	$(srcdir)/pl-stream.h $(INCLUDEDIR)
		cp $(srcdir)/pl-stream.h $@

$(STUBC):	$(srcdir)/pl-extend.c $(INCLUDEDIR)
		$(SED) 's/"pl-itf.h"/<SWI-Prolog.h>/' $(srcdir)/pl-extend.c > $@

parms.h:	Makefile
		@echo "#define PLHOME       \"$(PLBASE)\""      > $@%
		@echo "#define DEFSTARTUP   \"$(DEFSTARTUP)\"" >> $@%
		@echo $(PLVERSION) | \
		$(AWK) -F. '{ printf("#define PLVERSION %d\n", $$1 * 10000 + $$2 * 100 + $$3); }' >> $@%
		@echo "#define ARCH	    \"$(ARCH)\""       >> $@%
		@echo "#define C_LIBS	    \"$(LIBS)\""       >> $@%
		@echo "#define C_CC	    \"$(CC)\""	       >> $@%
		@echo "#define C_LDFLAGS    \"$(LDFLAGS)\""    >> $@%
		@if cmp -s $@% $@; then rm $@%; else mv $@% $@; fi

pl-main.o:	parms.h
pl-setup.o:	parms.h
pl-wic.o:	parms.h
pl-funct.o:	$(srcdir)/pl-funct.ih
pl-atom.o:	$(srcdir)/pl-funct.ih
pl-wam.o:	pl-alloc.c pl-index.c pl-fli.c

$(srcdir)/pl-funct.ih:	$(srcdir)/ATOMS
		cd $(srcdir) ; $(AWK) -f defatom.awk < ATOMS

$(srcdir)/pl-atom.ih:	$(srcdir)/ATOMS
		cd $(srcdir) ; $(AWK) -f defatom.awk < ATOMS

# Hack to deal with __register_frame problems in different glibc 2 versions
# (Linux, compiled on SuSE 6.0, glibc 2.0.7, egcs 1.1.1)

frame.o:
		ar x `$(CC) -print-libgcc-file-name` $@

pl-bite:	$(srcdir)/pl-bite.c
		$(CC) -I. -I$(srcdir) $(CFLAGS) -o $@ $(srcdir)/pl-bite.c

plld:		$(srcdir)/plld.c
		$(CC) -I. -I$(srcdir) $(CFLAGS) -o $@ $(srcdir)/plld.c

tags:		TAGS

TAGS:		$(SRC)
		$(ETAGS) $(SRC) $(HDR)

swipl:
		echo "." > $@
swiplbin:	
		echo ".." > $@

pl-funcs.h:	$(SRC)
		mkproto -p $(SRC) > $@

install:	dv-install

dv-install:	install-arch install-libs install-bins

install-bins:	install-arch
		-if [ ! -d $(bindir)  ]; then $(MKDIR) $(bindir); fi
		for f in $(PL) pl-bite plld plrc; do \
		    ( cd $(bindir); rm -f $$f; ln -s $(PLBASE)/bin/$(ARCH)/$$f . ); \
		done


install-arch:	idirs iruntime
		for f in $(PL) pl-bite plld rc/plrc; do \
		    $(INSTALL_PROGRAM) $$f $(PLBASE)/bin/$(ARCH); \
		done

install-libs:	idirs iboot ilib iruntime iinclude iman
		$(INSTALL_DATA) $(STARTUPPATH) $(PLBASE)/boot.prc
		$(INSTALL_DATA) swipl $(PLBASE)
		$(INSTALL_DATA) swiplbin $(PLBASE)/bin/swipl
		$(INSTALL_DATA) README.bin $(PLBASE)
		$(INSTALL_DATA) ../LICENSE $(PLBASE)
		cd $(PLBASE)/library; \
		   ../bin/$(ARCH)/$(PL) \
			-f none \
			-g make_library_index\('.'\) \
			-t halt

installdirs=	bin bin/$(ARCH) lib lib/$(ARCH) man boot library \
		include runtime runtime/$(ARCH)

iruntime:
		if [ ! -d $(PLBASE)/runtime/$(ARCH) ]; then \
			$(MKDIR) $(PLBASE)/runtime/$(ARCH); \
		fi
		if [ -f $(RUNTIME) ]; then \
			$(INSTALL_DATA) $(RUNTIME) $(PLBASE)/runtime/$(ARCH); \
		fi

idirs:
		-if [ ! -d $(prefix)/lib  ];    then $(MKDIR) $(prefix)/lib;     fi
		-if [ ! -d $(PLBASE)  ];    then $(MKDIR) $(PLBASE); 	fi
		for d in $(installdirs); do \
			if [ ! -d $(PLBASE)/$$d ]; then \
				$(MKDIR) $(PLBASE)/$$d; \
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
		if [ -d $(prefix)/include -a -w $(prefix)/include ]; then \
		    $(INSTALL_DATA) ../include/SWI-Prolog.h $(prefix)/include; \
		fi

iman:		
		$(INSTALL_DATA) pl.1 $(plmandir)/$(PL).$(mansec)
		$(INSTALL_DATA) $(srcdir)/plrc.1 $(plmandir)/plrc.$(mansec)
		$(INSTALL_DATA) $(srcdir)/plld.1 $(plmandir)/plld.$(mansec)
		$(INSTALL_DATA) $(srcdir)/readline.3 $(plmandir)/readline.$(lmansec)
		-if [ ! -d $(man_prefix) ]; then $(MKDIR) $(man_prefix); fi
		-if [ ! -d $(mandir)  ];    then $(MKDIR) $(mandir);     fi
		-$(INSTALL_DATA) $(plmandir)/$(PL).$(mansec) $(mandir)
		-$(INSTALL_DATA) $(plmandir)/plrc.$(mansec) $(mandir)
		-$(INSTALL_DATA) $(plmandir)/plld.$(mansec) $(mandir)

install_rlman:
		-if [ ! -d $(lmandir) ]; then $(MKDIR) $(lmandir); fi
		-if [ ! -r $(lmandir)/readline.$(lmansec) ]; then \
		  $(INSTALL_DATA) $(srcdir)/readline.3 $(lmandir)/readline.$(lmansec); \
		fi

#
# runtime-only environment installation
#

RTHOME=		$(prefix)/lib/rt-pl-$(PLVERSION)
RTDIRS=		$(RTHOME) $(RTHOME)/bin $(RTHOME)/lib $(RTHOME)/lib/$(ARCH) \
		$(RTHOME)/man $(RTHOME)/bin/$(ARCH)

rt-install:
		@echo "****************"
		@echo "Creating runtime environment in $(RTHOME)"
		@echo "****************"
		@for d in $(RTDIRS); do \
		    if [ ! -d $$d ]; then \
			echo $(MKDIR) $$d; \
			$(MKDIR) $$d; \
		    fi; \
		done
		$(INSTALL_PROGRAM) $(PL) $(RTHOME)/bin/$(ARCH)/pl
		$(INSTALL_DATA) $(srcdir)/pl.1 $(RTHOME)/man/pl.1
		$(INSTALL_DATA) swipl $(RTHOME)/swipl
		$(INSTALL_DATA) swiplbin $(RTHOME)/bin/swipl
		$(INSTALL_DATA) $(srcdir)/README.RT $(RTHOME)
#		-cd $(bindir); rm -f plrt; ln -s $(RTHOME)/bin/$(ARCH)/pl plrt

#
# Cleanup
#

clean:
		@(cd rc && $(MAKE) clean)
		rm -f *.o a.out core *~ #* parms.h

distclean:	clean
		@(cd rc && $(MAKE) distclean)
		rm -f pl plld pl-bite pl.1
		rm -f config.log config.cache

realclean:	clean
		rm -f $(STARTUPPATH)
		rm -f config.log config.cache
		rm -rf $(PL) pl-bite ../startup ../include ../runtime

