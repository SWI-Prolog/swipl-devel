################################################################
# Makefile for SWI-Prolog 3.x on MS-Windows
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
#	* make install-arch	Install machine dependent files
#	* make install-libs	Install machine independent files
#
# Copyright (C) University of Amsterdam, all rights reserved
# 
# Copyright policy:
#	
#	* GPL-2 (see file COPYING or http://www.gnu.org/)
#
#	* Licenses for use with proprietary software are available.
#	  See http://www.swi.pay.uva.nl/pub/SWI-Prolog/
#
################################################################

!include rules.mk

PL=pl
PLCON=..\bin\plcon.exe
PLWIN=..\bin\plwin.exe
PLLD=..\bin\plld.exe
PLRC=..\bin\plrc.exe
PLDLL=..\bin\libpl.dll
TERMDLL=..\bin\plterm.dll
PLLIB=..\lib\libpl.lib
TERMLIB=..\lib\plterm.lib

LOCALLIB=win32/uxnt/uxnt.lib rc/rc.lib

PB=..\boot
INCLUDEDIR=..\include
CINCLUDE=$(INCLUDEDIR)/SWI-Prolog.h
STREAMH=$(INCLUDEDIR)/SWI-Stream.h
BOOTFILE=boot32.prc
STARTUPPATH=..\$(BOOTFILE)
LIBRARYDIR=$(PLBASE)/library

OBJ=	pl-atom.obj pl-wam.obj pl-stream.obj pl-error.obj pl-arith.obj \
	pl-bag.obj pl-comp.obj pl-rc.obj pl-dwim.obj pl-ext.obj \
	pl-file.obj pl-flag.obj pl-fmt.obj pl-funct.obj pl-gc.obj \
	pl-glob.obj pl-itf.obj pl-list.obj pl-load.obj pl-modul.obj \
	pl-op.obj pl-os.obj pl-prims.obj pl-pro.obj pl-proc.obj \
	pl-prof.obj pl-read.obj pl-rec.obj pl-rl.obj pl-setup.obj \
	pl-sys.obj pl-table.obj pl-trace.obj pl-util.obj pl-wic.obj \
	pl-write.obj pl-term.obj pl-buffer.obj pl-thread.obj \
	pl-xterm.obj pl-feature.obj pl-ctype.obj pl-main.obj \
	pl-dde.obj pl-nt.obj

PLINIT=	$(PB)/init.pl

INCSRC=	pl-index.c pl-alloc.c pl-fli.c
SRC=	$(OBJ:.o=.c) $(DEPOBJ:.o=.c) $(EXT:.o=.c) $(INCSRC)
HDR=	config.h parms.h pl-buffer.h pl-ctype.h pl-incl.h pl-itf.h pl-main.h \
	pl-os.h pl-data.h

PLSRC=	../boot/syspred.pl ../boot/toplevel.pl ../boot/listing.pl \
	../boot/make.pl ../boot/sort.pl ../boot/bags.pl ../boot/apply.pl \
	../boot/list.pl ../boot/writef.pl ../boot/history.pl \
	../boot/profile.pl ../boot/dwim.pl ../boot/rc.pl \
	../boot/parms.pl ../boot/autoload.pl ../boot/qlf.pl \
	../boot/topvars.pl ../boot/messages.pl ../boot/load.pl
PLWINLIBS=	wise.pl dde.pl progman.pl registry.pl
PLLIBS= MANUAL helpidx.pl help.pl explain.pl \
	qsave.pl shlib.pl statistics.pl system.pl \
	backcomp.pl gensym.pl \
	bim.pl quintus.pl edinburgh.pl ctypes.pl files.pl \
	edit.pl emacs_interface.pl shell.pl check.pl \
	tty.pl readln.pl \
	am_match.pl oset.pl \
	netscape.pl url.pl \
	$(PLWINLIBS)

all:	banner \
	headers	swipl subdirs \
	$(PLCON) startup index $(PLWIN) $(PLLD)

system:		$(PLCON)
startup:	$(STARTUPPATH)
headers:	$(CINCLUDE) $(STREAMH)

banner:
		@echo ****************
		@echo Making SWI-Prolog $(PLVERSION) for $(ARCH)
		@echo To be installed in $(PLBASE)
		@echo ****************

$(PLLIB):	$(OBJ) $(LOCALLIB)
		$(LD) $(LDFLAGS) /dll /out:$(PLDLL) /implib:$@ $(OBJ) $(LOCALLIB) $(LIBS)

$(PLCON):	$(PLLIB) pl-ntcon.obj
		$(LD) $(LDFLAGS) /subsystem:console /out:$@ pl-ntcon.obj $(PLLIB)

$(PLWIN):	$(PLLIB) pl-ntmain.obj
		$(LD) $(LDFLAGS) /subsystem:windows /out:$@ pl-ntmain.obj $(PLLIB) $(TERMLIB) pl.res $(LIBS)

pl.res:		pl.rc pl.ico xpce.ico
		$(RSC) /fo$@ pl.rc

$(STARTUPPATH):	$(PLINIT) $(PLSRC) $(PLCON)
		$(PLCON) -O -o $(STARTUPPATH) -b $(PLINIT)

subdirs:
		chdir rc & $(MAKE)
		chdir win32\uxnt & $(MAKE)
		chdir win32\console & $(MAKE)

index:
		$(PLCON) -x $(STARTUPPATH) \
			-f none -F none \
			-g make_library_index('../library') \
			-t halt

$(INCLUDEDIR):
		if [ ! -d $@ ]; then $(MKDIR) $@; fi

$(CINCLUDE):	pl-itf.h $(INCLUDEDIR)
		copy $(srcdir)/pl-itf.h $(CINCLUDE)

$(STREAMH):	pl-stream.h $(INCLUDEDIR)
		copy pl-stream.h $@

pl-funct.obj:	pl-funct.ih
pl-atom.obj:	pl-funct.ih
pl-wam.obj:	pl-alloc.c pl-index.c pl-fli.c

# Use these if you have the awk program and want to add new reserved atoms
# to the C-code
#
#pl-funct.ih:	ATOMS
#		$(AWK) -f defatom.awk < ATOMS
#
#pl-atom.ih:	ATOMS
#		$(AWK) -f defatom.awk < ATOMS

$(PLLD):	plld.obj
		$(LD) /out:$@ /subsystem:console plld.obj $(LIBS)

tags:		TAGS

TAGS:		$(SRC)
		$(ETAGS) $(SRC) $(HDR)

swipl:
		echo . > $@

check:
		$(PLCON) -f test.pl -F none -g test,halt -t 'halt(1)'

################################################################
# Installation.  The default target is dv-install to install the
# normal development version
################################################################

install:	dv-install

dv-install:	install-arch install-libs

install-arch:	idirs
		$(INSTALL_PROGRAM) $(PLWIN) "$(PLBASE)\bin"
		$(INSTALL_PROGRAM) $(PLCON) "$(PLBASE)\bin"
		$(INSTALL_PROGRAM) $(PLDLL) "$(PLBASE)\bin"
		$(INSTALL_PROGRAM) $(PLLD)  "$(PLBASE)\bin"
		$(INSTALL_PROGRAM) $(PLRC)  "$(PLBASE)\bin"
		$(INSTALL_PROGRAM) $(TERMDLL) "$(PLBASE)\bin"
		$(INSTALL_DATA) $(PLLIB) "$(PLBASE)\lib"
		$(INSTALL_DATA) $(TERMLIB) "$(PLBASE)\lib"

install-libs:	idirs iinclude iboot ilib
		$(INSTALL_DATA) $(STARTUPPATH) "$(PLBASE)\$(BOOTFILE)"
		$(INSTALL_DATA) swipl $(PLBASE)
		chdir "$(PLBASE)\library" & \
		   $(PLCON) \
			-f none \
			-g make_library_index('.') \
			-t halt

IDIRS=		$(PLBASE)\bin $(PLBASE)\lib $(PLBASE)\include \
		$(PLBASE)\boot $(PLBASE)\library

$(IDIRS):
		if not exist "$@/$(NULL)" $(MKDIR) "$@"

idirs:		$(IDIRS)

iboot:		
		chdir ..\boot & copy *.pl "$(PLBASE)\boot"
ilib:		
		chdir ..\library & \
			for %f in ($(PLLIBS)) do copy %f "$(PLBASE)\library"

iinclude:       
		$(INSTALL_DATA) ..\include\SWI-Prolog.h "$(PLBASE)\include"
		$(INSTALL_DATA) ..\include\SWI-Stream.h "$(PLBASE)\include"

################################################################
# Cleanup
################################################################

clean:
		chdir rc & $(MAKE) clean
		chdir win32\uxnt & $(MAKE) clean
		chdir win32\console & $(MAKE) clean
		del *.obj *~ pl.res

distclean:	clean
		@cd rc & $(MAKE) distclean
		rm -rf $(INCLUDEDIR) $(RUNTIMEDIR)
		rm -f ../library/INDEX.pl
		rm -f swipl swiplbin pl.prc
		rm -f pl plld pl.1
		rm -f config.log config.cache

realclean:	clean
		rm -f $(STARTUPPATH)
		rm -f config.log config.cache
		rm -rf $(PL) ../startup ../include ../runtime

uninstall:
		rmdir /s /q $(PLBASE)
