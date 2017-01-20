################################################################
# Makefile for SWI-Prolog on MS-Windows
#
# Author:			Jan Wielemaker
#			     J.Wielemaker@cs.vu.nl
#		University of Amsterdam  VU University Amsterdam
#		Kruislaan 419		 De Boelelaan 181a
#		1098 VA  Amsterdam	 1081 HV Amsterdam
#			       The Netherlands
#
# Public targets:
#
#	* make			Simply makes all programs in the current tree
#	* make install		Installs the libraries and public executables
#	* make install-arch	Install machine dependent files
#	* make install-libs	Install machine independent files
#
# Copyright (C) University of Amsterdam
#
# Copyright policy:
#
#	* LGPL (see file COPYING or http://www.gnu.org/)
################################################################

# NOTE: The Unix/GCC versions use profile-based optimization.  This is
# also available for MSVC2005, but not for the Express version. It is
# called `POGO'. See
# http://blogs.msdn.com/vcblog/archive/2008/11/12/pogo.aspx

STACK=4000000

PLHOME=..
!include rules.mk
!include common.mk

PL=pl
PLCON=$(PLHOME)\bin\swipl.exe
PLWIN=$(PLHOME)\bin\swipl-win.exe
PLLD=$(PLHOME)\bin\swipl-ld.exe
PLRC=$(PLHOME)\bin\swipl-rc.exe
PLDLL=$(PLHOME)\bin\swipl.dll
TERMDLL=$(PLHOME)\bin\plterm.dll
OUTDIRS=$(PLHOME)\bin $(PLHOME)\lib $(PLHOME)\include

LOCALLIB=$(UXLIB) rc/rc.lib libtai/tai.lib

PB=$(PLHOME)\boot
INCLUDEDIR=$(PLHOME)\include
CINCLUDE=$(INCLUDEDIR)\SWI-Prolog.h
STREAMH=$(INCLUDEDIR)\SWI-Stream.h
STARTUPPATH=$(PLHOME)\$(PLBOOTFILE)
LIBRARYDIR=$(PLBASE)\library

OBJ=	$(OBJ:.o=.obj) $(OSOBJ:.o=.obj) pl-nt.obj pl-ntconsole.obj pl-dde.obj

PLINIT=	$(PB)/init.pl

INCSRC=	pl-index.c pl-alloc.c pl-fli.c
SRC=	$(OBJ:.o=.c) $(DEPOBJ:.o=.c) $(EXT:.o=.c) $(INCSRC)
HDR=	config.h parms.h pl-buffer.h pl-ctype.h pl-incl.h SWI-Prolog.h \
	pl-main.h pl-os.h pl-data.h
VMI=	pl-jumptable.ic pl-codetable.c pl-vmi.h

PLSRC=$(PLSRC) ../boot/menu.pl
PLWINLIBS=dde.pl progman.pl win_menu.pl
PLLIBS=$(PLLIBS) $(PLWINLIBS)
CLP=	bounds.pl clp_events.pl clp_distinct.pl simplex.pl clpfd.pl clpb.pl
UNICODE=blocks.pl unicode_data.pl
MANDIR= "$(PLBASE)\doc\Manual"

all:	lite packages

remake-all: distclean all install

lite:	banner \
	headers	swipl.home subdirs vmi \
	$(PLCON) startup index $(PLWIN) $(PLLD)

plcon:	$(PLCON)
plwin:	$(PLWIN)

system:		$(PLCON)
startup:	$(STARTUPPATH)
headers:	$(CINCLUDE) $(STREAMH)

banner:
		@echo ****************
		@echo Making SWI-Prolog $(PLVERSION) for $(PLARCH)
		@echo To be installed in $(PLBASE)
!IF "$(DBG)" == "true"
		@echo *** Compiling version for DEBUGGING
!ENDIF
!IF "$(MT)" == "true"
		@echo *** Building MULTI-Threading version
!ENDIF
		@echo ****************

$(PLLIB):	$(OBJ) $(LOCALLIB)
		$(LD) $(LDFLAGS) /dll /out:$(PLDLL) /implib:$@ $(OBJ) $(LOCALLIB) $(GMPLIB) $(LIBS) winmm.lib $(DBGLIBS)

# We first create swipl.exe  to  avoid   overriding  the  debug files of
# swipl.dll. Maybe using the same name for a   dll and exe is a bad idea
# afterall?

$(PLCON):	$(PLLIB) pl-main.obj
		$(LD) $(LDFLAGS) /subsystem:console /out:plcon.exe pl-main.obj $(PLLIB)
		editbin /stack:$(STACK) plcon.exe
		copy plcon.exe $@
		if exist plcon.exe.manifest copy plcon.exe.manifest $@.manifest

$(PLWIN):	$(PLLIB) pl-ntmain.obj swipl.res
		$(LD) $(LDFLAGS) /subsystem:windows /out:$@ pl-ntmain.obj $(PLLIB) $(TERMLIB) swipl.res $(LIBS)
		editbin /stack:$(STACK) $(PLWIN)

swipl.res:	swipl.rc swipl.ico
		$(RSC) /fo$@ swipl.rc

$(STARTUPPATH):	$(PLINIT) $(PLSRC) $(PLCON)
		$(PLCON) -O -o $(STARTUPPATH) -b $(PLINIT)

$(OUTDIRS):
		if not exist "$@/$(NULL)" $(MKDIR) "$@"

subdirs:	$(OUTDIRS)
		chdir os\windows & $(MAKE)
		chdir win32\console & $(MAKE)
		chdir rc & $(MAKE)
		chdir libtai & $(MAKE)

index:
		$(PLCON) -x $(STARTUPPATH) \
			-f none -F none \
			-g make_library_index('../library') \
			-t halt

$(CINCLUDE):	$(OUTDIRS) SWI-Prolog.h
		copy SWI-Prolog.h $@

$(STREAMH):	os\SWI-Stream.h $(INCLUDEDIR)
		copy os\SWI-Stream.h $@

$(OBJ):		pl-vmi.h
pl-funct.obj:	pl-funct.ih
pl-atom.obj:	pl-funct.ih
pl-wam.obj:	pl-vmi.c pl-alloc.c pl-index.c pl-fli.c pl-jumptable.ic
pl-prims.obj:	pl-termwalk.c
pl-rec.obj:	pl-termwalk.c
pl-stream.obj:	popen.c
pl-dtoa.obj:	dtoa.c
pl-arith.obj:	pl-segstack.h

# this should be pl-vmi.h, but that causes a recompile of everything.
# Seems NMAKE dependency computation is broken ...
vmi:		pl-vmi.c mkvmi.exe
		mkvmi.exe
		echo "ok" > vmi

pl-funct.ih:	ATOMS defatom.exe
		defatom.exe

pl-atom.ih:	ATOMS defatom.exe
		defatom.exe

defatom.exe:	defatom.obj
		$(LD) /out:$@ /subsystem:console defatom.obj $(LIBS)

mkvmi.exe:	mkvmi.obj
		$(LD) /out:$@ /subsystem:console mkvmi.obj $(LIBS)

$(PLLD):	swipl-ld.obj
		$(LD) /out:$@ /subsystem:console swipl-ld.obj $(LIBS) $(UXLIB)

tags:		TAGS

TAGS:		$(SRC)
		$(ETAGS) $(SRC) $(HDR)

swipl.home:
		echo . > $@

check:
		$(PLCON) -f test.pl -F none -g test,halt -t halt(1)

################################################################
# Installation.
################################################################

install::	install-arch install-libs install-readme install_packages \
		xpce_packages install-custom install-demo html-install

embed-manifests::
		win32\embed_manifests.cmd

install-arch:	idirs iprog
		$(INSTALL_PROGRAM) $(PLLD)  "$(BINDIR)"
		$(INSTALL_PROGRAM) $(PLRC)  "$(BINDIR)"
		$(INSTALL_DATA) $(PLLIB) "$(LIBDIR)"
		$(INSTALL_DATA) $(TERMLIB) "$(LIBDIR)"

iprog:		embed-manifests
		$(INSTALL_PROGRAM) $(PLWIN) "$(BINDIR)"
		$(INSTALL_PROGRAM) $(PLCON) "$(BINDIR)"
		$(INSTALL_PROGRAM) $(PLDLL) "$(BINDIR)"
		$(INSTALL_PROGRAM) $(TERMDLL) "$(BINDIR)"
!IF "$(PDB)" == "true"
		$(INSTALL_PROGRAM) ..\bin\swipl.pdb "$(BINDIR)"
		$(INSTALL_PROGRAM) ..\bin\swipl-win.pdb "$(BINDIR)"
		$(INSTALL_PROGRAM) ..\bin\swipl.pdb "$(BINDIR)"
		$(INSTALL_PROGRAM) ..\bin\plterm.pdb "$(BINDIR)"
!ENDIF
!IF "$(MT)" == "true"
		@echo Installing pthreadVC.dll
		$(INSTALL_PROGRAM) "$(EXTRALIBDIR)\$(LIBPTHREAD).dll" "$(BINDIR)"
		$(INSTALL_DATA) "$(EXTRALIBDIR)\$(LIBPTHREAD).lib" "$(LIBDIR)"
!ENDIF
!IF "$(MSVCRT)" != ""
		@echo Adding MSVC runtime
		$(INSTALL_PROGRAM) "$(MSVCRTDIR)\$(MSVCRT)" "$(BINDIR)"
!ENDIF

install-libs:	idirs iinclude iboot ilib
		$(INSTALL_DATA) $(STARTUPPATH) "$(PLBASE)\$(BOOTFILE)"
		$(INSTALL_DATA) swipl.home "$(PLBASE)"
		chdir "$(PLBASE)\library" & \
		   $(PLCON) \
			-f none \
			-g make_library_index('.') \
			-t halt

install-demo:	idirs
		$(INSTALL_DATA) ..\demo\likes.pl "$(PLBASE)\demo"
		$(INSTALL_DATA) ..\demo\README "$(PLBASE)\demo\README.TXT"

IDIRS=		"$(BINDIR)" "$(LIBDIR)" "$(PLBASE)\include" \
		"$(PLBASE)\include\sicstus" \
		"$(PLBASE)\include\Yap" \
		"$(PLBASE)\boot" "$(PLBASE)\library" "$(PKGDOC)" \
		"$(PLCUSTOM)" "$(PLBASE)\demo" "$(PLBASE)\library\clp" \
		"$(PLBASE)\library\dialect" "$(PLBASE)\library\dialect\yap" \
		"$(PLBASE)\library\dialect\swi" \
		"$(PLBASE)\library\dialect\iso" \
		"$(PLBASE)\library\dialect\sicstus" \
		"$(PLBASE)\library\unicode" \
		"$(PLBASE)\library\dcg" \
		$(MANDIR)

$(IDIRS):
		if not exist $@/$(NULL) $(MKDIR) $@

idirs:		$(IDIRS)

iboot:
		chdir $(PLHOME)\boot & copy *.pl "$(PLBASE)\boot"
		copy win32\misc\mkboot.bat "$(PLBASE)\bin\mkboot.bat"

ilib:		iclp idcg idialect iswi iyap isicstus iiso iunicode
		chdir $(PLHOME)\library & \
			for %f in ($(PLLIBS)) do copy %f "$(PLBASE)\library"

iclp::
		chdir $(PLHOME)\library\clp & \
			for %f in ($(CLP)) do copy %f "$(PLBASE)\library\clp"

idcg::
		chdir $(PLHOME)\library\dcg & \
			for %f in ($(DCG)) do copy %f "$(PLBASE)\library\dcg"

idialect:	iyap
		chdir $(PLHOME)\library\dialect & \
			for %f in ($(DIALECT)) do copy %f "$(PLBASE)\library\dialect"

iswi::
		chdir $(PLHOME)\library\dialect\swi & \
			for %f in ($(SWI)) do copy %f "$(PLBASE)\library\dialect\swi"

iyap::
		chdir $(PLHOME)\library\dialect\yap & \
			for %f in ($(YAP)) do copy %f "$(PLBASE)\library\dialect\yap"
		copy compat\YapInterface.h "$(PLBASE)\include\Yap\YapInterface.h"

isicstus::
		chdir $(PLHOME)\library\dialect\sicstus & \
			for %f in ($(SICSTUS)) do copy %f "$(PLBASE)\library\dialect\sicstus"
		copy compat\sicstus.h "$(PLBASE)\include\sicstus\sicstus.h"

iiso::
		chdir $(PLHOME)\library\dialect\iso & \
			for %f in ($(ISO)) do copy %f "$(PLBASE)\library\dialect\iso"

iunicode::
		chdir $(PLHOME)\library\unicode & \
		  for %f in ($(UNICODE)) do copy %f "$(PLBASE)\library\unicode"

iinclude:
		$(INSTALL_DATA) $(PLHOME)\include\SWI-Prolog.h "$(PLBASE)\include"
		$(INSTALL_DATA) $(PLHOME)\include\SWI-Stream.h "$(PLBASE)\include"
		$(INSTALL_DATA) $(PLHOME)\include\console.h "$(PLBASE)\include\plterm.h"
!IF "$(MT)" == "true"
		$(INSTALL_DATA) "$(EXTRAINCDIR)\pthread.h" "$(PLBASE)\include"
		$(INSTALL_DATA) "$(EXTRAINCDIR)\sched.h" "$(PLBASE)\include"
		$(INSTALL_DATA) "$(EXTRAINCDIR)\semaphore.h" "$(PLBASE)\include"
!ENDIF

install-readme::
		$(INSTALL_DATA) ..\README "$(PLBASE)\README.TXT"
		$(INSTALL_DATA) ..\VERSION "$(PLBASE)"
		$(INSTALL_DATA) swipl.ico "$(PLBASE)"
		$(INSTALL_DATA) ..\COPYING "$(PLBASE)\COPYING.TXT"
		$(INSTALL_DATA) ..\man\windows.html "$(PLBASE)\doc"

install-custom::
		$(INSTALL_DATA) ..\customize\dotswiplrc "$(PLCUSTOM)\swipl.ini"
		$(INSTALL_DATA) ..\customize\dotxpcerc "$(PLCUSTOM)\xpce.ini"
		$(INSTALL_DATA) ..\customize\README "$(PLCUSTOM)\README.TXT"

html-install::
		copy ..\man\Manual\*.html $(MANDIR) > nul
		copy ..\man\Manual\*.gif $(MANDIR) > nul


################################################################
# INSTALLER
################################################################

installer::
		$(INSTALL_DATA) win32\installer\options.ini "$(PLBASE)\.."
		$(INSTALL_DATA) win32\installer\pl.nsi "$(PLBASE)\.."
		$(INSTALL_DATA) win32\installer\mkinstaller.pl "$(PLBASE)\.."
		"$(NSIS)" $(NSISDEFS) "$(PLBASE)\..\pl.nsi"


################################################################
# Build and install packages
################################################################

packages:
		@for %p in ($(PKGS)) do \
		   @if exist "$(PKGDIR)\%p" \
		      $(CMD) /c "echo PACKAGE %p ... & chdir $(PKGDIR)\%p & $(MAKE)" || exit 1

install_packages:
		@for %p in ($(PKGS)) do \
		   @if exist "$(PKGDIR)\%p" \
		      $(CMD) /c "chdir $(PKGDIR)\%p & $(MAKE) install" || exit 1
!IF "$(CFG)" == "dev"
		@for %p in ($(PKGS)) do \
		   if exist "$(PKGDIR)\%p" \
		      $(CMD) /c "chdir $(PKGDIR)\%p & $(MAKE) html-install" || exit 1
		if exist $(PKGDIR)\index.html \
		    copy $(PKGDIR)\index.html "$(PKGDOC)"
!ENDIF

xpce_packages:
		@for %p in ($(PKGS)) do \
		   @if exist "$(PKGDIR)\%p" \
		      $(CMD) /c "chdir $(PKGDIR)\%p & $(MAKE) xpce-install" || exit 1

clean_packages:
		for %p in ($(PKGS)) do \
		   if exist "$(PKGDIR)\%p" \
		      $(CMD) /c "chdir $(PKGDIR)\%p & $(MAKE) clean" || exit 1

distclean_packages:
		for %p in ($(PKGS)) do \
		   if exist "$(PKGDIR)\%p" \
		      $(CMD) /c "chdir $(PKGDIR)\%p & $(MAKE) distclean" || exit 1


################################################################
# Quick common actions during development
################################################################

pce-dll::
		$(CMD) /c "chdir $(PKGDIR)\xpce\src & $(MAKE) idll"
clib-install::
		$(CMD) /c "chdir $(PKGDIR)\clib & $(MAKE) install"
odbc-install:
		$(CMD) /c "chdir $(PKGDIR)\odbc & $(MAKE) install"


################################################################
# Redistributable Requirements .cab files
################################################################

# To update the reqs-N-BITS.cab, do this:
#
# Extract the old one, update the files and update files.txt
# Then run
#
# cabarc.exe -p n reqs-N-BITS.cab @files.txt

################################################################
# Cleanup
################################################################

clean:		clean_packages
		chdir rc & $(MAKE) clean
		chdir libtai & $(MAKE) clean
		chdir os\windows & $(MAKE) clean
		chdir win32\console & $(MAKE) clean
		-del *.manifest *.obj os\*.obj *~ pl.res vmi 2>nul

distclean:	clean distclean_packages
		@chdir rc & $(MAKE) distclean
		@chdir libtai & $(MAKE) distclean
		-del ..\bin\*.exe ..\bin\*.dll ..\bin\*.pdb 2>nul
		-del ..\library\INDEX.pl 2>nul
		-del swipl.home swiplbin 2>nul

realclean:	clean
		del $(STARTUPPATH)

uninstall:
		rmdir /s /q $(PLBASE)

