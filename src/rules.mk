################################################################
# SWI-Prolog make include file for building SWI-Prolog on Windows
#
# This is the place to customise the build.  Notably define
# destination path, compiler, library search-path, etc.
################################################################

# Target architecture.  One of WIN32 or WIN64

MD=WIN32

# Installation target directory.  At the moment, the build will probably
# fail if there is whitespace in the $prefix directory.  You can however
# copy the result to wherever you want.

# prefix=C:\Program Files
#HOME=$(USERPROFILE)
prefix=$(HOME)\installed
PLBASE=$(prefix)\pl
BINDIR=$(PLBASE)\bin
LIBDIR=$(PLBASE)\lib
INCDIR=$(PLBASE)\include
PLCUSTOM=$(PLBASE)\custom

# We get pthreadVC.dll, pthreadVC.lib, pthread.h, sched.h and semaphore.h
# from the locations below
WINDLLDIR=$(WINDIR)\system32
PTHREADLIBDIR=$(HOME)\lib
PTHREADINCDIR=$(HOME)\include

# The OpenSSL library and include files
# http://www.slproweb.com/products/Win32OpenSSL.html
OPENSSL=C:\OpenSSL
OPENSSLLIBDIR=$(OPENSSL)\lib\VC
OPENSSLINCDIR=$(OPENSSL)\include

# Setup the environment.  Use this to additional libraries and include
# files to the path.  In particular provide access to the jpeg and xpm
# libraries required to build XPCE

INCLUDE=$(PLHOME)\include;$(INCLUDE);$(HOME)\include
LIB=$(LIB);$(HOME)\lib

# Configuration selection

# CFG: dev=development environment; rt=runtime system only
# DBG: true=for C-level debugging; false=non-debugging
# MT:  true=multi-threading version; false=normal single-threading
# GMP: true=enable unbounded arithmetic using GNU GMP library
CFG=dev
DBG=false
MT=true
GMP=true
PDB=false
SYMOPT=
SYMBOLS=true
DBGOPT=/Od
#DBGOPT=/O2

!IF "$(DBG)" == "true"
SYMOPT=/Zi
PDB=true
!ENDIF

!IF "$(SYMBOLS)" == "true"
SYMOPT=/Zi
PDB=true
!ENDIF

!IF "$(CFG)" == "rt"
CMFLAGS=/DO_RUNTIME
BINDIR=$(PLBASE)\runtime
!ENDIF

# Define the packages to be installed automatically.  Note that the
# Makefile also checks whether the package directory exists.

PKGS=	chr clpqr table cpp odbc clib sgml sgml\RDF semweb http \
	xpce jpl ssl nlp plunit pldoc zlib
PKGDIR=$(PLHOME)\packages
PKGDOC=$(PLBASE)\doc\packages

# Define programs.  The setup here is for standard Microsoft MSVC tools
# on Windows-NT or Windows-2000

# If you are developing, choose /incremental:yes for LD.  It is a *lot*
# faster linking pl2xpce.dll from the XPCE package

CC=cl.exe
!IF "$(LNK)" == "inc"
LD=link.exe /nologo /incremental:yes
!ELSE
LD=link.exe /nologo
!ENDIF
AR=lib.exe
RSC=rc.exe
CMD=cmd.exe
INSTALL=copy
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL)
MKDIR=mkdir
MAKE=nmake CFG="$(CFG)" DBG="$(DBG)" MT="$(MT)" MD="$(MD)" GMP="$(GMP)" /nologo /f Makefile.mak

LIBS= msvcprt.lib user32.lib shell32.lib gdi32.lib advapi32.lib wsock32.lib ole32.lib
!if "$(MT)" == "true"
!IF "$(MD)" == "WIN64"
LIBS=$(LIBS) pthreadVC2.lib
!ELSE
LIBS=$(LIBS) pthreadVC.lib
!ENDIF
!ENDIF

# Architecture identifier for Prolog's current_prolog_flag(arch, Arch)

!IF "$(MD)" == "WIN64"
ARCH=x64-win64
LIBS=$(LIBS) bufferoverflowU.lib
!ELSE
ARCH=i386-win32
!ENDIF

# Some libraries used by various packages

PLLIB=$(PLHOME)\lib\libpl.lib
TERMLIB=$(PLHOME)\lib\plterm.lib
UXLIB=$(PLHOME)\lib\uxnt.lib

CFLAGS=/MD /W3 $(SYMOPT) /EHsc /D__WINDOWS__ /D$(MD) /nologo /c

!IF "$(DBG)" == "false"
CFLAGS=/DNDEBUG $(CFLAGS)
LDFLAGS=
D=
DBGLIBS=
!ELSE
CFLAGS=/D_DEBUG $(CFLAGS)
LD=link.exe /nologo /incremental:yes
LDFLAGS=/DEBUG
D=D
DBGLIBS=msvcrtd.lib
!ENDIF

!IF "$(MT)" == "true"
CFLAGS=/DO_PLMT /D_REENTRANT $(CFLAGS)
!ENDIF
!IF "$(GMP)" == "true"
CFLAGS=/DO_GMP $(CFLAGS)
GMPLIB=gmp.lib
!ELSE
GMPLIB=
!ENDIF

# Enable for serious debugging
# CFLAGS=/DO_DEBUG /DO_SECURE $(CFLAGS)

.c.obj:
	$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<
.cxx.obj:
	@$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<

################################################################
# Used to update the library INDEX.pl after adding or deleing
# packages
################################################################

MAKEINDEX=chdir "$(PLBASE)" & del library\INDEX.pl & bin\plcon.exe \
			-f none -F none \
			-g make_library_index(library) \
			-t halt

PLCON=$(PLBASE)\bin\plcon.exe

################################################################
# Windows-versions garbage.  Most likely this won't work on Windows 98
# anyhow as we use constructs from cmd.exe such as FOR
################################################################

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
