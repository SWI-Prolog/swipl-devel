################################################################
# SWI-Prolog make include file for building SWI-Prolog on Windows
#
# This is the place to customise the build.  Notably define
# destination path, compiler, library search-path, etc.
################################################################

# Installation target directory.  At the moment, the build will probably
# fail if there is whitespace in the $prefix directory.  You can however
# copy the result to wherever you want.

# prefix=C:\Program Files
prefix=E:\jan\src\mt\installed
PLBASE=$(prefix)\pl
BINDIR=$(PLBASE)\bin
LIBDIR=$(PLBASE)\lib
INCDIR=$(PLBASE)\include
PLCUSTOM=$(PLBASE)\custom

# Setup the environment.  Use this to additional libraries and include
# files to the path.  In particular provide access to the jpeg and xpm
# libraries required to build XPCE

INCLUDE=$(PLHOME)\include;$(INCLUDE);E:\jan\include
LIB=$(LIB);E:\jan\lib

# Configuration selection

# CFG: dev=development environment; rt=runtime system only
# DBG: true=for C-level debugging; false=non-debugging
# MT:  true=multi-threading version; false=normal single-threading
CFG=dev
DBG=true
MT=true

!IF "$(CFG)" == "rt"
CMFLAGS=/DO_RUNTIME
BINDIR=$(PLBASE)\runtime
!ENDIF

# Define the packages to be installed automatically.  Note that the
# Makefile also checks whether the package directory exists.

PKGS=	table cpp odbc clib sgml sgml\RDF xpce
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
MAKE=nmake CFG="$(CFG)" DBG="$(DBG)" /nologo /f Makefile.mak

LIBS=user32.lib shell32.lib gdi32.lib advapi32.lib wsock32.lib
!if "$(MT)" == "true"
LIBS=$(LIBS) pthreadVC.lib
!ENDIF

# Architecture identifier for Prolog's current_prolog_flag(arch, Arch)

ARCH=i386-win32

# Some libraries used by various packages

PLLIB=$(PLHOME)\lib\libpl.lib
TERMLIB=$(PLHOME)\lib\plterm.lib
UXLIB=$(PLHOME)\lib\uxnt.lib

!IF "$(DBG)" == "false"
CFLAGS=/MD /W3 /O2 /GX /DNDEBUG /DWIN32 /D_WINDOWS $(CMFLAGS) /nologo /c
LDFLAGS=
D=
DBGLIBS=
!ELSE
CFLAGS=/MD /W3 /Zi /Od /GX /D_DEBUG /DWIN32 /D_WINDOWS $(CMFLAGS) /nologo /c
LD=link.exe /nologo /incremental:yes
LDFLAGS=/DEBUG
D=D
DBGLIBS=msvcrtd.lib
!ENDIF

!IF "$(MT)" == "true"
CFLAGS=/DO_PLMT /D_REENTRANT $(CFLAGS)
!ENDIF

.c.obj:
	@$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<
.cxx.obj:
	@$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<

################################################################
# Used to update the library INDEX.pl after adding or deleing
# packages
################################################################

MAKEINDEX=chdir $(PLBASE) & del library\INDEX.pl & bin\plcon.exe \
			-f none -F none \
			-g make_library_index(library) \
			-t halt

################################################################
# Windows-versions garbage.  Most likely this won't work on Windows 98
# anyhow as we use constructs from cmd.exe such as FOR
################################################################

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
