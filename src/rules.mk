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
prefix=E:\jan\installed
PLBASE=$(prefix)\pl

# Setup the environment.  Uset this to additional libraries and include
# files to the path.  In particular provide access to the jpeg and xpm
# libraries required to build XPCE

INCLUDE=$(INCLUDE);E:\jan\include;..\include
LIB=$(LIB);E:\jan\lib

# Define the packages to be installed automatically.  Note that the
# Makefile also checks whether the package directory exists.

PKGS=	table cpp clib sgml sgml\RDF xpce
PKGDIR=..\packages
PKGDOC=$(PLBASE)\doc\packages

# Define programs.  The setup here is for standard Microsoft MSVC tools
# on Windows-NT or Windows-2000

CC=cl.exe
LD=link.exe /nologo
AR=lib.exe
RSC=rc.exe
CMD=cmd.exe
LIBS=user32.lib shell32.lib gdi32.lib advapi32.lib wsock32.lib
INSTALL=copy
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL)
MKDIR=mkdir
MAKE=nmake /nologo /f Makefile.mak

# Architecture identifier for Prolog's current_prolog_flag(arch, Arch)

ARCH=i386-win32

# Some libraries used by various packages

PLLIB=$(PLHOME)\lib\libpl.lib
TERMLIB=$(PLHOME)\lib\plterm.lib
UXLIB=$(PLHOME)\lib\uxnt.lib

CFLAGS=/MD /W3 /O2 /GX /DNDEBUG /DWIN32 /D_WINDOWS /nologo /c
LDFLAGS=

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
