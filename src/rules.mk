################################################################
# SWI-Prolog make include file for building SWI-Prolog on Windows
################################################################

# prefix=C:\Program Files
prefix=E:\jan\installed
PLBASE=$(prefix)\pl

# Define the packages to be installed automatically.  Note that the
# Makefile also checks whether the package directory exists.

PKGS=	table cpp clib sgml sgml\RDF
PKGDIR=..\packages

CC=cl.exe
LD=link.exe /nologo
AR=lib.exe
RSC=rc.exe
CMD=cmd.exe
LIBS=user32.lib shell32.lib gdi32.lib advapi32.lib wsock32.lib
ARCH=i386-win32
INSTALL=copy
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL)
MKDIR=mkdir
MAKE=nmake /nologo /f Makefile.mak

PLLIB=$(PLHOME)\lib\libpl.lib
TERMLIB=$(PLHOME)\lib\plterm.lib

CFLAGS=/MD /W3 /O2 /GX /DNDEBUG /DWIN32 /D_WINDOWS /nologo /c
LDFLAGS=

.c.obj:
	$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) $<

MAKEINDEX=chdir $(PLBASE) & bin\plcon.exe \
			-f none -F none \
			-g make_library_index(library) \
			-t halt

################################################################

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
