################################################################
# SWI-Prolog make include file for building SWI-Prolog on Windows
################################################################

#prefix=C:\Program Files
prefix=E:\jan\installed
PLBASE=$(prefix)\pl

CC=cl.exe
LD=link.exe /nologo
AR=lib.exe
RSC=rc.exe
LIBS=user32.lib shell32.lib gdi32.lib advapi32.lib wsock32.lib
ARCH=i386-win32
INSTALL=copy
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL)
MKDIR=mkdir
MAKE=nmake /nologo /f Makefile.mak

CFLAGS=/MD /W3 /O2 /GX /DNDEBUG /DWIN32 /nologo /c
LDFLAGS=

.c.obj:
	$(CC) -I. -Irc -I ..\include $(CFLAGS) $<

################################################################

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
