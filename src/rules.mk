################################################################
# SWI-Prolog make include file for building SWI-Prolog on Windows
#
# This is the place to customise the build.  Notably define
# destination path, compiler, library search-path, etc.
################################################################

!include config.dat

# Setup the environment.  Use this to additional libraries and include
# files to the path.  In particular provide access to the jpeg and xpm
# libraries required to build XPCE

PATH=$(PATH);$(HOME)\$(TARGET_OS_ARCH)\bin;$(HOME)\bin;$(HOME)\$(TARGET_OS_ARCH)\lib;$(HOME)\lib
INCLUDE=$(PLHOME)\include;$(INCLUDE);$(HOME)\$(TARGET_OS_ARCH)\include;$(HOME)\include
LIB=$(LIB);$(HOME)\$(TARGET_OS_ARCH)\lib;$(HOME)\lib

# Installation target directory.  At the moment, the build will probably
# fail if there is whitespace in the $prefix directory.  You can however
# copy the result to wherever you want.

prefix=$(TARGET_PROGRAM_FILES)
PLBASE=$(TARGET_PROGRAM_FILES)\$(PL_DIR_NAME)
BINDIR=$(PLBASE)\bin
LIBDIR=$(PLBASE)\lib
INCDIR=$(PLBASE)\include
PLCUSTOM=$(PLBASE)\customize

# Get extra include files from here
EXTRAINCDIR=$(HOME)\$(TARGET_OS_ARCH)\include

# The OpenSSL library and include files
# http://www.slproweb.com/products/Win32OpenSSL.html
OPENSSL=$(TARGET_PROGRAM_FILES)\OpenSSL
OPENSSLLIBDIR=$(OPENSSL)\lib;$(OPENSSL)\lib\VC
OPENSSLINCDIR=$(OPENSSL)\include

# NullSoft installer
NSISDEFS=$(NSISDEFS) /DSWIPL=pl /DPTHREAD=$(LIBPTHREAD) /DZLIB=$(LIBZLIB) /DBOOT=$(PLBOOTFILE)

# Configuration selection

# CFG: dev=development environment; rt=runtime system only
# DBG: true=for C-level debugging; false=non-debugging
# MT:  true=multi-threading version; false=normal single-threading
# GMP: true=enable unbounded arithmetic using GNU GMP library
CFG=dev
DBG=false
MT=true
#GMP=true
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

PLPKG=chr clpqr http plunit pldoc R
PKGS=$(PLPKG) cpp odbc clib table sgml RDF semweb xpce nlp $(BUILD_ZLIB) $(BUILD_SSL) $(BUILD_JPL) $(BUILD_SPACE) protobufs windows PDT utf8proc archive cql

PKGDIR=$(PLHOME)\packages
PKGDOC=$(PLBASE)\doc\packages

# Define programs.  The setup here is for standard Microsoft MSVC tools
# on Windows-NT or Windows-2000

# If you are developing, choose /incremental:yes for LD.  It is a *lot*
# faster linking pl2xpce.dll from the XPCE package

CC=cl.exe
!IF "$(VC_VERSION)" == "VC8_OR_MORE"
VC8_OR_MORE_LFLAGS=/MACHINE:$(TARGET_OS_ARCH)
!ELSE
VC8_OR_MORE_LFLAGS=
!ENDIF
!IF "$(LNK)" == "inc"
LD=link.exe /nologo /incremental:yes $(VC8_OR_MORE_LFLAGS)
!ELSE
LD=link.exe /nologo $(VC8_OR_MORE_LFLAGS)
!ENDIF
AR=lib.exe
RSC=rc.exe
CMD=cmd.exe
INSTALL=copy
INSTALL_PROGRAM=$(INSTALL)
INSTALL_DATA=$(INSTALL)
MKDIR=mkdir
MAKE=nmake CFG="$(CFG)" DBG="$(DBG)" MT="$(MT)" MD="$(MD)" GMP="$(GMP)" /nologo /f Makefile.mak

LIBS=msvcprt.lib user32.lib shell32.lib gdi32.lib advapi32.lib ws2_32.lib ole32.lib $(EXTRALIBS) Dbghelp.lib
!if "$(MT)" == "true"
LIBS=$(LIBS) $(LIBPTHREAD).lib
!ENDIF

# Some libraries used by various packages

PLLIB=$(PLHOME)\lib\swipl.lib
TERMLIB=$(PLHOME)\lib\plterm.lib
UXLIB=$(PLHOME)\lib\uxnt.lib

!IF "$(VC_VERSION)" == "VC8_OR_MORE"
VC8_OR_MORE_CFLAGS=/D_$(MD) /D_CRT_SECURE_NO_WARNINGS /wd4996
!ELSE
VC8_OR_MORE_CFLAGS=
!ENDIF

CFLAGS=/MD /W3 $(SYMOPT) /EHsc /D__WINDOWS__ $(VC8_OR_MORE_CFLAGS) /D$(MD) /nologo /c

LDFLAGS=/DEBUG /NODEFAULTLIB:libcmt.lib

!IF "$(DBG)" == "false"
CFLAGS=/DNDEBUG /O2 $(CFLAGS)
D=
DBGLIBS=
!ELSE
CFLAGS=/D_DEBUG $(CFLAGS)
LDFLAGS=/incremental:yes $(LDFLAGS)
D=D
DBGLIBS=msvcrtd.lib
!ENDIF

!IF "$(MT)" == "true"
CFLAGS=/DO_PLMT /D_REENTRANT $(CFLAGS)
!ENDIF
!IF "$(GMP)" == "true"
CFLAGS=/DO_GMP $(CFLAGS)
GMPLIB=$(GMP_LIB)
!ELSE
GMPLIB=
!ENDIF

# Enable for serious debugging
# CFLAGS=/DO_DEBUG /DO_SECURE $(CFLAGS)

.c.obj:
	@$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<
.cxx.obj:
	@$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<

################################################################
# Used to update the library INDEX.pl after adding or deleting
# packages
################################################################

MAKEINDEX=chdir "$(PLBASE)" & del library\INDEX.pl & bin\swipl.exe \
			-f none -F none \
			-g make_library_index(library) \
			-t halt

PLCON=$(PLBASE)\bin\swipl.exe

################################################################
# Windows-versions garbage.  Most likely this won't work on Windows 98
# anyhow as we use constructs from cmd.exe such as FOR
################################################################

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF
