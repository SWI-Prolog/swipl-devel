################################################################
# Build the SWI-Prolog XML/SGML package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk
PKGDLL=db4pl

DBDIR=		e:\jan\src\db-4.0.14
LIBDB=		libdb40
DBLIB=		$(DBDIR)\build_win32\Release\$(LIBDB).lib
DBDLL=		$(DBDIR)\build_win32\Release\$(LIBDB).dll
DBDEFS=		-DHAVE_SET_RPC_SERVER

INCLUDE=$(INCLUDE);$(DBDIR)\build_win32

CFLAGS=$(CFLAGS) $(DBDEFS)

OBJ=		db4pl.obj atom.obj error.obj

all:		$(PKGDLL).dll

printenv::
		echo "%INCLUDE%"
		echo "%LIB%"

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(DBLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll $(PLBASE)\bin
		copy $(DBDLL) $(PLBASE)\bin
ilib::
		copy db.pl $(PLBASE)\library
		$(MAKEINDEX)

uninstall::
		del $(PLBASE)\bin\$(PKGDLL).dll
		del $(PLBASE)\bin\$(LIBDB).dll
		del $(PLBASE)\library\db.pl
		$(MAKEINDEX)

html-install::
		copy doc\db4pl.html $(PKGDOC)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

