################################################################
# Build the SWI-Prolog tabling package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
PKGDLL=socket

OBJ=		socket.obj error.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll $(BINDIR)
ilib::
		copy socket.pl $(PLBASE)\library
		$(MAKEINDEX)

uninstall::
		del $(BINDIR)\$(PKGDLL).dll
		del $(PLBASE)\library\socket.pl
		$(MAKEINDEX)

html-install::
		copy clib.html $(PKGDOC)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

