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
!include ..\..\src\rules.mk
PKGDLL=socket

OBJ=		socket.obj error.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

install::
		copy $(PKGDLL).dll $(PLBASE)\bin
		copy socket.pl $(PLBASE)\library
		$(MAKEINDEX)

uninstall::
		del $(PLBASE)\bin\$(PKGDLL).dll
		del $(PLBASE)\library\socket.pl
		$(MAKEINDEX)

html-install::
		copy clib.html $(PKGDOC)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

