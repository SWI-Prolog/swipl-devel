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
PKGDLL=table

OBJ=		table.obj order.obj error.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

install::
		copy $(PKGDLL).dll $(PLBASE)\bin
		copy table.pl $(PLBASE)\library
		copy table_util.pl $(PLBASE)\library
		$(MAKEINDEX)

html-install::
		copy table.html $(PKGDOC)

uninstall::
		del $(PLBASE)\bin\$(PKGDLL).dll
		del $(PLBASE)\library\table.pl
		del $(PLBASE)\library\table_util.pl
		$(MAKEINDEX)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

