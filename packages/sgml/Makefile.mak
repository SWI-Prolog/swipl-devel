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
PKGDLL=sgml2pl

OBJ=		parser.obj util.obj charmap.obj catalog.obj \
		model.obj xmlns.obj utf8.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

install::
		copy $(PKGDLL).dll $(PLBASE)\bin
		copy sgml.pl $(PLBASE)\library
		$(MAKEINDEX)

uninstall::
		del $(PLBASE)\bin\$(PKGDLL).dll
		del $(PLBASE)\library\sgml.pl
		$(MAKEINDEX)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

