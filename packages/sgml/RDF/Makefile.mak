################################################################
# Build the SWI-Prolog RDF package for MS-Windows
# NOTE: This package requires the SGML package
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..\..
!include $(PLHOME)\src\rules.mk

all:		

!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		copy rdf.pl "$(PLBASE)\library"
		copy rdf_parser.pl "$(PLBASE)\library"
		copy rdf_triple.pl "$(PLBASE)\library"
		copy rewrite.pl "$(PLBASE)\library"
		copy rdf_diagram.pl "$(PLBASE)\xpce\prolog\lib"
		$(MAKEINDEX)
!ENDIF

html-install::
		copy rdf2pl.html $(PKGDOC)

uninstall::
		del "$(PLBASE)\library\rdf.pl"
		del "$(PLBASE)\library\rdf_parser.pl"
		del "$(PLBASE)\library\rdf_triple.pl"
		del "$(PLBASE)\library\rewrite.pl"
		$(MAKEINDEX)

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

