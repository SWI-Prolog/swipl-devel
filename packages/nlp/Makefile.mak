################################################################
# Build the SWI-Prolog nlp package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk

DMPOBJ=		double_metaphone.obj

all:		double_metaphone.dll

double_metaphone.dll:	$(DMPOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(DMPOBJ) $(PLLIB)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

idll::
		copy double_metaphone.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy double_metaphone.pdb "$(BINDIR)"
!ENDIF

ilib::
		copy double_metaphone.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\double_metaphone.dll"
		del "$(PLBASE)\library\double_metaphone.pl"
		$(MAKEINDEX)

html-install::
		copy nlp.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


