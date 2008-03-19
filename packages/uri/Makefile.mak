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
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__

URIOBJ=		uri.obj

all:		uri.dll

uri.dll:	$(URIOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(URIOBJ) $(PLLIB)

uri.obj:	escape.c uri.c

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
		copy uri.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy uri.pdb "$(BINDIR)"
!ENDIF

ilib::
		copy uri.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\uri.dll"
		del "$(PLBASE)\library\uri.pl"
		$(MAKEINDEX)

html-install::
		copy uri.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


