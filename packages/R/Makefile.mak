################################################################
# Build the SWI-Prolog R package for MS-Windows
#
# Author: Jan Wielemaker
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
EXDIR=$(PKGDOC)\examples\R
EXAMPLES=r_demo.pl

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

install::
		copy R.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(PLBASE)\library\R.pl"
		$(MAKEINDEX)

html-install:	install-examples
		copy R.html "$(PKGDOC)"

install-examples::
		if not exist "$(EXDIR)\$(NULL)" $(MKDIR) "$(EXDIR)"
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"

xpce-install::

clean::
		if exist *~ del *~

distclean:	clean



