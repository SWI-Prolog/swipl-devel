################################################################
# Install the SWI-Prolog PlDoc package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk

LIBDIR=		$(PLBASE)\library\pldoc
EXDIR=		$(PKGDOC)\examples\pldoc
LIBPL=		html.pl wiki.pl modes.pl register.pl process.pl
PUBPL=		pldoc.pl doc_http.pl
SUPPORT=	pldoc.css pldoc.js \
		edit.gif zoomin.gif zoomout.gif reload.gif favicon.ico
DOCALL=		$(LIBPL) $(SUPPORT)

all:		

!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		if not exist "$(LIBDIR)/$(NULL)" $(MKDIR) "$(LIBDIR)"
		@echo Copying $(LIBPL)
		@for %f in ($(DOCALL)) do @copy %f "$(LIBDIR)"
		copy README "$(LIBDIR)\README.TXT"
		@for %f in ($(PUBPL)) do @copy %f "$(PLBASE)\library"
		$(MAKEINDEX)
!ENDIF

html-install:	install-examples
		copy http.html "$(PKGDOC)"
		copy httpserver.gif "$(PKGDOC)"

pdf-install:	install-examples
		copy http.pdf "$(PKGDOC)"

install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"
		cd examples & copy $(EXAMPLEEXE) "$(EXDIR)"

xpce-install::

uninstall::
		cd $(LIBDIR) & del $(DOCALL) README.TXT
		cd $(PLBASE)\library & del $(PUBPL)
		$(MAKEINDEX)

clean::
		if exist *~ del *~

distclean:	clean


