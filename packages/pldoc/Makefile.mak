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
LIBPL=		doc_html.pl doc_wiki.pl doc_modes.pl doc_register.pl \
		doc_process.pl doc_index.pl doc_search.pl doc_man.pl \
		doc_library.pl hooks.pl
PUBPL=		pldoc.pl doc_http.pl
SUPPORT=	pldoc.css pldoc.js \
		edit.gif zoomin.gif zoomout.gif reload.gif favicon.ico \
		up.gif
DOCALL=		$(LIBPL) $(SUPPORT)
EXAMPLES=	doc_log.pl README
EXAMPLEEXE=	man_server.pl

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
		copy pldoc.html "$(PKGDOC)"

pdf-install:	install-examples
		copy pldoc.pdf "$(PKGDOC)"

install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		cd server & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"
		cd server & copy $(EXAMPLEEXE) "$(EXDIR)"

xpce-install::

uninstall::
		cd $(LIBDIR) & del $(DOCALL) README.TXT
		cd $(PLBASE)\library & del $(PUBPL)
		$(MAKEINDEX)

clean::
		if exist *~ del *~

distclean:	clean


