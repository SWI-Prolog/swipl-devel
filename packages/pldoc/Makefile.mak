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
		doc_library.pl hooks.pl doc_htmlsrc.pl doc_colour.pl \
		doc_util.pl doc_access.pl
PUBPL=		pldoc.pl doc_http.pl doc_latex.pl
SUPPORT=	pldoc.css pldoc.js pllisting.css pldoc.sty \
		edit.gif zoomin.gif zoomout.gif reload.gif favicon.ico \
		up.gif source.gif
DOCALL=		$(LIBPL) $(SUPPORT)
EXAMPLES=	README
EXAMPLEEXE=	man_server.pl

all:		pldoc.sty

pldoc.sty:	..\..\man\pl.sty
		copy ..\..\man\pl.sty $@

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
		cd server & copy $(EXAMPLEEXE).in "$(EXDIR)\$(EXAMPLEEXE)"

xpce-install::

uninstall::
		cd $(LIBDIR) & del $(DOCALL) README.TXT
		cd $(PLBASE)\library & del $(PUBPL)
		$(MAKEINDEX)

clean::
		if exist *~ del *~

distclean:	clean


