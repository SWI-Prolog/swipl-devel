################################################################
# Build the SWI-Prolog semantic web package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk

LIBDIR=		$(PLBASE)\library\semweb
PKGDLL=rdf_db

LIBPL=		rdf_db.pl rdfs.pl rdf_edit.pl
DATA=		rdfs.rdfs dc.rdfs eor.rdfs
OBJ=		rdf_db.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll "$(BINDIR)"
ilib::
		if not exist "$(LIBDIR)/$(NULL)" $(MKDIR) "$(LIBDIR)"
		@echo Copying $(LIBPL)
		@for %f in ($(LIBPL)) do @copy %f "$(LIBDIR)"
		@for %f in ($(DATA)) do @copy %f "$(LIBDIR)"
		copy README "$(LIBDIR)\README.TXT"
		$(MAKEINDEX)

html-install::
		copy semweb.html "$(PKGDOC)"
		copy modules.gif "$(PKGDOC)"
pdf-install:	
		copy semweb.pdf "$(PKGDOC)"

xpce-install::

uninstall::
		del "$(PLBASE)\bin\$(PKGDLL).dll"
		cd $(LIBDIR) & del $(LIBPL) $(DATA) README.TXT
		rmdir $(LIBDIR)
		$(MAKEINDEX)

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.pdb *.ilk 2>nul

