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

LIBOBJ=		parser.obj util.obj charmap.obj catalog.obj \
		model.obj xmlns.obj utf8.obj
OBJ=		$(LIBOBJ) sgml2pl.obj error.obj quote.obj
SGMLOBJ=	$(LIBOBJ) sgml.obj
DTDFILES=	HTML4.dcl HTML4.dtd HTML4.soc \
		HTMLlat1.ent HTMLspec.ent HTMLsym.ent
DTDDIR=		$(PLBASE)\library\DTD

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

sgml.exe:	$(SGMLOBJ)
		$(LD) $(LDFLAGS) /out:$@ $(SGMLOBJ) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idtd idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll "$(PLBASE)\bin"
ilib::
		copy sgml.pl "$(PLBASE)\library"
		$(MAKEINDEX)

idtd::
		@if not exist "$(DTDDIR)\$(NULL)" $(MKDIR) "$(DTDDIR)"
		@echo "Installing DTD files in $(DTDDIR)"
		@for %f in ($(DTDFILES)) do \
		   @copy DTD\%f "$(DTDDIR)"
		@echo "done"

uninstall::
		del "$(PLBASE)\bin\$(PKGDLL).dll"
		del "$(PLBASE)\library\sgml.pl"
		$(MAKEINDEX)

html-install::
		copy doc\sgml2pl.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.dbg 2>nul

