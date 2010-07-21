################################################################
# Install the SWI-Prolog HTTP package for MS-Windows
#
# Author: Jan Wielemaker
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
!include common.mk

LIBDIR=		$(PLBASE)\library\http
EXDIR=		$(PKGDOC)\examples\http

OBJ=		http_stream.obj

all:		http_stream.dll json.dll

http_stream.dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)
json.dll:	json.obj
		$(LD) /dll /out:$@ $(LDFLAGS) json.obj $(PLLIB) $(LIBS)

http_stream.obj:	http_error.c http_chunked.c cgi_stream.c stream_range.c

all:

!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		if not exist "$(LIBDIR)\$(NULL)" $(MKDIR) "$(LIBDIR)"
		if not exist "$(LIBDIR)\web\$(NULL)" $(MKDIR) "$(LIBDIR)\web"
		if not exist "$(LIBDIR)\web\icons\$(NULL)" $(MKDIR) "$(LIBDIR)\web\icons"
		if not exist "$(LIBDIR)\web\css\$(NULL)" $(MKDIR) "$(LIBDIR)\web\css"
		@echo Copying $(LIBPL)
		@for %f in ($(LIBPL)) do @copy %f "$(LIBDIR)"
		copy README "$(LIBDIR)\README.TXT"
		copy web\icons\*.* "$(LIBDIR)\web\icons"
		copy web\css\*.* "$(LIBDIR)\web\css"
		copy http_stream.dll "$(BINDIR)"
		copy json.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy http_stream.pdb "$(BINDIR)"
		copy json.pdb "$(BINDIR)"
!ENDIF
		$(MAKEINDEX)
!ENDIF

html-install:	install-examples
		copy http.html "$(PKGDOC)"
		copy httpserver.gif "$(PKGDOC)"

pdf-install:	install-examples
		copy http.pdf "$(PKGDOC)"

install-examples::
		if not exist "$(EXDIR)\$(NULL)" $(MKDIR) "$(EXDIR)"
		if not exist "$(EXDIR)\pwp\$(NULL)" $(MKDIR) "$(EXDIR)\pwp"
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"
		cd examples & copy $(EXAMPLEEXE) "$(EXDIR)"
		cd examples & copy pwp\*.* "$(EXDIR)\pwp"

xpce-install::

uninstall::
		cd $(LIBDIR) & del $(LIBPL) README.TXT
		del "$(BINDIR)\http_stream.dll"
		$(MAKEINDEX)

clean::
		if exist *~ del *~
		if exist *.obj del *.obj

distclean:	clean
		if exist *.dll del *.dll
		if exist *.pdb del *.pdb


