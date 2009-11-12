################################################################
# Build the SWI-Prolog SSL package for MS-Windows
#
# Author: Jan Wielemaker
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk
PKGDLL=ssl4pl
EXDIR=		$(PKGDOC)\examples\ssl
HTTPDIR=	$(PLBASE)\library\http
CFLAGS=		-D__SWI_PROLOG__ $(CFLAGS)

#
# Constants below are defined in rules.mk
#
LIB=$(LIB);$(OPENSSLLIBDIR)
INCLUDE=$(INCLUDE);$(OPENSSLINCDIR)

EXAMPLES=	client.pl server.pl https.pl

OBJ=		ssl4pl.obj ssllib.obj ..\clib\error.obj

all:		$(PKGDLL).dll

$(PKGDLL).dll:	$(OBJ) ..\clib\socket.lib
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS) \
		ssleay32.lib libeay32.lib ..\clib\socket.lib

!IF "$(CFG)" == "rt"
install:	all idll
!ELSE
install:	all idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy $(PKGDLL).pdb "$(BINDIR)"
!ENDIF

ilib::
		copy ssl.pl "$(PLBASE)\library"
		if not exist "$(HTTPDIR)/$(NULL)" $(MKDIR) "$(HTTPDIR)"
		copy http_ssl_plugin.pl "$(HTTPDIR)"
		$(MAKEINDEX)

xpce-install::

html-install:	install-examples


install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		@for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"
		xcopy /Q /S /I /Y etc "$(EXDIR)\etc"
		if exist "$(EXDIR)\etc\README.TXT" del "$(EXDIR)\etc\README.TXT"
		ren "$(EXDIR)\etc\README" "README.TXT"

uninstall::
		del "$(PLBASE)\bin\$(PKGDLL).dll"
		del "$(PLBASE)\library\ssl.pl"
		$(MAKEINDEX)

clean::
		-del *.obj *~ 2>nul

distclean:	clean
		-del *.dll *.lib *.exe *.pdb *.ilk 2>nul

