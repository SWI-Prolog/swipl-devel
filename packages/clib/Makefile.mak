################################################################
# Build the SWI-Prolog tabling package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
PKGDLL=socket

SOCKOBJ=	socket.obj error.obj
CGIOBJ=		error.obj form.obj cgi.obj
MEMOBJ=		error.obj memfile.obj
MIMEOBJ=	error.obj mime.obj
MIMELIBS=	rfc2045.lib rfc822.lib
TIMEOBJ=	error.obj time.obj
TIMELIBS=	winmm.lib

all:		socket.dll cgi.dll memfile.dll mime.dll

socket.dll:	$(SOCKOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(SOCKOBJ) $(PLLIB) $(LIBS)
cgi.dll:	$(CGIOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(CGIOBJ) $(PLLIB) $(LIBS)
memfile.dll:	$(MEMOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(MEMOBJ) $(PLLIB) $(LIBS)
mime.dll:	$(MIMEOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(MIMEOBJ) $(PLLIB) $(LIBS) $(MIMELIBS)
time.dll:	$(TIMEOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(TIMEOBJ) $(PLLIB) $(LIBS) $(TIMELIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

idll::
		copy socket.dll $(BINDIR)
		copy cgi.dll $(BINDIR)
		copy memfile.dll $(BINDIR)
		copy mime.dll $(BINDIR)
ilib::
		copy socket.pl $(PLBASE)\library
		copy cgi.pl $(PLBASE)\library
		copy memfile.pl $(PLBASE)\library
		copy mime.pl $(PLBASE)\library
		$(MAKEINDEX)

uninstall::
		del $(BINDIR)\socket.dll
		del $(BINDIR)\cgi.dll
		del $(BINDIR)\memfile.dll
		del $(BINDIR)\mime.dll
		del $(PLBASE)\library\socket.pl
		del $(PLBASE)\library\cgi.pl
		del $(PLBASE)\library\memfile.pl
		del $(PLBASE)\library\mime.pl
		$(MAKEINDEX)

html-install::
		copy clib.html $(PKGDOC)

clean::
		DEL *.obj *~

distclean:	clean
		DEL socket.dll socket.lib socket.exp
		DEL cgi.dll cgi.lib cgi.exp
		DEL memfile.dll memfile.lib memfile.exp

