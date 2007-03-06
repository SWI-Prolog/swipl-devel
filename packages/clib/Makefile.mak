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
CFLAGS=$(CFLAGS) /DUSE_SHA256
PKGDLL=socket

SOCKOBJ=	socket.obj nonblockio.obj error.obj
CGIOBJ=		error.obj form.obj cgi.obj
CRYPTOBJ=	error.obj crypt.obj md5.obj md5passwd.obj
MEMOBJ=		error.obj memfile.obj
MIMEOBJ=	error.obj mime.obj
MIMELIBS=	rfc2045.lib rfc822.lib
TIMEOBJ=	error.obj time.obj
READOBJ=	readutil.obj
RANDOMOBJ=	random.obj
SHAOBJ=		error.obj sha4pl.obj sha1/sha1.obj sha1/sha2.obj sha1/hmac.obj
TIMELIBS=	winmm.lib

all:		socket.dll cgi.dll memfile.dll mime.dll time.dll readutil.dll \
		random.dll crypt.dll

readutil.dll:	$(READOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(READOBJ) $(PLLIB) $(LIBS)
socket.dll:	$(SOCKOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(SOCKOBJ) $(PLLIB) $(LIBS)
cgi.dll:	$(CGIOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(CGIOBJ) $(PLLIB) $(LIBS)
crypt.dll:	$(CRYPTOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(CRYPTOBJ) $(PLLIB) $(LIBS)
memfile.dll:	$(MEMOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(MEMOBJ) $(PLLIB) $(LIBS)
mime.dll:	$(MIMEOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(MIMEOBJ) $(PLLIB) $(LIBS) $(MIMELIBS)
time.dll:	$(TIMEOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(TIMEOBJ) $(PLLIB) $(LIBS) $(TIMELIBS)
random.dll:	$(RANDOMOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(RANDOMOBJ) $(PLLIB) $(LIBS)
sha4pl.dll:	$(SHAOBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(SHAOBJ) $(PLLIB) $(LIBS)


!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

################################################################
# Testing
################################################################

check:		check-socket

torture:	torture-socket

check-socket::
		"$(PLCON)" -q -f testsocket.pl -F none -g tcp_test,halt -t 'halt(1)'

torture-socket::
		"$(PLCON)" -q -f stresssocket.pl -F none -g test,halt -t 'halt(1)'

################################################################
# Installation
################################################################

idll::
		copy socket.dll "$(BINDIR)"
		copy cgi.dll "$(BINDIR)"
		copy crypt.dll "$(BINDIR)"
		copy memfile.dll "$(BINDIR)"
		copy mime.dll "$(BINDIR)"
		copy time.dll "$(BINDIR)"
		copy random.dll "$(BINDIR)"
		copy readutil.dll "$(BINDIR)"
		copy sha4pl.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy socket.pdb "$(BINDIR)"
		copy cgi.pdb "$(BINDIR)"
		copy memfile.pdb "$(BINDIR)"
		copy mime.pdb "$(BINDIR)"
		copy time.pdb "$(BINDIR)"
		copy readutil.pdb "$(BINDIR)"
		copy sha4pl.pdb "$(BINDIR)"
!ENDIF

ilib::
		copy socket.pl "$(PLBASE)\library"
		copy prolog_server.pl "$(PLBASE)\library"
		copy streampool.pl "$(PLBASE)\library"
		copy cgi.pl "$(PLBASE)\library"
		copy crypt.pl "$(PLBASE)\library"
		copy memfile.pl "$(PLBASE)\library"
		copy mime.pl "$(PLBASE)\library"
		copy random.pl "$(PLBASE)\library"
		copy time.pl "$(PLBASE)\library"
		copy sha.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\socket.dll"
		del "$(BINDIR)\streampool.dll"
		del "$(BINDIR)\cgi.dll"
		del "$(BINDIR)\crypt.dll"
		del "$(BINDIR)\memfile.dll"
		del "$(BINDIR)\mime.dll"
		del "$(BINDIR)\random.dll"
		del "$(BINDIR)\time.dll"
		del "$(BINDIR)\readutil.dll"
		del "$(BINDIR)\sha4pl.dll"
		del "$(PLBASE)\library\socket.pl"
		del "$(PLBASE)\library\cgi.pl"
		del "$(PLBASE)\library\crypt.pl"
		del "$(PLBASE)\library\memfile.pl"
		del "$(PLBASE)\library\mime.pl"
		del "$(PLBASE)\library\random.pl"
		del "$(PLBASE)\library\time.pl"
		del "$(PLBASE)\library\sha.pl"
		$(MAKEINDEX)

html-install::
		copy clib.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


