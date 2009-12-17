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

LIBDIR=		$(PLBASE)\library\http
EXDIR=		$(PKGDOC)\examples\http
LIBPL=		html_write.pl http_client.pl http_header.pl \
		http_mime_plugin.pl http_sgml_plugin.pl \
		mimepack.pl mimetype.pl dcg_basics.pl \
		thread_httpd.pl xpce_httpd.pl inetd_httpd.pl \
		http_wrapper.pl http_open.pl http_session.pl \
		http_error.pl http_parameters.pl http_dispatch.pl \
		http_authenticate.pl http_stream.pl http_log.pl \
		http_path.pl http_hook.pl html_head.pl http_exception.pl \
		json.pl http_json.pl json_convert.pl http_dirindex.pl \
		http_server_files.pl
EXAMPLES=	demo_body.pl demo_client.pl demo_threads.pl demo_xpce.pl \
		calc.pl
EXAMPLEEXE=	demo_inetd
XPCEPL=		http_image.pl

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
		if not exist "$(LIBDIR)/$(NULL)" $(MKDIR) "$(LIBDIR)"
		@echo Copying $(LIBPL)
		@for %f in ($(LIBPL)) do @copy %f "$(LIBDIR)"
		copy README "$(LIBDIR)\README.TXT"
		copy /r web "$(LIBDIR)\web"
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
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"
		cd examples & copy $(EXAMPLEEXE) "$(EXDIR)"

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


