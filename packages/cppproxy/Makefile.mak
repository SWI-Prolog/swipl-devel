################################################################
# Build the SWI-Prolog C++ proxy package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk

LIBDIR=		$(PLBASE)\library\cppproxy
EXDIR=		$(PKGDOC)\examples\cppproxy
EXAMPLES=	Makefile parms.pl \
		person.pl person.cpp \
		sqrt.pl sqrt.cpp \
		time.pl time.cpp

all:		serialize.dll

serialize.dll:	serialize.obj
		$(LD) /dll /out:$@ $(LDFLAGS) serialize.obj $(PLLIB)

install:	idll ilib

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

idll::
		copy serialize.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy serialize.pdb "$(BINDIR)"
!ENDIF

ilib::
		copy cpp_codegen.pl "$(PLBASE)\library"
		copy cpp_interface.pl "$(PLBASE)\library"
		copy cpp_server.pl "$(PLBASE)\library"
		copy typedef.pl "$(PLBASE)\library"
		copy SWI-proxy.cpp "$(PLBASE)\include"
		copy SWI-proxy.h "$(PLBASE)\include"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\serialize.dll"
		del "$(PLBASE)\library\cpp_codegen.pl"
		del "$(PLBASE)\library\cpp_interface.pl"
		del "$(PLBASE)\library\cpp_server.pl"
		del "$(PLBASE)\library\typedef.pl"
		del "$(PLBASE)\include\SWI-proxy.cpp"
		del "$(PLBASE)\include\SWI-proxy.h"
		$(MAKEINDEX)

html-install:	install-examples
		copy cppproxy.html "$(PKGDOC)"

install-examples::
		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
		cd examples & copy README "$(EXDIR)"\README.TXT
		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


