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

all:		serialize.dll

serialize.dll:	serialize.c
		$(LD) /dll /out:$@ $(LDFLAGS) serialize.c

install:	idll ilib

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

idll::
		serialize.dll "$(BINDIR)"
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

html-install::
		copy cppproxy.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


