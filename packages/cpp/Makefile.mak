################################################################
# Build the SWI-Prolog C++ interface for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk

all:		

install::
		copy SWI-cpp.h $(PLBASE)\include

uninstall::
		del $(PLBASE)\include\SWI-cpp.h

clean::
		DEL *.obj *~

distclean:	clean


