################################################################
# Makefile for SWI-Prolog resource manager
#
# Author: Jan Wielemaker
# E-mail: jan@swi.psy.uva.nl
################################################################

PLHOME=..\..
!include ..\rules.mk

LIBOBJ=		html.obj access.obj build.obj util.obj
PLRC=..\..\bin\swipl-rc.exe

all:		rc.lib $(PLRC)

$(PLRC):	rc.lib rc.obj
		$(LD) /subsystem:console /out:$@ rc.obj rc.lib $(UXLIB) $(LIBS)

rc.lib:		$(LIBOBJ)
		if exist $@ del $@
		$(AR) /out:$@ /nologo $(LIBOBJ)

################################################################
# Cleanup
################################################################

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-del plrc.exe rc.lib *.pdb 2>nul

