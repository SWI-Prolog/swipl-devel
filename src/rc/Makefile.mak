################################################################
# Makefile for SWI-Prolog resource manager
#
# Author: Jan Wielemaker
# E-mail: jan@swi.psy.uva.nl
################################################################

!include ..\rules.mk

LIBOBJ=		html.obj access.obj build.obj util.obj
PLRC=..\..\bin\plrc.exe

all:		rc.lib $(PLRC)

$(PLRC):	rc.lib rc.obj
		$(LD) /subsystem:console /out:$@ rc.obj rc.lib $(LIBS)

rc.lib:		$(LIBOBJ)
		if exist $@ del $@
		$(AR) /out:$@ /nologo $(LIBOBJ)

################################################################
# Cleanup
################################################################

clean:
		del *~ *.obj

distclean:	clean
		del plrc.exe rc.lib

