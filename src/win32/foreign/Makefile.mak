################################################################
# Make DLL demos
################################################################

PLHOME=..\..\..
!include $(PLHOME)\src\rules.mk

DLLS=	$(PLHOME)\bin\plregtry.dll $(PLHOME)\bin\dlltest.dll

all:	$(DLLS)

$(PLHOME)\bin\plregtry.dll:	plregtry.obj
	$(LD) /dll /out:$@ $(LDFLAGS) plregtry.obj $(PLLIB) $(LIBS)

$(PLHOME)\bin\dlltest.dll:	dlltest.obj
	$(LD) /dll /out:$@ $(LDFLAGS) dlltest.obj $(TERMLIB) $(PLLIB) $(LIBS)

clean::
	del *~ *.obj

distclean: clean
	del $(DLLS)
