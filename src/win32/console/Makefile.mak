################################################################
# Makefile for the SWI-Prolog console window
#
# Building: nmake /f Makefile.mak
################################################################

!include ..\..\rules.mk

PLHOME=		..\..\..
OUTDLL=		$(PLHOME)\bin\plterm.dll
OUTLIB=		$(PLHOME)\lib\plterm.lib
OUTINC=		$(PLHOME)\include\console.h

!IF "$(DBG)" == "false"
XLIBS=		gdi32.lib comdlg32.lib
!ELSE
XLIBS=		msvcrtd.lib gdi32.lib comdlg32.lib
!ENDIF

OBJ=		complete.obj console.obj edit.obj history.obj

all:		$(OUTLIB) $(OUTINC)

$(OUTLIB):	$(OBJ)
		$(LD) /out:$(OUTDLL) /implib:$@ /dll $(OBJ) $(LIBS) $(XLIBS)

$(OUTINC):	console.h
		copy console.h $@

clean:
		del *~ *.obj

distclean:	clean
		del $(OUTLIB) $(OUTDLL)
