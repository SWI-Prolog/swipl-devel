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
OUTDBG=		$(PLHOME)\bin\plterm.pdb

XLIBS=		gdi32.lib comdlg32.lib $(DBGLIBS)

OBJ=		complete.obj console.obj edit.obj history.obj menu.obj

all:		$(OUTLIB) $(OUTINC)

$(OUTLIB):	$(OBJ)
		$(LD) $(LDFLAGS) /out:$(OUTDLL) /implib:$@ /dll $(OBJ) $(LIBS) $(XLIBS)

$(OUTINC):	console.h
		copy console.h $@

install:	all
		copy $(OUTDLL) $(BINDIR)
		copy $(OUTLIB) $(LIBDIR)
		copy $(OUTINC) $(INCDIR)
!IF "$(DBG)" == "true"
		copy $(OUTDBG) $(BINDIR)
!ENDIF

clean:
		del *~ *.obj

distclean:	clean
		del $(OUTLIB) $(OUTDLL)

# dependencies

$(OBJ):		console_i.h
