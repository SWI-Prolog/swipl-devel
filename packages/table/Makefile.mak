################################################################
# Build the SWI-Prolog tabling package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

!include ..\..\src\rules.mk
PLHOME=		..\..

OBJ=		table.obj order.obj error.obj

all:		table.dll

table.dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

install:
		copy table.dll $(PLBASE)\bin
		copy table.pl $(PLBASE)\library
		copy table_util.pl $(PLBASE)\library
		$(MAKEINDEX)

clean:
		DEL *.obj *~

distclean:	clean
		DEL table.dll table.lib table.exp

