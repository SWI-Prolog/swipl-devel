################################################################
# The uxnt library provides some additional Unix compatibility
# stuff on top of the MSVC (posix) runtime library.
################################################################

!include ..\..\rules.mk

PLBASE=	..\..\..
OBJ=	uxnt.obj
HDR=	$(PLBASE)\include\uxnt.h \
	$(PLBASE)\include\dirent.h

all:	uxnt.lib $(HDR)

uxnt.lib:	$(OBJ)
	del $@
	$(AR) /nologo /out:$@ $(OBJ)

$(PLBASE)\include\uxnt.h: uxnt.h
	copy uxnt.h $@
$(PLBASE)\include\dirent.h: dirent.h
	copy dirent.h $@

clean:
	del *~ *.obj

distclean: clean
	del uxnt.lib
