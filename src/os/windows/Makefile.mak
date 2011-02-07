################################################################
# The uxnt library provides some additional Unix compatibility
# stuff on top of the MSVC (posix) runtime library.
################################################################

PLHOME=..\..\..
!include ..\..\rules.mk

OBJ=	uxnt.obj
OUT=	$(PLHOME)\lib\uxnt.lib
HDR=	$(PLHOME)\include\uxnt.h \
	$(PLHOME)\include\dirent.h

all:	$(OUT) $(HDR)

$(OUT):	$(OBJ)
	if exist "$@" del "$@"
	$(AR) /nologo /out:$@ $(OBJ)

$(PLHOME)\include\uxnt.h: uxnt.h
	copy uxnt.h $@
$(PLHOME)\include\dirent.h: dirent.h
	copy dirent.h $@

clean::
	if exist *.obj del *.obj
	if exist *~ del *~

distclean: clean
	if exist uxnt.lib del uxnt.lib
