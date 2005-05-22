# Destinations.  Please edit
prefix=E:\jan
LIBDIR=$(prefix)\lib
INCDIR=$(prefix)\include

CC=cl.exe
AR=lib.exe
CFLAGS=/MD /W3 /O2 /GX /DNDEBUG /DWIN32 /I.. /nologo /c

OUT=rfc2045.lib

OBJ=	rfc2045.obj rfc2045acchk.obj rfc2045acprep.obj \
	rfc2045appendurl.obj rfc2045cdecode.obj rfc2045decode.obj \
	rfc2045enomem.obj \
	rfc2045_base64encode.obj rfc2045_fromfd.obj \
	rfc2045find.obj rfc2045mkboundary.obj rfc2045rewrite.obj \
	rfc2045tryboundary.obj rfc2045xdump.obj

.c.obj:
	@$(CC) $(CFLAGS) /Fo$@ $<

$(OUT):	$(OBJ)
	del $@
	$(AR) /out:$@ /nologo $(OBJ)

install:	$(OUT)
		copy $(OUT) $(LIBDIR)
		copy rfc2045.h $(INCDIR)

clean::
		del *~ *.obj

distclean:	clean
		del $(OUT)

