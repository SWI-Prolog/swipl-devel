# Destinations.  Please edit
prefix=c:\Documents and Settings\jan
LIBDIR=$(prefix)\lib
INCDIR=$(prefix)\include

CC=cl.exe
AR=lib.exe
CFLAGS=/MD /W3 /O2 /EHsc /DNDEBUG /D__WINDOWS__ /I.. /nologo /c

OUT=rfc822.lib

OBJ=	rfc822.obj rfc822_getaddr.obj rfc822_getaddrs.obj \
	rfc822_mkdate.obj rfc822_parsedt.obj rfc2047u.obj \
	rfc2047.obj imapsubj.obj imaprefs.obj

.c.obj:
	@$(CC) $(CFLAGS) /Fo$@ $<

$(OUT):	$(OBJ)
	del $@
	$(AR) /out:$@ /nologo $(OBJ)

install:	$(OUT)
		copy $(OUT) "$(LIBDIR)"
		copy rfc822.h "$(INCDIR)"

clean::
		del *~ *.obj

distclean:	clean
		del $(OUT)

