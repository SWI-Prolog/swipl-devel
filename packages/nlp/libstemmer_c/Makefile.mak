!include ..\..\..\src\rules.mk
!include mkinc_utf8.mak
CFLAGS=-Iinclude $(CFLAGS)

all: libstemmer.lib

SOBJ=$(snowball_sources:.c=.obj)

libstemmer.lib: $(SOBJ)
	$(AR) /out:$@ /nologo $(SOBJ)

clean:
	rm -f stemwords *.obj src_c/*.obj runtime/*.obj libstemmer/*.obj *~
distclean: clean
	rm -f libstemmer.lib
