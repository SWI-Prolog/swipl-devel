!include ..\..\..\src\rules.mk
!include mkinc_utf8.mak
CFLAGS=-Iinclude $(CFLAGS)

all: libstemmer.lib

SOBJ=$(snowball_sources:.c=.obj)

libstemmer.lib: $(SOBJ)
	$(AR) /out:$@ /nologo $(SOBJ)

clean:
	if exist stemwords del stemwords
	if exist src_c\*.obj del src_c\*.obj
	if exist runtime\*.obj del runtime\*.obj
	if exist libstemmer\*.obj del libstemmer\*.obj
distclean: clean
	if exist libstemmer.lib del libstemmer.lib
