include mkinc_utf8.mak
CFLAGS+=-Iinclude

all: libstemmer.a

libstemmer.a: $(snowball_sources:.c=.o)
	$(AR) -cru $@ $^
stemwords: examples/stemwords.o libstemmer.a
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f stemwords *.o src_c/*.o runtime/*.o libstemmer/*.o *~
distclean: clean
	rm -f libstemmer.a
