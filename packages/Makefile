################################################################
# Very simplistic makefile to build all relevant packages
################################################################

PKG=	clib \
	table \
	sgml \
	sgml/RDF

CONFIG=	./configure --enable-mt

all:	configure objects
	
objects:
	for p in $(PKG); do \
	    ( cd $$p && $(MAKE) ) \
        done
	
install:
	for p in $(PKG); do \
	    ( cd $$p && $(MAKE) install ) \
        done
	
configure:
	for p in $(PKG); do \
	    ( cd $$p && $(CONFIG) ) \
        done
	
distclean:
	for p in $(PKG); do \
	    ( cd $$p && $(MAKE) distclean ) \
        done

clean:
	for p in $(PKG); do \
	    ( cd $$p && $(MAKE) clean ) \
        done

