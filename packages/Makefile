################################################################
# Very simplistic makefile to build all relevant packages
################################################################

PKG=	clib \
	table \
	sgml \
	sgml/RDF \
	xpce

CONFIG=	./configure --enable-mt

all:	objects
	
objects:
	for p in $(PKG); do \
	    ( cd $$p && $(MAKE) ) \
        done
	
install:
	for p in $(PKG); do \
	    ( cd $$p && $(MAKE) install ) \
        done
	
configure::
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

