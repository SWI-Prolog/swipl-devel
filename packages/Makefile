################################################################
# Very simplistic makefile to build all relevant packages
################################################################

PKG=	clib \
	table \
	sgml

CONFIG=	./configure --enable-mt

all:
	for p in $(PKG); do \
	    ( cd $$p && make distclean && $(CONFIG) && make && make install ) \
        done

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

