################################################################
# Simple relaying Makefile
#
# See also ./configure
################################################################

# Uncomment if not set automatically by your make
#MAKE=make

all:
	@cd src && $(MAKE) $@
install:
	@cd src && $(MAKE) $@
clean:
	@cd src && $(MAKE) $@
distclean:
	@cd src && $(MAKE) $@
