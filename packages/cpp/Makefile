################################################################
# Makefile for test programs
################################################################

PLBASE=$(shell eval `pl -dump-runtime-variables` && echo $$PLBASE)


all:		test.so likes

test.so:	test.cpp SWI-cpp.h
		g++ -Wall -O2 -c -I$(PLBASE)/include -fpic test.cpp
		g++ -shared -o $@ test.o

likes:		likes.cpp likes.pl SWI-cpp.h
		plld -o $@ -ld g++ -goal true likes.cpp likes.pl

clean:
		rm -f test.so test.o *~ likes
