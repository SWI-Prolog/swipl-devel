# Makefile for dvi2tty and disdvi       23/01/89   M.J.E. Mol
#
# For BSD Unix use the following CFLAGS definition
# CFLAGS = -Dstrchr=index
#
# This Makefile does not work for MSDOS. Make your 
# own one, or compile by hand.
#

CFLAGS=-O
prefix=/usr/local

all:	dvi2tty

dvi2tty: dvi2tty.o dvistuff.o
	$(CC) -o dvi2tty dvi2tty.o dvistuff.o

install:
	cp dvi2tty $(prefix)/bin/dvi2tty
	chmod 755 $(prefix)/bin/dvi2tty

distclean:	clean

clean:
	rm *.o dvi2tty
