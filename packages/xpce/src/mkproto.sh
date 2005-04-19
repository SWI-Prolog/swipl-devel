#!/bin/sh

# mkproto file ...
# 
# Generate the proto.h files in the various subdirectories. mkproto is a
# public domain program to generate ANSI prototypes from C-sources.

mkproto -p -d COMMON -D '#define COMMON(type) SO_LOCAL type' $* | \
	grep -vw NewClass | grep -v pceMT
