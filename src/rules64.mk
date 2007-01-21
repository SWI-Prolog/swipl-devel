# 64-bit version specific make rules

# Find additional .lib files here
EXTRALIBDIR=$(HOME)\lib64

# Prolog startup file
PLBOOTFILE=boot64.prc

# PTHREAD library
LIBPTHREAD=pthreadVC2

# ZLIB library
LIBZLIB=zlibwapi

# Installer
NSIS=C:\Program Files (x86)\NSIS\MakeNSIS.exe
NSISDEFS=/DWIN64

# Architecture ID
ARCH=x64-win64

# Extra libraries to load
EXTRALIBS=bufferoverflowU.lib
