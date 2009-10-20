
@echo off

SETLOCAL

call call_vcvars.cmd

rem Build default multi-threaded version
nmake /f makefile.mak
rem And install it
nmake /f makefile.mak install

rem Build for multi-threading and debugging
rem nmake DBG=true /f makefile.mak %*

rem Enable this line to build for single-threading.
rem nmake MT=false /f makefile.mak %*

pause

ENDLOCAL