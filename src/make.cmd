
@echo off

SETLOCAL

call call_vcvars.cmd

rem Build default multi-threaded version
nmake /f makefile.mak %*

rem Build for multi-threading and debugging
rem nmake DBG=true /f makefile.mak %*

ENDLOCAL
