@echo off

SETLOCAL

rem ****************************************************************
rem Configuration section

set CVSROOT=:pserver:jan@gollem:/usr/local/cvsroot
set HOME=%USERPROFILE%
set WINDIR=C:\WINNT
set DEVSTUDIO=C:\Program Files\DevStudio
set JAVA_HOME=C:\j2sdk1.4.2_04
set PATH=%PATH%;%USERPROFILE%\installed\pl\bin;%DEVSTUDIO%\VC\bin;%DEVSTUDIO%\SharedIDE\bin;%JAVA_HOME%\bin
set LIB=%LIB%;%DEVSTUDIO%\VC\lib
set INCLUDE=%INCLUDE%;%DEVSTUDIO%\VC\include
rem ****************************************************************

rem Build default multi-threaded version 
nmake /f makefile.mak %*

rem Build for multi-threading and debugging
rem nmake DBG=true /f makefile.mak %*

rem Enable this line to build for single-threading.
rem nmake MT=false /f makefile.mak %*

ENDLOCAL
