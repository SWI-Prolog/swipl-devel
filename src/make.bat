@echo off

SETLOCAL

rem ****************************************************************
rem Configuration section

set CVSROOT=:pserver:jan@gollem:/usr/local/plcvs
set HOME=%USERPROFILE%\My Documents
set WINDIR=C:\WINDOWS
set DEVSTUDIO=C:\Program Files\Microsoft Visual Studio
set JAVA_HOME=C:\j2sdk1.4.2_04
set PATH=%PATH%;%USERPROFILE%\installed\pl\bin;%DEVSTUDIO%\VC98\bin;%DEVSTUDIO%\SharedIDE\bin;%JAVA_HOME%\bin
set LIB=%LIB%;%HOME%\lib;%DEVSTUDIO%\VC98\lib
set INCLUDE=%INCLUDE%;%HOME%\include;%DEVSTUDIO%\VC98\include
rem ****************************************************************

rem Build default multi-threaded version 
nmake /f makefile.mak %*

rem Build for multi-threading and debugging
rem nmake DBG=true /f makefile.mak %*

rem Enable this line to build for single-threading.
rem nmake MT=false /f makefile.mak %*

ENDLOCAL
