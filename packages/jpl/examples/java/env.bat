@echo off

REM Set variables needed by the examples
REM This script assumes both the bin directories of SWI-Prolog and the Java
REM SDK to be in %PATH%.  If this is not the case, you may wish to set %PATH%
REM in this script.

REM Find the Prolog coordinates

plcon.exe -dump-runtime-variables=cmd > %TEMP%\plrtvars.bat
call %TEMP%\plrtvars.bat
del %TEMP%\plrtvars.bat

REM Find classpath for jpl.jar.  First case holds if we are in the source tree.

if exist ..\..\..\jpl.jar (
  set CLASSPATH=.;..\..\..\jpl.jar
) else (
  set CLASSPATH=.;%PLBASE%\lib\jpl.jar
)

