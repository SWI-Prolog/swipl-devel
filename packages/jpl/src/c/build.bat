@echo off

rem JAVA_HOME must (already) be set to the root dir of a recent Sun Java SDK
rem PL_HOME must (already) be set to the root dir of a recent SWI-Prolog installation

set DEFINES=/D_REENTRANT /DWIN32 /D_WINDOWS /D__SWI_PROLOG__ /D__SWI_EMBEDDED__
set JVM_INC=/I "%JAVA_HOME%\include" /I "%JAVA_HOME%\include/win32"
set PL_INC=/I "%PL_HOME%\include"
set JVM_LIB="%JAVA_HOME%\lib\jvm.lib"
set PL_LIB="%PL_HOME%\lib\libpl.lib"
set PTHREAD_LIB="%PL_HOME%\lib/pthreadVC.lib"

CL.EXE /W3 /nologo /MD /LD %DEFINES% %JVM_INC% %PL_INC% %JVM_LIB% %PL_LIB% %PTHREAD_LIB% jpl.c
pause

