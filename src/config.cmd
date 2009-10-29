
@echo off

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                                                                            ::
:: Test system: Windows XP x64 SP2, all patches applied                       ::
:: Succesfull builds of both 32 and 64 bit flavours on:                       ::
::                                                                            ::
:: Microsoft Visual Studio 6.0 SP6 (32-bit only)                              ::
:: Microsoft Platform SDK for Windows Server 2003 R2 (64-bit only)            ::
:: Microsoft Visual Studio 2005 SP1                                           ::
:: Microsoft Visual Studio 2008 SP1                                           ::
:: Microsoft Windows SDK for Windows 7 and .NET Framework 3.5 SP1             ::
:: Visual Studio 2010 (Beta1)                                                 ::
::                                                                            ::
::                        degski [ta] gmail [tod] com                         ::
::                                                                            ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

SETLOCAL

:what_host
set "PL!WINDIR=%SystemRoot%"
if defined ProgramFiles(x86) (set "PL!HOST_OS_ARCH=X64") ELSE (set "PL!HOST_OS_ARCH=X86")
if "%PL!HOST_OS_ARCH%"=="X64" (set "PL!PROGRAM_FILES_32=%ProgramFiles(x86)%") else (set "PL!PROGRAM_FILES_32=%ProgramFiles%")
:end_what_host


:start_create_welcome
echo Welcome to SWI-Prolog Windows Build Environment Configuration Utility > welcome.txt
echo Copyright (c) 1990-2009 University of Amsterdam. >> welcome.txt
echo SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software, >> welcome.txt
echo and you are welcome to redistribute it under certain conditions. >> welcome.txt
echo Please visit http://www.swi-prolog.org for details. >> welcome.txt
:end_start_create_welcome


:what_target
cls
type welcome.txt
echo.
set /P "BITS=Would you like to do a 32bit or 64bit build? (32 or 64): "
if "%BITS%"=="32" (goto what_target_bits_oke)
if "%BITS%"=="64" (goto what_target_bits_oke)
echo The only choices possible are 32 and 64!
goto what_target
:what_target_bits_oke
if "%BITS%"=="32" (set "PL!TARGET_OS_ARCH=X86") else (set "PL!TARGET_OS_ARCH=X64")
if "%BITS%"=="32" (set "PL!TARGET_PROGRAM_FILES=%PL!PROGRAM_FILES_32%") else (set "PL!TARGET_PROGRAM_FILES=%ProgramFiles%")
if "%BITS%"=="32" (set "PL!MD=WIN32") else (set "PL!MD=WIN64")
echo.
:end_what_target


:sanity_check
if not "%PL!HOST_OS_ARCH%%PL!TARGET_OS_ARCH%"=="X86X64" (goto end_sanity_check)
cls
type welcome.txt
echo.
echo A 64-bit target install cannot be built on a 32-bit host operating system, exiting...
echo.
pause
goto end_no_pause
:end_sanity_check


:set_vc_or_sdk_environment
::determine_vc_version
set "VCXX=UNKNOWN"
if exist "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio\VC98\" (set "VCXX=VC06")
if exist "%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\" (set "VCXX=VC08")
if exist "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 8\" (set "VCXX=VC08")
if exist "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\" (set "VCXX=VC09")
if exist "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\" (set "VCXX=VC10")
::set_vc6_environment_file
if "%VCXX%_%BITS%"=="VC06_32" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio\VC98\Bin\VCVARS32.BAT" ^> nul > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC06_64" (goto this_build_is_not_possible)
::set_vc8_or_sdk2003_R2_environment_file
if exist "%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\SetEnv.Cmd" (set "TYPE=SDK")
if exist "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\vcvarsall.bat" (set "TYPE=STU")
if "%VCXX%_%TYPE%_%BITS%"=="VC08_SDK_64" (@echo call "%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\SetEnv.Cmd" /X64 /RETAIL ^> nul > call_vcvars.cmd)
if "%VCXX%_%TYPE%_%BITS%"=="VC08_SDK_32" (goto this_build_is_not_possible)
if "%VCXX%_%TYPE%_%BITS%"=="VC08_STU_64" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\vcvarsall.bat" x86_amd64 ^> nul > call_vcvars.cmd)
if "%VCXX%_%TYPE%_%BITS%"=="VC08_STU_32" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\vcvarsall.bat" x86 ^> nul > call_vcvars.cmd)
::set_vc9_or_sdk2008_sdkwin7_environment_file
if exist "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" (set "TYPE=STU")
if "%VCXX%_%TYPE%_%BITS%"=="VC09_STU_64" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_amd64 ^> nul > call_vcvars.cmd)
if "%VCXX%_%TYPE%_%BITS%"=="VC09_STU_32" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86 ^> nul > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC09_64" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\bin\vcvarsx86_amd64.bat" ^> nul > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC09_32" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat" ^> nul > call_vcvars.cmd)
::set_vc10_or_sdkwin7_environment_file
if "%VCXX%_%BITS%"=="VC10_64" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86_amd64 ^> nul > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC10_32" (@echo call "%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86 ^> nul > call_vcvars.cmd)
:is_vc_version_determined
if not "%VCXX%"=="UNKNOWN" (goto set_vc_environment)
cls
type welcome.txt
echo.
echo VC version could not be determined, exiting...
echo.
pause
goto end_no_pause
:this_build_is_not_possible
cls
type welcome.txt
echo.
echo The requested %BITS%-bits build is not possible on %VCXX%, exiting...
echo.
pause
goto end_no_pause
:set_vc_environment
call call_vcvars.cmd
echo.
:end_set_vc_or_sdk_environment


:set_manifest_tool
set "PL!MTEXE=rem"
if "%VCXX%_%TYPE%"=="VC08_STU" (SET "PL!MTEXE=mt.exe -nologo")
if "%VCXX%"=="VC09" (SET "PL!MTEXE=mt.exe -nologo")
if "%PL!MTEXE%"=="rem" (goto end_set_manifest_tool)
:have_mt_exe in path
@for %%i in (mt.exe) do @if NOT "%%!$PATH:i"=="" (goto end_set_manifest_tool)
cls
type welcome.txt
echo.
echo mt.exe cannot be found in the path, exiting...
echo.
pause
goto end_no_pause
:end_set_manifest_tool


:determine_plhome
set "_=%CD%"
cd ..
set "PL!HOME=%CD%"
cd \
set "PL!HOME_DRIVE=%CD%"
set "PL!LIB=%LIB%;%PL!HOME%\lib"
set "PL!INCLUDE=%INCLUDE%;%PL!HOME%\include"
set "PATH=%PATH%;%PL!HOME%\bin";%PL!HOME%\lib"
cd "%_%"
:end_determine_plhome


:set_additional_variables
set "PL!EXTRALIBDIR=%PL!HOME%\lib"
set "PL!PLBOOTFILE=boot%BITS%.prc"
if "%BITS%"=="32" (set "PL!LIBPTHREAD=pthreadVC") else (set "PL!LIBPTHREAD=pthreadVC2")
if "%BITS%"=="32" (set "PL!LIBZLIB=zlib1") else (set "PL!LIBZLIB=zlibwapi")
if "%BITS%"=="32" (set "PL!ARCH=i386-win32") else (set "PL!ARCH=x64-win64")
:set_extra_libs
if "%VCXX%_%TYPE%_%BITS%"=="VC08_SDK_64" (set "BUFFEROVERFLOW=bufferoverflowU.lib") else (set "BUFFEROVERFLOW=")
set "PL!EXTRALIBS=%PL!GMP_LIB% %BUFFEROVERFLOW%"
:end_set_extra_libs

:: MSVC Runtime
:msvcrt8_sdk
if not "%VCXX%"=="VC08_SDK" (goto end_msvcrt8_sdk)
set "PL!MSVCRTDIR=%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\Bin\win64"
set "PL!MSVCRT=msvcr80.dll"
:end_msvcrt8_sdk
:msvcrt8_studio
if not "%VCXX%"=="VC08_STU" (goto end_msvcrt8_studio)
if "%BITS%"=="32" (set "PL!MSVCRTDIR=%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\redist\x86\Microsoft.VC80.CRT") else (set "PL!MSVCRTDIR=%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\redist\amd64\Microsoft.VC80.CRT")
set "PL!MSVCRT=msvcr80.dll"
:end_msvcrt8_studio
:msvcrt9
if not "%VCXX%"=="VC09" (goto end_msvcrt9)
if "%BITS%"=="32" (set "PL!MSVCRTDIR=%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\redist\x86\Microsoft.VC90.CRT") else (set "PL!MSVCRTDIR=%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\redist\amd64\Microsoft.VC90.CRT")
set "PL!MSVCRT=msvcr90.dll"
:end_msvcrt9
:msvcrt10
if not "%VCXX%"=="VC10" (goto end_msvcrt10)
if "%BITS%"=="32" (set "PL!MSVCRTDIR=%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\redist\x86\Microsoft.VC100.CRT") else (set "PL!MSVCRTDIR=%PL!PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\redist\x64\Microsoft.VC100.CRT")
set "PL!MSVCRT=msvcr100.dll"
:end_msvcrt10


if exist "%PL!PROGRAM_FILES_32%\NSIS\MakeNSIS.exe" (set "NSIS=%PL!PROGRAM_FILES_32%\NSIS\MakeNSIS.exe")
if exist "%PL!PROGRAM_FILES_32%\NSIS\MakeNSIS.exe" (set "NSISDEFS=/DWIN%BITS% /DMSVCRT=$(MSVCRT)")
if "%VCXX%_%TYPE%" GEQ "VC08_STU" (SET "PL!VC_VERSION=VC8_OR_MORE") else (SET "PL!VC_VERSION=VC7_OR_LESS")
:end_set_additional_variables


:get_pre-requisites
set "PRE-REQ_LOCA=http://www.swi-prolog.org/download/MS-Windows/"
set "PRE-REQ_FILE=reqs%BITS%.cab"
if exist "%PRE-REQ_FILE%" (goto end_get_pre-requisites)
set "NO_TRIES=0"
:get_pre-requisites_start
echo set oHTTP = WScript.CreateObject("Microsoft.XMLHTTP") > reqs%BITS%.vbs
echo oHTTP.open "GET"^, "%PRE-REQ_LOCA%" ^& "%PRE-REQ_FILE%"^, False >> reqs%BITS%.vbs
echo oHTTP.send >> reqs%BITS%.vbs
echo set oStream = createobject("adodb.stream") >> reqs%BITS%.vbs
echo oStream.type = 1 >> reqs%BITS%.vbs
echo oStream.open >> reqs%BITS%.vbs
echo oStream.write oHTTP.responseBody >> reqs%BITS%.vbs
echo oStream.savetofile "%PRE-REQ_FILE%"^, 2 >> reqs%BITS%.vbs
echo set oStream = nothing >> reqs%BITS%.vbs
echo set oHTTP = nothing >> reqs%BITS%.vbs
goto execute_get_pre-requisites_script
:execute_get_pre-requisites_script_wait
ping -n 5 localhost > nul
:execute_get_pre-requisites_script
reqs%BITS%.vbs
if exist %PRE-REQ_FILE% (goto get_pre-requisites_succes)
set /A NO_TRIES+=1
cls
type welcome.txt
echo.
echo Download of the SWI-Prolog %BITS%-bit Pre-requisites failed on try %NO_TRIES% (of 3),
echo trying again in about 5 seconds...
echo.
if %NO_TRIES% LEQ 3 (goto execute_get_pre-requisites_script_wait)
cls
type welcome.txt
echo.
echo Download of the SWI-Prolog %BITS%-bit Pre-requisites failed permanently, exiting...
echo.
pause
goto end_no_pause
:get_pre-requisites_succes
cls
type welcome.txt
echo.
echo Downloaded the SWI-Prolog %BITS%-bit Pre-requisites...
echo.
if exist reqs%BITS%.vbs (del /Q reqs%BITS%.vbs)
:end_get_pre-requisites


:start_expanding_pre-requisites
if exist "%PL!HOME%\bin" (rd /S /Q "%PL!HOME%\bin")
md "%PL!HOME%\bin"
if exist "%PL!HOME%\include" (rd /S /Q "%PL!HOME%\include")
md "%PL!HOME%\include"
expand "reqs%BITS%.cab" -F:include.cab "%PL!HOME%\include" > nul
expand "%PL!HOME%\include\include.cab" -F:* "%PL!HOME%\include" > nul
if exist "%PL!HOME%\include\include.cab" (del /Q "%PL!HOME%\include\include.cab") > nul
if exist "%PL!HOME%\lib" (rd /S /Q "%PL!HOME%\lib")
md "%PL!HOME%\lib"
expand "reqs%BITS%.cab" -F:lib.cab "%PL!HOME%\lib" > nul
expand "%PL!HOME%\lib\lib.cab" -F:* "%PL!HOME%\lib" > nul
if exist "%PL!HOME%\lib\lib.cab" (del /Q "%PL!HOME%\lib\lib.cab") > nul
echo Expanded the Pre-requisites...
echo.
:end_start_expanding_pre-requisites


:clean_and_copy_user_libs
if exist "%PL!HOME%\%PL!TARGET_OS_ARCH%\*.*" (xcopy "%PL!HOME%\%PL!TARGET_OS_ARCH%\*.*" "%PL!HOME%\" /E /I /Q /R /Y > nul)
if exist "%PL!TARGET_PROGRAM_FILES%\pl" (rd /S /Q "%PL!TARGET_PROGRAM_FILES%\pl" > nul)
:end_clean_and_copy_user_libs


:detect_gmp
if not exist "%PL!HOME%\lib\gmp.lib" (goto detect_gmp_not_detected)
if not exist "%PL!HOME%\include\gmp.h" (goto detect_gmp_not_detected)
goto detect_gmp_detected
:detect_gmp_not_detected
cls
type welcome.txt
echo.
echo GMP (or one of its' components) not found...
echo.
echo The build will continue without GMP, it's functionality will not be
echo available. If GMP is required, install GMP and run this script again.
echo.
pause
set "PL!GMP=false"
set "PL!GMP_LIB="
goto end_detect_gmp
:detect_gmp_detected
set "PL!GMP=true"
set "PL!GMP_LIB=gmp.lib"
:end_detect_gmp


:detect_zlib
if not exist "%PL!HOME%\lib\%PL!LIBZLIB%.lib" (goto detect_zlib_not_detected)
if not exist "%PL!HOME%\lib\%PL!LIBZLIB%.dll" (goto detect_zlib_not_detected)
if not exist "%PL!HOME%\include\zlib.h" (goto detect_zlib_not_detected)
goto detect_zlib_detected
:detect_zlib_not_detected
cls
type welcome.txt
echo.
echo ZLIB (or one of its' components) not found...
echo.
echo The build will continue without ZLIB, it's functionality will not be
echo available. If ZLIB is required, install ZLIB and run this script again.
echo.
pause
set "PL!BUILD_ZLIB="
goto end_detect_zlib
:detect_zlib_detected
set "PL!BUILD_ZLIB=zlib"
:end_detect_zlib


:detect_ssl
if exist "%PL!TARGET_PROGRAM_FILES%\OpenSSL" (goto detect_ssl_pf_detected)
if exist "%SystemDrive%\OpenSSL" (goto detect_ssl_sd_detected)
cls
type welcome.txt
echo.
echo OpenSSl must be installed in "Program Files". For building a 32-bit target on a
echo 64-bit host OS, OpenSSL must be installed in "Program Files (x86)". During
echo installation, you are asked where to install the executables. They should
echo be installed in the OpenSSL directory. This is NOT the default!
echo.
echo OpenSSL not found, OpenSSL must be installed in %PL!TARGET_PROGRAM_FILES%\OpenSSL.
echo.
echo The OpenSSL for Windows packages can be downloaded at:
echo http://www.slproweb.com/products/Win32OpenSSL.html. Choose the full (i.e. not
echo light) distribution package.
echo.
echo The build will continue without OpenSSL, it's functionality will not be
echo available. If OpenSSL is required, install OpenSSL following the above
echo instructions and run this script again.
echo.
set "PL!BUILD_SSL="
pause
goto end_detect_ssl
:detect_ssl_pf_detected
set "PL!BUILD_SSL=ssl"
set "PL!OPENSSL_CONF=%PL!TARGET_PROGRAM_FILES%\OpenSSL\bin\openssl.cfg"
goto end_detect_ssl
:detect_ssl_sd_detected
set "PL!BUILD_SSL=ssl"
set "PL!OPENSSL_CONF=%SystemDrive%\OpenSSL\bin\openssl.cfg"
goto end_detect_ssl
:end_detect_ssl


:detect_java
if exist "%PL!TARGET_PROGRAM_FILES%\Java\jdk*" (goto detect_java_pf_detected)
if exist "%SystemDrive%\Java\jdk*" (goto detect_java_sd_detected)
if exist "%SystemDrive%\jdk*" (goto detect_java_sdnjd_detected)
cls
type welcome.txt
echo.
echo Java JDK not found...
echo.
echo The build will continue without the Java package, it's functionality will not
echo be available. If Java is required, install the Java JDK and run this script
echo again.
echo.
pause
set "PLBUILD_JPL="
goto end_detect_java
:detect_java_pf_detected
for /F %%_ in ('dir "%PL!TARGET_PROGRAM_FILES%\Java\jdk*" /A:D /B') do (set "PL!JAVA_JDK_VERSION=%%_")
set "PL!JAVA_HOME=%PL!TARGET_PROGRAM_FILES%\Java\%PL!JAVA_SDK_VERSION%"
goto detect_java_detected
:detect_java_sd_detected
for /F %%_ in ('dir "%SystemDrive%\Java\jdk*" /A:D /B') do (set "PL!JAVA_JDK_VERSION=%%_")
set "PL!JAVA_HOME=%SystemDrive%\Java\%PL!JAVA_SDK_VERSION%"
goto detect_java_detected
:detect_java_sdnjd_detected
for /F %%_ in ('dir "%SystemDrive%\jdk*" /A:D /B') do (set "PL!JAVA_JDK_VERSION=%%_")
set "PL!JAVA_HOME=%SystemDrive%\%PL!JAVA_SDK_VERSION%"
goto detect_java_detected
:detect_java_detected
if not exist "%PL!HOME%\lib\junit.jar" (goto detect_junit_jar_not_detected)
set "PL!JUNIT=%PL!HOME%\lib\junit.jar"
set "PL!BUILD_JPL=jpl"
goto end_detect_java
:detect_junit_jar_not_detected
cls
type welcome.txt
echo.
echo Java %PL!JAVA_JDK_VERSION% found, but %PL!HOME%\lib\junit.jar not found...
echo.
echo The build will continue without the Java package, it's functionality will not
echo be available. If Java is required, install the Java JDK and run this script
echo again.
echo.
pause
set "PL!BUILD_JPL="
:end_detect_java


:export_environment
if exist "config.dat" (del /Q "config.dat")
for /F "delims=! tokens=2*" %%_ in ('set PL!') do (echo %%_ >> "config.dat")
:end_export_environment


:build_now
cls
type welcome.txt
echo.
set "ANSWER=enter"
set /P "ANSWER=Would you like to build and install SWI-Prolog %BITS%-bits now? (y or n): "
if "%ANSWER%"=="y" (goto build_now_answer_yes)
if "%ANSWER%"=="n" (goto build_now_answer_no)
if "%ANSWER%"=="Y" (goto build_now_answer_yes)
if "%ANSWER%"=="N" (goto build_now_answer_no)
if "%ANSWER%"=="enter" (goto build_now_answer_yes)
echo The only choices possible are y/Y and n/N or [enter]!
goto build_now
:build_now_answer_no
cls
type welcome.txt
echo.
echo The environment variables are setup as follows:
echo.
type config.dat
echo.
goto end_build_now
:build_now_answer_yes
call automake.cmd
goto end_no_pause
:end_build_now


:end
pause
:end_no_pause
if exist welcome.txt (del /Q welcome.txt > nul)

ENDLOCAL

