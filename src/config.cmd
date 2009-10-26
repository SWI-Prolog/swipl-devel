
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
set "PL~WINDIR=%SystemRoot%"
if defined ProgramFiles(x86) (set "PL~HOST_OS_ARCH=X64") ELSE (set "PL~HOST_OS_ARCH=X86")
if "%PL~HOST_OS_ARCH%"=="X64" (set "PL~PROGRAM_FILES_32=%ProgramFiles(x86)%") else (set "PL~PROGRAM_FILES_32=%ProgramFiles%")
:end_what_host


:what_target
cls
echo Welcome to SWI-Prolog Windows Build Environment Configuration Utility
echo Copyright (c) 1990-2009 University of Amsterdam.
echo SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
echo and you are welcome to redistribute it under certain conditions.
echo Please visit http://www.swi-prolog.org for details.
echo.
set /P "BITS=Would you like to do a 32bit or 64bit build? (32 or 64): "
if "%BITS%"=="32" (goto what_target_bits_oke)
if "%BITS%"=="64" (goto what_target_bits_oke)
echo The only choices possible are 32 and 64!
goto what_target
:what_target_bits_oke
if "%BITS%"=="32" (set "PL~TARGET_OS_ARCH=X86") else (set "PL~TARGET_OS_ARCH=X64")
if "%BITS%"=="32" (set "PL~TARGET_PROGRAM_FILES=%PL~PROGRAM_FILES_32%") else (set "PL~TARGET_PROGRAM_FILES=%ProgramFiles%")
if "%BITS%"=="32" (set "PL~MD=WIN32") else (set "PL~MD=WIN64")
:end_what_target


:sanity_check
if not "%PL~HOST_OS_ARCH%%PL~TARGET_OS_ARCH%"=="X86X64" (goto end_sanity_check)
echo A 64-bit target install cannot be built on a 32-bit host operating system, exiting...)
goto end
:end_sanity_check


:set_vc_or_sdk_environment
::determine_vc_version
set "VCXX=UNKNOWN"
if exist "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio\VC98\" (set "VCXX=VC06")
if exist "%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\" (set "VCXX=VC08")
if exist "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 8\" (set "VCXX=VC08")
if exist "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\" (set "VCXX=VC09")
if exist "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\" (set "VCXX=VC10")
::set_vc6_environment_file
if "%VCXX%_%BITS%"=="VC06_32" (@echo echo. > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC06_64" (goto this_build_is_not_possible)
::set_vc71_environment_file
:: TBD
::set_vc8_or_sdk2003_R2_environment_file
if exist "%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\SetEnv.Cmd" (set "TYPE=SDK")
if exist "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\vcvarsall.bat" (set "TYPE=STU")
if "%VCXX%_%TYPE%_%BITS%"=="VC08_SDK_64" (@echo call "%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\SetEnv.Cmd" /X64 /RETAIL > call_vcvars.cmd)
if "%VCXX%_%TYPE%_%BITS%"=="VC08_SDK_32" (goto this_build_is_not_possible)
if "%VCXX%_%TYPE%_%BITS%"=="VC08_STU_64" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\vcvarsall.bat" x86_amd64 > call_vcvars.cmd)
if "%VCXX%_%TYPE%_%BITS%"=="VC08_STU_32" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\vcvarsall.bat" x86 > call_vcvars.cmd)
::set_vc9_or_sdk2008_sdkwin7_environment_file
if exist "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" (set "TYPE=STU")
if "%VCXX%_%TYPE%_%BITS%"=="VC09_STU_64" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_amd64 > call_vcvars.cmd)
if "%VCXX%_%TYPE%_%BITS%"=="VC09_STU_32" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86 > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC09_64" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\bin\vcvarsx86_amd64.bat" > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC09_32" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat" > call_vcvars.cmd)
::set_vc10_or_sdkwin7_environment_file
if "%VCXX%_%BITS%"=="VC10_64" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86_amd64 > call_vcvars.cmd)
if "%VCXX%_%BITS%"=="VC10_32" (@echo call "%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86 > call_vcvars.cmd)
:is_vc_version_determined
if not "%VCXX%"=="UNKNOWN" (goto set_vc_environment)
echo VC version could not be determined, exiting...
goto end
:this_build_is_not_possible
echo The requested %BITS%-bits build is not possible on %VCXX%, exiting...
goto end
:set_vc_environment
call call_vcvars.cmd
:end_set_vc_or_sdk_environment


:set_manifest_tool
set "PL~MTEXE=rem"
if "%VCXX%_%TYPE%"=="VC08_STU" (SET "PL~MTEXE=mt.exe -nologo")
if "%VCXX%"=="VC09" (SET "PL~MTEXE=mt.exe -nologo")
if "%PL~MTEXE%"=="rem" (goto end_set_manifest_tool)
:have_mt_exe in path
@for %%i in (mt.exe) do @if NOT "%%~$PATH:i"=="" (goto end_set_manifest_tool)
echo mt.exe cannot be found in the path, exiting...
goto end
:end_set_manifest_tool


:determine_plhome
set "_=%CD%"
cd ..
set "PL~HOME=%CD%"
cd \
set "PL~HOME_DRIVE=%CD%"
set "PL~LIB=%LIB%;%PL~HOME%\lib"
set "PL~INCLUDE=%INCLUDE%;%PL~HOME%\include"
set "PATH=%PATH%;%PL~HOME%\bin"
cd "%_%"
:end_determine_plhome


:set_additional_variables
set "PL~EXTRALIBDIR=%PL~HOME%\lib"
set "PL~PLBOOTFILE=boot%BITS%.prc"
if "%BITS%"=="32" (set "PL~LIBPTHREAD=pthreadVC") else (set "PL~LIBPTHREAD=pthreadVC2")
if "%BITS%"=="32" (set "PL~LIBZLIB=zlib1") else (set "PL~LIBZLIB=zlibwapi")
if "%BITS%"=="32" (set "PL~ARCH=i386-win32") else (set "PL~ARCH=x64-win64")
:set_extra_libs
if "%VCXX%_%TYPE%_%BITS%"=="VC08_SDK_64" (set "BUFFEROVERFLOW=bufferoverflowU.lib") else (set "BUFFEROVERFLOW=")
set "PL~EXTRALIBS=%PL~GMP_LIB% %BUFFEROVERFLOW%"
:end_set_extra_libs

:: MSVC Runtime
:msvcrt8_sdk
if not "%VCXX%"=="VC08_SDK" (goto end_msvcrt8_sdk)
set "PL~MSVCRTDIR=%ProgramFiles%\Microsoft Platform SDK for Windows Server 2003 R2\Bin\win64"
set "PL~MSVCRT=msvcr80.dll"
:end_msvcrt8_sdk
:msvcrt8_studio
if not "%VCXX%"=="VC08_STU" (goto end_msvcrt8_studio)
if "%BITS%"=="32" (set "PL~MSVCRTDIR=%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\redist\x86\Microsoft.VC80.CRT") else (set "PL~MSVCRTDIR=%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 8\VC\redist\amd64\Microsoft.VC80.CRT")
set "PL~MSVCRT=msvcr80.dll"
:end_msvcrt8_studio
:msvcrt9
if not "%VCXX%"=="VC09" (goto end_msvcrt9)
if "%BITS%"=="32" (set "PL~MSVCRTDIR=%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\redist\x86\Microsoft.VC90.CRT") else (set "PL~MSVCRTDIR=%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 9.0\VC\redist\amd64\Microsoft.VC90.CRT")
set "PL~MSVCRT=msvcr90.dll"
:end_msvcrt9
:msvcrt10
if not "%VCXX%"=="VC10" (goto end_msvcrt10)
if "%BITS%"=="32" (set "PL~MSVCRTDIR=%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\redist\x86\Microsoft.VC100.CRT") else (set "PL~MSVCRTDIR=%PL~PROGRAM_FILES_32%\Microsoft Visual Studio 10.0\VC\redist\x64\Microsoft.VC100.CRT")
set "PL~MSVCRT=msvcr100.dll"
:end_msvcrt10


if exist "%PL~PROGRAM_FILES_32%\NSIS\MakeNSIS.exe" (set "NSIS=%PL~PROGRAM_FILES_32%\NSIS\MakeNSIS.exe")
if exist "%PL~PROGRAM_FILES_32%\NSIS\MakeNSIS.exe" (set "NSISDEFS=/DWIN%BITS% /DMSVCRT=$(MSVCRT)")
if "%VCXX%_%TYPE%" GEQ "VC08_STU" (SET "PL~VC_VERSION=VC8_OR_MORE") else (SET "PL~VC_VERSION=VC7_OR_LESS")
:end_set_additional_variables

:: Assumes that the pre-requisites (gmp, jpeg, xpm, pthread, zlib, rfc882 and rfc2045) are present
:: in ../pl/x86/bin, ../pl/x86/include and ../pl/x86/lib (../pl/x64/bin, ../pl/x64/include and ../pl/x64/lib)."
:: The whole tree gets copied to the ../pl/ directory...
:clean_and_copy_libs
nmake /f makefile.mak distclean > nul
del "%PL~HOME%\bin\*.*" /Q /S > nul
del "%PL~HOME%\include\*.*" /Q /S > nul
del "%PL~HOME%\lib\*.*" /Q /S > nul
xcopy "%PL~HOME%\%PL~TARGET_OS_ARCH%\*.*" "%PL~HOME%\" /E /I /Q /R /Y
rd /S /Q "%PL~TARGET_PROGRAM_FILES%\pl" > nul
:end_clean_and_copy_libs


:detect_zlib
if not exist "%PL~HOME%\lib\%PL~LIBZLIB%.lib" (goto detect_zlib_not_detected)
if not exist "%PL~HOME%\lib\%PL~LIBZLIB%.dll" (goto detect_zlib_not_detected)
if not exist "%PL~HOME%\include\zlib.h" (goto detect_zlib_not_detected)
goto detect_zlib_detected
:detect_zlib_not_detected
echo ZLIB (or one of its' components) not found...
set "PL~BUILD_ZLIB="
goto end_detect_zlib
:detect_zlib_detected
echo zlib found...
set "PL~BUILD_ZLIB=zlib"
:end_detect_zlib


:detect_gmp
if not exist "%PL~HOME%\lib\gmp.lib" (goto detect_gmp_not_detected)
if not exist "%PL~HOME%\include\gmp.h" (goto detect_gmp_not_detected)
goto detect_gmp_detected
:detect_gmp_not_detected
echo GMP (or one of its' components) not found...
set "PL~GMP=false"
set "PL~GMP_LIB="
goto end_detect_gmp
:detect_gmp_detected
echo GMP found...
set "PL~GMP=true"
set "PL~GMP_LIB=gmp.lib"
:end_detect_gmp


:detect_ssl
if exist "%PL~TARGET_PROGRAM_FILES%\OpenSSL" (goto detect_ssl_pf_detected)
if exist "%SystemDrive%\OpenSSL" (goto detect_ssl_sd_detected)
cls
echo Welcome to SWI-Prolog Windows Build Environment Configuration Utility
echo Copyright (c) 1990-2009 University of Amsterdam.
echo SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
echo and you are welcome to redistribute it under certain conditions.
echo Please visit http://www.swi-prolog.org for details.
echo.
echo OpenSSl must be installed in "Program Files". For building a 32-bit target on a
echo 64-bit host OS, OpenSSL must be installed in "Program Files (x86)". During
echo installation, you are asked where to install the executables. They should
echo be installed in the OpenSSL directory. This is NOT the default!
echo.
echo OpenSSL not found, OpenSSL must be installed in %PL~TARGET_PROGRAM_FILES%\OpenSSL.
echo.
echo The OpenSSL for Windows packages can be downloaded at:
echo http://www.slproweb.com/products/Win32OpenSSL.html. Choose the full (i.e. not
echo light) distribution package.
echo.
echo The build will continue without OpenSSL, it's functionality will not be
echo available. If OpenSSL is required, install OpenSSL following the above
echo  instructions and run this script again.
echo.
set "PL~BUILD_SSL="
pause
goto end_detect_ssl
:detect_ssl_pf_detected
echo OpenSSL found...
set "PL~BUILD_SSL=ssl"
set "PL~OPENSSL=%PL~TARGET_PROGRAM_FILES%\OpenSSL"
set "PL~OPENSSL_CONF=%PL~TARGET_PROGRAM_FILES%\OpenSSL\bin\openssl.cfg"
goto detect_ssl_detected
:detect_ssl_sd_detected
echo OpenSSL found...
set "PL~BUILD_SSL=ssl"
set "PL~OPENSSL=%SystemDrive%\OpenSSL"
set "PL~OPENSSL_CONF=%SystemDrive%\OpenSSL\bin\openssl.cfg"
goto detect_ssl_detected
:detect_ssl_detected
if exist "%PL~OPENSSL%\lib\VC\libeay32.lib" (copy /Y "%PL~OPENSSL%\lib\VC\libeay32.lib" "%PL~EXTRALIBDIR%" > nul)
if exist "%PL~OPENSSL%\lib\VC\ssleay32.lib" (copy /Y "%PL~OPENSSL%\lib\VC\ssleay32.lib" "%PL~EXTRALIBDIR%" > nul)
:end_detect_ssl


:detect_java
if exist "%PL~TARGET_PROGRAM_FILES%\Java\jdk*" (goto detect_java_pf_detected)
if exist "%SystemDrive%\Java\jdk*" (goto detect_java_sd_detected)
if exist "%SystemDrive%\jdk*" (goto detect_java_sdnjd_detected)
echo Java JDK not found...
set "PLBUILD_JPL="
goto end_detect_java
:detect_java_pf_detected
for /F %%_ in ('dir "%PL~TARGET_PROGRAM_FILES%\Java\jdk*" /A:D /B') do (set "PL~JAVA_JDK_VERSION=%%_")
set "PL~JAVA_HOME=%PL~TARGET_PROGRAM_FILES%\Java\%PL~JAVA_SDK_VERSION%"
goto detect_java_detected
:detect_java_sd_detected
for /F %%_ in ('dir "%SystemDrive%\Java\jdk*" /A:D /B') do (set "PL~JAVA_JDK_VERSION=%%_")
set "PL~JAVA_HOME=%SystemDrive%\Java\%PL~JAVA_SDK_VERSION%"
goto detect_java_detected
:detect_java_sdnjd_detected
for /F %%_ in ('dir "%SystemDrive%\jdk*" /A:D /B') do (set "PL~JAVA_JDK_VERSION=%%_")
set "PL~JAVA_HOME=%SystemDrive%\%PL~JAVA_SDK_VERSION%"
goto detect_java_detected
:detect_java_detected
if not exist "%PL~HOME%\lib\junit.jar" (goto detect_junit_jar_not_detected)
set "PL~JUNIT=%PL~HOME%\lib\junit.jar"
echo Java %PL~JAVA_JDK_VERSION% found, jpl included in build...
set "PL~BUILD_JPL=jpl"
goto end_detect_java
:detect_junit_jar_not_detected
echo Java %PL~JAVA_JDK_VERSION% found, but %PL~HOME%\lib\junit.jar not found...
set "PL~BUILD_JPL="
:end_detect_java


:export_environment
if exist "config.dat" (del /Q config.dat)
for /F "delims=~ tokens=2*" %%_ in ('set PL~') do (echo %%_ >> config.dat)
:end_export_environment


:build_now
cls
echo Welcome to SWI-Prolog Windows Build Environment Configuration Utility
echo Copyright (c) 1990-2009 University of Amsterdam.
echo SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
echo and you are welcome to redistribute it under certain conditions.
echo Please visit http://www.swi-prolog.org for details.
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
echo.
echo The environment has now been set up, the configuration has been saved
echo to file config.dat in the current directory. automake.cmd starts the build
echo process. For consecutive builds (with the current setup), it's not
echo necessary to run this script again. When changes are made or when a different
echo target is required, run this script.
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

ENDLOCAL

