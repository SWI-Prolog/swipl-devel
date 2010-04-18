@echo off

:: Powered by degski

SETLOCAL
set _BITS=32
echo SWI-Prolog Pre-requisites %_BITS%-bits Archiver 2010
echo.
echo Busy...

:: Vars
set "BASE=%CD%\"
set "STRIPPER="%BASE:~3%"

:: Doit
echo "files.txt" files.txt > tmp.txt
if exist files.txt (del /Q files.txt)
for /F "delims=" %%1 in ('dir /B /A:D') do (call :add_sd_files %%1)
cabarc -m LZX:21 n reqs%_BITS%.cab @tmp.txt > nul

:: Clean
if exist tmp.txt (del /Q tmp.txt)
if exist files.txt (del /Q files.txt)
goto :end

:add_sd_files
pushd %1
for /F "delims=" %%1 in ('dir /S /B /A:-D') do (call :add_file "%%1")
popd
goto:eof

:add_file
set _TMP=%~1
call set _RES=%%_TMP:%BASE%=%%
echo "%_RES%" >> "%BASE%files.txt"
echo %1 %_RES% >> "%BASE%tmp.txt"
goto:eof

:end
ENDLOCAL
