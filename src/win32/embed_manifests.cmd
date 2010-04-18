@echo off
SETLOCAL

echo "Adding manifests to .DLLs and .EXEs"

call call_vcvars.cmd

set "_=%CD%"
cd ..
for /F "delims=" %%1 in ('dir *.exe /B /S /A:-D') do (if exist "%%1.manifest" (echo "%%1" && mt.exe -nologo -manifest "%%1.manifest" -outputresource:"%%1";1))
for /F "delims=" %%1 in ('dir *.dll /B /S /A:-D') do (if exist "%%1.manifest" (echo "%%1" && mt.exe -nologo -manifest "%%1.manifest" -outputresource:"%%1";2))
cd %_%

ENDLOCAL
