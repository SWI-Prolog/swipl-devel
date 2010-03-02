SETLOCAL

call call_vcvars.cmd

set "_=%CD%"
cd ..
for /F %%1 in ('dir *.exe /B /S /A:-D') do (mt.exe -manifest "%%1.manifest" -outputresource:"%%1";1)
for /F %%1 in ('dir *.dll /B /S /A:-D') do (mt.exe -manifest "%%1.manifest" -outputresource:"%%1";2)
cd %_%

ENDLOCAL

pause