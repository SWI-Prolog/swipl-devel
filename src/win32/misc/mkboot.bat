REM This bat-file recreates the SWI-Prolog boot-file boot32.prc from
REM the Prolog sources in the boot directory.  There are two reasons
REM for using this.  One is (of course) if you changed any files in
REM the boot directory and the other is if source-file information
REM on the bootfiles is not accurate.
REM
REM To run this, simply double-click it.

plwin.exe -O -o ..\boot32.prc -b ../boot/init.pl
