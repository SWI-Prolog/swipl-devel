# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=plwin - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to plwin - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "plwin - Win32 Release" && "$(CFG)" != "plwin - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "plwin.mak" CFG="plwin - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "plwin - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "plwin - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "plwin - Win32 Debug"
CPP=cl.exe
RSC=rc.exe
MTL=mktyplib.exe

!IF  "$(CFG)" == "plwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "d:\development\lib"
# PROP Intermediate_Dir "d:\development\objects\plwin"
OUTDIR=d:\development\lib
INTDIR=d:\development\objects\plwin

ALL : "$(OUTDIR)\plwin.exe"

CLEAN : 
	-@erase "$(INTDIR)\pl-ntmain.obj"
	-@erase "$(INTDIR)\pl.res"
	-@erase "$(OUTDIR)\plwin.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

$(OUTDIR)/plwin.bsc : $(OUTDIR)  $(BSC32_SBRS)
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN" /Fp"$(INTDIR)/plwin.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=d:\development\objects\plwin/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/pl.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/plwin.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 uxnt.lib libpl.lib console.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=uxnt.lib libpl.lib console.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/plwin.pdb" /machine:I386 /out:"$(OUTDIR)/plwin.exe" 
LINK32_OBJS= \
	"$(INTDIR)\pl-ntmain.obj" \
	"$(INTDIR)\pl.res"

"$(OUTDIR)\plwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "plwin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "d:\development\lib"
# PROP Intermediate_Dir "d:\development\objects\plwin"
OUTDIR=d:\development\lib
INTDIR=d:\development\objects\plwin

ALL : "$(OUTDIR)\plwin.exe"

CLEAN : 
	-@erase "$(INTDIR)\pl-ntmain.obj"
	-@erase "$(INTDIR)\pl.res"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\plwin.exe"
	-@erase "$(OUTDIR)\plwin.ilk"
	-@erase "$(OUTDIR)\plwin.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

$(OUTDIR)/plwin.bsc : $(OUTDIR)  $(BSC32_SBRS)
# ADD BASE CPP /nologo /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "d:\development\pl\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "d:\development\pl\include" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN"\
 /Fp"$(INTDIR)/plwin.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=d:\development\objects\plwin/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/pl.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/plwin.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 uxnt.lib libpl.lib console.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=uxnt.lib libpl.lib console.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/plwin.pdb" /debug /machine:I386 /out:"$(OUTDIR)/plwin.exe" 
LINK32_OBJS= \
	"$(INTDIR)\pl-ntmain.obj" \
	"$(INTDIR)\pl.res"

"$(OUTDIR)\plwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "plwin - Win32 Release"
# Name "plwin - Win32 Debug"

!IF  "$(CFG)" == "plwin - Win32 Release"

!ELSEIF  "$(CFG)" == "plwin - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=".\pl-ntmain.c"
DEP_CPP_PL_NT=\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	

"$(INTDIR)\pl-ntmain.obj" : $(SOURCE) $(DEP_CPP_PL_NT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\pl.rc

"$(INTDIR)\pl.res" : $(SOURCE) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
