# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101
# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=plrc - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to plrc - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "plwin - Win32 Release" && "$(CFG)" != "plwin - Win32 Debug" &&\
 "$(CFG)" != "plrc - Win32 Release" && "$(CFG)" != "plrc - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "plwin.mak" CFG="plrc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "plwin - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "plwin - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "plrc - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "plrc - Win32 Debug" (based on "Win32 (x86) Console Application")
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

!IF  "$(CFG)" == "plwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "E:\pl\bin"
# PROP Intermediate_Dir "E:\objects\plwin"
OUTDIR=E:\pl\bin
INTDIR=E:\objects\plwin

ALL : "plrc - Win32 Release" "$(OUTDIR)\plwin.exe"

CLEAN : 
	-@erase "$(INTDIR)\pl-ntmain.obj"
	-@erase "$(INTDIR)\pl.res"
	-@erase "$(OUTDIR)\plwin.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

$(OUTDIR)/plwin.bsc : $(OUTDIR)  $(BSC32_SBRS)
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN" /Fp"$(INTDIR)/plwin.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=E:\objects\plwin/
CPP_SBRS=.\.

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

MTL=mktyplib.exe
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
RSC=rc.exe
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
# PROP Output_Dir "E:\pl\bin"
# PROP Intermediate_Dir "E:\objects\plwin"
OUTDIR=E:\pl\bin
INTDIR=E:\objects\plwin

ALL : "plrc - Win32 Debug" "$(OUTDIR)\plwin.exe"

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
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "d:\development\pl\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MD /W3 /Gm /GX /Zi /Od /I "d:\development\pl\include" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "PL_WIN"\
 /Fp"$(INTDIR)/plwin.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=E:\objects\plwin/
CPP_SBRS=.\.

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

MTL=mktyplib.exe
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
RSC=rc.exe
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

!ELSEIF  "$(CFG)" == "plrc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "plrc\Release"
# PROP BASE Intermediate_Dir "plrc\Release"
# PROP BASE Target_Dir "plrc"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "E:\pl\bin"
# PROP Intermediate_Dir "E:\objects\plrc"
# PROP Target_Dir "plrc"
OUTDIR=E:\pl\bin
INTDIR=E:\objects\plrc

ALL : "$(OUTDIR)\plrc.exe"

CLEAN : 
	-@erase "$(INTDIR)\access.obj"
	-@erase "$(INTDIR)\build.obj"
	-@erase "$(INTDIR)\html.obj"
	-@erase "$(INTDIR)\rc.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(OUTDIR)\plrc.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/plrc.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=E:\objects\plrc/
CPP_SBRS=.\.

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

RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/plrc.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/plrc.pdb" /machine:I386 /out:"$(OUTDIR)/plrc.exe" 
LINK32_OBJS= \
	"$(INTDIR)\access.obj" \
	"$(INTDIR)\build.obj" \
	"$(INTDIR)\html.obj" \
	"$(INTDIR)\rc.obj" \
	"$(INTDIR)\util.obj"

"$(OUTDIR)\plrc.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "plrc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "plrc\Debug"
# PROP BASE Intermediate_Dir "plrc\Debug"
# PROP BASE Target_Dir "plrc"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "E:\pl\bin"
# PROP Intermediate_Dir "E:\objects\plrc"
# PROP Target_Dir "plrc"
OUTDIR=E:\pl\bin
INTDIR=E:\objects\plrc

ALL : "$(OUTDIR)\plrc.exe"

CLEAN : 
	-@erase "$(INTDIR)\access.obj"
	-@erase "$(INTDIR)\build.obj"
	-@erase "$(INTDIR)\html.obj"
	-@erase "$(INTDIR)\rc.obj"
	-@erase "$(INTDIR)\util.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\plrc.exe"
	-@erase "$(OUTDIR)\plrc.ilk"
	-@erase "$(OUTDIR)\plrc.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE"\
 /Fp"$(INTDIR)/plrc.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=E:\objects\plrc/
CPP_SBRS=.\.

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

RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/plrc.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/plrc.pdb" /debug /machine:I386 /out:"$(OUTDIR)/plrc.exe" 
LINK32_OBJS= \
	"$(INTDIR)\access.obj" \
	"$(INTDIR)\build.obj" \
	"$(INTDIR)\html.obj" \
	"$(INTDIR)\rc.obj" \
	"$(INTDIR)\util.obj"

"$(OUTDIR)\plrc.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

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
################################################################################
# Begin Project Dependency

# Project_Dep_Name "plrc"

!IF  "$(CFG)" == "plwin - Win32 Debug"

"plrc - Win32 Debug" : 
   $(MAKE) /$(MAKEFLAGS) /F ".\plwin.mak" CFG="plrc - Win32 Debug" 

!ELSEIF  "$(CFG)" == "plwin - Win32 Release"

"plrc - Win32 Release" : 
   $(MAKE) /$(MAKEFLAGS) /F ".\plwin.mak" CFG="plrc - Win32 Release" 

!ENDIF 

# End Project Dependency
# End Target
################################################################################
# Begin Target

# Name "plrc - Win32 Release"
# Name "plrc - Win32 Debug"

!IF  "$(CFG)" == "plrc - Win32 Release"

!ELSEIF  "$(CFG)" == "plrc - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\rc\util.c
DEP_CPP_UTIL_=\
	".\config\win32.h"\
	".\rc\rc.h"\
	".\rc\rcutil.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\util.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rc\build.c
DEP_CPP_BUILD=\
	".\config\win32.h"\
	".\rc\html.h"\
	".\rc\rc.h"\
	".\rc\rcutil.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\build.obj" : $(SOURCE) $(DEP_CPP_BUILD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rc\html.c
DEP_CPP_HTML_=\
	".\config\win32.h"\
	".\rc\html.h"\
	

"$(INTDIR)\html.obj" : $(SOURCE) $(DEP_CPP_HTML_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rc\rc.c
DEP_CPP_RC_C8=\
	".\config\win32.h"\
	".\rc\rc.h"\
	

"$(INTDIR)\rc.obj" : $(SOURCE) $(DEP_CPP_RC_C8) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rc\access.c
DEP_CPP_ACCES=\
	".\config\win32.h"\
	".\rc\html.h"\
	".\rc\rc.h"\
	".\rc\rcutil.h"\
	{$(INCLUDE)}"\sys\stat.h"\
	{$(INCLUDE)}"\sys\types.h"\
	

"$(INTDIR)\access.obj" : $(SOURCE) $(DEP_CPP_ACCES) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
