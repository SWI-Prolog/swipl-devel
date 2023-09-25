if(WIN32)

add_compile_options(-D__WINDOWS__)
add_compile_options(-D_WIN32_WINNT=0x0600)
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  add_compile_options(-DWIN64)
  set(WIN64 1)
  set(WIN_PROGRAM_FILES "Program Files")
  set(SWIPL_ARCH x64-win64)
else()
  set(WIN_PROGRAM_FILES "Program Files (x86)")
  set(SWIPL_ARCH i386-win32)
endif()
set(PLHOME "c:/${WIN_PROGRAM_FILES}/swipl")

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ws2_32)

if(MINGW_ROOT)
  include(port/MinGW)
elseif(DEFINED ENV{CONDA_BUILD})
  include(port/CondaWindows)
endif()

# Separator for swipl -p alias=dir<sep>dir...
set(SWIPL_PATH_SEP "\;")
set(SO_PATH PATH)

set(SRC_OS_SPECIFIC pl-nt.c pl-ntconsole.c pl-dde.c os/windows/uxnt.c)
set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} winmm.lib ws2_32.lib psapi.lib)

if(MINGW_ROOT)
# For MinGW we need to copy the   dlls  to the target destinations. When
# using MSVC we use vcpkg. The  included toolchain installs the required
# DLLs for us.

if(NOT DEFINED WIN32_DLLS)

function(find_file_from_pattern var dir pattern)
  file(GLOB files ${dir}/${pattern})
  message("   -- ${pattern}: ${files}")
  list(LENGTH files len)
  if(len EQUAL 1)
    get_filename_component(file ${files} NAME)
    set(${var} ${file} PARENT_SCOPE)
  elseif(len EQUAL 0)
    message(STATUS "Cannot file ${dir}/${pattern} - continuing")
  else()
    message(FATAL_ERROR "${dir}/${pattern} is ambiguous: ${files}")
  endif()
endfunction()

set(WIN32_DLL_PATTERNS zlib*.dll)

if(USE_GMP)
  list(APPEND WIN32_DLL_PATTERNS "libgmp-*.dll")
endif()
# libgcc_s_seh-1.dll needs the thread libraries?
if(MULTI_THREADED OR TRUE)
  list(APPEND WIN32_DLL_PATTERNS "*pthread*.dll")
endif()
if(MINGW)
  list(APPEND WIN32_DLL_PATTERNS "libgcc_s*.dll")
  list(APPEND WIN32_DLL_PATTERNS "libssp*.dll")
endif()

function(find_windows_dlls var)
   set(dlls)
   foreach(p ${ARGN})
     set(dll)
     find_file_from_pattern(dll ${MINGW_ROOT}/bin ${p})
     if(dll)
       set(dlls ${dlls} ${dll})
     endif()
   endforeach()
   set(${var} ${dlls} PARENT_SCOPE)
endfunction()

message("-- Finding required external DLLs")
find_windows_dlls(WIN32_DLLS ${WIN32_DLL_PATTERNS})
find_windows_dlls(WIN32_CPP_DLLS "libstdc*.dll")

foreach(dll ${WIN32_DLLS} ${WIN32_CPP_DLLS})
  file(COPY ${MINGW_ROOT}/bin/${dll}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src)
endforeach()

# Only install libstdc++.dll when installing the C++ tests.
# It raises the installer size from 13 to 17 Mb.
if(INSTALL_TESTS)
  list(APPEND WIN32_DLLS ${WIN32_CPP_DLLS})
endif()

set(WIN32_DLLS ${WIN32_DLLS} CACHE INTERNAL "WIN32 DLLs to copy")
endif(NOT DEFINED WIN32_DLLS)
endif(MINGW_ROOT)

endif(WIN32)
