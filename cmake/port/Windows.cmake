if(WIN32)

add_compile_options(-D__WINDOWS__)
add_compile_options(-D_WIN32_WINNT=0x0600)
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  add_compile_options(-DWIN64)
  set(WIN64 1)
  set(WIN_PROGRAM_FILES "Program Files")
else()
  set(WIN_PROGRAM_FILES "Program Files (x86)")
endif()

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ws2_32)

if(MINGW_ROOT)
  include(port/MinGW)
endif()

# Separator for swipl -p alias=dir<sep>dir...
set(SWIPL_PATH_SEP "\;")
set(SO_PATH PATH)

set(SRC_OS_SPECIFIC pl-nt.c pl-ntconsole.c pl-dde.c os/windows/uxnt.c)
set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} winmm.lib ws2_32.lib psapi.lib)

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

foreach(dll ${WIN32_DLLS})
  file(COPY ${MINGW_ROOT}/bin/${dll}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/src)
endforeach()

set(WIN32_DLLS ${WIN32_DLLS} CACHE INTERNAL "WIN32 DLLs to copy")
endif(NOT DEFINED WIN32_DLLS)

endif(WIN32)
