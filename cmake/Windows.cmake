if(WIN32)

add_compile_options(-D__WINDOWS__)
if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  add_compile_options(-DWIN64)
endif()

# Separator for swipl -p alias=dir<sep>dir...
set(SWIPL_PATH_SEP "\;")
set(SO_PATH PATH)

set(SRC_OS_SPECIFIC pl-nt.c pl-ntconsole.c pl-dde.c os/windows/uxnt.c)
set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} winmm.lib)

if(NOT DEFINED WIN32_DLLS)
function(find_file_from_pattern var dir pattern)
  file(GLOB files ${dir}/${pattern})
  list(LENGTH files len)
  if(len EQUAL 1)
    get_filename_component(file ${files} NAME)
    set(${var} ${file} PARENT_SCOPE)
  elseif(len EQUAL 0)
    message(FATAL_ERROR "Cannot file ${dir}/${pattern}")
  else()
    message(FATAL_ERROR "${dir}/${pattern} is ambigous: ${files}")
  endif()
endfunction()

set(WIN32_DLL_PATTERNS
    "libwinpthread*.dll"
    "libgcc_s*.dll"
    "zlib*.dll"
    "libgmp*.dll")

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

find_windows_dlls(WIN32_DLLS ${WIN32_DLL_PATTERNS})
message("-- MinGW dlls to include: ${WIN32_DLLS}")

foreach(dll ${WIN32_DLLS})
  file(COPY ${MINGW_ROOT}/bin/${dll}
       DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endforeach()

set(WIN32_DLLS ${WIN32_DLLS} CACHE INTERNAL "WIN32 DLLs to copy")
endif(NOT DEFINED WIN32_DLLS)

endif(WIN32)
