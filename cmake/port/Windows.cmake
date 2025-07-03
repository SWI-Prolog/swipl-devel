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

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ws2_32)

if(MINGW_ROOT)
  include(port/MinGW)
elseif(CONDA)
  include(port/CondaWindows)
elseif(MSYS2)
  include(port/MSYS2)
endif()

if(NOT PLHOME)
  set(PLHOME "c:/${WIN_PROGRAM_FILES}/swipl")
endif()

# Separator for swipl -p alias=dir<sep>dir...
set(SWIPL_PATH_SEP "\;")
set(SO_PATH PATH)

set(SRC_OS_SPECIFIC pl-nt.c pl-ntconsole.c pl-dde.c os/windows/uxnt.c)
set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} winmm.lib ws2_32.lib psapi.lib)

if(MINGW_ROOT)
set(MINGW_BIN "${MINGW_ROOT}/bin")
# For MinGW we need to copy the   dlls  to the target destinations. When
# using MSVC we use vcpkg. The  included toolchain installs the required
# DLLs for us.

function(add_mingw_indirect_deps result_list)
  set(queue "")
  set(visited "")
  set(found "")
  
  set(initial_dlls ${ARGN})

  foreach(dll IN LISTS initial_dlls)
    if(IS_ABSOLUTE "${dll}")
      list(APPEND queue "${dll}")
    else()
      file(GLOB dll_path "${MINGW_BIN}/${dll}")
      if(NOT dll_path)
	message(FATAL_ERROR "Initial DLL '${dll}' not found in ${MINGW_BIN}")
      endif()
      list(APPEND queue "${dll_path}")
    endif()
  endforeach()
  set(found ${queue})

  while(queue)
    list(GET queue 0 current)
    list(REMOVE_AT queue 0)

    list(FIND visited "${current}" already_idx)
    if(NOT already_idx EQUAL -1)
      continue()
    endif()
    list(APPEND visited "${current}")
    
    execute_process(
      COMMAND x86_64-w64-mingw32-objdump -p "${current}"
      OUTPUT_VARIABLE objdump_out
      ERROR_QUIET
      RESULT_VARIABLE objdump_res
    )

    if(NOT objdump_res EQUAL 0)
      message(WARNING "Failed to objdump ${current}")
      continue()
    endif()

    string(REGEX MATCHALL "DLL Name:[^\n\r]*" dll_lines "${objdump_out}")
    foreach(line IN LISTS dll_lines)
      string(REGEX REPLACE ".*DLL Name:[ \t]*" "" dep_name "${line}")

      file(GLOB dep_path "${MINGW_BIN}/${dep_name}")
      if(dep_path)
        list(FIND visited "${dep_path}" visited_idx)
        if(visited_idx EQUAL -1)
          list(APPEND queue "${dep_path}")
        endif()
        list(APPEND found "${dep_path}")
      endif()
    endforeach()
  endwhile()

  list(REMOVE_DUPLICATES found)
  set(${result_list} "${found}" PARENT_SCOPE)
endfunction()

# Find the DLLs required immediately by a target 

function(get_target_mingw_dlls result_list target)
  get_target_property(libs ${target} LINK_LIBRARIES)

  if(NOT libs)
    message(WARNING "Target '${target}' has no link libraries")
    set(${result_list} "" PARENT_SCOPE)
    return()
  endif()

  set(dlls "")
  
  if(CMAKE_BUILD_TYPE)
    string(TOUPPER "${CMAKE_BUILD_TYPE}" config)
  else()
    set(config RELWITHDEBINFO)
  endif()

  foreach(lib IN LISTS libs)
    if(TARGET ${lib})
      get_target_property(is_imported ${lib} IMPORTED)
      if(is_imported)
        get_target_property(loc ${lib} IMPORTED_LOCATION_${config})
	if(NOT loc)
	  get_target_property(loc "${lib}" IMPORTED_LOCATION)
	endif()
	if(NOT loc)
	  get_target_property(loc "${lib}" LOCATION_${config})
	endif()
	if(NOT loc)
	  get_target_property(loc "${lib}" LOCATION)
	endif()

        if(loc)
          file(TO_CMAKE_PATH "${loc}" lib_path)
	else()
	  get_target_property(llibs "${lib}" INTERFACE_LINK_LIBRARIES)
	  foreach(llib ${llibs})
	    get_filename_component(lname "${llib}" NAME_WE)
	    file(GLOB dll "${MINGW_BIN}/${lname}*.dll")
	    list(APPEND dlls ${dll})
	  endforeach()
        endif()
      else()
	file(TO_CMAKE_PATH "${lib}" lib_path)
      endif()

      if(lib_path MATCHES "${MINGW_BIN}/.*\\.dll$")
        list(APPEND dlls "${lib_path}")
      endif()
    endif()
  endforeach()

  list(REMOVE_DUPLICATES dlls)
  set(${result_list} "${dlls}" PARENT_SCOPE)
endfunction()


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
