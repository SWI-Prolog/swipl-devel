# Include in all Prolog packages

# Get cmake files from this package, the package infrastructure and
# SWI-Prolog overall
set(CMAKE_MODULE_PATH
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/../cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")

# CMake modules we always need
include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckSymbolExists)

# Arity is of size_t.  This should now be the case for all packages
set(PL_ARITY_AS_SIZE 1)

if(NOT SWIPL_ROOT)
  get_filename_component(SWIPL_ROOT ../.. ABSOLUTE)
endif()
if(NOT SWIPL_INSTALL_DIR)
  set(SWIPL_INSTALL_DIR swipl)
endif()
if(NOT SWIPL_INSTALL_PREFIX)
  set(SWIPL_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX}/lib/${SWIPL_INSTALL_DIR})
endif()
if(NOT SWIPL_ARCH)
  string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
	 SWIPL_ARCH)
endif()
if(NOT DEFINED O_PLMT)
  set(O_PLMT 1)
  set(_REENTRANT 1)
endif()
string(REGEX REPLACE "^.*-" "" SWIPL_PKG ${PROJECT_NAME})

# get SWI-Prolog.h and SWI-Stream.h
include_directories(BEFORE ${SWIPL_ROOT}/src ${SWIPL_ROOT}/src/os)
if(WIN32)
  include_directories(BEFORE
		      ${SWIPL_ROOT}/src/os/windows
		      ${SWIPL_ROOT}/src/win32/console)
endif()
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})

if(CMAKE_EXECUTABLE_FORMAT STREQUAL "ELF")
  set(SWIPL_LIBRARIES "")
else()
  set(SWIPL_LIBRARIES libswipl)
endif()

if(NOT SWIPL_INSTALL_MODULES)
  if(WIN32)
    set(SWIPL_INSTALL_MODULES ${SWIPL_INSTALL_PREFIX}/bin)
  else()
    set(SWIPL_INSTALL_MODULES ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
  endif()
endif()
if(NOT SWIPL_INSTALL_LIBRARY)
  set(SWIPL_INSTALL_LIBRARY ${SWIPL_INSTALL_PREFIX}/library)
endif()
if(NOT SWIPL_INSTALL_INCLUDE)
  set(SWIPL_INSTALL_INCLUDE ${SWIPL_INSTALL_PREFIX}/include)
endif()

# swipl_plugin(name
#	       [C_SOURCES file ...]
#	       [C_LIBS lib ...]
#	       [PL_LIB_SUBDIR subdir]
#	       [PL_LIBS file ...])

function(swipl_plugin name)
  set(target "plugin_${name}")
  set(c_sources)
  set(c_libs)
  set(pl_libs)
  set(pl_lib_subdir)

  set(mode)

  foreach(arg ${ARGN})
    if(arg STREQUAL "C_SOURCES")
      set(mode c_sources)
    elseif(arg STREQUAL "C_LIBS")
      set(mode c_libs)
    elseif(arg STREQUAL "PL_LIBS")
      set(mode pl_libs)
    elseif(arg STREQUAL "PL_LIB_SUBDIR")
      set(mode pl_lib_subdir)
    else()
      set(${mode} ${${mode}} ${arg})
    endif()
  endforeach()

  if(c_sources)
    add_library(${target} MODULE ${c_sources})
    set_target_properties(${target} PROPERTIES OUTPUT_NAME ${name} PREFIX "")
    target_link_libraries(${target} ${c_libs} ${SWIPL_LIBRARIES})

    install(TARGETS ${target}
	    LIBRARY DESTINATION ${SWIPL_INSTALL_MODULES})
  endif()

  install(FILES ${pl_libs}
	  DESTINATION ${SWIPL_INSTALL_LIBRARY}/${pl_lib_subdir})
endfunction(swipl_plugin)

# install_dll(file ...)
#
# Install support DLL files.  This function is normally passes the link
# library used to build the dll.  It determines the dll from this link
# library and copies this both to the current binary and final installation
# tree.

function(install_dll)
if(WIN32)
  set(dlls)

  foreach(lib ${ARGN})
    set(dll)

    if(lib MATCHES "\\.lib$")
      string(REPLACE ".lib" ".dll" dll ${lib})
    elseif(lib MATCHES "\\.dll$")
      set(dll ${lib})
    elseif(lib MATCHES "\\.dll\\.a$")
      string(REPLACE ".dll.a" ".la" la ${lib})
      if(EXISTS ${la})
	file(READ ${la} la_content)
	string(REGEX MATCH "dlname='[-._a-zA-Z/0-9]*'" line ${la_content})
	string(REGEX REPLACE "^dlname='(.*)'" "\\1" dlname ${line})
	get_filename_component(dir ${lib} DIRECTORY)
	get_filename_component(dll ${dir}/${dlname} ABSOLUTE)
      else()
        get_filename_component(base ${lib} NAME_WE)
        file(STRINGS ${lib} dlname REGEX "${base}.*\\.dll$")
	get_filename_component(dir ${lib} DIRECTORY)
	get_filename_component(dll ${dir}/../bin/${dlname} ABSOLUTE)
      endif()
    endif()
    if(dll)
      set(dlls ${dlls} ${dll})
    else()
      message("Could not find DLL from ${lib}")
    endif()
  endforeach()

  file(COPY ${dlls}
       DESTINATION ${CMAKE_BINARY_DIR}/src)
  install(FILES ${dlls}
	  DESTINATION ${SWIPL_INSTALL_ARCH_EXE})
endif()
endfunction()

# swipl_examples(file ... [SUBDIR dir])
#
# Install the examples

function(swipl_examples)
  set(mode mfiles)
  set(files)
  set(dirs)
  set(subdir)

  foreach(arg ${ARGN})
    if(arg STREQUAL "SUBDIR")
      set(mode msubdir)
    elseif(arg STREQUAL "FILES")
      set(mode mfiles)
    elseif(arg STREQUAL "DIRECTORIES")
      set(mode mdirectories)
    elseif(mode STREQUAL "msubdir")
      set(subdir ${arg})
    elseif(mode STREQUAL "mfiles")
      set(files ${files} ${arg})
    elseif(mode STREQUAL "mdirectories")
      set(dirs ${dirs} ${arg})
    endif()
  endforeach()

  set(extdest ${SWIPL_INSTALL_PREFIX}/doc/packages/examples/${SWIPL_PKG})
  if(subdir)
    set(extdest ${extdest}/${subdir})
  endif()

  if(files)
    install(FILES ${files} DESTINATION ${extdest})
  endif()
  if(dirs)
    install(DIRECTORY ${dirs} DESTINATION ${extdest})
  endif()
endfunction()

# test_lib(name
#	   [PACKAGES ...]
#	   [PARENT_LIB])
#
# Run test_${name} in test_${name}.pl

if(NOT SWIPL_PATH_SEP)
  set(SWIPL_PATH_SEP ":")
endif()

function(test_lib name)
  set(test_source "test_${name}.pl")
  set(test_goal   "test_${name}")
  set(mode)
  set(packages)
  set(pforeign ${CMAKE_CURRENT_BINARY_DIR})
  set(plibrary ".")

  foreach(arg ${ARGN})
    if(arg STREQUAL "PACKAGES")
      set(mode "packages")
    elseif(arg STREQUAL "PARENT_LIB")
      set(plibrary "${plibrary}${sep}..")
    else()
      set(${mode} ${${mode}} ${arg})
    endif()
  endforeach()

  foreach(pkg ${packages})
    get_filename_component(src ${CMAKE_CURRENT_SOURCE_DIR}/../${pkg} ABSOLUTE)
    get_filename_component(bin ${CMAKE_CURRENT_BINARY_DIR}/../${pkg} ABSOLUTE)
    set(plibrary "${plibrary}${SWIPL_PATH_SEP}${src}")
    set(pforeign "${pforeign}${SWIPL_PATH_SEP}${bin}")
  endforeach()

  add_test(NAME "${SWIPL_PKG}:${name}"
	   COMMAND swipl -p "foreign=${pforeign}"
			 -p "library=${plibrary}"
			 -f none -s ${test_source}
			 -g "${test_goal}"
			 -t halt
	   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endfunction(test_lib)

# test_libs(name ...
#	    [PACKAGES package ...]
#	    [PARENT_LIB])

function(test_libs)
  set(mode tests)
  set(tests)
  set(packages)
  set(extra)

  foreach(arg ${ARGN})
    if(arg STREQUAL "PACKAGES")
      set(mode "packages")
    elseif(arg STREQUAL "PARENT_LIB")
      set(extra PARENT_LIB)
    else()
      set(${mode} ${${mode}} ${arg})
    endif()
  endforeach()

  foreach(test ${tests})
    test_lib(${test} PACKAGES ${packages} ${extra})
  endforeach()
endfunction(test_libs)

# has_package(var name)
# Set var to ON if the package name is included in the targets

function(has_package name var)
  list(FIND SWIPL_PACKAGE_LIST ${name} index)
  if ( ${index} GREATER -1 )
    set(var ON)
  else()
    set(var OFF)
  endif()
endfunction()

include(PackageDoc)
