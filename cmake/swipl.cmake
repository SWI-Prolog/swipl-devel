# This CMake include file is intended  to facilitate building SWI-Prolog
# packs  that  include  foreign  code  using   CMake.  It  provides  the
# following:
#
#   - A library target `swipl::libswipl`. If this is a public
#     installaton this is achieved by including
#     `<prefix>/lib/cmake/swipl/SWIPLTargets.cmake` which is created
#     by the SWI-Prolog build.  Else it is assembled here from the
#     environment variables that are provided by pack_install/1.
#   - If figures out whether modules need to be linked against
#     libswipl.so.  Notably on ELF platforms this is not needed,
#     which implies the module remains independent from the exact
#     Prolog version.
#   - It provides a function target_link_swipl(target) that uses
#     the above to either add `swipl::libswipl` to your target,
#     or, if linking is not needed, only the include directories.

if("$ENV{SWIPL_PACK_VERSION}" EQUAL 2)
  set(swipl_home_dir   $ENV{SWIPL_HOME_DIR})
  set(swipl_version    $ENV{SWIPL_VERSION})
  set(swipl_module_lib $ENV{SWIPL_MODULE_LIB})
elif($ENV{SWIHOME})		# Pack version 1
  set(swipl_home_dir   $ENV{SWIHOME})
  set(swipl_version    $ENV{SWIPLVERSION})
  set(swipl_module_lib $ENV{SWISOLIB})
else()				# Outside pack_install
  function(swipl_config)
    find_program(SWIPL swipl REQUIRED)
    execute_process(
      COMMAND ${SWIPL} --dump-runtime-variables
      OUTPUT_VARIABLE swipl_config)
    string(REGEX REPLACE "\n" ";" swipl_config_lines ${swipl_config})
    foreach(swipl_config_line ${swipl_config_lines})
      if(swipl_config_line MATCHES "^([A-Z_]*)=\"(.*)\"")
	set(${CMAKE_MATCH_1} ${CMAKE_MATCH_2} PARENT_SCOPE)
      endif()
    endforeach()
  endfunction()
  swipl_config()
  set(swipl_home_dir   ${PLBASE})
  set(swipl_version    ${PLVERSION})
  if(CMAKE_EXECUTABLE_FORMAT MATCHES ELF)
    set(swipl_module_lib})
  else()
    set(swipl_module_lib ${PLLIB)
  endif()
endif()

math(EXPR swipl_version_major "${swipl_version} / 10000")

if(${swipl_home_dir} MATCHES "/home$")
  cmake_path(GET swipl_home_dir PARENT_PATH swipl_build_dir)
  message("Using swipl from build directory ${swipl_build_dir}.")

  find_library(swipl_libs
    NAMES swipl
    PATHS "${swipl_build_dir}/src"
    NO_CMAKE_ENVIRONMENT_PATH
    NO_CMAKE_FIND_ROOT_PATH
    NO_CMAKE_PATH
    NO_CMAKE_SYSTEM_PATH
    NO_DEFAULT_PATH
    NO_PACKAGE_ROOT_PATH
    NO_SYSTEM_ENVIRONMENT_PATH)
  cmake_path(GET swipl_libs FILENAME swipl_soname)
  set(swipl_soname "${swipl_soname}.${swipl_version_major}")

  add_library(swipl::libswipl SHARED IMPORTED)
  set_target_properties(swipl::libswipl PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES ${swipl_home_dir}/include
    IMPORTED_LOCATION ${swipl_libs}
    IMPORTED_SONAME ${swipl_soname})
else()
  cmake_path(GET swipl_home_dir PARENT_PATH libdir)
  if(EXISTS "${libdir}/cmake/swipl/SWIPLTargets.cmake")
    message("Using CMake exports from ${libdir}/cmake/swipl")
    set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${libdir}/cmake/swipl")
    include(SWIPLTargets)
  endif()
endif()

if(swipl_module_lib)
  if("${swipl_module_lib}" STREQUAL "")
    set(SWIPL_MODULE_LINK_LIBSWIPL OFF)
  else()
    set(SWIPL_MODULE_LINK_LIBSWIPL ON)
  endif()
else()
  if(CMAKE_EXECUTABLE_FORMAT STREQUAL "ELF")
    set(SWIPL_MODULE_LINK_LIBSWIPL OFF)
  else()
    set(SWIPL_MODULE_LINK_LIBSWIPL ON)
  endif()
endif()

function(target_link_swipl target)
  if(SWIPL_MODULE_LINK_LIBSWIPL)
    message("Platform needs to link against libswipl.")
    target_link_libraries(${target} PRIVATE swipl::libswipl)
  else()
    message("Platform does not need to link against libswipl.")
    get_property(swipl_include_dir
      TARGET swipl::libswipl
      PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
    target_include_directories(${target}
      PRIVATE ${swipl_include_dir})
  endif()
  set_target_properties(${target} PROPERTIES
    OUTPUT_NAME ${target} PREFIX "")
endfunction()

# Avoid message on unused variable
set(SWIPL "${SWIPL}")
