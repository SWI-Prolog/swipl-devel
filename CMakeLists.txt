cmake_minimum_required(VERSION 2.8.12)
project(SWI-Prolog)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(Version)
include(Ports)
include(Locations)
include(InstallSource)
include(QLF)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_install_prefix()
  message("-- Using install prefix \"${CMAKE_INSTALL_PREFIX}\"")
endif()

include(BuildType)
include(PackageSelection)

option(INSTALL_DOCUMENTATION
       "Install the HTML documentation files"
       ON)

# Verbosity
set(CMAKE_INSTALL_MESSAGE NEVER)

# Installation locations
string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
       SWIPL_ARCH)

set(SWIPL_INSTALL_DIR swipl)
if(WIN32)
set(SWIPL_INSTALL_PREFIX   ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_DIR})
set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin)
set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/bin)
set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/bin)
else()
set(SWIPL_INSTALL_PREFIX   ${CMAKE_INSTALL_PREFIX}/lib/${SWIPL_INSTALL_DIR})
set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH})
set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
endif()
set(SWIPL_INSTALL_LIBRARY  ${SWIPL_INSTALL_PREFIX}/library)
set(SWIPL_INSTALL_BOOT     ${SWIPL_INSTALL_PREFIX}/boot)
set(SWIPL_INSTALL_INCLUDE  ${SWIPL_INSTALL_PREFIX}/include)

if(MSVC)
  add_compile_options(/W3)
else()
  add_compile_options(-Wall)
endif()

if(UNIX)
  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST}
      libedit
      readline)
endif()

enable_testing()

# Add the core Prolog system
add_subdirectory(src)

# build the documentation
add_custom_target(
    doc
    COMMENT "Build the documentation")
add_subdirectory(man)

# Add the packages
foreach(package ${SWIPL_PACKAGE_LIST})
  if(EXISTS ${CMAKE_SOURCE_DIR}/packages/${package}/CMakeLists.txt)
    add_subdirectory(packages/${package})
  else()
    message("-- Skipping package ${package}")
  endif()
endforeach(package)

# Packaging
include(Pack)
