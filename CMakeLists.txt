cmake_minimum_required(VERSION 2.8.12)
project(SWI-Prolog)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(Utils)
include(BuildType)
include(Version)
include(Ports)
include(Locations)
include(InstallSource)
include(QLF)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_install_prefix()
  message("-- Using install prefix \"${CMAKE_INSTALL_PREFIX}\"")
endif()

option(MULTI_THREADED
       "Enable multiple Prolog threads"
       ON)
option(USE_GMP
       "Use GNU MP Bignum library (GPL)"
       ON)
option(SWIPL_SHARED_LIB
       "Put kernel in a shared library"
       ON)
option(SWIPL_VERSIONED_DIR
       "Install into a versioned directory"
       OFF)
option(INSTALL_DOCUMENTATION
       "Install the HTML documentation files"
       ON)
option(BUILD_PDF_DOCUMENTATION
       "Build the PDF manuals from source"
       OFF)

include(PackageSelection)

# Verbosity
set(CMAKE_INSTALL_MESSAGE NEVER)

# Installation locations
string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
       SWIPL_ARCH)

if(WIN32)
set(SWIPL_INSTALL_DIR_DEF  ".")
set(SWIPL_INSTALL_PREFIX   ${SWIPL_INSTALL_DIR})
set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin)
set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/bin)
set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/bin)
# Place all .exe and .dll in one directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/src)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/src)
else(WIN32)
if(SWIPL_VERSIONED_DIR)
  set(SWIPL_INSTALL_DIR_DEF  swipl-${SWIPL_VERSION_STRING})
else()
  set(SWIPL_INSTALL_DIR_DEF  swipl)
endif()
set(SWIPL_INSTALL_PREFIX   lib/${SWIPL_INSTALL_DIR})
set(SWIPL_INSTALL_MANPAGES share/man/man1
    CACHE STRING "Directory for man pages")
set(SWIPL_INSTALL_PKGCONFIG share/pkgconfig
    CACHE STRING "Directory for pkg-config pages")
set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH})
set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
endif(WIN32)

set(SWIPL_INSTALL_DIR ${SWIPL_INSTALL_DIR_DEF}
    CACHE STRING "Directory below <prefix> for installation")

set(SWIPL_INSTALL_LIBRARY  ${SWIPL_INSTALL_PREFIX}/library)
set(SWIPL_INSTALL_BOOT     ${SWIPL_INSTALL_PREFIX}/boot)
set(SWIPL_INSTALL_INCLUDE  ${SWIPL_INSTALL_PREFIX}/include)

if(MSVC)
  add_compile_options(/W3)
else()
  add_compile_options(-Wall)
endif()

enable_testing()

# Configuration we need everywhere
if(MULTI_THREADED)
  find_package(Threads)
endif()

set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME "Core_system")

# Add the core Prolog system
add_subdirectory(src)
install(FILES LICENSE README.md DESTINATION ${SWIPL_INSTALL_PREFIX})

if(INSTALL_DOCUMENTATION)
  add_custom_target(
      doc ALL
      COMMENT "Build the documentation")

  add_custom_target(
      doc.html
      COMMENT "Build HTML documentation")

  add_dependencies(doc doc.html)

  if(BUILD_PDF_DOCUMENTATION)
    add_custom_target(
	doc.pdf
	COMMENT "Build PDF documentation")
    add_dependencies(doc doc.pdf doc.html)
  endif()

  set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME Documentation)
  add_subdirectory(man)
  install(FILES packages/index.html
	  DESTINATION ${SWIPL_INSTALL_PREFIX}/doc/packages)
endif(INSTALL_DOCUMENTATION)

# Add the packages
foreach(package ${SWIPL_PACKAGE_LIST})
  swipl_package_component(${package}
			  CMAKE_INSTALL_DEFAULT_COMPONENT_NAME)
  add_subdirectory(packages/${package})
endforeach(package)

# Check for environment variables that may cause the build to fail
include(CheckEnv)

# Packaging
include(Pack)
