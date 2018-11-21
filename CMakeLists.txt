cmake_minimum_required(VERSION 2.8.12)
project(SWI-Prolog)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_POLICY_DEFAULT_CMP0075 NEW)

option(MULTI_THREADED
       "Enable multiple Prolog threads"
       ON)
option(USE_SIGNALS
       "Enable signal handling"
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
option(BUILD_MACOS_BUNDLE
       "Install for a MacOS bundle (SWI-Prolog.app)"
       OFF)
option(BUILD_TESTING
       "Build test files and setup for ctest"
       ON)
option(BUILD_SWIPL_LD
       "Create the swipl-ld utility"
       ON)
 option(INSTALL_TESTS
       "Install script and files needed to run tests of the final installation"
       OFF)

include(Utils)
include(BuildType)
include(Version)
include(Ports)
include(Locations)
include(InstallSource)
include(QLF)
include(PackageSelection)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_install_prefix()
  message("-- Using install prefix \"${CMAKE_INSTALL_PREFIX}\"")
endif()

# Verbosity
set(CMAKE_INSTALL_MESSAGE NEVER)

# Installation locations
string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
       SWIPL_ARCH)

################
# Installation directories
if(WIN32)

set(SWIPL_INSTALL_DIR "."
    CACHE STRING "Directory below <prefix> for installation")
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

set(SWIPL_INSTALL_DIR ${SWIPL_INSTALL_DIR_DEF}
    CACHE STRING "Directory below <prefix> for installation")

if(BUILD_MACOS_BUNDLE)
  set(SWIPL_INSTALL_PREFIX    SWI-Prolog.app/Contents/swipl)
  set(SWIPL_INSTALL_ARCH_EXE  SWI-Prolog.app/Contents/MacOS)
  set(SWIPL_INSTALL_ARCH_LIB  SWI-Prolog.app/Contents/Frameworks)
  set(SWIPL_INSTALL_MANPAGES  SWI-Prolog.app/Contents/man)
  set(SWIPL_INSTALL_PKGCONFIG SWI-Prolog.app/Contents/pkgconfig)
  set(SWIPL_INSTALL_RESOURCES SWI-Prolog.app/Contents/Resources)
else()
  set(SWIPL_INSTALL_PREFIX   lib/${SWIPL_INSTALL_DIR})
  set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH})
  set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
  set(SWIPL_INSTALL_MANPAGES share/man/man1
      CACHE STRING "Directory for man pages")
  set(SWIPL_INSTALL_PKGCONFIG share/pkgconfig
      CACHE STRING "Directory for pkg-config pages")
endif()

set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})

endif(WIN32)

set(SWIPL_INSTALL_LIBRARY  ${SWIPL_INSTALL_PREFIX}/library)
set(SWIPL_INSTALL_BOOT     ${SWIPL_INSTALL_PREFIX}/boot)
set(SWIPL_INSTALL_INCLUDE  ${SWIPL_INSTALL_PREFIX}/include)

if(MSVC)
  add_compile_options(/W3)
else()
  add_compile_options(-Wall)
endif()

if(BUILD_TESTING)
  enable_testing()
endif()

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

  add_custom_command(
      OUTPUT "${SWIPL_BUILD_HOME}/doc/manindex.db"
      COMMAND ${PROG_SWIPL} -f none --home=${SWIPL_BUILD_HOME}
	                    -g cmake_save_man_index -t halt
      DEPENDS doc.html)
  add_custom_target(
      man_index
      DEPENDS "${SWIPL_BUILD_HOME}/doc/manindex.db")
  add_dependencies(doc man_index)
  install(FILES "${SWIPL_BUILD_HOME}/doc/manindex.db"
	  DESTINATION ${SWIPL_INSTALL_PREFIX}/doc)

  if(BUILD_PDF_DOCUMENTATION)
    add_custom_target(
	doc.pdf
	COMMENT "Build PDF documentation")
    add_dependencies(doc doc.pdf)
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
