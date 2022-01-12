set(CMAKE_POLICY_DEFAULT_CMP0075 NEW)
set(CMAKE_POLICY_DEFAULT_CMP0074 NEW)

cmake_minimum_required(VERSION 3.5)
project(SWI-Prolog)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

option(MULTI_THREADED
       "Enable multiple Prolog threads"
       ON)
option(VMI_FUNCTIONS
       "Create a function for each VM instruction"
       OFF)
option(USE_SIGNALS
       "Enable signal handling"
       ON)
option(USE_GMP
       "Use GNU MP Bignum library (GPL)"
       ON)
if(APPLE)
# tcmalloc breaks setlocale() on MacOS 12 (Monterey)
option(USE_TCMALLOC
       "Use Google tcmalloc instead of default malloc"
       OFF)
else()
option(USE_TCMALLOC
       "Use Google tcmalloc instead of default malloc"
       ON)
endif()
option(SWIPL_SHARED_LIB
       "Put kernel in a shared library"
       ON)
option(SWIPL_VERSIONED_DIR
       "Install into a versioned directory"
       OFF)
option(SWIPL_INSTALL_IN_LIB
       "Install library in ${CMAKE_INSTALL_PREFIX}/lib"
       OFF)
option(SWIPL_INSTALL_IN_SHARE
       "Install docs in ${CMAKE_INSTALL_PREFIX}/share/swipl"
       OFF)
option(SWIPL_M32
       "Build 32-bit version on 64-bit Linux using multilib and gcc -m32"
       OFF)
option(INSTALL_DOCUMENTATION
       "Install the HTML documentation files"
       ON)
option(BUILD_PDF_DOCUMENTATION
       "Build the PDF manuals from source"
       OFF)
option(MACOS_UNIVERSAL_BINARY
       "Build a universal binary for x86_64 and arm64"
       OFF)
option(BUILD_MACOS_BUNDLE
       "Install for a MacOS bundle (SWI-Prolog.app)"
       OFF)
option(BUILD_TESTING
       "Build test files and setup for ctest"
       ON)
option(SKIP_SSL_TESTS
       "Skip the SSL tests"
       OFF)
option(TEST_PROTOBUFS_PROTOC
       "Run protobuf tests that require protoc, etc."
       OFF)
option(BUILD_SWIPL_LD
       "Create the swipl-ld utility"
       ON)
option(INSTALL_TESTS
       "Install script and files needed to run tests of the final installation"
       OFF)
option(GCOV
       "Compile for coverage analysis using gcov (gcc)"
       OFF)

set(JNIDIR ""
    CACHE STRING "Directory for linking Java JNI components")

if(NOT SWIPL_SHARED_LIB)
  set(CMAKE_ENABLE_EXPORTS ON)
endif()

include(Utils)
include(BuildType)
include(Version)
include(PGO)
include(Ports)
include(Locations)
include(InstallSource)
include(QLF)
include(PackageSelection)
include(Dependencies)
include(DocDepends)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_install_prefix()
  message("-- Using install prefix \"${CMAKE_INSTALL_PREFIX}\"")
endif()

# Verbosity
set(CMAKE_INSTALL_MESSAGE NEVER)

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
  set(SWIPL_TMP_DIR "c:/tmp" CACHE STRING
      "Directory to be used if the environment variable TEMP is not set")

else(WIN32)

  if(SWIPL_VERSIONED_DIR)
    set(SWIPL_INSTALL_DIR_DEF  swipl-${SWIPL_VERSION_STRING})
  else()
    set(SWIPL_INSTALL_DIR_DEF  swipl)
  endif()

  set(SWIPL_INSTALL_DIR ${SWIPL_INSTALL_DIR_DEF}
      CACHE STRING "Directory below <prefix> for installation")
  set(SWIPL_TMP_DIR "/tmp" CACHE STRING
      "Directory to be used if the environment variable TMP is not set")

  if(BUILD_MACOS_BUNDLE)
    set(SWIPL_INSTALL_PREFIX    SWI-Prolog.app/Contents/swipl)
    set(SWIPL_INSTALL_ARCH_EXE  SWI-Prolog.app/Contents/MacOS)
    set(SWIPL_INSTALL_ARCH_LIB  SWI-Prolog.app/Contents/Frameworks)
    set(SWIPL_INSTALL_MANPAGES  SWI-Prolog.app/Contents/man)
    set(SWIPL_INSTALL_PKGCONFIG SWI-Prolog.app/Contents/pkgconfig)
    set(SWIPL_INSTALL_RESOURCES SWI-Prolog.app/Contents/Resources)
  else()
    set(SWIPL_INSTALL_PREFIX   lib/${SWIPL_INSTALL_DIR})
    if(SWIPL_INSTALL_IN_SHARE)
      set(SWIPL_INSTALL_SHARE_PREFIX share/${SWIPL_INSTALL_DIR})
    endif()
    set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH})
    set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
    set(SWIPL_INSTALL_MANPAGES share/man/man1
        CACHE STRING "Directory for man pages")
    set(SWIPL_INSTALL_PKGCONFIG share/pkgconfig
        CACHE STRING "Directory for pkg-config pages")
  endif()

  set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})

endif(WIN32)

set(SWIPL_CMAKE_NAMESPACE swipl::)
set(SWIPL_INSTALL_CMAKE_CONFIG_DIR lib/cmake/swipl)

if(NOT SWIPL_INSTALL_SHARE_PREFIX)
  set(SWIPL_INSTALL_SHARE_PREFIX "${SWIPL_INSTALL_PREFIX}")
endif()

set(SWIPL_BOOT_BASE        "boot.prc")
set(SWIPL_BOOT_FILE        "${CMAKE_BINARY_DIR}/home/${SWIPL_BOOT_BASE}")
set(SWIPL_INSTALL_LIBRARY  ${SWIPL_INSTALL_PREFIX}/library)
set(SWIPL_INSTALL_BOOT     ${SWIPL_INSTALL_PREFIX}/boot)
set(SWIPL_INSTALL_INCLUDE  ${SWIPL_INSTALL_PREFIX}/include)
set(SWIPL_INSTALL_DOC	   ${SWIPL_INSTALL_SHARE_PREFIX}/doc)
if(INSTALL_TESTS)
   set(INSTALL_TESTS_DIR   ${SWIPL_INSTALL_PREFIX}/test)
endif()

if(NOT SWIPL_PKG_NAME)
  if(SWIPL_INSTALL_DIR STREQUAL "swi-prolog")
    set(SWIPL_PKG_NAME ${SWIPL_INSTALL_DIR})
  else()
    set(SWIPL_PKG_NAME "swipl")
  endif()
endif()

if(MSVC)
  add_compile_options(/W3)
else()
  add_compile_options(-Wall)
endif()

if(MACOS_UNIVERSAL_BINARY)
# Does not work!?  Now uses CFLAGS=-arcg x86_64 on M1.  See scripts/make-distribution
# add_compile_options("SHELL:-arch x86_64" "SHELL:-arch arm64")
endif()

if(SWIPL_M32)
  include(cross/linux_i386)
endif()

if(BUILD_TESTING)
  enable_testing()
endif()

# Configuration we need everywhere
if(MULTI_THREADED)
  set(THREADS_PREFER_PTHREAD_FLAG TRUE)
  find_package(Threads)
endif()

include(TestLargeFiles)
OPJ_TEST_LARGE_FILES(HAVE_LARGE_FILES)

set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME "Core_system")

# Add the core Prolog system
add_subdirectory(src)
install(FILES LICENSE README.md DESTINATION ${SWIPL_INSTALL_PREFIX})
install(FILES customize/edit customize/init.pl customize/README.md
	DESTINATION ${SWIPL_INSTALL_PREFIX}/customize)

if(INSTALL_DOCUMENTATION)
  include(Documentation)
  set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME Documentation)

  add_custom_target(
      doc ALL
      COMMENT "Build the documentation")

  add_custom_target(
      doc.html
      COMMENT "Build HTML documentation")

  add_dependencies(doc doc.html)

  add_custom_command(
      OUTPUT ${MAN_INDEX}
      COMMAND ${PROG_SWIPL} -f none --no-packs --home=${SWIPL_BUILD_HOME}
			    -g "use_module(library(prolog_install))"
	                    -g cmake_save_man_index -t halt
      DEPENDS swipl core doc.html
      VERBATIM)
  add_custom_target(
      man_index
      DEPENDS ${MAN_INDEX})
  add_dependencies(doc man_index)
  install(FILES ${MAN_INDEX} DESTINATION ${SWIPL_INSTALL_DOC})

  if(BUILD_PDF_DOCUMENTATION)
    add_custom_target(
	doc.pdf
	COMMENT "Build PDF documentation")
    add_dependencies(doc doc.pdf)
  endif()

  add_subdirectory(man)
  install(FILES packages/index.html
	  DESTINATION ${SWIPL_INSTALL_DOC}/packages)
endif(INSTALL_DOCUMENTATION)

# Install a prolog script to run tests on target device
# in which ctest is not available
if(INSTALL_TESTS)
  set(INSTALL_TESTS_DIR ${SWIPL_INSTALL_PREFIX}/test)
  set(PKGS_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/packages)
  set(INSTALL_TESTS_DB ${CMAKE_BINARY_DIR}/cmake_pkg_tests.db)
  #Move test db to installation
  install(FILES       ${INSTALL_TESTS_DB}
	  DESTINATION ${INSTALL_TESTS_DIR}
	  COMPONENT   Tests)
  file(REMOVE ${INSTALL_TESTS_DB})
endif(INSTALL_TESTS)

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
