set(CMAKE_POLICY_DEFAULT_CMP0074 NEW)
set(CMAKE_POLICY_DEFAULT_CMP0075 NEW)
set(CMAKE_POLICY_DEFAULT_CMP0079 NEW)
set(CMAKE_POLICY_DEFAULT_CMP0177 NEW)

cmake_minimum_required(VERSION 3.10)
project(SWI-Prolog)

if(CMAKE_BINARY_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  message(FATAL_ERROR
	  " Can not build in the source directory.\n"
	  " Please create a subdirectory and build in that directory.  E.g.\n"
	  "     mkdir build\n"
	  "     cd build\n"
	  "     cmake [option ...] ..")
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# While GCC is good at optimization large   functions, MSVC is bad at it
# and using VMI functions more then   doubles the performance. For Clang
# the picture is less clear. The difference  is small, with a slight win
# on either depending on CPU and workload.

if(MSVC OR CMAKE_C_COMPILER_ID STREQUAL Clang)
  set(DEFAULT_VMI_FUNCTIONS ON)
else()
  set(DEFAULT_VMI_FUNCTIONS OFF)
endif()

option(MULTI_THREADED
       "Enable multiple Prolog threads"
       ON)
option(ENGINES
       "Enable multiple Prolog engines"
       ON)
option(VMI_FUNCTIONS
       "Create a function for each VM instruction"
       ${DEFAULT_VMI_FUNCTIONS})
option(USE_SIGNALS
       "Enable signal handling"
       ON)
option(USE_GMP
       "Use GNU MP Bignum library (LGPL)"
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
option(SWIPL_SO_VERSIONS
       "Set the version properties of libswipl"
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
if(WIN32)
option(SWIPL_INSTALL_AS_LINK
       "Install public binaries using a symlink"
       OFF)
else()
option(SWIPL_INSTALL_AS_LINK
       "Install public binaries using a symlink"
       ON)
endif()
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
option(INSTALL_QLF
       "Build and install .qlf files for all libraries"
       ON)
option(INSTALL_PROLOG_SRC
       "Install the Prolog sources (.pl files)"
       ON)
option(GCOV
       "Compile for coverage analysis using gcov (gcc)"
       OFF)
option(STATIC_EXTENSIONS
       "Add foreign extensions statically"
       OFF)
if(EMSCRIPTEN)
  set(DEFAULT_CCACHE OFF)
else()
  set(DEFAULT_CCACHE ON)
endif()
option(CCACHE
       "Use ccache when available"
       ${DEFAULT_CCACHE})
option(VALIDATE_API
       "Add runtime argument checks for PL_*() functions"
       ON)
option(EPILOG
       "Configure Prolog for using the XPCE Epilog interface"
       ON)

set(JNIDIR ""
    CACHE STRING "Directory for linking Java JNI components")

if(NOT SWIPL_SHARED_LIB)
  set(CMAKE_ENABLE_EXPORTS ON)
endif()

set(CMAKE_C_STANDARD 11)

include(Utils)
include(BuildType)
include(Version)
include(PGO)
include(Locations)
include(Ports)
include(LocationsPostPorts)
include(InstallSource)
include(QLF)
include(PackageSelection)
include(Dependencies)
include(DocDepends)

if(GMP_FOUND)
  set(USE_LIBBF_DEFAULT OFF)
else()
  set(USE_LIBBF_DEFAULT ON)
endif()

option(USE_LIBBF
       "Use LibBF for bignums (MIT)"
       ${USE_LIBBF_DEFAULT})

# Verbosity
set(CMAKE_INSTALL_MESSAGE NEVER)

if(CCACHE)
  find_program(PROG_CCACHE ccache)
  if(PROG_CCACHE)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ${PROG_CCACHE})
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ${PROG_CCACHE})
  endif(PROG_CCACHE)
endif(CCACHE)

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

# on ARM, backtrace() requires -funwind-tables. See
# https://stackoverflow.com/questions/24700150/on-raspberry-pi-backtrace-returns-0-frames
# We must do this at the toplevel to ensure all packages use this as well.
if( (CMAKE_COMPILER_IS_GNUCC OR CMAKE_C_COMPILER_ID STREQUAL Clang) AND
    CMAKE_SYSTEM_PROCESSOR MATCHES "arm")
  include(CheckFunctionExists)
  check_function_exists(backtrace HAVE_BACKTRACE)
  if(HAVE_BACKTRACE)
    add_compile_options(-funwind-tables)
  endif()
endif()

if(SWIPL_M32)
  include(cross/linux_i386)
endif()

if(BUILD_TESTING)
  enable_testing()
endif()

# Configuration we need everywhere
if(MULTI_THREADED)
  if(NOT TARGET Threads::Threads)
    if(MSVC AND NOT ENV{CONDA_BUILD})
      find_package(PThreads4W)
    endif()
    if(TARGET PThreads4W::PThreads4W)
      add_library(Threads::Threads ALIAS PThreads4W::PThreads4W)
      set(CMAKE_USE_PTHREADS_INIT ON)
    else()
      if(WIN32 AND NOT MINGW)
	set(THREADS_USE_PTHREADS_WIN32 TRUE)
      else()
	set(THREADS_PREFER_PTHREAD_FLAG TRUE)
      endif()
      find_package(Threads)
    endif()
  endif()
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
			    -g "use_module(library(pldoc/man_index))"
	                    -g save_man_index -t halt
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

################################################################
# Testing

set(SWIPL_TEST_DIRS unprotected core db attvar debug tabling library
    compile charset eclipse clp GC save files transaction
    xsb/basic_tests xsb/ai_tests xsb/ptq xsb/neg_tests xsb/delay_tests
    xsb/wfs_tests xsb/table_tests xsb/incremental_tests xsb/nonmt_tests
    xsb/sub_tests xsb/attv_tests)
if(MULTI_THREADED)
  list(APPEND SWIPL_TEST_DIRS thread thread_wait signals)
endif()
if(ENGINES)
  list(APPEND SWIPL_TEST_DIRS engines)
endif()
if(O_BIGNUM)
  list(APPEND SWIPL_TEST_DIRS rational)
endif()

add_test(NAME swipl:abi-version
	 COMMAND ${PROG_SWIPL} --abi-version)
add_test(NAME swipl:basic
	 COMMAND ${PROG_SWIPL} -f none --no-packs --on-error=status
	 -q ${CMAKE_CURRENT_SOURCE_DIR}/tests/test.pl --no-subdirs)
foreach(test ${SWIPL_TEST_DIRS})
  add_test(NAME swipl:${test}
	   COMMAND ${PROG_SWIPL} -f none --no-packs --on-error=status
	   -q ${CMAKE_CURRENT_SOURCE_DIR}/tests/test.pl --no-core ${test})
endforeach()

# Install a prolog script to run tests on target device
# in which ctest is not available
if(INSTALL_TESTS)
  set(INSTALL_TESTS_DIR ${SWIPL_INSTALL_PREFIX}/test)
  set(PKGS_SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/packages)
  set(INSTALL_TESTS_DB ${CMAKE_BINARY_DIR}/cmake_pkg_tests.db)

# Use a function to scope CMAKE_INSTALL_DEFAULT_COMPONENT_NAME
function(install_tests)
  set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME Tests)
  install(DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}/tests/
          DESTINATION ${INSTALL_TESTS_DIR}
	  PATTERN "*~" EXCLUDE)
  #Move test db to installation
  install(FILES       ${INSTALL_TESTS_DB}
	  DESTINATION ${INSTALL_TESTS_DIR})
  file(REMOVE ${INSTALL_TESTS_DB})
endfunction()

install_tests()
endif(INSTALL_TESTS)

# Add the packages
if(STATIC_EXTENSIONS)
  set_property(GLOBAL PROPERTY static_extension_libs)
endif()
foreach(package ${SWIPL_PACKAGE_LIST})
  swipl_package_component(${package}
			  CMAKE_INSTALL_DEFAULT_COMPONENT_NAME)
  add_subdirectory(packages/${package})
endforeach(package)
if(STATIC_EXTENSIONS)
  get_property(extlibs GLOBAL PROPERTY static_extension_libs)
  include(StaticPackages)
  write_static_extensions(${CMAKE_BINARY_DIR}/src/static_packages.h ${extlibs})
endif()

# Check for environment variables that may cause the build to fail
include(CheckEnv)

# Configure and install desktop integration files.
include(Desktop)

# Packaging
include(Pack)
