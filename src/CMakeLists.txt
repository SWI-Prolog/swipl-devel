cmake_minimum_required(VERSION 3.5)
project(swipl)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")

include(Version)
include(Utils)
include(CStack)
include(LibIndex)

include(CheckIncludeFile)
include(CheckLibraryExists)
include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(AlignOf)
include(CheckStructHasMember)
include(CheckCCompilerFlag)
include(CheckCSourceRuns)
include(Dependencies)
include(Config)
include(GCCBuiltins)
include(TestSignalType)
include(TestSCNProcessors)
include(TestHeaderTime)
include(TestLLRound)
include(TestModF)

# FIXME: Is this still needed?
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-fno-strict-aliasing)
endif()

# Source locations
get_filename_component(SWIPL_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/.." ABSOLUTE)
set(SWIPL_LIBRARY_ROOT ${SWIPL_ROOT}/library)
set(SWIPL_BOOT_ROOT ${SWIPL_ROOT}/boot)

# Tools (swipl-ld)
string(REPLACE "." "" SO_EXT ${CMAKE_SHARED_MODULE_SUFFIX})

# Installation
include(Install)

################
# C files
################

set(SRC_OS pl-buffer.c pl-ctype.c pl-file.c pl-files.c pl-glob.c pl-os.c
    pl-stream.c pl-string.c pl-table.c pl-text.c pl-utf8.c pl-fmt.c
    pl-dtoa.c pl-option.c pl-cstack.c pl-codelist.c pl-prologflag.c pl-tai.c
    pl-locale.c pl-apple.c)
prepend(SRC_OS os/ ${SRC_OS})


set(SRC_TAI caltime_utc.c caltime_tai.c leapsecs_sub.c leapsecs_add.c
    caldate_fmjd.c caldate_mjd.c leapsecs_init.c leapsecs_read.c tai_pack.c
    tai_unpack.c)
prepend(SRC_TAI libtai/ ${SRC_TAI})

set(SRC_MINIZIP zip.c unzip.c ioapi.c)
prepend(SRC_MINIZIP minizip/ ${SRC_MINIZIP})

set(SRC_CORE pl-atom.c pl-wam.c pl-arith.c pl-bag.c pl-error.c
    pl-comp.c pl-zip.c pl-dwim.c pl-ext.c pl-flag.c
    pl-funct.c pl-gc.c pl-privitf.c pl-list.c pl-string.c
    pl-load.c pl-modul.c pl-op.c pl-prims.c pl-pro.c
    pl-proc.c pl-prof.c pl-read.c pl-rec.c pl-setup.c
    pl-sys.c pl-trace.c pl-util.c pl-wic.c pl-write.c
    pl-term.c pl-thread.c pl-xterm.c pl-srcfile.c
    pl-beos.c pl-attvar.c pl-gvar.c pl-btree.c
    pl-init.c pl-gmp.c pl-segstack.c pl-hash.c
    pl-version.c pl-codetable.c pl-supervisor.c
    pl-dbref.c pl-termhash.c pl-variant.c pl-assert.c
    pl-copyterm.c pl-debug.c pl-cont.c pl-ressymbol.c pl-dict.c
    pl-trie.c pl-indirect.c pl-tabling.c pl-rsort.c pl-mutex.c
    pl-allocpool.c pl-wrap.c pl-event.c pl-transaction.c
    pl-undo.c pl-alloc.c pl-index.c pl-fli.c)

set(LIBSWIPL_SRC
    ${SRC_CORE}
    ${SRC_OS}
    ${SRC_OS_SPECIFIC}
    ${SRC_TAI}
    ${SRC_MINIZIP})
set(SWIPL_SRC pl-main.c)

set(SRC_SWIPL_LD swipl-ld.c)
if(WIN32)
  set(SRC_SWIPL_LD ${SRC_SWIPL_LD} os/windows/uxnt.c)
endif()

set(POSIX_SHELL "/bin/sh" CACHE STRING "Full path to a POSIX compliant shell")

################
# Prolog data files
################

set(SWIPL_DATA_INDEXED_DIRS
    library library/clp library/dcg library/unicode library/lynx)
set(SWIPL_DATA_DIRS
    boot
    ${SWIPL_DATA_INDEXED_DIRS}
    library/dialect library/dialect/swi library/dialect/eclipse
    library/dialect/hprolog library/dialect/sicstus library/dialect/sicstus4 library/dialect/iso
    library/dialect/yap library/dialect/xsb library/theme library/iri_scheme library/build
    demo cmake)

set(SWIPL_DATA_boot init.pl syspred.pl toplevel.pl license.pl bags.pl apply.pl
    expand.pl dcg.pl history.pl attvar.pl packs.pl dwim.pl rc.pl predopts.pl
    autoload.pl qlf.pl topvars.pl messages.pl load.pl dicts.pl gc.pl
    engines.pl iri.pl tabling.pl)
if(WIN32)
  set(SWIPL_DATA_boot ${SWIPL_DATA_boot} menu.pl)
endif()
prepend(SWIPL_BOOT_FILES ${SWIPL_BUILD_HOME}/boot/ ${SWIPL_DATA_boot})

set(SWIPL_DATA_library explain.pl sort.pl prolog_config.pl
    qsave.pl shlib.pl statistics.pl system.pl error.pl
    backcomp.pl gensym.pl listing.pl debug.pl vm.pl
    quintus.pl edinburgh.pl ctypes.pl files.pl modules.pl
    edit.pl shell.pl check.pl heaps.pl console_input.pl
    tty.pl readln.pl readutil.pl make.pl hotfix.pl option.pl
    oset.pl ordsets.pl occurs.pl lists.pl pprint.pl atom.pl
    www_browser.pl url.pl utf8.pl main.pl assoc.pl nb_set.pl
    threadutil.pl qpforeign.pl dif.pl when.pl ugraphs.pl
    checklast.pl checkselect.pl operators.pl date.pl
    prolog_stack.pl prolog_clause.pl prolog_xref.pl prolog_source.pl
    broadcast.pl pairs.pl base64.pl record.pl rbtrees.pl settings.pl
    thread.pl dialect.pl apply_macros.pl apply.pl nb_rbtrees.pl
    aggregate.pl pure_input.pl pio.pl thread_pool.pl terms.pl
    charsio.pl portray_text.pl csv.pl persistency.pl fastrw.pl
    coinduction.pl ansi_term.pl base32.pl prolog_history.pl
    optparse.pl arithmetic.pl writef.pl predicate_options.pl
    random.pl prolog_breakpoints.pl prolog_autoload.pl
    prolog_colour.pl varnumbers.pl codesio.pl prolog_codewalk.pl
    prolog_pack.pl git.pl prolog_metainference.pl quasi_quotations.pl
    sandbox.pl prolog_format.pl prolog_install.pl check_installation.pl
    solution_sequences.pl iostream.pl dicts.pl yall.pl tabling.pl
    lazy_lists.pl prolog_jiti.pl zip.pl obfuscate.pl wfs.pl
    prolog_wrap.pl prolog_trace.pl prolog_code.pl intercept.pl
    prolog_deps.pl tables.pl hashtable.pl strings.pl increval.pl
    prolog_debug.pl prolog_versions.pl)
if(INSTALL_DOCUMENTATION)
  set(SWIPL_DATA_library ${SWIPL_DATA_library} help.pl)
endif()
if(WIN32)
  set(SWIPL_DATA_library ${SWIPL_DATA_library}
      dde.pl win_menu.pl progman.pl)
else()
  has_package(swipl-win HAVE_SWIPL_WIN)
  if(HAVE_SWIPL_WIN)
    set(SWIPL_DATA_library ${SWIPL_DATA_library} win_menu.pl)
  endif()
endif()

set(SWIPL_DATA_library_lynx format.pl html_style.pl html_text.pl
    pldoc_style.pl)

set(SWIPL_DATA_library_theme auto.pl dark.pl)

set(SWIPL_DATA_library_iri_scheme file.pl)

set(SWIPL_DATA_library_clp bounds.pl clp_events.pl clp_distinct.pl
    clpfd.pl clpb.pl)
if(GMP_FOUND)
  set(SWIPL_DATA_library_clp ${SWIPL_DATA_library_clp} simplex.pl)
endif()

set(SWIPL_DATA_library_dcg basics.pl high_order.pl)

set(SWIPL_DATA_library_unicode blocks.pl unicode_data.pl)

set(SWIPL_DATA_library_dialect bim.pl commons.pl hprolog.pl ifprolog.pl
    sicstus.pl sicstus4.pl yap.pl xsb.pl)

set(SWIPL_DATA_library_dialect_swi syspred_options.pl)
set(SWIPL_DATA_library_dialect_eclipse test_util_iso.pl)
set(SWIPL_DATA_library_dialect_hprolog format.pl)
set(SWIPL_DATA_library_dialect_sicstus arrays.pl block.pl lists.pl ordsets.pl
    README.TXT sockets.pl swipl-lfr.pl system.pl terms.pl timeout.pl)
set(SWIPL_DATA_library_dialect_sicstus4 aggregate.pl between.pl clpfd.pl
    file_systems.pl lists.pl ordsets.pl samsort.pl sets.pl sockets.pl
    system.pl terms.pl timeout.pl types.pl)
set(SWIPL_DATA_library_dialect_iso iso_predicates.pl)
set(SWIPL_DATA_library_dialect_yap README.TXT)
set(SWIPL_DATA_library_dialect_xsb README.md source.pl basics.pl machine.pl
    storage.pl ordsets.pl intern.pl string.pl setof.pl consult.pl
    curr_sym.pl error_handler.pl lists.pl timed_call.pl
    standard.pl gpp.pl thread.pl gensym.pl)
set(SWIPL_DATA_library_build tools.pl conan.pl cmake.pl make.pl)

set(SWIPL_DATA_demo likes.pl README.md)
set(SWIPL_DATA_cmake swipl.cmake)

################
# Custom targets and commands
################

if(NOT PGO_SUFFIX) # Don't define PGO versions of the aux targets (see PGO.cmake)

# build helpers
if(NOT CMAKE_CROSSCOMPILING)
  add_executable(mkvmi mkvmi.c)
  add_executable(defatom defatom.c)
  add_executable(wrap-gcc EXCLUDE_FROM_ALL wrap-gcc.c) #only needed for PGO with GCC
  set(PROG_MKVMI mkvmi)
  set(PROG_DEFATOM defatom)
  set(TARGET_WRAPGCC wrap-gcc)
  set(PROG_WRAPGCC
      ${CMAKE_CURRENT_BINARY_DIR}/wrap-gcc${CMAKE_HOST_EXECUTABLE_SUFFIX})
else()
  set(PROG_MKVMI
      ${CMAKE_CURRENT_BINARY_DIR}/mkvmi${CMAKE_HOST_EXECUTABLE_SUFFIX})
  set(PROG_DEFATOM
      ${CMAKE_CURRENT_BINARY_DIR}/defatom${CMAKE_HOST_EXECUTABLE_SUFFIX})
  set(PROG_WRAPGCC
      ${CMAKE_CURRENT_BINARY_DIR}/wrap-gcc${CMAKE_HOST_EXECUTABLE_SUFFIX})

  add_custom_command(
      OUTPUT  ${PROG_DEFATOM}
      COMMAND ${CMAKE_HOST_CC} -o ${PROG_DEFATOM}
	      ${CMAKE_CURRENT_SOURCE_DIR}/defatom.c
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/defatom.c
  )

  add_custom_command(
      OUTPUT  ${PROG_MKVMI}
      COMMAND ${CMAKE_HOST_CC} -o ${PROG_MKVMI}
              ${CMAKE_CURRENT_SOURCE_DIR}/mkvmi.c
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/mkvmi.c
  )

  add_custom_command(
      OUTPUT  ${PROG_WRAPGCC}
      COMMAND ${CMAKE_HOST_CC} -o ${PROG_WRAPGCC}
              ${CMAKE_CURRENT_SOURCE_DIR}/wrap-gcc.c
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wrap-gcc.c
  )
  add_custom_target(build-wrap-gcc DEPENDS ${PROG_WRAPGCC})
  set(TARGET_WRAPGCC build-wrap-gcc)
endif()

add_custom_target(
    vmi-metadata
    DEPENDS vmi-metadata.h
)
add_custom_command(
    COMMAND ${PROG_MKVMI} ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT vmi-metadata.h
    DEPENDS ${PROG_MKVMI} pl-vmi.c
    COMMENT "Generating VMI metadata into vmi-metadata.h"
    VERBATIM
)

add_custom_target(
    core-constants
    DEPENDS pl-atom.ih pl-funct.ih)

add_custom_command(
    COMMAND ${PROG_DEFATOM} ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT pl-atom.ih pl-funct.ih
    DEPENDS ${PROG_DEFATOM} ATOMS
    COMMENT "Generating static atoms and functors"
)


set(USE_GIT_VERSION_H 1)
include(GitVersion)
git_version(GIT_VERSION)
configure_file(version.h.in version.h)

add_custom_command(
    OUTPUT  ${SWIPL_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E remove -f ${SWIPL_BOOT_FILE}
    COMMAND ${PROG_SWIPL_FOR_BOOT} -q -O -o ${SWIPL_BOOT_FILE}
				   -b ${SWIPL_BUILD_HOME}/boot/init.pl
    DEPENDS ${DEP_SWIPL_FOR_BOOT} ${SWIPL_BOOT_FILES}
)
add_custom_target(bootfile DEPENDS ${SWIPL_BOOT_FILE})

add_custom_target(library_index)

library_index(${SWIPL_DATA_INDEXED_DIRS})

add_custom_target(core ALL
    DEPENDS swipl bootfile
)

# Precompile some popular larger libraries to .QLF
set(SWIPL_QLF_BASE ${SWIPL_BUILD_HOME}/library)
add_qcompile_target(prolog_colour)
add_qcompile_target(prolog_xref)

# The C config file
configure_file(config.h.cmake config.h)

# export all O_XYZ variables to be accessible at the top level (i.e. to packages)
get_directory_property(varnames VARIABLES)
# message(STATUS "Exporting config options to top level:")
foreach(varname IN LISTS varnames)
  if(varname MATCHES "^O_")
    # message(STATUS "  ${varname} = ${${varname}}")
    set(${varname} ${${varname}} PARENT_SCOPE)
  endif()
endforeach()
unset(varname)
unset(varnames)

# Configure some data files.  Uses a function because these
# files use old-style variable names and we want to avoid these
# leaking into the cmake environment.
function(config_data_files)
  set(PL swipl)
  set(PLBASE ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_DIR})
  set(PLVERSION ${SWIPL_VERSION_STRING})
  set(INSTALL_PLARCH ${SWIPL_ARCH})
  set(prefix ${CMAKE_INSTALL_PREFIX})

  configure_file(swipl.1.in swipl.1 @ONLY)
  configure_file(swipl.pc.in swipl.pc @ONLY)
endfunction()

config_data_files()

# Set swipl.home above binaries to a relative path to the root
file(WRITE "${CMAKE_BINARY_DIR}/swipl.home" "home\n")
file(WRITE "${CMAKE_BINARY_DIR}/src/swipl.home" "../home\n")
file(WRITE "${CMAKE_BINARY_DIR}/packages/swipl.home" "../home\n")
symlink_or_copy(${SWIPL_BOOT_ROOT}/build_home.pl ${CMAKE_BINARY_DIR}/home/swipl.rc)
symlink_or_copy(${SWIPL_BOOT_ROOT}/build_home.pl ${CMAKE_BINARY_DIR}/home/swipl-win.rc)
# Final swipl.home for installed system
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/dot.txt" ".\n")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/dotdot.txt" "..\n")

endif(NOT PGO_SUFFIX)

# Establish libraries
if(CURSES_FOUND)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} ${CURSES_LIBRARIES})
endif()
if(GMP_FOUND)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} ${GMP_LIBRARIES})
  set(LIBSWIPL_INCLUDES ${LIBSWIPL_INCLUDES} ${GMP_INCLUDE_DIRS})
endif()
set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} ${ZLIB_LIBRARIES})
if(MULTI_THREADED)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} Threads::Threads)
endif()
if(HAVE_LIBDL)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} dl)
endif()
if(HAVE_LIBM)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} m)
endif()
if(HAVE_LIBRT)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} rt)
endif()
if(HAVE_LIBATOMIC)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} atomic)
endif()
if(LIBTCMALLOC_LIBRARIES)
  set(SWIPL_LIBRARIES ${LIBTCMALLOC_LIBRARIES} ${SWIPL_LIBRARIES})
endif()
if(APPLE)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} "-framework CoreFoundation")
endif()

# build swipl
add_executable(swipl${PGO_SUFFIX} ${SWIPL_SRC})
target_link_libraries(swipl${PGO_SUFFIX} ${SWIPL_LIBRARIES} libswipl${PGO_SUFFIX})
target_c_stack(swipl${PGO_SUFFIX} 4000000)
if(MINGW_ROOT)
  target_link_options(swipl${PGO_SUFFIX} PRIVATE -municode)
endif()

# build the library
if(SWIPL_SHARED_LIB)
  set(LIBSWIPL_TYPE SHARED)
else()
  set(LIBSWIPL_TYPE STATIC)
endif()

# Create an OBJECT library so we can build both a static and dynamic
# library

add_library(swiplobjs${PGO_SUFFIX} OBJECT ${LIBSWIPL_SRC})
set_property(TARGET swiplobjs${PGO_SUFFIX} PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET swiplobjs${PGO_SUFFIX} PROPERTY C_STANDARD 11) # Core libswipl code conforms to C11
if(CHECK_PROTOTYPES) # Can be defined on the command line
target_compile_options(swiplobjs${PGO_SUFFIX} PRIVATE -Wmissing-prototypes -Wmissing-declarations)
endif()
if(LIBSWIPL_TYPE STREQUAL "SHARED")
  set_property(TARGET swiplobjs${PGO_SUFFIX} PROPERTY POSITION_INDEPENDENT_CODE 1)
endif()
add_dependencies(swiplobjs${PGO_SUFFIX} vmi-metadata core-constants)
target_include_directories(swiplobjs${PGO_SUFFIX} BEFORE PRIVATE
			   ${CMAKE_CURRENT_BINARY_DIR}
			   ${CMAKE_CURRENT_SOURCE_DIR}
			   ${ZLIB_INCLUDE_DIRS}
			   ${LIBSWIPL_INCLUDES})

function(libswipl_properties lib outname)
  if(MSVC AND NOT SWIPL_SHARED_LIB)
    # MSVC does not automatically prepend 'lib' to the library name
    # Furthermore it would generate swipl.lib in the same directory as swipl.exe,
    # causing a linking failure:
    # "LNK1114 cannot overwrite the original file 'build/src/Debug/swipl.lib'
    set(LIB_PREFIX lib)
  else()
    set(LIB_PREFIX)
  endif()

  set_target_properties(${lib} PROPERTIES
			OUTPUT_NAME ${LIB_PREFIX}${outname}
			VERSION ${SWIPL_VERSION_STRING}
			SOVERSION ${SWIPL_VERSION_MAJOR}
			GNUtoMS ON)
  target_include_directories(${lib} INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/os>
    $<INSTALL_INTERFACE:${SWIPL_INSTALL_INCLUDE}>)
  target_link_libraries(${lib} PRIVATE ${LIBSWIPL_LIBRARIES})
endfunction()

# Build the default library

add_library(libswipl${PGO_SUFFIX} ${LIBSWIPL_TYPE} $<TARGET_OBJECTS:swiplobjs${PGO_SUFFIX}>)
libswipl_properties(libswipl${PGO_SUFFIX} swipl${PGO_SUFFIX})


###################################################################################
#                         END OF MAIN TARGET DEFINITIONS                          #
# Everything needed to build the standard swipl binary should be above here! The  #
# PGO/instrumentation build doesn't parse past here. Any changes that affect the  #
# compilation of files listed in PGO_TARGETS below should be placed above this.   #
###################################################################################

if (PGO_SUFFIX)
  message(STATUS "PGO: configuration of instrumented build complete")
  return()
endif()


# If the default is shared, also build a static library

if(LIBSWIPL_TYPE STREQUAL "SHARED" AND SWIPL_STATIC_LIB)
add_library(libswipl_static STATIC $<TARGET_OBJECTS:swiplobjs>)
libswipl_properties(libswipl_static swipl_static)
set(STATIC_LIB libswipl_static)
else()
set(STATIC_LIB)
endif()

if(BUILD_SWIPL_LD)
  add_executable(swipl-ld ${SRC_SWIPL_LD})
  target_include_directories(swipl-ld BEFORE PRIVATE
			     ${CMAKE_CURRENT_BINARY_DIR}
			     ${CMAKE_CURRENT_SOURCE_DIR}
			     ${LIBSWIPL_INCLUDES})
  add_dependencies(swipl-ld vmi-metadata core-constants)
endif()

if(WIN32)
  set(SWIPL_PROGRAM libswipl.dll)
else()
  set(SWIPL_PROGRAM swipl)
endif()

if(WIN32)
  include(WindowsTargets)
elseif(EMSCRIPTEN)
  include(EmscriptenTargets)
elseif(ANDROID)
  include(AndroidTargets)
endif()

################
# PGO (profile guided optimization)

set(PGO_TARGETS swipl swiplobjs libswipl)

if(GCOV)
  set(GCOV_FLAGS --coverage -fkeep-inline-functions -fkeep-static-functions)

  message("-- GCOV: setup for data collection")
  foreach(t ${PGO_TARGETS})
    target_compile_options(${t} PRIVATE ${GCOV_FLAGS})
    string(REPLACE ";" " " gen_flags "${GCOV_FLAGS}")
    set_target_properties(${t} PROPERTIES LINK_FLAGS "${gen_flags}")
  endforeach()
endif()

if (CMAKE_BUILD_TYPE STREQUAL "PGO" OR CMAKE_BUILD_TYPE STREQUAL "DEB")
  configure_pgo("pgo-instrumented")
  generate_pgo_data(${PGO_TARGETS})
  add_pgo_dependency(bootfile prolog_home)
  run_pgo_program(swipl ${PGO_SWIPL_OPTIONS} "${PGO_PROGRAM}" ${PGO_PROGRAM_OPTIONS})
  use_pgo_data(${PGO_TARGETS})
elseif(PROFILE_GUIDED_OPTIMIZATION)
  if(PROFILE_GUIDED_OPTIMIZATION STREQUAL "GENERATE")
    message("-- PGO: setup for data collection")
    configure_pgo("")
    generate_pgo_data(${PGO_TARGETS})
  elseif(PROFILE_GUIDED_OPTIMIZATION STREQUAL "USE")
    message("-- PGO: setup for compiling final version")
    configure_pgo("")
    use_pgo_data(${PGO_TARGETS})
  endif()
endif()

################
# Testing

add_custom_target(check
    COMMAND ${PROG_SWIPL} -f none --no-packs -q ${CMAKE_CURRENT_SOURCE_DIR}/test.pl)

set(SWIPL_TEST_DIRS unprotected core attvar debug tabling library
    compile charset eclipse clp GC save files transaction
    xsb/basic_tests xsb/ai_tests xsb/ptq xsb/neg_tests xsb/delay_tests
    xsb/wfs_tests xsb/table_tests xsb/incremental_tests xsb/nonmt_tests
    xsb/sub_tests)
if(MULTI_THREADED)
  set(SWIPL_TEST_DIRS ${SWIPL_TEST_DIRS} thread thread_wait)
endif()
if(GMP_FOUND)
  set(SWIPL_TEST_DIRS ${SWIPL_TEST_DIRS} rational)
endif()

add_test(NAME swipl:abi-version
	 COMMAND ${PROG_SWIPL} --abi-version)
add_test(NAME swipl:basic
	 COMMAND ${PROG_SWIPL} -f none --no-packs --on-error=status
	 -q ${CMAKE_CURRENT_SOURCE_DIR}/test.pl --no-subdirs)
foreach(test ${SWIPL_TEST_DIRS})
  add_test(NAME swipl:${test}
	   COMMAND ${PROG_SWIPL} -f none --no-packs --on-error=status
	   -q ${CMAKE_CURRENT_SOURCE_DIR}/test.pl --no-core ${test})
endforeach()

# Use a function to scope CMAKE_INSTALL_DEFAULT_COMPONENT_NAME
function(install_tests)
   set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME Tests)
   install(DIRECTORY   ${CMAKE_CURRENT_SOURCE_DIR}/Tests
           DESTINATION ${INSTALL_TESTS_DIR})
   install(FILES       ${CMAKE_CURRENT_SOURCE_DIR}/test.pl
           DESTINATION ${INSTALL_TESTS_DIR}/)
endfunction()

if(INSTALL_TESTS)
install_tests()
endif()

# Populate parms.h, making the compilation environment known to Prolog
include(Params)
configure_file(parms.h.cmake parms.h)

# Provide ninja etags
file(GLOB_RECURSE etags_sources RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.[ch]")
add_custom_target(
    etags
    etags ${etags_sources}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Create TAGS file for main sources")

# Create cmake config files
include(CMakePackageConfigHelpers)
write_basic_package_version_file(SWIPLConfigVersion.cmake
  VERSION "${SWIPL_VERSION_STRING}"
  COMPATIBILITY SameMajorVersion)
configure_package_config_file(
  "${CMAKE_SOURCE_DIR}/cmake/SWIPLConfig.cmake.in"
  SWIPLConfig.cmake
  PATH_VARS SWIPL_INSTALL_ARCH_EXE
  INSTALL_DESTINATION "${SWIPL_INSTALL_CMAKE_CONFIG_DIR}"
  NO_SET_AND_CHECK_MACRO
  NO_CHECK_REQUIRED_COMPONENTS_MACRO)

################
# Installation
################

if(SWIPL_INSTALL_IN_LIB)
  set(LIBSWIPL_DIR ${CMAKE_INSTALL_PREFIX}/lib)
else()
  set(LIBSWIPL_DIR ${SWIPL_INSTALL_ARCH_LIB})
endif()

# Make sure tmp directory exists
#
# This is used on Android, where there  is normally no TMP directory and
# the choosen location is not shared  with   any  other  users, since on
# Android every application is its own user.

if(NOT IS_DIRECTORY ${SWIPL_TMP_DIR} AND ANDROID)
 install(DIRECTORY DESTINATION ${SWIPL_TMP_DIR})
endif()

install(TARGETS swipl libswipl ${STATIC_LIB} EXPORT SWIPL_EXPORT
	RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE}
        LIBRARY DESTINATION ${LIBSWIPL_DIR}
        ARCHIVE DESTINATION ${LIBSWIPL_DIR}
)
install(FILES ${SWIPL_BOOT_FILE}
	DESTINATION ${SWIPL_INSTALL_PREFIX}
)
if(WIN32)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libswipl.dll.a
	DESTINATION ${LIBSWIPL_DIR}
	RENAME libswipl.lib)
endif()

foreach(d ${SWIPL_DATA_DIRS})
    string(REGEX REPLACE "/" "_" filevar ${d})
    prepend(files ${SWIPL_ROOT}/${d} ${SWIPL_DATA_${filevar}})
    install_src(core_${filevar}
		FILES ${files}
		DESTINATION ${SWIPL_INSTALL_PREFIX}/${d})
endforeach()
install_src(core_headers
	    FILES
	    ${CMAKE_CURRENT_SOURCE_DIR}/SWI-Prolog.h
	    ${CMAKE_CURRENT_SOURCE_DIR}/os/SWI-Stream.h
	    DESTINATION ${SWIPL_INSTALL_INCLUDE})
install_src(core_compat_sicstus
	    FILES
	    ${CMAKE_CURRENT_SOURCE_DIR}/compat/sicstus.h
	    DESTINATION ${SWIPL_INSTALL_INCLUDE}/sicstus)
install_src(core_compat_yap
	    FILES
	    ${CMAKE_CURRENT_SOURCE_DIR}/compat/YapInterface.h
	    DESTINATION ${SWIPL_INSTALL_INCLUDE}/Yap)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/dot.txt
	DESTINATION ${SWIPL_INSTALL_PREFIX}
	RENAME "swipl.home")
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/dotdot.txt
	DESTINATION ${SWIPL_INSTALL_PREFIX}/bin
	RENAME "swipl.home")

# Install cmake config files
install(EXPORT SWIPL_EXPORT
	FILE SWIPLTargets.cmake
	NAMESPACE "${SWIPL_CMAKE_NAMESPACE}"
	DESTINATION "${SWIPL_INSTALL_CMAKE_CONFIG_DIR}")
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/SWIPLConfig.cmake"
              "${CMAKE_CURRENT_BINARY_DIR}/SWIPLConfigVersion.cmake"
        DESTINATION "${SWIPL_INSTALL_CMAKE_CONFIG_DIR}")

if(SWIPL_INSTALL_PKGCONFIG)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/swipl.pc
	DESTINATION ${SWIPL_INSTALL_PKGCONFIG})
endif()
if(SWIPL_INSTALL_MANPAGES)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/swipl.1
	DESTINATION ${SWIPL_INSTALL_MANPAGES})
install(FILES swipl-ld.1
	DESTINATION ${SWIPL_INSTALL_MANPAGES})
endif()

if(BUILD_SWIPL_LD)
install(TARGETS swipl-ld
	RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE}
)
endif()

if(WIN32)
install(TARGETS swipl-win
	RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE}
        LIBRARY DESTINATION ${SWIPL_INSTALL_ARCH_LIB}
)
prepend(WIN32_DLL_FILES ${CMAKE_CURRENT_BINARY_DIR}/ ${WIN32_DLLS})
install(FILES ${WIN32_DLL_FILES} DESTINATION ${SWIPL_INSTALL_ARCH_EXE})
install_src(core_swipl_ico
	    FILES ${CMAKE_CURRENT_SOURCE_DIR}/swipl.ico
	    DESTINATION ${SWIPL_INSTALL_PREFIX})

else(WIN32)

if(NOT BUILD_MACOS_BUNDLE)
# Create symbolic link from public installation dir to executables
install(DIRECTORY DESTINATION bin)
ilink(${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_ARCH_EXE}/swipl
      ${CMAKE_INSTALL_PREFIX}/bin/swipl)
ilink(${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_ARCH_EXE}/swipl-ld
      ${CMAKE_INSTALL_PREFIX}/bin/swipl-ld)
endif()

endif(WIN32)

