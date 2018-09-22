cmake_minimum_required(VERSION 2.8.12)
project(swipl)

option(BUILD_SHARED_LIBS
       "Do (not) build shared libraries"
       ON)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")

include(Version)
include(Utils)
include(Windows)
include(Darwin)
include(CStack)

include(CheckIncludeFile)
include(CheckLibraryExists)
include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckTypeSize)
include(AlignOf)
include(CheckStructHasMember)

find_package(Threads)
find_package(ZLIB)
find_package(GMP)
if(UNIX)
find_package(Curses)
endif(UNIX)

include(Config)
include(GCCBuiltins)
include(TestSignalType)
include(TestSCNProcessors)

# FIXME: Is this still needed?
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
  add_compile_options(-fno-strict-aliasing)
endif()

# Source locations
get_filename_component(SWIPL_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/.." ABSOLUTE)
set(SWIPL_LIBRARY_ROOT ${SWIPL_ROOT}/library)
set(SWIPL_BOOT_ROOT ${SWIPL_ROOT}/boot)

if(CMAKE_SIZEOF_VOID_P EQUAL 8)
  set(SWIPL_BOOT_FILE "boot64.prc")
else()
  set(SWIPL_BOOT_FILE "boot32.prc")
endif()

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
    pl-locale.c)
prepend(SRC_OS os/ ${SRC_OS})


set(SRC_TAI caltime_utc caltime_tai leapsecs_sub leapsecs_add
    caldate_fmjd caldate_mjd leapsecs_init leapsecs_read tai_pack
    tai_unpack)
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
    pl-trie.c pl-indirect.c pl-tabling.c pl-rsort.c pl-mutex.c)

set(HDR_CORE pl-atom.ih pl-funct.ih)

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


################
# Prolog data files
################

set(SWIPL_DATA_INDEXED_DIRS
    library library/clp library/dcg)
set(SWIPL_DATA_DIRS
    boot
    ${SWIPL_DATA_INDEXED_DIRS}
    library/dialect library/dialect/swi library/dialect/eclipse
    library/dialect/hprolog library/dialect/sicstus library/dialect/iso
    library/dialect/yap
    library/unicode
    demo)

set(SWIPL_DATA_boot init.pl syspred.pl toplevel.pl license.pl bags.pl apply.pl
    expand.pl dcg.pl history.pl attvar.pl packs.pl dwim.pl rc.pl predopts.pl
    parms.pl autoload.pl qlf.pl topvars.pl messages.pl load.pl dicts.pl
    engines.pl iri.pl tabling.pl)
prepend(SWIPL_BOOT_FILES ${SWIPL_BOOT_ROOT} ${SWIPL_DATA_boot})

set(SWIPL_DATA_library helpidx.pl help.pl explain.pl sort.pl
    qsave.pl shlib.pl statistics.pl system.pl error.pl
    backcomp.pl gensym.pl listing.pl debug.pl vm.pl
    quintus.pl edinburgh.pl ctypes.pl files.pl modules.pl
    edit.pl shell.pl check.pl heaps.pl console_input.pl
    tty.pl readln.pl readutil.pl make.pl hotfix.pl option.pl
    oset.pl ordsets.pl occurs.pl lists.pl pprint.pl
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
    lazy_lists.pl prolog_jiti.pl zip.pl obfuscate.pl)
if(WIN32)
  set(SWIPL_DATA_library ${SWIPL_DATA_library}
      dde.pl win_menu.pl)
endif()

set(SWIPL_DATA_library_clp bounds.pl clp_events.pl clp_distinct.pl
    simplex.pl clpfd.pl clpb.pl)

set(SWIPL_DATA_library_dcg basics.pl)

set(SWIPL_DATA_library_unicode blocks.pl unicode_data.pl)

set(SWIPL_DATA_library_dialect bim.pl commons.pl hprolog.pl ifprolog.pl
    sicstus.pl yap.pl)

set(SWIPL_DATA_library_dialect_swi syspred_options.pl)
set(SWIPL_DATA_library_dialect_eclipse test_util_iso.pl)
set(SWIPL_DATA_library_dialect_hprolog format.pl)
set(SWIPL_DATA_library_dialect_sicstus arrays.pl block.pl lists.pl
    README.TXT sockets.pl swipl-lfr.pl system.pl terms.pl timeout.pl)
set(SWIPL_DATA_library_dialect_iso iso_predicates.pl)
set(SWIPL_DATA_library_dialect_yap README.TXT)

set(SWIPL_DATA_demo likes.pl README)

# ${PL_LIB_FILES_ALL} holds all files from the library that are indexed
# ${PL_LIB_INDEX} holds the index file

set(PL_LIB_FILES_ALL)
foreach(d ${SWIPL_DATA_INDEXED_DIRS})
    string(REGEX REPLACE "/" "_" filevar ${d})
    prepend(files ${SWIPL_ROOT}/${d} ${SWIPL_DATA_${filevar}})
    set(PL_LIB_FILES_ALL ${PL_LIB_FILES_ALL} ${files})
endforeach()
set(PL_LIB_INDEX ${SWIPL_LIBRARY_ROOT}/INDEX.pl)

################
# Custom targets and commands
################

# FIXME: we should create these in the build directory
add_custom_target(
    vmi-metadata
    COMMAND mkvmi ${CMAKE_CURRENT_SOURCE_DIR}
    BYPRODUCTS pl-vmi.h pl-codetable.ic pl-jumptable.ic
    DEPENDS pl-vmi.c
    COMMENT "Generating VMI metadata"
)

add_custom_target(
    core-constants
    COMMAND defatom ${CMAKE_CURRENT_SOURCE_DIR}
    BYPRODUCTS pl-atom.ih pl-funct.ih
    DEPENDS ATOMS
    COMMENT "Generating static atoms and functors"
)

set(USE_GIT_VERSION_H 1)
add_custom_target(
    version.h
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/mkversion.sh
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Updating (git) version"
)

add_custom_command(
    OUTPUT  swipl.prc
    COMMAND swipl -O -b ${SWIPL_BOOT_ROOT}/init.pl
    DEPENDS swipl ${SWIPL_BOOT_FILES}
)

add_custom_command(
    OUTPUT  ${PL_LIB_INDEX}
    COMMAND swipl -f none -g "\"make_library_index('${SWIPL_LIBRARY_ROOT}')\"" -t halt
    DEPENDS swipl.prc ${PL_LIB_FILES_ALL}
)

add_custom_target(prolog_products ALL
    DEPENDS swipl.prc ${PL_LIB_INDEX}
)

configure_file(config.h.cmake config.h)

# Set swipl.home above binaries to a relative path to the root
file(RELATIVE_PATH SWIPL_HOME_RELATIVE
     ${CMAKE_CURRENT_BINARY_DIR}/.. ${SWIPL_ROOT}/X)
get_filename_component(SWIPL_HOME_RELATIVE ${SWIPL_HOME_RELATIVE} DIRECTORY)
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/../swipl.home" "${SWIPL_HOME_RELATIVE}\n")
# Final swipl.home for installed system
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/dot.txt" ".\n")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/dotdot.txt" "..\n")

# build helpers
add_executable(mkvmi mkvmi.c)
add_executable(defatom defatom.c)

# Establish libraries
if(CURSES_FOUND)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} ${CURSES_LIBRARIES})
endif()
if(GMP_FOUND)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} ${GMP_LIBRARIES})
  set(LIBSWIPL_INCLUDES ${LIBSWIPL_INCLUDES} ${GMP_INCLUDE_DIRS})
endif()
set(LIBSWIPL_LIBRARIES
    ${LIBSWIPL_LIBRARIES}
    ${ZLIB_LIBRARIES}
    ${CMAKE_THREAD_LIBS_INIT})
if(HAVE_LIBDL)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} dl)
endif()
if(HAVE_LIBM)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} m)
endif()
if(HAVE_LIBRT)
  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} rt)
endif()

# build swipl
add_executable(swipl ${SWIPL_SRC})
target_link_libraries(swipl libswipl)
target_c_stack(swipl 4000000)

# build the library
add_library(libswipl ${LIBSWIPL_SRC} ${HDR_CORE})
add_dependencies(libswipl version.h vmi-metadata core-constants)
set_target_properties(libswipl PROPERTIES
		      OUTPUT_NAME swipl
		      MACOSX_RPATH TRUE
		      VERSION ${SWIPL_VERSION_STRING}
		      SOVERSION ${SWIPL_VERSION_MAJOR})
target_include_directories(libswipl BEFORE PRIVATE
			   ${CMAKE_CURRENT_BINARY_DIR}
			   ${CMAKE_CURRENT_SOURCE_DIR}
			   ${ZLIB_INCLUDE_DIRS}
			   ${LIBSWIPL_INCLUDES})
target_link_libraries(libswipl ${LIBSWIPL_LIBRARIES})

add_executable(swipl-ld ${SRC_SWIPL_LD})
target_include_directories(swipl-ld BEFORE PRIVATE
			   ${CMAKE_CURRENT_BINARY_DIR}
			   ${CMAKE_CURRENT_SOURCE_DIR}
			   ${LIBSWIPL_INCLUDES})
add_dependencies(swipl-ld vmi-metadata core-constants)

if(WIN32)
include(WindowsTargets)
endif()

# testing (traditional)
add_custom_target(check
    COMMAND swipl -q -g test -t halt ${CMAKE_CURRENT_SOURCE_DIR}/test.pl
)
# For ctest
# FIXME: split the core tests such that ctest shows progress.
add_test(NAME core
	 COMMAND swipl -q -g test -t halt ${CMAKE_CURRENT_SOURCE_DIR}/test.pl)

# Populate parms.h, making the compilation environment known to Prolog
include(Params)
configure_file(parms.h.cmake parms.h)

################
# Installation
################

install(TARGETS swipl libswipl swipl-ld
	RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE}
        LIBRARY DESTINATION ${SWIPL_INSTALL_ARCH_LIB}
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/swipl.prc
	DESTINATION ${SWIPL_INSTALL_PREFIX}
	RENAME ${SWIPL_BOOT_FILE}
)
install(CODE "MESSAGE(\"Installing the library.\")")
foreach(d ${SWIPL_DATA_DIRS})
    string(REGEX REPLACE "/" "_" filevar ${d})
    prepend(files ${SWIPL_ROOT}/${d} ${SWIPL_DATA_${filevar}})
    install(FILES ${files}
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/${d})
endforeach()
install(FILES ${PL_LIB_INDEX}
	DESTINATION ${SWIPL_INSTALL_PREFIX}/library)
install(FILES
	${CMAKE_CURRENT_SOURCE_DIR}/SWI-Prolog.h
	${CMAKE_CURRENT_SOURCE_DIR}/os/SWI-Stream.h
	DESTINATION ${SWIPL_INSTALL_INCLUDE})
install(FILES
	${CMAKE_CURRENT_SOURCE_DIR}/compat/sicstus.h
	DESTINATION ${SWIPL_INSTALL_INCLUDE}/sicstus)
install(FILES
	${CMAKE_CURRENT_SOURCE_DIR}/compat/YapInterface.h
	DESTINATION ${SWIPL_INSTALL_INCLUDE}/Yap)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/dot.txt
	DESTINATION ${SWIPL_INSTALL_PREFIX}
	RENAME "swipl.home")
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/dotdot.txt
	DESTINATION ${SWIPL_INSTALL_PREFIX}/bin
	RENAME "swipl.home")

if(WIN32)
install(TARGETS swipl-win
	RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE}
        LIBRARY DESTINATION ${SWIPL_INSTALL_ARCH_LIB}
)
prepend(WIN32_DLL_FILES ${CMAKE_CURRENT_BINARY_DIR}/ ${WIN32_DLLS})
install(FILES ${WIN32_DLL_FILES} DESTINATION ${SWIPL_INSTALL_ARCH_EXE})
install(FILES swipl.ico DESTINATION ${SWIPL_INSTALL_PREFIX})

else(WIN32)

# Create symbolic link from public installation dir to executables
install(DIRECTORY DESTINATION bin)
ilink(${SWIPL_INSTALL_ARCH_EXE}/swipl
      ${CMAKE_INSTALL_PREFIX}/bin/swipl)
endif(WIN32)

# Final update of library indexes

run_installed_swipl(
	make
	COMMENT "-- Updating library indexes")
