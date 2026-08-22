include_guard(GLOBAL)

# Generate macOS `.dSYM' debug bundles
#
# On macOS the DWARF produced by the compiler stays in the object files.
# The linked binary only carries a _debug map_ that points at them, and
# atos(1) follows that map to turn an address into a source location.
# That is how '$foreign_predicate_source'/2 reports where a predicate
# defined in C lives, which is used by edit/1 and to make predicate
# references in messages a hyperlink.
#
# The debug map is fragile: it refers to the object files by path and
# modification time, so it stops working once they are removed, and some
# toolchains produce a map that atos cannot follow (notably MacPorts
# gcc).  Running dsymutil(1) after linking collects the DWARF into a
# `<binary>.dSYM' bundle next to the binary, which atos locates by UUID.
#
# The bundles are build tree artefacts: they are deliberately not
# installed.

set(MACOS_DSYM_DEFAULT OFF)

if(APPLE)
  find_program(DSYMUTIL NAMES dsymutil)
  mark_as_advanced(DSYMUTIL)

  # /usr/bin/dsymutil may be an xcode-select shim that cannot find the
  # real tool.  Only use it if it actually runs, or the link would fail.

  if(DSYMUTIL AND NOT DEFINED MACOS_DSYM_WORKS)
    execute_process(COMMAND ${DSYMUTIL} --version
		    RESULT_VARIABLE dsymutil_status
		    OUTPUT_QUIET ERROR_QUIET)
    if(dsymutil_status EQUAL 0)
      set(MACOS_DSYM_WORKS ON CACHE INTERNAL "dsymutil(1) is usable")
    else()
      message(STATUS "dsymutil(1) found but not usable; no .dSYM bundles")
      set(MACOS_DSYM_WORKS OFF CACHE INTERNAL "dsymutil(1) is usable")
    endif()
  endif()

  if(MACOS_DSYM_WORKS)
    string(TOUPPER "${CMAKE_BUILD_TYPE}" MACOS_DSYM_BUILD_TYPE)
    if(MACOS_DSYM_BUILD_TYPE MATCHES "^(DEBUG|RELWITHDEBINFO|PGO|SANITIZE)$")
      set(MACOS_DSYM_DEFAULT ON)
    endif()
  endif()
endif()

option(MACOS_DSYM
       "Run dsymutil(1) on the libraries and programs we build"
       ${MACOS_DSYM_DEFAULT})

# add_dsym(target)
#
# Create `<target>.dSYM' next to the binary  after linking it.  Does
# nothing if MACOS_DSYM is disabled or dsymutil(1) is not available.
#
# Frameworks are skipped: their binary lives inside the bundle, so the
# bundle would end up in the installed framework.

function(add_dsym target)
  if(NOT MACOS_DSYM OR NOT MACOS_DSYM_WORKS)
    return()
  endif()

  get_target_property(dsym_framework ${target} FRAMEWORK)
  if(dsym_framework)
    return()
  endif()

  add_custom_command(
      TARGET ${target} POST_BUILD
      COMMAND ${DSYMUTIL} $<TARGET_FILE:${target}>
      COMMENT "dsymutil: $<TARGET_FILE_NAME:${target}>"
      VERBATIM)
endfunction()
