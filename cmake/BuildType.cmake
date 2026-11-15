# Set a default build type if none was specified
# Based on https://blog.kitware.com/cmake-and-the-default-build-type/
if(NOT default_build_type)
  set(default_build_type "Release")
  if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
    set(default_build_type "RelWithDebInfo")
  endif()
endif()

find_program(APT apt)

# As is, PGO builds do not  build   with  multi-config  generators as it
# depends on more than just  changing   some  C/C++ compile flags. Also,
# there is no point using this for the DEB build type.

set(SWIPL_CORE_BUILD_TYPES
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "Sanitize")
if(MSVC)
  list(APPEND SWIPL_CORE_BUILD_TYPES "PGO")
endif()
set(SWIPL_EXTRA_BUILD_TYPES)
if(CMAKE_COMPILER_IS_GNUCC OR
   CMAKE_C_COMPILER_ID STREQUAL Clang OR
   CMAKE_C_COMPILER_ID STREQUAL AppleClang)
  list(APPEND SWIPL_EXTRA_BUILD_TYPES "PGO")
endif()
if(APT)
  list(APPEND SWIPL_EXTRA_BUILD_TYPES "DEB")
endif()

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
      STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
	       ${SWIPL_CORE_BUILD_TYPES} ${SWIPL_EXTRA_BUILD_TYPES})
endif()

set(CMAKE_CONFIGURATION_TYPES ${SWIPL_CORE_BUILD_TYPES} CACHE
    STRING "Configurations for multi-config generators." FORCE)

if(CMAKE_BUILD_TYPE STREQUAL "DEB")
  message("-- Setting up flags for Debian based distro packaging")

  function(dpkg_buildflags var flags)
    execute_process(COMMAND dpkg-buildflags --get ${flags}
		    OUTPUT_VARIABLE ${var}
		    OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(var ${var} PARENT_SCOPE)
  endfunction()

  dpkg_buildflags(CMAKE_C_FLAGS_DEB		CFLAGS)
  dpkg_buildflags(CMAKE_CXX_FLAGS_DEB           CPPFLAGS)
  dpkg_buildflags(CMAKE_SHARED_LINKER_FLAGS_DEP LDFLAGS)
  dpkg_buildflags(CMAKE_EXE_LINKER_FLAGS_DEP    LDFLAGS)
endif()

# Using gdwarf-2 -g3 allows using macros in gdb, which helps a lot
# when debugging the Prolog internals.  Possibly tuning individual
# optimization flags can reach better results than the -O below.

set(SANITIZE "address" CACHE STRING
  "Value for -fsanitize when using -DCMAKE_BUILD_TYPE=Sanitize (address)")

# Establish CC_DBGFLAGS: the debug flags

if(EMSCRIPTEN)
  set(CC_DBGFLAGS "-g -gsource-map")
  if(NOT VMI_FUNCTIONS)	 # Otherwise too many locals in PL_next_solution()
    set(CC_DBGFLAGS "${CC_DBGFLAGS} -O1")
  endif()
elseif(CMAKE_C_COMPILER_ID STREQUAL Clang OR
       CMAKE_C_COMPILER_ID STREQUAL AppleClang)
  # The macros are for gdb and gcc: clang accepts -g3 and emits the same
  # object as -g2, macro information included in neither (-fdebug-macro
  # is its option for that, and it crashes Apple clang 21).  All
  # -gdwarf-2 does here is take the debug info back from DWARF 5 to
  # DWARF 2 and the accelerator tables from __debug_names to the legacy
  # __apple_* ones, which is the format lldb is least tested against.
  set(CC_DBGFLAGS "-g")
elseif(CMAKE_COMPILER_IS_GNUCC AND APPLE)
  # gcc-mp-15 only support -g2
  set(CC_DBGFLAGS "-gdwarf-2 -g2")
else()
  set(CC_DBGFLAGS "-gdwarf-2 -g3")
endif()

# Establish CC_OPTFLAGS and CXX_OPTFLAGS: the optimization flags of the
# build types below.  If the environment asks for a level we use that
# one rather than our default: cmake puts $CFLAGS and $CXXFLAGS in
# CMAKE_C_FLAGS and CMAKE_CXX_FLAGS, which come before the flags of the
# build type on the command line, so our -O would be the one that
# counts.
#
# Only the -O option is taken.  The rest of the environment is on the
# command line already and repeating it there only makes it longer; the
# C and the C++ flags must be kept apart as well, and taking $CFLAGS
# whole is how they came to be mixed: the C++ build types used the C
# optimization flags, so every option of $CFLAGS reached the C++
# compiler.  See https://github.com/SWI-Prolog/swipl-devel/issues/1518

function(opt_flags var env_flags)
  string(REGEX MATCHALL "(^| )-O[a-zA-Z0-9]*" opts "${env_flags}")
  if(opts)
    list(GET opts -1 opt)		# the last one is the one gcc obeys
    string(STRIP "${opt}" opt)
    set(${var} ${opt} PARENT_SCOPE)
  elseif(EMSCRIPTEN)
    set(${var} "-O3 -DNDEBUG" PARENT_SCOPE)
  elseif(CMAKE_COMPILER_IS_GNUCC OR
         CMAKE_C_COMPILER_ID STREQUAL Clang OR
         CMAKE_C_COMPILER_ID STREQUAL AppleClang)
    # Measured with bench/run.pl on Apple clang 21 (arm64): -O3 runs the
    # benchmarks in 7.52s against 7.64s for -O2, for 5% more library.
    set(${var} -O3 PARENT_SCOPE)
  else()
    set(${var} -O2 PARENT_SCOPE)
  endif()
endfunction()

opt_flags(CC_OPTFLAGS  "$ENV{CFLAGS}")
opt_flags(CXX_OPTFLAGS "$ENV{CXXFLAGS}")

# No build type below defines NDEBUG, so assert() is live in a released
# binary.  That is not an oversight and it is not free the other way
# about: measured with bench/run.pl on Apple clang 21 (arm64), a Release
# build with -DNDEBUG is 5% smaller and 8% slower, 32 of the 35
# benchmarks losing and none gaining.  assert(x) branches to a noreturn
# function, which leaves the compiler knowing x holds for the rest of
# the block; through the WAM interpreter that is worth more than the
# tests cost.  Measure again before adding it, x86_64 included.

if(CMAKE_COMPILER_IS_GNUCC)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG -DO_DEBUG_ATOMGC -O0 ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CC_OPTFLAGS} ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE "${CC_OPTFLAGS}"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_C_FLAGS_PGO "${CC_OPTFLAGS} ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a PGO build" FORCE)
  set(CMAKE_C_FLAGS_SANITIZE
      "-O0 ${CC_DBGFLAGS} -fsanitize=${SANITIZE} -fno-omit-frame-pointer"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG -O0 ${CC_DBGFLAGS}"
      CACHE STRING "CXXFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CXX_OPTFLAGS} ${CC_DBGFLAGS}"
      CACHE STRING "CXXFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE "${CXX_OPTFLAGS}"
      CACHE STRING "CXXFLAGS for a Release build" FORCE)
  set(CMAKE_CXX_FLAGS_SANITIZE
      "-O0 ${CC_DBGFLAGS} -fsanitize=${SANITIZE} -fno-omit-frame-pointer"
      CACHE STRING "CXXFLAGS for a Sanitize build" FORCE)
elseif(EMSCRIPTEN)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG ${CC_DBGFLAGS}"
      CACHE STRING "CXXFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE "${CC_OPTFLAGS}"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE "${CXX_OPTFLAGS}"
      CACHE STRING "CXXFLAGS for a Release build" FORCE)
  set(CMAKE_EXE_LINKER_FLAGS_DEBUG "-sASSERTIONS"
      CACHE STRING "LDFLAGS for a Debug build" FORCE)
elseif(CMAKE_C_COMPILER_ID STREQUAL Clang OR
       CMAKE_C_COMPILER_ID STREQUAL AppleClang)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CC_OPTFLAGS} ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE "${CC_OPTFLAGS}"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_C_FLAGS_SANITIZE
      "${CC_DBGFLAGS} -fsanitize=${SANITIZE} -O1 -fno-omit-frame-pointer"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)
  set(CMAKE_C_FLAGS_PGO "${CC_OPTFLAGS} -O3 ${CC_DBGFLAGS}"
      CACHE STRING "CFLAGS for a PGO build" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG ${CC_DBGFLAGS}"
      CACHE STRING "CXXFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CXX_OPTFLAGS} ${CC_DBGFLAGS}"
      CACHE STRING "CXXFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE "${CXX_OPTFLAGS}"
      CACHE STRING "CXXFLAGS for a Release build" FORCE)
  set(CMAKE_CXX_FLAGS_SANITIZE
      "${CC_DBGFLAGS} -fsanitize=${SANITIZE} -O1 -fno-omit-frame-pointer"
      CACHE STRING "CXXFLAGS for a Sanitize build" FORCE)
elseif(MSVC)
  # Common MSVC flags
  set(_SWI_MSVC_C_COMMON   "/nologo")
  set(_SWI_MSVC_CXX_COMMON "/nologo /EHsc")   # /EHsc for consistent C++ exception semantics

  # Debug flags: no optimization + full symbols + SWI debug macros
  set(CMAKE_C_FLAGS_DEBUG
      "${_SWI_MSVC_C_COMMON} /D_DEBUG /DO_DEBUG /DO_DEBUG_ATOMGC /Od /Zi"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG
      "${_SWI_MSVC_CXX_COMMON} /D_DEBUG /DO_DEBUG /DO_DEBUG_ATOMGC /Od /Zi $ENV{CXXFLAGS}"
      CACHE STRING "CXXFLAGS for a Debug build" FORCE)

  # RelWithDebInfo: optimization + symbols
  set(CMAKE_C_FLAGS_RELWITHDEBINFO
      "${_SWI_MSVC_C_COMMON} /O2 /Zi"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO
      "${_SWI_MSVC_CXX_COMMON} /O2 /Zi $ENV{CXXFLAGS}"
      CACHE STRING "CXXFLAGS for a RelWithDebInfo build" FORCE)

  # Release: optimization, no debug macros
  set(CMAKE_C_FLAGS_RELEASE
      "${_SWI_MSVC_C_COMMON} /O2"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE
      "${_SWI_MSVC_CXX_COMMON} /O2 $ENV{CXXFLAGS}"
      CACHE STRING "CXXFLAGS for a Release build" FORCE)

  # MinSizeRel: size-oriented optimization
  set(CMAKE_C_FLAGS_MINSIZEREL
      "${_SWI_MSVC_C_COMMON} /O1"
      CACHE STRING "CFLAGS for a MinSizeRel build" FORCE)
  set(CMAKE_CXX_FLAGS_MINSIZEREL
      "${_SWI_MSVC_CXX_COMMON} /O1 $ENV{CXXFLAGS}"
      CACHE STRING "CXXFLAGS for a MinSizeRel build" FORCE)

  # PGO base flags: same optimization as Release.  /GL (whole program
  # optimization) is added per-target in PGO.cmake so that utility tools
  # (defatom, mkvmi, etc.) are unaffected.
  set(CMAKE_C_FLAGS_PGO
      "${_SWI_MSVC_C_COMMON} /O2"
      CACHE STRING "CFLAGS for a PGO build" FORCE)
  set(CMAKE_CXX_FLAGS_PGO
      "${_SWI_MSVC_CXX_COMMON} /O2 $ENV{CXXFLAGS}"
      CACHE STRING "CXXFLAGS for a PGO build" FORCE)

  # Optional: Sanitize build type (if you intend to support it on MSVC)
  # MSVC uses /fsanitize=address (requires installed VS components).
  # Note: /D_DEBUG is omitted because MSVC ASAN requires the release CRT
  # (/MD), but _DEBUG pulls in debug-CRT symbols (_CrtDbgReport) and causes
  # Python to demand python3XX_d.lib.  /DO_DEBUG is omitted because during
  # boot.prc generation the library directory is not yet available, so
  # autoload(library(debug), ...) in tabling.pl fails.  Use the Debug
  # configuration for SWI-Prolog internal debug assertions.
  set(CMAKE_C_FLAGS_SANITIZE
      "${_SWI_MSVC_C_COMMON} /Od /Zi /fsanitize=address"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)
  set(CMAKE_CXX_FLAGS_SANITIZE
      "${_SWI_MSVC_CXX_COMMON} /Od /Zi /fsanitize=address $ENV{CXXFLAGS}"
      CACHE STRING "CXXFLAGS for a Sanitize build" FORCE)

  # Linker flags for Sanitize: all target types (EXE, SHARED, MODULE) need
  # /INCREMENTAL:NO because ASAN is incompatible with incremental linking
  # /DEBUG for better ASAN source-level error reporting
  set(CMAKE_EXE_LINKER_FLAGS_SANITIZE
      "/INCREMENTAL:NO /DEBUG"
      CACHE STRING "LDFLAGS for Sanitize EXEs" FORCE)
  set(CMAKE_SHARED_LINKER_FLAGS_SANITIZE
      "/INCREMENTAL:NO /DEBUG"
      CACHE STRING "LDFLAGS for Sanitize shared libs" FORCE)
  set(CMAKE_MODULE_LINKER_FLAGS_SANITIZE
      "/INCREMENTAL:NO /DEBUG"
      CACHE STRING "LDFLAGS for Sanitize module libs (plugins)" FORCE)
else()
  message("Unknown C compiler.  ${CMAKE_C_COMPILER_ID}")
endif()

# The variable CMAKE_MAP_IMPORTED_CONFIG_PGO tells cmake to use one of
# these build  types for the  dependencies.  This however  breaks SDL3
# and Python  dependencies using  cmake 4.2 on  Arch Linux.   It seems
# that not  defining this makes  it fall  back to something  that does
# work.  I  do not  fully understand  the issue,  so please  provide a
# correct portable way to fix this if you know how.

if(MSVC)
  set(CMAKE_MAP_IMPORTED_CONFIG_SANITIZE Release RelWithDebInfo "")
  set(CMAKE_MAP_IMPORTED_CONFIG_PGO Release RelWithDebInfo "")
endif()
