# Set a default build type if none was specified
# Based on https://blog.kitware.com/cmake-and-the-default-build-type/
if(NOT default_build_type)
  set(default_build_type "Release")
  if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
    set(default_build_type "RelWithDebInfo")
  endif()
endif()

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE
      STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
	       "Debug" "Release" "MinSizeRel" "RelWithDebInfo" "PGO" "DEB")
endif()

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
# when debugging the Prolog internals.
# For GCC, using -O3 makes the program bigger and slower.  -O2 is
# better.  Possibly tuning individual flags can reach better results.

set(SANITIZE "address" CACHE STRING
  "Value for -fsanitize when using -DCMAKE_BUILD_TYPE=Sanitize (address)")

if(EMSCRIPTEN)
set(GCC_GFLAGS "-g -gsource-map")
else()
set(GCC_GFLAGS "-gdwarf-2 -g3")
endif()
if(CMAKE_COMPILER_IS_GNUCC)
  if($ENV{CFLAGS})
    string(REGEX MATCH "-O" match $ENV{CFLAGS})
  endif()
  if(NOT match)
    set(GCC_OPTFLAGS -O2)
  endif()

  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG -DO_DEBUG_ATOMGC -O0 ${GCC_GFLAGS} $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "${GCC_OPTFLAGS} ${GCC_GFLAGS} $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE "${GCC_OPTFLAGS} $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_C_FLAGS_PGO "${GCC_OPTFLAGS} ${GCC_GFLAGS} $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a PGO build" FORCE)
  set(CMAKE_C_FLAGS_SANITIZE
      "-O0 ${GCC_GFLAGS} -fsanitize=${SANITIZE} -fno-omit-frame-pointer $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)

  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG -O0 ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${GCC_OPTFLAGS} ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE "${GCC_OPTFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_CXX_FLAGS_SANITIZE
      "-O0 ${GCC_GFLAGS} -fsanitize=${SANITIZE} -fno-omit-frame-pointer $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)
elseif(CMAKE_C_COMPILER_ID STREQUAL Clang)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG ${GCC_GFLAGS} $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_SANITIZE
      "${GCC_GFLAGS} -fsanitize=${SANITIZE} -O1 -fno-omit-frame-pointer $ENV{CFLAGS}"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)

  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${GCC_OPTFLAGS} ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_CXX_FLAGS_RELEASE "${GCC_OPTFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_CXX_FLAGS_SANITIZE
      "${GCC_GFLAGS} -fsanitize=${SANITIZE} -O1 -fno-omit-frame-pointer $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)
elseif(CMAKE_C_COMPILER_ID STREQUAL AppleClang)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
elseif(EMSCRIPTEN)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG "-DO_DEBUG ${GCC_GFLAGS} $ENV{CXXFLAGS}"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_EXE_LINKER_FLAGS "-sASSERTIONS"
      CACHE STRING "LDFLAGS for a Debug build" FORCE)
elseif(MSVC)
else()
  message("Unknown C compiler.  ${CMAKE_C_COMPILER_ID}")
endif()
