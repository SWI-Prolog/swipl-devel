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
if(CMAKE_COMPILER_IS_GNUCC)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG -DO_DEBUG_ATOMGC -O0 -gdwarf-2 -g3"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -gdwarf-2 -g3"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE "-O2"
      CACHE STRING "CFLAGS for a Release build" FORCE)
  set(CMAKE_C_FLAGS_PGO "-O2 -gdwarf-2 -g3"
      CACHE STRING "CFLAGS for a PGO build" FORCE)
  set(CMAKE_C_FLAGS_SANITIZE
      "-O0 -gdwarf-2 -g3 -fsanitize=address -fno-omit-frame-pointer"
      CACHE STRING "CFLAGS for a Sanitize build" FORCE)
elseif(CMAKE_C_COMPILER_ID STREQUAL AppleClang)
  set(CMAKE_C_FLAGS_DEBUG "-DO_DEBUG -gdwarf-2 -g3"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
else()
  message("Unknown C compiler.  ${CMAKE_C_COMPILER_ID}")
endif()
