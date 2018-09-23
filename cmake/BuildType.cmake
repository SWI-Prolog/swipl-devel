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
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

# Using gdwarf-2 -g3 allows using macros in gdb, which helps a lot
# when debugging the Prolog internals.
if(CMAKE_COMPILER_IS_GNUCC)
  set(CMAKE_C_FLAGS_DEBUG "-gdwarf-2 -g3"
      CACHE STRING "CFLAGS for a Debug build" FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -gdwarf-2 -g3"
      CACHE STRING "CFLAGS for a RelWithDebInfo build" FORCE)
  set(CMAKE_C_FLAGS_RELEASE "-O2"
      CACHE STRING "CFLAGS for a Release build" FORCE)
endif()
