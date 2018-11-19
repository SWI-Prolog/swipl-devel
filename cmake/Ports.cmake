# Architecture ID
string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
       SWIPL_ARCH)

if(APPLE)
  include(port/Darwin)
elseif(WIN32)
  include(port/Windows)
elseif(EMSCRIPTEN)
  include(port/Emscripten)
endif()

# Setup cross compiling.  As discussed with _erlanger_ in
# https://github.com/SWI-Prolog/swipl-devel/issues/358#issuecomment-439675854
# we distinguish three scenarios for building:
#
#   1. Native compilation
#   2. Cross compilation with a good emulator (Wine for Windows, Node.js
#      for WASM)
#   3. Cross compilation without a usable emulator.
#
# The helper tools mkvmi and  defatoms   (see  src  directory) are build
# using ${CMAKE_HOST_CC} when cross-compiling.
#
# Prolog build steps may be executed using ${SWIPL_NATIVE_FRIEND}, which
# is  either  the  name  of  a  sibling  build  directory  containing  a
# compatible native SWI-Prolog or an absolute   path  pointing at such a
# directory.

if(CMAKE_CROSSCOMPILING)
  set(CMAKE_HOST_EXECUTABLE_SUFFIX "" CACHE STRING
      "File name extension for executables for the host")
  set(CMAKE_HOST_CC cc CACHE STRING
      "C compiler used to compile build tools while cross-compiling")

  set(SWIPL_NATIVE_FRIEND "" CACHE STRING
      "CMake binary directory holding a compatible native system")

  if(SWIPL_NATIVE_FRIEND)
    if(IS_ABSOLUTE ${SWIPL_NATIVE_FRIEND})
      set(SWIPL_NATIVE_FRIEND_DIR ${SWIPL_NATIVE_FRIEND})
    else()
      set(SWIPL_NATIVE_FRIEND_DIR ${CMAKE_SOURCE_DIR}/${SWIPL_NATIVE_FRIEND})
    endif()

    set(PROG_SWIPL
	${SWIPL_NATIVE_FRIEND_DIR}/src/swipl${CMAKE_HOST_EXECUTABLE_SUFFIX}
	CACHE STRING "SWI-Prolog executable to perform build tasks")
  else()
    set(PROG_SWIPL swipl
	CACHE STRING "SWI-Prolog executable to perform build tasks")
  endif()
else(CMAKE_CROSSCOMPILING)
  set(CMAKE_HOST_EXECUTABLE_SUFFIX ${CMAKE_EXECUTABLE_SUFFIX})
  set(PROG_SWIPL swipl)
endif(CMAKE_CROSSCOMPILING)

