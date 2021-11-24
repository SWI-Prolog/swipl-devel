if(APPLE)
  include(port/Darwin)
elseif(WIN32)
  include(port/Windows)
elseif(EMSCRIPTEN)
  include(port/Emscripten)
elseif(ANDROID)
  include(port/Android)
endif()

# Architecture id (Prolog flag `arch`, architecture subdirs)
if(NOT SWIPL_ARCH)
  string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
	 SWIPL_ARCH)
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
# In case #3 we need to make two builds:
#   1. Compile SWI-Prolog on the build host with a compatible pointer size
#   2. Cross-compile to the final target
#
# The first step is used to produce build artifacts that are needed
# by the cross-compilation step. SWIPL_NATIVE_FRIEND points to the directory
# which contains the build for step #1.
#

# ${SWIPL_NATIVE_FRIEND}, is  either  the  name  of  a  sibling  build
# directory  containing  a compatible native SWI-Prolog or an absolute   path
# pointing at such a directory.  By compatible we mean that the pointer size
# is the same.
#
# The following variables are set, and can be used elsewhere:
#
#   - ${PROG_SWIPL} is a native or friend Prolog executable that can
#                   be used for general Prolog tasks such as generating
#                   the library index or documentation
#
#   - ${PROG_SWIPL_FOR_BOOT} is a native or friend Prolog executable that can
#                            be used for compiling boot.prc and .qlf files.
#                            These used to be platform specific.  Now,
#			     ${PROG_SWIPL} should do. We keep a separate
#			     variable for the time being.
#
# Note: The helper tools mkvmi and  defatoms   (see  src  directory) are built
# using ${CMAKE_HOST_CC} when cross-compiling, and do not need SWIPL_NATIVE_FRIEND.

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
    set(PROG_SWIPL_FOR_BOOT ${PROG_SWIPL})
  else()
    set(PROG_SWIPL_TARGET_FILE $<TARGET_FILE:swipl>
	CACHE STRING "SWI-Prolog executable to perform build tasks")
    set(PROG_SWIPL ${CMAKE_CROSSCOMPILING_EMULATOR} ${PROG_SWIPL_TARGET_FILE})
    if(CMAKE_BUILD_TYPE STREQUAL "PGO")
      set(PROG_SWIPL_FOR_BOOT ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:swipl-pgo-instrumented>)
      set(DEP_SWIPL_FOR_BOOT $<TARGET_FILE:swipl-pgo-instrumented>)
    else()
      set(PROG_SWIPL_FOR_BOOT ${PROG_SWIPL})
      set(DEP_SWIPL_FOR_BOOT ${PROG_SWIPL_TARGET_FILE})
    endif()
  endif()
elseif((CMAKE_BUILD_TYPE STREQUAL "PGO" OR CMAKE_BUILD_TYPE STREQUAL "DEB") AND
       EXISTS "${PGO_PROGRAM}")
  set(CMAKE_HOST_EXECUTABLE_SUFFIX ${CMAKE_EXECUTABLE_SUFFIX})
  set(PROG_SWIPL $<TARGET_FILE:swipl>)
  set(PROG_SWIPL_FOR_BOOT $<TARGET_FILE:swipl-pgo-instrumented>)
  set(DEP_SWIPL_FOR_BOOT $<TARGET_FILE:swipl-pgo-instrumented>)
else(CMAKE_CROSSCOMPILING)
  set(CMAKE_HOST_EXECUTABLE_SUFFIX ${CMAKE_EXECUTABLE_SUFFIX})
  set(PROG_SWIPL $<TARGET_FILE:swipl>)
  set(PROG_SWIPL_FOR_BOOT ${PROG_SWIPL})
  set(DEP_SWIPL_FOR_BOOT ${PROG_SWIPL})
endif(CMAKE_CROSSCOMPILING)

################
# Misc tests

include(CheckFloatingPointFormat)
include(TestBigEndian)

TEST_BIG_ENDIAN(WORDS_BIGENDIAN)
ub_check_floating_point_format(IEEE754_FLOATS FLOAT_BYTES_BIGENDIAN FLOAT_WORDS_BIGENDIAN)
