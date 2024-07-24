# Populate parms.h

set(SWIPL_CC "from-cmake" CACHE STRING
    "Value for c_cc flag.  When from-cmake, we use CMAKE_C_COMPILER")
set(SWIPL_CXX "from-cmake" CACHE STRING
    "Value for c_cxx flag.  When from-cmake, we use CMAKE_CXX_COMPILER")

if(SWIPL_CC STREQUAL "from-cmake")
  set(SWIPL_CC ${CMAKE_C_COMPILER})
endif()
if(SWIPL_CXX STREQUAL "from-cmake")
  set(SWIPL_CXX ${CMAKE_CXX_COMPILER})
endif()

if(NOT PLHOME)
  set(PLHOME   ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_PREFIX})
endif()
if(SWIPL_INSTALL_IN_SHARE)
  set(PLSHAREDHOME ${CMAKE_INSTALL_PREFIX}/share/${SWIPL_INSTALL_DIR})
endif()
set(PLARCH   ${SWIPL_ARCH})
string(REGEX REPLACE "\\." "" SO_EXT "${CMAKE_SHARED_MODULE_SUFFIX}")

if(NOT SWIPL_INSTALL_AS_LINK AND NOT PLRELHOME)
  if(SWIPL_INSTALL_WIN_BUNDLE)
    set(PLRELHOME "..")
  elseif(IS_ABSOLUTE ${SWIPL_INSTALL_ARCH_EXE} AND IS_ABSOLUTE "${PLHOME}")
    file(RELATIVE_PATH PLRELHOME "${SWIPL_INSTALL_ARCH_EXE}" "${PLHOME}")
  endif()
endif()

if(SWIPL_SHARED_LIB)
  set(C_CFLAGS "-fPIC")
else()
  set(C_CFLAGS "")
endif()

if(CMAKE_EXECUTABLE_FORMAT STREQUAL "ELF")
  set(C_LIBPLSO "")
else()
  set(C_LIBPLSO "-lswipl")
endif()

if(MULTI_THREADED)
  set(C_CFLAGS "${C_CFLAGS} -pthread")
endif()

file(RELATIVE_PATH
     SWIPL_RELATIVE_LIBDIR
     ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_PREFIX}
     ${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_ARCH_LIB})

if(WIN32)
  set(SO_PATH PATH)
elseif(APPLE)
  set(SO_PATH DYLD_LIBRARY_PATH)
else()
  set(SO_PATH LD_LIBRARY_PATH)
endif()

get_filename_component(
  LIBPL_PATH
  "${LIBSWIPL_DIR}/libswipl.${SO_EXT}"
  ABSOLUTE
  BASE_DIR ${CMAKE_INSTALL_PREFIX})

if(CMAKE_EXECUTABLE_FORMAT)
string(TOLOWER ${CMAKE_EXECUTABLE_FORMAT} EXEC_FORMAT)
endif()

if(PLHOME EQUAL no-home)
  set(PLHOME)
endif()
