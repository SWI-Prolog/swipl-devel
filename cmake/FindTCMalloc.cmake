# - Try to find Google TCMalloc
# Once done this will define
#  LIBTCMALLOC_FOUND - System has tcmalloc (minimal)
#  LIBTCMALLOC_INCLUDE_DIRS - The tcmalloc include directories
#  LIBTCMALLOC_LIBRARIES - The libraries needed to use tcmalloc
#  LIBTCMALLOC_DEFINITIONS - Compiler switches required for using tcmalloc

find_package(PkgConfig)
pkg_check_modules(PC_LIBTCMALLOC_MINIMAL QUIET libtcmalloc_minimal)
set(LIBTCMALLOC_DEFINITIONS ${PC_LIBTCMALLOC_MINIMAL_CFLAGS_OTHER})

find_path(LIBTCMALLOC_INCLUDE_DIR gperftools/tcmalloc.h
          HINTS ${PC_LIBTCMALLOC_MINIMAL_INCLUDEDIR}
	        ${PC_LIBTCMALLOC_MINIMAL_INCLUDE_DIRS})

find_library(LIBTCMALLOC_LIBRARY NAMES tcmalloc_minimal
             HINTS ${PC_LIBTCMALLOC_MINIMAL_LIBDIR}
		   ${PC_LIBTCMALLOC_MINIMAL_LIBRARY_DIRS} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LIBTCMALLOC_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(LibTcMalloc DEFAULT_MSG
                                  LIBTCMALLOC_LIBRARY LIBTCMALLOC_INCLUDE_DIR)

mark_as_advanced(LIBTCMALLOC_INCLUDE_DIR LIBTCMALLOC_LIBRARY )

set(LIBTCMALLOC_LIBRARIES ${LIBTCMALLOC_LIBRARY} )
set(LIBTCMALLOC_INCLUDE_DIRS ${LIBXML2_INCLUDE_DIR} )
