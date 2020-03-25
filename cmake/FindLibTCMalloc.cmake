# - Try to find Google LibTCMalloc
# Once done this will define
#  LIBTCMALLOC_FOUND - System has tcmalloc (minimal)
#  LIBTCMALLOC_LIBRARIES - The libraries needed to use tcmalloc
#  LIBTCMALLOC_DEFINITIONS - Compiler switches required for using tcmalloc

find_package(PkgConfig)
pkg_check_modules(PC_LIBTCMALLOC_MINIMAL QUIET libtcmalloc_minimal)
set(LIBTCMALLOC_DEFINITIONS ${PC_LIBTCMALLOC_MINIMAL_CFLAGS_OTHER})

find_library(LIBTCMALLOC_LIBRARY NAMES tcmalloc_minimal
             HINTS ${PC_LIBTCMALLOC_MINIMAL_LIBDIR}
		   ${PC_LIBTCMALLOC_MINIMAL_LIBRARY_DIRS} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set LIBTCMALLOC_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(LibTCMalloc DEFAULT_MSG
                                  LIBTCMALLOC_LIBRARY)

mark_as_advanced(LIBTCMALLOC_LIBRARY )

set(LIBTCMALLOC_LIBRARIES ${LIBTCMALLOC_LIBRARY} )
