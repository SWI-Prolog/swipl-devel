#
# - Find GMP/MPIR libraries and headers
# This module defines the following variables:
#
# GMP_FOUND         - true if GMP/MPIR was found
# GMP_INCLUDE_DIRS  - include search path
# GMP_LIBRARIES      - libraries to link with
# GMP_LIBRARY_DLL    - library DLL to install. Only available on WIN32.
# GMP_LIBRARIES_DIR - the directory the library we link with is found in.

if (ANDROID AND (NOT $ENV{TERMUX_CMAKE_BUILD}))
set( GMP_ROOT ${CMAKE_SOURCE_DIR}/../gmp/${ANDROID_ABI} )
  set (GMP_FOUND ON)
  set (GMP_INCLUDE_DIRS ${GMP_ROOT})
  set (GMP_LIBRARIES ${GMP_ROOT}/libgmp.so)
  set (GMP_LIBRARIES_DIR ${GMP_ROOT})
else(ANDROID AND (NOT $ENV{TERMUX_CMAKE_BUILD}))
if(MSVC)
   find_library(GMP_LIBRARIES NAMES mpir mpird
                PATHS
			$ENV{GMP_ROOT}
			$ENV{GMP_ROOT}/lib
			${GMP_ROOT}
			${GMP_ROOT}/lib
			${CMAKE_SOURCE_DIR}/../tools/mpir/lib
			${CMAKE_SOURCE_DIR}/../tools/mpird/lib
			${CMAKE_SOURCE_DIR}/../mpir/lib
			${CMAKE_SOURCE_DIR}/../mpird/lib

				$ENV{PROGRAMFILES}/mpir/lib
			$ENV{PROGRAMFILES}/mpird/lib
			$ENV{HOME}/mpir/lib
			$ENV{HOME}/mpird/lib
			${CMAKE_INSTALL_PREFIX}/lib
			DOC "Try first the MPIR DLL when in an Windows environment"
	  )

		get_filename_component(GMP_LIBRARIES_DIR "${GMP_LIBRARIES}" PATH)

find_file(GMP_LIBRARY_DLL NAMES mpir.dll mpird.dll
        PATHS
			${GMP_LIBRARIES_DIR}/../bin
			${GMP_LIBRARIES_DIR}
)

   find_path(GMP_INCLUDE_DIRS
        NAMES mpir.h mpird.h
        PATHS
			${GMP_LIBRARIES_DIR}/../include
			${GMP_LIBRARIES_DIR}
)

else(MSVC)

#use GMP, notice that there are two cases, everything is the same directory, or everything is in
#its proper places


  find_library(GMP_LIBRARIES
	  NAMES gmp	libgmp
                HINTS
                .
			$ENV{GMP_ROOT}
			$ENV{GMP_ROOT}/lib
			${GMP_ROOT}
			    			${GMP_ROOT}/lib
			/usr/local/opt/gmp/lib
			/opt/lib
			/usr/local/lib
			$ENV{HOME}/lib
			${CMAKE_INSTALL_PREFIX}/lib
  )


  find_path(GMP_INCLUDE_DIRS
          NAMES gmp.h
          HINTS
              .
              $ENV{GMP_ROOT}
              $ENV{GMP_ROOT}/include
              ${GMP_ROOT}
              ${GMP_ROOT}/include
  			${GMP_LIBRARIES_DIR}/../include
  			${GMP_LIBRARIES_DIR}
  			)

get_filename_component(GMP_LIBRARIES_DIR "${GMP_LIBRARIES}" PATH CACHE)


endif(MSVC)
endif(ANDROID AND (NOT $ENV{TERMUX_CMAKE_BUILD}))

# handle the QUIET and REQUIRED arguments and set GMP_FOUND to TRUE if
# all listed variables are true
include(FindPackageHandleStandardArgs)
if(MSVC)
  find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_LIBRARIES_DIR GMP_LIBRARY_DLL GMP_INCLUDE_DIRS)
mark_as_advanced(GMP_LIBRARY_DLL)
else()
  find_package_handle_standard_args(GMP DEFAULT_MSG GMP_LIBRARIES GMP_LIBRARIES_DIR GMP_INCLUDE_DIRS)
endif()

mark_as_advanced(GMP_LIBRARIES GMP_LIBRARIES_DIR GMP_INCLUDE_DIRS)
