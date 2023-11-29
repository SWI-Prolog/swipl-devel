# Setup locations, notably for installing the  system. This is highly OS
# dependent, where on most OSes there  are multiple choices depending on
# whether the system is built for local use or some package manager.

# Set the installation prefix from $SWIPL_INSTALL_PREFIX.  If this
# environment variable ends in /@builddir@, replace @builddir@ with
# the name of the binary build directory, so we can easily install
# multiple different versions in the same filesystem

function(set_install_prefix)
  set(prefix "$ENV{SWIPL_INSTALL_PREFIX}")

  if(prefix)
    if(prefix MATCHES "@builddir@")
      get_filename_component(bindir ${CMAKE_CURRENT_BINARY_DIR} NAME)
      string(REGEX REPLACE "\@builddir\@" ${bindir} install_prefix ${prefix})
    else()
      set(install_prefix ${prefix})
    endif()
    set(CMAKE_INSTALL_PREFIX ${install_prefix}
	CACHE STRING "Installation prefix" FORCE)
  endif()
endfunction()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_install_prefix()
  message("-- Using install prefix \"${CMAKE_INSTALL_PREFIX}\"")
endif()

# Set well-known locations. Should be merged as  they are now at various
# places.

set(SWIPL_ROOT ${CMAKE_CURRENT_SOURCE_DIR})

################
# Installation directories

if(WIN32)
  # Place all .exe and .dll in one directory
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/src)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/src)

if(NOT MSYS2 AND NOT CONDA)
  set(SWIPL_INSTALL_WIN_BUNDLE ON)
endif()

endif()

if(SWIPL_INSTALL_WIN_BUNDLE)
# Install a classical Windows application as a self-contained
# directory, normally in C:\Program Files\swipl

  set(SWIPL_INSTALL_DIR "."
      CACHE STRING "Directory below <prefix> for installation")
  set(SWIPL_INSTALL_PREFIX   ${SWIPL_INSTALL_DIR})
  set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin)
  set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/bin)
  set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/bin)
  set(SWIPL_INSTALL_AS_LINK OFF)
  set(SWIPL_TMP_DIR "c:/tmp" CACHE STRING
      "Directory to be used if the environment variable TEMP is not set")

elseif(BUILD_MACOS_BUNDLE)
# Install as a MacOS application bundle

  set(SWIPL_INSTALL_PREFIX    SWI-Prolog.app/Contents/swipl)
  set(SWIPL_INSTALL_ARCH_EXE  SWI-Prolog.app/Contents/MacOS)
  set(SWIPL_INSTALL_ARCH_LIB  SWI-Prolog.app/Contents/Frameworks)
  set(SWIPL_INSTALL_MANPAGES  SWI-Prolog.app/Contents/man)
  set(SWIPL_INSTALL_PKGCONFIG SWI-Prolog.app/Contents/pkgconfig)
  set(SWIPL_INSTALL_RESOURCES SWI-Prolog.app/Contents/Resources)
  set(SWIPL_INSTALL_AS_LINK OFF)
  set(SWIPL_TMP_DIR "/tmp" CACHE STRING
      "Directory to be used if the environment variable TMP is not set")

else()
# Typical installation on POSIX systems. Also  used by Windows and MacOS
# if we do not build a  bundle,   e.g.,  using Macports, Homebrew, MSYS,
# Cygwin, Conda, etc.
#
# This installs in CMAKE_INSTALL_PREFIX/lib/swipl, where  `swipl` may be
# versioned, e.g., `swipl-9.1.9`. By default,   the  entire directory is
# stand-alone. The location of libswipl is   compiled  into swipl (using
# RPATH or similar mechanism). The system   is made publically available
# using  symlinks  from  CMAKE_INSTALL_PREFIX/bin  for  the  executables
# (swipl, swipl-ld and swipl-rc).
#
# Some Linux distributions do not like this.   For this reason there are
# the options `SWIPL_INSTALL_IN_LIB`, which installs  libswipl.so in the
# system default library directory  and  we   do  not  need `RPATH`. The
# option  `SWIPL_INSTALL_IN_SHARE`  installs   the    documentation   in
# CMAKE_INSTALL_PREFIX/share/swipl.
#
# On some systems we cannot create  symlinks,   so  we  must install the
# executables directory into the system binary   directory. In this case
# we compile a relative path to the   home.  Installing with symlinks or
# not is controlled by the option `SWIPL_INSTALL_AS_LINK`

  if(SWIPL_VERSIONED_DIR)		# install in lib/swipl-X.Y.Z
    set(SWIPL_INSTALL_DIR_DEF  swipl-${SWIPL_VERSION_STRING})
  else()
    set(SWIPL_INSTALL_DIR_DEF  swipl)
  endif()

  set(SWIPL_INSTALL_DIR ${SWIPL_INSTALL_DIR_DEF}
      CACHE STRING "Directory below <prefix> for installation")
  set(SWIPL_TMP_DIR "/tmp" CACHE STRING
      "Directory to be used if the environment variable TMP is not set")

  set(SWIPL_INSTALL_PREFIX   lib/${SWIPL_INSTALL_DIR})
  if(SWIPL_INSTALL_IN_SHARE)
    set(SWIPL_INSTALL_SHARE_PREFIX share/${SWIPL_INSTALL_DIR})
  endif()
  set(SWIPL_INSTALL_ARCH_LIB ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
  if(SWIPL_INSTALL_AS_LINK)
    set(SWIPL_INSTALL_ARCH_EXE ${SWIPL_INSTALL_PREFIX}/bin/${SWIPL_ARCH})
  else()
    set(SWIPL_INSTALL_ARCH_EXE ${CMAKE_INSTALL_PREFIX}/bin)
  endif()
  set(SWIPL_INSTALL_MANPAGES share/man/man1
      CACHE STRING "Directory for man pages")
  set(SWIPL_INSTALL_PKGCONFIG share/pkgconfig
      CACHE STRING "Directory for pkg-config pages")

  set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
endif()

set(SWIPL_CMAKE_NAMESPACE swipl::)
set(SWIPL_INSTALL_CMAKE_CONFIG_DIR lib/cmake/swipl)

if(NOT SWIPL_INSTALL_SHARE_PREFIX)
  set(SWIPL_INSTALL_SHARE_PREFIX "${SWIPL_INSTALL_PREFIX}")
endif()

set(SWIPL_BOOT_BASE        "boot.prc")
set(SWIPL_BOOT_FILE        "${CMAKE_CURRENT_BINARY_DIR}/home/${SWIPL_BOOT_BASE}")
set(SWIPL_INSTALL_LIBRARY  ${SWIPL_INSTALL_PREFIX}/library)
set(SWIPL_INSTALL_BOOT     ${SWIPL_INSTALL_PREFIX}/boot)
set(SWIPL_INSTALL_INCLUDE  ${SWIPL_INSTALL_PREFIX}/include)
set(SWIPL_INSTALL_DOC	   ${SWIPL_INSTALL_SHARE_PREFIX}/doc)
if(INSTALL_TESTS)
   set(INSTALL_TESTS_DIR   ${SWIPL_INSTALL_PREFIX}/test)
endif()
