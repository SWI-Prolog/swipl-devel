# Fixup the installation  locations  after   we  set  platform dependent
# variables such as `SWIPL_ARCH`.
#
# Note that this is the  second  step.   The  first  step is done before
# platform dependent variables are setup.

if(SWIPL_INSTALL_GENERIC_BUNDLE)
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
endif()

if(NOT SWIPL_INSTALL_MODULES)
  set(SWIPL_INSTALL_MODULES  ${SWIPL_INSTALL_PREFIX}/lib/${SWIPL_ARCH})
endif()

set(SWIPL_CMAKE_NAMESPACE swipl::)
if(NOT SWIPL_INSTALL_CMAKE_CONFIG_DIR)
  set(SWIPL_INSTALL_CMAKE_CONFIG_DIR lib/cmake/swipl)
endif()

if(NOT SWIPL_INSTALL_SHARE_PREFIX)
  set(SWIPL_INSTALL_SHARE_PREFIX "${SWIPL_INSTALL_PREFIX}")
endif()

set(SWIPL_BOOT_BASE        "boot.prc")
set(SWIPL_BOOT_FILE        "${CMAKE_CURRENT_BINARY_DIR}/home/${SWIPL_BOOT_BASE}")
set(SWIPL_ABI_FILE         "${CMAKE_CURRENT_BINARY_DIR}/home/ABI")
set(SWIPL_INSTALL_APP	   ${SWIPL_INSTALL_PREFIX}/app)
set(SWIPL_INSTALL_LIBRARY  ${SWIPL_INSTALL_PREFIX}/library)
set(SWIPL_INSTALL_BOOT     ${SWIPL_INSTALL_PREFIX}/boot)
set(SWIPL_INSTALL_INCLUDE  ${SWIPL_INSTALL_PREFIX}/include)
set(SWIPL_INSTALL_DOC	   ${SWIPL_INSTALL_SHARE_PREFIX}/doc)
if(INSTALL_TESTS)
  set(INSTALL_TESTS_DIR   ${SWIPL_INSTALL_PREFIX}/test)
endif()
