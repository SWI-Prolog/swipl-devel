include(CMakeDependentOption)

option(SWIPL_PACKAGES
       "Build and install packages"
       ON)

macro(pkg_option name comment)
  cmake_dependent_option(
      SWIPL_PACKAGES_${name} ${comment} ON
      SWIPL_PACKAGES OFF)
endmacro()

pkg_option(BASIC "Basic packages")
pkg_option(ODBC  "ODBC interface")
pkg_option(JAVA  "Java interface (JPL)")
pkg_option(X     "Graphics (xpce)")

set(SWIPL_PACKAGE_LIST_BASIC
    archive
    bdb
    chr
    clib
    clpqr
    inclpr
    cpp
    http
    ltx2htm
    nlp
    paxos
    pcre
    PDT
    pengines
    pldoc
    plunit
    protobufs
    RDF
    semweb
    sgml
    ssl
    table
    tipc
    utf8proc
    yaml
    zlib)

set(SWIPL_PACKAGE_LIST_ODBC
    odbc
    cql)

set(SWIPL_PACKAGE_LIST_JAVA
    jpl)

set(SWIPL_PACKAGE_LIST_X
    xpce)

# has_package(name var)
#
# Set var to ON if the package  name   is  included in the targets. Note
# that as of cmake 3.3. we can use if("string" IN_LIST list).

function(has_package name var)
  list(FIND SWIPL_PACKAGE_LIST ${name} index)
  if ( index GREATER -1 )
    set(${var} ON PARENT_SCOPE)
  else()
    set(${var} OFF PARENT_SCOPE)
  endif()
endfunction()

# del_package(name reason)
#

function(swipl_del_package pkg reason)
  has_package(${pkg} has_pkg)
  if(has_pkg)
    list(REMOVE_ITEM SWIPL_PACKAGE_LIST ${pkg})
    set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
    message("-- Dropped package ${pkg}: ${reason}")
  endif()
endfunction()

# add_package(name reason)

function(swipl_add_package pkg reason)
  has_package(${pkg} has_pkg)
  if(NOT has_pkg)
    list(APPEND SWIPL_PACKAGE_LIST ${pkg})
    set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
    message("-- Added package ${pkg}: ${reason}")
  endif()
endfunction()

function(add_package_sets)
  foreach(set ${ARGN})
    if(${SWIPL_PACKAGES_${set}})
      set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} ${SWIPL_PACKAGE_LIST_${set}})
    endif()
  endforeach()
  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
endfunction()

set(SWIPL_PACKAGE_LIST)
if(SWIPL_PACKAGES)
  add_package_sets(BASIC ODBC JAVA X)
endif()

if(NOT SWIPL_SHARED_LIB)
  swipl_del_package(jpl "not compatible with a static Prolog kernel")
endif()

if(INSTALL_DOCUMENTATION)
  swipl_add_package(ltx2htm "required to build documentation")
endif()
