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
