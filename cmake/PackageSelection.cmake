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

# Package dependencies as computed using script/xref_packages.pl
# FIXME: Some dependencies should be avoided, for example:
#   - term//2 should move from `pengines` to `http`

set(SWIPL_PKG_DEPS_RDF clib semweb sgml)
set(SWIPL_PKG_DEPS_archive clib)
set(SWIPL_PKG_DEPS_clib sgml)
set(SWIPL_PKG_DEPS_http clib sgml ssl)
set(SWIPL_PKG_DEPS_ltx2htm clib)
set(SWIPL_PKG_DEPS_pengines clib http)
set(SWIPL_PKG_DEPS_pldoc clib http pengines sgml)
set(SWIPL_PKG_DEPS_semweb RDF clib http nlp sgml zlib)
set(SWIPL_PKG_DEPS_ssl clib http sgml zlib)
set(SWIPL_PKG_DEPS_tipc clib paxos)

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

  set(not_available)
  foreach(pkg ${SWIPL_PACKAGE_LIST})
    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/packages/${pkg}/CMakeLists.txt)
      set(not_available ${not_available} ${pkg})
    endif()
  endforeach()

  if(not_available)
    list(JOIN not_available " " missing)
    message("-- The following packages are disabled (no source):")
    message("   -- ${missing}")
    list(REMOVE_ITEM SWIPL_PACKAGE_LIST ${not_avaialble})
  endif()

  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
endfunction()

function(check_package_dependencies newvar)
  set(new)

  foreach(pkg ${ARGN})
    foreach(dep ${SWIPL_PKG_DEPS_${pkg}})
      has_package(${dep} has_dep)
      if(NOT has_dep)
        swipl_add_package(${dep} "Required by package ${pkg}")
	set(new ${new} ${dep})
      endif()
    endforeach()
  endforeach()

  set(${newvar} ${new} PARENT_SCOPE)
  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
endfunction()

################
# Assemble the package list

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

################
# Add dependent packages

set(new ON)
set(deps)
while(new)
  set(new OFF)
  check_package_dependencies(new ${SWIPL_PACKAGE_LIST})
  list(APPEND deps ${new})
endwhile()

list(REMOVE_DUPLICATES deps)
foreach(pkg ${deps})
  if(NOT EXISTS ${CMAKE_SOURCE_DIR}/packages/${pkg}/CMakeLists.txt)
    message(SEND_ERROR
	    "No source for required package ${pkg}. "
	    "Please run the command below from the top directory and retry"
	    "\n   \"git submodule update --init packages/${pkg}\"")
  endif()
endforeach()

