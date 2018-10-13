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
pkg_option(TERM  "Terminal support (Unix only)")
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

set(SWIPL_PACKAGE_LIST_TERM
    libedit
    readline)

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

set(SWIPL_PKG_EXPLICIT)				# Explicitly requested packages
set(SWIPL_PKG_DEPENDENCY)			# Required packages

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

# add_package(name reason message ..)

function(swipl_add_package pkg reason)
  has_package(${pkg} has_pkg)
  if(NOT has_pkg)
    list(APPEND SWIPL_PACKAGE_LIST ${pkg})
    set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
    if(reason STREQUAL EXPLICIT)
      list(APPEND SWIPL_PKG_EXPLICIT ${pkg})
      set(SWIPL_PKG_EXPLICIT ${SWIPL_PKG_EXPLICIT} PARENT_SCOPE)
    endif()
    message("-- Added package ${pkg}: " ${ARGN})
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

function(join_list out sep)
  set(str)
  foreach(s ${ARGN})
    set(str "${str}${sep}${s}")
  endforeach()
  set(${out} ${str} PARENT_SCOPE)
endfunction()

function(remove_packages_without_source)
  set(not_available)
  foreach(pkg ${SWIPL_PACKAGE_LIST})
    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/packages/${pkg}/CMakeLists.txt)
      set(not_available ${not_available} ${pkg})
    endif()
  endforeach()

  if(not_available)
    join_list(missing " " ${not_available})
    message("-- The following packages are disabled because the required "
	    "sources are not installed: "
            ${missing}
	    "\nUse one of the commands below to add all packages or specific "
	    "packages"
	    "\n\n   git -C .. submodule update --init"
	    "  \n   git -C .. submodule update --init packages/<pkg>"
	    "\n")
    list(REMOVE_ITEM SWIPL_PACKAGE_LIST ${not_available})
  endif()

  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
endfunction()


function(check_package_dependencies newvar)
  set(new)

  foreach(pkg ${ARGN})
    foreach(dep ${SWIPL_PKG_DEPS_${pkg}})
      has_package(${dep} has_dep)
      if(NOT has_dep)
        swipl_add_package(${dep} DEPENDENCY "Required by package ${pkg}")
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
  if(UNIX)
    add_package_sets(TERM)
  endif()
  remove_packages_without_source()
endif()

if(NOT SWIPL_SHARED_LIB)
  swipl_del_package(jpl "not compatible with a static Prolog kernel")
endif()

if(INSTALL_DOCUMENTATION)
  if(SWIPL_PACKAGES)
    swipl_add_package(ltx2htm EXPLICIT
		      "required to build documentation.  Use "
		      "-DINSTALL_DOCUMENTATION=OFF to avoid this dependency")
  else()
    message("-- Cannot install documentation without packages")
  endif()
endif()

################
# Add dependent packages

set(new ON)
set(SWIPL_PKG_DEPENDENCY)
while(new)
  set(new OFF)
  check_package_dependencies(new ${SWIPL_PACKAGE_LIST})
  list(APPEND SWIPL_PKG_DEPENDENCY ${new})
endwhile()

if(SWIPL_PACKAGE_LIST)
  list(REMOVE_DUPLICATES SWIPL_PACKAGE_LIST)
  set(missing)
  foreach(pkg ${SWIPL_PACKAGE_LIST})
    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/packages/${pkg}/CMakeLists.txt)
      set(missing "${missing} packages/${pkg}")
    endif()
  endforeach()
  if(missing)
    message(FATAL_ERROR
	    "No source for some required packages. "
	    "Please run the command below from the top directory and retry"
	    "\n   git -C .. submodule update --init ${missing}"
	    "\n")
  endif()
endif()
