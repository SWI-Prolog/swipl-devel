include(CMakeDependentOption)

option(SWIPL_PACKAGES
       "Build and install packages"
       ON)

macro(pkg_option name comment)
  cmake_dependent_option(
      SWIPL_PACKAGES_${name} ${comment} ON
      SWIPL_PACKAGES OFF)
endmacro()

# FIXME: Can we get spaces in component names?
# Do not change the titles as these are the COMPONENT names
# and used by modular install scripts
set(SWIPL_PACKAGE_LIST_BASIC_title   "Core_packages")
set(SWIPL_PACKAGE_LIST_ARCHIVE_title "Archive_interface")
set(SWIPL_PACKAGE_LIST_TERM_title    "Commandline_editors")
set(SWIPL_PACKAGE_LIST_ODBC_title    "ODBC_interface")
set(SWIPL_PACKAGE_LIST_BDB_title     "BerkeleyDB_interface")
set(SWIPL_PACKAGE_LIST_PCRE_title    "Perl_regex")
set(SWIPL_PACKAGE_LIST_YAML_title    "YAML_support")
set(SWIPL_PACKAGE_LIST_JAVA_title    "Java_interface")
set(SWIPL_PACKAGE_LIST_SSL_title     "OpenSSL_interface")
set(SWIPL_PACKAGE_LIST_TIPC_title    "TIPC_networking")
set(SWIPL_PACKAGE_LIST_QT_title	     "Qt_console")
set(SWIPL_PACKAGE_LIST_X_title	     "Graphics_subsystem")

set(SWIPL_PACKAGE_SETS
    BASIC ARCHIVE ODBC BDB PCRE YAML JAVA SSL TIPC QT X)
if(UNIX)
  list(APPEND SWIPL_PACKAGE_SETS TERM)
endif()

foreach(pkgset ${SWIPL_PACKAGE_SETS})
  pkg_option(${pkgset} ${SWIPL_PACKAGE_LIST_${pkgset}_title})
endforeach()

# The pckages below do not depend on external libraries except for
# the zlib package, but the core system already depends on zlib.
set(SWIPL_PACKAGE_LIST_BASIC
    chr
    clib
    clpqr
    inclpr
    cpp
    http
    mqi
    ltx2htm
    nlp
    paxos
    redis
    stomp
    PDT
    pengines
    pldoc
    plunit
    protobufs
    RDF
    semweb
    sgml
    table
    utf8proc
    zlib)

if(WIN32)
  list(APPEND SWIPL_PACKAGE_LIST_BASIC windows)
endif()

# Each of the package sets below depend on additional external libraries
# and are typically distributed in separate .deb or .rpm files.

set(SWIPL_PACKAGE_LIST_ARCHIVE
    archive)

set(SWIPL_PACKAGE_LIST_TERM
    libedit
    readline)

set(SWIPL_PACKAGE_LIST_ODBC
    odbc
    cql)

set(SWIPL_PACKAGE_LIST_BDB
    bdb)

set(SWIPL_PACKAGE_LIST_PCRE
    pcre)

set(SWIPL_PACKAGE_LIST_YAML
    yaml)

set(SWIPL_PACKAGE_LIST_SSL
    ssl)

set(SWIPL_PACKAGE_LIST_JAVA
    jpl)

set(SWIPL_PACKAGE_LIST_TIPC
    tipc)

set(SWIPL_PACKAGE_LIST_QT
    swipl-win)

set(SWIPL_PACKAGE_LIST_X
    xpce)

# swipl_package_component(pkg var)
#
# Set ${var} to the package group to which ${pkg} belongs

function(swipl_package_component pkg outvar)
  foreach(s ${SWIPL_PACKAGE_SETS})
    list(FIND SWIPL_PACKAGE_LIST_${s} ${pkg} index)
    if(index GREATER -1)
      set(${outvar} ${SWIPL_PACKAGE_LIST_${s}_title} PARENT_SCOPE)
      break()
    endif()
  endforeach()
endfunction()


# Package dependencies as computed using script/xref_packages.pl
# FIXME: Some dependencies should be avoided, for example:
#   - term//2 should move from `pengines` to `http`

set(SWIPL_PKG_DEPS_RDF clib semweb sgml)
set(SWIPL_PKG_DEPS_archive clib)
set(SWIPL_PKG_DEPS_clib sgml)
set(SWIPL_PKG_DEPS_http clib sgml ssl)
set(SWIPL_PKG_DEPS_ltx2htm clib)
set(SWIPL_PKG_DEPS_pengines clib http)
set(SWIPL_PKG_DEPS_stomp clib http)
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

# swipl_add_packages([EXPLICIT]
#		     [DEPENDENCY]
#		     PACKAGES ...
#		     [COMMENT ...])

function(swipl_add_packages)
  cmake_parse_arguments(my "EXPLICIT;DEPENDENCY" "" "COMMENT;PACKAGES" ${ARGN})
  set(pkgs)

  foreach(pkg ${my_PACKAGES})
    has_package(${pkg} has_pkg)
    if(NOT has_pkg)
      set(pkgs "${pkgs} ${pkg}")
      list(APPEND SWIPL_PACKAGE_LIST ${pkg})
      set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
      if(my_EXPLICIT)
	list(APPEND SWIPL_PKG_EXPLICIT ${pkg})
	set(SWIPL_PKG_EXPLICIT ${SWIPL_PKG_EXPLICIT} PARENT_SCOPE)
      endif()
    endif()
  endforeach()

  if(pkgs)
    message("-- Added packages ${pkgs}: " ${my_COMMENT})
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

function(remove_packages_without_source)
  set(not_available)
  foreach(pkg ${SWIPL_PACKAGE_LIST})
    if(NOT EXISTS ${CMAKE_SOURCE_DIR}/packages/${pkg}/CMakeLists.txt)
      set(not_available ${not_available} ${pkg})
    endif()
  endforeach()

  if(not_available)
    list(REMOVE_ITEM SWIPL_PACKAGE_LIST ${not_available})

    if(NOT PKG_UNAVAILABLE_MESSAGE_DONE)
      join_list(missing " " ${not_available})
      message("-- The following SWI-Prolog packages are disabled because "
	      "the required sources are not installed:\n"
	      ${missing}
	      "\nUse one of the commands below to add all packages or specific "
	      "packages"
	      "\n\n   git -C .. submodule update --init"
	      "  \n   git -C .. submodule update --init packages/<pkg>"
	      "\n")
    endif()
    set(PKG_UNAVAILABLE_MESSAGE_DONE ON CACHE INTERNAL
	"Message about unavailable packages was printed")
  endif()

  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
endfunction()


function(check_package_dependencies newvar)
  set(new)

  foreach(pkg ${ARGN})
    set(deps)
    foreach(dep ${SWIPL_PKG_DEPS_${pkg}})
      has_package(${dep} has_dep)
      if(NOT has_dep)
        set(deps ${deps} ${dep})
	set(new ${new} ${dep})
      endif()
    endforeach()
    if(deps)
      swipl_add_packages(PACKAGES ${deps} DEPENDENCY
			 COMMENT "Required by package ${pkg}")
    endif()
  endforeach()

  set(${newvar} ${new} PARENT_SCOPE)
  set(SWIPL_PACKAGE_LIST ${SWIPL_PACKAGE_LIST} PARENT_SCOPE)
endfunction()

################
# Assemble the package list

set(SWIPL_PACKAGE_LIST)
if(SWIPL_PACKAGES)
  add_package_sets(${SWIPL_PACKAGE_SETS})
  remove_packages_without_source()
endif()

if(NOT SWIPL_SHARED_LIB)
  swipl_del_package(jpl "not compatible with a static Prolog kernel")
endif()
if(NOT MULTI_THREADED)
  swipl_del_package(swipl-win "requires multi-threading")
  swipl_del_package(tipc      "requires multi-threading")
  swipl_del_package(stomp     "requires multi-threading")
  swipl_del_package(mqi       "requires multi-threading")
endif()

if(INSTALL_DOCUMENTATION)
  if(SWIPL_PACKAGES)
    swipl_add_packages(EXPLICIT
		       PACKAGES ltx2htm pldoc nlp
		       COMMENT "required for online documentation.  Use "
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
