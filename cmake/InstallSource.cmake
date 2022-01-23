# Installation of SWI-Prolog data files that are literally
# copied from the sources to their installation location.
#
# We want to use this to create a shadow data tree in the
# CMAKE_BINARY_DIRECTORY such that we can run the full system
# without installing it

# ${SWIPL_BUILD_HOME} holds the direcory where we link the Prolog
# resource files.
set(SWIPL_BUILD_HOME    ${CMAKE_BINARY_DIR}/home)
set(SWIPL_BUILD_LIBRARY ${SWIPL_BUILD_HOME}/library)

add_custom_target(prolog_home)

function(symlink from to)
  get_filename_component(LNTDIR ${to} DIRECTORY)
  get_filename_component(LNTNAME ${to} NAME)
  file(RELATIVE_PATH LNLNK ${LNTDIR} ${from})
  if(NOT EXISTS ${LNTDIR})
    execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${LNTDIR})
  endif()
  execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
		  ${LNLNK} ./${LNTNAME}
		  WORKING_DIRECTORY ${LNTDIR})
endfunction()

# Build on MSYS2 does not like symlink
function(symlink_or_copy from to)
  get_filename_component(LNTDIR ${to} DIRECTORY)
  get_filename_component(LNTNAME ${to} NAME)
  file(RELATIVE_PATH LNLNK ${LNTDIR} ${from})
  if(NOT EXISTS ${LNTDIR})
    execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${LNTDIR})
  endif()
  if(CMAKE_HOST_UNIX)
    execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${LNLNK} ./${LNTNAME}
            WORKING_DIRECTORY ${LNTDIR})
  else()
    execute_process(COMMAND ${CMAKE_COMMAND} -E copy
            ${LNLNK} ./${LNTNAME}
            WORKING_DIRECTORY ${LNTDIR})
  endif()
endfunction()

# create_directory(dir)
#
# Creates a custom target to create   a directory. Multiple projects may
# try to generate the same target. Using make, each such target will end
# up in its own directory Makefile while using ninja all are in one file
# and we must avoid duplication of these rules. Note that multiple rules
# racing to create a directory are fine.

if(CMAKE_HOST_UNIX)
  set(TOUCH_EPOCH touch -t 200001010000)
else()
  set(TOUCH_EPOCH ${CMAKE_COMMAND} -E touch)
endif()

function(create_directory dir)
  if(CMAKE_GENERATOR MATCHES Ninja)
    set(done)
    get_property(done GLOBAL PROPERTY CREATE_DIRECTORY_STATE)
    list(FIND done ${dir} index)
    if(index LESS 0)
      add_custom_command(
	  OUTPUT ${LNTDIR}/.created
	  COMMAND ${CMAKE_COMMAND} -E make_directory ${dir}
	  COMMAND ${TOUCH_EPOCH} ${dir}/.created
	  VERBATIM)
      list(APPEND done ${dir})
      set_property(GLOBAL PROPERTY CREATE_DIRECTORY_STATE "${done}")
    endif()
  else()
    add_custom_command(
	OUTPUT ${LNTDIR}/.created
	COMMAND ${CMAKE_COMMAND} -E make_directory ${dir}
	COMMAND ${TOUCH_EPOCH} ${dir}/.created
	VERBATIM)
  endif()
endfunction()

function(add_symlink_command from to)
  get_filename_component(LNTDIR ${to} DIRECTORY)
  get_filename_component(LNTNAME ${to} NAME)
  file(RELATIVE_PATH LNLNK ${LNTDIR} ${from})
  create_directory(${LNTDIR})
  if(CMAKE_HOST_UNIX)
    add_custom_command(
	OUTPUT ${to}
	COMMAND ${CMAKE_COMMAND} -E create_symlink ${LNLNK} ./${LNTNAME}
	WORKING_DIRECTORY ${LNTDIR}
	DEPENDS ${LNTDIR}/.created
	VERBATIM)
  else()
    add_custom_command(
	OUTPUT ${to}
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LNLNK} ./${LNTNAME}
	WORKING_DIRECTORY ${LNTDIR}
	DEPENDS ${LNTDIR}/.created ${from}
	VERBATIM)
  endif()
endfunction()

# install_in_home(name ...)
#
# Install   the   targets   in   the     local   home.   This   replaces
# SWIPL_INSTALL_PREFIX or SWIPL_INSTALL_SHARE_PREFIX by `home`

function(install_in_home name)
  cmake_parse_arguments(my "" "RENAME;DESTINATION" "FILES" ${ARGN})
  if(my_DESTINATION AND my_FILES)
    string(REPLACE "." "\\." pattern ${SWIPL_INSTALL_PREFIX})
    string(REGEX REPLACE
	   "^${pattern}"
	   "${SWIPL_BUILD_HOME}" buildhome ${my_DESTINATION})

    if(buildhome STREQUAL my_DESTINATION AND
       NOT SWIPL_INSTALL_PREFIX STREQUAL SWIPL_INSTALL_SHARE_PREFIX)
      string(REPLACE "." "\\." pattern ${SWIPL_INSTALL_SHARE_PREFIX})
      string(REGEX REPLACE
	     "^${pattern}"
	     "${SWIPL_BUILD_HOME}" buildhome ${my_DESTINATION})
    endif()

    set(deps)

    foreach(file ${my_FILES})
      if(NOT IS_ABSOLUTE ${file})
        set(file ${CMAKE_CURRENT_SOURCE_DIR}/${file})
      endif()
      if(my_RENAME)
        set(base ${my_RENAME})
      else()
        get_filename_component(base ${file} NAME)
      endif()
      if(NOT EXISTS ${file})
        message(FATAL_ERROR
		"Cannot link from build home: ${file} does not exist")
      endif()
      add_symlink_command(${file} ${buildhome}/${base})
      set(deps ${deps} ${buildhome}/${base})
    endforeach()

    add_custom_target(
	${name} ALL
	DEPENDS ${deps})
    add_dependencies(prolog_home ${name})
  endif()
endfunction()

function(install_src name)
  install_in_home(${name} ${ARGN})
  install(${ARGN})
endfunction()
