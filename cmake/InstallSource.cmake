# Installation of SWI-Prolog data files that are literally
# copied from the sources to their installation location.
#
# We want to use this to create a shadow data tree in the
# CMAKE_BINARY_DIR such that we can run the full system
# without installing it

# ${SWIPL_BUILD_HOME} holds the direcory where we link the Prolog
# resource files.
set(SWIPL_BUILD_HOME    ${CMAKE_CURRENT_BINARY_DIR}/home)
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
      if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.20")
	cmake_path(SET dep NORMALIZE ${buildhome}/${base})
      else()
	set(dep ${buildhome}/${base})
	string(REGEX REPLACE "//*" "/" dep ${dep})
      endif()
      set(deps ${deps} ${dep})
    endforeach()

    add_custom_target(
	${name} ALL
	DEPENDS ${deps})
    add_dependencies(prolog_home ${name})

    install_qlfs(${my_DESTINATION} ${deps})
  endif()
endfunction()

# wrapper around install(). Processes  RENAME,   DESTINATION  and FILES.
# This populates ${CMAKE_BINARY_DIR}/home with symlinks  and calls CMake
# install()  to  install   the   result.    If   INSTALL_QLF   is   set,
# install_in_home() calls install_qlfs(DESTINATION, file ...)
#
# Installing   .qlf   files    must    be     kept    in    sync    with
# library(prolog_qlfmake).

function(install_src name)
  install_in_home(${name} ${ARGN})
  install_prolog_src(${ARGN})
endfunction()

set(index_patteern
    ".*/INDEX[.]pl"
    ".*/MKINDEX[.]pl"
    ".*/CLASSINDEX[.]pl")

set(noqlf_pattern
    ${index_patteern}
    "home/boot/.*[.]pl"
    "home/doc/.*"
    "home/demo/.*"
    "home/xpce/prolog/boot/.*[.]pl"
    "home/xpce/prolog/lib/compatibility/.*[.]pl"
    "home/library/ext/ltx2htm/sty_.*[.]pl")

set(noqlf_file
    home/library/tabling.pl
    home/library/prolog_qlfmake.pl
    home/library/check_installation.pl
    home/library/threadutil.pl
    home/library/theme/dark.pl
    home/library/ext/pldoc/pldoc/hooks.pl
    home/xpce/prolog/lib/swi_compatibility.pl
    home/xpce/prolog/lib/english/pce_messages.pl
    home/xpce/prolog/lib/trace/gui.pl
    home/xpce/prolog/lib/trace/util.pl
    home/xpce/prolog/lib/trace/source.pl
    home/xpce/prolog/lib/trace/settings.pl
    home/xpce/prolog/lib/trace/viewterm.pl
    home/xpce/prolog/lib/trace/clause.pl
    home/xpce/prolog/lib/trace/stack.pl
    home/xpce/prolog/lib/trace/pprint.pl
    home/xpce/prolog/lib/xref/quintus.pl
    home/xpce/prolog/lib/xref/sicstus.pl
    home/xpce/prolog/lib/emacs/language_mode.pl
    home/xpce/prolog/lib/emacs/outline_mode.pl
    home/xpce/prolog/lib/emacs/server.pl
    home/xpce/prolog/lib/emacs/help_buffer.pl
    home/xpce/prolog/lib/emacs/window.pl
    home/xpce/prolog/lib/emacs/bookmarks.pl
    home/xpce/prolog/lib/emacs/buffer.pl
    home/xpce/prolog/lib/emacs/buffer_menu.pl
    home/xpce/prolog/lib/emacs/fundamental_mode.pl
    home/xpce/prolog/lib/emacs/application.pl
    home/xpce/prolog/lib/emacs/history.pl
    home/library/ext/http/http/dcg_basics.pl
    home/library/ext/json/http/json.pl
    home/library/ext/json/http/json_convert.pl
    home/library/ext/json/http/js_grammar.pl)

set(include_src_pattern
    "library/tabling[.]pl$"
    "library/prolog_qlfmake[.]pl$"
    "theme/[^/]*[.]pl$"
    "ltx2htm/sty_[^/]*[.]pl$"
    "hooks[.]pl$"			# Should only match pldoc/hooks.pl
    "xref/quintus[.]pl$"
    "xref/sicstus[.]pl$"
    "http/dcg_basics[.]pl$"
    "trace/pprint[.]pl$"
    "lib/swi_compatibility[.]pl")

set(WASM_prolog_src
    tabling.pl
    theme/dark.pl)

function(install_prolog_src)
  if(INSTALL_PROLOG_SRC)
    install(${ARGN})
  else()
    set(argv)
    foreach(plfile ${ARGN})
      set(skip OFF)
      get_filename_component(base ${plfile} NAME)
      if ( base MATCHES "[.]pl$" AND NOT base STREQUAL "INDEX.pl" )
        set(skip ON)
        foreach(pat ${include_src_pattern})
	  if(skip AND plfile MATCHES ${pat})
	    set(skip OFF)
	  endif()
	endforeach()
      endif()
      if ( NOT skip )
        list(APPEND argv ${plfile})
      endif()
    endforeach()
    install(${argv})
  endif()
endfunction()

function(install_qlfs destination)
  if(INSTALL_QLF)
    string(LENGTH ${CMAKE_BINARY_DIR}/ binlen)
    set(qlf)
    foreach(plfile ${ARGN})
      if ( plfile MATCHES "[.]pl$" )
	string(SUBSTRING ${plfile} ${binlen} -1 plfile)
	set(skip OFF)
	if(${plfile} IN_LIST noqlf_file)
	  set(skip ON)
	endif()
	if(NOT skip)
	  foreach(pat ${noqlf_pattern})
	    if(NOT skip AND plfile MATCHES ${pat})
	      set(skip ON)
	    endif()
	  endforeach()
	endif()
	if(NOT skip)
	  string(REGEX REPLACE "[.]pl$" ".qlf" qlffile ${plfile})
	  list(APPEND qlf ${CMAKE_BINARY_DIR}/${qlffile})
	endif()
      endif()
    endforeach()
    if ( qlf )
      #message("QLF: install FILES ${qlf} DESTINATION ${destination}")
      install(FILES ${qlf} DESTINATION ${destination})
    endif()
  endif()
endfunction()
