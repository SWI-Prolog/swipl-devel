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

function(add_symlink_command from to)
  get_filename_component(LNTDIR ${to} DIRECTORY)
  get_filename_component(LNTNAME ${to} NAME)
  file(RELATIVE_PATH LNLNK ${LNTDIR} ${from})
  add_custom_command(
      OUTPUT ${to}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${LNTDIR}
      COMMAND ${CMAKE_COMMAND} -E create_symlink ${LNLNK} ./${LNTNAME}
      WORKING_DIRECTORY ${LNTDIR})
endfunction()

function(install_in_home)
  cmake_parse_arguments(my "" "RENAME;DESTINATION" "FILES" ${ARGN})
  if(my_DESTINATION AND my_FILES)
    string(REPLACE
	   "${SWIPL_INSTALL_PREFIX}/"
	   "${SWIPL_BUILD_HOME}/" buildhome ${my_DESTINATION})

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

    string(SHA1 tname "$my_DESTINATION:${my_FILES}")
    add_custom_target(
	${tname} ALL
	DEPENDS ${deps})
    add_dependencies(prolog_home ${tname})
  endif()
endfunction()

function(install_src)
  install_in_home(${ARGN})
  install(${ARGN})
endfunction()
