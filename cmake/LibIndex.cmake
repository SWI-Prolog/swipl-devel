function(library_index)

foreach(dir ${ARGN})
  string(REGEX REPLACE "/" "_" dirtarget ${dir})
  set(target library_index_${dirtarget})

  if(NOT TARGET ${target})
    add_custom_command(
	OUTPUT  home/${dir}/INDEX.pl
	COMMAND ${PROG_SWIPL} -f none --no-packs --nopce -q
		-g "\"make_library_index('${SWIPL_BUILD_HOME}/${dir}')\"" -t halt
	DEPENDS ${SWIPL_BOOT_FILE} prolog_home
    )

    add_custom_target(${target} DEPENDS home/${dir}/INDEX.pl)

    add_dependencies(library_index ${target})
    install(FILES ${CMAKE_BINARY_DIR}/home/${dir}/INDEX.pl
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/${dir})
  endif()
endforeach()

endfunction()
