include(QLF)

function(library_index)

foreach(dir ${ARGN})
  string(REGEX REPLACE "/" "_" dirtarget ${dir})
  set(target library_index_${dirtarget})

  if(NOT TARGET ${target})
    add_swipl_target(
	${target}
	OUTPUT  home/${dir}/INDEX.pl
	COMMAND "make_library_index('${SWIPL_BUILD_HOME}/${dir}')"
	COMMENT "Build home/${dir}/INDEX.pl")
    add_dependencies(library_index ${target})
    install(FILES ${CMAKE_BINARY_DIR}/home/${dir}/INDEX.pl
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/${dir})
  endif()
endforeach()

endfunction()
