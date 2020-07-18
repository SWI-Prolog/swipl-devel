include(QLF)

function(library_index)

foreach(dir ${ARGN})
  string(REGEX REPLACE "/" "_" dirtarget ${dir})
  set(target library_index_${dirtarget})

  if(NOT TARGET ${target})
    add_custom_target(
	${target}_always ALL
	DEPENDS ${SWIPL_BUILD_HOME}/${dir}/__INDEX.pl)

    add_swipl_target(
	${target}
	OUTPUT  ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl
		${SWIPL_BUILD_HOME}/${dir}/__INDEX.pl
	QUIET
	COMMAND "make_library_index('${SWIPL_BUILD_HOME}/${dir}')"
	COMMENT "Build home/${dir}/INDEX.pl")
    add_dependencies(library_index ${target})
  endif()
endforeach()

endfunction()
