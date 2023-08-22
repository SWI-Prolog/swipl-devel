include(QLF)

function(library_index)

foreach(dir ${ARGN})
  string(REGEX REPLACE "/" "_" dirtarget ${dir})
  set(target library_index_${dirtarget})

  if(NOT TARGET ${target})
    add_custom_target(
	${target}_always ALL
	DEPENDS ${SWIPL_BUILD_HOME}/${dir}/__INDEX.pl)

    if(CMAKE_GENERATOR STREQUAL "Unix Makefiles")
      set(out ${SWIPL_BUILD_HOME}/${dir}/__INDEX.pl)
      set(by)
    else()
      set(by ${SWIPL_BUILD_HOME}/${dir}/__INDEX.pl)
      set(out)
    endif()

    add_swipl_target(
	${target}
	OUTPUT  ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl ${out}
	BYPRODUCTS ${by}
	QUIET
	COMMAND "make_library_index('${SWIPL_BUILD_HOME}/${dir}')"
	COMMENT "Build home/${dir}/INDEX.pl")
    add_dependencies(library_index ${target})
  endif()
endforeach()

endfunction()
