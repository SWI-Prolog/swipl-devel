include(QLF)

# Rebuild all library indexes. First build is   always  ok, but as we do
# not know all dependencies for each library index and these may change,
# we wish to re-run these on every build  run. We do this by setting the
# OUTPUT to a dummy file that is not really created.

function(library_index)

foreach(dir ${ARGN})
  string(REGEX REPLACE "/$" "" dir ${dir})
  string(REGEX REPLACE "/" "_" dirtarget ${dir})
  set(target library_index_${dirtarget})

  if(NOT TARGET ${target})
    add_swipl_target(
	${target}
	NOINSTALL
	QUIET
	OUTPUT ${SWIPL_BUILD_HOME}/${dir}/__INDEX.pl
	COMMAND "make_library_index('${SWIPL_BUILD_HOME}/${dir}')"
	COMMENT "Build home/${dir}/INDEX.pl")
    install(FILES ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/${dir})
    add_dependencies(library_index ${target})
  endif()
endforeach()

endfunction()
