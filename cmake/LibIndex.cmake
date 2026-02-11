include(QLF)

# Rebuild all library indexes. First build is   always  ok, but as we do
# not know all dependencies for each library index and these may change,
# we wish to re-run these on every build run. The OUTPUT is set to a dummy
# __INDEX.pl file (touched after build) while the real INDEX.pl is declared
# as a BYPRODUCT. This forces CMake to rebuild indexes while avoiding
# MSB8065 warnings on Visual Studio.

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
	BYPRODUCTS ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl
	COMMAND "make_library_index('${SWIPL_BUILD_HOME}/${dir}')"
	COMMENT "Build home/${dir}/INDEX.pl")
    install(FILES ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/${dir})
    add_dependencies(library_index ${target})
  endif()
endforeach()

endfunction()
