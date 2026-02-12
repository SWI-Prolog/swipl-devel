include(QLF)

# library_dirindex(dir dependency ...)
#
# Rebuild the library indexes. `src/CMakeLists.txt`   defines the target
# `library_index`. This function adds a  sub   dependency  for the given
# directory to build  the  index  based  on   the  *.pl  files  in  that
# directory. Packages call this through   swipl_plugin(), where a single
# package tends to  call  swipl_plugin()   multiple  times  for multiple
# plugins, each with a set of *.pl files.   We  create the target on the
# first call and extend in on subsequent calls.

function(library_dirindex dir)
  string(REGEX REPLACE "/" "_" dirtarget ${dir})
  set(target library_index_${dirtarget})

  prepend(deps ${SWIPL_BUILD_HOME}/${dir} ${ARGN})

  if(NOT TARGET ${target})
    add_swipl_target(
	  ${target}
	  NOINSTALL
	  QUIET
	  DEPENDS ${deps}
	  OUTPUT ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl
	  COMMAND "make_library_index('${SWIPL_BUILD_HOME}/${dir}')"
	  COMMENT "Build home/${dir}/INDEX.pl")
      install(FILES ${SWIPL_BUILD_HOME}/${dir}/INDEX.pl
	      DESTINATION ${SWIPL_INSTALL_PREFIX}/${dir})
      add_dependencies(library_index ${target})
  else()
    set_property(TARGET ${target} APPEND PROPERTY DEPENDS ${deps})
  endif()
endfunction()
