# compile_qlf spec ...

# run_installed_swipl(command
#		      [QUIET]
#		      [SCRIPT script ...]
#		      [PACKAGES pkg ...]
#		      [COMMENT comment])
#
# Run the compiled Prolog system with its home set to the installed system.
# This is used for post installation tasks

if(NOT SWIPL_PATH_SEP)
  set(SWIPL_PATH_SEP ":")
endif()

function(run_installed_swipl command)
  set(pforeign ${CMAKE_CURRENT_BINARY_DIR})
  set(plibrary ${CMAKE_CURRENT_SOURCE_DIR})
  set(rc -F none)

  set(options -f none -t halt --home=${SWIPL_INSTALL_PREFIX})
  cmake_parse_arguments(my "QUIET" "COMMENT;RC" "SCRIPT;PACKAGES" ${ARGN})

  if(my_QUIET)
    set(options ${options} -q)
  endif()

  if(my_COMMENT)
    install(CODE "message(\"${my_COMMENT}\")")
  endif()

  if(my_RC)
    set(rc -F ${my_RC})
  endif()

  foreach(s ${my_SCRIPT})
    set(options ${options} -s ${s})
  endforeach()

  foreach(pkg ${packages})
    get_filename_component(src ${CMAKE_SOURCE_DIR}/packages/${pkg} ABSOLUTE)
    get_filename_component(bin ${CMAKE_BINARY_DIR}/packages/${pkg} ABSOLUTE)
    set(plibrary "${plibrary}${SWIPL_PATH_SEP}${src}")
    set(pforeign "${pforeign}${SWIPL_PATH_SEP}${bin}")
  endforeach()

  set(SWIPL ${CMAKE_BINARY_DIR}/src/swipl${CMAKE_EXECUTABLE_SUFFIX})

  install(CODE "EXECUTE_PROCESS(COMMAND
                   ${SWIPL} ${options} ${rc} -f none
                   -p \"foreign=${pforeign}\" -p \"library=${plibrary}\"
		   -g \"${command}\")")
endfunction()

# qcompile(spec ...)
# Generate QLF files for the given libraries.

function(qcompile)
  cmake_parse_arguments(my "" "RC" "LIBRARIES" ${ARGN})
  if(NOT my_RC)
    set(my_RC none)
  endif()
  foreach(f ${my_LIBRARIES})
    run_installed_swipl("qcompile(library(${f}))"
			RC ${my_RC}
			COMMENT "-- QLF compiling library(${f})")
  endforeach()
endfunction()

