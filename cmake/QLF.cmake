# compile_qlf spec ...

# add_swipl_target(name
#		   OUTPUT output
#                  COMMAND command
#	           [OPTIONS ...]
#	           [SCRIPT ...]
#	           [QUIET]
#	           [COMMENT comment]
#		   [DEPENDS ...])
#
# Add a custom target that runs Prolog   to create output in the locally
# created Prolog home ${SWIPL_BUILD_HOME}. The  created product is added
# to the installation target at the same location.
#
# Generate QLF files for the given libraries.  This is used to compile a
# couple of large (aggregate) .qlf files.   If  INSTALL_QLF is used, all
# sources are compiled to .qlf, so we do not need this.
#
# Variables:
#   - ${SWIPL_COMMAND_DEPENDS} is added to the dependencies

function(add_swipl_target name)
  set(options --no-packs "--home=${SWIPL_BUILD_HOME}" -DSDL_VIDEODRIVER=dummy)
  cmake_parse_arguments(
      my "QUIET;QLF;NOINSTALL"
         "COMMENT;COMMAND;APP;WORKING_DIRECTORY"
         "OUTPUT;BYPRODUCTS;SCRIPT;DEPENDS;OPTIONS;LIBS"
         ${ARGN})

  if(my_QUIET)
    set(options ${options} -q)
  endif()

  if(NOT my_COMMENT)
    if(my_COMMENT)
      set(my_COMMENT "-- swipl -g ${my_COMMAND}")
    else()
      set(my_COMMENT "-- swipl ${my_APP}")
    endif()
  endif()

  if(my_QLF)
    set(PROG_SWIPL ${PROG_SWIPL_FOR_BOOT})
  endif()

  if(my_COMMAND)
    set(options ${options} -f none -t halt)
    foreach(s ${my_LIBS})
      set(options ${options} -g "use_module(library(${s}))")
    endforeach()
    foreach(s ${my_SCRIPT})
      set(options ${options} -l ${s})
    endforeach()
    set(options ${options} -g ${my_COMMAND} --)
  elseif(my_APP)
    set(options ${options} ${my_APP})
  else()
    message(FATAL_ERROR "add_swipl_target() requires COMMAND or APP")
  endif()

  add_custom_command(
      OUTPUT ${my_OUTPUT}
      BYPRODUCTS ${my_BYPRODUCTS}
      COMMAND ${CMAKE_COMMAND} -E env --unset=DISPLAY SDL_VIDEODRIVER=dummy
	      ${PROG_SWIPL} ${options} ${my_OPTIONS}
      COMMENT "${my_COMMENT}"
      WORKING_DIRECTORY "${my_WORKING_DIRECTORY}"
      DEPENDS core prolog_home
              ${SWIPL_COMMAND_DEPENDS} "${my_DEPENDS}"
      VERBATIM)
  add_custom_target(
      ${name} ALL
      DEPENDS ${my_OUTPUT})

  if(NOT my_NOINSTALL)
    list(GET my_OUTPUT 0 primary)
    string(REPLACE "${SWIPL_BUILD_HOME}" "" rel "${primary}")
    string(REGEX REPLACE "^/*([^/].*)" "\\1" rel "${rel}")
    get_filename_component(rel ${rel} DIRECTORY)
    install(FILES ${primary}
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/${rel})
  endif()
endfunction()

# add_qcompile_target(
#     target
#     [SOURCES ...]
#     [DEPENDS ...]
#     [PRELOAD ...]
#
# Preloading a library is written as e.g. PRELOAD lib:pldoc to load
# library(pldoc).

function(add_qcompile_target target)
  if(NOT INSTALL_QLF)
    cmake_parse_arguments(my "" "" "SOURCES;DEPENDS;EXPECTDEPS;PRELOAD" ${ARGN})

    prepend(src ${SWIPL_QLF_BASE}/ ${my_SOURCES})
    set(src ${src} ${SWIPL_QLF_BASE}/${target}.pl)
    string(REPLACE "/" "-" tname "${target}")

    if(my_PRELOAD)
      set(extra --preload ${my_PRELOAD})
    else()
      set(extra)
    endif()

    set(expectdeps ${src} ${my_EXPECTDEPS})

    add_swipl_target(
	qlf-${tname}
	OUTPUT ${SWIPL_QLF_BASE}/${target}.qlf
	APP qlf
	OPTIONS compile ${SWIPL_QLF_BASE}/${target}
	        --expect-deps ${expectdeps} ${extra}
	COMMENT "QLF compiling ${target}.qlf"
	DEPENDS ${src} ${my_DEPENDS})
  endif()
endfunction()

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

  set(options -f none --no-packs -t halt --home=${SWIPL_INSTALL_PREFIX})
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
                   ${SWIPL} ${options} ${rc} -f none --no-packs
                   -p \"foreign=${pforeign}\" -p \"library=${plibrary}\"
		   -g \"${command}\")")
endfunction()

# qcompile(spec ...)
#
# Generate QLF files for the given libraries.  This is used to compile a
# couple of large (aggregate) .qlf files.   If  INSTALL_QLF is used, all
# sources are compiled to .qlf, so we do not need this.

function(qcompile)
  if(NOT INSTALL_QLF)
    cmake_parse_arguments(my "" "RC" "LIBRARIES" ${ARGN})
    if(NOT my_RC)
      set(my_RC none)
    endif()
    foreach(f ${my_LIBRARIES})
      run_installed_swipl("qcompile(library(${f}))"
			  RC ${my_RC}
			  COMMENT "-- QLF compiling library(${f})")
    endforeach()
  endif()
endfunction()
