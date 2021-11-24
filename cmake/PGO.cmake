################
# PGO (profile guided optimization)

if(CMAKE_ARGC AND CMAKE_ARGV2 MATCHES "PGO.cmake")
  # Script mode, run as ``/path/to/cmake -P /path/to/PGO.cmake subcommand [args...]''
  if(CMAKE_ARGV3 STREQUAL write-timestamp-header)
    # cmake -P PGO.cmake write-timestamp-header PGO_HEADER_FILE [PGO_OUTPUT_FILE]
    set(PGO_HEADER_FILE "${CMAKE_ARGV4}")
    set(PGO_OUTPUT_FILE "${CMAKE_ARGV5}")
    if(PGO_OUTPUT_FILE)
      file(TIMESTAMP "${PGO_OUTPUT_FILE}" PGO_GEN_TIMESTAMP)
    endif()
    configure_file("${CMAKE_CURRENT_LIST_DIR}/pgo-timestamp.h.in" "${PGO_HEADER_FILE}" ESCAPE_QUOTES)
  else()
    message(FATAL_ERROR "PGO subcommand ${CMAKE_ARGV3} not recognized")
  endif()
  return()
endif()

set(PGO_SCRIPT ${CMAKE_CURRENT_LIST_FILE})

set(PGO_PROGRAM ${CMAKE_SOURCE_DIR}/bench/run.pl
    CACHE STRING
    "Program to use for Profile Guided Optimization")

set(PGO_SWIPL_OPTIONS -f none --no-packs --no-threads -O
    CACHE STRING
    "Prolog options for PGO run")

set(PGO_PROGRAM_OPTIONS --speedup=10
    CACHE STRING
    "Options to give to the benchmark script")

set(PGO_DIR ${CMAKE_BINARY_DIR}/PGO-data
    CACHE PATH
    "Directory to store PGO data in")

function(prepare_pgo_target t is_generate)
  # This function is a no-op by default, but may be overridden
  # by compiler-specific code in configure_pgo()
endfunction()

macro(configure_pgo pgo_tag)
  set(PGO_TAG ${pgo_tag})
  if(PGO_TAG)
    message(STATUS "PGO: configuring instrumented build")
    set(PGO_SUFFIX "-${PGO_TAG}")
    # Re-run the current CMakeLists to pull in all the same target definitions
    # The variable PGO_SUFFIX will be set, append all targets with it
    include(${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt)

    set(PGO_RUN_STAMP ${PGO_DIR}/pgo-run.stamp)
    add_custom_command(OUTPUT ${PGO_RUN_STAMP}
          COMMAND ${CMAKE_COMMAND} -E remove_directory "${PGO_DIR}"
          COMMAND ${CMAKE_COMMAND} -E make_directory "${PGO_DIR}"
          COMMENT "Collecting profile data..."
          USES_TERMINAL
          VERBATIM)
  endif()

  # Set compiler-specific PGO parameters
  if(CMAKE_C_COMPILER_ID MATCHES "Clang")
    set(PGO_OUTPUT_FILE     ${PGO_DIR}/swipl.profdata)
    set(PGO_GENERATE_FLAGS -fprofile-generate=${PGO_DIR})
    set(PGO_USE_FLAGS      -fprofile-use=${PGO_OUTPUT_FILE})
    get_filename_component(CMAKE_C_COMPILER_DIR ${CMAKE_C_COMPILER} DIRECTORY)
    set(PROFDATA_NAMES llvm-profdata)
    get_filename_component(CMAKE_C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME_WE)
    if(CMAKE_C_COMPILER_NAME MATCHES "-[0-9]+$")
      string(REGEX MATCH "-[0-9]+$" LLVM_VERSION_SUFFIX "${CMAKE_C_COMPILER_NAME}")
      list(APPEND PROFDATA_NAMES "llvm-profdata${LLVM_VERSION_SUFFIX}")
    endif()
    find_program(LLVM_PROFDATA NAMES ${PROFDATA_NAMES}
             HINTS ${CMAKE_C_COMPILER_DIR})
    add_custom_command(OUTPUT ${PGO_OUTPUT_FILE}
          DEPENDS ${PGO_RUN_STAMP}
          COMMAND ${LLVM_PROFDATA} merge -output=${PGO_OUTPUT_FILE} ${PGO_DIR}/*.profraw)
  else()
    set(PGO_CFLAGS_EXTRA   -Wno-maybe-uninitialized "-fprofile-dir=${PGO_DIR}")
    set(PGO_GENERATE_FLAGS -fprofile-generate ${PGO_CFLAGS_EXTRA})
    set(PGO_USE_FLAGS      -fprofile-use ${PGO_CFLAGS_EXTRA})
    if(PGO_TAG)
      set(PGO_OUTPUT_FILE ${PGO_RUN_STAMP})
      function(prepare_pgo_target t is_generate)
        # GCC identifies static functions by keying them on the path that the
        # *object* file is compiled to, rather than the path of the source file.
        # The wrapper here allows Ninja to pass the -o location in the instrumentation
        # directory, as it expects (and requires, to support building the same library
        # twice from the same build file), but the wrapper translates it to a -o
        # location in the NON-instrumented directory and, assuming the compilation
        # is successful, moves the object file to its final resting place afterwards.
        set(PGO_COMPILER_LAUNCHER ${PROG_WRAPGCC} ${PGO_SUFFIX})
        if(is_generate)
          set_target_properties(${t} PROPERTIES C_COMPILER_LAUNCHER "${PGO_COMPILER_LAUNCHER}")
          add_dependencies(${t} ${TARGET_WRAPGCC})
        endif()
      endfunction()
    endif(PGO_TAG)
  endif()

  set(PGO_HEADER_FILE ${CMAKE_CURRENT_BINARY_DIR}/pgo-timestamp.h)
  add_custom_command(OUTPUT ${PGO_HEADER_FILE}
          DEPENDS ${PGO_OUTPUT_FILE}
          COMMAND ${CMAKE_COMMAND} -P ${PGO_SCRIPT} write-timestamp-header ${PGO_HEADER_FILE} ${PGO_OUTPUT_FILE}
          VERBATIM)
  set(PGO_USE_FLAGS ${PGO_USE_FLAGS} -include "${PGO_HEADER_FILE}")
  add_custom_target(pgo_data DEPENDS ${PGO_HEADER_FILE})

endmacro()

function(generate_pgo_data) # generate_pgo_data(targets...)
  string(REPLACE ";" " " gen_flags "${PGO_GENERATE_FLAGS}")
  foreach(t IN LISTS ARGV)
    set(tinstr "${t}${PGO_SUFFIX}")
    target_compile_options(${tinstr} PRIVATE ${PGO_GENERATE_FLAGS})
    if(${CMAKE_VERSION} VERSION_GREATER 3.12)
      target_link_options(${tinstr} PRIVATE ${PGO_GENERATE_FLAGS})
    else()
      set_target_properties(${tinstr} PROPERTIES LINK_FLAGS "${gen_flags}")
    endif()
    prepare_pgo_target(${tinstr} true)
  endforeach()
endfunction()

function(add_pgo_dependency) # add_pgo_dependency(targets...)
  add_dependencies(pgo_data ${ARGN})
endfunction()

function(run_pgo_program exec_target) # run_pgo_program(exec_target [args...])
  if(CMAKE_CROSSCOMPILING AND CMAKE_CROSSCOMPILING_EMULATOR)
    set(executable ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${exec_target}${PGO_SUFFIX}>)
  else()
    set(executable "${exec_target}${PGO_SUFFIX}")
  endif()
  add_custom_command(OUTPUT ${PGO_RUN_STAMP} APPEND
        DEPENDS "${exec_target}${PGO_SUFFIX}"
        COMMAND ${executable} ${ARGN}
        VERBATIM)
  # if we rebuild the binary, we have to clear the pgo data
  add_custom_command(TARGET "${exec_target}${PGO_SUFFIX}"
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E remove_directory ${PGO_DIR}
        COMMENT "Clearing PGO data..."
        VERBATIM)
endfunction()

function(use_pgo_data) # use_pgo_data(targets...)
  if(PGO_RUN_STAMP)
    add_custom_command(OUTPUT ${PGO_RUN_STAMP} APPEND
          COMMAND ${CMAKE_COMMAND} -E touch ${PGO_RUN_STAMP}
          VERBATIM)
  endif()
  string(REPLACE ";" " " use_flags "${PGO_USE_FLAGS}")
  foreach(t IN LISTS ARGV)
    target_compile_options(${t} PRIVATE ${PGO_USE_FLAGS})
    if(${CMAKE_VERSION} VERSION_GREATER 3.12)
      target_link_options(${t} PRIVATE ${PGO_USE_FLAGS})
    else()
      set_target_properties(${t} PROPERTIES LINK_FLAGS "${use_flags}")
    endif()
    add_dependencies(${t} pgo_data)
    prepare_pgo_target(${t} false)
  endforeach()
endfunction()
