# Get the GIT version as Major.Minor.Patch[-<hash>[-DIRTY]]

function(git_version var)
  find_program(GIT git)
  if(GIT)
    execute_process(COMMAND ${GIT}
		    -C "${CMAKE_CURRENT_SOURCE_DIR}"
		    describe --match "V*"
		    RESULT_VARIABLE git_rc
		    OUTPUT_VARIABLE git_out
		    OUTPUT_STRIP_TRAILING_WHITESPACE
		    ERROR_QUIET)
    if(git_rc EQUAL 0)
      string(REGEX REPLACE "^V" "" gitv "${git_out}")

      execute_process(COMMAND ${GIT}
		      -C "${CMAKE_CURRENT_SOURCE_DIR}"
		      diff
		      OUTPUT_VARIABLE git_diff
		      OUTPUT_STRIP_TRAILING_WHITESPACE
		      ERROR_QUIET)
      if(NOT git_diff STREQUAL "")
        set(gitv "${gitv}-DIRTY")
      endif()
      set(${var} "${gitv}" PARENT_SCOPE)
    endif()
  endif()
endfunction()
