# Define variables for alignment of int64, void*, and double

function(alignof VAR_ALIGNOF_INT64_T VAR_ALIGNOF_VOIDP VAR_ALIGNOF_DOUBLE)
   if(NOT DEFINED ${VAR_ALIGNOF_DOUBLE})
      # Compile alignment checker source
      set(alignof_checker_target ${CMAKE_BINARY_DIR}/${CMAKE_FILES_DIRECTORY}/CheckAlignOf.bin)
      set(alignof_checker_source_dir ${CMAKE_CURRENT_LIST_DIR})
      try_compile(alignof_checker_ok
         ${CMAKE_BINARY_DIR}
         ${alignof_checker_source_dir}/CheckAlignment.c
         COPY_FILE ${alignof_checker_target})

      if(alignof_checker_ok)
         # Match 1,4, 8,16,32 or 64 alignments
         set(alignof_pat_regex "(1|4|8|16|32|64)")

         # Read patterns from compiled executable
         FILE(STRINGS ${alignof_checker_target} alignof_int64_pat
            REGEX "INT64_ALIGNMENT=${alignof_pat_regex}" LIMIT_COUNT 1)

         FILE(STRINGS ${alignof_checker_target} alignof_voidp_pat
            REGEX "VOIDP_ALIGNMENT=${alignof_pat_regex}" LIMIT_COUNT 1)

         FILE(STRINGS ${alignof_checker_target} alignof_double_pat
            REGEX "DOUBLE_ALIGNMENT=${alignof_pat_regex}" LIMIT_COUNT 1)

         # Extract alignments from patterns and assign them to the variables
         string(REGEX MATCH "${alignof_pat_regex}$" int64_alignment "${alignof_int64_pat}")
         string(REGEX MATCH "${alignof_pat_regex}$" double_alignment "${alignof_double_pat}")
         string(REGEX MATCH "${alignof_pat_regex}$" voidp_alignment "${alignof_voidp_pat}")

         # Cache the results
         set(${VAR_ALIGNOF_INT64_T} ${int64_alignment} CACHE STRING "Alignment of int64_t")
         set(${VAR_ALIGNOF_DOUBLE} ${double_alignment} CACHE STRING "Alignment of double")
         set(${VAR_ALIGNOF_VOIDP} ${voidp_alignment} CACHE STRING "Alignment of void*")

         # Report the results
         foreach(alignof_type INT64_T DOUBLE VOIDP)
            MESSAGE(STATUS "Check alignment of ${alignof_type}: ${${VAR_ALIGNOF_${alignof_type}}}")
         endforeach(alignof_type)
      else()
         MESSAGE(FATAL_ERROR "Check alignment: unable to compile test program.")
      endif(alignof_checker_ok)

      if(NOT DEFINED ${VAR_ALIGNOF_INT64_T})
         MESSAGE(FATAL_ERROR "Check alignment: unable to determine void*, double and int64_t alignment.")
      endif()
   endif(NOT DEFINED ${VAR_ALIGNOF_DOUBLE})
endfunction()
