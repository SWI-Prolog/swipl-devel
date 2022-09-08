IF(NOT DEFINED LLROUND_OK)
  TRY_RUN(RUN_RESULT COMPILE_RESULT
	  "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
	  "${CMAKE_CURRENT_LIST_DIR}/TestLLRound.c"
	  COMPILE_OUTPUT_VARIABLE COMPILE_OUT
	  RUN_OUTPUT_VARIABLE RUN_OUT
	 )
  if(RUN_RESULT EQUAL 0)
    message("-- llround(nextoward(0.5),-10) == 0 (ok)")
    set(LLROUND_OK 1 CACHE INTERNAL "llround is ok")
  else()
    set(LLROUND_OK 0 CACHE INTERNAL "llround is broken")
    message("-- llround(nextoward(0.5),-10) == 1 (broken)")
  endif()
endif(NOT DEFINED LLROUND_OK)
