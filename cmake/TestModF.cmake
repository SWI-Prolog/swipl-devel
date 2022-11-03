IF(NOT DEFINED MODF_OK)
  TRY_RUN(RUN_RESULT COMPILE_RESULT
	  "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
	  "${CMAKE_CURRENT_LIST_DIR}/TestModF.c"
	  COMPILE_OUTPUT_VARIABLE COMPILE_OUT
	  RUN_OUTPUT_VARIABLE RUN_OUT
	 )
  if(RUN_RESULT EQUAL 0)
    message("-- copysign(1.0, modf(-0.0, _)) == -1.0 (ok)")
    set(MODF_OK 1 CACHE INTERNAL "modf is ok")
  else()
    set(MODF_OK 0 CACHE INTERNAL "modf is broken")
    message("-- copysign(1.0, modf(-0.0, _)) == 1.0 (broken)")
  endif()
endif(NOT DEFINED MODF_OK)
