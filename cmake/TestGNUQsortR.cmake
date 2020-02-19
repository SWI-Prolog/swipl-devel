IF(NOT DEFINED QSORT_R_GNU)
  TRY_RUN(RUN_RESULT COMPILE_RESULT
	  "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
	  "${CMAKE_CURRENT_LIST_DIR}/TestGNUQsortR.c"
	  COMPILE_OUTPUT_VARIABLE COMPILE_OUT
	  RUN_OUTPUT_VARIABLE RUN_OUT
	 )
  if(RUN_RESULT EQUAL 0)
    message("-- qsort_r() has GNU signature")
    set(QSORT_R_GNU 1 CACHE INTERNAL "qsort_r() has GNU signature")
  else()
    message("-- qsort_r() has BSD signature")
    set(QSORT_R_GNU 0 CACHE INTERNAL "qsort_r() has BSD signature")
  endif()
endif(NOT DEFINED QSORT_R_GNU)
