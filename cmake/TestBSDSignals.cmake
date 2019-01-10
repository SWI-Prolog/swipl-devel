IF(NOT DEFINED BSD_SIGNALS)
  TRY_RUN(RUN_RESULT COMPILE_RESULT
	  "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/"
	  "${CMAKE_CURRENT_LIST_DIR}/TestBSDSignals.c"
	  COMPILE_OUTPUT_VARIABLE COMPILE_OUT
	  RUN_OUTPUT_VARIABLE RUN_OUT
	 )
  if(RUN_RESULT EQUAL 0)
    message("-- signal handler is not reset (BSD signals)")
    set(BSD_SIGNALS 1 CACHE INTERNAL "BSD signal semantics")
  else()
    set(BSD_SIGNALS 0 CACHE INTERNAL "BSD signal semantics")
    message("-- signal handler is reset")
  endif()
endif(NOT DEFINED BSD_SIGNALS)
