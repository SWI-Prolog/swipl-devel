function(target_c_stack target limit)
if(WIN32)
  if(CMAKE_C_COMPILER_ID MATCHES "Clang|GNU")
    set_target_properties(${target} PROPERTIES
			  LINK_FLAGS "-Wl,--stack,${limit}")
  else()
    set_target_properties(${target} PROPERTIES
			  LINK_FLAGS /STACK:${limit})
  endif()
endif()
endfunction()
