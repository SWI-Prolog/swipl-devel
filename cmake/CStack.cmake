function(target_c_stack target limit)
if(WIN32)
  if(CMAKE_C_COMPILER_ID MATCHES "Clang|GNU")
    set_target_properties(${target} PROPERTIES
			  LINK_FLAGS "-Wl,--stack,${limit}")
  else()
    # Use generator expression for multi-config generators (Visual Studio)
    # Default to Release stack size, but use Debug stack size for Debug configuration
    if(SWIPL_C_STACK_SIZE_DEBUG AND SWIPL_C_STACK_SIZE_RELEASE)
      target_link_options(${target} PRIVATE
        $<$<CONFIG:Debug>:/STACK:${SWIPL_C_STACK_SIZE_DEBUG}>
        $<$<CONFIG:Release>:/STACK:${SWIPL_C_STACK_SIZE_RELEASE}>
        $<$<CONFIG:RelWithDebInfo>:/STACK:${SWIPL_C_STACK_SIZE_RELEASE}>
        $<$<CONFIG:MinSizeRel>:/STACK:${SWIPL_C_STACK_SIZE_RELEASE}>
      )
    else()
      set_target_properties(${target} PROPERTIES
			    LINK_FLAGS /STACK:${limit})
    endif()
  endif()
endif()
endfunction()
