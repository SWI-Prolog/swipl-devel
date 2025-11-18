# Deal  with  installing  runtime  dependencies    such   as  DLLs  from
# dependencies. This file produces these facilities   based  on cmake >=
# 3.21 features.
#
# It is currently only activated for  the   MSVC  build  on Windows. The
# problem is dealth with using old code for MinGW in ports/Windows.cmake
# and using a "bundle fixup" for MacOS.

if(MSVC)
  set(USE_RUNTIME_DEPENDENCY_SET ON)
endif()

if(USE_RUNTIME_DEPENDENCY_SET)

function(register_runtime_dependency_component name)
  get_property(deps GLOBAL PROPERTY SWIPL_RUNTIME_COMPONENTS)
  list(APPEND deps ${name})
  set_property(GLOBAL PROPERTY SWIPL_RUNTIME_COMPONENTS "${deps}")
endfunction()

function(target_runtime_dependencies)
  install(TARGETS ${ARGN}
	  RUNTIME_DEPENDENCY_SET
	  ${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME})
  register_runtime_dependency_component(${CMAKE_INSTALL_DEFAULT_COMPONENT_NAME})
endfunction()

# Get the MSVC runtime distribution
set(_cl "${CMAKE_C_COMPILER}")
file(TO_CMAKE_PATH "${_cl}" _cl_norm)
string(REGEX REPLACE "^(.*/VC)/.*$" "\\1" _vcroot "${_cl_norm}")
file(GLOB _msvc_redists
     "${_vcroot}/Redist/MSVC/*/x64/Microsoft.VC*.CRT")

function(install_target_runtime_dependencies)
  get_property(components GLOBAL PROPERTY SWIPL_RUNTIME_COMPONENTS)
  list(REMOVE_DUPLICATES components)

  foreach(comp ${components})
    install(RUNTIME_DEPENDENCY_SET ${comp}
	    COMPONENT ${comp}
	    PRE_EXCLUDE_REGEXES
	      "api-ms-" "ext-ms-"
	      "python[0-9]*\.dll"
	    POST_EXCLUDE_REGEXES
	      ".*system32/.*"
	    DIRECTORIES
	      ${CMAKE_BINARY_DIR}/src
	      ${_msvc_redists}
	    DESTINATION ${SWIPL_INSTALL_ARCH_EXE})
  endforeach()
endfunction()

else(USE_RUNTIME_DEPENDENCY_SET)

function(target_runtime_dependencies)
endfunction()

function(install_target_runtime_dependencies)
endfunction()

endif(USE_RUNTIME_DEPENDENCY_SET)
