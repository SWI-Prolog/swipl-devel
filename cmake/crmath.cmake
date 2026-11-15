# The crmath target is created from the upstream CMakeLists.txt by
# FetchContent_MakeAvailable() in Deps.cmake.  Here we adjust the
# target to fit the libswipl build without modifying the sources.

include(CheckCCompilerFlag)

if(TARGET crmath)
  if(LIBSWIPL_TYPE STREQUAL "SHARED")
    set_property(TARGET crmath PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()

  # Adjust the compile options set by the upstream CMakeLists.txt:
  #  - Remove -O2, which would override our CMAKE_C_FLAGS_<CONFIG>, so
  #    the project optimization flags apply.  Upstream's strict FP
  #    options (-fno-fast-math -ffp-contract=off) are kept.
  #  - Remove -march=x86-64-v3 when the compiler does not understand
  #    it.  This microarchitecture level is only known to gcc >= 11 and
  #    clang >= 12; older compilers error out on it.  It is only a
  #    performance hint, so dropping it merely loses AVX2 tuning.
  get_target_property(_crmath_options crmath COMPILE_OPTIONS)
  if(_crmath_options)
    list(REMOVE_ITEM _crmath_options -O2)
    if("-march=x86-64-v3" IN_LIST _crmath_options)
      check_c_compiler_flag(-march=x86-64-v3 HAVE_MARCH_X86_64_V3)
      if(NOT HAVE_MARCH_X86_64_V3)
	list(REMOVE_ITEM _crmath_options -march=x86-64-v3)
      endif()
    endif()
    set_target_properties(crmath PROPERTIES
			  COMPILE_OPTIONS "${_crmath_options}")
  endif()

  # Make <crmath.h> available for compiling the libswipl objects.
  get_target_property(CRMATH_INCLUDE_DIRS crmath INTERFACE_INCLUDE_DIRECTORIES)
  set(LIBSWIPL_INCLUDES ${LIBSWIPL_INCLUDES} ${CRMATH_INCLUDE_DIRS})

  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} crmath)
endif()
