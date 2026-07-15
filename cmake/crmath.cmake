# The crmath target is created from the upstream CMakeLists.txt by
# FetchContent_MakeAvailable() in Deps.cmake.  Here we adjust the
# target to fit the libswipl build without modifying the sources.

if(TARGET crmath)
  if(LIBSWIPL_TYPE STREQUAL "SHARED")
    set_property(TARGET crmath PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()

  # Upstream hard-codes -O2, which would override our
  # CMAKE_C_FLAGS_<CONFIG>.  Remove it so the project optimization
  # flags apply, while keeping upstream's strict FP options
  # (-fno-fast-math -ffp-contract=off).
  get_target_property(_crmath_options crmath COMPILE_OPTIONS)
  if(_crmath_options)
    list(REMOVE_ITEM _crmath_options -O2)
    set_target_properties(crmath PROPERTIES
			  COMPILE_OPTIONS "${_crmath_options}")
  endif()

  # Make <crmath.h> available for compiling the libswipl objects.
  get_target_property(CRMATH_INCLUDE_DIRS crmath INTERFACE_INCLUDE_DIRECTORIES)
  set(LIBSWIPL_INCLUDES ${LIBSWIPL_INCLUDES} ${CRMATH_INCLUDE_DIRS})

  set(LIBSWIPL_LIBRARIES ${LIBSWIPL_LIBRARIES} crmath)
endif()
