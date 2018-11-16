# Architecture ID
string(TOLOWER ${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_HOST_SYSTEM_NAME}
       SWIPL_ARCH)

if(APPLE)
  include(port/Darwin)
elseif(WIN32)
  include(port/Windows)
elseif(EMSCRIPTEN)
  include(port/Emscripten)
endif()

# See https://gitlab.kitware.com/cmake/cmake/issues/17553
# For now we assume we do not cross-compile from Windows

if(CMAKE_CROSSCOMPILING)
  set(CMAKE_HOST_EXECUTABLE_SUFFIX "")
else()
  set(CMAKE_HOST_EXECUTABLE_SUFFIX ${CMAKE_EXECUTABLE_SUFFIX})
endif()
