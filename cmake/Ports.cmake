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
