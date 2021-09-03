set(CMAKE_SYSTEM_NAME Windows)
set(GNU_HOST x86_64-w64-mingw32)

set(CMAKE_C_COMPILER ${GNU_HOST}-gcc)
set(CMAKE_CXX_COMPILER ${GNU_HOST}-g++)
set(CMAKE_RC_COMPILER ${GNU_HOST}-windres)
set(CMAKE_FIND_ROOT_PATH /usr/${GNU_HOST})

set(CMAKE_CROSSCOMPILING_EMULATOR wine)

if(NOT DEFINED MINGW_ROOT)
  set(MINGW_ROOT $ENV{MINGW64_ROOT} CACHE FILEPATH "MinGW dependencies")
endif()

if(CMAKE_TOOLCHAIN_FILE)
  # Avoid "Manually-specified variables were not used by the project"
endif()
