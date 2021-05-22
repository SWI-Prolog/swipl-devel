set(CMAKE_SYSTEM_NAME Windows)
set(GNU_HOST x86_64-w64-mingw32)

set(CMAKE_C_COMPILER ${GNU_HOST}-gcc)
set(CMAKE_CXX_COMPILER ${GNU_HOST}-g++)
set(CMAKE_RC_COMPILER ${GNU_HOST}-windres)
set(CMAKE_FIND_ROOT_PATH /usr/${GNU_HOST})

if(NOT DEFINED CAN_RUN_NATIVE_W64)
  message(STATUS "Checking for OS support for Windows binaries...")
  set(TEST_BINARY "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/test.exe")
  make_directory("${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp")
  execute_process(
      COMMAND echo "int main(){return 0;}"
      COMMAND ${CMAKE_C_COMPILER} -x c - -o ${TEST_BINARY}
      RESULT_VARIABLE COMPILE_RESULT
      OUTPUT_QUIET ERROR_QUIET
  )
  if(COMPILE_RESULT EQUAL 0)
    execute_process(
        COMMAND ${TEST_BINARY}
        RESULT_VARIABLE EXECUTE_RESULT
        OUTPUT_QUIET ERROR_QUIET
    )
    if(EXECUTE_RESULT EQUAL 0)
      set(CAN_RUN_NATIVE_W64 1 CACHE INTERNAL "Can this system run Win64 binaries directly?" FORCE)
    endif()
  endif()
  unset(COMPILE_RESULT)
  unset(EXECUTE_RESULT)
  unset(TEST_BINARY)
  if(NOT CAN_RUN_NATIVE_W64)
    set(CAN_RUN_NATIVE_W64 "" CACHE INTERNAL "Can this system run Win64 binaries directly?" FORCE)
  endif()
endif(NOT DEFINED CAN_RUN_NATIVE_W64)

if(CAN_RUN_NATIVE_W64)
  message(STATUS "Running Windows binaries with OS support.")
  set(CMAKE_CROSSCOMPILING_EMULATOR "/usr/bin/env")
else(CAN_RUN_NATIVE_W64)
  find_program(CMAKE_CROSSCOMPILING_EMULATOR NAMES wine64 wine
               HINTS ${WINE_DIR} ENV WINE_DIR
               DOC "Location of Wine emulator for cross-compiling build")
  if(NOT CMAKE_CROSSCOMPILING_EMULATOR)
    message(FATAL_ERROR "Could not locate wine. Install it, or set the environment variable WINE_DIR to its path.")
  endif()
  message(STATUS "Using ${CMAKE_CROSSCOMPILING_EMULATOR} to run Windows binaries.")
endif(CAN_RUN_NATIVE_W64)

if(NOT DEFINED MINGW_ROOT)
  set(MINGW_ROOT $ENV{MINGW64_ROOT} CACHE FILEPATH "MinGW dependencies")
endif()

if(CMAKE_TOOLCHAIN_FILE)
  # Don't warn for "unused variable CMAKE_TOOLCHAIN_FILE"
endif()