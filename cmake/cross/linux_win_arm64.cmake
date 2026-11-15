# Cross-compile for Windows on ARM64 using llvm-mingw
# (https://github.com/mstorsjo/llvm-mingw) with dependencies from the
# MSYS2 CLANGARM64 environment in $MINGW_ROOT.  Build-time Prolog runs
# use a Wine that supports ARM64 PE executables (--enable-archs=aarch64)
# on an aarch64 Linux host.

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR arm64)
set(GNU_HOST aarch64-w64-mingw32)

set(CMAKE_C_COMPILER ${GNU_HOST}-clang)
set(CMAKE_CXX_COMPILER ${GNU_HOST}-clang++)
set(CMAKE_RC_COMPILER ${GNU_HOST}-windres)
set(CMAKE_OBJDUMP ${GNU_HOST}-objdump)

set(CMAKE_CROSSCOMPILING_EMULATOR wine)

if(NOT DEFINED MINGW_ROOT)
  set(MINGW_ROOT $ENV{MINGW_ROOT} CACHE FILEPATH "MinGW dependencies")
endif()

set(CMAKE_FIND_ROOT_PATH ${MINGW_ROOT})

if(CMAKE_TOOLCHAIN_FILE)
  # Avoid "Manually-specified variables were not used by the project"
endif()
