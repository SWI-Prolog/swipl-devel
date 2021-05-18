CMAKE_COMMAND=cmake
PGO_DIR=PGO-data
SRC_DIR="$(dirname "$(dirname "$0")")"
PGO_PROGRAM="${SRC_DIR}/bench/run.pl"

set -e # Exit on any error, rather than continuing

if [ "$1" = "--off" ]; then
  ${CMAKE_COMMAND} -E echo "PGO: disabling"
  ${CMAKE_COMMAND} -DPROFILE_GUIDED_OPTIMIZATION=OFF -G Ninja ${SRC_DIR}
  ${CMAKE_COMMAND} -E remove_directory ${PGO_DIR}
  ninja -t clean libswipl
  ${CMAKE_COMMAND} -E echo "PGO: run ninja to complete rebuild"
else
  ${CMAKE_COMMAND} -DPROFILE_GUIDED_OPTIMIZATION=GENERATE -G Ninja ${SRC_DIR}
  ninja -t clean libswipl
  ${CMAKE_COMMAND} -E echo "PGO: Compiling instrumented version"
  ${CMAKE_COMMAND} -E remove_directory ${PGO_DIR}
  ninja prolog_home
  ninja core
  ${CMAKE_COMMAND} -E echo "PGO: Running program"
  ${CMAKE_COMMAND} -E remove_directory ${PGO_DIR}

  if [ -x src/swipl.exe ]; then
    SWIPL=src/swipl.exe
    if ! $SWIPL --help &>/dev/null; then
      # Can't run directly, try using wine
      SWIPL="wine $SWIPL"
    fi
  else
    SWIPL=src/swipl
  fi

  ${SWIPL} -f none --no-packs --no-threads ${PGO_PROGRAM}

  ${CMAKE_COMMAND} -DPROFILE_GUIDED_OPTIMIZATION=USE -G Ninja ${SRC_DIR}
  ${CMAKE_COMMAND} -E echo "PGO: Assembling profile data (for Clang)"
  ninja pgo_data
  ninja -t clean libswipl
  ${CMAKE_COMMAND} -E echo "PGO: Compiling optimized core"
  ninja core
  ${CMAKE_COMMAND} -E echo "PGO: run ninja to complete the build"
fi
