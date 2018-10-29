CMAKE_COMMAND=cmake
PGO_DIR=PGO-data
PGO_PROGRAM=../bench/run.pl

if [ "$1" = "--off" ]; then
  ${CMAKE_COMMAND} -E echo "PGO: disabling"
  ${CMAKE_COMMAND} -DPROFILE_GUIDED_OPTIMIZATION=OFF -G Ninja ..
  ${CMAKE_COMMAND} -E remove_directory ${PGO_DIR}
  ninja -t clean libswipl
  ${CMAKE_COMMAND} -E echo "PGO: run ninja to complete rebuild"
else
  ${CMAKE_COMMAND} -DPROFILE_GUIDED_OPTIMIZATION=GENERATE -G Ninja ..
  ninja -t clean libswipl
  ${CMAKE_COMMAND} -E echo "PGO: Compiling instrumented version"
  ninja prolog_products
  ${CMAKE_COMMAND} -E echo "PGO: Running program"
  ${CMAKE_COMMAND} -E remove_directory ${PGO_DIR}
  src/swipl ${PGO_PROGRAM}
  ${CMAKE_COMMAND} -DPROFILE_GUIDED_OPTIMIZATION=USE -G Ninja ..
  ninja -t clean libswipl
  ${CMAKE_COMMAND} -E echo "PGO: Compiling optimized core"
  ninja prolog_products
  ${CMAKE_COMMAND} -E echo "PGO: run ninja to complete the build"
fi
