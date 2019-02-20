# We could set set linker flags for all excutables if we
# could override them for the browser binary.
#set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -s ASSERTIONS=2 -s NODERAWFS=1 -s EXIT_RUNTIME=1")

# AlignOf.cmake doesn't work for Emscripten
set(ALIGNOF_INT64_T 8 CACHE STRING "Alignment for int64_t")
set(ALIGNOF_VOIDP   ${SIZEOF_VOIDP} CACHE STRING "Alignment for pointers")
set(ALIGNOF_DOUBLE  8 CACHE STRING "Alignment for double")
