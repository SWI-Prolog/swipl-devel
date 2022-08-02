# AlignOf.cmake doesn't work for Emscripten
set(ALIGNOF_INT64_T 8 CACHE STRING "Alignment for int64_t")
set(ALIGNOF_VOIDP   ${SIZEOF_VOIDP} CACHE STRING "Alignment for pointers")
set(ALIGNOF_DOUBLE  8 CACHE STRING "Alignment for double")

set(PLHOME     "/swipl")
set(SWIPL_ARCH "wasm-emscripten")
