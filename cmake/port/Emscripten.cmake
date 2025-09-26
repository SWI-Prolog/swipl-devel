# AlignOf.cmake doesn't work for Emscripten
set(ALIGNOF_INT64_T 8 CACHE STRING "Alignment for int64_t")
set(ALIGNOF_VOIDP   ${SIZEOF_VOIDP} CACHE STRING "Alignment for pointers")
set(ALIGNOF_DOUBLE  8 CACHE STRING "Alignment for double")

set(PLHOME     "/swipl")
set(SWIPL_ARCH "wasm-emscripten")
set(USE_TCMALLOC OFF)
set(USE_SIGNALS OFF)
set(MULTI_THREADED OFF)
set(STATIC_EXTENSIONS ON)
set(BUILD_SWIPL_LD OFF)

set(SRC_OS_SPECIFIC wasm/pl-wasm.c)

if(WASM_EXCEPTIONS)
  add_compile_options(-fwasm-exceptions -sSUPPORT_LONGJMP=wasm)
endif()
if(MULTI_THREADED)
  add_compile_options(-pthread)
endif()

set(WASM_PRELOAD_DIR "${CMAKE_BINARY_DIR}/src/wasm-preload")
add_custom_target(wasm_preload)

# Install a file from the binary directory of a package in a specific
# directory of the wasm-preload directory

function(install_in_wasm_preload dir file)
  string(REPLACE "/" "-" depname ${dir}${file})
  add_custom_command(
      OUTPUT ${WASM_PRELOAD_DIR}/${dir}/${file}
      COMMAND ${CMAKE_COMMAND} -E copy ${file} ${WASM_PRELOAD_DIR}/${dir}
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file})
  add_custom_target(wasm_preload${depname}
		    DEPENDS ${WASM_PRELOAD_DIR}/${dir}/${file})
  add_dependencies(wasm_preload wasm_preload${depname})
endfunction()
