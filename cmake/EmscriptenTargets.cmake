# Make console binaries runnable through Node.js.

set(WASM_NODE_LINK_FLAGS
    -s NODERAWFS=1
    -s EXIT_RUNTIME=1)
join_list(WASM_NODE_LINK_FLAGS_STRING " " ${WASM_NODE_LINK_FLAGS})

set_target_properties(swipl PROPERTIES
		      LINK_FLAGS "${WASM_NODE_LINK_FLAGS_STRING}")

# Create the preload data containing the libraries. Note that
# alternatively we can put the library in the resource file and
# link the resource file into the main executable.

set(WASM_PRELOAD_DIR "wasm-preload")
set(WASM_BOOT_FILE "wasm-preload/boot.prc")
add_custom_command(
    OUTPUT ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${WASM_PRELOAD_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIPL_BOOT_FILE} ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E copy_directory
			     ${SWIPL_BUILD_LIBRARY} ${WASM_PRELOAD_DIR}/library
    DEPENDS ${SWIPL_BOOT_FILE} prolog_home library_index
    VERBATIM)

add_custom_target(wasm_preload DEPENDS ${WASM_BOOT_FILE})

set(WASM_POST_JS ${CMAKE_CURRENT_SOURCE_DIR}/src/wasm/prolog.js)
add_custom_command(
    OUTPUT wasm/prolog.js
    COMMAND ${CMAKE_COMMAND} -E make_directory wasm
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/wasm/prolog.js wasm/prolog.js
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wasm/prolog.js)
add_custom_target(wasm_js DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wasm/prolog.js)

# Build the browser-deployed binary with a bit different linker flags.

set(WASM_WEB_LINK_FLAGS
    -s WASM=1
    -s MODULARIZE=1
    -s EXPORT_NAME=SWIPL
    -s NO_EXIT_RUNTIME=0
    -s ALLOW_MEMORY_GROWTH=1
    -s EXPORTED_FUNCTIONS=@${CMAKE_SOURCE_DIR}/src/wasm/exports.json
    -s EXPORTED_RUNTIME_METHODS=@${CMAKE_SOURCE_DIR}/src/wasm/runtime_exports.json
    --preload-file ${CMAKE_CURRENT_BINARY_DIR}/${WASM_PRELOAD_DIR}@swipl
    --post-js ${CMAKE_CURRENT_BINARY_DIR}/wasm/prolog.js)
if(MULTI_THREADED)
  list(APPEND WASM_WEB_LINK_FLAGS
       -pthread
       -s PTHREAD_POOL_SIZE=4)
endif()
join_list(WASM_WEB_LINK_FLAGS_STRING " " ${WASM_WEB_LINK_FLAGS})

add_executable(swipl-web ${SWIPL_SRC})
set_target_properties(swipl-web PROPERTIES
		      LINK_FLAGS "${WASM_WEB_LINK_FLAGS_STRING}")
target_link_libraries(swipl-web libswipl)
add_dependencies(swipl-web wasm_preload wasm_js)

