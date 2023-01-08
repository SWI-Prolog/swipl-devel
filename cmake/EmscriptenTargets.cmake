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

set(WASM_BOOT_FILE "${WASM_PRELOAD_DIR}/boot.prc")
add_custom_command(
    OUTPUT ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${WASM_PRELOAD_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIPL_BOOT_FILE} ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E copy_directory
			     ${SWIPL_BUILD_LIBRARY} ${WASM_PRELOAD_DIR}/library
    DEPENDS ${SWIPL_BOOT_FILE} prolog_home library_index
    VERBATIM)

add_custom_target(wasm_preload_dir DEPENDS ${WASM_BOOT_FILE})
add_dependencies(wasm_preload wasm_preload_dir)

# Build the browser-deployed binary with a bit different linker flags.

set(POSTJS ${CMAKE_CURRENT_SOURCE_DIR}/wasm/prolog.js)
set(PREJS ${CMAKE_CURRENT_SOURCE_DIR}/wasm/pre.js)

set(WASM_SHARED_LINK_FLAGS
    -s WASM=1
    -s MODULARIZE=1
    -s EXPORT_NAME=SWIPL
    -s NO_EXIT_RUNTIME=0
    -s WASM_BIGINT=1
    -s ALLOW_MEMORY_GROWTH=1
    -s EXPORTED_FUNCTIONS=@${CMAKE_SOURCE_DIR}/src/wasm/exports.json
    -s EXPORTED_RUNTIME_METHODS=@${CMAKE_SOURCE_DIR}/src/wasm/runtime_exports.json
    --pre-js ${PREJS}
    --post-js ${POSTJS})
if(MULTI_THREADED)
  list(APPEND WASM_SHARED_LINK_FLAGS
       -pthread
       -s PTHREAD_POOL_SIZE=4)
endif()

set(WASM_WEB_LINK_FLAGS
    --preload-file ${WASM_PRELOAD_DIR}@swipl)
join_list(WASM_WEB_LINK_FLAGS_STRING " " ${WASM_WEB_LINK_FLAGS} " " ${WASM_SHARED_LINK_FLAGS})
add_executable(swipl-web ${SWIPL_SRC})
set_target_properties(swipl-web PROPERTIES
		      LINK_FLAGS "${WASM_WEB_LINK_FLAGS_STRING}")
target_link_libraries(swipl-web libswipl)
add_dependencies(swipl-web wasm_preload)
set_property(TARGET swipl-web PROPERTY LINK_DEPENDS
	     ${POSTJS} ${PREJS})

set(WASM_BUNDLE_LINK_FLAGS
    -s SINGLE_FILE
    --embed-file ${WASM_PRELOAD_DIR}@swipl)
join_list(WASM_BUNDLE_LINK_FLAGS_STRING " " ${WASM_BUNDLE_LINK_FLAGS} " " ${WASM_SHARED_LINK_FLAGS})
add_executable(swipl-bundle ${SWIPL_SRC})
set_target_properties(swipl-bundle PROPERTIES
		      LINK_FLAGS "${WASM_BUNDLE_LINK_FLAGS_STRING}")
target_link_libraries(swipl-bundle libswipl)
add_dependencies(swipl-bundle wasm_preload)
set_property(TARGET swipl-bundle PROPERTY LINK_DEPENDS
	     ${POSTJS} ${PREJS})
