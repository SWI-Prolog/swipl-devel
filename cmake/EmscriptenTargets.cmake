# Make console binaries runnable through Node.js.

set(SWIPL_C_STACK_SIZE 1048576)

set(WASM_SHARED_LINK_FLAGS
    -s WASM_BIGINT=1
    -s ALLOW_MEMORY_GROWTH=1
    -s STACK_SIZE=${SWIPL_C_STACK_SIZE}
    -s LZ4=1
    -s STACK_OVERFLOW_CHECK=1)
if(WASM_EXCEPTIONS)
  list(APPEND WASM_SHARED_LINK_FLAGS
       -s SUPPORT_LONGJMP=wasm)
endif()
if(MULTI_THREADED)
  list(APPEND WASM_SHARED_LINK_FLAGS
       -pthread
       -s PTHREAD_POOL_SIZE=4)
endif()

# First, configure src/swipl.js as  a  node   executable  with  raw file
# system access. This can be executed   as `node src/swipl.js [options]`
# as a normal local SWI-Prolog executable. We   use  this to perform the
# Prolog build steps such as generating the   boot.prc file and the .qlf
# files.

set(WASM_NODE_LINK_FLAGS
    -s NODERAWFS=1
    -s EXIT_RUNTIME=1)
list(APPEND WASM_NODE_LINK_FLAGS ${WASM_SHARED_LINK_FLAGS})
join_list(WASM_NODE_LINK_FLAGS_STRING " " ${WASM_NODE_LINK_FLAGS})

set_target_properties(swipl PROPERTIES
		      LINK_FLAGS "${WASM_NODE_LINK_FLAGS_STRING}")

# Create the preload data containing the libraries. Note that
# alternatively we can put the library in the resource file and
# link the resource file into the main executable.

set(WASM_BOOT_FILE "${WASM_PRELOAD_DIR}/boot.prc")
set(WASM_ABI_FILE "${WASM_PRELOAD_DIR}/ABI")

has_package(chr PKG_CHR)

find_program(PROG_RSYNC rsync)
if(PROG_RSYNC)
  set(RSYNC_DEL_FLAGS -r --ignore-existing --delete)
else()
  set(PROG_RSYNC :)
endif()

set(wasm_preload_depends
    ${SWIPL_BOOT_FILE} prolog_home bootfile library_index)
if(INSTALL_QLF)
  list(APPEND wasm_preload_depends library_qlf)
endif()

if(INSTALL_PROLOG_SRC)
  set(CP_SRC :)
else()
  list(APPEND RSYNC_DEL_FLAGS --delete-excluded --exclude=[a-z]*.pl)
  list(TRANSFORM WASM_prolog_src PREPEND ${SWIPL_BUILD_LIBRARY}/)
  set(CP_SRC cp -Llf ${WASM_prolog_src} ${WASM_PRELOAD_DIR}/library)
endif()

# Copy the CHR main file.  These are built in packages/chr rather than
# home/library, so we need to deal with them explicitly.
if(PKG_CHR)
  set(chr_bin_dir ${CMAKE_BINARY_DIR}/packages/chr)
  set(chr_files)
  set(chr_chr_files)
  if(INSTALL_PROLOG_SRC)
    list(APPEND chr_files ${chr_bin_dir}/chr.pl)
    list(APPEND chr_chr_files
	 ${chr_bin_dir}/chr_translate.pl
	 ${chr_bin_dir}/guard_entailment.pl)
  endif()
  if(INSTALL_QLF)
    list(APPEND chr_files ${chr_bin_dir}/chr.qlf)
    list(APPEND chr_chr_files
	 ${chr_bin_dir}/chr_translate.qlf
	 ${chr_bin_dir}/guard_entailment.qlf)
  endif()
  set(CP_CHR1
      cp -LlRf ${chr_files} ${WASM_PRELOAD_DIR}/library/ext/chr/)
  set(CP_CHR2
      cp -LlRf ${chr_chr_files} ${WASM_PRELOAD_DIR}/library/ext/chr/chr/)
  list(APPEND wasm_preload_depends chr)
else()
  set(CP_CHR1 :)
  set(CP_CHR2 :)
endif()

# On Linux we use  hard links using GNU cp -l.   Using soft links does
# not work  as it makes  the preload  file system holds  just dangling
# symlinks.   Using links  makes sure  edits in  the Prolog  files are
# reflected in the output.
if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
  #message("${SWIPL_BUILD_LIBRARY} ${WASM_PRELOAD_DIR}")
  add_custom_command(
    OUTPUT ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${WASM_PRELOAD_DIR}
    COMMAND cp -Llf ${SWIPL_BOOT_FILE} ${WASM_BOOT_FILE}
    COMMAND cp -Llf ${SWIPL_ABI_FILE} ${WASM_ABI_FILE}
    COMMAND cp -LlRf ${SWIPL_BUILD_LIBRARY} ${WASM_PRELOAD_DIR}
    COMMAND ${PROG_RSYNC} ${RSYNC_DEL_FLAGS} ${SWIPL_BUILD_LIBRARY}/ ${WASM_PRELOAD_DIR}/library
    COMMAND ${CP_CHR1}
    COMMAND ${CP_CHR2}
    COMMAND ${CP_SRC}
    DEPENDS ${wasm_preload_depends}
    VERBATIM)
else()
  add_custom_command(
    OUTPUT ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${WASM_PRELOAD_DIR}
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIPL_BOOT_FILE} ${WASM_BOOT_FILE}
    COMMAND ${CMAKE_COMMAND} -E copy ${SWIPL_ABI_FILE} ${WASM_ABI_FILE}
    COMMAND ${CMAKE_COMMAND} -E copy_directory
			     ${SWIPL_BUILD_LIBRARY} ${WASM_PRELOAD_DIR}/library
    DEPENDS ${wasm_preload_depends}
    VERBATIM)
endif()

add_custom_target(wasm_preload_dir DEPENDS ${WASM_BOOT_FILE})
add_dependencies(wasm_preload wasm_preload_dir)

# Next, build the binaries for deployment.
# Build the browser-deployed binary with a bit different linker flags.

set(PREJS ${CMAKE_CURRENT_SOURCE_DIR}/wasm/prolog.js)

set(WASM_DIST_LINK_FLAGS
    -s WASM=1
    -s MODULARIZE=1
    -s EXPORT_NAME=SWIPL
    -s NO_EXIT_RUNTIME=0
    -s EXPORTED_FUNCTIONS=@${CMAKE_SOURCE_DIR}/src/wasm/exports.json
    -s EXPORTED_RUNTIME_METHODS=@${CMAKE_SOURCE_DIR}/src/wasm/runtime_exports.json
    --pre-js ${PREJS})
list(APPEND WASM_DIST_LINK_FLAGS ${WASM_SHARED_LINK_FLAGS})
if(MULTI_THREADED)
  list(APPEND WASM_DIST_LINK_FLAGS
       -pthread
       -s PTHREAD_POOL_SIZE=4)
endif()

# Create swipl-web.js, swipl-web.wasm, swipl-web.data
set(WASM_WEB_LINK_FLAGS
    --preload-file ${WASM_PRELOAD_DIR}@swipl -Wno-unused-main)
join_list(WASM_WEB_LINK_FLAGS_STRING " "
	  ${WASM_WEB_LINK_FLAGS} ${WASM_DIST_LINK_FLAGS})
add_executable(swipl-web ${SWIPL_SRC})
set_target_properties(swipl-web PROPERTIES
		      LINK_FLAGS "${WASM_WEB_LINK_FLAGS_STRING}")
target_link_libraries(swipl-web libswipl)
add_dependencies(swipl-web wasm_preload)
set_property(TARGET swipl-web PROPERTY LINK_DEPENDS
	     ${POSTJS} ${PREJS})

# Create swipl-bundle.js
set(WASM_BUNDLE_LINK_FLAGS
    -s SINGLE_FILE -Wno-unused-main
    --embed-file ${WASM_PRELOAD_DIR}@swipl)
join_list(WASM_BUNDLE_LINK_FLAGS_STRING " "
	  ${WASM_BUNDLE_LINK_FLAGS} ${WASM_DIST_LINK_FLAGS})
add_executable(swipl-bundle ${SWIPL_SRC})
set_target_properties(swipl-bundle PROPERTIES
		      LINK_FLAGS "${WASM_BUNDLE_LINK_FLAGS_STRING}")
target_link_libraries(swipl-bundle libswipl)
add_dependencies(swipl-bundle wasm_preload)
set_property(TARGET swipl-bundle PROPERTY LINK_DEPENDS
	     ${POSTJS} ${PREJS})

# Create swipl-bundle-no-data.js
set(WASM_NO_DATA_BUNDLE_LINK_FLAGS
    -s SINGLE_FILE -Wno-unused-main)
join_list(WASM_NO_DATA_BUNDLE_LINK_FLAGS_STRING " "
	  ${WASM_NO_DATA_BUNDLE_LINK_FLAGS} ${WASM_DIST_LINK_FLAGS})
add_executable(swipl-bundle-no-data ${SWIPL_SRC})
set_target_properties(swipl-bundle-no-data PROPERTIES
		      LINK_FLAGS "${WASM_NO_DATA_BUNDLE_LINK_FLAGS_STRING}")
target_link_libraries(swipl-bundle-no-data libswipl)
set_property(TARGET swipl-bundle-no-data PROPERTY LINK_DEPENDS
	     ${POSTJS} ${PREJS})
