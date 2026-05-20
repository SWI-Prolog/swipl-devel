# This subdirectory exists solely to host an install(CODE) that runs
# AFTER all other subdirectories have installed their files.  CMake
# emits top-level cmake_install.cmake commands before include()ing
# subdirectory cmake_install scripts, so anything in the top-level
# CMakeLists.txt runs too early to see the foreign extensions placed
# by packages/* subdirs.
#
# add_subdirectory(cmake/macos_app_fixup) is called from the top-level
# CMakeLists.txt AFTER every other add_subdirectory, so this script's
# install(CODE) is the last include in cmake_install.cmake.

if(BUILD_MACOS_BUNDLE)
  install(CODE
    "execute_process(
       COMMAND ${CMAKE_SOURCE_DIR}/scripts/macosx_bundle_fixup.sh
               \${CMAKE_INSTALL_PREFIX}/swipl-win.app)")
endif()
