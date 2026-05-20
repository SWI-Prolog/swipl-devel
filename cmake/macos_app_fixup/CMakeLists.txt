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
               \${CMAKE_INSTALL_PREFIX})")

  # `ninja pkg' produces a single .pkg installer that delivers both
  # /Applications/swipl-win.app and /Library/Frameworks/swipl.framework.
  # It runs `cmake --install' to populate CMAKE_INSTALL_PREFIX (which
  # also runs the bundle-fixup script), then pkgbuild captures the
  # tree as the installer payload.
  #
  # We bypass CPack's productbuild generator because that stages each
  # CMake install component into a separate prefix, breaking the
  # fixup script which has to see the combined tree (framework + app)
  # to rewrite cross-component install_names.
  set(pkg_file
      ${CMAKE_BINARY_DIR}/swipl-${SWIPL_VERSION_STRING}-${CMAKE_SYSTEM_PROCESSOR}.pkg)

  if(SWIPL_PKGBUILD_IDENTITY)
    set(pkgbuild_sign --sign "${SWIPL_PKGBUILD_IDENTITY}")
  else()
    set(pkgbuild_sign)
  endif()

  add_custom_target(pkg
    COMMAND ${CMAKE_COMMAND} --install ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E remove -f ${pkg_file}
    COMMAND /usr/bin/pkgbuild
            --root ${CMAKE_INSTALL_PREFIX}
            --install-location /
            --identifier org.swi-prolog.swipl-win.pkg
            --version ${SWIPL_VERSION_STRING}
            ${pkgbuild_sign}
            ${pkg_file}
    COMMAND ${CMAKE_COMMAND} -E echo "Created ${pkg_file}"
    USES_TERMINAL
    COMMENT "Building macOS .pkg installer")
endif()
