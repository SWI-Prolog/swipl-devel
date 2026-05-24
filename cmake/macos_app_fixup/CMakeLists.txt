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
  # also runs the bundle-fixup script), then pkgbuild wraps the tree
  # into a component .pkg and productbuild adds the installer UI
  # (title, welcome/license/conclusion pages, background icon).
  #
  # We bypass CPack's productbuild generator because that stages each
  # CMake install component into a separate prefix, breaking the
  # fixup script which has to see the combined tree (framework + app)
  # to rewrite cross-component install_names.
  set(component_pkg ${CMAKE_BINARY_DIR}/swipl-component.pkg)
  set(pkg_file
      ${CMAKE_BINARY_DIR}/swipl-${SWIPL_VERSION_STRING}-${CMAKE_SYSTEM_PROCESSOR}.pkg)
  set(installer_resources ${CMAKE_BINARY_DIR}/installer-resources)

  configure_file(
      ${CMAKE_SOURCE_DIR}/desktop/distribution.xml.in
      ${CMAKE_BINARY_DIR}/distribution.xml
      @ONLY)

  if(SWIPL_PKGBUILD_IDENTITY)
    set(productbuild_sign --sign "${SWIPL_PKGBUILD_IDENTITY}")
  else()
    set(productbuild_sign)
  endif()

  add_custom_target(pkg
    COMMAND ${CMAKE_COMMAND} --install ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E remove -f ${pkg_file} ${component_pkg}
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${installer_resources}
    COMMAND sh ${CMAKE_SOURCE_DIR}/scripts/macosx_installer_resources.sh
            ${CMAKE_SOURCE_DIR}/man/macosx
            ${CMAKE_SOURCE_DIR}/desktop/swipl-256.png
            ${installer_resources}
    COMMAND /usr/bin/pkgbuild
            --root ${CMAKE_INSTALL_PREFIX}
            --install-location /
            --identifier org.swi-prolog.swipl-win.pkg
            --version ${SWIPL_VERSION_STRING}
            ${component_pkg}
    COMMAND /usr/bin/productbuild
            --distribution ${CMAKE_BINARY_DIR}/distribution.xml
            --resources ${installer_resources}
            --package-path ${CMAKE_BINARY_DIR}
            ${productbuild_sign}
            ${pkg_file}
    COMMAND ${CMAKE_COMMAND} -E echo "Created ${pkg_file}"
    USES_TERMINAL
    COMMENT "Building macOS .pkg installer")
endif()
