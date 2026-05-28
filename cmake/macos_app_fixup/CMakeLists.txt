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
  # Forward the code-signing identity and entitlements to the
  # fixup script via env.  When SWIPL_CODESIGN_IDENTITY is empty the
  # script's `if [ -n "$CODESIGN_ID" ]' guard skips the Developer-ID
  # signing pass and the bundle ships ad-hoc signed only.
  set(swipl_entitlements
      ${CMAKE_SOURCE_DIR}/desktop/swipl.entitlements.plist)
  install(CODE
    "execute_process(
       COMMAND ${CMAKE_COMMAND} -E env
               \"CODESIGN_ID=${SWIPL_CODESIGN_IDENTITY}\"
               \"ENTITLEMENTS=${swipl_entitlements}\"
               ${CMAKE_SOURCE_DIR}/scripts/macosx_bundle_fixup.sh
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
  if(MACOS_UNIVERSAL_BINARY)
    # CMAKE_SYSTEM_PROCESSOR only reflects the build host; for a fat
    # build the pkg ships binaries for both arm64 and x86_64.
    set(pkg_arch fat)
  else()
    set(pkg_arch ${CMAKE_SYSTEM_PROCESSOR})
  endif()
  set(pkg_file
      ${CMAKE_BINARY_DIR}/swipl-${SWIPL_VERSION_STRING}-${pkg_arch}.pkg)
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

  # If the user has stored notarytool credentials under a profile name
  # (see `xcrun notarytool store-credentials'), submit the productbuild
  # output for notarization and staple the resulting ticket so it's
  # accepted offline.  Both steps are skipped silently otherwise.
  set(notarize_commands)
  if(SWIPL_NOTARYTOOL_PROFILE)
    list(APPEND notarize_commands
         COMMAND xcrun notarytool submit ${pkg_file}
                 --keychain-profile ${SWIPL_NOTARYTOOL_PROFILE}
                 --wait
         COMMAND xcrun stapler staple ${pkg_file})
  endif()

  add_custom_target(pkg
    COMMAND ${CMAKE_COMMAND} --install ${CMAKE_BINARY_DIR}
    COMMAND ${CMAKE_COMMAND} -E remove -f ${pkg_file} ${component_pkg}
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${installer_resources}
    COMMAND sh ${CMAKE_SOURCE_DIR}/scripts/macosx_installer_resources.sh
            ${CMAKE_SOURCE_DIR}/man/macosx
            ${CMAKE_SOURCE_DIR}/desktop/installer-background.png
            ${installer_resources}
    # cmake/port/Darwin.cmake's DragNDrop branch puts Readme.html and
    # License.html at the root of the install tree so they appear at
    # the top of the .dmg.  With pkgbuild --install-location=/ those
    # would target /Readme.html etc. on the sealed System volume;
    # SIP rejects this with "The package is attempting to install
    # content to the system volume."  Drop them from the staging tree
    # before packaging so only the Applications/ and Library/ trees
    # ship in the .pkg.
    COMMAND ${CMAKE_COMMAND} -E rm -f
            ${CMAKE_INSTALL_PREFIX}/Readme.html
            ${CMAKE_INSTALL_PREFIX}/License.html
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
    ${notarize_commands}
    COMMAND ${CMAKE_COMMAND} -E echo "Created ${pkg_file}"
    USES_TERMINAL
    # VERBATIM is required because SWIPL_PKGBUILD_IDENTITY contains
    # parentheses (the Team ID, e.g. "(ACDDGQR4VW)").  Without it
    # CMake only escapes spaces, not parens, and `/bin/sh' chokes on
    # the unquoted `(' with "syntax error near unexpected token".
    VERBATIM
    COMMENT "Building macOS .pkg installer")
endif()
