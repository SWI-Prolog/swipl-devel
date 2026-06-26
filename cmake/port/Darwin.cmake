if(APPLE)

set(CMAKE_FIND_APPBUNDLE NEVER) # Make sure cmake does not find the Apple gui version of GIT

if(MACOS_UNIVERSAL_BINARY)
  set(SWIPL_ARCH fat-darwin)
endif()

function(latest_subdir var dir)
  FILE(GLOB children RELATIVE "${dir}" "${dir}/*")
  set(subdir)
  foreach(child ${children})
    if(NOT subdir OR child VERSION_GREATER subdir)
      set(subdir "${child}")
    endif()
  endforeach()
  set(${var} ${dir}/${subdir} PARENT_SCOPE)
endfunction()

if(NOT MACOSX_DEPENDENCIES_FROM)
  if(EXISTS /opt/local/bin/port)
     set(MACOSX_DEPENDENCIES_FROM Macports)
  elseif(EXISTS /opt/homebrew)
  	 set(MACOSX_DEPENDENCIES_FROM Homebrew)
  elseif(EXISTS /usr/local/Cellar)
     set(MACOSX_DEPENDENCIES_FROM Homebrew)
  else()
    set(MACOSX_DEPENDENCIES_FROM None)
    message(WARNING "Could not find Macport or Homebrew to provide dependencies \
		     trying to configure with default search paths")
  endif()
  set(MACOSX_DEPENDENCIES_FROM ${MACOSX_DEPENDENCIES_FROM}
      CACHE STRING "Get dependencies from Macports or HomeBrew")
endif()

# Intel Macs use /usr/local; Apple Silicon uses /opt/homebrew
if(EXISTS /opt/homebrew)
  set(HOMEBREW_PREFIX /opt/homebrew)
elseif(EXISTS /usr/local/Cellar)
  set(HOMEBREW_PREFIX /usr/local)
endif()

if(MACOSX_DEPENDENCIES_FROM STREQUAL "Macports")
  message("-- Using Macports packages from /opt/local")
  set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
      /opt/local/lib)
  set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
      /opt/local/include)
  set(CMAKE_FRAMEWORK_PATH /opt/local/Library/Frameworks)
  if(EXISTS /opt/local/libexec/jpeg)
    set(JPEG_ROOT /opt/local/libexec/jpeg)
  endif()
elseif(MACOSX_DEPENDENCIES_FROM STREQUAL "Homebrew")
  message("-- Using Homebrew packages from ${HOMEBREW_PREFIX}")
  set(CMAKE_FIND_ROOT_PATH ${HOMEBREW_PREFIX})
  set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
      ${HOMEBREW_PREFIX}/lib)
  set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
      ${HOMEBREW_PREFIX}/include)
  latest_subdir(OPENSSL_ROOT_DIR ${HOMEBREW_PREFIX}/Cellar/openssl@3/)
  latest_subdir(LibArchive_ROOT ${HOMEBREW_PREFIX}/Cellar/libarchive)
  latest_subdir(BDB_ROOT ${HOMEBREW_PREFIX}/Cellar/berkeley-db)
  set(CMAKE_IGNORE_PATH
      /opt/local/lib
      /opt/local/include
      /opt/local/bin)
elseif(MACOSX_DEPENDENCIES_FROM STREQUAL None)
  message("-- Trying to build without Macports or Homebrew dependencies")
elseif(MACOSX_DEPENDENCIES_FROM MATCHES "/.*")
  message("-- Trying to build with dependencies from ${MACOSX_DEPENDENCIES_FROM}")
  set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
      "${MACOSX_DEPENDENCIES_FROM}/lib")
  set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
      "${MACOSX_DEPENDENCIES_FROM}/include")
  set(CMAKE_IGNORE_PATH
      /opt/local/lib
      /opt/local/include
      /opt/local/bin
      /usr/local/lib
      /usr/local/include
      /usr/local/bin
      /opt/homebrew/lib
      /opt/homebrew/include
      /opt/homebrew/bin)
else()
  message(FATAL_ERROR "Invalid MACOSX_DEPENDENCIES_FROM: ${MACOSX_DEPENDENCIES_FROM}")
endif()

if(BUILD_MACOS_BUNDLE)
  set(MACOS_APP "swipl-win")
  set(EPILOG_APP "${MACOS_APP}")

  if(MACOS_UNIVERSAL_BINARY)
    set(SWIPL_CPACK_ARCH fat)
  endif()

# These definitions must be here rather than in CPack.cmake as that
# file is loaded after attaching packages/swipl-win and thus isn't picked
# up.  See URL below for the defined variables.
# https://cmake.org/cmake/help/latest/prop_tgt/MACOSX_BUNDLE_INFO_PLIST.html

  # Default CPack generator is DragNDrop (a .dmg).  The .pkg installer
  # is built by a separate `pkg' custom target (see below) using
  # pkgbuild directly against ${CMAKE_INSTALL_PREFIX}, which is the
  # only way to get the fixup-script-modified bits with the absolute
  # /Applications + /Library/Frameworks split layout intact.  CPack's
  # productbuild generator stages each CMake component separately,
  # which prevents the fixup script from seeing the combined tree.
  set(SWIPL_MACOS_PACKAGE "DragNDrop"
      CACHE STRING "macOS CPack generator (DragNDrop)")
  set(CPACK_GENERATOR "${SWIPL_MACOS_PACKAGE}")

  set(SWIPL_APP_NAME swipl-win)
  set(CPACK_BUNDLE_NAME         "${SWIPL_APP_NAME}")
  set(CPACK_BUNDLE_EXECUTABLE   "${EPILOG_APP}")
  set(CPACK_BUNDLE_VERSION      "${SWIPL_VERSION_STRING}")

  set(CPACK_BUNDLE_ICON         "swipl.icns")
  set(CPACK_BUNDLE_IDENTIFIER   "org.swi-prolog.swipl-win")
  set(CPACK_BUNDLE_COPYRIGHT    "BSD-2")
  set(CPACK_BUNDLE_PLIST        "${CMAKE_SOURCE_DIR}/desktop/Info.plist.in")

  # Identity / keychain variables for the `pkg' target.  Left empty so
  # the .pkg is produced unsigned; supply them via `cmake -D' (or the
  # cache) once an Apple Developer ID is available.
  set(SWIPL_PKGBUILD_IDENTITY ""
      CACHE STRING "Developer ID Installer identity for .pkg signing")
  set(SWIPL_CODESIGN_IDENTITY ""
      CACHE STRING "Developer ID Application identity for code signing")
  set(SWIPL_NOTARYTOOL_PROFILE ""
      CACHE STRING "notarytool keychain profile name (for stapler/notarize step)")

  function(deploy)
    set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME ZZRuntime)
    if(CPACK_GENERATOR STREQUAL "DragNDrop")
      # Top-level files visible on the disk image
      install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/SWIapp.html
        DESTINATION .
        RENAME Readme.html)
      install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/License.html
        DESTINATION .)
    else()
      # For .pkg installs, place the docs inside the app so the
      # installer doesn't litter /Applications with extra files.
      install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/SWIapp.html
        DESTINATION ${SWIPL_INSTALL_RESOURCES}
        RENAME Readme.html)
      install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/License.html
        DESTINATION ${SWIPL_INSTALL_RESOURCES})
    endif()
  endfunction()

  deploy()
  # The fixup script must run AFTER all package subdirs install their
  # foreign extensions; it is therefore registered from the top-level
  # CMakeLists.txt at the bottom of the install order — search for
  # MACOSX_BUNDLE_FIXUP_HOOK there.
endif()

set(CMAKE_MACOSX_RPATH ON)

# Prefer sem_open() over deprecated sem_init()
set(USE_SEM_OPEN 1)
set(SO_PATH DYLD_LIBRARY_PATH)

endif(APPLE)
