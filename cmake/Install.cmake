set(SWIPL_ABS_INSTALL_ARCH_LIB "${CMAKE_INSTALL_PREFIX}/${SWIPL_INSTALL_ARCH_LIB}")

set(CMAKE_SKIP_BUILD_RPATH  FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
if(SWIPL_SHARED_LIB)
  set(CMAKE_INSTALL_RPATH "${SWIPL_ABS_INSTALL_ARCH_LIB}")
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

  list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES
       "${SWIPL_ABS_INSTALL_ARCH_LIB}" isSystemDir)
  if(isSystemDir STREQUAL "-1")
     set(CMAKE_INSTALL_RPATH "${SWIPL_ABS_INSTALL_ARCH_LIB}")
  endif()
endif()

if(BUILD_MACOS_BUNDLE)
  # Split layout: framework at /Library/Frameworks, app at /Applications.
  # From <prefix>/Applications/swipl-win.app/Contents/MacOS/<exe> it is
  # four directories up to <prefix>, then Library/Frameworks.  This lets
  # the binaries find the framework when installed to a non-root prefix
  # (typically a local test dir).  /Library/Frameworks is the deployed
  # absolute path.
  #
  # The second pair of rpaths point at the framework's bundled-dep
  # directory (swipl.framework/Versions/A/Frameworks/).  The bundle
  # fixup pass rewrites every reference to a MacPorts/Homebrew dylib
  # to `@rpath/<basename>' --- without that suffix --- because
  # rewriting to the full nested path would not fit in the headerpad
  # of upstream dylibs (`larger updated load commands do not fit').
  # CMake reserves headerpad for these rpath entries automatically.
  set(CMAKE_INSTALL_RPATH
      "@executable_path/../../../../Library/Frameworks"
      "@executable_path/../../../../Library/Frameworks/swipl.framework/Versions/A/Frameworks"
      "/Library/Frameworks"
      "/Library/Frameworks/swipl.framework/Versions/A/Frameworks")
elseif(BUILD_MACOS_FRAMEWORK)
  # bin/swipl{,-ld} live at swipl.framework/Versions/A/bin/ and link
  # against @rpath/swipl.framework/Versions/A/swipl.  RPATH must point
  # at the directory holding the .framework/ — i.e. four levels up
  # (../../../.. = the install prefix root).  We also add a sibling
  # RPATH for third-party dylibs that we bundle inside the framework
  # at Versions/A/Frameworks/.
  set(CMAKE_INSTALL_RPATH
      "@executable_path/../../../.."
      "@executable_path/../Frameworks")
endif()
