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
  elseif(EXISTS /usr/local/bin/brew)
	  set(MACOSX_DEPENDENCIES_FROM HomebrewLocal)
  elseif(EXISTS /opt/homebrew)
	  set(MACOSX_DEPENDENCIES_FROM HomebrewOpt)
  else()
    set(MACOSX_DEPENDENCIES_FROM None)
    message(WARNING "Could not find Macport or Homebrew to provide dependencies \
		     trying to configure with default search paths")
  endif()
  set(MACOSX_DEPENDENCIES_FROM ${MACOSX_DEPENDENCIES_FROM}
      CACHE STRING "Get dependencies from Macports or HomeBrew")
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
  message("-- Using Homebrew packages from /opt/homebrew")
  set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
      /opt/homebrew/lib)
  set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
      /opt/homebrew/include)
  latest_subdir(OPENSSL_ROOT_DIR /opt/homebrew/Cellar/openssl@3/)
  latest_subdir(LibArchive_ROOT /opt/homebrew/Cellar/libarchive)
  latest_subdir(BDB_ROOT /opt/homebrew/Cellar/berkeley-db)
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
  set(MACOS_APP "SWI-Prolog")
  set(EPILOG_APP "${MACOS_APP}")

  if(MACOS_UNIVERSAL_BINARY)
    set(SWIPL_CPACK_ARCH fat)
  endif()

# These definitions must be here rather than in CPack.cmake as that
# file is loaded after attaching packages/swipl-win and thus isn't picked
# up.  See URL below for the defined variables.
# https://cmake.org/cmake/help/latest/prop_tgt/MACOSX_BUNDLE_INFO_PLIST.html

  set(CPACK_GENERATOR "DragNDrop")
  set(SWIPL_APP_NAME SWI-Prolog)
  set(CPACK_BUNDLE_NAME         "${SWIPL_APP_NAME}")
  set(CPACK_BUNDLE_EXECUTABLE   "${EPILOG_APP}")
  set(CPACK_BUNDLE_VERSION      "${SWIPL_VERSION_STRING}")

  set(CPACK_BUNDLE_ICON         "swipl.icns")
  set(CPACK_BUNDLE_IDENTIFIER   "org.swi-prolog.app")
  set(CPACK_BUNDLE_COPYRIGHT    "BSD-2")
  set(CPACK_BUNDLE_PLIST        "${CMAKE_SOURCE_DIR}/desktop/Info.plist.in")

  function(deploy)
    set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME ZZRuntime)
    install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/SWIapp.html
      DESTINATION .
      RENAME Readme.html)
    install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/License.html
      DESTINATION .)

    install(CODE "set(fixup_script ${CMAKE_SOURCE_DIR}/scripts/macosx_bundle_fixup.sh)
		 ")
    install(CODE [===[
      execute_process(COMMAND ln -sf SWI-Prolog swipl-win
		      WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app/Contents/MacOS)
      execute_process(COMMAND ${fixup_script} --epilog ${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app)
    ]===])
  endfunction()

  deploy()
endif()

set(CMAKE_MACOSX_RPATH ON)

# Prefer sem_open() over deprecated sem_init()
set(USE_SEM_OPEN 1)
set(SO_PATH DYLD_LIBRARY_PATH)

endif(APPLE)
