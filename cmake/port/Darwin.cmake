if(APPLE)

if(NOT MACOSX_DEPENDENCIES_FROM)
  if(EXISTS /opt/local/bin/port)
    set(MACOSX_DEPENDENCIES_FROM Macports)
  elseif(EXISTS /usr/local/bin/brew)
    set(MACOSX_DEPENDENCIES_FROM Homebrew)
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
elseif(MACOSX_DEPENDENCIES_FROM STREQUAL "Homebrew")
  message("-- Using Homebrew packages from /usr/local")
  set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
      /usr/local/lib)
  set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
      /usr/local/include)

  if(IS_DIRECTORY /usr/local/opt/openssl)
    set(OPENSSL_ROOT_DIR "/usr/local/opt/openssl")
  elseif(IS_DIRECTORY /usr/local/opt/openssl@1.1)
    set(OPENSSL_ROOT_DIR "/usr/local/opt/openssl@1.1")
  else()
    set(OPENSSL_ROOT_DIR "/usr/local/opt/openssl")
  endif()

  set(LibArchive_ROOT /usr/local/opt/libarchive)
  set(Readline_ROOT /usr/local/opt/readline)
elseif(MACOSX_DEPENDENCIES_FROM STREQUAL None)
  message("-- Trying to build without Macports or Homebrew dependencies")
else()
  message(FATAL_ERROR "Invalid MACOSX_DEPENDENCIES_FROM: ${MACOSX_DEPENDENCIES_FROM}")
endif()

if(BUILD_MACOS_BUNDLE)
  set(MACOS_APP "SWI-Prolog")

  find_package(Qt5 COMPONENTS Widgets REQUIRED)

  get_target_property(uic_location Qt5::uic IMPORTED_LOCATION)
  get_filename_component( _dir ${uic_location} DIRECTORY)
  set(MACOS_DEPLOYQT "${_dir}/macdeployqt")
  if(NOT EXISTS ${MACOS_DEPLOYQT})
    message(FATAL_ERROR "Failed to locate macdeployqt executable: [${MACOS_DEPLOYQT}]")
  endif()

  set(CPACK_GENERATOR "DragNDrop")

  function(deployqt)
    set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME ZZRuntime)
    install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/SWIapp.html
	    DESTINATION .
	    RENAME Readme.html)
    install(FILES ${CMAKE_SOURCE_DIR}/man/macosx/License.html
	    DESTINATION .)

    install(CODE "set(deployqt \"${MACOS_DEPLOYQT}\")
                  set(fixup_script \"${CMAKE_SOURCE_DIR}/scripts/macosx_bundle_fixup.sh\")
                 ")
    install(CODE [===[
      execute_process(COMMAND ln -sf SWI-Prolog swipl-win
		      WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app/Contents/MacOS)
      message("Deploying Qt to ${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app")
      execute_process(COMMAND "${deployqt}"
		      "${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app")
      file(WRITE "${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app/Contents/swipl.home" "swipl\n")

      message("Adding dependencies for modules to bundle")
      execute_process(COMMAND ${fixup_script} "${CMAKE_INSTALL_PREFIX}/SWI-Prolog.app")
    ]===])
  endfunction()

  deployqt()
endif()

set(CMAKE_MACOSX_RPATH ON)

# Prefer sem_open() over deprecated sem_init()
set(USE_SEM_OPEN 1)
set(SO_PATH DYLD_LIBRARY_PATH)

endif(APPLE)
