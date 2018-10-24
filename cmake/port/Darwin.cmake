if(APPLE)

message("-- Using Macports packages from /opt/local and /usr/local")
set(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH}
    /opt/local/lib /usr/local/lib)
set(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}
    /opt/local/include /usr/local/include)

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
