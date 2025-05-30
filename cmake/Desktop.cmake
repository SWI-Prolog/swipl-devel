# Install desktop files

has_package(xpce has_xpce)
has_package(swipl-win has_swipl_win)

if(has_xpce OR has_swipl_win)
  set(DESKTOP_SOURCE ${CMAKE_SOURCE_DIR}/desktop)
  set(SWIPL_INSTALL_DESKTOP ${SWIPL_INSTALL_PREFIX}/desktop)

  install_src(icon
    FILES
    ${DESKTOP_SOURCE}/swipl.png
    ${DESKTOP_SOURCE}/swipl-cli.png
    DESTINATION
    DESTINATION ${SWIPL_INSTALL_DESKTOP})

  if(UNIX AND NOT APPLE)
    message("Configuring freedesktop.org desktop files")
    if(has_swipl_win)
      configure_file(${DESKTOP_SOURCE}/swipl-win.desktop.in
	${SWIPL_BUILD_HOME}/desktop/swipl-win.desktop)
      install(FILES ${SWIPL_BUILD_HOME}/desktop/swipl-win.desktop
	DESTINATION ${SWIPL_INSTALL_PREFIX}/desktop)
    endif()
    if(has_xpce)
      configure_file(${DESKTOP_SOURCE}/swipl.desktop.in
	${SWIPL_BUILD_HOME}/desktop/swipl.desktop)
      install(FILES ${SWIPL_BUILD_HOME}/desktop/swipl.desktop
	DESTINATION ${SWIPL_INSTALL_PREFIX}/desktop)
    endif()
    install_src(mimetype
      FILES
      ${DESKTOP_SOURCE}/prolog-mime.xml
      DESTINATION
      DESTINATION ${SWIPL_INSTALL_DESKTOP})
  endif()
endif()
