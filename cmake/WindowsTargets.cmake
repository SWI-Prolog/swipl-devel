# Setup the additional targets for Windows: swipl-win.exe and plterm.dll

# build plterm.dll
add_subdirectory(win32/console)

# build swipl-win.exe
add_executable(swipl-win WIN32
	       pl-ntmain.c swipl.rc)
target_c_stack(swipl-win 4000000)
target_link_libraries(swipl-win libswipl libplterm ${CMAKE_THREAD_LIBS_INIT})
target_include_directories(swipl-win BEFORE PRIVATE
			   ${CMAKE_CURRENT_BINARY_DIR}
			   ${CMAKE_CURRENT_SOURCE_DIR})
