set(WIN32_CONSOLE_SOURCES complete.c console.c edit.c history.c menu.c)

add_library(libplterm SHARED
	    ${WIN32_CONSOLE_SOURCES})
set_target_properties(libplterm PROPERTIES
		      OUTPUT_NAME plterm PREFIX "")
target_compile_options(libplterm PRIVATE
		       -DUNICODE -D_UNICODE)

install(TARGETS libplterm
	RUNTIME DESTINATION ${SWIPL_INSTALL_ARCH_EXE})
