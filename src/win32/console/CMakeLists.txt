set(WIN32_CONSOLE_SOURCES complete.c console.c edit.c history.c menu.c)

add_library(libplterm SHARED
	    ${WIN32_CONSOLE_SOURCES})
set_target_properties(libplterm PROPERTIES
		      OUTPUT_NAME plterm)
target_compile_options(libplterm PRIVATE
		       -DUNICODE -D_UNICODE)
