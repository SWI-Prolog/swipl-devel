# Create src/static_packages.h holding the names of the foreign
# libraries and their initialization function.

function(write_static_extensions file)
  set(decls)
  set(array "static_extension static_extensions[] = {\n")
  foreach(ext ${ARGN})
    set(decls "${decls}extern void install_${ext}(void);\n")
    set(array "${array}  { \"${ext}\", install_${ext} },\n")
  endforeach(ext)
  set(array "${array}  { NULL, NULL }\n};\n")

  set(content "${decls}\n${array}")

  if(EXISTS ${file})
    file(READ ${file} old)
    if(content EQUAL old)
      set(keep ON)
    endif()
  endif()

  if(NOT keep)
    file(WRITE ${file} "${content}")
  endif()
endfunction()
