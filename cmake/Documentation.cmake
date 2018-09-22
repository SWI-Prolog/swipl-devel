set(DOC_OPTIONS --quiet)
set(TEX_CLEAN_EXTENSIONS aux bbl blg idx ilg ind log out toc)

# tex_byproducts(base var)
# Fill var with a list of all LaTeX byproducts when emitting ${base}

function(tex_byproducts doc out)
  set(list)
  foreach(e ${TEX_CLEAN_EXTENSIONS})
    set(list ${list} ${doc}.${e})
  endforeach()
  set(${out} ${list} PARENT_SCOPE)
endfunction()

