set(DOC_OPTIONS --quiet)
set(TEX_CLEAN_EXTENSIONS aux blg idx ilg ind log out toc)

# Conversion programs from various formats to LaTeX
set(PLDOC2TEX_STATE ${CMAKE_BINARY_DIR}/man/pldoc2tex)
set(RUNTEX          ${SWIPL_ROOT}/man/runtex ${DOC_OPTIONS})
set(LATEX2HTML      ${SWIPL_BUILD_LIBRARY}/latex2html/latex2html.pl -- ${DOC_OPTIONS})
set(DOC2TEX         ${SWIPL_ROOT}/man/doc2tex.pl)
set(DOC2TEX_DEPENDS core prolog_home ${SWIPL_ROOT}/man/doc2tex.pl)
set(PLDOC2TEX       -x ${CMAKE_BINARY_DIR}/man/pldoc2tex --)
set(PLDOC2TEX_DEPENDS pldoc2tex_state prolog_home core pldoc)
set(MAN_INDEX	    "${SWIPL_BUILD_HOME}/doc/manindex.db")

# tex_byproducts(base var)
# Fill var with a list of all LaTeX byproducts when emitting ${base}

function(tex_byproducts doc out)
  set(list)
  foreach(e ${TEX_CLEAN_EXTENSIONS})
    set(list ${list} ${doc}.${e})
  endforeach()
  set(${out} ${list} PARENT_SCOPE)
endfunction()

