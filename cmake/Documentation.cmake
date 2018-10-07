set(DOC_OPTIONS --quiet)
set(TEX_CLEAN_EXTENSIONS aux blg idx ilg ind log out toc)

# Conversion programs from various formats to LaTeX
set(RUNTEX     ${SWIPL_ROOT}/man/runtex ${DOC_OPTIONS})
set(LATEX2HTML ${SWIPL_BUILD_LIBRARY}/latex2html/latex2html.pl -- ${DOC_OPTIONS})
set(DOC2TEX    ${SWIPL_ROOT}/man/doc2tex)
set(PLDOC2TEX  ${SWIPL_ROOT}/man/pldoc2tex.pl --)
set(TXTTOTEX   ${SWIPL_ROOT}/packages/txttotex.pl --)

# tex_byproducts(base var)
# Fill var with a list of all LaTeX byproducts when emitting ${base}

function(tex_byproducts doc out)
  set(list)
  foreach(e ${TEX_CLEAN_EXTENSIONS})
    set(list ${list} ${doc}.${e})
  endforeach()
  set(${out} ${list} PARENT_SCOPE)
endfunction()

