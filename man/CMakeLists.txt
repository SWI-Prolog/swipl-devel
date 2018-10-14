cmake_minimum_required(VERSION 2.8.12)
project(swipl-doc-core)

include(Documentation)

set(DOC SWI-Prolog-${SWIPL_VERSION_STRING})

# Libraries documented in .doc files
set(DOC_LIB assoc.tex broadcast.tex clpb.tex clpfd.tex clpqr.tex gensym.tex
    library.tex libsummary.tex nbset.tex pio.tex predicateoptions.tex
    prologxref.tex readutil.tex record.tex registry.tex simplex.tex
    ugraphs.tex wwwbrowser.tex)
prepend(DOC_LIB lib/ ${DOC_LIB})

set(FIGS broadcast by-sa profnode swipl xrefchatdep xrefchatfile)

# Main files
set(DOC_CORE intro.tex overview.tex builtin.tex module.tex
    foreign.tex runtime.tex hack.tex summary.tex xpce.tex glossary.tex
    ide.tex license.tex threads.tex engines.tex profile.tex
    attvar.tex chr.tex xref.tex bit64.tex dialect.tex extensions.tex
    tabling.tex)

set(STYFILES html.sty txt.sty)

add_custom_command(
    OUTPUT ${PLDOC2TEX_STATE}
    COMMAND swipl -o ${PLDOC2TEX_STATE} --class=development
	    -c ${CMAKE_CURRENT_SOURCE_DIR}/pldoc2tex.pl
    DEPENDS pldoc2tex.pl swipl pldoc clib sgml prolog_products)

add_custom_target(
    pldoc2tex_state
    DEPENDS ${PLDOC2TEX_STATE})

function(copy_file file)
  add_custom_command(
      OUTPUT ${file}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
              ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${file})
  set(cpfiles ${cpfiles} ${file} PARENT_SCOPE)
endfunction()

function(copy_figures)
  foreach(f ${ARGN})
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/figs/${f}.pdf)
    copy_file(figs/${f}.pdf)
  endif()
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/figs/${f}.png)
    copy_file(figs/${f}.png)
  endif()
  endforeach()
  set(cpfiles ${cpfiles} ${file} PARENT_SCOPE)
endfunction()

function(doc2tex)
  foreach(f ${ARGN})
    string(REGEX REPLACE "\\.tex" ".doc" docin ${f})
    get_filename_component(dir ${f} DIRECTORY)
    if(dir)
      add_custom_command(
	  OUTPUT ${f}
	  COMMAND ${CMAKE_COMMAND} -E make_directory ${dir}
	  COMMAND ${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/${docin} > ${f}
	  DEPENDS ${docin})
    else()
      add_custom_command(
	  OUTPUT ${f}
	  COMMAND ${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/${docin} > ${f}
	  DEPENDS ${docin})
    endif()
  endforeach()
endfunction()

doc2tex(${DOC_CORE})

# pldoc(tex input
#	[OPTIONS ...]
#	[DEPENDS ...]

set(PLDOC_LIB)

function(pldoc tex input)
    set(options
	--source=${CMAKE_CURRENT_SOURCE_DIR}
	--summaries
        --outdir=lib)
    set(depends ${PLDOC2TEX_DEPENDS})
    set(byproducts)

    set(PLDOC_LIB ${PLDOC_LIB} ${tex} PARENT_SCOPE)

    if(input MATCHES "^library")
      string(REGEX REPLACE "library\\((.*)\\)" "\\1" base ${input})
      set(depends "../library/${base}.pl")
    endif()

    get_filename_component(base ${tex} NAME_WE)
    set(byproducts lib/summaries.d/${base}.tex)

    foreach(arg ${ARGN})
      if(arg MATCHES "^--")
        set(options ${options} ${arg})
      elseif(arg STREQUAL "DEPENDS")
      elseif(arg STREQUAL "OPTIONS")
      else()
        set(depends ${depends} ${arg})
      endif()
    endforeach()

    add_custom_command(
	OUTPUT  ${tex}
	COMMAND swipl ${PLDOC2TEX} ${options} "'${input}'"
	DEPENDS ${depends}
	BYPRODUCTS ${byproducts})
endfunction()

function(libdoc lib)
  string(REGEX REPLACE "_" "" texf ${lib})
  pldoc(lib/${texf}.tex "library(${lib})" ${ARGN})
  set(PLDOC_LIB ${PLDOC_LIB} PARENT_SCOPE)
endfunction()

function(libsdoc)
  foreach(lib ${ARGN})
    libdoc(${lib})
  endforeach()
  set(PLDOC_LIB ${PLDOC_LIB} PARENT_SCOPE)
endfunction()

# Specify how to generate tex files from PlDoc
# Note: all files must be in
libsdoc(apply dicts error url pairs option optparse ordsets aggregate
        thread_pool charsio debug csv lists main check random varnumbers
        quasi_quotations solution_sequences iostream persistency yall)
libdoc(shlib --subsection)
libdoc(pure_input --subsection)
libdoc(prolog_pack --section)
libdoc(help --subsection)
libdoc(explain --subsection)
pldoc(lib/assoclib.tex lib/assoclib.md
      OPTIONS --lib=assoc --module=assoc
      DEPENDS lib/assoclib.md ../library/assoc.pl)
pldoc(lib/clpblib.tex lib/clpblib.md
      OPTIONS --lib=clpb --module=clpb
      DEPENDS lib/clpblib.md ../library/clp/clpb.pl)
pldoc(lib/clpfdlib.tex lib/clpfdlib.md
      OPTIONS --lib=clpfd --module=clpfd
      DEPENDS lib/clpfdlib.md ../library/clp/clpfd.pl)
pldoc(lib/simplexlib.tex lib/simplexlib.md
      OPTIONS --lib=simplex --module=simplex
      DEPENDS lib/simplexlib.md ../library/clp/simplex.pl)
pldoc(lib/predopts.tex lib/predopts.txt
      OPTIONS --lib=predicate_options
      DEPENDS lib/predopts.txt ../library/predicate_options.pl)

# Check that we do not have both a .doc file and extract from the
# Prolog source
foreach(f ${PLDOC_LIB})
  string(REGEX REPLACE "\\.tex" ".doc" docin ${f})
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${docin})
    message("WARNING: DOC file for ${f}")
  endif()
endforeach()

doc2tex(${DOC_LIB})

# Create SWI-Prolog-<version>.tex

add_custom_command(
    OUTPUT  ${DOC}.doc
    COMMAND swipl ${CMAKE_CURRENT_SOURCE_DIR}/select.pl PDF
                  ${CMAKE_CURRENT_SOURCE_DIR}/main.doc > ${DOC}.doc
    DEPENDS main.doc select.pl)
add_custom_command(
    OUTPUT  ${DOC}.tex
    COMMAND ${DOC2TEX} ${DOC}.doc > ${DOC}.tex
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${DOC}.doc)

set(BIBTEX_DATA ${CMAKE_CURRENT_BINARY_DIR}/pl.bib)

add_custom_command(
    OUTPUT pl.bib
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
	    ${CMAKE_CURRENT_SOURCE_DIR}/pl.bib .
    DEPENDS pl.bib)

# Collect all input files

prepend(TEXFILES ${CMAKE_CURRENT_BINARY_DIR}/
	${DOC}.tex ${DOC_CORE} ${DOC_LIB} ${PLDOC_LIB})

# Collect LaTeX aux files
string(REPLACE ".tex" ".aux" AUX_FILES "${DOC_CORE}")
tex_byproducts(${DOC} TEX_BYPRODUCTS)
set(CLEAN_FILES ${AUX_FILES} ${TEX_BYPRODUCTS})
SET_DIRECTORY_PROPERTIES(PROPERTIES
			 ADDITIONAL_MAKE_CLEAN_FILES "${CLEAN_FILES}")

# Demand copied files (figures, etc)
set(cpfiles)
copy_figures(${FIGS})

if(BUILD_PDF_DOCUMENTATION)
add_custom_command(
    OUTPUT  ${DOC}.pdf ${DOC}.bbl
    COMMAND env TEXINPUTS=lib:$$TEXINPUTS ${RUNTEX} --pdf ${DOC}
    DEPENDS ${TEXFILES} ${BIBTEX_DATA} ${cpfiles} prolog_home
    COMMENT "Generating ${DOC}.pdf")
else()
add_custom_command(
    OUTPUT  ${DOC}.bbl
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_CURRENT_SOURCE_DIR}/gen/swipl.bbl ${DOC}.bbl
    COMMENT "Copying pre-build LaTeX .bbl file")
endif()

# Create the HTML manual
add_custom_command(
    OUTPUT manual.html
    COMMAND rm -f Manual/*.html Manual/*.gif
    COMMAND swipl ${LATEX2HTML} --texinputs=lib:: ${DOC}
    COMMAND touch manual.html
    COMMENT "Generating HTML documentation for core system"
    DEPENDS ${TEXFILES} ${CMAKE_CURRENT_BINARY_DIR}/${DOC}.bbl ${cpfiles}
	    prolog_products latex2html)

################
# Windows and MacOS platform guides

if(WIN32)
  doc2tex(windows.tex)
  add_custom_command(
      OUTPUT windows.html
      COMMAND ${LATEX2HTML} windows
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/windows.tex)

  add_custom_target(
      core.doc.windows.html
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/windows.html)
endif()

if(APPLE)
  add_custom_command(
      OUTPUT  macosx/macosx.tex
      COMMAND ${CMAKE_COMMAND} -E make_directory macosx
      COMMAND ${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/macosx/macosx.doc
			 > macosx/macosx.tex
      DEPENDS macosx/macosx.doc)

  add_custom_command(
      OUTPUT  macosx/macosx.html
      COMMAND ${LATEX2HTML} macosx
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/macosx/macosx.tex
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/macosx)

  add_custom_target(
      core.doc.macosx.html
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/macosx/macosx.html)
endif()

if(INSTALL_DOCUMENTATION)
  add_custom_target(
      core.doc.html
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/manual.html)
  add_dependencies(doc.html core.doc.html)

  if(BUILD_PDF_DOCUMENTATION)
    add_custom_target(
	core.doc.pdf
	DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${DOC}.pdf)
    add_dependencies(doc.pdf core.doc.pdf)
  endif()

  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Manual
	  DESTINATION ${SWIPL_INSTALL_PREFIX}/doc)
endif(INSTALL_DOCUMENTATION)
