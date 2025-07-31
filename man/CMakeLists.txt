cmake_minimum_required(VERSION 3.10)
project(swipl-doc-core)

include(Documentation)

set(DOC SWI-Prolog-${SWIPL_VERSION_STRING})

option(SWIPL_DOC_SERVER
       "Document files that are not part of the installed system"
       $ENV{SWIPL_DOC_SERVER})

set(SWIPL_COMMAND_OPTIONS
  -f none --no-packs "--home=${SWIPL_BUILD_HOME}" -DSDL_VIDEODRIVER=dummy)

# Libraries documented in .doc files
set(DOC_LIB assoc.tex broadcast.tex clpb.tex clpfd.tex clpqr.tex
    library.tex libsummary.tex nbset.tex pio.tex predicateoptions.tex
    record.tex registry.tex)
if(GMP_FOUND)
set(DOC_LIB ${DOC_LIB} simplex.tex)
endif()
prepend(DOC_LIB lib/ ${DOC_LIB})

set(FIGS broadcast by-sa profnode swipl xrefchatdep xrefchatfile)

# Main files
set(DOC_CORE abstract.tex intro.tex overview.tex builtin.tex module.tex
    foreign.tex wasm.tex runtime.tex hack.tex summary.tex xpce.tex glossary.tex
    ide.tex license.tex threads.tex engines.tex streams.tex
    attvar.tex chr.tex xref.tex bit64.tex dialect.tex extensions.tex ssu.tex
    tabling.tex packs.tex)
if(MULTI_THREADED)
list(APPEND DOC_CORE profile.tex)
endif()

set(STYFILES html.sty txt.sty)

# FIXME: we should reduce dependencies
add_custom_command(
    OUTPUT ${PLDOC2TEX_STATE}
    COMMAND ${PROG_SWIPL} -f none --no-packs -o ${PLDOC2TEX_STATE}
            --class=development --no-autoload --no-stand_alone
	    -c ${CMAKE_CURRENT_SOURCE_DIR}/pldoc2tex.pl
            -- --nopce
    DEPENDS pldoc2tex.pl swipl core prolog_home
            pldoc clib sgml http nlp utf8proc
    VERBATIM)

add_custom_target(
    pldoc2tex_state
    DEPENDS ${PLDOC2TEX_STATE})

function(copy_file file)
  add_custom_command(
      OUTPUT ${file}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
              ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${file}
      VERBATIM)
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
	  COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS}
			${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/${docin} ${f}
	  DEPENDS ${docin} ${DOC2TEX_DEPENDS}
	  VERBATIM)
    else()
      add_custom_command(
	  OUTPUT ${f}
	  COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS}
			${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/${docin} ${f}
	  DEPENDS ${docin} ${DOC2TEX_DEPENDS}
	  VERBATIM)
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
        --out=${tex})
    set(ploptions
        -f none --no-packs)
    set(depends ${PLDOC2TEX_DEPENDS})
    set(byproducts)

    set(PLDOC_LIB ${PLDOC_LIB} ${tex} PARENT_SCOPE)

    if(input MATCHES "^library")
      string(REGEX REPLACE "library\\((.*)\\)" "\\1" base ${input})
      set(depends "../library/${base}.pl")
    endif()

    foreach(arg ${ARGN})
      if(arg MATCHES "^--")
        set(options ${options} ${arg})
      elseif(arg STREQUAL "DEPENDS")
      elseif(arg STREQUAL "OPTIONS")
      else()
        set(depends ${depends} ${arg})
      endif()
    endforeach()

    if(tex MATCHES "^lib/")
      set(haspl)
      foreach(dep ${depends})
	get_filename_component(ext ${dep} EXT)
	if(ext STREQUAL ".pl")
	  set(haspl true)
	endif()
      endforeach()

      if(haspl)
	set(options ${options} --summaries)
	get_filename_component(base ${tex} NAME_WE)
	set(byproducts lib/summaries.d/${base}.tex)
      endif()
    endif()

    add_custom_command(
	OUTPUT  ${tex}
	COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS} ${ploptions}
			${PLDOC2TEX} ${options} "${input}"
	DEPENDS ${depends}
	BYPRODUCTS ${byproducts}
	VERBATIM)
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

has_package(ssl HAVE_SSL_PACKAGE)
has_package(archive HAVE_ARCHIVE_PACKAGE)
if(HAVE_SSL_PACKAGE)
  set(doc_depends_ssl ${doc_depends_ssl} ssl)
endif()
if(HAVE_ARCHIVE_PACKAGE)
  set(doc_depends_ssl ${doc_depends_ssl} archive)
endif()

# Specify how to generate tex files from PlDoc
# NOTE: all files must be included from lib/library.doc and
# NOTE: lib/libsummary.doc

libsdoc(apply dicts error url pairs option optparse ordsets aggregate
        charsio debug csv lists check random varnumbers
        quasi_quotations solution_sequences iostream persistency yall
        settings occurs ansi_term readutil prolog_xref intercept
        prolog_jiti tables listing strings terms ugraphs portray_text
        increval prolog_debug prolog_trace rbtrees statistics heaps fastrw gensym
        www_browser macros prolog_versions prolog_coverage exceptions)
if(MULTI_THREADED)
  libsdoc(thread_pool thread rwlocks)
endif()
if(NOT STATIC_EXTENSIONS)
libdoc(shlib --subsection)
endif()
libdoc(pure_input --subsection)
libdoc(explain --subsection)
pldoc(lib/prologpack.tex "library(prolog_pack)"
      OPTIONS --section DEPENDS ${doc_depends_ssl})
pldoc(lib/assoclib.tex lib/assoclib.md
      OPTIONS --lib=assoc --module=assoc
      DEPENDS lib/assoclib.md ../library/assoc.pl)
pldoc(lib/clpblib.tex lib/clpblib.md
      OPTIONS --lib=clpb --module=clpb
      DEPENDS lib/clpblib.md ../library/clp/clpb.pl)
pldoc(textdebugger.tex textdebugger.md
      DEPENDS textdebugger.md)
pldoc(lib/clpfdlib.tex lib/clpfdlib.md
      OPTIONS --lib=clpfd --module=clpfd
      DEPENDS lib/clpfdlib.md ../library/clp/clpfd.pl)
if(GMP_FOUND)
pldoc(lib/simplexlib.tex lib/simplexlib.md
      OPTIONS --lib=simplex --module=simplex
      DEPENDS lib/simplexlib.md ../library/clp/simplex.pl)
endif()
pldoc(lib/predopts.tex lib/predopts.txt
      OPTIONS --lib=predicate_options
      DEPENDS lib/predopts.txt ../library/predicate_options.pl)
pldoc(lib/help.tex "library(help)"
      OPTIONS --subsection
      DEPENDS nlp)
has_package(xpce HAVE_XPCE)
if(HAVE_XPCE)
pldoc(lib/main.tex "library(main)"
      DEPENDS pl2xpce)
else()
pldoc(lib/main.tex "library(main)")
endif()
pldoc(lib/dcgbasics.tex "library(dcg/basics)")
pldoc(lib/dcghighorder.tex "library(dcg/high_order)")
if(MULTI_THREADED)
  pldoc(libprofile.tex "library(prolog_profile)" --subsection)
endif()

if(SWIPL_DOC_SERVER)
# Install documentation for files we do not serve
  pldoc(lib/wasmlib.tex "${CMAKE_SOURCE_DIR}/library/wasm.pl"
	OPTIONS "--file_synopsis=library(wasm)"
	DEPENDS ${CMAKE_SOURCE_DIR}/library/wasm.pl)
  pldoc(lib/dom.tex  "${CMAKE_SOURCE_DIR}/library/dom.pl"
	OPTIONS "--file_synopsis=library(dom)"
        DEPENDS ${CMAKE_SOURCE_DIR}/library/dom.pl)
  pldoc(lib/taudom.tex  "${CMAKE_SOURCE_DIR}/library/dialect/tau/dom.pl"
	OPTIONS "--file_synopsis=library(dialect/tau/dom)"
		"--label=taudom"
        DEPENDS ${CMAKE_SOURCE_DIR}/library/dialect/tau/dom.pl)
endif()

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
    COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS}
               ${CMAKE_CURRENT_SOURCE_DIR}/select.pl PDF
               ${CMAKE_CURRENT_SOURCE_DIR}/main.doc > ${DOC}.doc
    DEPENDS main.doc select.pl)
add_custom_command(
    OUTPUT  ${DOC}.tex
    COMMAND ${PROG_SWIPL}  ${SWIPL_COMMAND_OPTIONS}
               ${DOC2TEX} ${DOC}.doc ${DOC}.tex
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${DOC}.doc ${DOC2TEX_DEPENDS}
    VERBATIM)

set(BIBTEX_DATA ${CMAKE_CURRENT_BINARY_DIR}/pl.bib)

add_custom_command(
    OUTPUT pl.bib
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
	    ${CMAKE_CURRENT_SOURCE_DIR}/pl.bib .
    DEPENDS pl.bib
    VERBATIM)

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
    COMMAND env TEXINPUTS=lib:$ENV{TEXINPUTS} ${RUNTEX} --pdf ${DOC}
    DEPENDS ${TEXFILES} ${BIBTEX_DATA} ${cpfiles} prolog_home
    COMMENT "Generating ${DOC}.pdf"
    VERBATIM)
else()
add_custom_command(
    OUTPUT  ${DOC}.bbl
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_CURRENT_SOURCE_DIR}/gen/swipl.bbl ${DOC}.bbl
    COMMENT "Copying pre-build LaTeX .bbl file"
    VERBATIM)
endif()

# Create the HTML manual. First we clean  it to avoid spurious files. If
# we cannot do that, that is ok. Ideally we use `cmake -E`, but there is
# no command to remove with wildcarts.

if(CMAKE_HOST_UNIX)
  set(clean rm -f Manual/*.html Manual/*.gif)
elseif(WIN32)
  set(clean cmd /c "del /Q Manual\*.html Manual\*.gif")
else()
  set(clean ${CMAKE_COMMAND} -E true)
endif()

prepend(cpfiles ${CMAKE_CURRENT_BINARY_DIR}/ ${cpfiles})
add_custom_command(
    OUTPUT manual.html
    COMMAND ${clean}
    COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS}
		${LATEX2HTML} --texinputs=lib:: ${DOC}
    COMMAND ${CMAKE_COMMAND} -E remove ${MAN_INDEX}
    COMMAND ${CMAKE_COMMAND} -E touch manual.html
    COMMENT "Generating HTML documentation for core system"
    DEPENDS ${TEXFILES} ${CMAKE_CURRENT_BINARY_DIR}/${DOC}.bbl ${cpfiles}
	    core latex2html)

################
# Windows and MacOS platform guides

if(WIN32)
  doc2tex(windows.tex)
  add_custom_command(
      OUTPUT windows.html
      COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS} ${LATEX2HTML} windows
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/windows.tex
              core latex2html
      VERBATIM)

  add_custom_target(
      core.doc.windows.html
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/windows.html)
endif()

if(APPLE)
  add_custom_command(
      OUTPUT  macosx/macosx.tex
      COMMAND ${CMAKE_COMMAND} -E make_directory macosx
      COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS} ${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/macosx/macosx.doc
	                       macosx/macosx.tex
      DEPENDS macosx/macosx.doc ${DOC2TEX_DEPENDS}
      VERBATIM)

  add_custom_command(
      OUTPUT  macosx/macosx.html
      COMMAND ${PROG_SWIPL} ${SWIPL_COMMAND_OPTIONS} ${LATEX2HTML} macosx
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/macosx/macosx.tex
              core latex2html
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/macosx
      VERBATIM)

  add_custom_target(
      core.doc.macosx.html
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/macosx/macosx.html)

if(BUILD_MACOS_BUNDLE)
  add_dependencies(doc core.doc.macosx.html)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/macosx/macosx.html
	  DESTINATION ${SWIPL_INSTALL_RESOURCES})
endif()
endif(APPLE)

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
	  DESTINATION ${SWIPL_INSTALL_DOC})
endif(INSTALL_DOCUMENTATION)
