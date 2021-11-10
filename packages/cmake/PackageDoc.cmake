include(Documentation)

function(doc2tex file)
  string(REPLACE ".doc" ".tex" tex ${file})
  add_custom_command(
      OUTPUT ${tex}
      COMMAND ${PROG_SWIPL} -f none --no-packs ${DOC2TEX} ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${tex}
      DEPENDS ${file} ${DOC2TEX_DEPENDS}
      VERBATIM)
  set(texfiles ${texfiles} ${tex} PARENT_SCOPE)
endfunction()

function(txt2tex file)
  string(REPLACE ".txt" ".tex" tex ${file})
  add_custom_command(
      OUTPUT ${tex}
      COMMAND ${PROG_SWIPL} -f none --no-packs ${PLDOC2TEX} --outdir=. ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      DEPENDS ${PLDOC2TEX_DEPENDS} ${file}
      VERBATIM)
  set(texfiles ${texfiles} ${tex} PARENT_SCOPE)
endfunction()

function(copy_file file)
  add_custom_command(
      OUTPUT ${file}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
              ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${file}
      VERBATIM)
  set(cpfiles ${cpfiles} ${file} PARENT_SCOPE)
endfunction()


# pldoc file.pl [out.tex] [library(lib)]

function(pldoc file)
  set(tex)
  set(lib)
  set(options)

  foreach(arg ${ARGN})
    if(arg MATCHES ".*\\.tex")
      set(tex ${arg})
    elseif(arg MATCHES "library")
      set(lib "${arg}")
    elseif(arg MATCHES "^--")
      set(options ${options} ${arg})
    endif()
  endforeach()

  if(NOT tex)
    string(REGEX REPLACE "\\.(pl|md)" ".tex" tex ${file})
    string(REPLACE "_" "" tex ${tex})
  endif()

  if(NOT lib)
    if(file MATCHES "\\.md")
      set(lib ${CMAKE_CURRENT_SOURCE_DIR}/${file})
    else()
      get_filename_component(base ${file} NAME_WE)
      if(libsubdir)
	set(lib "library('${libsubdir}/${base}')")
      else()
	set(lib "library('${base}')")
      endif()
    endif()
  endif()

  get_filename_component(base ${file} NAME_WE)
  add_custom_command(
      OUTPUT ${tex}
      COMMAND ${PROG_SWIPL} -f none --no-packs ${PLDOC2TEX} --out=${tex} ${seclevel} ${options} ${lib}
      DEPENDS ${PLDOC2TEX_DEPENDS} ${file}
      VERBATIM)

  set(texfiles ${texfiles} ${tex} PARENT_SCOPE)
endfunction()

function(flush_src)
  if(src)
    pldoc(${src})
  endif()
  set(src "" PARENT_SCOPE)
  set(texfiles ${texfiles} PARENT_SCOPE)
endfunction()

# pkg_doc(pkg
#	  [ SOURCE file.pl [out.tex] [library(...)] ]*
#	  [ SOURCES file.pl file.doc ... ])

function(pkg_doc pkg)
  set(pldoc)
  set(docfiles)
  set(mode)
  set(texfiles)
  set(cpfiles)
  set(src)
  set(seclevel)
  set(libsubdir)
  set(bbl)
  set(depends)
  set(vimages)

  foreach(arg ${ARGN})
    if(arg STREQUAL "DEPENDS")
      set(mode m_depends)
    elseif(arg STREQUAL "SOURCES")
      flush_src()
      set(mode sources)
    elseif(arg STREQUAL "SOURCE")
      flush_src()
      set(mode source)
      set(src)
    elseif(arg STREQUAL "IMAGES")
      flush_src()
      set(mode images)
    elseif(arg STREQUAL "LIBSUBDIR")
      set(mode lbsubdir)
    elseif(arg STREQUAL "SECTION")
      flush_src()
      set(seclevel --section)
      set(mode sources)
    elseif(arg STREQUAL "SUBSECTION")
      flush_src()
      set(seclevel --subsection)
      set(mode sources)
    elseif(arg STREQUAL "SUBSUBSECTION")
      flush_src()
      set(seclevel --subsubsection)
      set(mode sources)
    elseif(mode STREQUAL "source")
      set(src ${src} ${arg})
    elseif(mode STREQUAL "lbsubdir")
      set(libsubdir ${arg})
      set(mode)
    elseif(mode STREQUAL "images")
      set(vimages ${vimages} ${arg})
    elseif(mode STREQUAL "m_depends")
      set(depends ${depends} ${arg})
    else()
      if(arg MATCHES "\\.(pl|md)")
        pldoc(${arg})
      elseif(arg MATCHES "\\.doc")
        doc2tex(${arg})
      elseif(arg MATCHES "\\.txt")
        txt2tex(${arg})
      elseif(arg MATCHES "\\.bib")
        set(bbl ${pkg}.bbl)
        copy_file(${arg})
      elseif(arg MATCHES "\\.(gif|png|pdf|eps)")
        copy_file(${arg})
      elseif(arg MATCHES "\\.tex")
        set(texfiles ${texfiles} ${arg})
      endif()
    endif()
  endforeach()
  flush_src()

  # Some packages rely on man/pl.bib rather than a local bibliography
  if(NOT bbl AND EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/gen/${pkg}.bbl)
    set(bbl ${pkg}.bbl)
  endif()

  doc2tex(${pkg}.doc)

  tex_byproducts(${pkg} byproducts)
  SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES
			   "${byproducts}")

  prepend(texdeps ${CMAKE_CURRENT_BINARY_DIR}/ ${pkg}.tex ${texfiles} ${cpfiles})

  if(NOT depends)
    set(depends ${pkg})
  endif()

  if(INSTALL_DOCUMENTATION)
    if(BUILD_PDF_DOCUMENTATION)
      add_custom_command(
	  OUTPUT ${pkg}.pdf ${bbl}
	  COMMAND ${RUNTEX} --pdf ${pkg}
	  DEPENDS ${texdeps} ${depends}
	  COMMENT "Generating ${pkg}.pdf")

      add_custom_target(
	  ${pkg}.doc.pdf
	  DEPENDS ${pkg}.pdf)
      add_dependencies(doc.pdf  ${pkg}.doc.pdf)
    elseif(bbl)
      add_custom_command(
	  OUTPUT ${bbl}
	  COMMAND ${CMAKE_COMMAND} -E copy_if_different
		  ${CMAKE_CURRENT_SOURCE_DIR}/gen/${bbl} ${bbl}
	  COMMENT "Copying pre-build LaTeX .bbl file")
    endif(BUILD_PDF_DOCUMENTATION)

    add_custom_command(
	OUTPUT ${pkg}.html
	COMMAND ${PROG_SWIPL} -f none --no-packs ${LATEX2HTML} ${pkg}
	COMMAND ${CMAKE_COMMAND} -E remove ${MAN_INDEX}
	DEPENDS latex2html core ${texdeps} ${bbl} ${depends})

    add_custom_target(
	${pkg}.doc.html
	DEPENDS ${pkg}.html)

    add_dependencies(doc.html ${pkg}.doc.html)

    set(CMAKE_INSTALL_DEFAULT_COMPONENT_NAME Documentation)
    prepend(doc_files ${CMAKE_CURRENT_BINARY_DIR}/ ${pkg}.html ${vimages})
    install(FILES ${doc_files}
	    DESTINATION ${SWIPL_INSTALL_DOC}/packages
	    COMPONENT Documentation
	    OPTIONAL)
  endif(INSTALL_DOCUMENTATION)
endfunction()
