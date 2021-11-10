# prepend(out prefix ...)
# assign ${out} with all arguments from ... after prepending prefix to
# it.

FUNCTION(PREPEND var prefix)
  SET(listVar "")
  FOREACH(f ${ARGN})
    LIST(APPEND listVar "${prefix}/${f}")
  ENDFOREACH(f)
  SET(${var} "${listVar}" PARENT_SCOPE)
ENDFUNCTION(PREPEND)

# join_list(out set element ...)
#
# Modern cmake has list(JOIN ...), but older do not.
function(join_list out sep)
  set(str)
  foreach(s ${ARGN})
    if(NOT str)
      set(str "${s}")
    else()
      set(str "${str}${sep}${s}")
    endif()
  endforeach()
  set(${out} ${str} PARENT_SCOPE)
endfunction()

# ilink(from to)
# Install ${from} in ${to} using a relative symbolic link

FUNCTION(ILINK from to)
  get_filename_component(LNTDIR ${to} DIRECTORY)
  get_filename_component(LNTNAME ${from} NAME)
  file(RELATIVE_PATH LNLNK ${LNTDIR} ${from})
  install(CODE "EXECUTE_PROCESS(COMMAND ln -sf ${LNLNK} ./${LNTNAME}
				WORKING_DIRECTORY \$ENV{DESTDIR}${LNTDIR})")
ENDFUNCTION(ILINK)

# Some autoconf utilities

function(AC_CHECK_HEADERS)
  foreach(f ${ARGN})
    string(REGEX REPLACE "[./]" "_" DEF ${f})
    string(TOUPPER "HAVE_${DEF}" DEF)
    check_include_file(${f} ${DEF})
    set(${DEF} ${${DEF}} PARENT_CONTEXT)
  endforeach()
endfunction()

function(AC_CHECK_FUNCS)
  foreach(f ${ARGN})
    string(TOUPPER "HAVE_${f}" DEF)
    check_function_exists(${f} ${DEF})
    set(${DEF} ${${DEF}} PARENT_CONTEXT)
  endforeach()
endfunction()

# Prolog parts

function(library_index dir)
  add_custom_command(
      OUTPUT ${dir}/INDEX.pl
      COMMAND ${PROG_SWIPL} -f none --no-packs -g "make_library_index('${dir}')" -t halt
      DEPENDS ${dir}
      VERBATIM)
endfunction()
