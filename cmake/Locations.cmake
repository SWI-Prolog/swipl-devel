# Set the installation prefix from $SWIPL_INSTALL_PREFIX.  If this
# environment variable ends in /@builddir@, replace @builddir@ with
# the name of the binary build directory, so we can easily install
# multiple different versions in the same filesystem

function(set_install_prefix)
  set(prefix "$ENV{SWIPL_INSTALL_PREFIX}")

  if(prefix)
    if(prefix MATCHES "@builddir@")
      get_filename_component(bindir ${CMAKE_CURRENT_BINARY_DIR} NAME)
      string(REGEX REPLACE "\@builddir\@" ${bindir} install_prefix ${prefix})
    else()
      set(install_prefix ${prefix})
    endif()
    set(CMAKE_INSTALL_PREFIX ${install_prefix}
	CACHE STRING "Installation prefix" FORCE)
  endif()
endfunction()

# Set well-known locations. Should be merged as  they are now at various
# places.

set(SWIPL_ROOT ${CMAKE_SOURCE_DIR})
