# Installation of SWI-Prolog data files that are literally
# copied from the sources to their installation location.
#
# We want to use this to create a shadow data tree in the
# CMAKE_BINARY_DIRECTORY such that we can run the full system
# without installing it

function(install_src)
  install(${ARGN})
endfunction()
