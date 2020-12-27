if(INSTALL_DOCUMENTATION OR WARN_NO_DOCUMENTATION)

# Verify all packages required to build the HTML documentation are
# present.

function(check_doc_dependencies)
  set(doc_depends pldoc ltx2htm archive)
  set(missing)

  foreach(pkg ${doc_depends})
    has_package(${pkg} has_pkg)
    if(NOT has_pkg)
      set(missing "${missing} ${pkg}")
      list(APPEND SWIPL_PACKAGE_LIST ${pkg})
    endif()
  endforeach()

  if(missing)
    set(INSTALL_DOCUMENTATION OFF CACHE BOOL "Install the HTML documentation files" FORCE)
    set(WARN_NO_DOCUMENTATION ON  CACHE BOOL "Re-check documentation" FORCE)
    message(
"WARNING: Dropped building the documentation because the following \
packages are missing: ${missing}")
  else()
    set(INSTALL_DOCUMENTATION ON  CACHE BOOL "Install the HTML documentation files" FORCE)
    set(WARN_NO_DOCUMENTATION OFF CACHE BOOL "Re-check documentation" FORCE)
  endif()
endfunction()

check_doc_dependencies()

endif()
