# Fetch and make available external dependencies

# cmake -DUSE_CRMATH=ON includes INRIAs correct rounding math library

if(USE_CRMATH)
  if(NOT TARGET crmath)  # already built?
    set(CRMATH_REPO "https://github.com/indigobio/core-math-binary64.git")
    # Pin to an exact commit for security and reproducibility.  This
    # also allows FetchContent to skip contacting the remote when the
    # sources are already populated, so re-configuring works offline.
    set(CRMATH_TAG  "1749d29131eebee7b928f71a9388168f212e4ed4")

    include(FetchContent)
    # EXCLUDE_FROM_ALL (CMake 3.28, which crmath requires anyway)
    # keeps crmath out of the ALL target and skips its install()
    # rules, so it is built only as dependency of libswipl and not
    # installed.
    FetchContent_Declare(
      crmath_repo
      GIT_REPOSITORY "${CRMATH_REPO}"
      GIT_TAG        "${CRMATH_TAG}"
      EXCLUDE_FROM_ALL
    )
    FetchContent_MakeAvailable(crmath_repo)
  endif()  # NOT TARGET crmath
  set(HAVE_CRMATH 1)
endif() # USE_CRMATH
