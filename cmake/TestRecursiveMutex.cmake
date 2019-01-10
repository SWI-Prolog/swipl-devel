if(NOT DEFINED RECURSIVE_MUTEXES)

message("-- Checking for recursive mutex support")
check_c_source_compiles(
    "#include <pthread.h>\nint main() { int i = PTHREAD_MUTEX_RECURSIVE; return 0;}"
    HAVE_PTHREAD_MUTEX_RECURSIVE)
if(HAVE_PTHREAD_MUTEX_RECURSIVE)
  set(RECURSIVE_MUTEXES 1 CACHE INTERNAL "Supports recursive mutexes")
else()
  check_c_source_compiles(
      "#include <pthread.h>\nint main() { int i = PTHREAD_MUTEX_RECURSIVE_NP; return 0;}"
      HAVE_PTHREAD_MUTEX_RECURSIVE_NP)
  if(HAVE_PTHREAD_MUTEX_RECURSIVE_NP)
    set(RECURSIVE_MUTEXES 1 CACHE INTERNAL "Supports recursive mutexes")
  endif()
endif()

if(RECURSIVE_MUTEXES)
  message("-- Checking for recursive mutex support -- found")
else()
  message("-- Checking for recursive mutex support -- not found")
endif()

endif(NOT DEFINED RECURSIVE_MUTEXES)
