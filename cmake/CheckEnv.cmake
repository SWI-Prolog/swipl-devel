function(conflicting_var name)
  message(WARNING
	  "-- $${name} is set to $ENV{${name}}\n"
	  "This may conflict with building SWI-Prolog as this may cause the "
	  "build process to use components from an older installed version "
	  "of SWI-Prolog.  Make sure to remove this environment from the "
	  "environment before building the system.")
endfunction()

if(CMAKE_EXECUTABLE_FORMAT STREQUAL "ELF" AND
   NOT x"$ENV{LD_PRELOAD}" STREQUAL x AND
   "$ENV{LD_PRELOAD}" MATCHES swipl)
  conflicting_var(LD_PRELOAD)
endif()

if(UNIX)
  if(APPLE)
    if(NOT x"$ENV{DYLD_LIBRARY_PATH}" STREQUAL x AND
       "$ENV{DYLD_LIBRARY_PATH}" MATCHES swipl)
      conflicting_var(DYLD_LIBRARY_PATH)
    endif()
  else()
    if(NOT x"$ENV{LD_LIBRARY_PATH}" STREQUAL x AND
       "$ENV{LD_LIBRARY_PATH}" MATCHES swipl)
      conflicting_var(DYLD_LIBRARY_PATH)
    endif()
  endif()
elseif(WIN32)
  if(NOT x"$ENV{PATH}" STREQUAL x AND
     "$ENV{PATH}" MATCHES swipl)
    conflicting_var(PATH)
  endif()
endif()

if(NOT x"$ENV{SWI_HOME_DIR}" STREQUAL x AND
   IS_DIRECTORY "$ENV{SWIPL}")
  conflicting_var(SWI_HOME_DIR)
endif()
if(NOT "$ENV{SWIPL}" STREQUAL x AND
   IS_DIRECTORY "$ENV{SWIPL}")
  conflicting_var(SWIPL)
endif()
