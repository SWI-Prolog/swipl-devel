function(conflicting_var name)
  message(WARNING
	  "-- Environment variable ${name} is set to $ENV{${name}}\n"
	  "This suggests that the variable is referring to a existing installation of SWI-Prolog. "
	  "This may conflict with building SWI-Prolog as this may cause the "
	  "build process to use components from such (older) installed version "
	  "of SWI-Prolog.  Make sure to remove this environment from the "
	  "environment before building the system.")
endfunction()

if(CMAKE_EXECUTABLE_FORMAT STREQUAL "ELF" AND
   DEFINED ENV{LD_PRELOAD} AND
   "$ENV{LD_PRELOAD}" MATCHES swipl|swi-prolog)
  conflicting_var(LD_PRELOAD)
endif()

if(UNIX)
  if(APPLE)
    if(DEFINED ENV{DYLD_LIBRARY_PATH} AND
       "$ENV{DYLD_LIBRARY_PATH}" MATCHES swipl|swi-prolog)
      conflicting_var(DYLD_LIBRARY_PATH)
    endif()
  else()
    if(DEFINED ENV{LD_LIBRARY_PATH} AND
       "$ENV{LD_LIBRARY_PATH}" MATCHES swipl|swi-prolog)
      conflicting_var(LD_LIBRARY_PATH)
    endif()
  endif()
elseif(WIN32)
  if(DEFINED ENV{PATH} AND
     "$ENV{PATH}" MATCHES swipl|swi-prolog)
    conflicting_var(PATH)
  endif()
endif()

if(DEFINED ENV{SWI_HOME_DIR} AND
   IS_DIRECTORY "$ENV{SWI_HOME_DIR}")
  conflicting_var(SWI_HOME_DIR)
endif()
if(DEFINED ENV{SWIPL} AND
   IS_DIRECTORY "$ENV{SWIPL}")
  conflicting_var(SWIPL)
endif()
