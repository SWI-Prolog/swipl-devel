# Configuration under Conda

message("Building under Conda for Windows")

if(MSVC)
  add_library(Threads::Threads INTERFACE IMPORTED)
  set_property(TARGET Threads::Threads PROPERTY
	       INTERFACE_LINK_LIBRARIES pthreads.lib)
  set(CMAKE_USE_PTHREADS_INIT 1)
endif()

set(PLHOME no-home)
set(PLRELHOME ../lib/${SWIPL_INSTALL_DIR})
