# Populate parms.h

set(C_CC     ${CMAKE_C_COMPILER})
# Hack.  How to get what is in `flags.make`?
set(C_CFLAGS ${CMAKE_C_FLAGS_RELWITHDEBINFO})
set(PLHOME   ${SWIPL_INSTALL_PREFIX})
set(PLARCH   ${SWIPL_ARCH})
set(SO_EXT   ${CMAKE_SHARED_MODULE_SUFFIX})
string(REGEX REPLACE "\\." "" SO_EXT "${SO_EXT}")
