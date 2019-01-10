# Build check_installation.pl in the home directory
# it is calles by InstallSource.cmake
configure_file(../../library/check_installation.pl.in
               ${SWIPL_BUILD_LIBRARY}/check_installation.pl
               @ONLY)
