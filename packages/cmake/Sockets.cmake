include(CheckTypeSize)

if(WIN32)
  set(SOCKET_LIBRARIES ws2_32.lib gdi32.lib)
  set(HAVE_SOCKET 1)
else()
  check_library_exists(socket    socket      "" HAVE_LIBSOCKET)
  check_library_exists(nsl       gethostent  "" HAVE_LIBNSL)

  if(HAVE_LIBSOCKET)
    set(SOCKET_LIBRARIES ${SOCKET_LIBRARIES} -lsocket)
  endif()
  if(HAVE_LIBNSL)
    set(SOCKET_LIBRARIES ${SOCKET_LIBRARIES} -lnsl)
  endif()

  if(HAVE_LIBSOCKET)
    set(HAVE_SOCKET 1)
  else()
    check_function_exists(socket HAVE_SOCKET)
  endif()
endif()

if(HAVE_SOCKET)
  AC_CHECK_HEADERS(sys/socket.h)
  AC_CHECK_HEADERS(sys/un.h)
  if(HAVE_SYS_SOCKET_H)
    set(CMAKE_EXTRA_INCLUDE_FILES ${CMAKE_EXTRA_INCLUDE_FILES} sys/socket.h)
  endif()

  check_type_size(socklen_t SIZEOF_SOCKLEN_T)
  if(NOT SIZEOF_SOCKLEN_T STREQUAL "")
    set(HAVE_SOCKLEN_T 1)
  endif()

  check_c_source_compiles(
      "#include <sys/types.h>
       #include <sys/socket.h>
       #include <netdb.h>
       #include <netinet/in.h>

       int main() { int x = h_errno; return 0;}"
      HAVE_H_ERRNO)

  check_c_source_compiles(
    "#include <sys/types.h>
     #include <sys/socket.h>
     #include <netdb.h>
     #include <netinet/in.h>

     int main(void)
     { struct ip_mreqn mreqn;
       return 0;
     }"
     HAVE_IP_MREQN)
endif(HAVE_SOCKET)
