/* If your compiler supports an 8-byte integer, define this to the type */
#undef LONGLONG

/* Define if you have BSD signals (i.e. signal handler does *not* reset */
#undef BSD_SIGNALS

/* Define if you have the dl library (-ldl).  */
#undef HAVE_LIBDL

/* Define if you have the jpeg library (-ljpeg).  */
#undef HAVE_LIBJPEG

/* Define if you have the Xpm library (-lXpm).  */
#undef HAVE_LIBXPM

/* Define if you have the elf library (-lelf).  */
#undef HAVE_LIBELF

/* Define if you have the nsl library (-lnsl).  */
#undef HAVE_LIBNSL

/* Define if you have the readline library (-lreadline).  */
#undef HAVE_LIBREADLINE

/* Define if you have the socket library (-lsocket).  */
#undef HAVE_LIBSOCKET

/* Define if you have the termcap library (-ltermcap).  */
#undef HAVE_LIBTERMCAP

/* Define if wait() uses union wait* for the 2nd argument. */
#undef UNION_WAIT

/* Define if (type)pointer = value is allowed */
#undef TAGGED_LVALUE

/* Define if you want to include the C++ interface */
#undef O_CPLUSPLUS

/* Define if struct termios has a member c_line */
#undef TERMIOS_HAS_C_LINE

/* Define if data-pointer is in high memory */
#undef POINTER_OFFSET

/* Define if text-pointer is in high memory */
#undef TEXT_OFFSET

/* Define to "file.h" to include additional system prototypes */
#undef SYSLIB_H

/* Define if you want the runtime version */
#undef O_RUNTIME

/* Define if struct tm has tm_gmtoff field */
#undef HAVE_TM_GMTOFF

/* Define if you want the C++ interface compiled in */
#undef O_CPLUSPLUS

/* Define to include the password checking code */
#undef O_LICENCE

/* Define if position of data-segment isn't more or less constant */
#undef VARIABLE_POINTER_OFFSET

/* Define if iocll(tty, I_PUSH, "ttcompat") works on your machine */
#undef HAVE_TTCOMPAT

/* Define if sigaction() supports SA_SIGINFO and passes thechild pid in info */
#undef USE_SIGINFO

/* Define if your system is Unix-like, but doesn't know for itself */
#undef __unix__

/* Define to prepare for multi-threading */
#undef _REENTRANT
