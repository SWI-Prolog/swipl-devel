/* config.h.  Generated automatically by configure.  */
/* config.h.in.  Generated automatically from configure.in by autoheader.  */

#define __WIN32__ 1

#define O_ASYNC_HOOK 1

#define OPEN_MAX 32

#define HAVE_UXNT_H 1
#define HAVE_MALLOC_H 1
#define HAVE_VIRTUAL_ALLOC 1
#define AVOID_0X80000000_BIT 1
#define HAVE_CLOCK 1			/* clock() timing function */
#define inline __inline

/* Define if you want to associate states */
#define ASSOCIATE_STATE ".qlx"
#define ASSOCIATE_SRC	".pl"

/* Define for the _xos_... functions */
#define O_XOS 1

/* Define O_RLC for the ../readline library */
#define O_RLC 1

/* Define for Windows DDE support */
#define O_DDE 1

/* Define for Windows DLL support */
#define O_DLL 1

/* Define if you disk-drives are special to you (DOS, Windows, OS/2) */
#define O_HASDRIVES 1

/* Maximum length of a path-name.  Note XOS! */
#define MAXPATHLEN 512

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#undef HAVE_SYS_WAIT_H

/* Define if you have <vfork.h>.  */
#undef HAVE_VFORK_H

/* Define as __inline if that's what the C compiler calls it.  */
/* #undef inline */

/* Define to `int' if <sys/types.h> doesn't define.  */
/* #undef pid_t */

/* Define if you need to in order for stat and other things to work.  */
/* #undef _POSIX_SOURCE */

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
 */
#define STACK_DIRECTION -1

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#undef TIME_WITH_SYS_TIME

/* Define vfork as fork if vfork does not work.  */
/* #undef vfork */

/* Define if BSD compatible signals (i.e. no reset when fired) */
/* #undef BSD_SIGNALS */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* Define if malloc()'ed data is in 0x10000000L .. 0x20000000L */
/* #undef DATA_AT_0X1 */

/* Define if malloc()'ed data is in 0x20000000L .. 0x40000000L */
/* #undef DATA_AT_0X2 */

/* Define if malloc()'ed data is in 0x40000000L .. 0x80000000L */
/* #undef DATA_AT_0X4 */

/* Define if malloc()'ed data is in 0x80000000L .. */
/* #undef DATA_AT_0X8 */

/* Define if mmap() can be used to allocate stacks */
#undef MMAP_STACK

/* Define if maximum address we can map at */
#undef MMAP_MAX_ADDRESS

/* Define if minimum address we can map at if > sbrk(0) */
/* #undef MMAP_MIN_ADDRESS */

/* Define if uchar is not defined in <sys/types.h> */
#define NEED_UCHAR 1

/* Define if SIGPROF and setitimer() are available */
#undef O_PROFILE

/* Define if signal handler is of the form f(sig, type, context, addr) */
/* #undef SIGNAL_HANDLER_PROVIDES_ADDRESS */

/* Define if (type)var = value is allowed */
#undef TAGGED_LVALUE

/* Define as 0 if text addresses start above 40K */
/* #undef VMCODE_IS_ADDRESS */

/* Define if first data symbol not is environ */
/* #undef FIRST_DATA_SYMBOL */

/* Define if pl-save.c works */
#undef O_SAVE

/* Define how to reset stdin after a restore */
#undef RESET_STDIN

/* Define if symbolic links are supported by the OS */
#undef HAVE_SYMLINKS

/* Define if AIX foreign language interface is to be used */
/* #undef O_AIX_FOREIGN */

/* Define if MACH foreign language interface is to be used */
/* #undef O_MACH_FOREIGN */

/* Define if BSD Unix ld -A foreign language interface is to be used */
#undef O_FOREIGN

/* Define if ld accepts -A option */
#undef HAVE_LD_A

/* Define if /dev/null is named differently */
/* #undef DEVNULL */

/* Define if wait() uses union wait */
/* #undef UNION_WAIT */

/* Define if <sys/ioctl> should *not* be included after <sys/termios.h> */
/* #undef NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H */

/* Define if, in addition to <errno.h>, extern int errno; is needed */
/* #undef NEED_DECL_ERRNO */

/* Define to "file.h" to include additional system prototypes */
/* #undef SYSLIB_H */

/* Define if you have the access function.  */
#define HAVE_ACCESS 1

/* Define if you have the chmod function.  */
#define HAVE_CHMOD 1

/* Define if you have the dlopen function.  */
/* #undef HAVE_DLOPEN */

/* Define if you have the dossleep function.  */
#undef HAVE_DOSSLEEP

/* Define if you have the delay function.  */
#undef HAVE_DELAY

/* Define if you have the fstat function.  */
#define HAVE_FSTAT 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getdtablesize function.  */
#undef HAVE_GETDTABLESIZE

/* Define if you have the getpagesize function.  */
#undef HAVE_GETPAGESIZE

/* Define if you have the getpwnam function.  */
#undef HAVE_GETPWNAM 

/* Define if you have the getrlimit function.  */
#undef HAVE_GETRLIMIT

/* Define if you have the gettimeofday function.  */
#undef HAVE_GETTIMEOFDAY

/* Define if you have the getw function.  */
#undef HAVE_GETW

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the opendir function.  */
#define HAVE_OPENDIR 1

/* Define if you have the popen function.  */
#undef HAVE_POPEN

/* Define if you have the putenv function.  */
#define HAVE_PUTENV 1

/* Define if you have the random function.  */
#undef HAVE_RANDOM

/* Define if you have the readlink function.  */
#undef HAVE_READLINK

/* Define if you have the remove function.  */
#define HAVE_REMOVE 1

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

/* Define if you have the stricmp() function. */
#define HAVE_STRICMP 1

/* Define if you have the strlwr() function */
#define HAVE_STRLWR 1

/* Define if you have the rl_insert_close function.  */
#define HAVE_RL_INSERT_CLOSE 1

/* Define if you have the select function.  */
#undef HAVE_SELECT

/* Define if you have the signal function.  */
#define HAVE_SIGNAL 1

/* Define if you have the sleep function.  */
#undef HAVE_SLEEP

/* Define if you have the srand function.  */
#define HAVE_SRAND 1

/* Define if you have the srandom function.  */
#undef HAVE_SRANDOM

/* Define if you have the stat function.  */
#define HAVE_STAT 1

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

#define HAVE_CEIL  1
#define HAVE_FLOOR 1

/* Define if you have the tgetent function.  */
#undef HAVE_TGETENT

/* Define if you have the times function.  */
#undef HAVE_TIMES

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <malloc.h> header file.  */
#define HAVE_MALLOC_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <pwd.h> header file.  */
#undef HAVE_PWD_H 

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/file.h> header file.  */
#undef HAVE_SYS_FILE_H

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/param.h> header file.  */
#undef HAVE_SYS_PARAM_H

/* Define if you have the <sys/resource.h> header file.  */
#undef HAVE_SYS_RESOURCE_H 

/* Define if you have the <sys/select.h> header file.  */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/stat.h> header file.  */
#define HAVE_SYS_STAT_H 1

/* Define if you have the <sys/termios.h> header file.  */
#undef HAVE_SYS_TERMIOS_H

/* Define if you have the <sys/time.h> header file.  */
#undef HAVE_SYS_TIME_H

/* Define if you have the <unistd.h> header file.  */
#undef HAVE_UNISTD_H

/* Define if you have the dl library (-ldl).  */
/* #undef HAVE_LIBDL */

/* Define if you have the elf library (-lelf).  */
/* #undef HAVE_LIBELF */

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define if you have the readline library (-lreadline).  */
#define HAVE_LIBREADLINE

/* Define if you have the termcap library (-ltermcap).  */
#undef HAVE_LIBTERMCAP

/* Define if you have the ucb library (-lucb).  */
/* #undef HAVE_LIBUCB */

