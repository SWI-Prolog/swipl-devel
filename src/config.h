/* config.h.  Generated automatically by configure.  */
/* config.h.in.  Generated automatically from configure.in by autoheader.  */

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#define HAVE_SYS_WAIT_H 1

/* Define if you have <vfork.h>.  */
#define HAVE_VFORK_H 1

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
/* #undef STDC_HEADERS */

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#define TIME_WITH_SYS_TIME 1

/* Define vfork as fork if vfork does not work.  */
/* #undef vfork */

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
#define WORDS_BIGENDIAN 1

/* Define if BSD compatible signals (i.e. no reset when fired) */
#define BSD_SIGNALS 1

/* Define if malloc()'ed data is in 0x10000000L .. 0x20000000L */
/* #undef DATA_AT_0X1 */

/* Define if malloc()'ed data is in 0x20000000L .. 0x40000000L */
/* #undef DATA_AT_0X2 */

/* Define if malloc()'ed data is in 0x40000000L .. 0x80000000L */
/* #undef DATA_AT_0X4 */

/* Define if malloc()'ed data is in 0x80000000L .. */
/* #undef DATA_AT_0X8 */

/* Define if mmap() can be used to allocate stacks */
#define MMAP_STACK 1

/* Define if maximum address we can map at */
#define MMAP_MAX_ADDRESS 0x20000000L

/* Define if minimum address we can map and if > sbrk(0) */
/* #undef MMAP_MIN_ADDRESS */

/* Define if MAP_ANON is defined and works ok */
/* #undef HAVE_MAP_ANON */

/* Define if you can't use asm("nop") to separate two labels */
/* #undef NO_ASM_NOP */

/* Define if uchar is not defined in <sys/types.h> */
#define NEED_UCHAR 1

/* Define if SIGPROF and setitimer() are available */
#define O_PROFILE 1

/* Define if signal handler is of the form f(sig, type, context, addr) */
#define SIGNAL_HANDLER_PROVIDES_ADDRESS 1

/* Define if (type)var = value is allowed */
#define TAGGED_LVALUE 1

/* Define as 0 if text addresses start above 40K */
/* #undef VMCODE_IS_ADDRESS */

/* Define if first data symbol not is environ */
/* #undef FIRST_DATA_SYMBOL */

/* Define if pl-save.c works */
#define O_SAVE 1

/* Define if symbolic links are supported by the OS */
#define HAVE_SYMLINKS 1

/* Define if AIX foreign language interface is to be used */
/* #undef O_AIX_FOREIGN */

/* Define if MACH foreign language interface is to be used */
/* #undef O_MACH_FOREIGN */

/* Define if BSD Unix ld -A foreign language interface is to be used */
/* #undef O_FOREIGN */

/* Define if ld accepts -A option */
/* #undef HAVE_LD_A */

/* Define if /dev/null is named differently */
/* #undef DEVNULL */

/* Define if wait() uses union wait */
/* #undef UNION_WAIT */

/* Define if <sys/ioctl> should *not* be included after <sys/termios.h> */
#define NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H 1

/* Define if, in addition to <errno.h>, extern int errno; is needed */
/* #undef NEED_DECL_ERRNO */

/* Define to "file.h" to include additional system prototypes */
/* #undef SYSLIB_H */

/* Define how to invoke the linker for incremental linking (default: ld) */
/* #undef LD_COMMAND */

/* Define to make runtime version */
/* #undef O_RUNTIME */

/* Define if you have the access function.  */
#define HAVE_ACCESS 1

/* Define if you have the chmod function.  */
#define HAVE_CHMOD 1

/* Define if you have the dlopen function.  */
#define HAVE_DLOPEN 1

/* Define if you have the dossleep function.  */
/* #undef HAVE_DOSSLEEP */

/* Define if you have the fstat function.  */
#define HAVE_FSTAT 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the getdtablesize function.  */
#define HAVE_GETDTABLESIZE 1

/* Define if you have the getpagesize function.  */
#define HAVE_GETPAGESIZE 1

/* Define if you have the getpwnam function.  */
#define HAVE_GETPWNAM 1

/* Define if you have the getrlimit function.  */
#define HAVE_GETRLIMIT 1

/* Define if you have the gettimeofday function.  */
#define HAVE_GETTIMEOFDAY 1

/* Define if you have the memmove function.  */
/* #undef HAVE_MEMMOVE */

/* Define if you have the opendir function.  */
#define HAVE_OPENDIR 1

/* Define if you have the popen function.  */
#define HAVE_POPEN 1

/* Define if you have the putenv function.  */
#define HAVE_PUTENV 1

/* Define if you have the random function.  */
#define HAVE_RANDOM 1

/* Define if you have the readlink function.  */
#define HAVE_READLINK 1

/* Define if you have the remove function.  */
#define HAVE_REMOVE 1

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

/* Define if you have the rl_insert_close function.  */
#define HAVE_RL_INSERT_CLOSE 1

/* Define if you have the select function.  */
#define HAVE_SELECT 1

/* Define if you have the shl_load function.  */
/* #undef HAVE_SHL_LOAD */

/* Define if you have the signal function.  */
#define HAVE_SIGNAL 1

/* Define if you have the sleep function.  */
#define HAVE_SLEEP 1

/* Define if you have the srand function.  */
#define HAVE_SRAND 1

/* Define if you have the srandom function.  */
#define HAVE_SRANDOM 1

/* Define if you have the stat function.  */
#define HAVE_STAT 1

/* Define if you have the strerror function.  */
/* #undef HAVE_STRERROR */

/* Define if you have the stricmp function.  */
#define HAVE_STRICMP 1

/* Define if you have the tgetent function.  */
#define HAVE_TGETENT 1

/* Define if you have the times function.  */
#define HAVE_TIMES 1

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <malloc.h> header file.  */
#define HAVE_MALLOC_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
/* #undef HAVE_NDIR_H */

/* Define if you have the <pwd.h> header file.  */
#define HAVE_PWD_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/dir.h> header file.  */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/file.h> header file.  */
#define HAVE_SYS_FILE_H 1

/* Define if you have the <sys/ndir.h> header file.  */
/* #undef HAVE_SYS_NDIR_H */

/* Define if you have the <sys/param.h> header file.  */
#define HAVE_SYS_PARAM_H 1

/* Define if you have the <sys/resource.h> header file.  */
#define HAVE_SYS_RESOURCE_H 1

/* Define if you have the <sys/select.h> header file.  */
/* #undef HAVE_SYS_SELECT_H */

/* Define if you have the <sys/stat.h> header file.  */
#define HAVE_SYS_STAT_H 1

/* Define if you have the <sys/termio.h> header file.  */
#define HAVE_SYS_TERMIO_H 1

/* Define if you have the <sys/termios.h> header file.  */
#define HAVE_SYS_TERMIOS_H 1

/* Define if you have the <sys/time.h> header file.  */
#define HAVE_SYS_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
#define HAVE_UNISTD_H 1

/* Define if you have the dl library (-ldl).  */
#define HAVE_LIBDL 1

/* Define if you have the dld library (-ldld).  */
/* #undef HAVE_LIBDLD */

/* Define if you have the elf library (-lelf).  */
/* #undef HAVE_LIBELF */

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define if you have the readline library (-lreadline).  */
#define HAVE_LIBREADLINE 1

/* Define if you have the termcap library (-ltermcap).  */
#define HAVE_LIBTERMCAP 1

/* Define if you have the ucb library (-lucb).  */
/* #undef HAVE_LIBUCB */
