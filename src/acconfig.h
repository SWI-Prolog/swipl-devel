/* Define if BSD compatible signals (i.e. no reset when fired) */
#undef BSD_SIGNALS

/* Define if malloc()'ed data is in 0x10000000L .. 0x20000000L */
#undef DATA_AT_0X1

/* Define if malloc()'ed data is in 0x20000000L .. 0x40000000L */
#undef DATA_AT_0X2

/* Define if malloc()'ed data is in 0x40000000L .. 0x80000000L */
#undef DATA_AT_0X4

/* Define if malloc()'ed data is in 0x80000000L .. */
#undef DATA_AT_0X8

/* Define if mmap() can be used to allocate stacks */
#undef MMAP_STACK

/* Define if maximum address we can map at */
#undef MMAP_MAX_ADDRESS

/* Define if minimum address we can map at if > sbrk(0) */
#undef MMAP_MIN_ADDRESS

/* Define if ulong is not defined in <sys/types.h> */
#undef NEED_ULONG

/* Define if ushort is not defined in <sys/types.h> */
#undef NEED_USHORT

/* Define if uchar is not defined in <sys/types.h> */
#undef NEED_UCHAR

/* Define if SIGPROF and setitimer() are available */
#undef O_PROFILE

/* Define if signal handler is of the form f(sig, type, context, addr) */
#undef SIGNAL_HANDLER_PROVIDES_ADDRESS

/* Define if (type)var = value is allowed */
#undef TAGGED_LVALUE

/* Define as 0 if text addresses start above 40K */
#undef VMCODE_IS_ADDRESS

/* Define if first data symbol not is environ */
#undef FIRST_DATA_SYMBOL

/* Define if pl-save.c works */
#undef O_SAVE

/* Define how to reset stdin after a restore */
#undef RESET_STDIN

/* Define if symbolic links are supported by the OS */
#undef HAVE_SYMLINKS

/* Define if AIX foreign language interface is to be used */
#undef O_AIX_FOREIGN

/* Define if MACH foreign language interface is to be used */
#undef O_MACH_FOREIGN

/* Define if BSD Unix ld -A foreign language interface is to be used */
#undef O_FOREIGN

/* Define if ld accepts -A option */
#undef HAVE_LD_A

/* Define if /dev/null is named differently */
#undef DEVNULL

/* Define if wait() uses union wait */
#undef UNION_WAIT

/* Define if <sys/ioctl> should *not* be included after <sys/termios.h> */
#undef NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H

/* Define if -lreadline is present */
#undef HAVE_LIBREADLINE

/* Define if, in addition to <errno.h>, extern int errno; is needed */
#undef NEED_DECL_ERRNO

/* Define to "file.h" to include additional system prototypes */
#undef SYSLIB_H

/* Define if running SunOs 5.x (solaris 2.x) */
#undef __solaris__

/* Define how to invoke the linker for incremental linking (default: ld) */
#undef LD_COMMAND
