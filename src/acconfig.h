/* Define if BSD compatible signals (i.e. no reset when fired) */
#undef BSD_SIGNALS

/* Define if mmap() can be used to allocate stacks */
#undef MMAP_STACK

/* Define if maximum address we can map at */
#undef MMAP_MAX_ADDRESS

/* Define if minimum address we can map and if > sbrk(0) */
#undef MMAP_MIN_ADDRESS

/* Define if MAP_ANON is defined and works ok */
#undef HAVE_MAP_ANON

/* Define if you can't use asm("nop") to separate two labels */
#undef NO_ASM_NOP

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

/* Define as 0 if it gives problems (shouldn't do anymore) */
#undef VMCODE_IS_ADDRESS

/* Define if first data symbol not is environ */
#undef FIRST_DATA_SYMBOL

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

/* Define how to invoke the linker for incremental linking (default: ld) */
#undef LD_COMMAND

/* Define to make runtime version */
#undef O_RUNTIME

/* Define if you don't have termio(s), but struct sgttyb */
#undef HAVE_SGTTYB

/* Define if <assert.h> requires <stdio.h> */
#undef ASSERT_H_REQUIRES_STDIO_H

/* Define if doubles cannot be aligned as longs */
#undef DOUBLE_ALIGNMENT

/* Define top of heap.  See pl-setup.c */
#undef TOPOFHEAP

/* Define max size of mmapp()ed stacks.  See test/mmap.c */
#undef MMAP_STACKSIZE

/* Define if the type rlim_t is defined by <sys/resource.h> */
#undef HAVE_RLIM_T

/* Define to 1 if &&label and goto *var is supported (GCC-2) */
#undef O_LABEL_ADDRESSES

/* Define to 1 not to use SIGSEGV for guarding stack-overflows */
#undef NO_SEGV_HANDLING 
