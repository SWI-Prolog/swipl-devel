/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Generic machine description file
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			COMPILER FLAGS

Compiler  flags  for  making  `xmakefile'  from  `Makefile'  using  cpp.
Default is gcc, which is preferred on machines that have it.  SWI-Prolog
needs to be loaded statically  when   using  O_SAVE  or O_STORE_PROGRAM.
Hence the -static.  If gcc complains  it   does  not  know -static, this
probably implies the operating system has   no  dynamic libraries so you
may safely drop -static then.

The -funsigned-char option is needed  to   get  transparent  handling of
8-bit characters required for many character-set extensions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			gcc
#define M_OPTIMIZE	        -O2
#define M_LDFLAGS		-static
#define M_CFLAGS		-Wall -funsigned-char
#define M_LIBS			-lm -ltermcap


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			COMPILER OPTIONS

If it exists for  your  machine,  I   suggest  to  use  the  GNU project
C-compiler GCC.  It is the only compiler   I've seen which is capable of
optimising  the  virtual  machine  interpreter.   Many  compilers  don't
understand the complex flow control of this gigantic function and either
tell you they refuse to optimise, silently   refuse  to optimise or even
crash.  GCC passes small structures often more efficient than the native
compiler.  Notably the implementation of clause  indexing is improved by
this.

  ANSI
      This flag indicates the compiler provides ANSI prototypes and  the
      standard ANSI header files.

  PROTO
      This flag indicates the compiler accepts ANSI prototypes, but does
      not offer the standard ANSI include files.

  O_NO_LEFT_CAST
      Compiler does not understand ``(cast)variable = value''.

  O_NO_VOID_POINTER
      Compiler does not understand ``void *'' as  an  anonymous  pointer
      type.   Any ANSI compiler should understand this and many non-ANSI
      compilers appear to do.

  O_SHORT_SYMBOLS
      Ancient compiler, whose symbols are taken to be equal if the first
      8 characters are equal.  If your cpp accepts  longer  symbols  you
      are  alright  with this option.  Otherwise you will have to change
      names of identifiers.  I do not know if pl-ssymb.h still  is  upto
      date.

  O_ULONG_PREDEFINED
      The  data type  ulong has been  predefined by the compiler.  Its
      definition should be `typedef unsigned long ulong'.

  O_UCHAR_PREDEFINED
      The data type uchar has  been predefined by  the compiler.   Its
      definition should be `typedef unsigned char uchar'.

  TIME_INCLUDE
      Include file to get `struct tm'.  Define when not <sys/time.h>


		       OPERATING SYSTEM OPTIONS

  O_16_BITS
      Indicate global declared structures may be aligned  at  two  bytes
      instead  of  4  bytes  or  malloc()  may  return a 2 bytes aligned
      address.  Normally the low 2 bits of pointers  are  used  for  the
      garbage collector.  With this flag, the low bit and a high bit are
      used for this purpose.  As a result, addressable memory is limited
      to  256 MB instead of 512 MB, but this is unlikely to be a problem
      on 16 bit machines.  If you change this flag, recompile the entire
      source!

  O_DATE_AT_0X1
      The data segment starts at 0x10000000.  Many Ultrix machines

  O_DATA_AT_0X2
      The data segment starts at 0x20000000.  Many AIX machines

  O_DATA_AT_0X4
      The data segment starts at 0x40000000.  Many HPUX machines

  DATA_START
      Start     of   the  data  segment.    Needed     when O_SAVE  or
      O_STORE_PROGRAM  is  defined.   For    O_SAVE  (preferred), only
      necessary  when not equal   to    &environ  (true for most   BSD
      systems).  Must often be set on System-V machines.

  TEXT_START
      Start of the text-segment.  Needed when O_STORE_PROGRAM is set

  SIGNAL_HANDLER_TYPE
      Type returned by signal handlers.  If not defined, this defaults
      to void.  See signal.h for the value used on your operating system.

  O_PROFILE
      Include the Prolog execution profiler (like Unix prof  (1)).   The
      profiler  is  a useful tool to find performance pigs in the system
      predicates or user programs.  It  can  only  be  included  if  the
      system supports the signal SIGPROF, which signals every time slice
      Prolog becomes active.  Including it increases the amount of space
      used  to  store  predicates  with  8 bytes per predicate.  It also
      slightly decreases overall performance (about 1%).

  O_SIG_AUTO_RESET
      Signals set with signal() automatically reset   after a signal has
      been catched.  Use this with v7 Unix systems.

  O_NOSELECT
      System does not provide select() system   call.   As a consequence
      sleep/1 will only honour 1   second granularity.  wait_for_input/3
      is not supported without select().

  O_GETCWD
      System provides getcwd() library function.  If not set, getwd() is
      assumed.

  O_NOGETTIMEOFDAY
      System lacks gettimeofday() function.  Used   by get_time/1.  When
      not available time is in whole seconds.

  DEFAULT_PATH
      The search path for executables, used if $PATH is not  set.   This
      value  is necessary to find the running executable if $PATH is not
      a defined environment variable.  The path  of  the  executable  is
      used  by the foreign language interface, creating saved states and
      the intermediate code compiler.

			      DYNAMIC STACKS

      Dynamic stacks indicate we can get  access  to  the  machines  MMU
      (Memory  Management  Unit).   In  some  unix  systems  this can be
      accomplished via file mapping using the mmap()  system  call  (see
      O_CAN_MAP).   In  others via shared memory.  If both are possible,
      choose the O_CAN_MAP option as shared memory is normally a limited
      resource.

  O_SHARED_MEMORY
      System has a (properly working) system-V compatible shared  memory
      system  (shmget(), shmat(), shmdt()). If set, O_DYNAMIC_STACK will
      be set automatically.  Only  choose  one  of  O_SHARED_MEMORY  and
      O_CAN_MAP.   O_CAN_MAP  is to be preferred as there is no limit on
      the number of maps that can be established.

  O_CAN_MAP
      System  has  mmap()  and  munmap()   system   calls.    See   also
      O_SHARED_MEMORY.

  O_NO_SEGV_ADDRESS
      If the  system  provides  O_DYNAMIC_STACKS,  this  flag  indicates
      whether  the  signal  handler  is provided with the address of the
      segmentation fault.  If not, set this flag.  Prolog  will  try  to
      find  the  faulting stack using heuristics.  This takes (slightly)
      longer and might result in a stack being expanded  that  did  not
      actually require expansion, but otherwise it works fine.

  MAX_VIRTUAL_ADDRESS
      Highest virtual address we can use for  mapping  the  stacks.   On
      many  machine  the  C-stack grows from the maximum virtual address
      downwards.  In this case leave enough space  for  the  C-stack  to
      grow.  SWI-Prolog places the stacks downwards from this address.

  O_FOREIGN
      Include foreign language code linker.  This  requires the (Unix)
      linker to  accept  the -A flag that  allows  you to  generate an
      image  fitting with  a existing image.   Some systems  (AIX  and
      MACH) define  functions to load  object-files.  Scan the various
      existing md-files and pl-load.c

  O_SAVE
      Define save/[1,2], save_program/[1,2] and restore/1 for handling
      saved execution states.   This is  the  new and   hopefully more
      portable   alternative to  O_STORE_PROGRAM.  The  file pl-save.c
      describes  how  it works.    Only  define   one of   O_SAVE   or
      O_STORE_PROGRAM.

  O_STORE_PROGRAM
      Include saved states.  This is difficult  to  port.   The  current
      version  uses the GNU-emacs code for generating the dump (slightly
      modified).  O_SAVE is to be preferred.

  DESCRIPTOR_TABLE_SIZE
      If this macro is defined, it should evaluate to the  size  of  the
      file  descriptor  table.   If  it  is  not  defined  the  function
      getdtablesize() is called to resolve the descriptor table size.


		        TERMINAL DRIVER OPTIONS

  O_READLINE
      Use the GNU  library  readline.    This  library  is  distributed
      separately from many ftp sites.  When 0 SWI-Prolog is supposed to
      read its input just using getchar().

  O_TERMIOS
      Use   the   System-V   termio   terminal    driver.    Used   for
      get_single_char/1 and by the tracer.  When 0 the user should type
      a return after each tracer command.

  O_FOLD
      Integer specifying default line folding for your  OS.   0  implies
      Prolog does not do line folding.

			/*** WINDOWING INTERFACE ***/

  O_PCE
      Includes hooks for   the XPCE  object-oriented  user   interface
      package.  XPCE runs on  various  platforms supporting X11.   For
      more information contact:

		      jan@swi.psy.uva.nl (Jan Wielemaker)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

			/* compiler */
#define ANSI			1
/*#define PROTO			0 not needed when ANSI is set*/
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
			/* Operating system */
#define O_PROFILE		0
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 * 1024)
#define O_FOREIGN		0
#define O_SAVE			0
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:"

			/* terminal driver */
#define O_READLINE		1
#define O_TERMIOS 		1
#define O_FOLD 			0

			/* Interfaces */
#define O_PCE 			0

#define MACHINE			"generic"
#define OPERATING_SYSTEM	"generic"
