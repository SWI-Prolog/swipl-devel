/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Generic machine description file
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compiler flags for making `makefile' from `Makefile' using cpp
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define M_CC			cc
#define M_OPTIMIZE	        -O
#define M_LDFLAGS		
#define M_CFLAGS		
#define M_LIBS			-lm -ltermcap


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
????
system.  At the bottom of this file a default setting that can  be  used
as  starting point for a new architecture can be found.  To set up a new
archtecture, copy this file to a md- file with appropriate name,  delete
the  comment  and change values as appropriate.  If you are not sure, it
might be wise to use the default setting of this file as a first guess.

The  #define statements below should be set to 1 if a feature is wanted.
Otherwise it should be set to 0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			COMPILER OPTIONS

If it exists for  your  machine,  I  suggest  to  use  the  GNU  project
C-compiler  GCC.   It is the only compiler I've seen which is capable of
optimising  the  virtual  machine  interpreter.   Many  compilers  don't
understand the complex flow control of this gigantic function and either
tell  you  they  refuse to optimise, silently refuse to optimise or even
crash.  GCC passes small structures often more efficient than the native
compiler.  Notably the implementation of clause indexing is improved  by
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

  O_ASM_SWITCH
      Modify assembler output of the compiler of  pl-wam.c  to  optimise
      the  switch  statement.   See  pl-comp.c and pl-wam.c. I have used
      this on SUN-SPARC, where it provides an performance improvement of
      about 10%. It is very hairy and clumbersome  to  install  and  for
      this reason I normally omit it.  Left in for `just in case'.

  O_16_BITS
      Indicate global declared structures may be aligned  at  two  bytes
      instead  of  4  bytes  or  malloc()  may  return a 2 bytes aligned
      address.  Normally the low 2 bits of pointers  are  used  for  the
      garbage collector.  With this flag, the low bit and a high bit are
      used for this purpose.  As a result, addressable memory is limited
      to  256 MB instead of 512 MB, but this is unlikely to be a problem
      on 16 bit machines.  If you change this flag, recompile the entire
      source!

  O_DATA_AT_0X2
      The data segment starts at 0x20000000.  Many IBM machines do this.

		       OPERATING SYSTEM OPTIONS

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
      Signals set with signal() automaticaly reset after  a  signal  has
      been catched.  Use this with v7 Unix systems.

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
      longer and might result in a stack beeing expanded  that  did  not
      actually require expansion, but otherwise it works fine.

  MAX_VIRTUAL_ADDRESS
      Highest virtual address we can use for  mapping  the  stacks.   On
      many  machine  the  C-stack grows from the maximum virtual address
      downwards.  In this case leave enough space  for  the  C-stack  to
      grow.  SWI-Prolog places the stacks downwards from this address.

  O_FOREIGN
      Include foreign language code linker.  This  requires  the  (Unix)
      linker  to accept the -A flag that allows you to generate an image
      fitting with a existing image.

  O_STORE_PROGRAM
      Include saved states.  This is difficult  to  port.   The  current
      version  uses the GNU-emacs code for generating the dump (slightly
      modified).  If GNU-emacs can be dumped, it should not be too hard.

  DESCRIPTOR_TABLE_SIZE
      If this macro is defined, it should evaluate to the  size  of  the
      file  descriptor  table.   If  it  is  not  defined  the  function
      getdtablesize() is called to resolve the descriptor table size.


		        TERMINAL DRIVER OPTIONS

  O_TERMIOS
      Use the System-V termio terminal driver. If not set the BSD sgtty
      package is used.  Set this flag if you can!

  O_EXTEND_ATOMS
      Include automatic extension of atoms if the user  types  ^[  (ESC)
      and inform the user about existing atoms on ^D similar to the Unix
      4.2  and  newer  csh(1)  do  to  files.  It can be included if the
      terminal driver allows for a second end-of-line character  (t_brkc
      in  struct tchars) or has the new `termios' terminal driver.  This
      is still somewhat experimental (in particular we should try to  be
      a bit more selective than returning any atom).

      The current implementation  also  allows  this  option  if  it  is
      possible to read characters without waiting for a return.  See the
      O_LINE_EDIT option.

  O_LINE_EDIT
      Makes Prolog reading terminal input in  Unix  cbreak  mode.   Line
      editing  capabilities  are  provided by Prolog itself.  Allows for
      O_EXTEND_ATOMS, even if the terminal driver does not  support  the
      necessary options.

  O_MAP_TAB_ON_ESC	1
      Makes the prolog reader mapping the TAB character on ESC (for
      extending atoms) when reading from a terminal.

  O_FOLD
      Integer specifying default line folding for your  OS.   0  implies
      Prolog does not do line folding.

			/*** WINDOWING INTERFACE ***/

  O_PCE
      Include hooks for the PCE object oriented user interface  package.
      Currently PCE only runs on SUN.  An X-version is under development
      and  will  be  available  by  october  1990.  For more information
      contact:

		      anjo@swi.psy.uva.nl (Anjo Anjewierden)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

			/* compiler */
#define ANSI			0
#define PROTO			0
#define O_NO_LEFT_CAST		0
#define O_NO_VOID_POINTER	0
#define O_SHORT_SYMBOLS		0
#define O_ASM_SWITCH		0
			/* Operating system */
#define O_PROFILE		0
#define O_SIG_AUTO_RESET	0
#define O_SHARED_MEMORY		0
#define O_CAN_MAP		0
#define O_NO_SEGV_ADDRESS	0
#define MAX_VIRTUAL_ADDRESS	(220 * 1024 * 1024)
#define O_FOREIGN		0
#define O_STORE_PROGRAM		0
#define DEFAULT_PATH		":/usr/ucb:/bin:/usr/bin:/usr/local:.:"
/*#define DESCRIPTOR_TABLE_SIZE do what you need to */
			/* terminal driver */
#define O_TERMIOS 		0
#define O_EXTEND_ATOMS 		1
#define O_LINE_EDIT 		1
#define O_MAP_TAB_ON_ESC	1
#define O_FOLD 			0
			/* Interfaces */
#define O_PCE 			0

#define MACHINE			"generic"
#define OPERATING_SYSTEM	"generic"
