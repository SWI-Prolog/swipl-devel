/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Create saved state
*/

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines  a package to  create and restore  states of Prolog.
Actually there is little  that makes it specific  to prolog.   Earlier
versions of Prolog used the GNU unexec() code underlying gnu-emacs and
undump.  Together with  dynamic  stack  allocation and loading foreign
code, this used the be the main reason for Prolog not  to be portable.
This package  is an  attempt  to  create  a reasonable   portable save
functionality.


HOW DOES IT WORK?

This program  tries to find all parts   of  memory that contains valid
information,  with  except for the  text-segment.   These sections are
saved  on   file  together   with  some administrative    information.
Restoring a  state implies memory areas  are set-up equivalent  to the
state when the state is saved; all the sections are read back in place
and the system performs  a  longjmp(), either to   the start  of   the
program, or to where it was.


ASSUMPTIONS

  1) We  assume  the running prolog image to  consists of a  `text' area
  that   is read-only and in   any  run  of prolog   loaded  at the same
  addresses.  This  will be  true  on almost any machine  using  virtual
  memory (and (thus) no relocation).

  2) There is memory area  that holds the programs data  as well as  the
  data belonging to malloc(), etc.   By default,  this is area  &environ
  ... sbrk(0)

  3)  (for save/[1,2] only)  The C-stack  can be   reloaded at the  same
  position.

  4) The program  knows  about possible other  memory areas.  When using
  dynamic-stacks, the mapped areas are passed for save/[1,2].


PORTABILITY/OPTIONS

  O_C_STACK_GROWS_UP
    Each subsequent C-stackframe  is  at a  higher address.  Note that
    addresses are considered unsigned.  By default we assume the stack
    grows downwards.

  BASE_OF_C_STACK 
    If your machine has a fixed address where the C-stack  starts, you
    may #define BASE_OF_C_STACK to  this  address.  Normally using the
    STACK_BASE_ALIGN   mechanism  is   a better   guarantee   you  are
    independant of versions of the OS.

  STACK_BASE_ALIGN
    If BASE_OF_C_STACK is  not defined,  we will round  the pointer to
    argc  of main()  depending  on  O_C_STACK_GROWS_UP down/up  to   a
    multiple of this value (default: 64 Kbytes).  This generally finds
    the stack's base address.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if TEST
#undef DEBUG
#define DEBUG(l, g) {g;}
#endif

#if O_SAVE
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <setjmp.h>
#include <fcntl.h>
#include "pl-save.h"

int	brk P((caddr_t));
caddr_t sbrk P((int));
extern  etext;			/* end-of-text */
extern  char **environ;		/* normally first data-address */

#ifndef STACK_BASE_ALIGN
#define STACK_BASE_ALIGN (1<<16)	/* 64K */
#endif
#define MAXSTACKFRAMESIZE 256	/* Should generally do ... */
#define MAXLINE1	256	/* Maximum length of the ASCII header */

#define SAVE_MAGIC	3823212	/* Just a random number */

typedef struct save_header
{ long	magic;			/* Determine type of file */
  long  save_version;		/* Version of the hosting process */
  caddr brk;			/* Value of the break when saved */
  int	nsections;		/* Number of sections available */
} * SaveHeader;

#define exit(status)	Halt(status);

#define SectionOffset(n)	(sizeof(struct save_header) + \
				 (n) * sizeof(struct save_section))

#define tryRead(fd, buf, n) \
	if ( read(fd, buf, n) != n ) \
  	  return warning("restore/1: read failed: %s", OsError())
#define tryWrite(fd, buf, n) \
	if ( write(fd, buf, n) != n ) \
  	  return warning("save/1: write failed: %s", OsError())
#define trySeek(fd, where) \
	if ( lseek(fd, where, SEEK_SET) != where ) \
  	  return warning("restore/1: seek failed: %s", OsError())


		/********************************
		*           VARIABLES		*
		********************************/

static long    save_version;	/* Magic code to verify same version */
static jmp_buf ret_main_ctx;	/* Context of RET_MAIN */
static jmp_buf ret_return_ctx;	/* Context of RET_RETURN */
static caddr   c_stack_base;	/* Base address of C-stack */


		/********************************
		*          SAVE VERSION		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function  returns  a number depending on  the text-segment.   The
text-segment should be  the same when  reloading a state.  This is not
an ideal solution: it forces the entire text to be loaded.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
saveVersion()
{ if ( save_version == 0 )
  { long *start = (long *) saveVersion;
#ifdef TEXT_END
    long *end	= (long *) TEXT_END;
#else
    long *end   = (long *) &etext;
#endif
    int  step   = (end - start) / 500;
    
    for(; start < end; start += step)
      save_version ^= *start;
  }

  return save_version;
}


		/********************************
		*              IO		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The FILE related IO buffer  will be restored  in the same  state as it
was  before the saved state  was created.  We want  to  read new input
instead  of saved input   and therefore we   have to  clear  the input
stream.  Currently this is done by setting the _cnt slot of  the stdio
structure to 0.  This is not very portable.

Alternatives?   One  would be  to allocate  new  IO buffers, but then,
assigning these to stdin is not very neat either.   I guess a macro is
best ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetIO()
{ ResetTty();
}


		/********************************
		*            C-STACK		*
		********************************/

static caddr
topOfCStack()
{ int local;

  return (caddr) &local;
}


static caddr
baseOfCStack(mainargcaddr)
caddr mainargcaddr;
{ 
#ifdef BASE_OF_C_STACK
  return (caddr) BASE_OF_C_STACK;
#else
# if ( O_C_STACK_GROWS_UP )		/* round-down */
  ulong base = (ulong) mainargcaddr;
  return (caddr) (base & ~(STACK_BASE_ALIGN-1))
# else
  ulong base = (ulong) mainargcaddr;	/* round-up */
  return (caddr) ((base+STACK_BASE_ALIGN-1) & ~(STACK_BASE_ALIGN-1));
# endif
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read the C-stack back.  First of all, we must  try  to get out  of the
region that  is  overwritten by  the  read.   We do  this   by calling
ourselfves recursively until  we are  out.  We  take a  small security
region: MAXSTACKFRAMESIZE.

Note, your  compiler  might  be smart  enough   to  use tail-recursion
optimization on this function.  If so, you'll have to fool it or don't
optimise this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
readCStack(fd, start, length)
int fd;
caddr start;
long length;
{ 
#if C_STACK_GROWS_UP
  if ( (ulong) &fd - MAXSTACKFRAMESIZE > (ulong) start )
#else
  if ( (ulong) &fd + MAXSTACKFRAMESIZE < (ulong) start )
#endif
  { DEBUG(1, printf("&fd = 0x%x\n", (unsigned) &fd));
    tryRead(fd, start, length);
    DEBUG(1, printf("C-stack read; starting longjmp\n"));
    resetIO();
    longjmp(ret_return_ctx, 1);
  } else
    return readCStack(fd, start, length);
}


		/********************************
		*            RESTORE		*
		********************************/

int
restore(file, allocf)
char *file;
int (*allocf) P((SaveSection));
{ int fd;
  char buf[MAXLINE1];
  int n;
  long header_offset = 0;
  struct save_header header;
  char *s;
  
  if ( (fd = open(file, O_RDONLY)) < 0 )
    return warning("restore/1: cannot open %s: %s", file, OsError());
  tryRead(fd, buf, MAXLINE1);

  for(n = 0, s = buf; n++ < MAXLINE1; s++)
  { if ( *s == EOS )
    { header_offset = n;
      break;
    }
  }
  DEBUG(1, printf("header_offset = %d\n", header_offset));
  if ( header_offset == 0 )
    return warning("restore/1: %s is not a saved state", file);

  trySeek(fd, header_offset);
  tryRead(fd, &header, sizeof(header));

  if ( header.magic != SAVE_MAGIC )
    return warning("restore/1: %s is not a saved state", file);
  if ( header.save_version != saveVersion() )
    return warning("restore/1: %s has incompatible save version", file);
  
  if ( brk(header.brk) )
    return warning("restore/1: failed to set the break: %s", OsError());

  { jmp_buf restore_ctx;	/* Will be destroyed */

    memcpy(restore_ctx, ret_main_ctx, sizeof(restore_ctx));

    for(n = 0; n < header.nsections; n++)
    { struct save_section section_header;

      trySeek(fd, header_offset+SectionOffset(n));
      tryRead(fd, &section_header, sizeof(section_header));
      DEBUG(1, printf("Restoring # %d (0x%x-0x%x) offset = %ld)\n",
		      n, (unsigned) section_header.start,
		      (unsigned) section_header.start + section_header.length,
		      header_offset+section_header.offset));
      trySeek(fd, header_offset+section_header.offset);
      if ( section_header.type == S_CSTACK )
	readCStack(fd, section_header.start, section_header.length);
      else 
      { if ( allocf )
	  (*allocf)(&section_header);
	tryRead(fd, section_header.start, section_header.length);
      }
    }

    memcpy(ret_main_ctx, restore_ctx, sizeof(restore_ctx));

    resetIO();
    longjmp(ret_main_ctx, 1);
    /*NOTREACHED*/
  }
}

		/********************************
		*            SAVE		*
		********************************/

int
save(file, interpreter, kind, nsections, sections)
char *file;
char *interpreter;
int kind;			/* RET_RETURN; RET_MAIN */
int nsections;			/* additional sections */
SaveSection sections;
{ int fd;
  char buf[MAXLINE1];
  long header_offset;
  volatile long section_offset;		/* volatile to keep gcc happy */
  struct save_header header;
  int nsects = nsections + (kind == RET_RETURN ? 2 : 1);
  int sects_size = sizeof(struct save_section) * nsects;
  SaveSection sects = alloca(sects_size);
  int n;
  SaveSection sect;

  if ( (fd = open(file, O_WRONLY|O_CREAT|O_TRUNC, 0777)) < 0 )
    return warning("save/1: cannot write %s: %s\n", file, OsError());
  
/*sprintf(buf, "#!%s -r\n", interpreter);*/
  sprintf(buf, "#!/bin/sh\nexec %s -r $0 $*\n", interpreter);
  header_offset = strlen(buf) + 1; /* +1 to write the EOS too */
  DEBUG(1, printf("header_offset = %d\n", header_offset));
  tryWrite(fd, buf, header_offset);

  header.magic 		= SAVE_MAGIC;
  header.save_version   = saveVersion();
  header.brk		= sbrk(0);
  header.nsections	= nsects;
    
  section_offset = sizeof(header) + sects_size;

#ifdef DATA_START
  sects[0].start	= (caddr) DATA_START;
#else
  sects[0].start	= &environ;
#endif
  sects[0].length	= (long) sbrk(0) - (long) sects[0].start;
  sects[0].type		= S_DATA;
  sects[0].flags	= 0;

  memcpy(&sects[1], sections, nsections * sizeof(struct save_section));

  if ( kind == RET_RETURN )
  { SaveSection stack_sect = &sects[nsections+1];

    if ( setjmp(ret_return_ctx) )
    { DEBUG(1, printf("Yipie, returning from state\n"));
      return SAVE_RESTORE;
    } 
    
    stack_sect->start  = topOfCStack();
    stack_sect->length = (ulong) c_stack_base - (ulong) stack_sect->start;
    stack_sect->type   = S_CSTACK;
    stack_sect->flags  = 0;
  }

  for(n=0, sect = sects; n++ < header.nsections; sect++)
  { sect->offset = section_offset;
    section_offset += sect->length;
  }

  tryWrite(fd, &header, sizeof(header));
  tryWrite(fd, sects, sects_size);

  for(n=0, sect = sects; n++ < header.nsections; sect++)
  { DEBUG(1, printf("Saving # %d 0x%x-0x%x (offset = %ld)\n",
		    n,
		    (unsigned) sect->start,
		    (unsigned) sect->start+sect->length,
		    tell(fd)));
    tryWrite(fd, sect->start, sect->length);
  }

  close(fd);

  return SAVE_SAVE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
main() stub
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
main(argc, argv, env)
int argc;
char **argv;
char **env;
{ int rval;

  c_stack_base = baseOfCStack((caddr) &argc);

  if ( setjmp(ret_main_ctx) )
  { DEBUG(1, printf("Restarting startProlog()\n"));
    environ = env;
    rval = startProlog(argc, argv, env);
  } else
  { if ( argc >= 3 && streq(argv[1], "-r") )
#if O_DYNAMIC_STACKS
    { extern int allocateSection P((SaveSection));

      if ( !restore(argv[2], allocateSection) )
	exit(1);
    }
#else
    { if ( !restore(argv[2], NULL) )
	exit(1);
    }
#endif

    rval = startProlog(argc, argv, env);
  }

  exit(rval);
  return 1;
}

#else !O_SAVE

int
main(argc, argv, env)
int argc;
char **argv;
char **env;
{ exit(startProlog(argc, argv, env));
  return 1;
}

#endif


#if TEST				/* test stand-alone */

static value = 1;
static dumped = 0;

bool
warning(va_alist)
va_dcl
{ va_list args;
  char *fm;

  va_start(args);
  fm = va_arg(args, char *);
  fprintf(stderr, "[WARNING: ");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "]\n");
  va_end(args);

  fail;
}


char *
OsError()
{ static char errmsg[64];
  extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;

  if ( errno < sys_nerr )
    return sys_errlist[errno];

  sprintf(errmsg, "Unknown Error (%d)", errno);
  return errmsg;
}


startProlog(argc, argv, env)
int argc;
char **argv, **env;
{ char *interpreter = argv[0];

  argc--; argv++;

  printf("value = %d\n", value++);

  for ( ; argc > 1; argc--, argv++ )
  { if ( streq(argv[0], "-s") )
    { save(argv[1], interpreter, RET_MAIN, 0, NULL);
      printf("Saved (-s) in %s\n", argv[1]);
      exit(0);
    }
    if ( streq(argv[0], "-S") )
    { if ( save(argv[1], interpreter, RET_RETURN, 0, NULL) == SAVE_SAVE )
	printf("Saved (-S) in %s\n", argv[1]);
      else
	printf("Restored from %s\n", argv[1]);
      exit(0);
    }
  }

  exit(0);
}

#endif
