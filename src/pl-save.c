/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Create saved state
*/

/*#define O_DEBUG 1*/
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

This program tries  to find all  parts of memory  that contains  valid
information, except for the text-segment.  These sections are saved on
file together with some administrative information.  Restoring a state
implies memory areas are set-up equivalent to the state when the state
is  saved; all the  sections are read back  in  place and  the  system
performs a longjmp(), either to the start of the  program, or to where
it was.


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

  STACK_DIRECTION > 0
    Each subsequent C-stackframe  is  at a  higher address.  Note that
    addresses are considered unsigned.  By default we assume the stack
    grows downwards.

  BASE_OF_C_STACK 
    If your machine has a fixed address where the C-stack  starts, you
    may #define BASE_OF_C_STACK to  this  address.  Normally using the
    STACK_BASE_ALIGN   mechanism  is   a better   guarantee   you  are
    independant of versions of the OS.

  STACK_BASE_ALIGN
    If BASE_OF_C_STACK is not defined, we   will  round the pointer to
    argc of main() depending on STACK_DIRECTION  to a multiple of this
    value (default: 64 Kbytes).  This generally finds the stack's base
    address.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                       Notes on the OS/2 port.
			atoenne@mpi-sb.mpg.de

    The save/2 primitive had a rather unusual problem in OS/2. The save
    function tries to write the memory between start of data and the
    end of the heap as one huge chunk to disk. OS/2 however does not
    allocate the junk area between the end of data and the start of
    the heap. Consequently we get a protection violation with the original
    save function. We have to write two sections, one for data and
    one for the heap. These areas are determined as follows:

    data ranges from &_data to &_end
    heap ranges from _heap_base to sbrk(0)

    The routines for determing the stack frame are fine.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __WIN32__
#undef TEST				/* ??? */
#endif

#if TEST
#include <stdio.h>
#undef DEBUG
#define DEBUG(l, g) {g;}
#define O_SAVE 1
#define OsPath(x) x
#define ResetTty()
#define Sdprintf printf
#define Ssprintf sprintf
#define PL_initialise main
#endif

#if O_SAVE
#include <sys/types.h>
#include <unistd.h>
#ifndef SIO_MAGIC			/* using stream-package */
#include <stdio.h>
#endif
#include <setjmp.h>
#include <fcntl.h>
#include <errno.h>
#include "pl-save.h"

#ifdef NEED_DECL_ERRNO
extern int errno;
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Machine dependant stuff.  Should be  generated   by  configure, but some
things are just too hard to figure out ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if OS2 & EMX
#define HEAP_START _heap_base
#define LAST_DATA_SYMBOL _end
extern caddr _heap_base;
extern long _heap_end;
#endif /* OS2 */

#ifdef __linux__
#define DATA_START ((long) &etext + sizeof(long))
#endif

#ifdef __solaris__
#define O_SAVE_STDIO 1
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generic stuff again.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern char **environ;
extern char **etext;

#ifndef DATA_START
#ifndef FIRST_DATA_SYMBOL
#define FIRST_DATA_SYMBOL environ
#endif
#define DATA_START &FIRST_DATA_SYMBOL
#endif

#ifndef DATA_END
#ifdef LAST_DATA_SYMBOL
#define DATA_END ((caddr) &LAST_DATA_SYMBOL)
#else
#define DATA_END ((caddr) sbrk(0))
#endif
#endif

#ifdef  FIRST_DATA_SYMBOL
extern  char **FIRST_DATA_SYMBOL;
#endif
#ifdef LAST_DATA_SYMBOL            /* --- atoenne@mpi-sb.mpg.de --- */
extern  char **LAST_DATA_SYMBOL;
#endif

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
} *SaveHeader;

#ifndef TEST
#define exit(status)	Halt(status);
#endif

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
    long *end	= (long *) startProlog;
    int  step   = (end - start) / 500;
    
    if ( step <= 0 )
      step = 10;
    DEBUG(2, Sdprintf("Computing saveVersion in 0x%x .. 0x%x\n", start, end));

    for(; start < end; start += step)
      save_version ^= *start;
  }

  DEBUG(1, Sdprintf("saveVersion = %ld\n", save_version));

  return save_version;
}


		/********************************
		*              IO		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The IOSTREAM related IO buffer  will be restored  in the same  state as it
was  before the saved state  was created.  We want  to  read new input
instead  of saved input   and therefore we   have to  clear  the input
stream.  Currently this is done by setting the _cnt slot of  the stdio
structure to 0.  This is not very portable.

Alternatives?   One  would be  to allocate  new  IO buffers, but then,
assigning these to Sinput is not very neat either.   I guess a macro is
best ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetIO(void)
{ ResetTty();
}


		/********************************
		*            C-STACK		*
		********************************/

static caddr
topOfCStack(void)
{ int local;

  return (caddr) &local;
}

static caddr
baseOfCStack(int *argc, char **argv, char **env)
{ 
#ifdef BASE_OF_C_STACK
  return (caddr) BASE_OF_C_STACK;
#else

  unsigned long base = (unsigned long) &argc;
  char **p;

#if STACK_DIRECTION > 0
#define BoundStack(a,b) ((a) < (b) ? (a) : (b))
#else
#define BoundStack(a,b) ((a) > (b) ? (a) : (b))
#endif

  base = BoundStack(base, (unsigned long)argv);
  for(p = argv; *p; p++)
    base = BoundStack(base, (unsigned long)*p);
  base = BoundStack(base, (unsigned long)env);
  for(p = env; *p; p++)
    base = BoundStack(base, (unsigned long)*p);

#if STACK_DIRECTION > 0
  return (caddr) (base & ~(STACK_BASE_ALIGN-1));
#else
  return (caddr) ((base+STACK_BASE_ALIGN-1) & ~(STACK_BASE_ALIGN-1));
#endif
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
readCStack(int fd, caddr start, long int length)
{ 
#if STACK_DIRECTION > 0
  if ( (unsigned long) &fd - MAXSTACKFRAMESIZE > (unsigned long) ( (unsigned) start + length ) )
#else
  if ( (unsigned long) &fd + MAXSTACKFRAMESIZE < (unsigned long) start )
#endif
  { DEBUG(1, Sdprintf("&fd = 0x%x\n", (unsigned) &fd));
    tryRead(fd, start, length);
    DEBUG(1, Sdprintf("C-stack read; starting longjmp\n"));
    resetIO();
    longjmp(ret_return_ctx, 1);
  } else
    return readCStack(fd, start, length);
}


		/********************************
		*            RESTORE		*
		********************************/

#ifndef O_BINARY
#define O_BINARY 0
#endif

int
restore(char *file, int (*allocf) (SaveSection))
{ int fd;
  char buf[MAXLINE1];
  int n;
  long header_offset = 0;
  struct save_header header;
  char *s;

  if ((fd = open(file, O_RDONLY|O_BINARY)) < 0)
    return warning("restore/1: cannot open %s: %s", file, OsError());
  tryRead(fd, buf, MAXLINE1);

  for(n = 0, s = buf; n++ < MAXLINE1; s++)
  { if ( *s == EOS )
    { header_offset = n;
      break;
    }
  }
  DEBUG(2, Sdprintf("header_offset = %d\n", header_offset));
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
      DEBUG(2, Sdprintf("Restoring # %d (0x%x-0x%x) offset = %ld)\n",
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

#ifdef HAVE_LIBELF

#include <libelf.h>

#define ElfError(id) do { warning("%s: %s\n", \
				  id, elf_errmsg(elf_errno())); \
			  fail; \
			} while(0)

static int
fill_c_data_sections(interpreter, nsects, sections)
char *interpreter;
int *nsects;
SaveSection sections;
{ Elf *elf;
  Elf32_Ehdr *ehdr;
  Elf_Scn *scn = 0;
  int fd;
  int ndx;

  *nsects = 0;

  if ( elf_version(EV_CURRENT) == EV_NONE )
    ElfError("can't initialise elf version");
  if ( (fd = open(interpreter, O_RDONLY)) < 0 )
    return warning("save/2: can't open %s: %s", interpreter, OsError());
  if ( !(elf = elf_begin(fd, ELF_C_READ, NULL)) )
    ElfError("can't begin elf library");
  if ((ehdr = elf32_getehdr(elf)) == 0)
    ElfError("can't get elf header");
  ndx = ehdr->e_shstrndx;
  
  while ((scn = elf_nextscn(elf, scn)) != 0)
  { Elf32_Shdr *hdr = elf32_getshdr(scn);
    char *name = elf_strptr(elf, ndx, (size_t)hdr->sh_name);

    if ( streq(name, ".data") ||
	 streq(name, ".data1") )
    { assert(hdr->sh_type == SHT_PROGBITS);
      sections->start  = (caddr) hdr->sh_addr;
      sections->length = hdr->sh_size;
      sections->type   = S_DATA;
      sections->flags  = 0;

      (*nsects)++;
      sections++;
    } else if ( streq(name, ".bss") )
    { assert(hdr->sh_type == SHT_NOBITS);
      sections->start  = (caddr) hdr->sh_addr;
      sections->length = (unsigned long) sbrk(0) - (unsigned long) sections->start;
      sections->type   = S_DATA;
      sections->flags  = 0;

      (*nsects)++;
      sections++;
    } 
  }

  elf_end(elf);
  close(fd);;

  succeed;
}

#else /* HAVE_LIBELF */

#ifdef HEAP_START
#define C_DATA_SECTIONS 2		/* heap separate from data  */
#else
#define C_DATA_SECTIONS 1
#endif

static int
fill_c_data_sections(char *interpreter, int *nsects, SaveSection sections)
{ sections[0].start	= (caddr) DATA_START;
  sections[0].length	= (long) DATA_END - (long) sections[0].start;
  sections[0].type	= S_DATA;
  sections[0].flags	= 0;
  *nsects = 1;
#ifdef HEAP_START
  sections[1].start     = HEAP_START;
  sections[1].length    = (long) sbrk(0) - (long) sections[1].start;
  sections[1].type      = S_DATA;
  sections[1].flags     = 0;
  *nsects = 2;
#endif /* HEAP_START */

  succeed;
}
#endif /* HAVE_LIBELF */

int
save(char *file, char *interpreter, int kind,
     int nsections, SaveSection sections)
{ int fd;
  struct save_section sects[MAX_SAVE_SECTIONS];
  char buf[MAXLINE1];
  long header_offset;
  volatile long section_offset;		/* volatile to keep gcc happy */
  struct save_header header;
  int csects, nsects, sects_size;
  int n;
  SaveSection sect;

  fill_c_data_sections(interpreter, &csects, sects);
  nsects = csects + nsections;
  if ( kind == RET_RETURN )
    nsects++;				/* C-stack section  */
  sects_size = sizeof(struct save_section) * nsects;

  if ( (fd = open(file, O_WRONLY|O_CREAT|O_TRUNC|O_BINARY, 0777)) < 0 )
    return warning("save/1: cannot write %s: %s\n", file, OsError());
  
#if OS2
  Ssprintf(buf, "/* Self-starting SWI-Prolog state */\r\n'@ECHO OFF'\r\nparse source . . name\r\n\"%s -r \" name arg(1)\r\nexit\r\n\032", OsPath(interpreter));
#else
  Ssprintf(buf, "#!/bin/sh\nexec %s -r $0 \"$@\"\n", OsPath(interpreter));
#endif
  header_offset = strlen(buf) + 1; /* +1 to write the EOS too */
  DEBUG(1, Sdprintf("header_offset = %d\n", header_offset));
  tryWrite(fd, buf, header_offset);

  header.magic 		= SAVE_MAGIC;
  header.save_version   = saveVersion();
  header.brk		= (caddr)sbrk(0);
  header.nsections	= nsects;
    
  section_offset = sizeof(header) + sects_size;

  memcpy(&sects[csects], sections,
	 nsections * sizeof(struct save_section));

  if ( kind == RET_RETURN )
  { SaveSection stack_sect = &sects[nsections+csects];

    if ( setjmp(ret_return_ctx) )
    { DEBUG(1, Sdprintf("Yipie, returning from state\n"));
      return SAVE_RESTORE;
    } 
    
#if STACK_DIRECTION > 0
    stack_sect->start  = c_stack_base;
    stack_sect->length = (unsigned long) topOfCStack() - 
			 (unsigned long) stack_sect->start;
#else
    stack_sect->start  = topOfCStack();
    stack_sect->length = (unsigned long) c_stack_base -
			 (unsigned long) stack_sect->start;
#endif
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
  { DEBUG(1, Sdprintf("Saving # %d 0x%x-0x%x (offset = %ld)\n",
		    n,
		    (unsigned) sect->start,
		    (unsigned) sect->start+sect->length,
		    tell(fd)));
    tryWrite(fd, sect->start, sect->length);
  }

  close(fd);

  return SAVE_SAVE;
}


#ifdef FORCED_MALLOC_BASE
#include "morecore.c"
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
main() stub
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_initialise(int argc, char **argv, char **env)
{ int rval;

#ifdef FORCED_MALLOC_BASE
  start_memory((void *)FORCED_MALLOC_BASE);
  fprintf(stderr, "FORCED_MALLOC_BASE at 0x%08x\n", FORCED_MALLOC_BASE);
#endif

  c_stack_base = baseOfCStack(&argc, argv, env);

  if ( setjmp(ret_main_ctx) )
  { DEBUG(1, Sdprintf("Restarting startProlog()\n"));
    environ = env;
    rval = startProlog(argc, argv, env);
  } else
  { char **av;

#ifndef TEST
    for(av=argv; *av; av++)
      if ( streq(*av, "-d") && av[1] )
	status.debugLevel = atoi(av[1]);
#endif

    if ( argc >= 3 && streq(argv[1], "-r") )
#if defined(O_DYNAMIC_STACKS) && !defined(TEST)
    { extern int allocateSection(SaveSection);

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

  return(rval);
}

#else /*O_SAVE*/

#ifdef FORCED_MALLOC_BASE
#include "morecore.c"
#endif

int
PL_initialise(int argc, char **argv, char **env)
{ 
#ifdef FORCED_MALLOC_BASE
  start_memory((void *)FORCED_MALLOC_BASE);
  Sdprintf("FORCED_MALLOC_BASE at 0x%08x\n", FORCED_MALLOC_BASE);
#endif

  return startProlog(argc, argv, env);
}

#endif /*O_SAVE*/


#ifdef TEST				/* test stand-alone */

static value = 1;
static dumped = 0;

bool
warning(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  fprintf(stderr, "[WARNING: ");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "]\n");
  va_end(args);

  fail;
}


char *
OsError()
{
#ifdef HAVE_STRERROR
  return strerror(errno);
#else
  static char errmsg[64];
  extern int sys_nerr;
  extern char *sys_errlist[];

  if ( errno < sys_nerr )
    return sys_errlist[errno];

  Ssprintf(errmsg, "Unknown Error (%d)", errno);
  return errmsg;
#endif
}


int
startProlog(int argc, char **argv, char **env)
{ char *interpreter = argv[0];

  argc--; argv++;

  printf("value = %d\n", value++);

  for ( ; argc > 1; argc--, argv++ )
  { if ( streq(argv[0], "-s") )
    { save(argv[1], interpreter, RET_MAIN, 0, NULL);
      Sdprintf("Saved (-s) in %s\n", argv[1]);
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

#endif /* TEST */
