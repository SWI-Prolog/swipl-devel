/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Create a saved state (stand alone executable)
*/

/*
** This file contains changes which are part of a port to HPUX 8.0
** T. Kielmann, 01 Jun 92
*/

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Creating of saved states of the program (the  stacks  are  lost).   This
module takes the currently running program and generates a complete Unix
executable file from it.  It uses the GNU-Emacs code for unexec(), which
is modified a bit to improve the interaction with SWI-Prolog.

Hack hack hack hack ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_STORE_PROGRAM

#include <sys/param.h>

int	unexec P((char *, char *, char *, unsigned, unsigned, unsigned));

word
saveProgram(new)
Word new;
{ char *dest, *old_name, *new_name, *sym_name;
  char tmp[MAXPATHLEN];
  
  if ( !isAtom(*new) )
    return warning("save_program/1: instantiation fault");

  if ( cannot_save_program != NULL )
    return warning("Cannot save the program: %s", cannot_save_program);

  TRY( getSymbols() );
  
  old_name = stringAtom(loaderstatus.orgsymbolfile);
  if ( loaderstatus.symbolfile != loaderstatus.orgsymbolfile )
    sym_name = stringAtom(loaderstatus.symbolfile);
  else
    sym_name = NULL;
  
  dest = stringAtom(*new);
  sprintf(tmp, "%s#%d", dest, getpid());
  new_name = tmp;

  systemDefaults.state = (char *) NULL;		/* does not need this any */
  closeFiles();

  Putf("Running program: %s\n", old_name);
  if ( sym_name != NULL )
    Putf("Taking symbol table from %s\n", sym_name);
  Putf("Saving to %s ... ", new_name); pl_flush();
  if ( unexec(new_name, old_name, sym_name, 0, 0, 0) < 0 )
  { DeleteFile(tmp);
    fail;
  }
  Putf("ok.\n");
  Putf("Moving %s to %s ... ", tmp, dest); pl_flush();
  if ( RenameFile(tmp, dest) == FALSE )
  { warning("Failed to install %s: %s", dest, OsError());
    DeleteFile(tmp);
    fail;
  }
  Putf("ok.\n");

  succeed;
}

static
bool
sizeOption(i, v)
int *i;
word v;
{ if ( !isInteger(v) )
    return warning("save_program/2: illegal option argument");
  *i = valNum(v);

  succeed;
}

static
bool
stringOption(s, v)
char **s;
word v;
{ if ( !isAtom(v) )
    return warning("save_program/2: illegal option argument");
  *s = stringAtom(v);

  succeed;
}

static
bool
boolOption(b, v)
bool *b;
word v;
{ if ( v == (word) ATOM_on )
    *b = TRUE;
  else if ( v == (word) ATOM_off )
    *b = FALSE;
  else
    return warning("save_program/2: illegal option argument");

  succeed;
}

word
pl_save_program(new, args)
Word new, args;
{ Word a;
  Word Option, Value;
  Atom option;
  word value;

  while( isList(*args) )
  { a = HeadList(args);
    deRef(a);
    if ( !isTerm(*a) || functorTerm(*a) != FUNCTOR_equals2 )
      return warning("save_program/2: Illegal option list");

    Option = argTermP(*a, 0); deRef(Option);
    Value  = argTermP(*a, 1); deRef(Value);
    if ( !isAtom(*Option) )
      return warning("save_program/2: Illegal option list");

    option = (Atom) *Option;
    value = *Value;
    if        ( option == ATOM_local )
    { TRY( sizeOption(&systemDefaults.local, value) );
    } else if ( option == ATOM_global )
    { TRY( sizeOption(&systemDefaults.global, value) );
    } else if ( option == ATOM_trail )
    { TRY( sizeOption(&systemDefaults.trail, value) );
    } else if ( option == ATOM_argument )
    { TRY( sizeOption(&systemDefaults.argument, value) );
    } else if ( option == ATOM_goal )
    { TRY( stringOption(&systemDefaults.goal, value) );
    } else if ( option == ATOM_toplevel )
    { TRY( stringOption(&systemDefaults.toplevel, value) );
    } else if ( option == ATOM_init_file )
    { TRY( stringOption(&systemDefaults.startup, value) );
    } else if ( option == ATOM_tty )
    { TRY( boolOption(&systemDefaults.notty, value) );
      systemDefaults.notty = !systemDefaults.notty;
    } else
    { return warning("save_program/2: Unknown option: %s", stringAtom(option));
    }

    args = TailList(args);
    deRef(args);
  }
  if ( !isNil(*args) )
    return warning("save_program/2: Illegal option list");

  return saveProgram(new);
}

#define NO_REMAP 1			/* Do not change data start */

#ifdef UNEXEC_SOURCE

#include UNEXEC_SOURCE

#else ~UNEXEC_SOURCE

#include <a.out.h>

#if sun
#define A_TEXT_OFFSET(HDR) sizeof (HDR)
#define TEXT_START	(SEGSIZ + sizeof(struct exec))
#endif

#if gould
#define A_TEXT_OFFSET(HDR) sizeof (HDR)
#define ADJUST_EXEC_HEADER unexec_text_start = hdr.a_txbase + sizeof (hdr);
#define GOULD 1
#endif

#if AIX					/* PS2 version; still in use? */
#define COFF		1		/* Coff ld output format */
#define TEXT_START	0x00000000	/* Hope text always starts here */
#define DATA_START	0x00400000	/* Hope data always starts here */
#endif

#if hpux
#define HPUX
#define N_TXTOFF(hdr)	TEXT_OFFSET(hdr)
#define N_DATADDR(hdr)	(DATA_OFFSET(hdr) - EXEC_PAGESIZE)
#define N_SYMOFF(hdr)	LESYM_OFFSET(hdr)
#endif

#define PERROR(msg) warning("save_program/2: %s: %s", msg, OsError()); \
		    return -1

char *
start_of_data ()
{
#ifdef DATA_START
  return ((char *) DATA_START);
#else
#  if hpux
{     extern etext;
      return (char *) &etext + EXEC_PAGESIZE; /* Rounded down by unexec() */
}
#  else
      extern char **environ;
      return ((char *)&environ);
#  endif
#endif
}


char *
start_of_text(hdr)
struct exec *hdr;
{
#ifdef N_TXTADDR
#ifdef A_TEXT_OFFSET
  return (char *) N_TXTADDR(*hdr) + A_TEXT_OFFSET(*hdr);
#else
  return (char *) N_TXTADDR(*hdr);
#endif
#else N_TXTADDR
#ifdef TEXT_START
  return ((char *) TEXT_START);
#else
#if gould				/* GOULD machine */
  extern csrt();
  return ((char *) csrt);
#else gould
  return (char *) hdr->a_entry;
#endif
#endif TEXT_START
#endif N_TXTADDR
}

#include "gnu/unexec.c"
#endif UNEXEC_SOURCE

#else ~O_STORE_PROGRAM

word
saveProgram(new)
Word new;
{ return warning("store_program/1: not ported to this machine");
}

word
pl_save_program(old, new)
Word old, new;
{ return warning("store_program/2: not ported to this machine");
}

#endif O_STORE_PROGRAM
