/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Create a saved state (stand alone executable)
*/

#include "pl-incl.h"

static opt_spec save_options[] = 
{ { ATOM_local,      OPT_INT },
  { ATOM_global,     OPT_INT },
  { ATOM_trail,	     OPT_INT },
  { ATOM_argument,   OPT_INT },
  { ATOM_goal,       OPT_STRING },
  { ATOM_toplevel,   OPT_STRING },
  { ATOM_init_file,  OPT_STRING },
  { ATOM_tty,	     OPT_BOOL },
  { ATOM_stand_alone,OPT_BOOL },
  { NULL_ATOM,	     0 }
};


word
parseSaveProgramOptions(term_t args,
			int *local, int *global, int *trail, int *argument,
			char **goal, char **toplevel, char **init_file,
			bool *tty, bool *standalone)
{ return scan_options(args, 0, save_options,
		      local, global, trail, argument,
		      goal, toplevel, init_file,
		      tty, standalone);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Creating of saved states of the program (the  stacks  are  lost).   This
module takes the currently running program and generates a complete Unix
executable file from it.  It uses the GNU-Emacs code for unexec(), which
is modified a bit to improve the interaction with SWI-Prolog.

Hack hack hack hack ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_STORE_PROGRAM || O_SAVE

#include <sys/param.h>
#include <unistd.h>
#if O_STORE_PROGRAM
int	unexec(char *, char *, char *, unsigned, unsigned, unsigned);
#else
#include "pl-save.h"
#endif

word
saveProgram(term_t new)
{ char *new_name, *sym_name, *dest;
  char tmp[MAXPATHLEN];
  char old_name[MAXPATHLEN];
  char buf[MAXPATHLEN];
  
  if ( !(dest = PL_get_filename(new, buf, sizeof(buf))) )
    return warning("save_program/1: instantiation fault");

  if ( cannot_save_program != NULL )
    return warning("Cannot save the program: %s", cannot_save_program);

  TRY( getSymbols() );
  
  strcpy(old_name,OsPath(stringAtom(loaderstatus.orgsymbolfile)));
  if ( loaderstatus.symbolfile != loaderstatus.orgsymbolfile )
    sym_name = stringAtom(loaderstatus.symbolfile);
  else
    sym_name = NULL;
  
  Ssprintf(tmp, "%s#%d", dest, (int) getpid());
  new_name = tmp;

  systemDefaults.state = (char *) NULL;		/* does not need this any */
  closeFiles();

#if O_SAVE
  TRY(save(dest, old_name, RET_MAIN, 0, NULL));
#else
  Putf("Running program: %s\n", old_name);
  if ( sym_name != NULL )
    Putf("Taking symbol table from %s\n", sym_name);
  Putf("Saving to %s ... ", new_name); pl_flush();
  if ( unexec(new_name, old_name, sym_name, 0, 0, 0) < 0 )
  { RemoveFile(tmp);
    fail;
  }
  Putf("ok.\n");
  Putf("Moving %s to %s ... ", tmp, dest); pl_flush();
  if ( RenameFile(tmp, dest) == FALSE )
  { warning("Failed to install %s: %s", dest, OsError());
    RemoveFile(tmp);
    fail;
  }
#endif
  Putf("Program saved in %s\n", dest);

  succeed;
}

word
pl_save_program(term_t new, term_t options)
{ TRY(parseSaveProgramOptions(options,
			      &systemDefaults.local,
			      &systemDefaults.global,
			      &systemDefaults.trail,
			      &systemDefaults.argument,
			      &systemDefaults.goal,
			      &systemDefaults.toplevel,
			      &systemDefaults.startup,
			      &systemDefaults.notty,
			      NULL));

  return saveProgram(new);
}

#if O_SAVE

word
pl_save(term_t file, term_t restore)
{ char *state, *interpreter;
  char buf[MAXPATHLEN];
#if O_DYNAMIC_STACKS
  struct save_section sections[4];
#endif

  if ( !(state = PL_get_filename(file, buf, sizeof(buf))) )
    return warning("save/2: instantiation fault");

  TRY( getSymbols() );
  interpreter = stringAtom(loaderstatus.orgsymbolfile);
  
#if O_DYNAMIC_STACKS
#define fill_section(sec, stack) \
  { (sec)->start  = (caddr) (stack)->base; \
    (sec)->length = (unsigned long) (stack)->top - \
 		    (unsigned long) (stack)->base; \
    (sec)->type   = S_PLSTACK; \
    (sec)->flags  = 0; \
  }
  fill_section(&sections[0], &stacks.local);
  fill_section(&sections[1], &stacks.global);
  fill_section(&sections[2], &stacks.trail);
  fill_section(&sections[3], &stacks.argument);
#undef fill_section

  switch( save(state, interpreter, RET_RETURN, 4, sections) )
#else
  switch( save(state, interpreter, RET_RETURN, 0, NULL) )
#endif
  { case SAVE_SAVE:
      return PL_unify_integer(restore, 0);
    case SAVE_RESTORE:
#ifdef HAVE_SIGNAL
      initSignals();
#endif
      return PL_unify_integer(restore, 1);
    case SAVE_FAILURE:
    default:
      fail;
  }
}

#if O_DYNAMIC_STACKS
bool
allocateSection(SaveSection s)
{ if ( s->type == S_PLSTACK )
  { if ((stacks.local.base    == s->start &&
	 restoreStack((Stack) &stacks.local)) ||
	(stacks.global.base   == s->start &&
	 restoreStack((Stack) &stacks.global)) ||
	(stacks.trail.base    == s->start &&
	 restoreStack((Stack) &stacks.trail)) ||
	(stacks.argument.base == s->start &&
	 restoreStack((Stack) &stacks.argument)) )
      succeed;

    fatalError("Cannot locate stack to restore");
    fail;
  }

  succeed;
}
#endif

word
pl_restore(term_t file)
{ char *state;
  char buf[MAXPATHLEN];

  if ( !(state = PL_get_filename(file, buf, sizeof(buf))) )
    return warning("restore/1: instantiation fault");
  if ( !ExistsFile(state) )
    return warning("restore/1: no such file: %s", state);

#if O_DYNAMIC_STACKS
  deallocateStacks();
  restore(state, allocateSection);
#else
  restore(state, NULL);
#endif
  fatalError("restore/1: restore failed");
  fail;
}

#endif /* O_SAVE */

#if O_STORE_PROGRAM

#define NO_REMAP 1			/* Do not change data start */

#ifdef UNEXEC_SOURCE

#include UNEXEC_SOURCE

#else /* ~UNEXEC_SOURCE */

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
#else /* N_TXTADDR */
#ifdef TEXT_START
  return ((char *) TEXT_START);
#else
#if gould				/* GOULD machine */
  extern csrt();
  return ((char *) csrt);
#else /* gould */
  return (char *) hdr->a_entry;
#endif
#endif /* TEXT_START */
#endif /* N_TXTADDR */
}

#include "gnu/unexec.c"
#endif /* UNEXEC_SOURCE */
#endif /* O_STORE_PROGRAM */

#else /* O_STORE_PROGRAM || O_SAVE */

word
pl_save(term_t state, term_t rval)
{ return warning("save/2: not ported to this machine.  See qsave_program/[1,2]");
}


word
pl_restore(term_t state)
{ return warning("restore/1: not ported to this machine");
}


word
pl_save_program(term_t old, term_t new)
{ return warning("store_program/2: not ported to this machine.  See qsave_program/[1,2]");
}

#endif /* O_STORE_PROGRAM || O_SAVE */
