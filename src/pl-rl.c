/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Binding to the GNU readline library
*/

#ifndef __WIN32__
#include "pl-incl.h"
#endif
#include <string.h>
#include <stdlib.h>
#include "pl-stream.h"
#include "pl-itf.h"

#ifdef __WIN32__
#include "config/win32.h"
#else
#include <config.h>
#endif

#ifdef HAVE_LIBREADLINE

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_UXNT_H
#include <uxnt.h>
#endif
#ifdef HAVE_CLOCK
#include <time.h>
#endif
#ifdef __WIN32__
#include <io.h>
#endif
#ifdef O_RLC
#include <console.h>
#endif

#ifdef HAVE_RL_INSERT_CLOSE
#define PAREN_MATCHING 1
extern rl_delete_text(int from, int to);
#endif

#undef ESC				/* will be redefined ... */
#include <stdio.h>			/* readline needs it */
#define savestring(x)			/* avoid definition there */
#include <readline/readline.h>
extern void add_history(char *);	/* should be in readline.h */
extern rl_begin_undo_group(void);	/* delete when conflict arrises! */
extern rl_end_undo_group(void);
extern Function *rl_event_hook;
extern char *filename_completion_function(char *, int);


static foreign_t
pl_rl_read_init_file(term_t file)
{ char *f;

  if ( (f = PL_get_filename(file, NULL, 0)) )
  {
#ifdef O_XOS
    char buf[MAXPATHLEN];
    rl_read_init_file(_xos_os_filename(f, buf));
#else
    rl_read_init_file(f);
#endif

    PL_succeed;
  }

  PL_fail;
}


static foreign_t
pl_rl_add_history(term_t text)
{ char *s;

  if ( PL_get_chars(text, &s, CVT_ALL) )
  { add_history(s);

    PL_succeed;
  }

  return PL_warning("rl_add_history/1: instantation fault");
}


static int
event_hook()
{ return PL_dispatch(0, PL_DISPATCH_NOWAIT);
}


static int
Sread_readline(void *handle, char *buf, int size)
{ int fd = (int) handle;
  int ttymode = PL_ttymode(fd);
  int rval;
#ifdef HAVE_CLOCK
  long oldclock = clock();
#endif

  PL_write_prompt(fd, ttymode == PL_NOTTY);
  
  switch( ttymode )
  { case PL_RAWTTY:			/* get_single_char/1 */
#ifdef O_RLC
    { int chr = getkey();
      
      if ( chr == 04 || chr == 26 )
	return 0;			/* EOF */

      buf[0] = chr & 0xff;
      return 1;
    }
#endif
    case PL_NOTTY:			/* -tty */
    { PL_dispatch(fd, PL_DISPATCH_WAIT);
      rval = read(fd, buf, size);
      if ( rval > 0 && buf[rval-1] == '\n' )
	PL_prompt_next(fd);

      break;
    }
    case PL_COOKEDTTY:
    default:
    { char *line;

      if ( PL_dispatch(0, PL_DISPATCH_INSTALLED) )
	rl_event_hook = event_hook;
      else
	rl_event_hook = NULL;

      if ( (line = readline(PL_prompt_string(fd))) )
      { char *s;
	int l = strlen(line);
	  
	if ( l > size )
	{ PL_warning("Input line too long");	/* must be tested! */
	  l = size-1;
	}
	memcpy(buf, line, l);
	buf[l++] = '\n';
	rval = l;

	for(s = line; *s; s++)
	{ if ( (*s & 0xff) > ' ' )
	    break;
	}

        if ( !*s )			/* blanks only! */
	  free(line);
      } else
	rval = 0;
    }
  }

  if ( ttymode != PL_RAWTTY )
  { PL_add_to_protocol(buf, rval);
  }

#ifdef HAVE_CLOCK
  PL_clock_wait_ticks(clock() - oldclock);
#endif

  return rval;
}


static void
prolog_complete(int ignore, int key)
{ if ( rl_point > 0 && rl_line_buffer[rl_point-1] != ' ' )
  { rl_begin_undo_group();
    rl_complete(ignore, key);
    if ( rl_point > 0 && rl_line_buffer[rl_point-1] == ' ' )
    {
#ifdef HAVE_RL_INSERT_CLOSE		/* actually version >= 1.2 */
      rl_delete_text(rl_point-1, rl_point);
      rl_point -= 1;
#else
      rl_delete(-1);
#endif
    }
    rl_end_undo_group();
  } else
    rl_complete(ignore, key);
}


static char **
prolog_completion(char *text, int start, int end)
{ char **matches = NULL;

  if ( (start == 1 && rl_line_buffer[0] == '[') )	/* [file */
    matches = completion_matches(text,
				 (Function *) filename_completion_function);
  else if (start == 2 && strncmp(rl_line_buffer, "['", 2))
    matches = completion_matches(text,
				 (Function *) filename_completion_function);
  else
    matches = completion_matches(text, PL_atom_generator);

  return matches;
}

#undef read				/* UXNT redefinition */

install_t
PL_install_readline()
{ static IOFUNCTIONS funcs;

#ifndef __WIN32__
  if ( status.notty || !isatty(0) )
    return;
#endif

  rl_readline_name = "Prolog";
  rl_attempted_completion_function = prolog_completion;
#ifdef __WIN32__
  rl_basic_word_break_characters = "\t\n\"\\'`@$><= [](){}+*!,|%&?";
#else
  rl_basic_word_break_characters = ":\t\n\"\\'`@$><= [](){}+*!,|%&?";
#endif
  rl_add_defun("prolog-complete", (Function *) prolog_complete, '\t');
#if HAVE_RL_INSERT_CLOSE
  rl_add_defun("insert-close", rl_insert_close, ')');
#endif

  funcs = *Sinput->functions;		/* structure copy */
  funcs.read = Sread_readline;		/* read through readline */

  Sinput->functions  = &funcs;
  Soutput->functions = &funcs;
  Serror->functions  = &funcs;

  PL_register_foreign("rl_read_init_file", 1, pl_rl_read_init_file, 0);
  PL_register_foreign("rl_add_history",    1, pl_rl_add_history,    0);
  PL_set_feature("readline", PL_ATOM, "true");
  PL_set_feature("tty_control", PL_ATOM, "true");
}

#else /*HAVE_LIBREADLINE*/

install_t
PL_install_readline()
{
}

#endif /*HAVE_LIBREADLINE*/
