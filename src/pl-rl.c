/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module binds the  SWI-Prolog  terminal   I/O  to  the  GNU readline
library. Existence of this  this  library   is  detected  by  configure.
Binding is achieved by rebinding the read function of the Sinput stream. 

This  module  only  depends  on  the  public  interface  as  defined  by
SWI-Prolog.h (pl-itf.h) and SWI-Stream.h (pl-strea.h).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


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

#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_READLINE_H)

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
extern int rl_delete_text(int from, int to);
#endif

#undef ESC				/* will be redefined ... */
#include <stdio.h>			/* readline needs it */
#define savestring(x)			/* avoid definition there */
#include <readline/readline.h>
extern void add_history(char *);	/* should be in readline.h */
extern int rl_begin_undo_group(void);	/* delete when conflict arrises! */
extern int rl_end_undo_group(void);
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
{ return PL_dispatch(0, PL_DISPATCH_WAIT);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The GNU-readline library is not reentrant (or does not appear to be so).
Therefore we will detect this and simply   call  the default function if
reentrant access is tried.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int in_readline = 0;

static void
reset_readline()
{ in_readline = 0;
}


static int
Sread_readline(void *handle, char *buf, int size)
{ long h = (long)handle;
  int fd = (int) h;
  int ttymode = PL_ttymode(fd);
  int rval;
#ifdef HAVE_CLOCK
  long oldclock = clock();
#endif

  PL_write_prompt(ttymode == PL_NOTTY);
  
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
    notty:
    { PL_dispatch(fd, PL_DISPATCH_WAIT);
      rval = read(fd, buf, size);
      if ( rval > 0 && buf[rval-1] == '\n' )
	PL_prompt_next(fd);

      break;
    }
    case PL_COOKEDTTY:
    default:
    { char *line;
      char *prompt;

      if ( in_readline )
      { Sprintf("[readline disabled] ");
	PL_write_prompt(TRUE);
	goto notty;			/* avoid reentrance */
      }

      if ( PL_dispatch(0, PL_DISPATCH_INSTALLED) )
	rl_event_hook = event_hook;
      else
	rl_event_hook = NULL;

      prompt = PL_prompt_string(fd);
      if ( prompt )
	PL_add_to_protocol(prompt, strlen(prompt));

      in_readline++;
      line = readline(prompt);
      in_readline--;

      if ( line )
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


static char *
atom_generator(char *prefix, int state)
{ char *s = PL_atom_generator(prefix, state);
  
  if ( s )
    return strcpy(xmalloc(1 + strlen(s)), s);

  return s;
}


static char **
prolog_completion(char *text, int start, int end)
{ char **matches = NULL;

  if ( (start == 1 && rl_line_buffer[0] == '[') )	/* [file */
    matches = completion_matches(text,
				 (void *) filename_completion_function);
  else if (start == 2 && strncmp(rl_line_buffer, "['", 2))
    matches = completion_matches(text,
				 (void *) filename_completion_function);
  else
    matches = completion_matches(text, atom_generator);

  return matches;
}

#undef read				/* UXNT redefinition */

install_t
PL_install_readline()
{ 
#ifndef __WIN32__
  if ( !trueFeature(TTY_CONTROL_FEATURE) || !isatty(0) )
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

  GD->os.rl_functions = *Sinput->functions;	/* structure copy */
  GD->os.rl_functions.read = Sread_readline;	/* read through readline */

  Sinput->functions  = &GD->os.rl_functions;
  Soutput->functions = &GD->os.rl_functions;
  Serror->functions  = &GD->os.rl_functions;

  PL_register_foreign("rl_read_init_file", 1, pl_rl_read_init_file, 0);
  PL_register_foreign("rl_add_history",    1, pl_rl_add_history,
		      PL_FA_NOTRACE);
  PL_set_feature("readline",    PL_BOOL, TRUE);
  PL_set_feature("tty_control", PL_BOOL, TRUE);
  PL_abort_hook(reset_readline);
}

#else /*HAVE_LIBREADLINE*/

install_t
PL_install_readline()
{
}

#endif /*HAVE_LIBREADLINE*/
