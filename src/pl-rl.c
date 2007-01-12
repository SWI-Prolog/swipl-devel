/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module binds the  SWI-Prolog  terminal   I/O  to  the  GNU readline
library. Existence of this  this  library   is  detected  by  configure.
Binding is achieved by rebinding the read function of the Sinput stream. 

This  module  only  depends  on  the  public  interface  as  defined  by
SWI-Prolog.h (pl-itf.h) and SWI-Stream.h (pl-stream.h).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#ifndef __WINDOWS__
#include "pl-incl.h"
#endif
#include <string.h>
#include <stdlib.h>
#include "pl-stream.h"
#include "pl-itf.h"

#ifdef __WINDOWS__
#include "config/win32.h"
#else
#include <config.h>
#endif

#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_READLINE_H)

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_UXNT_H
#include <uxnt.h>
#endif
#ifdef HAVE_CLOCK
#include <time.h>
#endif
#ifdef __WINDOWS__
#include <io.h>
#endif
#ifdef O_RLC
#include "win32/console/console.h"
#endif

#ifdef HAVE_RL_INSERT_CLOSE
#define PAREN_MATCHING 1
#endif

#undef ESC				/* will be redefined ... */
#include <stdio.h>			/* readline needs it */
#define savestring(x)			/* avoid definition there */
#include <readline/readline.h>
extern int rl_done;			/* should be in readline.h, but */
					/* isn't in some versions ... */
#ifdef HAVE_READLINE_HISTORY_H
#include <readline/history.h>
#else
extern void add_history(char *);	/* should be in readline.h */
#endif
					/* missing prototypes in older */
					/* readline.h versions */
extern int rl_begin_undo_group(void);	/* delete when conflict arrises! */
extern int rl_end_undo_group(void);
extern Function *rl_event_hook;
#ifndef HAVE_RL_FILENAME_COMPLETION_FUNCTION
#define rl_filename_completion_function filename_completion_function
extern char *filename_completion_function(const char *, int);
#endif

#ifndef HAVE_RL_COMPLETION_MATCHES
#define rl_completion_matches completion_matches
#endif

#ifndef HAVE_RL_READLINE_STATE		/* before version 4.2 */
static int rl_readline_state = 0;
#define RL_STATE_INITIALIZED 0
#endif
#ifndef HAVE_RL_SET_PROMPT
#define rl_set_prompt(x) (void)0
#endif
#ifndef RL_CLEAR_PENDING_INPUT
#define rl_clear_pending_input() (void)0
#endif
#ifndef RL_CLEANUP_AFTER_SIGNAL
#define rl_cleanup_after_signal() (void)0
#endif

static foreign_t
pl_rl_read_init_file(term_t file)
{ char *f;

  if ( PL_get_file_name(file, &f, 0) )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is not really clear using   wide-character  handling. We now assume
that the readline library can only do   wide characters using UTF-8. Not
sure this is true, but is certainly covers most installations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_rl_add_history(term_t text)
{ atom_t a;
  static atom_t last = 0;
  PL_chars_t txt;

  if ( PL_get_atom_ex(text, &a) )
  { if ( a != last )
    { if ( last )
	PL_unregister_atom(last);
      last = a;
      PL_register_atom(last);

      PL_get_text(text, &txt, CVT_ATOM);
      PL_text_recode(&txt, ENC_UTF8);

      add_history(txt.text.t);
      PL_free_text(&txt);
    }

    return TRUE;
  }

  return FALSE;
}


static int
input_on_fd(int fd)
{ fd_set rfds;
  struct timeval tv;

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
}


static char *my_prompt   = NULL;
static int   in_readline = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Signal handling wrapper.

This is tricky. The GNU readline   library places signal handlers around
the   below   given   signals.   They    re-send   all   signals   using
kill(getpid(),sig). This goes wrong in our  multi-threaded context as it
will send signals meant for a thread (using pthread_kill()) to the wrong
thread. The library time.pl from the  clib   package  was victim of this
behaviour.

We disable readline's signal handling  using   rl_catch_signals  = 0 and
redo the work ourselves, where we call   the handler directly instead of
re-sending the signal. See  "info  readline"   for  details  on readline
signal handling issues.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ int			signo;		/* number of the signal */
  struct sigaction	old_state;	/* old state for the signal */
} sigstate;

static void rl_sighandler(int sig);

static sigstate signals[] =
{ { SIGINT },
#ifdef SIGTSTP
  { SIGTSTP },
  { SIGTTOU },
  { SIGTTIN },
#endif
  { SIGALRM },
  { SIGTERM },
  { SIGQUIT },
  { -1 },
};


static void
prepare_signals()
{ sigstate *s;

  for(s=signals; s->signo != -1; s++)
  { struct sigaction new;

    memset(&new, 0, sizeof(new));	
    new.sa_handler = rl_sighandler;
    sigaction(s->signo, &new, &s->old_state);
  }
}


static void
restore_signals()
{ sigstate *s;

  for(s=signals; s->signo != -1; s++)
  { sigaction(s->signo, &s->old_state, NULL);
  }
}


static void
rl_sighandler(int sig)
{ sigstate *s;

  DEBUG(3, Sdprintf("Signal %d in readline\n", sig));

  if ( sig == SIGINT )
    rl_free_line_state ();
  rl_cleanup_after_signal ();
  restore_signals();

  for(s=signals; s->signo != -1; s++)
  { if ( s->signo == sig )
    { void (*func)(int) = s->old_state.sa_handler;

      if ( func == SIG_DFL )
      { unblockSignal(sig);
	DEBUG(3, Sdprintf("Re-sending signal\n"));
	kill(getpid(), sig);
      } else if ( func != SIG_IGN )
      { (*func)(sig);
      }

      break;
    }
  }

  DEBUG(3, Sdprintf("Resetting after signal\n"));
  prepare_signals();
  rl_reset_after_signal ();
  Sreset();
}


static char *
pl_readline(const char *prompt)
{ char *line;

  prepare_signals();
  line = readline(prompt);
  restore_signals();

  return line;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The GNU-readline library is not reentrant (or does not appear to be so).
Therefore we will detect this and simply   call  the default function if
reentrant access is tried.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_RL_EVENT_HOOK
static int
event_hook()
{ if ( Sinput && Sinput->position )
  { intptr_t c0 = Sinput->position->charno;

    while( !input_on_fd(0) )
    { PL_dispatch(0, PL_DISPATCH_NOWAIT);
      if ( Sinput->position->charno != c0 )
      { if ( my_prompt )
	  rl_set_prompt(my_prompt);
	rl_forced_update_display();
	c0 = Sinput->position->charno;
#ifdef HAVE_RL_DONE
	rl_done = FALSE;
#endif
      }
    }
  } else
    PL_dispatch(0, PL_DISPATCH_WAIT);

  return TRUE;
}
#endif


static void
reset_readline()
{ if ( in_readline )
    rl_cleanup_after_signal();

  if ( my_prompt )
    remove_string(my_prompt);
  my_prompt = NULL;
  in_readline = 0;
}


static ssize_t
Sread_readline(void *handle, char *buf, size_t size)
{ intptr_t h = (intptr_t)handle;
  int fd = (int) h;
  int ttymode = PL_ttymode(Suser_input); /* Not so nice */
  int rval;
#ifdef HAVE_CLOCK
  intptr_t oldclock = clock();
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
#ifdef RL_NO_REENTRANT
    notty:
#endif
    { PL_dispatch(fd, PL_DISPATCH_WAIT);
      rval = read(fd, buf, size);
      if ( rval > 0 && buf[rval-1] == '\n' )
	PL_prompt_next(fd);

      break;
    }
    case PL_COOKEDTTY:
    default:
    { char *line;
      const char *prompt;

#ifdef RL_NO_REENTRANT
      if ( in_readline )
      { Sprintf("[readline disabled] ");
	PL_write_prompt(TRUE);
	goto notty;			/* avoid reentrance */
      }
#endif

#ifdef HAVE_RL_EVENT_HOOK
      if ( PL_dispatch(0, PL_DISPATCH_INSTALLED) )
	rl_event_hook = event_hook;
      else
	rl_event_hook = NULL;
#endif

      prompt = PL_prompt_string(fd);
      if ( prompt )
	PL_add_to_protocol(prompt, strlen(prompt));

      { char *oldp = my_prompt;

	my_prompt = prompt ? store_string(prompt) : (char *)NULL;
  
	if ( in_readline++ )
	{ int state = rl_readline_state;
  
	  rl_clear_pending_input();
	  rl_discard_argument();
	  rl_deprep_terminal();
	  rl_readline_state = (RL_STATE_INITIALIZED);
	  line = pl_readline(prompt);
	  rl_prep_terminal(FALSE);
	  rl_readline_state = state;
#ifdef HAVE_RL_DONE
	  rl_done = 0;
#endif
	} else
	  line = pl_readline(prompt);
	in_readline--;

	if ( my_prompt )
	  remove_string(my_prompt);
	my_prompt = oldp;
      }

      if ( line )
      { int l = strlen(line);
	  
	if ( l >= size )
	{ PL_warning("Input line too intptr_t");	/* must be tested! */
	  l = size-1;
	}
	memcpy(buf, line, l);
	buf[l++] = '\n';
	rval = l;

	/*Sdprintf("Read: '%s'\n", line);*/
	free(line);
      } else
	rval = 0;
    }
  }

#ifdef HAVE_CLOCK
  PL_clock_wait_ticks(clock() - oldclock);
#endif

  return rval;
}


static int
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

  return 0;
}


static char *
atom_generator(const char *prefix, int state)
{ char *s = PL_atom_generator(prefix, state);
  
  if ( s )
    return strcpy(PL_malloc(1 + strlen(s)), s);

  return s;
}


static char **
prolog_completion(const char *text, int start, int end)
{ char **matches = NULL;

  if ( (start == 1 && rl_line_buffer[0] == '[') ||	/* [file */
       (start == 2 && strncmp(rl_line_buffer, "['", 2)) )
    matches = rl_completion_matches((char *)text,	/* for pre-4.2 */
				    rl_filename_completion_function);
  else
    matches = rl_completion_matches((char *)text,
				    atom_generator);

  return matches;
}

#undef read				/* UXNT redefinition */

install_t
PL_install_readline()
{ 
#ifndef __WINDOWS__
  if ( !trueFeature(TTY_CONTROL_FEATURE) || !isatty(0) )
    return;
#endif

  rl_catch_signals = 0;
  rl_readline_name = "Prolog";
  rl_attempted_completion_function = prolog_completion;
#ifdef __WINDOWS__
  rl_basic_word_break_characters = "\t\n\"\\'`@$><= [](){}+*!,|%&?";
#else
  rl_basic_word_break_characters = ":\t\n\"\\'`@$><= [](){}+*!,|%&?";
#endif
  rl_add_defun("prolog-complete", prolog_complete, '\t');
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
  PL_license("gpl", "GNU Readline library");
}

#else /*HAVE_LIBREADLINE*/

install_t
PL_install_readline()
{
}

#endif /*HAVE_LIBREADLINE*/
