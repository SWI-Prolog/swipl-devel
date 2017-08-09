/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifdef __MINGW32__
#include <winsock2.h>
#include <windows.h>
#endif

#define bool pl_bool			/* avoid conflict with curses */
#include "pl-incl.h"
#undef bool

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines some hacks to get to the unix  termcap  library.   I
realise this is not a proper answer to terminal control from Prolog, but
I  needed  it  some day and at least it is better than doing things like
shell(clear), coding terminal sequences hard, etc.   One  day  I  should
write a decent interface to handle the terminal.  Maybe this will be too
late;  character terminals  disappear quickly now.  Use XPCE if you want
windowing!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_TGETENT
#ifndef NO_SYS_IOCTL_H_WITH_SYS_TERMIOS_H
#include <sys/ioctl.h>
#endif

#undef clear				/* conflicts */

#ifdef HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#endif

#ifdef HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#endif

#define MAX_TERMBUF	1024		/* Conforming manual */
#define STAT_START	0		/* must be FALSE */
#define STAT_OK		1
#define STAT_ERROR	2

#ifdef TERMCAP_NEEDS_VARS
char	PC;				/* Term lib variables */
char   *BC;
char   *UP;
short	ospeed;
#endif

#define term_initialised    (GD->terminal.initialised)
#define string_area	    (GD->terminal._string_area)
#define buf_area	    (GD->terminal._buf_area)
#define capabilities        (GD->terminal._capabilities)

typedef struct
{ atom_t type;				/* type of the entry */
  atom_t name;				/* Name of the value */
  word  value;				/* Value of the entry */
} entry, *Entry;


void
cleanupTerm(void)
{ Table t;

  if ( (t=capabilities) )
  { capabilities = NULL;
    for_table(t, name, value,
	      freeHeap(value, sizeof(entry)));
    destroyHTable(t);
  }

  term_initialised = STAT_START;
}


/* MT: Locked by calling lookupEntry()
*/

static int
initTerm(void)
{ GET_LD

  if ( term_initialised == STAT_START )
  { char term[100];

    if ( !capabilities )
      capabilities = newHTable(16);

    term_initialised = STAT_ERROR;
    if ( !Getenv("TERM", term, sizeof(term)) )
    { term_t env = PL_new_term_ref();

      PL_put_atom_chars(env, "TERM");
      PL_error(NULL, 0, NULL, ERR_EXISTENCE,
	       ATOM_environment, env);
      goto out;
    }

    if ( buf_area == NULL )
      buf_area = allocHeapOrHalt(MAX_TERMBUF);
    if ( string_area == NULL )
      string_area = allocHeapOrHalt(MAX_TERMBUF);

    switch( tgetent(buf_area, term) )
    { case -1:
      { term_t t = PL_new_term_ref();

	PL_put_atom_chars(t, "termcap");
	PL_error(NULL, 0, "tgetent() cannot read database",
		 ERR_PERMISSION,
		 ATOM_file, ATOM_read, t);
	goto out;
      }
      case  1:
	break;
      default:
      case  0:
      { term_t t = PL_new_term_ref();

	PL_put_atom_chars(t, term);
	PL_error(NULL, 0, NULL, ERR_EXISTENCE,
		 ATOM_terminal, t);
	goto out;
      }
    }

    term_initialised = STAT_OK;
  }

out:
  return term_initialised == STAT_OK;
}


static Entry
lookupEntry(atom_t name, atom_t type)
{ GET_LD
  Entry e;

  PL_LOCK(L_TERM);
  if ( !capabilities ||
       !(e = lookupHTable(capabilities, (void*)name)) )
  { if ( !initTerm() )
    { e = NULL;
      goto out;
    }

    e = (Entry) allocHeapOrHalt(sizeof(entry));
    e->name = name;
    e->type = type;
    e->value = 0L;

    if ( type == ATOM_number )
    { int n;

      if ( (n = tgetnum(stringAtom(name))) != -1 )
        e->value  = consInt(n);
    } else if ( type == ATOM_bool )
    { int b;

      if ( (b = tgetflag(stringAtom(name))) != -1 )
        e->value = (b ? ATOM_on : ATOM_off);
    } else if ( type == ATOM_string )
    { char *s;

      if ( (s = tgetstr(stringAtom(name), &string_area)) != NULL )
        e->value  = PL_new_atom(s);	/* locked: ok */
    } else
    { term_t t = PL_new_term_ref();
      PL_put_atom(t, type);
      PL_domain_error("termcap_type", t);
      freeHeap(e, sizeof(entry));
      e = NULL;
      goto out;
    }

    addNewHTable(capabilities, (void *)name, e);
  }

out:
  PL_UNLOCK(L_TERM);
  return e;
}


static
PRED_IMPL("tty_get_capability", 3, tty_get_capability, 0)
{ PRED_LD
  Entry e;
  atom_t n, t;

  term_t name  = A1;
  term_t type  = A2;
  term_t value = A3;

  if ( !PL_get_atom_ex(name, &n) || !PL_get_atom_ex(type, &t) )
    fail;
  if ( !(e = lookupEntry(n, t)) )
    fail;

  if ( e->value != 0L )
    return _PL_unify_atomic(value, e->value);

  fail;
}


static int
tputc(int chr)
{ GET_LD
  return Sputc(chr, Suser_output);
}


static
PRED_IMPL("tty_goto", 2, tty_goto, 0)
{ PRED_LD
  Entry e;
  char *s;
  int ix, iy;

  if ( !PL_get_integer_ex(A1, &ix) ||
       !PL_get_integer_ex(A2, &iy) )
    fail;

  if ( (e = lookupEntry(ATOM_cm, ATOM_string)) == NULL ||
        e->value == 0L )
  { term_t obj = PL_new_term_ref();

    PL_put_atom(obj, ATOM_cm);
    return PL_error("tty_goto", 2, NULL, ERR_EXISTENCE,
		    ATOM_terminal_capability, obj);
  }

  s = tgoto(stringAtom(e->value), ix, iy);
  if ( streq(s, "OOPS") )
    fail;

  tputs(s, 1, tputc);
  succeed;
}


static
PRED_IMPL("tty_put", 2, pl_tty_put, 0)
{ char *s;
  int n;

  term_t a = A1;
  term_t affcnt = A2;

  if ( PL_get_chars(a, &s, CVT_ALL|CVT_EXCEPTION) &&
       PL_get_integer_ex(affcnt, &n) )
  { tputs(s, n, tputc);
    succeed;
  }

  fail;
}


#define HAVE_TTY_SIZE_PRED 1

static
PRED_IMPL("tty_size", 2, tty_size, 0)
{ PRED_LD
  int rows, cols;
  int fd = Sfileno(Suser_input);

  term_t r = A1;
  term_t c = A2;

  if ( fd < 0 )
    fd = 0;

#ifdef __unix__
  int iorval;

#ifdef TIOCGSIZE
  struct ttysize ws;
  iorval = ioctl(fd, TIOCGSIZE, &ws);

  rows = ws.ts_lines;
  cols = ws.ts_cols;
#else
#ifdef TIOCGWINSZ
  struct winsize ws;
  iorval = ioctl(fd, TIOCGWINSZ, &ws);

  rows = ws.ws_row;
  cols = ws.ws_col;
#else
  Entry er, ec;

  if ( (er=lookupEntry(ATOM_li, ATOM_number)) &&
       (ec=lookupEntry(ATOM_co, ATOM_number)) &&
       er->value && ec->value )
  { rows = valInt(er->value);
    cols = valInt(ec->value);
    iorval = 0;
  } else
    iorval = -1;
#endif
#endif

  if ( iorval != 0 )
    return PL_error("tty_size", 2, MSG_ERRNO, ERR_SYSCALL, "ioctl");
#else /*__unix__*/
  rows = ScreenRows();			/* old stuff refering to plterm.dll */
  cols = ScreenCols();			/* not used anyway */
#endif /*__unix__*/

  return PL_unify_integer(r, rows) &&
	 PL_unify_integer(c, cols);
}

#else /* ~TGETENT */

#ifdef __WINDOWS__

static void *
getModuleFunction(const char *module, const char *name)
{ HMODULE hconsole;

  if ( (hconsole=GetModuleHandle(module)) )
  { return GetProcAddress(hconsole, name);
  }

  return NULL;
}

#define HAVE_TTY_SIZE_PRED 1

static
PRED_IMPL("tty_size", 2, tty_size, 0)
{ PRED_LD
  int (*ScreenCols)(void *h) = getModuleFunction("plterm", "ScreenCols");
  int (*ScreenRows)(void *h) = getModuleFunction("plterm", "ScreenRows");

  term_t r = A1;
  term_t c = A2;

  if ( ScreenCols && ScreenRows )
  { void *(*get_console)(void) = getModuleFunction(NULL, "PL_current_console");
    void *con = (get_console ? (*get_console)() : NULL);
    int rows = (*ScreenRows)(con);
    int cols = (*ScreenCols)(con);

    if ( PL_unify_integer(r, rows) &&
	 PL_unify_integer(c, cols) )
      succeed;

    fail;
  }

  return notImplemented("tty_size", 2);
}

#define HAVE_PL_TTY_SIZE 1

#endif /*__WINDOWS__*/

void resetTerm(void)
{
}

void cleanupTerm(void)
{
}

#endif /* TGETENT */


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(term)
#ifdef HAVE_TGETENT
  PRED_DEF("tty_get_capability", 3, tty_get_capability,	0)
  PRED_DEF("tty_goto",		 2, tty_goto,		0)
  PRED_DEF("tty_put",		 2, pl_tty_put,		0)
#endif
#if HAVE_TTY_SIZE_PRED
  PRED_DEF("tty_size",		 2, tty_size,		0)
#endif
EndPredDefs
