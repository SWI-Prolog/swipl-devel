/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"

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

extern int  tgetent();
extern int  tgetnum();
extern int  tgetflag();
extern char *tgetstr();
extern char *tgoto();
extern int  tputs();

#define MAX_TERMBUF	1024		/* Confirming manual */
#define STAT_START	0
#define STAT_OK		1
#define STAT_ERROR	2

#ifdef TERMCAP_NEEDS_VARS
char	PC;				/* Term lib variables */
char   *BC;
char   *UP;
short	ospeed;
#endif

static int	term_initialised;	/* Extracted term info? */
static char     *string_area_pointer;	/* Current location */
static Table	capabilities;		/* Terminal capabilities */

typedef struct
{ atom_t type;				/* type of the entry */
  atom_t name;				/* Name of the value */
  word  value;				/* Value of the entry */
} entry, *Entry;

forwards bool	initTerm(void);

void
resetTerm()
{ if ( capabilities == NULL )
  { capabilities = newHTable(16);
  } else
  { term_initialised = STAT_START;
    for_table(capabilities, s,
	      freeHeap(s->value, sizeof(entry)));
    clearHTable(capabilities);
  }
}

static bool
initTerm(void)
{ static char *buf = NULL;
  static char *string_area = NULL;

  if ( term_initialised == STAT_START )
  { char term[100];

    term_initialised = STAT_ERROR;
    if ( !getenv3("TERM", term, sizeof(term)) )
    { term_t env = PL_new_term_ref();

      PL_put_atom_chars(env, "TERM");
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE,
		      ATOM_environment, env);
    }

    if ( buf == NULL )         buf         = allocHeap(MAX_TERMBUF);
    if ( string_area == NULL ) string_area = allocHeap(MAX_TERMBUF);
    string_area_pointer = string_area;

    switch( tgetent(buf, term) )
    { case -1:	return warning("Cannot open termcap file");
      case  1:	break;
      default:
      case  0:	return warning("Unknown terminal: %s", term);
    }

    term_initialised = STAT_OK;
  }

  return term_initialised == STAT_OK;
}

static Entry
lookupEntry(atom_t name, atom_t type)
{ Symbol s;
  Entry e;

  if ( (s = lookupHTable(capabilities, (void*)name)) == NULL )
  { if ( initTerm() == FALSE )
      return NULL;

    e = (Entry) allocHeap(sizeof(entry));
    e->name = name;
    e->type = type;
    e->value = 0L;

    if ( type == ATOM_number )
    { int n;

      if ( (n = tgetnum(stringAtom(name))) != -1 )
        e->value  = consInt(n);
    } else if ( type == ATOM_bool )
    { bool b;
    
      if ( (b = tgetflag(stringAtom(name))) != -1 )
        e->value = (b ? ATOM_on : ATOM_off);
    } else if ( type == ATOM_string )
    { char *s;
    
      if ( (s = tgetstr(stringAtom(name), &string_area_pointer)) != NULL )
        e->value  = PL_new_atom(s);	/* locked: ok */
    } else
    { warning("tgetent/3: Illegal type");
      freeHeap(e, sizeof(entry));
      return NULL;
    }

    addHTable(capabilities, (void *)name, e);
    return e;
  } else
    return (Entry) s->value;
}
      
word
pl_tty_get_capability(term_t name, term_t type, term_t value)
{ Entry e;
  atom_t n, t;

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
{ return Sputc(chr, Suser_output); 
}


word
pl_tty_goto(term_t x, term_t y)
{ Entry e;
  char *s;
  int ix, iy;

  if ( !PL_get_integer_ex(x, &ix) ||
       !PL_get_integer_ex(y, &iy) )
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

word
pl_tty_put(term_t a, term_t affcnt)
{ char *s;
  int n;

  if ( PL_get_chars_ex(a, &s, CVT_ALL) &&
       PL_get_integer_ex(affcnt, &n) )
  { tputs(s, n, tputc);
    succeed;
  }

  fail;
}


word
pl_tty_size(term_t r, term_t c)
{ int rows, cols;

#ifdef __unix__
  int iorval;

#ifdef TIOCGSIZE
  struct ttysize ws;
  iorval = ioctl(0, TIOCGSIZE, &ws);
	
  rows = ws.ts_lines;
  cols = ws.ts_cols;
#else
#ifdef TIOCGWINSZ
  struct winsize ws;
  iorval = ioctl(0, TIOCGWINSZ, &ws);

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
    return PL_error("tty_size", 2, MSG_ERRNO, ERR_SYSCALL, ATOM_ioctl);
#else /*__unix__*/
  rows = ScreenRows();			/* old stuff refering to plterm.dll */
  cols = ScreenCols();			/* not used anyway */
#endif /*__unix__*/

  return PL_unify_integer(r, rows) &&
	 PL_unify_integer(c, cols);
}

#else /* ~TGETENT */

void resetTerm()
{
}

word
pl_tty_get_capability(term_t name, term_t type, term_t value)
{ return notImplemented("tty_get_capability", 3);
}

word
pl_tty_goto(term_t x, term_t y)
{ return notImplemented("tty_goto", 2);
}

word
pl_tty_put(term_t a, term_t affcnt)
{ return notImplemented("tty_put", 2);
}

word
pl_set_tty(term_t old, term_t new)
{ return notImplemented("set_tty", 2);
}

word
pl_tty_size(term_t r, term_t c)
{ return notImplemented("tty_size", 2);
}

#endif /* TGETENT */
