/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Simple terminal handling
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

extern int Output;			/* Current output stream */
char	PC;				/* Term lib variables */
char   *BC;
char   *UP;
short	ospeed;

static int	term_initialised;	/* Extracted term info? */
static char     *string_area_pointer;	/* Current location */
static Table	capabilities;		/* Terminal capabilities */
static atom_t	tty_stream;		/* stream on which to do tty */

typedef struct
{ atom_t type;				/* type of the entry */
  atom_t name;				/* Name of the value */
  word  value;				/* Value of the entry */
} entry, *Entry;

forwards bool	initTerm(void);
forwards Entry	lookupEntry(atom_t, atom_t);

void
resetTerm()
{ if ( capabilities == NULL )
  { capabilities = newHTable(16);
  } else
  { Symbol s;

    term_initialised = STAT_START;
    for_table(s, capabilities)
      freeHeap(s->value, sizeof(entry));
    clearHTable(capabilities);
  }

  tty_stream = ATOM_user_output;
}

static bool
initTerm(void)
{ static char *buf = NULL;
  static char *string_area = NULL;

  if ( term_initialised == STAT_START )
  { char term[100];

    term_initialised = STAT_ERROR;
    if ( !getenv3("TERM", term, sizeof(term)) )
      return warning("No variable TERM");

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
        e->value  = lookupAtom(s);
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

  if ( !PL_get_atom(name, &n) || !PL_get_atom(type, &t) )
    return warning("tgetent/3: instantiation fault");
  if ( !(e = lookupEntry(n, t)) )
    fail;

  if ( e->value != 0L )
    return _PL_unify_atomic(value, e->value);

  fail;
}
  
word
pl_tty_goto(term_t x, term_t y)
{ Entry e;
  char *s;
  int ix, iy;
  term_t ttys = PL_new_term_ref();

  if ( !PL_get_integer(x, &ix) ||
       !PL_get_integer(y, &iy) )
    return warning("tty_goto: instantiation fault");

  if ( (e = lookupEntry(ATOM_cm, ATOM_string)) == NULL ||
        e->value == 0L )
    fail;

  s = tgoto(stringAtom(e->value), ix, iy);
  if ( streq(s, "OOPS") )
    fail;

  PL_put_atom(ttys, tty_stream);
  streamOutput(ttys, (tputs(s, 1, Put), TRUE));
}

word
pl_tty_put(term_t a, term_t affcnt)
{ char *s;
  int n;

  if ( PL_get_chars(a, &s, CVT_ALL) &&
       PL_get_integer(affcnt, &n) )
  { term_t ttys = PL_new_term_ref();
    PL_put_atom(ttys, tty_stream);

    streamOutput(ttys, (tputs(s, n, Put), TRUE));
  }

  return warning("tty_put: instantiation fault");
}

word
pl_set_tty(term_t old, term_t new)
{ atom_t a;

  if ( PL_unify_atom(old, tty_stream) &&
       PL_get_atom(new, &a) &&
       streamNo(new, F_WRITE) >= 0 )
  { tty_stream = a;
    succeed;
  }

  fail;
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

#endif /* TGETENT */
