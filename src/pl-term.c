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
late;  character  terminals  disappear quickly now.  Use PCE if you want
windowing!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if unix || EMX

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
static word	tty_stream;		/* stream on which to do tty */

typedef struct
{ Atom	type;				/* type of the entry */
  Atom  name;				/* Name of the value */
  word  value;				/* Value of the entry */
} entry, *Entry;

forwards bool	initTerm P((void));
forwards Entry	lookupEntry P((Atom, Atom));

void
resetTerm()
{ if ( capabilities == NULL )
  { capabilities = newHTable(32);
  } else
  { Symbol s;

    term_initialised = STAT_START;
    for_table(s, capabilities)
      freeHeap(s->value, sizeof(entry));
    clearHTable(capabilities);
  }

  tty_stream = (word) ATOM_user_output;
}

static bool
initTerm()
{ static char *buf = NULL;
  static char *string_area = NULL;

  if ( term_initialised == STAT_START )
  { char *term;

    term_initialised = STAT_ERROR;
    if ( (term = getenv("TERM")) == NULL )
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
lookupEntry(name, type)
Atom name, type;
{ Symbol s;
  Entry e;

  if ( (s = lookupHTable(capabilities, name)) == NULL )
  { if ( initTerm() == FALSE )
      return NULL;

    e = (Entry) allocHeap(sizeof(entry));
    e->name = name;
    e->type = type;
    e->value = 0L;

    if ( type == ATOM_number )
    { int n;

      if ( (n = tgetnum(stringAtom(name))) != -1 )
        e->value  = consNum(n);
    } else if ( type == ATOM_bool )
    { bool b;
    
      if ( (b = tgetflag(stringAtom(name))) != -1 )
        e->value = (word) (b ? ATOM_on : ATOM_off);
    } else if ( type == ATOM_string )
    { char *s;
    
      if ( (s = tgetstr(stringAtom(name), &string_area_pointer)) != NULL )
        e->value  = (word) lookupAtom(s);
    } else
    { warning("tgetent/3: Illegal type");
      freeHeap(e, sizeof(entry));
      return NULL;
    }

    addHTable(capabilities, name, e);
    return e;
  } else
    return (Entry) s->value;
}
      
word
pl_tty_get_capability(name, type, value)
Word name, type, value;
{ Entry e;

  if ( !isAtom(*name) || !isAtom(*type) )
    return warning("tgetent/3: instantiation fault");
  if ( (e = lookupEntry((Atom) *name, (Atom) *type)) == NULL )
    fail;

  if ( e->value != 0L )
    return unifyAtomic(value, e->value);

  fail;
}
  
word
pl_tty_goto(x, y)
Word x, y;
{ Entry e;
  char *s;

  if ( !isInteger(*x) || !isInteger(*y) )
    return warning("tty_goto: instantiation fault");

  if ( (e = lookupEntry(ATOM_cm, ATOM_string)) == NULL ||
        e->value == 0L )
    fail;

  s = tgoto(stringAtom(e->value), (int)valNum(*x), (int)valNum(*y));
  if ( streq(s, "OOPS") )
    fail;
  streamOutput(&tty_stream, (tputs(s, 1, put_character), TRUE));
}

word
pl_tty_put(a, affcnt)
Word a;
Word affcnt;
{ char *s = primitiveToString(*a, FALSE);

  if ( s == NULL || !isInteger(*affcnt) )
    return warning("tty_put: instantiation fault");
  streamOutput(&tty_stream, (tputs(s, (int)valNum(*affcnt), put_character), TRUE));
}

word
pl_set_tty(old, new)
Word old, new;
{ TRY( unifyAtomic(old, tty_stream) );
  if ( streamNo(new, F_WRITE) < 0 )
    fail;

  tty_stream = *new;
  succeed;
}

#else /* ~unix */

void resetTerm()
{
}

word
pl_tty_get_capability(name, type, value)
Word name, type, value;
{ return notImplemented("tty_get_capability", 3);
}

word
pl_tty_goto(x, y)
Word x, y;
{ return notImplemented("tty_goto", 2);
}

word
pl_tty_put(a, affcnt)
Word a;
Word affcnt;
{ return notImplemented("tty_put", 2);
}

word
pl_set_tty(old, new)
Word old, new;
{ return notImplemented("set_tty", 2);
}

#endif /* unix */
