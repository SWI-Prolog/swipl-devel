/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Formated write
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formatted output (Prolog predicates format/[1,2,3]).   One  day,  the  C
source should also use format() to produce error messages, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-itf.h"
extern int Output;

#define BUFSIZE 	10240
#define DEFAULT 	(-1)
#define SHIFT   	{ argc--; argv++; }
#define NEED_ARG	{ if ( argc <= 0 ) \
			  { ERROR("not enough arguments"); \
			  } else \
			  { deRef2(argv, a); \
			  } \
			}
#define ERROR(fmt)	return warning("format/2: %s", fmt)
#define ERROR1(fmt, a)	{ char tp[50]; \
			  strcpy(tp, "format/2: "); \
			  strcat(tp, fmt); \
			  return warning(tp, a); \
			}
#define OUTSTRING(s)	{ char *q = s; \
			  for(; *q; q++) OUTCHR(*q); \
			}
#define OUTCHR(c)	{ if ( pending_rubber ) \
			    buffer[index++] = (c); \
			  else \
			    Put((Char)(c)); \
			  column = update_column(column, c); \
			}

#define MAXRUBBER 100

struct rubber
{ int where;				/* where is rubber in output */
  int size;				/* how big should it be */
  Char pad;				/* padding character */
};

static Table format_predicates;		/* Prolog defined fromatting */

forwards int	update_column(int, Char);
forwards bool	do_format(char *fmt, int argc, Word argv);
forwards void	distribute_rubber(struct rubber *, int, int);
forwards void	emit_rubber(char *buf, int, struct rubber *, int);

		/********************************
		*       PROLOG CONNECTION	*
		********************************/

word
pl_format_predicate(Word chr, Word descr)
{ long c;
  Procedure proc;
  Symbol s;

  if ( isInteger(*chr) )
  { c = valNum(*chr);
    if ( c < 0 || c > 255 )
      return warning("format_predicate/2: illegal character");
  } else if ( isAtom(*chr) )
  { c = stringAtom(*chr)[0];
  } else
    return warning("format_predicate/2: illegal character");

  if ( (proc = findCreateProcedure(descr)) == NULL )
    fail;
  if ( proc->functor->arity == 0 )
    return warning("format_predicate/2: predicate must have at least 1 argument");

  if ( format_predicates == NULL )
    format_predicates = newHTable(64);
  
  if ( (s = lookupHTable(format_predicates, (Void)c)) != NULL )
    s->value = (word) proc;
  else
    addHTable(format_predicates, (Void)c, proc);

  succeed;
}


word
pl_format(Word fmt, register Word args)
{ Word argv;
  word rval;
  int argc = 0;
  char *f;
  mark m;

  if ( isAtom(*fmt) )
    f = stringAtom(*fmt);
  else if ( isString(*fmt) )
    f = valString(*fmt);
  else if ( (f = listToString(*fmt)) != NULL )
    f = store_string(f);
  else
    return warning("format/2: format is not an atom or string");

  Mark(m);
  deRef(args);
  if ( isNil(*args) )
  { argc = 0;
    argv = NULL;
  } else if ( !isList(*args) )
  { argc = 1;
    argv = args;
  } else
  { Word ap;

    if ( (argc = lengthList(args)) < 0 )
      return warning("format/2: argument list is not proper");
    ap = argv = allocGlobal(argc * sizeof(word));

    while( isList(*args) )
    { Word a = HeadList(args);

      deRef(a);
      *ap++ = (isVar(*a) ? makeRef(a) : *a);

      args = TailList(args);
      deRef(args);
    }
  }

  rval = do_format(f, argc, argv);
  Undo(m);

  return rval;
}

word
pl_format3(Word stream, Word fmt, Word args)
{ streamOutput(stream, pl_format(fmt, args));
}

#if O_C_FORMAT

		/********************************
		*          C-CONNECTION		*
		********************************/

static bool
vformat(fm, args)
char *fm;
va_list args;
{ 
}


bool
format(char *fm, ...)
{ va_list args;
  bool rval;

  va_start(args, fm);
  rval = vformat(fm, args);
  va_end(args);

  return rval;
}

#endif /* O_C_FORMAT */
		/********************************
		*       ACTUAL FORMATTING	*
		********************************/

static int
update_column(register int col, register Char c)
{ switch(c)
  { case '\n':	return 0;
    case '\t':	return (col + 1) | 0x7;
    case '\b':	return (col <= 0 ? 0 : col - 1);
    default:	return col + 1;
  }
}   

static bool
do_format(char *fmt, int argc, Word argv)
{ char buffer[BUFSIZE];			/* to store chars with tabs */
  int index = 0;			/* index in buffer */
  int column = currentLinePosition();	/* current output column */
  int tab_stop = 0;			/* padded tab stop */
  int pending_rubber = 0;		/* number of not-filled ~t's */
  struct rubber rub[MAXRUBBER];
  Symbol s;

  while(*fmt)
  { switch(*fmt)
    { case '~':
	{ int arg = DEFAULT;		/* Numeric argument */
	  Word a;			/* (List) argument */
					/* Get the numeric argument */
	  if ( isDigit(*++fmt) )
	  { for( ; isDigit(*fmt); fmt++ )
	      arg = (arg == DEFAULT ? arg = *fmt - '0' : arg*10 + *fmt - '0');
	  } else if ( *fmt == '*' )
	  { NEED_ARG;
	    if ( isInteger(*a) && (arg = (int)valNum(*a)) >= 0 )
	    { SHIFT;
	    } else
	      ERROR("no or negative integer for `*' argument");
	    fmt++;
	  } else if ( *fmt == '`' )
	  { arg = *++fmt;
	    fmt++;
	  }
	    
					/* Check for user defined format */
	  if ( format_predicates != NULL &&
#if gould
	       (s = lookupHTable(format_predicates, (word)*fmt)) != NULL )
#else
	       (s = lookupHTable(format_predicates,
				 (Void)((long)*fmt))) != NULL )
#endif
	  { Procedure proc = (Procedure) s->value;
	    char buf[BUFSIZE];
	    mark m;
	    word goal;
	    Word g;
	    int n;

	    Mark(m);
	    goal = globalFunctor(FUNCTOR_module2);
	    unifyAtomic(argTermP(goal, 0), proc->definition->module->name);
	    unifyAtomic(argTermP(goal, 1), globalFunctor(proc->functor));
	    g = argTermP(goal, 1);
	    unifyAtomic(argTermP(*g, 0), arg == DEFAULT ? (word)ATOM_default
						        : consNum(arg));
	    for(n = 1; n < proc->functor->arity; n++)
	    { NEED_ARG;
	      pl_unify(argTermP(*g, n), a);
	      SHIFT;
	    }
	    tellString(buf, BUFSIZE);
	    debugstatus.suspendTrace++;
	    callGoal(MODULE_user, goal, FALSE);
	    debugstatus.suspendTrace--;
	    toldString();
	    OUTSTRING(buf);
	    Undo(m);

	    fmt++;
	  } else
	  { switch(*fmt)		/* Build in formatting */
	    { case 'a':			/* atomic */
		{ char *s;

		  NEED_ARG;
		  if ( (s = primitiveToString(*a, FALSE)) == (char *) NULL )
		    ERROR("illegal argument to ~a");
		  SHIFT;
		  OUTSTRING(s);
		  fmt++;
		  break;
		}
	      case 'c':			/* ascii */
		{ NEED_ARG;
		  if ( isInteger(*a) )
		  { Char c = (int)valNum(*a);

		    if ( c < 0 || c > 255 )
		      ERROR("illegal argument to ~c");
		    OUTCHR(c);
		    SHIFT;
		  } else
		    ERROR("illegal argument to ~c");
		  fmt++;
		  break;
		}
	      case 'e':			/* exponential float */
	      case 'E':			/* Exponential float */
	      case 'f':			/* float */
	      case 'g':			/* shortest of 'f' and 'e' */
	      case 'G':			/* shortest of 'f' and 'E' */
		{ real f;
		  char tmp[12];
		  char buf[256];

		  NEED_ARG;
		  if ( wordToReal(*a, &f) == FALSE )
		    ERROR1("illegal argument to ~%c", *fmt);
		  SHIFT;
		  sprintf(tmp, "%%.%d%c", arg == DEFAULT ? 6 : arg, *fmt);
		  sprintf(buf, tmp, f);
		  OUTSTRING(buf);
		  fmt++;
		  break;
		}
	      case 'd':			/* integer */
	      case 'D':			/* grouped integer */
	      case 'r':			/* radix number */
	      case 'R':			/* Radix number */
		{ long i;
		  char *s;

		  NEED_ARG;
		  if ( wordToInteger(*a, &i) == FALSE )
		    ERROR1("illegal argument to ~%c", *fmt);
		  SHIFT;
		  if ( arg == DEFAULT )
		    arg = 0;
		  s = ( (*fmt == 'd' || *fmt == 'D')
			? formatInteger(*fmt == 'D', arg, 10, TRUE, i)
			: formatInteger(FALSE, 0, arg, *fmt == 'r', i)
		      );
		  OUTSTRING(s);			
		  fmt++;
		  break;
		}
	      case 's':			/* string */
		{ char *s;

		  NEED_ARG;
		  if ( (s = listToString(*a)) == (char *)NULL )
		    ERROR("illegal argument to ~s");
		  OUTSTRING(s);
		  SHIFT;
		  fmt++;
		  break;
		}
	      case 'i':			/* ignore */
		{ NEED_ARG;
		  SHIFT;
		  fmt++;
		  break;
		}
		{ Func f;
		  char buf[BUFSIZE];

	      case 'k':			/* displayq */
		  f = pl_displayq;	goto pl_common;
	      case 'p':			/* print */
		  f = pl_print;		goto pl_common;
	      case 'q':			/* writeq */
		  f = pl_writeq;	goto pl_common;
	      case 'w':			/* write */
		  f = pl_write;
		  pl_common:

		  NEED_ARG;
		  tellString(buf, BUFSIZE);
		  (*f)(a);
		  toldString();
		  SHIFT;
		  OUTSTRING(buf);
		  fmt++;
		  break;
		}
	      case '~':			/* ~ */
		{ OUTCHR('~');
		  fmt++;
		  break;
		}
	      case 'n':			/* \n */
	      case 'N':			/* \n if not on newline */
		{ if ( arg == DEFAULT )
		    arg = 1;
		  if ( *fmt == 'N' && column == 0 )
		    arg--;
		  while( arg-- > 0 )
		    OUTCHR('\n');
		  fmt++;
		  break;
		}
	      case 't':			/* insert tab */
		{ rub[pending_rubber].where = index;
		  rub[pending_rubber].pad   = (arg == DEFAULT ? (Char) ' '
							      : (Char) arg);
		  pending_rubber++;
		  fmt++;
		  break;
		}
	      case '|':			/* set tab */
		{ int stop;

		  if ( arg == DEFAULT )
		    arg = column;
	      case '+':			/* tab relative */
		  if ( arg == DEFAULT )
		    arg = 8;
		  stop = (*fmt == '+' ? tab_stop + arg : arg);

		  if ( pending_rubber == 0 ) /* nothing to distribute */
		  { rub[0].where = index;
		    rub[0].pad = ' ';
		    pending_rubber++;
		  }
		  distribute_rubber(rub, pending_rubber, stop - column);
		  emit_rubber(buffer, index, rub, pending_rubber);
		  index = 0;
		  pending_rubber = 0;

		  column = tab_stop = stop;
		  fmt++;
		  break;
		}
	      default:
		ERROR1("unknown format: %c", *fmt);
	    }
	  }
	  break;			/* the '~' switch */
	}
      default:
	{ OUTCHR(*fmt);
	  fmt++;
	  break;
	}
    }
  }

  if ( pending_rubber )			/* not closed ~t: flush out */
    emit_rubber(buffer, index, rub, 0);

  succeed;
}

static void
distribute_rubber(struct rubber *r, int rn, int space)
{ if ( space > 0 )
  { int s = space / rn;
    int n, m;

    for(n=0; n < rn; n++)		/* give them equal size */
      r[n].size = s;
					/* distribute from the center */
    space -= s*rn;
    for(m = rn / 2, n = 0; space; n++, space--)
    { r[m + (n % 2 ? n : -n)].size++;
    }
  } else
  { int n;

    for(n=0; n < rn; n++)		/* set all rubber to 0 */
      r[n].size = 0;
  }
}

static void
emit_rubber(char *buf, int i, struct rubber *r, int rn)
{ int j;

  for(j = 0; j <= i; j++)
  { if ( r->where == j && rn )
    { int n;
      for(n=0; n<r->size; n++)
        Put(r->pad);
      r++;
      rn--;
    }
    if ( j < i )
      Put(buf[j]);
  }
}
