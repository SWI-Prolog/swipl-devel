/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: write/1 and display/1 definition
*/

#include <math.h>
#include "pl-incl.h"
#include "pl-ctype.h"

extern int Output;

forwards int	priorityOperator(atom_t);
forwards bool	writeTerm(term_t, int, bool, term_t);

char *
varName(term_t t)
{ Word adr = valTermRef(t);
  static char name[10];

  deRef(adr);

  if (adr > (Word) lBase)
    Ssprintf(name, "_L%ld", (Word)adr - (Word)lBase);
  else
    Ssprintf(name, "_G%ld", (Word)adr - (Word)gBase);

  return name;
}


#define AT_LOWER	0
#define AT_QUOTE	1
#define AT_FULLSTOP	2
#define AT_SYMBOL	3
#define AT_SOLO		4
#define AT_SPECIAL	5

static int
atomType(atom_t a)
{ char *s = stringAtom(a);

  if ( isLower(*s) )
  { char *s2;

    for(s2 = s; *s2 && isAlpha(*s2); )
      s2++;
    return *s2 == EOS ? AT_LOWER : AT_QUOTE;
  }

  if ( a == ATOM_dot )
    return AT_FULLSTOP;
  
  if (isSymbol(*s))
  { char *s2;

    for(s2 = s; *s2 && isSymbol(*s2); )
      s2++;
    return *s2 == EOS ? AT_SYMBOL : AT_QUOTE;
  }

					/* % should be quoted! */
  if ((isSolo(*s) || *s == ',') && s[1] == EOS && s[0] != '%' )
    return AT_SOLO;

  if ( a == ATOM_nil || a == ATOM_curl )
    return AT_SPECIAL;
  
  return AT_QUOTE;
}


static int
writeAtom(atom_t a, bool quote)
{ if ( quote )
  { switch( atomType(a) )
    { case AT_LOWER:
      case AT_SYMBOL:
      case AT_SOLO:
      case AT_SPECIAL:
      { Puts(stringAtom(a));
	break;
      }
      case AT_QUOTE:
      case AT_FULLSTOP:
      default:
      { char *s = stringAtom(a);
	char c;

	Put('\'');
	while( (c = *s++) != EOS )
	  if (c == '\'')
	    Putf("''");
	else
	  Put(c);
	Put('\'');
      }
    }
  } else
    Puts(stringAtom(a));

  succeed;
}

#if !defined(HAVE_ISNAN) && defined(NaN)
#define isnan(f)  ((f) == NaN)
#define HAVE_ISNAN
#endif

static void
writePrimitive(term_t t, bool quote)
{ double f;
  char *s;
  atom_t a;
  int n;

  if ( PL_is_variable(t) )
  { Putf("%s", varName(t) );
    return;
  }    

  if ( PL_get_atom(t, &a) )
  { writeAtom(a, quote);
    return;
  }

  if ( PL_is_integer(t) )		/* beware of automatic conversion */
  { long i;

    PL_get_long(t, &i);
    Putf("%ld", i);
    return;
  }

  if ( PL_get_float(t, &f) )
  {
#ifdef HUGE_VAL
    if ( f == HUGE_VAL )
    { if ( quote )
	Putf("%s", "'$Infinity'");
      else
	Putf("%s", "Infinity");
    } else
#endif
#ifdef HAVE_NAN
    if ( isnan(f) )
    { if ( quote )
	Putf("%s", "'$NaN'");
      else
	Putf("%s", "NaN");
    } else
#endif
    Putf(stringAtom(float_format), f);
    return;
  }

#if O_STRING
  if ( PL_get_string(t, &s, &n) )
  { int c;

    if ( quote == TRUE )
    { Put('\"');
      while( (c = *s++) != EOS )
      { if ( c == '"' )
          Put('"');
        Put(c);
      }
      Put('\"');
    } else
    { Putf("%s", s);
    }
    return;
  }
#endif /* O_STRING */
}


word
pl_nl()
{ return Put('\n');
}

word
pl_nl1(term_t stream)
{ streamOutput(stream, pl_nl());
}


static bool
display(term_t t, bool quote)
{ atom_t name;
  int arity;

  if ( PL_get_name_arity(t, &name, &arity) && arity > 0 )
  { term_t a = PL_new_term_ref();
    int n;

    if ( name == ATOM_comma )
      Putf("','");
    else
      writeAtom(name, quote);
    Putf("(");
    for(n=0; n<arity; n++)
    { if (n > 0)
	Putf(", ");
      PL_get_arg(n+1, t, a);
      display(a, quote);
    }
    Putf(")");
  } else
    writePrimitive(t, quote);

  succeed;
}

word
pl_display(term_t term)
{ return display(term, FALSE);
}

word
pl_displayq(term_t term)
{ return display(term, TRUE);
}

static word
displayStream(term_t stream, term_t term, bool quote)
{ streamOutput(stream, display(term, quote));
}

word
pl_display2(term_t stream, term_t term)
{ return displayStream(stream, term, FALSE);
}

word
pl_displayq2(term_t stream, term_t term)
{ return displayStream(stream, term, TRUE);
}

static int
priorityOperator(atom_t atom)
{ int type, priority;
  int result = 0;

  if (isPrefixOperator(atom, &type, &priority) && priority > result)
    result = priority;
  if (isPostfixOperator(atom, &type, &priority) && priority > result)
    result = priority;
  if (isInfixOperator(atom, &type, &priority) && priority > result)
    result = priority;

  return result;
}

/*  write a term. The 'enviroment' precedence is prec. 'style' askes
    for plain writing (write/1), quoting (writeq/1) or portray (print/1)

 ** Sun Apr 17 12:48:09 1988  jan@swivax.UUCP (Jan Wielemaker)  */

#define PLAIN		0
#define QUOTE_ATOMS	1
#define PORTRAY		2

/*  Call Prolog predicate $portray/1 on 'term'. Succeed or fail
    according to the result.

 ** Sun Jun  5 15:37:12 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
pl_call2(term_t goal, term_t arg)
{ fid_t cid = PL_open_foreign_frame();
  term_t head = PL_new_term_ref();
  term_t a	= PL_new_term_ref();
  term_t argv;
  qid_t qid;
  Procedure proc;
  functor_t fd;
  Module mod = NULL;
  int n;
  bool rval;

  PL_strip_module(goal, &mod, head);
  if ( !PL_get_functor(head, &fd) )
  { warning("pl_call/2: instantiation fault");
    rval = FALSE;
    goto out;
  }

  fd = PL_new_functor(fd->name, fd->arity + 1);
  proc = lookupProcedure(fd, mod);
  argv = PL_new_term_refs(fd->arity);
  for(n=0; n<fd->arity-1; n++)
  { PL_get_arg(n+1, head, a);
    PL_put_term(argv+n, a);
  }
  PL_put_term(argv+n, arg);

  qid = PL_open_query(mod, FALSE, proc, argv);
  rval = PL_next_solution(qid);
  PL_close_query(qid);

out:
  PL_discard_foreign_frame(cid);

  return rval;
}


static atom_t
toAtom(term_t t)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
    return a;

  return NULL_ATOM;
}


static bool
needSpace(atom_t a1, atom_t a2)
{ if ( a1 && a2 )
  { int t1 = atomType(a1);
    int t2 = atomType(a2);

    if ( (t1 == AT_SYMBOL && t2 == AT_LOWER) ||
	 (t1 == AT_LOWER  && t1 == AT_SYMBOL) )
      fail;
  }

  succeed;
}


static bool
writeTerm(term_t t, int prec, bool style, term_t g)
{ atom_t functor;
  int arity, n;
  int op_type, op_pri;
  atom_t a;
  bool quote = (style != PLAIN);

  if ( !PL_is_variable(t) && style == PORTRAY && pl_call2(g, t) )
    succeed;

  if ( PL_get_atom(t, &a) )
  { if ( priorityOperator(a) > prec )
    { Put('(');
      writeAtom(a, quote);
      Put(')');
    } else
      writeAtom(a, quote);

    succeed;
  }

  if ( !PL_get_name_arity(t, &functor, &arity) )
  { writePrimitive(t, quote);

    succeed;
  } else
  { term_t arg = PL_new_term_ref();

    if ( arity == 1 )
    { if ( functor == ATOM_curl )	/* {a,b,c} */
      { term_t a = PL_new_term_ref();

	PL_get_arg(1, t, arg);
	Put('{');
	for(;;)
	{ if ( !PL_is_functor(arg, FUNCTOR_comma2) )
	    break;
	  PL_get_arg(1, arg, a);
	  writeTerm(a, 999, style, g);
	  Putf(", ");
	  PL_get_arg(2, arg, arg);
	}
	writeTerm(arg, 999, style, g);      
	Put('}');

	succeed;
      }

					/* op <term> */
      if ( isPrefixOperator(functor, &op_type, &op_pri) )
      { term_t arg = PL_new_term_ref();

	PL_get_arg(1, t, arg);
	if ( op_pri > prec )
	  Put('(');
	writeAtom(functor, quote);
	if ( needSpace(functor, toAtom(arg)) )
	  Put(' ');
	writeTerm(arg, op_type == OP_FX ? op_pri-1 : op_pri, style, g);
	if ( op_pri > prec )
	  Put(')');

	succeed;
      }

					/* <term> op */
      if ( isPostfixOperator(functor, &op_type, &op_pri) )
      { term_t arg = PL_new_term_ref();

	PL_get_arg(1, t, arg);
	if ( op_pri > prec )
	  Put('(');
	writeTerm(arg, op_type == OP_XF ? op_pri-1 : op_pri, style, g);
	if ( needSpace(toAtom(arg), functor) )
	  Put(' ');
	writeAtom(functor, quote);
	if (op_pri > prec)
	  Put(')');

	succeed;
      }
    } else if ( arity == 2 )
    { if ( functor == ATOM_dot )	/* [...] */
      { term_t head = PL_new_term_ref();
	term_t l    = PL_copy_term_ref(t);

	Put('[');
	for(;;)
	{ PL_get_list(l, head, l);

	  writeTerm(head, 999, style, g);
	  if ( PL_get_nil(l) )
	    break;
	  if ( !PL_is_functor(l, FUNCTOR_dot2) )
	  { Put('|');
	    writeTerm(l, 999, style, g);
	    break;
	  }
	  Putf(", ");
	}
	Put(']');

	succeed;
      }

					/* <term> op <term> */
      if ( isInfixOperator(functor, &op_type, &op_pri) )
      { term_t a = PL_new_term_ref();

	if ( op_pri > prec )
	Put('(');
	PL_get_arg(1, t, a);
	writeTerm(a, 
		  op_type == OP_XFX || op_type == OP_XFY ? op_pri-1 : op_pri, 
		  style, g);
	if ( functor != ATOM_comma )
	  Put(' ');
	writeAtom(functor, quote);
	Put(' ');
	PL_get_arg(2, t, a);
	writeTerm(a, 
		  op_type == OP_XFX || op_type == OP_YFX ? op_pri-1 : op_pri, 
		  style, g);
	if ( op_pri > prec )
	  Put(')');
	succeed;
      }
    }

					/* functor(<args> ...) */
    { term_t a = PL_new_term_ref();

      writeAtom(functor, quote);
      Put('(');
      for(n=0; n<arity; n++, arg++)
      { if (n > 0)
	  Putf(", ");
	PL_get_arg(n+1, t, a);
	writeTerm(a, 999, style, g);
      }
      Put(')');
    }
  }

  succeed;
}

word
pl_write(term_t term)
{ writeTerm(term, 1200, PLAIN, 0);

  succeed;
}

word
pl_writeq(term_t term)
{ writeTerm(term, 1200, QUOTE_ATOMS, 0);

  succeed;
}

word
pl_print(term_t term)
{ term_t g = PL_new_term_ref();

  PL_put_atom(g, ATOM_portray);

  writeTerm(term, 1200, PORTRAY, g);

  succeed;
}

word
pl_dprint(term_t term, term_t g)
{ writeTerm(term, 1200, PORTRAY, g);

  succeed;
}

					/* use a predicate pointer? */
static word
writeStreamTerm(term_t stream, term_t term,
		int prec, int style, term_t g)
{ streamOutput(stream, writeTerm(term, prec, style, g));
}

word
pl_write2(term_t stream, term_t term)
{ return writeStreamTerm(stream, term, 1200, PLAIN, 0);
}

word
pl_writeq2(term_t stream, term_t term)
{ return writeStreamTerm(stream, term, 1200, QUOTE_ATOMS, 0);
}

word
pl_print2(term_t stream, term_t term)
{ term_t g = PL_new_term_ref();

  PL_put_atom(g, ATOM_portray);

  return writeStreamTerm(stream, term, 1200, PORTRAY, g);
}


