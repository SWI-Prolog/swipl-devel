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
forwards bool	writeTerm(term_t term, int pri, int flags);

char *
varName(term_t t, char *name)
{ Word adr = valTermRef(t);

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


static bool
writeAtom(atom_t a, bool quote)
{ if ( quote )
  { switch( atomType(a) )
    { case AT_LOWER:
      case AT_SYMBOL:
      case AT_SOLO:
      case AT_SPECIAL:
	return Puts(stringAtom(a));
      case AT_QUOTE:
      case AT_FULLSTOP:
      default:
      { char *s = stringAtom(a);
	char c;

	TRY(Put('\''));
	while( (c = *s++) != EOS )
	{ if (c == '\'')
	  { TRY(Putf("''"));
	  } else
	  { TRY(Put(c));
	  }
	}
	return Put('\'');
      }
    }
  } else
    return Puts(stringAtom(a));
}

#if !defined(HAVE_ISNAN) && defined(NaN)
#define isnan(f)  ((f) == NaN)
#define HAVE_ISNAN
#endif

static bool
writePrimitive(term_t t, bool quote)
{ double f;
  char *s;
  atom_t a;
  int n;
  char buf[16];

  if ( PL_is_variable(t) )
    return Putf("%s", varName(t, buf));

  if ( PL_get_atom(t, &a) )
    return writeAtom(a, quote);

  if ( PL_is_integer(t) )		/* beware of automatic conversion */
  { long i;

    PL_get_long(t, &i);
    return Putf("%ld", i);
  }

  if ( PL_get_float(t, &f) )
  { char *s = NULL;

#ifdef HUGE_VAL
    if ( f == HUGE_VAL )
    { s = (quote ? "'$Infinity'" : "Infinity");
    } else
#endif
#ifdef HAVE_ISNAN
    if ( isnan(f) )
    { s = (quote ? "'$NaN'" : "NaN");
    } else
#endif
    if ( s )
      return Puts(s);
    else
      return Putf(stringAtom(float_format), f);

    succeed;
  }

#if O_STRING
  if ( PL_get_string(t, &s, &n) )
  { int c;

    if ( quote == TRUE )
    { TRY(Put('\"'));
      while( (c = *s++) != EOS )
      { if ( c == '"' )
	{ TRY(Put('"'));
	}
        TRY(Put(c));
      }
      return Put('\"');
    } else
      return Puts(s);
  }
#endif /* O_STRING */

  assert(0);
  fail;
}


word
pl_nl()
{ return Put('\n');
}

word
pl_nl1(term_t stream)
{ streamOutput(stream, pl_nl());
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Write_term flags.  
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define WRT_QUOTED	1		/* quote atoms */
#define WRT_IGNOREOPS	2		/* ignore list/operators */
#define WRT_NUMBERVARS	4		/* print $VAR(N) as a variable */
#define WRT_PORTRAY	8		/* call portray */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call user:portray/1 if defined.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
callPortray(term_t arg)
{ predicate_t portray;

  portray = _PL_predicate("portray", 1, "user", &GD->procedures.portray);

  if ( portray->definition->definition.clauses )
  { fid_t fid   = PL_open_foreign_frame();
    int rval;

    rval = PL_call_predicate(NULL, FALSE, portray, arg);

    PL_discard_foreign_frame(fid);

    return rval;
  }

  fail;
}



static bool
needSpace(atom_t a1, term_t t)
{ int t1 = atomType(a1);
  atom_t a;

  if ( PL_get_atom(t, &a) )
  { int t2 = atomType(a);

    if ( (t1 == AT_SYMBOL && t2 == AT_LOWER) ||
	 (t1 == AT_LOWER  && t1 == AT_SYMBOL) )
      fail;

    succeed;
  }
  if ( t1 == AT_SYMBOL && PL_is_integer(t) )
    fail;

  succeed;
}


static bool
writeTerm(term_t t, int prec, bool style)
{ atom_t functor;
  int arity, n;
  int op_type, op_pri;
  atom_t a;
  bool quote = (style & WRT_QUOTED);

  if ( !PL_is_variable(t) && (style & WRT_PORTRAY) && callPortray(t) )
    succeed;

  if ( PL_get_atom(t, &a) )
  { if ( priorityOperator(a) > prec )
    { if ( Put('(') &&
	   writeAtom(a, quote) &&
	   Put(')') )
	succeed;
    } else
      return writeAtom(a, quote);
  }

  if ( !PL_get_name_arity(t, &functor, &arity) )
  { return writePrimitive(t, quote);
  } else
  { if ( !(style & WRT_IGNOREOPS) )
    { term_t arg = PL_new_term_ref();

      if ( arity == 1 )
      { if ( functor == ATOM_curl )	/* {a,b,c} */
	{ term_t a = PL_new_term_ref();
  
	  PL_get_arg(1, t, arg);
	  TRY(Put('{'));
	  for(;;)
	  { if ( !PL_is_functor(arg, FUNCTOR_comma2) )
	      break;
	    PL_get_arg(1, arg, a);
	    TRY(writeTerm(a, 999, style) &&
		Putf(", "));
	    PL_get_arg(2, arg, arg);
	  }
	  TRY(writeTerm(arg, 999, style) &&
	      Put('}'));
  
	  succeed;
	}
  
	if ( functor == ATOM_isovar )	/* $VAR/1 */
	{ int n;
  
	  PL_get_arg(1, t, arg);
	  if ( PL_get_integer(arg, &n) && n >= 0 )
	  { int i = n % 26;
	    int j = n / 26;
	    
	    if ( j == 0 )
	      return Putf("%c", i+'A');
	    else
	      return Putf("%c%d", i+'A', j);
	  }
	}
					  /* op <term> */
	if ( isPrefixOperator(functor, &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();
  
	  PL_get_arg(1, t, arg);
	  if ( op_pri > prec )
	  { TRY(Put('('));
	  }
	  TRY(writeAtom(functor, quote));
	  if ( needSpace(functor, arg) )
	  { TRY(Put(' '));
	  }
	  TRY(writeTerm(arg, op_type == OP_FX ? op_pri-1 : op_pri, style));
	  if ( op_pri > prec )
	  { TRY(Put(')'));
	  }

	  succeed;
	}
  
					  /* <term> op */
	if ( isPostfixOperator(functor, &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();
  
	  PL_get_arg(1, t, arg);
	  if ( op_pri > prec )
	    TRY(Put('('));
	  TRY(writeTerm(arg, op_type == OP_XF ? op_pri-1 : op_pri, style));
	  if ( needSpace(functor, arg) ) /* indendent of order? */
	    TRY(Put(' '));
	  TRY(writeAtom(functor, quote));
	  if (op_pri > prec)
	    TRY(Put(')'));
  
	  succeed;
	}
      } else if ( arity == 2 )
      { if ( functor == ATOM_dot )	/* [...] */
	{ term_t head = PL_new_term_ref();
	  term_t l    = PL_copy_term_ref(t);
  
	  TRY(Put('['));
	  for(;;)
	  { PL_get_list(l, head, l);
  
	    TRY(writeTerm(head, 999, style));
	    if ( PL_get_nil(l) )
	      break;
	    if ( !PL_is_functor(l, FUNCTOR_dot2) )
	    { TRY(Put('|'));
	      TRY(writeTerm(l, 999, style));
	      break;
	    }
	    TRY(Putf(", "));
	  }
	  return Put(']');
	}
  
					  /* <term> op <term> */
	if ( isInfixOperator(functor, &op_type, &op_pri) )
	{ term_t l = PL_new_term_ref();
	  term_t r = PL_new_term_ref();
	  int needspaces = TRUE;
  
	  PL_get_arg(1, t, l);
	  PL_get_arg(2, t, r);
  
	  if ( functor == ATOM_divide &&
	       PL_is_atom(l) && PL_is_integer(r) )
	    needspaces = FALSE;
  
	  if ( op_pri > prec )
	    TRY(Put('('));
	  TRY(writeTerm(l, 
			op_type == OP_XFX || op_type == OP_XFY
				? op_pri-1 : op_pri, 
			style));
	  if ( needspaces && functor != ATOM_comma )
	    TRY(Put(' '));
	  TRY(writeAtom(functor, quote));
	  if ( needspaces )
	    TRY(Put(' '));
	  TRY(writeTerm(r, 
			op_type == OP_XFX || op_type == OP_YFX
				? op_pri-1 : op_pri, 
			style));
	  if ( op_pri > prec )
	    TRY(Put(')'));
	  succeed;
	}
      }
    }
					/* functor(<args> ...) */
    { term_t a = PL_new_term_ref();

      TRY(writeAtom(functor, quote) &&
	  Put('('));
      for(n=0; n<arity; n++)
      { if (n > 0)
	  TRY(Putf(", "));
	PL_get_arg(n+1, t, a);
	TRY(writeTerm(a, 999, style));
      }
      return Put(')');
    }
  }
}

static const opt_spec write_term_options[] = 
{ { ATOM_quoted,	    OPT_BOOL },
  { ATOM_ignore_ops,	    OPT_BOOL },
  { ATOM_numbervars,        OPT_BOOL },
  { ATOM_portray,           OPT_BOOL },
  { NULL_ATOM,	     	    0 }
};

word
pl_write_term(term_t term, term_t options)
{ bool quoted     = FALSE;
  bool ignore_ops = FALSE;
  bool numbervars = FALSE;
  bool portray    = FALSE;
  int mask = 0;
  
  if ( !scan_options(options, 0, ATOM_write_option, write_term_options,
		     &quoted, &ignore_ops, &numbervars, &portray) )
    fail;

  if ( quoted )     mask |= WRT_QUOTED;
  if ( ignore_ops ) mask |= WRT_IGNOREOPS;
  if ( numbervars ) mask |= WRT_NUMBERVARS;
  if ( portray )    mask |= WRT_PORTRAY;
    
  return writeTerm(term, 1200, mask);
}

word
pl_write_term3(term_t stream, term_t term, term_t options)
{ streamOutput(stream, pl_write_term(term, options));
}


word
pl_write(term_t term)
{ return writeTerm(term, 1200, 0);
}

word
pl_writeq(term_t term)
{ return writeTerm(term, 1200, WRT_QUOTED);
}

word
pl_print(term_t term)
{ return writeTerm(term, 1200, WRT_PORTRAY|WRT_QUOTED);
}


static word
writeStreamTerm(term_t stream, term_t term,
		int prec, int style)
{ streamOutput(stream, writeTerm(term, prec, style));
}

word
pl_write2(term_t stream, term_t term)
{ return writeStreamTerm(stream, term, 1200, 0);
}

word
pl_writeq2(term_t stream, term_t term)
{ return writeStreamTerm(stream, term, 1200, WRT_QUOTED);
}

word
pl_print2(term_t stream, term_t term)
{ return writeStreamTerm(stream, term, 1200, WRT_QUOTED|WRT_PORTRAY);
}

word
pl_write_canonical(term_t term)
{ return writeTerm(term, 1200, WRT_QUOTED|WRT_IGNOREOPS);
}

word
pl_write_canonical2(term_t stream, term_t term)
{ return writeStreamTerm(stream, term, 1200, WRT_QUOTED|WRT_IGNOREOPS);
}

