/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include <math.h>
#include "pl-incl.h"
#include "pl-ctype.h"
#include <stdio.h>			/* sprintf() */

typedef struct
{ int   flags;				/* PL_WRT_* flags */
  int   max_depth;			/* depth limit */
  int   depth;				/* current depth */
  Module module;			/* Module for operators */
  IOSTREAM *out;			/* stream to write to */
} write_options;

static bool	writeTerm2(term_t term, int prec, write_options *options);

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
{ Atom atom = atomValue(a);
  char *s = atom->name;
  int len = atom->length;

  if ( len == 0 )
    return AT_QUOTE;

  if ( isLower(*s) )
  { for(++s; --len > 0 && isAlpha(*s); s++)
      ;
    return len == 0 ? AT_LOWER : AT_QUOTE;
  }

  if ( a == ATOM_dot )
    return AT_FULLSTOP;
  
  if ( isSymbol(*s) )
  { for(++s; --len > 0 && isSymbol(*s); s++)
      ;
    return len == 0 ? AT_SYMBOL : AT_QUOTE;
  }

					/* % should be quoted! */
  if ( len == 1 && *s != '%' )
  { if ( isSolo(*s) )
      return AT_SOLO;
    switch( *s )
    { /*case ',':*/
      case '|':
	return AT_SOLO;
    }
  }

  if ( a == ATOM_nil || a == ATOM_curl )
    return AT_SPECIAL;
  
  return AT_QUOTE;
}


		 /*******************************
		 *	 PRIMITIVE WRITES	*
		 *******************************/

static bool
Putc(int c, IOSTREAM *s)
{ return Sputc(c, s) == EOF ? FALSE : TRUE;
}


static bool
PutString(const char *str, IOSTREAM *s)
{ return Sfputs(str, s) == EOF ? FALSE : TRUE;
}


static bool
PutStringN(const char *str, unsigned int length, IOSTREAM *s)
{ int len = (int)length;

  return Sfwrite(str, 1, len, s) == len ? TRUE : FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PutOpenToken() inserts a space in the output stream if the last-written
and given character require a space to ensure a token-break.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
PutOpenToken(int c, IOSTREAM *s)
{ if ( c == EOF )
  { s->lastc = EOF;
    return TRUE;
  } else if ( s->lastc != EOF &&
	      ((isAlpha(s->lastc) && isAlpha(c)) ||
	       (isSymbol(s->lastc) && isSymbol(c)) ||
	       (s->lastc != '(' && c == '(')) )
  { return Putc(' ', s);
  }

  return TRUE;
}


static bool
PutToken(const char *s, IOSTREAM *stream)
{ if ( s[0] )
    return PutOpenToken(s[0], stream) && PutString(s, stream);

  return TRUE;
}


static bool
PutTokenN(const char *s, unsigned int len, IOSTREAM *stream)
{ if ( s[0] )
    return PutOpenToken(s[0], stream) && PutStringN(s, len, stream);

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PutOpenBrace()/PutCloseBrace() are used to put  additional braces around
a term to avoid an operator  precedence   problem.  If  the last emitted
character  is  alphanumerical,  there  should  be  a  space  before  the
openbrace to avoid interpretation as a term.   E.g. not (a,b) instead of
not(a,b).  Reported by Stefan.Mueller@dfki.de.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
PutOpenBrace(IOSTREAM *s)
{ return PutOpenToken('(', s) && Putc('(', s);
}


static bool
PutCloseBrace(IOSTREAM *s)
{ return Putc(')', s);
}


static bool
writeQuoted(IOSTREAM *stream, const unsigned char *s, int len, int quote,
	    write_options *options)
{ TRY(Putc(quote, stream));

  while(len-- > 0)
  { int c = *s++;

    if ( true(options, PL_WRT_CHARESCAPES) )
    { if ( !isControl(c) && c != quote && c != '\\' )
      { TRY(Putc(c, stream));
      } else
      { char esc[8];

	esc[1] = EOS;

	if ( c == quote )
	{ esc[0] = c;
	} else
	{ switch(c)
	  { case 7:
	      esc[0] = 'a';
	      break;
	    case '\b':
	      esc[0] = 'b';
	      break;
	    case '\t':
	      esc[0] = 't';
	      break;
	    case '\n':
	      esc[0] = 'n';
	      break;
	    case 11:
	      esc[0] = 'v';
	      break;
	    case '\r':
	      esc[0] = 'r';
	      break;
	    case '\f':
	      esc[0] = 'f';
	      break;
	    case '\\':
	      esc[0] = '\\';
	      break;
	    default:
	      Ssprintf(esc, "%03o\\", c);
	  }
	}
	if ( !Putc('\\', stream) ||
	     !PutString(esc, stream) )
	  fail;
      }
    } else
    { if ( c == quote )
      { TRY(Putc(c, stream) && Putc(c, stream));
      } else
      { TRY(Putc(c, stream));
      }
    }
  }

  return Putc(quote, stream);
}


static bool
writeAtom(atom_t a, write_options *options)
{ Atom atom = atomValue(a);
  unsigned char *s = atom->name;
  unsigned int len = atom->length;
  IOSTREAM *stream = options->out;

  if ( true(options, PL_WRT_QUOTED) )
  { switch( atomType(a) )
    { case AT_LOWER:
      case AT_SYMBOL:
      case AT_SOLO:
      case AT_SPECIAL:
	return PutToken(s, stream);
      case AT_QUOTE:
      case AT_FULLSTOP:
      default:
	return writeQuoted(stream, s, len, '\'', options);
    }
  } else
    return PutTokenN(s, len, stream);
}


#if !defined(HAVE_ISNAN) && defined(NaN)
#define isnan(f)  ((f) == NaN)
#define HAVE_ISNAN
#endif

static bool
writePrimitive(term_t t, write_options *options)
{ double f;
  char *s;
  atom_t a;
  int n;
  char buf[16];
  IOSTREAM *out = options->out;

  if ( PL_is_variable(t) )
    return PutToken(varName(t, buf), out);

  if ( PL_get_atom(t, &a) )
    return writeAtom(a, options);

  if ( PL_is_integer(t) )		/* beware of automatic conversion */
  { long i;
    char buf[32];

    PL_get_long(t, &i);
    sprintf(buf, "%ld", i);
    return PutToken(buf, out);
  }

  if ( PL_get_float(t, &f) )
  { char *s = NULL;

#ifdef HUGE_VAL
    if ( f == HUGE_VAL )
    { s = (true(options, PL_WRT_QUOTED) ? "'$Infinity'" : "Infinity");
    } else
#endif
#ifdef HAVE_ISNAN
    if ( isnan(f) )
    { s = (true(options, PL_WRT_QUOTED) ? "'$NaN'" : "NaN");
    } else
#endif
    if ( s )
      return PutToken(s, out);
    else
    { char buf[100];

      sprintf(buf, LD->float_format, f);
      return PutToken(buf, out);
    }

    succeed;
  }

#if O_STRING
  if ( PL_get_string(t, &s, &n) )
  { if ( true(options, PL_WRT_QUOTED) )
      return writeQuoted(out, (const unsigned char *)s, n, '"', options);
    else
      return PutString(s, out);
  }
#endif /* O_STRING */

  assert(0);
  fail;
}


word
pl_nl1(term_t stream)
{ IOSTREAM *s;

  if ( getOutputStream(stream, &s) )
  { Sputc('\n', s);
    return streamStatus(s);
  }

  fail;
}

word
pl_nl()
{ return pl_nl1(0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call user:portray/1 if defined.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
callPortray(term_t arg, write_options *options)
{ predicate_t portray;

  portray = _PL_predicate("portray", 1, "user", &GD->procedures.portray);

  if ( portray->definition->definition.clauses )
  { fid_t fid   = PL_open_foreign_frame();
    IOSTREAM *old = Scurout;
    int rval;

    Scurout = options->out;
    rval = PL_call_predicate(NULL, FALSE, portray, arg);
    Scurout = old;

    PL_discard_foreign_frame(fid);

    return rval;
  }

  fail;
}


static bool
writeTerm(term_t t, int prec, write_options *options)
{ int rval;
  int levelSave = options->depth;
  term_t save = PL_new_term_refs(0);

  if ( ++options->depth > options->max_depth && options->max_depth )
    rval = PutString("...", options->out);
  else
    rval = writeTerm2(t, prec, options);

  options->depth = levelSave;
  PL_reset_term_refs(save);

  return rval;
}


static bool
writeTerm2(term_t t, int prec, write_options *options)
{ atom_t functor;
  int arity, n;
  int op_type, op_pri;
  atom_t a;
  IOSTREAM *out = options->out;

  if ( !PL_is_variable(t) &&
       true(options, PL_WRT_PORTRAY) &&
       callPortray(t, options) )
    succeed;

  if ( PL_get_atom(t, &a) )
  { if ( priorityOperator(NULL, a) > prec )
    { if ( PutOpenBrace(out) &&
	   writeAtom(a, options) &&
	   PutCloseBrace(out) )
	succeed;
    } else
      return writeAtom(a, options);
  }

  if ( !PL_get_name_arity(t, &functor, &arity) )
  { return writePrimitive(t, options);
  } else
  { if ( false(options, PL_WRT_IGNOREOPS) )
    { term_t arg = PL_new_term_ref();

      if ( arity == 1 )
      { if ( functor == ATOM_curl )	/* {a,b,c} */
	{ term_t a = PL_new_term_ref();
  
	  PL_get_arg(1, t, arg);
	  TRY(Putc('{', out));
	  for(;;)
	  { if ( !PL_is_functor(arg, FUNCTOR_comma2) )
	      break;
	    PL_get_arg(1, arg, a);
	    TRY(writeTerm(a, 999, options) &&
		PutString(", ", out));
	    PL_get_arg(2, arg, arg);
	  }
	  TRY(writeTerm(arg, 999, options) &&
	      Putc('}', out));
  
	  succeed;
	}
  
	if ( functor == ATOM_isovar )	/* $VAR/1 */
	{ int n;
  
	  PL_get_arg(1, t, arg);
	  if ( PL_get_integer(arg, &n) && n >= 0 )
	  { int i = n % 26;
	    int j = n / 26;
	    char buf[16];

	    if ( j == 0 )
	    { buf[0] = i+'A';
	      buf[1] = EOS;
	    } else
	    { sprintf(buf, "%c%d", i+'A', j);
	    }

	    return PutToken(buf, out);
	  }
	}
					  /* op <term> */
	if ( currentOperator(options->module, functor, OP_PREFIX,
			     &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();
  
	  PL_get_arg(1, t, arg);
	  if ( op_pri > prec )
	  { TRY(PutOpenBrace(out));
	  }
	  TRY(writeAtom(functor, options));
	  TRY(writeTerm(arg,
			op_type == OP_FX ? op_pri-1 : op_pri,
			options));
	  if ( op_pri > prec )
	  { TRY(PutCloseBrace(out));
	  }

	  succeed;
	}
  
					  /* <term> op */
	if ( currentOperator(options->module, functor, OP_POSTFIX,
			     &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();
  
	  PL_get_arg(1, t, arg);
	  if ( op_pri > prec )
	    TRY(PutOpenBrace(out));
	  TRY(writeTerm(arg,
			op_type == OP_XF ? op_pri-1 : op_pri,
			options));
	  TRY(writeAtom(functor, options));
	  if (op_pri > prec)
	    TRY(PutCloseBrace(out));
  
	  succeed;
	}
      } else if ( arity == 2 )
      { if ( functor == ATOM_dot )	/* [...] */
	{ term_t head = PL_new_term_ref();
	  term_t l    = PL_copy_term_ref(t);
  
	  TRY(Putc('[', out));
	  for(;;)
	  { PL_get_list(l, head, l);
  
	    TRY(writeTerm(head, 999, options));
	    if ( PL_get_nil(l) )
	      break;
	    if ( ++options->depth >= options->max_depth && options->max_depth )
	      return PutString("|...]", options->out);
	    if ( !PL_is_functor(l, FUNCTOR_dot2) )
	    { TRY(Putc('|', out));
	      TRY(writeTerm(l, 999, options));
	      break;
	    }
	    TRY(PutString(", ", out));
	  }
	  return Putc(']', out);
	}
  
					  /* <term> op <term> */
	if ( currentOperator(options->module, functor, OP_INFIX,
			     &op_type, &op_pri) )
	{ term_t l = PL_new_term_ref();
	  term_t r = PL_new_term_ref();
  
	  PL_get_arg(1, t, l);
	  PL_get_arg(2, t, r);
  
	  if ( op_pri > prec )
	    TRY(PutOpenBrace(out));
	  TRY(writeTerm(l, 
			op_type == OP_XFX || op_type == OP_XFY
				? op_pri-1 : op_pri, 
			options));
	  if ( functor == ATOM_comma )
	  { TRY(Putc(',', out));
	  } else
	  { TRY(writeAtom(functor, options));
	  }
	  if ( functor == ATOM_comma )
	    TRY(Putc(' ', out));
	  TRY(writeTerm(r, 
			op_type == OP_XFX || op_type == OP_YFX
				? op_pri-1 : op_pri, 
			options));
	  if ( op_pri > prec )
	    TRY(PutCloseBrace(out));
	  succeed;
	}
      }
    }
					/* functor(<args> ...) */
    { term_t a = PL_new_term_ref();

      TRY(writeAtom(functor, options) &&
	  Putc('(', out));
      for(n=0; n<arity; n++)
      { if (n > 0)
	  TRY(PutString(", ", out));
	PL_get_arg(n+1, t, a);
	TRY(writeTerm(a, 1000, options));
      }
      return Putc(')', out);
    }
  }
}

static const opt_spec write_term_options[] = 
{ { ATOM_quoted,	    OPT_BOOL },
  { ATOM_ignore_ops,	    OPT_BOOL },
  { ATOM_numbervars,        OPT_BOOL },
  { ATOM_portray,           OPT_BOOL },
  { ATOM_character_escapes, OPT_BOOL },
  { ATOM_max_depth,	    OPT_INT  },
  { ATOM_module,	    OPT_ATOM },
  { NULL_ATOM,	     	    0 }
};

word
pl_write_term3(term_t stream, term_t term, term_t opts)
{ bool quoted     = FALSE;
  bool ignore_ops = FALSE;
  bool numbervars = FALSE;
  bool portray    = FALSE;
  bool charescape = -1;			/* not set */
  atom_t mname    = ATOM_user;
  IOSTREAM *s;
  write_options options;

  options.flags	    = 0;
  options.max_depth = 0;
  options.depth     = 0;

  if ( !scan_options(opts, 0, ATOM_write_option, write_term_options,
		     &quoted, &ignore_ops, &numbervars, &portray,
		     &charescape, &options.max_depth, &mname) )
    fail;

  if ( !getOutputStream(stream, &s) )
    fail;
  
  options.module = lookupModule(mname);
  if ( charescape == TRUE ||
       (charescape == -1 && true(options.module, CHARESCAPE)) )
    options.flags |= PL_WRT_CHARESCAPES;
  
  if ( quoted )     options.flags |= PL_WRT_QUOTED;
  if ( ignore_ops ) options.flags |= PL_WRT_IGNOREOPS;
  if ( numbervars ) options.flags |= PL_WRT_NUMBERVARS;
  if ( portray )    options.flags |= PL_WRT_PORTRAY;

  options.out = s;
  PutOpenToken(EOF, s);			/* reset this */
  writeTerm(term, 1200, &options);

  return streamStatus(s);
}


word
pl_write_term(term_t term, term_t options)
{ return pl_write_term3(0, term, options);
}


int
PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags)
{ write_options options;

  options.flags	    = flags;
  options.max_depth = 0;
  options.depth     = 0;
  options.out	    = s;
  options.module    = MODULE_user;

  PutOpenToken(EOF, s);			/* reset this */
  return writeTerm(term, precedence, &options);
}


static word
do_write2(term_t stream, term_t term, int flags)
{ IOSTREAM *s;

  if ( getOutputStream(stream, &s) )
  { write_options options;

    options.flags     = flags;
    options.max_depth = 0;
    options.depth     = 0;
    options.out	      = s;
    options.module    = MODULE_user;
    if ( options.module && true(options.module, CHARESCAPE) )
      options.flags |= PL_WRT_CHARESCAPES;

    PutOpenToken(EOF, s);		/* reset this */
    writeTerm(term, 1200, &options);
    
    return streamStatus(s);
  }

  fail;
}


word
pl_write2(term_t stream, term_t term)
{ return do_write2(stream, term, 0);
}

word
pl_writeq2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_QUOTED);
}

word
pl_print2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_QUOTED|PL_WRT_PORTRAY);
}

word
pl_write_canonical2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_QUOTED|PL_WRT_IGNOREOPS);
}

word
pl_write(term_t term)
{ return pl_write2(0, term);
}

word
pl_writeq(term_t term)
{ return pl_writeq2(0, term);
}

word
pl_print(term_t term)
{ return pl_print2(0, term);
}

word
pl_write_canonical(term_t term)
{ return pl_write_canonical2(0, term);
}




