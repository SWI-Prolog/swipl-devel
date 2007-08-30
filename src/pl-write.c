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

#include <math.h>
#include "pl-incl.h"
#include "pl-ctype.h"
#include <stdio.h>			/* sprintf() */
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef HAVE_FLOAT_H
#include <float.h>
#endif
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#ifdef fpclassify
#define HAVE_FPCLASSIFY 1
#endif

typedef struct visited
{ Word address;				/* we have done this address */
  struct visited *next;			/* next already visited */
} visited;

typedef struct
{ int   flags;				/* PL_WRT_* flags */
  int   max_depth;			/* depth limit */
  int   depth;				/* current depth */
  Module module;			/* Module for operators */
  IOSTREAM *out;			/* stream to write to */
  visited *visited;			/* visited (attributed-) variables */
} write_options;

static bool	writeTerm2(term_t term, int prec, write_options *options);
static bool	writeTerm(term_t t, int prec, write_options *options);

static Word
address_of(term_t t)
{ Word adr = valTermRef(t);

  deRef(adr);
  switch(tag(*adr))
  { case TAG_ATTVAR:
      return adr;
    case TAG_COMPOUND:
      return valPtr(*adr);
    default:
      return NULL;			/* non-recursive structure */
  }
}


static int
has_visited(visited *v, Word addr)
{ for( ; v; v=v->next )
  { if ( v->address == addr )
      succeed;
  }

  fail;
}


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

/* Note: this only deals with ISO Latin-1 atoms; wide atoms are handled
   by writeUCSAtom()
*/

static int
atomType(atom_t a, IOSTREAM *fd)
{ Atom atom = atomValue(a);
  char *s = atom->name;
  size_t len = atom->length;

  if ( len == 0 )
    return AT_QUOTE;

  if ( isLower(*s) )
  { for(++s; --len > 0 && isAlpha(*s) && Scanrepresent(*s, fd)==0; s++)
      ;
    return len == 0 ? AT_LOWER : AT_QUOTE;
  }

  if ( a == ATOM_dot )
    return AT_FULLSTOP;
  
  if ( isSymbol(*s) )
  { if ( len >= 2 && s[0] == '/' && s[1] == '*' )
      return AT_QUOTE;

    for(++s; --len > 0 && isSymbol(*s) && Scanrepresent(*s, fd)==0; s++)
      ;

    return len == 0 ? AT_SYMBOL : AT_QUOTE;
  }

					/* % should be quoted! */
  if ( len == 1 && *s != '%' )
  { if ( isSolo(*s) )
      return AT_SOLO;
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
{ return Sputcode(c, s) == EOF ? FALSE : TRUE;
}


static bool
PutString(const char *str, IOSTREAM *s)
{ const unsigned char *q = (const unsigned char *)str;

  for( ; *q != EOS; q++ )
  { if ( Sputcode(*q, s) == EOF )
      return FALSE;
  }

  return TRUE;
}


static bool
PutStringN(const char *str, size_t length, IOSTREAM *s)
{ size_t i;
  const unsigned char *q = (const unsigned char *)str;

  for(i=0; i<length; i++, q++)
  { if ( Sputcode(*q, s) == EOF )
      return FALSE;
  } 

  return TRUE;
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
	      ((isAlphaW(s->lastc) && isAlphaW(c)) ||
	       (isSymbolW(s->lastc) && isSymbolW(c)) ||
	       (s->lastc != '(' && !isBlank(s->lastc) && c == '(')) )
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
PutTokenN(const char *s, size_t len, IOSTREAM *stream)
{ if ( len > 0 )
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
putQuoted(int c, int quote, int flags, IOSTREAM *stream)
{ if ( (flags & PL_WRT_CHARESCAPES) )
  { if ( !(c < 0xff && isControl(c)) && c != quote && c != '\\' )
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
	    if ( c <= 0xff )
	      Ssprintf(esc, "%03o\\", c);
	    else
	      assert(0);			/* to be done */
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
    { return Putc(c, stream);
    }
  }

  return TRUE;
}



static bool
writeQuoted(IOSTREAM *stream, const char *text, size_t len, int quote,
	    write_options *options)
{ const unsigned char *s = (const unsigned char *)text;

  TRY(Putc(quote, stream));

  while(len-- > 0)
  { TRY(putQuoted(*s++, quote, options->flags, stream));
  }

  return Putc(quote, stream);
}


#if O_ATTVAR
static bool
writeAttVar(term_t av, write_options *options)
{ char buf[32];

  TRY(PutToken(varName(av, buf), options->out));

  if ( (options->flags & PL_WRT_ATTVAR_DOTS) )
  { return PutString("{...}", options->out);
  } else if ( (options->flags & PL_WRT_ATTVAR_WRITE) )
  { fid_t fid = PL_open_foreign_frame();
    term_t a = PL_new_term_ref();
    visited v;

    v.address = address_of(av);
    if ( has_visited(options->visited, v.address) )
      succeed;
    v.next = options->visited;
    options->visited = &v;
    Sputc('{', options->out);
    PL_get_attr(av, a);
    if ( !writeTerm(a, 1200, options) )
      goto error;
    Sputc('}', options->out);
    PL_discard_foreign_frame(fid);

    options->visited = v.next;
    succeed;

  error:
    options->visited = v.next;
    fail;
  } else if ( (options->flags & PL_WRT_ATTVAR_PORTRAY) &&
	      GD->cleaning <= CLN_PROLOG )
  { fid_t fid   = PL_open_foreign_frame();
    predicate_t pred;
    IOSTREAM *old;

    pred = _PL_predicate("portray_attvar", 1, "$attvar",
			 &GD->procedures.portray_attvar1);
    
    old = Scurout;
    Scurout = options->out;
    PL_call_predicate(NULL, PL_Q_NODEBUG, pred, av);
    Scurout = old;

    PL_discard_foreign_frame(fid);
  }

  succeed;
}
#endif


static bool
writeBlob(atom_t a, write_options *options)
{ Atom atom = atomValue(a);
  unsigned char const *s, *e;

  TRY(PutString("<#", options->out));
  s = (unsigned char const *)atom->name;
  for (e = s + atom->length; s != e; s++)
  { static char *digits = "0123456789abcdef";

    TRY(Putc(digits[(*s >> 4) & 0xf], options->out));
    TRY(Putc(digits[(*s     ) & 0xf], options->out));
  }
  
  return PutString(">", options->out);
}


static bool
writeAtom(atom_t a, write_options *options)
{ Atom atom = atomValue(a);

  if ( atom->type->write )
    return (*atom->type->write)(options->out, a, options->flags);
  if ( false(atom->type, PL_BLOB_TEXT) )
    return writeBlob(a, options);

  if ( true(options, PL_WRT_QUOTED) )
  { switch( atomType(a, options->out) )
    { case AT_LOWER:
      case AT_SYMBOL:
      case AT_SOLO:
      case AT_SPECIAL:
	return PutToken(atom->name, options->out);
      case AT_QUOTE:
      case AT_FULLSTOP:
      default:
	return writeQuoted(options->out,
			   atom->name,
			   atom->length,
			   '\'', options);
    }
  } else
    return PutTokenN(atom->name, atom->length, options->out);
}


int
writeAtomToStream(IOSTREAM *s, atom_t atom)
{ write_options options;

  memset(&options, 0, sizeof(options));
  options.out = s;
  options.module = MODULE_user;

  return writeAtom(atom, &options);
}


int
writeUCSAtom(IOSTREAM *fd, atom_t atom, int flags)
{ Atom a = atomValue(atom);
  pl_wchar_t *s = (pl_wchar_t*)a->name;
  size_t len = a->length/sizeof(pl_wchar_t);
  pl_wchar_t *e = &s[len];

  if ( flags & PL_WRT_QUOTED )
  { pl_wchar_t quote = L'\'';
    int rc;

    if ( isLowerW(*s) )
    { pl_wchar_t *q;

      for(q=s; q<e; q++)
      { if ( !isAlphaW(*q) || Scanrepresent(*q, fd) < 0 )
	  break;
      }
      if ( q == e )
        goto unquoted;
    }

    TRY(Putc(quote, fd));

    while(s < e)
    { TRY(putQuoted(*s++, quote, flags, fd));
    }

    rc = Putc(quote, fd);

    return rc;
  }

unquoted:
  if ( s < e && !PutOpenToken(s[0], fd) )
    return FALSE;
  for( ; s<e; s++)
  { if ( !Putc(*s, fd) )
      return FALSE;
  }

  return TRUE;
}


#if O_STRING

static inline int
get_chr_from_text(const PL_chars_t *t, int index)
{ switch(t->encoding)
  { case ENC_ISO_LATIN_1:
      return t->text.t[index]&0xff;
    case ENC_WCHAR:
      return t->text.w[index];
    default:
      assert(0);
    return 0;
  }
}


static int
writeString(term_t t, write_options *options)
{ PL_chars_t txt;

  PL_get_text(t, &txt, CVT_STRING);
  
  if ( true(options, PL_WRT_QUOTED) )
  { int quote;
    unsigned int i;

    if ( true(options, PL_WRT_BACKQUOTED_STRING) )
      quote = '`';
    else
      quote = '"';
      
    TRY(Putc(quote, options->out));

    for(i=0; i<txt.length; i++)
    { int chr = get_chr_from_text(&txt, i);

      TRY(putQuoted(chr, quote, options->flags, options->out));
    }

    return Putc(quote, options->out);
  } else
  { unsigned int i;

    for(i=0; i<txt.length; i++)
    { int chr = get_chr_from_text(&txt, i);

      TRY(Putc(chr, options->out));
    }
  }

  succeed;
}

#endif /*O_STRING*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formatting a float. This is very  complicated   as  we must write floats
such that it can be read as a float. This means using the conventions of
the C locale and if the float happens to be integer as <int>.0.

Switching the locale is no option as  locale handling is not thread-safe
and may have unwanted  consequences  for   embedding.  There  is  a intptr_t
discussion on the very same topic on  the Python mailinglist. Many hacks
are proposed, none is very satisfactory.   Richard  O'Keefe suggested to
use ecvt(), fcvt() and gcvt(). These  are   not  thread-safe.  The GNU C
library provides *_r() variations that  can   do  the  trick. An earlier
patch used localeconv() to find the  decimal   point,  but  this is both
complicated and not thread-safe.

Finally, with help of Richard we decided  to replace the first character
that is not a digit nor [eE], as this must be the decimal point. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
format_float(double f, char *buf, const char *format)
{ char *q;

  sprintf(buf, format, f);

  q = buf;
  if ( *q == '-' )			/* skip -?[0-9]* */
    q++;
  while(*q && (isDigit(*q) || *q <= ' '))
    q++;

  switch( *q )
  { case '\0':
      *q++ = '.';
      *q++ = '0';
      *q = EOS;
      break;
    case 'e':
    case 'E':
      break;
    default:
      *q = '.';
  }

  return buf;
}


static bool
WriteNumber(Number n, write_options *options)
{ switch(n->type)
  { case V_INTEGER:
    { char buf[32];

      sprintf(buf, INT64_FORMAT, n->value.i);
      return PutToken(buf, options->out);
    }
#ifdef O_GMP
    case V_MPZ:
    { char tmp[1024];
      char *buf;
      size_t sz = mpz_sizeinbase(n->value.mpz, 10) + 2;
      bool rc;

      if ( sz <= sizeof(tmp) )
	buf = tmp;
      else
	buf = PL_malloc(sz);

      mpz_get_str(buf, 10, n->value.mpz);
      rc = PutToken(buf, options->out);
      if ( buf != tmp )
	PL_free(buf);
	
      return rc;
    }
    case V_MPQ:				/* should not get here */
#endif
    case V_REAL:
      assert(0);
  }

  fail;
}



static bool
writePrimitive(term_t t, write_options *options)
{ double f;
  atom_t a;
  char buf[32];
  IOSTREAM *out = options->out;

#if O_ATTVAR
  if ( PL_is_attvar(t) )
    return writeAttVar(t, options);
#endif

  if ( PL_is_variable(t) )
    return PutToken(varName(t, buf), out);

  if ( PL_get_atom(t, &a) )
    return writeAtom(a, options);

  if ( PL_is_integer(t) )		/* beware of automatic conversion */
  { number n;

    PL_get_number(t, &n);

    return WriteNumber(&n, options);
  }

  if ( PL_get_float(t, &f) )
  { char *s = NULL;

#ifdef HAVE_FPCLASSIFY
    switch(fpclassify(f))
    { case FP_NAN:
	s = (true(options, PL_WRT_QUOTED) ? "'$NaN'" : "NaN");
        break;
      case FP_INFINITE:
	s = (true(options, PL_WRT_QUOTED) ? "'$Infinity'" : "Infinity");
        break;
    }
#else
#ifdef HAVE_FPCLASS
    switch(fpclass(f))
    { case FP_SNAN:
      case FP_QNAN:
	s = (true(options, PL_WRT_QUOTED) ? "'$NaN'" : "NaN");
        break;
      case FP_NINF:
      case FP_PINF:
	s = (true(options, PL_WRT_QUOTED) ? "'$Infinity'" : "Infinity");
        break;
      case FP_NDENORM:			/* pos/neg denormalized non-zero */
      case FP_PDENORM:
      case FP_NNORM:			/* pos/neg normalized non-zero */
      case FP_PNORM:
      case FP_NZERO:			/* pos/neg zero */
      case FP_PZERO:
	break;
    }
#else
#ifdef HAVE__FPCLASS
    switch(_fpclass(f))
    { case _FPCLASS_SNAN:
      case _FPCLASS_QNAN:
	s = (true(options, PL_WRT_QUOTED) ? "'$NaN'" : "NaN");
        break;
      case _FPCLASS_NINF:
      case _FPCLASS_PINF:
	s = (true(options, PL_WRT_QUOTED) ? "'$Infinity'" : "Infinity");
        break;
    }
#else
#ifdef HAVE_ISINF
    if ( isinf(f) )
    { s = (true(options, PL_WRT_QUOTED) ? "'$Infinity'" : "Infinity");
    } else
#endif
#ifdef HAVE_ISNAN
    if ( isnan(f) )
    { s = (true(options, PL_WRT_QUOTED) ? "'$NaN'" : "NaN");
    }
#endif
#endif /*HAVE__FPCLASS*/
#endif /*HAVE_FPCLASS*/
#endif /*HAVE_FPCLASSIFY*/

    if ( s )
    { return PutToken(s, out);
    } else
    { char buf[100];

      format_float(f, buf, LD->float_format);

      return PutToken(buf, out);
    }
  }

#if O_STRING
  if ( PL_is_string(t) )
    return writeString(t, options);
#endif /* O_STRING */

  assert(0);
  fail;
}


word
pl_nl1(term_t stream)
{ IOSTREAM *s;

  if ( getOutputStream(stream, &s) )
  { Sputcode('\n', s);
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

  if ( GD->cleaning > CLN_PROLOG )
    fail;				/* avoid dangerous callbacks */

  portray = _PL_predicate("portray", 1, "user", &GD->procedures.portray);

  if ( portray->definition->definition.clauses )
  { fid_t wake    = saveWakeup(PASS_LD1);
    fid_t fid     = PL_open_foreign_frame();
    IOSTREAM *old = Scurout;
    int rval;

    Scurout = options->out;
    rval = PL_call_predicate(NULL, PL_Q_NODEBUG, portray, arg);
    Scurout = old;

    PL_discard_foreign_frame(fid);
    restoreWakeup(wake PASS_LD);

    return rval;
  }

  fail;
}


static bool
writeTerm(term_t t, int prec, write_options *options)
{ int rval;
  int levelSave = options->depth;
  fid_t fid = PL_open_foreign_frame();

  if ( ++options->depth > options->max_depth && options->max_depth )
    rval = PutString("...", options->out);
  else if ( PL_is_compound(t) )
  { visited v;

    v.address = address_of(t);
    if ( has_visited(options->visited, v.address) )
    { rval = PutString("**", options->out);
    } else
    { v.next = options->visited;
      options->visited = &v;
      rval = writeTerm2(t, prec, options);
      options->visited = v.next;
    }
  } else
  { rval = writeTerm2(t, prec, options);
  }

  options->depth = levelSave;
  PL_close_foreign_frame(fid);

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
  { if ( functor == ATOM_isovar &&			/* $VAR/1 */
	 true(options, PL_WRT_NUMBERVARS) )
    { int n;
      atom_t a;
      term_t arg = PL_new_term_ref();

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
      if ( PL_get_atom(arg, &a) )
      { write_options o2 = *options;
	clear(&o2, PL_WRT_QUOTED);
	
	return writeAtom(a, &o2);
      }	    
    }


    if ( false(options, PL_WRT_IGNOREOPS) )
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
  
					  /* op <term> */
	if ( currentOperator(options->module, functor, OP_PREFIX,
			     &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();
	  int embrace;

	  embrace = ( op_pri > prec );

	  PL_get_arg(1, t, arg);
	  if ( embrace )
	  { TRY(PutOpenBrace(out));
	  }
	  TRY(writeAtom(functor, options));

				/* +/-(Number) : avoid parsing as number */
	  if ( (functor == ATOM_minus || functor == ATOM_plus) &&
	       PL_is_number(arg) )
	  { TRY(Putc('(', out));
	    TRY(writeTerm(arg, 999, options));
	    TRY(Putc(')', out));
	  } else
	  { TRY(writeTerm(arg,
			  op_type == OP_FX ? op_pri-1 : op_pri,
			  options));
	  }
	  if ( embrace )
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
	TRY(writeTerm(a, 999, options));
      }
      return Putc(')', out);
    }
  }
}


int
writeAttributeMask(atom_t a)
{ if ( a == ATOM_ignore )
  { return PL_WRT_ATTVAR_IGNORE;
  } else if ( a == ATOM_dots )
  { return PL_WRT_ATTVAR_DOTS;
  } else if ( a == ATOM_write )
  { return PL_WRT_ATTVAR_WRITE;
  } else if ( a == ATOM_portray )
  { return PL_WRT_ATTVAR_PORTRAY;
  } else
    return 0;
}


static const opt_spec write_term_options[] = 
{ { ATOM_quoted,	    OPT_BOOL },
  { ATOM_ignore_ops,	    OPT_BOOL },
  { ATOM_numbervars,        OPT_BOOL },
  { ATOM_portray,           OPT_BOOL },
  { ATOM_character_escapes, OPT_BOOL },
  { ATOM_max_depth,	    OPT_INT  },
  { ATOM_module,	    OPT_ATOM },
  { ATOM_backquoted_string, OPT_BOOL },
  { ATOM_attributes,	    OPT_ATOM },
  { ATOM_priority,	    OPT_INT },
  { NULL_ATOM,	     	    0 }
};

word
pl_write_term3(term_t stream, term_t term, term_t opts)
{ bool quoted     = FALSE;
  bool ignore_ops = FALSE;
  bool numbervars = -1;			/* not set */
  bool portray    = FALSE;
  bool bqstring   = trueFeature(BACKQUOTED_STRING_FEATURE);
  bool charescape = -1;			/* not set */
  atom_t mname    = ATOM_user;
  atom_t attr     = ATOM_nil;
  int  priority   = 1200;
  IOSTREAM *s;
  write_options options;

  memset(&options, 0, sizeof(options));

  if ( !scan_options(opts, 0, ATOM_write_option, write_term_options,
		     &quoted, &ignore_ops, &numbervars, &portray,
		     &charescape, &options.max_depth, &mname,
		     &bqstring, &attr, &priority) )
    fail;

  if ( attr == ATOM_nil )
  { options.flags |= LD->feature.write_attributes;
  } else
  { int mask = writeAttributeMask(attr);

    if ( !mask )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_write_option, opts);
    
    options.flags |= mask;
  }
  if ( priority < 0 || priority > OP_MAXPRIORITY )
  { term_t t = PL_new_term_ref();
    PL_put_integer(t, priority);

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_operator_priority, t);
  }
  if ( !getOutputStream(stream, &s) )
    fail;
  
  options.module = lookupModule(mname);
  if ( charescape == TRUE ||
       (charescape == -1 && true(options.module, CHARESCAPE)) )
    options.flags |= PL_WRT_CHARESCAPES;
  if ( numbervars == -1 )
    numbervars = (portray ? TRUE : FALSE);
  
  if ( quoted )     options.flags |= PL_WRT_QUOTED;
  if ( ignore_ops ) options.flags |= PL_WRT_IGNOREOPS;
  if ( numbervars ) options.flags |= PL_WRT_NUMBERVARS;
  if ( portray )    options.flags |= PL_WRT_PORTRAY;
  if ( bqstring )   options.flags |= PL_WRT_BACKQUOTED_STRING;

  options.out = s;
  PutOpenToken(EOF, s);			/* reset this */
  if ( (options.flags & PL_WRT_QUOTED) && !(s->flags&SIO_REPPL) )
  { s->flags |= SIO_REPPL;
    writeTerm(term, priority, &options);
    s->flags &= ~SIO_REPPL;
  } else
  { writeTerm(term, priority, &options);
  }

  return streamStatus(s);
}


word
pl_write_term(term_t term, term_t options)
{ return pl_write_term3(0, term, options);
}


int
PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags)
{ write_options options;

  memset(&options, 0, sizeof(options));
  options.flags	    = flags;
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

    memset(&options, 0, sizeof(options));
    options.flags     = flags;
    options.out	      = s;
    options.module    = MODULE_user;
    if ( options.module && true(options.module, CHARESCAPE) )
      options.flags |= PL_WRT_CHARESCAPES;
    if ( trueFeature(BACKQUOTED_STRING_FEATURE) )
      options.flags |= PL_WRT_BACKQUOTED_STRING;

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
{ return do_write2(stream, term,
		   PL_WRT_PORTRAY|PL_WRT_NUMBERVARS);
}

word
pl_write_canonical2(term_t stream, term_t term)
{ fid_t fid = PL_open_foreign_frame();
  nv_options options;
  word rc;

  options.functor = FUNCTOR_isovar1;
  options.on_attvar = AV_SKIP;
  options.singletons = TRUE;
  numberVars(term, &options, 0 PASS_LD);
  rc = do_write2(stream, term,
		 PL_WRT_QUOTED|PL_WRT_IGNOREOPS|PL_WRT_NUMBERVARS);
  PL_discard_foreign_frame(fid);

  return rc;
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

word					/* for debugging purposes! */
pl_writeln(term_t term)
{ if ( pl_write2(0, term) && pl_nl() )
    succeed;

  fail;
}



