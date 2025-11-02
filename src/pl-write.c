/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include "pl-write.h"
#include "pl-arith.h"
#include "pl-dict.h"
#include "pl-fli.h"
#include "pl-read.h"
#include "pl-attvar.h"
#include "pl-op.h"
#include "pl-wam.h"
#include "pl-prims.h"
#include "pl-modul.h"
#include "pl-setup.h"
#include <math.h>
#include "os/pl-dtoa.h"
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "os/pl-prologflag.h"
#include "os/pl-fmt.h"
#include "pl-termset.h"
#include <stdio.h>			/* sprintf() */
#include <errno.h>
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

typedef struct
{ unsigned int flags;			/* PL_WRT_* flags */
  int   max_depth;			/* depth limit */
  int   depth;				/* current depth */
  atom_t integer_format;		/* How to format integers */
  atom_t float_format;			/* How to format floats */
  atom_t spacing;			/* Where to insert spaces */
  Module module;			/* Module for operators */
  IOSTREAM *out;			/* stream to write to */
  term_t portray_goal;			/* call/2 activated portray hook */
  term_t write_options;			/* original write options */
  term_t prec_opt;			/* term in write options with prec */
} write_options;

#define WRITE_OPTIONS_DEFAULTS \
	{ .spacing = ATOM_standard, \
	  .integer_format = ATOM_int_format_specifier, \
	  .float_format = ATOM_float_format_specifier \
	}

#define W_OP_ARG	1		/* writeTerm() location argument */

#define W_TOP		0		/* top term */
#define W_LIST_ARG	0		/* normal list argument */
#define W_LIST_TAIL	0		/* List tail (behind |) */
#define W_COMPOUND_ARG	0		/* f(arg) */
#define W_KEY		0		/* dict key */
#define W_VALUE		0		/* dict value */
#define W_TAG		0		/* dict tag */
#define W_BLOCK_OP	0		/* {} or [] block operator */
#define W_PREFIX_ARG	W_OP_ARG	/* f arg */
#define W_POSTFIX_ARG	W_OP_ARG	/* arg f */
#define W_INFIX_ARG1	W_OP_ARG	/* arg1 f arg2 */
#define W_INFIX_ARG2	W_OP_ARG	/* arg1 f arg2 */

#if USE_LD_MACROS
#define	enterPortray(_)		LDFUNC(enterPortray, _)
#define	leavePortray(_)		LDFUNC(leavePortray, _)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static bool	writeTerm2(term_t term, int prec,
			   write_options *options, int flags) WUNUSED;
static bool	writeTerm(term_t t, int prec,
			  write_options *options, int flags) WUNUSED;
static int	PutToken(const char *s, IOSTREAM *stream);
static int	writeAtom(atom_t a, write_options *options);
static int	callPortray(term_t arg, int prec, write_options *options);
static int	enterPortray(void);
static void	leavePortray(void);

#undef LDFUNC_DECLARATIONS

char *
var_name_ptr(DECL_LD Word p, char *name)
{ size_t iref;

  deRef(p);

  if (p > (Word) lBase)
    iref = ((Word)p - (Word)lBase)*2+1;
  else
    iref = ((Word)p - (Word)gBase)*2;

  Ssprintf(name, "_%" PRIi64, (int64_t)iref);

  return name;
}


char *
varName(term_t t, char *name)
{ GET_LD
  Word p = valTermRef(t);

  return var_name_ptr(p, name);
}


static bool
atomIsVarName(atom_t a)
{ Atom atom = atomValue(a);

  if ( isoff(atom->type, PL_BLOB_TEXT) || atom->length == 0 )
    return false;
  if ( isUCSAtom(atom) )
  { pl_wchar_t *w = (pl_wchar_t*)atom->name;
    size_t len = atom->length / sizeof(pl_wchar_t);

    return atom_varnameW(w, len);
  } else
  { const char *s = atom->name;
    size_t len = atom->length;

    if ( isUpper(*s) || *s == '_' )
    { for(s++; --len > 0; s++)
      { if ( !isAlpha(*s) )
	  return false;
      }

      return true;
    }

    return false;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return:	true:  processes
	false: not processed
	-1:    error
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define writeNumberVar(t, options) LDFUNC(writeNumberVar, t, options)
static int
writeNumberVar(DECL_LD term_t t, write_options *options)
{ Word p = valTermRef(t);
  Functor f;

  deRef(p);
  if ( !isTerm(*p) )
    return false;
  f = valueTerm(*p);

  if ( f->definition != FUNCTOR_isovar1 )
    return false;

  if ( LD->var_names.numbervars_frame )
  { FliFrame fr = (FliFrame)valTermRef(LD->var_names.numbervars_frame);

    FLI_ASSERT_VALID(fr);
    if ( isoff(options, PL_WRT_NUMBERVARS) &&
         fr->mark.globaltop.as_ptr > (Word)f )
      return false;			/* older $VAR term */
  }

  p = &f->arguments[0];
  deRef(p);
  if ( isTaggedInt(*p) )
  { int64_t n = valInt(*p);
    char buf[32];			/* Max is H354745078340568300 */

    if ( n < 0 )
    { sprintf(buf, "S_%" PRId64, -n);
    } else
    { int i = (int)(n % 26);
      int64_t j = n / 26;

      if ( j == 0 )
      { buf[0] = (char)(i+'A');
	buf[1] = EOS;
      } else
      { sprintf(buf, "%c%" PRId64, i+'A', j);
      }
    }

    return PutToken(buf, options->out) ? true : -1;
  }

  if ( isAtom(*p) && atomIsVarName(word2atom(*p)) )
  { write_options o2 = *options;
    clear(&o2, PL_WRT_QUOTED);

    return writeAtom(word2atom(*p), &o2) ? true : -1;
  }

  return false;
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
truePrologFlagNoLD(unsigned int flag)
{ GET_LD

  return truePrologFlag(flag);
}


static inline int
wr_is_symbol(int c, write_options *options)
{ return ( isSymbol(c) ||
	   (c == '`' &&
	    options &&
	    (options->flags & PL_WRT_BACKQUOTE_IS_SYMBOL)) );
}

static int
code_requires_quoted(int c, IOSTREAM *fd, int flags)
{ if ( c > 0x7f && (flags&PL_WRT_QUOTE_NON_ASCII) )
    return true;
  if ( fd && Scanrepresent(c, fd) != 0 )
    return true;

  return false;
}

static int
atomType(atom_t a, write_options *options)
{ Atom atom = atomValue(a);
  char *s = atom->name;
  size_t len = atom->length;
  IOSTREAM *fd = options ? options->out : NULL;
  Module m = options ? options->module : MODULE_user;
  int flags = options ? options->flags : 0;

  if ( len == 0 )
    return AT_QUOTE;

  if ( isLower(*s) || (ison(m, M_VARPREFIX) && isAlpha(*s)) )
  { do
    { for( ++s;
	   --len > 0 && isAlpha(*s) && !code_requires_quoted(*s, fd, flags);
	   s++)
	;
    } while ( len >= 2 &&
	      *s == '.' && isAlpha(s[1]) &&
	      truePrologFlagNoLD(PLFLAG_DOT_IN_ATOM) &&
	      (!options || isoff(options, PL_WRT_NODOTINATOM))
	    );

    return len == 0 ? AT_LOWER : AT_QUOTE;
  }

  if ( wr_is_symbol(*s, options) )
  { size_t left = len;

    if ( len == 1 && s[0] == '.' )
      return AT_FULLSTOP;
    if ( len >= 2 && s[0] == '/' && s[1] == '*' )
      return AT_QUOTE;

    for( ;
	 left > 0 && wr_is_symbol(*s, options) &&
	 !code_requires_quoted(*s, fd, flags);
	 s++, left--)
      ;
    if ( left > 0 )
      return AT_QUOTE;

    return AT_SYMBOL;
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


static int
unquoted_atomW(atom_t atom, IOSTREAM *fd, int flags)
{ Atom ap = atomValue(atom);
  const pl_wchar_t *s = (const pl_wchar_t*)ap->name;
  const pl_wchar_t *s1;
  size_t len = ap->length/sizeof(pl_wchar_t);
  const pl_wchar_t *e = &s[len];
  int c;

  if ( len == 0 )
    return false;

  s1 = get_wchar(s, &c);
  if ( !f_is_prolog_atom_start(c) )	/* Sequence of symbol chars */
  { while ( s < e )
    { s = get_wchar(s, &c);

      if ( !f_is_prolog_symbol(c) ||
	   code_requires_quoted(c, fd, flags) )
	return false;
    }
    return true;
  }

  if ( code_requires_quoted(c, fd, flags) )
    return false;

  s = s1;				/* 1st char is ID_START */
  int dot_in_atom = ( truePrologFlagNoLD(PLFLAG_DOT_IN_ATOM) &&
		      !(flags&PL_WRT_NODOTINATOM) );

  while ( s < e )
  { s = get_wchar(s, &c);

    if ( c == '.' && dot_in_atom && s < e )
    { s = get_wchar(s, &c);
      if ( f_is_prolog_identifier_continue(c) &&
	   !code_requires_quoted(c, fd, flags) )
	continue;
      return false;
    }

    if ( !f_is_prolog_identifier_continue(c) ||
	 code_requires_quoted(c, fd, flags) )
      return false;
  }

  return true;
}


int
unquoted_atom(atom_t a)
{ Atom ap = atomValue(a);

  if ( ison(ap->type, PL_BLOB_TEXT) )
  { if ( !ap->type->write )		/* ordinary atoms */
    { return atomType(a, NULL) != AT_QUOTE;
    } else if ( isUCSAtom(ap) )		/* wide atoms */
    { return unquoted_atomW(a, NULL, 0);
    }
  }

  return false;
}


		 /*******************************
		 *	 PRIMITIVE WRITES	*
		 *******************************/

#define TRUE_WITH_SPACE 2		/* OK, and emitted leading space before token */

static bool
Putc(int c, IOSTREAM *s)
{ return Sputcode(c, s) != EOF;
}


static bool
PutString(const char *str, IOSTREAM *s)
{ const unsigned char *q = (const unsigned char *)str;

  for( ; *q != EOS; q++ )
  { if ( Sputcode(*q, s) == EOF )
      return false;
  }

  return true;
}


static bool
PutComma(write_options *options)
{ if ( options->spacing == ATOM_next_argument )
    return PutString(", ", options->out);
  else
    return PutString(",", options->out);
}


static bool
PutBar(write_options *options)
{ if ( options->spacing == ATOM_next_argument )
    return PutString("| ", options->out);
  else
    return PutString("|", options->out);
}


static bool
PutStringN(const char *str, size_t length, IOSTREAM *s)
{ size_t i;
  const unsigned char *q = (const unsigned char *)str;

  for(i=0; i<length; i++, q++)
  { if ( Sputcode(*q, s) == EOF )
      return false;
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PutOpenToken() inserts a space in the  output stream if the last-written
and given character require a space to ensure a token-break.

The C_* flags denote special cases handled  using a flag. The first flag
is 0x200000, which is above the Unicode range.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define C_PREFIX_SIGN		0x00200000	/* +/- as prefix op */
#define C_PREFIX_OP		0x00400000	/* any prefix op */
#define C_INFIX_OP		0x00800000	/* any infix op */
#define C_MASK			0xffe00000

#define isquote(c) ((c) == '\'' || (c) == '"')

static bool
needSpace(int c, IOSTREAM *s)
{ if ( c == EOF )
  { s->lastc = EOF;
    return false;
  }
  if ( s->lastc == EOF )
    return false;

  if ( (s->lastc&C_PREFIX_SIGN) && (isDigit(c) || f_is_prolog_symbol(c)) )
    return true;
  if ( (s->lastc&C_PREFIX_OP) && ( c == '(' || c == '{' ) )
    return true;				/* avoid op(...) */
  if ( (s->lastc&C_INFIX_OP) && c == '(' )
    return false;

  s->lastc &= ~C_MASK;

  if ( ((f_is_prolog_identifier_continue(s->lastc) &&
	 f_is_prolog_identifier_continue(c)) ||
	(f_is_prolog_symbol(s->lastc) && f_is_prolog_symbol(c)) ||
	(c == '(' && !(isPunctW(s->lastc)||isBlank(s->lastc)) ) ||
	(c == '\'' && (isDigit(s->lastc))) ||
	(isquote(c) && s->lastc == c)
       ) )
    return true;

  return false;
}


static int			/* false, true, TRUE_WITH_SPACE */
PutOpenToken(int c, IOSTREAM *s)
{ if ( needSpace(c, s) )
  { TRY(Putc(' ', s));
    return TRUE_WITH_SPACE;
  }

  return true;
}


static int			/* false, true, TRUE_WITH_SPACE */
PutToken(const char *s, IOSTREAM *stream)
{ if ( s[0] )
  { int rc;

    TRY(rc=PutOpenToken(s[0]&0xff, stream));
    TRY(PutString(s, stream));

    return rc;
  }

  return true;
}


static int			/* false, true, TRUE_WITH_SPACE */
PutTokenN(const char *s, size_t len, IOSTREAM *stream)
{ if ( len > 0 )
  { int rc;

    TRY(rc=PutOpenToken(s[0]&0xff, stream));
    TRY(PutStringN(s, len, stream));

    return rc;
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PutOpenBrace()/PutCloseBrace() are used to put  additional braces around
a term to avoid an operator  precedence   problem.  If  the last emitted
character  is  alphanumerical,  there  should  be  a  space  before  the
openbrace to avoid interpretation as a term.   E.g. not (a,b) instead of
not(a,b).  Reported by Stefan.Mueller@dfki.de.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int			/* false, true, TRUE_WITH_SPACE */
PutOpenBrace(IOSTREAM *s)
{ int rc;

  TRY(rc=PutOpenToken('(', s));
  TRY(Putc('(', s));

  return rc;
}


static bool
PutCloseBrace(IOSTREAM *s)
{ return Putc(')', s);
}


static bool
putQuoted(int c, int quote, int flags, IOSTREAM *stream)
{ if ( (flags & PL_WRT_CHARESCAPES) )
  { if ( !unicode_quoted_escape(c) && c != quote && c != '\\' &&
	 (!stream || Scanrepresent(c, stream) == 0) )
    { TRY(Putc(c, stream));
    } else
    { char esc[10];			/* Longest is UXXXXXXXX */

      esc[1] = EOS;

      if ( c == quote )
      { esc[0] = (char)c;
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
	    if ( (flags & PL_WRT_CHARESCAPES_UNICODE) )
	    { if ( c <= 0xffff )
		Ssprintf(esc, "u%04X", c);
	      else
		Ssprintf(esc, "U%08X", c);
	    } else
	    { Ssprintf(esc, "x%X\\", c);
	    }
	}
      }
      if ( !Putc('\\', stream) ||
	   !PutString(esc, stream) )
	fail;
    }
  } else
  { if ( !Putc(c, stream) )
      fail;
    if ( c == quote )	/* write '' */
    { if ( !Putc(c, stream) )
	fail;
    }
  }

  return true;
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
writeAttributes(term_t t, int prec, write_options *options, int flags)
{ term_t name  = PL_new_term_ref();
  term_t value = PL_new_term_ref();
  term_t att   = PL_new_term_ref();

  for(bool first=true; PL_is_functor(t, FUNCTOR_att3); first = false)
  { _PL_get_arg(1, t, name);
    _PL_get_arg(2, t, value);
    _PL_get_arg(3, t, t);

    if ( !first && !PutComma(options) )
      return false;
    if ( !PL_cons_functor(att, FUNCTOR_colon2, name, value) ||
	 !writeTerm(att, 999, options, W_COMPOUND_ARG) )
      return false;
  }
  if ( !PL_get_nil(t) &&
       !writeTerm(t, 999, options, W_COMPOUND_ARG) )
    return false;

  PL_reset_term_refs(name);
  return true;
}

static bool
writeAttVar(term_t av, write_options *options)
{ GET_LD
  char buf[32];

  TRY(PutToken(varName(av, buf), options->out));

  if ( (options->flags & PL_WRT_ATTVAR_DOTS) )
  { return PutString("{...}", options->out);
  } else if ( (options->flags & PL_WRT_ATTVAR_WRITE) )
  { fid_t fid;
    term_t a;

    if ( !(fid = PL_open_foreign_frame()) )
      return false;

    Sputcode('{', options->out);
    a = PL_new_term_ref();
    PL_get_attr(av, a);
    if ( !writeAttributes(a, 1200, options, W_TOP) )
      return false;
    Sputcode('}', options->out);
    PL_close_foreign_frame(fid);

    return true;
  } else if ( (options->flags & PL_WRT_ATTVAR_PORTRAY) &&
	      GD->halt.cleaning <= CLN_PROLOG )
  { predicate_t pred;
    IOSTREAM *old;
    wakeup_state wstate;
    int rc;

    pred = _PL_predicate("portray_attvar", 1, "$attvar",
			 &GD->procedures.portray_attvar1);

    if ( !enterPortray() )
      return false;
    if ( !saveWakeup(&wstate, true) )
      return false;
    old = Scurout;
    Scurout = options->out;
    rc = PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION, pred, av);
    if ( rc != true && !PL_exception(0) )
      rc = true;
    Scurout = old;
    restoreWakeup(&wstate);
    leavePortray();

    return rc;
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


static int				/* false, true or TRUE_WITH_SPACE */
writeAtom(atom_t a, write_options *options)
{ Atom atom = atomValue(a);

  if ( (options->flags & PL_WRT_BLOB_PORTRAY) &&
       isoff(atom->type, PL_BLOB_TEXT) &&
       GD->halt.cleaning <= CLN_PROLOG &&
       a != ATOM_nil )
  { GET_LD
    int rc;
    fid_t fid;
    term_t av;

    if ( !(fid = PL_open_foreign_frame()) )
      return false;
    av = PL_new_term_ref();
    PL_put_atom(av, a);
    rc = callPortray(av, 1200, options);
    PL_close_foreign_frame(fid);
    switch(rc)
    { case true:
	return true;
      case false:
	break;
      default:
	return false;				/* error */
    }
  }

  if ( atom->type->write )
    return ((*atom->type->write)(options->out, a, options->flags) &&
	    !Sferror(options->out));
  if ( isoff(atom->type, PL_BLOB_TEXT) )
    return writeBlob(a, options);

  if ( ison(options, PL_WRT_QUOTED) )
  { switch( atomType(a, options) )
    { case AT_LOWER:
      case AT_SYMBOL:
      case AT_SOLO:
      case AT_SPECIAL:
	return PutToken(atom->name, options->out);
      case AT_QUOTE:
      case AT_FULLSTOP:
      default:
      { int rc;

	TRY(rc=PutOpenToken('\'', options->out));
	TRY(writeQuoted(options->out,
			atom->name,
			atom->length,
			'\'', options));
	return rc;
      }
    }
  } else
    return PutTokenN(atom->name, atom->length, options->out);
}


bool
writeAtomToStream(IOSTREAM *s, atom_t atom)
{ write_options options = WRITE_OPTIONS_DEFAULTS;

  options.out = s;
  options.module = MODULE_user;

  return !!writeAtom(atom, &options);
}


bool
writeUCSAtom(IOSTREAM *fd, atom_t atom, int flags)
{ Atom a = atomValue(atom);
  const pl_wchar_t *s = (const pl_wchar_t*)a->name;
  size_t len = a->length/sizeof(pl_wchar_t);
  const pl_wchar_t *e = &s[len];

  if ( (flags&PL_WRT_QUOTED) && !unquoted_atomW(atom, fd, flags) )
  { pl_wchar_t quote = L'\'';

    TRY(PutOpenToken(quote, fd) &&
	Putc(quote, fd));

    while(s < e)
    { int c;

      s = get_wchar(s, &c);
      TRY(putQuoted(c, quote, flags, fd));
    }

    return Putc(quote, fd);
  }

  if ( e > s )
  { int c;

    get_wchar(s, &c);
    if ( !PutOpenToken(c, fd) )
      return false;

    while ( s < e )
    { s = get_wchar(s, &c);

      if ( !Putc(c, fd) )
	return false;
    }
  }

  return true;
}

#ifdef O_RESERVED_SYMBOLS
bool
writeReservedSymbol(IOSTREAM *fd, atom_t atom, int flags)
{ Atom a = atomValue(atom);
  const char *s = a->name;
  size_t len = a->length;
  const char *e = &s[len];

  if ( atom == ATOM_nil )
    return !!PutToken("[]", fd);

  if ( (flags&PL_WRT_QUOTED) )
  { char quote = '\'';

    if ( PutOpenToken('C', fd) &&
	 Putc('C', fd) &&
	 Putc(quote, fd) )
    { while(s < e)
      { if ( !putQuoted(*s++, quote, flags, fd) )
	  return false;
      }

      return Putc(quote, fd);
    }
  }

  if ( s < e && !PutOpenToken(s[0], fd) )
    return false;
  for( ; s<e; s++)
  { if ( !Putc(*s, fd) )
      return false;
  }

  return true;
}
#endif


#if O_STRING

static int
writeString(term_t t, write_options *options)
{ GET_LD
  PL_chars_t txt;
  int rc = true;

  PL_STRINGS_MARK();
  PL_get_text(t, &txt, CVT_STRING);

  if ( ison(options, PL_WRT_QUOTED) )
  { int quote;

    if ( ison(options, PL_WRT_BACKQUOTED_STRING) )
      quote = '`';
    else
      quote = '"';

    if ( !(rc=Putc(quote, options->out)) )
      goto out;

    switch(txt.encoding)
    { case ENC_ISO_LATIN_1:
      { const unsigned char *s = (const unsigned char*)txt.text.t;
	const unsigned char *e = &s[txt.length];

	while(s<e)
	{ int chr = *s++;

	  if ( !(rc=putQuoted(chr, quote, options->flags, options->out)) )
	    goto out;
	}
	break;
      }
      case ENC_WCHAR:
      { const wchar_t *s = txt.text.w;
	const wchar_t *e = &s[txt.length];

	while(s<e)
	{ int chr;

	  s = get_wchar(s, &chr);
	  if ( !(rc=putQuoted(chr, quote, options->flags, options->out)) )
	    goto out;
	}
	break;
      }
      default:
	assert(0);
    }
    rc = Putc(quote, options->out);
  } else
  { switch(txt.encoding)
    { case ENC_ISO_LATIN_1:
      { const unsigned char *s = (const unsigned char*)txt.text.t;
	const unsigned char *e = &s[txt.length];

	while(s<e)
	{ int chr = *s++;

	  if ( !(rc=Putc(chr, options->out)) )
	    goto out;
	}
	break;
      }
      case ENC_WCHAR:
      { const wchar_t *s = txt.text.w;
	const wchar_t *e = &s[txt.length];

	while(s<e)
	{ int chr;

	  s = get_wchar(s, &chr);
	  if ( !(rc=Putc(chr, options->out)) )
	    goto out;
	}
	break;
      }
      default:
	assert(0);
    }
  }
  PL_STRINGS_RELEASE();

out:
  PL_free_text(&txt);

  return rc;
}

#endif /*O_STRING*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formatting a float. This used  to  use   sprintf(),  but  there  are two
problems with this. First of all, this uses the current locale, which is
complicated to avoid. Second, it does not provide a mode that guarantees
reliable read-back. Using %g gets closest,   but %.15g doesn't guarantee
read-back and %.17g does, but prints 0.1 as 0.100..001, etc.

This uses dtoa.c. See pl-dtoa.c for how this is packed into SWI-Prolog.

TBD: The number of cases are large. We should see whether it is possible
to clean this up a bit. The 5 cases   as  such are real: there is no way
around these.

NaN         and         Inf         printing           based          on
http://eclipseclp.org/Specs/core_update_float.html, with comments   from
Joachim Schimpf.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_IEEE754_H
#include <ieee754.h>
#else
union ieee754_double
{ double d;
  struct
  {
#ifdef WORDS_BIGENDIAN
    unsigned int negative:1;
    unsigned int exponent:11;
    unsigned int mantissa0:20;
    unsigned int mantissa1:32;
#else
#ifdef FLOAT_WORDS_BIGENDIAN
    unsigned int mantissa0:20;
    unsigned int exponent:11;
    unsigned int negative:1;
    unsigned int mantissa1:32;
#else
    unsigned int mantissa1:32;
    unsigned int mantissa0:20;
    unsigned int exponent:11;
    unsigned int negative:1;
#endif
#endif
  } ieee;
};
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Joachim Schimpf: The exponent is  stored  with   a  "bias"  of  1023, so
3ff=1023 means 0. And 0 means that the   mantissa is to be multiplied by
2^0 = 1, maybe that's where my confusion came from...

To add to the confusion, the mantissa  is a "hidden bit" representation,
i.e. its actual value is 1.<the 52 bits   actually  stored>, so with a 0
exponent the value of the resulting number is always >= 1 and < 2.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

double
NaN_value(double f)
{ union ieee754_double u;

  u.d = f;
  assert(u.ieee.exponent == 0x7ff);	/* NaN exponent */
  u.ieee.exponent = 0x3ff;

  return u.d;
}


strnumstat
make_nan(double *f)
{ union ieee754_double u;

  u.d = *f;
  u.ieee.exponent = 0x7ff;		/* NaN exponent */
  if ( isnan(u.d) )
  { *f = u.d;
    return NUM_OK;
  }

  return NUM_CONSTRANGE;		/* 1.0NaN is in fact 1.0Inf */
}


static size_t
writeNaN(char *buf, size_t len, double f)
{ size_t sz = format_float(buf, len, NaN_value(f), 3, 'e');

  if ( sz+4 < len )
    strcpy(buf+sz, "NaN");
  return sz+3;
}

static size_t
writeINF(char *buf, size_t len, double f)
{ number n;

  n.value.f = f;
  n.type = V_FLOAT;

  if ( ar_signbit(&n) < 0 )
  { if ( len >= 8 )
      strcpy(buf, "-1.0Inf");
    return 7;
  } else
  { if ( len >= 7 )
      strcpy(buf, "1.0Inf");
    return 6;
  }
}


static size_t
format_special_float(char *buf, size_t len, double f)
{
#ifdef HAVE_FPCLASSIFY
  switch(fpclassify(f))
  { case FP_NAN:
      return writeNaN(buf, len, f);
    case FP_INFINITE:
      return writeINF(buf, len, f);
  }
#else
#ifdef HAVE_FPCLASS
  switch(fpclass(f))
  { case FP_SNAN:
    case FP_QNAN:
      return writeNaN(buf, len, f);
    case FP_NINF:
    case FP_PINF:
      return writeINF(buf, len, f);
    case FP_NDENORM:		/* pos/neg denormalized non-zero */
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
      return writeNaN(buf, len, f);
    case _FPCLASS_NINF:
    case _FPCLASS_PINF:
      return writeINF(buf, len, f);
  }
#else
#ifdef HAVE_ISINF
  if ( isinf(f) )
  { return writeINF(buf, len, f);
  } else
#endif
#ifdef HAVE_ISNAN
  if ( isnan(f) )
  { return writeNaN(buf, len, f);
  }
#endif
#endif /*HAVE__FPCLASS*/
#endif /*HAVE_FPCLASS*/
#endif /*HAVE_FPCLASSIFY*/

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Format a float in fixed point or exponential notation with the minimal
number of digits such that it is read back to the same value.

Parameters:
  - `buf` is where the result is stored.
  - `size` is the size of `buf`
  - `f` is the float to be formatted
  - `N` is the number as used by format `~Nh`.  See format/2.
  - `E` is 'e' or 'E'

Return:
  - Number of characters output, minus the terminating 0
  - If truncated, the return value >= size
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

size_t
format_float(char *buf, size_t size, double f, int N, char E)
{ char *end, *o=buf, *s;
  int decpt, sign;
  size_t sz;
  char *limit = &buf[size];

  if ( (sz=format_special_float(buf, size, f)) )
    return sz;

  s = dtoa(f, 0, 30, &decpt, &sign, &end);
  DEBUG(MSG_WRITE_FLOAT,
	Sdprintf("dtoa(): decpt=%d, sign=%d, len = %d, '%s'\n",
		 decpt, sign, end-s, s));

#undef OUT				/* minwindef.h */
#define OUT(c) \
	do { if ( o<limit ) *o++ = (c); else o++; } while(0)
#define OUTN(s,n) \
	do { if ( o+(n) <= limit ) memcpy(o, s, (n)); o+=(n); } while(0)
#define OUTS(s) OUTN(s, strlen(s))

  if ( sign )
    OUT('-');

  if ( decpt <= 0 )			/* decimal dot before */
  { int e = decpt-1;
    if ( N < 0 || e < -N-1 )
    { OUT(s[0]);
      OUT('.');
      if ( end-s > 1 )
      { OUTN(s+1, end-s-1);
      } else
	OUT('0');
      char eb[16];
      snprintf(eb, sizeof(eb), "%c%03d", E, e);
      OUTS(eb);
    } else
    { int i;

      OUT('0');
      OUT('.');
      for(i=0; i < -decpt; i++)
	OUT('0');
      OUTN(s, end-s);
    }
  } else if ( N >=0 && end-s > decpt ) /* decimal dot inside */
  { OUTN(s, decpt);
    OUT('.');
    OUTN(s+decpt, end-s-decpt);
  } else				/* decimal dot after */
  { int i;
    int d = (int)(end-s);
    int trailing = decpt-d;
    int exp = trailing+d-1;

    DEBUG(MSG_WRITE_FLOAT,
	  Sdprintf("trailing = %d; exp=%d; d=%d; N=%d\n",
		   trailing, exp, d, N));
    if ( N < 0 || exp >= N+d )	/* over precision: use eE */
    { OUT(s[0]);
      OUT('.');
      if ( d > 1 )
      { OUTN(s+1, d-1);
      } else
	OUT('0');
      char eb[16];
      snprintf(eb, sizeof(eb), "%c+%02d", E, exp);
      OUTS(eb);
    } else				/* within precision trail with .0 */
    { OUTN(s, end-s);

      for(i=d; i<decpt; i++)
	OUT('0');
      OUT('.');
      OUT('0');
    }
  }

#undef OUTN
#undef OUTS
#undef OUT

  freedtoa(s);

  if ( o < limit )
    *o = 0;
  else if ( size > 0 )
    buf[size-1] = 0;
  return o-buf;
}

#ifdef O_BIGNUM
static bool
mpz_get_str_ex(char *buf, int base, mpz_t mpz)
{ int rc;

  /* mpz_get_str() can perform large intermediate allocations */
  EXCEPTION_GUARDED({ mpz_get_str(buf, 10, mpz);
                      rc = true;
		    },
		    { rc = false;
		    });

  return rc;
}


#define writeMPZ(mpz, options) LDFUNC(writeMPZ, mpz, options)
static bool
writeMPZ(DECL_LD mpz_t mpz, write_options *options)
{ char tmp[1024];
  char *buf;
  size_t sz = (mpz_sizeinbase(mpz, 2)*10)/3 + 10; /* log2(10)=3.322 */
  bool rc;

  if ( sz <= sizeof(tmp) )
    buf = tmp;
  else if ( !(buf = tmp_malloc(sz)) )
    return PL_no_memory();

  if ( LD->gmp.context )
  { rc = mpz_get_str_ex(buf, 10, mpz);
  } else
  { AR_CTX;
    AR_BEGIN();
    rc = mpz_get_str_ex(buf, 10, mpz);
    AR_END();
  }

  rc = rc && PutToken(buf, options->out);
  if ( buf != tmp )
    tmp_free(buf);

  return rc;
}
#endif

static bool
separate_number(IOSTREAM *s, Number n, PL_chars_t *fmt)
{ int c;

  if ( fmt->length > 0 )
    c = text_get_char(fmt, 0);
  else
    c = -1;

  if ( !(c >= 0 && c != '~') )
    c = ar_signbit(n) < 0 ? '-' : '0';

  if ( needSpace(c, s) )
    return Putc(' ', s);

  return true;
}

#define writeNumber(t, options) LDFUNC(writeNumber, t, options)

static bool
writeNumber(DECL_LD term_t t, write_options *options)
{ number n;

  PL_get_number(t, &n);

  switch(n.type)
  { case V_INTEGER:
    case V_FLOAT:
#ifdef O_BIGNUM
    case V_MPZ:
#endif
    { PL_chars_t fmt;

      get_atom_text(n.type == V_FLOAT ? options->float_format
				      : options->integer_format,
		    &fmt);
      if ( !separate_number(options->out, &n, &fmt) )
	return false;

      return do_format(options->out, &fmt, 1, t, options->module);
    }
#ifdef O_BIGNUM
    case V_MPQ:
    { mpz_t num, den;			/* num/den */
      char sep = ison(options, PL_WRT_RAT_NATURAL) ? '/' : 'r';

      num[0] = *mpq_numref(n.value.mpq);
      den[0] = *mpq_denref(n.value.mpq);
      return ( writeMPZ(num, options) &&
	       Sputcode(sep, options->out) != EOF &&
	       (options->out->lastc = EOF) &&
	       writeMPZ(den, options) );
    }
#endif
    default:
      assert(0);
      return false;			/* make compiler happy */
  }
}



static bool
writePrimitive(term_t t, write_options *options)
{ GET_LD
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

  if ( PL_is_number(t) )		/* beware of automatic conversion */
    return writeNumber(t, options);

#if O_STRING
  if ( PL_is_string(t) )
    return writeString(t, options);
#endif /* O_STRING */

  assert(0);
  fail;
}


#define pl_nl(stream) LDFUNC(pl_nl, stream)
static int
pl_nl(DECL_LD term_t stream)
{ IOSTREAM *s;

  if ( getTextOutputStream(stream, &s) )
  { Sputcode('\n', s);
    return streamStatus(s);
  }

  return false;
}


static
PRED_IMPL("nl", 1, nl, PL_FA_ISO)
{ PRED_LD

  return pl_nl(A1);
}

static
PRED_IMPL("nl", 0, nl, PL_FA_ISO)
{ PRED_LD

  return pl_nl(0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call user:portray/1 if defined.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_write_options(term_t opts_in, write_options *options)
{ GET_LD
  term_t newlist = PL_new_term_ref();
  term_t precopt = PL_new_term_ref();
  fid_t fid = PL_open_foreign_frame();
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(opts_in);
  term_t newhead = PL_new_term_ref();
  term_t newtail = PL_copy_term_ref(newlist);
  int rc = true;

  while(rc && PL_get_list(tail, head, tail))
  { if ( !PL_is_functor(head, FUNCTOR_priority1) )
      rc = ( PL_unify_list(newtail, newhead, newtail) &&
	     PL_unify(newhead, head) );
  }

  if ( rc )
  { rc = ( PL_unify_list(newtail, head, newtail) &&
	   PL_unify_functor(head, FUNCTOR_priority1) &&
	   PL_get_arg(1, head, precopt) &&
	   PL_unify_nil(newtail) );
  }
  if ( rc )
  { options->write_options = newlist;
    options->prec_opt = precopt;
  }

  PL_close_foreign_frame(fid);
  return rc;
}


static int
enterPortray(DECL_LD)
{ if ( LD->IO.portray_nesting >= MAX_PORTRAY_NESTING )
    return PL_resource_error("portray_nesting");
  LD->IO.portray_nesting++;
  return true;
}


static void
leavePortray(DECL_LD)
{ LD->IO.portray_nesting--;
}


/* returns: -1: error, false: failed, true: succeeded
*/

static int
callPortray(term_t arg, int prec, write_options *options)
{ predicate_t pred;

  if ( GD->halt.cleaning > CLN_PROLOG )
    fail;				/* avoid dangerous callbacks */

  if ( options->portray_goal )
  { pred = _PL_predicate("call", 3, "user", &GD->procedures.call3);
  } else
  { pred = _PL_predicate("portray", 1, "user", &GD->procedures.portray);
    if ( !pred->definition->impl.any.defined )
      return false;
  }

  { GET_LD
    wakeup_state wstate;
    IOSTREAM *old = Scurout;
    int rval;
    term_t av;

    if ( !enterPortray() )
      return -1;
    if ( !saveWakeup(&wstate, true) )
      return -1;
    Scurout = options->out;
    if ( options->portray_goal )
    { av = PL_new_term_refs(3);

      PL_put_term(av+0, options->portray_goal);
      PL_put_term(av+1, arg);
      PL_unify_integer(options->prec_opt, prec);
      PL_put_term(av+2, options->write_options);
    } else
    { av = arg;
    }
    rval = PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
			     pred, av);
    if ( !rval && PL_exception(0) )
      rval = -1;
    Scurout = old;
    restoreWakeup(&wstate);
    leavePortray();

    return rval;
  }

  fail;
}


static bool
writeTerm(term_t t, int prec, write_options *options, int flags)
{ GET_LD
  int rval;
  int levelSave = options->depth;
  fid_t fid;

  if ( !(fid = PL_open_foreign_frame()) )
    return false;

  if ( PL_handle_signals() < 0 )
  { rval = false;
    goto out;
  }

  if ( ++options->depth > options->max_depth && options->max_depth )
  { PutOpenToken('.', options->out);
    rval = PutString("...", options->out);
  } else
  { rval = writeTerm2(t, prec, options, flags);
  }

out:
  options->depth = levelSave;
  PL_close_foreign_frame(fid);

  return rval;
}


static bool
writeList(term_t list, write_options *options)
{ GET_LD
  term_t head = PL_new_term_ref();
  term_t l    = PL_copy_term_ref(list);

  if ( isoff(options, PL_WRT_DOTLISTS|PL_WRT_NO_LISTS) )
  { TRY(Putc('[', options->out));
    for(;;)
    { PL_get_list(l, head, l);
      TRY(writeTerm(head, 999, options, W_LIST_ARG));

      if ( PL_get_nil(l) )
	break;
      if ( ++options->depth >= options->max_depth && options->max_depth )
	return PutString("|...]", options->out);
      if ( !PL_is_functor(l, FUNCTOR_dot2) )
      { TRY(Putc('|', options->out));
	TRY(writeTerm(l, 999, options, W_LIST_TAIL));
	break;
      }

      TRY(PutComma(options));
    }

    return Putc(']', options->out);
  } else
  { int depth = 0;

    for(;;)
    { PL_get_list(l, head, l);
      if ( ison(options, PL_WRT_DOTLISTS) )
      { if ( !PutToken(".", options->out) )
	  return false;
      } else
      { if ( !writeAtom(ATOM_dot, options) )
	  return false;
      }

      if ( !Putc('(', options->out) ||
	   !writeTerm(head, 999, options, W_COMPOUND_ARG) ||
	   !PutComma(options) )
	return false;

      depth++;

      if ( PL_get_nil(l) )
      { if ( !PutToken("[]", options->out) )
	  return false;
	break;
      }

      if ( ++options->depth >= options->max_depth && options->max_depth )
      { if ( !PutToken("...", options->out) )
	  return false;
	while(depth-->0)
	{ if ( !Putc(')', options->out) )
	    return false;
	}
	return true;
      }

      if ( !PL_is_functor(l, FUNCTOR_dot2) )
      { if ( !writeTerm(l, 999, options, W_COMPOUND_ARG) )
	  return false;
	break;
      }
    }

    while(depth-->0)
    { if ( !Putc(')', options->out) )
	return false;
    }
    return true;
  }
}


#define isBlockOp(t, arg, functor) LDFUNC(isBlockOp, t, arg, functor)
static int
isBlockOp(DECL_LD term_t t, term_t arg, atom_t functor)
{ if ( functor == ATOM_nil || functor == ATOM_curl )
  { _PL_get_arg(1, t, arg);
    if ( (functor == ATOM_nil  && PL_is_pair(arg)) ||
	 (functor == ATOM_curl && PL_is_functor(arg, FUNCTOR_curl1)) )
      return true;
  }

  return false;
}

#define writeDictPair(name, value, last, closure) LDFUNC(writeDictPair, name, value, last, closure)

static int
writeDictPair(DECL_LD term_t name, term_t value, int last, void *closure)
{ write_options *options = closure;

  if ( writeTerm(name, 1200, options, W_KEY) &&
       PutToken(":", options->out) &&
       writeTerm(value, 999, options, W_VALUE) &&
       (last || PutComma(options)) )
    return 0;				/* continue */

  return -1;
}


static bool
writeTerm2(term_t t, int prec, write_options *options, int flags)
{ GET_LD
  atom_t functor;
  size_t arity, n;
  unsigned char op_type;
  short op_pri;
  atom_t a;
  IOSTREAM *out = options->out;

  if ( !PL_is_variable(t) &&
       ison(options, PL_WRT_PORTRAY) )
  { switch( callPortray(t, prec, options) )
    { case true:
	return true;
      case false:
	break;
      default:					/* error */
	return false;
    }
  }

  if ( PL_get_atom(t, &a) )
  { if ( (flags&W_OP_ARG) && priorityOperator(options->module, a) > 0 )
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
  { if ( ison(options, PL_WRT_NUMBERVARS|PL_WRT_VARNAMES) )
    { switch( writeNumberVar(t, options) )
      { case -1:
	  return false;
	case true:
	  return true;
      }
    }

					/* handle {a,b,c} */
    if ( isoff(options, PL_WRT_BRACETERMS) &&
	 functor == ATOM_curl && arity == 1 )
    { term_t arg;

      if ( (arg=PL_new_term_ref()) &&
	   PL_get_arg(1, t, arg) &&
	   PutToken("{", out) &&
	   writeTerm(arg, 1200, options, W_TOP) &&
	   Putc('}', out) )
	return true;

      return false;
    }

					/* handle lists */
    if ( functor == ATOM_dot && arity == 2 )
      return writeList(t, options);

					/* handle dicts */
    if ( isoff(options, PL_WRT_NODICT) &&
	 functor == ATOM_dict && PL_is_dict(t) )
    { term_t class;

      if ( (class=PL_new_term_ref()) &&
	   PL_get_arg(1, t, class) )
      { if ( writeTerm(class, 1200, options, W_TAG) &&
	     Putc('{', out) &&
	     _PL_for_dict(t, writeDictPair, options, PL_FOR_DICT_SORTED) == 0 &&
	     Putc('}', out) )
	  return true;
      }

      return false;
    }
					/* operators */
    if ( isoff(options, PL_WRT_IGNOREOPS) )
    { term_t arg;

      if ( !(arg=PL_new_term_ref()) )
	return false;

      if ( arity == 1 ||
	  (arity == 2 && isBlockOp(t, arg, functor)) )
      {
					  /* op <term> */
	if ( currentOperator(options->module, functor, OP_PREFIX,
			     &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();
	  int embrace;

	  embrace = ( op_pri > prec );

	  if ( embrace )
	    TRY(PutOpenBrace(out));
	  if ( arity == 1 )
	  { TRY(writeAtom(functor, options));
	  } else
	  { _PL_get_arg(1, t, arg);
	    TRY(writeTerm(arg, 1200, options, W_BLOCK_OP));
	  }
				/* +/-(Number) : avoid parsing as number */
	  options->out->lastc |= C_PREFIX_OP;
	  if ( functor == ATOM_minus )
	    options->out->lastc |= C_PREFIX_SIGN;

	  _PL_get_arg(arity, t, arg);
	  TRY(writeTerm(arg,
			op_type == OP_FX ? op_pri-1 : op_pri,
			options, W_PREFIX_ARG));

	  if ( embrace )
	   TRY(PutCloseBrace(out));

	  succeed;
	}

					  /* <term> op */
	if ( currentOperator(options->module, functor, OP_POSTFIX,
			     &op_type, &op_pri) )
	{ term_t arg = PL_new_term_ref();

	  if ( op_pri > prec )
	    TRY(PutOpenBrace(out));
	  _PL_get_arg(arity, t, arg);
	  TRY(writeTerm(arg,
			op_type == OP_XF ? op_pri-1 : op_pri,
			options, W_POSTFIX_ARG));
	  if ( arity == 1 )
	  { TRY(writeAtom(functor, options));
	  } else
	  { if ( functor == ATOM_curl &&
		 (PL_is_atom(arg) || PL_is_variable(arg)) )
	      TRY(Putc(' ', out));
	    _PL_get_arg(1, t, arg);

	    TRY(writeTerm(arg, 1200, options, W_BLOCK_OP));
	  }
	  if (op_pri > prec)
	    TRY(PutCloseBrace(out));

	  succeed;
	}
      } else if ( arity == 2 ||
		 (arity == 3 && isBlockOp(t, arg, functor)) )
      {					  /* <term> op <term> */
	if ( currentOperator(options->module, functor, OP_INFIX,
			     &op_type, &op_pri) )
	{ static atom_t ATOM_fdot = 0;

	  if ( !ATOM_fdot )			/* ATOM_dot can be '[|]' */
	    ATOM_fdot = PL_new_atom(".");

	  if ( op_pri > prec )
	    TRY(PutOpenBrace(out));
	  _PL_get_arg(arity-1, t, arg);
	  TRY(writeTerm(arg,
			op_type == OP_XFX || op_type == OP_XFY
				? op_pri-1 : op_pri,
			options, W_INFIX_ARG1));
	  if ( arity == 2 )
	  { if ( functor == ATOM_comma )
	    { TRY(PutComma(options));
	    } else if ( functor == ATOM_bar )
	    { TRY(PutBar(options));
	    } else if ( functor == ATOM_fdot )
	    { TRY(PutToken(".", out));
	    } else if ( functor == ATOM_divide &&
			ison(options, PL_WRT_RAT_NATURAL) &&
			PL_is_integer(arg) &&
			_PL_get_arg(1, t, arg) &&
			PL_is_number(arg) )
	    { TRY(PutString(" / ", out));
	    } else
	    { switch(writeAtom(functor, options))
	      { case false:
		  fail;
		case TRUE_WITH_SPACE:
		  TRY(Putc(' ', out));
	      }
	    }
	    options->out->lastc |= C_INFIX_OP;
	  } else			/* block operator */
	  { _PL_get_arg(1, t, arg);
	    TRY(writeTerm(arg, 1200, options, W_BLOCK_OP));
	  }
	  _PL_get_arg(arity, t, arg);
	  TRY(writeTerm(arg,
			op_type == OP_XFX || op_type == OP_YFX
				? op_pri-1 : op_pri,
			options, W_INFIX_ARG2));
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
	  TRY(PutComma(options));
	_PL_get_arg(n+1, t, a);
	TRY(writeTerm(a, 999, options, W_COMPOUND_ARG));
      }
      return Putc(')', out);
    }
  }
}


		 /*******************************
		 *	  CYCLE HANDLING	*
		 *******************************/

static int
reunify_acyclic_substitutions(term_t substitutions, term_t cycles,
			      write_options *options)
{ GET_LD
  term_t s_tail, c_tail, s_head, c_head, var, value;
  intptr_t count = 0;

  if ( !(s_tail = PL_copy_term_ref(substitutions)) ||
       !(c_tail = PL_copy_term_ref(cycles)) ||
       !(s_head = PL_new_term_ref()) ||
       !(c_head = PL_new_term_ref()) ||
       !(var    = PL_new_term_ref()) ||
       !(value  = PL_new_term_ref()) )
    return false;

  while(PL_get_list(s_tail, s_head, s_tail))
  { _PL_get_arg(1, s_head, var);
    _PL_get_arg(2, s_head, value);
    if ( PL_var_occurs_in(var, value) )
    { if ( (options->flags&PL_WRT_NUMBERVARS) )
      { if ( !PL_unify_term(var,
			    PL_FUNCTOR, FUNCTOR_isovar1,
			      PL_INTPTR, --count) )
	  return false;
      }

      if ( !PL_unify_list(c_tail, c_head, c_tail) ||
	   !PL_unify(c_head, s_head) )
	return false;
    } else
    { if ( !PL_unify(var, value) )
	return false;
    }
  }

  return PL_unify_nil(c_tail);
}


static bool
writeTopTerm(term_t term, int prec, write_options *options)
{ GET_LD
  bool rc;
  int wflags;

  if ( ison(options, PL_WRT_PARTIAL) && prec != 999 && prec != 1200 )
    wflags = W_OP_ARG;
  else
    wflags = W_TOP;

  Slock(options->out);
  if ( (!(options->flags&PL_WRT_NO_CYCLES) && options->max_depth) ||
       PL_is_acyclic(term) )
  { C_STACK_OVERFLOW_GUARDED(
	rc,
	writeTerm(term, prec, options, wflags),
        (void)0);
  } else
  { fid_t fid;
    term_t template, substitutions, cycles, at_term;

    if ( options->flags & PL_WRT_NO_CYCLES )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_cyclic_term, term);

    if ( !(fid = PL_open_foreign_frame()) ||
	 !(template = PL_new_term_ref()) ||
	 !(substitutions = PL_new_term_ref()) ||
	 !(cycles = PL_new_term_ref()) ||
	 !(at_term = PL_new_term_ref()) ||
	 !PL_factorize_term(term, template, substitutions) ||
	 !reunify_acyclic_substitutions(substitutions, cycles, options) ||
	 !PL_unify_term(at_term,
			PL_FUNCTOR, FUNCTOR_at_sign2,
			  PL_TERM, template,
			  PL_TERM, cycles) )
      return false;
    C_STACK_OVERFLOW_GUARDED(
	rc,
	writeTerm(at_term, prec, options, wflags),
	(void)0);
    PL_discard_foreign_frame(fid);
  }
  Sunlock(options->out);

  return rc;
}


#define bind_varnames(names) LDFUNC(bind_varnames, names)
static bool
bind_varnames(DECL_LD term_t names)
{ term_t tail, head, var, namet;
  int check_cycle_after = 1000;

  if ( !(tail = PL_copy_term_ref(names)) ||
       !(head = PL_new_term_ref()) ||
       !(var  = PL_new_term_ref()) ||
       !(namet = PL_new_term_ref()) )
    return false;

  while(PL_get_list_ex(tail, head, tail))
  { if ( PL_is_functor(head, FUNCTOR_equals2) )
    { atom_t name;

      _PL_get_arg(2, head, var);
      _PL_get_arg(1, head, namet);

      if ( !PL_get_atom_ex(namet, &name) )
	return false;
      if ( !atomIsVarName(name) )
	return PL_domain_error("variable_name", namet);

      if ( PL_is_variable(var) )
      { if ( !PL_unify_term(var,
			    PL_FUNCTOR, FUNCTOR_isovar1,
			      PL_ATOM, name) )
	  return false;
      }
    } else
      return PL_type_error("variable_assignment", head);

    if ( --check_cycle_after == 0 &&
	 lengthList(tail, false) == -1 )
      return PL_type_error("list", head);
  }

  return PL_get_nil_ex(tail);
}


		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/

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


static int
writeBlobMask(atom_t a)
{ if ( a == ATOM_default )
  { return 0;
  } else if ( a == ATOM_portray )
  { return PL_WRT_BLOB_PORTRAY;
  } else
    return -1;
}


static const PL_option_t write_term_options[] =
{ { ATOM_quoted,		    OPT_BOOL },
  { ATOM_quote_non_ascii,	    OPT_BOOL },
  { ATOM_ignore_ops,		    OPT_BOOL },
  { ATOM_dotlists,		    OPT_BOOL },
  { ATOM_brace_terms,		    OPT_BOOL },
  { ATOM_numbervars,		    OPT_BOOL },
  { ATOM_portray,		    OPT_BOOL },
  { ATOM_portrayed,		    OPT_BOOL },
  { ATOM_portray_goal,		    OPT_TERM },
  { ATOM_character_escapes,	    OPT_BOOL },
  { ATOM_character_escapes_unicode, OPT_BOOL },
  { ATOM_max_depth,		    OPT_INT  },
  { ATOM_module,		    OPT_ATOM },
  { ATOM_back_quotes,		    OPT_ATOM },
  { ATOM_attributes,		    OPT_ATOM },
  { ATOM_priority,		    OPT_INT },
  { ATOM_partial,		    OPT_BOOL },
  { ATOM_spacing,		    OPT_ATOM },
  { ATOM_blobs,			    OPT_ATOM },
  { ATOM_cycles,		    OPT_BOOL },
  { ATOM_variable_names,	    OPT_TERM },
  { ATOM_nl,			    OPT_BOOL },
  { ATOM_fullstop,		    OPT_BOOL },
  { ATOM_no_lists,		    OPT_BOOL },
  { ATOM_integer_format,	    OPT_ATOM },
  { ATOM_float_format,		    OPT_ATOM },
  { NULL_ATOM,			    0 }
};

foreign_t
pl_write_term3(term_t stream, term_t term, term_t opts)
{ GET_LD
  int quoted      = false;
  int ignore_ops  = false;
  int dotlists    = false;
  int braceterms  = true;
  int numbervars  = -1;			/* not set */
  int portray     = false;
  term_t gportray = 0;
  atom_t bq       = 0;
  int  charescape = -1;			/* not set */
  int charescape_unicode = -1;
  int quote_non_ascii = false;
  atom_t mname    = ATOM_user;
  atom_t attr     = ATOM_nil;
  atom_t blobs    = ATOM_nil;
  int  priority   = 1200;
  int partial     = false;
  int cycles      = true;
  int nl          = false;
  int fullstop    = false;
  int no_lists    = false;
  term_t varnames = 0;
  IOSTREAM *s = NULL;
  write_options options = WRITE_OPTIONS_DEFAULTS;
  int rc;

  if ( !PL_scan_options(opts, 0, "write_option", write_term_options,
			&quoted, &quote_non_ascii, &ignore_ops, &dotlists, &braceterms,
			&numbervars, &portray, &portray, &gportray,
			&charescape, &charescape_unicode,
			&options.max_depth, &mname,
			&bq, &attr, &priority, &partial, &options.spacing,
			&blobs, &cycles, &varnames, &nl, &fullstop,
			&no_lists,
			&options.integer_format,
			&options.float_format) )
    return false;

  if ( attr == ATOM_nil )
  { options.flags |= LD->prolog_flag.write_attributes;
  } else
  { int mask = writeAttributeMask(attr);

    if ( !mask )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_write_option, opts);

    options.flags |= mask;
  }
  if ( blobs != ATOM_nil )
  { int mask = writeBlobMask(blobs);

    if ( mask < 0 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_write_option, opts);

    options.flags |= mask;
  }
  if ( priority < 0 || priority > OP_MAXPRIORITY )
  { term_t t = PL_new_term_ref();
    PL_put_integer(t, priority);

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_operator_priority, t);
  }
  switch( options.spacing )
  { case ATOM_standard:
    case ATOM_next_argument:
      break;
    default:
    { term_t t = PL_new_term_ref();
      PL_put_atom(t, options.spacing);

      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_spacing, t);
    }
  }

  options.module = isCurrentModule(mname);
  if ( !options.module )
    options.module = MODULE_user;
  if ( charescape == true ||
       (charescape == -1 && ison(options.module, M_CHARESCAPE)) )
    options.flags |= PL_WRT_CHARESCAPES;
  if ( charescape_unicode == true ||
       (charescape_unicode == -1 && truePrologFlag(PLFLAG_CHARESCAPE_UNICODE)) )
    options.flags |= PL_WRT_CHARESCAPES_UNICODE;
  if ( ison(options.module, RAT_NATURAL) )
    options.flags |= PL_WRT_RAT_NATURAL;
  if ( gportray )
  { options.portray_goal = gportray;
    if ( !put_write_options(opts, &options) ||
	 !PL_qualify(options.portray_goal, options.portray_goal) )
      return false;
    if ( isoff(&options, PL_WRT_BLOB_PORTRAY) )
      portray = true;
  }
  if ( numbervars == -1 )
    numbervars = (portray ? true : false);

  if ( quoted )          options.flags |= PL_WRT_QUOTED;
  if ( quote_non_ascii ) options.flags |= PL_WRT_QUOTE_NON_ASCII;
  if ( ignore_ops )      options.flags |= PL_WRT_IGNOREOPS;
  if ( dotlists )        options.flags |= PL_WRT_DOTLISTS;
  if ( !braceterms )     options.flags |= PL_WRT_BRACETERMS;
  if ( numbervars )      options.flags |= PL_WRT_NUMBERVARS;
  if ( portray )         options.flags |= PL_WRT_PORTRAY;
  if ( !cycles )         options.flags |= PL_WRT_NO_CYCLES;
  if ( no_lists )        options.flags |= PL_WRT_NO_LISTS;
  if ( partial )	 options.flags |= PL_WRT_PARTIAL;
  if ( bq )
  { unsigned int flags = 0;

    if ( !setBackQuotes(bq, &flags) )
      return false;
    if ( (flags&BQ_STRING) )
      options.flags |= PL_WRT_BACKQUOTED_STRING;
    else if ( flags == 0 )
      options.flags |= PL_WRT_BACKQUOTE_IS_SYMBOL;
  }

  BEGIN_NUMBERVARS(varnames);
  if ( varnames )
  { if ( (rc=bind_varnames(varnames)) )
      options.flags |= PL_WRT_VARNAMES;
    else
      goto out;
  }
  if ( !(rc=getTextOutputStream(stream, &s)) )
    goto out;

  options.out = s;
  if ( !partial )
    PutOpenToken(EOF, s);		/* reset this */
  if ( (options.flags & PL_WRT_QUOTED) && !(s->flags&(SIO_REPPL|SIO_REPPLU)) )
  { unsigned int flag = truePrologFlag(PLFLAG_CHARESCAPE_UNICODE) ? SIO_REPPLU
								  : SIO_REPPL;
    s->flags |= flag;
    rc = writeTopTerm(term, priority, &options);
    s->flags &= ~flag;
  } else
  { rc = writeTopTerm(term, priority, &options);
  }

  if ( rc && fullstop )
    rc = PutToken(".", s) && Putc(nl ? '\n' : ' ', s);
  else if ( nl )
    rc = Putc('\n', s);

out:
  END_NUMBERVARS(varnames);

  return (!s || streamStatus(s)) && rc;
}


foreign_t
pl_write_term(term_t term, term_t options)
{ return pl_write_term3(0, term, options);
}


bool
PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags)
{ write_options options = WRITE_OPTIONS_DEFAULTS;
  int rc;

  options.flags	    = flags & ~PL_WRT_NEWLINE;
  options.out	    = s;
  options.module    = MODULE_user;

  if ( !(flags & (PL_WRT_CHARESCAPES|PL_WRT_NO_CHARESCAPES) ) )
  { if ( ison(options.module, M_CHARESCAPE) )
      options.flags |= PL_WRT_CHARESCAPES;
  }

  if ( (s=PL_acquire_stream(s)) )
  { PutOpenToken(EOF, s);			/* reset this */
    rc = writeTopTerm(term, precedence, &options);
    if ( rc && (flags&PL_WRT_NEWLINE) )
      rc = Putc('\n', s);
    rc = PL_release_stream(s) && rc;
  } else
    rc = false;

  return rc;
}


static foreign_t
do_write2(term_t stream, term_t term, int flags, int canonical)
{ GET_LD
  IOSTREAM *s;

  if ( getTextOutputStream(stream, &s) )
  { write_options options = WRITE_OPTIONS_DEFAULTS;
    int rc;

    options.flags     = flags;
    if ( !canonical )
      options.flags |= LD->prolog_flag.write_attributes;
    options.out	      = s;
    options.module    = MODULE_user;
    if ( ison(options.module, M_CHARESCAPE) )
      options.flags |= PL_WRT_CHARESCAPES;
    if ( ison(options.module, BQ_STRING) )
      options.flags |= PL_WRT_BACKQUOTED_STRING;
    if ( ison(options.module, RAT_NATURAL) )
      options.flags |= PL_WRT_RAT_NATURAL;

    PutOpenToken(EOF, s);		/* reset this */
    rc = writeTopTerm(term, 1200, &options);
    if ( rc && (flags&PL_WRT_NEWLINE) )
      rc = Putc('\n', s);

    return streamStatus(s) && rc;
  }

  return false;
}


foreign_t
pl_write2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_NUMBERVARS, false);
}

foreign_t
pl_writeln2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_NUMBERVARS|PL_WRT_NEWLINE, false);
}

foreign_t
pl_writeq2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_QUOTED|PL_WRT_NUMBERVARS, false);
}

foreign_t
pl_print2(term_t stream, term_t term)
{ GET_LD
  fid_t fid = PL_open_foreign_frame();
  term_t opts = PL_new_term_ref();
  foreign_t rc;

  if ( PL_current_prolog_flag(ATOM_print_write_options, PL_TERM, &opts) )
    rc = pl_write_term3(stream, term, opts);
  else
    rc = do_write2(stream, term,
		   PL_WRT_PORTRAY|PL_WRT_NUMBERVARS|PL_WRT_QUOTED, false);

  PL_discard_foreign_frame(fid);

  return rc;
}

foreign_t
pl_write_canonical2(term_t stream, term_t term)
{ GET_LD
  nv_options options;
  foreign_t rc;

  BEGIN_NUMBERVARS(true);

  options.functor = FUNCTOR_isovar1;
  options.on_attvar = AV_SKIP;
  options.singletons = PL_is_acyclic(term);
  options.numbered_check = false;

  rc = ( numberVars(term, &options, 0) != NV_ERROR &&
	 do_write2(stream, term,
		   PL_WRT_QUOTED|PL_WRT_QUOTE_NON_ASCII|
		   PL_WRT_IGNOREOPS|PL_WRT_VARNAMES|
		   PL_WRT_NODOTINATOM|PL_WRT_BRACETERMS, true)
       );

  END_NUMBERVARS(true);

  return rc;
}

foreign_t
pl_write(term_t term)
{ return pl_write2(0, term);
}

foreign_t
pl_writeq(term_t term)
{ return pl_writeq2(0, term);
}

foreign_t
pl_print(term_t term)
{ return pl_print2(0, term);
}

foreign_t
pl_write_canonical(term_t term)
{ return pl_write_canonical2(0, term);
}

foreign_t
pl_writeln(term_t term)
{ return do_write2(0, term, PL_WRT_NUMBERVARS|PL_WRT_NEWLINE, false);
}


static
PRED_IMPL("$put_token", 2, put_token, 0)
{ char *s;
  size_t len;
  IOSTREAM *out;

  if ( !PL_get_stream_handle(A1, &out) )
    fail;
  if ( !PL_get_nchars(A2, &len, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
    fail;

  if ( PutTokenN(s, len, out) )
    return PL_release_stream(out);

  PL_release_stream(out);
  fail;
}

/** '$put_quoted'(+Stream, +Quote, +Codes, +Options)

Emit Codes using the escaped character  syntax,   but  does not emit the
start and end-code itself.
*/

static const PL_option_t put_quoted_options[] =
{ { ATOM_character_escapes_unicode, OPT_BOOL },
  { NULL_ATOM,			    0 }
};


static
PRED_IMPL("$put_quoted", 4, put_quoted_codes, 0)
{ PRED_LD
  IOSTREAM *out;
  pl_wchar_t *w;
  size_t i, len;
  int quote;
  int flags = PL_WRT_CHARESCAPES;
  int rc = true;
  int charescape_unicode = -1;

  if ( !PL_scan_options(A4, 0, "write_option", put_quoted_options,
			&charescape_unicode) )
    return false;

  if ( charescape_unicode == true ||
       (charescape_unicode == -1 && truePrologFlag(PLFLAG_CHARESCAPE_UNICODE)) )
    flags |= PL_WRT_CHARESCAPES_UNICODE;

  if ( !PL_get_stream_handle(A1, &out) ||
       !PL_get_char_ex(A2, &quote, false) ||
       !PL_get_wchars(A3, &len, &w, CVT_LIST|CVT_STRING|CVT_EXCEPTION) )
    return false;

  for(i=0; rc && i<len; i++)
    rc = putQuoted(w[i], quote, flags, out);

  if ( rc )
    rc = PL_release_stream(out);

  return rc;
}


		 /*******************************
		 *	   PRINT LENGTH		*
		 *******************************/

typedef struct limit_size_stream
{ IOSTREAM	*stream;		/* Limited stream */
  int64_t	 length;		/* Max size */
} limit_size_stream;

static ssize_t
Swrite_lss(void *handle, char *buf, size_t size)
{ limit_size_stream *lss = handle;
  (void)buf;

  if ( lss->stream->position->charno > lss->length )
  { errno = EINVAL;
    return -1;
  }

  return size;
}

static int
Sclose_lss(void *handle)
{ (void)handle;

  return 0;
}

static const IOFUNCTIONS lss_functions =
{ NULL,
  Swrite_lss,
  NULL,
  Sclose_lss
};

/** write_length(+Term, -Len, +Options) is det.

(*) Avoid error on max_length in iso mode.  It might be nicer to get the
option processing out of pl_write_term3(), so we can take control of the
whole lot here more easily.
*/

static
PRED_IMPL("write_length", 3, write_length, 0)
{ PRED_LD
  limit_size_stream lss;
  int sflags = SIO_NBUF|SIO_RECORDPOS|SIO_OUTPUT|SIO_TEXT;
  IOSTREAM *s;
  term_t options = PL_copy_term_ref(A3);
  term_t head = PL_new_term_ref();
  char buf[100];

  lss.length = PLMAXINT;
  while(PL_get_list(options, head, options))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(head, &name, &arity) &&
	 name == ATOM_max_length && arity == 1 )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, head, a);
      if ( !PL_get_int64_ex(a, &lss.length) )
	return false;
    }
  }

  if ( (s = Snew(&lss, sflags, (IOFUNCTIONS *)&lss_functions)) )
  { int64_t len;
    int rc;
    pl_features_t oldmask = LD->prolog_flag.mask; /* (*) */

    lss.stream = s;
    s->encoding = ENC_UTF8;
    Ssetbuffer(s, buf, sizeof(buf));
    s->flags |= SIO_USERBUF;

    clearPrologFlagMask(PLFLAG_ISO);
    pushOutputContext(s);
    rc = pl_write_term3(0, A1, A3);
    popOutputContext();
    LD->prolog_flag.mask = oldmask;

    if ( rc && s->position->charno <= lss.length )
    { len = s->position->charno;
    } else
    { len = -1;
      if ( s->position->charno > lss.length )
	PL_clear_exception();
    }

    Sclose(s);
    if ( len >= 0 )
      return PL_unify_int64(A2, len);
  }

  return false;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(write)
  PRED_DEF("nl", 0, nl, PL_FA_ISO)
  PRED_DEF("nl", 1, nl, PL_FA_ISO)
  PRED_DEF("$put_token", 2, put_token, 0)
  PRED_DEF("$put_quoted", 4, put_quoted_codes, 0)
  PRED_DEF("write_length", 3, write_length, 0)
EndPredDefs
