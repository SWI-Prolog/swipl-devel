/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2016, University of Amsterdam
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

#include "pl-incl.h"
#include "pl-dict.h"
#include <math.h>
#include "os/pl-dtoa.h"
#include "os/pl-ctype.h"
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

typedef struct
{ int   flags;				/* PL_WRT_* flags */
  int   max_depth;			/* depth limit */
  int   depth;				/* current depth */
  atom_t spacing;			/* Where to insert spaces */
  Module module;			/* Module for operators */
  IOSTREAM *out;			/* stream to write to */
  term_t portray_goal;			/* call/2 activated portray hook */
  term_t write_options;			/* original write options */
  term_t prec_opt;			/* term in write options with prec */
} write_options;

static bool	writeTerm2(term_t term, int prec,
			   write_options *options, bool arg) WUNUSED;
static bool	writeTerm(term_t t, int prec,
			  write_options *options) WUNUSED;
static bool	writeArgTerm(term_t t, int prec,
			     write_options *options, bool arg) WUNUSED;
static int	PutToken(const char *s, IOSTREAM *stream);
static int	writeAtom(atom_t a, write_options *options);
static int	callPortray(term_t arg, int prec, write_options *options);
static int	enterPortray(ARG1_LD);
static void	leavePortray(ARG1_LD);

char *
var_name_ptr__LD(Word p, char *name ARG_LD)
{ size_t iref;

  deRef(p);

  if (p > (Word) lBase)
    iref = ((Word)p - (Word)lBase)*2+1;
  else
    iref = ((Word)p - (Word)gBase)*2;

  Ssprintf(name, "_%lld", (int64_t)iref);

  return name;
}


char *
varName(term_t t, char *name)
{ GET_LD
  Word p = valTermRef(t);

  return var_name_ptr(p, name);
}


static int
atomIsVarName(atom_t a)
{ Atom atom = atomValue(a);

  if ( false(atom->type, PL_BLOB_TEXT) || atom->length == 0 )
    fail;
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
	  return FALSE;
      }

      return TRUE;
    }

    return FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return:	TRUE:  processes
	FALSE: not processed
	-1:    error
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
writeNumberVar(term_t t, write_options *options ARG_LD)
{ Word p = valTermRef(t);
  Functor f;

  deRef(p);
  if ( !isTerm(*p) )
    return FALSE;
  f = valueTerm(*p);

  if ( f->definition != FUNCTOR_isovar1 )
    return FALSE;

  if ( LD->var_names.numbervars_frame )
  { FliFrame fr = (FliFrame)valTermRef(LD->var_names.numbervars_frame);

    assert(fr->magic == FLI_MAGIC);
    if ( fr->mark.globaltop > (Word)f )
      return FALSE;			/* older $VAR term */
  }

  p = &f->arguments[0];
  deRef(p);
  if ( isInteger(*p) )
  { int64_t n = valInteger(*p);
    char buf[32];			/* Max is H354745078340568300 */

    if ( n < 0 )
    { sprintf(buf, "S_%" PRId64, -n);
    } else
    { int i = (int)(n % 26);
      int64_t j = n / 26;

      if ( j == 0 )
      { buf[0] = i+'A';
	buf[1] = EOS;
      } else
      { sprintf(buf, "%c%" PRId64, i+'A', j);
      }
    }

    return PutToken(buf, options->out) ? TRUE : -1;
  }

  if ( isAtom(*p) && atomIsVarName(*p) )
  { write_options o2 = *options;
    clear(&o2, PL_WRT_QUOTED);

    return writeAtom(*p, &o2) ? TRUE : -1;
  }

  return FALSE;
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
atomType(atom_t a, write_options *options)
{ Atom atom = atomValue(a);
  char *s = atom->name;
  size_t len = atom->length;
  IOSTREAM *fd = options ? options->out : NULL;
  Module m = options ? options->module : MODULE_user;

  if ( len == 0 )
    return AT_QUOTE;

  if ( isLower(*s) || (true(m, M_VARPREFIX) && isAlpha(*s)) )
  { do
    { for( ++s;
	   --len > 0 && isAlpha(*s) && (!fd || Scanrepresent(*s, fd)==0);
	   s++)
	;
    } while ( len >= 2 &&
	      *s == '.' && isAlpha(s[1]) &&
	      truePrologFlagNoLD(PLFLAG_DOT_IN_ATOM) &&
	      (!options || false(options, PL_WRT_NODOTINATOM))
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
	 (!fd || Scanrepresent(*s, fd)==0);
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
  pl_wchar_t *s = (pl_wchar_t*)ap->name;
  size_t len = ap->length/sizeof(pl_wchar_t);

  if ( len == 0 )
    return FALSE;

  if ( !f_is_prolog_atom_start(*s) )
  { for( ; len > 0; s++, len--)
    { if ( !f_is_prolog_symbol(*s) ||
	   (fd && Scanrepresent(*s, fd)<0) )
	return FALSE;
    }
    return TRUE;
  }

  do
  { for( ++s;
	 ( --len > 0 &&
	   f_is_prolog_identifier_continue(*s) &&
	   (!fd || Scanrepresent(*s, fd)==0)
	 );
	 s++)
      ;
  } while ( len >= 2 &&
	    *s == '.' && f_is_prolog_identifier_continue(s[1]) &&
	    truePrologFlagNoLD(PLFLAG_DOT_IN_ATOM) &&
	    !(flags&PL_WRT_NODOTINATOM)
	  );

  return len == 0;
}


int
unquoted_atom(atom_t a)
{ Atom ap = atomValue(a);

  if ( true(ap->type, PL_BLOB_TEXT) )
  { if ( !ap->type->write )		/* ordinary atoms */
    { return atomType(a, NULL) != AT_QUOTE;
    } else if ( isUCSAtom(ap) )		/* wide atoms */
    { return unquoted_atomW(a, NULL, 0);
    }
  }

  return FALSE;
}


		 /*******************************
		 *	 PRIMITIVE WRITES	*
		 *******************************/

#define TRUE_WITH_SPACE 2		/* OK, and emitted leading space before token */

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
      return FALSE;
  }

  return TRUE;
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
    return FALSE;
  }
  if ( s->lastc == EOF )
    return FALSE;

  if ( (s->lastc&C_PREFIX_SIGN) && (isDigit(c) || isSymbolW(c)) )
    return TRUE;
  if ( (s->lastc&C_PREFIX_OP) && ( c == '(' || c == '{' ) )
    return TRUE;				/* avoid op(...) */
  if ( (s->lastc&C_INFIX_OP) && c == '(' )
    return FALSE;

  s->lastc &= ~C_MASK;

  if ( ((isAlphaW(s->lastc) && isAlphaW(c)) ||
	(isSymbolW(s->lastc) && isSymbolW(c)) ||
	(c == '(' && !isPunctW(s->lastc)) ||
	(c == '\'' && (isDigit(s->lastc))) ||
	(isquote(c) && s->lastc == c)
       ) )
    return TRUE;

  return FALSE;
}


static int
PutOpenToken(int c, IOSTREAM *s)
{ if ( needSpace(c, s) )
  { TRY(Putc(' ', s));
    return TRUE_WITH_SPACE;
  }

  return TRUE;
}


static int
PutToken(const char *s, IOSTREAM *stream)
{ if ( s[0] )
  { int rc;

    TRY(rc=PutOpenToken(s[0]&0xff, stream));
    TRY(PutString(s, stream));

    return rc;
  }

  return TRUE;
}


static int
PutTokenN(const char *s, size_t len, IOSTREAM *stream)
{ if ( len > 0 )
  { int rc;

    TRY(rc=PutOpenToken(s[0]&0xff, stream));
    TRY(PutStringN(s, len, stream));

    return rc;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PutOpenBrace()/PutCloseBrace() are used to put  additional braces around
a term to avoid an operator  precedence   problem.  If  the last emitted
character  is  alphanumerical,  there  should  be  a  space  before  the
openbrace to avoid interpretation as a term.   E.g. not (a,b) instead of
not(a,b).  Reported by Stefan.Mueller@dfki.de.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
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
  { if ( c == ' ' ||
	 (!(c < 0xff && !isGraph(c)) && c != quote && c != '\\') )
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
  { if ( !Putc(c, stream) )
      fail;
    if ( c == quote || c == '\\' )	/* write '' or \\ */
    { if ( !Putc(c, stream) )
	fail;
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
{ GET_LD
  char buf[32];

  TRY(PutToken(varName(av, buf), options->out));

  if ( (options->flags & PL_WRT_ATTVAR_DOTS) )
  { return PutString("{...}", options->out);
  } else if ( (options->flags & PL_WRT_ATTVAR_WRITE) )
  { fid_t fid;
    term_t a;

    if ( !(fid = PL_open_foreign_frame()) )
      return FALSE;

    Sputcode('{', options->out);
    a = PL_new_term_ref();
    PL_get_attr__LD(av, a PASS_LD);
    if ( !writeTerm(a, 1200, options) )
      return FALSE;
    Sputcode('}', options->out);
    PL_close_foreign_frame(fid);

    return TRUE;
  } else if ( (options->flags & PL_WRT_ATTVAR_PORTRAY) &&
	      GD->cleaning <= CLN_PROLOG )
  { static predicate_t pred;
    IOSTREAM *old;
    wakeup_state wstate;
    int rc;

    if ( !pred )
      pred = _PL_predicate("portray_attvar", 1, "$attvar",
			   &GD->procedures.portray_attvar1);

    if ( !enterPortray(PASS_LD1) )
      return FALSE;
    if ( !saveWakeup(&wstate, TRUE PASS_LD) )
      return FALSE;
    old = Scurout;
    Scurout = options->out;
    rc = PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION, pred, av);
    if ( rc != TRUE && !PL_exception(0) )
      rc = TRUE;
    Scurout = old;
    restoreWakeup(&wstate PASS_LD);
    leavePortray(PASS_LD1);

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


static int				/* FALSE, TRUE or TRUE_WITH_SPACE */
writeAtom(atom_t a, write_options *options)
{ Atom atom = atomValue(a);

  if ( (options->flags & PL_WRT_BLOB_PORTRAY) &&
       false(atom->type, PL_BLOB_TEXT) &&
       GD->cleaning <= CLN_PROLOG &&
       a != ATOM_nil )
  { GET_LD
    int rc;
    fid_t fid;
    term_t av;

    if ( !(fid = PL_open_foreign_frame()) )
      return FALSE;
    av = PL_new_term_ref();
    PL_put_atom(av, a);
    rc = callPortray(av, 1200, options);
    PL_close_foreign_frame(fid);
    switch(rc)
    { case TRUE:
	return TRUE;
      case FALSE:
	break;
      default:
	return FALSE;				/* error */
    }
  }

  if ( atom->type->write )
    return (*atom->type->write)(options->out, a, options->flags);
  if ( false(atom->type, PL_BLOB_TEXT) )
    return writeBlob(a, options);

  if ( true(options, PL_WRT_QUOTED) )
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

  if ( (flags&PL_WRT_QUOTED) && !unquoted_atomW(atom, fd, flags) )
  { pl_wchar_t quote = L'\'';

    TRY(PutOpenToken(quote, fd) &&
	Putc(quote, fd));

    while(s < e)
    { TRY(putQuoted(*s++, quote, flags, fd));
    }

    return Putc(quote, fd);
  }

  if ( s < e && !PutOpenToken(s[0], fd) )
    return FALSE;
  for( ; s<e; s++)
  { if ( !Putc(*s, fd) )
      return FALSE;
  }

  return TRUE;
}

#ifdef O_RESERVED_SYMBOLS
int
writeReservedSymbol(IOSTREAM *fd, atom_t atom, int flags)
{ Atom a = atomValue(atom);
  const char *s = a->name;
  size_t len = a->length;
  const char *e = &s[len];

  if ( atom == ATOM_nil )
    return PutToken("[]", fd);

  if ( (flags&PL_WRT_QUOTED) )
  { char quote = '\'';

    if ( PutOpenToken('C', fd) &&
	 Putc('C', fd) &&
	 Putc(quote, fd) )
    { while(s < e)
      { if ( !putQuoted(*s++, quote, flags, fd) )
	  return FALSE;
      }

      return Putc(quote, fd);
    }
  }

  if ( s < e && !PutOpenToken(s[0], fd) )
    return FALSE;
  for( ; s<e; s++)
  { if ( !Putc(*s, fd) )
      return FALSE;
  }

  return TRUE;
}
#endif


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
{ GET_LD
  PL_chars_t txt;

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


static char *
writeNaN(double f, char *buf)
{ format_float(NaN_value(f), buf);
  strcat(buf, "NaN");
  return buf;
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


static char *
writeINF(double f, char *buf)
{ number n;

  n.value.f = f;
  n.type = V_FLOAT;

  if ( ar_signbit(&n) < 0 )
    return strcpy(buf, "-1.0Inf");
  else
    return strcpy(buf, "1.0Inf");
}


static char *
format_special_float(double f, char *buf)
{
#ifdef HAVE_FPCLASSIFY
  switch(fpclassify(f))
  { case FP_NAN:
      return writeNaN(f, buf);
    case FP_INFINITE:
      return writeINF(f, buf);
  }
#else
#ifdef HAVE_FPCLASS
  switch(fpclass(f))
  { case FP_SNAN:
    case FP_QNAN:
      return writeNaN(f, buf);
    case FP_NINF:
    case FP_PINF:
      return writeINF(n, buf);
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
      return writeNaN(f, buf);
    case _FPCLASS_NINF:
    case _FPCLASS_PINF:
      return writeINF(f, buf);
  }
#else
#ifdef HAVE_ISINF
  if ( isinf(f) )
  { return writeINF(f, buf);
  } else
#endif
#ifdef HAVE_ISNAN
  if ( isnan(f) )
  { return writeNaN(f, buf);
  }
#endif
#endif /*HAVE__FPCLASS*/
#endif /*HAVE_FPCLASS*/
#endif /*HAVE_FPCLASSIFY*/

  return NULL;
}


char *
format_float(double f, char *buf)
{ char *end, *o=buf, *s;
  int decpt, sign;

  if ( (s=format_special_float(f, buf)) )
    return s;

  s = dtoa(f, 0, 30, &decpt, &sign, &end);
  DEBUG(2, Sdprintf("decpt=%d, sign=%d, len = %d, '%s'\n",
		    decpt, sign, end-s, s));

  if ( sign )
    *o++ = '-';

  if ( decpt <= 0 )			/* decimal dot before */
  { if ( decpt <= -4 )
    { *o++ = s[0];
      *o++ = '.';
      if ( end-s > 1 )
      { memcpy(o, s+1, end-s-1);
	o += end-s-1;
      } else
	*o++ = '0';
      sprintf(o, "e%d", decpt-1);
    } else
    { int i;

      *o++ = '0';
      *o++ = '.';
      for(i=0; i < -decpt; i++)
	*o++ = '0';
      memcpy(o, s, end-s);
      o[end-s] = 0;
    }
  } else if ( end-s > decpt )		/* decimal dot inside */
  { memcpy(o, s, decpt);
    o += decpt;
    *o++ = '.';
    memcpy(o, s+decpt, end-s-decpt);
    o[end-s-decpt] = 0;
  } else				/* decimal dot after */
  { int i;
    int trailing = decpt-(int)(end-s);

    if ( decpt > 15 )			/* over precision: use eE */
    { *o++ = s[0];
      *o++ = '.';
      if ( end-s > 1 )
      { trailing += (int)(end-s)-1;
	memcpy(o, s+1, end-s-1);
	o += end-s-1;
      } else
	*o++ = '0';
      sprintf(o, "e+%d", trailing);
    } else				/* within precision trail with .0 */
    { memcpy(o, s, end-s);
      o += end-s;

      for(i=(int)(end-s); i<decpt; i++)
	*o++ = '0';
      *o++ = '.';
      *o++ = '0';
      *o = 0;
    }
  }

  freedtoa(s);

  return buf;
}


static int
WriteNumber(Number n, write_options *options)
{ switch(n->type)
  { case V_INTEGER:
    { char buf[32];

      sprintf(buf, "%" PRId64, n->value.i);
      return PutToken(buf, options->out);
    }
#ifdef O_GMP
    case V_MPZ:
    { GET_LD
      char tmp[1024];
      char *buf;
      size_t sz = mpz_sizeinbase(n->value.mpz, 10) + 2;
      bool rc;

      if ( sz <= sizeof(tmp) )
	buf = tmp;
      else
	buf = PL_malloc(sz);

      /* mpz_get_str() can perform large intermediate allocations :-( */
      EXCEPTION_GUARDED({ LD->gmp.persistent++;
			  mpz_get_str(buf, 10, n->value.mpz);
			  LD->gmp.persistent--;
			},
			{ LD->gmp.persistent--;
			  rc = PL_rethrow();
			})
      rc = PutToken(buf, options->out);
      if ( buf != tmp )
	PL_free(buf);

      return rc;
    }
#endif
    case V_FLOAT:
    { char buf[100];

      format_float(n->value.f, buf);
      return PutToken(buf, options->out);
    }
    default:
      assert(0);
      return FALSE;			/* make compiler happy */
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
  { number n;

    PL_get_number(t, &n);
    return WriteNumber(&n, options);
  }

#if O_STRING
  if ( PL_is_string(t) )
    return writeString(t, options);
#endif /* O_STRING */

  assert(0);
  fail;
}


static int
pl_nl__LD(term_t stream ARG_LD)
{ IOSTREAM *s;

  if ( getTextOutputStream(stream, &s) )
  { Sputcode('\n', s);
    return streamStatus(s);
  }

  return FALSE;
}


static
PRED_IMPL("nl", 1, nl, PL_FA_ISO)
{ PRED_LD

  return pl_nl__LD(A1 PASS_LD);
}

static
PRED_IMPL("nl", 0, nl, PL_FA_ISO)
{ PRED_LD

  return pl_nl__LD(0 PASS_LD);
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
  int rc = TRUE;

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
enterPortray(ARG1_LD)
{ if ( LD->IO.portray_nesting >= MAX_PORTRAY_NESTING )
    return PL_resource_error("portray_nesting");
  LD->IO.portray_nesting++;
  return TRUE;
}


static void
leavePortray(ARG1_LD)
{ LD->IO.portray_nesting--;
}


/* returns: -1: error, FALSE: failed, TRUE: succeeded
*/

static int
callPortray(term_t arg, int prec, write_options *options)
{ predicate_t pred;

  if ( GD->cleaning > CLN_PROLOG )
    fail;				/* avoid dangerous callbacks */

  if ( options->portray_goal )
  { pred = _PL_predicate("call", 3, "user", &GD->procedures.call3);
  } else
  { pred = _PL_predicate("portray", 1, "user", &GD->procedures.portray);
    if ( !pred->definition->impl.any )
      return FALSE;
  }

  { GET_LD
    wakeup_state wstate;
    IOSTREAM *old = Scurout;
    int rval;
    term_t av;

    if ( !enterPortray(PASS_LD1) )
      return -1;
    if ( !saveWakeup(&wstate, TRUE PASS_LD) )
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
    restoreWakeup(&wstate PASS_LD);
    leavePortray(PASS_LD1);

    return rval;
  }

  fail;
}


static bool
writeArgTerm(term_t t, int prec, write_options *options, bool arg)
{ GET_LD
  int rval;
  int levelSave = options->depth;
  fid_t fid;

  if ( !(fid = PL_open_foreign_frame()) )
    return FALSE;

  if ( PL_handle_signals() < 0 )
  { rval = FALSE;
    goto out;
  }

  if ( ++options->depth > options->max_depth && options->max_depth )
  { PutOpenToken('.', options->out);
    rval = PutString("...", options->out);
  } else
  { rval = writeTerm2(t, prec, options, arg);
  }

out:
  options->depth = levelSave;
  PL_close_foreign_frame(fid);

  return rval;
}

static bool
writeTerm(term_t t, int prec, write_options *options)
{
  return writeArgTerm(t, prec, options, FALSE);
}

static bool
writeList(term_t list, write_options *options)
{ GET_LD
  term_t head = PL_new_term_ref();
  term_t l    = PL_copy_term_ref(list);

  if ( false(options, PL_WRT_DOTLISTS) )
  { TRY(Putc('[', options->out));
    for(;;)
    { PL_get_list(l, head, l);
      TRY(writeArgTerm(head, 999, options, TRUE));

      if ( PL_get_nil(l) )
	break;
      if ( ++options->depth >= options->max_depth && options->max_depth )
	return PutString("|...]", options->out);
      if ( !PL_is_functor(l, FUNCTOR_dot2) )
      { TRY(Putc('|', options->out));
	TRY(writeArgTerm(l, 999, options, TRUE));
	break;
      }

      TRY(PutComma(options));
    }

    return Putc(']', options->out);
  } else
  { int depth = 0;

    for(;;)
    { PL_get_list(l, head, l);
      if ( !PutToken(".", options->out) ||
	   !Putc('(', options->out) ||
	   !writeArgTerm(head, 999, options, TRUE) ||
	   !PutComma(options) )
	return FALSE;

      depth++;

      if ( PL_get_nil(l) )
      { if ( !PutToken("[]", options->out) )
	  return FALSE;
	break;
      }

      if ( ++options->depth >= options->max_depth && options->max_depth )
      { if ( !PutToken("...", options->out) )
	  return FALSE;
	while(depth-->0)
	{ if ( !Putc(')', options->out) )
	    return FALSE;
	}
	return TRUE;
      }

      if ( !PL_is_functor(l, FUNCTOR_dot2) )
      { if ( !writeArgTerm(l, 999, options, TRUE) )
	  return FALSE;
	break;
      }
    }

    while(depth-->0)
    { if ( !Putc(')', options->out) )
	return FALSE;
    }
    return TRUE;
  }
}


static int
isBlockOp(term_t t, term_t arg, atom_t functor ARG_LD)
{ if ( functor == ATOM_nil || functor == ATOM_curl )
  { _PL_get_arg(1, t, arg);
    if ( (functor == ATOM_nil  && PL_is_pair(arg)) ||
	 (functor == ATOM_curl && PL_is_functor(arg, FUNCTOR_curl1)) )
      return TRUE;
  }

  return FALSE;
}


static int
writeDictPair(term_t name, term_t value, int last, void *closure)
{ write_options *options = closure;

  if ( writeTerm(name, 1200, options) &&
       PutToken(":", options->out) &&
       writeTerm(value, 999, options) &&
       (last || PutComma(options)) )
    return 0;				/* continue */

  return -1;
}


static bool
writeTerm2(term_t t, int prec, write_options *options, bool arg)
{ GET_LD
  atom_t functor;
  size_t arity, n;
  int op_type, op_pri;
  atom_t a;
  IOSTREAM *out = options->out;

  if ( !PL_is_variable(t) &&
       true(options, PL_WRT_PORTRAY) )
  { switch( callPortray(t, prec, options) )
    { case TRUE:
	return TRUE;
      case FALSE:
	break;
      default:					/* error */
	return FALSE;
    }
  }

  if ( PL_get_atom(t, &a) )
  { if ( !arg && prec < 1200 && priorityOperator(NULL, a) > 0 )
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
  { if ( true(options, PL_WRT_NUMBERVARS|PL_WRT_VARNAMES) )
    { switch( writeNumberVar(t, options PASS_LD) )
      { case -1:
	  return FALSE;
	case TRUE:
	  return TRUE;
      }
    }

					/* handle {a,b,c} */
    if ( false(options, PL_WRT_BRACETERMS) &&
	 functor == ATOM_curl && arity == 1 )
    { term_t arg;

      if ( (arg=PL_new_term_ref()) &&
	   PL_get_arg(1, t, arg) &&
	   PutToken("{", out) &&
	   writeTerm(arg, 1200, options) &&
	   Putc('}', out) )
	return TRUE;

      return FALSE;
    }

					/* handle lists */
    if ( functor == ATOM_dot && arity == 2 )
      return writeList(t, options);

					/* handle dicts */
    if ( false(options, PL_WRT_NODICT) &&
	 functor == ATOM_dict && PL_is_dict(t) )
    { term_t class;

      if ( (class=PL_new_term_ref()) &&
	   PL_get_arg(1, t, class) )
      { if ( writeTerm(class, 1200, options) &&
	     Putc('{', out) &&
	     PL_for_dict(t, writeDictPair, options, DICT_SORTED) == 0 &&
	     Putc('}', out) )
	  return TRUE;
      }

      return FALSE;
    }
					/* operators */
    if ( false(options, PL_WRT_IGNOREOPS) )
    { term_t arg;

      if ( !(arg=PL_new_term_ref()) )
	return FALSE;

      if ( arity == 1 ||
	  (arity == 2 && isBlockOp(t, arg, functor PASS_LD)) )
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
	    TRY(writeTerm(arg, 1200, options));
	  }
				/* +/-(Number) : avoid parsing as number */
	  options->out->lastc |= C_PREFIX_OP;
	  if ( functor == ATOM_minus || functor == ATOM_plus )
	    options->out->lastc |= C_PREFIX_SIGN;

	  _PL_get_arg(arity, t, arg);
	  TRY(writeTerm(arg,
			op_type == OP_FX ? op_pri-1 : op_pri,
			options));

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
			options));
	  if ( arity == 1 )
	  { TRY(writeAtom(functor, options));
	  } else
	  { _PL_get_arg(1, t, arg);
	    TRY(writeTerm(arg, 1200, options));
	  }
	  if (op_pri > prec)
	    TRY(PutCloseBrace(out));

	  succeed;
	}
      } else if ( arity == 2 ||
		 (arity == 3 && isBlockOp(t, arg, functor PASS_LD)) )
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
			options));
	  if ( arity == 2 )
	  { if ( functor == ATOM_comma )
	    { TRY(PutComma(options));
	    } else if ( functor == ATOM_bar )
	    { TRY(PutBar(options));
	    } else if ( functor == ATOM_fdot )
	    { TRY(PutToken(".", out));
	    } else
	    { switch(writeAtom(functor, options))
	      { case FALSE:
		  fail;
	        case TRUE_WITH_SPACE:
		  TRY(Putc(' ', out));
	      }
	    }
	    options->out->lastc |= C_INFIX_OP;
	  } else			/* block operator */
	  { _PL_get_arg(1, t, arg);
	    TRY(writeTerm(arg, 1200, options));
	  }
	  _PL_get_arg(arity, t, arg);
	  TRY(writeTerm(arg,
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
	  TRY(PutComma(options));
	_PL_get_arg(n+1, t, a);
	TRY(writeArgTerm(a, 999, options, TRUE));
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
    return FALSE;

  while(PL_get_list(s_tail, s_head, s_tail))
  { _PL_get_arg(1, s_head, var);
    _PL_get_arg(2, s_head, value);
    if ( PL_var_occurs_in(var, value) )
    { if ( (options->flags&PL_WRT_NUMBERVARS) )
      { if ( !PL_unify_term(var,
			    PL_FUNCTOR, FUNCTOR_isovar1,
			      PL_INTPTR, --count) )
	  return FALSE;
      }

      if ( !PL_unify_list(c_tail, c_head, c_tail) ||
	   !PL_unify(c_head, s_head) )
	return FALSE;
    } else
    { if ( !PL_unify(var, value) )
	return FALSE;
    }
  }

  return PL_unify_nil(c_tail);
}


static int
writeTopTerm(term_t term, int prec, write_options *options)
{ GET_LD
  int rc;

  Slock(options->out);
  if ( (!(options->flags&PL_WRT_NO_CYCLES) && options->max_depth) ||
       PL_is_acyclic(term) )
  { rc = writeTerm(term, prec, options);
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
			PL_FUNCTOR, FUNCTOR_xpceref2,
			  PL_TERM, template,
			  PL_TERM, cycles) )
      return FALSE;
    rc = writeTerm(at_term, prec, options);
    PL_discard_foreign_frame(fid);
  }
  Sunlock(options->out);

  return rc;
}


static int
bind_varnames(term_t names ARG_LD)
{ term_t tail, head, var, namet;
  int check_cycle_after = 1000;

  if ( !(tail = PL_copy_term_ref(names)) ||
       !(head = PL_new_term_ref()) ||
       !(var  = PL_new_term_ref()) ||
       !(namet = PL_new_term_ref()) )
    return FALSE;

  while(PL_get_list_ex(tail, head, tail))
  { if ( PL_is_functor(head, FUNCTOR_equals2) )
    { atom_t name;

      _PL_get_arg(2, head, var);
      _PL_get_arg(1, head, namet);

      if ( !PL_get_atom_ex(namet, &name) )
	return FALSE;
      if ( !atomIsVarName(name) )
	return PL_domain_error("variable_name", namet);

      if ( PL_is_variable(var) )
      { if ( !PL_unify_term(var,
			    PL_FUNCTOR, FUNCTOR_isovar1,
			      PL_ATOM, name) )
	  return FALSE;
      }
    } else
      return PL_type_error("variable_assignment", head);

    if ( --check_cycle_after == 0 &&
	 lengthList(tail, FALSE) == -1 )
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


static const opt_spec write_term_options[] =
{ { ATOM_quoted,	    OPT_BOOL },
  { ATOM_ignore_ops,	    OPT_BOOL },
  { ATOM_dotlists,	    OPT_BOOL },
  { ATOM_brace_terms,	    OPT_BOOL },
  { ATOM_numbervars,        OPT_BOOL },
  { ATOM_portray,           OPT_BOOL },
  { ATOM_portrayed,         OPT_BOOL },
  { ATOM_portray_goal,      OPT_TERM },
  { ATOM_character_escapes, OPT_BOOL },
  { ATOM_max_depth,	    OPT_INT  },
  { ATOM_module,	    OPT_ATOM },
  { ATOM_back_quotes,	    OPT_ATOM },
  { ATOM_attributes,	    OPT_ATOM },
  { ATOM_priority,	    OPT_INT },
  { ATOM_partial,	    OPT_BOOL },
  { ATOM_spacing,	    OPT_ATOM },
  { ATOM_blobs,		    OPT_ATOM },
  { ATOM_cycles,	    OPT_BOOL },
  { ATOM_variable_names,    OPT_TERM },
  { ATOM_nl,		    OPT_BOOL },
  { ATOM_fullstop,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};

foreign_t
pl_write_term3(term_t stream, term_t term, term_t opts)
{ GET_LD
  bool quoted     = FALSE;
  bool ignore_ops = FALSE;
  bool dotlists   = FALSE;
  bool braceterms = FALSE;
  bool numbervars = -1;			/* not set */
  bool portray    = FALSE;
  term_t gportray = 0;
  atom_t bq       = 0;
  bool charescape = -1;			/* not set */
  atom_t mname    = ATOM_user;
  atom_t attr     = ATOM_nil;
  atom_t blobs    = ATOM_nil;
  int  priority   = 1200;
  bool partial    = FALSE;
  bool cycles     = TRUE;
  bool nl         = FALSE;
  bool fullstop   = FALSE;
  term_t varnames = 0;
  int local_varnames;
  IOSTREAM *s = NULL;
  write_options options;
  int rc;

  memset(&options, 0, sizeof(options));
  options.spacing = ATOM_standard;

  if ( !scan_options(opts, 0, ATOM_write_option, write_term_options,
		     &quoted, &ignore_ops, &dotlists, &braceterms,
		     &numbervars, &portray, &portray, &gportray,
		     &charescape, &options.max_depth, &mname,
		     &bq, &attr, &priority, &partial, &options.spacing,
		     &blobs, &cycles, &varnames, &nl, &fullstop) )
    fail;

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
  if ( charescape == TRUE ||
       (charescape == -1 && true(options.module, M_CHARESCAPE)) )
    options.flags |= PL_WRT_CHARESCAPES;
  if ( gportray )
  { options.portray_goal = gportray;
    if ( !put_write_options(opts, &options) ||
	 !PL_qualify(options.portray_goal, options.portray_goal) )
      return FALSE;
    if ( false(&options, PL_WRT_BLOB_PORTRAY) )
      portray = TRUE;
  }
  if ( numbervars == -1 )
    numbervars = (portray ? TRUE : FALSE);

  if ( quoted )     options.flags |= PL_WRT_QUOTED;
  if ( ignore_ops ) options.flags |= PL_WRT_IGNOREOPS;
  if ( dotlists )   options.flags |= PL_WRT_DOTLISTS;
  if ( braceterms ) options.flags |= PL_WRT_BRACETERMS;
  if ( numbervars ) options.flags |= PL_WRT_NUMBERVARS;
  if ( portray )    options.flags |= PL_WRT_PORTRAY;
  if ( !cycles )    options.flags |= PL_WRT_NO_CYCLES;
  if ( bq )
  { unsigned int flags = 0;

    if ( !setBackQuotes(bq, &flags) )
      return FALSE;
    if ( (flags&BQ_STRING) )
      options.flags |= PL_WRT_BACKQUOTED_STRING;
    else if ( flags == 0 )
      options.flags |= PL_WRT_BACKQUOTE_IS_SYMBOL;
  }

  local_varnames = (varnames && false(&options, PL_WRT_NUMBERVARS));

  BEGIN_NUMBERVARS(local_varnames);
  if ( varnames )
  { if ( (rc=bind_varnames(varnames PASS_LD)) )
      options.flags |= PL_WRT_VARNAMES;
    else
      goto out;
  }
  if ( !(rc=getTextOutputStream(stream, &s)) )
    goto out;

  options.out = s;
  if ( !partial )
    PutOpenToken(EOF, s);		/* reset this */
  if ( (options.flags & PL_WRT_QUOTED) && !(s->flags&SIO_REPPL) )
  { s->flags |= SIO_REPPL;
    rc = writeTopTerm(term, priority, &options);
    s->flags &= ~SIO_REPPL;
  } else
  { rc = writeTopTerm(term, priority, &options);
  }

  if ( rc && fullstop )
    rc = PutToken(".", s) && Putc(nl ? '\n' : ' ', s);
  else if ( nl )
    rc = Putc('\n', s);

out:
  END_NUMBERVARS(local_varnames);

  return (!s || streamStatus(s)) && rc;
}


foreign_t
pl_write_term(term_t term, term_t options)
{ return pl_write_term3(0, term, options);
}


int
PL_write_term(IOSTREAM *s, term_t term, int precedence, int flags)
{ write_options options;
  int rc;

  memset(&options, 0, sizeof(options));
  options.flags	    = flags;
  options.out	    = s;
  options.module    = MODULE_user;

  if ( (s=PL_acquire_stream(s)) )
  { PutOpenToken(EOF, s);			/* reset this */
    rc = writeTopTerm(term, precedence, &options);
    if ( rc && (flags&PL_WRT_NEWLINE) )
      rc = Putc('\n', s);
    rc = PL_release_stream(s) && rc;
  } else
    rc = FALSE;

  return rc;
}


static word
do_write2(term_t stream, term_t term, int flags, int canonical)
{ GET_LD
  IOSTREAM *s;

  if ( getTextOutputStream(stream, &s) )
  { write_options options;
    int rc;

    memset(&options, 0, sizeof(options));
    options.flags     = flags;
    if ( !canonical )
      options.flags |= LD->prolog_flag.write_attributes;
    options.out	      = s;
    options.module    = MODULE_user;
    if ( true(options.module, M_CHARESCAPE) )
      options.flags |= PL_WRT_CHARESCAPES;
    if ( true(options.module, BQ_STRING) )
      options.flags |= PL_WRT_BACKQUOTED_STRING;

    PutOpenToken(EOF, s);		/* reset this */
    rc = writeTopTerm(term, 1200, &options);
    if ( rc && (flags&PL_WRT_NEWLINE) )
      rc = Putc('\n', s);

    return streamStatus(s) && rc;
  }

  return FALSE;
}


foreign_t
pl_write2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_NUMBERVARS, FALSE);
}

foreign_t
pl_writeln2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_NUMBERVARS|PL_WRT_NEWLINE, FALSE);
}

foreign_t
pl_writeq2(term_t stream, term_t term)
{ return do_write2(stream, term, PL_WRT_QUOTED|PL_WRT_NUMBERVARS, FALSE);
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
		   PL_WRT_PORTRAY|PL_WRT_NUMBERVARS|PL_WRT_QUOTED, FALSE);

  PL_discard_foreign_frame(fid);

  return rc;
}

word
pl_write_canonical2(term_t stream, term_t term)
{ GET_LD
  nv_options options;
  word rc;

  BEGIN_NUMBERVARS(TRUE);

  options.functor = FUNCTOR_isovar1;
  options.on_attvar = AV_SKIP;
  options.singletons = PL_is_acyclic(term);
  options.numbered_check = FALSE;

  rc = ( numberVars(term, &options, 0 PASS_LD) != NV_ERROR &&
	 do_write2(stream, term,
		   PL_WRT_QUOTED|PL_WRT_IGNOREOPS|PL_WRT_NUMBERVARS|
		   PL_WRT_NODOTINATOM, TRUE)
       );

  END_NUMBERVARS(TRUE);

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
{ return do_write2(0, term, PL_WRT_NUMBERVARS|PL_WRT_NEWLINE, FALSE);
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

/** '$put_quoted_codes'(+Stream, +Quote, +Codes, +Options)

Emit Codes using the escaped character  syntax,   but  does not emit the
start and end-code itself. Options is  currently ignored. It is intended
to provide additional preferences, so as using \uXXXX, \UXXXXXXXX, etc.
*/

static
PRED_IMPL("$put_quoted", 4, put_quoted_codes, 0)
{ IOSTREAM *out;
  pl_wchar_t *w;
  size_t i, len;
  int quote;
  int flags = PL_WRT_CHARESCAPES;
  int rc = TRUE;

  if ( !PL_get_stream_handle(A1, &out) ||
       !PL_get_char_ex(A2, &quote, FALSE) ||
       !PL_get_wchars(A3, &len, &w, CVT_LIST|CVT_STRING|CVT_EXCEPTION) )
    return FALSE;

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
    return -1;

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
	return FALSE;
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
    pushOutputContext();
    Scurout = s;
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

  return FALSE;
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

