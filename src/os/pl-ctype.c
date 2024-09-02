/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-ctype.h"
#include "pl-text.h"
#include "pl-utf8.h"
#include "pl-buffer.h"
#include "../pl-read.h"
#include "../pl-fli.h"
#include <ctype.h>
#include <errno.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines:

	char_type(?Char, ?Type)
	code_type(?Char, ?Type)

See manual for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CTX_CHAR 0			/* Class(Char) */
#define CTX_CODE 1			/* Class(Int) */

typedef struct
{ atom_t	name;			/* name of the class */
  int (*test)(int chr);			/* boolean */
  int (*reverse)(int chr);		/* reverse mapping */
  short		arity;			/* arity of class (i.e. lower('A')) */
  short		ctx_type;		/* CTX_* */
} char_type;

#define ENUM_NONE	0x00
#define ENUM_CHAR	0x01
#define ENUM_CLASS	0x02
#define ENUM_BOTH	0x03

typedef struct
{ int		current;		/* current character */
  const char_type   *class;		/* current class */
  int		do_enum;		/* what to enumerate */
} generator;


static int
iswhite(int chr)
{ return chr == ' ' || chr == '\t';
}


static int
fiscsym(int chr)
{ return iswalnum(chr) || chr == '_';
}


static int
fiscsymf(int chr)
{ return iswalpha(chr) || chr == '_';
}

static int
iseof(int chr)
{ return chr == -1;
}

static int
iseol(int chr)
{ return chr >= 10 && chr <= 13;
}

static int
isnl(int chr)
{ return chr == '\n';
}

static int
isperiod(int chr)
{ return chr && strchr(".?!", chr) != NULL;
}

static int
isquote(int chr)
{ return chr && strchr("'`\"", chr) != NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Character  based  case conversion.   This  is  not well  supported  on
Windows as  `wint_t` is only  16 bits.   We use the  string conversion
functions instead.   Now, in theory,  the case converted version  of a
Unicode character can  have multiple characters.  For now,  we print a
warning.  This does not seem to happen on Windows.  To test, use

    ?- forall(between(0, 0x10ffff, X), code_type(X, to_lower(U))).
    ?- forall(between(0, 0x10ffff, X), code_type(X, to_upper(L))).

The fupper() and flower() function only return a value if the input is
lower/upper.   We have  the same  `wint_t` problem  here and  assume a
character is  not lower if `ftoupper(c)  != c`.  This too  is somewhat
dubious.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
ftoupper(int chr)
{
#if SIZEOF_WINT_T == 2
  wchar_t tmp[4];
  int upr;

  *put_wchar(tmp, chr) = 0;
  _wcsupr_s(tmp, sizeof(tmp)/sizeof(wchar_t));
  if ( *get_wchar(tmp, &upr) )
    Sdprintf("Oops, upper for u%x has multiple characters\n");
  return upr;
#endif
  return towupper(chr);
}

static int
ftolower(int chr)
{
#if SIZEOF_WINT_T == 2
  wchar_t tmp[4];
  int lwr;

  *put_wchar(tmp, chr) = 0;
  _wcslwr_s(tmp, sizeof(tmp)/sizeof(wchar_t));
  if ( *get_wchar(tmp, &lwr) )
    Sdprintf("Oops, lower for u%x has multiple characters\n");
  return lwr;
#endif
  return towlower(chr);
}

static int
fupper(int chr)
{
#if SIZEOF_WINT_T == 2
  int upr = ftoupper(chr);
  return upr == chr ? -1 : upr;
#endif
  return iswlower(chr) ? (int)towupper(chr) : -1;
}

static int
flower(int chr)
{
#if SIZEOF_WINT_T == 2
  int lwr = ftoupper(chr);
  return lwr == chr ? -1 : lwr;
#endif
  return iswupper(chr) ? (int)towlower(chr) : -1;
}

static int
fparen(int chr)
{ switch(chr)
  { case '(':
      return ')';
    case '{':
      return '}';
    case '[':
      return ']';
    default:
      return -1;
  }
}


static int
rparen(int chr)
{ switch(chr)
  { case ')':
      return '(';
    case '}':
      return '{';
    case ']':
      return '[';
    default:
      return -1;
  }
}


static int
fdigit(int chr)
{ if ( chr <= 0xff && isdigit(chr) )
    return chr - '0';
  return -1;
}


static int
rdigit(int d)
{ if ( (int)d >= 0 && d <= 9 )
    return d+'0';
  return -1;
}


static int
ff_decimal(int chr)
{ if ( f_is_decimal(chr) )
    return decimal_weight(chr);
  else
    return -1;
}

static int
fxdigit(int chr)
{ if ( chr > 0xff )
    return -1;
  if ( isdigit(chr) )
    return chr - '0';
  if ( chr >= 'a' && chr <= 'f' )
    return chr - 'a' + 10;
  if ( chr >= 'A' && chr <= 'F' )
    return chr - 'A' + 10;
  return -1;
}


static int
rxdigit(int d)
{ if ( (int)d >= 0 && d <= 9 )
    return d+'0';
  if ( d >= 10 && d <= 15 )
    return d-10+'a';
  return -1;
}

#if SIZEOF_WINT_T == 2
#define mkfunction(name) \
	static int f ## name(int chr) { if ( chr > 0xffff ) return 0; \
					return name(chr); }
#else
#define mkfunction(name) \
	static int f ## name(int chr) { return name(chr); }
#endif

mkfunction(iswalnum)
mkfunction(iswalpha)
mkfunction(isascii)
mkfunction(iswcntrl)
mkfunction(iswdigit)
mkfunction(iswprint)
mkfunction(iswgraph)
mkfunction(iswlower)
mkfunction(iswupper)
mkfunction(iswpunct)
mkfunction(iswspace)

static const char_type char_types[] =
{ { ATOM_alnum,			     fiswalnum },
  { ATOM_alpha,			     fiswalpha },
  { ATOM_csym,			     fiscsym },
  { ATOM_csymf,			     fiscsymf },
  { ATOM_prolog_var_start,	     f_is_prolog_var_start },
  { ATOM_prolog_atom_start,	     f_is_prolog_atom_start },
  { ATOM_prolog_identifier_continue, f_is_prolog_identifier_continue },
  { ATOM_decimal,		     f_is_decimal },
  { ATOM_decimal,		     ff_decimal, NULL, 1, CTX_CODE },
  { ATOM_prolog_symbol,		     f_is_prolog_symbol },
  { ATOM_csymf,			     fiscsymf },
  { ATOM_ascii,			     fisascii },
  { ATOM_white,			     iswhite },
  { ATOM_cntrl,			     fiswcntrl },
  { ATOM_digit,			     fiswdigit },
  { ATOM_print,			     fiswprint },
  { ATOM_graph,			     fiswgraph },
  { ATOM_lower,			     fiswlower },
  { ATOM_upper,			     fiswupper },
  { ATOM_punct,			     fiswpunct },
  { ATOM_space,			     fiswspace },
  { ATOM_end_of_file,		     iseof },
  { ATOM_end_of_line,		     iseol },
  { ATOM_newline,		     isnl },
  { ATOM_period,		     isperiod },
  { ATOM_quote,			     isquote },
  { ATOM_lower,			     fupper,	flower,   1, CTX_CHAR },
  { ATOM_upper,			     flower,	fupper,   1, CTX_CHAR },
  { ATOM_to_lower,		     ftoupper,	ftolower, 1, CTX_CHAR },
  { ATOM_to_upper,		     ftolower,	ftoupper, 1, CTX_CHAR },
  { ATOM_paren,			     fparen,	rparen,   1, CTX_CHAR },
  { ATOM_digit,			     fdigit,	rdigit,   1, CTX_CODE },
  { ATOM_xdigit,		     fxdigit,	rxdigit,  1, CTX_CODE },
  { NULL_ATOM,			     NULL }
};


static const char_type *
char_type_by_name(atom_t name, int arity)
{ const char_type *cc;

  for(cc = char_types; cc->name; cc++)
  { if ( cc->name == name && cc->arity == arity )
      return cc;
  }

  return NULL;
}


static int
advanceGen(generator *gen)
{ if ( gen->do_enum & ENUM_CHAR )
  { if ( ++gen->current > UNICODE_MAX )
      fail;
  } else
  { gen->class++;
    if ( !gen->class->name )
      fail;
  }

  succeed;
}


static int
unify_char_type(term_t type, const char_type *ct, int context, int how)
{ GET_LD

  if ( ct->arity == 0 )
  { return PL_unify_atom(type, ct->name);
  } else /*if ( ct->arity == 1 )*/
  { if ( PL_unify_functor(type, PL_new_functor(ct->name, 1)) )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, type, a);

      if ( ct->ctx_type == CTX_CHAR )
	return PL_unify_char(a, context, how);
      else
	return PL_unify_integer(a, context);
    }
  }

  fail;
}


static foreign_t
do_char_type(term_t chr, term_t class, control_t h, int how)
{ GET_LD
  generator *gen;
  fid_t fid;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { const char_type *cc = NULL;
      int c;
      int do_enum = ENUM_NONE;
      atom_t cn;
      size_t arity;

      if ( PL_is_variable(chr) )
	do_enum |= ENUM_CHAR;
      if ( PL_is_variable(class) )
	do_enum |= ENUM_CLASS;

      if ( do_enum == ENUM_BOTH )
	return PL_error("char_type", 2, NULL, ERR_INSTANTIATION);

      if ( !(do_enum & ENUM_CHAR) )
      { if ( !PL_get_char_ex(chr, &c, true) )
	  return false;
	if ( c == -1 )
	  return PL_unify_atom(class, ATOM_end_of_file);
      }

      if ( !(do_enum & ENUM_CLASS) )
      { if ( !PL_get_name_arity(class, &cn, &arity) ||
	     !(cc = char_type_by_name(cn, (int)arity)) )
	  return PL_error("char_type", 2, NULL,
			  ERR_TYPE, ATOM_char_type, class);
      }

      if ( do_enum == ENUM_NONE )
      { if ( arity == 0 )
	  return (*cc->test)(c) ? true : false;
	else
	{ int rval = (*cc->test)(c);

	  if ( rval >= 0 )
	  { term_t a = PL_new_term_ref();
	    int ok;

	    _PL_get_arg(1, class, a);

	    if ( cc->ctx_type == CTX_CHAR )
	      ok = PL_unify_char(a, rval, how);
	    else
	      ok = PL_unify_integer(a, rval);

	    if ( ok )
	      return true;
	    else
	      do_enum = ENUM_CHAR;	/* try the other way around */
	  } else
	    fail;
	}
      }

      if ( do_enum == ENUM_CHAR && arity == 1 && cc->reverse )
      {	term_t a = PL_new_term_ref();	/* char_type(X, lower('A')) */
	int ca;

	_PL_get_arg(1, class, a);
	if ( !PL_is_variable(a) )
	{ if ( PL_get_char(a, &ca, false) )
	  { int c = (*cc->reverse)(ca);

	    if ( c < 0 )
	      fail;

	    return PL_unify_char(chr, c, how);
	  }
	  fail;				/* error */
	}
      }

      gen = allocForeignState(sizeof(*gen));
      gen->do_enum = do_enum;

      if ( do_enum & ENUM_CHAR )
      { gen->class      = cc;
	gen->current    = -1;
      } else if ( do_enum & ENUM_CLASS )
      { gen->class	= char_types;
	gen->current    = c;
      }

      break;
    }
    case FRG_REDO:
      gen = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
      gen = ForeignContextPtr(h);
      if ( gen )
	freeForeignState(gen, sizeof(*gen));
    default:
      succeed;
  }

  if ( !(fid = PL_open_foreign_frame()) )
    goto error;

  for(;;)
  { int rval = (*gen->class->test)(gen->current);

    if ( (gen->class->arity == 0 && rval) ||
	 (gen->class->arity  > 0 && rval >= 0) )
    { if ( (gen->do_enum & ENUM_CHAR) )
      { if ( !PL_unify_char(chr, gen->current, how) )
	{ if ( LD->exception.term )
	    goto error;
	  goto next;
	}
      }
      if ( (gen->do_enum & ENUM_CLASS) || gen->class->arity > 0 )
      { if ( !unify_char_type(class, gen->class, rval, how) )
	{ if ( LD->exception.term )
	    goto error;
	  goto next;
	}
      }

      if ( advanceGen(gen) )		/* ok, found one */
      { ForeignRedoPtr(gen);
      } else
      { freeForeignState(gen, sizeof(*gen));	/* the only one */
	succeed;
      }
    }
  next:
    PL_rewind_foreign_frame(fid);

    if ( !advanceGen(gen) )
      break;
  }

error:
  freeForeignState(gen, sizeof(*gen));
  fail;
}



static
PRED_IMPL("char_type", 2, char_type, PL_FA_NONDETERMINISTIC)
{ return do_char_type(A1, A2, PL__ctx, PL_CHAR);
}


static
PRED_IMPL("code_type", 2, code_type, PL_FA_NONDETERMINISTIC)
{ return do_char_type(A1, A2, PL__ctx, PL_CODE);
}


#if 0
static
PRED_IMPL("iswctype", 2, iswctype, 0)
{ char *s;
  int chr;
  wctype_t t;

  if ( !PL_get_char_ex(A1, &chr, false) ||
       !PL_get_chars(A2, &s, CVT_ATOM|CVT_EXCEPTION) )
    return false;

  if ( !(t=wctype(s)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_type, A2);

  return iswctype(chr, t) ? true : false;
}
#endif


static int
get_chr_from_text(const PL_chars_t *t, size_t *index)
{ if ( *index >= t->length )
    return -1;

  switch(t->encoding)
  { case ENC_ISO_LATIN_1:
      return t->text.t[(*index)++]&0xff;
    case ENC_WCHAR:
    { int c = t->text.w[(*index)++];
#if SIZEOF_WCHAR_T == 2
      if ( IS_UTF16_LEAD(c) )
      { int c2 = t->text.w[(*index)++];
	c = utf16_decode(c, c2);
      }
#endif
      return c;
    }
    default:
      assert(0);
      return 0;
  }
}


#define modify_case_atom(in, out, down, text_type) \
	LDFUNC(modify_case_atom, in, out, down, text_type)

static void
modify_case_latin_1_to_wide(PL_chars_t *tin, PL_chars_t *tout, Buffer b, int down)
{ const unsigned char *in = (const unsigned char*)tin->text.t;
  PL_free_text(tout);
  initBuffer(b);

  if ( down )
  { for(size_t i=0; i<tin->length; i++)
    { wint_t c = ftolower(in[i]);

      addWcharBuffer(b, c);
    }
  } else				/* upcase */
  { for(size_t i=0; i<tin->length; i++)
    { wint_t c = ftoupper(in[i]);

      addWcharBuffer(b, c);
    }
  }

  tout->storage   = PL_CHARS_HEAP;
  tout->text.w    = baseBuffer(b, wchar_t);
  tout->length    = entriesBuffer(b, wchar_t);
  tout->encoding  = ENC_WCHAR;
  tout->canonical = false;
}


static foreign_t
modify_case_atom(DECL_LD term_t in, term_t out, int down, int text_type)
{ PL_chars_t tin, tout;

  if ( !PL_get_text(in, &tin, CVT_ATOMIC|CVT_EXCEPTION) )
    return false;

  if ( PL_get_text(out, &tout, CVT_ATOMIC) )
  { size_t i1=0, i2=0;

    for(;;)
    { int ci = get_chr_from_text(&tin, &i1);
      int co = get_chr_from_text(&tout, &i2);

      if ( ci == -1 || co == -1 )
	return ci == co;

      if ( down )
      { if ( co != ftolower(ci) )
	  return false;
      } else
      { if ( co != ftoupper(ci) )
	  return false;
      }
    }

    return true;
  } else if ( PL_is_variable(out) )
  { tmp_buffer b;
    int rc;

    if ( tin.encoding == ENC_ISO_LATIN_1 )
    { const unsigned char *in = (const unsigned char*)tin.text.t;
      size_t i;

      tout.encoding  = tin.encoding;
      tout.length    = tin.length;
      tout.canonical = true;
      if ( tout.length < sizeof(tout.buf) )
      { tout.text.t  = tout.buf;
	tout.storage = PL_CHARS_LOCAL;
      } else
      { tout.text.t  = PL_malloc(tout.length);
	tout.storage = PL_CHARS_MALLOC;
      }

      if ( down )
      { for(i=0; i<tin.length; i++)
	{ wint_t c = ftolower(in[i]);

	  if ( c > 0xff )
	  { modify_case_latin_1_to_wide(&tin, &tout, (Buffer)&b, down);
	    break;
	  }
	  tout.text.t[i] = (char)c;
	}
      } else				/* upcase */
      { for(i=0; i<tin.length; i++)
	{ wint_t c = ftoupper(in[i]);

	  if ( c > 0xff )
	  { modify_case_latin_1_to_wide(&tin, &tout, (Buffer)&b, down);
	    break;
	  }
	  tout.text.t[i] = (char)c;
	}
      }
    } else				/* ENC_WCHAR */
    { const wchar_t* s = (const wchar_t*)tin.text.w;
      const wchar_t* e = &s[tin.length];
      int c;

      initBuffer(&b);

      if ( down )
      { while( s < e )
	{ s = get_wchar(s, &c);
	  c = ftolower(c);
	  addWcharBuffer((Buffer)&b, c);
	}
      } else
      { while( s < e )
	{ s = get_wchar(s, &c);
	  c = ftoupper(c);
	  addWcharBuffer((Buffer)&b, c);
	}
      }

      tout.storage   = PL_CHARS_HEAP;
      tout.text.w    = baseBuffer(&b, wchar_t);
      tout.length    = entriesBuffer(&b, wchar_t);
      tout.encoding  = ENC_WCHAR;
      tout.canonical = false;
    }

    rc = PL_unify_text(out, 0, &tout, text_type);
    PL_free_text(&tin);
    PL_free_text(&tout);
    if ( tout.encoding != ENC_ISO_LATIN_1 )
      discardBuffer(&b);

    return rc;
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE,
		    text_type == PL_STRING ? ATOM_string : ATOM_atom,
		    out);
  }
}


static
PRED_IMPL("downcase_atom", 2, downcase_atom, 0)
{ PRED_LD
  return modify_case_atom(A1, A2, true, PL_ATOM);
}


static
PRED_IMPL("upcase_atom", 2, upcase_atom, 0)
{ PRED_LD
  return modify_case_atom(A1, A2, false, PL_ATOM);
}


static
PRED_IMPL("string_lower", 2, string_lower, 0)
{ PRED_LD
  return modify_case_atom(A1, A2, true, PL_STRING);
}


static
PRED_IMPL("string_upper", 2, string_upper, 0)
{ PRED_LD
  return modify_case_atom(A1, A2, false, PL_STRING);
}


		 /*******************************
		 *	    WHITE SPACE		*
		 *******************************/

static void
skip_separators(PL_chars_t *t, size_t *i)
{ for(;;)
  { size_t at = *i;
    int c  = get_chr_from_text(t, &at);

    if ( c == -1 )
      return;

    if ( unicode_separator(c) )
      *i=at;
    else
      return;
  }
}


static int
copy_non_separators(IOSTREAM *out, PL_chars_t *t, size_t *i)
{ for(;;)
  { size_t at = *i;
    int c  = get_chr_from_text(t, &at);

    if ( c == -1 )
      return true;

    if ( !unicode_separator(c) )
    { *i=at;
      if ( Sputcode(c, out) < 0 )
	return false;
    } else
      return true;
  }
}


static int
write_normalize_space(IOSTREAM *out, term_t in)
{ GET_LD
  PL_chars_t tin;
  size_t i = 0, end;

  if ( !PL_get_text(in, &tin, CVT_ATOMIC|CVT_EXCEPTION) )
    return false;

  end = tin.length;
  skip_separators(&tin, &i);
  while( i<end )
  { copy_non_separators(out, &tin, &i);
    skip_separators(&tin, &i);
    if ( i < end )
    { if (  Sputcode(' ', out) < 0 )
	return false;
    }
  }

  return true;
}


static
PRED_IMPL("normalize_space", 2, normalize_space, 0)
{ redir_context ctx;
  foreign_t rc;

  if ( (rc = setupOutputRedirect(A1, &ctx, false)) )
  { if ( (rc = write_normalize_space(ctx.stream, A2)) )
      rc = closeOutputRedirect(&ctx);
    else
      discardOutputRedirect(&ctx);
  }

  return rc;
}



		 /*******************************
		 *	       LOCALE		*
		 *******************************/

#if defined(HAVE_LOCALE_H) && defined(HAVE_SETLOCALE)
#include <locale.h>

typedef struct
{ const char *name;
  IOENC encoding;
} enc_map;

static const enc_map map[] =
{ { "UTF-8",	  ENC_UTF8 },
  { "utf8",	  ENC_UTF8 },
  { "ISO8859-1",  ENC_ISO_LATIN_1 },
  { "ISO8859_1",  ENC_ISO_LATIN_1 },
  { "iso88591",   ENC_ISO_LATIN_1 },
  { "iso_8859_1", ENC_ISO_LATIN_1 },
  { NULL, ENC_UNKNOWN }
};

static IOENC
enc_from_locale(const char *enc)
{ const char *ext = strchr(enc, '.');
  const enc_map *m;

  if ( ext )
    ext++;
  else
    ext = enc;

  DEBUG(MSG_LOCALE, Sdprintf("Set encoding from \"%s\"\n", ext));

  for ( m=map; m->name; m++ )
  { if ( strcasecmp(ext, m->name) == 0 )
    { return m->encoding;
    }
  }

  return ENC_ANSI;			/* text encoding */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: on some installations, locale doesn't   work correctly. Printing a
message isn't really cute. It would be better to use printMessage(), but
the system isn't yet initialised far enough.   Maybe we should store the
failure and print a message at the end of the initialisation?

We only return false if LC_CTYPE  fails.   This  is a serious indication
that locale support is broken. We don't   depend too much on the others,
so we ignore possible problems.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static IOENC
init_locale(void)
{ int rc = ENC_UNKNOWN;

#ifdef __WINDOWS__
  UINT cp = GetACP();

  if ( cp == 65001 )
    rc = ENC_UTF8;
#endif

  if ( rc == ENC_UNKNOWN && setlocale(LC_CTYPE, "") )
  { char *enc;

    if ( (enc = setlocale(LC_CTYPE, NULL)) )
      rc = enc_from_locale(enc);
  } else if ( rc == ENC_UNKNOWN )
  { rc = ENC_ISO_LATIN_1;
    DEBUG(0, Sdprintf("Failed to set LC_CTYPE locale: %s\n", strerror(errno)));
  }

  if ( !setlocale(LC_TIME, "") )
  { DEBUG(0, Sdprintf("Failed to set LC_TIME locale\n"));
  }
  if ( !setlocale(LC_COLLATE, "") )
  { DEBUG(0, Sdprintf("Failed to set LC_COLLATE locale\n"));
  }

  return rc;
}

typedef struct
{ int category;
  const char *name;
} lccat;

static lccat lccats[] =
{ { LC_ALL,      "all" },
  { LC_COLLATE,  "collate" },
  { LC_CTYPE,    "ctype" },
#ifdef LC_MESSAGES
  { LC_MESSAGES, "messages" },
#endif
  { LC_MONETARY, "monetary" },
  { LC_NUMERIC,  "numeric" },
  { LC_TIME,     "time" },
  { 0,           NULL }
};


static
PRED_IMPL("setlocale", 3, setlocale, 0)
{ PRED_LD
  char *what;
  char *locale;
  const lccat *lcp;

  if ( !PL_get_chars(A1, &what, CVT_ATOM|CVT_EXCEPTION) )
    fail;
  if ( PL_is_variable(A3) )
    locale = NULL;
  else if ( !PL_get_chars(A3, &locale, CVT_ATOM|CVT_EXCEPTION) )
    fail;

  for ( lcp = lccats; lcp->name; lcp++ )
  { if ( streq(lcp->name, what) )
    { char *old = setlocale(lcp->category, NULL);

      if ( !PL_unify_chars(A2, PL_ATOM, -1, old) )
	fail;

      if ( PL_compare(A2, A3) != 0 )
      { if ( !setlocale(lcp->category, locale) )
	{ if ( errno == ENOENT )
	    return PL_existence_error("locale", A3);
	  return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "setlocale");
	}
      }

#ifdef O_LOCALE
      updateLocale(lcp->category, locale);
#endif

      succeed;
    }
  }

  return PL_domain_error("category", A1);
}

#else

#define init_locale() 1

static
PRED_IMPL("setlocale", 3, setlocale, 0)
{ return notImplemented("setlocale", 3);
}

#endif


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(ctype)
  PRED_DEF("char_type",	      2, char_type,	  PL_FA_NONDETERMINISTIC)
  PRED_DEF("code_type",	      2, code_type,	  PL_FA_NONDETERMINISTIC)
  PRED_DEF("setlocale",	      3, setlocale,	  0)
  PRED_DEF("downcase_atom",   2, downcase_atom,	  0)
  PRED_DEF("upcase_atom",     2, upcase_atom,	  0)
  PRED_DEF("string_lower",    2, string_lower,	  0)
  PRED_DEF("string_upper",    2, string_upper,	  0)
  PRED_DEF("normalize_space", 2, normalize_space, 0)
EndPredDefs


		 /*******************************
		 *	PROLOG CHARACTERS	*
		 *******************************/

const char _PL_char_types[] = {
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
   CT, CT, CT, CT, CT, CT, CT, CT, CT, SP, SP, SP, SP, SP, CT, CT,
/* ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_   16-31 */
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT,
/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   32-47 */
   SP, SO, DQ, SY, SY, SO, SY, SQ, PU, PU, SY, SY, PU, SY, SY, SY,
/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   48-63 */
   DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, SY, SO, SY, SY, SY, SY,
/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   64-79 */
   SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC,
/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   80-95 */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, PU, SY, PU, SY, UC,
/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   96-111 */
   BQ, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  ^?   112-127 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, PU, PU, PU, SY, CT,
			  /* 128-159 (C1 controls) */
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT,
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT,
			  /* 160-255 (G1 graphics) */
			  /* ISO Latin 1 (=Unicode) is assumed */
/*  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F */
   SP, SY, SY, SY, SY, SY, SY, SY, SY, SY, LC, SY, SY, SO, SY, SY, /*00AX*/
   SY, SY, SO, SO, SY, LC, SY, SY, SY, SO, LC, SY, SO, SO, SO, SY, /*00BX*/
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, /*00CX*/
   UC, UC, UC, UC, UC, UC, UC, SY, UC, UC, UC, UC, UC, UC, UC, LC, /*00DX*/
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, /*00EX*/
   LC, LC, LC, LC, LC, LC, LC, SY, LC, LC, LC, LC, LC, LC, LC, LC  /*00FX*/
};


IOENC
initEncoding(void)
{ GET_LD

  if ( HAS_LD )
  { if ( !LD->encoding )
    {
#if __APPLE__
      if ( !getenv("LANG") && !getenv("LC_CTYPE") )
	putenv("LC_CTYPE=UTF-8");
#endif

      LD->encoding = init_locale();
    }

    return LD->encoding;
  }

  return ENC_ANSI;
}


void
initCharTypes(void)
{ initEncoding();
}
