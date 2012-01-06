/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "pl-incl.h"
#include <ctype.h>
#include "pl-ctype.h"

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
  int (*test)(wint_t chr);		/* boolean */
  int (*reverse)(wint_t chr);		/* reverse mapping */
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
iswhite(wint_t chr)
{ return chr == ' ' || chr == '\t';
}


static int
fiscsym(wint_t chr)
{ return iswalnum(chr) || chr == '_';
}


static int
fiscsymf(wint_t chr)
{ return iswalpha(chr) || chr == '_';
}

static int
iseof(wint_t chr)
{ return chr == (wint_t)-1;
}

static int
iseol(wint_t chr)
{ return chr >= 10 && chr <= 13;
}

static int
isnl(wint_t chr)
{ return chr == '\n';
}

static int
isperiod(wint_t chr)
{ return chr && strchr(".?!", chr) != NULL;
}

static int
isquote(wint_t chr)
{ return chr && strchr("'`\"", chr) != NULL;
}

static int
fupper(wint_t chr)
{ return iswlower(chr) ? (int)towupper(chr) : -1;
}

static int
flower(wint_t chr)
{ return iswupper(chr) ? (int)towlower(chr) : -1;
}

static int
ftoupper(wint_t chr)
{ return towupper(chr);
}

static int
ftolower(wint_t chr)
{ return towlower(chr);
}

static int
fparen(wint_t chr)
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
rparen(wint_t chr)
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
fdigit(wint_t chr)
{ if ( chr <= 0xff && isdigit(chr) )
    return chr - '0';
  return -1;
}


static int
rdigit(wint_t d)
{ if ( (int)d >= 0 && d <= 9 )
    return d+'0';
  return -1;
}


static int
fxdigit(wint_t chr)
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
rxdigit(wint_t d)
{ if ( (int)d >= 0 && d <= 9 )
    return d+'0';
  if ( d >= 10 && d <= 15 )
    return d-10+'a';
  return -1;
}



#define mkfunction(name) \
	static int f ## name(wint_t chr) { return name(chr); }

mkfunction(iswalnum)
mkfunction(iswalpha)
mkfunction(isascii)
mkfunction(iswcntrl)
mkfunction(iswdigit)
mkfunction(iswgraph)
mkfunction(iswlower)
mkfunction(iswupper)
mkfunction(iswpunct)
mkfunction(iswspace)

static const char_type char_types[] =
{ { ATOM_alnum,		fiswalnum },
  { ATOM_alpha,		fiswalpha },
  { ATOM_csym,		fiscsym },
  { ATOM_csymf,		fiscsymf },
  { ATOM_ascii,		fisascii },
  { ATOM_white,		iswhite },
  { ATOM_cntrl,		fiswcntrl },
  { ATOM_digit,		fiswdigit },
  { ATOM_graph,		fiswgraph },
  { ATOM_lower,		fiswlower },
  { ATOM_upper,		fiswupper },
  { ATOM_punct,		fiswpunct },
  { ATOM_space,		fiswspace },
  { ATOM_end_of_file,	iseof },
  { ATOM_end_of_line,	iseol },
  { ATOM_newline,	isnl },
  { ATOM_period,	isperiod },
  { ATOM_quote,	        isquote },
  { ATOM_lower,		fupper,		flower,   1, CTX_CHAR },
  { ATOM_upper,		flower,		fupper,   1, CTX_CHAR },
  { ATOM_to_lower,	ftoupper,	ftolower, 1, CTX_CHAR },
  { ATOM_to_upper,	ftolower,	ftoupper, 1, CTX_CHAR },
  { ATOM_paren,		fparen,		rparen,   1, CTX_CHAR },
  { ATOM_digit,		fdigit,		rdigit,   1, CTX_CODE  },
  { ATOM_xdigit,	fxdigit,	rxdigit,  1, CTX_CODE  },
  { NULL_ATOM,		NULL }
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
  { if ( ++gen->current == 256 )
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
      int arity;

      if ( PL_is_variable(chr) )
	do_enum |= ENUM_CHAR;
      if ( PL_is_variable(class) )
	do_enum |= ENUM_CLASS;

      if ( do_enum == ENUM_BOTH )
	return PL_error("char_type", 2, NULL, ERR_INSTANTIATION);

      if ( !(do_enum & ENUM_CHAR) )
      { if ( !PL_get_char(chr, &c, TRUE) )
	  fail;
	if ( c == -1 )
	  return PL_unify_atom(class, ATOM_end_of_file);
      }

      if ( !(do_enum & ENUM_CLASS) )
      { if ( !PL_get_name_arity(class, &cn, &arity) ||
	     !(cc = char_type_by_name(cn, arity)) )
	  return PL_error("char_type", 2, NULL,
			  ERR_TYPE, ATOM_char_type, class);
      }

      if ( do_enum == ENUM_NONE )
      { if ( arity == 0 )
	  return (*cc->test)((wint_t)c) ? TRUE : FALSE;
	else
	{ int rval = (*cc->test)((wint_t)c);

	  if ( rval >= 0 )
	  { term_t a = PL_new_term_ref();
	    int ok;

	    _PL_get_arg(1, class, a);

	    if ( cc->ctx_type == CTX_CHAR )
	      ok = PL_unify_char(a, rval, how);
	    else
	      ok = PL_unify_integer(a, rval);

	    if ( ok )
	      return TRUE;
	    else
	      do_enum = ENUM_CHAR;	/* try the other way around */
	  } else
	    fail;
	}
      }

      if ( do_enum == ENUM_CHAR && arity == 1 )
      {	term_t a = PL_new_term_ref();	/* char_type(X, lower('A')) */
	int ca;

	_PL_get_arg(1, class, a);
	if ( !PL_is_variable(a) )
	{ if ( PL_get_char(a, &ca, FALSE) )
	  { int c = (*cc->reverse)((wint_t)ca);

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
  { int rval;

    if ( (rval = (*gen->class->test)((wint_t)gen->current)) )
    { if ( gen->do_enum & ENUM_CHAR )
      { if ( !PL_unify_char(chr, gen->current, how) )
	  goto next;
      }
      if ( gen->class->arity > 0 )
      { if ( rval < 0 ||
	     !unify_char_type(class, gen->class, rval, how) )
	  goto next;

      } else if ( gen->do_enum & ENUM_CLASS )
      { if ( !unify_char_type(class, gen->class, rval, how) )
	  goto next;
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

  if ( !PL_get_char_ex(A1, &chr, FALSE) ||
       !PL_get_chars(A2, &s, CVT_ATOM|CVT_EXCEPTION) )
    return FALSE;

  if ( !(t=wctype(s)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_type, A2);

  return iswctype(chr, t) ? TRUE : FALSE;
}
#endif


static int
init_tout(PL_chars_t *t, size_t len)
{ switch(t->encoding)
  { case ENC_ISO_LATIN_1:
      if ( len < sizeof(t->buf) )
      { t->text.t = t->buf;
	t->storage = PL_CHARS_LOCAL;
      } else
      { t->text.t = PL_malloc(len);
	t->storage = PL_CHARS_MALLOC;
      }
      succeed;
    case ENC_WCHAR:
      if ( len*sizeof(pl_wchar_t) < sizeof(t->buf) )
      { t->text.w = (pl_wchar_t*)t->buf;
	t->storage = PL_CHARS_LOCAL;
      } else
      { t->text.w = PL_malloc(len*sizeof(pl_wchar_t));
	t->storage = PL_CHARS_MALLOC;
      }
      succeed;
    default:
      assert(0);
      fail;
  }
}


static inline wint_t
get_chr_from_text(const PL_chars_t *t, size_t index)
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


static foreign_t
modify_case_atom(term_t in, term_t out, int down)
{ GET_LD
  PL_chars_t tin, tout;

  if ( !PL_get_text(in, &tin, CVT_ATOMIC|CVT_EXCEPTION) )
    return FALSE;

  if ( PL_get_text(out, &tout, CVT_ATOMIC) )
  { unsigned int i;

    if ( tin.length != tout.length )
      fail;

    for(i=0; i<tin.length; i++)
    { wint_t ci = get_chr_from_text(&tin, i);
      wint_t co = get_chr_from_text(&tout, i);

      if ( down )
      { if ( co != towlower(ci) )
	  fail;
      } else
      { if ( co != towupper(ci) )
	  fail;
      }
    }

    succeed;
  } else if ( PL_is_variable(out) )
  { unsigned int i;

    tout.encoding  = tin.encoding;
    tout.length    = tin.length;
    tout.canonical = FALSE;		/* or TRUE? Can WCHAR map to ISO? */

    init_tout(&tout, tin.length);

    if ( tin.encoding == ENC_ISO_LATIN_1 )
    { const unsigned char *in = (const unsigned char*)tin.text.t;

      if ( down )
      { for(i=0; i<tin.length; i++)
	{ wint_t c = towlower(in[i]);

	  if ( c > 255 )
	  { PL_promote_text(&tout);
	    for( ; i<tin.length; i++)
	    { tout.text.w[i] = towlower(in[i]);
	    }
	    break;
	  } else
	  { tout.text.t[i] = (char)c;
	  }
	}
      } else				/* upcase */
      { for(i=0; i<tin.length; i++)
	{ wint_t c = towupper(in[i]);

	  if ( c > 255 )
	  { PL_promote_text(&tout);
	    for( ; i<tin.length; i++)
	    { tout.text.w[i] = towupper(in[i]);
	    }
	    break;
	  } else
	  { tout.text.t[i] = (char)c;
	  }
	}
      }
    } else
    { if ( down )
      { for(i=0; i<tin.length; i++)
	{ tout.text.w[i] = towlower(tin.text.w[i]);
	}
      } else
      { for(i=0; i<tin.length; i++)
	{ tout.text.w[i] = towupper(tin.text.w[i]);
	}
      }
    }

    PL_unify_text(out, 0, &tout, PL_ATOM);
    PL_free_text(&tout);

    succeed;
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, out);
  }
}


static
PRED_IMPL("downcase_atom", 2, downcase_atom, 0)
{ return modify_case_atom(A1, A2, TRUE);
}


static
PRED_IMPL("upcase_atom", 2, upcase_atom, 0)
{ return modify_case_atom(A1, A2, FALSE);
}


		 /*******************************
		 *	    WHITE SPACE		*
		 *******************************/

static int
write_normalize_space(IOSTREAM *out, term_t in)
{ GET_LD
  PL_chars_t tin;
  size_t i, end;

  if ( !PL_get_text(in, &tin, CVT_ATOMIC|CVT_EXCEPTION) )
    return FALSE;

  end = tin.length;
  i = 0;

  while(i<end && unicode_separator(get_chr_from_text(&tin, i)))
    i++;
  while( i<end )
  { wint_t c;

    while(i<end && !unicode_separator((c=get_chr_from_text(&tin, i))))
    { if ( Sputcode(c, out) < 0 )
	fail;
      i++;
    }
    while(i<end && unicode_separator(get_chr_from_text(&tin, i)))
      i++;
    if ( i < end )
    { if (  Sputcode(' ', out) < 0 )
	fail;
    }
  }

  succeed;
}


static
PRED_IMPL("normalize_space", 2, normalize_space, 0)
{ redir_context ctx;
  word rc;

  if ( (rc = setupOutputRedirect(A1, &ctx, FALSE)) )
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: on some installations, locale doesn't   work correctly. Printing a
message isn't really cute. It would be better to use printMessage(), but
the system isn't yet initialised far enough.   Maybe we should store the
failure and print a message at the end of the initialisation?

We only return FALSE if LC_CTYPE  fails.   This  is a serious indication
that locale support is broken. We don't   depend too much on the others,
so we ignore possible problems.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
initLocale(void)
{ int rc = TRUE;

  if ( !setlocale(LC_CTYPE, "") )
  { rc = FALSE;
    DEBUG(0, Sdprintf("Failed to set LC_CTYPE locale\n"));
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
	  return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "setlocale");
      }

      succeed;
    }
  }

  return PL_domain_error("category", A1);
}

#else

#define initLocale() 1

static
PRED_IMPL("setlocale", 3, setlocale, 0)
{ return notImplemented("setlocale", 3);
}

#endif


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(ctype)
  PRED_DEF("char_type", 2, char_type, PL_FA_NONDETERMINISTIC)
  PRED_DEF("code_type", 2, code_type, PL_FA_NONDETERMINISTIC)
  PRED_DEF("setlocale", 3, setlocale, 0)
  PRED_DEF("downcase_atom", 2, downcase_atom, 0)
  PRED_DEF("upcase_atom", 2, upcase_atom, 0)
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
   SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC,
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

IOENC
initEncoding(void)
{ GET_LD

  if ( LD )
  { if ( !LD->encoding )
    { char *enc;

      if ( !initLocale() )
      { LD->encoding = ENC_ISO_LATIN_1;
      } else if ( (enc = setlocale(LC_CTYPE, NULL)) )
      { LD->encoding = ENC_ANSI;		/* text encoding */

	if ( (enc = strchr(enc, '.')) )
	{ const enc_map *m;
	  enc++;				/* skip '.' */

	  for ( m=map; m->name; m++ )
	  { if ( strcmp(enc, m->name) == 0 )
	    { LD->encoding = m->encoding;
	      break;
	    }
	  }
	}
      } else
      { LD->encoding = ENC_ISO_LATIN_1;
      }
    }

    return LD->encoding;
  }

  return ENC_ANSI;
}


void
initCharTypes(void)
{ initEncoding();
}

