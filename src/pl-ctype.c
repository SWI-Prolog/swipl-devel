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

#include "pl-incl.h"
#include <ctype.h>
#include "pl-ctype.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines:

	char_type(?Char, ?Type)
	code_type(?Char, ?Type)

See manual for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CHAR_MODE 0
#define CODE_MODE 1

#define CTX_CHAR 0			/* Class(Char) */
#define CTX_INT  1			/* Class(Int) */

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
  int   	do_enum;		/* what to enumerate */
} generator;


static int
iswhite(int chr)
{ return chr == ' ' || chr == '\t';
}


static int
fiscsym(int chr)
{ return isalnum(chr) || chr == '_';
}


static int
fiscsymf(int chr)
{ return isalpha(chr) || chr == '_';
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
{ return strchr(".?!", chr) != NULL;
}

static int
isquote(int chr)
{ return strchr("'`\"", chr) != NULL;
}

static int
fupper(int chr)
{ return islower(chr) ? toupper(chr) : -1;
}

static int
flower(int chr)
{ return isupper(chr) ? tolower(chr) : -1;
}

static int
ftoupper(int chr)
{ return toupper(chr);
}

static int
ftolower(int chr)
{ return tolower(chr);
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
{ if ( isdigit(chr) )
    return chr - '0';
  return -1;
}


static int
rdigit(int d)
{ if ( d >= 0 && d <= 9 )
    return d+'0';
  return -1;
}


static int
fxdigit(int chr)
{ if ( isdigit(chr) )
    return chr - '0';
  if ( chr >= 'a' && chr <= 'f' )
    return chr - 'a' + 10;
  if ( chr >= 'A' && chr <= 'F' )
    return chr - 'A' + 10;
  return -1;
}


static int
rxdigit(int d)
{ if ( d >= 0 && d <= 9 )
    return d+'0';
  if ( d >= 10 && d <= 15 )
    return d-10+'a';
  return -1;
}



#define mkfunction(name) \
	static int f ## name(int chr) { return name(chr); }

mkfunction(isalnum)
mkfunction(isalpha)
mkfunction(isascii)
mkfunction(iscntrl)
mkfunction(isdigit)
mkfunction(isgraph)
mkfunction(islower)
mkfunction(isupper)
mkfunction(ispunct)
mkfunction(isspace)

const static char_type char_types[] =
{ { ATOM_alnum,		fisalnum },
  { ATOM_alpha,		fisalpha },
  { ATOM_csym,		fiscsym },
  { ATOM_csymf,		fiscsymf },
  { ATOM_ascii,		fisascii },
  { ATOM_white,		iswhite },
  { ATOM_cntrl,		fiscntrl },
  { ATOM_digit,		fisdigit },
  { ATOM_graph,		fisgraph },
  { ATOM_lower,		fislower },
  { ATOM_upper,		fisupper },
  { ATOM_punct,		fispunct },
  { ATOM_space,		fisspace },
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
  { ATOM_digit,		fdigit,		rdigit,   1, CTX_INT  },
  { ATOM_xdigit,	fxdigit,	rxdigit,  1, CTX_INT  },
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
{ if ( ct->arity == 0 )
    return PL_unify_atom(type, ct->name);
  else /*if ( ct->arity == 1 )*/
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
do_char_type(term_t chr, term_t class, word h, int how)
{ generator *gen;
  mark m;

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

      if ( !(do_enum & ENUM_CHAR) &&
	   !PL_get_char(chr, &c) )
	fail;
      if ( !(do_enum & ENUM_CLASS) )
      { if ( !PL_get_name_arity(class, &cn, &arity) ||
	     !(cc = char_type_by_name(cn, arity)) )
	  return PL_error("char_type", 2, NULL,
			  ERR_TYPE, ATOM_char_type, class);
      }

      if ( do_enum == ENUM_NONE )
      { if ( arity == 0 )
	  return (*cc->test)(c) ? TRUE : FALSE;
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
	{ if ( PL_get_char(a, &ca) )
	  { int c = (*cc->reverse)(ca);

	    if ( c < 0 )
	      fail;

	    return PL_unify_char(chr, c, how);
	  }
	  fail;				/* error */
	}
      }

      gen = allocHeap(sizeof(*gen));
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
	freeHeap(gen, sizeof(*gen));
    default:
      succeed;
  }

  Mark(m);
  for(;;)
  { int rval;

    if ( (rval = (*gen->class->test)(gen->current)) )
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
	ForeignRedoPtr(gen);
      else
      { freeHeap(gen, sizeof(*gen));	/* the only one */
	succeed;
      }
    }
  next:
    Undo(m);

    if ( !advanceGen(gen) )
      break;
  }

  freeHeap(gen, sizeof(*gen));
  fail;
}



foreign_t
pl_char_type(term_t chr, term_t class, word h)
{ return do_char_type(chr, class, h, CHAR_MODE);
}


foreign_t
pl_code_type(term_t chr, term_t class, word h)
{ return do_char_type(chr, class, h, CODE_MODE);
}

#if defined(HAVE_LOCALE_H) && defined(HAVE_SETLOCALE)
#include <locale.h>

static void
initLocale()
{ if ( !setlocale(LC_CTYPE, "") )	/* this is all we use */
    Sdprintf("Failed to set locale\n");
}

#else

#define initLocale()

#endif


		 /*******************************
		 *	PROLOG CHARACTERS	*
		 *******************************/

char _PL_char_types[] = {
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, 
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
			  /* 128-255 */
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, 
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, 
			  /* 160-255 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC };

void
initCharTypes()
{ int i;

  for(i=128; i<256; i++)
  { if ( islower(i) )
      _PL_char_types[i] = LC;
    else if ( isupper(i) )
      _PL_char_types[i] = UC;
    else if ( ispunct(i) )
      _PL_char_types[i] = SY;
  }

  initLocale();
}


void
systemMode(bool accept)
{ _PL_char_types[(int)'$'] = (accept ? LC : SY);
  if ( accept )
    debugstatus.styleCheck |= DOLLAR_STYLE;
  else
    debugstatus.styleCheck &= ~DOLLAR_STYLE;
}

