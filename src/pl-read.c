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

/*#define O_DEBUG 1*/
#include "pl-read.h"
#include "pl-arith.h"
#include <math.h>
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "os/pl-dtoa.h"
#include "os/pl-prologflag.h"
#include "pl-umap.c"			/* Unicode map */
#include "pl-dict.h"
#include "pl-fli.h"
#include "pl-prims.h"
#include "pl-pro.h"
#include "pl-proc.h"
#include "pl-write.h"
#include "pl-gc.h"
#include "pl-funct.h"
#include "pl-op.h"
#include "pl-modul.h"
#include "pl-setup.h"
#include <errno.h>

typedef const unsigned char * cucharp;
typedef       unsigned char * ucharp;

#define utf8_get_uchar(s, chr) (ucharp)utf8_get_char((char *)(s), chr)

#undef LD
#define LD LOCAL_LD

static void	  addUTF8Buffer(Buffer b, int c);


		 /*******************************
		 *     UNICODE CLASSIFIERS	*
		 *******************************/

#define CharTypeW(c, t, w) \
	((unsigned)(c) <= 0xff ? (_PL_char_types[(unsigned)(c)] t) \
			       : (uflagsW(c) & (w)))

#define PlBlankW(c)	CharTypeW(c, == SP, U_SEPARATOR)
#define PlUpperW(c)	CharTypeW(c, == UC, U_UPPERCASE)
#define PlIdStartW(c)	((unsigned)c <= 0xff ? \
				(isLower(c)||isUpper(c)||c=='_') \
				: uflagsW(c) & U_ID_START)
#define PlIdContW(c)	CharTypeW(c, >= UC, U_ID_CONTINUE)
#define PlSymbolW(c)	CharTypeW(c, == SY, U_SYMBOL)
#define PlDecimalW(c)	CharTypeW(c, == DI, U_DECIMAL)
#define PlPunctW(c)	CharTypeW(c, == PU, 0)
#define PlSoloW(c)	CharTypeW(c, == SO, U_OTHER)
#define PlInvalidW(c)   (uflagsW(c) == 0)

int
f_is_prolog_var_start(int c)
{ return (PlUpperW(c) || c == '_');
}

int
f_is_prolog_atom_start(int c)
{ return PlIdStartW(c) && !((PlUpperW(c) || c == '_'));
}

int
f_is_prolog_identifier_continue(int c)
{ return PlIdContW(c) || c == '_';
}

int
f_is_prolog_symbol(int c)
{ return PlSymbolW(c) != 0;
}

int
f_is_decimal(int c)
{ return PlDecimalW(c) != 0;
}

int
unicode_separator(int c)
{ return PlBlankW(c);
}

int
unicode_quoted_escape(int c)
{ if ( c != ' ' )
  { int uflags = uflagsW(c);

    return !uflags || (uflags&(U_SEPARATOR|U_CONTROL));
  } else
  { return false;
  }
}


int
decimal_weight(int code)
{ if ( code >= '0' && code <= '9' )
  { return code-'0';
  } else
  { const int *s = decimal_bases;
    const int *e = &decimal_bases[sizeof(decimal_bases)/sizeof(decimal_bases[0])];
    const int *m = &s[(e-s)/2];

    for(; e > s; )
    { if ( code < *m )
      { e = (e == m ? e-1 : m);
	m = &s[(e-s)/2];
      } else if ( code > *m+10 )
      { s = (s == m ? s+1 : m);
	m = &s[(e-s)/2];
      } else
      { return code - *m;
      }
    }

    assert(0);
    return -1;
  }
}




/* unquoted_atomW() returns true if text can be written to s as unquoted atom
*/

static int
truePrologFlagNoLD(unsigned int flag)
{ GET_LD

  return truePrologFlag(flag);
}


int
atom_varnameW(const pl_wchar_t *s, size_t len)
{ if ( f_is_prolog_var_start(*s) )
  { for(s++; --len > 0; s++)
    { int c = *s;

      if ( !PlIdContW(c) )
	return false;
    }

    return true;
  }

  return false;
}


/* returns 1: properly named variable
	   0: neutral (_<digit>)
	  -1: anonymous (_<Upper> or __<lower>
*/

int
atom_is_named_var(atom_t name)		/* see warn_singleton() */
{ const char *s;
  const pl_wchar_t *w;

  if ( (s=PL_atom_nchars(name, NULL)) )
  { if ( s[0] != '_' ) return 1;
    if ( s[1] )
    { if ( s[1] == '_' ) return -1;
      if ( isDigitW(s[1]) ) return 0;
      if ( !PlUpperW(s[1]) ) return 1;
    }
  } else if ( (w=PL_atom_wchars(name, NULL)) )
  { if ( w[0] != '_' ) return 1;
    if ( w[1] )
    { if ( w[1] == '_' ) return -1;
      if ( isDigitW(w[1]) ) return 0;
      if ( !PlUpperW(w[1]) ) return 1;
    }
  }

  return -1;
}


static
PRED_IMPL("$is_named_var", 1, is_named_var, 0)
{ PRED_LD
  atom_t name;

  if ( PL_get_atom_ex(A1, &name) )
    return atom_is_named_var(name) == 1;

  return false;
}



		 /*******************************
		 *	   CHAR-CONVERSION	*
		 *******************************/

static int  char_table[257];	/* also map -1 (EOF) */
static int *char_conversion_table = &char_table[1];

void
initCharConversion(void)
{ int i;

  for(i=-1; i< 256; i++)
    char_conversion_table[i] = i;
}


foreign_t
pl_char_conversion(term_t in, term_t out)
{ int cin, cout;

  if ( !PL_get_char(in, &cin, false) ||
       !PL_get_char(out, &cout, false) )
    fail;

  char_conversion_table[cin] = cout;

  succeed;
}


foreign_t
pl_current_char_conversion(term_t in, term_t out, control_t h)
{ GET_LD
  int ctx;
  fid_t fid;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { int cin;

      if ( !PL_is_variable(in) )
      { if ( PL_get_char(in, &cin, false) )
	  return PL_unify_char(out, char_conversion_table[cin], PL_CHAR);
	fail;
      }
      ctx = 0;
      break;
    }
    case FRG_REDO:
      ctx = (int)ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      ctx = 0;				/* for compiler */
      succeed;
  }

  if ( !(fid = PL_open_foreign_frame()) )
    return false;

  for( ; ctx < 256; ctx++)
  { if ( PL_unify_char(in, ctx, PL_CHAR) &&
	 PL_unify_char(out, char_conversion_table[ctx], PL_CHAR) )
      ForeignRedoInt(ctx+1);

    PL_rewind_foreign_frame(fid);
  }

  fail;
}


		 /*******************************
		 *	   TERM-READING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the Prolog parser.  Reading a term takes two passes:

	* Reading the term into memory, deleting multiple blanks, comments
	  etc.
	* Parsing this string into a Prolog term.

The separation has two reasons: we can call the  first  part  separately
and  insert  the  read  strings in the history and we can produce better
error messages as the parsed part of the source is available.

The raw reading pass is quite tricky as PCE requires  us  to  allow  for
callbacks  from  C  during  this  process  and the callback might invoke
another read.  Notable raw reading needs to be studied studied once more
as it  takes  about  30%  of  the  entire  compilation  time  and  looks
promissing  for  optimisations.   It  also  could  be  made  a  bit more
readable.

This module is considerably faster when compiled  with  GCC,  using  the
-finline-functions option.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct variable
{ char *	name;		/* Name of the variable */
  size_t	namelen;	/* length of the name */
  term_t	variable;	/* Term-reference to the variable */
  unsigned int	times;		/* Number of occurences */
  unsigned int	hash_next;	/* Offset for next with same hash */
  word		signature;	/* Pseudo atom */
} *Variable;

typedef struct token
{ int type;			/* type of token */
  int64_t start;		/* start-position */
  int64_t end;			/* end-position */
  union
  { number	number;		/* int or float */
    atom_t	atom;		/* atom value */
    term_t	term;		/* term (list or string) */
    int		character;	/* a punctuation character (T_PUNCTUATION) */
    Variable	variable;	/* a variable record (T_VARIABLE) */
  } value;			/* value of token */
} *Token;


#define FASTBUFFERSIZE	256	/* read quickly upto this size */

struct read_buffer
{ int	size;			/* current size of read buffer */
  unsigned char *base;		/* base of read buffer */
  unsigned char *here;		/* current position in read buffer */
  unsigned char *end;		/* end of the valid buffer */
  IOSTREAM *stream;		/* stream we are reading from */
					/* Buffer its NOT cleared */
  unsigned char fast[FASTBUFFERSIZE];	/* Quick internal buffer */
};


struct var_table
{ tmp_buffer _var_name_buffer;	/* stores the names */
  tmp_buffer _var_buffer;	/* array of struct variables */
  unsigned int _var_hash_size;	/* #buckets */
  unsigned int *_var_buckets;	/* hash table */
};


typedef struct term_stack
{ tmp_buffer terms;		/* Term handles */
  size_t     allocated;		/* #valid terms allocated */
  size_t     top;		/* #valid terms on the stack */
} term_stack;


typedef struct
{ union
  { atom_t atom;		/* Normal operator */
    term_t block;		/* [...] or {...} operator */
  } op;				/* Name of the operator */
  unsigned isblock : 1;		/* [...] or {...} operator */
  unsigned isterm : 1;		/* Union is a term */
  char	kind;			/* kind (OP_PREFIX, ...) */
  unsigned char type;		/* OP_FX, ... */
  short	left_pri;		/* priority at left-hand */
  short	right_pri;		/* priority at right hand */
  short	op_pri;			/* priority of operator */
  term_t tpos;			/* term-position */
  unsigned char *token_start;	/* start of the token for message */
} op_entry;


typedef struct
{ term_t tpos;			/* its term-position */
  int	 pri;			/* priority of the term */
} out_entry;


typedef struct
{ tmp_buffer	out_queue;	/* Queued `out' terms */
  tmp_buffer	side_queue;	/* Operators pushed `aside' */
} op_queues;


#define T_FUNCTOR	0	/* name of a functor (atom, followed by '(') */
#define T_DICT		1	/* name of a dict class (atom, followed by '{') */
#define T_QNAME		2	/* quoted name */
#define T_NAME		3	/* ordinary name */
#define T_VCLASS_DICT	4	/* variable name followed by '{' */
#define T_VARIABLE	5	/* variable name */
#define T_VOID_DICT	6	/* void variable followed by '{' */
#define T_VOID		7	/* void variable */
#define T_NUMBER	8	/* integer or float */
#define T_STRING	9	/* "string" */
#define T_PUNCTUATION  10	/* punctuation character */
#define T_FULLSTOP     11	/* Prolog end of clause */
#define T_QQ_OPEN      12	/* "{|" of {|Syntax||Quotation|} stuff */
#define T_QQ_BAR       13	/* "||" of {|Syntax||Quotation|} stuff */

#define E_SILENT	0	/* Silently fail */
#define E_EXCEPTION	1	/* Generate an exception */
#define E_PRINT		2	/* Print to Serror */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bundle all data required by the  various   passes  of read into a single
structure.  This  makes  read  truly  reentrant,  fixing  problems  with
interrupt and XPCE  call-backs  as   well  as  transparently  supporting
multi-threading.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define RD_MAGIC 0xefebe128

typedef struct
{ unsigned char *here;			/* current character */
  unsigned char *base;			/* base of clause */
  unsigned char *end;			/* end of the clause */
  unsigned char *token_start;		/* start of most recent read token */
  struct token  token;			/* current token */
  int		_unget;			/* unget_token() */
  int		magic;			/* RD_MAGIC */

  source_location start_of_term;	/* Position of start of term */
  unsigned char *posp;			/* position pointer */
  size_t	posi;			/* position number */

  Module	module;			/* Current source module */
  unsigned int	flags;			/* Module syntax flags (M_*) */
  int		styleCheck;		/* style-checking mask */
  int	       *char_conversion_table;	/* active conversion table */

  atom_t	on_error;		/* Handling of syntax errors */
  int		has_exception;		/* exception is raised */

  term_t	exception;		/* raised exception */
  term_t	variables;		/* report variables */
  term_t	varnames;		/* Report variables+names */
  term_t	singles;		/* Report singleton variables */
  term_t	subtpos;		/* Report Subterm positions */
  term_t	comments;		/* Report comments */
#ifdef O_QUASIQUOTATIONS
  term_t	quasi_quotations;	/* User option quasi_quotations(QQ) */
  term_t	qq;			/* Quasi quoted list */
  term_t	qq_tail;		/* Tail of the quoted stuff */
#endif
  bool		cycles;			/* Re-establish cycles */
  bool		dotlists;		/* read .(a,b) as a list */
  int		strictness;		/* Strictness level */

  atom_t	locked;			/* atom that must be unlocked */
					/* NOT ZEROED BELOW HERE (_rb is first) */
  struct read_buffer	_rb;		/* keep read characters here */
  struct var_table	vt;		/* Data about variables */
  struct term_stack	term_stack;	/* Stack for creating output term */
  op_queues		op;		/* Operator handling */
} read_data, *ReadData;

#define	rdhere		  (_PL_rd->here)
#define	rdbase		  (_PL_rd->base)
#define	rdend		  (_PL_rd->end)
#define	last_token_start  (_PL_rd->token_start)
#define	cur_token	  (_PL_rd->token)
#define	unget		  (_PL_rd->_unget)
#define	rb		  (_PL_rd->_rb)

#define var_name_buffer	  (_PL_rd->vt._var_name_buffer)
#define var_buffer	  (_PL_rd->vt._var_buffer)
#define var_hash_size	  (_PL_rd->vt._var_hash_size)
#define var_buckets	  (_PL_rd->vt._var_buckets)

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif

static void	init_term_stack(ReadData _PL_rd);
static void	clear_term_stack(ReadData _PL_rd);


#define init_read_data(_PL_rd, in) LDFUNC(init_read_data, _PL_rd, in)
static void
init_read_data(DECL_LD ReadData _PL_rd, IOSTREAM *in)
{ memset(_PL_rd, 0, offsetof(read_data, _rb.fast));

  initBuffer(&var_name_buffer);
  initBuffer(&var_buffer);
  initBuffer(&_PL_rd->op.out_queue);
  initBuffer(&_PL_rd->op.side_queue);
  var_hash_size = 0;
  init_term_stack(_PL_rd);
  _PL_rd->exception = PL_new_term_ref();
  rb.stream = in;
  _PL_rd->magic = RD_MAGIC;
  _PL_rd->module = MODULE_parse;
  _PL_rd->flags  = _PL_rd->module->flags; /* change for options! */
  _PL_rd->styleCheck = debugstatus.styleCheck;
  _PL_rd->on_error = ATOM_error;
  if ( truePrologFlag(PLFLAG_CHARCONVERSION) )
    _PL_rd->char_conversion_table = char_conversion_table;
  else
    _PL_rd->char_conversion_table = NULL;
}


static void
free_read_data(ReadData _PL_rd)
{ if ( rdbase && rdbase != rb.fast )
    PL_free(rdbase);

  if ( _PL_rd->locked )
    PL_unregister_atom(_PL_rd->locked);

  discardBuffer(&var_name_buffer);
  discardBuffer(&var_buffer);
  discardBuffer(&_PL_rd->op.out_queue);
  discardBuffer(&_PL_rd->op.side_queue);
  if ( var_hash_size )
    PL_free(var_buckets);
  clear_term_stack(_PL_rd);
}


static void
set_module_read_data(ReadData _PL_rd, Module m)
{ _PL_rd->module = m;
  _PL_rd->flags  = _PL_rd->module->flags;
}

#define NeedUnlock(a) need_unlock(a, _PL_rd)
#define Unlock(a) unlock(a, _PL_rd)

static void
need_unlock(atom_t a, ReadData _PL_rd)
{ _PL_rd->locked = a;
}


static void
unlock(atom_t a, ReadData _PL_rd)
{ if (  _PL_rd->locked == a )
  { _PL_rd->locked = 0;
    PL_unregister_atom(a);
  }
}


		/********************************
		*         ERROR HANDLING        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Syntax Error exceptions:

	end_of_clause
	end_of_clause_expected
	end_of_file
	end_of_file_in_quoted(Type)
	end_of_file_in_block_comment
	illegal_number
	long_atom
	long_string
	operator_clash
	operator_expected
	operator_balance
	quoted_punctuation
	list_rest

Error term:

	error(syntax_error(Id), file(Path, Line))
	error(syntax_error(Id), string(String, CharNo))
	error(syntax_error(Id), stream(Stream, Line, CharPos))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define syntaxError(what, rd) \
	do { errorWarning(what, 0, rd); fail; } while(0)
#define syntaxError1(what, arg, rd) \
	do { errorWarningA1(what, arg, 0, rd); fail; } while(0)

const char *
str_number_error(strnumstat rc)
{ switch(rc)
  { case NUM_ERROR:      return "illegal_number";
    case NUM_OK:	 return "no_error";
    case NUM_FUNDERFLOW: return "float_underflow";
    case NUM_FOVERFLOW:  return "float_overflow";
    case NUM_IOVERFLOW:  return "integer_overflow";
    case NUM_CONSTRANGE: return "numeric constant out of range";
  }

  return NULL;
}


extern IOFUNCTIONS Sstringfunctions;

static bool
isStringStream(IOSTREAM *s)
{ return s->functions == &Sstringfunctions;
}


static void
ptr_to_location(const unsigned char *here, source_location *pos, ReadData _PL_rd)
{ unsigned char const *s, *ll = NULL;
  int c;

  *pos = _PL_rd->start_of_term;

						/* update line number */
  s=rdbase;
  while( (s = utf8_get_uchar(s, &c)) < here )
  { pos->position.charno++;

    if ( c == '\n' )
    { pos->position.lineno++;
      ll = s+1;
    }
  }
						/* update line position */
  if ( ll )
  { s = ll;
    pos->position.linepos = 0;
  } else
  { s = rdbase;
  }

  for(; s<here; s++)
  { switch(*s)
    { case '\b':
	if ( pos->position.linepos > 0 )
	  pos->position.linepos--;
      break;
    case '\t':
      pos->position.linepos |= 7;		/* TBD: set tab distance */
    default:
      pos->position.linepos++;
    }
  }

  pos->position.byteno = 0;			/* we do not know */
}



static int
unify_location(term_t loc, const source_location *pos, ReadData _PL_rd)
{ GET_LD
  int rc = true;

  if ( pos->file )				/* reading a file */
  { rc = PL_unify_term(loc,
		       PL_FUNCTOR, FUNCTOR_file4,
			 PL_ATOM,  pos->file,
			 PL_INT,   pos->position.lineno,
			 PL_INT,   pos->position.linepos,
			 PL_INT64, pos->position.charno);
  } else if ( isStringStream(rb.stream) )
  { int64_t charno;

    charno = pos->position.charno - _PL_rd->start_of_term.position.charno;

    rc = PL_unify_term(loc,
		       PL_FUNCTOR, FUNCTOR_string2,
			 PL_UTF8_STRING, rdbase,
			 PL_INT64,       charno);
  } else				/* any stream */
  { term_t stream;

    if ( !(stream=PL_new_term_ref()) ||
	 !PL_unify_stream_or_alias(stream, rb.stream) ||
	 !PL_unify_term(loc,
			PL_FUNCTOR, FUNCTOR_stream4,
			  PL_TERM,  stream,
			  PL_INT,   pos->position.lineno,
			  PL_INT,   pos->position.linepos,
			  PL_INT64, pos->position.charno) )
      rc = false;
  }

  return rc;
}


static int
unify_ptr_location(term_t loc, const unsigned char *here, ReadData _PL_rd)
{ source_location pos;

  ptr_to_location(here, &pos, _PL_rd);

  return unify_location(loc, &pos, _PL_rd);
}


static term_t
makeErrorTerm(const char *id_str, const char *id_arg,
	      term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex, loc=0;			/* keep compiler happy */
  int rc = true;

  if ( !(ex = PL_new_term_ref()) ||
       !(loc = PL_new_term_ref()) )
    rc = false;

  if ( rc && !id_term )
  { if ( (id_term=PL_new_term_ref()) )
    { if ( id_arg )
      { rc = PL_unify_term(id_term,
			   PL_FUNCTOR_CHARS, id_str, 1,
			     PL_CHARS, id_arg);
      } else
      { rc = PL_put_atom_chars(id_term, id_str);
      }
    } else
    { rc = false;
    }
  }

  if ( rc && (rc = PL_unify_term(ex,
				 PL_FUNCTOR, FUNCTOR_error2,
				   PL_FUNCTOR, FUNCTOR_syntax_error1,
				     PL_TERM, id_term,
				   PL_TERM, loc)) )
  { rc = unify_ptr_location(loc, last_token_start, _PL_rd);
  }

  return (rc ? ex : (term_t)0);
}


static bool
errorWarningA1(const char *id_str, const char *id_arg,
	       term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex;

  if ( Sferror(rb.stream) )		/* Stream error; will be reported */
    fail;				/* elsewhere */

  LD->exception.processing = true;	/* allow using spare stack */

  ex = makeErrorTerm(id_str, id_arg, id_term, _PL_rd);

  if ( _PL_rd )
  { _PL_rd->has_exception = true;
    if ( ex )
      PL_put_term(_PL_rd->exception, ex);
    else
      PL_put_term(_PL_rd->exception, exception_term);
  } else
  { if ( ex )
      PL_raise_exception(ex);
  }

  fail;
}


static bool
errorWarning(const char *id_str, term_t id_term, ReadData _PL_rd)
{ return errorWarningA1(id_str, NULL, id_term, _PL_rd);
}


static int
numberError(strnumstat rc, ReadData _PL_rd)
{ return errorWarning(str_number_error(rc), 0, _PL_rd);
}


static int
singletonWarning(term_t term, const char *which, const char **vars, int nvars)
{ GET_LD
  fid_t fid;

  if ( (fid = PL_open_foreign_frame()) )
  { term_t l = PL_new_term_ref();
    term_t a = PL_copy_term_ref(l);
    term_t h = PL_new_term_ref();
    int n, rc = true;

    for(n=0; n<nvars; n++)
    { if ( !(rc=PL_unify_list(a, h, a)) ||
	   !(rc=PL_unify_chars(h, REP_UTF8|PL_ATOM, -1, vars[n])) )
	break;
    }
    rc = ( rc &&
	   PL_unify_nil(a) &&
	   printMessage(ATOM_warning,
			PL_FUNCTOR_CHARS, which, 2,
			  PL_TERM, term,
			  PL_TERM, l) );

    PL_discard_foreign_frame(fid);

    return rc;
  }

  return false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	false	return false
	true	redo
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
reportReadError(ReadData rd)
{ int rc;

  if ( rd->on_error == ATOM_error )
    return PL_raise_exception(rd->exception);
  if ( rd->on_error != ATOM_quiet )
    rc = printMessage(ATOM_error, PL_TERM, rd->exception);
  (void)rc;
  PL_clear_exception();

  if ( rd->on_error == ATOM_dec10 )
    return true;

  return false;
}


		/********************************
		*           RAW READING         *
		*********************************/

static cucharp
backskip_utf8(cucharp s)
{ for(s--; ISUTF8_CB(*s); s--)
    ;

  return s;
}

static unsigned char *
backSkipUTF8(unsigned const char *start, unsigned const char *end, int *chr)
{ const unsigned char *s;

  for(s=end-1 ; s>start && ISUTF8_CB(*s); s--)
    ;
  utf8_get_char((char*)s, chr);

  return (unsigned char *)s;
}


static int
prev_code(unsigned const char *start, unsigned const char *s)
{ int c;

  backSkipUTF8(start, s, &c);
  return c;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the input, give prompts when necessary and return a char *  holding
a  stripped  version of the next term.  Contiguous white space is mapped
on a single space, block and % ... \n comment  is  deleted.   Memory  is
claimed automatically en enlarged if necessary.

(char *) NULL is returned on a syntax error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
clearBuffer(ReadData _PL_rd)
{ if (rb.size == 0)
  { rb.base = rb.fast;
    rb.size = sizeof(rb.fast);
  }
  rb.end = rb.base + rb.size;
  rdbase = rb.here = rb.base;

  _PL_rd->posp = rdbase;
  _PL_rd->posi = 0;
}


static void
growToBuffer(int c, ReadData _PL_rd)
{ if ( rb.base == rb.fast )		/* long clause: jump to use malloc() */
  { rb.base = PL_malloc_atomic(FASTBUFFERSIZE * 2);
    memcpy(rb.base, rb.fast, FASTBUFFERSIZE);
  } else
    rb.base = PL_realloc(rb.base, rb.size*2);

  DEBUG(8, Sdprintf("Reallocated read buffer at %p\n", rb.base));
  _PL_rd->posp = rdbase = rb.base;
  rb.here = rb.base + rb.size;
  rb.size *= 2;
  rb.end  = rb.base + rb.size;
  _PL_rd->posi = 0;

  *rb.here++ = (char)c;
}


static inline void
addByteToBuffer(int c, ReadData _PL_rd)
{ c &= 0xff;

  if ( rb.here >= rb.end )
    growToBuffer(c, _PL_rd);
  else
    *rb.here++ = (char)c;
}


static void
addToBuffer(int c, ReadData _PL_rd)
{ if ( c <= 0x7f )
  { addByteToBuffer(c, _PL_rd);
  } else
  { char buf[10];
    char *s, *e;

    e = utf8_put_char(buf, c);
    for(s=buf; s<e; s++)
      addByteToBuffer(*s, _PL_rd);
  }
}


#define setCurrentSourceLocation(_PL_rd) LDFUNC(setCurrentSourceLocation, _PL_rd)
static void
setCurrentSourceLocation(DECL_LD ReadData _PL_rd)
{ atom_t a;
  IOSTREAM *s = rb.stream;

  if ( (a = fileNameStream(s)) )
    _PL_rd->start_of_term.file = a;
  else
    _PL_rd->start_of_term.file = NULL_ATOM;

  if ( s->position )
  { _PL_rd->start_of_term.position.lineno  = s->position->lineno;
    _PL_rd->start_of_term.position.linepos = s->position->linepos - 1;
    _PL_rd->start_of_term.position.charno  = s->position->charno - 1;
    /* byteno maintained get getchr__() */
  } else
  { _PL_rd->start_of_term.position.lineno  = -1;
    _PL_rd->start_of_term.position.linepos = -1;
    _PL_rd->start_of_term.position.charno  = 0;
    _PL_rd->start_of_term.position.byteno  = 0;
  }

  LD->read_source = _PL_rd->start_of_term;
}


static inline int
getchr__(ReadData _PL_rd)
{ int c;

  if ( rb.stream->position )
    _PL_rd->start_of_term.position.byteno = rb.stream->position->byteno;
  c = Sgetcode(rb.stream);
  if ( !_PL_rd->char_conversion_table || c < 0 || c >= 256 )
    return c;

  return _PL_rd->char_conversion_table[c];
}


#define getchr()  getchr__(_PL_rd)
#define getchrq() Sgetcode(rb.stream)

#define ensure_space(c) { if ( something_read && \
			       (c == '\n' || !isBlank(rb.here[-1])) ) \
			   addToBuffer(c, _PL_rd); \
			}
#define set_start_line { if ( !something_read ) \
			 { setCurrentSourceLocation(_PL_rd); \
			   something_read = true; \
			 } \
		       }

#define rawSyntaxError(what) rawSyntaxError1(what, NULL)
#define rawSyntaxError1(what, arg) \
	{ addToBuffer(EOS, _PL_rd); \
	  rdbase = rb.base, last_token_start = rb.here-1; \
	  syntaxError1(what, arg, _PL_rd); \
	}

static void
setErrorLocation(IOPOS *pos, ReadData _PL_rd)
{ if ( pos )
  { GET_LD

    LD->read_source.position = *pos;
  }
  rb.here = rb.base+1;			/* see rawSyntaxError() */
}

static int
raw_read_quoted(int q, ReadData _PL_rd)
{ IOPOS pbuf;
  IOPOS *pos;
  int c;

  if ( rb.stream->position )
  { pbuf = *rb.stream->position;
    pbuf.charno--;
    pbuf.linepos--;
    pos = &pbuf;
  } else
    pos = NULL;

  addToBuffer(q, _PL_rd);
  while((c=getchrq()) != EOF && c != q)
  {
  next:
    if ( c == '\\' && ison(_PL_rd, M_CHARESCAPE) )
    { int base;

      addToBuffer(c, _PL_rd);

      switch( (c=getchrq()) )
      { case EOF:
	  goto eofinstr;
	case 'u':			/* \uXXXX */
	case 'U':			/* \UXXXXXXXX */
	  addToBuffer(c, _PL_rd);
	  continue;
	case 'x':			/* \xNN\ */
	  addToBuffer(c, _PL_rd);
	  c = getchrq();
	  if ( c == EOF )
	    goto eofinstr;
	  if ( digitValue(16, c) >= 0 )
	  { base = 16;
	    addToBuffer(c, _PL_rd);

	  xdigits:
	    c = getchrq();
	    while( digitValue(base, c) >= 0 )
	    { addToBuffer(c, _PL_rd);
	      c = getchrq();
	    }
	  }
	  if ( c == EOF )
	    goto eofinstr;
	  addToBuffer(c, _PL_rd);
	  if ( c == q )
	    return true;
	  continue;
	case 'c':			/* \c<whitespace>* */
	  addToBuffer(c, _PL_rd);	/* 'c' */
	  c = getchrq();
	  while( PlBlankW(c) )
	  { addToBuffer(c, _PL_rd);
	    c = getchrq();
	  }
	  if ( c == EOF || c == q )
	    goto out;
	  goto next;
	default:
	  addToBuffer(c, _PL_rd);
	  if ( digitValue(8, c) >= 0 )	/* \NNN\ */
	  { base = 8;
	    goto xdigits;
	  } else if ( c == '\n' )	/* \<newline> */
	  { c = getchrq();
	    if ( c == EOF )
	      goto eofinstr;
	    addToBuffer(c, _PL_rd);
	    if ( c == q )
	      return true;
	  }
	  continue;			/* \symbolic-control-char */
      }
    }
    addToBuffer(c, _PL_rd);
  }

out:
  if (c == EOF)
  { char what[2];
  eofinstr:
    if ( Sferror(rb.stream) )
      return false;
    setErrorLocation(pos, _PL_rd);
    what[0] = (char)q;
    what[1] = EOS;
    rawSyntaxError1("end_of_file_in_quoted", what);
  }
  addToBuffer(c, _PL_rd);

  return true;
}


#ifdef O_QUASIQUOTATIONS
static int
raw_read_quasi_quotation(int c, ReadData _PL_rd)
{ addToBuffer(c, _PL_rd);

  while((c=getchrq()) != EOF)
  { addToBuffer(c, _PL_rd);
    if ( c == '}' &&
	 rb.here[-2] == '|' )
      return true;
  }

  rawSyntaxError("end_of_file_in_quasi_quotation");
}
#endif


static int
raw_read_identifier(int c, ReadData _PL_rd)
{ do
  { addToBuffer(c, _PL_rd);
    c = getchr();
  } while( c != EOF && PlIdContW(c) );

  return c;
}


#define add_comment(b, pos, _PL_rd) LDFUNC(add_comment, b, pos, _PL_rd)
static int
add_comment(DECL_LD Buffer b, IOPOS *pos, ReadData _PL_rd)
{ term_t head, str;

  if ( !(head=PL_new_term_ref()) ||
       !(str=PL_new_term_ref()) ||
       !PL_unify_chars(str, PL_STRING|REP_UTF8,
		       entriesBuffer(b, char),
		       baseBuffer(b, char)) )
    return false;

  assert(_PL_rd->comments);
  if ( !PL_unify_list(_PL_rd->comments, head, _PL_rd->comments) )
    return false;
  if ( pos )
  { if ( !PL_unify_term(head,
			PL_FUNCTOR, FUNCTOR_minus2,
			  PL_FUNCTOR, FUNCTOR_dstream_position4,
			    PL_INT64, pos->charno,
			    PL_INT, pos->lineno,
			    PL_INT, pos->linepos,
			    PL_INT, 0,
			  PL_TERM, str) )
      return false;
  } else
  { if ( !PL_unify_term(head,
			PL_FUNCTOR, FUNCTOR_minus2,
			  PL_ATOM, ATOM_minus,
			PL_TERM, str) )
      return false;
  }

  PL_reset_term_refs(head);
  return true;
}


#define raw_read2(_PL_rd) LDFUNC(raw_read2, _PL_rd)
static int
raw_read2(DECL_LD ReadData _PL_rd)
{ int c;
  bool something_read = false;
  IOPOS pbuf;					/* comment start */
  IOPOS *pos;

  clearBuffer(_PL_rd);				/* clear input buffer */
  _PL_rd->strictness = truePrologFlag(PLFLAG_ISO);
  source_line_no = -1;

  for(;;)
  { c = getchr();

  handle_c:
    switch(c)
    { case EOF:
		if ( Sferror(rb.stream) )
		  return false;
		if ( Sfpasteof(rb.stream) )
		{ term_t stream;

		  LD->exception.processing = true;
		  stream = PL_new_term_ref();
		  PL_unify_stream_or_alias(stream, rb.stream);
		  PL_error(NULL, 0, NULL, ERR_PERMISSION,
			   ATOM_input, ATOM_past_end_of_stream, stream);
		  return false;
		}
		if ( something_read )
		{ if ( isStringStream(rb.stream) )
		  { ensure_space(' ');
		    addToBuffer('.', _PL_rd);
		    ensure_space(' ');
		    addToBuffer(EOS, _PL_rd);
		    return true;
		  }
		  rawSyntaxError("end_of_file");
		} else if ( ison(_PL_rd, M_RDSTRING_TERM) )
		{ rawSyntaxError("end_of_string");
		} else
		{ set_start_line;
		  strcpy((char *)rb.base, "end_of_file. ");
		  rb.here = rb.base + 14;
		  return true;
		}
      case '/': if ( rb.stream->position )
		{ pbuf = *rb.stream->position;
		  pbuf.charno--;
		  pbuf.linepos--;
		  pos = &pbuf;
		} else
		  pos = NULL;

		c = getchr();
		if ( c == '*' )
		{ int last;
		  int level = 1;
		  tmp_buffer ctmpbuf;
		  Buffer cbuf;

		  if ( _PL_rd->comments )
		  { initBuffer(&ctmpbuf);
		    cbuf = (Buffer)&ctmpbuf;
		    addUTF8Buffer(cbuf, '/');
		    addUTF8Buffer(cbuf, '*');
		  } else
		  { cbuf = NULL;
		  }

		  if ((last = getchr()) == EOF)
		  { if ( cbuf )
		      discardBuffer(cbuf);
		    setErrorLocation(pos, _PL_rd);
		    if ( Sferror(rb.stream) )
		      return false;
		    rawSyntaxError("end_of_file_in_block_comment");
		  }
		  if ( cbuf )
		    addUTF8Buffer(cbuf, last);

		  if ( something_read )
		  { addToBuffer(' ', _PL_rd);	/* positions */
		    addToBuffer(' ', _PL_rd);
		    addToBuffer(last == '\n' ? last : ' ', _PL_rd);
		  }

		  for(;;)
		  { c = getchr();

		    if ( cbuf )
		      addUTF8Buffer(cbuf, c);

		    switch( c )
		    { case EOF:
			if ( cbuf )
			  discardBuffer(cbuf);
			setErrorLocation(pos, _PL_rd);
			if ( Sferror(rb.stream) )
			  return false;
			rawSyntaxError("end_of_file_in_block_comment");
		      case '*':
			if ( last == '/' )
			  level++;
			break;
		      case '/':
			if ( last == '*' &&
			     (--level == 0 || _PL_rd->strictness) )
			{ if ( cbuf )
			  { if ( !add_comment(cbuf, pos, _PL_rd) )
			    { discardBuffer(cbuf);
			      return false;
			    }
			    discardBuffer(cbuf);
			  }
			  c = ' ';
			  goto handle_c;
			}
			break;
		    }
		    if ( something_read )
		      addToBuffer(c == '\n' ? c : ' ', _PL_rd);
		    last = c;
		  }
		} else
		{ set_start_line;
		  addToBuffer('/', _PL_rd);
		  if ( isSymbolW(c) )
		  { while( c != EOF && isSymbolW(c) &&
			   !(c == '`' && ison(_PL_rd, BQ_MASK)) )
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    }
		  }
		  goto handle_c;
		}
      case '%': if ( something_read )
		  addToBuffer(' ', _PL_rd);
		if ( _PL_rd->comments )
		{ tmp_buffer ctmpbuf;
		  Buffer cbuf;

		  if ( rb.stream->position )
		  { pbuf = *rb.stream->position;
		    pbuf.charno--;
		    pbuf.linepos--;
		    pos = &pbuf;
		  } else
		    pos = NULL;

		  initBuffer(&ctmpbuf);
		  cbuf = (Buffer)&ctmpbuf;
		  addUTF8Buffer(cbuf, '%');

		  for(;;)
		  { while((c=getchr()) != EOF && c != '\n')
		    { addUTF8Buffer(cbuf, c);
		      if ( something_read )		/* record positions */
			addToBuffer(' ', _PL_rd);
		    }
		    if ( c == '\n' )
		    { int c2 = Speekcode(rb.stream);

		      if ( c2 == '%' )
		      { if ( something_read )
			{ addToBuffer(c, _PL_rd);
			  addToBuffer(' ', _PL_rd);
			}
			addUTF8Buffer(cbuf, c);
			c = Sgetcode(rb.stream);
			assert(c==c2);
			addUTF8Buffer(cbuf, c);
			continue;
		      }
		    }
		    break;
		  }
		  if ( !add_comment(cbuf, pos, _PL_rd) )
		  { discardBuffer(cbuf);
		    return false;
		  }
		  discardBuffer(cbuf);
		} else
		{ while((c=getchr()) != EOF && c != '\n')
		  { if ( something_read )		/* record positions */
		      addToBuffer(' ', _PL_rd);
		  }
		}
		goto handle_c;		/* is the newline */
     case '\'': if ( rb.here > rb.base && isDigit(rb.here[-1]) )
		{ cucharp bs = &rb.here[-1];

		  if ( bs > rb.base && isDigit(bs[-1]) )
		    bs--;

		  if ( bs == rb.base || !PlIdContW(prev_code(rb.base, bs)) )
		  { int base;

		    addToBuffer(0, _PL_rd); /* temp add trailing 0 */
		    rb.here--;
		    base = atoi((char*)bs);
		    if ( base <= 36 )
		    { if ( base == 0 )			/* 0'<c> */
		      { addToBuffer(c, _PL_rd);
			{ if ( (c=getchr()) != EOF )
			  { addToBuffer(c, _PL_rd);
			    if ( c == '\\' )		/* 0'\<c> */
			    { if ( (c=getchr()) != EOF )
				addToBuffer(c, _PL_rd);
			    } else if ( c == '\'' )	/* 0'' */
			    { if ( (c=getchr()) != EOF )
			      { if ( c == '\'' )
				  addToBuffer(c, _PL_rd);
				else
				  goto handle_c;
			      }
#ifdef O_QUASIQUOTATIONS
			    } else if ( c == '|' )	/* 0'|| */
			    { if ( (c=getchr()) != EOF )
			      { if ( c == '|' )
				  addToBuffer(c, _PL_rd);
				else
				  goto handle_c;
			      }
#endif
			    }
			    break;
			  }
			  rawSyntaxError("end_of_file");
			}
		      } else
		      { int c2 = Speekcode(rb.stream);

			if ( c2 != EOF )
			{ if ( digitValue(base, c2) >= 0 )
			  { addToBuffer(c, _PL_rd);
			    c = Sgetcode(rb.stream);
			    addToBuffer(c, _PL_rd);
			    break;
			  }
			  goto sqatom;
			}
			rawSyntaxError("end_of_file");
		      }
		    }
		  }
		}

	      sqatom:
		set_start_line;
		if ( !raw_read_quoted(c, _PL_rd) )
		  fail;
		break;
      case '"':	set_start_line;
		if ( !raw_read_quoted(c, _PL_rd) )
		  fail;
		break;
      case '.': addToBuffer(c, _PL_rd);
		set_start_line;
		c = Speekcode(rb.stream);
		if ( isBlankW(c) || c == '%' || c == -1 )
		{ if ( rb.here - rb.base == 1 )
		    rawSyntaxError("end_of_clause");
		  addToBuffer(' ', _PL_rd);
		  addToBuffer(EOS, _PL_rd);
		  return true;
		}
		c = getchr();
		if ( PlSymbolW(c) )
		{ while( c != EOF && PlSymbolW(c) &&
			 !(c == '`' && ison(_PL_rd, BQ_MASK)) )
		  { addToBuffer(c, _PL_rd);
		    c = getchr();
		  }
		}
		goto handle_c;
      case '`': if ( ison(_PL_rd, BQ_MASK) )
		{ set_start_line;
		  if ( !raw_read_quoted(c, _PL_rd) )
		    fail;
		  break;
		}
		/*FALLTHROUGH*/
      default:	if ( (unsigned)c <= 0xff )
		{ switch(_PL_char_types[c])
		  { case SP:
		    blank:
		      do
		      { if ( something_read ) /* positions, \0 --> ' ' */
			  addToBuffer(c ? c : ' ', _PL_rd);
			else
			  ensure_space(c);
			c = getchr();
		      } while( c != EOF && PlBlankW(c) );
		      goto handle_c;
		    case SY:
		    symbol:
		      set_start_line;
		      do
		      { addToBuffer(c, _PL_rd);
			c = getchr();
			if ( c == '`' && ison(_PL_rd, BQ_MASK) )
			  break;
		      } while( c != EOF && PlSymbolW(c) );
		      goto handle_c;
		    case LC:
		    case UC:
		      set_start_line;
		      c = raw_read_identifier(c, _PL_rd);
		      goto handle_c;
		    default:
#ifdef O_QUASIQUOTATIONS		/* detect || from {|Syntax||Quotation|} */
		      if ( c == '|' &&
			   rb.here - rb.base >= 1 &&
			   rb.here[-1] == '|' &&
			   truePrologFlag(PLFLAG_QUASI_QUOTES) )
		      { if ( !raw_read_quasi_quotation(c, _PL_rd) )
			  return false;
			break;
		      }
#endif
		      addToBuffer(c, _PL_rd);
		      set_start_line;
		  }
		} else			/* > 255 */
		{ if ( PlIdStartW(c) )
		  { set_start_line;
		    c = raw_read_identifier(c, _PL_rd);
		    goto handle_c;
		  } else if ( PlBlankW(c) )
		  { goto blank;
		  } else if ( PlSymbolW(c) )
		  { goto symbol;
		  }
		  { addToBuffer(c, _PL_rd);
		    set_start_line;
		  }
		}
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Raw reading returns a string in  UTF-8   notation  of the a Prolog term.
Comment inside the term is  replaced  by   spaces  or  newline to ensure
proper reconstruction of source locations. Comment   before  the term is
skipped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define raw_read(_PL_rd, endp) LDFUNC(raw_read, _PL_rd, endp)
static int
raw_read(DECL_LD ReadData _PL_rd, unsigned char **endp)
{ int rc;

  if ( (rb.stream->flags & SIO_ISATTY) && Sfileno(rb.stream) >= 0 )
  { ttybuf tab;

    PushTty(rb.stream, &tab, TTY_SAVE);		/* make sure tty is sane */
    PopTty(rb.stream, &ttytab, false);
    rc = raw_read2(_PL_rd);
    PopTty(rb.stream, &tab, true);
  } else
  { rc = raw_read2(_PL_rd);
  }

  if ( endp )
    *endp = _PL_rd->_rb.here;

  return rc;
}


		/*********************************
		*        VARIABLE DATABASE       *
		**********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions manipulate the  variable-name  base   for  a  term. When
building the term on the global stack,  variables are represented a term
with tagex() TAG_VAR|STG_RESERVED, which is handled properly by GC.

The buffer `var_buffer' is a  list  of   pointers  to  a  stock of these
variable structures. This list is dynamically expanded if necessary. The
buffer var_name_buffer contains the  actual   strings.  They  are packed
together to avoid memory fragmentation. This   buffer too is reallocated
if necessary. In this  case,  the   pointers  of  the  existing variable
structures are relocated.

Note that the variables are kept  in   a  simple  array. This means that
reading terms with many named variables   result in quadratic behaviour.
Not sure whether it is worth the trouble to use a hash-table here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isVarInfo(w)	(tagex(w) == (TAG_VAR|STG_RESERVED))
#define consVarInfo(i)	((word)(nv)<<LMASK_BITS)|TAG_VAR|STG_RESERVED
#define valVarInfo(w)   ((size_t)((word)(w)>>LMASK_BITS))
#define VAR_INDEX_HASH_OFFSET 1

#define MAX_SINGLETONS 256		/* max singletons _reported_ */

#define FOR_VARS(v) \
	for( Variable v = baseBuffer(&var_buffer, struct variable), \
		    _ev = topBuffer(&var_buffer, struct variable); \
	     v < _ev; \
	     v++ )

#define isAnonVarName(n)       ((n)[0] == '_' && (n)[1] == EOS)
#define isAnonVarNameN(n, l)   ((n)[0] == '_' && (l) == 1)

static char *
save_var_name(const char *name, size_t len, ReadData _PL_rd)
{ char *nb, *ob = baseBuffer(&var_name_buffer, char);
  size_t e = entriesBuffer(&var_name_buffer, char);

  addMultipleBuffer(&var_name_buffer, name, len, char);
  addBuffer(&var_name_buffer, EOS, char);
  if ( (nb = baseBuffer(&var_name_buffer, char)) != ob )
  { ptrdiff_t shift = nb - ob;

    FOR_VARS(v) v->name += shift;
  }

  return baseBuffer(&var_name_buffer, char) + e;
}


static unsigned int
variableHash(Variable var)
{ return MurmurHashAligned2(var->name, strlen(var->name), MURMUR_SEED);
}


static void
linkVariable(Variable var, ReadData _PL_rd)
{ unsigned int key = variableHash(var) % var_hash_size;

  var->hash_next = var_buckets[key];
  var_buckets[key] = valVarInfo(var->signature)+VAR_INDEX_HASH_OFFSET;
}


static int
rehashVariables(ReadData _PL_rd)
{ if ( !var_hash_size )
  { var_hash_size = 32;
    var_buckets = PL_malloc(var_hash_size*sizeof(*var_buckets));
  } else
  { var_hash_size *= 2;
    var_buckets = PL_realloc(var_buckets, var_hash_size*sizeof(*var_buckets));
  }

  if ( var_buckets )
  { memset(var_buckets, 0, var_hash_size*sizeof(*var_buckets));

    FOR_VARS(v) linkVariable(v, _PL_rd);
    return 0;
  } else
    return MEMORY_OVERFLOW;
}

static int
hashVariable(Variable var, ReadData _PL_rd)
{ size_t i = valVarInfo(var->signature);

  if ( i > var_hash_size )
    return rehashVariables(_PL_rd);
  else
  { linkVariable(var, _PL_rd);
    return 0;
  }
}

static inline Variable
var_from_index(size_t i, ReadData _PL_rd)
{ return &baseBuffer(&var_buffer, struct variable)[i];
}

static inline Variable
varInfo(word w, ReadData _PL_rd)
{ if ( isVarInfo(w) )
    return var_from_index(valVarInfo(w), _PL_rd);

  return NULL;
}


static Variable
lookupVariable(const char *name, size_t len, ReadData _PL_rd)
{ Variable var;
  size_t nv;

  if ( !isAnonVarNameN(name, len) )		/* always add _ */
  { if ( var_hash_size )
    { unsigned int key = MurmurHashAligned2(name, len, MURMUR_SEED) % var_hash_size;
      unsigned int vi;

      for(vi = var_buckets[key]; vi; vi=var->hash_next)
      { var = var_from_index(vi-VAR_INDEX_HASH_OFFSET, _PL_rd);

	if ( len == var->namelen && strncmp(name, var->name, len) == 0 )
	{ var->times++;
	  return var;
	}
      }
    } else
    { FOR_VARS(v)
      { if ( len == v->namelen && strncmp(name, v->name, len) == 0 )
	{ v->times++;
	  return v;
	}
      }
    }
  }

  char *sname = save_var_name(name, len, _PL_rd);
  nv = entriesBuffer(&var_buffer, struct variable);
  var = allocFromBuffer(&var_buffer, sizeof(*var));
  var->name      = sname;
  var->namelen   = len;
  var->times     = 1;
  var->variable  = 0;
  var->signature = consVarInfo(nv);
  if ( nv >= 16 )
    hashVariable(var, _PL_rd);

  return var;
}


static int
warn_singleton(const char *name)	/* Name in UTF-8 */
{ if ( name[0] != '_' )			/* not _*: always warn */
    return true;
  if ( name[1] == '_' )			/* __*: never warn */
    return false;
  if ( name[1] )			/* _a: warn */
  { int c;

    utf8_get_char(&name[1], &c);
    if ( isDigitW(c) )
      return false;
    if ( !PlUpperW(c) )
      return true;
  }
  return false;
}


static int
warn_multiton(const char *name)
{ if ( !warn_singleton(name) )
  { if ( name[0] == '_' && name[1] )
    { int c;

      utf8_get_char(&name[1], &c);
      if ( isDigitW(c) )			/* _<digit>: never warn */
	return false;
      if ( !PlUpperW(c) )			/* _<lower>: never warn */
	return false;
    }

    return true;
  }

  return false;
}



/* is_singleton() is true if var is a singleton.  As quasi quotations
   may insert the (named) variable into the quotation, we must scan
   the quasi quotation list and check that the variable does not appear
   in any of the terms.
*/

#define IS_SINGLETON    0
#define LIST_SINGLETONS 1
#define IS_MULTITON     2

#define is_singleton(var, type, _PL_rd) LDFUNC(is_singleton, var, type, _PL_rd)
static int
is_singleton(DECL_LD Variable var, int type, ReadData _PL_rd)
{ if ( var->times == 1 )
  { if ( (type == IS_SINGLETON    && warn_singleton(var->name)) ||
	 (type == LIST_SINGLETONS) ||
	 (type == IS_MULTITON     && warn_multiton(var->name)) )
    {
#ifdef O_QUASIQUOTATIONS
      if ( _PL_rd->qq )
      { term_t tail = PL_copy_term_ref(_PL_rd->qq);
	term_t head = PL_new_term_ref();
	term_t result = PL_new_term_ref();

	while(PL_get_list(tail, head, tail))
	{ if ( PL_get_arg(4, head, result) &&
	       PL_var_occurs_in(var->variable, result) )
	  { var->times++;			/* avoid a second scan */
	    break;
	  }
	}
      }
    }
#endif
  }

  if ( type == IS_SINGLETON )
    return var->times == 1 && warn_singleton(var->name);
  else if ( type == LIST_SINGLETONS )
    return var->times == 1;
  else
    return var->times  > 1 && warn_multiton(var->name);
}


#define check_singletons(term, _PL_rd) LDFUNC(check_singletons, term, _PL_rd)
static bool				/* TBD: new schema */
check_singletons(DECL_LD term_t term, ReadData _PL_rd)
{ if ( _PL_rd->singles != true )	/* returns <name> = var bindings */
  { term_t list = PL_copy_term_ref(_PL_rd->singles);
    term_t head = PL_new_term_ref();

    FOR_VARS(var)
    { if ( is_singleton(var, LIST_SINGLETONS, _PL_rd) )
      {	if ( !PL_unify_list(list, head, list) ||
	     !PL_unify_term(head,
			    PL_FUNCTOR,    FUNCTOR_equals2,
			    PL_UTF8_CHARS, var->name,
			    PL_TERM,       var->variable) )
	  return false;
      }
    }

    return PL_unify_nil(list);
  } else				/* just report */
  { const char *singletons[MAX_SINGLETONS];
    int i = 0;

					/* singletons */
    FOR_VARS(var)
    { if ( is_singleton(var, IS_SINGLETON, _PL_rd) )
      { if ( i < MAX_SINGLETONS )
	  singletons[i++] = var->name;
      }
    }

    if ( i > 0 )
    { if ( !singletonWarning(term, "singletons", singletons, i) )
	return false;
    }

    if ( (_PL_rd->styleCheck&MULTITON_CHECK) )
    { i = 0;				/* multiple _X* */
      FOR_VARS(var)
      { if ( is_singleton(var, IS_MULTITON, _PL_rd) )
	{ if ( i < MAX_SINGLETONS )
	    singletons[i++] = var->name;
	}
      }

      if ( i > 0 )
      { if ( !singletonWarning(term, "multitons", singletons, i) )
	  return false;
      }
    }

    succeed;
  }
}


#define bind_variable_names(_PL_rd) LDFUNC(bind_variable_names, _PL_rd)
static bool
bind_variable_names(DECL_LD ReadData _PL_rd)
{ term_t list = PL_copy_term_ref(_PL_rd->varnames);
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();

  FOR_VARS(var)
  { if ( !isAnonVarName(var->name) )
    { PL_chars_t txt;
      int rc;

      txt.text.t    = var->name;
      txt.length    = strlen(var->name);
      txt.storage   = PL_CHARS_HEAP;
      txt.encoding  = ENC_UTF8;
      txt.canonical = false;

      rc = ( PL_unify_list(list, head, list) &&
	     PL_unify_functor(head, FUNCTOR_equals2) &&
	     PL_get_arg(1, head, a) &&
	     PL_unify_text(a, 0, &txt, PL_ATOM) &&
	     PL_get_arg(2, head, a) &&
	     PL_unify(a, var->variable) );

      PL_free_text(&txt);

      if ( !rc )
	return false;
    }
  }

  return PL_unify_nil(list);
}


#define bind_variables(_PL_rd) LDFUNC(bind_variables, _PL_rd)
static bool
bind_variables(DECL_LD ReadData _PL_rd)
{ term_t list = PL_copy_term_ref(_PL_rd->variables);
  term_t head = PL_new_term_ref();

  FOR_VARS(var)
  { if ( !PL_unify_list(list, head, list) ||
	 !PL_unify(head, var->variable) )
      return false;
  }

  return PL_unify_nil(list);
}


#ifdef O_QUASIQUOTATIONS
/** '$qq_open'(+QQRange, -Stream) is det.

Opens a quasi-quoted memory range.

@arg QQRange is a term '$quasi_quotation'(ReadData, Start, Length)
@arg Stream  is a UTF-8 encoded string, whose position indication
	     reflects the location in the real file.
*/

static
PRED_IMPL("$qq_open", 2, qq_open, 0)
{ PRED_LD

  if ( PL_is_functor(A1, FUNCTOR_dquasi_quotation3) )
  { void *ptr;
    size_t start, len;
    term_t arg = PL_new_term_ref();

    if ( PL_get_arg(1, A1, arg) && PL_get_pointer_ex(arg, &ptr) &&
	 PL_get_arg(2, A1, arg) && PL_get_size_ex(arg, &start) &&
	 PL_get_arg(3, A1, arg) && PL_get_size_ex(arg, &len) )
    { ReadData _PL_rd = ptr;

      if ( _PL_rd->magic == RD_MAGIC )
      { char *s_start = (char*)rdbase+start;
	IOSTREAM *s;

	if ( (s=Sopenmem(&s_start, &len, "r")) )
	{ source_location pos;

	  s->encoding = ENC_UTF8;
	  ptr_to_location((unsigned char*)s_start, &pos, _PL_rd);
	  if ( pos.file )
	    setFileNameStream(s, pos.file);
	  if ( pos.position.lineno > 0 )
	  { s->position = &s->posbuf;
	    *s->position = pos.position;
	  }

	  return PL_unify_stream(A2, s);
	}
      } else
	PL_existence_error("read_context", A1);
    }
  } else
    PL_type_error("read_context", A1);

  return false;
}


#define parse_quasi_quotations(_PL_rd) LDFUNC(parse_quasi_quotations, _PL_rd)
static int
parse_quasi_quotations(DECL_LD ReadData _PL_rd)
{ if ( _PL_rd->qq_tail )
  { term_t av;
    int rc;

    if ( !PL_unify_nil(_PL_rd->qq_tail) )
      return false;

    if ( !_PL_rd->quasi_quotations )
    { if ( (av = PL_new_term_refs(2)) &&
	   PL_put_term(av+0, _PL_rd->qq) &&
	   PL_put_atom(av+1, _PL_rd->module->name) &&
	   PL_cons_functor_v(av, FUNCTOR_dparse_quasi_quotations2, av) )
      { term_t ex;

	rc = callProlog(MODULE_system, av+0, PL_Q_CATCH_EXCEPTION, &ex);
	if ( rc )
	  return true;
	if ( ex )
	{ PL_put_term(_PL_rd->exception, ex);
	  _PL_rd->has_exception = true;
	}
      }
      return false;
    } else
      return true;
  } else if ( _PL_rd->quasi_quotations )	/* user option, but no quotes */
  { return PL_unify_nil(_PL_rd->quasi_quotations);
  } else
    return true;
}


static int
is_quasi_quotation_syntax(term_t type, ReadData _PL_rd)
{ GET_LD
  term_t plain = PL_new_term_ref();
  term_t ex;
  Module m = _PL_rd->module;
  atom_t name;
  size_t arity;

  if ( !PL_strip_module(type, &m, plain) )
    return false;

  if ( PL_get_name_arity(plain, &name, &arity) )
  { if ( _PL_rd->quasi_quotations )
    { return true;
    } else
    { Procedure proc;

      if ( (proc=resolveProcedure(PL_new_functor(name, 4), m)) &&
	   ison(proc->definition, P_QUASI_QUOTATION_SYNTAX) )
	return true;

      if ( (ex = PL_new_term_ref()) &&
	   PL_unify_term(ex,
			 PL_FUNCTOR_CHARS, "unknown_quasi_quotation_syntax", 2,
			   PL_TERM, type,
			   PL_ATOM, m->name) )
	return errorWarning(NULL, ex, _PL_rd);
    }
  } else
  { if ( (ex = PL_new_term_ref()) &&
	 PL_unify_term(ex, PL_FUNCTOR_CHARS, "invalid_quasi_quotation_syntax", 1,
		       PL_TERM, type) )
      return errorWarning(NULL, ex, _PL_rd);
  }

  return false;
}

#endif /*O_QUASIQUOTATIONS*/


		/********************************
		*           TOKENISER           *
		*********************************/

const char *
utf8_skip_blanks(const char *in)
{ char *s;

  for( ; *in; in=s)
  { int chr;

    s = utf8_get_char(in, &chr);

    if ( !PlBlankW(chr) )
      return in;
  }

  return in;
}


static inline ucharp
skipSpaces(cucharp in)
{ return (ucharp)utf8_skip_blanks((const char*)in);
}


static inline unsigned char *
SkipVarIdCont(unsigned char *in)
{ int chr;
  unsigned char *s;

  for( ; *in; in=s)
  { s = (unsigned char*)utf8_get_char((char*)in, &chr);

    if ( !PlIdContW(chr) )
      return in;
  }

  return in;
}


static inline unsigned char *
SkipAtomIdCont(unsigned char *in)
{ int chr;
  unsigned char *s;

  for( ; *in; in=s)
  { s = (unsigned char*)utf8_get_char((char*)in, &chr);

    if ( !PlIdContW(chr) )
    { if ( chr == '.' && truePrologFlagNoLD(PLFLAG_DOT_IN_ATOM) )
      { s = (unsigned char*)utf8_get_char((char*)s, &chr);
	if ( PlIdContW(chr) )
	  continue;
      }
      return in;
    }
  }

  return in;
}


static unsigned char *
SkipSymbol(unsigned char *in, ReadData _PL_rd)
{ int chr;
  unsigned char *s;

  for( ; *in; in=s)
  { s = (unsigned char*)utf8_get_char((char*)in, &chr);

    if ( chr == '`' && isoff(_PL_rd, BQ_MASK) )
      continue;				/* ` is a symbol char */
    if ( !PlSymbolW(chr) )
      return in;
  }

  return in;
}


#define unget_token()	{ unget = true; }


/* skip_digit_separator() skips a digit separator as defined by Ulrich
   Neumerkel in the SWI-Prolog mailinglist.  That is, for numbers that
   only include digits, this is _<blank>* or ' '; while for for other
   numbers (e.g., hexadecimal), it is only _<blank>*.
*/

static int
skip_digit_separator(cucharp *sp, int base, int *grouped)
{ cucharp s = *sp;

  if ( *s == '_' )
    s = skipSpaces(s+1);
  else if ( *s == ' ' && base <= 10 )
    s++;

  if ( digitValue(base, *s) >= 0 )
  { *sp = s;
    if ( grouped )
      *grouped = true;
    return true;
  }

  return false;
}


static int
skip_decimal_separator(cucharp *sp, int zero, int *grouped)
{ cucharp s = *sp;
  int c;

  if ( *s == '_' )
    s = skipSpaces(s+1);
  else if ( *s == ' ' )
    s++;

  utf8_get_uchar(s, &c);
  if ( isDecimal(zero, c) )
  { *sp = s;
    if ( grouped )
      *grouped = true;
    return true;
  }

  return false;
}


static strnumstat
scan_decimal(cucharp *sp, int zero, int negative, Number n, int *grouped)
{ int64_t maxi = PLMAXINT/10;
  int maxlastdigit = PLMAXINT % 10;
  int64_t mini = PLMININT/10;
  int minlastdigit = PLMININT % 10;
  int64_t t = 0;
  cucharp s = *sp, sn;
  int c;

  utf8_get_uchar(s, &c);
  if ( !isDecimal(zero, c) )
    return NUM_ERROR;

  *grouped = false;

  do
  { for(sn = utf8_get_uchar(s, &c); isDecimal(zero, c); sn = utf8_get_uchar(s, &c))
    { if (    (  negative && ( (t < mini) || (t == mini && zero - c < minlastdigit) ))
	   || ( !negative && ( (t > maxi) || (t == maxi && c - zero > maxlastdigit) )) )
      {
#ifdef O_BIGNUM
	n->value.i = t;
	n->type = V_INTEGER;
	promoteToMPZNumber(n);

	do
	{ for(sn = utf8_get_uchar(s, &c); isDecimal(zero, c); sn = utf8_get_uchar(s, &c))
	  { s = sn;
	    mpz_mul_ui(n->value.mpz, n->value.mpz, 10);
	    if (negative)
	      mpz_sub_ui(n->value.mpz, n->value.mpz, c - zero);
	    else
	      mpz_add_ui(n->value.mpz, n->value.mpz, c - zero);
	  }
	} while ( skip_decimal_separator(&s, zero, grouped) );

	*sp = s;

	return NUM_OK;
#else
	double maxf =  MAXREAL / 10.0 - 10.0;
	double minf = -MAXREAL / 10.0 + 10.0;
	double tf = (double)t;
	do
	{ for(sn = utf8_get_uchar(s, &c); isDecimal(zero, c); sn = utf8_get_uchar(s, &c))
	  { s = sn;
	    if (negative)
	    { if ( tf < minf )
		fail;				/* number too large */
	      tf = tf * 10.0 - (double)(c - zero);
	    } else
	    { if ( tf > maxf )
		fail;				/* number too large */
	      tf = tf * 10.0 + (double)(c - zero);
	    }
	  }
	} while ( skip_decimal_separator(&s, zero, grouped) );
	n->value.f = tf;
	n->type = V_FLOAT;
	*sp = s;
	return NUM_OK;
#endif
      } else
      { s = sn;

	if (negative)
	  t = t * 10 - (c - zero);
	else
	  t = t * 10 + (c - zero);
      }
    }
  } while ( skip_decimal_separator(&s, zero, grouped) );

  *sp = s;

  n->value.i = t;
  n->type = V_INTEGER;
  return NUM_OK;
}


static strnumstat
scan_number(cucharp *s, int negative, int b, Number n)
{ int d;
  int64_t maxi = PLMAXINT/b;		/* cache? */
  int maxlastdigit = (int)(PLMAXINT % b);
  int64_t mini = PLMININT/b;
  int minlastdigit = (int)(PLMININT % b);
  int64_t t = 0;
  cucharp q = *s;

  if ( (d = digitValue(b, *q)) < 0 )
    return NUM_ERROR;			/* syntax error */

  do
  { while((d = digitValue(b, *q)) >= 0)
    { if (    (  negative && ( (t < mini) || (t == mini && d > minlastdigit) ))
	   || ( !negative && ( (t > maxi) || (t == maxi && d > maxlastdigit) )) )
      {
#ifdef O_BIGNUM
	n->value.i = t;
	n->type = V_INTEGER;
	promoteToMPZNumber(n);

	do
	{ while((d = digitValue(b, *q)) >= 0)
	  { q++;
	    mpz_mul_ui(n->value.mpz, n->value.mpz, b);
	    if (negative)
	      mpz_sub_ui(n->value.mpz, n->value.mpz, d);
	    else
	      mpz_add_ui(n->value.mpz, n->value.mpz, d);
	  }
	} while ( skip_digit_separator(&q, b, NULL) );

	*s = q;

	return NUM_OK;
#else

	double maxf =  MAXREAL / (double) b - (double) b;
	double minf = -MAXREAL / (double) b + (double) b;
	double tf = (double)t;
	do
	{ while((d = digitValue(b, *q)) >= 0)
	  { q++;
	    if (negative)
	    { if ( tf < minf )
		fail;				/* number too large */
	      tf = tf * (double)b - (double)d;
	    } else
	    { if ( tf > maxf )
		fail;				/* number too large */
	      tf = tf * (double)b + (double)d;
	    }
	  }
	} while ( skip_digit_separator(&q, b, NULL) );
	n->value.f = tf;
	n->type = V_FLOAT;
	*s = q;
	return NUM_OK;
#endif
      } else
      { q++;
	if (negative)
	  t = t * b - d;
	else
	  t = t * b + d;
      }
    }
  } while ( skip_digit_separator(&q, b, NULL) );

  n->value.i = t;
  n->type = V_INTEGER;
  *s = q;
  return NUM_OK;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
escape_char() decodes a \<chr> specification that can appear either in a
quoted atom/string or as a  character   specification  such as 0'\n. The
return value is either a valid character   code,  ESC_EOS if there is no
more data or ESC_ERROR if there is an error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ESC_EOS	  (-1)
#define ESC_ERROR (-2)

static int
escape_char(cucharp in, ucharp *end, int quote, ReadData _PL_rd)
{ int base;
  int chr;
  int c;
  cucharp e;

#define OK(v) do { chr = (v); goto ok; } while(0)

again:
  in = utf8_get_uchar(in, &c);
  switch(c)
  { case 'a':
      OK(7);				/* 7 is ASCII BELL */
    case 'b':
      OK('\b');
    case 'c':				/* skip \c<blank>* */
      if ( _PL_rd )
      { in = skipSpaces(in);
	e = utf8_get_uchar(in, &c);
      skip_cont:
	in = e;
	if ( c == '\\' )
	  goto again;
	if ( c == quote )		/* \c ' --> no output */
	  OK(ESC_EOS);
	OK(c);
      }
      OK('c');
    case '\r':				/* \\\r\n is the same as \\\n */
    { int c2;
      cucharp in2 = utf8_get_uchar(in, &c2);
      if ( c2 == '\n' )
      { c = c2;
	in = in2;
      }
    }
    /*FALLTHROUGH*/
    case '\n':				/* \LF<blank>* */
      if ( _PL_rd )			/* quoted string, _not_ 0'\.. */
      { if ( !_PL_rd->strictness )
	{ unsigned char *errpos = (unsigned char *)in;
	  int skipped = 0;
	  e = in;
	  for( ; *in; in=e )
	  { e = utf8_get_uchar(in, &c);
	    if ( c == '\n' || !PlBlankW(c) )
	    { if ( skipped )
	      { term_t ex;
		unsigned char *old_start = last_token_start;

		last_token_start = errpos;
		ex = makeErrorTerm("swi_backslash_newline", NULL, 0, _PL_rd);
		last_token_start = old_start;
		if ( !ex || !printMessage(ATOM_warning, PL_TERM, ex) )
		  return ESC_ERROR;
	      }
	      break;
	    }
	    skipped++;
	  }
	} else
	{ e = utf8_get_uchar(in, &c);
	}
	goto skip_cont;
      }
      OK('\n');
    case 'e':
      OK(27);				/* 27 is ESC (\e is a gcc extension) */
    case 'f':
      OK('\f');
    case '\\':
    case '\'':
    case '"':
    case '`':
      OK(c);
    case 'n':
      OK('\n');
    case 'r':
      OK('\r');
    case 't':
      OK('\t');
    case 's':				/* \s is space (NU-Prolog, Quintus) */
      OK(' ');
    case 'v':
      OK(11);				/* 11 is ASCII Vertical Tab */
    case 'u':				/* \uXXXX */
    case 'U':				/* \UXXXXXXXX */
    { int digits = (c == 'u' ? 4 : 8);
      cucharp errpos = in-1;
      chr = 0;

      while(digits-- > 0)
      { int dv;

	c = *in++;
	if ( (dv=digitValue(16, c)) >= 0 )
	{ chr = (chr<<4)+dv;
	} else
	{ if ( _PL_rd )
	  { last_token_start = (unsigned char*)errpos;
	    errorWarning("Illegal \\u or \\U sequence", 0, _PL_rd);
	  }
	  return ESC_ERROR;
	}
      }
      if ( !VALID_CODE_POINT(chr) )
      { if ( _PL_rd )
	{ last_token_start = (unsigned char*)errpos;
	  errorWarning("Illegal character code", 0, _PL_rd);
	}
	return ESC_ERROR;
      }
      OK(chr);
    }
    case 'x':
      c = *in++;
      if ( digitValue(16, c) >= 0 )
      { base = 16;
	goto numchar;
      } else
      { in -= 2;
	c = *in++;
	goto undef;
      }
    default:
      if ( c >= '0' && c <= '7' )	/* octal number */
      { int dv;
	cucharp errpos;

	base = 8;

      numchar:
	errpos = in-1;
	chr = digitValue(base, c);
	c = *in++;
	while( (dv = digitValue(base, c)) >= 0 )
	{ chr = chr * base + dv;
	  c = *in++;
	  if ( !VALID_CODE_POINT(chr) )
	  { if ( _PL_rd )
	    { last_token_start = (unsigned char*)errpos;
	      errorWarning("Illegal character code", 0, _PL_rd);
	    }
	    return ESC_ERROR;
	  }
	}
	if ( c != '\\' )
	  in--;
	OK(chr);
      } else if ( c == quote )
      { OK(c);
      } else
      { undef:
	if ( _PL_rd )
	{ char tmp[2];

	  tmp[0] = (char)c;
	  tmp[1] = EOS;
	  last_token_start = (unsigned char*)(in-1);
	  errorWarningA1("undefined_char_escape", tmp, 0, _PL_rd);
	}
	return ESC_ERROR;
      }
  }

#undef OK

ok:
  if ( end )
    *end = (ucharp)in;
  return chr;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_string() fills the argument buffer with  a UTF-8 representation of a
quoted string.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addUTF8Buffer(Buffer b, int c)
{ if ( c >= 0x80 )
  { char buf[6];
    char *p, *end;

    end = utf8_put_char(buf, c);
    for(p=buf; p<end; p++)
    { addBuffer(b, *p, char);
    }
  } else
  { addBuffer(b, (char)c, char);
  }
}


static int
get_string(unsigned char *in, unsigned char *ein, unsigned char **end, Buffer buf,
	   ReadData _PL_rd)
{ int quote;
  int c;

  quote = *in++;

  for(;;)
  { c = *in++;

  next:
    if ( c == quote )
    { if ( *in == quote )
      { in++;
      } else
	break;
    } else if ( c == '\\' && ison(_PL_rd, M_CHARESCAPE) )
    { c = escape_char(in, &in, quote, _PL_rd);
      if ( c >= 0 )
      { addUTF8Buffer(buf, c);

	continue;
      } else if ( c == ESC_ERROR )
      { return false;
      } else
      { break;
      }
    } else if ( c >= 0x80 )		/* copy UTF-8 sequence */
    { do
      { addBuffer(buf, (char)c, char);
	c = *in++;
      } while( c > 0x80 );

      goto next;
    } else if ( in > ein )
    { errorWarning("end_of_file_in_string", 0, _PL_rd);
      return false;
    }

    addBuffer(buf, (char)c, char);
  }

  if ( end )
    *end = in;

  return true;
}


#ifdef O_QUASIQUOTATIONS
static int
get_quasi_quotation(term_t t, unsigned char **here, unsigned char *ein,
		    ReadData _PL_rd)
{ unsigned char *in, *start = *here;

  for(in=start; in <= ein; in++)
  { if ( in[0] == '}' &&
	 in[-1] == '|' )
    { *here = in+1;			/* after } */
      in--;				/* Before | */

      if ( _PL_rd->quasi_quotations )	/* option; must return strings */
      { PL_chars_t txt;
	int rc;

	txt.text.t    = (char*)start;
	txt.length    = in-start;
	txt.storage   = PL_CHARS_HEAP;
	txt.encoding  = ENC_UTF8;
	txt.canonical = false;

	rc = PL_unify_text(t, 0, &txt, PL_CODE_LIST);
	PL_free_text(&txt);

	return rc;
      } else
      { GET_LD

	return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dquasi_quotation3,
				  PL_POINTER, _PL_rd,
				  PL_INTPTR, (intptr_t)(start-rdbase),
				  PL_INTPTR, (intptr_t)(in-start));
      }
    }
  }

  return errorWarning("end_of_file_in_quasi_quotation", 0, _PL_rd);
}
#endif /*O_QUASIQUOTATIONS*/


static cucharp
float_tag(cucharp in, cucharp tag)
{ while(*in && *in == *tag)
    in++, tag++;

  if ( !*tag )
  { int c;

    if ( *in )
    { utf8_get_uchar(in, &c);
      if ( PlIdContW(c) )
	return NULL;
    }

    return in;
  }

  return NULL;
}


static int
starts_1dot(cucharp s)
{ if ( s[0] == '-' )
    s++;
  return (s[0] == '1' && s[1] == '.');
}


static int
points_at_decimal(cucharp in, int zero)
{ int c;

  utf8_get_uchar(in, &c);

  return isDecimal(zero, c);
}


static cucharp
skip_decimals(cucharp in, int zero)
{ int c;
  cucharp n;

  for(n=utf8_get_uchar(in, &c); isDecimal(zero, c); n=utf8_get_uchar(in, &c))
    in = n;

  return in;
}


static strnumstat
special_float(cucharp *in, cucharp start, Number value)
{ cucharp s;

  if ( (s=float_tag(*in, (cucharp)"Inf")) )
  { if ( *start == '-' )
      value->value.f = -HUGE_VAL;
    else
      value->value.f = HUGE_VAL;
  } else if ( (s=float_tag(*in, (cucharp)"NaN")) &&
	      starts_1dot(start) )
  { char *e;
    double f = strtod((char*)start, &e);

    if ( e == (char*)(*in) )
    { strnumstat rc = make_nan(&f);
      if ( rc != NUM_OK )
	return rc;
      value->value.f = f;
    } else
      return NUM_CONSTRANGE;
  } else
    return NUM_ERROR;

  *in = s;
  return NUM_OK;
}

static int
ascii_to_double(cucharp s, cucharp e, double *dp)
{ char *es;
  double d;

  errno = 0;
  d = strtod((char*)s, &es);
  if ( (cucharp)es == e || (e[0] == '.' && e+1 == (cucharp)es) )
  { if ( errno == ERANGE )
    { GET_LD

      if ( fabs(d) > 1.0 )
      { if ( !(LD->arith.f.flags & FLT_OVERFLOW) )
	  return NUM_FOVERFLOW;
      } else
      { if ( !(LD->arith.f.flags & FLT_UNDERFLOW) )
	  return NUM_FOVERFLOW;
      }
    }
    *dp = d;
    return NUM_OK;
  }

  return NUM_ERROR;
}

static int
to_double(cucharp s, cucharp e, int zero, double *dp)
{ if ( zero == '0' )
  { return ascii_to_double(s, e, dp);
  } else
  { tmp_buffer b;
    int rc;

    initBuffer(&b);
    for(; s<e; )
    { int c;

      s = utf8_get_uchar(s, &c);
      if ( c >= zero )
      { assert(c <= zero+9);
	addBuffer(&b, (char)(c-zero+'0'), char);
      } else
      { assert(c <= 127);
	addBuffer(&b, (char)c, char);
      }
    }
    addBuffer(&b, 0, char);

    rc = ascii_to_double(baseBuffer(&b, unsigned char),
			 topBuffer(&b, unsigned char)-1, dp);
    discardBuffer(&b);
    return rc;
  }
}



strnumstat
str_number(cucharp in, ucharp *end, Number value, int flags)
{ int negative = false;
  cucharp start = in;
  strnumstat rc;
  int grouped;

  if ( *in == '-' )			/* skip optional sign */
  { negative = true;
    in++;
  } else if ( *in == '+' )
    in++;

  if ( *in == '0' )
  { int base = 0;

    switch(in[1])
    { case '\'':			/* 0'<char> */
      { int chr;

	if ( (flags&M_CHARESCAPE) && in[2] == '\\' )	/* 0'\n, etc */
	{ chr = escape_char(in+3, end, '\'', NULL);
	  if ( chr < 0 )
	    return NUM_ERROR;
	} else
	{ *end = utf8_get_uchar(in+2, &chr);
	  if ( chr == '\'' && **end == '\'' ) /* handle 0''' as 0'' */
	    (*end)++;
	}

	value->value.i = (int64_t)chr;
	if ( negative )			/* -0'a is a bit dubious! */
	  value->value.i = -value->value.i;
	value->type = V_INTEGER;

	succeed;
      }
      case 'b':
	base = 2;
	break;
      case 'x':
	base = 16;
	break;
      case 'o':
	base = 8;
	break;
    }

    if ( base )				/* 0b<binary>, 0x<hex>, 0o<oct> */
    { in += 2;

      if ( (rc = scan_number(&in, negative, base, value)) != NUM_OK )
	return rc;
      *end = (ucharp)in;

      return NUM_OK;
    }
  }

  int c0;
  int zero = '0';
  utf8_get_uchar(in, &c0);
  if ( PlDecimalW(c0) )
    zero = c0-decimal_weight(c0);

  if ( (rc=scan_decimal(&in, zero, negative, value, &grouped)) != NUM_OK )
    return rc;				/* too large? */

#ifdef O_BIGNUM
  if ( ((*in == '/' && (flags&RAT_NATURAL)) ||
	 *in == 'r') &&
       points_at_decimal(in+1, zero) )
  { number num, den;

    in++;
    if ( (rc=scan_decimal(&in, zero, false, &den, &grouped)) != NUM_OK )
    { clearNumber(value);
      return rc;			/* too large? */
    }
    if ( den.type == V_INTEGER && den.value.i == 0 )
    { clearNumber(value);		/* n/0 */
      return NUM_ERROR;
    }

    cpNumber(&num, value);
    promoteToMPZNumber(&num);
    promoteToMPZNumber(&den);
    clearNumber(value);
    ar_rdiv_mpz(&num, &den, value);
    clearNumber(&num);
    clearNumber(&den);
    *end = (ucharp)in;
    return NUM_OK;
  }
#endif

  if ( grouped )
  { *end = (ucharp)in;
    return NUM_OK;
  }
					/* base'value number */
  if ( *in == '\'' &&
       zero == '0' &&
       value->type == V_INTEGER &&
       value->value.i <= 36 &&
       value->value.i > 1 &&
       digitValue((int)value->value.i, in[1]) >= 0 )
  { in++;

    if ( !(rc=scan_number(&in, negative, (int)value->value.i, value)) )
      return rc;			/* number too large */

    *end = (ucharp)in;

    return NUM_OK;
  }

					/* floating point numbers */
  if ( *in == '.' && points_at_decimal(in+1, zero) )
  { clearNumber(value);
    value->type = V_FLOAT;

    in = skip_decimals(in+1, zero);

    if ( (rc = special_float(&in, start, value)) != NUM_ERROR)
    { if ( rc == NUM_OK )
	*end = (ucharp)in;
      return rc;
    }
  }

  if ( (*in == 'e' || *in == 'E') &&
       ((isSign(in[1]) && points_at_decimal(in+2, zero)) || points_at_decimal(in+1, zero)) )
  { clearNumber(value);
    value->type = V_FLOAT;

    in++;
    if ( isSign(*in) )
      in++;
    in = skip_decimals(in, zero);
  }

  if ( value->type == V_FLOAT )
  { if ( (rc=to_double(start, in, zero, &value->value.f)) != NUM_OK )
      return rc;

    *end = (ucharp)in;

    return NUM_OK;
  }

  *end = (ucharp)in;

  return NUM_OK;
}


static int
checkASCII(unsigned char *name, size_t len, const char *type)
{ size_t i;

  for(i=0; i<len; i++)
  { if ( (name[i]) >= 128 )
    { return printMessage(ATOM_warning,
			  PL_FUNCTOR_CHARS, "non_ascii", 2,
			    PL_NCHARS, len, (char const *)name,
			    PL_CHARS, type);
    }
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ptr_to_pos(p, context) gets the code index of   a pointer in the buffer,
considering the fact that the buffer is  in UTF-8. It remembers the last
returned value, so it doesn't have to start all over again each time.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
ptr_to_pos(const unsigned char *p, ReadData _PL_rd)
{ size_t i;

  if ( p == NULL || p < _PL_rd->posp )
  { _PL_rd->posp = rdbase;
    _PL_rd->posi = 0;
  }

  i = utf8_strlen((const char*) _PL_rd->posp, p-_PL_rd->posp);
  _PL_rd->posp = (unsigned char*)p;
  _PL_rd->posi += i;

  return _PL_rd->posi;
}


#define get_token(must_be_op, _PL_rd) LDFUNC(get_token, must_be_op, _PL_rd)
static Token
get_token(DECL_LD bool must_be_op, ReadData _PL_rd)
{ int c;
  unsigned char *start;

  if ( unget )
  { unget = false;
    return &cur_token;
  }

  rdhere = skipSpaces(rdhere);
  start = last_token_start = rdhere;
  cur_token.start = source_char_no + ptr_to_pos(last_token_start, _PL_rd);
					/* TBD: quadratic due to ptr_to_pos()? */

  rdhere = (unsigned char*)utf8_get_char((char *)rdhere, &c);
  if ( c > 0xff )
  { if ( PlIdStartW(c) )
    { if ( PlUpperW(c) )
	goto upper;
      goto lower;
    }
    if ( PlSymbolW(c) )
      goto case_symbol;
    if ( PlDecimalW(c) )
      goto case_digit;
    if ( PlInvalidW(c) )
      syntaxError("illegal_character", _PL_rd);
    goto case_solo;
  }

  switch(_PL_char_types[c])
  { case LC:
    lower:
		{ PL_chars_t txt;

		  rdhere = SkipAtomIdCont(rdhere);
		symbol:
		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		  { if ( !checkASCII(start, rdhere-start, "atom") )
		      return false;
		  }

		functor:
		  txt.text.t    = (char *)start;
		  txt.length    = rdhere-start;
		  txt.storage   = PL_CHARS_HEAP;
		  txt.encoding  = ENC_UTF8;
		  txt.canonical = false;
		  cur_token.value.atom = textToAtom(&txt);
		  NeedUnlock(cur_token.value.atom);
		  PL_free_text(&txt);

		  if ( *rdhere == '(' )
		  { cur_token.type = T_FUNCTOR;
		  } else if ( *rdhere == '{' )
		  { cur_token.type = T_DICT;
		  } else
		  { cur_token.type = T_NAME;
		  }

		  DEBUG(MSG_READ_TOKEN,
			Sdprintf("%s: %s\n",
				 cur_token.type == T_FUNCTOR ? "FUNC" : "NAME",
				 stringAtom(cur_token.value.atom)));

		  break;
		}
    case UC:
    upper:
		if ( c != '_' && ison(_PL_rd, M_VARPREFIX) )
		  goto lower;

		{ rdhere = SkipVarIdCont(rdhere);
		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		  { if ( !checkASCII(start, rdhere-start, "variable") )
		      return false;
		  }
		  if ( *rdhere == '(' && truePrologFlag(ALLOW_VARNAME_FUNCTOR) )
		    goto functor;
		  if ( start[0] == '_' &&
		       rdhere == start + 1 &&
		       !_PL_rd->variables ) /* report them */
		  { DEBUG(MSG_READ_TOKEN, Sdprintf("VOID\n"));
		    if ( *rdhere == '{' )
		      cur_token.type = T_VOID_DICT;
		    else
		      cur_token.type = T_VOID;
		  } else
		  { cur_token.value.variable = lookupVariable((char *)start,
							      rdhere-start,
							      _PL_rd);
		    DEBUG(MSG_READ_TOKEN,
			  Sdprintf("VAR: %s\n",
				   cur_token.value.variable->name));
		    if ( *rdhere == '{' )
		      cur_token.type = T_VCLASS_DICT;
		    else
		      cur_token.type = T_VARIABLE;
		  }

		  break;
		}
    case_digit:
    case DI:	{ number value;
		  strnumstat rc;

		  if ( (rc=str_number(backskip_utf8(rdhere), &rdhere, &value,
				      _PL_rd->flags)) == NUM_OK )
		  { cur_token.value.number = value;
		    cur_token.type = T_NUMBER;
		    break;
		  } else
		  { numberError(rc, _PL_rd);
		    return NULL;
		  }
		}
    case_solo:
    case SO:	{ cur_token.value.atom = codeToAtom(c);		/* not registered */
		  cur_token.type = (*rdhere == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(MSG_READ_TOKEN,
			Sdprintf("%s: %s\n",
				 *rdhere == '(' ? "FUNC" : "NAME",
				 stringAtom(cur_token.value.atom)));

		  break;
		}
    case_symbol:
    case SY:	if ( c == '`' && ison(_PL_rd, BQ_MASK) )
		  goto case_bq;

		rdhere = SkipSymbol(rdhere, _PL_rd);
		if ( rdhere == start+1 )
		{ if ( c == '-' &&			/* -number */
		       !must_be_op &&
		       isDigit(*rdhere) )
		  { goto case_digit;
		  }
		  if ( c == '.' && *rdhere )
		  { int c2;

		    utf8_get_uchar(rdhere, &c2);
		    if ( PlBlankW(c2) )			/* .<blank> */
		    { cur_token.type = T_FULLSTOP;
		      break;
		    }
		  }
		}

		goto symbol;
    case PU:	{ switch(c)
		  { case '{':
#ifdef O_QUASIQUOTATIONS
		      if ( rdhere[0] == '|' &&
			   truePrologFlag(PLFLAG_QUASI_QUOTES) )
		      { rdhere++;
			cur_token.type = T_QQ_OPEN;
			goto out;
		      }
		    /*FALLTHROUGH*/
#endif
		    case '[':
		      rdhere = skipSpaces(rdhere);
		      if (rdhere[0] == matchingBracket(c))
		      { rdhere++;
			switch(c)
			{ case '{': cur_token.value.atom = ATOM_curl; break;
			  case '[': cur_token.value.atom = ATOM_nil;  break;
			}
			cur_token.type = rdhere[0] == '(' ? T_FUNCTOR : T_NAME;
			DEBUG(MSG_READ_TOKEN,
			      Sdprintf("NAME: %s\n",
				       stringAtom(cur_token.value.atom)));
			goto out;
		      }
#ifdef O_QUASIQUOTATIONS
		    case '|':
		      if ( rdhere[0] == '|' &&
			   truePrologFlag(PLFLAG_QUASI_QUOTES) )
		      { rdhere++;
			cur_token.type = T_QQ_BAR;
			goto out;
		      }
#endif
		  }
		  cur_token.value.character = c;
		  cur_token.type = T_PUNCTUATION;
		  DEBUG(MSG_READ_TOKEN,
			Sdprintf("PUNCT: %c\n", cur_token.value.character));

		  break;
		}
    case SQ:	{ tmp_buffer b;
		  PL_chars_t txt;

		  initBuffer(&b);
		  if ( !get_string(rdhere-1, rdend, &rdhere, (Buffer)&b, _PL_rd) )
		    fail;
		  txt.text.t    = baseBuffer(&b, char);
		  txt.length    = entriesBuffer(&b, char);
		  txt.storage   = PL_CHARS_HEAP;
		  txt.encoding  = ENC_UTF8;
		  txt.canonical = false;
		  cur_token.value.atom = textToAtom(&txt);
		  NeedUnlock(cur_token.value.atom);
		  PL_free_text(&txt);
		  if ( rdhere[0] == '(' )
		    cur_token.type = T_FUNCTOR;
		  else if ( rdhere[0] == '{' )
		    cur_token.type = T_DICT;
		  else
		    cur_token.type = T_QNAME;
		  discardBuffer(&b);
		  break;
		}
    case DQ:	{ tmp_buffer b;
		  term_t t = PL_new_term_ref();
		  PL_chars_t txt;
		  int type;

		  initBuffer(&b);
		  if ( !get_string(rdhere-1, rdend, &rdhere, (Buffer)&b, _PL_rd) )
		    fail;
		  txt.text.t    = baseBuffer(&b, char);
		  txt.length    = entriesBuffer(&b, char);
		  txt.storage   = PL_CHARS_HEAP;
		  txt.encoding  = ENC_UTF8;
		  txt.canonical = false;
#if O_STRING
		  if ( ison(_PL_rd, DBLQ_STRING) )
		    type = PL_STRING;
		  else
#endif
		  if ( ison(_PL_rd, DBLQ_ATOM) )
		    type = PL_ATOM;
		  else if ( ison(_PL_rd, DBLQ_CHARS) )
		    type = PL_CHAR_LIST;
		  else
		    type = PL_CODE_LIST;

		  if ( !PL_unify_text(t, 0, &txt, type) )
		  { PL_free_text(&txt);
		    return false;
		  }
		  PL_free_text(&txt);
		  cur_token.value.term = t;
		  cur_token.type = T_STRING;
		  discardBuffer(&b);
		  break;
		}
    case BQ:
    case_bq:    if ( ison(_PL_rd, BQ_MASK) )
		{ tmp_buffer b;
		  term_t t = PL_new_term_ref();
		  PL_chars_t txt;
		  int type;

		  type = ( ison(_PL_rd, BQ_STRING) ? PL_STRING :
			   ison(_PL_rd, BQ_CODES)  ? PL_CODE_LIST :
			   PL_CHAR_LIST
			 );

		  initBuffer(&b);
		  if ( !get_string(rdhere-1, rdend, &rdhere, (Buffer)&b, _PL_rd) )
		    fail;
		  txt.text.t    = baseBuffer(&b, char);
		  txt.length    = entriesBuffer(&b, char);
		  txt.storage   = PL_CHARS_HEAP;
		  txt.encoding  = ENC_UTF8;
		  txt.canonical = false;
		  if ( !PL_unify_text(t, 0, &txt, type) )
		  { PL_free_text(&txt);
		    return false;
		  }
		  PL_free_text(&txt);
		  cur_token.value.term = t;
		  cur_token.type = T_STRING;
		  discardBuffer(&b);
		  break;
		} else
		{ goto case_symbol;
		}
    case CT:
		syntaxError("illegal_character", _PL_rd);
    default:
		{ sysError("read/1: tokeniser internal error");
		  break;		/* make lint happy */
		}
  }

out:
  cur_token.end = source_char_no + ptr_to_pos(rdhere, _PL_rd);

  return &cur_token;
}



		 /*******************************
		 *	     TERM STACK		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alloc_term() allocates a new term in the

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_term_stack(ReadData _PL_rd)
{ initBuffer(&_PL_rd->term_stack.terms);
  _PL_rd->term_stack.allocated = 0;
  _PL_rd->term_stack.top = 0;
}


static void
clear_term_stack(ReadData _PL_rd)
{ discardBuffer(&_PL_rd->term_stack.terms);
}


#define alloc_term(_PL_rd) LDFUNC(alloc_term, _PL_rd)
static term_t
alloc_term(DECL_LD ReadData _PL_rd)
{ term_stack *ts = &_PL_rd->term_stack;
  term_t t;

  if ( ts->top < ts->allocated )
  { t = baseBuffer(&ts->terms, term_t)[ts->top++];
    PL_put_variable(t);
  } else
  { t = PL_new_term_ref();
    addBuffer(&ts->terms, t, term_t);
    ts->top = ++ts->allocated;
  }
  return t;
}


/* Called as e.g. term_av(-2, _PL_rd) to get the top-most 2 terms
*/

static inline term_t *
term_av(int n, ReadData _PL_rd)
{ term_stack *ts = &_PL_rd->term_stack;

  return &baseBuffer(&ts->terms, term_t)[ts->top+n];
}


static inline void
truncate_term_stack(term_t *top, ReadData _PL_rd)
{ term_stack *ts = &_PL_rd->term_stack;

  ts->top = top - baseBuffer(&ts->terms, term_t);
}


		 /*******************************
		 *	   TERM-BUILDING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build a term on the global stack,  given   the  atom of the functor, the
arity and a vector of  arguments.   The  argument vector either contains
nonvar terms or a variable block. The latter looks like an atom (to make
a possible invocation of the   garbage-collector or stack-shifter happy)
and is recognised by  the  value   of  its  character-pointer,  which is
statically allocated and thus unique.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define setHandle(h, w)		(*valTermRef(h) = (w))

#define readValHandle(term, argp, _PL_rd) LDFUNC(readValHandle, term, argp, _PL_rd)
static inline void
readValHandle(DECL_LD term_t term, Word argp, ReadData _PL_rd)
{ word w = *valTermRef(term);
  Variable var;

  if ( (var = varInfo(w, _PL_rd)) )
  { DEBUG(9, Sdprintf("readValHandle(): var at 0x%x\n", var));

    if ( !var->variable )		/* new variable */
    { var->variable = PL_new_term_ref_noshift();
      assert(var->variable);
      setVar(*argp);
      DEBUG(0, assert(argp < (Word)lBase));
      *valTermRef(var->variable) = makeRefG(argp);
    } else				/* reference to existing var */
    { *argp = *valTermRef(var->variable);
    }
  } else
    *argp = w;				/* plain value */

  setVar(*valTermRef(term));
}


#define ensureSpaceForTermRefs(n) LDFUNC(ensureSpaceForTermRefs, n)
static inline bool
ensureSpaceForTermRefs(DECL_LD size_t n)
{ return ensureLocalSpace(n*sizeof(word));
}


/* build_term(name, arity) builds a term from the top arity terms
   on the term-stack and pushes the result back to the term-stack
*/

#define build_term(atom, arity, _PL_rd) LDFUNC(build_term, atom, arity, _PL_rd)
static int
build_term(DECL_LD atom_t atom, int arity, ReadData _PL_rd)
{ functor_t functor = lookupFunctorDef(atom, arity);
  word w;
  Word argp;
  int rc;

  if ( !hasGlobalSpace(arity+1) &&
       (rc=ensureGlobalSpace(arity+1, ALLOW_GC|ALLOW_SHIFT)) != true )
    return raiseStackOverflow(rc);
  if ( (rc=ensureSpaceForTermRefs(arity)) != true )
    return rc;

  DEBUG(8, Sdprintf("Building term %s/%d ... ", stringAtom(atom), arity));
  argp = gTop;
  w = consPtr(argp, TAG_COMPOUND|STG_GLOBAL);
  gTop += 1+arity;
  *argp++ = functor;

  if ( arity > 0 )
  { term_t *av, *argv = term_av(-arity, _PL_rd);

    for(av=argv; arity-- > 0; av++, argp++)
      readValHandle(*av, argp, _PL_rd);

    setHandle(argv[0], w);
    truncate_term_stack(&argv[1], _PL_rd);
    DEBUG(8, Sdprintf("result: ");
	     PL_write_term(Serror, argv[0], 1200, PL_WRT_QUOTED|PL_WRT_NEWLINE));
  } else
  { term_t t = alloc_term(_PL_rd);

    setHandle(t, w);
    DEBUG(8, Sdprintf("result: ");
	     PL_write_term(Serror, t, 1200, PL_WRT_QUOTED|PL_WRT_NEWLINE));
  }

  return true;
}


/* build_dict(int pairs, ...) builds a dict from the data on the stack.
   and pushes the result back to the term-stack. The stack first
   contains:

	tag, key1, value1, key2, value2, ...
*/

#define build_dict(pairs, _PL_rd) LDFUNC(build_dict, pairs, _PL_rd)
static bool
build_dict(DECL_LD int pairs, ReadData _PL_rd)
{ int arity = pairs*2+1;
  term_t *argv = term_av(-arity, _PL_rd);
  word w;
  Word argp;
  int i;
  bool rc;
  int index_buf[64];
  int *indexes = index_buf;

  if ( pairs > 64 )
  { if ( !(indexes = malloc(sizeof(int)*pairs)) )
      return PL_no_memory();
  }
  for(i=0; i<pairs; i++)
    indexes[i] = i;

  if ( (i=dict_order_term_refs(argv+1, indexes, pairs)) )
  { term_t ex = PL_new_term_ref();

    rc = ( PL_unify_term(ex,
			 PL_FUNCTOR, FUNCTOR_duplicate_key1,
			   PL_TERM, argv[indexes[i]*2+1]) &&
	   errorWarningA1("duplicate_key", NULL, ex, _PL_rd)
	 );

    if ( indexes != index_buf )
      free(indexes);

    return rc;
  }

  if ( !hasGlobalSpace(pairs*2+2) &&
       !ensureGlobalSpace(pairs*2+2, ALLOW_GC|ALLOW_SHIFT) )
    return false;
  if ( !ensureSpaceForTermRefs(arity) )
    return false;

  DEBUG(9, Sdprintf("Building dict with %d pairs ... ", pairs));
  argp = gTop;
  w = consPtr(argp, TAG_COMPOUND|STG_GLOBAL);
  gTop += pairs*2+2;
  *argp++ = dict_functor(pairs);
  readValHandle(argv[0], argp++, _PL_rd); /* the tag */

  for(i=0; i<pairs; i++)
  { readValHandle(argv[indexes[i]*2+2], argp++, _PL_rd); /* value */
    readValHandle(argv[indexes[i]*2+1], argp++, _PL_rd); /* key */
  }

  setHandle(argv[0], w);
  truncate_term_stack(&argv[1], _PL_rd);

  if ( indexes != index_buf )
    free(indexes);

  return true;
}


		 /*******************************
		 *	  OPERATOR QUEUES	*
		 *******************************/

/* The side-queue is for pushing operators to the side the cannot yet
   be reduced
*/

static inline void
queue_side_op(op_entry *new, ReadData _PL_rd)
{ addBuffer(&_PL_rd->op.side_queue, *new, op_entry);
}

static inline op_entry *
side_op(int i, ReadData _PL_rd)
{ return &baseBuffer(&_PL_rd->op.side_queue, op_entry)[i];
}

static inline void
pop_side_op(ReadData _PL_rd)
{ _PL_rd->op.side_queue.top -= sizeof(op_entry);
}

static inline int
side_p0(ReadData _PL_rd)
{ return (int)entriesBuffer(&_PL_rd->op.side_queue, op_entry) - 1;
}

/* The out-queue is used for pushing operands.  If an operator can be
   reduced, it pops 1 or 2 arguments from the out-queue and pushes the
   result.  Note that the terms themselves live on the term-stack.
*/

static void
queue_out_op(short pri, term_t tpos, ReadData _PL_rd)
{ out_entry e;

  e.tpos = tpos;
  e.pri  = pri;

  addBuffer(&_PL_rd->op.out_queue, e, out_entry);
}

static inline out_entry *
out_op(int i, ReadData _PL_rd)
{ return &((out_entry*)_PL_rd->op.out_queue.top)[i];
}

static inline void
pop_out_op(ReadData _PL_rd)
{ _PL_rd->op.out_queue.top -= sizeof(out_entry);
}

#define PopOut() \
	pop_out_op(_PL_rd); \
	cstate.out_n--;

#define get_int_arg(t, n) LDFUNC(get_int_arg, t, n)
static sword
get_int_arg(DECL_LD term_t t, int n)
{ Word p = valTermRef(t);

  deRef(p);

  return valInt(argTerm(*p, n-1));
}


#define opPos(op, args) LDFUNC(opPos, op, args)
static term_t
opPos(DECL_LD op_entry *op, out_entry *args)
{ if ( op->tpos )
  { sword fs = get_int_arg(op->tpos, 1);
    sword fe = get_int_arg(op->tpos, 2);
    term_t r;

    if ( !(r=PL_new_term_ref()) )
      return 0;

    if ( op->kind == OP_INFIX )
    { sword s = get_int_arg(args[0].tpos, 1);
      sword e = get_int_arg(args[1].tpos, 2);

      if ( !op->isblock )
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_SWORD, s,
			    PL_SWORD, e,
			    PL_SWORD, fs,
			    PL_SWORD, fe,
			    PL_LIST, 2, PL_TERM, args[0].tpos,
					PL_TERM, args[1].tpos) )
	  return (term_t)0;
      } else
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_SWORD, s,
			    PL_SWORD, e,
			    PL_INT, 0,
			    PL_INT, 0,
			    PL_LIST, 3, PL_TERM, op->tpos,
					PL_TERM, args[0].tpos,
					PL_TERM, args[1].tpos) )
	  return (term_t)0;
      }
    } else
    { sword s, e;

      if ( op->kind == OP_PREFIX )
      { s = fs;
	e = get_int_arg(args[0].tpos, 2);
      } else
      { s = get_int_arg(args[0].tpos, 1);
	e = fe;
      }

      if ( !op->isblock )
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_SWORD, s,
			    PL_SWORD, e,
			    PL_SWORD, fs,
			    PL_SWORD, fe,
			      PL_LIST, 1, PL_TERM, args[0].tpos) )
	  return (term_t)0;
      } else
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_SWORD, s,
			    PL_SWORD, e,
			    PL_INT, 0,
			    PL_INT, 0,
			      PL_LIST, 2, PL_TERM, op->tpos,
					  PL_TERM, args[0].tpos) )
	  return (term_t)0;
      }
    }

    return r;
  }

  return 0;
}


#define op_name(e) LDFUNC(op_name, e)
static inline atom_t
op_name(DECL_LD op_entry *e)
{ if ( !e->isterm )
  { return e->op.atom;
  } else
  { atom_t name;

    if ( PL_get_name_arity(e->op.block, &name, NULL) )
    { if ( name == ATOM_dot )
	name = ATOM_nil;
    } else
    { assert(0);
      name = ATOM_nil;
    }

    return name;
  }
}


#define build_op_term(op, _PL_rd) LDFUNC(build_op_term, op, _PL_rd)
static int
build_op_term(DECL_LD op_entry *op, ReadData _PL_rd)
{ term_t tmp;
  out_entry *e;
  int arity = (op->kind == OP_INFIX ? 2 : 1);

  if ( !(tmp = PL_new_term_ref()) )
    return false;

  e = out_op(-arity, _PL_rd);
  if ( !op->isblock )
  { if ( !build_term(op->op.atom, arity, _PL_rd) )
      return false;
  } else
  { term_t term = alloc_term(_PL_rd);
    term_t *av = term_av(-(arity+1), _PL_rd);
    int i;

    for(i=arity; i>0; i--)
      av[i] = av[i-1];
    av[0] = term;
    PL_put_term(term, op->op.block);

    if ( !build_term(op_name(op), arity+1, _PL_rd) )
      return false;
  }

  e->pri = op->op_pri;
  if ( op->tpos && !(e->tpos = opPos(op, e)) )
    return false;

  _PL_rd->op.out_queue.top = (char*)(e+1);

  return true;
}


		/********************************
		*             PARSER            *
		*********************************/

#define priorityClash { syntaxError("operator_clash"); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This part of the parser actually constructs  the  term.   It  calls  the
tokeniser  to  find  the next token and assumes the tokeniser implements
one-token pushback efficiently.  It consists  of  two  mutual  recursive
functions:  complex_term()  which is involved with operator handling and
simple_term() which reads everything, except for operators.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define simple_term(token, positions, _PL_rd) LDFUNC(simple_term, token, positions, _PL_rd)
static int simple_term(DECL_LD Token token, term_t positions, ReadData _PL_rd);

typedef struct cterm_state
{ ReadData	rd;			/* Read global data */
  int		out_n;			/* entries in out queue */
  int		side_n;			/* entries in side queue */
  int		side_p;			/* top (index) of side queue */
  int		rmo;			/* Operands more than operators */
} cterm_state;

#define isOp(e, kind, _PL_rd) LDFUNC(isOp, e, kind, _PL_rd)
static bool
isOp(DECL_LD op_entry *e, unsigned char kind, ReadData _PL_rd)
{ short pri;
  unsigned char type;

  if ( !currentOperator(_PL_rd->module, op_name(e), kind, &type, &pri) )
    fail;
  e->type   = type;
  e->kind   = kind;
  e->op_pri = pri;

  switch(type)
  { case OP_FX:		e->left_pri = 0;     e->right_pri = pri-1; break;
    case OP_FY:		e->left_pri = 0;     e->right_pri = pri;   break;
    case OP_XF:		e->left_pri = pri-1; e->right_pri = 0;     break;
    case OP_YF:		e->left_pri = pri;   e->right_pri = 0;     break;
    case OP_XFX:	e->left_pri = pri-1; e->right_pri = pri-1; break;
    case OP_XFY:	e->left_pri = pri-1; e->right_pri = pri;   break;
    case OP_YFX:	e->left_pri = pri;   e->right_pri = pri-1; break;
  }

  succeed;
}


#define stringOp(e) \
	stringAtom(op_name(e))
#define PushOp() \
	queue_side_op(&in_op, _PL_rd); \
	cstate.side_n++, cstate.side_p++;
#define PopOp(cstate) \
	pop_side_op((cstate)->rd); \
	(cstate)->side_n--, (cstate)->side_p--;
#define SideOp(i) \
	side_op(i, _PL_rd)

#define modify_op(cstate, cpri) LDFUNC(modify_op, cstate, cpri)
static int
modify_op(DECL_LD cterm_state *cstate, int cpri)
{ ReadData _PL_rd = cstate->rd;

  if ( cstate->side_n > 0 && cstate->rmo == 0 &&
       cpri > SideOp(cstate->side_p)->right_pri )
  { op_entry *op = SideOp(cstate->side_p);
    if ( op->kind == OP_PREFIX )
    { term_t tmp;

      DEBUG(MSG_READ_OP, Sdprintf("Prefix %s to atom", stringOp(op)));
      cstate->rmo++;
      if ( !(tmp = alloc_term(_PL_rd)) )
	return false;
      if ( op->isblock )
	PL_put_term(tmp, op->op.block);
      else
	PL_put_atom(tmp, op->op.atom);
      queue_out_op(0, op->tpos, _PL_rd);
      cstate->out_n++;
      PopOp(cstate);
    } else if ( op->kind == OP_INFIX && cstate->out_n > 0 &&
		isOp(op, OP_POSTFIX, _PL_rd) )
    { DEBUG(MSG_READ_OP, Sdprintf("Infix %s to postfix\n",
				  stringOp(op)));
      cstate->rmo++;
      if ( !build_op_term(op, _PL_rd) )
	return false;
      PopOp(cstate);
    }
  }

  return true;
}


static int
bad_operator(out_entry *out, op_entry *op, ReadData _PL_rd)
{ GET_LD
  char *opname = stringOp(op);

  last_token_start = op->token_start;

  switch(op->kind)
  { case OP_INFIX:
      if ( op->left_pri < out[0].pri )
      { /*t = out[0].term;*/
	;
      } else
      { last_token_start += strlen(opname);
	/*t = out[1].term;*/
      }
      break;
    case OP_PREFIX:
      last_token_start += strlen(opname);
      /*FALL THROUGH*/
    default:
      /*t = out[0].term;*/
      ;
  }

/* might use this to improve the error message!?
  if ( PL_get_name_arity(t, &name, &arity) )
  { Ssnprintf(buf, sizeof(buf), "Operator `%s' conflicts with `%s'",
	      opname, stringAtom(name));
  }
*/

  syntaxError("operator_clash", _PL_rd);
}


/* can_reduce() returns

	true  if operator can be reduced;
	false if operator can not be reduced;
	-1    if attempting is a syntax error
*/

static int
can_reduce(op_entry *op, short cpri, int out_n, ReadData _PL_rd)
{ int rc;
  int arity = op->kind == OP_INFIX ? 2 : 1;
  out_entry *e = out_op(-arity, _PL_rd);

  if ( arity <= out_n )
  { switch(op->kind)
    { case OP_PREFIX:
	rc = op->right_pri >= e[0].pri;
	break;
      case OP_POSTFIX:
	rc = op->left_pri >= e[0].pri;
	break;
      case OP_INFIX:
	rc = op->left_pri  >= e[0].pri &&
	     op->right_pri >= e[1].pri;
	break;
      default:
	assert(0);
	rc = false;
    }
  } else
    return false;

  if ( rc == false && (cpri) == (OP_MAXPRIORITY+1) )
  { bad_operator(e, op, _PL_rd);
    return -1;
  }

  DEBUG(MSG_READ_OP,
	if ( rc )
	{ GET_LD
	  Sdprintf("Reducing %s/%d\n", stringOp(op), arity);
	});

  return rc;
}


#define reduce_one_op(cstate, op, side)		\
  LDFUNC(reduce_one_op, cstate, op, side)

typedef enum
{ REDUCE_LEFT,
  REDUCE_RIGHT
} reduce_side;

static bool
must_reduce(const op_entry *sop, const op_entry *fop, reduce_side side)
{ int cpri = side == REDUCE_LEFT ? fop->left_pri : fop->right_pri;

  if ( cpri == sop->op_pri )
  { /* Deal with `fy 2 yf`, `1 xfy 2 yfx 3`, etc.  */

    if ( ((sop->kind == OP_PREFIX || sop->kind == OP_INFIX) &&
	  sop->op_pri == sop->right_pri) &&
	 ((fop->kind == OP_POSTFIX || fop->kind == OP_INFIX) &&
	  fop->op_pri == fop->left_pri) )
      return false;
  }

  return cpri >= sop->op_pri;
}

static int
reduce_one_op(DECL_LD cterm_state *cstate,
	      const op_entry *op, reduce_side side)
{ ReadData _PL_rd = cstate->rd;
  int cpri = side == REDUCE_LEFT ? op->left_pri : op->right_pri;

  if ( cstate->out_n > 0 && cstate->side_n > 0 )
  { op_entry *sop = SideOp(cstate->side_p);

    if ( must_reduce(sop, op, side) )
    { int rc;

      rc = can_reduce(sop, cpri, cstate->out_n, cstate->rd);
      if ( rc > 0 )
      { if ( !build_op_term(sop, cstate->rd) )
	  return -1;
	if ( sop->kind == OP_INFIX )
	  cstate->out_n--;
	PopOp(cstate);
      }

      return rc;
    }
  }

  return false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Combine operators from  the side queue and out queue  as long as there
are sufficient operators and operands as requested by the left side of
the given operator.

Returns: true:   Ok
	 false:  Error
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define reduce_op(cstate, op) \
	LDFUNC(reduce_op, cstate, op)

static bool
reduce_op(DECL_LD cterm_state *cstate, const op_entry *op)
{ int rc;

  while((rc=reduce_one_op(cstate, op, REDUCE_LEFT)) == true)
    ;

  return !rc;		/* false --> true, -1 --> false */
}


static int
is_name_token(Token token, int must_be_op, ReadData _PL_rd)
{ switch(token->type)
  { case T_NAME:
      return true;
    case T_QNAME:
      return GD->options.traditional || !unquoted_atom(token->value.atom);
    case T_FUNCTOR:
    case T_DICT:
      return must_be_op;
    case T_PUNCTUATION:
    { switch(token->value.character)
      { case '[':
	case '{':
	  return true;
	case '(':
	  return false;
	case ')':
	case '}':
	case ']':
	  errorWarning("cannot_start_term", 0, _PL_rd);
	  return -1;
	case '|':
	  if ( !must_be_op )
	  { errorWarning("quoted_punctuation", 0, _PL_rd);
	    return -1;
	  }
	  return true;
	default:
	  return true;
      }
    }
    default:
      return false;
  }
}


static inline atom_t
name_token(Token token, op_entry *e, ReadData _PL_rd)
{ switch(token->type)
  { case T_PUNCTUATION:
      need_unlock(0, _PL_rd);
      switch(token->value.character)
      { case '[':
	  if ( e )
	    e->isblock = true;
	  return ATOM_nil;
	case '{':
	  if ( e )
	    e->isblock = true;
	  return ATOM_curl;
	default:
	  return codeToAtom(token->value.character);
      }
    case T_FULLSTOP:
      need_unlock(0, _PL_rd);
      return codeToAtom('.');			/* ATOM_dot can be [|] */
    default:
      return token->value.atom;
  }
}


#define unify_atomic_position(positions, token) \
	LDFUNC(unify_atomic_position, positions, token)

static int
unify_atomic_position(DECL_LD term_t positions, Token token)
{ if ( positions )
  { return PL_unify_term(positions,
			 PL_FUNCTOR, FUNCTOR_minus2,
			   PL_INT64, token->start,
			   PL_INT64, token->end);
  } else
    return true;
}


#define unify_string_position(positions, token) \
	LDFUNC(unify_string_position, positions, token)

static int
unify_string_position(DECL_LD term_t positions, Token token)
{ if ( positions )
  { return PL_unify_term(positions,
			 PL_FUNCTOR, FUNCTOR_string_position2,
			   PL_INT64, token->start,
			   PL_INT64, token->end);
  } else
    return true;
}


#define prepare_op(in_op, token, pin, _PL_rd) \
	LDFUNC(push_op, in_op, token, pin, _PL_rd)

static int
prepare_op(DECL_LD op_entry *in_op, Token token, term_t pin, ReadData _PL_rd)
{ int rc = true;

  Unlock(in_op->op.atom);		/* ok; part of an operator */

  if ( in_op->isblock )
  { term_t *top;

    if ( (rc = simple_term(token, pin, _PL_rd)) != true )
      return rc;			/* TBD: need cleanup? */
    top = term_av(-1, _PL_rd);
    in_op->op.block = PL_new_term_ref();
    in_op->isterm = true;
    PL_put_term(in_op->op.block, *top);
    truncate_term_stack(top, _PL_rd);
  } else
  { if ( !unify_atomic_position(pin, token) )
      return false;
  }

  return rc;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
complex_term() handles operator resolution. This is based on an old book
on compiler design of which I've forgotten the name. The idea is to have
two queues, the _side_ queue and the _out_ queue. The number of elements
pushed there is in `side_n` and  `out_n`.   As  the  side queue may hold
stuff from outer parse steps  we  have   `side_p`  pointing  at its top.
`side_p` is `side_n` - 1 -  |elems-at-start| and `SideOp(side_p)` is the
top-most operator pushed at the side queue.

Now the core idea is that we  push   operators  that we encounter to the
_side_ queue and have  operands  on  the   _out_  queue.  At  times,  we
_reduce_, which means we combine the top operator on the side queue with
the top element(s) on the out queue as   long as the side queue operator
has lower priority than the  reduce   context.  The  resulting terms are
pushed on the out stack.

Prolog  cannot  have  two  consequetive  operands.  This  constraint  it
maintained in `rmo`, (Ope)rands-more-then-operators.  This cannot become
more than 1 and thus, if we get into a situation where this threatens to
become two, we try to interpret the next   token as an operator, even if
it is a T_FUNCTOR token  (e.g.,  `name(`).   See  the  first argument of
get_token().

`rmo` can also not become below zero,  so   if  this threatens we try to
consider  the  last  operator  on  the  side   queue  as  an  atom.  See
modify_op().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define complex_term(stop, maxpri, positions, _PL_rd) \
	LDFUNC(complex_term, stop, maxpri, positions, _PL_rd)

static int
complex_term(DECL_LD const char *stop, short maxpri, term_t positions,
	     ReadData _PL_rd)
{ op_entry in_op;
  op_entry end_op;
  term_t pin;
  Token token;
  cterm_state cstate =
  { .rd = _PL_rd,
    .out_n = 0,
    .side_n = 0, .side_p = side_p0(_PL_rd),
    .rmo = 0
  };

  if ( _PL_rd->strictness == 0 )
    maxpri = OP_MAXPRIORITY+1;
  end_op.left_pri = maxpri;

  in_op.left_pri = 0;
  in_op.right_pri = 0;

  for(;;)
  { int rc;

    if ( positions )
      pin = PL_new_term_ref();
    else
      pin = 0;

    if ( !(token = get_token(cstate.rmo == 1, _PL_rd)) )
      return false;

    if ( cstate.out_n != 0 || cstate.side_n != 0 ) /* Check for end of term */
    { switch(token->type)
      { case T_FULLSTOP:
	  if ( stop == NULL )
	    goto exit;			/* exit for-loop */
	  break;
	case T_PUNCTUATION:
	  if ( stop != NULL && strchr(stop, token->value.character) )
	    goto exit;
	  break;
#ifdef O_QUASIQUOTATIONS
	case T_QQ_BAR:
	  if ( stop != NULL && stop[0] == '|' )
	    goto exit;
	  break;
#endif
      }
    }

    if ( (rc=is_name_token(token, cstate.rmo == 1, _PL_rd)) == true )
    { in_op.isblock     = false;
      in_op.isterm      = false;
      in_op.op.atom     = name_token(token, &in_op, _PL_rd);
      in_op.tpos        = pin;
      in_op.token_start = last_token_start;

      DEBUG(MSG_READ_OP, Sdprintf("name %s, rmo = %d\n",
				  stringOp(&in_op), cstate.rmo));

      if ( cstate.rmo == 0 && isOp(&in_op, OP_PREFIX, _PL_rd) )
      { DEBUG(MSG_READ_OP, Sdprintf("Prefix op: %s\n", stringOp(&in_op)));

	if ( !prepare_op(&in_op, token, pin, _PL_rd) )
	  return false;
	PushOp();

	continue;
      }
      if ( isOp(&in_op, OP_INFIX, _PL_rd) )
      { DEBUG(MSG_READ_OP, Sdprintf("Infix op: %s\n", stringOp(&in_op)));

	if ( !modify_op(&cstate, in_op.left_pri) )
	  return false;
	if ( cstate.rmo == 1 )
	{ if ( !reduce_op(&cstate, &in_op) )
	    return false;
	  cstate.rmo--;
	  if ( !prepare_op(&in_op, token, pin, _PL_rd) )
	    return false;
	  PushOp();
	  continue;
	}
      }
      if ( isOp(&in_op, OP_POSTFIX, _PL_rd) )
      { DEBUG(MSG_READ_OP, Sdprintf("Postfix op: %s\n", stringOp(&in_op)));

	if ( !modify_op(&cstate, in_op.left_pri) )
	  return false;
	if ( cstate.rmo == 1 )
	{ op_entry *red_op = &end_op;
	  reduce_side side = REDUCE_LEFT;

	  if ( !reduce_op(&cstate, &in_op) )
	    return false;

	  if ( cstate.side_n > 0 )
	  { op_entry *prev = SideOp(cstate.side_p);
	    if ( prev->kind == OP_PREFIX || prev->kind == OP_INFIX )
	    { red_op = prev;
	      side = REDUCE_RIGHT;
	    }
	  }

	  if ( !prepare_op(&in_op, token, pin, _PL_rd) )
	    return false;
	  PushOp();
	  if ( reduce_one_op(&cstate, red_op, side) == -1 )
	    return false;
	  continue;
	}
      }
    } else if ( rc < 0 )
      return false;

    if ( cstate.rmo == 1 )
      syntaxError("operator_expected", _PL_rd);

					/* Read `simple' term */
    rc = simple_term(token, pin, _PL_rd);
    if ( rc != true )
      return rc;

    if ( cstate.rmo != 0 )
      syntaxError("operator_expected", _PL_rd);
    cstate.rmo++;
    queue_out_op(0, pin, _PL_rd);
    cstate.out_n++;
  }

exit:
  unget_token();			/* the full-stop or punctuation */
  if ( !modify_op(&cstate, maxpri) )
    return false;
  if ( !reduce_op(&cstate, &end_op) )
    return false;

  if ( cstate.out_n == 1 && cstate.side_n == 0 ) /* simple term */
  { out_entry *e = out_op(-1, _PL_rd);
    int rc;

    if ( positions && (rc=PL_unify(positions, e->tpos)) != true )
      return rc;
    PopOut();

    return true;
  }

  if ( cstate.out_n == 0 && cstate.side_n == 1 ) /* single operator */
  { op_entry *op = SideOp(cstate.side_p);
    term_t term = alloc_term(_PL_rd);
    int rc;

    if ( !op->isblock )
      PL_put_atom(term, op->op.atom);
    else
      PL_put_term(term, op->op.block);

    if ( positions && (rc=PL_unify(positions, op->tpos)) != true )
      return rc;

    PopOp(&cstate);

    return true;
  }

  if ( cstate.side_n == 1 && !SideOp(0)->isblock &&
       ( SideOp(0)->op.atom == ATOM_comma ||
	 SideOp(0)->op.atom == ATOM_semicolon
       ))
  { term_t ex;

    LD->exception.processing = true;

    if ( (ex = PL_new_term_ref()) &&
	 PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_punct2,
			 PL_ATOM, SideOp(cstate.side_p)->op.atom,
			 PL_ATOM, name_token(token, NULL, _PL_rd)) )
      return errorWarning(NULL, ex, _PL_rd);

    return false;
  }

  syntaxError("operator_balance", _PL_rd);
}


#define set_range_position(positions, start, end) LDFUNC(set_range_position, positions, start, end)
static void
set_range_position(DECL_LD term_t positions, int64_t start, int64_t end)
{ Word p = valTermRef(positions);

  deRef(p);
  p = argTermP(*p, 0);
  if ( start >= 0 ) p[0] = consInt(start);
  if ( end   >= 0 ) p[1] = consInt(end);
}


#define end_range(positions) LDFUNC(end_range, positions)
static sword
end_range(DECL_LD term_t positions)
{ Word p = valTermRef(positions);

  deRef(p);
  return valInt(argTerm(*p,1));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_list(-positions)  reads  a  list  and  places    the  list  on  the
term-stack. Token is the [-token. The idea was that moving this function
out of simple_term() would reduce the  stack-usage of simple_term() when
not reading a list, but  this   assumption  appears wrong: when inlined,
simple_term() uses less stack. Why is that?  Nevertheless, this is a lot
more readable and makes selecting between inlining and not trivial.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define read_list(token, positions, _PL_rd) LDFUNC(read_list, token, positions, _PL_rd)
static inline int
read_list(DECL_LD Token token, term_t positions, ReadData _PL_rd)
{ term_t term, tail, *tmp;
  term_t pv;

#define P_ELEM (pv+0)			/* pos of element */
#define P_LIST (pv+1)			/* position list */
#define P_TAIL (pv+2)			/* position of tail */

  if ( !(tail = PL_new_term_ref()) )
    return false;

  if ( positions )
  { if ( !(pv = PL_new_term_refs(3)) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_list_position4,
			PL_INT64, token->start,
			PL_VARIABLE,
			PL_TERM, P_LIST,
			PL_TERM, P_TAIL) )
      return false;
  } else
    pv = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reading a list. Tmp is used to  read   the  next element. Tail is a very
special term-ref. It is always a reference   to the place where the next
term is to be written.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  term = alloc_term(_PL_rd);
  if ( !PL_put_term(tail, term) )
    return false;

  for(;;)
  { int rc;
    Word argp;

    if ( positions )
    { if ( !PL_unify_list(P_LIST, P_ELEM, P_LIST) )
	return false;
    }

    rc = complex_term(",|]", 999, P_ELEM, _PL_rd);
    if ( rc != true )
      return rc;
    if ( (rc=ensureSpaceForTermRefs(2)) != true )
      return rc;
    if ( !hasGlobalSpace(3) &&
	 (rc=ensureGlobalSpace(3, ALLOW_GC)) != true )
      return rc;
    argp = gTop;
    gTop += 3;
    *unRef(*valTermRef(tail)) = consPtr(argp,
					TAG_COMPOUND|STG_GLOBAL);
    *argp++ = FUNCTOR_dot2;
    setVar(argp[0]);
    setVar(argp[1]);
    tmp = term_av(-1, _PL_rd);
    readValHandle(tmp[0], argp++, _PL_rd);
    truncate_term_stack(tmp, _PL_rd);
    setHandle(tail, makeRefG(argp));

    token = get_token(false, _PL_rd);

    switch(token->value.character)
    { case ']':
	{ if ( positions )
	  { set_range_position(positions, -1, token->end);
	    if ( !PL_unify_nil(P_LIST) ||
		 !PL_unify_atom(P_TAIL, ATOM_none) )
	      return false;
	  }
	  return PL_unify_nil(tail);
	}
      case '|':
	{ int rc;
	  term_t pt = (pv ? P_TAIL : 0);

	  if ( (rc=complex_term(",|]", 999, pt, _PL_rd)) != true )
	    return rc;
	  argp = unRef(*valTermRef(tail));
	  tmp = term_av(-1, _PL_rd);
	  readValHandle(tmp[0], argp, _PL_rd);
	  truncate_term_stack(tmp, _PL_rd);
	  token = get_token(false, _PL_rd); /* discard ']' */
	  switch(token->value.character)
	  { case ',':
	    case '|':
	      syntaxError("list_rest", _PL_rd);
	  }
	  if ( positions )
	  { set_range_position(positions, -1, token->end);
	    if ( !PL_unify_nil(P_LIST) )
	      return false;
	  }
	  succeed;
	}
      case ',':
	continue;
    }
  }
#undef P_ELEM
#undef P_LIST
#undef P_TAIL
}


#define read_brace_term(token, positions, _PL_rd) LDFUNC(read_brace_term, token, positions, _PL_rd)
static inline int				/* read {...} */
read_brace_term(DECL_LD Token token, term_t positions, ReadData _PL_rd)
{ int rc;
  term_t pa;

  if ( positions )
  { if ( !(pa = PL_new_term_ref()) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_brace_term_position3,
			PL_INT64, token->start,
			PL_VARIABLE,
			PL_TERM, pa) )
      return false;
  } else
    pa = 0;

  if ( (rc=complex_term("}", OP_MAXPRIORITY+1, pa, _PL_rd)) != true )
    return rc;
  token = get_token(false, _PL_rd);
  if ( positions )
    set_range_position(positions, -1, token->end);

  return build_term(ATOM_curl, 1, _PL_rd);
}


#define read_parentheses_term(token, positions, _PL_rd) LDFUNC(read_parentheses_term, token, positions, _PL_rd)
static inline int				/* read (...) */
read_parentheses_term(DECL_LD Token token, term_t positions, ReadData _PL_rd)
{ int rc;
  term_t pa;

  if ( positions )
  { if ( !(pa = PL_new_term_ref()) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_parentheses_term_position3,
			PL_INT64, token->start,
			PL_VARIABLE,
			PL_TERM, pa) )
      return false;
  } else
    pa = 0;

  if ( (rc=complex_term(")", OP_MAXPRIORITY+1, pa, _PL_rd)) != true )
    return rc;
  token = get_token(false, _PL_rd);	/* skip ')' */
  if ( positions )
    set_range_position(positions, -1, token->end);

  succeed;
}


#define read_compound(token, positions, _PL_rd) LDFUNC(read_compound, token, positions, _PL_rd)
static inline int				/* read f(a1, ...) */
read_compound(DECL_LD Token token, term_t positions, ReadData _PL_rd)
{ int arity = 0;
  atom_t functor;
  term_t pv;
  int unlock;
  int rc;

#define P_HEAD (pv+0)
#define P_ARG  (pv+1)

  if ( positions )
  { if ( !(pv = PL_new_term_refs(2)) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_term_position5,
			PL_INT64, token->start,
			PL_VARIABLE,
			PL_INT64, token->start,
			PL_INT64, token->end,
			PL_TERM, P_ARG) )
      return false;
  } else
    pv = 0;

  functor = token->value.atom;
  unlock = (_PL_rd->locked == functor);
  _PL_rd->locked = 0;

  if ( !(token=get_token(false, _PL_rd)) ) /* gets '(' */
    return false;
  if ( !(token=get_token(false, _PL_rd)) ) /* first token */
    return false;

  if ( !(token->type == T_PUNCTUATION && token->value.character == ')') )
  { unget_token();

    do
    { if ( positions )
      { if ( !PL_unify_list(P_ARG, P_HEAD, P_ARG) )
	  return false;
      }
      if ( (rc=complex_term(",)", 999, P_HEAD, _PL_rd)) != true )
      { if ( unlock )
	  PL_unregister_atom(functor);
	return rc;
      }
      arity++;
      token = get_token(false, _PL_rd);	/* `,' or `)' */
    } while( token->value.character != ')' );
  }

  if ( positions )
  { set_range_position(positions, -1, token->end);
    if ( !PL_unify_nil(P_ARG) )
      return false;
  }

#undef P_HEAD
#undef P_ARG

  if ( _PL_rd->dotlists )
  { static atom_t dot = 0;
    if ( !dot )
      dot = PL_new_atom(".");
    if ( functor == dot )
      functor = ATOM_dot;		/* the abstract cons name */
  }

  rc = build_term(functor, arity, _PL_rd);
  if ( unlock )
    PL_unregister_atom(functor);

  return rc;
}


static int
is_key_token(Token token, ReadData _PL_rd)
{ switch(token->type)
  { case T_NAME:
    case T_QNAME:
    case T_FUNCTOR:
    case T_DICT:
      return true;
    case T_PUNCTUATION:
    { switch(token->value.character)
      { case '[':
	case '{':
	case '(':
	case ')':
	case '}':
	case ']':
	case '|':
	case ',':
	  return false;
	default:
	  return true;
      }
    }
    default:
      return false;
  }
}


/* read_dict() reads <class>{key:value, ...} into a dict as defined
   in pl-dict.c
*/

#define read_dict(token, positions, _PL_rd) LDFUNC(read_dict, token, positions, _PL_rd)
static inline int
read_dict(DECL_LD Token token, term_t positions, ReadData _PL_rd)
{ int pairs = 0;
  term_t pv;
  int rc;
  Token tstart;

#define P_HEAD  (pv+0)
#define P_ARG   (pv+1)
#define P_VALUE (pv+2)

  if ( positions )
  { if ( !(pv = PL_new_term_refs(3)) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_dict_position5,
			PL_INT64, token->start, /* whole term */
			PL_VARIABLE,
			PL_INT64, token->start, /* class position */
			PL_INT64, token->end,   /* key-value pairs */
			PL_TERM, P_ARG) )
      return false;
  } else
    pv = 0;

					/* Push the class */
  switch ( token->type )
  { case T_DICT:
    { term_t term = alloc_term(_PL_rd);
      PL_put_atom(term, token->value.atom);
      Unlock(token->value.atom);
      break;
    }
    case T_VCLASS_DICT:
    { term_t term = alloc_term(_PL_rd);
      setHandle(term, token->value.variable->signature);
      break;
    }
    case T_VOID_DICT:
    { alloc_term(_PL_rd);
    }
  }

  tstart = get_token(false, _PL_rd);	/* Skip '{' */

					/* process the key-values */
  if ( !(tstart->type == T_NAME && tstart->value.atom == ATOM_curl) )
  { do
    { Token key, sep;
      int64_t kstart, kend;
      term_t key_term;

      if ( positions )
      { if ( !PL_unify_list(P_ARG, P_HEAD, P_ARG) )
	  return false;
      }

      if ( !(key = get_token(false, _PL_rd)) )
	return false;

      if ( is_key_token(key, _PL_rd) )
      { key_term = alloc_term(_PL_rd);
	PL_put_atom(key_term, key->value.atom);
	Unlock(key->value.atom);
      } else if ( key->type == T_NUMBER )
      { Number n = &key->value.number;

	if ( n->type == V_INTEGER && valInt(consInt(n->value.i)) == n->value.i )
	{ key_term = alloc_term(_PL_rd);
	  PL_put_int64(key_term, n->value.i);
	} else
	  syntaxError("key_domain", _PL_rd); /* representation error? */
      } else
	syntaxError("key_expected", _PL_rd);

      kstart = token->start;
      kend   = token->end;
      if ( !(sep = get_token(false, _PL_rd)) )
	return false;

      if ( !is_key_token(sep, _PL_rd) ||
	   key->value.atom != ATOM_colon )
	syntaxError("colon_expected", _PL_rd);

      if ( positions )
      { PL_put_variable(P_VALUE);

	/* key_value_position(start, end, sep-start, sep-end, key, key-pos, value-pos) */

	if ( !PL_unify_term(P_HEAD,
			    PL_FUNCTOR, FUNCTOR_key_value_position7,
			    PL_INT64, kstart,		/* whole term */
			    PL_VARIABLE,
			    PL_INT64, sep->start, /* : start */
			    PL_INT64, sep->end,   /* : end */
			    PL_TERM,   key_term,
			    PL_FUNCTOR, FUNCTOR_minus2,
			      PL_INT64, kstart,
			      PL_INT64, kend,
			    PL_TERM, P_VALUE) )
	  return false;
      }

      if ( (rc=complex_term(",}", 999,
			    positions ? P_VALUE : 0,
			    _PL_rd)) != true )
	return rc;

      if ( positions )
      { sword vend = end_range(P_VALUE);

	set_range_position(P_HEAD, -1, vend);
      }

      pairs++;
      token = get_token(false, _PL_rd);	/* `,' or `}' */
    } while(token->value.character == ',');
  }

  if ( positions )
  { set_range_position(positions, -1, token->end);
    if ( !PL_unify_nil(P_ARG) )
      return false;
  }

#undef P_HEAD
#undef P_ARG
#undef P_VALUE

  return build_dict(pairs, _PL_rd);
}


/* simple_term() reads a term and leaves it on the top of the term-stack

Token is the first token of the term.
*/

static int
simple_term(DECL_LD Token token, term_t positions, ReadData _PL_rd)
{ switch(token->type)
  { case T_FULLSTOP:
      syntaxError("end_of_clause", _PL_rd);
    case T_VOID:
      alloc_term(_PL_rd);
      /* nothing to do; term is already a variable */
      return unify_atomic_position(positions, token);
    case T_VARIABLE:
    { term_t term = alloc_term(_PL_rd);
      setHandle(term, token->value.variable->signature);
      DEBUG(9, Sdprintf("Pushed var at 0x%x\n", token->value.variable));
      return unify_atomic_position(positions, token);
    }
    case T_NAME:
    case T_QNAME:
    { term_t term = alloc_term(_PL_rd);
      PL_put_atom(term, token->value.atom);
      Unlock(token->value.atom);
      return unify_atomic_position(positions, token);
    }
    case T_NUMBER:
    { term_t term = alloc_term(_PL_rd);
      if ( !_PL_put_number(term, &token->value.number) )
	return false;
      clearNumber(&token->value.number);
      return unify_atomic_position(positions, token);
    }
    case T_STRING:
    { term_t term = alloc_term(_PL_rd);
      PL_put_term(term, token->value.term);
      return unify_string_position(positions, token);
    }
    case T_FUNCTOR:
      return read_compound(token, positions, _PL_rd);
    case T_DICT:
    case T_VCLASS_DICT:
    case T_VOID_DICT:
      return read_dict(token, positions, _PL_rd);
    case T_PUNCTUATION:
    { switch(token->value.character)
      { case '(':
	  return read_parentheses_term(token, positions, _PL_rd);
	case '{':
	  return read_brace_term(token, positions, _PL_rd);
	case '[':
	  return read_list(token, positions, _PL_rd);
	case ',':
	  return errorWarning("quoted_punctuation", 0, _PL_rd);
	default:
	{ term_t term = alloc_term(_PL_rd);
	  PL_put_atom(term, codeToAtom(token->value.character));
	  return unify_atomic_position(positions, token);
	}
      }
    }
#ifdef O_QUASIQUOTATIONS
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
subterm_positions = quasi_quotation_position(From, To, TypePos, ContentPos)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    case T_QQ_OPEN:
    { int rc;
      term_t result, t, *argv, pv, av;

      result = alloc_term(_PL_rd);

				/* prepare (if we are the first in term) */
      if ( !_PL_rd->varnames )
	_PL_rd->varnames = PL_new_term_ref();
      if ( !_PL_rd->qq )
      { if ( _PL_rd->quasi_quotations )
	{ _PL_rd->qq = _PL_rd->quasi_quotations;
	} else
	{ if ( !(_PL_rd->qq = PL_new_term_ref()) )
	    return false;
	}

	if ( !(_PL_rd->qq_tail = PL_copy_term_ref(_PL_rd->qq)) )
	  return false;
      }

					/* allocate for quasi_quotation/4 */
      if ( !(av=PL_new_term_refs(4)) )
	return false;

      if ( positions )
      { if ( !(pv = PL_new_term_refs(3)) ||
	     !PL_unify_term(positions,
			    PL_FUNCTOR, FUNCTOR_quasi_quotation_position5,
			      PL_INT64, token->start,
			      PL_VARIABLE,
			      PL_TERM, pv+0,
			      PL_TERM, pv+1,
			      PL_TERM, pv+2) )
	  return false;
      } else
	pv = 0;
						/* push type */
      rc = complex_term("|", OP_MAXPRIORITY+1,
			positions ? pv+1 : 0,
			_PL_rd);
      if ( rc != true )
	return rc;
      token = get_token(false, _PL_rd);		/* get the '|' */
      if ( token->type != T_QQ_BAR )
	syntaxError("double_bar_expected", _PL_rd);

      argv = term_av(-1, _PL_rd);
      PL_put_term(av+0, argv[0]);		/* Arg 0: the type */
      truncate_term_stack(argv, _PL_rd);
      if ( !is_quasi_quotation_syntax(av, _PL_rd) )
	return false;
						/* Arg 1: the content */
      if ( !get_quasi_quotation(av+1, &rdhere, rdend, _PL_rd) )
	return false;

      if ( positions )
      { int64_t qqend = source_char_no + ptr_to_pos(rdhere, _PL_rd);

	if ( !PL_unify(pv+0, av+0) )
	  return false;
	set_range_position(positions, -1, qqend);
	if ( !PL_unify_term(pv+2,
			    PL_FUNCTOR, FUNCTOR_minus2,
			      PL_INT64, token->end,	/* end of | token */
			      PL_INT64, qqend-2) )     /* end minus "|}" */
	  return false;
      }

      PL_put_term(av+2, _PL_rd->varnames);	/* Arg 2: the var dictionary */
      if ( !PL_unify(av+3, result) )		/* Arg 3: the result */
	return false;

      if ( !PL_cons_functor_v(av+0, FUNCTOR_quasi_quotation4, av) )
	return false;

      if ( !(t = PL_new_term_ref()) ||
	   !PL_unify_list(_PL_rd->qq_tail, t, _PL_rd->qq_tail) ||
	   !PL_unify(t, av+0) )
	return false;

      return true;
    }
    case T_QQ_BAR:
      syntaxError("double_bar_outside_quasiquotation", _PL_rd);
#endif
    default:;
      sysError("read/1: Illegal token type (%d)", token->type);
      /*NOTREACHED*/
      fail;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
instantiate_template(-Term, +AtTerm)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define instantiate_template(term, at_term) LDFUNC(instantiate_template, term, at_term)
static int
instantiate_template(DECL_LD term_t term, term_t at_term)
{ term_t template, substitutions, head, var, value;

  if ( !(template = PL_new_term_ref()) ||
       !(substitutions = PL_new_term_ref()) ||
       !(head = PL_new_term_ref()) ||
       !(var = PL_new_term_ref()) ||
       !(value = PL_new_term_ref()) )
    return false;

  _PL_get_arg(1, at_term, template);
  _PL_get_arg(2, at_term, substitutions);
  if ( !PL_unify(term, template) )
    return false;
  if ( !PL_is_list(substitutions) )
    return PL_error(NULL, 0, "invalid template",
		    ERR_TYPE, ATOM_list, substitutions);
  while( PL_get_list(substitutions, head, substitutions) )
  { if ( PL_is_functor(head, FUNCTOR_equals2) )
    { _PL_get_arg(1, head, var);
      _PL_get_arg(2, head, value);
      if ( !PL_unify(var, value) )
	return false;
    } else
    { return PL_error(NULL, 0, "invalid template",
		      ERR_TYPE, ATOM_equal, head);
    }
  }

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_term(?term, ReadData rd)
    Common part of all read variations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define read_term(term, rd) LDFUNC(read_term, term, rd)
static bool
read_term(DECL_LD term_t term, ReadData rd)
{ int rc2, rc = false;
  term_t *result;
  Token token;
  Word p;
  fid_t fid;

  if ( !raw_read(rd, &rd->end) )
    fail;

  if ( !(fid=PL_open_foreign_frame()) )
    return false;

  rd->here = rd->base;
  rd->strictness = truePrologFlag(PLFLAG_ISO);

  C_STACK_OVERFLOW_GUARDED(
      rc2,
      complex_term(NULL, OP_MAXPRIORITY+1, rd->subtpos, rd),
      (void)0);
  if ( rc2 != true )
  { rc = raiseStackOverflow(rc2);
    goto out;
  }

  assert(rd->term_stack.top == 1);
  result = term_av(-1, rd);
  p = valTermRef(result[0]);
  if ( varInfo(*p, rd) )		/* reading a single variable */
  { Word v = allocGlobal(1);

    if ( !v )
      goto out;
    setVar(*v);
    if ( (rc2=ensureSpaceForTermRefs(1)) != true )
    { rc = raiseStackOverflow(rc2);
      goto out;
    }
    p = valTermRef(result[0]);		/* may be shifted */
    readValHandle(result[0], v, rd);
    *p = makeRefG(v);
  }

  if ( !(token = get_token(false, rd)) )
    goto out;
  if ( token->type != T_FULLSTOP )
  { errorWarning("end_of_clause_expected", 0, rd);
    goto out;
  }

  if ( rd->cycles && PL_is_functor(result[0], FUNCTOR_at_sign2) )
    rc = instantiate_template(term, result[0]);
  else
    rc = PL_unify(term, result[0]);

  truncate_term_stack(result, rd);
  if ( !rc )
    goto out;
  if ( rd->varnames && !(rc=bind_variable_names(rd)) )
    goto out;
#ifdef O_QUASIQUOTATIONS
  if ( !(rc=parse_quasi_quotations(rd)) )
    goto out;
#endif
  if ( rd->variables && !(rc=bind_variables(rd)) )
    goto out;
  if ( rd->singles && !(rc=check_singletons(term, rd)) )
    goto out;

  rc = true;

out:
  PL_close_foreign_frame(fid);

  return rc;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static unsigned char *
backSkipBlanks(const unsigned char *start, const unsigned char *end)
{ const unsigned char *s;

  for( ; end > start; end = s)
  { unsigned char *e;
    int chr;

    for(s=end-1 ; s>start && ISUTF8_CB(*s); s--)
      ;
    e = (unsigned char*)utf8_get_char((char*)s, &chr);
    assert(e == end);
    (void)e;
    if ( !PlBlankW(chr) )
      return (unsigned char*)end;
  }

  return (unsigned char *)start;
}


foreign_t
pl_raw_read2(term_t from, term_t term)
{ GET_LD
  unsigned char *s, *e, *t2, *top;
  read_data rd;
  foreign_t rval;
  IOSTREAM *in;
  int chr;
  PL_chars_t txt;

  if ( !getTextInputStream(from, &in) )
    fail;

  init_read_data(&rd, in);
  if ( !raw_read(&rd, &e) )
  { if ( Sferror(in) )
    { rval = streamStatus(in);
    } else
    { rval = PL_raise_exception(rd.exception);
      PL_release_stream(in);
    }

    free_read_data(&rd);

    return rval;
  } else
  { PL_release_stream(in);
  }

					/* strip the input from blanks */
  s   = rd.base;
  top = backSkipBlanks(s, e-1);
  t2  = backSkipUTF8(s, top, &chr);
  if ( chr == '.' )
    top = backSkipBlanks(s, t2);
					/* watch for "0' ." */
  if ( top < e && top-2 >= s && top[-1] == '\'' && top[-2] == '0' )
    top++;
  *top = EOS;
  s = skipSpaces(s);

  txt.text.t    = (char*)s;
  txt.length    = top-s;
  txt.storage   = PL_CHARS_HEAP;
  txt.encoding  = ENC_UTF8;
  txt.canonical = false;

  rval = PL_unify_text(term, 0, &txt, PL_ATOM);
  PL_free_text(&txt);
  free_read_data(&rd);

  return rval;
}


foreign_t
pl_raw_read(term_t term)
{ return pl_raw_read2(0, term);
}


foreign_t
pl_read2(term_t from, term_t term)
{ GET_LD
  read_data rd;
  int rval;
  IOSTREAM *s;

  if ( !getTextInputStream(from, &s) )
    fail;

  init_read_data(&rd, s);
  rval = read_term(term, &rd);
  if ( rd.has_exception )
    rval = PL_raise_exception(rd.exception);
  free_read_data(&rd);

  if ( Sferror(s) )
    return streamStatus(s);
  else
    PL_release_stream(s);

  return rval;
}


foreign_t
pl_read(term_t term)
{ return pl_read2(0, term);
}


#define unify_read_term_position(tpos) LDFUNC(unify_read_term_position, tpos)
static int
unify_read_term_position(DECL_LD term_t tpos)
{ if ( tpos && source_line_no > 0 )
  { return PL_unify_term(tpos,
			 PL_FUNCTOR, FUNCTOR_dstream_position4,
			   PL_INT64, source_char_no,
			   PL_INT, source_line_no,
			   PL_INT, source_line_pos,
			   PL_INT64, source_byte_no);
  } else
  { return true;
  }
}


/** read_clause(+Stream, -Clause, +Options)

Options:
	* variable_names(-Names)
	* process_comment(+Boolean)
	* comments(-List)
	* syntax_errors(+Atom)
	* term_position(-Position)
	* subterm_positions(-Layout)
*/

static const PL_option_t read_clause_options[] =
{ { ATOM_variable_names,    OPT_TERM },
  { ATOM_term_position,	    OPT_TERM },
  { ATOM_subterm_positions, OPT_TERM },
  { ATOM_process_comment,   OPT_BOOL },
  { ATOM_comments,	    OPT_TERM },
  { ATOM_syntax_errors,     OPT_ATOM },
  { NULL_ATOM,		    0 }
};


static int
callCommentHook(predicate_t comment_hook,
		term_t comments, term_t tpos, term_t term)
{ GET_LD
  fid_t fid;
  term_t av;
  int rc = true;

  if ( (fid = PL_open_foreign_frame()) &&
       (av = PL_new_term_refs(3)) )
  { qid_t qid;

    PL_put_term(av+0, comments);
    PL_put_term(av+1, tpos);
    PL_put_term(av+2, term);

    if ( (qid = PL_open_query(NULL, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
			      comment_hook, av)) )
    { term_t ex;

      if ( !PL_next_solution(qid) && (ex=PL_exception(qid)) )
	rc = false;

      PL_close_query(qid);
    }

    PL_discard_foreign_frame(fid);
  } else
    rc = false;

  return rc;
}


int
read_clause(DECL_LD IOSTREAM *s, term_t term, term_t options)
{ read_data rd;
  int rval;
  fid_t fid;
  term_t tpos = 0;
  term_t comments = 0;
  term_t opt_comments = 0;
  int process_comment;
  atom_t syntax_errors = ATOM_dec10;
  predicate_t comment_hook;

  comment_hook = _PL_predicate("comment_hook", 3, "prolog",
			       &GD->procedures.comment_hook3);
  process_comment = (comment_hook->definition->impl.any.defined != NULL);

  if ( !(fid=PL_open_foreign_frame()) )
    return false;

retry:
  init_read_data(&rd, s);

  if ( options &&
       !PL_scan_options(options, 0, "read_option", read_clause_options,
			&rd.varnames,
			&tpos,
			&rd.subtpos,
			&process_comment,
			&opt_comments,
			&syntax_errors) )
  { PL_close_foreign_frame(fid);
    return false;
  }

  if ( opt_comments )
  { comments = PL_new_term_ref();
  } else if ( process_comment )
  { if ( !tpos )
      tpos = PL_new_term_ref();
    comments = PL_new_term_ref();
  }

  set_module_read_data(&rd, LD->modules.source);
  if ( comments )
    rd.comments = PL_copy_term_ref(comments);
  rd.on_error = syntax_errors;
  rd.singles = rd.styleCheck & SINGLETON_CHECK ? true : false;
  if ( (rval=read_term(term, &rd)) &&
       (!tpos || (rval=unify_read_term_position(tpos))) )
  { if ( rd.comments &&
	 (rval = PL_unify_nil(rd.comments)) )
    { if ( opt_comments )
	rval = PL_unify(opt_comments, comments);
      else if ( !PL_get_nil(comments) )
	rval = callCommentHook(comment_hook, comments, tpos, term);
    }
  } else
  { if ( rd.has_exception && reportReadError(&rd) )
    { LD->exception.processing = false;
      PL_rewind_foreign_frame(fid);
      free_read_data(&rd);
      goto retry;
    }
  }
  free_read_data(&rd);

  return rval;
}


static
PRED_IMPL("read_clause", 3, read_clause, 0)
{ PRED_LD
  int rc;
  IOSTREAM *s;

  if ( !getTextInputStream(A1, &s) )
    return false;
  rc = read_clause(s, A2, A3);
  if ( Sferror(s) )
    return streamStatus(s);
  else
    PL_release_stream(s);

  return rc;
}


static const PL_option_t read_term_options[] =
{ { ATOM_variable_names,    OPT_TERM },
  { ATOM_variables,         OPT_TERM },
  { ATOM_singletons,        OPT_TERM },
  { ATOM_term_position,     OPT_TERM },
  { ATOM_subterm_positions, OPT_TERM },
  { ATOM_character_escapes, OPT_BOOL },
  { ATOM_var_prefix,        OPT_BOOL },
  { ATOM_double_quotes,	    OPT_ATOM },
  { ATOM_module,	    OPT_ATOM },
  { ATOM_syntax_errors,     OPT_ATOM },
  { ATOM_back_quotes,       OPT_ATOM },
  { ATOM_comments,	    OPT_TERM },
#ifdef O_QUASIQUOTATIONS
  { ATOM_quasi_quotations,  OPT_TERM },
#endif
  { ATOM_cycles,	    OPT_BOOL },
  { ATOM_dotlists,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};


#define read_term_from_stream(s, term, options) LDFUNC(read_term_from_stream, s, term, options)
static foreign_t
read_term_from_stream(DECL_LD IOSTREAM *s, term_t term, term_t options)
{ term_t tpos = 0;
  term_t tcomments = 0;
  int rval;
  atom_t w;
  read_data rd;
  int charescapes = -1;
  int varprefix = -1;
  atom_t dq = NULL_ATOM;
  atom_t bq = NULL_ATOM;
  atom_t mname = NULL_ATOM;
  fid_t fid = PL_open_foreign_frame();

retry:
  init_read_data(&rd, s);

#ifdef O_QUASIQUOTATIONS
#define QQ_ARG &rd.quasi_quotations,
#else
#define QQ_ARG
#endif

  if ( !PL_scan_options(options, 0, "read_option", read_term_options,
			&rd.varnames,
			&rd.variables,
			&rd.singles,
			&tpos,
			&rd.subtpos,
			&charescapes,
			&varprefix,
			&dq,
			&mname,
			&rd.on_error,
			&bq,
			&tcomments,
			QQ_ARG
			&rd.cycles,
			&rd.dotlists) )
    return false;

  if ( mname )
  { rd.module = isCurrentModule(mname);
    if ( !rd.module )
      rd.module = MODULE_user;
    rd.flags  = rd.module->flags;
  }

  if ( charescapes != -1 )
  { if ( charescapes )
      set(&rd, M_CHARESCAPE);
    else
      clear(&rd, M_CHARESCAPE);
  }
  if ( varprefix != -1 )
  { if ( varprefix )
      set(&rd, M_VARPREFIX);
    else
      clear(&rd, M_VARPREFIX);
  }
  if ( dq )
  { if ( !setDoubleQuotes(dq, &rd.flags) )
      return false;
  }
  if ( bq )
  { if ( !setBackQuotes(bq, &rd.flags) )
      return false;
  }
  if ( rd.singles && PL_get_atom(rd.singles, &w) && w == ATOM_warning )
    rd.singles = true;
  if ( tcomments )
    rd.comments = PL_copy_term_ref(tcomments);
  if ( rd.subtpos )
    s->position = &s->posbuf;

  rval = read_term(term, &rd);
  if ( Sferror(s) )
    return false;

  if ( rval )
  { if ( tpos )
      rval = unify_read_term_position(tpos);
    if ( rval && tcomments )
    { if ( !PL_unify_nil(rd.comments) )
	rval = false;
    }
  } else
  { if ( rd.has_exception && reportReadError(&rd) )
    { PL_rewind_foreign_frame(fid);
      free_read_data(&rd);
      goto retry;
    }
  }

  free_read_data(&rd);

  return rval;
}


/** read_term(+Stream, -Term, +Options) is det.
*/

static
PRED_IMPL("read_term", 3, read_term, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s;

  if ( getTextInputStream(A1, &s) )
  { if ( read_term_from_stream(s, A2, A3) )
      return PL_release_stream(s);
    if ( Sferror(s) )
      return streamStatus(s);
    PL_release_stream(s);
    return false;
  }

  return false;
}


/** read_term(-Term, +Options) is det.
*/

static
PRED_IMPL("read_term", 2, read_term, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s;

  if ( getTextInputStream(0, &s) )
  { if ( read_term_from_stream(s, A1, A2) )
      return PL_release_stream(s);
    if ( Sferror(s) )
      return streamStatus(s);
    PL_release_stream(s);
    return false;
  }

  return false;
}


		 /*******************************
		 *	   TERM <->ATOM		*
		 *******************************/

/** read_term_from_atom(+Atom, -Term, +Options) is det.

Read a term from Atom using read_term/3.
*/

static
PRED_IMPL("read_term_from_atom", 3, read_term_from_atom, 0)
{ PRED_LD
  PL_chars_t txt;

  if ( PL_get_text(A1, &txt,
		   CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION|BUF_STACK) )
  { int rc;
    IOSTREAM *stream;
    source_location oldsrc = LD->read_source;

    if ( (stream = Sopen_text(&txt, "r")) )
    { rc = read_term_from_stream(stream, A2, A3);
      Sclose(stream);
    } else
      rc = false;

    LD->read_source = oldsrc;

    return rc;
  }

  return false;
}


#define atom_to_term(atom, term, bindings, text_type) \
	LDFUNC(atom_to_term, atom, term, bindings, text_type)

static int
atom_to_term(DECL_LD term_t atom, term_t term, term_t bindings, int text_type)
{ PL_chars_t txt;

  if ( !bindings && PL_is_variable(atom) ) /* term_to_atom(+, -) */
  { char buf[1024];
    size_t bufsize = sizeof(buf);
    int rval;
    char *s = buf;
    IOSTREAM *stream;
    PL_chars_t txt;

    stream = Sopenmem(&s, &bufsize, "w");
    stream->encoding = ENC_UTF8;
    rval = PL_write_term(stream, term, 1200, PL_WRT_QUOTED);
    if (rval)
    { Sflush(stream);

      txt.text.t = s;
      txt.length = bufsize;
      txt.storage = PL_CHARS_HEAP;
      txt.encoding = ENC_UTF8;
      txt.canonical = false;
      rval = PL_unify_text(atom, 0, &txt, text_type);
    }

    Sclose(stream);
    if ( s != buf )
      Sfree(s);

    return rval;
  }

  if ( PL_get_text(atom, &txt, CVT_ALL|CVT_EXCEPTION) )
  { GET_LD
    read_data rd;
    int rval;
    IOSTREAM *stream;
    source_location oldsrc = LD->read_source;

    stream = Sopen_text(&txt, "r");

    init_read_data(&rd, stream);
    if ( bindings && (PL_is_variable(bindings) || PL_is_list(bindings)) )
      rd.varnames = bindings;
    else if ( bindings )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, bindings);
    set(&rd, M_RDSTRING_TERM);

    if ( !(rval = read_term(term, &rd)) && rd.has_exception )
      rval = PL_raise_exception(rd.exception);
    free_read_data(&rd);
    Sclose(stream);
    LD->read_source = oldsrc;

    return rval;
  }

  fail;
}


static
PRED_IMPL("atom_to_term", 3, atom_to_term, 0)
{ GET_LD
  return atom_to_term(A1, A2, A3, PL_ATOM);
}


static
PRED_IMPL("term_to_atom", 2, term_to_atom, 0)
{ GET_LD
  return atom_to_term(A2, A1, 0, PL_ATOM);
}


static
PRED_IMPL("term_string", 2, term_string, 0)
{ GET_LD
  return atom_to_term(A2, A1, 0, PL_STRING);
}


bool
PL_put_term_from_chars(term_t t, int flags, size_t len, const char *s)
{ GET_LD
  read_data rd;
  int rval;
  IOSTREAM *stream;
  source_location oldsrc;

  if ( len == (size_t)-1 )
    len = strlen(s);

  if ( len >= 1 &&
       (isDigit(*s&0xff) || *s == '-') &&
       isDigit(s[len-1]) )
  { char buf[256];
    unsigned char *e;
    char *ns;
    number n;
    int isnum;

    if ( s[len] != EOS )		/* not 0-terminated */
    { if ( len+1 > sizeof(buf) )
      { if ( !(ns=malloc(len+1)) )
	  return PL_resource_error("memory");
      } else
	ns = buf;
      memcpy(ns, s, len);
      ns[len] = EOS;
    } else
    { ns = (char*)s;
    }

    isnum = ( str_number((cucharp)ns, &e, &n, 0) == NUM_OK &&
	      e == (unsigned char *)ns+len );
    if ( ns != s && ns != buf )
      free(ns);
    if ( isnum )
    { int rc = PL_put_number(t, &n);
      clearNumber(&n);
      return rc;
    }
  }

  stream = Sopen_string(NULL, (char *)s, len, "r");
  stream->encoding = ((flags&REP_UTF8) ? ENC_UTF8 : \
		      (flags&REP_MB)   ? ENC_ANSI : ENC_ISO_LATIN_1);
  oldsrc = LD->read_source;

  init_read_data(&rd, stream);
  PL_put_variable(t);
  if ( !(rval = read_term(t, &rd)) && rd.has_exception )
  { if ( (flags&CVT_EXCEPTION) )
      rval = PL_raise_exception(rd.exception);
    else
      PL_put_term(t, rd.exception);
  }
  free_read_data(&rd);
  Sclose(stream);
  LD->read_source = oldsrc;

  return rval;
}


bool
PL_chars_to_term(const char *s, term_t t)
{ return PL_put_term_from_chars(t, REP_ISO_LATIN_1, (size_t)-1, s);
}


bool
PL_wchars_to_term(const wchar_t *s, term_t t)
{ GET_LD
  int rc;
  IOSTREAM *stream;
  PL_chars_t text;

  text.text.w    = (pl_wchar_t *)s;
  text.encoding  = ENC_WCHAR;
  text.storage   = PL_CHARS_HEAP;
  text.length    = wcslen(s);
  text.canonical = false;

  if ( (stream = Sopen_text(&text, "r")) )
  { read_data rd;

    source_location oldsrc = LD->read_source;
    init_read_data(&rd, stream);
    PL_put_variable(t);
    if ( !(rc = read_term(t, &rd)) && rd.has_exception )
      PL_put_term(t, rd.exception);
    free_read_data(&rd);
    Sclose(stream);
    LD->read_source = oldsrc;
  } else
    rc = false;

  PL_free_text(&text);

  return rc;
}


		 /*******************************
		 *	     CODE TYPE		*
		 *******************************/

/* '$code_class'(+Code, +Category) is semidet.

True if Code is a member of Category.   This predicate is added to allow
for inspection of the chararacter categories   used for parsing. Defined
categories are:

    * layout
    * graphic
    These are the glueing symbol characters
    * solo
    These are the non-glueing symbol characters
    * punct
    These are the Prolog characters with reserved meaning
    * id_start
    Start of an unquoted atom or variable
    * id_continue
    Continue an unquoted atom or variable
    * upper
    If the id_start char fits, this is a variable.
    * invalid
    Character may not appear outside comments or quoted strings
*/

static
PRED_IMPL("$code_class", 2, code_class, 0)
{ PRED_LD
  int code, rc;
  atom_t class;
  const char *c;

  if ( !PL_get_char_ex(A1, &code, false) ||
       !PL_get_atom_ex(A2, &class) )
    return false;

  if ( !VALID_CODE_POINT(code) )
    PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, A1);

  c = PL_atom_chars(class);
  if ( streq(c, "layout") )
    rc = PlBlankW(code);
  else if ( streq(c, "graphic") )
    rc = PlSymbolW(code);
  else if ( streq(c, "solo") )
    rc = PlSoloW(code);
  else if ( streq(c, "punct") )
    rc = PlPunctW(code);
  else if ( streq(c, "upper") )
    rc = PlUpperW(code);
  else if ( streq(c, "id_start") )
    rc = PlIdStartW(code);
  else if ( streq(c, "id_continue") )
    rc = PlIdContW(code);
  else if ( streq(c, "invalid") )
    rc = PlInvalidW(code);
  else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_category, A2);

  return rc ? true : false;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(read)
  PRED_DEF("read_term",		  3, read_term,		  PL_FA_ISO)
  PRED_DEF("read_term",		  2, read_term,		  PL_FA_ISO)
  PRED_DEF("read_clause",	  3, read_clause,	  0)
  PRED_DEF("read_term_from_atom", 3, read_term_from_atom, 0)
  PRED_DEF("atom_to_term",	  3, atom_to_term,	  0)
  PRED_DEF("term_to_atom",	  2, term_to_atom,	  0)
  PRED_DEF("term_string",	  2, term_string,	  0)
  PRED_DEF("$code_class",	  2, code_class,	  0)
  PRED_DEF("$is_named_var",       1, is_named_var,        0)
#ifdef O_QUASIQUOTATIONS
  PRED_DEF("$qq_open",            2, qq_open,             0)
#endif
EndPredDefs
