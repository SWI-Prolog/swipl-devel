/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include <math.h>
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"
#include "os/pl-dtoa.h"
#include "pl-umap.c"			/* Unicode map */

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
#define PlIdStartW(c)	(c <= 0xff ? (isLower(c)||isUpper(c)||c=='_') \
				   : uflagsW(c) & U_ID_START)
#define PlIdContW(c)	CharTypeW(c, >= UC, U_ID_CONTINUE)
#define PlSymbolW(c)	CharTypeW(c, == SY, U_SYMBOL)
#define PlPunctW(c)	CharTypeW(c, == PU, 0)
#define PlSoloW(c)	CharTypeW(c, == SO, U_OTHER)
#define PlInvalidW(c)   (uflagsW(c) == 0)

int
f_is_prolog_var_start(wint_t c)
{ return PlIdStartW(c) && (PlUpperW(c) || c == '_');
}

int
f_is_prolog_atom_start(wint_t c)
{ return PlIdStartW(c) != 0;
}

int
f_is_prolog_identifier_continue(wint_t c)
{ return PlIdContW(c) || c == '_';
}

int
f_is_prolog_symbol(wint_t c)
{ return PlSymbolW(c) != 0;
}

int
unicode_separator(pl_wchar_t c)
{ return PlBlankW(c);
}

/* unquoted_atomW() returns TRUE if text can be written to s as unquoted atom
*/

int
unquoted_atomW(const pl_wchar_t *s, size_t len, IOSTREAM *fd)
{ const pl_wchar_t *e = &s[len];

  if ( len == 0 )
    return FALSE;

  if ( !PlIdStartW(*s) || PlUpperW(*s) )
    return FALSE;

  for(s++; s<e; )
  { int c = *s++;

    if ( !(PlIdContW(c) && (!fd || Scanrepresent(c, fd) == 0)) )
      return FALSE;
  }

  return TRUE;
}


int
atom_varnameW(const pl_wchar_t *s, size_t len)
{ if ( PlUpperW(*s) || *s == '_' )
  { for(s++; --len > 0; s++)
    { int c = *s;

      if ( !PlIdContW(c) )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


int
atom_is_named_var(atom_t name)		/* see warn_singleton() */
{ const char *s;
  const pl_wchar_t *w;

  if ( (s=PL_atom_chars(name)) )
  { if ( s[0] != '_' ) return TRUE;
    if ( s[1] == '_' ) return FALSE;
    if ( s[1] && !PlUpperW(s[1]) ) return TRUE;
  } else if ( (w=PL_atom_wchars(name, NULL)) )
  { if ( w[0] != '_' ) return TRUE;
    if ( w[1] == '_' ) return FALSE;
    if ( w[1] && !PlUpperW(w[1]) ) return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	   CHAR-CONVERSION	*
		 *******************************/

static int  char_table[257];	/* also map -1 (EOF) */
static int *char_conversion_table = &char_table[1];

void
initCharConversion()
{ int i;

  for(i=-1; i< 256; i++)
    char_conversion_table[i] = i;
}


foreign_t
pl_char_conversion(term_t in, term_t out)
{ int cin, cout;

  if ( !PL_get_char(in, &cin, FALSE) ||
       !PL_get_char(out, &cout, FALSE) )
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
      { if ( PL_get_char(in, &cin, FALSE) )
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
    return FALSE;

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
  int		times;		/* Number of occurences */
  word		signature;	/* Pseudo atom */
} *Variable;

typedef struct token
{ int type;			/* type of token */
  intptr_t start;		/* start-position */
  intptr_t end;			/* end-position */
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
  char	kind;			/* kind (prefix/postfix/infix) */
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
#define T_NAME		1	/* ordinary name */
#define T_VARIABLE	2	/* variable name */
#define T_VOID		3	/* void variable */
#define T_NUMBER	4	/* integer or float */
#define T_STRING	5	/* "string" */
#define T_PUNCTUATION	6	/* punctuation character */
#define T_FULLSTOP	7	/* Prolog end of clause */
#define T_QQ_OPEN	8	/* "{|" of {|Syntax||Quotation|} stuff */
#define T_QQ_BAR	9	/* "||" of {|Syntax||Quotation|} stuff */

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
  int	        _unget;			/* unget_token() */
  int		magic;			/* RD_MAGIC */

  source_location start_of_term;	/* Position of start of term */
  unsigned char *posp;			/* position pointer */
  size_t	posi;			/* position number */

  Module	module;			/* Current source module */
  unsigned int	flags;			/* Module syntax flags (M_*) */
  int		styleCheck;		/* style-checking mask */
  bool		backquoted_string;	/* Read `hello` as string */
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

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif

static void	init_term_stack(ReadData _PL_rd);
static void	clear_term_stack(ReadData _PL_rd);


static void
init_read_data(ReadData _PL_rd, IOSTREAM *in ARG_LD)
{ memset(_PL_rd, 0, offsetof(read_data, _rb.fast));

  initBuffer(&var_name_buffer);
  initBuffer(&var_buffer);
  initBuffer(&_PL_rd->op.out_queue);
  initBuffer(&_PL_rd->op.side_queue);
  init_term_stack(_PL_rd);
  _PL_rd->exception = PL_new_term_ref();
  rb.stream = in;
  _PL_rd->magic = RD_MAGIC;
  _PL_rd->module = MODULE_parse;
  _PL_rd->flags  = _PL_rd->module->flags; /* change for options! */
  _PL_rd->styleCheck = debugstatus.styleCheck;
  _PL_rd->on_error = ATOM_error;
  _PL_rd->backquoted_string = truePrologFlag(PLFLAG_BACKQUOTED_STRING);
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
  clear_term_stack(_PL_rd);
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
	end_of_file_in_atom
	end_of_file_in_block_comment
	end_of_file_in_string
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


#define syntaxError(what, rd) do { errorWarning(what, 0, rd); fail; } while(0)

const char *
str_number_error(strnumstat rc)
{ switch(rc)
  { case NUM_ERROR:      return "illegal_number";
    case NUM_OK:	 return "no_error";
    case NUM_FUNDERFLOW: return "float_underflow";
    case NUM_FOVERFLOW:  return "float_overflow";
    case NUM_IOVERFLOW:  return "integer_overflow";
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
  for(s=rdbase; s<here; s = utf8_get_uchar(s, &c))
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
  int rc = TRUE;

  if ( pos->file )				/* reading a file */
  { rc = PL_unify_term(loc,
		       PL_FUNCTOR, FUNCTOR_file4,
			 PL_ATOM,  pos->file,
			 PL_INT,   pos->position.lineno,
			 PL_INT,   pos->position.linepos,
			 PL_INT64, pos->position.charno);
  } else if ( isStringStream(rb.stream) )
  { intptr_t charno;

    charno = pos->position.charno - _PL_rd->start_of_term.position.charno;

    rc = PL_unify_term(loc,
		       PL_FUNCTOR, FUNCTOR_string2,
			 PL_UTF8_STRING, rdbase,
			 PL_INTPTR,      charno);
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
      rc = FALSE;
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
  int rc = TRUE;

  if ( !(ex = PL_new_term_ref()) ||
       !(loc = PL_new_term_ref()) )
    rc = FALSE;

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
    { rc = FALSE;
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

  LD->exception.processing = TRUE;	/* allow using spare stack */

  ex = makeErrorTerm(id_str, id_arg, id_term, _PL_rd);

  if ( _PL_rd )
  { _PL_rd->has_exception = TRUE;
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
singletonWarning(const char *which, const char **vars, int nvars)
{ GET_LD
  fid_t fid;

  if ( (fid = PL_open_foreign_frame()) )
  { term_t l = PL_new_term_ref();
    term_t a = PL_copy_term_ref(l);
    term_t h = PL_new_term_ref();
    int n;

    for(n=0; n<nvars; n++)
    { if ( !PL_unify_list(a, h, a) ||
	   !PL_unify_chars(h, REP_UTF8|PL_ATOM, -1, vars[n]) )
	return FALSE;
    }
    if ( !PL_unify_nil(a) )
      return FALSE;

    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, which, 1,
		   PL_TERM, l);

    PL_discard_foreign_frame(fid);

    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	FALSE	return false
	TRUE	redo
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
reportReadError(ReadData rd)
{ if ( rd->on_error == ATOM_error )
    return PL_raise_exception(rd->exception);
  if ( rd->on_error != ATOM_quiet )
    printMessage(ATOM_error, PL_TERM, rd->exception);
  PL_clear_exception();

  if ( rd->on_error == ATOM_dec10 )
    return TRUE;

  return FALSE;
}


		/********************************
		*           RAW READING         *
		*********************************/

static unsigned char *
backSkipUTF8(unsigned const char *start, unsigned const char *end, int *chr)
{ const unsigned char *s;

  for(s=end-1 ; s>start && *s&0x80; s--)
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

  *rb.here++ = c;
}


static inline void
addByteToBuffer(int c, ReadData _PL_rd)
{ c &= 0xff;

  if ( rb.here >= rb.end )
    growToBuffer(c, _PL_rd);
  else
    *rb.here++ = c;
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


static void
setCurrentSourceLocation(ReadData _PL_rd ARG_LD)
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
			 { setCurrentSourceLocation(_PL_rd PASS_LD); \
			   something_read++; \
			 } \
		       }

#define rawSyntaxError(what) { addToBuffer(EOS, _PL_rd); \
			       rdbase = rb.base, last_token_start = rb.here-1; \
			       syntaxError(what, _PL_rd); \
			     }

static int
raw_read_quoted(int q, ReadData _PL_rd)
{ int newlines = 0;
  int c;

  addToBuffer(q, _PL_rd);
  while((c=getchrq()) != EOF && c != q)
  {
  next:
    if ( c == '\\' && true(_PL_rd, M_CHARESCAPE) )
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
	    return TRUE;
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
	      return TRUE;
	  }
	  continue;			/* \symbolic-control-char */
      }
    } else if (c == '\n' &&
	       newlines++ > MAXNEWLINES &&
	       (_PL_rd->styleCheck & LONGATOM_CHECK))
    { rawSyntaxError("long_string");
    }
    addToBuffer(c, _PL_rd);
  }

out:
  if (c == EOF)
  {
  eofinstr:
    if ( Sferror(rb.stream) )
      return FALSE;
    rawSyntaxError("end_of_file_in_string");
  }
  addToBuffer(c, _PL_rd);

  return TRUE;
}


#ifdef O_QUASIQUOTATIONS
static int
raw_read_quasi_quotation(int c, ReadData _PL_rd)
{ addToBuffer(c, _PL_rd);

  while((c=getchrq()) != EOF)
  { addToBuffer(c, _PL_rd);
    if ( c == '}' &&
	 rb.here[-2] == '|' )
      return TRUE;
  }

  rawSyntaxError("end_of_file_in_quasi_quotation");
}
#endif


static int
add_comment(Buffer b, IOPOS *pos, ReadData _PL_rd ARG_LD)
{ term_t head = PL_new_term_ref();

  assert(_PL_rd->comments);
  if ( !PL_unify_list(_PL_rd->comments, head, _PL_rd->comments) )
    return FALSE;
  if ( pos )
  { if ( !PL_unify_term(head,
			PL_FUNCTOR, FUNCTOR_minus2,
			  PL_FUNCTOR, FUNCTOR_stream_position4,
			    PL_INT64, pos->charno,
			    PL_INT, pos->lineno,
			    PL_INT, pos->linepos,
			    PL_INT, 0,
			  PL_UTF8_STRING, baseBuffer(b, char)) )
      return FALSE;
  } else
  { if ( !PL_unify_term(head,
			PL_FUNCTOR, FUNCTOR_minus2,
			  ATOM_minus,
			  PL_UTF8_STRING, baseBuffer(b, char)) )
      return FALSE;
  }

  PL_reset_term_refs(head);
  return TRUE;
}


static void
setErrorLocation(IOPOS *pos, ReadData _PL_rd)
{ if ( pos )
  { GET_LD

    LD->read_source.position = *pos;
  }
  rb.here = rb.base+1;			/* see rawSyntaxError() */
}


static unsigned char *
raw_read2(ReadData _PL_rd ARG_LD)
{ int c;
  bool something_read = FALSE;
  bool dotseen = FALSE;
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
		  return NULL;
		if ( isStringStream(rb.stream) ) /* do not require '. ' when */
		{ addToBuffer(' ', _PL_rd);     /* reading from a string */
		  addToBuffer('.', _PL_rd);
		  addToBuffer(' ', _PL_rd);
		  addToBuffer(EOS, _PL_rd);
		  return rb.base;
		}
		if (something_read)
		{ if ( dotseen )		/* term.<EOF> */
		  { if ( rb.here - rb.base == 1 )
		      rawSyntaxError("end_of_clause");
		    ensure_space(' ');
		    addToBuffer(EOS, _PL_rd);
		    return rb.base;
		  }
		  rawSyntaxError("end_of_file");
		}
		if ( Sfpasteof(rb.stream) )
		{ term_t stream;

		  LD->exception.processing = TRUE;
		  stream = PL_new_term_ref();
		  PL_unify_stream_or_alias(stream, rb.stream);
		  PL_error(NULL, 0, NULL, ERR_PERMISSION,
			   ATOM_input, ATOM_past_end_of_stream, stream);
		  return NULL;
		}
		set_start_line;
		strcpy((char *)rb.base, "end_of_file. ");
		rb.here = rb.base + 14;
		return rb.base;
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
		      return NULL;
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
			  return NULL;
			rawSyntaxError("end_of_file_in_block_comment");
		      case '*':
			if ( last == '/' )
			  level++;
			break;
		      case '/':
			if ( last == '*' &&
			     (--level == 0 || _PL_rd->strictness) )
			{ if ( cbuf )
			  { addUTF8Buffer(cbuf, EOS);
			    if ( !add_comment(cbuf, pos, _PL_rd PASS_LD) )
			    { discardBuffer(cbuf);
			      return FALSE;
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
			   !(c == '`' && _PL_rd->backquoted_string) )
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    }
		  }
		  dotseen = FALSE;
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
		  addUTF8Buffer(cbuf, EOS);
		  if ( !add_comment(cbuf, pos, _PL_rd PASS_LD) )
		  { discardBuffer(cbuf);
		    return FALSE;
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
			    dotseen = FALSE;
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
		dotseen = FALSE;
		break;
      case '"':	set_start_line;
                if ( !raw_read_quoted(c, _PL_rd) )
		  fail;
		dotseen = FALSE;
		break;
      case '.': addToBuffer(c, _PL_rd);
		set_start_line;
		dotseen++;
		c = getchr();
		if ( isSymbolW(c) )
		{ while( c != EOF && isSymbolW(c) &&
			 !(c == '`' && _PL_rd->backquoted_string) )
		  { addToBuffer(c, _PL_rd);
		    c = getchr();
		  }
		  dotseen = FALSE;
		}
		goto handle_c;
      case '`': if ( _PL_rd->backquoted_string )
		{ set_start_line;
		  if ( !raw_read_quoted(c, _PL_rd) )
		    fail;
		  dotseen = FALSE;
		  break;
		}
	        /*FALLTHROUGH*/
      default:	if ( c < 0xff )
		{ switch(_PL_char_types[c])
		  { case SP:
		    blank:
		      if ( dotseen )
		      { if ( rb.here - rb.base == 1 )
			  rawSyntaxError("end_of_clause");
			ensure_space(c);
			addToBuffer(EOS, _PL_rd);
			return rb.base;
		      }
		      do
		      { if ( something_read ) /* positions, \0 --> ' ' */
			  addToBuffer(c ? c : ' ', _PL_rd);
			else
			  ensure_space(c);
			c = getchr();
		      } while( c != EOF && PlBlankW(c) );
		      goto handle_c;
		    case SY:
		      set_start_line;
		      do
		      { addToBuffer(c, _PL_rd);
			c = getchr();
			if ( c == '`' && _PL_rd->backquoted_string )
			  break;
		      } while( c != EOF && c <= 0xff && isSymbol(c) );
					/* TBD: wide symbols? */
		      dotseen = FALSE;
		      goto handle_c;
		    case LC:
		    case UC:
		      set_start_line;
		      do
		      { addToBuffer(c, _PL_rd);
			c = getchr();
		      } while( c != EOF && PlIdContW(c) );
		      dotseen = FALSE;
		      goto handle_c;
		    default:
#ifdef O_QUASIQUOTATIONS		/* detect || from {|Syntax||Quotation|} */
		      if ( c == '|' &&
			   rb.here - rb.base >= 1 &&
			   rb.here[-1] == '|' &&
			   truePrologFlag(PLFLAG_QUASI_QUOTES) )
		      { if ( !raw_read_quasi_quotation(c, _PL_rd) )
			  return FALSE;
			dotseen = FALSE;
			break;
		      }
#endif
		      addToBuffer(c, _PL_rd);
		      dotseen = FALSE;
		      set_start_line;
		  }
		} else			/* > 255 */
		{ if ( PlIdStartW(c) )
		  { set_start_line;
		    do
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    } while( c != EOF && PlIdContW(c) );
		    dotseen = FALSE;
		    goto handle_c;
		  } else if ( PlBlankW(c) )
		  { goto blank;
		  } else
		  { addToBuffer(c, _PL_rd);
		    dotseen = FALSE;
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

static unsigned char *
raw_read(ReadData _PL_rd, unsigned char **endp ARG_LD)
{ unsigned char *s;

  if ( (rb.stream->flags & SIO_ISATTY) && Sfileno(rb.stream) >= 0 )
  { ttybuf tab;

    PushTty(rb.stream, &tab, TTY_SAVE);		/* make sure tty is sane */
    PopTty(rb.stream, &ttytab, FALSE);
    s = raw_read2(_PL_rd PASS_LD);
    PopTty(rb.stream, &tab, TRUE);
  } else
  { s = raw_read2(_PL_rd PASS_LD);
  }

  if ( endp )
    *endp = _PL_rd->_rb.here;

  return s;
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

#define MAX_SINGLETONS 256		/* max singletons _reported_ */

#define for_vars(v, code) \
	{ Variable v   = baseBuffer(&var_buffer, struct variable); \
	  Variable _ev = topBuffer(&var_buffer, struct variable); \
	  for( ; v < _ev; v++ ) { code; } \
	}

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

    for_vars(v, v->name += shift);
  }

  return baseBuffer(&var_name_buffer, char) + e;
}

					/* use hash-key? */

static Variable
varInfo(word w, ReadData _PL_rd)
{ if ( tagex(w) == (TAG_VAR|STG_RESERVED) )
    return &baseBuffer(&var_buffer, struct variable)[w>>LMASK_BITS];

  return NULL;
}


static Variable
lookupVariable(const char *name, size_t len, ReadData _PL_rd)
{ struct variable next;
  Variable var;
  size_t nv;

  if ( !isAnonVarNameN(name, len) )		/* always add _ */
  { for_vars(v,
	     if ( len == v->namelen && strncmp(name, v->name, len) == 0 )
	     { v->times++;
	       return v;
	     })
  }

  nv = entriesBuffer(&var_buffer, struct variable);
  next.name      = save_var_name(name, len, _PL_rd);
  next.namelen   = len;
  next.times     = 1;
  next.variable  = 0;
  next.signature = (nv<<LMASK_BITS)|TAG_VAR|STG_RESERVED;
  addBuffer(&var_buffer, next, struct variable);
  var = topBuffer(&var_buffer, struct variable);

  return var-1;
}


static int
warn_singleton(const char *name)	/* Name in UTF-8 */
{ if ( name[0] != '_' )			/* not _*: always warn */
    return TRUE;
  if ( name[1] == '_' )			/* __*: never warn */
    return FALSE;
  if ( name[1] )			/* _a: warn */
  { int c;

    utf8_get_char(&name[1], &c);
    if ( !PlUpperW(c) )
      return TRUE;
  }
  return FALSE;
}


/* is_singleton() is true if var is a singleton.  As quasi quotations
   may insert the (named) variable into the quotation, we must scan
   the quasi quotation list and check that the variable does not appear
   in any of the terms.
*/

#define IS_SINGLETON 0
#define IS_MULTITON  1

static int
is_singleton(Variable var, int type, ReadData _PL_rd ARG_LD)
{ if ( var->times == 1 )
  { if ( (type == IS_SINGLETON &&  warn_singleton(var->name)) ||
	 (type == IS_MULTITON  && !warn_singleton(var->name)) )
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
    return var->times == 1 &&  warn_singleton(var->name);
  else
    return var->times  > 1 && !warn_singleton(var->name);
}


static bool				/* TBD: new schema */
check_singletons(ReadData _PL_rd ARG_LD)
{ if ( _PL_rd->singles != TRUE )	/* returns <name> = var bindings */
  { term_t list = PL_copy_term_ref(_PL_rd->singles);
    term_t head = PL_new_term_ref();

    for_vars(var,
	     if ( is_singleton(var, IS_SINGLETON, _PL_rd PASS_LD) )
	     {	if ( !PL_unify_list(list, head, list) ||
		     !PL_unify_term(head,
				    PL_FUNCTOR,    FUNCTOR_equals2,
				    PL_UTF8_CHARS, var->name,
				    PL_TERM,       var->variable) )
		  fail;
	     });

    return PL_unify_nil(list);
  } else				/* just report */
  { const char *singletons[MAX_SINGLETONS];
    int i = 0;

					/* singletons */
    for_vars(var,
	     if ( is_singleton(var, IS_SINGLETON, _PL_rd PASS_LD) )
	     { if ( i < MAX_SINGLETONS )
		 singletons[i++] = var->name;
	     });

    if ( i > 0 )
    { if ( !singletonWarning("singletons", singletons, i) )
	return FALSE;
    }

    if ( (_PL_rd->styleCheck&MULTITON_CHECK) )
    { i = 0;				/* multiple _X* */
      for_vars(var,
	       if ( is_singleton(var, IS_MULTITON, _PL_rd PASS_LD) )
	       { if ( i < MAX_SINGLETONS )
		   singletons[i++] = var->name;
	       });

      if ( i > 0 )
      { if ( !singletonWarning("multitons", singletons, i) )
	  return FALSE;
      }
    }

    succeed;
  }
}


static bool
bind_variable_names(ReadData _PL_rd ARG_LD)
{ term_t list = PL_copy_term_ref(_PL_rd->varnames);
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();

  for_vars(var,
	   if ( !isAnonVarName(var->name) )
	   { PL_chars_t txt;

	     txt.text.t    = var->name;
	     txt.length    = strlen(var->name);
	     txt.storage   = PL_CHARS_HEAP;
	     txt.encoding  = ENC_UTF8;
	     txt.canonical = FALSE;

	     if ( !PL_unify_list(list, head, list) ||
		  !PL_unify_functor(head, FUNCTOR_equals2) ||
		  !PL_get_arg(1, head, a) ||
		  !PL_unify_text(a, 0, &txt, PL_ATOM) ||
		  !PL_get_arg(2, head, a) ||
		  !PL_unify(a, var->variable) )
	       fail;
	   });

  return PL_unify_nil(list);
}


static bool
bind_variables(ReadData _PL_rd ARG_LD)
{ term_t list = PL_copy_term_ref(_PL_rd->variables);
  term_t head = PL_new_term_ref();

  for_vars(var,
	   if ( !PL_unify_list(list, head, list) ||
		!PL_unify(head, var->variable) )
	     fail;
	   );

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

  return FALSE;
}


static int
parse_quasi_quotations(ReadData _PL_rd ARG_LD)
{ if ( _PL_rd->qq_tail )
  { term_t av;
    int rc;

    if ( !PL_unify_nil(_PL_rd->qq_tail) )
      return FALSE;

    if ( !_PL_rd->quasi_quotations )
    { if ( (av = PL_new_term_refs(2)) &&
	   PL_put_term(av+0, _PL_rd->qq) &&
	   PL_put_atom(av+1, _PL_rd->module->name) &&
	   PL_cons_functor_v(av, FUNCTOR_dparse_quasi_quotations2, av) )
      { term_t ex;

	rc = callProlog(MODULE_system, av+0, PL_Q_CATCH_EXCEPTION, &ex);
	if ( rc )
	  return TRUE;
	_PL_rd->exception = ex;
	_PL_rd->has_exception = TRUE;
      }
      return FALSE;
    } else
      return TRUE;
  } else if ( _PL_rd->quasi_quotations )	/* user option, but no quotes */
  { return PL_unify_nil(_PL_rd->quasi_quotations);
  } else
    return TRUE;
}


static int
is_quasi_quotation_syntax(term_t type, ReadData _PL_rd)
{ GET_LD
  term_t plain = PL_new_term_ref();
  term_t ex;
  Module m = _PL_rd->module;
  atom_t name;
  int arity;

  PL_strip_module(type, &m, plain);

  if ( PL_get_name_arity(plain, &name, &arity) )
  { if ( _PL_rd->quasi_quotations )
    { return TRUE;
    } else
    { Procedure proc;

      if ( (proc=resolveProcedure(PL_new_functor(name, 4), m)) &&
	   true(proc->definition, P_QUASI_QUOTATION_SYNTAX) )
	return TRUE;

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

  return FALSE;
}

#endif /*O_QUASIQUOTATIONS*/


		/********************************
		*           TOKENISER           *
		*********************************/

static inline int
void_allowed(ReadData _PL_rd)
{ int type, priority;

  if ( !_PL_rd->strictness &&
       currentOperator(_PL_rd->module, ATOM_void, OP_POSTFIX, &type, &priority) )
    return TRUE;

  return FALSE;
}


static inline ucharp
skipSpaces(cucharp in)
{ int chr;
  ucharp s;

  for( ; *in; in=s)
  { s = utf8_get_uchar(in, &chr);

    if ( !PlBlankW(chr) )
      return (ucharp)in;
  }

  return (ucharp)in;
}


static inline unsigned char *
SkipIdCont(unsigned char *in)
{ int chr;
  unsigned char *s;

  for( ; *in; in=s)
  { s = (unsigned char*)utf8_get_char((char*)in, &chr);

    if ( !PlIdContW(chr) )
      return in;
  }

  return in;
}


static unsigned char *
SkipSymbol(unsigned char *in, ReadData _PL_rd)
{ int chr;
  unsigned char *s;

  for( ; *in; in=s)
  { s = (unsigned char*)utf8_get_char((char*)in, &chr);

    if ( !PlSymbolW(chr) )
      return in;
    if ( chr == '`' && _PL_rd->backquoted_string )
      return in;
  }

  return in;
}


#define unget_token()	{ unget = TRUE; }


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
      *grouped = TRUE;
    return TRUE;
  }

  return FALSE;
}


static strnumstat
scan_decimal(cucharp *sp, int negative, Number n, int *grouped)
{ int64_t maxi = PLMAXINT/10;
  int maxlastdigit = PLMAXINT % 10;
  int64_t mini = PLMININT/10;
  int minlastdigit = PLMININT % 10;
  int64_t t = 0;
  cucharp s = *sp;
  int c = *s;

  if ( !isDigit(c) )
    return NUM_ERROR;

  *grouped = FALSE;

  do
  { for(c = *s; isDigit(c); c = *++s)
    { if (    (  negative && ( (t < mini) || (t == mini && '0' - c < minlastdigit) ))
           || ( !negative && ( (t > maxi) || (t == maxi && c - '0' > maxlastdigit) )) )
      {
#ifdef O_GMP
	n->value.i = t;
	n->type = V_INTEGER;
	promoteToMPZNumber(n);

	do
	{ for(c = *s; isDigit(c); c = *++s)
	  { mpz_mul_ui(n->value.mpz, n->value.mpz, 10);
            if (negative)
	      mpz_sub_ui(n->value.mpz, n->value.mpz, c - '0');
            else
              mpz_add_ui(n->value.mpz, n->value.mpz, c - '0');
	  }
	} while ( skip_digit_separator(&s, 10, grouped) );

	*sp = s;

	return NUM_OK;
#else
	double maxf =  MAXREAL / 10.0 - 10.0;
	double minf = -MAXREAL / 10.0 + 10.0;
	double tf = (double)t;
	do
        { for(c = *s; isDigit(c); c = *++s)
	  { if (negative)
            { if ( tf < minf )
	        fail;				/* number too large */
              tf = tf * 10.0 - (double)(c - '0');
            } else
            { if ( tf > maxf )
	        fail;				/* number too large */
              tf = tf * 10.0 + (double)(c - '0');
            }
	  }
	} while ( skip_digit_separator(&s, 10, grouped) );
	n->value.f = tf;
	n->type = V_FLOAT;
	*sp = s;
	return NUM_OK;
#endif
      } else
      { if (negative)
          t = t * 10 - c + '0';
        else
          t = t * 10 + c - '0';
      }
    }
  } while ( skip_digit_separator(&s, 10, grouped) );

  *sp = s;

  n->value.i = t;
  n->type = V_INTEGER;
  return NUM_OK;
}


static strnumstat
scan_number(cucharp *s, int negative, int b, Number n)
{ int d;
  int64_t maxi = PLMAXINT/b;		/* cache? */
  int maxlastdigit = PLMAXINT % b;
  int64_t mini = PLMININT/b;
  int minlastdigit = PLMININT % b;
  int64_t t = 0;
  cucharp q = *s;

  if ( (d = digitValue(b, *q)) < 0 )
    return NUM_ERROR;			/* syntax error */

  do
  { while((d = digitValue(b, *q)) >= 0)
    { if (    (  negative && ( (t < mini) || (t == mini && d > minlastdigit) ))
           || ( !negative && ( (t > maxi) || (t == maxi && d > maxlastdigit) )) )
      {
#ifdef O_GMP
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
	    { if ( skipped && (_PL_rd->styleCheck & LONGATOM_CHECK) )
	      { term_t ex;
		unsigned char *old_start = last_token_start;

		last_token_start = errpos;
		ex = makeErrorTerm("swi_backslash_newline", NULL, 0, _PL_rd);
		last_token_start = old_start;
		printMessage(ATOM_warning, PL_TERM, ex);
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
      if ( chr > PLMAXWCHAR )
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
	  if ( chr > PLMAXWCHAR )
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

	  tmp[0] = c;
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
    { addBuffer(b, *p&0xff, char);
    }
  } else
  { addBuffer(b, c, char);
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
    } else if ( c == '\\' && true(_PL_rd, M_CHARESCAPE) )
    { c = escape_char(in, &in, quote, _PL_rd);
      if ( c >= 0 )
      { addUTF8Buffer(buf, c);

	continue;
      } else if ( c == ESC_ERROR )
      { return FALSE;
      } else
      { break;
      }
    } else if ( c >= 0x80 )		/* copy UTF-8 sequence */
    { do
      { addBuffer(buf, c, char);
	c = *in++;
      } while( c > 0x80 );

      goto next;
    } else if ( in > ein )
    { errorWarning("end_of_file_in_string", 0, _PL_rd);
      return FALSE;
    }

    addBuffer(buf, c, char);
  }

  if ( end )
    *end = in;

  return TRUE;
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
	txt.canonical = FALSE;

	rc = PL_unify_text(t, 0, &txt, PL_CODE_LIST);
	PL_free_text(&txt);

	return rc;
      } else
      { return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dquasi_quotation3,
			          PL_POINTER, _PL_rd,
				  PL_INTPTR, (intptr_t)(start-rdbase),
			          PL_INTPTR, (intptr_t)(in-start));
      }
    }
  }

  return errorWarning("end_of_file_in_quasi_quotation", 0, _PL_rd);
}
#endif /*O_QUASIQUOTATIONS*/


strnumstat
str_number(cucharp in, ucharp *end, Number value, int escape)
{ int negative = FALSE;
  cucharp start = in;
  strnumstat rc;
  int grouped;

  if ( *in == '-' )			/* skip optional sign */
  { negative = TRUE;
    in++;
  } else if ( *in == '+' )
    in++;

  if ( *in == '0' )
  { int base = 0;

    switch(in[1])
    { case '\'':			/* 0'<char> */
      { int chr;

	if ( escape && in[2] == '\\' )	/* 0'\n, etc */
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

  if ( (rc=scan_decimal(&in, negative, value, &grouped)) != NUM_OK )
    return rc;				/* too large? */
  if ( grouped )
  { *end = (ucharp)in;
    return NUM_OK;
  }
					/* base'value number */
  if ( *in == '\'' &&
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
  if ( *in == '.' && isDigit(in[1]) )
  { clearNumber(value);
    value->type = V_FLOAT;

    in++;
    while( isDigit(*in) )
      in++;
  }

  if ( (*in == 'e' || *in == 'E') &&
       ((isSign(in[1]) && isDigit(in[2])) || isDigit(in[1])) )
  { clearNumber(value);
    value->type = V_FLOAT;

    in++;
    if ( isSign(*in) )
      in++;
    while( isDigit(*in) )
      in++;
  }

  if ( value->type == V_FLOAT )
  { char *e;

    errno = 0;
    value->value.f = strtod((char*)start, &e);
    if ( e != (char*)in && !(*in == '.' && (char*)in+1 == e) )
      return NUM_ERROR;
    if ( errno == ERANGE && fabs(value->value.f) > 1.0 )
      return NUM_FOVERFLOW;

    *end = (ucharp)in;

    return NUM_OK;
  }

  *end = (ucharp)in;

  return NUM_OK;
}


static void
checkASCII(unsigned char *name, size_t len, const char *type)
{ size_t i;

  for(i=0; i<len; i++)
  { if ( (name[i]) >= 128 )
    { printMessage(ATOM_warning,
		   PL_FUNCTOR_CHARS, "non_ascii", 2,
                   PL_NCHARS, len, (char const *)name,
		     PL_CHARS, type);
      return;
    }
  }
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


static Token
get_token__LD(bool must_be_op, ReadData _PL_rd ARG_LD)
{ int c;
  unsigned char *start;

  if ( unget )
  { unget = FALSE;
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
    if ( PlInvalidW(c) )
      syntaxError("illegal_character", _PL_rd);
    goto case_solo;
  }

  switch(_PL_char_types[c])
  { case LC:
    lower:
		{ PL_chars_t txt;

		  rdhere = SkipIdCont(rdhere);
		symbol:
		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		    checkASCII(start, rdhere-start, "atom");

		functor:
		  txt.text.t    = (char *)start;
		  txt.length    = rdhere-start;
		  txt.storage   = PL_CHARS_HEAP;
		  txt.encoding  = ENC_UTF8;
		  txt.canonical = FALSE;
		  cur_token.value.atom = textToAtom(&txt);
		  NeedUnlock(cur_token.value.atom);
		  PL_free_text(&txt);

		  if ( *rdhere == '(' )
		  { cur_token.type = T_FUNCTOR;
		  } else
		  { cur_token.type = T_NAME;
		  }

		  DEBUG(9, Sdprintf("%s: %s\n",
				    cur_token.type == T_FUNCTOR ? "FUNC" : "NAME",
				    stringAtom(cur_token.value.atom)));

		  break;
		}
    case UC:
    upper:
		{ rdhere = SkipIdCont(rdhere);
		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		    checkASCII(start, rdhere-start, "variable");
		  if ( *rdhere == '(' && truePrologFlag(ALLOW_VARNAME_FUNCTOR) )
		    goto functor;
		  if ( start[0] == '_' &&
		       rdhere == start + 1 &&
		       !_PL_rd->variables ) /* report them */
		  { DEBUG(9, Sdprintf("VOID\n"));
		    cur_token.type = T_VOID;
		  } else
		  { cur_token.value.variable = lookupVariable((char *)start,
							      rdhere-start,
							      _PL_rd);
		    DEBUG(9, Sdprintf("VAR: %s\n",
				      cur_token.value.variable->name));
		    cur_token.type = T_VARIABLE;
		  }

		  break;
		}
    case_digit:
    case DI:	{ number value;
		  strnumstat rc;

		  if ( (rc=str_number(&rdhere[-1], &rdhere, &value,
				      true(_PL_rd, M_CHARESCAPE))) == NUM_OK )
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
		  DEBUG(9, Sdprintf("%s: %s\n",
				  *rdhere == '(' ? "FUNC" : "NAME",
				  stringAtom(cur_token.value.atom)));

		  break;
		}
    case_symbol:
    case SY:	if ( c == '`' && _PL_rd->backquoted_string )
		  goto case_bq;

	        rdhere = SkipSymbol(rdhere, _PL_rd);
		if ( rdhere == start+1 )
		{ if ( (c == '+' || c == '-') &&	/* +- number */
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
		    case '(':
		    case '[':
		      rdhere = skipSpaces(rdhere);
		      if (rdhere[0] == matchingBracket(c))
		      { rdhere++;
			switch(c)
			{ case '{': cur_token.value.atom = ATOM_curl; break;
			  case '[': cur_token.value.atom = ATOM_nil;  break;
			  case '(':
			    if ( void_allowed(_PL_rd) )
			      cur_token.value.atom = ATOM_void;
			    else
			      syntaxError("void_not_allowed", _PL_rd);
			    break;
			}
			cur_token.type = rdhere[0] == '(' ? T_FUNCTOR : T_NAME;
			DEBUG(9, Sdprintf("NAME: %s\n",
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
		  DEBUG(9, Sdprintf("PUNCT: %c\n", cur_token.value.character));

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
		  txt.canonical = FALSE;
		  cur_token.value.atom = textToAtom(&txt);
		  NeedUnlock(cur_token.value.atom);
		  PL_free_text(&txt);
		  cur_token.type = (rdhere[0] == '(' ? T_FUNCTOR : T_NAME);
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
		  txt.canonical = FALSE;
#if O_STRING
		  if ( true(_PL_rd, DBLQ_STRING) )
		    type = PL_STRING;
		  else
#endif
		  if ( true(_PL_rd, DBLQ_ATOM) )
		    type = PL_ATOM;
		  else if ( true(_PL_rd, DBLQ_CHARS) )
		    type = PL_CHAR_LIST;
		  else
		    type = PL_CODE_LIST;

		  if ( !PL_unify_text(t, 0, &txt, type) )
		  { PL_free_text(&txt);
		    return FALSE;
		  }
		  PL_free_text(&txt);
		  cur_token.value.term = t;
		  cur_token.type = T_STRING;
		  discardBuffer(&b);
		  break;
		}
#ifdef O_STRING
    case BQ:
    case_bq:    { tmp_buffer b;
		  term_t t = PL_new_term_ref();
		  PL_chars_t txt;

		  initBuffer(&b);
		  if ( !get_string(rdhere-1, rdend, &rdhere, (Buffer)&b, _PL_rd) )
		    fail;
		  txt.text.t    = baseBuffer(&b, char);
		  txt.length    = entriesBuffer(&b, char);
		  txt.storage   = PL_CHARS_HEAP;
		  txt.encoding  = ENC_UTF8;
		  txt.canonical = FALSE;
		  if ( !PL_unify_text(t, 0, &txt, PL_STRING) )
		  { PL_free_text(&txt);
		    return FALSE;
		  }
		  PL_free_text(&txt);
		  cur_token.value.term = t;
		  cur_token.type = T_STRING;
		  discardBuffer(&b);
		  break;
		}
#endif
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

#define get_token(must_be_op, rd) get_token__LD(must_be_op, rd PASS_LD)


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


static term_t
alloc_term(ReadData _PL_rd ARG_LD)
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

static inline void
readValHandle(term_t term, Word argp, ReadData _PL_rd ARG_LD)
{ word w = *valTermRef(term);
  Variable var;

  if ( (var = varInfo(w, _PL_rd)) )
  { DEBUG(9, Sdprintf("readValHandle(): var at 0x%x\n", var));

    if ( !var->variable )		/* new variable */
    { var->variable = PL_new_term_ref_noshift();
      assert(var->variable);
      setVar(*argp);
      *valTermRef(var->variable) = makeRef(argp);
    } else				/* reference to existing var */
    { *argp = *valTermRef(var->variable);
    }
  } else
    *argp = w;				/* plain value */

  setVar(*valTermRef(term));
}


static int
ensureSpaceForTermRefs(size_t n ARG_LD)
{ size_t bytes = n*sizeof(word);

  if ( addPointer(lTop, bytes) > (void*)lMax )
  { int rc;

    rc = ensureLocalSpace(bytes, ALLOW_SHIFT);
    if ( rc != TRUE )
      return rc;
  }

  return TRUE;
}


/* build_term(name, arity) builds a term from the top arity terms
   on the term-stack and pushes the result back to the term-stack
*/

static int
build_term(atom_t atom, int arity, ReadData _PL_rd ARG_LD)
{ functor_t functor = lookupFunctorDef(atom, arity);
  term_t *av, *argv = term_av(-arity, _PL_rd);
  word w;
  Word argp;
  int rc;

  if ( !hasGlobalSpace(arity+1) &&
       (rc=ensureGlobalSpace(arity+1, ALLOW_GC|ALLOW_SHIFT)) != TRUE )
    return rc;
  if ( (rc=ensureSpaceForTermRefs(arity PASS_LD)) != TRUE )
    return rc;

  DEBUG(9, Sdprintf("Building term %s/%d ... ", stringAtom(atom), arity));
  argp = gTop;
  w = consPtr(argp, TAG_COMPOUND|STG_GLOBAL);
  gTop += 1+arity;
  *argp++ = functor;

  for(av=argv; arity-- > 0; av++, argp++)
    readValHandle(*av, argp, _PL_rd PASS_LD);

  setHandle(argv[0], w);
  truncate_term_stack(&argv[1], _PL_rd);

  DEBUG(9, Sdprintf("result: "); pl_write(argv[0]); Sdprintf("\n") );
  return TRUE;
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
        out_n--;

static intptr_t
get_int_arg(term_t t, int n ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);

  return valInt(argTerm(*p, n-1));
}


static term_t
opPos(op_entry *op, out_entry *args ARG_LD)
{ if ( op->tpos )
  { intptr_t fs = get_int_arg(op->tpos, 1 PASS_LD);
    intptr_t fe = get_int_arg(op->tpos, 2 PASS_LD);
    term_t r;

    if ( !(r=PL_new_term_ref()) )
      return 0;

    if ( op->kind == OP_INFIX )
    { intptr_t s = get_int_arg(args[0].tpos, 1 PASS_LD);
      intptr_t e = get_int_arg(args[1].tpos, 2 PASS_LD);

      if ( !op->isblock )
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_INTPTR, s,
			    PL_INTPTR, e,
			    PL_INTPTR, fs,
			    PL_INTPTR, fe,
			    PL_LIST, 2, PL_TERM, args[0].tpos,
					PL_TERM, args[1].tpos) )
	  return (term_t)0;
      } else
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_INTPTR, s,
			    PL_INTPTR, e,
			    PL_INT, 0,
			    PL_INT, 0,
			    PL_LIST, 3, PL_TERM, op->tpos,
					PL_TERM, args[0].tpos,
					PL_TERM, args[1].tpos) )
	  return (term_t)0;
      }
    } else
    { intptr_t s, e;

      if ( op->kind == OP_PREFIX )
      { s = fs;
	e = get_int_arg(args[0].tpos, 2 PASS_LD);
      } else
      { s = get_int_arg(args[0].tpos, 1 PASS_LD);
	e = fe;
      }

      if ( !op->isblock )
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_INTPTR, s,
			    PL_INTPTR, e,
			    PL_INTPTR, fs,
			    PL_INTPTR, fe,
			      PL_LIST, 1, PL_TERM, args[0].tpos) )
	  return (term_t)0;
      } else
      { if ( !PL_unify_term(r,
			    PL_FUNCTOR,	FUNCTOR_term_position5,
			    PL_INTPTR, s,
			    PL_INTPTR, e,
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


static inline atom_t
op_name(op_entry *e)
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


static int
build_op_term(op_entry *op, ReadData _PL_rd ARG_LD)
{ term_t tmp;
  out_entry *e;
  int arity = (op->kind == OP_INFIX ? 2 : 1);
  int rc;

  if ( !(tmp = PL_new_term_ref()) )
    return FALSE;

  e = out_op(-arity, _PL_rd);
  if ( !op->isblock )
  { if ( (rc = build_term(op->op.atom, arity, _PL_rd PASS_LD)) != TRUE )
      return rc;
  } else
  { term_t term = alloc_term(_PL_rd PASS_LD);
    term_t *av = term_av(-(arity+1), _PL_rd);
    int i;

    for(i=arity; i>0; i--)
      av[i] = av[i-1];
    av[0] = term;
    PL_put_term(term, op->op.block);

    if ( (rc = build_term(op_name(op), arity+1, _PL_rd PASS_LD)) != TRUE )
      return rc;
  }

  e->pri  = op->op_pri;
  e->tpos = opPos(op, e PASS_LD);

  _PL_rd->op.out_queue.top = (char*)(e+1);

  return rc;
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

static int simple_term(Token token, term_t positions, ReadData _PL_rd ARG_LD);


static bool
isOp(op_entry *e, int kind, ReadData _PL_rd)
{ int pri;
  int type;

  if ( !currentOperator(_PL_rd->module, op_name(e), kind, &type, &pri) )
    fail;
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
	side_n++, side_p++;
#define PopOp() \
	pop_side_op(_PL_rd); \
	side_n--, side_p--;
#define SideOp(i) \
	side_op(i, _PL_rd)

#define Modify(cpri) \
	if ( side_n > 0 && rmo == 0 && cpri > SideOp(side_p)->right_pri ) \
	{ op_entry *op = SideOp(side_p); \
	  if ( op->kind == OP_PREFIX && !op->isblock ) \
	  { term_t tmp; \
	    DEBUG(9, Sdprintf("Prefix %s to atom\n", \
			      stringOp(op))); \
	    rmo++; \
	    if ( !(tmp = alloc_term(_PL_rd PASS_LD)) ) return FALSE; \
	    PL_put_atom(tmp, op->op.atom); \
	    queue_out_op(0, op->tpos, _PL_rd); \
	    out_n++; \
	    PopOp(); \
	  } else if ( op->kind == OP_INFIX && out_n > 0 && \
		      isOp(op, OP_POSTFIX, _PL_rd) ) \
	  { int rc; \
	    DEBUG(9, Sdprintf("Infix %s to postfix\n", \
			      stringOp(op))); \
	    rmo++; \
	    rc = build_op_term(op, _PL_rd PASS_LD); \
	    if ( rc != TRUE ) return rc; \
	    PopOp(); \
	  } \
	}


static int
bad_operator(out_entry *out, op_entry *op, ReadData _PL_rd)
{ /*term_t t;*/
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
  { Ssprintf(buf, "Operator `%s' conflicts with `%s'",
	     opname, stringAtom(name));
  }
*/

  syntaxError("operator_clash", _PL_rd);
}


/* can_reduce() returns

	TRUE  if operator can be reduced;
        FALSE if operator can not be reduced;
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
	rc = FALSE;
    }
  } else
    return FALSE;

  if ( rc == FALSE && (cpri) == (OP_MAXPRIORITY+1) )
  { bad_operator(e, op, _PL_rd);
    return -1;
  }

  DEBUG(9, if ( rc ) Sdprintf("Reducing %s/%d\n",
			      stringOp(op), arity));

  return rc;
}


#define Reduce(cpri) \
	while( out_n > 0 && side_n > 0 && (cpri) >= SideOp(side_p)->op_pri ) \
	{ int rc; \
	  rc = can_reduce(SideOp(side_p), cpri, out_n, _PL_rd); \
	  if ( rc == FALSE ) break; \
	  if ( rc < 0 ) return FALSE; \
	  rc = build_op_term(SideOp(side_p), _PL_rd PASS_LD); \
	  if ( rc != TRUE ) return rc; \
	  if ( SideOp(side_p)->kind == OP_INFIX ) out_n--; \
	  PopOp(); \
	}


static int
is_name_token(Token token, int must_be_op, ReadData _PL_rd)
{ switch(token->type)
  { case T_NAME:
      return TRUE;
    case T_FUNCTOR:
      return must_be_op;
    case T_PUNCTUATION:
    { switch(token->value.character)
      { case '[':
	case '{':
	  return TRUE;
	case '(':
	  return FALSE;
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
	  return TRUE;
	case ',':
	  if ( !must_be_op && _PL_rd->strictness > 0 )
	  { errorWarning("quoted_punctuation", 0, _PL_rd);
	    return -1;
	  }
	default:
	  return TRUE;
      }
    }
    default:
      return FALSE;
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
	    e->isblock = TRUE;
	  return ATOM_nil;
	case '{':
	  if ( e )
	    e->isblock = TRUE;
	  return ATOM_curl;
	default:
	  return codeToAtom(token->value.character);
      }
    case T_FULLSTOP:
      need_unlock(0, _PL_rd);
      return ATOM_dot;
    default:
      return token->value.atom;
  }
}


static int
unify_atomic_position(term_t positions, Token token)
{ if ( positions )
  { return PL_unify_term(positions,
			 PL_FUNCTOR, FUNCTOR_minus2,
			   PL_INTPTR, token->start,
			   PL_INTPTR, token->end);
  } else
    return TRUE;
}


static int
unify_string_position(term_t positions, Token token)
{ if ( positions )
  { return PL_unify_term(positions,
			 PL_FUNCTOR, FUNCTOR_string_position2,
			   PL_INTPTR, token->start,
			   PL_INTPTR, token->end);
  } else
    return TRUE;
}


static int
complex_term(const char *stop, short maxpri, term_t positions,
	     ReadData _PL_rd ARG_LD)
{ op_entry  in_op;
  int out_n = 0, side_n = 0;
  int rmo = 0;				/* Rands more than operators */
  int side_p = side_p0(_PL_rd);
  term_t pin;
  Token token;

  if ( _PL_rd->strictness == 0 )
    maxpri = OP_MAXPRIORITY+1;

  in_op.left_pri = 0;
  in_op.right_pri = 0;

  for(;;)
  { int rc;

    if ( positions )
      pin = PL_new_term_ref();
    else
      pin = 0;

    if ( !(token = get_token(rmo == 1, _PL_rd)) )
      return FALSE;

    if ( out_n != 0 || side_n != 0 )	/* Check for end of term */
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

    if ( (rc=is_name_token(token, rmo == 1, _PL_rd)) == TRUE )
    { in_op.isblock     = FALSE;
      in_op.isterm      = FALSE;
      in_op.op.atom     = name_token(token, &in_op, _PL_rd);
      in_op.tpos        = pin;
      in_op.token_start = last_token_start;

      DEBUG(9, Sdprintf("name %s, rmo = %d\n", stringOp(&in_op), rmo));

      if ( rmo == 0 && isOp(&in_op, OP_PREFIX, _PL_rd) )
      { DEBUG(9, Sdprintf("Prefix op: %s\n", stringOp(&in_op)));

      push_op:
	Unlock(in_op.op.atom);		/* ok; part of an operator */
	if ( in_op.isblock )
	{ term_t *top;

	  if ( (rc = simple_term(token, pin, _PL_rd PASS_LD)) != TRUE )
	    return rc;			/* TBD: need cleanup? */
	  top = term_av(-1, _PL_rd);
	  in_op.op.block = PL_new_term_ref();
	  in_op.isterm = TRUE;
	  PL_put_term(in_op.op.block, *top);
	  truncate_term_stack(top, _PL_rd);
	} else
	{ if ( !unify_atomic_position(pin, token) )
	    return FALSE;
	}
	PushOp();

	continue;
      }
      if ( isOp(&in_op, OP_INFIX, _PL_rd) )
      { DEBUG(9, Sdprintf("Infix op: %s\n", stringOp(&in_op)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri);
	  rmo--;
	  goto push_op;
	}
      }
      if ( isOp(&in_op, OP_POSTFIX, _PL_rd) )
      { DEBUG(9, Sdprintf("Postfix op: %s\n", stringOp(&in_op)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri);
	  goto push_op;
	}
      }
    } else if ( rc < 0 )
      return FALSE;

    if ( rmo == 1 )
      syntaxError("operator_expected", _PL_rd);

					/* Read `simple' term */
    rc = simple_term(token, pin, _PL_rd PASS_LD);
    if ( rc != TRUE )
      return rc;

    if ( rmo != 0 )
      syntaxError("operator_expected", _PL_rd);
    rmo++;
    queue_out_op(0, pin, _PL_rd);
    out_n++;
  }

exit:
  unget_token();			/* the full-stop or punctuation */
  Modify(maxpri);
  Reduce(maxpri);

  if ( out_n == 1 && side_n == 0 )	/* simple term */
  { out_entry *e = out_op(-1, _PL_rd);
    int rc;

    if ( positions && (rc=PL_unify(positions, e->tpos)) != TRUE )
      return rc;
    PopOut();

    return TRUE;
  }

  if ( out_n == 0 && side_n == 1 )	/* single operator */
  { op_entry *op = SideOp(side_p);
    term_t term = alloc_term(_PL_rd PASS_LD);
    int rc;

    if ( !op->isblock )
      PL_put_atom(term, op->op.atom);
    else
      PL_put_term(term, op->op.block);

    if ( positions && (rc=PL_unify(positions, op->tpos)) != TRUE )
      return rc;

    PopOp();

    return TRUE;
  }

  if ( side_n == 1 && !SideOp(0)->isblock &&
       ( SideOp(0)->op.atom == ATOM_comma ||
	 SideOp(0)->op.atom == ATOM_semicolon
       ))
  { term_t ex;

    LD->exception.processing = TRUE;

    if ( (ex = PL_new_term_ref()) &&
	 PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_punct2,
		         PL_ATOM, SideOp(side_p)->op.atom,
		         PL_ATOM, name_token(token, NULL, _PL_rd)) )
      return errorWarning(NULL, ex, _PL_rd);

    return FALSE;
  }

  syntaxError("operator_balance", _PL_rd);
}


static void
set_range_position(term_t positions, intptr_t start, intptr_t end ARG_LD)
{ Word p = valTermRef(positions);

  deRef(p);
  p = argTermP(*p, 0);
  if ( start >= 0 ) p[0] = consInt(start);
  if ( end   >= 0 ) p[1] = consInt(end);
}


static void
swap_functor_position(term_t positions, intptr_t *sp, intptr_t *ep ARG_LD)
{ Word p = valTermRef(positions);
  intptr_t s, e;

  deRef(p);
  p = argTermP(*p, 0);
  s = valInt(p[2]);
  e = valInt(p[3]);
  p[2] = consInt(*sp);
  p[3] = consInt(*ep);
  *sp = s;
  *ep = e;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_list(-positions)  reads  a  list  and  places    the  list  on  the
term-stack. Token is the [-token. The idea was that moving this function
out of simple_term() would reduce the  stack-usage of simple_term() when
not reading a list, but  this   assumption  appears wrong: when inlined,
simple_term() uses less stack. Why is that?  Nevertheless, this is a lot
more readable and makes selecting between inlining and not trivial.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline int
read_list(Token token, term_t positions, ReadData _PL_rd ARG_LD)
{ term_t term, tail, *tmp;
  term_t pv;

#define P_ELEM (pv+0)			/* pos of element */
#define P_LIST (pv+1)			/* position list */
#define P_TAIL (pv+2)			/* position of tail */

  if ( !(tail = PL_new_term_ref()) )
    return FALSE;

  if ( positions )
  { if ( !(pv = PL_new_term_refs(3)) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_list_position4,
			PL_INTPTR, token->start,
			PL_VARIABLE,
			PL_TERM, P_LIST,
			PL_TERM, P_TAIL) )
      return FALSE;
  } else
    pv = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reading a list. Tmp is used to  read   the  next element. Tail is a very
special term-ref. It is always a reference   to the place where the next
term is to be written.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  term = alloc_term(_PL_rd PASS_LD);
  PL_put_term(tail, term);

  for(;;)
  { int rc;
    Word argp;

    if ( positions )
    { if ( !PL_unify_list(P_LIST, P_ELEM, P_LIST) )
	return FALSE;
    }

    rc = complex_term(",|]", 999, P_ELEM, _PL_rd PASS_LD);
    if ( rc != TRUE )
      return rc;
    if ( (rc=ensureSpaceForTermRefs(2 PASS_LD)) != TRUE )
      return rc;
    if ( !hasGlobalSpace(3) &&
         (rc=ensureGlobalSpace(3, ALLOW_GC)) != TRUE )
      return rc;
    argp = gTop;
    gTop += 3;
    *unRef(*valTermRef(tail)) = consPtr(argp,
					TAG_COMPOUND|STG_GLOBAL);
    *argp++ = FUNCTOR_dot2;
    setVar(argp[0]);
    setVar(argp[1]);
    tmp = term_av(-1, _PL_rd);
    readValHandle(tmp[0], argp++, _PL_rd PASS_LD);
    truncate_term_stack(tmp, _PL_rd);
    setHandle(tail, makeRef(argp));

    token = get_token(FALSE, _PL_rd);

    switch(token->value.character)
    { case ']':
	{ if ( positions )
	  { set_range_position(positions, -1, token->end PASS_LD);
	    if ( !PL_unify_nil(P_LIST) ||
		 !PL_unify_atom(P_TAIL, ATOM_none) )
	      return FALSE;
	  }
	  return PL_unify_nil(tail);
	}
      case '|':
	{ int rc;
	  term_t pt = (pv ? P_TAIL : 0);

	  if ( (rc=complex_term(",|]", 999, pt, _PL_rd PASS_LD)) != TRUE )
	    return rc;
	  argp = unRef(*valTermRef(tail));
	  tmp = term_av(-1, _PL_rd);
	  readValHandle(tmp[0], argp, _PL_rd PASS_LD);
	  truncate_term_stack(tmp, _PL_rd);
	  token = get_token(FALSE, _PL_rd); /* discard ']' */
	  switch(token->value.character)
	  { case ',':
	    case '|':
	      syntaxError("list_rest", _PL_rd);
	  }
	  if ( positions )
	  { set_range_position(positions, -1, token->end PASS_LD);
	    if ( !PL_unify_nil(P_LIST) )
	      return FALSE;
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


static inline int				/* read {...} */
read_brace_term(Token token, term_t positions, ReadData _PL_rd ARG_LD)
{ int rc;
  term_t pa;

  if ( positions )
  { if ( !(pa = PL_new_term_ref()) ||
	 !PL_unify_term(positions,
			PL_FUNCTOR, FUNCTOR_brace_term_position3,
			PL_INTPTR, token->start,
			PL_VARIABLE,
			PL_TERM, pa) )
      return FALSE;
  } else
    pa = 0;

  if ( (rc=complex_term("}", OP_MAXPRIORITY+1, pa, _PL_rd PASS_LD)) != TRUE )
    return rc;
  token = get_token(FALSE, _PL_rd);
  if ( positions )
    set_range_position(positions, -1, token->end PASS_LD);

  return build_term(ATOM_curl, 1, _PL_rd PASS_LD);
}


static inline int				/* read (...) */
read_embraced_term(Token token, term_t positions, ReadData _PL_rd ARG_LD)
{ int rc;

  rc = complex_term(")", OP_MAXPRIORITY+1, positions, _PL_rd PASS_LD);
  if ( rc != TRUE )
    return rc;
  token = get_token(FALSE, _PL_rd);	/* skip ')' */

  succeed;
}


static inline int				/* read f(a1, ...) */
read_compound(Token token, term_t positions, ReadData _PL_rd ARG_LD)
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
			PL_INTPTR, token->start,
			PL_VARIABLE,
			PL_INTPTR, token->start,
			PL_INTPTR, token->end,
			PL_TERM, P_ARG) )
      return FALSE;
  } else
    pv = 0;

  functor = token->value.atom;
  unlock = (_PL_rd->locked == functor);
  _PL_rd->locked = 0;

  if ( (token=get_token(FALSE, _PL_rd)) &&
       token->type == T_NAME &&
       token->value.atom == ATOM_void )
  { term_t term;

    if ( positions )
    { intptr_t fs = token->start, fe = token->end;

      set_range_position(positions, -1, token->end PASS_LD);
      swap_functor_position(positions, &fs, &fe PASS_LD);
      if ( !PL_unify_term(P_ARG, PL_LIST, 1,
			  PL_FUNCTOR, FUNCTOR_minus2,
			    PL_INTPTR, fs,
			    PL_INTPTR, fe) )
	return FALSE;
    }

    term = alloc_term(_PL_rd PASS_LD);
    PL_put_atom(term, functor);
    rc = build_term(ATOM_void, 1, _PL_rd PASS_LD);
    if ( unlock )
      PL_unregister_atom(functor);

    return rc;
  }

  do
  { if ( positions )
    { if ( !PL_unify_list(P_ARG, P_HEAD, P_ARG) )
	return FALSE;
    }
    if ( (rc=complex_term(",)", 999, P_HEAD, _PL_rd PASS_LD)) != TRUE )
    { if ( unlock )
	PL_unregister_atom(functor);
      return rc;
    }
    arity++;
    token = get_token(FALSE, _PL_rd);	/* `,' or `)' */
  } while(token->value.character == ',');

  if ( positions )
  { set_range_position(positions, -1, token->end PASS_LD);
    if ( !PL_unify_nil(P_ARG) )
      return FALSE;
  }

#undef P_HEAD
#undef P_ARG

  rc = build_term(functor, arity, _PL_rd PASS_LD);
  if ( rc != TRUE )
    return rc;
  if ( unlock )
    PL_unregister_atom(functor);

  succeed;
}


/* simple_term() reads a term and leaves it on the top of the term-stack

Token is the first token of the term.
*/

static int
simple_term(Token token, term_t positions, ReadData _PL_rd ARG_LD)
{ switch(token->type)
  { case T_FULLSTOP:
      syntaxError("end_of_clause", _PL_rd);
    case T_VOID:
      alloc_term(_PL_rd PASS_LD);
      /* nothing to do; term is already a variable */
      return unify_atomic_position(positions, token);
    case T_VARIABLE:
    { term_t term = alloc_term(_PL_rd PASS_LD);
      setHandle(term, token->value.variable->signature);
      DEBUG(9, Sdprintf("Pushed var at 0x%x\n", token->value.variable));
      return unify_atomic_position(positions, token);
    }
    case T_NAME:
    { term_t term = alloc_term(_PL_rd PASS_LD);
      PL_put_atom(term, token->value.atom);
      Unlock(token->value.atom);
      return unify_atomic_position(positions, token);
    }
    case T_NUMBER:
    { term_t term = alloc_term(_PL_rd PASS_LD);
      if ( !_PL_put_number(term, &token->value.number) )
	return FALSE;
      clearNumber(&token->value.number);
      return unify_atomic_position(positions, token);
    }
    case T_STRING:
    { term_t term = alloc_term(_PL_rd PASS_LD);
      PL_put_term(term, token->value.term);
      return unify_string_position(positions, token);
    }
    case T_FUNCTOR:
      return read_compound(token, positions, _PL_rd PASS_LD);
    case T_PUNCTUATION:
    { switch(token->value.character)
      { case '(':
	  return read_embraced_term(token, positions, _PL_rd PASS_LD);
	case '{':
	  return read_brace_term(token, positions, _PL_rd PASS_LD);
	case '[':
	  return read_list(token, positions, _PL_rd PASS_LD);
	default:
	{ term_t term = alloc_term(_PL_rd PASS_LD);
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

      result = alloc_term(_PL_rd PASS_LD);

				/* prepare (if we are the first in term) */
      if ( !_PL_rd->varnames )
	_PL_rd->varnames = PL_new_term_ref();
      if ( !_PL_rd->qq )
      { if ( _PL_rd->quasi_quotations )
	{ _PL_rd->qq = _PL_rd->quasi_quotations;
	} else
	{ if ( !(_PL_rd->qq = PL_new_term_ref()) )
	    return FALSE;
	}

	if ( !(_PL_rd->qq_tail = PL_copy_term_ref(_PL_rd->qq)) )
	  return FALSE;
      }

					/* allocate for quasi_quotation/4 */
      if ( !(av=PL_new_term_refs(4)) )
	return FALSE;

      if ( positions )
      { if ( !(pv = PL_new_term_refs(3)) ||
	     !PL_unify_term(positions,
			    PL_FUNCTOR, FUNCTOR_quasi_quotation_position5,
			      PL_INTPTR, token->start,
			      PL_VARIABLE,
			      PL_TERM, pv+0,
			      PL_TERM, pv+1,
			      PL_TERM, pv+2) )
	  return FALSE;
      } else
	pv = 0;
						/* push type */
      rc = complex_term("|", OP_MAXPRIORITY+1,
			positions ? pv+1 : 0,
			_PL_rd PASS_LD);
      if ( rc != TRUE )
	return rc;
      token = get_token(FALSE, _PL_rd);		/* get the '|' */
      if ( token->type != T_QQ_BAR )
	syntaxError("double_bar_expected", _PL_rd);

      argv = term_av(-1, _PL_rd);
      PL_put_term(av+0, argv[0]);		/* Arg 0: the type */
      truncate_term_stack(argv, _PL_rd);
      if ( !is_quasi_quotation_syntax(av, _PL_rd) )
	return FALSE;
						/* Arg 1: the content */
      if ( !get_quasi_quotation(av+1, &rdhere, rdend, _PL_rd) )
	return FALSE;

      if ( positions )
      { intptr_t qqend = source_char_no + ptr_to_pos(rdhere, _PL_rd);

	if ( !PL_unify(pv+0, av+0) )
	  return FALSE;
	set_range_position(positions, -1, qqend PASS_LD);
	if ( !PL_unify_term(pv+2,
			    PL_FUNCTOR, FUNCTOR_minus2,
			      PL_INTPTR, token->end,	/* end of | token */
			      PL_INTPTR, qqend-2) )     /* end minus "|}" */
	  return FALSE;
      }

      PL_put_term(av+2, _PL_rd->varnames);	/* Arg 2: the var dictionary */
      if ( !PL_unify(av+3, result) )		/* Arg 3: the result */
        return FALSE;

      if ( !PL_cons_functor_v(av+0, FUNCTOR_quasi_quotation4, av) )
        return FALSE;

      if ( !(t = PL_new_term_ref()) ||
	   !PL_unify_list(_PL_rd->qq_tail, t, _PL_rd->qq_tail) ||
	   !PL_unify(t, av+0) )
	return FALSE;

      return TRUE;
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

static int
instantiate_template(term_t term, term_t at_term ARG_LD)
{ term_t template, substitutions, head, var, value;

  if ( !(template = PL_new_term_ref()) ||
       !(substitutions = PL_new_term_ref()) ||
       !(head = PL_new_term_ref()) ||
       !(var = PL_new_term_ref()) ||
       !(value = PL_new_term_ref()) )
    return FALSE;

  _PL_get_arg(1, at_term, template);
  _PL_get_arg(2, at_term, substitutions);
  if ( !PL_unify(term, template) )
    return FALSE;
  if ( !PL_is_list(substitutions) )
    return PL_error(NULL, 0, "invalid template",
		    ERR_TYPE, ATOM_list, substitutions);
  while( PL_get_list(substitutions, head, substitutions) )
  { if ( PL_is_functor(head, FUNCTOR_equals2) )
    { _PL_get_arg(1, head, var);
      _PL_get_arg(2, head, value);
      if ( !PL_unify(var, value) )
	return FALSE;
    } else
    { return PL_error(NULL, 0, "invalid template",
		      ERR_TYPE, ATOM_equal, head);
    }
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_term(?term, ReadData rd)
    Common part of all read variations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
read_term(term_t term, ReadData rd ARG_LD)
{ int rc2, rc = FALSE;
  term_t *result;
  Token token;
  Word p;
  fid_t fid;

  if ( !(rd->base = raw_read(rd, &rd->end PASS_LD)) )
    fail;

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

  rd->here = rd->base;
  rd->strictness = truePrologFlag(PLFLAG_ISO);
  if ( (rc2=complex_term(NULL, OP_MAXPRIORITY+1,
			 rd->subtpos, rd PASS_LD)) != TRUE )
  { rc = raiseStackOverflow(rc2);
    goto out;
  }
  assert(rd->term_stack.top == 1);
  result = term_av(-1, rd);
  p = valTermRef(result[0]);
  if ( varInfo(*p, rd) )		/* reading a single variable */
  { if ( (rc2=ensureSpaceForTermRefs(1 PASS_LD)) != TRUE )
    { rc = raiseStackOverflow(rc2);
      goto out;
    }
    p = valTermRef(result[0]);		/* may be shifted */
    readValHandle(result[0], p, rd PASS_LD);
  }

  if ( !(token = get_token(FALSE, rd)) )
    goto out;
  if ( token->type != T_FULLSTOP )
  { errorWarning("end_of_clause_expected", 0, rd);
    goto out;
  }

  if ( rd->cycles && PL_is_functor(result[0], FUNCTOR_xpceref2) )
    rc = instantiate_template(term, result[0] PASS_LD);
  else
    rc = PL_unify(term, result[0]);

  truncate_term_stack(result, rd);
  if ( !rc )
    goto out;
  if ( rd->varnames && !(rc=bind_variable_names(rd PASS_LD)) )
    goto out;
#ifdef O_QUASIQUOTATIONS
  if ( !(rc=parse_quasi_quotations(rd PASS_LD)) )
    goto out;
#endif
  if ( rd->variables && !(rc=bind_variables(rd PASS_LD)) )
    goto out;
  if ( rd->singles && !(rc=check_singletons(rd PASS_LD)) )
    goto out;

  rc = TRUE;

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
    if ( !PlBlankW(chr) )
      return (unsigned char*)end;
  }

  return (unsigned char *)start;
}


word
pl_raw_read2(term_t from, term_t term)
{ GET_LD
  unsigned char *s, *e, *t2, *top;
  read_data rd;
  word rval;
  IOSTREAM *in;
  int chr;
  PL_chars_t txt;

  if ( !getTextInputStream(from, &in) )
    fail;

  init_read_data(&rd, in PASS_LD);
  if ( !(s = raw_read(&rd, &e PASS_LD)) )
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
  top = backSkipBlanks(s, e-1);
  t2 = backSkipUTF8(s, top, &chr);
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
  txt.canonical = FALSE;

  rval = PL_unify_text(term, 0, &txt, PL_ATOM);
  free_read_data(&rd);

  return rval;
}


word
pl_raw_read(term_t term)
{ return pl_raw_read2(0, term);
}


word
pl_read2(term_t from, term_t term)
{ GET_LD
  read_data rd;
  int rval;
  IOSTREAM *s;

  if ( !getTextInputStream(from, &s) )
    fail;

  init_read_data(&rd, s PASS_LD);
  rval = read_term(term, &rd PASS_LD);
  if ( rd.has_exception )
    rval = PL_raise_exception(rd.exception);
  free_read_data(&rd);

  if ( Sferror(s) )
    return streamStatus(s);
  else
    PL_release_stream(s);

  return rval;
}


word
pl_read(term_t term)
{ return pl_read2(0, term);
}


static int
unify_read_term_position(term_t tpos ARG_LD)
{ if ( tpos && source_line_no > 0 )
  { return PL_unify_term(tpos,
			 PL_FUNCTOR, FUNCTOR_stream_position4,
			   PL_INT64, source_char_no,
			   PL_INT, source_line_no,
			   PL_INT, source_line_pos,
			   PL_INT64, source_byte_no);
  } else
  { return TRUE;
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

static const opt_spec read_clause_options[] =
{ { ATOM_variable_names,    OPT_TERM },
  { ATOM_term_position,	    OPT_TERM },
  { ATOM_subterm_positions, OPT_TERM },
  { ATOM_process_comment,   OPT_BOOL },
  { ATOM_comments,	    OPT_TERM },
  { ATOM_syntax_errors,     OPT_ATOM },
  { NULL_ATOM,		    0 }
};


static void
callCommentHook(predicate_t comment_hook,
		term_t comments, term_t tpos, term_t term)
{ GET_LD
  fid_t fid;
  term_t av;

  if ( (fid = PL_open_foreign_frame()) &&
       (av = PL_new_term_refs(3)) )
  { qid_t qid;

    PL_put_term(av+0, comments);
    PL_put_term(av+1, tpos);
    PL_put_term(av+2, term);

    if ( (qid = PL_open_query(NULL, PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION,
			      comment_hook, av)) )
    { term_t ex;

      if ( !PL_next_solution(qid) && (ex=PL_exception(qid)) )
	printMessage(ATOM_error, PL_TERM, ex);

      PL_close_query(qid);
    }

    PL_discard_foreign_frame(fid);
  }
}


int
read_clause(IOSTREAM *s, term_t term, term_t options ARG_LD)
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
  process_comment = (comment_hook->definition->impl.any != NULL);

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

retry:
  init_read_data(&rd, s PASS_LD);

  if ( options &&
       !scan_options(options, 0, ATOM_read_option, read_clause_options,
		     &rd.varnames,
		     &tpos,
		     &rd.subtpos,
		     &process_comment,
		     &opt_comments,
		     &syntax_errors) )
  { PL_close_foreign_frame(fid);
    return FALSE;
  }

  if ( opt_comments )
  { comments = PL_new_term_ref();
  } else if ( process_comment )
  { if ( !tpos )
      tpos = PL_new_term_ref();
    comments = PL_new_term_ref();
  }

  rd.module = LD->modules.source;
  if ( comments )
    rd.comments = PL_copy_term_ref(comments);
  rd.on_error = syntax_errors;
  rd.singles = rd.styleCheck & SINGLETON_CHECK ? TRUE : FALSE;
  if ( (rval=read_term(term, &rd PASS_LD)) &&
       (!tpos || (rval=unify_read_term_position(tpos PASS_LD))) )
  { if ( rd.comments &&
	 (rval = PL_unify_nil(rd.comments)) )
    { if ( opt_comments )
	rval = PL_unify(opt_comments, comments);
      else if ( !PL_get_nil(comments) )
	callCommentHook(comment_hook, comments, tpos, term);
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


static
PRED_IMPL("read_clause", 3, read_clause, 0)
{ PRED_LD
  int rc;
  IOSTREAM *s;

  if ( !getTextInputStream(A1, &s) )
    return FALSE;
  rc = read_clause(s, A2, A3 PASS_LD);
  if ( Sferror(s) )
    return streamStatus(s);
  else
    PL_release_stream(s);

  return rc;
}


static const opt_spec read_term_options[] =
{ { ATOM_variable_names,    OPT_TERM },
  { ATOM_variables,         OPT_TERM },
  { ATOM_singletons,        OPT_TERM },
  { ATOM_term_position,     OPT_TERM },
  { ATOM_subterm_positions, OPT_TERM },
  { ATOM_character_escapes, OPT_BOOL },
  { ATOM_double_quotes,	    OPT_ATOM },
  { ATOM_module,	    OPT_ATOM },
  { ATOM_syntax_errors,     OPT_ATOM },
  { ATOM_backquoted_string, OPT_BOOL },
  { ATOM_comments,	    OPT_TERM },
#ifdef O_QUASIQUOTATIONS
  { ATOM_quasi_quotations,  OPT_TERM },
#endif
  { ATOM_cycles,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};


static foreign_t
read_term_from_stream(IOSTREAM *s, term_t term, term_t options ARG_LD)
{ term_t tpos = 0;
  term_t tcomments = 0;
  int rval;
  atom_t w;
  read_data rd;
  bool charescapes = -1;
  atom_t dq = NULL_ATOM;
  atom_t mname = NULL_ATOM;
  fid_t fid = PL_open_foreign_frame();

retry:
  init_read_data(&rd, s PASS_LD);

  if ( !scan_options(options, 0, ATOM_read_option, read_term_options,
		     &rd.varnames,
		     &rd.variables,
		     &rd.singles,
		     &tpos,
		     &rd.subtpos,
		     &charescapes,
		     &dq,
		     &mname,
		     &rd.on_error,
		     &rd.backquoted_string,
		     &tcomments,
#ifdef O_QUASIQUOTATIONS
		     &rd.quasi_quotations,
#endif
		     &rd.cycles) )
    return FALSE;

  if ( mname )
  { rd.module = lookupModule(mname);
    rd.flags  = rd.module->flags;
  }

  if ( charescapes != -1 )
  { if ( charescapes )
      set(&rd, M_CHARESCAPE);
    else
      clear(&rd, M_CHARESCAPE);
  }
  if ( dq )
  { if ( !setDoubleQuotes(dq, &rd.flags) )
      return FALSE;
  }
  if ( rd.singles && PL_get_atom(rd.singles, &w) && w == ATOM_warning )
    rd.singles = TRUE;
  if ( tcomments )
    rd.comments = PL_copy_term_ref(tcomments);

  rval = read_term(term, &rd PASS_LD);
  if ( Sferror(s) )
    return FALSE;

  if ( rval )
  { if ( tpos )
      rval = unify_read_term_position(tpos PASS_LD);
    if ( rval && tcomments )
    { if ( !PL_unify_nil(rd.comments) )
	rval = FALSE;
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
  { if ( read_term_from_stream(s, A2, A3 PASS_LD) )
      return PL_release_stream(s);
    if ( Sferror(s) )
      return streamStatus(s);
    PL_release_stream(s);
    return FALSE;
  }

  return FALSE;
}


/** read_term(-Term, +Options) is det.
*/

static
PRED_IMPL("read_term", 2, read_term, PL_FA_ISO)
{ PRED_LD
  IOSTREAM *s;

  if ( getTextInputStream(0, &s) )
  { if ( read_term_from_stream(s, A1, A2 PASS_LD) )
      return PL_release_stream(s);
    if ( Sferror(s) )
      return streamStatus(s);
    PL_release_stream(s);
    return FALSE;
  }

  return FALSE;
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

  if ( PL_get_text(A1, &txt, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { int rc;
    IOSTREAM *stream;
    source_location oldsrc = LD->read_source;

    if ( (stream = Sopen_text(&txt, "r")) )
    { rc = read_term_from_stream(stream, A2, A3 PASS_LD);
      Sclose(stream);
    } else
      rc = FALSE;

    LD->read_source = oldsrc;

    return rc;
  }

  return FALSE;
}


static int
atom_to_term(term_t atom, term_t term, term_t bindings)
{ GET_LD
  PL_chars_t txt;

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
      txt.canonical = FALSE;
      rval = PL_unify_text(atom, 0, &txt, PL_ATOM);
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

    init_read_data(&rd, stream PASS_LD);
    if ( bindings && (PL_is_variable(bindings) || PL_is_list(bindings)) )
      rd.varnames = bindings;
    else if ( bindings )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, bindings);

    if ( !(rval = read_term(term, &rd PASS_LD)) && rd.has_exception )
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
{ return atom_to_term(A1, A2, A3);
}


static
PRED_IMPL("term_to_atom", 2, term_to_atom, 0)
{ return atom_to_term(A2, A1, 0);
}


int
PL_chars_to_term(const char *s, term_t t)
{ GET_LD
  read_data rd;
  int rval;
  IOSTREAM *stream = Sopen_string(NULL, (char *)s, -1, "r");
  source_location oldsrc = LD->read_source;

  init_read_data(&rd, stream PASS_LD);
  PL_put_variable(t);
  if ( !(rval = read_term(t, &rd PASS_LD)) && rd.has_exception )
    PL_put_term(t, rd.exception);
  free_read_data(&rd);
  Sclose(stream);
  LD->read_source = oldsrc;

  return rval;
}


int
PL_wchars_to_term(const wchar_t *s, term_t t)
{ GET_LD
  int rc;
  IOSTREAM *stream;
  PL_chars_t text;

  text.text.w    = (pl_wchar_t *)s;
  text.encoding  = ENC_WCHAR;
  text.storage   = PL_CHARS_HEAP;
  text.length    = wcslen(s);
  text.canonical = FALSE;

  if ( (stream = Sopen_text(&text, "r")) )
  { read_data rd;

    source_location oldsrc = LD->read_source;
    init_read_data(&rd, stream PASS_LD);
    PL_put_variable(t);
    if ( !(rc = read_term(t, &rd PASS_LD)) && rd.has_exception )
      PL_put_term(t, rd.exception);
    free_read_data(&rd);
    Sclose(stream);
    LD->read_source = oldsrc;
  } else
    rc = FALSE;

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

  if ( !PL_get_char_ex(A1, &code, FALSE) ||
       !PL_get_atom_ex(A2, &class) )
    return FALSE;

  if ( code > 0x10ffff )
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

  return rc ? TRUE : FALSE;
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
  PRED_DEF("$code_class",	  2, code_class,	  0)
#ifdef O_QUASIQUOTATIONS
  PRED_DEF("$qq_open",            2, qq_open,             0)
#endif
EndPredDefs
