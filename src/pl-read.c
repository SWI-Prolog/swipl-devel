/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam

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
/*#define O_DEBUG 1*/
#include "pl-incl.h"
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
static strnumstat scan_decimal(cucharp *sp, Number n);


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

typedef struct token * Token;

typedef struct
{ char *	name;		/* Name of the variable */
  size_t	namelen;	/* length of the name */
  term_t	variable;	/* Term-reference to the variable */
  int		times;		/* Number of occurences */
  word		signature;	/* Pseudo atom */
} variable, *Variable;


struct token
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
};


#define FASTBUFFERSIZE	256	/* read quickly upto this size */

struct read_buffer
{ int	size;			/* current size of read buffer */
  unsigned char *base;		/* base of read buffer */
  unsigned char *here;		/* current position in read buffer */
  unsigned char *end;		/* end of the valid buffer */
  IOSTREAM *stream;		/* stream we are reading from */
  unsigned char fast[FASTBUFFERSIZE];	/* Quick internal buffer */
};


struct var_table
{ buffer _var_name_buffer;	/* stores the names */
  buffer _var_buffer;		/* array of struct variables */
};

#define T_FUNCTOR	0	/* name of a functor (atom, followed by '(') */
#define T_NAME		1	/* ordinary name */
#define T_VARIABLE	2	/* variable name */
#define T_VOID		3	/* void variable */
#define T_NUMBER	4	/* integer or float */
#define T_STRING	5	/* "string" */
#define T_PUNCTUATION	6	/* punctuation character */
#define T_FULLSTOP	7	/* Prolog end of clause */

#define E_SILENT	0	/* Silently fail */
#define E_EXCEPTION	1	/* Generate an exception */
#define E_PRINT		2	/* Print to Serror */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bundle all data required by the  various   passes  of read into a single
structure.  This  makes  read  truly  reentrant,  fixing  problems  with
interrupt and XPCE  call-backs  as   well  as  transparently  supporting
multi-threading.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ unsigned char *here;			/* current character */
  unsigned char *base;			/* base of clause */
  unsigned char *end;			/* end of the clause */
  unsigned char *token_start;		/* start of most recent read token */
  struct token token;			/* current token */
  bool _unget;				/* unget_token() */

  unsigned char *posp;			/* position pointer */
  size_t	posi;			/* position number */

  Module	module;			/* Current source module */
  unsigned int	flags;			/* Module syntax flags */
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
  int		strictness;		/* Strictness level */

  atom_t	locked;			/* atom that must be unlocked */
  struct var_table vt;			/* Data about variables */
  struct read_buffer _rb;		/* keep read characters here */
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

static void
init_read_data(ReadData _PL_rd, IOSTREAM *in ARG_LD)
{ memset(_PL_rd, 0, sizeof(*_PL_rd));	/* optimise! */

  initBuffer(&var_name_buffer);
  initBuffer(&var_buffer);
  _PL_rd->exception = PL_new_term_ref();
  rb.stream = in;
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


#define DO_CHARESCAPE true(_PL_rd, CHARESCAPE)


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


#define syntaxError(what, rd) { errorWarning(what, 0, rd); fail; }

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


static term_t
makeErrorTerm(const char *id_str, term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex, loc=0;			/* keep compiler happy */
  unsigned char const *s, *ll = NULL;
  int rc = TRUE;

  if ( !(ex = PL_new_term_ref()) ||
       !(loc = PL_new_term_ref()) )
    rc = FALSE;

  if ( rc && !id_term )
  { if ( !(id_term=PL_new_term_ref()) ||
	 !PL_put_atom_chars(id_term, id_str) )
      rc = FALSE;
  }

  if ( rc )
    rc = PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_FUNCTOR, FUNCTOR_syntax_error1,
		           PL_TERM, id_term,
		         PL_TERM, loc);

  source_char_no += last_token_start - rdbase;
  for(s=rdbase; s<last_token_start; s++)
  { if ( *s == '\n' )
    { source_line_no++;
      ll = s+1;
    }
  }

  if ( ll )
  { int lp = 0;

    for(s = ll; s<last_token_start; s++)
    { switch(*s)
      { case '\b':
	  if ( lp > 0 ) lp--;
	  break;
	case '\t':
	  lp |= 7;
	default:
	  lp++;
      }
    }

    source_line_pos = lp;
  }

  if ( rc )
  { if ( ReadingSource )			/* reading a file */
    { rc = PL_unify_term(loc,
			 PL_FUNCTOR, FUNCTOR_file4,
			   PL_ATOM, source_file_name,
			   PL_INT, source_line_no,
			   PL_INT, source_line_pos,
			   PL_INT64, source_char_no);
    } else if ( isStringStream(rb.stream) )
    { size_t pos;

      pos = utf8_strlen((char *)rdbase, last_token_start-rdbase);

      rc = PL_unify_term(loc,
			 PL_FUNCTOR, FUNCTOR_string2,
			   PL_UTF8_STRING, rdbase,
			   PL_INT, (int)pos);
    } else				/* any stream */
    { term_t stream;

      if ( !(stream=PL_new_term_ref()) ||
	   !PL_unify_stream_or_alias(stream, rb.stream) ||
	   !PL_unify_term(loc,
			  PL_FUNCTOR, FUNCTOR_stream4,
			    PL_TERM, stream,
			    PL_INT, source_line_no,
			    PL_INT, source_line_pos,
			    PL_INT64, source_char_no) )
	rc = FALSE;
    }
  }

  return (rc ? ex : (term_t)0);
}



static bool
errorWarning(const char *id_str, term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex;

  LD->exception.processing = TRUE;	/* allow using spare stack */

  ex = makeErrorTerm(id_str, id_term, _PL_rd);

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
		   PL_TERM,    l);

    PL_discard_foreign_frame(fid);

    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	FAIL	return false
	TRUE	redo
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
reportReadError(ReadData rd)
{ if ( rd->on_error == ATOM_error )
    return PL_raise_exception(rd->exception);
  if ( rd->on_error == ATOM_quiet )
    fail;

  printMessage(ATOM_error, PL_TERM, rd->exception);

  if ( rd->on_error == ATOM_dec10 )
    succeed;

  fail;
}


		/********************************
		*           RAW READING         *
		*********************************/


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
{ if ( rb.base == rb.fast )		/* intptr_t clause: jump to use malloc() */
  { rb.base = PL_malloc(FASTBUFFERSIZE * 2);
    memcpy(rb.base, rb.fast, FASTBUFFERSIZE);
  } else
    rb.base = PL_realloc(rb.base, rb.size*2);

  DEBUG(8, Sdprintf("Reallocated read buffer at %ld\n", (intptr_t) rb.base));
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
setCurrentSourceLocation(IOSTREAM *s ARG_LD)
{ atom_t a;

  if ( s->position )
  { source_line_no  = s->position->lineno;
    source_line_pos = s->position->linepos - 1;	/* char just read! */
    source_char_no  = s->position->charno - 1;	/* char just read! */
  } else
  { source_line_no  = -1;
    source_line_pos = -1;
    source_char_no  = 0;
  }

  if ( (a = fileNameStream(s)) )
    source_file_name = a;
  else
    source_file_name = NULL_ATOM;
}


static inline int
getchr__(ReadData _PL_rd)
{ int c = Sgetcode(rb.stream);

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
			 { setCurrentSourceLocation(rb.stream PASS_LD); \
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
  { if ( c == '\\' && DO_CHARESCAPE )
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
  if (c == EOF)
  { eofinstr:
      rawSyntaxError("end_of_file_in_string");
  }
  addToBuffer(c, _PL_rd);

  return TRUE;
}


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

    source_char_no = pos->charno;
    source_line_pos = pos->linepos;
    source_line_no = pos->lineno;
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
		  if ( bs > rb.base && isSign(bs[-1]) )
		    bs--;

		  if ( bs == rb.base || !PlIdContW(bs[-1]) )
		  { int base;

		    if ( isSign(bs[0]) )
		      bs++;
		    rb.here[0] = EOS;
		    base = atoi((char*)bs);
		    if ( base <= 36 )
		    { if ( base == 0 )			/* 0'<c> */
		      { addToBuffer(c, _PL_rd);
			{ if ( (c=getchr()) != EOF )
			  { addToBuffer(c, _PL_rd);
			    if ( c == '\\' ) 		/* 0'\<c> */
			    { if ( (c=getchr()) != EOF )
				addToBuffer(c, _PL_rd);
			    } else if ( c == '\'' ) 	/* 0'' */
			    { if ( (c=getchr()) != EOF )
			      { if ( c == '\'' )
				  addToBuffer(c, _PL_rd);
				else
				  goto handle_c;
			      }
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
building the term on the global stack, variables are represented using a
reference to a `struct variable'. The first  part of this structure is a
struct atom, so if a garbage   collection happens, the garbage collector
will simply assume an atom.

The buffer `var_buffer' is a  list  of   pointers  to  a  stock of these
variable structures. This list is dynamically expanded if necessary. The
buffer var_name_buffer contains the  actual   strings.  They  are packed
together to avoid memory fragmentation. This   buffer too is reallocated
if necessary. In this  case,  the   pointers  of  the  existing variable
structures are relocated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_SINGLETONS 256

#define for_vars(v, code) \
	{ Variable v   = baseBuffer(&var_buffer, variable); \
	  Variable _ev = topBuffer(&var_buffer, variable); \
	  for( ; v < _ev; v++ ) { code; } \
	}

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
isVarAtom(word w, ReadData _PL_rd)
{ if ( tagex(w) == (TAG_VAR|STG_RESERVED) )
    return &baseBuffer(&var_buffer, variable)[w>>7];

  return NULL;
}


static Variable
lookupVariable(const char *name, size_t len, ReadData _PL_rd)
{ variable next;
  Variable var;
  size_t nv;

  if ( name[0] != '_' || name[1] != EOS ) /* anonymous: always add */
  { for_vars(v,
	     if ( len == v->namelen && strncmp(name, v->name, len) == 0 )
	     { v->times++;
	       return v;
	     })
  }

  nv = entriesBuffer(&var_buffer, variable);
  next.name      = save_var_name(name, len, _PL_rd);
  next.namelen   = len;
  next.times     = 1;
  next.variable  = 0;
  next.signature = (nv<<7)|TAG_VAR|STG_RESERVED;
  addBuffer(&var_buffer, next, variable);
  var = topBuffer(&var_buffer, variable);

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


static bool				/* TBD: new schema */
check_singletons(ReadData _PL_rd ARG_LD)
{ if ( _PL_rd->singles != TRUE )	/* returns <name> = var bindings */
  { term_t list = PL_copy_term_ref(_PL_rd->singles);
    term_t head = PL_new_term_ref();

    for_vars(var,
	     if ( var->times == 1 && warn_singleton(var->name) )
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
	     if ( var->times == 1 && warn_singleton(var->name) )
	     { if ( i < MAX_SINGLETONS )
		 singletons[i++] = var->name;
	     });

    if ( i > 0 )
    { if ( !singletonWarning("singletons", singletons, i) )
	return FALSE;
    }

    i = 0;				/* multiple _X* */
    for_vars(var,
	     if ( var->times > 1 && !warn_singleton(var->name) )
	     { if ( i < MAX_SINGLETONS )
		 singletons[i++] = var->name;
	     });

    if ( i > 0 )
    { if ( !singletonWarning("multitons", singletons, i) )
	return FALSE;
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
	   if ( !(var->name[0] == '_' && !var->name[1]) )
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


		/********************************
		*           TOKENISER           *
		*********************************/

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

#ifndef O_GMP
static double
uint64_to_double(uint64_t i)
{
#ifdef __WINDOWS__
  int64_t s = (int64_t)i;
  if ( s >= 0 )
    return (double)s;
  else
    return (double)s + 18446744073709551616.0;
#else
  return (double)i;
#endif
}
#endif


static strnumstat
scan_decimal(cucharp *sp, Number n)
{ uint64_t maxi = PLMAXINT/10;
  uint64_t t = 0;
  cucharp s = *sp;
  int c = *s;

  if ( !isDigit(c) )
    return NUM_ERROR;

  for(; isDigit(c); c = *++s)
  { if ( t > maxi || t * 10 + c - '0' > PLMAXINT )
    {
#ifdef O_GMP
      n->value.i = (int64_t)t;
      n->type = V_INTEGER;
      promoteToMPZNumber(n);

      for(c = *s; isDigit(c); c = *++s)
      { mpz_mul_ui(n->value.mpz, n->value.mpz, 10);
	mpz_add_ui(n->value.mpz, n->value.mpz, c - '0');
      }
      *sp = s;

      return NUM_OK;
#else
      char *end;

      while(isDigit(*s))
	s++;
      errno = 0;
      n->value.f = strtod(*sp, &end);
      if ( s != end )
	return NUM_ERROR;
      if ( errno == ERANGE )
	return NUM_FOVERFLOW;
      *sp = s;

      n->type = V_FLOAT;
      return NUM_OK;
#endif
    } else
      t = t * 10 + c - '0';
  }

  *sp = s;

  n->value.i = t;
  n->type = V_INTEGER;
  return NUM_OK;
}


static strnumstat
scan_number(cucharp *s, int b, Number n)
{ int d;
  uint64_t maxi = PLMAXINT/b;		/* cache? */
  uint64_t t;
  cucharp q = *s;

  if ( (d = digitValue(b, *q)) < 0 )
    return NUM_ERROR;			/* syntax error */
  t = d;
  q++;

  while((d = digitValue(b, *q)) >= 0)
  { if ( t > maxi || t * b + d > PLMAXINT )
    {
#ifdef O_GMP
      n->value.i = (int64_t)t;
      n->type = V_INTEGER;
      promoteToMPZNumber(n);

      while((d = digitValue(b, *q)) >= 0)
      { q++;
	mpz_mul_ui(n->value.mpz, n->value.mpz, b);
	mpz_add_ui(n->value.mpz, n->value.mpz, d);
      }
      *s = q;

      return NUM_OK;
#else
      double maxf = MAXREAL / (double) b - (double) b;
      double tf = uint64_to_double(t);

      tf = tf * (double)b + (double)d;
      while((d = digitValue(b, *q)) >= 0)
      { q++;
        if ( tf > maxf )
	  fail;				/* number too large */
        tf = tf * (double)b + (double)d;
      }
      n->value.f = tf;
      n->type = V_FLOAT;
      *s = q;
      return NUM_OK;
#endif
    } else
    { q++;
      t = t * b + d;
    }
  }

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
      if ( quote )
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
    case '\n':				/* \LF<blank>* */
      if ( quote )
      { e = in;
	for( ; *in; in=e )
	{ e = utf8_get_uchar(in, &c);
	  if ( c == '\n' || !PlBlankW(c) )
	    break;
	}
	goto skip_cont;
      }
      OK('\n');
    case 'e':
      OK(27);				/* 27 is ESC (\e is a gcc extension) */
    case 'f':
      OK('\f');
    case '\\':
      OK('\\');
    case 'n':
      OK('\n');
    case 'r':
      OK('\r');
    case 't':
      OK('\t');
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
      { in--;
	OK('x');
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
      } else
	OK(c);
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
    } else if ( c == '\\' && DO_CHARESCAPE )
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


static void
neg_number(Number n)
{ switch(n->type)
  { case V_INTEGER:
      if ( n->value.i == PLMININT )
      {
#ifdef O_GMP
	promoteToMPZNumber(n);
	mpz_neg(n->value.mpz, n->value.mpz);
#else
	n->type = V_FLOAT;
	n->value.f = -(double)n->value.i;
#endif
      } else
      { n->value.i = -n->value.i;
      }
      break;
#ifdef O_GMP
    case V_MPZ:
      mpz_neg(n->value.mpz, n->value.mpz);
      break;
    case V_MPQ:
      assert(0);			/* are not read directly */
#endif
    case V_FLOAT:
      n->value.f = -n->value.f;
  }
}


strnumstat
str_number(cucharp in, ucharp *end, Number value, int escape)
{ int negative = FALSE;
  cucharp start = in;
  strnumstat rc;

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
	{ chr = escape_char(in+3, end, 0, NULL);
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

      if ( (rc = scan_number(&in, base, value)) != NUM_OK )
	return rc;
      *end = (ucharp)in;
      if ( negative )
	neg_number(value);

      return NUM_OK;
    }
  }

  if ( (rc=scan_decimal(&in, value)) != NUM_OK )
    return rc;				/* too large? */

					/* base'value number */
  if ( *in == '\'' &&
       value->type == V_INTEGER &&
       value->value.i <= 36 &&
       value->value.i > 1 &&
       digitValue(value->value.i, in[1]) >= 0 )
  { in++;

    if ( !(rc=scan_number(&in, (int)value->value.i, value)) )
      return rc;			/* number too large */

    if ( negative )
      neg_number(value);

    *end = (ucharp)in;

    return NUM_OK;
  }
					/* floating point numbers */
  if ( *in == '.' && isDigit(in[1]) )
  { value->type = V_FLOAT;

    in++;
    while( isDigit(*in) )
      in++;
  }

  if ( (*in == 'e' || *in == 'E') &&
       ((isSign(in[1]) && isDigit(in[2])) || isDigit(in[1])) )
  { value->type = V_FLOAT;

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
    if ( e != (char*)in )
      return NUM_ERROR;
    if ( errno == ERANGE && abs(value->value.f) > 1.0 )
      return NUM_FOVERFLOW;

    *end = (ucharp)in;

    return NUM_OK;
  }

  if ( negative )
    neg_number(value);

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

		  cur_token.type = (*rdhere == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n", c == '(' ? "FUNC" : "NAME",
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

		  if ( (rc=str_number(&rdhere[-1],
				      &rdhere, &value, DO_CHARESCAPE)) == NUM_OK )
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
		  if ( c == '.' && PlBlankW(*rdhere) )	/* .<blank> */
		  { cur_token.type = T_FULLSTOP;
		    break;
		  }
		}

		goto symbol;
    case PU:	{ switch(c)
		  { case '{':
		    case '[':
		      while( isBlank(*rdhere) )
			rdhere++;
		      if (rdhere[0] == matchingBracket(c))
		      { rdhere++;
			cur_token.value.atom = (c == '[' ? ATOM_nil : ATOM_curl);
			cur_token.type = rdhere[0] == '(' ? T_FUNCTOR : T_NAME;
			DEBUG(9, Sdprintf("NAME: %s\n",
					  stringAtom(cur_token.value.atom)));
			goto out;
		      }
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

  if ( (var = isVarAtom(w, _PL_rd)) )
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


static int
build_term(term_t term, atom_t atom, int arity, term_t *argv,
	   ReadData _PL_rd ARG_LD)
{ functor_t functor = lookupFunctorDef(atom, arity);
  Word argp;
  int rc;

  if ( !hasGlobalSpace(arity+1) &&
       (rc=ensureGlobalSpace(arity+1, ALLOW_GC|ALLOW_SHIFT)) != TRUE )
    return rc;
  if ( (rc=ensureSpaceForTermRefs(arity PASS_LD)) != TRUE )
    return rc;

  DEBUG(9, Sdprintf("Building term %s/%d ... ", stringAtom(atom), arity));
  argp = gTop;
  gTop += 1+arity;
  setHandle(term, consPtr(argp, TAG_COMPOUND|STG_GLOBAL));
  *argp++ = functor;

  for( ; arity-- > 0; argv++, argp++)
    readValHandle(*argv, argp, _PL_rd PASS_LD);

  DEBUG(9, Sdprintf("result: "); pl_write(term); Sdprintf("\n") );
  return TRUE;
}


static void
PL_assign_term(term_t to, term_t from ARG_LD)
{ *valTermRef(to) = *valTermRef(from);
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

static int
simple_term(bool must_be_op, term_t term, bool *name,
	    term_t positions,
	    ReadData _PL_rd ARG_LD);

typedef struct
{ atom_t op;				/* Name of the operator */
  short	kind;				/* kind (prefix/postfix/infix) */
  short	left_pri;			/* priority at left-hand */
  short	right_pri;			/* priority at right hand */
  short	op_pri;				/* priority of operator */
  term_t tpos;				/* term-position */
  unsigned char *token_start;		/* start of the token for message */
} op_entry;


typedef struct
{ term_t term;				/* the term */
  term_t tpos;				/* its term-position */
  int	 pri;				/* priority of the term */
} out_entry;


static bool
isOp(atom_t atom, int kind, op_entry *e, ReadData _PL_rd)
{ int pri;
  int type;

  if ( !currentOperator(_PL_rd->module, atom, kind, &type, &pri) )
    fail;
  e->op     = atom;
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

static int
build_op_term(term_t term,
	      atom_t atom, int arity, out_entry *argv,
	      ReadData _PL_rd ARG_LD)
{ term_t av[2];

  av[0] = argv[0].term;
  av[1] = argv[1].term;
  return build_term(term, atom, arity, av, _PL_rd PASS_LD);
}


static bool
outOfCStack(ReadData _PL_rd)
{ GET_LD
  term_t ex;

  LD->exception.processing = TRUE;
  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_resource_error1,
		     PL_CHARS, "c_stack") )
    return errorWarning(NULL, ex, _PL_rd);

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
realloca() maintains a buffer using  alloca()   that  has  the requested
size. The current size is  maintained  in   a  intptr_t  just  below the
returned area. This is a intptr_t, to   ensure  proper allignment of the
remainder of the values.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define realloca(p, unit, n) \
	{ intptr_t *ip = (intptr_t *)(p); \
	  if ( ip == NULL || ip[-1] < (n) ) \
	  { intptr_t nsize = (((n)+(n)/2) + 3) & ~3; \
	    intptr_t *np = alloca(nsize * unit + sizeof(intptr_t)); \
	    if ( !np ) return outOfCStack(_PL_rd); \
	    *np++ = nsize; \
	    if ( ip ) \
	      memcpy(np, ip, ip[-1] * unit); \
	    p = (void *)np; \
	  } \
	}

#define PushOp() \
	realloca(side, sizeof(*side), side_n+1); \
	side[side_n++] = in_op; \
	side_p = (side_n == 1 ? 0 : side_p+1);

#define Modify(cpri) \
	if ( side_p >= 0 && cpri > side[side_p].right_pri ) \
	{ term_t tmp; \
	  if ( side[side_p].kind == OP_PREFIX && rmo == 0 ) \
	  { DEBUG(9, Sdprintf("Prefix %s to atom\n", \
			      stringAtom(side[side_p].op))); \
	    rmo++; \
	    tmp = PL_new_term_ref(); \
	    PL_put_atom(tmp, side[side_p].op); \
	    realloca(out, sizeof(*out), out_n+1); \
	    out[out_n].term = tmp; \
	    out[out_n].tpos = side[side_p].tpos; \
	    out[out_n].pri = 0; \
	    out_n++; \
	    side_n--; \
	    side_p = (side_n == 0 ? -1 : side_p-1); \
	  } else if ( side[side_p].kind == OP_INFIX && out_n > 0 && rmo == 0 && \
		      isOp(side[side_p].op, OP_POSTFIX, &side[side_p], _PL_rd) ) \
	  { int rc; \
	    DEBUG(9, Sdprintf("Infix %s to postfix\n", \
			      stringAtom(side[side_p].op))); \
	    rmo++; \
	    tmp = PL_new_term_ref(); \
	    rc = build_op_term(tmp, side[side_p].op, 1, &out[out_n-1], _PL_rd PASS_LD); \
	    if ( rc != TRUE ) return rc; \
	    out[out_n-1].pri  = side[side_p].op_pri; \
	    out[out_n-1].term = tmp; \
	    side[side_p].kind = OP_POSTFIX; \
	    out[out_n-1].tpos = opPos(&side[side_p], &out[out_n-1] PASS_LD); \
	    side_n--; \
	    side_p = (side_n == 0 ? -1 : side_p-1); \
	  } \
	}


static long
get_int_arg(term_t t, int n ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);

  return (long)valInt(argTerm(*p, n-1));
}


static term_t
opPos(op_entry *op, out_entry *args ARG_LD)
{ if ( op->tpos )
  { long fs = get_int_arg(op->tpos, 1 PASS_LD);
    long fe = get_int_arg(op->tpos, 2 PASS_LD);
    term_t r = PL_new_term_ref();

    if ( op->kind == OP_INFIX )
    { long s = get_int_arg(args[0].tpos, 1 PASS_LD);
      long e = get_int_arg(args[1].tpos, 2 PASS_LD);

      if ( !PL_unify_term(r,
			  PL_FUNCTOR,	FUNCTOR_term_position5,
			  PL_LONG, s,
			  PL_LONG, e,
			  PL_LONG, fs,
			  PL_LONG, fe,
			  PL_LIST, 2, PL_TERM, args[0].tpos,
		    		      PL_TERM, args[1].tpos) )
	return (term_t)0;
    } else
    { long s, e;

      if ( op->kind == OP_PREFIX )
      { s = fs;
	e = get_int_arg(args[0].tpos, 2 PASS_LD);
      } else
      { s = get_int_arg(args[0].tpos, 1 PASS_LD);
	e = fe;
      }

      if ( !PL_unify_term(r,
			  PL_FUNCTOR,	FUNCTOR_term_position5,
			  PL_LONG, s,
			  PL_LONG, e,
			  PL_LONG, fs,
			  PL_LONG, fe,
			    PL_LIST, 1, PL_TERM, args[0].tpos) )
	return (term_t)0;
    }

    return r;
  }

  return 0;
}


static int
can_reduce(out_entry *out, op_entry *op)
{ int rval;

  switch(op->kind)
  { case OP_PREFIX:
      rval = op->right_pri >= out[0].pri;
      break;
    case OP_POSTFIX:
      rval = op->left_pri >= out[0].pri;
      break;
    case OP_INFIX:
      rval = op->left_pri >= out[0].pri &&
	     op->right_pri >= out[1].pri;
      break;
    default:
      assert(0);
      rval = FALSE;
  }

  return rval;
}

static int
bad_operator(out_entry *out, op_entry *op, ReadData _PL_rd)
{ /*term_t t;*/
  char *opname = stringAtom(op->op);

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

#define Reduce(cpri) \
	while( out_n > 0 && side_p >= 0 && (cpri) >= side[side_p].op_pri ) \
	{ int rc; \
	  int arity = (side[side_p].kind == OP_INFIX ? 2 : 1); \
	  term_t tmp; \
	  if ( arity > out_n ) break; \
	  if ( !can_reduce(&out[out_n-arity], &side[side_p]) ) \
          { if ( (cpri) == (OP_MAXPRIORITY+1) ) \
              return bad_operator(&out[out_n-arity], &side[side_p], _PL_rd); \
	    break; \
	  } \
	  DEBUG(9, Sdprintf("Reducing %s/%d\n", \
			    stringAtom(side[side_p].op), arity));\
	  tmp = PL_new_term_ref(); \
	  out_n -= arity; \
	  rc = build_op_term(tmp, side[side_p].op, arity, &out[out_n], _PL_rd PASS_LD); \
	  if ( rc != TRUE ) return rc; \
	  out[out_n].pri  = side[side_p].op_pri; \
	  out[out_n].term = tmp; \
	  out[out_n].tpos = opPos(&side[side_p], &out[out_n] PASS_LD); \
	  out_n ++; \
	  side_n--; \
	  side_p = (side_n == 0 ? -1 : side_p-1); \
	}


static int
complex_term(const char *stop, short maxpri, term_t term, term_t positions,
	     ReadData _PL_rd ARG_LD)
{ out_entry *out  = NULL;
  op_entry  *side = NULL;
  op_entry  in_op;
  int out_n = 0, side_n = 0;
  int rmo = 0;				/* Rands more than operators */
  int side_p = -1;
  term_t pin;
  int thestop;				/* encountered stop-character */

  if ( _PL_rd->strictness == 0 )
    maxpri = OP_MAXPRIORITY+1;

  in_op.left_pri = 0;
  in_op.right_pri = 0;

  for(;;)
  { int rc;
    bool isname;
    Token token;
    term_t in = PL_new_term_ref();

    if ( positions )
      pin = PL_new_term_ref();
    else
      pin = 0;

    if ( out_n != 0 || side_n != 0 )	/* Check for end of term */
    { if ( !(token = get_token(rmo == 1, _PL_rd)) )
	fail;
      unget_token();			/* only look-ahead! */

      switch(token->type)
      { case T_FULLSTOP:
	  if ( stop == NULL )
	  { thestop = '.';
	    goto exit;
	  }
	  break;
	case T_PUNCTUATION:
	{ if ( stop != NULL && strchr(stop, token->value.character) )
	  { thestop = token->value.character;
	    goto exit;
	  }
	}
      }
    }

					/* Read `simple' term */
    rc = simple_term(rmo == 1, in, &isname, pin, _PL_rd PASS_LD);
    if ( rc != TRUE )
      return rc;

    if ( isname )			/* Check for operators */
    { atom_t name;

      PL_get_atom(in, &name);
      in_op.tpos = pin;
      in_op.token_start = last_token_start;

      DEBUG(9, Sdprintf("name %s, rmo = %d\n", stringAtom(name), rmo));

      if ( rmo == 0 && isOp(name, OP_PREFIX, &in_op, _PL_rd) )
      { DEBUG(9, Sdprintf("Prefix op: %s\n", stringAtom(name)));

	PushOp();

	continue;
      }
      if ( isOp(name, OP_INFIX, &in_op, _PL_rd) )
      { DEBUG(9, Sdprintf("Infix op: %s\n", stringAtom(name)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri);
	  PushOp();
	  rmo--;

	  continue;
	}
      }
      if ( isOp(name, OP_POSTFIX, &in_op, _PL_rd) )
      { DEBUG(9, Sdprintf("Postfix op: %s\n", stringAtom(name)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri);
	  PushOp();

	  continue;
	}
      }
    }

    if ( rmo != 0 )
      syntaxError("operator_expected", _PL_rd);
    rmo++;
    realloca(out, sizeof(*out), out_n+1);
    out[out_n].pri = 0;
    out[out_n].term = in;
    out[out_n++].tpos = pin;
  }

exit:
  Modify(maxpri);
  Reduce(maxpri);

  if ( out_n == 1 && side_n == 0 )	/* simple term */
  { PL_assign_term(term, out[0].term PASS_LD);
    if ( positions )
      return PL_unify(positions, out[0].tpos);
    succeed;
  }

  if ( out_n == 0 && side_n == 1 )	/* single operator */
  { PL_put_atom(term, side[0].op);
    if ( positions )
      return PL_unify(positions, side[0].tpos);
    succeed;
  }

  if ( side_n == 1 &&
       ( side[0].op == ATOM_comma ||
	 side[0].op == ATOM_semicolon
       ))
  { term_t ex;
    char tmp[2];

    tmp[0] = thestop;
    tmp[1] = EOS;

    LD->exception.processing = TRUE;

    if ( (ex = PL_new_term_ref()) &&
	 PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_punct2,
		         PL_ATOM, side[0].op,
		         PL_CHARS, tmp) )
      return errorWarning(NULL, ex, _PL_rd);

    return FALSE;
  }

  syntaxError("operator_balance", _PL_rd);
}


static int
simple_term(bool must_be_op, term_t term, bool *name,
	    term_t positions, ReadData _PL_rd ARG_LD)
{ Token token;

  *name = FALSE;

  if ( !(token = get_token(must_be_op, _PL_rd)) )
    fail;

  switch(token->type)
  { case T_FULLSTOP:
      syntaxError("end_of_clause", _PL_rd);
    case T_VOID:
      if ( must_be_op )
      { not_an_op:
	syntaxError("operator_expected", _PL_rd);
      }
      setHandle(term, 0L);		/* variable */
      goto atomic_out;
    case T_VARIABLE:
      if ( must_be_op ) goto not_an_op;
      setHandle(term, token->value.variable->signature);
      DEBUG(9, Sdprintf("Pushed var at 0x%x\n", token->value.variable));
      goto atomic_out;
    case T_NAME:
      *name = TRUE;
      PL_put_atom(term, token->value.atom);
      Unlock(token->value.atom);
      goto atomic_out;
    case T_NUMBER:
      if ( must_be_op ) goto not_an_op;
      if ( !_PL_put_number(term, &token->value.number) )
	return FALSE;
      clearNumber(&token->value.number);
    atomic_out:
      if ( positions )
      { if ( !PL_unify_term(positions,
			    PL_FUNCTOR, FUNCTOR_minus2,
			    PL_INTPTR, token->start,
			    PL_INTPTR, token->end) )
	  return FALSE;
      }
      succeed;
    case T_STRING:
      if ( must_be_op ) goto not_an_op;
      PL_put_term(term,	token->value.term);
      if ( positions )
      { if ( !PL_unify_term(positions,
			    PL_FUNCTOR, FUNCTOR_string_position2,
			    PL_INTPTR, token->start,
			    PL_INTPTR, token->end) )
	  return FALSE;
      }
      succeed;
    case T_FUNCTOR:
      { if ( must_be_op )
	{ *name = TRUE;
	  PL_put_atom(term, token->value.atom);
	  Unlock(token->value.atom);
	  goto atomic_out;
	} else
	{ term_t av[16];
	  int avn = 16;
	  term_t *argv = av;
	  int argc;
	  atom_t functor;
	  term_t pa;			/* argument */
	  term_t pe;			/* term-end */
	  term_t ph;
	  int unlock;
	  int rc;

	  if ( positions )
	  { if ( !(pa = PL_new_term_ref()) ||
		 !(pe = PL_new_term_ref()) ||
		 !(ph = PL_new_term_ref()) ||
		 !PL_unify_term(positions,
				PL_FUNCTOR, FUNCTOR_term_position5,
				PL_INTPTR, token->start,
				PL_TERM, pe,
				PL_INTPTR, token->start,
				PL_INTPTR, token->end,
				PL_TERM, pa) )
	      return FALSE;
	  } else
	    pa = pe = ph = 0;

	  functor = token->value.atom;
	  unlock = (_PL_rd->locked == functor);
	  _PL_rd->locked = 0;
	  argc = 0, argv;
	  get_token(must_be_op, _PL_rd); /* skip '(' */

	  do
	  { if ( argc == avn )
	    { term_t *nargv = alloca(sizeof(term_t) * avn * 2);
	      memcpy(nargv, argv, sizeof(term_t) * avn);
	      avn *= 2;
	      argv = nargv;
	    }
	    if ( !(argv[argc] = PL_new_term_ref()) )
	      return FALSE;
	    if ( positions )
	    { if ( !PL_unify_list(pa, ph, pa) )
		return FALSE;
	    }
	    if ( (rc=complex_term(",)", 999, argv[argc], ph, _PL_rd PASS_LD)) != TRUE )
	    { if ( unlock )
		PL_unregister_atom(functor);
	      return rc;
	    }
	    argc++;
	    token = get_token(must_be_op, _PL_rd); /* `,' or `)' */
	  } while(token->value.character == ',');

	  if ( positions )
	  { if ( !PL_unify_integer(pe, token->end) ||
		 !PL_unify_nil(pa) )
	      return FALSE;
	  }

	  rc = build_term(term, functor, argc, argv, _PL_rd PASS_LD);
	  if ( rc != TRUE )
	    return rc;
	  if ( unlock )
	    PL_unregister_atom(functor);
	}
	succeed;
      }
    case T_PUNCTUATION:
      { switch(token->value.character)
	{ case '(':
	    { int rc;
	      size_t start = token->start;

	      if ( must_be_op ) goto not_an_op;

	      rc = complex_term(")", OP_MAXPRIORITY+1, term, positions, _PL_rd PASS_LD);
	      if ( rc != TRUE )
		return rc;
	      token = get_token(must_be_op, _PL_rd);	/* skip ')' */

	      if ( positions )
	      { Word p = argTermP(*valTermRef(positions), 0);

		*p++ = consInt(start);
		*p   = consInt(token->end);
	      }

	      succeed;
	    }
	  case '{':
	    { int rc;
	      term_t arg;
	      term_t pa, pe;

	      if ( must_be_op ) goto not_an_op;
	      if ( !(arg = PL_new_term_ref()) )
		return FALSE;

	      if ( positions )
	      { if ( !(pa = PL_new_term_ref()) ||
		     !(pe = PL_new_term_ref()) ||
		     !PL_unify_term(positions,
				    PL_FUNCTOR, FUNCTOR_brace_term_position3,
				    PL_INTPTR, token->start,
				    PL_TERM, pe,
				    PL_TERM, pa) )
		  return FALSE;
	      } else
		pe = pa = 0;

	      if ( (rc=complex_term("}", OP_MAXPRIORITY+1, arg, pa, _PL_rd PASS_LD)) != TRUE )
		return rc;
	      token = get_token(must_be_op, _PL_rd);
	      if ( positions )
	      { if ( !PL_unify_integer(pe, token->end) )
		  return FALSE;
	      }

	      return build_term(term, ATOM_curl, 1, &arg, _PL_rd PASS_LD);
	    }
	  case '[':
	    { term_t tail, tmp;
	      term_t pa, pe, pt, p2;

	      if ( must_be_op ) goto not_an_op;
	      if ( !(tail = PL_new_term_ref()) ||
		   !(tmp  = PL_new_term_ref()) )
		return FALSE;

	      if ( positions )
	      { if ( !(pa = PL_new_term_ref()) ||
		     !(pe = PL_new_term_ref()) ||
		     !(pt = PL_new_term_ref()) ||
		     !(p2 = PL_new_term_ref()) ||
		     !PL_unify_term(positions,
				    PL_FUNCTOR, FUNCTOR_list_position4,
				    PL_INTPTR, token->start,
				    PL_TERM, pe,
				    PL_TERM, pa,
				    PL_TERM, pt) )
		  return FALSE;
	      } else
		pa = pe = p2 = pt = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reading a list. Tmp is used to  read   the  next element. Tail is a very
special term-ref. It is always a reference   to the place where the next
term is to be written.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	      PL_put_term(tail, term);

	      for(;;)
	      { int rc;
		Word argp;

		if ( positions )
		{ if ( !PL_unify_list(pa, p2, pa) )
		    return FALSE;
		}

		rc = complex_term(",|]", 999, tmp, p2, _PL_rd PASS_LD);
		if ( rc != TRUE )
		  return rc;
		if ( (rc=ensureSpaceForTermRefs(2 PASS_LD)) != TRUE )
		  return rc;
		argp = allocGlobal(3);
		*unRef(*valTermRef(tail)) = consPtr(argp,
						    TAG_COMPOUND|STG_GLOBAL);
		*argp++ = FUNCTOR_dot2;
		setVar(argp[0]);
		setVar(argp[1]);
		readValHandle(tmp, argp++, _PL_rd PASS_LD);
		setHandle(tail, makeRef(argp));

		token = get_token(must_be_op, _PL_rd);

		switch(token->value.character)
		{ case ']':
		    { if ( positions )
		      { if ( !PL_unify_nil(pa) ||
			     !PL_unify_atom(pt, ATOM_none) ||
			     !PL_unify_integer(pe, token->end) )
			  return FALSE;
		      }
		      return PL_unify_nil(tail);
		    }
		  case '|':
		    { int rc;

		      if ( (rc=complex_term(",|]", 999, tmp, pt, _PL_rd PASS_LD)) != TRUE )
			return rc;
		      argp = unRef(*valTermRef(tail));
		      readValHandle(tmp, argp, _PL_rd PASS_LD);
		      token = get_token(must_be_op, _PL_rd); /* discard ']' */
		      switch(token->value.character)
		      { case ',':
			case '|':
			  syntaxError("list_rest", _PL_rd);
		      }
		      if ( positions )
		      { if ( !PL_unify_nil(pa) ||
			     !PL_unify_integer(pe, token->end) )
			  return FALSE;
		      }
		      succeed;
		    }
		  case ',':
		      continue;
		}
	      }
	    }
	  case ')':
	  case '}':
	  case ']':
	    syntaxError("cannot_start_term", _PL_rd);
	  case '|':
	    if ( !must_be_op  && _PL_rd->strictness == 0 )
	    { term_t ex;

	      ex = makeErrorTerm("quoted_punctuation", 0, _PL_rd);
	      if ( ex )
	      { printMessage(ATOM_warning, PL_TERM, ex);
	      } else			/* no space for warning */
	      { PL_put_term(_PL_rd->exception, exception_term);
		return FALSE;
	      }

	    }
	  case ',':
	    if ( !must_be_op && _PL_rd->strictness > 0 )
	      syntaxError("quoted_punctuation", _PL_rd);
	  default:
	    *name = TRUE;
	    PL_put_atom(term, codeToAtom(token->value.character));
	    goto atomic_out;
	}
      } /* case T_PUNCTUATION */
    default:;
      sysError("read/1: Illegal token type (%d)", token->type);
      /*NOTREACHED*/
      fail;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
read_term(?term, ReadData rd)
    Common part of all read variations. Please note that the
    variable-handling code uses terms of the type STG_GLOBAL|TAG_ATOM,
    which are not valid Prolog terms. Some of the temporary
    term-references will even be initialised to this data after
    read has completed.  Hence the PL_reset_term_refs() in this
    function, which not only saves memory, but also guarantees the
    stacks are in a sane state after read has completed.

    Should one ever think of it, the garbage collector cannot be
    activated during read for this reason, unless it is programmed
    to deal with this intermediate type! We actually block GC, as
    errors and interrupts may cause Prolog to become active with
    these terms around.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
read_term(term_t term, ReadData rd ARG_LD)
{ int rc2, rc = FALSE;
  term_t result;
  Token token;
  Word p;
  fid_t fid;

  if ( !(rd->base = raw_read(rd, &rd->end PASS_LD)) )
    fail;

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

  result = PL_new_term_ref();
  rd->here = rd->base;
  LD->read.active++;
  rd->strictness = truePrologFlag(PLFLAG_ISO);
  if ( (rc2=complex_term(NULL, OP_MAXPRIORITY+1, result, rd->subtpos, rd PASS_LD)) != TRUE )
  { rc = raiseStackOverflow(rc2);
    goto out;
  }
  p = valTermRef(result);
  if ( isVarAtom(*p, rd) )		/* reading a single variable */
  { if ( (rc2=ensureSpaceForTermRefs(1 PASS_LD)) != TRUE )
    { rc = raiseStackOverflow(rc2);
      goto out;
    }
    p = valTermRef(result);		/* may be shifted */
    readValHandle(result, p, rd PASS_LD);
  }

  if ( !(token = get_token(FALSE, rd)) )
    goto out;
  if ( token->type != T_FULLSTOP )
  { errorWarning("end_of_clause_expected", 0, rd);
    goto out;
  }

  if ( !PL_unify(term, result) )
    goto out;
  if ( rd->varnames && !bind_variable_names(rd PASS_LD) )
    goto out;
  if ( rd->variables && !bind_variables(rd PASS_LD) )
    goto out;
  if ( rd->singles && !check_singletons(rd PASS_LD) )
    goto out;

  rc = TRUE;

out:
  PL_close_foreign_frame(fid);
  LD->read.active--;

  return rc;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static unsigned char *
backSkipUTF8(unsigned const char *start, unsigned const char *end, int *chr)
{ const unsigned char *s;

  for(s=end-1 ; s>start && *s&0x80; s--)
    ;
  utf8_get_char((char*)s, chr);

  return (unsigned char *)s;
}


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

  if ( !getInputStream(from, &in) )
    fail;

  init_read_data(&rd, in PASS_LD);
  if ( !(s = raw_read(&rd, &e PASS_LD)) )
  { rval = PL_raise_exception(rd.exception);
    goto out;
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

out:
  free_read_data(&rd);
  if ( Sferror(in) )
    return streamStatus(in);
  else
    PL_release_stream(in);

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

  if ( !getInputStream(from, &s) )
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


/* read_clause([+Stream, ]-Clause) */

int
read_clause(IOSTREAM *s, term_t term ARG_LD)
{ read_data rd;
  int rval;
  fid_t fid;

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

retry:
  init_read_data(&rd, s PASS_LD);
  rd.on_error = ATOM_dec10;
  rd.singles = rd.styleCheck & SINGLETON_CHECK ? TRUE : FALSE;
  if ( !(rval = read_term(term, &rd PASS_LD)) && rd.has_exception )
  { if ( reportReadError(&rd) )
    { PL_rewind_foreign_frame(fid);
      free_read_data(&rd);
      goto retry;
    }
  }
  free_read_data(&rd);

  return rval;
}


static int
read_clause_pred(term_t from, term_t term ARG_LD)
{ int rval;
  IOSTREAM *s;

  if ( !getInputStream(from, &s) )
    fail;
  rval = read_clause(s, term PASS_LD);
  if ( Sferror(s) )
    return streamStatus(s);
  else
    PL_release_stream(s);

  return rval;
}


static
PRED_IMPL("read_clause", 1, read_clause, 0)
{ PRED_LD

  return read_clause_pred(0, A2 PASS_LD);
}

static
PRED_IMPL("read_clause", 2, read_clause, 0)
{ PRED_LD

  return read_clause_pred(A1, A2 PASS_LD);
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
  { NULL_ATOM,	     	    0 }
};

word
pl_read_term3(term_t from, term_t term, term_t options)
{ GET_LD
  term_t tpos = 0;
  term_t tcomments = 0;
  int rval;
  atom_t w;
  read_data rd;
  IOSTREAM *s;
  bool charescapes = -1;
  atom_t dq = NULL_ATOM;
  atom_t mname = NULL_ATOM;
  fid_t fid = PL_open_foreign_frame();

retry:
  if ( !getInputStream(from, &s) )
    fail;
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
		     &tcomments) )
  { PL_release_stream(s);
    fail;
  }

  if ( mname )
  { rd.module = lookupModule(mname);
    rd.flags  = rd.module->flags;
  }

  if ( charescapes != -1 )
  { if ( charescapes )
      set(&rd, CHARESCAPE);
    else
      clear(&rd, CHARESCAPE);
  }
  if ( dq )
  { if ( !setDoubleQuotes(dq, &rd.flags) )
    { PL_release_stream(s);
      fail;
    }
  }
  if ( rd.singles && PL_get_atom(rd.singles, &w) && w == ATOM_warning )
    rd.singles = TRUE;
  if ( tcomments )
    rd.comments = PL_copy_term_ref(tcomments);

  rval = read_term(term, &rd PASS_LD);
  if ( Sferror(s) )
    rval = streamStatus(s);
  else
    PL_release_stream(s);

  if ( rval )
  { if ( tpos && source_line_no > 0 )
      rval = PL_unify_term(tpos,
			   PL_FUNCTOR, FUNCTOR_stream_position4,
			   PL_INT64, source_char_no,
			   PL_INT, source_line_no,
			   PL_INT, source_line_pos,
			   PL_INT, 0);			/* should be byteno */
    if ( tcomments )
    { if ( !PL_unify_nil(rd.comments) )
	return FALSE;
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

word
pl_read_term(term_t term, term_t options)
{ return pl_read_term3(0, term, options);
}

		 /*******************************
		 *	   TERM <->ATOM		*
		 *******************************/

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
    PL_write_term(stream, term, 1200, PL_WRT_QUOTED);
    Sflush(stream);

    txt.text.t = s;
    txt.length = bufsize;
    txt.storage = PL_CHARS_HEAP;
    txt.encoding = ENC_UTF8;
    txt.canonical = FALSE;
    rval = PL_unify_text(atom, 0, &txt, PL_ATOM);

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
  PRED_DEF("read_clause", 1, read_clause, 0)
  PRED_DEF("read_clause", 2, read_clause, 0)
  PRED_DEF("atom_to_term", 3, atom_to_term, 0)
  PRED_DEF("term_to_atom", 2, term_to_atom, 0)
  PRED_DEF("$code_class", 2, code_class, 0)
EndPredDefs
