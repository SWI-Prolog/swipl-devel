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
/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-ctype.h"

#undef LD
#define LD LOCAL_LD

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
{ int ctx;
  mark m;
  GET_LD

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { int cin;

      if ( !PL_is_variable(in) )
      { if ( PL_get_char(in, &cin, FALSE) )
	  return PL_unify_char(out, char_conversion_table[cin], CHAR_MODE);
	fail;
      }
      ctx = 0;
      break;
    }
    case FRG_REDO:
      ctx = ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      ctx = 0;				/* for compiler */
      succeed;
  }

  Mark(m);
  for( ; ctx < 256; ctx++)
  { if ( PL_unify_char(in, ctx, CHAR_MODE) &&
	 PL_unify_char(out, char_conversion_table[ctx], CHAR_MODE) )
      ForeignRedoInt(ctx+1);
    Undo(m);
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
  term_t	variable;	/* Term-reference to the variable */
  int		times;		/* Number of occurences */
  word		signature;	/* Pseudo atom */
} variable, *Variable;


struct token
{ int type;			/* type of token */
  long start;			/* start-position */
  long end;			/* end-position */
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
  unsigned char *token_start;		/* start of most recent read token */
  struct token token;			/* current token */
  bool _unget;				/* unget_token() */

  Module	module;			/* Current source module */
  unsigned int	flags;			/* Module syntax flags */
  int		styleCheck;		/* style-checking mask */
  bool		backquoted_string;	/* Read `hello` as string */

  atom_t	on_error;		/* Handling of syntax errors */
  int		has_exception;		/* exception is raised */

  term_t	exception;		/* raised exception */
  term_t	variables;		/* report variables */
  term_t	varnames;		/* Report variables+names */
  term_t	singles;		/* Report singleton variables */
  term_t	subtpos;		/* Report Subterm positions */

  struct var_table vt;			/* Data about variables */
  struct read_buffer _rb;		/* keep read characters here */
} read_data, *ReadData;

#define	rdhere		  (_PL_rd->here)
#define	rdbase		  (_PL_rd->base)
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
  _PL_rd->backquoted_string = trueFeature(BACKQUOTED_STRING_FEATURE);
}

static void
free_read_data(ReadData _PL_rd)
{ if ( rdbase && rdbase != rb.fast )
    PL_free(rdbase);

  discardBuffer(&var_name_buffer);
  discardBuffer(&var_buffer);
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

Error term:

	error(syntax_error(Id), file(Path, Line))
	error(syntax_error(Id), string(String, CharNo))
	error(syntax_error(Id), stream(Stream, Line, CharPos))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define syntaxError(what, rd) { errorWarning(what, 0, rd); fail; }

extern IOFUNCTIONS Sstringfunctions;

static bool
isStringStream(IOSTREAM *s)
{ return s->functions == &Sstringfunctions;
}


static bool
errorWarning(const char *id_str, term_t id_term, ReadData _PL_rd)
{ GET_LD
  term_t ex = PL_new_term_ref();
  term_t loc = PL_new_term_ref();
  unsigned char const *s, *ll = NULL;

  if ( !id_term )
  { id_term = PL_new_term_ref();
    PL_put_atom_chars(id_term, id_str);
  }

  PL_unify_term(ex,
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

  if ( ReadingSource )			/* reading a file */
  { PL_unify_term(loc,
		  PL_FUNCTOR, FUNCTOR_file4,
		    PL_ATOM, source_file_name,
		    PL_INT, source_line_no,
		    PL_INT, source_line_pos,
		    PL_LONG, source_char_no);
  } else if ( isStringStream(rb.stream) )
  { PL_unify_term(loc,
		  PL_FUNCTOR, FUNCTOR_string2,
		    PL_STRING, rdbase,
		    PL_LONG, last_token_start-rdbase);
  } else				/* any stream */
  { term_t stream = PL_new_term_ref();

    PL_unify_stream_or_alias(stream, rb.stream);
    PL_unify_term(loc,
		  PL_FUNCTOR, FUNCTOR_stream4,
		    PL_TERM, stream,
		    PL_INT, source_line_no,
		    PL_INT, source_line_pos,
		    PL_LONG, source_char_no);
  }

  _PL_rd->has_exception = TRUE;
  PL_put_term(_PL_rd->exception, ex);

  fail;
}


static void
singletonWarning(atom_t *vars, int nvars)
{ GET_LD
  fid_t cid = PL_open_foreign_frame();
  term_t l = PL_new_term_ref();
  term_t a = PL_copy_term_ref(l);
  term_t h = PL_new_term_ref();
  int n;

  for(n=0; n<nvars; n++)
  { PL_unify_list(a, h, a);
    PL_unify_atom(h, vars[n]);
    PL_unregister_atom(vars[n]);
  }
  PL_unify_nil(a);

  printMessage(ATOM_warning,
	       PL_FUNCTOR, FUNCTOR_singletons1,
	         PL_TERM,    l);

  PL_discard_foreign_frame(cid);
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
}      

#define addToBuffer(c, _PL_rd) \
	do \
        { \
	  if ( rb.here >= rb.end ) \
	    growToBuffer(c, _PL_rd); \
	  else \
	    *rb.here++ = c; \
	} while(0)

static void
growToBuffer(int c, ReadData _PL_rd)
{ if ( rb.base == rb.fast )		/* long clause: jump to use malloc() */
  { rb.base = PL_malloc(FASTBUFFERSIZE * 2);
    memcpy(rb.base, rb.fast, FASTBUFFERSIZE);
  } else
    rb.base = PL_realloc(rb.base, rb.size*2);

  DEBUG(8, Sdprintf("Reallocated read buffer at %ld\n", (long) rb.base));
  rdbase = rb.base;
  rb.here = rb.base + rb.size;
  rb.size *= 2;
  rb.end  = rb.base + rb.size;

  *rb.here++ = c & 0xff;
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


#define getchr()  char_conversion_table[Sgetc(rb.stream)]
#define getchrq() Sgetc(rb.stream)

#define ensure_space(c) { if ( something_read && \
			       (c == '\n'|| !isBlank(rb.here[-1])) ) \
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
      int xdigits;

      addToBuffer(c, _PL_rd);

      switch( (c=getchrq()) )
      { case EOF:
	  goto eofinstr;
	case 'x':			/* \xNN\ */
	  addToBuffer(c, _PL_rd);
	  base = 16;
	  xdigits = 2;
	  goto xdigits;
	default:
	  addToBuffer(c, _PL_rd);
	  if ( digitValue(8, c) >= 0 )	/* \NNN\ */
	  { base = 8;
	    xdigits = 2;
	  } else
	    continue;			/* \symbolic-control-char */
	xdigits:
	  c = getchrq();
	  while( xdigits-- > 0 && digitValue(base, c) >= 0 )
	  { addToBuffer(c, _PL_rd);
	    c = getchrq();
	  }
	  if ( c == EOF )
	    goto eofinstr;
	  addToBuffer(c, _PL_rd);
	  if ( c == q )
	    return TRUE;
	  continue;
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



static unsigned char *
raw_read2(ReadData _PL_rd ARG_LD)
{ int c;
  bool something_read = FALSE;
  bool dotseen = FALSE;
  
  clearBuffer(_PL_rd);				/* clear input buffer */
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
		{ warning("Attempt to read past end-of-file");
		  return NULL;
		}
		set_start_line;
		strcpy((char *)rb.base, "end_of_file. ");
		rb.here = rb.base + 14;
		return rb.base;
      case '/': c = getchr();
		if ( c == '*' )
		{ int last;
		  int level = 1;

		  if ((last = getchr()) == EOF)
		    rawSyntaxError("end_of_file_in_block_comment");

		  if ( something_read )
		  { addToBuffer(' ', _PL_rd);	/* positions */
		    addToBuffer(' ', _PL_rd);
		    addToBuffer(last == '\n' ? last : ' ', _PL_rd);
		  }

		  for(;;)
		  { switch(c = getchr())
		    { case EOF:
			rawSyntaxError("end_of_file_in_block_comment");
		      case '*':
			if ( last == '/' )
			  level++;
			break;
		      case '/':
			if ( last == '*' && --level == 0 )
			{ c = ' ';
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
		  if ( isSymbol(c) )
		  { while( c != EOF && isSymbol(c) )
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    }
		  }
		  dotseen = FALSE;
		  goto handle_c;
		}
      case '%': if ( something_read )
		  addToBuffer(' ', _PL_rd);	/* positions */
		while((c=getchr()) != EOF && c != '\n')
		{ if ( something_read )		/* record positions */
		    addToBuffer(' ', _PL_rd);
		}
		c = '\n';
		goto handle_c;
     case '\'': if ( rb.here > rb.base && isDigit(rb.here[-1]) )
		{ addToBuffer(c, _PL_rd); 		/* <n>' */
		  if ( rb.here[-2] == '0' )		/* 0'<c> */
		  { if ( (c=getchr()) != EOF )
		    { addToBuffer(c, _PL_rd);
		      break;
		    }
		    rawSyntaxError("end_of_file");
		  }
		  dotseen = FALSE;
		  break;
		}

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
		if ( isSymbol(c) )
		{ while( c != EOF && isSymbol(c) )
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
      default:	switch(_PL_char_types[c])
		{ case SP:
		  case CT:
		    if ( dotseen )
		    { if ( rb.here - rb.base == 1 )
			rawSyntaxError("end_of_clause");
		      ensure_space(c);
		      addToBuffer(EOS, _PL_rd);
		      return rb.base;
		    }
		    do
		    { if ( something_read ) /* positions */
			addToBuffer(c, _PL_rd);
		      else
			ensure_space(c);
		      c = getchr();
		    } while( c != EOF && isBlank(c) );
		    goto handle_c;
		  case SY:
		    set_start_line;
		    do
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		      if ( c == '`' && _PL_rd->backquoted_string )
			break;
		    } while( c != EOF && isSymbol(c) );
		    dotseen = FALSE;
		    goto handle_c;
		  case LC:
		  case UC:
		    set_start_line;
		    do
		    { addToBuffer(c, _PL_rd);
		      c = getchr();
		    } while( c != EOF && isAlpha(c) );
		    dotseen = FALSE;
		    goto handle_c;
		  default:
		    addToBuffer(c, _PL_rd);
		    dotseen = FALSE;
		    set_start_line;
		}
    }
  }
}


static unsigned char *
raw_read(ReadData _PL_rd ARG_LD)
{ if ( (rb.stream->flags & SIO_ISATTY) && Sfileno(rb.stream) >= 0 )
  { unsigned char *s;
    ttybuf tab;
    
    PushTty(rb.stream, &tab, TTY_SAVE);		/* make sure tty is sane */
    PopTty(rb.stream, &ttytab);
    s = raw_read2(_PL_rd PASS_LD);
    PopTty(rb.stream, &tab);

    return s;
  } else
    return raw_read2(_PL_rd PASS_LD);
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
save_var_name(const char *name, ReadData _PL_rd)
{ int l = strlen(name);
  char *nb, *ob = baseBuffer(&var_name_buffer, char);
  int e = entriesBuffer(&var_name_buffer, char);

  addMultipleBuffer(&var_name_buffer, name, l+1, char);
  if ( (nb = baseBuffer(&var_name_buffer, char)) != ob )
  { ptrdiff_t shift = nb - ob;

    for_vars(v, v->name += shift);
  }

  return baseBuffer(&var_name_buffer, char) + e;
}

					/* use hash-key? */

static Variable
isVarAtom(word w, ReadData _PL_rd)
{ if ( tagex(w) == (TAG_ATOM|STG_GLOBAL) )
    return &baseBuffer(&var_buffer, variable)[w>>7];
  
  return NULL;
}


static Variable
lookupVariable(const char *name, ReadData _PL_rd)
{ variable next;
  Variable var;
  int nv;

  if ( name[0] != '_' || name[1] != EOS ) /* anonymous: always add */
  { for_vars(v,
	     if ( streq(name, v->name) )
	     { v->times++;
	       return v;
	     })
  }
       
  nv = entriesBuffer(&var_buffer, variable);
  next.name      = save_var_name(name, _PL_rd);
  next.times     = 1;
  next.variable  = 0;
  next.signature = (nv<<7)|TAG_ATOM|STG_GLOBAL;
  addBuffer(&var_buffer, next, variable);
  var = topBuffer(&var_buffer, variable);

  return var-1;
}


static bool
check_singletons(ReadData _PL_rd ARG_LD)
{ if ( _PL_rd->singles != TRUE )	/* returns <name> = var bindings */
  { term_t list = PL_copy_term_ref(_PL_rd->singles);
    term_t head = PL_new_term_ref();

    for_vars(var,
	     if ( var->times == 1 && var->name[0] != '_' )
	     {	if ( !PL_unify_list(list, head, list) ||
		     !PL_unify_term(head,
				    PL_FUNCTOR, FUNCTOR_equals2,
				    PL_CHARS,	var->name,
				    PL_TERM,    var->variable) )
		  fail;
	     });

    return PL_unify_nil(list);
  } else				/* just report */
  { atom_t singletons[MAX_SINGLETONS];
    int i = 0;

    for_vars(var,
	     if ( var->times == 1 && var->name[0] != '_' )
	     { if ( i < MAX_SINGLETONS )
		 singletons[i++] = PL_new_atom(var->name);
	     });

    if ( i > 0 )
      singletonWarning(singletons, i);

    succeed;
  }
}


static bool
bind_variable_names(ReadData _PL_rd ARG_LD)
{ term_t list = PL_copy_term_ref(_PL_rd->varnames);
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();

  for_vars(var,
	   /*if ( var->name[0] != '_' ) Just _ isn't in this table */
	   { if ( !PL_unify_list(list, head, list) ||
		  !PL_unify_functor(head, FUNCTOR_equals2) ||
		  !PL_get_arg(1, head, a) ||
		  !PL_unify_atom_chars(a, var->name) ||
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

#define skipSpaces	{ while(isBlank(*rdhere) ) rdhere++; c = *rdhere++; }
#define unget_token()	{ unget = TRUE; }

typedef const unsigned char * cucharp;
typedef       unsigned char * ucharp;

static int
scan_decimal(cucharp *sp, Number n)
{ unsigned long maxi = PLMAXINT/10;	/* cache? */
  unsigned long t = 0;
  cucharp s = *sp;
  int c;

  for(c = *s; isDigit(c); c = *++s)
  { if ( t > maxi )
    { real maxf = MAXREAL / (real) 10 - (real) 10;
      real tf = (real)t;

      for(c = *s; isDigit(c); c = *++s)
      { if ( tf > maxf )
	  fail;				/* number too large */
        tf = tf * (real)10 + (real)(c - '0');
      }
      *sp = s;
      n->value.f = tf;
      n->type = V_REAL;
      succeed;
    } else
      t = t * 10 + c - '0';
  }  

  *sp = s;

  if ( t > PLMAXINT )
  { n->value.f = (real)t;
    n->type = V_REAL;
    succeed;
  }

  n->value.i = t;
  n->type = V_INTEGER;
  succeed;
}


static int
scan_number(cucharp *s, int b, Number n)
{ int d;
  unsigned long maxi = PLMAXINT/b;	/* cache? */
  unsigned long t;
  cucharp q = *s;

  if ( (d = digitValue(b, *q)) < 0 )
    fail;				/* syntax error */
  t = d;
  q++;

  while((d = digitValue(b, *q)) >= 0)
  { q++;

    if ( t > maxi )
    { real maxf = MAXREAL / (real) b - (real) b;
      real tf = (real)t;

      tf = tf * (real)b + (real)d;
      while((d = digitValue(b, *q)) >= 0)
      { q++;
        if ( tf > maxf )
	  fail;				/* number too large */
        tf = tf * (real)b + (real)d;
      }
      n->value.f = tf;
      n->type = V_REAL;
      *s = q;
      succeed;
    } else
      t = t * b + d;
  }  

  if ( t > PLMAXINT )
  { n->value.f = (real)t;
    n->type = V_REAL;
    *s = q;
    succeed;
  }

  n->value.i = t;
  n->type = V_INTEGER;
  *s = q;
  succeed;
}


static int
escape_char(cucharp in, ucharp *end, unsigned int quote)
{ int base, xdigits;
  int chr;
  unsigned c;

#define OK(v) if (1) {chr = (v); goto ok;} else (void)0

again:
  switch((c = *in++))
  { case 'a':
      OK(7);				/* 7 is ASCII BELL */
    case 'b':
      OK('\b');
    case 'c':				/* skip \c<blank>* */
      if ( quote )
      { while(isBlank(*in))
	  in++;
      skip_cont:
	c = *in;
	if ( c == '\\' )
	{ in++;
	  goto again;
	}
	if ( c == quote )		/* \c ' --> no output */
	{ OK(EOF);
	}
	in++;
	OK(c);
      }
      OK('c');
    case '\n':				/* \LF<blank>* */
      if ( quote )
      { while(isBlank(*in) && *in != '\n' )
	  in++;
	goto skip_cont;
      }
      OK('\n');
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
    case 'x':
      c = *in++;
      if ( digitValue(16, c) >= 0 )
      { base = 16;
	xdigits = 1;
	goto numchar;
      } else
	OK('x');
    default:
      if ( c >= '0' && c <= '7' )	/* octal number */
      { int dv;

	base = 8;
	xdigits = 2;

      numchar:
	chr = digitValue(base, c);
	c = *in++;
	while(xdigits-- > 0 &&
	      (dv = digitValue(base, c)) >= 0 )
	{ chr = chr * base + dv;
	  c = *in++;
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

static void
get_string(unsigned char *in, unsigned char **end, Buffer buf,
	   ReadData _PL_rd)
{ int quote;
  int c;

  quote = *in++;

  for(;;)
  { c = *in++;

    if ( c == quote )
    { if ( *in == quote )
      { in++;
      } else
	break;
    } else if ( c == '\\' && DO_CHARESCAPE )
    { c = escape_char(in, &in, quote);
      if ( c == EOF )
	continue;
    }

    addBuffer(buf, c, char);
  }

  if ( end )
    *end = in;
}  


int
get_number(cucharp in, ucharp *end, Number value, int escape)
{ int negative = FALSE;
  unsigned int c;

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
	{ chr = escape_char(in+3, end, 0);
	} else
	{ chr = in[2] & 0xff;
	  *end = (ucharp)in+3;
	}

	if ( isAlpha(**end) )
	  fail;				/* illegal number */
	value->value.i = (long)chr;
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
    { int rval;
      in += 2;
      rval = scan_number(&in, base, value);
      *end = (ucharp)in;
      if ( negative )
	 value->value.i = -value->value.i;
      return rval;
    }
  }

  if ( !isDigit(*in) || !scan_decimal(&in, value) )
    fail;				/* too large? */

					/* base'value number */
  if ( *in == '\'' )
  { in++;

    if ( !intNumber(value) || value->value.i > 36 )
      fail;				/* illegal base */
    if ( !scan_number(&in, (int)value->value.i, value) )
      fail;				/* number too large */

    if ( isAlpha(*in) )
      fail;				/* illegal number */

    if ( negative )
    { if ( intNumber(value) )
	value->value.i = -value->value.i;
      else
	value->value.f = -value->value.f;
    }

    *end = (ucharp)in;
    succeed;
  }
					/* Real numbers */
  if ( *in == '.' && isDigit(in[1]) )
  { double n;

    if ( intNumber(value) )
    { value->value.f = (double) value->value.i;
      value->type = V_REAL;
    }
    n = 10.0, in++;
    while( isDigit(c = *in) )
    { in++;
      value->value.f += (double)(c-'0') / n;
      n *= 10.0;
    }
  }

  if ( *in == 'e' || *in == 'E' )
  { number exponent;
    bool neg_exponent;

    in++;
    DEBUG(9, Sdprintf("Exponent\n"));
    switch(*in)
    { case '-':
	in++;
        neg_exponent = TRUE;
	break;
      case '+':
	in++;
      default:
	neg_exponent = FALSE;
        break;
    }

    if ( !scan_decimal(&in, &exponent) || !intNumber(&exponent) )
      fail;				/* too large exponent */

    if ( intNumber(value) )
    { value->value.f = (double) value->value.i;
      value->type = V_REAL;
    }

    value->value.f *= pow((double)10.0,
			  neg_exponent ? -(double)exponent.value.i
			               : (double)exponent.value.i);
  }

  if ( negative )
  { if ( intNumber(value) )
      value->value.i = -value->value.i;
    else
      value->value.f = -value->value.f;
  }

  *end = (ucharp)in;
  succeed;
}


static void
checkASCII(unsigned char *name, int len, const char *type)
{ int i;

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


static Token
get_token__LD(bool must_be_op, ReadData _PL_rd ARG_LD)
{ unsigned int c;
  unsigned char *start;
  int end;

  if ( unget )
  { unget = FALSE;
    return &cur_token;
  }

  skipSpaces;
  last_token_start = rdhere - 1;
  cur_token.start = source_char_no + last_token_start - rdbase;
  switch(_PL_char_types[c])
  { case LC:	{ start = rdhere-1;
		  while(isAlpha(*rdhere) )
		    rdhere++;
		  c = *rdhere;
		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		    checkASCII(start, rdhere-start, "atom");
		  cur_token.value.atom = lookupAtom((char *)start,
						    rdhere-start);
		  cur_token.type = (c == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n", c == '(' ? "FUNC" : "NAME",
				    stringAtom(cur_token.value.atom)));

		  break;
		}
    case UC:	{ start = rdhere-1;
		  while(isAlpha(*rdhere) )
		    rdhere++;
		  c = *rdhere;
		  *rdhere = EOS;
		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		    checkASCII(start, rdhere-start, "variable");
		  if ( c == '(' && trueFeature(ALLOW_VARNAME_FUNCTOR) )
		  { cur_token.value.atom = lookupAtom((char *)start,
						      rdhere-start);
		    cur_token.type = T_FUNCTOR;
		    *rdhere = c;
		    break;
		  }
		  if ( start[0] == '_' &&
		       rdhere == start + 1 &&
		       !_PL_rd->variables ) /* report them */
		  { DEBUG(9, Sdprintf("VOID\n"));
		    cur_token.type = T_VOID;
		  } else
		  { cur_token.value.variable = lookupVariable((char *)start,
							      _PL_rd);
		    DEBUG(9, Sdprintf("VAR: %s\n",
				      cur_token.value.variable->name));
		    cur_token.type = T_VARIABLE;
		  }
		  *rdhere = c;

		  break;
		}
    case_digit:
    case DI:	{ number value;

		  if ( get_number(&rdhere[-1],
				  &rdhere, &value, DO_CHARESCAPE) &&
		       !isAlpha(rdhere[0]) )
		  { cur_token.value.number = value;
		    cur_token.type = T_NUMBER;
		    break;
		  } else
		    syntaxError("illegal_number", _PL_rd);
		}
    case SO:	{ char tmp[1];

		  tmp[0] = c;
		  cur_token.value.atom = lookupAtom(tmp, 1);
		  cur_token.type = (*rdhere == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n",
				  *rdhere == '(' ? "FUNC" : "NAME",
				  stringAtom(cur_token.value.atom)));

		  break;
		}
    case SY:	if ( c == '`' && _PL_rd->backquoted_string )
		  goto case_bq;

                { start = rdhere - 1;
		  while( isSymbol(*rdhere) &&
			 !(*rdhere == '`' && _PL_rd->backquoted_string) )
		    rdhere++;
		  end = *rdhere;

		  if ( rdhere == start+1 )
		  { if ( (c == '+' || c == '-') &&	/* +- number */
			 !must_be_op &&
			 isDigit(*rdhere) )
		    { goto case_digit;
		    }
		    if ( c == '.' && isBlank(*rdhere) )	/* .<blank> */
		    { cur_token.type = T_FULLSTOP;
		      break;
		    }
		  }

		  if ( _PL_rd->styleCheck & CHARSET_CHECK )
		    checkASCII(start, rdhere-start, "symbol");
		  cur_token.value.atom = lookupAtom((char *)start, rdhere-start);
		  cur_token.type = (end == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n",
				    end == '(' ? "FUNC" : "NAME",
				    stringAtom(cur_token.value.atom)));

		  break;
		}
    case PU:	{ switch(c)
		  { case '{':
		    case '[':
		      while( isBlank(*rdhere) )
			rdhere++;
		      if (rdhere[0] == matchingBracket(c))
		      { rdhere++;
			cur_token.value.atom = (c == '[' ? ATOM_nil : ATOM_curl);
			cur_token.type = rdhere[0] == '(' ? T_FUNCTOR : T_NAME;
			PL_register_atom(cur_token.value.atom);
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
		  int len;
		  char *s;

		  initBuffer(&b);
		  get_string(rdhere-1, &rdhere, (Buffer)&b, _PL_rd);
		  s = baseBuffer(&b, char);
		  len = entriesBuffer(&b, char);
		  cur_token.value.atom = lookupAtom(s, len);
		  cur_token.type = (rdhere[0] == '(' ? T_FUNCTOR : T_NAME);
		  discardBuffer(&b);
		  break;
		}
    case DQ:	{ tmp_buffer b;
		  term_t t = PL_new_term_ref();
		  char *s;
		  int len;

		  initBuffer(&b);
		  get_string(rdhere-1, &rdhere, (Buffer)&b, _PL_rd);
		  s   = baseBuffer(&b, char);
		  len = entriesBuffer(&b, char);
#if O_STRING
 		  if ( true(_PL_rd, DBLQ_STRING) )
		    PL_put_string_nchars(t, len, s);
		  else
#endif
		  if ( true(_PL_rd, DBLQ_ATOM) )
		    PL_put_atom_nchars(t, len, s);
		  else if ( true(_PL_rd, DBLQ_CHARS) )
		    PL_put_list_nchars(t, len, s);
		  else
		    PL_put_list_ncodes(t, len, s);

  		  cur_token.value.term = t;
		  cur_token.type = T_STRING;
		  discardBuffer(&b);
		  break;
		}
#ifdef O_STRING
    case BQ:
    case_bq:    { tmp_buffer b;
		  term_t t = PL_new_term_ref();
		  char *s;
		  int len;

		  initBuffer(&b);
		  get_string(rdhere-1, &rdhere, (Buffer)&b, _PL_rd);
		  s   = baseBuffer(&b, char);
		  len = entriesBuffer(&b, char);
		  PL_put_string_nchars(t, len, s);
  		  cur_token.value.term = t;
		  cur_token.type = T_STRING;
		  discardBuffer(&b);
		  break;
		}
#endif
    default:	{ sysError("read/1: tokeniser internal error");
    		  break;		/* make lint happy */
		}
  }

out:
  cur_token.end = source_char_no + rdhere - rdbase;

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
    { var->variable = PL_new_term_ref();
      setVar(*argp);
      *valTermRef(var->variable) = makeRef(argp);
    } else				/* reference to existing var */
    { *argp = *valTermRef(var->variable);
    }
  } else
    *argp = w;				/* plain value */
}


static void
build_term(term_t term, atom_t atom, int arity, term_t *argv,
	   ReadData _PL_rd ARG_LD)
{ functor_t functor = lookupFunctorDef(atom, arity);
  Word argp = allocGlobal(arity+1);

  DEBUG(9, Sdprintf("Building term %s/%d ... ", stringAtom(atom), arity));
  setHandle(term, consPtr(argp, TAG_COMPOUND|STG_GLOBAL));
  *argp++ = functor;

  for( ; arity-- > 0; argv++, argp++)
    readValHandle(*argv, argp, _PL_rd PASS_LD);

  DEBUG(9, Sdprintf("result: "); pl_write(term); Sdprintf("\n") );
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

static bool
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
    case OP_YFY:	e->left_pri = pri;   e->right_pri = pri;   break;
  }

  succeed;
}

static void
build_op_term(term_t term,
	      atom_t atom, int arity, out_entry *argv,
	      ReadData _PL_rd ARG_LD)
{ term_t av[2];

  av[0] = argv[0].term;
  av[1] = argv[1].term;
  build_term(term, atom, arity, av, _PL_rd PASS_LD);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
realloca() maintains a buffer using  alloca()   that  has  the requested
size. The current size is maintained in   a long just below the returned
area. This is a long, to ensure   proper  allignment of the remainder of
the values.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define realloca(p, unit, n) \
	{ long *ip = (long *)(p); \
	  if ( ip == NULL || ip[-1] < (n) ) \
	  { long nsize = (((n)+(n)/2) + 3) & ~3; \
	    long *np = alloca(nsize * unit + sizeof(long)); \
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
	  { DEBUG(9, Sdprintf("Infix %s to postfix\n", \
			      stringAtom(side[side_p].op))); \
	    rmo++; \
	    tmp = PL_new_term_ref(); \
	    build_op_term(tmp, side[side_p].op, 1, &out[out_n-1], _PL_rd PASS_LD); \
	    out[out_n-1].pri  = side[side_p].op_pri; \
	    out[out_n-1].term = tmp; \
	    side[side_p].kind = OP_POSTFIX; \
	    out[out_n-1].tpos = opPos(&side[side_p], &out[out_n-1] PASS_LD); \
	    side_n--; \
	    side_p = (side_n == 0 ? -1 : side_p-1); \
	  } \
	}


static int
get_int_arg(term_t t, int n ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);

  return valInt(argTerm(*p, n-1));
}


static term_t
opPos(op_entry *op, out_entry *args ARG_LD)
{ if ( op->tpos )
  { int fs = get_int_arg(op->tpos, 1 PASS_LD);
    int fe = get_int_arg(op->tpos, 2 PASS_LD);
    term_t r = PL_new_term_ref();

    if ( op->kind == OP_INFIX )
    { int s = get_int_arg(args[0].tpos, 1 PASS_LD);
      int e = get_int_arg(args[1].tpos, 2 PASS_LD);

      PL_unify_term(r,
		    PL_FUNCTOR,	FUNCTOR_term_position5,
		    PL_INT, s,
		    PL_INT, e,
		    PL_INT, fs,
		    PL_INT, fe,
		    PL_LIST, 2, PL_TERM, args[0].tpos,
		    		PL_TERM, args[1].tpos);
    } else
    { int s, e;
      
      if ( op->kind == OP_PREFIX )
      { s = fs;
	e = get_int_arg(args[0].tpos, 2 PASS_LD);
      } else
      { s = get_int_arg(args[0].tpos, 1 PASS_LD);
	e = fe;
      }

      PL_unify_term(r,
		    PL_FUNCTOR,	FUNCTOR_term_position5,
		    PL_INT, s,
		    PL_INT, e,
		    PL_INT, fs,
		    PL_INT, fe,
		    PL_LIST, 1, PL_TERM, args[0].tpos);
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
	{ int arity = (side[side_p].kind == OP_INFIX ? 2 : 1); \
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
	  build_op_term(tmp, side[side_p].op, arity, &out[out_n], _PL_rd PASS_LD); \
	  out[out_n].pri  = side[side_p].op_pri; \
	  out[out_n].term = tmp; \
	  out[out_n].tpos = opPos(&side[side_p], &out[out_n] PASS_LD); \
	  out_n ++; \
	  side_n--; \
	  side_p = (side_n == 0 ? -1 : side_p-1); \
	}


static bool
complex_term(const char *stop, term_t term, term_t positions,
	     ReadData _PL_rd ARG_LD)
{ out_entry *out  = NULL;
  op_entry  *side = NULL;
  op_entry  in_op;
  int out_n = 0, side_n = 0;
  int rmo = 0;				/* Rands more than operators */
  int side_p = -1;
  term_t pin;
  int thestop;				/* encountered stop-character */

  for(;;)
  { bool isname;
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
    TRY( simple_term(rmo == 1, in, &isname, pin, _PL_rd PASS_LD) );

    if ( isname )			/* Check for operators */
    { atom_t name;

      PL_get_atom(in, &name);
      in_op.tpos = pin;
      in_op.token_start = last_token_start;

      DEBUG(9, Sdprintf("name %s, rmo = %d\n", stringAtom(name), rmo));

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
      if ( rmo == 0 && isOp(name, OP_PREFIX, &in_op, _PL_rd) )
      { DEBUG(9, Sdprintf("Prefix op: %s\n", stringAtom(name)));
	
	PushOp();

	continue;
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
  Modify(OP_MAXPRIORITY+1);
  Reduce(OP_MAXPRIORITY+1);

  if ( out_n == 1 && side_n == 0 )	/* simple term */
  { PL_assign_term(term, out[0].term PASS_LD);
    if ( positions )
      PL_unify(positions, out[0].tpos);
    succeed;
  }

  if ( out_n == 0 && side_n == 1 )	/* single operator */
  { PL_put_atom(term, side[0].op);
    if ( positions )
      PL_unify(positions, side[0].tpos);
    succeed;
  }

  if ( side_n == 1 &&
       ( side[0].op == ATOM_comma ||
	 side[0].op == ATOM_semicolon
       ))
  { term_t ex = PL_new_term_ref();
    char tmp[2];

    tmp[0] = thestop;
    tmp[1] = EOS;
    PL_unify_term(ex,
		  PL_FUNCTOR, FUNCTOR_punct2,
		    PL_ATOM, side[0].op,
		    PL_CHARS, tmp);

    return errorWarning(NULL, ex, _PL_rd);
  }

  syntaxError("operator_balance", _PL_rd);
}


static bool
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
      setHandle(term, 0L);		/* variable */
      goto atomic_out;
    case T_VARIABLE:
      setHandle(term, token->value.variable->signature);
      DEBUG(9, Sdprintf("Pushed var at 0x%x\n", token->value.variable));
      goto atomic_out;
    case T_NAME:
      *name = TRUE;
      PL_put_atom(term, token->value.atom);
      PL_unregister_atom(token->value.atom);
      goto atomic_out;
    case T_NUMBER:
      _PL_put_number(term, &token->value.number);
    atomic_out:
      if ( positions )
      { PL_unify_term(positions,
		      PL_FUNCTOR, FUNCTOR_minus2,
		      PL_LONG, token->start,
		      PL_LONG, token->end);
      }
      succeed;
    case T_STRING:
      PL_put_term(term,	token->value.term);
      if ( positions )
      { PL_unify_term(positions,
		      PL_FUNCTOR, FUNCTOR_string_position2,
		      PL_LONG, token->start,
		      PL_LONG, token->end);
      }
      succeed;
    case T_FUNCTOR:
      { if ( must_be_op )
	{ *name = TRUE;
	  PL_put_atom(term, token->value.atom);
	  PL_unregister_atom(token->value.atom);
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

	  if ( positions )
	  { pa = PL_new_term_ref();
	    pe = PL_new_term_ref();
	    ph = PL_new_term_ref();
	    PL_unify_term(positions,
			  PL_FUNCTOR, FUNCTOR_term_position5,
			  PL_LONG, token->start,
			  PL_TERM, pe,
			  PL_LONG, token->start,
			  PL_LONG, token->end,
			  PL_TERM, pa);
	  } else
	    pa = pe = ph = 0;

	  functor = token->value.atom;
	  argc = 0, argv;
	  get_token(must_be_op, _PL_rd); /* skip '(' */

	  do
	  { if ( argc == avn )
	    { term_t *nargv = alloca(sizeof(term_t) * avn * 2);
	      memcpy(nargv, argv, sizeof(term_t) * avn);
	      avn *= 2;
	      argv = nargv;
	    }
	    argv[argc] = PL_new_term_ref();
	    if ( positions )
	    { PL_unify_list(pa, ph, pa);
	    }
	    TRY( complex_term(",)", argv[argc], ph, _PL_rd PASS_LD) );
	    argc++;
	    token = get_token(must_be_op, _PL_rd); /* `,' or `)' */
	  } while(token->value.character == ',');

	  if ( positions )
	  { PL_unify_integer(pe, token->end);
	    PL_unify_nil(pa);
	  }

	  build_term(term, functor, argc, argv, _PL_rd PASS_LD);
	  PL_unregister_atom(functor);
	}
	succeed;
      }
    case T_PUNCTUATION:
      { switch(token->value.character)
	{ case '(':
	    { int start = token->start;

	      TRY( complex_term(")", term, positions, _PL_rd PASS_LD) );
	      token = get_token(must_be_op, _PL_rd);	/* skip ')' */

	      if ( positions )
	      { Word p = argTermP(*valTermRef(positions), 0);

		*p++ = consInt(start);
		*p   = consInt(token->end);
	      }

	      succeed;
	    }
	  case '{':
	    { term_t arg = PL_new_term_ref();
	      term_t pa, pe;

	      if ( positions )
	      { pa = PL_new_term_ref();
		pe = PL_new_term_ref();

		PL_unify_term(positions,
			      PL_FUNCTOR, FUNCTOR_brace_term_position3,
			      PL_LONG, token->start,
			      PL_TERM, pe,
			      PL_TERM, pa);
	      } else
		pe = pa = 0;

	      TRY( complex_term("}", arg, pa, _PL_rd PASS_LD) );
	      token = get_token(must_be_op, _PL_rd);
	      if ( positions )
		PL_unify_integer(pe, token->end);
	      build_term(term, ATOM_curl, 1, &arg, _PL_rd PASS_LD);

	      succeed;
	    }
	  case '[':
	    { term_t tail = PL_new_term_ref();
	      term_t tmp  = PL_new_term_ref();
	      term_t pa, pe, pt, p2;

	      if ( positions )
	      { pa = PL_new_term_ref();
		pe = PL_new_term_ref();
		pt = PL_new_term_ref();
		p2 = PL_new_term_ref();

		PL_unify_term(positions,
			      PL_FUNCTOR, FUNCTOR_list_position4,
			      PL_LONG, token->start,
			      PL_TERM, pe,
			      PL_TERM, pa,
			      PL_TERM, pt);
	      } else
		pa = pe = p2 = pt = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reading a list. Tmp is used to  read   the  next element. Tail is a very
special term-ref. It is always a reference   to the place where the next
term is to be written.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	      PL_put_term(tail, term);

	      for(;;)
	      { Word argp;

		if ( positions )
		  PL_unify_list(pa, p2, pa);
		TRY( complex_term(",|]", tmp, p2, _PL_rd PASS_LD) );
		argp = allocGlobal(3);
		*unRef(*valTermRef(tail)) = consPtr(argp,
						    TAG_COMPOUND|STG_GLOBAL);
		*argp++ = FUNCTOR_dot2;
		readValHandle(tmp, argp++, _PL_rd PASS_LD);
		setVar(*argp);
		setHandle(tail, makeRef(argp));
		
		token = get_token(must_be_op, _PL_rd);

		switch(token->value.character)
		{ case ']':
		    { if ( positions )
		      { PL_unify_nil(pa);
			PL_unify_atom(pt, ATOM_none);
			PL_unify_integer(pe, token->end);
		      }
		      return PL_unify_nil(tail);
		    }
		  case '|':
		    { TRY( complex_term("]", tmp, pt, _PL_rd PASS_LD) );
		      argp = unRef(*valTermRef(tail));
		      readValHandle(tmp, argp, _PL_rd PASS_LD);
		      token = get_token(must_be_op, _PL_rd); /* discard ']' */
		      if ( positions )
		      { PL_unify_nil(pa);
			PL_unify_integer(pe, token->end);
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
	  case '|':			/* TBD: we need this, but */
	  case ',':			/* it should NOT be possible to */
					/* modify these operators to atoms */
					/* later.  Not really trivial how */
					/* to do that! */
					/* now x,,y is read as x, ',', y! */
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
{ Token token;
  term_t result;
  Word p;

  if ( !(rd->base = raw_read(rd PASS_LD)) )
    fail;

  rd->here = rd->base;

  result = PL_new_term_ref();
  blockGC(PASS_LD1);
  if ( !complex_term(NULL, result, rd->subtpos, rd PASS_LD) )
    goto failed;
  p = valTermRef(result);
  if ( isVarAtom(*p, rd) )		/* reading a single variable */
    readValHandle(result, p, rd PASS_LD);

  if ( !(token = get_token(FALSE, rd)) )
    goto failed;
  if ( token->type != T_FULLSTOP )
  { errorWarning("end_of_clause_expected", 0, rd);
    goto failed;
  }

  if ( !PL_unify(term, result) )
    goto failed;
  if ( rd->varnames && !bind_variable_names(rd PASS_LD) )
    goto failed;
  if ( rd->variables && !bind_variables(rd PASS_LD) )
    goto failed;
  if ( rd->singles && !check_singletons(rd PASS_LD) )
    goto failed;

  PL_reset_term_refs(result);
  unblockGC(PASS_LD1);
  succeed;

failed:
  PL_reset_term_refs(result);
  unblockGC(PASS_LD1);
  fail;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_raw_read2(term_t from, term_t term)
{ GET_LD
  unsigned char *s, *top;
  read_data rd;
  word rval;
  IOSTREAM *in;

  if ( !getInputStream(from, &in) )
    fail;

  init_read_data(&rd, in PASS_LD);
  if ( !(s = raw_read(&rd PASS_LD)) )
  { rval = PL_raise_exception(rd.exception);
    goto out;
  }

					/* strip the input from blanks */
  for(top = rd._rb.here-1; top > s && isBlank(*top); top--);
  if ( *top == '.' )
  { for(top--; top > s && isBlank(*top); top--)
      ;
  }
  *++top = EOS;

  for(; s < top && isBlank(*s); s++)
    ;

  rval = PL_unify_atom_nchars(term, top-s, (char *)s);

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
  mark m;

retry:
  Mark(m);

  init_read_data(&rd, s PASS_LD);
  rd.on_error = ATOM_dec10;
  rd.singles = rd.styleCheck & SINGLETON_CHECK ? TRUE : FALSE;
  if ( !(rval = read_term(term, &rd PASS_LD)) && rd.has_exception )
  { if ( reportReadError(&rd) )
    { Undo(m);
      free_read_data(&rd);
      goto retry;
    }
  }
  free_read_data(&rd);

  return rval;
}


static
PRED_IMPL("read_clause", 2, read_clause, 0)
{ PRED_LD
  int rval;

  switch( CTX_ARITY )
  { case 1:
    { IOSTREAM *s;

      if ( !getInputStream(0, &s) )	/* Scurin */
	fail;
      rval = read_clause(s, A1 PASS_LD);
      if ( Sferror(s) )
	return streamStatus(s);
      else
	PL_release_stream(s);

      return rval;
    }
    case 2:
    { IOSTREAM *s;

      if ( !getInputStream(A1, &s) )
	fail;
      rval = read_clause(s, A2 PASS_LD);
      if ( Sferror(s) )
	return streamStatus(s);
      else
	PL_release_stream(s);

      return rval;
    }
  }
  assert(0);
  fail;
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
  { NULL_ATOM,	     	    0 }
};

word
pl_read_term3(term_t from, term_t term, term_t options)
{ GET_LD
  term_t tpos = 0;
  int rval;
  atom_t w;
  read_data rd;
  IOSTREAM *s;
  bool charescapes = -1;
  atom_t dq = NULL_ATOM;
  atom_t mname = NULL_ATOM;
  mark m;

retry:
  Mark(m);

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
		     &rd.backquoted_string) )
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

  rval = read_term(term, &rd PASS_LD);
  if ( Sferror(s) )
    rval = streamStatus(s);
  else
    PL_release_stream(s);

  if ( rval )
  { if ( tpos && source_line_no > 0 )
      rval = PL_unify_term(tpos,
			   PL_FUNCTOR, FUNCTOR_stream_position3,
			   PL_LONG, source_char_no,
			   PL_INT,  source_line_no,
			   PL_INT,  0); /* should be charpos! */
  } else
  { if ( rd.has_exception && reportReadError(&rd) )
    { Undo(m);
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

word
pl_atom_to_term(term_t atom, term_t term, term_t bindings)
{ GET_LD
  char *s;

  if ( PL_is_variable(atom) )
  { char buf[1024];
    int bufsize = sizeof(buf);
    word rval;

    s = buf;
    tellString(&s, &bufsize);
    pl_writeq(term);
    toldString();

    rval = PL_unify_atom_nchars(atom, bufsize, s);
    if ( s != buf )
      free(s);

    return rval;
  }

  if ( PL_get_chars_ex(atom, &s, CVT_ALL) )
  { GET_LD
    read_data rd;
    word rval;
    IOSTREAM *stream = Sopen_string(NULL, (char *)s, -1, "r");
    source_location oldsrc = LD->read_source;

    init_read_data(&rd, stream PASS_LD);
    if ( PL_is_variable(bindings) || PL_is_list(bindings) )
      rd.varnames = bindings;

    if ( !(rval = read_term(term, &rd PASS_LD)) && rd.has_exception )
      rval = PL_raise_exception(rd.exception);
    free_read_data(&rd);
    Sclose(stream);
    LD->read_source = oldsrc;

    return rval;
  }

  fail;
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
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(read)
  PRED_DEF("read_clause", 1, read_clause, 0)
  PRED_DEF("read_clause", 2, read_clause, 0)
EndPredDefs
