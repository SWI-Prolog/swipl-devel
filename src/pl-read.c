/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: read/1, 2
*/

#include <math.h>
/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-ctype.h"

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

forwards void	startRead(void);
forwards void	stopRead(void);
forwards void	errorWarning(char *);
forwards void	singletonWarning(atom_t *, int);
forwards void	clearBuffer(void);
forwards void	addToBuffer(int);
forwards char *	raw_read2(void);
forwards char *	raw_read(void);

typedef struct token * Token;
typedef struct variable * Variable;

struct token
{ int type;			/* type of token */
  union
  { number	number;		/* int or float */
    atom_t	atom;		/* atom value */
    term_t	term;		/* term (list or string) */
    int		character;	/* a punctuation character (T_PUNCTUATION) */
    Variable	variable;	/* a variable record (T_VARIABLE) */
  } value;			/* value of token */
};


struct variable
{ struct atom	atom;		/* Normal atom ($VAR/0) */
  char *	name;		/* Name of the variable */
  term_t	variable;	/* Term-reference to the variable */
  int		times;		/* Number of occurences */
  Variable 	next;		/* Next of chain */
};


#define T_FUNCTOR	0	/* name of a functor (atom, followed by '(') */
#define T_NAME		1	/* ordinary name */
#define T_VARIABLE	2	/* variable name */
#define T_VOID		3	/* void variable */
#define T_NUMBER	4	/* integer or float */
#define T_STRING	5	/* "string" */
#define T_PUNCTUATION	6	/* punctuation character */
#define T_FULLSTOP	7	/* Prolog end of clause */

extern int Input;		/* current input stream (from pl-file.c) */
static char *here;		/* current character */
static char *base;		/* base of clause */
static char *token_start;	/* start of most recent read token */
static struct token token;	/* current token */
static bool unget = FALSE;	/* unget_token() */

#define CHARESCAPE trueFeature(CHARESCAPE_FEATURE)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The reading function (raw_read()) can  be   called  recursively  via the
notifier when running under event-driven packages  (like PCE).  To avoid
corruption of the database we push the read buffer rb on a stack and pop
in back when finished.  See raw_read() and raw_read2().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define RBSIZE	512		/* initial size of read buffer */
#define MAX_READ_NESTING 5	/* nesting of read (O_PCE only) */

static
struct read_buffer
{ int	size;			/* current size of read buffer */
  int	left;			/* left space in read buffer */
  char *base;			/* base of read buffer */
  char *here;			/* current position in read buffer */
  int   stream;			/* stream we are reading from */
} rb;

#if O_PCE
static struct read_buffer rb_stack[MAX_READ_NESTING];
int read_nesting = 0;		/* current nesting level */
#endif /* O_PCE */

void
resetRead(void)
{ 
#if O_PCE
  read_nesting = 0;
#endif
}

static
void
startRead(void)
{
#if O_PCE
  if (read_nesting >= MAX_READ_NESTING)
  { warning("Read stack too deeply nested");
    pl_abort();
  }
  rb_stack[read_nesting++] = rb;
  rb = rb_stack[read_nesting];
#endif /* O_PCE */
  rb.stream = Input;
  source_file_name = currentStreamName();
}

static void
stopRead(void)
{
#if O_PCE
  rb_stack[read_nesting] = rb;
  rb = rb_stack[--read_nesting];
  if (read_nesting < 0)
    fatalError("Read stack underflow???");
#endif /* O_PCE */
}

		/********************************
		*         ERROR HANDLING        *
		*********************************/

static int give_syntaxerrors = TRUE;

int
syntaxerrors(int new)
{ int old = give_syntaxerrors;
  give_syntaxerrors = new;

  return old;
}


word
pl_syntaxerrors(term_t old, term_t new)
{ return setBoolean(&give_syntaxerrors, "syntaxerrors", old, new);
}


#define syntaxError(what) { errorWarning(what); fail; }

static void
errorWarning(char *what)
{ unsigned int c;
  
  if ( !give_syntaxerrors )
    return;

  c = *token_start;

  if ( !ReadingSource )			/* not reading from a file */
  { Sfprintf(Serror, "\n[WARNING: Syntax error: %s \n", what);
    *token_start = EOS;
    Sfprintf(Serror, "%s\n** here **\n", base);  
    if (c != EOS)
    { *token_start = c;
      Sfprintf(Serror, "%s]\n", token_start);
    }
  } else
  { fid_t cid = PL_open_foreign_frame();
    qid_t qid;
    term_t argv = PL_new_term_refs(3);
    term_t a    = PL_new_term_ref();
    predicate_t pred = PL_pred(FUNCTOR_exception3, MODULE_user);
    int rval;
    char *s;

    for(s = base; s < token_start; s++ )
    { if ( *s == '\n' )
      	source_line_no++;
    }
	
    PL_put_atom(    argv+0, ATOM_syntax_error);
    PL_put_functor( argv+1, FUNCTOR_syntax_error3);
    PL_put_variable(argv+2);
    PL_get_arg(1, argv+1, a); PL_unify_atom(a, source_file_name);
    PL_get_arg(2, argv+1, a); PL_unify_integer(a, source_line_no);
    PL_get_arg(3, argv+1, a); PL_unify_atom_chars(a, what);
	       
    qid = PL_open_query(MODULE_user, FALSE, pred, argv);
    rval = PL_next_solution(qid);
    PL_close_query(qid);
    PL_discard_foreign_frame(cid);

    if ( !rval )
      warning("%s:%d: Syntax error: %s",
	      stringAtom(source_file_name), source_line_no, what);
  }
}


static void
singletonWarning(atom_t *vars, int nvars)
{ fid_t cid = PL_open_foreign_frame();
  qid_t qid;
  term_t argv      = PL_new_term_refs(3);
  term_t a         = PL_new_term_ref();
  term_t h	   = PL_new_term_ref();
  predicate_t pred = PL_pred(FUNCTOR_exception3, MODULE_user);
  int n, rval;
	
  PL_put_atom(    argv+0, ATOM_singleton);
  PL_put_functor( argv+1, FUNCTOR_singleton3);
  PL_put_variable(argv+2);
  PL_get_arg(1, argv+1, a); PL_unify_atom(a, source_file_name);
  PL_get_arg(2, argv+1, a); PL_unify_integer(a, source_line_no);
  PL_get_arg(3, argv+1, a);
	       
  for(n=0; n<nvars; n++)
  { PL_unify_list(a, h, a);
    PL_unify_atom(h, vars[n]);
  }
  PL_unify_nil(a);

  qid = PL_open_query(MODULE_user, FALSE, pred, argv);
  rval = PL_next_solution(qid);
  PL_close_query(qid);
  PL_discard_foreign_frame(cid);

  if ( !rval )
  { char buf[LINESIZ];

    buf[0] = EOS;
    for(n=0; n<nvars; n++)
    { if ( n > 0 )
	strcat(buf, ", ");
      strcat(buf, stringAtom(vars[n]));
    }

    warning("Singleton variables: %s", buf);
  }
}


		/********************************
		*           RAW READING         *
		*********************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the input, give prompts when necessary and return a char *  holding
a  stripped  version of the next term.  Contigeous white space is mapped
on a single space, block and % ... \n comment  is  deleted.   Memory  is
claimed automatically en enlarged if necessary.

Earlier versions used to local stack for   building the term.  This does
not work with O_PCE as we might be  called back via the notifier while
reading.

(char *) NULL is returned on a syntax error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
clearBuffer()
{ if (rb.size == 0)
  { if ((rb.base = Malloc(RBSIZE)) == (char *) NULL)
      fatalError("%s", OsError());
    rb.size = RBSIZE;
  }
  SECURE( if ( rb.base == 0 ) fatalError("read/1: nesting=%d, size=%d",
						read_nesting, rb.size) );
  rb.left = rb.size;
  base = rb.here = rb.base;
  DEBUG(8, Sdprintf("Cleared read buffer.rb at %ld, base at %ld\n",
		  (long) &rb, (long) rb.base));
}      


static inline void
addToBuffer(int c)
{ if (rb.left-- == 0)
  { if ((rb.base = Realloc(rb.base, rb.size * 2)) == (char *)NULL)
      fatalError("%s", OsError());
    DEBUG(8, Sdprintf("Reallocated read buffer at %ld\n", (long) rb.base));
    base = rb.base;
    rb.here = rb.base + rb.size;
    rb.left = rb.size - 1;
    rb.size *= 2;
  }
  *rb.here++ = c & 0xff;
}


#define getchr() Sgetc(ioi)

#define ensure_space(c) { if ( something_read && \
			       (c == '\n'|| !isBlank(rb.here[-1])) ) \
			   addToBuffer(c); \
		        }
#define set_start_line { if ( !something_read ) \
			 { source_file_name = currentStreamName(); \
			   source_line_no = currentInputLine(); \
			   something_read++; \
			 } \
		       }

#define rawSyntaxError(what) { addToBuffer(EOS); \
			       base = rb.base, token_start = rb.here-1; \
			       syntaxError(what); \
			     }

static char *
raw_read2(void)
{ int c;
  bool something_read = FALSE;
  int newlines;
  bool dotseen = FALSE;
  IOSTREAM *ioi = PL_current_input();

  if ( !ioi )
  { c = EOF;
    goto handle_c;
  }
  
  clearBuffer();				/* clear input buffer */
  source_line_no = -1;

  for(;;)
  { c = getchr();

  handle_c:
    switch(c)
    { case EOF:
		if (seeingString())		/* do not require '. ' when */
		{ addToBuffer(' ');		/* reading from a string */
		  addToBuffer('.');
		  addToBuffer(' ');
		  addToBuffer(EOS);
		  return rb.base;
		}
		if (something_read)
		{ rawSyntaxError("Unexpected end of file");
		}
		if ( Sfpasteof(ioi) )
		{ warning("Attempt to read past end-of-file");
		  return NULL;
		}
		set_start_line;
		strcpy(rb.base, "end_of_file. ");
		return rb.base;
      case '/': c = getchr();
		if ( c == '*' )
		{ Char last;
		  int level = 1;
		  if ((last = getchr()) == EOF)
		    rawSyntaxError("End of file in ``/* ... */'' comment");
		  for(;;)
		  { switch(c = getchr())
		    { case EOF:
			rawSyntaxError("End of file in ``/* ... */'' comment");
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
		      case '\n':
			if ( something_read )
			  addToBuffer(c);
		    }
		    last = c;
		  }
		} else
		{ addToBuffer('/');
		  if ( char_type[c] == SY )
		  { while( c != EOF && char_type[c] == SY )
		    { addToBuffer(c);
		      c = getchr();
		    }
		  }
		  dotseen = FALSE;
		  goto handle_c;
		}
      case '%': while((c=getchr()) != EOF && c != '\n') ;
		c = ' ';
		goto handle_c;
     case '\'': if ( rb.here > rb.base && isDigit(rb.here[-1]) )
		{ addToBuffer(c);			/* <n>' */
		  if ( rb.here[-2] == '0' )		/* 0'<c> */
		  { if ( (c=getchr()) != EOF )
		    { addToBuffer(c);
		      break;
		    }
		    rawSyntaxError("Unexpected end of file");
		  }
		  dotseen = FALSE;
		  break;
		}

		set_start_line;
		newlines = 0;
		addToBuffer(c);
		while((c=getchr()) != EOF && c != '\'')
		{ if ( c == '\\' && CHARESCAPE )
		  { addToBuffer(c);
		    if ( (c = getchr()) == EOF )
		      goto eofinquoted;
		  } else if (c == '\n' &&
			     newlines++ > MAXNEWLINES &&
			     (debugstatus.styleCheck & LONGATOM_CHECK))
		    rawSyntaxError("Atom too long");

		  addToBuffer(c);
		}
		if (c == EOF)
		{ eofinquoted:
		  rawSyntaxError("End of file in quoted atom");
		}
		addToBuffer(c);
		dotseen = FALSE;
		break;
      case '"':	set_start_line;
		newlines = 0;
		addToBuffer(c);
		while((c=getchr()) != EOF && c != '"')
		{ if ( c == '\\' && CHARESCAPE )
		  { addToBuffer(c);
		    if ( (c = getchr()) == EOF )
		      goto eofinstr;
		  } else if (c == '\n' &&
			     newlines++ > MAXNEWLINES &&
			     (debugstatus.styleCheck & LONGATOM_CHECK))
		    rawSyntaxError("String too long");
		  addToBuffer(c);
		}
		if (c == EOF)
		{ eofinstr:
		  rawSyntaxError("End of file in string");
		}
		addToBuffer(c);
		dotseen = FALSE;
		break;
      case '.': addToBuffer(c);
		set_start_line;
		dotseen++;
		c = getchr();
		if ( char_type[c] == SY )
		{ while( c != EOF && char_type[c] == SY )
		  { addToBuffer(c);
		    c = getchr();
		  }
		  dotseen = FALSE;
		}
		goto handle_c;
      default:	switch(char_type[c])
		{ case SP:
		    if ( dotseen )
		    { if ( rb.here - rb.base == 1 )
			rawSyntaxError("Unexpected end of file");
		      ensure_space(c);
		      addToBuffer(EOS);
		      return rb.base;
		    }
		    do
		    { ensure_space(c);
		      c = getchr();
		    } while( c != EOF && char_type[c] == SP );
		    goto handle_c;
		  case SY:
		    set_start_line;
		    do
		    { addToBuffer(c);
		      c = getchr();
		    } while( c != EOF && char_type[c] == SY );
		    dotseen = FALSE;
		    goto handle_c;
		  case LC:
		  case UC:
		    set_start_line;
		    do
		    { addToBuffer(c);
		      c = getchr();
		    } while( c != EOF &&
			     (char_type[c] == LC || char_type[c] == UC) );
		    dotseen = FALSE;
		    goto handle_c;
		  default:
		    addToBuffer(c);
		    dotseen = FALSE;
		    set_start_line;
		}
    }
  }
}

static char *
raw_read(void)
{ char *s;

  startRead();
  if ( Input == 0 )
  { ttybuf tab;

    PushTty(&tab, TTY_SAVE);		/* make sure tty is sane */
    PopTty(&ttytab);
    s = raw_read2();
    PopTty(&tab);
  } else
    s = raw_read2();
  stopRead();

  return s;
}


		/*********************************
		*        VARIABLE DATABASE       *
		**********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions manipulate the  variable-name  base   for  a  term. When
building the term on the global stack, variables are represented using a
reference to a `struct variable'. The first  part of this structure is a
stuct atom, so if a garbage   collection  happens, the garbage collector
will simply assume an atom.

The buffer `var_buffer' is a  list  if   pointers  to  a  stock of these
variable structures. This list is dynamically expanded if necessary. The
buffer var_name_buffer contains the actually   strings.  They are packed
together to avoid memory fragmentation. This   buffer too is reallocated
if necessary. In this  case,  the   pointers  of  the  existing variable
structures are relocated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_SINGLETONS 256

static buffer	 var_name_buffer;	/* stores the names */
static buffer	 var_buffer;		/* array of Variables */
static int	 var_allocated;		/* # allocated slots there */
static int	 var_free;		/* 1-st free slot there */
static Variable	 var_list;		/* linked list of vars */
static Variable  var_tail;		/* tail of linked list */

static void
initVarTable()
{ var_list = var_tail = NULL;
  var_free = 0;
  emptyBuffer(&var_name_buffer);
}


static char *
save_var_name(const char *name)
{ int l = strlen(name);
  char *nb, *ob = baseBuffer(&var_name_buffer, char);
  int e = entriesBuffer(&var_name_buffer, char);

  addMultipleBuffer(&var_name_buffer, name, l+1, char);
  if ( (nb = baseBuffer(&var_name_buffer, char)) != ob )
  { int shift = nb - ob;
    Variable v;

    for(v = var_list; v; v = v->next)
      v->name += shift;
  }

  return baseBuffer(&var_name_buffer, char) + e;
}

					/* use hash-key? */

static char *uniquename = "read/1 tmp var";
#define isVarAtom(w) (isAtom(w) && stringAtom(w) == uniquename)

static Variable
lookupVariable(const char *name)
{ Variable v;

  for( v = var_list; v; v = v->next )
  { if ( streq(name, v->name) )
    { v->times++;
      return v;
    }
  }
       
  while ( var_free >= var_allocated )
  { Variable v = allocHeap(sizeof(struct variable));
    v->atom.next        = NULL;
    v->atom.hash_value  = 0;
    v->atom.name	= uniquename;
    addBuffer(&var_buffer, v, Variable);
    var_allocated++;
  }

  v = baseBuffer(&var_buffer, Variable)[var_free++];
  v->next     = NULL;
  v->name     = save_var_name(name);
  v->times    = 1;
  v->variable = 0;
  if ( var_tail )
  { var_tail->next = v;
    var_tail = v;
  } else
    var_list = var_tail = v;
  
  return v;
}


static void
check_singletons()
{ Variable var;
  atom_t singletons[MAX_SINGLETONS];
  int i=0;

  for(var = var_list; var; var=var->next)
  { if (var->times == 1 && var->name[0] != '_' && i < MAX_SINGLETONS)
      singletons[i++] = lookupAtom(var->name);
  }
  
  if ( i > 0 )
    singletonWarning(singletons, i);
}


static bool
bind_variables(term_t bindings)
{ Variable var;
  term_t list = PL_copy_term_ref(bindings);
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();

  for(var = var_list; var; var=var->next)
  { if ( var->name[0] != '_' )
    { if ( !PL_unify_list(list, head, list) ||
	   !PL_unify_functor(head, FUNCTOR_equals2) ||
	   !PL_get_arg(1, head, a) ||
	   !PL_unify_atom_chars(a, var->name) ||
	   !PL_get_arg(2, head, a) ||
	   !PL_unify(a, var->variable) )
	fail;
    }
  }

  return PL_unify_nil(list);
}


		/********************************
		*           TOKENISER           *
		*********************************/

#define skipSpaces	{ while(isBlank(*here) ) here++; c = *here++; }
#define unget_token()	{ unget = TRUE; }

forwards Token	get_token(bool);
forwards void	build_term(term_t term, atom_t name, int arity, term_t *args);
forwards bool	complex_term(const char *end, term_t term);
forwards bool	simple_term(bool, term_t term, bool *isname);

int
scan_number(char **s, int b, Number n)
{ int d;
  unsigned int maxi = PLMAXINT/b;	/* cache? */
  unsigned int t = 0;

  while((d = digitValue(b, **s)) >= 0)
  { (*s)++;

    if ( t > maxi )
    { real maxf = MAXREAL / (real) b - (real) b;
      real tf = (real)t;

      tf = tf * (real)b + (real)d;
      while((d = digitValue(b, **s)) >= 0)
      { (*s)++;
        if ( tf > maxf )
	  fail;				/* number too large */
        tf = tf * (real)b + (real)d;
      }
      n->value.f = tf;
      n->type = V_REAL;
      succeed;
    } else
      t = t * b + d;
  }  

  if ( t > PLMAXINT )
  { n->value.f = (real)t;
    n->type = V_REAL;
    succeed;
  }

  n->value.i = t;
  n->type = V_INTEGER;
  succeed;
}


#define NEXT(v)	do { c = (v); goto next; } while(0) 
#define AddChar(c) \
	do \
	{ if ( n >= bufsize ) \
	  { if ( bufsize == 0 ) \
	    { bufsize = 512; \
	      buf = Malloc(bufsize); \
	    } else \
	    { bufsize *= 2; \
	      buf = Realloc(buf, bufsize); \
	    } \
	    if ( buf == NULL ) \
	      fatalError("%s", OsError()); \
	  } \
	  buf[n++] = (c); \
	} while(0)
		

static char *
get_string(char *in, char **end)
{ static char *buf;
  static int bufsize = 0;
  int n;
  int quote;
  char c;

  n = 0;
  quote = *in++;

  for(;;)
  { c = *in++;

    if ( c == quote )
    { if ( *in == quote )
      { in++;
	NEXT(quote);
      }

      break;
    }
    if ( c == '\\' && CHARESCAPE )
    { int c2;
      int base;
      int xdigits;
      
      switch((c2 = *in++))
      { case 'a':
	  NEXT(7);			/* 7 is ASCII BELL */
	case 'b':
	  NEXT('\b');
	case 'c':
	  while(isBlank(*in))
	    in++;
	  continue;
	case 10:			/* linefeed */
	  while(isBlank(*in) && *in != 10 )
	    in++;
	  continue;
	case 'f':
	  NEXT('\f');
	case 'n':
	  NEXT('\n');
	case 'r':
	  NEXT('\r');
	case 't':
	  NEXT('\t');
	case 'v':
	  NEXT(11);			/* 11 is ASCII Vertical Tab */
	case 'x':
	  c2 = *in++;
	  if ( digitValue(16, c2) >= 0 )
	  { base = 16;
	    xdigits = 1;
	    goto numchar;
	  } else
	    NEXT('x');
	default:
	  if ( c2 >= '0' && c2 <= '7' )	/* octal number */
	  { int chr;
	    int dv;

	    base = 8;
	    xdigits = 2;

	  numchar:
	    chr = digitValue(base, c2);
	    c2 = *in++;
	    while(xdigits-- > 0 &&
		  (dv = digitValue(base, c2)) >= 0 )
	    { chr = chr * base + dv;
	      c2 = *in++;
	    }
	    if ( c2 != '\\' )
	      in--;
	    NEXT(chr);
	  } else
	    NEXT(c2);
      }
    }

  next:
    AddChar(c);
  }

  AddChar(EOS);
  if ( end )
    *end = in;

  return buf;
}  


int
get_number(char *in, char **end, Number value)
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
      { if ( isAlpha(in[3]) )
	  fail;				/* illegal number */
	value->value.i = (long)in[2];
	if ( negative )			/* -0'a is a bit dubious! */
	  value->value.i = -value->value.i;
	*end = in+3;
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
      *end = in;
      return rval;
    }
  }

  if ( !isDigit(*in) || !scan_number(&in, 10, value) )
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

    *end = in;
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

    if ( !scan_number(&in, 10, &exponent) || !intNumber(&exponent) )
      fail;				/* too large exponent */

    if ( intNumber(value) )
    { value->value.f = (double) value->value.i;
      value->type = V_REAL;
    }

    value->value.f *= pow((double)10.0,
			  neg_exponent ? -(double)exponent.value.i
			               : (double)exponent.value.i);
  }

  if ( isAlpha(c = *in) )
    fail;				/* illegal number */

  if ( negative )
  { if ( intNumber(value) )
      value->value.i = -value->value.i;
    else
      value->value.f = -value->value.f;
  }

  *end = in;
  succeed;
}


static Token
get_token(bool must_be_op)
{ unsigned int c;
  char *start;
  int end;

  if ( unget )
  { unget = FALSE;
    return &token;
  }

  skipSpaces;
  token_start = here - 1;
  switch(char_type[c])
  { case LC:	{ start = here-1;
		  while(isAlpha(*here) )
		    here++;
		  c = *here;
		  *here = EOS;
		  token.value.atom = lookupAtom(start);
		  *here = c;
		  token.type = (c == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n", c == '(' ? "FUNC" : "NAME",
				    stringAtom(token.value.atom)));

		  return &token;
		}
    case UC:	{ start = here-1;
		  while(isAlpha(*here) )
		    here++;
		  c = *here;
		  *here = EOS;
		  if (start[0] == '_' && here == start + 1)
		  { DEBUG(9, Sdprintf("VOID\n"));
		    token.type = T_VOID;
		  } else
		  { token.value.variable = lookupVariable(start);
		    DEBUG(9, Sdprintf("VAR: %s\n", token.value.variable->name));
		    token.type = T_VARIABLE;
		  }
		  *here = c;

		  return &token;
		}
    case_digit:
    case DI:	{ number value;

		  if ( get_number(&here[-1], &here, &value) )
		  { token.value.number = value;
		    token.type = T_NUMBER;
		    return &token;
		  } else
		    syntaxError("Illegal number");
		}
    case SO:	{ char tmp[2];

		  tmp[0] = c, tmp[1] = EOS;
		  token.value.atom = lookupAtom(tmp);
		  token.type = (*here == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n",
				  *here == '(' ? "FUNC" : "NAME",
				  stringAtom(token.value.atom)));

		  return &token;
		}
    case SY:	{ start = here - 1;
		  while( isSymbol(*here) )
		    here++;

		  if ( here == start+1 )
		  { if ( (c == '+' || c == '-') &&	/* +- number */
			 !must_be_op &&
			 isDigit(*here) )
		    { goto case_digit;
		    }
		    if ( c == '.' && isBlank(*here) )	/* .<blank> */
		    { token.type = T_FULLSTOP;
		      return &token;
		    }
		  }

		  end = *here, *here = EOS;
		  token.value.atom = lookupAtom(start);
		  *here = end;

		  token.type = (end == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n",
				    end == '(' ? "FUNC" : "NAME",
				    stringAtom(token.value.atom)));

		  return &token;
		}
    case PU:	{ switch(c)
		  { case '{':
		    case '[':
		      while( isBlank(*here) )
			here++;
		      if (here[0] == matchingBracket(c))
		      { here++;
			token.value.atom = (c == '[' ? ATOM_nil : ATOM_curl);
			token.type = here[0] == '(' ? T_FUNCTOR : T_NAME;
			DEBUG(9, Sdprintf("NAME: %s\n",
					  stringAtom(token.value.atom)));
			return &token;
		      }
		  }
		  token.value.character = c;
		  token.type = T_PUNCTUATION;
		  DEBUG(9, Sdprintf("PUNCT: %c\n", token.value.character));

		  return &token;
		}
    case SQ:	{ char *s = get_string(here-1, &here);

		  token.value.atom = lookupAtom(s);
		  token.type = (here[0] == '(' ? T_FUNCTOR : T_NAME);
		  return &token;
		}
    case DQ:	{ char *s = get_string(here-1, &here);
		  term_t t = PL_new_term_ref();

#if O_STRING
 		  if ( debugstatus.styleCheck & STRING_STYLE )
		    PL_put_string_chars(t, s);
		  else
		    PL_put_list_chars(t, s);
#else
		  PL_put_list_chars(t, s);
#endif /* O_STRING */
  		  token.value.term = t;
		  token.type = T_STRING;
		  return &token;
		}
    default:	{ sysError("read/1: tokeniser internal error");
    		  return &token;	/* make lint happy */
		}
  }
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
readValHandle(term_t term, Word argp)
{ word w = *valTermRef(term);

  if ( isVarAtom(w) )
  { Variable var = (Variable) atomValue(w);

    DEBUG(2, Sdprintf("readValHandle(): var at 0x%x\n", var));

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
build_term(term_t term, atom_t atom, int arity, term_t *argv)
{ FunctorDef functor = lookupFunctorDef(atom, arity);
  Word argp = allocGlobal(arity+1);

  DEBUG(9, Sdprintf("Building term %s/%d ... ", stringAtom(atom), arity));
  setHandle(term, consPtr(argp, TAG_COMPOUND|STG_GLOBAL));
  *argp++ = functor->functor;

  for( ; arity-- > 0; argv++, argp++)
    readValHandle(*argv, argp);

  DEBUG(9, Sdprintf("result: "); pl_write(term); Sdprintf("\n") );
}


static void
PL_assign_term(term_t to, term_t from)
{ *valTermRef(to) = *valTermRef(from);
}


		/********************************
		*             PARSER            *
		*********************************/

#define priorityClash { syntaxError("Operator priority clash"); }

#define MAX_TERM_NESTING 200

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This part of the parser actually constructs  the  term.   It  calls  the
tokeniser  to  find  the next token and assumes the tokeniser implements
one-token pushback efficiently.  It consists  of  two  mutual  recursive
functions:  complex_term()  which is involved with operator handling and
simple_term() which reads everything, except for operators.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ atom_t op;
  int	kind;
  int	left_pri;
  int	right_pri;
  int	op_pri;
} op_entry;

static bool
isOp(atom_t atom, int kind, op_entry *e)
{ Operator op = isCurrentOperator(atom, kind);
  int pri;

  if ( op == NULL )
    fail;
  e->op     = atom;
  e->kind   = kind;
  e->op_pri = pri = op->priority;

  switch(op->type)
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

#define PushOp() \
	side[side_n++] = in_op; \
	if ( side_n >= MAX_TERM_NESTING ) \
	  syntaxError("Operator stack overflow"); \
	side_p = (side_n == 1 ? side : side_p+1);
	
#define Modify(pri) \
	if ( side_p != NULL && pri > side_p->right_pri ) \
	{ term_t tmp; \
	  if ( side_p->kind == OP_PREFIX && rmo == 0 ) \
	  { DEBUG(1, Sdprintf("Prefix %s to atom\n", \
			      stringAtom(side_p->op))); \
	    rmo++; \
	    tmp = PL_new_term_ref(); \
	    PL_put_atom(tmp, side_p->op); \
	    out[out_n++] = tmp; \
	    side_n--; \
	    side_p = (side_n == 0 ? NULL : side_p-1); \
	  } else if ( side_p->kind == OP_INFIX && out_n > 0 && rmo == 0 && \
		      isOp(side_p->op, OP_POSTFIX, side_p) ) \
	  { DEBUG(1, Sdprintf("Infix %s to postfix\n", \
			      stringAtom(side_p->op))); \
	    rmo++; \
	    tmp = PL_new_term_ref(); \
	    build_term(tmp, side_p->op, 1, &out[out_n-1]); \
	    out[out_n-1] = tmp; \
	    side_n--; \
	    side_p = (side_n == 0 ? NULL : side_p-1); \
	  } \
	}

#define Reduce(cond) \
	while( out_n > 0 && side_p != NULL && (cond) ) \
	{ int arity = (side_p->kind == OP_INFIX ? 2 : 1); \
	  term_t tmp; \
 	  if ( arity > out_n ) break; \
	  DEBUG(1, Sdprintf("Reducing %s/%d\n", \
			    stringAtom(side_p->op), arity));\
	  tmp = PL_new_term_ref(); \
	  build_term(tmp, side_p->op, arity, &out[out_n - arity]); \
	  out[out_n-arity] = tmp; \
	  out_n -= (arity-1); \
	  side_n--; \
	  side_p = (side_n == 0 ? NULL : side_p-1); \
	}


static bool
complex_term(const char *stop, term_t term)
{ term_t out[MAX_TERM_NESTING];
  op_entry in_op, side[MAX_TERM_NESTING];
  int out_n = 0, side_n = 0;
  int rmo = 0;				/* Rands more than operators */
  op_entry *side_p = NULL;

  for(;;)
  { bool isname;
    Token token;
    term_t in = PL_new_term_ref();

    if ( out_n != 0 || side_n != 0 )	/* Check for end of term */
    { if ( (token = get_token(rmo == 1)) == (Token) NULL )
	fail;
      unget_token();			/* only look-ahead! */

      switch(token->type)
      { case T_FULLSTOP:
	  if ( stop == NULL )
	    goto exit;
	  break;
	case T_PUNCTUATION:
	{ if ( stop != NULL && strchr(stop, token->value.character) )
	    goto exit;
	}
      }
    }

					/* Read `simple' term */
    TRY( simple_term(rmo == 1, in, &isname) );

    if ( isname )			/* Check for operators */
    { atom_t name;

      PL_get_atom(in, &name);

      DEBUG(1, Sdprintf("name %s, rmo = %d\n", stringAtom(name), rmo));

      if ( isOp(name, OP_INFIX, &in_op) )
      { DEBUG(1, Sdprintf("Infix op: %s\n", stringAtom(name)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.op_pri > side_p->right_pri);
	  PushOp();
	  rmo--;

	  continue;
	}
      }
      if ( isOp(name, OP_POSTFIX, &in_op) )
      { DEBUG(1, Sdprintf("Postfix op: %s\n", stringAtom(name)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.op_pri > side_p->right_pri);
	  PushOp();	
	
	  continue;
	}
      }
      if ( rmo == 0 && isOp(name, OP_PREFIX, &in_op) )
      { DEBUG(1, Sdprintf("Prefix op: %s\n", stringAtom(name)));
	
	PushOp();

	continue;
      }
    }

    if ( rmo != 0 )
      syntaxError("Operator expected");
    rmo++;
    out[out_n++] = in;
    if	( out_n >= MAX_TERM_NESTING )
      syntaxError("Operant stack overflow");
  }

exit:
  Modify(1000000);
  Reduce(TRUE);

  if ( out_n == 1 && side_n == 0 )	/* simple term */
  { PL_assign_term(term, out[0]);
    succeed;
  }

  if ( out_n == 0 && side_n == 1 )	/* single operator */
  { PL_put_atom(term, side[0].op);
    succeed;
  }

  syntaxError("Unbalanced operators");
}


static bool
simple_term(bool must_be_op, term_t term, bool *name)
{ Token token;

  *name = FALSE;

  if ( !(token = get_token(must_be_op)) )
    fail;

  switch(token->type)
  { case T_FULLSTOP:
      syntaxError("Unexpected end of clause");
    case T_VOID:
      setHandle(term, 0L);		/* variable */
      succeed;
    case T_VARIABLE:
      setHandle(term, consPtr(token->value.variable, TAG_ATOM|STG_HEAP));
      DEBUG(2, Sdprintf("Pushed var at 0x%x\n", token->value.variable));
      succeed;
    case T_NAME:
      *name = TRUE;
      PL_put_atom(term, token->value.atom);
      succeed;
    case T_NUMBER:
      _PL_put_number(term, &token->value.number);
      succeed;
    case T_STRING:
      PL_put_term(term,	token->value.term);
      succeed;
    case T_FUNCTOR:
      { if ( must_be_op )
	{ *name = TRUE;
	  PL_put_atom(term, token->value.atom);
	} else
	{ term_t av[16];
	  int avn = 16;
	  term_t *argv = av;
	  int argc;
	  atom_t functor;

	  functor = token->value.atom;
	  argc = 0, argv;
	  get_token(must_be_op);	/* skip '(' */

	  do
	  { if ( argc == avn )
	    { term_t *nargv = alloca(sizeof(term_t) * avn * 2);
	      memcpy(nargv, argv, sizeof(term_t) * avn);
	      avn *= 2;
	      argv = nargv;
	    }
	    argv[argc] = PL_new_term_ref();
	    TRY( complex_term(",)", argv[argc]) );
	    argc++;
	    token = get_token(must_be_op); /* `,' or `)' */
	  } while(token->value.character == ',');

	  build_term(term, functor, argc, argv);
	}
	succeed;
      }
    case T_PUNCTUATION:
      { switch(token->value.character)
	{ case '(':
	    { TRY( complex_term(")", term) );
	      token = get_token(must_be_op);	/* skip ')' */

	      succeed;
	    }
	  case '{':
	    { term_t arg = PL_new_term_ref();

	      TRY( complex_term("}", arg) );
	      token = get_token(must_be_op);
	      build_term(term, ATOM_curl, 1, &arg);

	      succeed;
	    }
	  case '[':
	    { term_t tail = PL_new_term_ref();
	      term_t tmp  = PL_new_term_ref();

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reading a list. Tmp is used to  read   the  next element. Tail is a very
special term-ref. It is always a reference   to the place where the next
term is to be written.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	      PL_put_term(tail, term);

	      for(;;)
	      { Word argp;

		TRY( complex_term(",|]", tmp) );
		argp = allocGlobal(3);
		*unRef(*valTermRef(tail)) = consPtr(argp,
						    TAG_COMPOUND|STG_GLOBAL);
		*argp++ = FUNCTOR_dot2->functor;
		readValHandle(tmp, argp++);
		setVar(*argp);
		setHandle(tail, makeRef(argp));
		
		token = get_token(must_be_op);

		switch(token->value.character)
		{ case ']':
		    { return PL_unify_nil(tail);
		      succeed;
		    }
		  case '|':
		    { TRY( complex_term("]", tmp) );
		      argp = unRef(*valTermRef(tail));
		      readValHandle(tmp, argp);
		      token = get_token(must_be_op); /* discard ']' */
		      succeed;
		    }
		  case ',':
		      continue;
		}
	      }
	    }
	  case '|':
	  case ',':
	  case ')':
	  case '}':
	  case ']':
	  default:
	    { char tmp[2];

	      *name = TRUE; 
	      tmp[0] = token->value.character;
	      tmp[1] = EOS;
	      PL_put_atom_chars(term, tmp);

	      succeed;
	    }
	}
      } /* case T_PUNCTUATION */
    default:;
      sysError("read/1: Illegal token type (%d)", token->type);
      /*NOTREACHED*/
      fail;
  }
}


static bool
read_term(term_t term, term_t variables, bool check)
{ Token token;
  term_t result;
  Word p;

  if ( !(base = raw_read()) )
    fail;

  initVarTable();
  here = base;
  unget = FALSE;

  result = PL_new_term_ref();
  TRY(complex_term(NULL, result));
  p = valTermRef(result);
  if ( isVarAtom(*p) )			/* reading a single variable */
    readValHandle(result, p);

  if ( !(token = get_token(FALSE)) )
    fail;
  if (token->type != T_FULLSTOP)
    syntaxError("End of clause expected");

  TRY(PL_unify(term, result));
  if ( variables )
    TRY(bind_variables(variables) );
  if ( check )
    check_singletons();

  succeed;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_raw_read(term_t term)
{ char *s, *top;

  s = raw_read();

  if ( s == (char *) NULL )
    fail;
  
					/* strip the input from blanks */
  for(top = s+strlen(s)-1; top > s && isBlank(*top); top--);
  if ( *top == '.' )
  { for(top--; top > s && isBlank(*top); top--)
      ;
  }
  top[1] = EOS;

  for(; isBlank(*s); s++);

  return PL_unify_atom_chars(term, s);
}

word
pl_read_variables(term_t term, term_t variables)
{ return read_term(term, variables, FALSE);
}

word
pl_read_variables3(term_t stream, term_t term, term_t variables)
{ streamInput(stream, pl_read_variables(term, variables));
}

word
pl_read(term_t term)
{ return read_term(term, 0, FALSE);
}

word
pl_read2(term_t stream, term_t term)
{ streamInput(stream, pl_read(term));
}

word
pl_read_clause(term_t term)
{ return read_term(term, 0,
		   debugstatus.styleCheck & SINGLETON_CHECK ? TRUE : FALSE);
}

word
pl_read_clause2(term_t stream, term_t term)
{ streamInput(stream, pl_read_clause(term));
}
