/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl
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
  int start;			/* start-position */
  int end;			/* end-position */
  union
  { number	number;		/* int or float */
    atom_t	atom;		/* atom value */
    term_t	term;		/* term (list or string) */
    int		character;	/* a punctuation character (T_PUNCTUATION) */
    Variable	variable;	/* a variable record (T_VARIABLE) */
  } value;			/* value of token */
};


struct variable
{ char *	name;		/* Name of the variable */
  term_t	variable;	/* Term-reference to the variable */
  int		times;		/* Number of occurences */
  word		signature;	/* Pseudo atom */
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
static unsigned char *here;		/* current character */
static unsigned char *base;		/* base of clause */
static unsigned char *token_start;	/* start of most recent read token */
static char *last_syntax_error; /* last syntax error */
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
  unsigned char *base;		/* base of read buffer */
  unsigned char *here;		/* current position in read buffer */
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
  { source_char_no += token_start - base;
    last_syntax_error = what;
    return;
  }

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
  { predicate_t pred = PL_pred(FUNCTOR_exception3, MODULE_user);
    int rval;
    unsigned char *s;

    for(s = base; s < token_start; s++ )
    { if ( *s == '\n' )
      	source_line_no++;
    }
	
    if ( pred->definition->definition.clauses )
    { fid_t cid = PL_open_foreign_frame();
      qid_t qid;
      term_t argv = PL_new_term_refs(3);
      term_t a    = PL_new_term_ref();
    
      PL_put_atom(    argv+0, ATOM_syntax_error);
      PL_put_functor( argv+1, FUNCTOR_syntax_error3);
      PL_put_variable(argv+2);
      PL_get_arg(1, argv+1, a); PL_unify_atom(a, source_file_name);
      PL_get_arg(2, argv+1, a); PL_unify_integer(a, source_line_no);
      PL_get_arg(3, argv+1, a); PL_unify_atom_chars(a, what);
	       
      qid = PL_open_query(MODULE_user, PL_Q_NODEBUG, pred, argv);
      rval = PL_next_solution(qid);
      PL_close_query(qid);
      PL_discard_foreign_frame(cid);
    } else
      rval = FALSE;

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

  qid = PL_open_query(MODULE_user, PL_Q_NODEBUG, pred, argv);
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
  { if ( !(rb.base = (unsigned char *)malloc(RBSIZE)) )
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
  { if ( !(rb.base = (unsigned char *)realloc(rb.base, rb.size * 2)) )
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
			 { setCurrentSourceLocation(); \
			   something_read++; \
			 } \
		       }

#define rawSyntaxError(what) { addToBuffer(EOS); \
			       base = rb.base, token_start = rb.here-1; \
			       syntaxError(what); \
			     }

static char *
raw_read2()
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
		{ if ( dotseen )		/* term.<EOF> */
		  { if ( rb.here - rb.base == 1 )
		      rawSyntaxError("Unexpected end of clause");
		    ensure_space(' ');
		    addToBuffer(EOS);
		    return rb.base;
		  }
		  rawSyntaxError("Unexpected end of file");
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
		{ int last;
		  int level = 1;

		  if ((last = getchr()) == EOF)
		    rawSyntaxError("End of file in ``/* ... */'' comment");

		  if ( something_read )
		  { addToBuffer(' ');	/* positions */
		    addToBuffer(' ');
		    addToBuffer(last == '\n' ? last : ' ');
		  }

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
		    }
		    if ( something_read )
		      addToBuffer(c == '\n' ? c : ' ');
		    last = c;
		  }
		} else
		{ addToBuffer('/');
		  if ( isSymbol(c) )
		  { while( c != EOF && isSymbol(c) )
		    { addToBuffer(c);
		      c = getchr();
		    }
		  }
		  dotseen = FALSE;
		  goto handle_c;
		}
      case '%': if ( something_read )
		  addToBuffer(' ');	/* positions */
		while((c=getchr()) != EOF && c != '\n')
		{ if ( something_read )	/* record positions */
		    addToBuffer(' ');
		}
		c = '\n';
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
		if ( isSymbol(c) )
		{ while( c != EOF && isSymbol(c) )
		  { addToBuffer(c);
		    c = getchr();
		  }
		  dotseen = FALSE;
		}
		goto handle_c;
      default:	switch(_PL_char_types[c])
		{ case SP:
		    if ( dotseen )
		    { if ( rb.here - rb.base == 1 )
			rawSyntaxError("Unexpected end of file");
		      ensure_space(c);
		      addToBuffer(EOS);
		      return rb.base;
		    }
		    do
		    { if ( something_read )
			addToBuffer(c == '\n' ? c : ' '); /* positions */
		      else
			ensure_space(c);
		      c = getchr();
		    } while( c != EOF && isBlank(c) );
		    goto handle_c;
		  case SY:
		    set_start_line;
		    do
		    { addToBuffer(c);
		      c = getchr();
		    } while( c != EOF && isSymbol(c) == SY );
		    dotseen = FALSE;
		    goto handle_c;
		  case LC:
		  case UC:
		    set_start_line;
		    do
		    { addToBuffer(c);
		      c = getchr();
		    } while( c != EOF && isAlpha(c) );
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
struct atom, so if a garbage   collection happens, the garbage collector
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
{ static int done = 0;

  if ( !done )
  { initBuffer(&var_name_buffer);
    initBuffer(&var_buffer);
    done++;
  }

  var_list = var_tail = NULL;
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

static Variable
isVarAtom(word w)
{ if ( tagex(w) == (TAG_ATOM|STG_GLOBAL) )
    return baseBuffer(&var_buffer, Variable)[w>>7];
  
  return NULL;
}


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
    addBuffer(&var_buffer, v, Variable);
    v->signature = (var_allocated<<7)|TAG_ATOM|STG_GLOBAL;
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


static bool
check_singletons(term_t singles)
{ Variable var;

  if ( singles != TRUE )		/* returns <name> = var bindings */
  { term_t list = PL_copy_term_ref(singles);
    term_t head = PL_new_term_ref();

    for(var = var_list; var; var=var->next)
    { if ( var->times == 1 && var->name[0] != '_' )
      {	if ( !PL_unify_list(list, head, list) ||
	     !PL_unify_term(head,
			    PL_FUNCTOR, FUNCTOR_equals2,
			    PL_CHARS,	var->name,
			    PL_TERM,    var->variable) )
	  fail;
      }
    }

    return PL_unify_nil(list);
  } else				/* just report */
  { atom_t singletons[MAX_SINGLETONS];
    int i = 0;

    for(var = var_list; var; var=var->next)
    { if ( var->times == 1 && var->name[0] != '_' )
      { if ( singles == 1 )
	{ if ( i < MAX_SINGLETONS )
	    singletons[i++] = lookupAtom(var->name);
	}
      }
    }

    if ( i > 0 )
      singletonWarning(singletons, i);

    succeed;
  }
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
forwards bool	complex_term(const char *end, term_t term, term_t positions);
forwards bool	simple_term(bool, term_t term, bool *isname, term_t positions);

static int
scan_number(char **s, int b, Number n)
{ int d;
  unsigned long maxi = PLMAXINT/b;	/* cache? */
  unsigned long t = 0;

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
	      buf = malloc(bufsize); \
	    } else \
	    { bufsize *= 2; \
	      buf = realloc(buf, bufsize); \
	    } \
	    if ( buf == NULL ) \
	      fatalError("%s", OsError()); \
	  } \
	  buf[n++] = (c); \
	} while(0)
		

static char *
get_string(unsigned char *in, unsigned char **end)
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
get_number(const unsigned char *cin, unsigned char **end, Number value)
{ int negative = FALSE;
  unsigned int c;
  char *in = (char *)cin;		/* const hack */

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
	value->value.i = (long)in[2] & 0xff;
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
      if ( negative )
	 value->value.i = -value->value.i;
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
  unsigned char *start;
  int end;

  if ( unget )
  { unget = FALSE;
    return &token;
  }

  skipSpaces;
  token_start = here - 1;
  token.start = source_char_no + token_start - base;
  switch(_PL_char_types[c])
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

		  break;
		}
    case UC:	{ start = here-1;
		  while(isAlpha(*here) )
		    here++;
		  c = *here;
		  *here = EOS;
		  if ( c == '(' && trueFeature(ALLOW_VARNAME_FUNCTOR) )
		  { token.value.atom = lookupAtom(start);
		    token.type = T_FUNCTOR;
		    *here = c;
		    break;
		  }
		  if (start[0] == '_' && here == start + 1)
		  { DEBUG(9, Sdprintf("VOID\n"));
		    token.type = T_VOID;
		  } else
		  { token.value.variable = lookupVariable(start);
		    DEBUG(9, Sdprintf("VAR: %s\n",
				      token.value.variable->name));
		    token.type = T_VARIABLE;
		  }
		  *here = c;

		  break;
		}
    case_digit:
    case DI:	{ number value;

		  if ( get_number(&here[-1], &here, &value) &&
		       !isAlpha(here[0]) )
		  { token.value.number = value;
		    token.type = T_NUMBER;
		    break;
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

		  break;
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
		      break;
		    }
		  }

		  end = *here, *here = EOS;
		  token.value.atom = lookupAtom((char *)start);
		  *here = end;

		  token.type = (end == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, Sdprintf("%s: %s\n",
				    end == '(' ? "FUNC" : "NAME",
				    stringAtom(token.value.atom)));

		  break;
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
			goto out;
		      }
		  }
		  token.value.character = c;
		  token.type = T_PUNCTUATION;
		  DEBUG(9, Sdprintf("PUNCT: %c\n", token.value.character));

		  break;
		}
    case SQ:	{ char *s = get_string(here-1, &here);

		  token.value.atom = lookupAtom(s);
		  token.type = (here[0] == '(' ? T_FUNCTOR : T_NAME);
		  break;
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
		  break;
		}
    default:	{ sysError("read/1: tokeniser internal error");
    		  break;		/* make lint happy */
		}
  }

out:
  token.end = source_char_no + here - base;

  return &token;
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
  Variable var;

  if ( (var = isVarAtom(w)) )
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
build_term(term_t term, atom_t atom, int arity, term_t *argv)
{ functor_t functor = lookupFunctorDef(atom, arity);
  Word argp = allocGlobal(arity+1);

  DEBUG(9, Sdprintf("Building term %s/%d ... ", stringAtom(atom), arity));
  setHandle(term, consPtr(argp, TAG_COMPOUND|STG_GLOBAL));
  *argp++ = functor;

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This part of the parser actually constructs  the  term.   It  calls  the
tokeniser  to  find  the next token and assumes the tokeniser implements
one-token pushback efficiently.  It consists  of  two  mutual  recursive
functions:  complex_term()  which is involved with operator handling and
simple_term() which reads everything, except for operators.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

static void
build_op_term(term_t term, atom_t atom, int arity, out_entry *argv)
{ term_t av[2];

  av[0] = argv[0].term;
  av[1] = argv[1].term;
  build_term(term, atom, arity, av);
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
		      isOp(side[side_p].op, OP_POSTFIX, &side[side_p]) ) \
	  { DEBUG(9, Sdprintf("Infix %s to postfix\n", \
			      stringAtom(side[side_p].op))); \
	    rmo++; \
	    tmp = PL_new_term_ref(); \
	    build_op_term(tmp, side[side_p].op, 1, &out[out_n-1]); \
	    out[out_n-1].pri  = side[side_p].op_pri; \
	    out[out_n-1].term = tmp; \
	    side[side_p].kind = OP_POSTFIX; \
	    out[out_n-1].tpos = opPos(&side[side_p], &out[out_n-1]); \
	    side_n--; \
	    side_p = (side_n == 0 ? -1 : side_p-1); \
	  } \
	}


static int
get_int_arg(term_t t, int n)
{ Word p = valTermRef(t);

  deRef(p);

  return valInt(argTerm(*p, n-1));
}


static term_t
opPos(op_entry *op, out_entry *args)
{ if ( op->tpos )
  { int fs = get_int_arg(op->tpos, 1);
    int fe = get_int_arg(op->tpos, 2);
    term_t r = PL_new_term_ref();

    if ( op->kind == OP_INFIX )
    { int s = get_int_arg(args[0].tpos, 1);
      int e = get_int_arg(args[1].tpos, 2);

      PL_unify_term(r,
		    PL_FUNCTOR,	FUNCTOR_term_position5,
		    PL_INTEGER,	s,
		    PL_INTEGER,	e,
		    PL_INTEGER, fs,
		    PL_INTEGER, fe,
		    PL_LIST,    2, PL_TERM, args[0].tpos,
		    		   PL_TERM, args[1].tpos);
    } else
    { int s, e;
      
      if ( op->kind == OP_PREFIX )
      { s = fs;
	e = get_int_arg(args[0].tpos, 2);
      } else
      { s = get_int_arg(args[0].tpos, 1);
	e = fe;
      }

      PL_unify_term(r,
		    PL_FUNCTOR,	FUNCTOR_term_position5,
		    PL_INTEGER,	s,
		    PL_INTEGER,	e,
		    PL_INTEGER, fs,
		    PL_INTEGER, fe,
		    PL_LIST,    1, PL_TERM, args[0].tpos);
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
bad_operator(out_entry *out, op_entry *op)
{ char buf[1024];
  term_t t;
  atom_t name;
  int arity;
  char *opname = stringAtom(op->op);

  token_start = op->token_start;

  switch(op->kind)
  { case OP_INFIX:
      if ( op->left_pri < out[0].pri )
	t = out[0].term;
      else
      { token_start += strlen(opname);
	t = out[1].term;
      }
      break;
    case OP_PREFIX:
      token_start += strlen(opname);
      /*FALL THROUGH*/
    default:
      t = out[0].term;
  }

  if ( PL_get_name_arity(t, &name, &arity) )
  { Ssprintf(buf, "Operator `%s' conflicts with `%s'",
	     opname, stringAtom(name));
  } else
  { Ssprintf(buf, "Unknown conflict with operator `%s'", opname);
  }

  syntaxError(buf);
}

#define Reduce(cpri) \
	while( out_n > 0 && side_p >= 0 && (cpri) >= side[side_p].op_pri ) \
	{ int arity = (side[side_p].kind == OP_INFIX ? 2 : 1); \
	  term_t tmp; \
	  if ( arity > out_n ) break; \
	  if ( !can_reduce(&out[out_n-arity], &side[side_p]) ) \
          { if ( (cpri) == (OP_MAXPRIORITY+1) ) \
              return bad_operator(&out[out_n-arity], &side[side_p]); \
	    break; \
	  } \
	  DEBUG(9, Sdprintf("Reducing %s/%d\n", \
			    stringAtom(side[side_p].op), arity));\
	  tmp = PL_new_term_ref(); \
	  out_n -= arity; \
	  build_op_term(tmp, side[side_p].op, arity, &out[out_n]); \
	  out[out_n].pri  = side[side_p].op_pri; \
	  out[out_n].term = tmp; \
	  out[out_n].tpos = opPos(&side[side_p], &out[out_n]); \
	  out_n ++; \
	  side_n--; \
	  side_p = (side_n == 0 ? -1 : side_p-1); \
	}


static bool
complex_term(const char *stop, term_t term, term_t positions)
{ out_entry *out  = NULL;
  op_entry  *side = NULL;
  op_entry  in_op;
  int out_n = 0, side_n = 0;
  int rmo = 0;				/* Rands more than operators */
  int side_p = -1;
  term_t pin;

  for(;;)
  { bool isname;
    Token token;
    term_t in = PL_new_term_ref();

    if ( positions )
      pin = PL_new_term_ref();
    else
      pin = 0;
  
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
    TRY( simple_term(rmo == 1, in, &isname, pin) );

    if ( isname )			/* Check for operators */
    { atom_t name;

      PL_get_atom(in, &name);
      in_op.tpos = pin;
      in_op.token_start = token_start;

      DEBUG(9, Sdprintf("name %s, rmo = %d\n", stringAtom(name), rmo));

      if ( isOp(name, OP_INFIX, &in_op) )
      { DEBUG(9, Sdprintf("Infix op: %s\n", stringAtom(name)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri);
	  PushOp();
	  rmo--;

	  continue;
	}
      }
      if ( isOp(name, OP_POSTFIX, &in_op) )
      { DEBUG(9, Sdprintf("Postfix op: %s\n", stringAtom(name)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri);
	  PushOp();	
	
	  continue;
	}
      }
      if ( rmo == 0 && isOp(name, OP_PREFIX, &in_op) )
      { DEBUG(9, Sdprintf("Prefix op: %s\n", stringAtom(name)));
	
	PushOp();

	continue;
      }
    }

    if ( rmo != 0 )
      syntaxError("Operator expected");
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
  { PL_assign_term(term, out[0].term);
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

  syntaxError("Unbalanced operators");
}


static bool
simple_term(bool must_be_op, term_t term, bool *name, term_t positions)
{ Token token;

  *name = FALSE;

  if ( !(token = get_token(must_be_op)) )
    fail;

  switch(token->type)
  { case T_FULLSTOP:
      syntaxError("Unexpected end of clause");
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
      goto atomic_out;
    case T_NUMBER:
      _PL_put_number(term, &token->value.number);
    atomic_out:
      if ( positions )
      { PL_unify_term(positions,
		      PL_FUNCTOR, FUNCTOR_minus2,
		      PL_INTEGER, token->start,
		      PL_INTEGER, token->end);
      }
      succeed;
    case T_STRING:
      PL_put_term(term,	token->value.term);
      if ( positions )
      { PL_unify_term(positions,
		      PL_FUNCTOR, FUNCTOR_string_position2,
		      PL_INTEGER, token->start,
		      PL_INTEGER, token->end);
      }
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
	  term_t pa;			/* argument */
	  term_t pe;			/* term-end */
	  term_t ph;

	  if ( positions )
	  { pa = PL_new_term_ref();
	    pe = PL_new_term_ref();
	    ph = PL_new_term_ref();
	    PL_unify_term(positions,
			  PL_FUNCTOR, FUNCTOR_term_position5,
			  PL_INTEGER, token->start,
			  PL_TERM,    pe,
			  PL_INTEGER, token->start,
			  PL_INTEGER, token->end,
			  PL_TERM,    pa);
	  } else
	    pa = pe = ph = 0;

	  functor = token->value.atom;
	  argc = 0, argv;
	  get_token(must_be_op); /* skip '(' */

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
	    TRY( complex_term(",)", argv[argc], ph) );
	    argc++;
	    token = get_token(must_be_op); /* `,' or `)' */
	  } while(token->value.character == ',');

	  if ( positions )
	  { PL_unify_integer(pe, token->end);
	    PL_unify_nil(pa);
	  }

	  build_term(term, functor, argc, argv);
	}
	succeed;
      }
    case T_PUNCTUATION:
      { switch(token->value.character)
	{ case '(':
	    { int start = token->start;

	      TRY( complex_term(")", term, positions) );
	      token = get_token(must_be_op);	/* skip ')' */

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
			      PL_INTEGER, token->start,
			      PL_TERM,	  pe,
			      PL_TERM,	  pa);
	      } else
		pe = pa = 0;

	      TRY( complex_term("}", arg, pa) );
	      token = get_token(must_be_op);
	      if ( positions )
		PL_unify_integer(pe, token->end);
	      build_term(term, ATOM_curl, 1, &arg);

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
			      PL_INTEGER, token->start,
			      PL_TERM,    pe,
			      PL_TERM,	  pa,
			      PL_TERM,	  pt);
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
		TRY( complex_term(",|]", tmp, p2) );
		argp = allocGlobal(3);
		*unRef(*valTermRef(tail)) = consPtr(argp,
						    TAG_COMPOUND|STG_GLOBAL);
		*argp++ = FUNCTOR_dot2;
		readValHandle(tmp, argp++);
		setVar(*argp);
		setHandle(tail, makeRef(argp));
		
		token = get_token(must_be_op);

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
		    { TRY( complex_term("]", tmp, pt) );
		      argp = unRef(*valTermRef(tail));
		      readValHandle(tmp, argp);
		      token = get_token(must_be_op); /* discard ']' */
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
	  case ',':
	  case '|':
	  case ')':
	  case '}':
	  case ']':
	  default:
	    { static atom_t cache[256];
	      int c = token->value.character;

	      if ( !cache[c] )
	      { char tmp[2];

		tmp[0] = c;
		tmp[1] = EOS;
		cache[c] = lookupAtom(tmp);
	      }

	      *name = TRUE; 
	      PL_put_atom(term, cache[c]);

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
read_term(term_t term, term_t variables, term_t positions, term_t singles)
{ Token token;
  term_t result;
  Word p;

  if ( !(base = raw_read()) )
    fail;

  initVarTable();
  here = base;
  unget = FALSE;

  result = PL_new_term_ref();
  TRY(complex_term(NULL, result, positions));
  p = valTermRef(result);
  if ( isVarAtom(*p) )			/* reading a single variable */
    readValHandle(result, p);

  if ( !(token = get_token(FALSE)) )
    fail;
  if (token->type != T_FULLSTOP)
    syntaxError("End of clause expected");

  TRY(PL_unify(term, result));
  if ( variables )
    TRY(bind_variables(variables));
  if ( singles )
    TRY(check_singletons(singles));

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
pl_raw_read2(term_t stream, term_t term)
{ streamInput(stream, pl_raw_read(term));
}


word
pl_read_variables(term_t term, term_t variables)
{ return read_term(term, variables, 0, FALSE);
}

word
pl_read_variables3(term_t stream, term_t term, term_t variables)
{ streamInput(stream, pl_read_variables(term, variables));
}

word
pl_read(term_t term)
{ return read_term(term, 0, 0, FALSE);
}

word
pl_read2(term_t stream, term_t term)
{ streamInput(stream, pl_read(term));
}

word
pl_read_clause(term_t term)
{ return read_term(term, 0, 0,
		   debugstatus.styleCheck & SINGLETON_CHECK ? TRUE : FALSE);
}

word
pl_read_clause2(term_t stream, term_t term)
{ streamInput(stream, pl_read_clause(term));
}

static const opt_spec read_term_options[] = 
{ { ATOM_syntax_errors,	    OPT_TERM },	/* quiet, fail, X */
  { ATOM_variable_names,    OPT_TERM },
  { ATOM_singletons,        OPT_TERM },
  { ATOM_term_position,     OPT_TERM },
  { ATOM_subterm_positions, OPT_TERM },
  { NULL_ATOM,	     	    0 }
};

word
pl_read_term(term_t term, term_t options)
{ term_t syntax_errors = 0;		/* default */
  term_t varnames      = 0;
  term_t singles       = 0;
  term_t tpos	       = 0;
  term_t subtpos       = 0;
  int osyntax 	       = give_syntaxerrors;
  int rval;
  atom_t w;

  if ( !scan_options(options, 0, ATOM_read_option, read_term_options,
		     &syntax_errors, &varnames, &singles, &tpos, &subtpos) )
    return warning("read_term/2: illegal option list");

  if ( !syntax_errors ||
       (PL_get_atom(syntax_errors, &w) && w != ATOM_quiet) )
    give_syntaxerrors = TRUE;
  else
    give_syntaxerrors = FALSE;
  if ( singles && PL_get_atom(singles, &w) && w == ATOM_warning )
    singles = TRUE;
  rval = read_term(term, varnames, subtpos, singles);
  give_syntaxerrors = osyntax;

  if ( rval )
  { if ( tpos && source_line_no > 0 )
      TRY(PL_unify_term(tpos,
			PL_FUNCTOR, FUNCTOR_stream_position3,
			PL_INTEGER, source_char_no,
			PL_INTEGER, source_line_no,
			PL_INTEGER, 0)); /* should be charpos! */
    if ( syntax_errors && PL_is_variable(syntax_errors) )
      return PL_unify_atom(syntax_errors, ATOM_none);
  } else
  { if ( syntax_errors && PL_is_variable(syntax_errors) )
      return PL_unify_term(syntax_errors,
			   PL_FUNCTOR, FUNCTOR_module2,
			     PL_FUNCTOR, FUNCTOR_stream_position3,
			       PL_INTEGER, source_char_no,
			       PL_INTEGER, source_line_no,
			       PL_INTEGER, 0, /* should be charpos! */
			     PL_CHARS, last_syntax_error);
  }

  return rval;
}

word
pl_read_term3(term_t stream, term_t term, term_t options)
{ streamInput(stream, pl_read_term(term, options));
}
