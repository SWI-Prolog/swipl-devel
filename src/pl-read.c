/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: read/1, 2
*/

#include "pl-incl.h"
#include "pl-ctype.h"
#include <math.h>

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
forwards void	singletonWarning(Atom *, int);
forwards void	clearBuffer(void);
forwards void	addToBuffer(char);
forwards Char	getchr(void);
forwards char *	raw_read2(void);
forwards char *	raw_read(void);

typedef struct token * Token;
typedef struct variable * Variable;

struct token
{ int type;			/* type of token */
  union
  { word prolog;		/* a Prolog value */
    char character;		/* a punctuation character (T_PUNCTUATION) */
    Variable variable;		/* a variable record (T_VARIABLE) */
  } value;			/* value of token */
};

struct variable
{ Word		address;	/* address of variable */
  char *	name;		/* name of variable */
  int		times;		/* number of occurences */
  Variable 	next;		/* next of chain */
};

#define T_FUNCTOR	0	/* name of a functor (atom, followed by '(') */
#define T_NAME		1	/* ordinary name */
#define T_VARIABLE	2	/* variable name */
#define T_VOID		3	/* void variable */
#define T_REAL		4	/* realing point number */
#define T_INTEGER	5	/* integer */
#define T_STRING	6	/* "string" */
#define T_PUNCTUATION	7	/* punctuation character */
#define T_FULLSTOP	8	/* Prolog end of clause */

extern int Input;		/* current input stream (from pl-file.c) */
static char *here;		/* current character */
static char *base;		/* base of clause */
static char *token_start;	/* start of most recent read token */
static struct token token;	/* current token */
static bool unget = FALSE;	/* unget_token() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The reading function (raw_read()) can  be  called  recursively  via  the
notifier  when  running  under  notifier  based packages (like O_PCE).  To
avoid corruption of the database we push the read buffer rb on  a  stack
and pop in back when finished.  See raw_read() and raw_read2().
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
  FILE *fd;			/* file descriptor we are reading from */
  bool	doExtend;		/* extension mode on? */
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
  rb.doExtend = (Input == 0 && status.notty == FALSE);
  rb.stream = Input;
  rb.fd = checkInput(rb.stream);
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
  if (read_nesting > 0)
  { rb.fd = checkInput(rb.stream);
  /*source_file_name = currentStreamName();*/
  }
#endif /* O_PCE */
}

		/********************************
		*         ERROR HANDLING        *
		*********************************/

#define syntaxError(what) { errorWarning(what); fail; }

static void
errorWarning(char *what)
{ char c = *token_start;
  
  if ( !ReadingSource )			/* not reading from a file */
  { fprintf(stderr, "\n[WARNING: Syntax error: %s \n", what);
    *token_start = EOS;
    fprintf(stderr, "%s\n** here **\n", base);  
    if (c != EOS)
    { *token_start = c;
      fprintf(stderr, "%s]\n", token_start);
    }
  } else
  { char *s;
    word goal;
    word arg;
    mark m;

    for(s = base; s < token_start; s++ )
      if ( *s == '\n' )
      	source_line_no++;

    Mark(m);
    goal = globalFunctor(FUNCTOR_exception3);
    unifyAtomic(argTermP(goal, 0), ATOM_syntax_error);
    unifyFunctor(argTermP(goal, 1), FUNCTOR_syntax_error3);
    arg = argTerm(goal, 1);
    unifyAtomic(argTermP(arg, 0), source_file_name);
    unifyAtomic(argTermP(arg, 1), consNum(source_line_no));
    unifyAtomic(argTermP(arg, 2), lookupAtom(what));

    if ( callGoal(MODULE_user, goal, FALSE) == FALSE )
      warning("Syntax error: %s", what);
    Undo(m);
  }
}


static void
singletonWarning(Atom *vars, int nvars)
{ word goal = globalFunctor(FUNCTOR_exception3);
  word arg;
  Word a;
  int n;
  mark m;

  Mark(m);
  unifyAtomic(argTermP(goal, 0), ATOM_singleton);
  unifyFunctor(argTermP(goal, 1), FUNCTOR_singleton3);
  arg = argTerm(goal, 1);
  unifyAtomic(argTermP(arg, 0), source_file_name);
  unifyAtomic(argTermP(arg, 1), consNum(source_line_no));
  a = argTermP(arg, 2);
  for(n=0; n<nvars; n++)
  { unifyFunctor(a, FUNCTOR_dot2);
    unifyAtomic(argTermP(*a, 0), vars[n]);
    a = argTermP(*a, 1);
  }
  unifyAtomic(a, ATOM_nil);

  if ( callGoal(MODULE_user, goal, FALSE) == FALSE )
  { char buf[LINESIZ];

    buf[0] = EOS;
    for(n=0; n<nvars; n++)
    { if ( n > 0 )
	strcat(buf, ", ");
      strcat(buf, stringAtom(vars[n]));
    }

    warning("Singleton variables: %s", buf);
  }
  Undo(m);
}


		/********************************
		*           RAW READING         *
		*********************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the input, give prompts when necessary and return a char *  holding
a  stripped  version of the next term.  Contigeous white space is mapped
on a single space, block and % ... \n comment  is  deleted.   Memory  is
claimed automatically en enlarged if necessary.

Earlier versions used to local stack for building the term.   This  does
not  work  with  O_PCE  as  we might be called back via the notifier while
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
  DEBUG(8, printf("Cleared read buffer.rb at %ld, base at %ld\n",
		  (long) &rb, (long) rb.base));
}      


static void
addToBuffer(register char c)
{ if (rb.left-- == 0)
  { if ((rb.base = Realloc(rb.base, rb.size * 2)) == (char *)NULL)
      fatalError("%s", OsError());
    DEBUG(8, printf("Reallocated read buffer at %ld\n", (long) rb.base));
    base = rb.base;
    rb.here = rb.base + rb.size;
    rb.left = rb.size - 1;
    rb.size *= 2;
  }
  *rb.here++ = c;
}


static Char
getchr(void)
{ register Char c;

  if (rb.fd == (FILE *)NULL)
  { c =  Get0();
    base = rb.base;
  } else if ((c = (Char) Getc(rb.fd)) == '\n')
    newLineInput();

  return c;
}

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
{ register Char c;
  bool something_read = FALSE;
  int newlines;

  clearBuffer();				/* clear input buffer */
  source_line_no = -1;

  for(;;)
  { c = getchr();
    DEBUG(3, if ( Input == 0 ) printf("getchr() -> %d (%c)\n", c, c));
    DEBUG(3, if ( Input == 0 ) printf("here = %ld, base = %ld",
				      (long) rb.here, (long) rb.base));
#if !O_READLINE
    if ( c == ttytab.tab.c_cc[VEOF] )		/* little hack ... */
      c = EOF ;
#endif

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
      e_o_f:
		strcpy(rb.base, "end_of_file. ");
		return rb.base;
      case '*':	if ( rb.here-rb.base >= 1 && rb.here[-1] == '/' )
		{ register Char last;
		  int level = 1;

		  rb.here--, rb.left++;	/* delete read '/' */

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
			  goto case_default; /* hack */
			}
			break;
		      case '\n':
			if ( something_read )
			  addToBuffer(c);
		    }
		    last = c;
		  }
		}

		set_start_line;
		addToBuffer(c);
		break;
      case '%': while((c=getchr()) != EOF && c != '\n') ;
		if (c == EOF)
		{ if (something_read)
		    rawSyntaxError("Unexpected end of file")
		  else
		    goto e_o_f;		  
		}

		goto case_default;		/* Hack */
     case '\'': if ( rb.here > rb.base && isDigit(rb.here[-1]) )
		{ addToBuffer(c);			/* <n>' */
		  if ( rb.here[-2] == '0' )		/* 0'<c> */
		  { if ( (c=getchr()) != EOF )
		    { addToBuffer(c);
		      break;
		    }
		    rawSyntaxError("Unexpected end of file");
		  }
		  break;
		}

		set_start_line;
		newlines = 0;
		addToBuffer(c);
		while((c=getchr()) != EOF && c != '\'')
		{ if (c == '\n' &&
		       newlines++ > MAXNEWLINES &&
		       (debugstatus.styleCheck & LONGATOM_CHECK))
		    rawSyntaxError("Atom too long");
		  addToBuffer(c);
		}
		if (c == EOF)
		  rawSyntaxError("End of file in quoted atom");
		addToBuffer(c);
		break;
      case '"':	set_start_line;
		newlines = 0;
		addToBuffer(c);
		while((c=getchr()) != EOF && c != '"')
		{ if (c == '\n' &&
		       newlines++ > MAXNEWLINES &&
		       (debugstatus.styleCheck & LONGATOM_CHECK))
		    rawSyntaxError("String too long");
		  addToBuffer(c);
		}
		if (c == EOF)
		  rawSyntaxError("End of file in string");
		addToBuffer(c);
		break;
      case_default:				/* Hack, needs fixing */
      default:	if ( isBlank(c) )
		{ long rd;

		  rd = rb.here - rb.base;
		  if (rd == 1 && rb.here[-1] == '.')
		    rawSyntaxError("Unexpected end of clause");
		  if (rd >= 2)
		  { if ( rb.here[-1] == '.' &&
			 !isSymbol(rb.here[-2]) &&
			 !(rb.here[-2] == '\'' && rd >= 3 && rb.here[-3] == '0'))
		    { ensure_space(c);
		      addToBuffer(EOS);
		      return rb.base;
		    }
		  }
		  ensure_space(c);
		} else
		{ addToBuffer(c);
		  if ( c != '/' )	/* watch comment start */
		    set_start_line;
		}
		break;
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

#define VARHASHSIZE 16
#define MAX_SINGLETONS 250

static char *	 allocBase;		/* local allocation base */
#if !O_DYNAMIC_STACKS
static char *    allocTop;		/* top of allocation */
#endif
static Variable* varTable;		/* hashTable for variables */

forwards void	check_singletons(void);
forwards bool	bind_variables(Word);
forwards char *	alloc_var(size_t);
forwards char *	save_var_name(char *);
forwards Variable lookupVariable(char *);
forwards void	initVarTable(void);

static void
check_singletons(void)
{ register Variable var;
  int n;
  Atom singletons[MAX_SINGLETONS];
  int i=0;

  for(n=0; n<VARHASHSIZE; n++)
  { for(var = varTable[n]; var; var=var->next)
    { if (var->times == 1 && var->name[0] != '_' && i < MAX_SINGLETONS)
	singletons[i++] = lookupAtom(var->name);
    }
  }
  
  if ( i > 0 )
    singletonWarning(singletons, i);
}

/*  construct a list of Var = <name> terms wich indicate the bindings
    of the variables. Anonymous variables are skipped. The result is
    unified with the argument.

 ** Sat Apr 16 23:09:04 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
bind_variables(Word bindings)
{ Variable var;
  int n;
  word binding;
  Word arg;

  for(n=0; n<VARHASHSIZE; n++)
  { for(var = varTable[n]; var; var=var->next)
    { if (var->name[0] != '_')
      { binding = globalFunctor(FUNCTOR_equals2);
	arg     = argTermP(binding, 0);
	*arg++  = (word) lookupAtom(var->name);
	*arg    = makeRef(var->address);
	APPENDLIST(bindings, &binding);
      }
    }
  }
  CLOSELIST(bindings);

  succeed;
}

static char *
alloc_var(register size_t n)
{ register char *space;

  n = ROUND(n, sizeof(word));

  STACKVERIFY(if (allocBase + n > allocTop) outOf((Stack)&stacks.local) );

  space = allocBase;
  allocBase += n;

  return space;
}

static char *
save_var_name(char *s)
{ char *copy = alloc_var(strlen(s) + 1);

  strcpy(copy, s);

  return copy;
}

static Variable
lookupVariable(char *s)
{ int v = stringHashValue(s, VARHASHSIZE);
  Variable var;

  for(var=varTable[v]; var; var=var->next)
  { if (streq(s, var->name) )
    { var->times++;
      return var;
    }
  }
  var = (Variable) alloc_var((size_t) sizeof(struct variable));
  DEBUG(9, printf("Allocated var at %ld\n", (long) var));
  var->next = varTable[v];
  varTable[v] = var;
  var->name = save_var_name(s);
  var->times = 1;
  var->address = (Word) NULL;

  return var;
}

static void
initVarTable(void)
{ int n;

  allocBase = (char *)(lTop+1) + (MAXARITY+MAXVARIABLES) * sizeof(word);
#if !O_DYNAMIC_STACKS
  allocTop  = (char *)lMax;
#endif

  varTable = (Variable *)alloc_var((size_t) sizeof(Variable)*VARHASHSIZE);
  for(n=0; n<VARHASHSIZE; n++)
    varTable[n] = (Variable) NULL;
}

		/********************************
		*           TOKENISER           *
		*********************************/

#define skipSpaces	{ while(isBlank(*here) ) here++; c = *here++; }
#define unget_token()	{ unget = TRUE; }

forwards Token	get_token(bool);
forwards word	build_term(Atom, int, Word);
forwards bool	complex_term(char *, Word);
forwards bool	simple_term(bool, Word, bool *);
forwards bool	read_term(Word, Word, bool);

typedef union
{ long	i;
  real  r;
} number;

#define V_ERROR 0
#define V_REAL  1
#define V_INT   2

forwards int scan_number(char **, int, number *);

word
charpToNumber(char *s)
{ number n;
  int type = scan_number(&s, 10, &n);

  if ( *s == EOS )
  { switch(type)
    { case V_ERROR:
	fail;
      case V_INT:
	return consNum(n.i);
      case V_REAL:
      default:
	return globalReal(n.r);
    }
  }

  fail;
}


static int
scan_number(char **s, int b, number *n)
{ int d;

  n->i = 0;
  while((d = digitValue(b, **s)) >= 0)
  { (*s)++;
    n->i = n->i * b + d;
    if ( n->i > PLMAXINT )
    { n->r = (real) n->i;
      while((d = digitValue(b, **s)) >= 0)
      { (*s)++;
        if ( n->r > MAXREAL / (real) b - (real) d )
	  return V_ERROR;
        n->r = n->r * (real)b + (real)d;
      }
      return V_REAL;
    }
  }  

  return V_INT;
}


static Token
get_token(bool must_be_op)
{ char c;
  char *start;
  char end;
  int negative = 1;

  if (unget)
  { unget = FALSE;
    return &token;
  }

  skipSpaces;
  token_start = here - 1;
  switch(char_type[(unsigned)c & 0xff])
  { case LC:	{ start = here-1;
		  while(isAlpha(*here) )
		    here++;
		  c = *here;
		  *here = EOS;
		  token.value.prolog = (word)lookupAtom(start);
		  *here = c;
		  token.type = (c == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, printf("%s: %s\n", c == '(' ? "FUNC" : "NAME", stringAtom(token.value.prolog)));

		  return &token;
		}
    case UC:	{ start = here-1;
		  while(isAlpha(*here) )
		    here++;
		  c = *here;
		  *here = EOS;
		  if (start[0] == '_' && here == start + 1)
		  { setVar(token.value.prolog);
		    DEBUG(9, printf("VOID\n"));
		    token.type = T_VOID;
		  } else
		  { token.value.variable = lookupVariable(start);
		    DEBUG(9, printf("VAR: %s\n", token.value.variable->name));
		    token.type = T_VARIABLE;
		  }
		  *here = c;

		  return &token;
		}
    case_digit:
    case DI:	{ number value;
		  int tp;

		  if (c == '0' && *here == '\'')		/* 0'<char> */
		  { if (isAlpha(here[2]))
		    { here += 2;
		      syntaxError("Illegal number");
		    }
		    token.value.prolog = consNum((long)here[1] * negative);
		    token.type = T_INTEGER;
		    here += 2;

		    DEBUG(9, printf("INT: %ld\n", valNum(token.value.prolog)));
		    return &token;
		  }

		  here--;		/* start of token */
		  if ( (tp = scan_number(&here, 10, &value)) == V_ERROR )
		    syntaxError("Number too large");

					/* base'value number */
		  if ( *here == '\'' )
		  { here++;

		    if ( tp == V_REAL || value.i > 36 )
		      syntaxError("Base of <base>'<value> too large");
		    if ( (tp = scan_number(&here, (int)value.i, &value))
								== V_ERROR )
		      syntaxError("Number too large"); 

		    if (isAlpha(*here) )
		      syntaxError("Illegal number");

		    if ( tp == V_INT )
		    { token.value.prolog = consNum(value.i * negative);
		      token.type = T_INTEGER;
		    } else
		    { token.value.prolog = globalReal(value.r * negative);
		      token.type = T_REAL;
		    }

		    return &token;
		  }
					/* Real numbers */
		  if ( *here == '.' && isDigit(here[1]) )
		  { real n;

		    if ( tp == V_INT )
		    { value.r = (real) value.i;
		      tp = V_REAL;
		    }
		    n = 10.0, here++;
		    while(isDigit(c = *here) )
		    { here++;
		      value.r += (real)(c-'0') / n;
		      n *= 10.0;
		    }
		  }

		  if ( *here == 'e' || c == 'E' )
		  { number exponent;
		    bool neg_exponent;

		    here++;
		    DEBUG(9, printf("Exponent\n"));
		    switch(*here)
		    { case '-':		here++;
					neg_exponent = TRUE;
					break;
		      case '+':		here++;
		      default:		neg_exponent = FALSE;
					break;
		    }

		    if ( scan_number(&here, 10, &exponent) != V_INT )
		      syntaxError("Exponent too large");

		    if ( tp == V_INT )
		    { value.r = (real) value.i;
		      tp = V_REAL;
		    }

		    value.r *= pow((double)10.0,
				   neg_exponent ? -(double)exponent.i
				                : (double)exponent.i);
		  }

		  if ( isAlpha(c = *here) )
		    syntaxError("Illegal number");

		  if ( tp == V_REAL )
		  { token.value.prolog = globalReal(value.r * negative);
		    token.type = T_REAL;
		  } else
		  { token.value.prolog = consNum(value.i * negative);
		    token.type = T_INTEGER;
		  }

		  return &token;
		}		  
    case SO:	{ char tmp[2];

		  tmp[0] = c, tmp[1] = EOS;
		  token.value.prolog = (word) lookupAtom(tmp);
		  token.type = (*here == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, printf("%s: %s\n",
				  *here == '(' ? "FUNC" : "NAME",
				  stringAtom(token.value.prolog)));

		  return &token;
		}
    case SY:	{ if (c == '.' && isBlank(here[0]))
		  { token.type = T_FULLSTOP;
		    return &token;
		  }
		  start = here - 1;
		  while( isSymbol(*here) )
		    here++;
		  end = *here, *here = EOS;
		  token.value.prolog = (word) lookupAtom(start);
		  *here = end;
		  if ( !must_be_op && isDigit(end) ) /* +- <number> case */
		  { if ( token.value.prolog == (word) ATOM_minus )
		    { negative = -1;
		      c = *here++;
		      goto case_digit;
		    } else if ( token.value.prolog == (word) ATOM_plus )
		    { c = *here++;
		      goto case_digit;
		    }
		  }
		  token.type = (end == '(' ? T_FUNCTOR : T_NAME);
		  DEBUG(9, printf("%s: %s\n", end == '(' ? "FUNC" : "NAME", stringAtom(token.value.prolog)));

		  return &token;
		}
    case PU:	{ switch(c)
		  { case '{':
		    case '[': while(isBlank(*here) )
				here++;
			      if (here[0] == matchingBracket(c))
			      { here++;
				token.value.prolog =
				    (word)(c == '[' ? ATOM_nil : ATOM_curl);
				token.type = T_NAME;
				DEBUG(9, printf("NAME: %s\n", stringAtom(token.value.prolog)));
				return &token;
			      }
		  }
		  token.value.character = c;
		  token.type = T_PUNCTUATION;
		  DEBUG(9, printf("PUNCT: %c\n", token.value.character));

		  return &token;
		}
    case SQ:	{ char *s;

		  start = here;
		  for(s=start;;)
		  { if (*here == '\'')
		    { if (here[1] != '\'')
		      { end = *s, *s = EOS;
			token.value.prolog = (word) lookupAtom(start);
			*s = end;
			token.type = (here[1] == '(' ? T_FUNCTOR : T_NAME);
			here++;
			DEBUG(9, printf("%s: %s\n", here[1] == '(' ? "FUNC" : "NAME", stringAtom(token.value.prolog)));
			return &token;
		      }
		      here++;
		    }
		    *s++ = *here++;
		  }
		}
    case DQ:	{ char *s;

		  start = here;
		  for(s=start;;)
		  { if (*here == '"')
		    { if (here[1] != '"')
		      { end = *s, *s = EOS;
#if O_STRING
			if ( debugstatus.styleCheck & O_STRING_STYLE )
			  token.value.prolog = globalString(start);
			else
			  token.value.prolog = (word) stringToList(start);
#else
			token.value.prolog = (word) stringToList(start);
#endif /* O_STRING */
			DEBUG(9, printf("STR: %s\n", start));
			*s = end;
			token.type = T_STRING;
			here++;
			return &token;
		      }
		      here++;
		    }
		    *s++ = *here++;
		  }
		}
    default:	{ sysError("read/1: tokeniser internal error");
    		  return &token;	/* make lint happy */
		}
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build a term on the global stack, given the atom  of  the  functor,  the
arity  and  a  vector of arguments.  The argument vector either contains
nonvar terms or a reference to a variable block.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
build_term(Atom atom, int arity, Word argv)
{ FunctorDef functor = lookupFunctorDef(atom, arity);
  word term;
  Word argp;

  DEBUG(9, printf("Building term %s/%d ... ", stringAtom(atom), arity));
  term = globalFunctor(functor);
  argp = argTermP(term, 0);
  while(arity-- > 0)
  { if (isRef(*argv) )
    { Variable var;
#if O_NO_LEFT_CAST
      Word w;
      deRef2(argv, w);
      var = (Variable) w;
#else
      deRef2(argv, (Word)var);
#endif
      if (var->address == (Word) NULL)
	var->address = argp++;
      else
	*argp++ = makeRef(var->address);
      argv++;
    } else
      *argp++ = *argv++;
  }
  DEBUG(9, printf("result: "); pl_write(&term); printf("\n") );

  return term;
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
{ Atom	op;
  int	kind;
  int	left_pri;
  int	right_pri;
  int	op_pri;
} op_entry;

static bool
isOp(Atom atom, int kind, op_entry *e)
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
	{ if ( side_p->kind == OP_PREFIX && rmo == 0 ) \
	  { DEBUG(1, printf("Prefix %s to atom\n", stringAtom(side_p->op))); \
	    rmo++; \
	    out[out_n++] = (word) side_p->op; \
	    side_n--; \
	    side_p = (side_n == 0 ? NULL : side_p-1); \
	  } else if ( side_p->kind == OP_INFIX && out_n > 0 && rmo == 0 && \
		      isOp(side_p->op, OP_POSTFIX, side_p) ) \
	  { DEBUG(1, printf("Infix %s to postfix\n", stringAtom(side_p->op)));\
	    rmo++; \
	    out[out_n-1] = build_term(side_p->op, 1, &out[out_n-1]); \
	    side_n--; \
	    side_p = (side_n == 0 ? NULL : side_p-1); \
	  } \
	}

#define Reduce(cond) \
	while( out_n > 0 && side_p != NULL && (cond) ) \
	{ int arity = (side_p->kind == OP_INFIX ? 2 : 1); \
							  \
 	  if ( arity > out_n ) break; \
	  DEBUG(1, printf("Reducing %s/%d\n", stringAtom(side_p->op), arity));\
	  out[out_n-arity] = build_term(side_p->op, \
					arity, \
					&out[out_n - arity]); \
	  out_n -= (arity-1); \
	  side_n--; \
	  side_p = (side_n == 0 ? NULL : side_p-1); \
	}




static bool
complex_term(char *stop, Word term)
{ word out[MAX_TERM_NESTING];
  op_entry in_op, side[MAX_TERM_NESTING];
  int out_n = 0, side_n = 0;
  int rmo = 0;				/* Rands more than operators */
  op_entry *side_p = NULL;

  for(;;)
  { bool isname;
    Token token;
    word in;

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
    TRY( simple_term(rmo == 1, &in, &isname) );

    if ( isname )			/* Check for operators */
    { DEBUG(1, printf("name %s, rmo = %d\n", stringAtom((Atom) in), rmo));

      if ( isOp((Atom) in, OP_INFIX, &in_op) )
      { DEBUG(1, printf("Infix op: %s\n", stringAtom((Atom) in)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri > side_p->right_pri);
	  PushOp();
	  rmo--;

	  continue;
	}
      }
      if ( isOp((Atom) in, OP_POSTFIX, &in_op) )
      { DEBUG(1, printf("Postfix op: %s\n", stringAtom((Atom) in)));

	Modify(in_op.left_pri);
	if ( rmo == 1 )
	{ Reduce(in_op.left_pri > side_p->right_pri);
	  PushOp();	
	
	  continue;
	}
      }
      if ( rmo == 0 && isOp((Atom) in, OP_PREFIX, &in_op) )
      { DEBUG(1, printf("Prefix op: %s\n", stringAtom((Atom) in)));
	
	Reduce(in_op.left_pri > side_p->right_pri);
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
  { *term = out[0];
    succeed;
  }

  if ( out_n == 0 && side_n == 1 )	/* single operator */
  { *term = (word) side[0].op;
    succeed;
  }

  syntaxError("Unbalanced operators");
}


static bool
simple_term(bool must_be_op, Word term, bool *name)
{ Token token;

  DEBUG(9, printf("simple_term(): Stack at %ld\n", (long) &term));

  *name = FALSE;

  if ( (token = get_token(must_be_op)) == NULL )
    fail;

  switch(token->type)
  { case T_FULLSTOP:
      syntaxError("Unexpected end of clause");
    case T_VOID:
      { *term = token->value.prolog;
	succeed;
      }
    case T_VARIABLE:
      { *term = makeRef(token->value.variable);
	succeed;
      }
    case T_NAME:
      *name = TRUE;
    case T_REAL:
    case T_INTEGER:
    case T_STRING:
      {	*term = token->value.prolog;
	succeed;
      }
    case T_FUNCTOR:
      { if ( must_be_op )
	{ *name = TRUE;
	  *term = token->value.prolog;
	} else
	{ word argv[MAXARITY+1];
	  int argc;
	  Word argp;
	  word functor;

	  functor = token->value.prolog;
	  argc = 0, argp = argv;
	  get_token(must_be_op);	/* skip '(' */

	  do
	  { TRY( complex_term(",)", argp++) );
	    if (++argc > MAXARITY)
	      syntaxError("Arity too high");
	    token = get_token(must_be_op); /* `,' or `)' */
	  } while(token->value.character == ',');

	  *term = build_term((Atom)functor, argc, argv);
	}
	succeed;
      }
    case T_PUNCTUATION:
      { switch(token->value.character)
	{ case '(':
	    { word arg;

	      TRY( complex_term(")", &arg) );
	      token = get_token(must_be_op);	/* skip ')' */
	      *term = arg;

	      succeed;
	    }
	  case '{':
	    { word arg;

	      TRY( complex_term("}", &arg) );
	      token = get_token(must_be_op);
	      *term = build_term(ATOM_curl, 1, &arg);

	      succeed;
	    }
	  case '[':
	    { Word tail = term;
	      word arg[2];
	      Atom dot = ATOM_dot;

	      for(;;)
	      { TRY( complex_term(",|]", &arg[0]) );

		arg[1] = (word) NULL;
		*tail = build_term(dot, 2, arg);
		tail = argTermP(*tail, 1);
		token = get_token(must_be_op);

		switch(token->value.character)
		{ case ']':
		    { *tail = (word) ATOM_nil;
		      succeed;
		    }
		  case '|':
		    { TRY( complex_term("]", &arg[0]) );

		      if (isRef(arg[0]))
		      { Variable var;
#if O_NO_LEFT_CAST
			Word w;
			deRef2(&arg[0], w);
			var = (Variable) w;
#else
			deRef2(&arg[0], (Word)var);
#endif
			if (var->address == (Word) NULL)
			  var->address = tail;
			else
			  *tail = makeRef(var->address);
		      } else
			*tail = arg[0];

		      token = get_token(must_be_op);
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
	      *term = (word) lookupAtom(tmp);

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
read_term(Word term, Word variables, bool check)
{ Token token;
  word result;

  if ((base = raw_read()) == (char *) NULL)
    fail;

  initVarTable();
  here = base;
  unget = FALSE;

  TRY( complex_term(NULL, &result) );

  if ((token = get_token(FALSE)) == (Token) NULL)
    fail;
  if (token->type != T_FULLSTOP)
    syntaxError("End of clause expected");

  if ( isRef(result) )	/* term is a single variable! */
  { Variable var;
#if O_NO_LEFT_CAST
    Word w;
    deRef2(&result, w);
    var = (Variable) w;
#else
    deRef2(&result, (Word)var);
#endif
    if ( var->times != 1 || var->address != (Word)NULL )
      sysError("Error while reading a single variable??");
    var->address = allocGlobal(sizeof(word));
    setVar(*var->address);
    result = makeRef(var->address);
  }

  TRY(pl_unify(term, &result) );
  if (variables != (Word) NULL)
    TRY(bind_variables(variables) );
  if (check)
    check_singletons();

  succeed;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_raw_read(Word term)
{ char *s;
  register char *top;

  lockp(&term);
  s = raw_read();
  unlockp(&term);

  if ( s == (char *) NULL )
    fail;
  
  for(top = s+strlen(s)-1; isBlank(*top); top--);
  if (*top == '.')
    *top = EOS;
  for(; isBlank(*s); s++);

  return unifyAtomic(term, lookupAtom(s));
}

word
pl_read_variables(Word term, Word variables)
{ return read_term(term, variables, FALSE);
}

word
pl_read_variables3(Word stream, Word term, Word variables)
{ streamInput(stream, pl_read_variables(term, variables));
}

word
pl_read(Word term)
{ return read_term(term, (Word)NULL, FALSE);
}

word
pl_read2(Word stream, Word term)
{ streamInput(stream, pl_read(term));
}

word
pl_read_clause(Word term)
{ return read_term(term, (Word) NULL,
		   debugstatus.styleCheck & SINGLETON_CHECK ? TRUE : FALSE);
}

word
pl_read_clause2(Word stream, Word term)
{ streamInput(stream, pl_read_clause(term));
}


