/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formatted output (Prolog predicates format/[1,2,3]).   One  day,  the  C
source should also use format() to produce error messages, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"
#include "pl-ctype.h"

#define BUFSIZE 	10240
#define DEFAULT 	(-1)
#define SHIFT   	{ argc--; argv++; }
#define NEED_ARG	{ if ( argc <= 0 ) \
			  { FMT_ERROR("not enough arguments"); \
			  } \
			}
#define FMT_ERROR(fmt)	do \
			{ Sunlock(fd); \
			  return PL_error(NULL, 0, NULL, ERR_FORMAT, fmt); \
			} while(0)
#define FMT_ARG(c, a)	return Sunlock(fd), \
			       PL_error(NULL, 0, NULL, \
					ERR_FORMAT_ARG, c, a)
#define OUTSTRING(s, n)	{ int _i; char *q = s; \
			  for(_i=0; _i++<(int)(n); q++) OUTCHR(*q); \
			}
#define OUTSTRING0(s)	{ char *q = s; \
			  for( ; *q; q++ ) OUTCHR(*q); \
			}
#define OUTCHR(c)	{ if ( pending_rubber ) \
			    buffer[index++] = (c); \
			  else \
			    Sputc((Char)(c), fd); \
			  column = update_column(column, c); \
			}

#define MAXRUBBER 100

struct rubber
{ int where;				/* where is rubber in output */
  int size;				/* how big should it be */
  Char pad;				/* padding character */
};

#define format_predicates (GD->format.predicates)

static int	update_column(int, Char);
static bool	do_format(IOSTREAM *fd,
			  const char *fmt, unsigned len, int ac, term_t av);
static void	distribute_rubber(struct rubber *, int, int);
static void	emit_rubber(IOSTREAM *fd, char *, int, struct rubber *, int);

		/********************************
		*       PROLOG CONNECTION	*
		********************************/

word
pl_format_predicate(term_t chr, term_t descr)
{ int c;
  Procedure proc;
  Symbol s;

  if ( !PL_get_char_ex(chr, &c, FALSE) )
    fail;

  if ( !get_procedure(descr, &proc, 0, GP_CREATE) )
    fail;
  if ( proc->definition->functor->arity == 0 )
    return PL_error(NULL, 0, "arity must be > 0", ERR_DOMAIN,
		    PL_new_atom("format_predicate"),
		    descr);

  if ( !format_predicates )
    format_predicates = newHTable(8);
  
  if ( (s = lookupHTable(format_predicates, (void *)c)) )
    s->value = proc;
  else
    addHTable(format_predicates, (void *)c, proc);

  succeed;
}


word
pl_current_format_predicate(term_t chr, term_t descr, control_t h)
{ Symbol s = NULL;
  TableEnum e;
  mark m;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( !format_predicates )
	fail;
      e = newTableEnum(format_predicates);
      break;
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
      e = ForeignContextPtr(h);
      freeTableEnum(e);
    default:
      succeed;
  }

  while( (s=advanceTableEnum(e)) )
  { Mark(m);

    if ( PL_unify_integer(chr, (long)s->name) &&
	 unify_definition(descr, ((Procedure)s->value)->definition, 0, 0) )
    { ForeignRedoPtr(e);
    }
    Undo(m);
  }

  freeTableEnum(e);
  fail;
}


word
pl_format3(term_t stream, term_t fmt, term_t Args)
{ term_t argv;
  int argc = 0;
  char *f;
  term_t args = PL_copy_term_ref(Args);
  IOSTREAM *out;
  int rval;
  unsigned len;

  if ( !getOutputStream(stream, &out) )
    fail;

  if ( !PL_get_nchars(fmt, &len, &f, CVT_ALL|BUF_RING) )
    return PL_error("format", 3, NULL, ERR_TYPE, ATOM_text, fmt);

  if ( (argc = lengthList(args, FALSE)) >= 0 )
  { term_t head = PL_new_term_ref();
    int n = 0;

    argv = PL_new_term_refs(argc);
    while( PL_get_list(args, head, args) )
      PL_put_term(argv+n++, head);
  } else
  { argc = 1;
    argv = PL_new_term_refs(argc);

    PL_put_term(argv, args);
  }
  
  if ( (rval = do_format(out, f, len, argc, argv)) )
    return streamStatus(out);
  else
  { PL_release_stream(out);
    fail;
  }
}


word
pl_format(term_t fmt, term_t args)
{ return pl_format3(0, fmt, args);
}

		/********************************
		*       ACTUAL FORMATTING	*
		********************************/

static int
update_column(int col, int c)
{ switch(c)
  { case '\n':	return 0;
    case '\r':  return 0;
    case '\t':	return (col + 1) | 0x7;
    case '\b':	return (col <= 0 ? 0 : col - 1);
    default:	return col + 1;
  }
}   


static bool
do_format(IOSTREAM *fd, const char *fmt, unsigned len, int argc, term_t argv)
{ char buffer[BUFSIZE];			/* to store chars with tabs */
  int index = 0;			/* index in buffer */
  int column;				/* current output column */
  int tab_stop = 0;			/* padded tab stop */
  int pending_rubber = 0;		/* number of not-filled ~t's */
  struct rubber rub[MAXRUBBER];
  Symbol s;
  const char *end = fmt+len;

  Slock(fd);				/* buffer locally */

  if ( fd->position )
    column = fd->position->linepos;
  else
    column = 0;

  while(fmt < end)
  { switch(*fmt)
    { case '~':
	{ int arg = DEFAULT;		/* Numeric argument */
					/* Get the numeric argument */
	  if ( isDigit(*++fmt) )
	  { for( ; isDigit(*fmt); fmt++ )
	      arg = (arg == DEFAULT ? *fmt - '0' : arg*10 + *fmt - '0');
	  } else if ( *fmt == '*' )
	  { NEED_ARG;
	    if ( PL_get_integer(argv, &arg) )
	    { SHIFT;
	    } else
	      FMT_ERROR("no or negative integer for `*' argument");
	    fmt++;
	  } else if ( *fmt == '`' )
	  { arg = *++fmt;
	    fmt++;
	  }
	    
					/* Check for user defined format */
	  if ( format_predicates &&
	       (s = lookupHTable(format_predicates, (Void)((long)*fmt))) )
	  { Procedure proc = (Procedure) s->value;
	    FunctorDef fdef = proc->definition->functor;
	    term_t av = PL_new_term_refs(fdef->arity);
	    char buf[BUFSIZE];
	    char *str = buf;
	    int bufsize = BUFSIZE;
	    unsigned int i;
	    qid_t qid;

	    if ( arg == DEFAULT )
	      PL_put_atom(av+0, ATOM_default);
	    else
	      PL_put_integer(av+0, arg);

	    for(i=1; i<fdef->arity; i++)
	    { NEED_ARG;
	      PL_put_term(av+i, argv);
	      SHIFT;
	    }

	    tellString(&str, &bufsize);
	    qid = PL_open_query(proc->definition->module, PL_Q_NODEBUG,
				proc, av);
	    PL_next_solution(qid);
	    PL_close_query(qid);
	    toldString();
	    OUTSTRING(str, bufsize);
	    if ( str != buf )
	      free(str);

	    fmt++;
	  } else
	  { switch(*fmt)		/* Build in formatting */
	    { case 'a':			/* atomic */
		{ char *s;
		  unsigned int len;

		  NEED_ARG;
		  if ( !PL_get_nchars(argv, &len, &s, CVT_ATOMIC) )
		    FMT_ARG("a", argv);
		  SHIFT;
		  OUTSTRING(s, len);
		  fmt++;
		  break;
		}
	      case 'c':			/* ascii */
		{ int c;

		  NEED_ARG;
		  if ( PL_get_integer(argv, &c) && c>=0 && c<=255 )
		  { int times = (arg == DEFAULT ? 1 : arg);

		    SHIFT;
		    while(times-- > 0)
		    { OUTCHR(c);
		    }
		  } else
		    FMT_ARG("c", argv);
		  fmt++;
		  break;
		}
	      case 'e':			/* exponential float */
	      case 'E':			/* Exponential float */
	      case 'f':			/* float */
	      case 'g':			/* shortest of 'f' and 'e' */
	      case 'G':			/* shortest of 'f' and 'E' */
		{ double f;
		  char tmp[12];
		  char buf[256];

		  NEED_ARG;
		  if ( !PL_get_float(argv, &f) )
		  { char f[2];
		    
		    f[0] = fmt[0];
		    f[1] = EOS;
		    FMT_ARG(f, argv);
		  }
		  SHIFT;
		  Ssprintf(tmp, "%%.%d%c", arg == DEFAULT ? 6 : arg, *fmt);
		  Ssprintf(buf, tmp, f);
		  OUTSTRING0(buf);
		  fmt++;
		  break;
		}
	      case 'd':			/* integer */
	      case 'D':			/* grouped integer */
	      case 'r':			/* radix number */
	      case 'R':			/* Radix number */
		{ int i;
		  char tmp[50];

		  NEED_ARG;
		  if ( !PL_get_integer(argv, &i) )
		  { char f[2];
		    
		    f[0] = fmt[0];
		    f[1] = EOS;
		    FMT_ARG(f, argv);
		  }
		  SHIFT;
		  if ( arg == DEFAULT )
		    arg = 0;
		  if ( *fmt == 'd' || *fmt == 'D' )
		    formatInteger(*fmt == 'D', arg, 10, TRUE, i, tmp);
		  else
		    formatInteger(FALSE, 0, arg, *fmt == 'r', i, tmp);
		  OUTSTRING0(tmp);			
		  fmt++;
		  break;
		}
	      case 's':			/* string */
		{ char *s;
		  unsigned int len;

		  NEED_ARG;
		  if ( !PL_get_nchars(argv, &len, &s, CVT_LIST|CVT_STRING) )
		    FMT_ARG("s", argv);
		  OUTSTRING(s, len);
		  SHIFT;
		  fmt++;
		  break;
		}
	      case 'i':			/* ignore */
		{ NEED_ARG;
		  SHIFT;
		  fmt++;
		  break;
		}
		{ Func f;
		  char buf[BUFSIZE];
		  char *str;

	      case 'k':			/* write_canonical */
		  f = pl_write_canonical; 
	          goto pl_common;
	      case 'p':			/* print */
		  f = pl_print;
	          goto pl_common;
	      case 'q':			/* writeq */
		  f = pl_writeq;
	          goto pl_common;
	      case 'w':			/* write */
		  f = pl_write;
		  pl_common:

		  NEED_ARG;
		  if ( pending_rubber )
		  { int bufsize = BUFSIZE;

		    str = buf;
		    tellString(&str, &bufsize);
		    (*f)(argv);
		    toldString();
		    OUTSTRING(str, bufsize);
		    if ( str != buf )
		      free(str);
		  } else
		  { if ( fd->position && fd->position->linepos == column )
		    { IOSTREAM *old = Scurout;

		      Scurout = fd;
		      (*f)(argv);
		      Scurout = old;

		      column = fd->position->linepos;
		    } else
		    { int bufsize = BUFSIZE;

		      str = buf;
		      tellString(&str, &bufsize);
		      (*f)(argv);
		      toldString();
		      OUTSTRING(str, bufsize);
		      if ( str != buf )
			free(str);
		    }
		  }
		  SHIFT;
		  fmt++;
		  break;
		}
	      case 'W':			/* write_term(Value, Options) */
	       { char buf[BUFSIZE];
		 char *str;

		 if ( argc < 2 )
		 { FMT_ERROR("not enough arguments");
		 }
		 if ( pending_rubber )
		  { int bufsize = BUFSIZE;

		    str = buf;
		    tellString(&str, &bufsize);
		    pl_write_term(argv, argv+1);
		    toldString();
		    OUTSTRING(str, bufsize);
		    if ( str != buf )
		      free(str);
		  } else
		  { if ( fd->position && fd->position->linepos == column )
		    { IOSTREAM *old = Scurout;

		      Scurout = fd;
		      pl_write_term(argv, argv+1);
		      Scurout = old;

		      column = fd->position->linepos;
		    } else
		    { int bufsize = BUFSIZE;

		      str = buf;
		      tellString(&str, &bufsize);
		      pl_write_term(argv, argv+1);
		      toldString();
		      OUTSTRING(str, bufsize);
		      if ( str != buf )
			free(str);
		    }
		  }
		  SHIFT;
		  SHIFT;
		  fmt++;
		  break;
	       }
	      case '@':
	        { char buf[BUFSIZE];
		  char *str = buf;
		  int bufsize = BUFSIZE;
		  term_t ex = 0;
		  int rval;

		  if ( argc < 1 )
		  { FMT_ERROR("not enough arguments");
		  }
		  tellString(&str, &bufsize);
		  rval = callProlog(NULL, argv, PL_Q_CATCH_EXCEPTION, &ex);
		  toldString();
		  OUTSTRING(str, bufsize);
		  if ( str != buf )
		    free(str);

		  if ( !rval && ex )
		    return PL_raise_exception(ex);

		  SHIFT;
		  fmt++;
		  break;
	        }
	      case '~':			/* ~ */
		{ OUTCHR('~');
		  fmt++;
		  break;
		}
	      case 'n':			/* \n */
	      case 'N':			/* \n if not on newline */
		{ if ( arg == DEFAULT )
		    arg = 1;
		  if ( *fmt == 'N' && column == 0 )
		    arg--;
		  while( arg-- > 0 )
		    OUTCHR('\n');
		  fmt++;
		  break;
		}
	      case 't':			/* insert tab */
		{ rub[pending_rubber].where = index;
		  rub[pending_rubber].pad   = (arg == DEFAULT ? (Char) ' '
							      : (Char) arg);
		  pending_rubber++;
		  fmt++;
		  break;
		}
	      case '|':			/* set tab */
		{ int stop;

		  if ( arg == DEFAULT )
		    arg = column;
	      case '+':			/* tab relative */
		  if ( arg == DEFAULT )
		    arg = 8;
		  stop = (*fmt == '+' ? tab_stop + arg : arg);

		  if ( pending_rubber == 0 ) /* nothing to distribute */
		  { rub[0].where = index;
		    rub[0].pad = ' ';
		    pending_rubber++;
		  }
		  distribute_rubber(rub, pending_rubber, stop - column);
		  emit_rubber(fd, buffer, index, rub, pending_rubber);
		  index = 0;
		  pending_rubber = 0;

		  column = tab_stop = stop;
		  fmt++;
		  break;
		}
	      default:
	      { term_t ex = PL_new_term_ref();

		Sunlock(fd);
		PL_put_atom(ex, codeToAtom(*fmt));
		return PL_error("format", 2, NULL, ERR_EXISTENCE,
				PL_new_atom("format_character"),
				ex);
	      }
	    }
	  }
	  break;			/* the '~' switch */
	}
      default:
	{ OUTCHR(*fmt);
	  fmt++;
	  break;
	}
    }
  }

  if ( pending_rubber )			/* not closed ~t: flush out */
    emit_rubber(fd, buffer, index, rub, 0);

  Sunlock(fd);

  succeed;
}


static void
distribute_rubber(struct rubber *r, int rn, int space)
{ if ( space > 0 )
  { int s = space / rn;
    int n, m;

    for(n=0; n < rn; n++)		/* give them equal size */
      r[n].size = s;
					/* distribute from the center */
    space -= s*rn;
    for(m = rn / 2, n = 0; space; n++, space--)
    { r[m + (n % 2 ? n : -n)].size++;
    }
  } else
  { int n;

    for(n=0; n < rn; n++)		/* set all rubber to 0 */
      r[n].size = 0;
  }
}


static void
emit_rubber(IOSTREAM *fd, char *buf, int i, struct rubber *r, int rn)
{ int j;

  for(j = 0; j <= i; j++)
  { if ( r->where == j && rn )
    { int n;
      for(n=0; n<r->size; n++)
        Sputc(r->pad, fd);
      r++;
      rn--;
    }
    if ( j < i )
      Sputc(buf[j], fd);
  }
}
