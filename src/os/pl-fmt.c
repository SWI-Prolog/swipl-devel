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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Formatted output (Prolog predicates format/[1,2,3]).   One  day,  the  C
source should also use format() to produce error messages, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"
#include "pl-ctype.h"
#include "pl-utf8.h"
#include <ctype.h>

static char *	formatNumber(bool split, int div, int radix,
			     bool smll, Number n, Buffer out);
static char *	formatFloat(int how, int arg, Number f, Buffer out);

#define MAXRUBBER 100

struct rubber
{ size_t where;				/* where is rubber in output */
  size_t size;				/* how big should it be */
  pl_wchar_t pad;			/* padding character */
};

typedef struct
{ IOSTREAM *out;			/* our output stream */
  int column;				/* current column */
  tmp_buffer buffer;			/* bin for characters with tabs */
  size_t buffered;			/* characters in buffer */
  int pending_rubber;			/* number of not-filled ~t's */
  struct rubber rub[MAXRUBBER];
} format_state;

#define BUFSIZE		1024
#define DEFAULT		(-1)
#define SHIFT		{ argc--; argv++; }
#define NEED_ARG	{ if ( argc <= 0 ) \
			  { FMT_ERROR("not enough arguments"); \
			  } \
			}
#define FMT_ERROR(fmt)	return (void)Sunlock(fd), \
			  PL_error(NULL, 0, NULL, ERR_FORMAT, fmt)
#define FMT_ARG(c, a)	return (void)Sunlock(fd), \
			       PL_error(NULL, 0, NULL, \
					ERR_FORMAT_ARG, c, a)


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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Low-level output. If there is pending  rubber   the  output is stored in
UTF-8 format in the state's `buffer'.   The  `buffered' field represents
the number of UTF-8 characters in the buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
outchr(format_state *state, int chr)
{ if ( state->pending_rubber )
  { if ( chr > 0x7f )
    { char buf[8];
      char *s, *e;

      e = utf8_put_char(buf, chr);
      for(s=buf; s<e; s++)
	addBuffer((Buffer)&state->buffer, *s, char);
    } else
    { char c = chr;

      addBuffer((Buffer)&state->buffer, c, char);
    }

    state->buffered++;
  } else
  { if ( Sputcode(chr, state->out) < 0 )
      return FALSE;
  }

  state->column = update_column(state->column, chr);

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Emit ASCII 0-terminated strings resulting from sprintf() on numeric
arguments.  No fuzz with wide characters here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
outstring(format_state *state, const char *s, size_t len)
{ const char *q;
  const char *e = &s[len];

  if ( state->pending_rubber )
  { addMultipleBuffer(&state->buffer, s, len, char);
    state->buffered += len;
  } else
  { for(q=s; q < e; q++)
    { if ( Sputcode(*q&0xff, state->out) < 0 )
	return FALSE;
    }
  }

  for(q=s; q < e; q++)
    state->column = update_column(state->column, *q&0xff);

  return TRUE;
}


static int
outstring0(format_state *state, const char *s)
{ return outstring(state, s, strlen(s));
}


static int
oututf8(format_state *state, const char *s, size_t len)
{ const char *e = &s[len];

  while(s<e)
  { int chr;

    s = utf8_get_char(s, &chr);
    if ( !outchr(state, chr) )
      return FALSE;
  }

  return TRUE;
}


static int
outtext(format_state *state, PL_chars_t *txt)
{ switch(txt->encoding)
  { case ENC_ISO_LATIN_1:
      return outstring(state, txt->text.t, txt->length);
    case ENC_WCHAR:
    { const pl_wchar_t *s = txt->text.w;
      const pl_wchar_t *e = &s[txt->length];

      while(s<e)
      { if ( !outchr(state, *s++) )
	  return FALSE;
      }

      return TRUE;
    }
    default:
    { assert(0);
      return FALSE;
    }
  }
}


#define format_predicates (GD->format.predicates)

static int	update_column(int, Char);
static bool	do_format(IOSTREAM *fd, PL_chars_t *fmt,
			  int ac, term_t av, Module m);
static void	distribute_rubber(struct rubber *, int, int);
static int	emit_rubber(format_state *state);


		/********************************
		*       PROLOG CONNECTION	*
		********************************/

word
pl_format_predicate(term_t chr, term_t descr)
{ int c;
  predicate_t proc = NULL;
  Symbol s;
  int arity;

  if ( !PL_get_char_ex(chr, &c, FALSE) )
    fail;

  if ( !get_procedure(descr, &proc, 0, GP_CREATE) )
    fail;
  PL_predicate_info(proc, NULL, &arity, NULL);
  if ( arity == 0 )
    return PL_error(NULL, 0, "arity must be > 0", ERR_DOMAIN,
		    PL_new_atom("format_predicate"),
		    descr);

  if ( !format_predicates )
    format_predicates = newHTable(8);

  if ( (s = lookupHTable(format_predicates, (void *)(intptr_t)c)) )
    s->value = proc;
  else
    addHTable(format_predicates, (void *)(intptr_t)c, proc);

  succeed;
}


word
pl_current_format_predicate(term_t chr, term_t descr, control_t h)
{ GET_LD
  Symbol s = NULL;
  TableEnum e;
  fid_t fid;

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

  if ( !(fid = PL_open_foreign_frame()) )
  { freeTableEnum(e);
    return FALSE;
  }
  while( (s=advanceTableEnum(e)) )
  { if ( PL_unify_integer(chr, (intptr_t)s->name) &&
	 PL_unify_predicate(descr, (predicate_t)s->value, 0) )
    { PL_close_foreign_frame(fid);
      ForeignRedoPtr(e);
    }

    PL_rewind_foreign_frame(fid);
  }

  PL_close_foreign_frame(fid);
  freeTableEnum(e);
  fail;
}


static word
format_impl(IOSTREAM *out, term_t format, term_t Args, Module m)
{ GET_LD
  term_t argv;
  int argc = 0;
  term_t args = PL_copy_term_ref(Args);
  int rval;
  PL_chars_t fmt;

  if ( !PL_get_text(format, &fmt, CVT_ALL|BUF_RING) )
    return PL_error("format", 3, NULL, ERR_TYPE, ATOM_text, format);

  if ( (argc = (int)lengthList(args, FALSE)) >= 0 )
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

  startCritical;
  switch(fmt.storage)			/* format can do call-back! */
  { case PL_CHARS_RING:
    case PL_CHARS_STACK:
      PL_save_text(&fmt, BUF_MALLOC);
      break;
    default:
      break;
  }

  rval = do_format(out, &fmt, argc, argv, m);
  PL_free_text(&fmt);
  if ( !endCritical )
    return FALSE;

  return rval;
}


word
pl_format3(term_t out, term_t format, term_t args)
{ GET_LD
  redir_context ctx;
  word rc;
  Module m = NULL;
  term_t list = PL_new_term_ref();

  if ( !PL_strip_module(args, &m, list) )
    return FALSE;

  if ( (rc=setupOutputRedirect(out, &ctx, FALSE)) )
  { if ( (rc = format_impl(ctx.stream, format, list, m)) )
      rc = closeOutputRedirect(&ctx);
    else
      discardOutputRedirect(&ctx);
  }

  return rc;
}


word
pl_format(term_t fmt, term_t args)
{ return pl_format3(0, fmt, args);
}


static inline int
get_chr_from_text(const PL_chars_t *t, int index)
{ switch(t->encoding)
  { case ENC_ISO_LATIN_1:
      return t->text.t[index]&0xff;
    case ENC_WCHAR:
      return t->text.w[index];
    default:
      assert(0);
      return 0;				/* not reached */
  }
}


		/********************************
		*       ACTUAL FORMATTING	*
		********************************/

static bool
do_format(IOSTREAM *fd, PL_chars_t *fmt, int argc, term_t argv, Module m)
{ GET_LD
  format_state state;			/* complete state */
  int tab_stop = 0;			/* padded tab stop */
  Symbol s;
  unsigned int here = 0;
  int rc = TRUE;

  Slock(fd);				/* buffer locally */

  state.out = fd;
  state.pending_rubber = 0;
  initBuffer(&state.buffer);
  state.buffered = 0;

  if ( fd->position )
    state.column = fd->position->linepos;
  else
    state.column = 0;

  while(here < fmt->length)
  { int c = get_chr_from_text(fmt, here);

    switch(c)
    { case '~':
	{ int arg = DEFAULT;		/* Numeric argument */
					/* Get the numeric argument */
	  c = get_chr_from_text(fmt, ++here);

	  if ( isDigitW(c) )
	  { arg = c - '0';

	    here++;
	    while(here < fmt->length)
	    { c = get_chr_from_text(fmt, here);

	      if ( isDigitW(c) )
	      { int dw = c - '0';
		int arg2 = arg*10 + dw;

		if ( (arg2 - dw)/10 != arg )	/* see mul64() in pl-arith.c */
		{ FMT_ERROR("argument overflow");
		}
		arg = arg2;
		here++;
	      } else
		break;
	    }
	  } else if ( c == '*' )
	  { NEED_ARG;
	    if ( PL_get_integer(argv, &arg) )
	    { SHIFT;
	    } else
	      FMT_ERROR("no or negative integer for `*' argument");
	    c = get_chr_from_text(fmt, ++here);
	  } else if ( c == '`' && here < fmt->length )
	  { arg = get_chr_from_text(fmt, ++here);
	    c = get_chr_from_text(fmt, ++here);
	  }

					/* Check for user defined format */
	  if ( format_predicates &&
	       (s = lookupHTable(format_predicates, (void*)((intptr_t)c))) )
	  { predicate_t proc = (predicate_t) s->value;
	    int arity;
	    term_t av;
	    char buf[BUFSIZE];
	    char *str = buf;
	    size_t bufsize = BUFSIZE;
	    int i;

	    PL_predicate_info(proc, NULL, &arity, NULL);
	    av = PL_new_term_refs(arity);

	    if ( arg == DEFAULT )
	      PL_put_atom(av+0, ATOM_default);
	    else
	      PL_put_integer(av+0, arg);

	    for(i=1; i < arity; i++)
	    { NEED_ARG;
	      PL_put_term(av+i, argv);
	      SHIFT;
	    }

	    tellString(&str, &bufsize, ENC_UTF8);
	    rc = PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, proc, av);
	    toldString();
	    if ( !rc )
	    { if ( str != buf )
		free(str);
	      goto out;
	    }
	    oututf8(&state, str, bufsize);
	    if ( str != buf )
	      free(str);

	    here++;
	  } else
	  { switch(c)			/* Build in formatting */
	    { case 'a':			/* atomic */
		{ PL_chars_t txt;

		  NEED_ARG;
		  if ( !PL_get_text(argv, &txt, CVT_ATOMIC) )
		    FMT_ARG("a", argv);
		  SHIFT;
		  rc = outtext(&state, &txt);
                  if ( !rc )
		    goto out;
		  here++;
		  break;
		}
	      case 'c':			/* ~c: character code */
		{ int chr;

		  NEED_ARG;
		  if ( PL_get_integer(argv, &chr) && chr >= 0 )
		  { int times = (arg == DEFAULT ? 1 : arg);

		    SHIFT;
		    while(times-- > 0)
		    { rc = outchr(&state, chr);
		      if ( !rc )
		        goto out;
		    }
		  } else
		    FMT_ARG("c", argv);
		  here++;
		  break;
		}
	      case 'e':			/* exponential float */
	      case 'E':			/* Exponential float */
	      case 'f':			/* float */
	      case 'g':			/* shortest of 'f' and 'e' */
	      case 'G':			/* shortest of 'f' and 'E' */
		{ number n;
		  union {
		  tmp_buffer b;
		    buffer b1;
		  } u;

		  NEED_ARG;
		  if ( !valueExpression(argv, &n PASS_LD) )
		  { char f[2];

		    f[0] = c;
		    f[1] = EOS;
		    FMT_ARG(f, argv);
		  }
		  SHIFT;

		  initBuffer(&u.b);
		  formatFloat(c, arg, &n, &u.b1);
		  clearNumber(&n);
		  rc = outstring0(&state, baseBuffer(&u.b, char));
		  discardBuffer(&u.b);
                  if ( !rc )
		    goto out;
		  here++;
		  break;
		}
	      case 'd':			/* integer */
	      case 'D':			/* grouped integer */
	      case 'r':			/* radix number */
	      case 'R':			/* Radix number */
		{ number i;
		  tmp_buffer b;

		  NEED_ARG;
		  if ( !valueExpression(argv, &i PASS_LD) ||
		       !toIntegerNumber(&i, 0) )
		  { char f[2];

		    f[0] = c;
		    f[1] = EOS;
		    FMT_ARG(f, argv);
		  }
		  SHIFT;
		  if ( arg == DEFAULT )
		    arg = 0;
		  initBuffer(&b);
		  if ( c == 'd' || c == 'D' )
		  { formatNumber(c == 'D', arg, 10, TRUE, &i, (Buffer)&b);
		  } else
		  { if ( arg < 1 || arg > 36 )
		    { term_t r = PL_new_term_ref();

		      PL_put_integer(r, arg);
		      Sunlock(fd);
		      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
				      ATOM_radix, r);
		    }
		    formatNumber(FALSE, 0, arg, c == 'r', &i, (Buffer)&b);
		  }
		  clearNumber(&i);
		  rc = outstring0(&state, baseBuffer(&b, char));
		  discardBuffer(&b);
		  if ( !rc )
		    goto out;
		  here++;
		  break;
		}
	      case 's':			/* string */
		{ PL_chars_t txt;

		  NEED_ARG;
		  if ( !PL_get_text(argv, &txt, CVT_LIST|CVT_STRING) &&
		       !PL_get_text(argv, &txt, CVT_ATOM) ) /* SICStus compat */
		    FMT_ARG("s", argv);
		  rc = outtext(&state, &txt);
		  SHIFT;
		  if ( !rc )
		    goto out;
		  here++;
		  break;
		}
	      case 'i':			/* ignore */
		{ NEED_ARG;
		  SHIFT;
		  here++;
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
		  if ( state.pending_rubber )
		  { size_t bufsize = BUFSIZE;

		    str = buf;
		    tellString(&str, &bufsize, ENC_UTF8);
		    rc = (*f)(argv);
		    toldString();
		    if ( !rc )
		      goto out;
		    oututf8(&state, str, bufsize);
		    if ( str != buf )
		      free(str);
		  } else
		  { if ( fd->position &&
			 fd->position->linepos == state.column )
		    { IOSTREAM *old = Scurout;

		      Scurout = fd;
		      rc = (int)(*f)(argv);
		      Scurout = old;
		      if ( !rc )
			goto out;

		      state.column = fd->position->linepos;
		    } else
		    { size_t bufsize = BUFSIZE;

		      str = buf;
		      tellString(&str, &bufsize, ENC_UTF8);
		      rc = (*f)(argv);
		      toldString();
		      if ( !rc )
		        goto out;
		      oututf8(&state, str, bufsize);
		      if ( str != buf )
			free(str);
		    }
		  }
		  SHIFT;
		  here++;
		  break;
		}
	      case 'W':			/* write_term(Value, Options) */
	       { char buf[BUFSIZE];
		 char *str;

		 if ( argc < 2 )
		 { FMT_ERROR("not enough arguments");
		 }
		 if ( state.pending_rubber )
		  { size_t bufsize = BUFSIZE;

		    str = buf;
		    tellString(&str, &bufsize, ENC_UTF8);
		    rc = (int)pl_write_term(argv, argv+1);
		    toldString();
		    if ( !rc )
		      goto out;
		    oututf8(&state, str, bufsize);
		    if ( str != buf )
		      free(str);
		  } else
		  { if ( fd->position &&
			 fd->position->linepos == state.column )
		    { IOSTREAM *old = Scurout;

		      Scurout = fd;
		      rc = (int)pl_write_term(argv, argv+1);
		      Scurout = old;
		      if ( !rc )
			goto out;

		      state.column = fd->position->linepos;
		    } else
		    { size_t bufsize = BUFSIZE;

		      str = buf;
		      tellString(&str, &bufsize, ENC_UTF8);
		      rc = (int)pl_write_term(argv, argv+1);
		      if ( !rc )
			goto out;
		      toldString();
		      oututf8(&state, str, bufsize);
		      if ( str != buf )
			free(str);
		    }
		  }
		  SHIFT;
		  SHIFT;
		  here++;
		  break;
	       }
	      case '@':
	        { char buf[BUFSIZE];
		  char *str = buf;
		  size_t bufsize = BUFSIZE;
		  term_t ex = 0;
		  int rval;

		  if ( argc < 1 )
		  { FMT_ERROR("not enough arguments");
		  }
		  tellString(&str, &bufsize, ENC_UTF8);
		  rval = callProlog(m, argv, PL_Q_CATCH_EXCEPTION, &ex);
		  toldString();
		  oututf8(&state, str, bufsize);
		  if ( str != buf )
		    free(str);

		  if ( !rval )
		  { Sunlock(fd);

		    if ( ex )
		      return PL_raise_exception(ex);
		    else
		      fail;
		  }

		  SHIFT;
		  here++;
		  break;
	        }
	      case '~':			/* ~ */
		{ rc = outchr(&state, '~');
		  if ( !rc )
		    goto out;
		  here++;
		  break;
		}
	      case 'n':			/* \n */
	      case 'N':			/* \n if not on newline */
		{ if ( arg == DEFAULT )
		    arg = 1;
		  if ( c == 'N' && state.column == 0 )
		    arg--;
		  while( arg-- > 0 )
                  { rc = outchr(&state, '\n');
		    if ( !rc )
		      goto out;
                  }
		  here++;
		  break;
		}
	      case 't':			/* insert tab */
		{ if ( state.pending_rubber >= MAXRUBBER )
		    FMT_ERROR("Too many tab stops");

		  state.rub[state.pending_rubber].where = state.buffered;
		  state.rub[state.pending_rubber].pad   =
					(arg == DEFAULT ? (pl_wchar_t)' '
							: (pl_wchar_t)arg);
		  state.rub[state.pending_rubber].size = 0;
		  state.pending_rubber++;
		  here++;
		  break;
		}
	      case '|':			/* set tab */
		{ int stop;

		  if ( arg == DEFAULT )
		    arg = state.column;
	      case '+':			/* tab relative */
		  if ( arg == DEFAULT )
		    arg = 8;
		  stop = (c == '+' ? tab_stop + arg : arg);

		  if ( state.pending_rubber == 0 ) /* nothing to distribute */
		  { state.rub[0].where = state.buffered;
		    state.rub[0].pad = ' ';
		    state.pending_rubber++;
		  }
		  distribute_rubber(state.rub,
				    state.pending_rubber,
				    stop - state.column);
		  emit_rubber(&state);

		  state.column = tab_stop = stop;
		  here++;
		  break;
		}
	      default:
	      { term_t ex = PL_new_term_ref();

		Sunlock(fd);
		PL_put_atom(ex, codeToAtom(c));
		return PL_error("format", 2, NULL, ERR_EXISTENCE,
				PL_new_atom("format_character"),
				ex);
	      }
	    }
	  }
	  break;			/* the '~' switch */
	}
      default:
	{ rc = outchr(&state, c);
	  if ( !rc )
	    goto out;
	  here++;
	  break;
	}
    }
  }

  if ( state.pending_rubber )		/* not closed ~t: flush out */
    emit_rubber(&state);

out:
  Sunlock(fd);

  return rc;
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


static int
emit_rubber(format_state *state)
{ const char *s = baseBuffer(&state->buffer, char);
  const char *e = &s[entriesBuffer(&state->buffer, char)];
  struct rubber *r = state->rub;
  int rn = state->pending_rubber;
  size_t j;

  for(j = 0; s <= e; j++)
  { int chr;

    if ( rn && r->where == j )
    { size_t n;

      for(n=0; n<r->size; n++)
      { if ( Sputcode(r->pad, state->out) < 0 )
	  return FALSE;
      }
      r++;
      rn--;
    }

    if ( s < e )
    { s = utf8_get_char(s, &chr);
      if ( Sputcode(chr, state->out) < 0 )
	return FALSE;
    } else
      break;
  }

  discardBuffer(&state->buffer);
  initBuffer(&state->buffer);
  state->buffered = 0;
  state->pending_rubber = 0;

  return TRUE;
}


/*  format an integer according to  a  number  of  modifiers  at various
    radius.   `split'  is a boolean asking to put ',' between each group
    of three digits (e.g. 67,567,288).  `div' askes to divide the number
    by radix^`div' before printing.   `radix'  is  the  radix  used  for
    conversion.  `n' is the number to be converted.

 ** Fri Aug 19 22:26:41 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static char *
formatNumber(bool split, int div, int radix, bool smll, Number i,
	     Buffer out)
{ switch(i->type)
  { case V_INTEGER:
    { int64_t n = i->value.i;
      char buf[100];
      char *tmp, *end, *s;
      int before = (div == 0);
      int digits = 0;
      bool negative = FALSE;

      if ( div+3 > (int)sizeof(buf) )	/* 0.000NNNN with div digits after 0. */
      { tmp = PL_malloc(div+3);
	end = tmp+div+3;
      } else
      { tmp = buf;
	end = tmp+sizeof(buf);
      }

      s = end;				/* i.e. start at the end */
      *--s = EOS;
      if ( n < 0 )
      { n = -n;
	negative = TRUE;
      }
      if ( n == 0 && div == 0 )
      { *--s = '0';
      } else
      { while( n > 0 || div >= 0 )
	{ if ( div-- == 0 && !before )
	  { *--s = '.';
	    before = 1;
	  }
	  if ( split && before && (digits++ % 3) == 0 && digits != 1 )
	    *--s = ',';
	  *--s = digitName((int)(n % radix), smll);
	  n /= radix;
	}
	if ( negative )
	  *--s = '-';
      }

      addMultipleBuffer(out, s, end-s, char);
      if ( tmp != buf )
	PL_free(tmp);

      return baseBuffer(out, char);
    }
#ifdef O_GMP
    case V_MPZ:
    { size_t len = mpz_sizeinbase(i->value.mpz, radix);
      char tmp[256];
      char *buf;

      if ( len+2 > sizeof(tmp) )
	buf = PL_malloc(len+2);
      else
	buf = tmp;

      mpz_get_str(buf, radix, i->value.mpz);
      if ( !smll && radix > 10 )
      { char *s;

	for(s=buf; *s; s++)
	  *s = toupper(*s);
      }
      if ( split || div > 0 )
      { int before = (int)(len-div);
	int leading;
	char *s = buf;

	if ( *s == '-' )
	{ addBuffer(out, *s, char);
	  s++;
	}
	if ( split )
	{ leading = before % 3;
	  if ( leading == 0 )
	    leading = 3;
	} else
	{ leading = (int)len;
	}
	for(; *s; s++)
	{ if ( before-- == 0 && div > 0 )
	  { addBuffer(out, '.', char);
	  } else if ( leading-- == 0 && before > 0 )
	  { addBuffer(out, ',', char);
	    leading = 2;
	  }
	  addBuffer(out, *s, char);
	}
	addBuffer(out, EOS, char);
      } else
      { addMultipleBuffer(out, buf, strlen(buf), char);
	addBuffer(out, EOS, char);
      }

      if ( buf != tmp )
	PL_free(buf);

      return baseBuffer(out, char);
    }
#endif /*O_GMP*/
    default:
      assert(0);
      return NULL;
  }
}


static char *
formatFloat(int how, int arg, Number f, Buffer out)
{ if ( arg == DEFAULT )
    arg = 6;

  switch(f->type)
  {
#ifdef O_GMP
    mpf_t mpf;
    case V_MPZ:
      mpf_init2(mpf, arg*4);
      mpf_set_z(mpf, f->value.mpz);
      goto print;
    case V_MPQ:
    { char tmp[12];
      int size;
      int written;
      int fbits;

      switch(how)
      { case 'f':
	case 'g':
	case 'G':
	{ mpz_t iv;

	  mpz_init(iv);
	  mpz_set_q(iv, f->value.mpq);
	  fbits = (int)mpz_sizeinbase(iv, 2) + 4*arg;
	  mpz_clear(iv);
	  break;
	}
	default:
	  fbits = 4*arg;
      }

      mpf_init2(mpf, fbits);
      mpf_set_q(mpf, f->value.mpq);

    print:
      Ssprintf(tmp, "%%.%dF%c", arg, how);
      size = 0;
      written = arg+4;
      while(written >= size)
      { size = written+1;

	if ( !growBuffer(out, size) )	/* reserve for -.e<null> */
	  outOfCore();
	written = gmp_snprintf(baseBuffer(out, char), size, tmp, mpf);
      }
      mpf_clear(mpf);
      out->top = out->base + written;

      return baseBuffer(out, char);
    }
#endif
    case V_INTEGER:
      promoteToFloatNumber(f);
      /*FALLTHROUGH*/
    case V_FLOAT:
    { char tmp[12];
      int written = arg+20;
      int size = 0;

      Ssprintf(tmp, "%%.%d%c", arg, how);
      while(written >= size)
      { size = written+1;

	if ( !growBuffer(out, size) )
	  outOfCore();
	written = snprintf(baseBuffer(out, char), size, tmp, f->value.f);
      }
      out->top = out->base + written;

      return baseBuffer(out, char);
    }
  }

  return NULL;
}
