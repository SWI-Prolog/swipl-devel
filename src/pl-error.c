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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
throw(error(<Formal>, <SWI-Prolog>))

<SWI-Prolog>	::= context(Name/Arity, Message)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"
/* BeOS has EACCES defined elsewhere, but errno is here */
#if !defined(EACCES) || defined(__BEOS__)
#include <errno.h>
#endif

static void
put_name_arity(term_t t, functor_t f)
{ FunctorDef fdef = valueFunctor(f);
  term_t a = PL_new_term_refs(2);

  PL_put_atom(a+0, fdef->name);
  PL_put_integer(a+1, fdef->arity);
  PL_cons_functor(t, FUNCTOR_divide2, a+0, a+1);
}


int
PL_error(const char *pred, int arity, const char *msg, int id, ...)
{ term_t except, formal, swi;
  va_list args;
  int do_throw = FALSE;
  fid_t fid;
  int rc;

  if ( id == ERR_FILE_OPERATION && !fileerrors )
    fail;

  if ( msg == MSG_ERRNO )
  { if ( errno == EPLEXCEPTION )
      return FALSE;
    msg = OsError();
  }

  fid    = PL_open_foreign_frame();
  except = PL_new_term_ref();
  formal = PL_new_term_ref();
  swi    = PL_new_term_ref();

					/* build (ISO) formal part  */
  va_start(args, id);
  switch(id)
  { case ERR_INSTANTIATION:
      err_instantiation:
      PL_unify_atom(formal, ATOM_instantiation_error);
      break;
    case ERR_TYPE:			/* ERR_INSTANTIATION if var(actual) */
    { atom_t expected = va_arg(args, atom_t);
      term_t actual   = va_arg(args, term_t);

      if ( PL_is_variable(actual) && expected != ATOM_variable )
	goto err_instantiation;

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_type_error2,
		      PL_ATOM, expected,
		      PL_TERM, actual);
      break;
    }
    case ERR_AR_TYPE:			/* arithmetic type error */
    { atom_t expected = va_arg(args, atom_t);
      Number num      = va_arg(args, Number);
      term_t actual   = PL_new_term_ref();

      _PL_put_number(actual, num);
      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_type_error2,
		      PL_ATOM, expected,
		      PL_TERM, actual);
      break;
    }
    case ERR_AR_UNDEF:
    { PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_evaluation_error1,
		      PL_ATOM, ATOM_undefined);
      break;
    }
    case ERR_AR_OVERFLOW:
    { PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_evaluation_error1,
		      PL_ATOM, ATOM_float_overflow);
      break;
    }
    case ERR_AR_UNDERFLOW:
    { PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_evaluation_error1,
		      PL_ATOM, ATOM_float_underflow);
      break;
    }
    case ERR_DOMAIN:			/*  ERR_INSTANTIATION if var(arg) */
    { atom_t domain = va_arg(args, atom_t);
      term_t arg    = va_arg(args, term_t);

      if ( PL_is_variable(arg) )
	goto err_instantiation;

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_domain_error2,
		      PL_ATOM, domain,
		      PL_TERM, arg);
      break;
    }
    case ERR_REPRESENTATION:
    { atom_t what = va_arg(args, atom_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_representation_error1,
		      PL_ATOM, what);
      break;
    }
    case ERR_MODIFY_STATIC_PROC:
    { Procedure proc = va_arg(args, Procedure);
      term_t pred = PL_new_term_ref();

      unify_definition(pred, proc->definition, 0, GP_NAMEARITY|GP_HIDESYSTEM);
      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_permission_error3,
		      PL_ATOM, ATOM_modify,
		      PL_ATOM, ATOM_static_procedure,
		      PL_TERM, pred);
      break;
    }
    case ERR_UNDEFINED_PROC:
    { Definition def = va_arg(args, Definition);
      term_t pred = PL_new_term_ref();

      unify_definition(pred, def, 0, GP_NAMEARITY);
      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_existence_error2,
		      PL_ATOM, ATOM_procedure,
		      PL_TERM, pred);
      break;
    }
    case ERR_PERMISSION_PROC:
    { atom_t op = va_arg(args, atom_t);
      atom_t type = va_arg(args, atom_t);
      Definition def = va_arg(args, Definition);
      term_t pred = PL_new_term_ref();

      unify_definition(pred, def, 0, GP_NAMEARITY|GP_HIDESYSTEM);
      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_permission_error3,
		    PL_ATOM, op,
		    PL_ATOM, type,
		    PL_TERM, pred);
      break;
    }
    case ERR_NOT_IMPLEMENTED_PROC:
    { const char *name = va_arg(args, const char *);
      int arity = va_arg(args, int);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_not_implemented2,
		      PL_ATOM, ATOM_procedure,
		      PL_FUNCTOR, FUNCTOR_divide2,
		        PL_CHARS, name,
		        PL_INTEGER, arity);
      break;
    }
    case ERR_FAILED:
    { term_t goal = va_arg(args, term_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_failure_error1,
		      PL_TERM, goal);

      break;
    }
    case ERR_EVALUATION:
    { atom_t what = va_arg(args, atom_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_evaluation_error1,
		      PL_ATOM, what);
      break;
    }
    case ERR_NOT_EVALUABLE:
    { functor_t f = va_arg(args, functor_t);
      term_t actual = PL_new_term_ref();

      put_name_arity(actual, f);
      
      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_type_error2,
		      PL_ATOM, ATOM_evaluable,
		      PL_TERM, actual);
      break;
    }
    case ERR_DIV_BY_ZERO:
    { PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_evaluation_error1,
		      PL_ATOM, ATOM_zero_divisor);
      break;
    }
    case ERR_PERMISSION:
    { atom_t type = va_arg(args, atom_t);
      atom_t op   = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_permission_error3,
			  PL_ATOM, type,
			  PL_ATOM, op,
			  PL_TERM, obj);

      break;
    }
    case ERR_TIMEOUT:
    { atom_t op   = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_timeout_error2,
			  PL_ATOM, op,
			  PL_TERM, obj);

      break;
    }
    case ERR_EXISTENCE:
    { atom_t type = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_existence_error2,
			  PL_ATOM, type,
			  PL_TERM, obj);

      break;
    }
    case ERR_FILE_OPERATION:
    { atom_t action = va_arg(args, atom_t);
      atom_t type   = va_arg(args, atom_t);
      term_t file   = va_arg(args, term_t);

      switch(errno)
      { case EACCES:
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_permission_error3,
			  PL_ATOM, action,
			  PL_ATOM, type,
			  PL_TERM, file);
	  break;
	case EMFILE:
	case ENFILE:
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_representation_error1,
			  PL_ATOM, ATOM_max_files);
	  break;
#ifdef EPIPE
	case EPIPE:
	  if ( !msg )
	    msg = "Broken pipe";
	  /*FALLTHROUGH*/
#endif
	default:			/* what about the other cases? */
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_existence_error2,
			  PL_ATOM, type,
			  PL_TERM, file);
	  break;
      }

      break;
    }
    case ERR_STREAM_OP:
    { atom_t action = va_arg(args, atom_t);
      term_t stream = va_arg(args, term_t);
      
      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_io_error2,
		      PL_ATOM, action,
		      PL_TERM, stream);
      break;
    }
    case ERR_DDE_OP:
    { const char *op  = va_arg(args, const char *);
      const char *err = va_arg(args, const char *);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_dde_error2,
		      PL_CHARS, op,
		      PL_CHARS, err);
      break;
    }
    case ERR_SHARED_OBJECT_OP:
    { atom_t action = va_arg(args, atom_t);
      const char *err = va_arg(args, const char *);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_shared_object2,
		      PL_ATOM,  action,
		      PL_CHARS, err);
      break;
    }
    case ERR_NOT_IMPLEMENTED_FEATURE:		/* non-ISO */
    { const char *what = va_arg(args, const char *);

      PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_not_implemented2,
		          PL_ATOM, ATOM_feature,
			  PL_CHARS, what);
      break;
    }
    case ERR_RESOURCE:
    { atom_t what = va_arg(args, atom_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_resource_error1,
		      PL_ATOM, what);
      break;
    }
    case ERR_SYNTAX:
    { const char *what = va_arg(args, const char *);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_syntax_error1,
		      PL_CHARS, what);
      break;
    }
    case ERR_NOMEM:
    { PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_resource_error1,
		      PL_ATOM, ATOM_no_memory);

      break;
    }
    case ERR_SYSCALL:
    { const char *op = va_arg(args, const char *);

      if ( !msg )
	msg = op;

      switch(errno)
      { case ENOMEM:
	  PL_unify_term(formal,
			PL_FUNCTOR, FUNCTOR_resource_error1,
			  PL_ATOM, ATOM_no_memory);
	  break;
	default:
	  PL_unify_atom(formal, ATOM_system_error);
	  break;
      }

      break;
    }
    case ERR_SHELL_FAILED:
    { term_t cmd = va_arg(args, term_t);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_shell2,
		      PL_ATOM, ATOM_execute,
		      PL_TERM, cmd);
      break;
    }
    case ERR_SHELL_SIGNALLED:
    { term_t cmd = va_arg(args, term_t);
      int sig = va_arg(args, int);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_shell2,
		      PL_FUNCTOR, FUNCTOR_signal1,
		        PL_INT, sig,
		      PL_TERM, cmd);
      break;
    }
    case ERR_SIGNALLED:
    { int   sig     = va_arg(args, int);
      char *signame = va_arg(args, char *);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_signal2,
			PL_CHARS,   signame,
		        PL_INT, sig);
      break;
    }
    case ERR_CLOSED_STREAM:
    { IOSTREAM *s = va_arg(args, IOSTREAM *);

      PL_unify_term(formal,
		    PL_FUNCTOR, FUNCTOR_existence_error2,
		    PL_ATOM, ATOM_stream,
		    PL_POINTER, s);
      do_throw = TRUE;
      break;
    }
    case ERR_BUSY:
    { atom_t type  = va_arg(args, atom_t);
      term_t mutex = va_arg(args, term_t);

      PL_unify_term(formal, PL_FUNCTOR, FUNCTOR_busy2, type, mutex);
      break;
    }
    default:
      assert(0);
  }
  va_end(args);

					/* build SWI-Prolog context term */
  if ( pred || msg )
  { term_t predterm = PL_new_term_ref();
    term_t msgterm  = PL_new_term_ref();

    if ( pred )
    { PL_unify_term(predterm,
		    PL_FUNCTOR, FUNCTOR_divide2,
		      PL_CHARS, pred,
		      PL_INT, arity);
    }
    if ( msg )
    { PL_put_atom_chars(msgterm, msg);
    }

    PL_unify_term(swi,
		  PL_FUNCTOR, FUNCTOR_context2,
		    PL_TERM, predterm,
		    PL_TERM, msgterm);
  }

  PL_unify_term(except,
		PL_FUNCTOR, FUNCTOR_error2,
		  PL_TERM, formal,
		  PL_TERM, swi);


  if ( do_throw )
    rc = PL_throw(except);
  else
    rc = PL_raise_exception(except);

  PL_close_foreign_frame(fid);

  return rc;
}


char *
tostr(char *buf, const char *fmt, ...)
{ va_list args;

  va_start(args, fmt);
  Svsprintf(buf, fmt, args);
  va_end(args);
  
  return buf;
}


		 /*******************************
		 *	PRINTING MESSAGES	*
		 *******************************/

void
printMessage(atom_t severity, ...)
{ fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(2);
  predicate_t pred = PROCEDURE_print_message2;
  va_list args;

  gc_status.blocked++;			/* sometimes called from dangerous */
					/* places */
  va_start(args, severity);
  PL_put_atom(av+0, severity);
  PL_unify_termv(av+1, args);
  va_end(args);

  if ( isDefinedProcedure(pred) )
    PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION, pred, av);
  else
  { Sfprintf(Serror, "Message: ");
    PL_write_term(Serror, av+1, 1200, 0);
    Sfprintf(Serror, "\n");
  }
  gc_status.blocked--;

  PL_discard_foreign_frame(fid);
}


		 /*******************************
		 *    ERROR-CHECKING *_get()	*
		 *******************************/

int
PL_get_nchars_ex(term_t t, unsigned int *len, char **s, unsigned int flags)
{ atom_t expected;

  if ( PL_get_nchars(t, len, s, flags) )
    return TRUE;

  if ( flags & CVT_LIST )
    expected = ATOM_text;
  else if ( flags & CVT_NUMBER )
    expected = ATOM_atomic;
  else
    expected = ATOM_atom;

  return PL_error(NULL, 0, NULL, ERR_TYPE, expected, t);
}


int
PL_get_chars_ex(term_t t, char **s, unsigned int flags)
{ return PL_get_nchars_ex(t, NULL, s, flags);
}


int
PL_get_atom_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, t);
}


int
PL_get_integer_ex(term_t t, int *i)
{ if ( PL_get_integer(t, i) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_long_ex(term_t t, long *i)
{ if ( PL_get_long(t, i) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_bool_ex(term_t t, int *i)
{ if ( PL_get_bool(t, i) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}


int
PL_get_float_ex(term_t t, double *f)
{ if ( PL_get_float(t, f) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_float, t);
}


int
PL_unify_list_ex(term_t l, term_t h, term_t t)
{ if ( PL_unify_list(l, h, t) )
    succeed;

  if ( PL_get_nil(l) )
    fail;
  
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


int
PL_unify_nil_ex(term_t l)
{ if ( PL_unify_nil(l) )
    succeed;

  if ( PL_is_list(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


int
PL_get_list_ex(term_t l, term_t h, term_t t)
{ if ( PL_get_list(l, h, t) )
    succeed;

  if ( PL_get_nil(l) )
    fail;
  
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}

int
PL_get_nil_ex(term_t l)
{ if ( PL_get_nil(l) )
    succeed;

  if ( PL_is_list(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}

int
PL_unify_bool_ex(term_t t, bool val)
{ bool v;

  if ( PL_is_variable(t) )
    return PL_unify_atom(t, val ? ATOM_true : ATOM_false);
  if ( PL_get_bool(t, &v) )
  { if ( (!val && !v) || (val && v) )
      succeed;
    fail;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}


int
PL_get_arg_ex(int n, term_t term, term_t arg)
{ if ( PL_get_arg(n, term, arg) )
    succeed;
  else
  { term_t a = PL_new_term_ref();

    PL_put_integer(a, n);

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_natural, a);
  }
}
