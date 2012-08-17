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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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

static int
put_name_arity(term_t t, functor_t f)
{ GET_LD
  FunctorDef fdef = valueFunctor(f);
  term_t a;

  if ( (a=PL_new_term_refs(2)) )
  { PL_put_atom(a+0, fdef->name);

    return (PL_put_integer(a+1, fdef->arity) &&
	    PL_cons_functor(t, FUNCTOR_divide2, a+0, a+1));
  }

  return FALSE;
}


static void
rewrite_callable(atom_t *expected, term_t actual)
{ GET_LD
  term_t a = 0;
  int loops = 0;

  while ( PL_is_functor(actual, FUNCTOR_colon2) )
  { if ( !a )
     a = PL_new_term_ref();

    _PL_get_arg(1, actual, a);
    if ( !PL_is_atom(a) )
    { *expected = ATOM_atom;
      PL_put_term(actual, a);
      return;
    } else
    { _PL_get_arg(2, actual, a);
      PL_put_term(actual, a);
    }

    if ( ++loops > 100 && !PL_is_acyclic(actual) )
      break;
  }
}


int
PL_error(const char *pred, int arity, const char *msg, PL_error_code id, ...)
{ GET_LD
  char msgbuf[50];
  Definition caller;
  term_t except, formal, swi, msgterm=0;
  va_list args;
  int do_throw = FALSE;
  fid_t fid;
  int rc;

  if ( exception_term )			/* do not overrule older exception */
    return FALSE;

  if ( environment_frame )
    caller = environment_frame->predicate;
  else
    caller = NULL;

  if ( id == ERR_FILE_OPERATION &&
       !truePrologFlag(PLFLAG_FILEERRORS) )
    fail;

  if ( msg == MSG_ERRNO )
  { if ( errno == EPLEXCEPTION )
      return FALSE;
    msg = OsError();
  }

  LD->exception.processing = TRUE;	/* allow using spare stack */

  if ( !(fid = PL_open_foreign_frame()) )
    goto nomem;

  except = PL_new_term_ref();
  formal = PL_new_term_ref();
  swi    = PL_new_term_ref();

					/* build (ISO) formal part  */
  va_start(args, id);
  switch(id)
  { case ERR_INSTANTIATION:
      err_instantiation:
      rc = PL_unify_atom(formal, ATOM_instantiation_error);
      break;
    case ERR_UNINSTANTIATION:
    { int argn = va_arg(args, int);
      term_t bound = va_arg(args, term_t);

      if ( !msg && argn > 0 )
      { Ssprintf(msgbuf, "%d-%s argument",
		 argn, argn == 1 ? "st" : argn == 2 ? "nd" : "th");
	msg = msgbuf;
      }

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_uninstantiation_error1,
			   PL_TERM, bound);
      break;
    }
    case ERR_TYPE:			/* ERR_INSTANTIATION if var(actual) */
    { atom_t expected = va_arg(args, atom_t);
      term_t actual   = va_arg(args, term_t);

    case_type_error:
      if ( expected == ATOM_callable )
	rewrite_callable(&expected, actual);
      if ( PL_is_variable(actual) && expected != ATOM_variable )
	goto err_instantiation;

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_type_error2,
			   PL_ATOM, expected,
			   PL_TERM, actual);
      break;
    case ERR_PTR_TYPE:			/* atom_t, Word */
      { Word ptr;

	expected = va_arg(args, atom_t);
	ptr      = va_arg(args, Word);
	actual   = PL_new_term_ref();

	*valTermRef(actual) = *ptr;
	goto case_type_error;
      }
    }
    case ERR_CHARS_TYPE:		/* ERR_INSTANTIATION if var(actual) */
    { const char *expected = va_arg(args, const char*);
      term_t actual        = va_arg(args, term_t);

      if ( PL_is_variable(actual) && !streq(expected, "variable") )
	goto err_instantiation;

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_type_error2,
			   PL_CHARS, expected,
			   PL_TERM, actual);
      break;
    }
    case ERR_AR_TYPE:			/* arithmetic type error */
    { atom_t expected = va_arg(args, atom_t);
      Number num      = va_arg(args, Number);
      term_t actual   = PL_new_term_ref();

      rc = (_PL_put_number(actual, num) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_type_error2,
			    PL_ATOM, expected,
			    PL_TERM, actual));
      break;
    }
    case ERR_AR_DOMAIN:
    { atom_t domain = va_arg(args, atom_t);
      Number num    = va_arg(args, Number);
      term_t actual = PL_new_term_ref();

      rc = (_PL_put_number(actual, num) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_domain_error2,
			    PL_ATOM, domain,
			    PL_TERM, actual));
      break;
    }
    case ERR_AR_UNDEF:
    { rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_evaluation_error1,
			   PL_ATOM, ATOM_undefined);
      break;
    }
    case ERR_AR_OVERFLOW:
    { rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_evaluation_error1,
			   PL_ATOM, ATOM_float_overflow);
      break;
    }
    case ERR_AR_UNDERFLOW:
    { rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_evaluation_error1,
			   PL_ATOM, ATOM_float_underflow);
      break;
    }
    case ERR_DOMAIN:			/*  ERR_INSTANTIATION if var(arg) */
    { atom_t domain = va_arg(args, atom_t);
      term_t arg    = va_arg(args, term_t);

      if ( PL_is_variable(arg) )
	goto err_instantiation;

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_domain_error2,
			   PL_ATOM, domain,
			   PL_TERM, arg);
      break;
    }
    case ERR_REPRESENTATION:
    { atom_t what = va_arg(args, atom_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_representation_error1,
			   PL_ATOM, what);
      break;
    }
    case ERR_MODIFY_STATIC_PROC:
    { Procedure proc = va_arg(args, Procedure);
      term_t pred = PL_new_term_ref();

      rc = (unify_definition(MODULE_user, pred, proc->definition, 0,
			     GP_NAMEARITY|GP_HIDESYSTEM) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_permission_error3,
			    PL_ATOM, ATOM_modify,
			    PL_ATOM, ATOM_static_procedure,
			    PL_TERM, pred));
      break;
    }
    case ERR_MODIFY_THREAD_LOCAL_PROC:
    { Procedure proc = va_arg(args, Procedure);
      term_t pred = PL_new_term_ref();

      rc = (unify_definition(MODULE_user, pred, proc->definition, 0,
			     GP_NAMEARITY|GP_HIDESYSTEM) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_permission_error3,
			    PL_ATOM, ATOM_modify,
			    PL_ATOM, ATOM_thread_local_procedure,
			    PL_TERM, pred));
      break;
    }
    case ERR_UNDEFINED_PROC:
    { Definition def = va_arg(args, Definition);
      Definition clr = va_arg(args, Definition);
      term_t pred = PL_new_term_ref();

      if ( clr )
	caller = clr;

      rc = (unify_definition(MODULE_user, pred, def, 0, GP_NAMEARITY) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_existence_error2,
			    PL_ATOM, ATOM_procedure,
			    PL_TERM, pred));
      break;
    }
    case ERR_PERMISSION_PROC:
    { atom_t op = va_arg(args, atom_t);
      atom_t type = va_arg(args, atom_t);
      predicate_t pred = va_arg(args, predicate_t);
      term_t pi = PL_new_term_ref();

      rc = ( PL_unify_predicate(pi, pred, GP_NAMEARITY|GP_HIDESYSTEM) &&
	     PL_unify_term(formal,
			   PL_FUNCTOR, FUNCTOR_permission_error3,
			     PL_ATOM, op,
			     PL_ATOM, type,
			     PL_TERM, pi));
      break;
    }
    case ERR_NOT_IMPLEMENTED_PROC:
    { const char *name = va_arg(args, const char *);
      int arity = va_arg(args, int);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_not_implemented2,
			   PL_ATOM, ATOM_procedure,
			   PL_FUNCTOR, FUNCTOR_divide2,
			     PL_CHARS, name,
			     PL_INT, arity);
      break;
    }
    case ERR_IMPORT_PROC:
    { predicate_t pred = va_arg(args, predicate_t);
      atom_t dest = va_arg(args, atom_t);
      atom_t old  = va_arg(args, atom_t);
      term_t pi = PL_new_term_ref();

      rc = ( PL_unify_predicate(pi, pred, GP_NAMEARITY) &&
	     PL_unify_term(formal,
			   PL_FUNCTOR, FUNCTOR_permission_error3,
			     PL_FUNCTOR, FUNCTOR_import_into1,
			       PL_ATOM, dest,
			     PL_ATOM, ATOM_procedure,
			     PL_TERM, pi));

      if ( rc && old )
      { rc = ( (msgterm = PL_new_term_ref()) &&
	       PL_unify_term(msgterm,
			     PL_FUNCTOR_CHARS, "already_from", 1,
			       PL_ATOM, old) );
      }

      break;
    }
    case ERR_FAILED:
    { term_t goal = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_failure_error1,
			   PL_TERM, goal);

      break;
    }
    case ERR_EVALUATION:
    { atom_t what = va_arg(args, atom_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_evaluation_error1,
			   PL_ATOM, what);
      break;
    }
    case ERR_NOT_EVALUABLE:
    { functor_t f = va_arg(args, functor_t);
      term_t actual = PL_new_term_ref();

      rc = (put_name_arity(actual, f) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_type_error2,
			    PL_ATOM, ATOM_evaluable,
			    PL_TERM, actual));
      break;
    }
    case ERR_DIV_BY_ZERO:
    { rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_evaluation_error1,
			   PL_ATOM, ATOM_zero_divisor);
      break;
    }
    case ERR_PERMISSION:
    { atom_t op   = va_arg(args, atom_t);
      atom_t type = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_permission_error3,
			   PL_ATOM, op,
			   PL_ATOM, type,
			   PL_TERM, obj);

      break;
    }
    case ERR_OCCURS_CHECK:
    { Word p1  = va_arg(args, Word);
      Word p2  = va_arg(args, Word);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_occurs_check2,
			   PL_TERM, pushWordAsTermRef(p1),
			   PL_TERM, pushWordAsTermRef(p2));
      popTermRef();
      popTermRef();

      break;
    }
    case ERR_TIMEOUT:
    { atom_t op   = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_timeout_error2,
			   PL_ATOM, op,
			   PL_TERM, obj);

      break;
    }
    case ERR_EXISTENCE:
    { atom_t type = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);

      rc = PL_unify_term(formal,
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
      { case EAGAIN:
	  action = ATOM_lock;		/* Hack for file-locking*/
	  /*FALLTHROUGH*/
	case EACCES:
	  rc = PL_unify_term(formal,
			     PL_FUNCTOR, FUNCTOR_permission_error3,
			       PL_ATOM, action,
			       PL_ATOM, type,
			       PL_TERM, file);
	  break;
	case EMFILE:
	case ENFILE:
	  rc = PL_unify_term(formal,
			     PL_FUNCTOR, FUNCTOR_resource_error1,
			       PL_ATOM, ATOM_max_files);
	  break;
#ifdef EPIPE
	case EPIPE:
	  if ( !msg )
	    msg = "Broken pipe";
	  /*FALLTHROUGH*/
#endif
	default:			/* what about the other cases? */
	  rc = PL_unify_term(formal,
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

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_io_error2,
			   PL_ATOM, action,
			   PL_TERM, stream);
      break;
    }
    case ERR_DDE_OP:
    { const char *op  = va_arg(args, const char *);
      const char *err = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_dde_error2,
			   PL_CHARS, op,
			   PL_CHARS, err);
      break;
    }
    case ERR_SHARED_OBJECT_OP:
    { atom_t action = va_arg(args, atom_t);
      const char *err = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_shared_object2,
			   PL_ATOM,  action,
			   PL_CHARS, err);
      break;
    }
    case ERR_NOT_IMPLEMENTED:		/* non-ISO */
    { const char *what = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_not_implemented2,
			   PL_ATOM, ATOM_feature,
			   PL_CHARS, what);
      break;
    }
    case ERR_RESOURCE:
    { atom_t what = va_arg(args, atom_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_resource_error1,
			   PL_ATOM, what);
      break;
    }
    case ERR_SYNTAX:
    { const char *what = va_arg(args, const char *);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_syntax_error1,
			   PL_CHARS, what);
      break;
    }
    case ERR_NOMEM:
    { rc = PL_unify_term(formal,
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
	  rc = PL_unify_term(formal,
			     PL_FUNCTOR, FUNCTOR_resource_error1,
			       PL_ATOM, ATOM_no_memory);
	  break;
	default:
	  rc = PL_unify_atom(formal, ATOM_system_error);
	  break;
      }

      break;
    }
    case ERR_SHELL_FAILED:
    { term_t cmd = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_shell2,
			   PL_ATOM, ATOM_execute,
			   PL_TERM, cmd);
      break;
    }
    case ERR_SHELL_SIGNALLED:
    { term_t cmd = va_arg(args, term_t);
      int sig = va_arg(args, int);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_shell2,
			   PL_FUNCTOR, FUNCTOR_signal1,
			     PL_INT, sig,
			 PL_TERM, cmd);
      break;
    }
    case ERR_SIGNALLED:
    { int   sig     = va_arg(args, int);
      char *signame = va_arg(args, char *);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_signal2,
			   PL_CHARS, signame,
			   PL_INT, sig);
      break;
    }
    case ERR_CLOSED_STREAM:
    { IOSTREAM *s = va_arg(args, IOSTREAM *);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_existence_error2,
			   PL_ATOM, ATOM_stream,
			   PL_POINTER, s);
      do_throw = TRUE;
      break;
    }
    case ERR_BUSY:
    { atom_t type  = va_arg(args, atom_t);
      term_t mutex = va_arg(args, term_t);

      rc = PL_unify_term(formal, PL_FUNCTOR, FUNCTOR_busy2, type, mutex);
      break;
    }
    case ERR_FORMAT:
    { const char *s = va_arg(args, const char*);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR_CHARS, "format", 1,
			   PL_CHARS, s);
      break;
    }
    case ERR_FORMAT_ARG:
    { const char *s = va_arg(args, const char*);
      term_t arg = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR_CHARS, "format_argument_type", 2,
			   PL_CHARS, s,
			   PL_TERM, arg);
      break;
    }
    default:
      assert(0);
  }
  va_end(args);

					/* build SWI-Prolog context term */
  if ( rc && (pred || msg || msgterm || caller) )
  { term_t predterm = PL_new_term_ref();

    if ( !msgterm )
      msgterm  = PL_new_term_ref();

    if ( pred )
    { rc = PL_unify_term(predterm,
			 PL_FUNCTOR, FUNCTOR_divide2,
			   PL_CHARS, pred,
			   PL_INT, arity);
    } else if ( caller )
    { rc = unify_definition(MODULE_user, predterm, caller, 0, GP_NAMEARITY);
    }

    if ( rc && msg )
    { rc = PL_put_atom_chars(msgterm, msg);
    }

    if ( rc )
      rc = PL_unify_term(swi,
			 PL_FUNCTOR, FUNCTOR_context2,
			   PL_TERM, predterm,
			   PL_TERM, msgterm);
  }

  if ( rc )
    rc = PL_unify_term(except,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_TERM, formal,
		         PL_TERM, swi);

  if ( !rc )
  { nomem:
    fatalError("Cannot report error: no memory");
  }

  if ( do_throw )
    rc = PL_throw(except);
  else
    rc = PL_raise_exception(except);

  PL_close_foreign_frame(fid);

  return rc;
}


		 /*******************************
		 *	  TYPICAL ERRORS	*
		 *******************************/

int
PL_instantiation_error(term_t actual)
{ (void)actual;

  return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
}

int
PL_uninstantiation_error(term_t actual)
{ return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 0, actual);
}

int
PL_representation_error(const char *resource)
{ atom_t r = PL_new_atom(resource);
  int rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, r);
  PL_unregister_atom(r);

  return rc;
}


int
PL_type_error(const char *expected, term_t actual)
{ return PL_error(NULL, 0, NULL, ERR_CHARS_TYPE, expected, actual);
}


int
PL_domain_error(const char *expected, term_t actual)
{ atom_t a = PL_new_atom(expected);
  int rc = PL_error(NULL, 0, NULL, ERR_DOMAIN, a, actual);
  PL_unregister_atom(a);

  return rc;
}


int
PL_existence_error(const char *type, term_t actual)
{ atom_t a = PL_new_atom(type);
  int rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, a, actual);
  PL_unregister_atom(a);

  return rc;
}


int
PL_permission_error(const char *op, const char *type, term_t obj)
{ atom_t t = PL_new_atom(type);
  atom_t o = PL_new_atom(op);
  int rc = PL_error(NULL, 0, NULL, ERR_PERMISSION, o, t, obj);

  PL_unregister_atom(t);
  PL_unregister_atom(o);

  return rc;
}


int
PL_resource_error(const char *resource)
{ atom_t r = PL_new_atom(resource);
  int rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, r);

  PL_unregister_atom(r);

  return rc;
}


int
PL_no_memory(void)
{ return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_memory);
}


		 /*******************************
		 *	PRINTING MESSAGES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
printMessage(atom_t severity, ...)

Calls print_message(severity, term), where  ...   are  arguments  as for
PL_unify_term(). This predicate saves possible   pending  exceptions and
restores them to make the call from B_THROW possible.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define OK_RECURSIVE 10

int
printMessage(atom_t severity, ...)
{ GET_LD
  wakeup_state wstate;
  term_t av;
  predicate_t pred = PROCEDURE_print_message2;
  va_list args;
  int rc;

  if ( ++LD->in_print_message >= OK_RECURSIVE*3 )
    fatalError("printMessage(): recursive call\n");
  if ( !saveWakeup(&wstate, TRUE PASS_LD) )
  { LD->in_print_message--;
    return FALSE;
  }

  av = PL_new_term_refs(2);
  va_start(args, severity);
  PL_put_atom(av+0, severity);
  rc = PL_unify_termv(av+1, args);
  va_end(args);

  if ( rc )
  { if ( isDefinedProcedure(pred) && LD->in_print_message <= OK_RECURSIVE )
    { rc = PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION,
			     pred, av);
    } else if ( LD->in_print_message <= OK_RECURSIVE*2 )
    { Sfprintf(Serror, "Message: ");
      rc = PL_write_term(Serror, av+1, 1200, 0);
      Sfprintf(Serror, "\n");
    } else				/* in_print_message == 2 */
    { Sfprintf(Serror, "printMessage(): recursive call\n");
    }
  }

  restoreWakeup(&wstate PASS_LD);
  LD->in_print_message--;

  return rc;
}


		 /*******************************
		 *    ERROR-CHECKING *_get()	*
		 *******************************/

int
PL_get_atom_ex__LD(term_t t, atom_t *a ARG_LD)
{ if ( PL_get_atom(t, a) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, t);
}


#undef PL_get_atom_ex
int
PL_get_atom_ex(term_t t, atom_t *a)
{ GET_LD

  return PL_get_atom_ex__LD(t, a PASS_LD);
}
#define PL_get_atom_ex(t, a)	PL_get_atom_ex__LD(t, a PASS_LD)


int
PL_get_integer_ex(term_t t, int *i)
{ GET_LD

  if ( PL_get_integer(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_long_ex(term_t t, long *i)
{ GET_LD

  if ( PL_get_long(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_long);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_int64_ex(term_t t, int64_t *i)
{ GET_LD

  if ( PL_get_int64(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int64_t);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


int
PL_get_intptr_ex(term_t t, intptr_t *i)
{
#if SIZEOF_LONG != SIZEOF_VOIDP && SIZEOF_VOIDP == 8
   return PL_get_int64_ex(t, i);
#else
   return PL_get_long_ex(t, (long*)i);
#endif
}


int
PL_get_size_ex(term_t t, size_t *i)
{ int64_t val;

  if ( !PL_get_int64_ex(t, &val) )
    fail;
  if ( val < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_not_less_than_zero, t);
#if SIZEOF_VOIDP < 8
#if SIZEOF_LONG == SIZEOF_VOIDP
  if ( val > (int64_t)ULONG_MAX )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_size_t);
#endif
#endif

  *i = (size_t)val;

  return TRUE;
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
PL_get_char_ex(term_t t, int *p, int eof)
{ if ( PL_get_char(t, p, eof) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, t);
}


int
PL_get_pointer_ex(term_t t, void **addrp)
{ GET_LD
  if ( PL_get_pointer(t, addrp) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_address, t);
}


int
PL_unify_list_ex(term_t l, term_t h, term_t t)
{ GET_LD

  if ( PL_unify_list(l, h, t) )
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
{ GET_LD

  if ( PL_get_list(l, h, t) )
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
PL_unify_bool_ex(term_t t, int val)
{ GET_LD
  bool v;

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
{ GET_LD

  if ( PL_get_arg(n, term, arg) )
  { succeed;
  } else
  { term_t a = PL_new_term_ref();

    PL_put_integer(a, n);

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_natural, a);
  }
}


int
PL_get_module_ex(term_t name, Module *m)
{ if ( !PL_get_module(name, m) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, name);

  succeed;
}
