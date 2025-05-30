/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2024, University of Amsterdam
			      VU University Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
throw(error(<Formal>, <SWI-Prolog>))

<SWI-Prolog>	::= context(Name/Arity, Message)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-fli.h"
#include "pl-attvar.h"
#include "pl-proc.h"
#include "os/pl-cstack.h"
/* BeOS has EACCES defined elsewhere, but errno is here */
#if !defined(EACCES) || defined(__BEOS__)
#include <errno.h>
#endif

static bool
put_name_arity(term_t t, functor_t f)
{ GET_LD
  FunctorDef fdef = valueFunctor(f);
  term_t a;

  if ( (a=PL_new_term_refs(2)) )
  { PL_put_atom(a+0, fdef->name);

    return (PL_put_integer(a+1, fdef->arity) &&
	    PL_cons_functor(t, FUNCTOR_divide2, a+0, a+1));
  }

  return false;
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


static bool
evaluation_error(term_t formal, atom_t which)
{ GET_LD
  return PL_unify_term(formal,
		       PL_FUNCTOR, FUNCTOR_evaluation_error1,
			 PL_ATOM, which);
}

bool
PL_error(const char *pred, int arity, const char *msg, PL_error_code id, ...)
{ GET_LD
  char msgbuf[50];
  Definition caller;
  term_t except, formal, swi, msgterm=0;
  va_list args;
  int do_throw = false;
  fid_t fid;
  bool rc;
  int msg_rep = REP_UTF8;

  if ( exception_term )			/* do not overrule older exception */
    return false;

  if ( environment_frame )
    caller = environment_frame->predicate;
  else
    caller = NULL;

  if ( id == ERR_FILE_OPERATION &&
       !truePrologFlag(PLFLAG_FILEERRORS) )
    fail;

  if ( msg == MSG_ERRNO )
  { if ( errno == EPLEXCEPTION )
      return false;
    msg = OsError();
    msg_rep = REP_MB;
  }

  LD->exception.processing = true;	/* allow using spare stack */

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
    { rc = evaluation_error(formal, ATOM_undefined);
      break;
    }
    case ERR_AR_OVERFLOW:
    { rc = evaluation_error(formal, ATOM_float_overflow);
      break;
    }
    case ERR_AR_RAT_OVERFLOW:
    { rc = evaluation_error(formal, ATOM_rational_overflow);
      break;
    }
    case ERR_AR_UNDERFLOW:
    { rc = evaluation_error(formal, ATOM_float_underflow);
      break;
    }
    case ERR_AR_TRIPWIRE:
    { atom_t tripwire = va_arg(args, atom_t);
      Number num      = va_arg(args, Number);
      term_t actual   = PL_new_term_ref();

      rc = (_PL_put_number(actual, num) &&
	    PL_unify_term(formal,
			  PL_FUNCTOR, FUNCTOR_resource_error1,
			    PL_FUNCTOR, FUNCTOR_tripwire2,
			      PL_ATOM, tripwire,
			      PL_TERM, actual));
      break;
    }
    case ERR_DOMAIN:			/*  ERR_INSTANTIATION if var(arg) */
    { atom_t domain = va_arg(args, atom_t);
      term_t arg    = va_arg(args, term_t);

    case_domain_error:
      if ( PL_is_variable(arg) )
	goto err_instantiation;

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_domain_error2,
			   PL_ATOM, domain,
			   PL_TERM, arg);
      break;
    case ERR_PTR_DOMAIN:		/* atom_t, Word */
      { Word ptr;

	domain = va_arg(args, atom_t);
	ptr    = va_arg(args, Word);
	arg    = PL_new_term_ref();

	*valTermRef(arg) = *ptr;
	goto case_domain_error;
      }
    }
    case ERR_RANGE:			/*  domain_error(range(low,high), arg) */
    { term_t low  = va_arg(args, term_t);
      term_t high = va_arg(args, term_t);
      term_t arg  = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_domain_error2,
			   PL_FUNCTOR, FUNCTOR_range2,
			     PL_TERM, low,
			     PL_TERM, high,
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
  { Definition def;				/* shared variables */
    Procedure proc;
    term_t pred;

    case ERR_MODIFY_STATIC_PROC:
      proc = va_arg(args, Procedure);
      def = proc->definition;
      goto modify_static;
    case ERR_MODIFY_STATIC_PREDICATE:
      def = va_arg(args, Definition);

    modify_static:
      rc = ((pred = PL_new_term_ref()) &&
	    unify_definition(MODULE_user, pred, def, 0,
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
    case ERR_PERMISSION_SSU_DEF:
    { Definition def = va_arg(args, Definition);

      term_t pi = PL_new_term_ref();

      rc = ( unify_definition(MODULE_user, pi, def, 0,
			      GP_NAMEARITY|GP_HIDESYSTEM) &&
	     PL_unify_term(formal,
			   PL_FUNCTOR, FUNCTOR_permission_error3,
			     PL_ATOM, ATOM_assert,
			     PL_ATOM, ATOM_procedure,
			     PL_TERM, pi));
      break;
    }
    case ERR_PERMISSION_VMI:
    { const char *vmi = va_arg(args, const char *);
      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_permission_error3,
			   PL_ATOM, ATOM_execute,
			   PL_ATOM, ATOM_vmi,
			   PL_CHARS, vmi);
      break;
    }
    case ERR_PERMISSION_YIELD:
    { term_t t;
      rc = ( (t=PL_new_term_ref()) &&
	     pl_thread_self(t) &&
	     PL_unify_term(formal,
			   PL_FUNCTOR, FUNCTOR_permission_error3,
			     PL_ATOM, ATOM_yield,
			     PL_ATOM, ATOM_engine,
			     PL_TERM, t) );
      PL_reset_term_refs(t);
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
    case ERR_DETERMINISM:
    { Definition def  = va_arg(args, Definition);
      atom_t expected = va_arg(args, atom_t);
      atom_t found    = va_arg(args, atom_t);
      atom_t decl     = va_arg(args, atom_t);
      term_t pi       = PL_new_term_ref();

      rc = ( unify_definition(MODULE_user, pi, def, 0,
			      GP_NAMEARITY|GP_HIDESYSTEM) &&
	     PL_unify_term(formal,
			   PL_FUNCTOR, FUNCTOR_determinism_error4,
			     PL_TERM, pi,
			     PL_ATOM, expected,
			     PL_ATOM, found,
			     PL_ATOM, decl));
      break;
    }
    case ERR_DET_GOAL:
    { term_t goal     = va_arg(args, term_t);
      atom_t expected = va_arg(args, atom_t);
      atom_t found    = va_arg(args, atom_t);

      rc = ( PL_unify_term(formal,
			   PL_FUNCTOR, FUNCTOR_determinism_error4,
			     PL_TERM, goal,
			     PL_ATOM, expected,
			     PL_ATOM, found,
			     PL_ATOM, ATOM_goal));
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
    case ERR_EXISTENCE3:
    { atom_t type = va_arg(args, atom_t);
      term_t obj  = va_arg(args, term_t);
      term_t in   = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_existence_error3,
			   PL_ATOM, type,
			   PL_TERM, obj,
			   PL_TERM, in);

      break;
    }
    case ERR_FILE_OPERATION:
    { atom_t action = va_arg(args, atom_t);
      atom_t type   = va_arg(args, atom_t);
      term_t file   = va_arg(args, term_t);
      atom_t repr   = ATOM_max_path_length;

      switch(errno)
      { case EAGAIN:
	  action = ATOM_lock;		/* Hack for file-locking*/
	  /*FALLTHROUGH*/
	case EACCES:
	case EPERM:
#ifdef EROFS
	case EROFS:
#endif
	case ENOTEMPTY:
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
#ifdef ELOOP
	case ELOOP:
	  repr = ATOM_max_symbolic_links;
	  /*FALLTHROUGH*/
#endif
	case ENAMETOOLONG:
	  rc = PL_unify_term(formal,
			     PL_FUNCTOR, FUNCTOR_representation_error1,
			       PL_ATOM, repr);
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
      do_throw = true;
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
    case ERR_DUPLICATE_KEY:
    { term_t key = va_arg(args, term_t);

      rc = PL_unify_term(formal,
			 PL_FUNCTOR, FUNCTOR_duplicate_key1,
			   PL_TERM, key);
      break;
    }
    default:
      rc = false;
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
    { rc = PL_put_chars(msgterm, PL_ATOM|msg_rep, (size_t)-1, msg);
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

bool
PL_instantiation_error(term_t actual)
{ (void)actual;

  return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
}

bool
PL_uninstantiation_error(term_t actual)
{ valid_term_t(actual);
  return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 0, actual);
}

bool
PL_representation_error(const char *representation)
{ atom_t r = PL_new_atom(representation);
  int rc = PL_error(NULL, 0, NULL, ERR_REPRESENTATION, r);
  PL_unregister_atom(r);

  return rc;
}


bool
PL_type_error(const char *expected, term_t actual)
{ valid_term_t(actual);
  return PL_error(NULL, 0, NULL, ERR_CHARS_TYPE, expected, actual);
}


bool
PL_domain_error(const char *expected, term_t actual)
{ valid_term_t(actual);
  atom_t a = PL_new_atom(expected);
  int rc = PL_error(NULL, 0, NULL, ERR_DOMAIN, a, actual);
  PL_unregister_atom(a);

  return rc;
}


bool
PL_existence_error(const char *type, term_t actual)
{ valid_term_t(actual);
  atom_t a = PL_new_atom(type);
  int rc = PL_error(NULL, 0, NULL, ERR_EXISTENCE, a, actual);
  PL_unregister_atom(a);

  return rc;
}


bool
PL_permission_error(const char *op, const char *type, term_t obj)
{ valid_term_t(obj);
  atom_t t = PL_new_atom(type);
  atom_t o = PL_new_atom(op);
  int rc = PL_error(NULL, 0, NULL, ERR_PERMISSION, o, t, obj);

  PL_unregister_atom(t);
  PL_unregister_atom(o);

  return rc;
}

bool
PL_resource_error(const char *resource)
{ atom_t r = PL_new_atom(resource);
  int rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, r);

  PL_unregister_atom(r);

  return rc;
}


bool
PL_no_memory(void)
{ return PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_memory);
}


bool
PL_syntax_error(const char *msg, IOSTREAM *in)
{ GET_LD
  term_t ex, loc;

  if ( (ex = PL_new_term_ref()) &&
       (loc = PL_new_term_ref()) &&
       PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_syntax_error1,
			 PL_CHARS, msg,
		       PL_TERM, loc) )
  { if ( in )
    { IOPOS *pos;
      term_t s;

      if ( (s=PL_new_term_ref()) &&
	   PL_unify_stream_or_alias(s, in) )
      { if ( (pos=in->position) )
	{ if ( PL_unify_term(loc, PL_FUNCTOR, FUNCTOR_stream4,
				    PL_TERM,  s,
				    PL_INT,   pos->lineno,
				    PL_INT,   pos->linepos,
				    PL_INT64, pos->charno) )
	    goto ok;
	} else
	{ if ( PL_unify_term(loc, PL_FUNCTOR, FUNCTOR_stream1,
				    PL_TERM,  s) )
	    goto ok;
	}
      }

      return false;
    }

  ok:
    return PL_raise_exception(ex);
  }

  return false;
}

		 /*******************************
		 *	PRINTING MESSAGES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
printMessage(atom_t severity, ...)

Calls print_message(severity, term), where  ...   are  arguments  as for
PL_unify_term(). This predicate saves possible   pending  exceptions and
restores them to make the call from B_THROW possible.

Returns false if there was an   exception while executing printMessage()
and true if the printing succeeded or merely failed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define OK_RECURSIVE 10

bool
printMessage(atom_t severity, ...)
{ va_list args;
  bool rc;

  va_start(args, severity);
  rc = printMessagev(severity, args);
  va_end(args);

  return rc;
}

bool
printMessagev(atom_t severity, va_list args)
{ GET_LD
  valid_atom_t(severity);
  wakeup_state wstate;
  term_t av;
  predicate_t pred = PROCEDURE_print_message2;
  bool rc;

  if ( ++LD->in_print_message >= OK_RECURSIVE*3 )
    fatalError("printMessage(): recursive call\n");
  if ( !saveWakeup(&wstate, true) )
  { LD->in_print_message--;
    return false;
  }

  av = PL_new_term_refs(2);
  PL_put_atom(av+0, severity);
  rc = PL_unify_termv(av+1, args);

  if ( rc )
  { if ( !Sferror(Suser_error) )
    { if ( isDefinedProcedure(pred) &&
	   LD->in_print_message <= OK_RECURSIVE )
      { rc = PL_call_predicate(NULL, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
			       pred, av);
      } else
      { if ( LD->in_print_message <= OK_RECURSIVE*2 )
	{ Sdprintf("print_message/2: recursive call: ");
	  if ( ReadingSource )
	    Sdprintf("%s:%d ",
		     PL_atom_chars(source_file_name), (int)source_line_no);
	  rc = PL_write_term(Serror, av+1, 1200, 0);
	  Sdprintf("\n");
	  PL_backtrace(5, 1);
	} else				/* in_print_message == 2 */
	{ Sdprintf("printMessage(): recursive call\n");
	}
      }
    } else
    { if ( !Sferror(Serror) && truePrologFlag(PLFLAG_DEBUG_ON_ERROR) )
      { Sdprintf("[%d] printMessage(): Cannot write to user_error:\n",
		 PL_thread_self());
	rc = PL_write_term(Serror, av+1, 1200, 0);
	Sdprintf("\n");
      } else
	rc = false;
    }
  }

  if ( !rc && PL_exception(0) )
    set(&wstate, WAKEUP_KEEP_URGENT_EXCEPTION);
  else
    rc = true;

  restoreWakeup(&wstate);
  LD->in_print_message--;

  return rc;
}


		 /*******************************
		 *    ERROR-CHECKING *_get()	*
		 *******************************/

bool
PL_get_atom_ex(DECL_LD term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, t);
}


API_STUB(bool)
(PL_get_atom_ex)(term_t t, atom_t *a)
( valid_term_t(t);
  return PL_get_atom_ex(t, a);
)


bool
PL_get_integer_ex(term_t t, int *i)
{ GET_LD
  valid_term_t(t);

  if ( PL_get_integer(t, i) )
    succeed;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}

bool
PL_get_long_ex(DECL_LD term_t t, long *i)
{ if ( PL_get_long(t, i) )
    return true;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_long);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}

API_STUB(bool)
(PL_get_long_ex)(term_t t, long *i)
( valid_term_t(t);
  return PL_get_long_ex(t, i);
)

bool
PL_get_int64_ex(DECL_LD term_t t, int64_t *i)
{ if ( PL_get_int64(t, i) )
    return true;

  if ( PL_is_integer(t) )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int64_t);

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}

API_STUB(bool)
(PL_get_int64_ex)(term_t t, int64_t *i)
( valid_term_t(t);
  return PL_get_int64_ex(t, i);
)

bool
PL_get_intptr_ex(DECL_LD term_t t, intptr_t *i)
{
#if SIZEOF_LONG != SIZEOF_VOIDP && SIZEOF_VOIDP == 8
   return PL_get_int64_ex(t, i);
#else
   return PL_get_long_ex(t, (long*)i);
#endif
}

API_STUB(bool)
(PL_get_intptr_ex)(term_t t, intptr_t *i)
( valid_term_t(t);
  return PL_get_intptr_ex(t, i);
)

#if SIZEOF_VOIDP < 8
#ifndef UINTPTR_MAX
#define UINTPTR_MAX ~(uintptr_t)0;
#endif

static int
fits_size(int64_t val)
{ if ( (uintptr_t)val <= (uintptr_t)UINTPTR_MAX )
    return true;
  return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_size_t);
}
#else
#define fits_size(v) true
#endif

bool
PL_get_size_ex(DECL_LD term_t t, size_t *i)
{ number n;
  Word p = valTermRef(t);

  deRef(p);
  if ( isTaggedInt(*p) )
  { sword v = valInt(*p);

    if ( v >= 0 )
    {
#if SIZEOF_VOIDP < SIZEOF_WORD
      if ( v > SIZE_MAX )
	return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_size_t);
#endif
      *i = (size_t)v;
      return true;
    }
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_not_less_than_zero, t);
  }

  if ( PL_get_number(t, &n) )
  { switch(n.type)
    { case V_INTEGER:
	if ( n.value.i >= 0 )
	{ if ( fits_size(n.value.i) )
	  { *i = (size_t)n.value.i;
	    return true;
	  }
	  return false;
	} else
	{ return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			  ATOM_not_less_than_zero, t);
	}
#if SIZEOF_VOIDP == 8 && defined(O_BIGNUM)
      case V_MPZ:
      { uint64_t v;

	switch(mpz_to_uint64(n.value.mpz, &v))
	{ case 0:
	    *i = v;
	    return true;
	  case -1:
	    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			    ATOM_not_less_than_zero, t);
	  case 1:
	    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_size_t);
	  default:
	    assert(0);
	    return false;
	}
      }
#else
      return PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_size_t);
#endif
      default:
	break;
    }
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t);
}


bool
pl_get_uint64(DECL_LD term_t t, uint64_t *i, int ex)
{ number n;
  Word p = valTermRef(t);

  deRef(p);
  if ( isTaggedInt(*p) )
  { sword v = valInt(*p);

    if ( v >= 0 )
    { *i = v;
      return true;
    }
    return ex ? PL_error(NULL, 0, NULL, ERR_DOMAIN,
			 ATOM_not_less_than_zero, t)
	      : false;
  }

  if ( PL_get_number(t, &n) )
  { switch(n.type)
    { case V_INTEGER:
	if ( n.value.i >= 0 )
	{ *i = n.value.i;
	  return true;
	} else
	{ return ex ? PL_error(NULL, 0, NULL, ERR_DOMAIN,
			       ATOM_not_less_than_zero, t)
		    : false;
	}
#if O_BIGNUM
      case V_MPZ:
      { uint64_t v;

	switch(mpz_to_uint64(n.value.mpz, &v))
	{ case 0:
	    *i = v;
	    return true;
	  case -1:
	    return ex ? PL_error(NULL, 0, NULL, ERR_DOMAIN,
				 ATOM_not_less_than_zero, t)
		      : false;
	  case 1:
	    return ex ? PL_representation_error("uint64_t") : false;
	  default:
	    assert(0);
	    return false;
	}
      }
#else
      return ex ? PL_representation_error("uint64_t") : false;
#endif
      default:
	break;
    }
  }

  return ex ? PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, t) : false;
}

bool
PL_get_uint64_ex(DECL_LD term_t t, uint64_t *i)
{ return pl_get_uint64(t, i, true);
}

API_STUB(bool)
(PL_get_size_ex)(term_t t, size_t *i)
( valid_term_t(t);
  return PL_get_size_ex(t, i);
)

bool
PL_get_bool_ex(term_t t, int *i)
{ valid_term_t(t);
  if ( PL_get_bool(t, i) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}


bool
PL_get_char_ex(term_t t, int *p, int eof)
{ valid_term_t(t);
  if ( PL_get_char(t, p, eof) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_character, t);
}


bool
PL_get_pointer_ex(term_t t, void **addrp)
{ GET_LD
  valid_term_t(t);
  if ( PL_get_pointer(t, addrp) )
    succeed;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_address, t);
}


bool
PL_unify_list_ex(term_t l, term_t h, term_t t)
{ GET_LD
  valid_term_t(l);
  valid_term_t(h);
  valid_term_t(t);

  if ( PL_unify_list(l, h, t) )
    succeed;

  if ( PL_get_nil(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


bool
PL_unify_nil_ex(term_t l)
{ valid_term_t(l);
  if ( PL_unify_nil(l) )
    succeed;

  if ( PL_is_list(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}


bool
PL_get_list_ex(term_t l, term_t h, term_t t)
{ GET_LD
  valid_term_t(l);
  valid_term_t(h);
  valid_term_t(t);

  if ( PL_get_list(l, h, t) )
    succeed;

  if ( PL_get_nil(l) )
    fail;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}

bool
PL_get_nil_ex(term_t l)
{ if ( PL_exception(0) )
    return false;

  valid_term_t(l);
  if ( PL_get_nil(l) )
    return true;

  if ( PL_is_list(l) )
    return false;

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
}

bool
PL_unify_bool_ex(term_t t, int val)
{ GET_LD
  int v;
  valid_term_t(t);

  if ( PL_is_variable(t) )
    return PL_unify_atom(t, val ? ATOM_true : ATOM_false);
  if ( PL_get_bool(t, &v) )
  { if ( (!val && !v) || (val && v) )
      succeed;
    fail;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, t);
}


bool
PL_get_arg_ex(size_t n, term_t term, term_t arg)
{ GET_LD
  valid_term_t(term);
  valid_term_t(arg);

  if ( PL_get_arg(n, term, arg) )
  { return true;
  } else
  { term_t a;

    return ( (a=PL_new_term_ref()) &&
	     PL_put_uint64(a, n) &&
	     PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_natural, a) );
  }
}


bool
PL_get_module_ex(term_t name, Module *m)
{ valid_term_t(name);
  if ( !PL_get_module(name, m) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, name);

  return true;
}


static
PRED_IMPL("$inc_message_count", 1, inc_message_count, 0)
{ PRED_LD
  atom_t a;

  if ( PL_get_atom_ex(A1, &a) )
  { if ( a == ATOM_error )
    { ATOMIC_INC(&GD->statistics.errors);
      LD->statistics.errors++;
    } else if ( a == ATOM_warning )
    { ATOMIC_INC(&GD->statistics.warnings);
      LD->statistics.warnings++;
    } /* else ignore other levels */

    return true;
  }

  return false;
}

BeginPredDefs(error)
  PRED_DEF("$inc_message_count", 1, inc_message_count, 0)
EndPredDefs
