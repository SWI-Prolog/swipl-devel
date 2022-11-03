/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-wrap.h"
#include "pl-dbref.h"
#include "pl-util.h"
#include "pl-supervisor.h"
#include "pl-proc.h"
#include "pl-gc.h"
#include "pl-fli.h"
#include "pl-funct.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Wrap and unwrap predicates. Wrapping  is   realised  by manipulating the
predicate _supervisor_:

  - The wrapped predicate P gets a new supervisor that calls a
    dedicated clause in '$wrap$P'.
  - This clause may use call(Closure, Arg ...), where Closure is
    a blob that contains a predicate (struct definition) that is
    a copy of P which contains
    - A pointer to P in impl.wrapped.predicate
    - A pointer to P's supervisor in impl.wrapped.supervisor
    - A supervisor running S_WRAP.
  - I_USERCALLN picks up call(Closure, Arg ...) and sets up a
    call using the closure's copy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      CLOSURES		*
		 *******************************/

static int
write_closure(IOSTREAM *s, atom_t aref, int flags)
{ closure *c = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<closure>(%s)", predicateName(&c->def));
  return TRUE;
}


static void
acquire_closure(atom_t aref)
{ closure *c = PL_blob_data(aref, NULL, NULL);
  (void)c;
}


static int
release_closure(atom_t aref)
{ closure *c = PL_blob_data(aref, NULL, NULL);
  Definition def = &c->def;

  freeCodesDefinition(def, FALSE);
  free_lingering(&def->lingering, GEN_MAX);

  return TRUE;
}


static int
save_closure(atom_t aref, IOSTREAM *fd)
{ closure *c = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save closure <closure>(%s)", predicateName(&c->def));
}


static atom_t
load_closure(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-closure>");
}


PL_blob_t _PL_closure_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "closure",
  release_closure,
  NULL,
  write_closure,
  acquire_closure,
  save_closure,
  load_closure
};


static int
unify_closure(term_t t, Definition def, Code supervisor)
{ closure c;

  c.def = *def;
  c.def.impl.wrapped.predicate  = def;
  c.def.impl.wrapped.supervisor = supervisor;
  c.def.codes = SUPERVISOR(wrapper);

  return PL_unify_blob(t, &c, sizeof(c), &_PL_closure_blob);
}


/** get_closure_predicate(term_t t, Definition *def)
 * Get the predicate that is referenced by a closure.  Fails silently
 * if t is not a closure.
 */

int
get_closure_predicate(DECL_LD term_t t, Definition *def)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) &&
       type == &_PL_closure_blob )
  { closure *c = data;
    *def = c->def.impl.wrapped.predicate;

    return TRUE;
  }

  return FALSE;
}


void
unregisterWrappedSupervisor(Code codes)
{ if ( unlikely(codes[0] == encode(S_CALLWRAPPER)) )
  { atom_t aref  = (atom_t)codes[2];
    atom_t wname = (atom_t)codes[3];

    PL_unregister_atom(aref);
    PL_unregister_atom(wname);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called  from  freeCodesDefinition()  to  reset  the  eventually  wrapped
supervisor to S_VIRGIN after a change   to the wrapped predicate. S_WRAP
will eventually trap this and re-create an appropriate new supervisor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
resetWrappedSupervisor(Definition def0, int do_linger)
{ Definition def = def0;
  Code codes = def->codes;

  while ( codes[0] == encode(S_CALLWRAPPER) )
  { closure *c = PL_blob_data(codes[2], NULL, NULL);

    def   = &c->def;
    codes = c->def.impl.wrapped.supervisor;
  }

  assert(def != def0);	/* def is the definition inside the closure */
  def->impl.wrapped.supervisor = SUPERVISOR(virgin);
  freeSupervisor(def, codes, do_linger);
}


static Code
find_wrapper(Definition def, atom_t name)
{ Code codes = def->codes;

  while ( codes[0] == encode(S_CALLWRAPPER) )
  { closure *c;

    if ( codes[3] == (code)name )
      return codes;

    c = PL_blob_data(codes[2], NULL, NULL);
    codes = c->def.impl.wrapped.supervisor;
  }

  return NULL;
}


#define assert_wrapper(clause) LDFUNC(assert_wrapper, clause)
static ClauseRef
assert_wrapper(DECL_LD term_t clause)
{ Clause cl;

  if ( (cl = assert_term(clause, NULL, CL_END, NULL_ATOM, NULL, 0)) )
  { Definition def = cl->predicate;
    definition_ref *dref = pushPredicateAccessObj(def);
    ClauseRef cref;

    if ( !dref )
    { retractClauseDefinition(def, cl, FALSE);
      return NULL;
    }
    acquire_def(def);
    for( cref = def->impl.clauses.first_clause; cref; cref = cref->next)
    { if ( cref->value.clause == cl )
	break;
    }
    release_def(def);
    popPredicateAccess(def);

    assert(cref && cref->value.clause == cl);

    return cref;
  }

  return NULL;
}


#define unify_wrapped(wrapped, closure, head) LDFUNC(unify_wrapped, wrapped, closure, head)
static int
unify_wrapped(DECL_LD term_t wrapped, atom_t closure, term_t head)
{ Word from;

retry:
  from = valTermRef(head);
  deRef(from);
  if ( isTerm(*from) )
  { Functor fd = valueTerm(*from);
    size_t arity = arityFunctor(fd->definition);

    if ( hasGlobalSpace(arity+1) )		/* ensure space for unify */
    { Word to = allocGlobalNoShift(arity+1);
      word w  = consPtr(to, TAG_COMPOUND|STG_GLOBAL);
      Word f  = fd->arguments;

      *to++ = PL_new_functor(closure, arity);
      for(; arity > 0; arity--)
	*to++ = linkValI(f++);

      return _PL_unify_atomic(wrapped, w);
    } else
    { int rc;

      if ( (rc = ensureGlobalSpace(1+arity, ALLOW_GC)) == TRUE )
	goto retry;

      return raiseStackOverflow(rc);
    }
  } else
  { return PL_unify_atom(wrapped, closure);
  }
}

		 /*******************************
		 *	      PROLOG		*
		 *******************************/

/** '$c_wrap_predicate'(:Head, +Name, -Closure, -Wrapped, +Body)
 *
 * Install a wrapper for the predicate Head
 */

static
PRED_IMPL("$c_wrap_predicate", 5, c_wrap_predicate, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;
  atom_t wname;
  Code codes = NULL;
  term_t head = PL_new_term_ref();
  term_t closure = A3;
  Definition def;

  if ( !PL_get_atom_ex(A2, &wname) ||
       !get_procedure(A1, &proc, head, GP_DEFINE) )
    return FALSE;
  def = proc->definition;

  if ( (codes = find_wrapper(def, wname)) )
  { ClauseRef cref;
    atom_t aref = (atom_t)codes[2];

    if ( !PL_unify_atom(closure, aref) ||
	 !unify_wrapped(A4, aref, head) )
      return FALSE;

    if ( (cref = assert_wrapper(A5)) )
    { Clause cl = ((ClauseRef)codes[1])->value.clause;

      codes[1] = (code)cref;
      retractClauseDefinition(cl->predicate, cl, FALSE);

      return TRUE;
    }
  } else
  { if ( unify_closure(closure, def, def->codes) )
    { ClauseRef cref;
      atom_t aref;

      if ( !PL_get_atom_ex(closure, &aref) ||
	   !unify_wrapped(A4, aref, head) )
	return FALSE;				/* something really wrong */

      if ( (cref = assert_wrapper(A5)) )
      { codes = allocCodes(4);
	PL_register_atom(aref);
	PL_register_atom(wname);

	codes[0] = encode(S_CALLWRAPPER);
	codes[1] = (code)cref;
	codes[2] = (code)aref;
	codes[3] = (code)wname;

	setSupervisor(def, codes);

	return TRUE;
      }
    }
  }

  return FALSE;
}


/**
 * wrapped_predicate(:Head, -Wrappers)
 */

static
PRED_IMPL("wrapped_predicate", 2, wrapped_predicate, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;

  if ( get_procedure(A1, &proc, 0, GP_RESOLVE) )
  { Definition def = proc->definition;
    Code codes = def->codes;

    if ( def->codes[0] == encode(S_CALLWRAPPER) )
    { term_t tail = PL_copy_term_ref(A2);
      term_t head = PL_new_term_ref();
      term_t ct   = PL_new_term_ref();

      for(;;)
      { closure *c = PL_blob_data(codes[2], NULL, NULL);
	ClauseRef cref = (ClauseRef)codes[1];

	if ( !PL_put_clref(ct, cref->value.clause) ||
	     !PL_unify_list(tail, head, tail) ||
	     !PL_unify_term(head,
			    PL_FUNCTOR, FUNCTOR_minus2,
			      PL_ATOM, codes[3],
			      PL_TERM, ct) )
	  return FALSE;

	codes = c->def.impl.wrapped.supervisor;
	if ( codes[0] != encode(S_CALLWRAPPER) )
	  return PL_unify_nil(tail);
      }
    }
  }

  return FALSE;
}


/** '$wrapped_implementation'(:Goal, +Name, -Wrapped)
 *
 *  Calling Wrapped calls the predicate referenced by Goal as seen
 *  from the wrapper Name.
 */

static
PRED_IMPL("$wrapped_implementation", 3, wrapped_implementation,
	  PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;
  atom_t wname;
  Code codes;
  term_t head = PL_new_term_ref();

  if ( !PL_get_atom_ex(A2, &wname) ||
       !get_procedure(A1, &proc, head, GP_RESOLVE) )
    return FALSE;

  if ( (codes = find_wrapper(proc->definition, wname)) )
  { atom_t aref = codes[2];
    size_t arity = proc->definition->functor->arity;

    if ( arity > 0 )
    { Word p;
      term_t impl = PL_new_term_ref();

      if ( (p=allocGlobal(1+arity)) )
      { Word a = valTermRef(head);
	size_t i;

	deRef(a);
	a = valueTerm(*a)->arguments;
	p[0] = lookupFunctorDef(aref, arity);
	for(i=0; i<arity; i++)
	  p[i+1] = linkValI(&a[i]);
	*valTermRef(impl) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	return PL_unify(A3, impl);
      }
    } else
    { return PL_unify_atom(A3, aref);
    }
  }

  return FALSE;
}


/**
 * unwrap_predicate(:Head, ?Name) is semidet.
 *
 * Remove the latest wrapper. Should we   removing  a specifically named
 * supervisor?
 */

static
PRED_IMPL("unwrap_predicate", 2, uwrap_predicate, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;

  if ( get_procedure(A1, &proc, 0, GP_NAMEARITY|GP_RESOLVE) )
  { Definition def = proc->definition;
    Code *cp = &def->codes;
    Code codes = *cp;

    while ( codes[0] == encode(S_CALLWRAPPER) )
    { ClauseRef cref = (ClauseRef)codes[1];
      Clause cl = cref->value.clause;
      atom_t aref = (atom_t)codes[2];
      atom_t wname = (atom_t)codes[3];
      closure *cls = PL_blob_data(aref, NULL, NULL);

      if ( !PL_unify_atom(A2, wname) )
      { cp = &cls->def.impl.wrapped.supervisor;
	codes = *cp;
	continue;
      }

      retractClauseDefinition(cl->predicate, cl, FALSE);
      *cp = cls->def.impl.wrapped.supervisor;

      freeSupervisor(def, codes, TRUE);
      PL_unregister_atom(aref);
      PL_unregister_atom(wname);

      return TRUE;
    }
  }

  return FALSE;
}


static
PRED_IMPL("$closure_predicate", 2, closure_predicate, 0)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(A1, &data, NULL, &type) &&
       type == &_PL_closure_blob )
  { closure *c = data;

    return unify_definition(MODULE_user, A2, &c->def, 0, GP_QUALIFY|GP_NAMEARITY);
  }

  return PL_type_error("closure", A1);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define META PL_FA_TRANSPARENT

BeginPredDefs(wrap)
  PRED_DEF("$c_wrap_predicate",	      5, c_wrap_predicate,       META)
  PRED_DEF("unwrap_predicate",        2, uwrap_predicate,        META)
  PRED_DEF("$wrapped_predicate",      2, wrapped_predicate,      META)
  PRED_DEF("$wrapped_implementation", 3, wrapped_implementation, META)
  PRED_DEF("$closure_predicate",      2, closure_predicate,      0)
EndPredDefs
