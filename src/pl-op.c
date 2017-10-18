/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
In traditional Prolog, the operator table is global.  In modern systems
this is undesirable for at least two reasons:

	* Operators influence syntax and possibly semantics.  Using modules
	  we would like to minimise the scope of operator-changes.

	* Using multiple-threads, changing operators in one thread causes
	  read to behave differently in the other thread.

Proposal:

	* Any module has an operator-table.  Directives encountered in the
	  scope of a module are added to the module's local table.

	* Directives outside any module (or explicitly in user) manipulate
	  the global operator table.

To find the  current  definition  of   some  operator,  first  check the
thread's table, then the module-table and finally the global table.

current_op/3 gets difficult.  TBD.

Unfortunately, one atom can  be  at   most  3  operators (prefix, infix,
postfix). The best solution is probably to  define one structure for the
whole thing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t	operatorTypeToAtom(int type);

		 /*******************************
		 *	      TYPES		*
		 *******************************/

typedef struct _operator		/* storage in tables */
{ unsigned char	type[3];
  short priority[3];
} operator;

typedef struct _opdef			/* predefined and enumerated */
{ atom_t name;
  short  type;
  short  priority;
} opdef;

#define OP_INHERIT	0

		 /*******************************
		 *	  OPERATOR TABLE	*
		 *******************************/

static void
copyOperatorSymbol(void *name, void **value)
{ operator *op = *value;
  operator *o2 = allocHeapOrHalt(sizeof(*o2));

  *o2 = *op;
  *value = o2;
}


static void
freeOperatorSymbol(void *name, void *value)
{ operator *op = value;

  PL_unregister_atom((atom_t) name);
  freeHeap(op, sizeof(*op));
}


static Table
newOperatorTable(int size)
{ Table t = newHTable(size);

  t->copy_symbol = copyOperatorSymbol;
  t->free_symbol = freeOperatorSymbol;

  return t;
}


		 /*******************************
		 *	DEFINING OPERATORS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define operator in some table.  See   discussion  above for the relevant
tables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
defOperator(Module m, atom_t name, int type, int priority, int force)
{ GET_LD
  operator *op;
  int t = (type & OP_MASK);		/* OP_PREFIX, ... */
  int must_reg = FALSE;

  DEBUG(7, Sdprintf(":- op(%d, %s, %s) in module %s\n",
		    priority,
		    PL_atom_chars(operatorTypeToAtom(type)),
		    PL_atom_chars(name),
		    PL_atom_chars(m->name)));

  assert(t>=OP_PREFIX && t<=OP_POSTFIX);

  if ( !force && !SYSTEM_MODE )
  { if ( name == ATOM_comma ||
	 (name == ATOM_bar && ((t&OP_MASK) != OP_INFIX ||
			       (priority < 1001 && priority != 0))) )
    { GET_LD
      atom_t action = (name == ATOM_comma ? ATOM_modify : ATOM_create);
      term_t t = PL_new_term_ref();

      PL_put_atom(t, name);
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      action, ATOM_operator, t);
    }
  }


  PL_LOCK(L_OP);
  if ( !m->operators )
    m->operators = newOperatorTable(8);

  if ( (op = lookupHTable(m->operators, (void *)name)) )
  { ;
  } else if ( priority < 0 )
  { PL_UNLOCK(L_OP);				/* already inherited: do not change */
    return TRUE;
  } else
  { op = allocHeapOrHalt(sizeof(*op));

    op->priority[OP_PREFIX]  = -1;
    op->priority[OP_INFIX]   = -1;
    op->priority[OP_POSTFIX] = -1;
    op->type[OP_PREFIX]      = OP_INHERIT;
    op->type[OP_INFIX]       = OP_INHERIT;
    op->type[OP_POSTFIX]     = OP_INHERIT;

    must_reg = TRUE;
  }

  op->priority[t] = priority;
  op->type[t]     = (priority >= 0 ? type : OP_INHERIT);
  if ( must_reg )
  { PL_register_atom(name);
    addNewHTable(m->operators, (void *)name, op);
  }
  PL_UNLOCK(L_OP);

  return TRUE;
}


		 /*******************************
		 *	  QUERY OPERATORS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check whether an atom is defined as an   operator  in the context of the
current thread and provided module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static operator *
visibleOperator(Module m, atom_t name, int kind)
{ GET_LD
  operator *op;
  ListCell c;

  if ( m->operators &&
       (op = lookupHTable(m->operators, (void *)name)) )
  { if ( op->type[kind] != OP_INHERIT )
      return op;
  }
  for(c = m->supers; c; c=c->next)
  { if ( (op = visibleOperator(c->value, name, kind)) )
      return op;
  }

  return NULL;
}


int
currentOperator(Module m, atom_t name, int kind, int *type, int *priority)
{ operator *op;

  assert(kind >= OP_PREFIX && kind <= OP_POSTFIX);

  if ( !m )
    m = MODULE_user;

  if ( (op = visibleOperator(m, name, kind)) )
  { if ( op->priority[kind] > 0 )
    { *type     = op->type[kind];
      *priority = op->priority[kind];

      DEBUG(MSG_OPERATOR,
	    Sdprintf("currentOperator(%s) --> %s %d\n",
		     PL_atom_chars(name),
		     PL_atom_chars(operatorTypeToAtom(*type)),
		     *priority));

      succeed;
    }
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the priority of the atom as   operator ignoring its type. Used
by write/1 to determine whether or not braces are required. If more then
one priority is defined, use the highest.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
maxOp(operator *op, int *done, int sofar)
{ int i;

  for(i=0; i<3; i++)
  { if ( !(*done & (1<<i))  && op->type[i] != OP_INHERIT )
    { if ( op->priority[i] > sofar )
	sofar = op->priority[i];
      *done |= (1<<i);
    }
  }

  return sofar;
}


static int
scanPriorityOperator(Module m, atom_t name, int *done, int sofar)
{ GET_LD
  if ( *done != 0x7 )
  { operator *op;

    if ( m->operators && (op = lookupHTable(m->operators, (void *)name)) )
      sofar = maxOp(op, done, sofar);

    if ( *done != 0x7 )
    { ListCell c;

      for(c=m->supers; c; c=c->next)
	sofar = scanPriorityOperator(c->value, name, done, sofar);
    }
  }

  return sofar;
}


int
priorityOperator(Module m, atom_t name)
{ int done = 0;

  if ( !m )
    m = MODULE_user;

  return scanPriorityOperator(m, name, &done, 0);
}

#undef LD
#define LD LOCAL_LD

		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

static int
atomToOperatorType(atom_t atom)
{ if (atom == ATOM_fx)			return OP_FX;
  else if (atom == ATOM_fy)		return OP_FY;
  else if (atom == ATOM_xfx)		return OP_XFX;
  else if (atom == ATOM_xfy)		return OP_XFY;
  else if (atom == ATOM_yfx)		return OP_YFX;
  else if (atom == ATOM_yf)		return OP_YF;
  else if (atom == ATOM_xf)		return OP_XF;

  return 0;
}

static atom_t
operatorTypeToAtom(int type)
{ static const atom_t opnames[] =
  { NULL_ATOM,
    ATOM_fx, ATOM_fy,
    ATOM_xf, ATOM_yf,
    ATOM_xfx, ATOM_xfy, ATOM_yfx
  };

  return opnames[(type>>4)];
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define an operator from Prolog (op/3). The  current version is fully ISO
compliant for all exception cases.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("op", 3, op, PL_FA_TRANSPARENT|PL_FA_ISO)
{ PRED_LD
  atom_t nm;
  atom_t tp;
  int t;
  int p;
  Module m = MODULE_parse;

  term_t pri = A1;
  term_t type = A2;
  term_t name = A3;

  if ( !PL_strip_module(name, &m, name) )
    return FALSE;
  if ( m == MODULE_system )
  { term_t t = PL_new_term_ref();
    term_t a = PL_new_term_ref();

    PL_put_atom(a, m->name);
    return (PL_cons_functor(t, FUNCTOR_colon2, a, name) &&
	    PL_error(NULL, 0, "system operators are protected",
		     ERR_PERMISSION, ATOM_redefine, ATOM_operator,
		     t));
  }

  if ( !PL_get_atom_ex(type, &tp) )
    fail;
  if ( !PL_get_integer_ex(pri, &p) )
    fail;
  if ( !((p >= 0 && p <= OP_MAXPRIORITY) || (p == -1 && m != MODULE_user)) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_operator_priority, pri);
  if ( !(t = atomToOperatorType(tp)) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_operator_specifier, type);

  if ( PL_get_atom(name, &nm) )
  { return defOperator(m, nm, t, p, FALSE);
  } else
  { term_t l = PL_copy_term_ref(name);
    term_t e = PL_new_term_ref();

    while( PL_get_list(l, e, l) )
    { if ( !PL_get_atom(e, &nm) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, e);
      if ( !defOperator(m, nm, t, p, FALSE) )
	return FALSE;
    }
    if ( !PL_get_nil(l) )
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, l);
  }

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
current_op is a bit hard with   the distributed operator tables (thread,
module and global). The simplest solution is   probably to create a list
of matching operators and (on backtracking) return the matching ones.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
addOpToBuffer(Buffer b, atom_t name, int type, int priority)
{ opdef *op = baseBuffer(b, opdef);
  int mx    = (int)entriesBuffer(b, opdef);
  int i;
  opdef new;

  for(i = 0; i<mx; op++, i++)
  { if ( op->name == name && (op->type&OP_MASK) == (type&OP_MASK) )
      return;				/* got this one already */
  }

  new.name = name;
  new.type = type;
  new.priority = priority;
  addBuffer(b, new, opdef);
}


static void
addOpsFromTable(Table t, atom_t name, int priority, int type, Buffer b)
{ TableEnum e = newTableEnum(t);
  atom_t nm;
  operator *op;

  while( advanceTableEnum(e, (void**)&nm, (void**)&op) )
  { if ( nm == name || name == NULL_ATOM )
    { if ( type )
      { int kind = type&OP_MASK;

	assert(kind >= OP_PREFIX && kind <= OP_POSTFIX);

	if ( op->priority[kind] < 0 ||
	     (op->type[kind]&OP_MASK) != (type&OP_MASK) )
	  continue;

	if ( !priority ||
	     op->priority[kind] == priority ||
	     op->priority[kind] == 0 )
	  addOpToBuffer(b, nm, op->type[kind], op->priority[kind]);
      } else
      { int kind;

	for(kind = OP_PREFIX; kind <= OP_POSTFIX; kind++)
	{ if ( op->priority[kind] < 0 )
	    continue;

	  if ( !priority ||
	     op->priority[kind] == priority ||
	     op->priority[kind] == 0 )
	    addOpToBuffer(b, nm, op->type[kind], op->priority[kind]);
	}
      }
    }
  }

  freeTableEnum(e);
}


static void
scanVisibleOperators(Module m, atom_t name, int priority, int type, Buffer b,
		     int inherit)
{ if ( m->operators )
    addOpsFromTable(m->operators, name, priority, type, b);

  if ( inherit )
  { ListCell c;

    for(c=m->supers; c; c=c->next)
      scanVisibleOperators(c->value, name, priority, type, b, inherit);
  }
}


typedef struct
{ buffer buffer;
  int    index;
} op_enum;


static word
current_op(Module m, int inherit,
	   term_t prec, term_t type, term_t name,
	   control_t h ARG_LD)
{ op_enum *e;
  Buffer b;
  int mx;
  opdef *match;
  fid_t fid;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { atom_t a, nm;			/* NULL_ATOM: any name */
      int p;				/* 0: any priority */
      int t;				/* 0: any type */

      if ( PL_is_variable(name) )
	nm = NULL_ATOM;
      else if ( !PL_get_atom_ex(name, &nm) )
	return FALSE;

      if ( PL_is_variable(prec) )
      { p = 0;
      } else if ( PL_get_integer_ex(prec, &p) )
      { if ( !(p > 0 && p <= 1200) )
	  return PL_domain_error("operator_priority", prec);
      } else
      { return FALSE;
      }

      if ( PL_is_variable(type) )
	t = 0;
      else if ( PL_get_atom_ex(type, &a) )
      { if ( !(t = atomToOperatorType(a)) )
	  return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			  ATOM_operator_specifier, type);
      } else
	return FALSE;

      e = allocHeapOrHalt(sizeof(*e));
      b = &e->buffer;
      initBuffer(b);
      e->index = 0;

      scanVisibleOperators(m, nm, p, t, b, inherit);
      break;
    }
    case FRG_REDO:
    { e = ForeignContextPtr(h);
      b = &e->buffer;
      break;
    }
    case FRG_CUTTED:
    { e = ForeignContextPtr(h);

      if ( e )
      { discardBuffer(&e->buffer);
	freeHeap(e, sizeof(*e));
      }

      succeed;
    }
    default:
      assert(0);
      fail;				/*NOTREACHED*/
  }

  fid = PL_open_foreign_frame();
  mx = (int)entriesBuffer(b, opdef);
  match = baseBuffer(b, opdef) + e->index;
  for(; e->index++<mx; match++)
  { if ( match->priority == 0 )		/* canceled operator */
      continue;

    if ( PL_unify_atom(name, match->name) &&
	 PL_unify_integer(prec, match->priority) &&
	 PL_unify_atom(type, operatorTypeToAtom(match->type)) )
    { if ( e->index == mx )
      { discardBuffer(&e->buffer);
        freeHeap(e, sizeof(*e));
	return TRUE;
      }
      ForeignRedoPtr(e);
    }

    PL_rewind_foreign_frame(fid);
  }

  discardBuffer(&e->buffer);
  freeHeap(e, sizeof(*e));

  fail;
}

static int
get_op_module(term_t a3, term_t name, Module *m ARG_LD)
{ atom_t mname = 0;
  Word p;

  if ( !(p=stripModuleName(valTermRef(a3), &mname PASS_LD)) )
    return FALSE;
  *valTermRef(name) = linkVal(p);

  if ( mname && !(*m=isCurrentModule(mname)) )
    *m = MODULE_user;

  return TRUE;
}


static
PRED_IMPL("current_op", 3, current_op,
	  PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT|PL_FA_ISO)
{ PRED_LD
  Module m = MODULE_parse;
  term_t name = A3;

  if ( CTX_CNTRL != FRG_CUTTED )
  { if ( !(name = PL_new_term_ref()) ||
	 !get_op_module(A3, name, &m PASS_LD) )
      return FALSE;
  }

  return current_op(m, TRUE, A1, A2, name, PL__ctx PASS_LD);
}

/** '$local_op'(?Precedence, ?Type, ?Name) is nondet.

Same as curent_op/3, but  only  operators   defined  at  the  module are
reported and not the operators that  are   inherited.  This  is used for
additional  reflexive  capabilities,   such    as   save_operators/1  in
library(qsave).
*/

static
PRED_IMPL("$local_op", 3, local_op, PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = MODULE_user;
  term_t name = A3;

  if ( CTX_CNTRL != FRG_CUTTED )
  { if ( !(name = PL_new_term_ref()) ||
	 !get_op_module(A3, name, &m PASS_LD) )
      return FALSE;
  }

  return current_op(m, FALSE, A1, A2, name, PL__ctx PASS_LD);
}


		 /*******************************
		 *     INITIALISE OPERATORS	*
		 *******************************/

#define OP(a, t, p) { a, t, p }

static const opdef operators[] = {
  OP(ATOM_star,			 OP_YFX, 400),	/* * */
  OP(ATOM_plus,			 OP_FY,	 200),	/* + */
  OP(ATOM_plus,			 OP_YFX, 500),
  OP(ATOM_comma,		 OP_XFY, 1000),	/* , */
  OP(ATOM_minus,		 OP_FY,	 200),	/* - */
  OP(ATOM_minus,		 OP_YFX, 500),
  OP(ATOM_grammar,		 OP_XFX, 1200),	/* --> */
  OP(ATOM_ifthen,		 OP_XFY, 1050),	/* -> */
  OP(ATOM_softcut,		 OP_XFY, 1050),	/* *-> */
  OP(ATOM_divide,		 OP_YFX, 400),	/* / */
  OP(ATOM_gdiv,			 OP_YFX, 400),	/* // */
  OP(ATOM_div,			 OP_YFX, 400),	/* div */
  OP(ATOM_rdiv,			 OP_YFX, 400),	/* rdiv */
  OP(ATOM_and,			 OP_YFX, 500),	/* /\ */
  OP(ATOM_colon,		 OP_XFY, 600),	/* : */
  OP(ATOM_prove,		 OP_FX,	 1200),	/* :- */
  OP(ATOM_prove,		 OP_XFX, 1200),
  OP(ATOM_semicolon,		 OP_XFY, 1100),	/* ; */
  OP(ATOM_bar,			 OP_XFY, 1105),	/* | */
  OP(ATOM_smaller,		 OP_XFX, 700),	/* < */
  OP(ATOM_lshift,		 OP_YFX, 400),	/* << */
  OP(ATOM_equals,		 OP_XFX, 700),	/* = */
  OP(ATOM_univ,			 OP_XFX, 700),	/* =.. */
  OP(ATOM_ar_equals,		 OP_XFX, 700),	/* =:= */
  OP(ATOM_smaller_equal,	 OP_XFX, 700),	/* =< */
  OP(ATOM_larger_equal,		 OP_XFX, 700),	/* >= */
  OP(ATOM_strict_equal,		 OP_XFX, 700),	/* == */
  OP(ATOM_ar_not_equal,		 OP_XFX, 700),	/* =\= */
  OP(ATOM_dict_punify,		 OP_XFX, 700),	/* >:< */
  OP(ATOM_dict_select,		 OP_XFX, 700),	/* :< */
  OP(ATOM_larger,		 OP_XFX, 700),	/* > */
  OP(ATOM_rshift,		 OP_YFX, 400),	/* >> */
  OP(ATOM_query,		 OP_FX,	 1200),	/* ?- */
  OP(ATOM_at_smaller,		 OP_XFX, 700),	/* @< */
  OP(ATOM_at_smaller_eq,	 OP_XFX, 700),	/* @=< */
  OP(ATOM_at_larger,		 OP_XFX, 700),	/* @> */
  OP(ATOM_at_larger_eq,		 OP_XFX, 700),	/* @>= */
  OP(ATOM_backslash,		 OP_FY,	 200),	/* \ */
  OP(ATOM_not_provable,		 OP_FY,	 900),	/* \+ */
  OP(ATOM_bitor,		 OP_YFX, 500),	/* \/ */
  OP(ATOM_not_equals,		 OP_XFX, 700),	/* \= */
  OP(ATOM_not_strict_equal,	 OP_XFX, 700),	/* \== */
  OP(ATOM_at_equals,		 OP_XFX, 700),	/* =@= */
  OP(ATOM_at_not_equals,	 OP_XFX, 700),	/* \=@= */
  OP(ATOM_hat,			 OP_XFY, 200),	/* ^ */
  OP(ATOM_doublestar,		 OP_XFX, 200),	/* ** */
  OP(ATOM_discontiguous,	 OP_FX,	 1150),	/* discontiguous */
  OP(ATOM_dynamic,		 OP_FX,	 1150),	/* dynamic */
  OP(ATOM_volatile,		 OP_FX,	 1150),	/* volatile */
  OP(ATOM_thread_local,		 OP_FX,	 1150),	/* thread_local */
  OP(ATOM_initialization,	 OP_FX,	 1150),	/* initialization */
  OP(ATOM_thread_initialization, OP_FX,	 1150),	/* thread_initialization */
  OP(ATOM_is,			 OP_XFX, 700),	/* is */
  OP(ATOM_as,			 OP_XFX, 700),	/* as */
  OP(ATOM_mod,			 OP_YFX, 400),	/* mod */
  OP(ATOM_rem,			 OP_YFX, 400),	/* rem */
  OP(ATOM_module_transparent,	 OP_FX,	 1150),	/* module_transparent */
  OP(ATOM_multifile,		 OP_FX,	 1150),	/* multifile */
  OP(ATOM_meta_predicate,	 OP_FX,	 1150),	/* meta_predicate */
  OP(ATOM_public,		 OP_FX,	 1150),	/* public */
  OP(ATOM_xor,			 OP_YFX, 400),	/* xor */

  OP(NULL_ATOM,			 0,	 0)
};


void
initOperators(void)
{ const opdef *op;

  MODULE_system->operators = newOperatorTable(32);

  for( op = operators; op->name; op++ )
    defOperator(MODULE_system, op->name, op->type, op->priority, TRUE);

  if ( !GD->options.traditional )
  { defOperator(MODULE_system, PL_new_atom("."), OP_YFX, 100, TRUE);
    defOperator(MODULE_system, ATOM_colon_eq,    OP_XFX, 800, TRUE);
  }
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(op)
  PRED_DEF("op", 3, op, PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("current_op", 3, current_op, PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT|PL_FA_ISO)
  PRED_DEF("$local_op", 3, local_op, PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT)
EndPredDefs
