/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Virtual machine instruction interpreter
*/

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"

#if sun
#include <prof.h>			/* in-function profiling */
#else
#define MARK(label)
#endif

forwards inline bool	callForeign(const Definition, LocalFrame);
forwards void		leaveForeignFrame(LocalFrame);

#if COUNTING

forwards void	countHeader(void);
forwards void	countArray(char *, int *);
forwards void	countOne(char *, int);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The counting code has been added while investigating the  time  critical
WAM  instructions.   I'm afraid it has not been updated correctly since.
Please  check  the  various  counting  macros  and  their  usage  before
including this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
struct
{ int i_nop;
  int h_const_n[256];
  int b_const_n[256];
  int h_sint[256];
  int b_sint[256];
  int h_nil;
  int h_var_n[256];
  int b_var_n[256];
  int b_argvar_n[256];
  int h_firstvar_n[256];
  int b_firstvar_n[256];
  int b_argfirstvar_n[256];
  int h_void;
  int b_void;
  int h_functor;
  int h_list;
  int b_functor;
  int i_pop;
  int i_pop_n[256];
  int i_enter;
#if O_BLOCK
  int i_cut_block;
  int b_exit;
#endif
  int i_cut;
  int i_usercall0;
  int i_usercalln[256];
  int i_apply;
  int i_depart;
  int i_call;
  int i_exit;
  int i_exitfact;
  int d_break;
#if O_COMPILE_ARITH
  int a_indirect;
  int a_func0[256];
  int a_func1[256];
  int a_func2[256];
  int a_func[256];
  int a_lt;
  int a_le;
  int a_gt;
  int a_ge;
  int a_eq;
  int a_ne;
  int a_is;
#endif /* O_COMPILE_ARITH */
#if O_COMPILE_OR
  int c_or[256];
  int c_jmp[256];
  int c_mark[256];
  int c_cut[256];
  int c_ifthenelse[512];
  int c_fail;
  int c_end;
#endif /* O_COMPILE_OR */
  int i_fail;
  int i_true;
} counting;

forwards void countHeader();
forwards void countOne();
forwards void countArray();

word
pl_count()
{ countHeader();
  countArray("H_CONST", 	counting.h_const_n);  
  countArray("B_CONST", 	counting.b_const_n);  
  countArray("B_INDIRECT",	counting.b_indirect_n);  
  countOne(  "H_NIL", 		counting.h_nil);
  countArray("H_VAR", 		counting.h_var_n);  
  countArray("B_VAR", 		counting.b_var_n);  
  countArray("B_ARGVAR", 	counting.b_argvar_n);  
  countArray("H_FIRSTVAR", 	counting.h_firstvar_n);  
  countArray("B_FIRSTVAR", 	counting.b_firstvar_n);  
  countArray("B_ARGFIRSTVAR", 	counting.b_argfirstvar_n);  
  countOne(  "H_VOID", 		counting.h_void);
  countOne(  "B_VOID", 		counting.b_void);
  countOne(  "H_FUNCTOR", 	counting.h_functor_n);  
  countOne(  "H_LIST", 		counting.h_list);  
  countOne(  "B_FUNCTOR", 	counting.b_functor_n);  
  countOne(  "I_POPF", 		counting.i_pop);
  countOne(  "I_ENTER", 	counting.i_enter);
  countOne(  "I_CONTEXT", 	counting.i_context);
#if O_BLOCK
  countOne(  "I_CUT_BLOCK",	counting.i_cut_block);
  countOne(  "B_EXIT",		counting.b_exit);
#endif
  countOne(  "I_CUT", 		counting.i_cut);
  countOne(  "I_USERCALL0", 	counting.i_usercall0);
  countArray("I_USERCALLN",	counting.i_usercalln);
  countOne(  "I_APPLY", 	counting.i_apply);
  countOne(  "I_DEPART", 	counting.i_depart);
  countOne(  "I_CALL", 		counting.i_call);
  countOne(  "I_EXIT", 		counting.i_exit);
  countOne(  "I_EXITFACT",	counting.i_exitfact);
  countOne(  "D_BREAK",		countOne.d_break);
  countOne(  "I_FAIL",		countOne.i_fail);
  countOne(  "I_TRUE",		countOne.i_true);

  succeed;
}

static void
countHeader()
{ int m;

  Sdprintf("%13s: ", "Instruction");
  for(m=0; m < 20; m++)
    Sdprintf("%8d", m);
  Sdprintf("\n");
  for(m=0; m<(15+20*8); m++)
    Sdprintf("=");
  Sdprintf("\n");
}  

static void
countArray(char *s, int *array)
{ int n, m;

  for(n=255; array[n] == 0; n--) ;
  Sdprintf("%13s: ", s);
  for(m=0; m <= n; m++)
    Sdprintf("%8d", array[m]);
  Sdprintf("\n");
}

static void
countOne(char *s, int i)
{ Sdprintf("%13s: %8d\n", s, i);
}

#define COUNT_N(name)  { counting.name[*PC]++; }
#define COUNT_2N(name) { counting.name[*PC]++; counting.name[PC[1]+256]++; }
#define COUNT(name)    { counting.name++; }
#else /* ~COUNTING */
#define COUNT_N(name)
#define COUNT_2N(name)
#define COUNT(name)
#endif /* COUNTING */


#include "pl-alloc.c"
#include "pl-index.c"

		 /*******************************
		 *	    ASYNC HOOKS		*
		 *******************************/

#if O_ASYNC_HOOK

static struct
{ PL_async_hook_t	hook;		/* the hook function */
  unsigned int		mask;		/* the mask */
} async;


PL_async_hook_t
PL_async_hook(unsigned int count, PL_async_hook_t hook)
{ PL_async_hook_t old = async.hook;

  async.hook = hook;
  async.mask = 1;
  while(async.mask < count)
    async.mask <<= 1;
  async.mask--;

  return old;
}


#endif /*O_ASYNC_HOOK*/

		 /*******************************
		 *	     SIGNALS		*
		 *******************************/

#if 0 /*def O_SAFE_SIGNALS*/

static inline int
is_signalled()
{ sigset_t set;

  sigpending(&set);

  return set != 0;			/* non-portable! */
}

#else

#define is_signalled() (LD->pending_signals != 0)

#endif


		 /*******************************
		 *	   STACK-LAYOUT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Brief description of the local stack-layout.  This stack contains:

	* struct localFrame structures for the Prolog stackframes.
	* argument vectors and local variables for Prolog goals.
	* term-references for foreign code.  The layout:


	lTop  -->| first free location |
		 -----------------------
		 | local variables     |
		 |        ...	       |
		 | arguments for goal  |
		 | localFrame struct   |
		 | queryFrame struct   |
		 -----------------------
		 |        ...	       |
		 | term-references     |
		 -----------------------
	lBase -->| # fliFrame struct   |
		 -----------------------

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	    FOREIGN FRAME	*
		 *******************************/

#undef LD
#define LD LOCAL_LD

void
finish_foreign_frame(ARG1_LD)
{ if ( fli_context )
  { FliFrame fr = fli_context;

    if ( (unsigned long)environment_frame < (unsigned long) fr )
    { fr->size = (Word) lTop - (Word)addPointer(fr, sizeof(struct fliFrame));
      DEBUG(9, Sdprintf("Pushed fli context with %d term-refs\n", fr->size));
    }
  }
}


fid_t
PL_open_foreign_frame()
{ GET_LD
  FliFrame fr = (FliFrame) lTop;

  finish_foreign_frame(PASS_LD1);
  requireStack(local, sizeof(struct fliFrame));
  lTop = addPointer(lTop, sizeof(struct fliFrame));
  fr->size = 0;
  Mark(fr->mark);
  fr->parent = fli_context;
  fli_context = fr;

  return consTermRef(fr);
}


void
PL_close_foreign_frame(fid_t id)
{ GET_LD
  FliFrame fr = (FliFrame) valTermRef(id);

  fli_context = fr->parent;
  lTop = (LocalFrame) fr;
}


void
PL_rewind_foreign_frame(fid_t id)
{ GET_LD
  FliFrame fr = (FliFrame) valTermRef(id);

  Undo(fr->mark);
  lTop = addPointer(fr, sizeof(struct fliFrame));
  fr->size = 0;
}


void
PL_discard_foreign_frame(fid_t id)
{ GET_LD
  FliFrame fr = (FliFrame) valTermRef(id);

  Undo(fr->mark);
  fli_context = fr->parent;
  lTop = (LocalFrame) fr;
}

		/********************************
		*         FOREIGN CALLS         *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Calling foreign predicates.  We will have to  set  `lTop',  compose  the
argument  vector  for  the  foreign  function,  call  it and analyse the
result.  The arguments of the frame are derefenced  here  to  avoid  the
need for explicit dereferencing in most foreign predicates themselves.

A foreign predicate can  return  either  the  constant  FALSE  to  start
backtracking,  TRUE to indicate success without alternatives or anything
else.  The return value is saved in the `clause' slot of the frame.   In
this  case  the  interpreter  will  leave a backtrack point and call the
foreign function again with  the  saved  value  as  `backtrack  control'
argument  if  backtracking is needed.  This `backtrack control' argument
is appended to the argument list normally given to the foreign function.
This makes it possible for  foreign  functions  that  do  not  use  this
mechanism  to  ignore it.  For the first call the constant FIRST_CALL is
given as `backtrack control'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_FLI_ARGS 10			/* extend switches on change */

#define CALLDETFN(r, argc) \
  { switch(argc) \
    { case 0: \
	r = F(); \
        break; \
      case 1: \
	r = F(A(0)); \
	break; \
      case 2: \
	r = F(A(0),A(1)); \
        break; \
      case 3: \
	r = F(A(0),A(1),A(2)); \
        break; \
      case 4: \
	r = F(A(0),A(1),A(2),A(3)); \
        break; \
      case 5: \
	r = F(A(0),A(1),A(2),A(3),A(4)); \
        break; \
      case 6: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5)); \
        break; \
      case 7: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6)); \
        break; \
      case 8: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7)); \
        break; \
      case 9: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8)); \
        break; \
      case 10: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9)); \
        break; \
      default: \
	r = sysError("Too many arguments to foreign function (>%d)", \
		     MAX_FLI_ARGS); \
    } \
  }

#define CALLNDETFN(r, argc, c) \
  { switch(argc) \
    { case 0: \
	r = F(c); \
        break; \
      case 1: \
	r = F(A(0),(c)); \
	break; \
      case 2: \
	r = F(A(0),A(1),(c)); \
        break; \
      case 3: \
	r = F(A(0),A(1),A(2),(c)); \
        break; \
      case 4: \
	r = F(A(0),A(1),A(2),A(3),(c)); \
        break; \
      case 5: \
	r = F(A(0),A(1),A(2),A(3),A(4),(c)); \
        break; \
      case 6: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),(c)); \
        break; \
      case 7: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),(c)); \
        break; \
      case 8: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),(c)); \
        break; \
      case 9: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),(c)); \
        break; \
      case 10: \
	r = F(A(0),A(1),A(2),A(3),A(4),A(5),A(6),A(7),A(8),A(9),(c)); \
        break; \
      default: \
	r = sysError("Too many arguments to foreign function (>%d)", \
		     MAX_FLI_ARGS); \
    } \
  }


static inline bool
callForeign(const Definition def, LocalFrame frame)
{ GET_LD
  Func function = def->definition.function;
  int argc = def->functor->arity;
  word result;
  term_t h0 = argFrameP(frame, 0) - (Word)lBase;
  fid_t cid;
  SaveLocalPtr(s1, frame);
  
  lTop = (LocalFrame) argFrameP(frame, argc);
  cid  = PL_open_foreign_frame();
  exception_term = 0;

  SECURE({ int n;
	   Word p0 = argFrameP(frame, 0);
	   
	   for(n=0; n<argc; n++)
	     checkData(p0+n);
	 });
  SECURE(checkStacks(frame));

#define F (*function)    
  if ( true(def, P_VARARG) )
  { result = F(h0, argc, (word) frame->clause);
  } else
#define A(n) (h0+n)
  { if ( false(def, NONDETERMINISTIC) )	/* deterministic */
    { CALLDETFN(result, argc);
    } else				/* non-deterministic */
    { word context = (word) frame->clause;
      CALLNDETFN(result, argc, context);
    }
#undef A
  }
#undef F

  PL_close_foreign_frame(cid);		/* invalidates exception_term! */
  RestoreLocalPtr(s1, frame);

  SECURE({ int n;
	   Word p0 = argFrameP(frame, 0);

	   for(n=0; n<argc; n++)
	     checkData(p0+n);
	 });
  SECURE(checkStacks(frame));

  if ( result <= 1 )			/* FALSE || TRUE */
  { frame->clause = NULL;

    if ( exception_term && result == 1 ) /* False alarm */
    { exception_term = 0;
      setVar(*valTermRef(exception_bin));
    }

    return (bool) result;
  } else
  { if ( true(def, NONDETERMINISTIC) )
    { assert(result & FRG_CONTROL_MASK);
      frame->clause = (ClauseRef) result;
      succeed;
    } else
    { FunctorDef fd = def->functor;
      term_t ex = PL_new_term_ref();

      PL_put_integer(ex, result);

      return PL_error(stringAtom(fd->name), fd->arity, NULL, ERR_DOMAIN,
		      ATOM_foreign_return_value, ex);
    }
  }
}


static void
leaveForeignFrame(LocalFrame fr)
{ Definition def = fr->predicate;
  int argc       = def->functor->arity;
  Func function  = def->definition.function;
  word context   = ((word) fr->clause & ~FRG_CONTROL_MASK) | FRG_CUTTED;
  int  result;

#define F	(*function)
#define A(n)	((Word)NULL)

  DEBUG(5, Sdprintf("\tCut %s, context = 0x%lx\n",
		    predicateName(def), context));

  CALLNDETFN(result, argc, context);
#undef A
#undef F
}

#if O_DEBUGGER
static void
frameFinished(LocalFrame fr)
{ fid_t cid = PL_open_foreign_frame();

  callEventHook(PLEV_FRAMEFINISHED, fr);

  PL_discard_foreign_frame(cid);
}
#endif

		 /*******************************
		 *	     TRAILING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trail an assignment.  This function  is  now   local  to  this module to
exploit inlining facilities provided  by   good  C-compilers.  Note that
-when using dynamic stacks-, the  assignment   should  be  made *before*
calling Trail()!

The first version of Trail() is used only  by the WAM interpreter and is
so much simpler because it is *known* that   p is either on the local or
global stack and fr is available.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define Trail(p, fr) \
  if ( p >= (Word)lBase || !fr || p < fr->mark.globaltop ) \
  { requireStack(trail, sizeof(struct trail_entry)); \
    (tTop++)->address = p; \
  }

void
DoTrail(Word p)
{ GET_LD
  Trail(p, environment_frame);
}


#ifdef O_DESTRUCTIVE_ASSIGNMENT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trailing of destructive assignments.  This feature is used by setarg/3.

Such an assignment is trailed by first  pushing the assigned address (as
normal) and then pushing a marked pointer to  a cell on the global stack
holding the old (overwritten) value.

Undo is slightly more complicated as it has to check for these special
cells on the trailstack.

The garbage collector has to take care in  a number of places: it has to
pass through the trail-stack, marking   the  global-stack references for
assigned data and the sweep_trail() must be   careful about this type of
marks.

Note this function doesn't call Trail() for   the address as it can only
be called from setarg/3 and the argument  is thus always a term-argument
on the global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
TrailAssignment(Word p)
{ GET_LD
  Word old = allocGlobal(1);

  *old = *p;				/* save the old value on the global */
  requireStack(trail, 2*sizeof(struct trail_entry));
  (tTop++)->address = p;
  (tTop++)->address = tagTrailPtr(old);
}


static inline void
__do_undo(mark *m ARG_LD)
{ TrailEntry tt = tTop;
  TrailEntry mt = m->trailtop;

  SECURE(assert(m->trailtop  != INVALID_TRAILTOP);
	 assert(m->globaltop != INVALID_GLOBALTOP));

  while(--tt >= mt)
  { Word p = tt->address;

    if ( isTrailVal(p) )
    { DEBUG(2, Sdprintf("Undoing a trailed assignment\n"));
      *(--tt)->address = trailVal(p);
    } else
      setVar(*p);
  }

  tTop = mt;
  gTop = m->globaltop;
}


void
do_undo(mark *m)
{ GET_LD
  __do_undo(m PASS_LD);
}

#undef Undo
#define Undo(m) __do_undo(&m PASS_LD)
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/

		/********************************
		*          UNIFICATION          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify is the general unification procedure. This raw routine should only
be called by interpret as it  does   not  undo  bindings made during the
unification in case the unification fails. pl_unify() (implementing =/2)
does undo bindings and should be used   by  foreign predicates. See also
unify_ptrs().

Unification depends on the datatypes available in the system and will in
general need updating if new types are added.  It should be  noted  that
unify()  is  not  the only place were unification happens.  Other points
are:

  - various of the virtual machine instructions
  - various macros, for example APPENDLIST and CLOSELIST
  - unifyAtomic(): unification of atomic data.
  - various builtin predicates. They should be flagged some way.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
unify(Word t1, Word t2 ARG_LD)
{ 
  word w1;
  word w2;

right_recursion:
  w1 = *t1;
  w2 = *t2;

  while(isRef(w1))			/* this is deRef() */
  { t1 = unRef(w1);
    w1 = *t1;
  }
  while(isRef(w2))
  { t2 = unRef(w2);
    w2 = *t2;
  }

  if ( isVar(w1) )
  { if ( isVar(w2) )
    { if ( t1 < t2 )			/* always point downwards */
      { *t2 = makeRef(t1);
	Trail(t2, LD->environment);
	succeed;
      }
      if ( t1 == t2 )
	succeed;
      *t1 = makeRef(t2);
      Trail(t1, LD->environment);
      succeed;
    }
    *t1 = w2;
    Trail(t1, LD->environment);
    succeed;
  }
  if ( isVar(w2) )
  { *t2 = w1;
    Trail(t2, LD->environment);
    succeed;
  }

  if ( w1 == w2 )
    succeed;
  if ( tag(w1) != tag(w2) )
    fail;

  switch(tag(w1))
  { case TAG_ATOM:
      fail;
    case TAG_INTEGER:
      if ( storage(w1) == STG_INLINE ||
	   storage(w2) == STG_INLINE )
	fail;
    case TAG_STRING:
    case TAG_FLOAT:
      return equalIndirect(w1, w2);
    case TAG_COMPOUND:
    { Functor f1 = valueTerm(w1);
      Functor f2 = valueTerm(w2);
      Word e;

      if ( f1->definition != f2->definition )
	fail;

      t1 = f1->arguments;
      t2 = f2->arguments;
      e  = t1+arityFunctor(f1->definition)-1; /* right-recurse on last */

      for(; t1 < e; t1++, t2++)
      { if ( !unify(t1, t2 PASS_LD) )
	  fail;
      }
      goto right_recursion;
    }
  }

  succeed;
}


word
pl_unify(term_t t1, term_t t2)		/* =/2 */
{ GET_LD
  Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);
  mark m;
  int rval;

  Mark(m);
  if ( !(rval = unify(p1, p2 PASS_LD)) )
    Undo(m);

  return rval;  
}


word
pl_notunify(term_t t1, term_t t2)	/* A \= B */
{ GET_LD
  Word p1    = valTermRef(t1);
  Word p2    = valTermRef(t2);

  return can_unify(p1, p2) ? FALSE : TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Public unification procedure for  `raw'  data.   See  also  unify()  and
PL_unify().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unify_ptrs(Word t1, Word t2)
{ GET_LD
  mark m;
  bool rval;

  Mark(m);
  if ( !(rval = unify(t1, t2 PASS_LD)) )
    Undo(m);

  return rval;  
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
can_unify(t1, t2) succeeds if  two  terms   *can*  be  unified,  without
actually doing so. This  is  basically   a  stripped  version of unify()
above. See this function for comments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
can_unify(Word t1, Word t2)
{ GET_LD
  mark m;
  bool rval;

  Mark(m);
  rval = unify(t1, t2 PASS_LD);
  Undo(m);

  return rval;  
}

		 /*******************************
		 *	   OCCURS-CHECK		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int var_occurs_in(Word v, Word t)
    Succeeds of the term `v' occurs in `t'.  v must be dereferenced on
    entry.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
var_occurs_in(Word v, Word t)
{ GET_LD

right_recursion:
  deRef(t);
  if ( v == t )
    succeed;

  if ( isTerm(*t) )
  { Functor f = valueTerm(*t);
    int arity = arityFunctor(f->definition);

    t = f->arguments;
    for( ; --arity > 0; t++)
    { if ( var_occurs_in(v, t) )
	succeed;
    }
    goto right_recursion;
  }

  fail;
}


static bool
unify_with_occurs_check(Word t1, Word t2, LocalFrame fr)
{ GET_LD
  word w1;
  word w2;

right_recursion:
  w1 = *t1;
  w2 = *t2;

  while(isRef(w1))			/* this is deRef() */
  { t1 = unRef(w1);
    w1 = *t1;
  }
  while(isRef(w2))
  { t2 = unRef(w2);
    w2 = *t2;
  }

  if ( isVar(w1) )
  { if ( isVar(w2) )
    { if ( t1 < t2 )			/* always point downwards */
      { *t2 = makeRef(t1);
	Trail(t2, fr);
	succeed;
      }
      if ( t1 == t2 )
	succeed;
      *t1 = makeRef(t2);
      Trail(t1, fr);
      succeed;
    }
    if ( var_occurs_in(t1, t2) )
      fail;
    *t1 = w2;
    Trail(t1, fr);
    succeed;
  }
  if ( isVar(w2) )
  { if ( var_occurs_in(t2, t1) )
      fail;

    *t2 = w1;
    Trail(t2, fr);
    succeed;
  }

  if ( w1 == w2 )
    succeed;
  if ( tag(w1) != tag(w2) )
    fail;

  switch(tag(w1))
  { case TAG_ATOM:
      fail;
    case TAG_INTEGER:
      if ( storage(w1) == STG_INLINE ||
	   storage(w2) == STG_INLINE )
	fail;
    case TAG_STRING:
    case TAG_FLOAT:
      return equalIndirect(w1, w2);
    case TAG_COMPOUND:
    { int arity;
      Functor f1 = valueTerm(w1);
      Functor f2 = valueTerm(w2);

      if ( f1->definition != f2->definition )
	fail;

      arity = arityFunctor(f1->definition);
      t1 = f1->arguments;
      t2 = f2->arguments;

      for(; --arity > 0; t1++, t2++)
      { if ( !unify_with_occurs_check(t1, t2, fr) )
	  fail;
      }
      goto right_recursion;
    }
  }

  succeed;
}


word
pl_unify_with_occurs_check(term_t t1, term_t t2)
{ GET_LD
  mark m;
  Word p1, p2;
  word rval;

  Mark(m);
  p1 = valTermRef(t1);
  p2 = valTermRef(t2);
  rval = unify_with_occurs_check(p1, p2, environment_frame);
  if ( !rval )
    Undo(m);

  return rval;
}


		 /*******************************
		 *   FOREIGN-LANGUAGE INTERFACE *
		 *******************************/

#include "pl-fli.c"

#if O_BLOCK
		/********************************
		*         BLOCK SUPPORT         *
		*********************************/

static LocalFrame
findBlock(LocalFrame fr, Word block)
{ for(; fr; fr = fr->parent)
  { if ( fr->predicate == PROCEDURE_block3->definition &&
	 unify_ptrs(argFrameP(fr, 0), block) )
      return fr;
  }

  PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_block, wordToTermRef(block));

  return NULL;
}

#endif /*O_BLOCK*/

#if O_CATCHTHROW
		/********************************
		*        EXCEPTION SUPPORT      *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the frame running catch/3. If we found  it, we will mark this frame
and not find it again, as a catcher   can  only catch once from the 1-st
argument goal. Exceptions from the  recover   goal  should be passed (to
avoid a loop and allow for re-throwing).   With  thanks from Gertjan van
Noord.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
findCatcher(LocalFrame fr, Word catcher)
{ Definition catch3 = PROCEDURE_catch3->definition;

  for(; fr; fr = fr->parent)
  { if ( fr->predicate == catch3 &&
	 false(fr, FR_CATCHED) &&
	 unify_ptrs(argFrameP(fr, 1), catcher) )
    { set(fr, FR_CATCHED);
      return fr;
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether some outer  environment  will   catch  this  exception. I.e.
catch(Goal, ...), where Goal calls C, calls   Prolog  and then raises an
exception somewhere.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef offset
#define offset(s, f) ((int)(&((struct s *)NULL)->f))
#endif

static int
isCatchedInOuterQuery(QueryFrame qf, Word catcher)
{ Definition catch3 = PROCEDURE_catch3->definition;

  while( qf && true(qf, PL_Q_PASS_EXCEPTION) )
  { LocalFrame fr = qf->saved_environment;

    while( fr )
    { if ( fr->predicate == catch3 && can_unify(argFrameP(fr, 1), catcher) )
	succeed;

      if ( fr->parent )
	fr = fr->parent;
      else
      { qf = (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
	break;
      }
    }

  }

  fail;
}


#endif /*O_CATCHTHROW*/

		 /*******************************
		 *	  TAIL-RECURSION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Tail recursion copy of the arguments of the new frame back into the  old
one.   This  should  be  optimised  by the compiler someday, but for the
moment this will do.

The new arguments block can contain the following types:
  - Instantiated data (atoms, ints, reals, strings, terms
    These can just be copied.
  - Plain variables
    These can just be copied.
  - References to frames older than the `to' frame
    These can just be copied.
  - 1-deep references into the `to' frame.
    This is hard as there might be two of  them  pointing  to  the  same
    location  in  the  `to' frame, indicating sharing variables.  In the
    first pass we will fill the  variable  in  the  `to'  frame  with  a
    reference  to the new variable.  If we get another reference to this
    field we will copy the reference saved in the `to'  field.   Because
    on  entry  references into this frame are always 1 deep we KNOW this
    is a saved reference.  The critical program for this is:

	a :- b(X, X).
	b(X, Y) :- X == Y.
	b(X, Y) :- write(bug), nl.

					This one costed me 1/10 bottle of
					brandy to Huub Knops, SWI
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
copyFrameArguments(LocalFrame from, LocalFrame to, int argc ARG_LD)
{ Word ARGD, ARGS, ARGE;

  if ( argc == 0 )
    return;

  ARGS = argFrameP(from, 0);
  ARGE = ARGS+argc;
  ARGD = argFrameP(to, 0);
  for( ; ARGS < ARGE; ARGS++, ARGD++) /* dereference the block */
  { word k = *ARGS;

    if ( isRefL(k) )
    { Word p = unRefL(k);

      if ( p > (Word)to )
      { if ( isVar(*p) )
	{ *p = makeRefL(ARGD);
	  setVar(*ARGS);
	} else
	  *ARGS = *p;
      }
    }
  }    
  ARGS = argFrameP(from, 0);
  ARGD = argFrameP(to, 0);
  while( ARGS < ARGE )			/* now copy them */
    *ARGD++ = *ARGS++;  
}

		/********************************
		*          INTERPRETER          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			 MACHINE REGISTERS

  - DEF
    Definition structure of current procedure.
  - PC
    Virtual machine `program counter': pointer to the next byte code  to
    interpret.
  - ARGP
    Argument pointer.  Pointer to the next argument to be matched  (when
    in the clause head) or next argument to be instantiated (when in the
    clause  body).   Saved  and  restored  via  the  argument  stack for
    functors.
  - FR
    Current environment frame
  - BFR
    Frame where execution should continue if  the  current  goal  fails.
    Used by I_CALL and deviates to fill the backtrackFrame slot of a new
    frame and set by various instructions.
  - deterministic
    Last clause has been found deterministically
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FRAME_FAILED		goto frame_failed
#define CLAUSE_FAILED		goto clause_failed
#define BODY_FAILED		goto body_failed

#ifndef O_SECURE
#define SetBfr(fr)		(BFR = (fr))
#else
#define SetBfr(fr) \
    do { \
         assert(!(fr) || (fr)->mark.trailtop != INVALID_TRAILTOP); \
         BFR = (fr); \
       } while(0)
#endif

#ifndef ulong
#define ulong unsigned long
#endif

qid_t
PL_open_query(Module ctx, int flags, Procedure proc, term_t args)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr;
  Definition def;
  int arity;
  Word ap;
  ClauseRef clause;

  DEBUG(4, { FunctorDef f = proc->definition->functor;

	     if ( LD->IO.output )
	     { int n;

	       Sdprintf("PL_open_query: %s(", stringAtom(f->name));
	       for(n=0; n < f->arity; n++)
	       { if ( n > 0 )
		   Sdprintf(", ");
		 PL_write_term(Serror, args+n, 999, 0);
	       }
	       Sdprintf(")\n");
	     } else
	       Sdprintf("PL_open_query in unitialized environment.\n");
	   });

					/* should be struct alignment, */
					/* but for now, I think this */
					/* is always the same */
#ifdef DOUBLE_ALIGNMENT
  while ( (ulong)lTop % DOUBLE_ALIGNMENT )
    lTop = addPointer(lTop, sizeof(word));
#endif

  qf	= (QueryFrame) lTop;
  fr    = &qf->frame;
  def   = proc->definition;
  arity	= def->functor->arity;

  SECURE(checkStacks(environment_frame));
  assert((ulong)fli_context > (ulong)environment_frame);
  assert((ulong)lTop >= (ulong)(fli_context+1));

  finish_foreign_frame(PASS_LD1);	/* adjust the size of the context */

  if ( flags == TRUE )			/* compatibility */
    flags = PL_Q_NORMAL;
  else if ( flags == FALSE )
    flags = PL_Q_NODEBUG;
  flags &= 0x1f;			/* mask reserved flags */

  qf->magic		= QID_MAGIC;
  qf->flags		= flags;
  qf->saved_environment = environment_frame;
  qf->saved_bfr		= LD->choicepoints;
  qf->aSave             = aTop;
  qf->solutions         = 0;
  qf->exception		= 0;

  lTop = (LocalFrame) argFrameP(fr, arity);
  verifyStack(local);

  fr->parent = NULL;
					/* fill frame arguments */
  ap = argFrameP(fr, 0);
  { int n;
    Word p = valTermRef(args);

    for( n = arity; n-- > 0; p++ )
      *ap++ = linkVal(p);
  }

					/* find definition and clause */
  if ( !(clause = def->definition.clauses) && false(def, PROC_DEFINED) )
  { def = trapUndefined(def);
    clause = def->definition.clauses;
  }
  if ( true(def, FOREIGN) )
  { fr->clause = FIRST_CALL;
  } else
  { fr->clause = clause;
  }
					/* context module */
  if ( true(def, METAPRED) )
  { if ( ctx )
      fr->context = ctx;
    else if ( environment_frame )
      fr->context = environment_frame->context;
    else
      fr->context = MODULE_user;
  } else
    fr->context = def->module;

  clearFlags(fr);
{ LocalFrame parent;
  long plevel;

  if ( (parent = parentFrame(fr)) )
    plevel = levelFrame(parent);
  else
    plevel = 0L;

  setLevelFrame(fr, plevel);
}
			
  DEBUG(3, Sdprintf("Level = %d\n", levelFrame(fr)));
  if ( true(qf, PL_Q_NODEBUG) )
  { set(fr, FR_NODEBUG);
    debugstatus.suspendTrace++;
    qf->debugSave = debugstatus.debugging;
    debugstatus.debugging = FALSE;
#ifdef O_LIMIT_DEPTH
    qf->saved_depth_limit   = depth_limit;
    qf->saved_depth_reached = depth_reached;
    depth_limit = (unsigned long)DEPTH_NO_LIMIT;
#endif
  }
  LD->choicepoints   = NULL;
  fr->backtrackFrame = NULL;
  fr->predicate      = def;
#ifdef O_LOGICAL_UPDATE
  fr->generation = GD->generation;
#endif
  Mark(fr->mark);
  environment_frame = fr;

  DEBUG(2, Sdprintf("QID=%d\n", QidFromQuery(qf)));

  return QidFromQuery(qf);
}


static void
discard_query(QueryFrame qf)
{ GET_LD
  LocalFrame FR  = &qf->frame;
  LocalFrame BFR = LD->choicepoints;
  LocalFrame fr, fr2;

  set(FR, FR_CUT);			/* execute I_CUT */
  for(fr = BFR; fr > FR; fr = fr->backtrackFrame)
  { for(fr2 = fr; fr2->clause && fr2 > FR; fr2 = fr2->parent)
    { DEBUG(3, Sdprintf("discard %d\n", (Word)fr2 - (Word)lBase) );
      leaveFrame(fr2);
      fr2->clause = NULL;
    }
  }
  leaveFrame(FR);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Restore the environment.  If an exception was raised by the query, and no
new exception has been thrown, consider it handled.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
restore_after_query(QueryFrame qf)
{ GET_LD
  if ( qf->exception && !exception_term )
    *valTermRef(exception_printed) = 0;

  environment_frame = qf->saved_environment;
  LD->choicepoints  = qf->saved_bfr;
  aTop		    = qf->aSave;
  lTop		    = (LocalFrame)qf;
  if ( true(qf, PL_Q_NODEBUG) )
  { debugstatus.suspendTrace--;
    debugstatus.debugging = qf->debugSave;
#ifdef O_LIMIT_DEPTH
    depth_limit   = qf->saved_depth_limit;
    depth_reached = qf->saved_depth_reached;
#endif /*O_LIMIT_DEPTH*/
  }
  SECURE(checkStacks(environment_frame));
}


void
PL_cut_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  SECURE(assert(qf->magic == QID_MAGIC));
  qf->magic = 0;			/* disqualify the frame */

  if ( false(qf, PL_Q_DETERMINISTIC) )
    discard_query(qf);

  restore_after_query(qf);
}


void
PL_close_query(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);
  LocalFrame fr = &qf->frame;

  SECURE(assert(qf->magic == QID_MAGIC));
  qf->magic = 0;			/* disqualify the frame */

  if ( false(qf, PL_Q_DETERMINISTIC) )
    discard_query(qf);

  if ( !(qf->exception && true(qf, PL_Q_PASS_EXCEPTION)) )
    Undo(fr->mark);

  restore_after_query(qf);
}


term_t
PL_exception(qid_t qid)
{ GET_LD
  QueryFrame qf = QueryFromQid(qid);

  return qf->exception;
}


#if O_SHIFT_STACKS
#define SAVE_REGISTERS(qid) \
	{ QueryFrame qf = QueryFromQid(qid); \
	  qf->registers.fr  = FR; \
	}
#define LOAD_REGISTERS(qid) \
	{ QueryFrame qf = QueryFromQid(qid); \
	  FR = qf->registers.fr; \
	}
#else /*O_SHIFT_STACKS*/
#define SAVE_REGISTERS(qid)
#define LOAD_REGISTERS(qid)
#endif /*O_SHIFT_STACKS*/

#ifndef ASM_NOP
#define ASM_NOP _PL_nop_counter++
#endif

#ifdef ASM_NOP
int _PL_nop_counter;
#endif

int
PL_next_solution(qid_t qid)
{ GET_LD
  QueryFrame QF;			/* Query frame */
  LocalFrame FR;			/* current frame */
  Word	     ARGP = NULL;		/* current argument pointer */
  Code	     PC;			/* program counter */
//LocalFrame BFR = NULL;		/* last backtrack frame */
  Definition DEF = NULL;		/* definition of current procedure */
  bool	     deterministic;		/* clause found deterministically */
  Word *     aFloor = aTop;		/* don't overwrite old arguments */
#define	     CL (FR->clause)		/* clause of current frame */
#define	     BFR (LD->choicepoints)	/* choicepoint registration */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the labels of the various  virtual-machine instructions in an array.
This is for exploiting GCC's `goto   var' language extension. This array
can only be allocated insite this   function. The initialisation process
calls PL_next_solution() with qid =  QID_EXPORT_WAM_TABLE. This function
will export jmp_table as the compiler  needs   to  know  this table. See
pl-comp.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_LABEL_ADDRESSES
  static void *jmp_table[] =
  { &&I_NOP_LBL,
    &&I_ENTER_LBL,
    &&I_CALL_LBL,
    &&I_DEPART_LBL,
    &&I_EXIT_LBL,
    &&B_FUNCTOR_LBL,
    &&B_RFUNCTOR_LBL,
    &&H_FUNCTOR_LBL,
    &&H_RFUNCTOR_LBL,
    &&I_POPF_LBL,
    &&B_VAR_LBL,
    &&H_VAR_LBL,
    &&B_CONST_LBL,
    &&H_CONST_LBL,
    &&H_INDIRECT_LBL,
    &&B_INTEGER_LBL,
    &&H_INTEGER_LBL,
    &&B_FLOAT_LBL,
    &&H_FLOAT_LBL,

    &&B_FIRSTVAR_LBL,
    &&H_FIRSTVAR_LBL,
    &&B_VOID_LBL,
    &&H_VOID_LBL,
    &&B_ARGFIRSTVAR_LBL,
    &&B_ARGVAR_LBL,

    &&H_NIL_LBL,
    &&B_NIL_LBL,
    &&H_LIST_LBL,
    &&H_RLIST_LBL,
    &&B_LIST_LBL,
    &&B_RLIST_LBL,

    &&B_VAR0_LBL,
    &&B_VAR1_LBL,
    &&B_VAR2_LBL,

    &&I_USERCALL0_LBL,
    &&I_USERCALLN_LBL,
    &&I_CUT_LBL,
    &&I_APPLY_LBL,

#if O_COMPILE_ARITH
    &&A_ENTER_LBL,
    &&A_INTEGER_LBL,
    &&A_DOUBLE_LBL,
    &&A_VAR0_LBL,
    &&A_VAR1_LBL,
    &&A_VAR2_LBL,
    &&A_VAR_LBL,
    &&A_FUNC0_LBL,
    &&A_FUNC1_LBL,
    &&A_FUNC2_LBL,
    &&A_FUNC_LBL,
    &&A_LT_LBL,
    &&A_GT_LBL,
    &&A_LE_LBL,
    &&A_GE_LBL,
    &&A_EQ_LBL,
    &&A_NE_LBL,
    &&A_IS_LBL,
#endif /* O_COMPILE_ARITH */

#if O_COMPILE_OR
    &&C_OR_LBL,
    &&C_JMP_LBL,
    &&C_MARK_LBL,
    &&C_CUT_LBL,
    &&C_IFTHENELSE_LBL,
    &&C_VAR_LBL,
    &&C_END_LBL,
    &&C_NOT_LBL,
    &&C_FAIL_LBL,
#endif /* O_COMPILE_OR */

    &&B_INDIRECT_LBL,
#if O_BLOCK
    &&I_CUT_BLOCK_LBL,
    &&B_EXIT_LBL,
#endif /*O_BLOCK*/
#if O_INLINE_FOREIGNS
    &&I_CALL_FV0_LBL,
    &&I_CALL_FV1_LBL,
    &&I_CALL_FV2_LBL,
#endif /*O_INLINE_FOREIGNS*/
    &&I_FAIL_LBL,
    &&I_TRUE_LBL,
#ifdef O_SOFTCUT
    &&C_SOFTIF_LBL,
    &&C_SOFTCUT_LBL,
#endif
    &&I_EXITFACT_LBL,
    &&D_BREAK_LBL,
#if O_CATCHTHROW
    &&B_THROW_LBL,
#endif
    &&I_CONTEXT_LBL,
    NULL
  };

#define VMI(Name, Count, Msg)	Name ## _LBL: Count; DEBUG(8, Sdprintf Msg);
#if VMCODE_IS_ADDRESS
#define NEXT_INSTRUCTION	goto *(void *)((long)(*PC++))
#else
#define NEXT_INSTRUCTION	goto *jmp_table[*PC++]
#endif

#else /* O_LABEL_ADDRESSES */

code thiscode;

#define VMI(Name, Count, Msg)	case Name: Count; DEBUG(8, Sdprintf Msg);
#define NEXT_INSTRUCTION	goto next_instruction

#endif /* O_LABEL_ADDRESSES */

#if VMCODE_IS_ADDRESS
  if ( qid == QID_EXPORT_WAM_TABLE )
  { interpreter_jmp_table = jmp_table;	/* make it globally known */
    succeed;
  }
#endif /* VMCODE_IS_ADDRESS */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the real start point  of   this  function.  Simply loads the VMI
registers from the frame filled by   PL_open_query()  and either jump to
depart_continue() to do the normal thing or to the backtrack point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  QF  = QueryFromQid(qid);
  SECURE(assert(QF->magic == QID_MAGIC));
  FR  = &QF->frame;
  if ( true(QF, PL_Q_DETERMINISTIC) )	/* last one succeeded */
  { Undo(FR->mark);			/* undo */
    fail;
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check for exceptions raised by foreign code.  PL_throw() uses longjmp()
to get back here.  Our task is to restore the environment and throw the
Prolog exception.

setjmp()/longjmp clobbers register variables. FR   is  restored from the
environment. BFR is volatile, and qid is an argument. These are the only
variables used in the B_THROW instruction.

Is there a way to make the compiler keep its mouth shut!?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( setjmp(QF->exception_jmp_env) != 0 )
  { FliFrame ffr;
#ifdef O_PLMT
    __PL_ld = GLOBAL_LD;		/* might be clobbered */
#endif
    ffr = fli_context;

    FR = environment_frame;
    while(ffr && (void *)ffr > (void *)FR) /* discard foreign contexts */
      ffr = ffr->parent;
    fli_context = ffr;

    if ( LD->current_signal ) 
      unblockSignal(LD->current_signal);

    goto b_throw;
  }

  DEF = FR->predicate;
  if ( QF->solutions )
  { if ( true(DEF, FOREIGN) )
    { Undo(FR->mark);
#if O_DEBUGGER
      if ( debugstatus.debugging )
      { switch( tracePort(FR, BFR, REDO_PORT, NULL) )
	{ case ACTION_FAIL:
	    set(QF, PL_Q_DETERMINISTIC);
	    fail;
	  case ACTION_IGNORE:
	    set(QF, PL_Q_DETERMINISTIC);
	    succeed;
	  case ACTION_RETRY:
	    CL->clause = NULL;
	}
      }
#endif /*O_DEBUGGER*/
#ifdef O_PROFILE
      if ( LD->statistics.profiling )
	DEF->profile_redos++;
#endif /* O_PROFILE */
      goto call_builtin;
    } else
      goto body_failed;
  } else
    goto depart_continue;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Main entry of the virtual machine cycle.  A branch to `next instruction'
will  cause  the  next  instruction  to  be  interpreted.   All  machine
registers  should  hold  valid  data  and  the  machine stacks should be
initialised properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_LABEL_ADDRESSES
  NEXT_INSTRUCTION;
#else
next_instruction:
  thiscode = *PC++;
#ifdef O_DEBUGGER
resumebreak:
#endif
  switch( thiscode )
#endif
  {

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
D_BREAK implements break-points in the  code.   A  break-point is set by
replacing  an  instruction  by  a   D_BREAK  instruction.  The  orininal
instruction is saved in a table. replacedBreak() fetches it.

We might be in a state where  we   are  writing  the arguments above the
current lTop, and therefore with higher this  with the maximum number of
arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(D_BREAK, COUNT(d_break), ("d_break\n"))
#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;
      LocalFrame lSave = lTop;

      lTop = (LocalFrame)argFrameP(lTop, MAXARITY);
      clearUninitialisedVarsFrame(FR, PC-1);
      action = tracePort(FR, BFR, BREAK_PORT, PC-1);
      lTop = lSave;

      switch(action)
      { case ACTION_RETRY:
	  goto retry;
      }
    }
#if O_LABEL_ADDRESSES
    { void *c = (void *)replacedBreak(PC-1);
      
      goto *c;
    }
#else
    thiscode = replacedBreak(PC-1);
    goto resumebreak;
#endif      
#endif /*O_DEBUGGER*/

    VMI(I_NOP, COUNT(i_nop), ("i_nop\n"))
	NEXT_INSTRUCTION;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An atomic constant in the head  of  the  clause.   ARGP  points  to  the
current  argument  to be matched.  ARGP is derefenced and unified with a
constant argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { word c;
    register Word k;					MARK(HCONST);

    VMI(H_CONST,	COUNT_N(h_const_n),	("h_const %d\n", *PC))
	c = (word)*PC++;
	goto common_hconst;
    VMI(H_NIL,		COUNT(h_nil),		("h_nil\n"))
        c = ATOM_nil;

  common_hconst:
        deRef2(ARGP++, k);
        if (isVar(*k))
	{ *k = c;
	  Trail(k, FR);
	  NEXT_INSTRUCTION;
	}
        if (*k == c)
	  NEXT_INSTRUCTION;
        CLAUSE_FAILED;
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
32-bit integer in the head. Copy to the  global stack if the argument is
variable, compare the numbers otherwise.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_INTEGER,	COUNT(h_integer), ("h_integer %s\n", *PC)) MARK(HINT)
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ Word p = allocGlobal(3);

	  *k   = consPtr(p, TAG_INTEGER|STG_GLOBAL);
	  Trail(k, FR);
	  *p++ = mkIndHdr(1, TAG_INTEGER);
	  *p++ = (long)*PC++;
	  *p++ = mkIndHdr(1, TAG_INTEGER);
	  NEXT_INSTRUCTION;
	} else if ( isBignum(*k) && valBignum(*k) == (long)*PC++ )
	  NEXT_INSTRUCTION;

      	CLAUSE_FAILED;
      }  

    VMI(H_FLOAT,	COUNT(h_float), ("h_float\n")) MARK(HFLOAT)
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ Word p = allocGlobal(4);

	  *k   = consPtr(p, TAG_FLOAT|STG_GLOBAL);
	  Trail(k, FR);
	  *p++ = mkIndHdr(2, TAG_FLOAT);
	  *p++ = (long)*PC++;
	  *p++ = (long)*PC++;
	  *p++ = mkIndHdr(2, TAG_FLOAT);
	  NEXT_INSTRUCTION;
	} else if ( isReal(*k) )
	{ Word p = valIndirectP(*k);

	  if ( *p++ == *PC++ && *p == *PC++ ) 
	    NEXT_INSTRUCTION;
	}

      	CLAUSE_FAILED;
      }  

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General indirect in the head.  Used for strings only at the moment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_INDIRECT, COUNT(h_indirect), ("h_indirect %d\n", *PC)) MARK(HINDIR);
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ *k = globalIndirectFromCode(&PC);
	  Trail(k, FR);
	  NEXT_INSTRUCTION;
	}
	if ( isIndirect(*k) && equalIndirectFromCode(*k, &PC) )
	  NEXT_INSTRUCTION;
	CLAUSE_FAILED;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An atomic constant in the body of  a  clause.   We  know  that  ARGP  is
pointing  to  a  not  yet  instantiated  argument  of the next frame and
therefore can just fill the argument.  Trailing is not needed as this is
above the stack anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_CONST, COUNT_N(b_const_n), ("b_const %d\n", *PC)) MARK(BCONST);
      { *ARGP++ = (word)*PC++;
	NEXT_INSTRUCTION;
      }
    VMI(B_NIL, COUNT(b_nil), ("b_nil\n")) MARK(BNIL);
      { *ARGP++ = ATOM_nil;
        NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
32-bit integer in write-mode (body).  Simply   create  the bignum on the
global stack and assign the pointer to *ARGP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(B_INTEGER,	COUNT(b_integer), ("b_integer %s\n", *PC)) MARK(BINT)
      { Word p = allocGlobal(3);

	*ARGP++ = consPtr(p, TAG_INTEGER|STG_GLOBAL);
	*p++ = mkIndHdr(1, TAG_INTEGER);
	*p++ = (long)*PC++;
	*p++ = mkIndHdr(1, TAG_INTEGER);
	NEXT_INSTRUCTION;
      }  

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Double  in  the  body.  Simply  copy  to  the  global  stack.  See  also
globalReal().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(B_FLOAT,	COUNT(b_float), ("b_float\n")) MARK(BINT)
      { Word p = allocGlobal(4);

	*ARGP++ = consPtr(p, TAG_FLOAT|STG_GLOBAL);
	*p++ = mkIndHdr(2, TAG_FLOAT);
	*p++ = (long)*PC++;
	*p++ = (long)*PC++;
	*p++ = mkIndHdr(2, TAG_FLOAT);
	NEXT_INSTRUCTION;
      }  


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_INDIRECT need to copy the  value  on   the  global  stack  because the
XR-table might be freed due to a retract.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_INDIRECT, COUNT_N(b_indirect), ("b_indirect %d\n", *PC)) MARK(BIDT);
      { *ARGP++ = globalIndirectFromCode(&PC);
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the head which is not an anonymous one and is not used for
the first time.  Invoke general unification between the argument pointer
and the variable, whose offset is given relative to  the  frame.

Its doubtfull whether inlining (the simple   cases)  is worthwhile. I've
tested this on various platforms, and   the  results vary. Simplicity is
probably worth more than the 0.001% performance to gain.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_VAR, COUNT_N(h_var_n), ("h_var %d\n", *PC)) MARK(HVAR);
      { Word p1 = varFrameP(FR, *PC++);
	Word p2 = ARGP++;

	if ( unify(p1, p2 PASS_LD) )
	  NEXT_INSTRUCTION;
	CLAUSE_FAILED;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body which is not an anonymous one, is  not  used  for
the  first  time  and is nested in a term (with B_FUNCTOR).  We now know
that *ARGP is a variable,  so  we  either  copy  the  value  or  make  a
reference.   The  difference between this one and B_VAR is the direction
of the reference link in case *k turns out to be variable.

ARGP is pointing into the term on the global stack we are creating.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_ARGVAR, COUNT_N(b_argvar_n), ("b_argvar %d\n", *PC)) MARK(BAVAR);
      { Word k;

	deRef2(varFrameP(FR, *PC++), k);	
	if ( isVar(*k) )
	{ if ( ARGP < k )
	  { setVar(*ARGP);
	    *k = makeRefG(ARGP++);
	    Trail(k, FR);
	    NEXT_INSTRUCTION;
	  }
	  *ARGP++ = makeRefG(k);	/* both on global stack! */
	  NEXT_INSTRUCTION;	  
	}
	*ARGP++ = *k;

	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body which is not an anonymous one and is not used for
the first time.  We now know that *ARGP is a variable, so we either copy
the value or make a reference.  Trailing is not needed as we are writing
above the stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define BODY_VAR(n)   { *ARGP++ = linkVal(varFrameP(FR, (n))); \
			NEXT_INSTRUCTION; \
		      }
    VMI(B_VAR, COUNT_N(b_var_n), ("b_var %d\n", *PC)) MARK(BVARN);
      BODY_VAR(*PC++);
    VMI(B_VAR0, COUNT(b_var_n[9]), ("b_var 9\n")) MARK(BVAR0);
      BODY_VAR(ARGOFFSET / sizeof(word));
    VMI(B_VAR1, COUNT(b_var_n[10]), ("b_var 10\n")) MARK(BVAR1);
      BODY_VAR(1 + ARGOFFSET / sizeof(word));
    VMI(B_VAR2, COUNT(b_var_n[11]), ("b_var 11\n")) MARK(BVAR2);
      BODY_VAR(2 + ARGOFFSET / sizeof(word));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the head, which is  not anonymous, but encountered for the
first time. So we know that the variable   is  still a variable. Copy or
make a reference. Trailing is  not  needed   as  we  are writing in this
frame. As ARGP is pointing in the  argument   list,  it  is on the local
stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_FIRSTVAR, COUNT_N(h_firstvar_n), ("h_firstvar %d\n", *PC))
      MARK(HFVAR);
      { varFrame(FR, *PC++) = (isVar(*ARGP) ? makeRef(ARGP) : *ARGP);
	ARGP++;
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body nested in a term, encountered for the first time.
We now know both *ARGP and the variable are variables.  ARGP  points  to
the  argument  of  a  term  on  the  global stack.  The reference should
therefore go from k to ARGP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_ARGFIRSTVAR, COUNT_N(b_argfirstvar_n), ("b_argfirstvar %d\n", *PC))
      MARK(BAFVAR);
      { setVar(*ARGP);
	varFrame(FR, *PC++) = makeRefG(ARGP++);
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body, encountered for the first  time.   We  now  know
both  *ARGP and the variable are variables.  We set the variable to be a
variable (it is uninitialised memory) and make a reference.  No trailing
needed as we are writing in this and the next frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_FIRSTVAR, COUNT_N(b_firstvar_n), ("b_firstvar %d\n", *PC))
      MARK(BFVAR);
      { Word k = varFrameP(FR, *PC++);

	setVar(*k);
	*ARGP++ = makeRefL(k);
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A singleton variable in the head.  Just increment the argument  pointer.
Also generated for non-singleton variables appearing on their own in the
head  and  encountered  for  the  first  time.   Note  that the compiler
suppresses H_VOID when there are no other instructions before I_ENTER or
I_EXIT.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_VOID, COUNT(h_void), ("h_void\n")) MARK(HVOID);
      { ARGP++;
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A singleton variable in the body. Ensure the argument is a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_VOID, COUNT(b_void), ("b_void\n")) MARK(BVOID);
      { setVar(*ARGP++);
	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A functor in the head.  If the current argument is a  variable  we  will
instantiate  it  with  a  new  term,  all  whose  arguments  are  set to
variables.  Otherwise we check the functor  definition.   In  both  case
ARGP  is  pushed  on the argument stack and set to point to the leftmost
argument of the  term.   Note  that  the  instantiation  is  trailed  as
dereferencing might have caused we are now pointing in a parent frame or
the global stack (should we check?  Saves trail! How often?).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_FUNCTOR, COUNT_N(h_functor_n), ("h_functor %d\n", *PC))
      MARK(HFUNC);
      { functor_t f;

	requireStack(argument, sizeof(Word));
	*aTop++ = ARGP + 1;
    VMI(H_RFUNCTOR, COUNT_N(h_rfunctor_n), ("h_rfunctor %d\n", *PC))
	f = (functor_t) *PC++;
        deRef(ARGP);
	if ( isVar(*ARGP) )
	{ int arity = arityFunctor(f);
	  Word ap;

#ifdef O_SHIFT_STACKS
	  if ( gTop + 1 + arity > gMax )
	    growStacks(FR, PC, FALSE, TRUE, FALSE);
#else
	  requireStack(global, sizeof(word)*(1+arity));
#endif

	  ap = gTop;
	  *ARGP = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
	  Trail(ARGP, FR);
	  *ap++ = f;
	  ARGP = ap;
	  while(arity-- > 0)
	  { setVar(*ap++);
	  }
	  gTop = ap;
	  NEXT_INSTRUCTION;
	}
	if ( hasFunctor(*ARGP, f) )
	{ ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;	    

    VMI(H_LIST, COUNT(h_list), ("h_list\n")) MARK(HLIST);
        requireStack(argument, sizeof(Word));
	*aTop++ = ARGP + 1;
    VMI(H_RLIST, COUNT(h_rlist), ("h_rlist\n")) MARK(HRLIST);
	deRef(ARGP);
	if ( isVar(*ARGP) )
	{ 
#if O_SHIFT_STACKS
  	  if ( gTop + 3 > gMax )
	    growStacks(FR, PC, FALSE, TRUE, FALSE);
#else
	  requireStack(global, 3*sizeof(word));
#endif
	  *ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
	  Trail(ARGP, FR);
	  *gTop++ = FUNCTOR_dot2;
	  ARGP = gTop;
	  setVar(*gTop++);
	  setVar(*gTop++);
	  NEXT_INSTRUCTION;
	}
	if ( isList(*ARGP) )
	{ ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A functor in the body.  As we don't expect ARGP to point to  initialised
memory  while  in  body  mode  we  just  allocate  the  term,  but don't
initialise the arguments to variables.  Allocation is done in  place  to
avoid a function call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_FUNCTOR, COUNT(b_functor), ("b_functor %d\n", *PC)) MARK(BFUNC);
      { functor_t f;
	int arity;

	requireStack(argument, sizeof(Word));
	*aTop++ = ARGP+1;
    VMI(B_RFUNCTOR, COUNT(b_rfunctor), ("b_rfunctor %d\n", *PC)) MARK(BRFUNC);
	f = (functor_t) *PC++;
	arity = arityFunctor(f);
	requireStack(global, sizeof(word) * (1+arity));
	*ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
	*gTop++ = f;
	ARGP = gTop;
	gTop += arity;

	NEXT_INSTRUCTION;
      }

    VMI(B_LIST, COUNT(b_list), ("b_list\n")) MARK(BLIST);
      { requireStack(argument, sizeof(Word));
	*aTop++ = ARGP+1;
    VMI(B_RLIST, COUNT(b_rlist), ("b_rlist %d\n", *PC)) MARK(BRLIST);
	requireStack(global, sizeof(word) * 3);
	*ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
	*gTop++ = FUNCTOR_dot2;
	ARGP = gTop;
	gTop += 2;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop the saved argument pointer (see H_FUNCTOR and B_FUNCTOR).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_POPF, COUNT(i_pop), ("pop\n")) MARK(POP);
      { ARGP = *--aTop;
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enter the body of the clause.  This  instruction  is  left  out  if  the
clause  has no body.  The basic task of this instruction is to move ARGP
from the argument part of this frame into the argument part of the child
frame to be built.  `BFR' (the last frame with alternatives) is  set  to
this   frame   if   this   frame  has  alternatives,  otherwise  to  the
backtrackFrame of this frame.

If this frame has no alternatives it is possible to  put  the  backtrack
frame  immediately  on  the backtrack frame of this frame.  This however
makes debugging much more  difficult  as  the  system  will  do  a  deep
backtrack without showing the fail ports explicitely.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_ENTER, COUNT(i_enter), ("enter\n")) MARK(ENTER);
      { 
#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ clearUninitialisedVarsFrame(FR, PC);
	  switch(tracePort(FR, BFR, UNIFY_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      FRAME_FAILED;
	  }
	  if ( FR->mark.trailtop == INVALID_TRAILTOP )
	  { SetBfr(FR->backtrackFrame);
	  } else
	  { SetBfr(FR);
	  }
	} else
#endif /*O_DEBUGGER*/
	{ if ( true(FR, FR_CUT) )
	  { SetBfr(FR->backtrackFrame);
	  } else
	  { SetBfr(FR);
	  }
	}

	ARGP = argFrameP(lTop, 0);
        NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CONTEXT is used by  non-meta  predicates   that  are  compiled  into a
different  module  using  <module>:<head>  :-    <body>.  The  I_CONTEXT
instruction immediately follows the I_ENTER. The argument is the module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_CONTEXT, COUNT(i_context), ("context \n")) MARK(CONTEXT);
      { Module m = (Module)*PC++;

	FR->context = m;

	NEXT_INSTRUCTION;
      }

#if O_CATCHTHROW
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISO-Compliant catch/3, throw/1  support  in   the  virtual  machine. See
boot/init.pl for the definion of these predicates.

The B_THROW code is the implementation for   throw/1.  The call walks up
the stack, looking for a frame running catch/3 on which it can unify the
exception code. It then cuts all  choicepoints created since throw/3. If
throw/3 is not found, it sets  the   query  exception  field and returns
failure. Otherwise, it will simulate an I_USERCALL0 instruction: it sets
the FR and lTop as it it  was   running  the  throw/3 predicate. Then it
pushes the recovery goal from throw/3 and jumps to I_USERCALL0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    b_throw:
    VMI(B_THROW, COUNT(b_throw), ("b_throw")) MARK(B_THROW);
      { Word catcher;
	word except;
	LocalFrame catchfr, fr, fr2, bfr;

	if ( exception_term )		/* PL_throw() generated */
	  catcher = valTermRef(exception_term);
	else				/* throw/1 generated */
	  catcher = argFrameP(lTop, 0);

	SECURE(checkData(catcher));

	deRef(catcher);
	except = *catcher;
        catchfr = findCatcher(FR, catcher);

	SECURE(checkData(catcher));	/* verify all data on stacks stack */

#if O_DEBUGGER
	if ( !catchfr &&
	     hasFunctor(except, FUNCTOR_error2) &&
	     *valTermRef(exception_printed) != except )
	{ QF = QueryFromQid(qid);	/* reload for relocation */

	  if ( trueFeature(DEBUG_ON_ERROR_FEATURE) &&
	       false(QF, PL_Q_CATCH_EXCEPTION) &&
	       !isCatchedInOuterQuery(QF, catcher) )
	  { fid_t fid = PL_open_foreign_frame();
	    term_t t0 = PL_new_term_refs(2);
	    
	    PL_put_atom(t0+0, ATOM_error);
	    *valTermRef(t0+1) = except;
	    PL_call_predicate(NULL, FALSE, PROCEDURE_print_message2, t0);
	    PL_close_foreign_frame(fid);
	    *valTermRef(exception_printed) = except;

	    pl_trace();
	  }
	}
#endif /*O_DEBUGGER*/

	bfr = NULL;
	for( ; FR && FR > catchfr; FR = FR->parent )
	{ /* Destroy older choicepoints */
	  for(fr = bfr; fr && fr > FR; fr = fr->backtrackFrame)
	  { for(fr2 = fr; fr2 && fr2->clause && fr2 > FR; fr2 = fr2->parent)
	    { DEBUG(3, Sdprintf("discard %d\n", (Word)fr2 - (Word)lBase) );
	      leaveFrame(fr2);
	      fr2->clause = NULL;
	    }
	  }
	  bfr = FR;

#if O_DEBUGGER
	  if ( debugstatus.debugging )
	  { *valTermRef(LD->exception.pending) = except;

	    switch(tracePort(FR, NULL, EXCEPTION_PORT, PC))
	    { case ACTION_RETRY:
		*valTermRef(exception_printed) = 0;
		goto retry;
	    }

	    *valTermRef(LD->exception.pending) = 0;
	  }
#endif
	  leaveFrame(FR);
	  FR->clause = NULL;
	}

	if ( catchfr )
	{ static code exit_instruction;		/* may be gone otherwise */
	  Word p = argFrameP(FR, 1);

	  deRef(p);

	  assert(catchfr == FR);
	  SetBfr(FR->backtrackFrame);
	  environment_frame = FR;
	  undo_while_saving_term(&FR->mark, catcher);
	  unify_ptrs(p, catcher);	/* undo_while_saving_term() also */
					/* undoes unify of findCatcher() */
	  lTop = (LocalFrame) argFrameP(FR, 3); /* above the catch/3 */
	  if ( LD->trim_stack_requested )
	    trimStacks();
	  argFrame(lTop, 0) = argFrame(FR, 2);  /* copy recover goal */
	  *valTermRef(exception_printed) = 0;   /* consider it handled */
	  *valTermRef(exception_bin)     = 0;
	  exception_term		 = 0;

	  exit_instruction = encode(I_EXIT);    /* we must continue with */
	  PC = &exit_instruction;		/* an I_EXIT. Use catch? */

	  goto i_usercall0;
	} else
	{ Word p;

	  *valTermRef(exception_printed) = 0; /* consider it handled */

	  QF = QueryFromQid(qid);	/* may be shifted: recompute */
	  set(QF, PL_Q_DETERMINISTIC);
	  FR = environment_frame = &QF->frame;
	  lTop = (LocalFrame) argFrameP(FR, FR->predicate->functor->arity);

					/* needs a foreign frame? */
	  QF->exception = PL_new_term_ref();
	  p = valTermRef(QF->exception);
	  *p = except;
	  deRef(p);

	  undo_while_saving_term(&QF->frame.mark, p);
	  if ( false(QF, PL_Q_PASS_EXCEPTION) )
	  { *valTermRef(exception_bin)     = 0;
	    exception_term		   = 0;
	  } else
	  { *valTermRef(exception_bin)     = *p;
	    exception_term		   = exception_bin;
	  }

	  if ( LD->trim_stack_requested )
	    trimStacks();

	  fail;
	}
      }
#endif /*O_CATCHTHROW*/

#if O_BLOCK
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
exit(Block, RVal).  First does !(Block).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_EXIT, COUNT(b_exit), ("b_exit")) MARK(B_EXIT);
      { Word name, rval;
	LocalFrame blockfr, fr, fr2;

	name = argFrameP(lTop, 0); deRef(name);
	rval = argFrameP(lTop, 1); deRef(rval);

        if ( !(blockfr = findBlock(FR, name)) )
	{ if ( exception_term )
	    goto b_throw;

	  BODY_FAILED;
	}
	
	set(blockfr, FR_CUT);
	for(fr = BFR; fr > blockfr; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > blockfr; fr2 = fr2->parent)
	  { DEBUG(3, Sdprintf("discard %d\n", (Word)fr2 - (Word)lBase) );
	    leaveFrame(fr2);
	    fr2->clause = NULL;
	  }
	}
#ifdef O_DEBUGGER
        if ( debugstatus.debugging )
	{ SetBfr(blockfr->mark.trailtop != INVALID_TRAILTOP ?
		 blockfr : blockfr->backtrackFrame);
	} else
#endif
	{ SetBfr(blockfr->backtrackFrame);
	}

	for(fr = FR; fr > blockfr; fr = fr->parent)
	{ set(fr, FR_CUT);
	  fr->backtrackFrame = BFR;
	}

	DEBUG(3, Sdprintf("BFR = %d\n", (Word)BFR - (Word)lBase) );

	if ( unify(argFrameP(blockfr, 2), rval PASS_LD) )
	{ for( ; FR > blockfr; FR = FR->parent )
	  { leaveFrame(FR);
	    FR->clause = NULL;
	    if ( FR->parent == blockfr )
	      PC = FR->programPointer;
	  }
					/* TBD: tracing? */

          environment_frame = FR;
	  DEF = FR->predicate;
	  lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  ARGP = argFrameP(lTop, 0);

	  NEXT_INSTRUCTION;
	} else
	{ lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  ARGP = argFrameP(lTop, 0);

	  BODY_FAILED;
	}
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!(Block).  Cuts all alternatives created after entering the named block.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_CUT_BLOCK, COUNT(i_cut_block), ("i_cut_block\n")) MARK(CUT_BLOCK);
      { LocalFrame cutfr, fr, fr2;
	Word name;

	name = argFrameP(lTop, 0); deRef(name);

	if ( !(cutfr = findBlock(FR, name)) )
	{ if ( exception_term )
	    goto b_throw;
	  BODY_FAILED;
	}
	
#ifdef O_DEBUGGER
	if ( debugstatus.debugging )
	{ SetBfr(cutfr->mark.trailtop != INVALID_TRAILTOP ?
		 cutfr : cutfr->backtrackFrame);
	} else
#endif
	{ SetBfr(cutfr->backtrackFrame);
	}

	for(fr = FR; fr > cutfr; fr = fr->parent)
	{ set(fr, FR_CUT);
	  fr->backtrackFrame = BFR;
	}
	set(cutfr, FR_CUT);

	for(fr = BFR; fr > cutfr; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > cutfr; fr2 = fr2->parent)
	  { if ( false(fr, FR_CUT) )
	    { DEBUG(3, Sdprintf("discard [%ld] %s\n",
				levelFrame(fr), predicateName(fr->predicate)));
	      leaveFrame(fr2);
	      fr2->clause = NULL;
	    }
	  }
	}

	DEBUG(3, Sdprintf("BFR = [%ld] %s\n",
			  levelFrame(BFR),
			  predicateName(BFR->predicate)));

	lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
      }
#endif /*O_BLOCK*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!. Basic task is to mark the frame, telling it  is  cut  off,  restoring
`BFR'  to the backtrack frame of this frame (this, nor one of the childs
has alternatives left due to the cut).  `lTop'  is  set  to  point  just
above this frame, as all childs can be abbandoned now.

After the cut all child frames with alternatives and their parents  that
are childs of this frame become garbage.  The interpreter will visit all
these  frames  and  decrease the references of the clauses referenced by
the Prolog goals.

If the debugger is on we change the backtrack frame to this frame rather
than to the  backtrackframe  of  the  current  frame  to  avoid  a  long
backtrack that makes it difficult to understand the tracer's output.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    i_cut:			/* from I_USERCALL0 */
    VMI(I_CUT, COUNT(i_cut), ("cut frame %d\n", REL(FR))) MARK(CUT);
      { LocalFrame fr;
	register LocalFrame fr2;

#ifdef O_DEBUGGER
	if ( debugstatus.debugging )
	{ switch(tracePort(FR, BFR, CUT_CALL_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      FRAME_FAILED;
	  }
	}
#endif

	set(FR, FR_CUT);
	DEBUG(3, Sdprintf("Cutting [%ld] %s\n",
			  levelFrame(FR),
			  predicateName(FR->predicate)));
	for(fr = BFR; fr > FR; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > FR; fr2 = fr2->parent)
	  { DEBUG(3, Sdprintf("    Discard [%ld] %s\n",
			      levelFrame(fr2),
			      predicateName(fr2->predicate)));
	    leaveFrame(fr2);
	    fr2->clause = NULL;
	  }
	}
#ifdef O_DEBUGGER
        if ( debugstatus.debugging )
	{ SetBfr(FR->mark.trailtop != INVALID_TRAILTOP ?
		 FR : FR->backtrackFrame);
	} else
#endif
        { SetBfr(FR->backtrackFrame);
	}

	DEBUG(3,
	      if ( BFR )
	      { Sdprintf("  BFR at [%ld] %s\n",
			 levelFrame(BFR),
			 predicateName(BFR->predicate));
		assert(BFR->clause);
	      } else
	      { Sdprintf("  No BFR\n");
	      });
	  
	lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	ARGP = argFrameP(lTop, 0);

#ifdef O_DEBUGGER
	if ( debugstatus.debugging )
	{ switch(tracePort(FR, BFR, CUT_EXIT_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	    case ACTION_FAIL:
	      FRAME_FAILED;
	  }
	}
#endif

	NEXT_INSTRUCTION;
      }

#if O_COMPILE_OR
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
WAM support for ``A ; B'', ``A -> B'' and ``A -> B ; C'' constructs.  As
these functions introduce control within the WAM instructions  they  are
tagged `C_'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_JMP skips the amount stated in the pointed argument.   The  PC++
could be compiled out, but this is a bit more neath.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_JMP, COUNT_N(c_jmp), ("c_jmp %d\n", *PC)) MARK(C_JMP);
      { PC += *PC;
	PC++;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_MARK saves the value of BFR  (current   backtrack  frame) into a local
frame slot reserved by the compiler.  Note that the variable to hold the
local-frame pointer is  *not*  reserved   in  clause->variables,  so the
garbage collector won't see it.  With the introduction of stack-shifting
this slot has been made relative to lBase.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_MARK, COUNT_N(c_mark), ("c_mark %d\n", *PC)) MARK(C_MARK);
      { varFrame(FR, *PC++) = (word) BFR;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_VAR is generated by the compiler to ensure the  instantiation  pattern
of  the  variables  is  the  same after finishing both paths of the `or'
wired in the clause.  Its task is to make the n-th variable slot of  the
current frame to be a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_VAR, COUNT_N(c_var), ("c_var %d\n", *PC)) MARK(C_VAR);
      { setVar(varFrame(FR, *PC++));

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_CUT will  destroy  all  backtrack  points  created  after  the  C_MARK
instruction in this clause.  It assumes the value of BFR has been stored
in the nth-variable slot of the current local frame.

We can dereference all frames that are older that the old backtrackframe
and older than this frame.

All frames created since what becomes now the  backtrack  point  can  be
discarded.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_CUT, COUNT_N(c_cut), ("c_cut %d\n", *PC)) MARK(C_CUT);
      { LocalFrame obfr = (LocalFrame) varFrame(FR, *PC);
	LocalFrame cbfr = obfr;
	LocalFrame fr;
	register LocalFrame fr2;

	PC++;				/* cannot be in macro! */
	if ( BFR <= cbfr )		/* already done this */
	  NEXT_INSTRUCTION;
	if ( cbfr < FR )
	  cbfr = FR;

	for(fr = BFR; fr > cbfr; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > cbfr; fr2 = fr2->parent)
	  { DEBUG(3, Sdprintf("discard %d: ", (Word)fr2 - (Word)lBase) );
	    /*DEBUG(3, writeFrameGoal(fr2, 2); pl_nl() );*/
	    leaveFrame(fr2);
	    fr2->clause = NULL;
	  }
	}

	/*DEBUG(3, Sdprintf("BFR at "); writeFrameGoal(BFR, 2); pl_nl() );*/
	{ int nvar = (true(cbfr->predicate, FOREIGN)
				? cbfr->predicate->functor->arity
				: cbfr->clause->clause->variables);
	  lTop = (LocalFrame) argFrameP(cbfr, nvar);
	  ARGP = argFrameP(lTop, 0);
	}
        SetBfr(obfr);

        NEXT_INSTRUCTION;
      }

#ifdef O_SOFTCUT
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handle the commit-to of A *-> B; C.  Simply mark the $alt/1 frame as cutted,
and control will not reach C again.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_SOFTCUT, COUNT_N(c_softcut), ("c_softcut %d\n", *PC)) MARK(CSOFTCUT);
      { LocalFrame altfr = (LocalFrame) varFrame(FR, *PC);

	assert(altfr->predicate == PROCEDURE_alt0->definition);
	PC++;
	set(altfr, FR_CUT);
	NEXT_INSTRUCTION;
      }
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_END is a dummy instruction to help the decompiler to find the end of A
->  B.  (Note  that  a  :-  (b  ->  c),  d == a :- (b -> c, d) as far as
semantics.  They are different terms however.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_END, COUNT(c_end), ("c_end\n")) MARK(C_END);
      {	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_FAIL is equivalent to fail/0. Used to implement \+/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
   VMI(C_FAIL, COUNT(c_fail), ("c_fail\n")) MARK(C_FAIL);
      {	BODY_FAILED;
      }
#endif /* O_COMPILE_OR */

#if O_COMPILE_ARITH
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Arithmic is compiled using a  stack  machine.    ARGP  is  used as stack
pointer and the arithmic stack is allocated   on top of the local stack,
starting at the argument field of the next slot of the stack (where ARGP
points to when processing the body anyway).

Arguments to functions are pushed on the stack  starting  at  the  left,
thus `add1(X, Y) :- Y is X + 1' translates to:

    I_ENTER	% enter body
    B_VAR1	% push Y via ARGP
    A_ENTER	% align the stack to prepare for writing doubles
    A_VAR0	% evaluate X and push numeric result
    A_INTEGER 1	% Push 1 as numeric value
    A_FUNC2 0	% Add top-two of the stack and push result
    A_IS 	% unify Y with numeric result
    I_EXIT	% leave the clause

a_func0:	% executes arithmic function without arguments, pushing
		% its value on the stack
a_func1:	% unary function. Changes the top of the stack.
a_func2:	% binary function. Pops two values and pushes one.

Note that we do not call `ar_func0(*PC++, &ARGP)' as ARGP is a register
variable.  Also, for compilers that do register allocation it is unwise
to give the compiler a hint to put ARGP not into a register.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_ENTER, COUNT(a_enter), ("a_enter")) MARK(AENTER)
      { 
#ifdef DOUBLE_ALIGNMENT
	ARGP = (Word) (((unsigned long)ARGP + (DOUBLE_ALIGNMENT-1)) &
		       ~(DOUBLE_ALIGNMENT-1));
#endif
        NEXT_INSTRUCTION;
      }

    VMI(A_INTEGER, COUNT(a_integer), ("a_integer %d\n", *PC)) MARK(AINT);
      {	Number n = (Number)ARGP;

	n->value.i = (long) *PC++;
	n->type    = V_INTEGER;
	ARGP       = (Word)(n+1);
	NEXT_INSTRUCTION;
      }

    VMI(A_DOUBLE, COUNT(a_double), ("a_double %d\n", *PC)) MARK(ADOUBLE);
      {	Number n = (Number)ARGP;

	n->value.w[0] = *PC++;
	n->value.w[1] = *PC++;
	n->type       = V_REAL;
	ARGP          = (Word)(n+1);
	NEXT_INSTRUCTION;
      }

    VMI(A_VAR, COUNT_N(a_var_n), ("a_var %d\n", *PC)) MARK(AVARN);
    { int offset = *PC++;
      term_t v;
      Number n;

    a_var_n:
      v = consTermRef(varFrameP(FR, offset));
      n = (Number)ARGP;

      if ( valueExpression(v, n PASS_LD) )
      { ARGP = (Word)(n+1);
	NEXT_INSTRUCTION;
      } else
      {
#if O_CATCHTHROW
	if ( exception_term )
	  goto b_throw;
#endif
	BODY_FAILED;			/* check this */
      }

    VMI(A_VAR0, COUNT(a_var0), ("a_var0\n")) MARK(AVAR0);
      offset = ARGOFFSET / sizeof(word);
      goto a_var_n;
    VMI(A_VAR1, COUNT(a_var1), ("a_var1\n")) MARK(AVAR1);
      offset = ARGOFFSET / sizeof(word) + 1;
      goto a_var_n;
    VMI(A_VAR2, COUNT(a_var2), ("a_var2\n")) MARK(AVAR2);
      offset = ARGOFFSET / sizeof(word) + 2;
      goto a_var_n;
    }

  { int an;
    code fn;

    VMI(A_FUNC0, COUNT_N(a_func0), ("a_func0 %d\n", *PC)) MARK(A_FUNC0);
      {	an = 0;
	fn = *PC++;
	goto common_an;
      }

    VMI(A_FUNC1, COUNT_N(a_func1), ("a_func1 %d\n", *PC)) MARK(A_FUNC1);
      {	an = 1;
	fn = *PC++;
	goto common_an;
      }

    VMI(A_FUNC2, COUNT_N(a_func2), ("a_func2 %d\n", *PC)) MARK(A_FUNC2);
      {	an = 2;
	fn = *PC++;
	goto common_an;
      }

    VMI(A_FUNC, COUNT_N(a_func), ("a_func %d %d\n",*PC,PC[1])) MARK(A_FUNC);
      {	Number n;

	fn = *PC++;
	an = (int) *PC++;

      common_an:
	n = (Number) ARGP;

	if ( !ar_func_n(fn, an, &n) )
	{ if ( exception_term )
	    goto b_throw;
	  BODY_FAILED;
	}

	ARGP = (Word) n;
	NEXT_INSTRUCTION;
      }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of the arithmic comparison predicates (<, >, =<,  >=,  =:=).
Both sides are pushed on the stack, so we just compare the two values on
the  top  of  this  stack  and  backtrack  if  they  do  not suffice the
condition.  Example translation: `a(Y) :- b(X), X > Y'

    ENTER
    B_FIRSTVAR 1	% Link X from B's frame to a new var in A's frame
    CALL 0		% call b/1
    A_VAR 1		% Push X
    A_VAR 0		% Push Y
    A_GT		% compare
    EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_LT, COUNT(a_lt), ("a_lt\n")) MARK(A_LT);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, LT) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_LE, COUNT(a_le), ("a_le\n")) MARK(A_LE);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, LE) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_GT, COUNT(a_gt), ("a_gt\n")) MARK(A_GT);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, GT) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_GE, COUNT(a_ge), ("a_ge\n")) MARK(A_GE);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, GE) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_EQ, COUNT(a_eq), ("a_eq\n")) MARK(A_EQ);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, EQ) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

    VMI(A_NE, COUNT(a_ne), ("a_ne\n")) MARK(A_NE);
      { Number n = (Number)ARGP;
	n -= 2;
	ARGP = (Word)n;
	if ( !ar_compare(n, n+1, NE) )
	  BODY_FAILED;
	ARGP = argFrameP(lTop, 0);
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of is/2.  The stack has two pushed values: the variable for
the result (a word) and the number holding the result.  For example:

	 a(X) :- X is sin(3).

	I_ENTER
	B_VAR 0			push left argument of is/2
	A_INTEGER 3		push integer as number
	A_FUNC <sin>		run function on it
	A_IS			bind value
	I_EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_IS, COUNT(a_is), ("a_is\n")) MARK(A_IS);
      { Number n = (Number)ARGP;
	Word k;

	n--;				/* pop the number */
	ARGP = argFrameP(lTop, 0);	/* 1-st argument */
	deRef2(ARGP, k);
	canoniseNumber(n);		/* whole real --> long */

	if ( isVar(*k) )
	{ Mark(lTop->mark);
	  Trail(k, lTop);
	  if ( intNumber(n) )
	  { if ( inTaggedNumRange(n->value.i) )
	      *k = consInt(n->value.i);
	    else
	      *k = globalLong(n->value.i);
	  } else
	    *k = globalReal(n->value.f);
	  NEXT_INSTRUCTION;
	} else
	{ if ( isInteger(*k) && intNumber(n) && valInteger(*k) == n->value.i )
	    NEXT_INSTRUCTION;
	  if ( isReal(*k) && floatNumber(n) && valReal(*k) == n->value.f )
	    NEXT_INSTRUCTION;
	}

	BODY_FAILED;
      }
#endif /* O_COMPILE_ARITH */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_USERCALL0 is generated by the compiler if a variable is encountered as
a subclause. Note that the compount   statement  opened here is encloses
also I_APPLY and I_CALL. This allows us to use local register variables,
but still jump to the `normal_call' label to   do the common part of all
these three virtual machine instructions.

I_USERCALL0 has the task of  analysing  the   goal:  it  should fill the
->procedure slot of the new frame and  save the current program counter.
It also is responsible of filling the   argument part of the environment
frame with the arguments of the term.

BUG: have to find out how to proceed in case of failure (I am afraid the
`goto frame_failed' is a bit dangerous here).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#if O_CATCHTHROW
    i_usercall0:			/* from B_THROW */
#endif
    VMI(I_USERCALL0, COUNT(i_usercall0), ("user_call0\n")) MARK(USRCL0);
      { word goal;
	int arity;
	Word args, a;
	int n;
	register LocalFrame next;
	Module module;
	functor_t functor;
	int callargs;

	next = lTop;
	a = argFrameP(next, 0);		/* get the (now) instantiated */
	deRef(a);			/* variable */

	module = NULL;
	a = stripModule(a, &module);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the functor definition associated with the goal as well as the
arity and a pointer to the argument vector of the goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	if ( isAtom(goal = *a) )
	{ if ( *a == ATOM_cut )
	    goto i_cut;
	  functor = lookupFunctorDef(goal, 0);
	  arity   = 0;
	  args    = NULL;
	} else if ( isTerm(goal) )
	{ args    = argTermP(goal, 0);
	  functor = functorTerm(goal);
	  arity   = arityFunctor(functor);
	} else
	{ PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, wordToTermRef(a));
	  goto b_throw;
	}
	goto i_usercall_common;

    VMI(I_USERCALLN, COUNT_N(i_usercalln), ("user_calln %d\n", *PC)) MARK(USRCLN);
        callargs = *PC++;
	next = lTop;
	a = argFrameP(next, 0);		/* get the (now) instantiated */
	deRef(a);			/* variable */

	module = NULL;
	if ((a = stripModule(a, &module)) == (Word) NULL)
	  FRAME_FAILED;
	if ( isAtom(goal = *a) )
	{ arity   = 0;
	  functor = lookupFunctorDef(goal, callargs);
	  args    = NULL;
	} else if ( isTerm(goal) )
	{ FunctorDef fdef = valueFunctor(functorTerm(goal));

	  arity   = fdef->arity;
	  functor = lookupFunctorDef(fdef->name, arity + callargs);
	  args    = argTermP(goal, 0);
	} else
	{ PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, wordToTermRef(a));
	  goto b_throw;
	}

	if ( arity != 1 )
	{ int i, shift = arity - 1;

	  a = argFrameP(next, 1);	/* pointer to 1-st arg */
	  
	  if ( shift > 0 )
	  { for(i=callargs-1; i>=0; i--)
	    { if ( isRef(a[i]) )
	      { Word a1 = unRef(a[i]);
	    
		if ( a1 >= a && a1 < a+arity )
		  a[i+shift] = makeRef(a1+shift);
		else
		  a[i+shift] = a[i];
	      } else
		a[i+shift] = a[i];
	    }
	  } else
	  { for(i=0; i < callargs; i++)
	    { if ( isRef(a[i]) )
	      { Word a1 = unRef(a[i]);
		
		if ( a1 >= a && a1 < a+arity )
		  a[i+shift] = makeRef(a1+shift);
		else
		  a[i+shift] = a[i];
	      } else
		a[i+shift] = a[i];
	    }
	  }
	}

    i_usercall_common:
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )
	  set(next, FR_NODEBUG);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now scan the argument vector of the goal and fill the arguments  of  the
frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	if ( arity > 0 )
	{ ARGP = argFrameP(next, 0);

	  for(; arity-- > 0; ARGP++, args++)
	    *ARGP = linkVal(args);
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the associated procedure.  First look in the specified module.   If
the function is not there then look in the user module.  Finally specify
the context module environment for the goal. This is not necessary if it
will  be  specified  correctly  by  the goal started.  Otherwise tag the
frame and write  the  module  name  just  below  the  frame.   See  also
contextModule().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	DEF = resolveProcedure(functor, module)->definition;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Save the program counter (note  that   I_USERCALL0  has no argument) and
continue as with a normal call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	next->context = module;
	goto normal_call;
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fast control functions. Should  set-up  normal   call  if  the  function
doesn't exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_FAIL, COUNT(i_fail), ("i_fail\n")) MARK(I_FAIL);
#ifdef O_DEBUGGER
      if ( debugstatus.debugging )
      { next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	  set(next, FR_NODEBUG);
	DEF = lookupProcedure(FUNCTOR_fail0, MODULE_system)->definition;
	next->context = FR->context;

	goto normal_call;
      }
#endif
      BODY_FAILED;

    VMI(I_TRUE, COUNT(i_true), ("i_true\n")) MARK(I_TRUE);
#ifdef O_DEBUGGER
      if ( debugstatus.debugging )
      { next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	  set(next, FR_NODEBUG);
	DEF = lookupProcedure(FUNCTOR_true0, MODULE_system)->definition;
	next->context = FR->context;

	goto normal_call;
      }
#endif
      NEXT_INSTRUCTION;

#if O_COMPILE_OR
#ifdef O_SOFTCUT
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A *-> B ; C is translated to C_SOFIF <A> C_SOFTCUT <B> C_JMP end <C>.  See
pl-comp.c and C_SOFTCUT implementation for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_SOFTIF, COUNT_2N(c_softif), ("c_softif %d\n", *PC)) MARK(C_SOFTIF);
      { varFrame(FR, *PC++) = (word) lTop; /* see C_SOFTCUT */

	goto c_or;
      }

#endif
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If-then-else is a contraction of C_MARK and C_OR.  This contraction  has
been  made  to help the decompiler distinguis between (a ; b) -> c and a
-> b ; c, which would otherwise only be  possible  to  distinguis  using
look-ahead.

The asm("nop") is a tricky. The problem   is that C_NOT and C_IFTHENELSE
are the same instructions. The one is generated on \+/1 and the other on
(Cond -> True ; False). Their different   virtual-machine  id is used by
the decompiler. Now, as the VMCODE_IS_ADDRESS   is  in effect, these two
instruction would become the same. The  asm("nop") ensures they have the
same *functionality*, but a *different* address.  If your machine does't
like nop, define the macro ASM_NOP in  your md-file to do something that
1) has *no effect* and 2) is *not optimised* away by the compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_NOT, {}, ("c_not %d\n", *PC)) MARK(C_NOT)
#if VMCODE_IS_ADDRESS
#ifdef ASM_NOP
      ASM_NOP;
#else
      asm("nop");
#endif
#endif
    VMI(C_IFTHENELSE, COUNT_2N(c_ifthenelse), ("c_ifthenelse %d\n", *PC))
      MARK(C_ITE);
      { varFrame(FR, *PC++) = (word) BFR; /* == C_MARK */

	/*FALL-THROUGH to C_OR*/
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_OR introduces a backtrack point within the clause.   The  argument  is
how  many  entries  of  the  code  array  to skip should backtracking be
necessary.  It is implemented by calling a foreign  functions  predicate
with as argument the amount of bytes to skip.  The foreign function will
on  first  call  succeed,  leaving  a  backtrack  point.   It does so by
returning the amount to skip as backtracking  argument.   On  return  it
will increment PC in its frame with this amount (which will be popped on
its exit) and succeed deterministically.

Note that this one is enclosed in the compound statement of I_USERCALL0,
I_APPLY, I_CALL and I_DEPART to allow   sharing of the register variable
`next' with them and thus make the `goto common_call' valid.

NOTE: as of SWI-Prolog 2.0.2, the  call   to  $alt/1  is `inlined'. As a
consequence it has lost its argument and   is  now $alt/0. We just build
the frame for $alt/1 and then  continue   execution.  This  is ok as the
first call of $alt/1 simply succeeds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_OR, COUNT_N(c_or), ("c_or %d\n", *PC)) MARK(C_OR);
    c_or:
      { int skip = *PC++;

	DEBUG(9, Sdprintf("$alt to %d in [%ld] %s\n",
			  PC-CL->clause->codes + skip,
			  levelFrame(FR), predicateName(FR->predicate)));
	next = lTop;
	next->flags = FR->flags;
	next->predicate = PROCEDURE_alt0->definition;
	next->programPointer = PC;
	next->context = MODULE_system;

        requireStack(local, (int)argFrameP((LocalFrame)NULL, 0));
	next->backtrackFrame = BFR;
	next->parent = FR;
	incLevel(next);
	clear(next, FR_CUT|FR_SKIPPED);
	LD->statistics.inferences++;
	Mark(next->mark);
	lTop = (LocalFrame)argFrameP(next, 0);
					/* callForeign() here */
	next->clause = (ClauseRef) (ForeignRedoIntVal(skip)|FRG_REDO);
	SetBfr(next);
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
      }
#endif /* O_COMPILE_OR */

#ifdef O_INLINE_FOREIGNS
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALL_FV[012] Call a deterministics foreign procedures  with a 0, 1, or
2 arguments that appear as variables in   the  clause. This covers true,
fail, var(X) and other type-checking  predicates,   =/2  in  a number of
cases (i.e. X = Y, not X = 5).

The VMI for these calls are ICALL_FVN, proc, var-index ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    { int nvars;
      Procedure fproc;
      Word v;

      VMI(I_CALL_FV0, COUNT(i_call_fv0), ("i_call_fv0")) MARK(CFV0);
      { fproc = (Procedure) *PC++;
	nvars = 0;

	goto common_call_fv;
      }

      VMI(I_CALL_FV1, COUNT(i_call_fv1), ("i_call_fv1")) MARK(CFV1);
      { fproc = (Procedure) *PC++;
	nvars = 1;
	v = varFrameP(FR, *PC++);
	*ARGP++ = (isVar(*v) ? makeRefL(v) : *v);
	goto common_call_fv;
      }

      VMI(I_CALL_FV2, COUNT(i_call_fv2), ("i_call_fv2")) MARK(CFV2);
      { fproc = (Procedure) *PC++;
	nvars = 2;
	v = varFrameP(FR, *PC++);
	*ARGP++ = (isVar(*v) ? makeRefL(v) : *v);
	v = varFrameP(FR, *PC++);
	*ARGP++ = (isVar(*v) ? makeRefL(v) : *v);

      common_call_fv:
	{ Definition def = fproc->definition;
	  Func f = def->definition.function;
	  int rval;

	  if ( !f )
	  { def = trapUndefined(def);
	    f = def->definition.function;
	  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we are debugging, just build a normal  frame and do the normal thing,
so the inline call is expanded to a normal call and may be traced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	  if ( !f ||
#ifdef O_DEBUGGER
	       debugstatus.debugging ||
#endif
	       false(def, FOREIGN) )
	  { next = lTop;
	    next->flags = FR->flags;
	    if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	      set(next, FR_NODEBUG);
	    DEF = def;
	    next->context = FR->context;

	    goto normal_call;
	  } else
	  { LocalFrame oldtop = lTop;
	    term_t h0;
	    fid_t fid;
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We must create a frame and mark the  stacks for two reasons: undo if the
foreign call fails *AND*  make  sure   Trail()  functions  properly.  We
increase lTop too to prepare for asynchronous interrupts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	    LD->statistics.inferences++;
	    next = lTop;
	    h0 = argFrameP(next, 0) - (Word)lBase;
	    lTop = (LocalFrame) argFrameP(next, nvars);
	    if ( true(def, METAPRED) )
	      next->context = FR->context;
	    else
	      next->context = def->module;
	    next->predicate      = def;
	    next->programPointer = PC;
	    next->parent         = FR;
	    next->flags		 = FR->flags;
	    incLevel(next);
	    next->backtrackFrame = BFR;
#ifdef O_PROFILE
	    if ( LD->statistics.profiling )
	      def->profile_calls++;
#endif /* O_PROFILE */
	    environment_frame = next;
	    Mark(next->mark);

	    exception_term = 0;
	    SAVE_REGISTERS(qid);
	    fid = PL_open_foreign_frame();
	    if ( is_signalled() )
	    { PL_handle_signals();
	      if ( exception_term )
		goto b_throw;
	    }
	    switch(nvars)
	    { case 0:
		rval = (*f)();
	        break;
	      case 1:
		rval = (*f)(h0);
	        break;
	      case 2:
	      default:
		rval = (*f)(h0, h0+1);
	        break;
	    }
	    PL_close_foreign_frame(fid);
	    LOAD_REGISTERS(qid);

	    ARGP -= nvars;
	    environment_frame = FR;
	    lTop = oldtop;

	    if ( exception_term )
	    { if ( rval )
	      { exception_term = 0;
		setVar(*valTermRef(exception_bin));
	      } else
		goto b_throw;
	    }

	    if ( rval )
	    { NEXT_INSTRUCTION;
	    }

	    Undo(next->mark);
	    LD->statistics.inferences++;	/* is a redo! */
	    BODY_FAILED;
	  }
	}
      }
    }
#endif /*O_INLINE_FOREIGNS*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_APPLY is the code generated by the Prolog goal $apply/2 (see reference
manual for the definition of apply/2).  We   expect  a term in the first
argument of the frame and a  list   in  the second, comtaining aditional
arguments. Most comments of I_USERCALL0 apply   to I_APPLY as well. Note
that the two arguments are copied in  local variables as they will later
be overwritten by the arguments for the actual call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
      VMI(I_APPLY, COUNT(i_apply), ("apply\n")) MARK(APPLY);
      { atom_t functor;
	Word lp;
	Module module = (Module) NULL;
	Word gp;

	next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )
	  set(next, FR_NODEBUG);

	ARGP = argFrameP(next, 0); deRef(ARGP); gp = ARGP;
	ARGP = argFrameP(next, 1); deRef(ARGP); lp = ARGP;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Obtain the functor of the actual goal from the first argument  and  copy
the arguments of this term in the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	
	gp = stripModule(gp, &module);
	next->context = module;
	goal = *gp;

	ARGP = argFrameP(next, 0);

	if (isAtom(goal) )
	{ functor = goal;
	  arity = 0;
	} else if ( isTerm(goal) )
	{ Functor     f = valueTerm(goal);
	  FunctorDef fd = valueFunctor(f->definition);

	  functor = fd->name;
	  arity   = fd->arity;
	  args    = f->arguments;
	  for(n=0; n<arity; n++, ARGP++, args++)
	    *ARGP = linkVal(args);
	} else
	{ PL_error("apply", 2, NULL, ERR_TYPE,
		   ATOM_callable, wordToTermRef(a));
	  goto b_throw;
	}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the list and add the elements to the argument vector of the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	while(!isNil(*lp) )
	{ if (!isList(*lp) )
	  { PL_error("apply", 2, NULL, ERR_TYPE,
		     ATOM_list, wordToTermRef(lp));
	    goto b_throw;
	  }
	  args = argTermP(*lp, 0);	/* i.e. the head */
	  *ARGP++ = linkVal(args);
	  arity++;
	  if (arity > MAXARITY)
	  { PL_error("apply", 2, NULL, ERR_REPRESENTATION, ATOM_max_arity);
	    goto b_throw;
	  }
	  lp = argTermP(*lp, 1);	/* i.e. the tail */
	  deRef(lp);
	}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the associated procedure (see I_CALL for module handling), save the
program pointer and jump to the common part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	{ functor_t fdef;

	  fdef = lookupFunctorDef(functor, arity);
	  DEF = resolveProcedure(fdef, module)->definition;
	  next->context = module;
	}

	goto normal_call;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALL and I_DEPART form the normal code generated by the  compiler  for
calling  predicates.   The  arguments  are  already written in the frame
starting at `lTop'.  I_DEPART implies it is the last  subclause  of  the
clause.  This is be the entry point for tail recursion optimisation.

The task of I_CALL is to  save  necessary  information  in  the  current
frame,  fill  the next frame and initialise the machine registers.  Then
execution can continue at `next_instruction'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#define TAILRECURSION 1
      VMI(I_DEPART,
	  COUNT(i_depart),
	  ("depart %s\n", procedureName((Procedure)*PC)))
							 MARK(DEPART);
#if TAILRECURSION
	if ( true(FR, FR_CUT) && BFR <= FR
#if O_DEBUGGER
	     && !debugstatus.debugging
#endif
	   )
	{ 
#if O_DEBUGGER
	  if ( true(FR, FR_WATCHED) )
	  { LocalFrame lSave = lTop;
	    arity = ((Procedure) *PC)->definition->functor->arity;

	    lTop = (LocalFrame)argFrameP(lTop, arity);
	    frameFinished(FR);
	    lTop = lSave;
	  }
#endif
  	  leaveDefinition(DEF);
	  if ( true(DEF, HIDE_CHILDS) )
	    set(FR, FR_NODEBUG);
	  
	  FR->predicate = DEF = ((Procedure) *PC++)->definition;
	  copyFrameArguments(lTop, FR, DEF->functor->arity PASS_LD);

	  goto depart_continue;
	}
#endif /*TAILRECURSION*/
       /*FALLTHROUGH*/
      VMI(I_CALL,
	  COUNT(i_call),
	  ("call %s\n", procedureName((Procedure)*PC)))
	  MARK(CALL);
        next = lTop;
        next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )		/* parent has hide_childs */
	  set(next, FR_NODEBUG);
	DEF = ((Procedure) *PC++)->definition;
	next->context = FR->context;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the common part of the call variations.  By now the following is
true:

  - arguments, nodebug		filled
  - context			filled with context for
				transparent predicate
  - DEF				filled
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

      normal_call:
	requireStack(local, (int)argFrameP((LocalFrame)NULL, MAXARITY));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise those slots of the frame that are common to Prolog predicates
and foreign ones.  There might be some possibilities for optimisation by
delaying these initialisations till they are really  needed  or  because
the information they are calculated from is destroyed.  This probably is
not worthwile.

Note: we are working above `lTop' here!!   We restore this as quickly as
possible to be able to call-back to Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	next->backtrackFrame = BFR;
	next->parent         = FR;
	next->predicate	     = DEF;		/* TBD */
	next->programPointer = PC;		/* save PC in child */
	next->clause         = NULL;		/* for save atom-gc */
	environment_frame = FR = next;		/* open the frame */

      depart_continue:
        lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);

#ifdef O_DEBUGLOCAL
      {	Word ap = argFrameP(FR, DEF->functor->arity);
	int n;
	
	for(n=50; --n; )
	  *ap++ = (word)(((char*)ATOM_nil) + 1);
      }
#endif

#ifdef O_LOGICAL_UPDATE
	FR->generation     = GD->generation;
#endif
	incLevel(FR);
#ifdef O_DEBUGGER
      retry_continue:
#endif
	clear(FR, FR_CUT|FR_SKIPPED|FR_WATCHED);

	LD->statistics.inferences++;
	Mark(FR->mark);

	if ( is_signalled() )
	{ PL_handle_signals();
	  if ( exception_term )
	    goto b_throw;
	}

#if O_ASYNC_HOOK			/* Asynchronous hooks */
	{ if ( async.hook &&
	       !((++LD->statistics.inferences & async.mask)) )
	    (*async.hook)();		/* check the hook */
	}
#endif

#ifdef O_PROFILE
	if ( LD->statistics.profiling )
	  DEF->profile_calls++;
#endif /* O_PROFILE */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Undefined predicate detection and handling.   trapUndefined() takes care
of linking from the public modules or calling the exception handler.

Note that DEF->definition is  a  union   of  the  clause  or C-function.
Testing is suffices to find out that the predicate is defined.

Logical-update: note that trapUndefined() may add  clauses and we should
be able to access these!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	if ( !DEF->definition.clauses && false(DEF, PROC_DEFINED) )	
	{ FR->predicate = DEF = trapUndefined(DEF);
#ifdef O_LOGICAL_UPDATE
	  FR->generation = GD->generation;
#endif

	  if ( !DEF->definition.clauses &&
	       false(DEF, PROC_DEFINED) &&
	       true(DEF->module, UNKNOWN) )
	  { FR->clause = NULL;
	    if ( exception_term )
	      goto b_throw;
	  }
	}

	if ( false(DEF, METAPRED) )
	  FR->context = DEF->module;
	if ( false(DEF, SYSTEM) )
	  clear(FR, FR_NODEBUG);

#if O_DYNAMIC_STACKS
	if ( gc_status.requested )
	{ garbageCollect(FR);
	}
#else /*O_DYNAMIC_STACKS*/
#if O_SHIFT_STACKS
      { int gshift = narrowStack(global);
	int lshift = narrowStack(local);
	int tshift = narrowStack(trail);

	if ( gshift || lshift || tshift )
	{ if ( gshift || tshift )
	  { long gused = usedStack(global);
	    long tused = usedStack(trail);

	    garbageCollect(FR);
	    DEBUG(1, Sdprintf("\tgshift = %d; tshift = %d", gshift, tshift));
	    if ( gshift )
	      gshift = ((2 * usedStack(global)) > gused);
	    if ( tshift )
	      tshift = ((2 * usedStack(trail)) > tused);
	    DEBUG(1, Sdprintf(" --> gshift = %d; tshift = %d\n",
			    gshift, tshift));
	  }

	  if ( gshift || tshift || lshift )
	  { SAVE_REGISTERS(qid);
	    growStacks(FR, NULL, lshift, gshift, tshift);
	    LOAD_REGISTERS(qid);
	  }
	}
      }
#else /*O_SHIFT_STACKS*/
	if ( narrowStack(global) || narrowStack(trail) )
	  garbageCollect(FR);
#endif /*O_SHIFT_STACKS*/
#endif /*O_DYNAMIC_STACKS*/

	if ( LD->outofstack )
	  outOfStack(LD->outofstack, STACK_OVERFLOW_SIGNAL_IMMEDIATELY);

#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ CL = DEF->definition.clauses;
	  switch(tracePort(FR, BFR, CALL_PORT, NULL))
	  { case ACTION_FAIL:	goto frame_failed;
	    case ACTION_IGNORE: goto exit_builtin;
	    case ACTION_RETRY:  goto retry;
	  }
	}
#endif /*O_DEBUGGER*/

	if ( true(DEF, FOREIGN) )
	{ int rval;

	  CL = (ClauseRef) FIRST_CALL;
	call_builtin:			/* foreign `redo' action */
	  SAVE_REGISTERS(qid);
	  rval = callForeign(DEF, FR);
	  LOAD_REGISTERS(qid);
	  if ( rval )
	    goto exit_builtin;

#if O_CATCHTHROW
	  if ( exception_term )
	  { goto b_throw;
	  }
#endif

	  goto frame_failed;
	} 

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a normal Prolog predicate.  Just   load  the machine registers with
values found in the clause,  give  a   reference  to  the clause and set
`lTop' to point to the first location after the current frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	ARGP = argFrameP(FR, 0);
	enterDefinition(DEF);

#ifdef O_LIMIT_DEPTH
      { unsigned long depth = levelFrame(FR);

	if ( depth > depth_reached )
	  depth_reached = depth;
	if ( depth > depth_limit )
	   FRAME_FAILED;
      }
#endif
	DEBUG(9, Sdprintf("Searching clause ... "));

	lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	if ( !(CL = firstClause(ARGP, FR, DEF, &deterministic PASS_LD)) )
	{ DEBUG(9, Sdprintf("No clause matching index.\n"));
	  FRAME_FAILED;
	}
	DEBUG(9, Sdprintf("Clauses found.\n"));

	if ( deterministic )
	  set(FR, FR_CUT);

	{ Clause clause = CL->clause;

	  PC = clause->codes;
	  lTop = (LocalFrame)(ARGP + clause->variables);
	}

	SECURE(
	int argc; int n;
	argc = DEF->functor->arity;
	for(n=0; n<argc; n++)
	  checkData(argFrameP(FR, n));
	);

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Leave the clause:

  - update reference of current clause
    If there are no alternatives left and BFR  <=  frame  we  will
    never  return  at  this clause and can decrease the reference count.
    If BFR > frame the backtrack frame is a child of  this  frame, 
    so  this frame can become active again and we might need to continue
    this clause.

  - update BFR
    `BFR' will become the backtrack frame of other childs  of  the
    parent  frame  in which we are going to continue.  If this frame has
    alternatives and is newer than the old backFrame `BFR'  should
    become this frame.

    If there are no alternatives and  the  BFR  is  this  one  the
    BFR can become this frame's backtrackframe.

  - Update `lTop'.
    lTop can be set to this frame if there are no alternatives  in  this
    frame  and  BFR  is  older  than this frame (e.g. there are no
    frames with alternatives that are newer).

  - restore machine registers from parent frame
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
      {				MARK(I_EXIT);
    exit_builtin:
        if ( FR->clause )
	{ if ( FR > BFR )
	    SetBfr(FR);
	} else
	{ assert(BFR <= FR);
	  if ( BFR == FR )
	    SetBfr(FR->backtrackFrame);
	  lTop = FR;
	}
	goto normal_exit;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
i_exitfact is generated to close a fact. The reason for not generating a
plain I_EXIT is first of all that the actual sequence should be I_ENTER,
I_EXIT,  and  just  optimising   to    I_EXIT   looses   the  unify-port
interception. Second, there should be some room for optimisation here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_EXITFACT, COUNT(i_exitfact), ("exitfact ")) MARK(EXITFACT);
#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ switch(tracePort(FR, BFR, UNIFY_PORT, PC))
	  { case ACTION_RETRY:
	      goto retry;
	  }
	}
#endif /*O_DEBUGGER*/
	/*FALLTHROUGH*/

    VMI(I_EXIT, COUNT(i_exit), ("exit ")) MARK(EXIT);
	if ( false(FR, FR_CUT) )
	{ if ( FR > BFR )			/* alternatives */
	    SetBfr(FR);
	} else
	{ if ( BFR <= FR )			/* deterministic */
	  { if ( BFR == FR )
	      SetBfr(FR->backtrackFrame);
	    leaveDefinition(DEF);
	    lTop = FR;
	  }
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First, call the tracer. Basically,  the   current  frame is garbage, but
given that the tracer might need to print the variables, we have to be a
bit more careful.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    normal_exit:
#if O_DEBUGGER
	if ( debugstatus.debugging )
        { LocalFrame mintop;
	  int action;

	  LocalFrame lSave = lTop;
	  environment_frame = FR;

	  if ( false(DEF, FOREIGN) && CL )
	    mintop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	  else
	    mintop = (LocalFrame) argFrameP(FR, DEF->functor->arity);

	  if ( lTop < mintop )
	    lTop = mintop;

	  action = tracePort(FR, BFR, EXIT_PORT, PC);
	  lTop = lSave;
	  switch( action )
	  { case ACTION_RETRY:	goto retry;
	    case ACTION_FAIL:	set(FR, FR_CUT);
				FRAME_FAILED;
	  }
	}
#endif /*O_DEBUGGER*/

	if ( !FR->parent )		/* query exit */
	{ QF = QueryFromQid(qid);	/* may be shifted: recompute */
	  QF->solutions++;

	  assert(FR == &QF->frame);

	  if ( !BFR )			/* No alternatives */
	  { set(QF, PL_Q_DETERMINISTIC);
	    set(FR, FR_CUT);		/* execute I_CUT */
	    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);

#if O_DEBUGGER
	    if ( true(FR, FR_WATCHED) )
	      frameFinished(FR);
#endif
	  }

	  succeed;
	}

      {
#if O_DEBUGGER
	LocalFrame leave;

	leave = (true(FR, FR_WATCHED) && FR == lTop) ? FR : NULL;
#endif

	PC = FR->programPointer;
	environment_frame = FR = FR->parent;
	DEF = FR->predicate;
	ARGP = argFrameP(lTop, 0);

#if O_DEBUGGER
	if ( leave )
	  frameFinished(leave);
#endif
      }
	NEXT_INSTRUCTION;
      }	  
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TRACER RETRY ACTION

By default, retries the  current  frame.  If   another  frame  is  to be
retried, place the frame-reference, which  should   be  a  parent of the
current frame, in debugstatus.retryFrame and jump to this label. This is
implemented by returning retry(Frame) of the prolog_trace_interception/3
hook.

First, the system will leave any parent  frames. Next, it will undo back
to the call-port and finally, restart the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_DEBUGGER
retry:					MARK(RETRY);
{ LocalFrame rframe = debugstatus.retryFrame;
  LocalFrame fr;

  if ( !rframe )
    rframe = FR;
  debugstatus.retryFrame = NULL;

  if ( rframe->mark.globaltop == INVALID_GLOBALTOP )
  { Sdprintf("[Undo mark lost by garbage collection]\n");
    rframe = FR;
    assert(rframe->mark.globaltop != INVALID_GLOBALTOP);
  }

  Sdprintf("[Retrying frame %d running %s]\n",
	   (Word)rframe - (Word)lBase,
	   predicateName(rframe->predicate));

  for(fr = BFR; fr > rframe; fr = fr->backtrackFrame)
  { LocalFrame fr2;

    for(fr2 = fr; fr2->clause && fr2 > rframe; fr2 = fr2->parent)
    { DEBUG(3, Sdprintf("discard %d\n", (Word)fr2 - (Word)lBase) );
      leaveFrame(fr2);
      fr2->clause = NULL;
    }
  }

  environment_frame = FR = rframe;
  DEF = FR->predicate;
  Undo(FR->mark);
  SetBfr(FR);
  clear(FR, FR_CUT);
  lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);

  goto retry_continue;
}
#endif /*O_DEBUGGER*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The rest of this giant procedure handles backtracking.  There are  three
different ways we can get here:

  - Head unification code failed			(clause_failed)
    In this case we should continue with the next clause of the  current
    procedure  and  if we are out of clauses continue with the backtrack
    frame of this frame.

  - A foreign goal failed				(frame_failed)
    In this case we can continue at the backtrack frame of  the  current
    frame.

  - Body instruction failed				(body_failed)
    This can only occur since arithmetic is compiled.   Future  versions
    might incorporate more WAM instructions that can fail.  In this case
    we should continue with frame BFR.

In  all  cases,  once  the  right  frame  to  continue  is  found   data
backtracking  can be invoked, the registers can be reloaded and the main
loop resumed.

The argument stack is set back to its base as we cannot  be  sure  about
it's current value.

The `shallow_backtrack' entry is used from `deep_backtrack'  to  do  the
common part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A WAM instruction in the body wants to start backtracking.  If backtrack
frames have been created  after  this  frame  we  want  to  resume  that
backtrack frame.  In this case the current clause remains active.  If no
such frames are created the current clause fails.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

body_failed:				MARK(BKTRK);
  DEBUG(9, Sdprintf("body_failed\n"));
  if ( BFR > FR )
  { environment_frame = FR = BFR;
    goto resume_from_body;
  }

clause_failed:
  CL = CL->next;
  if ( !CL || true(FR, FR_CUT) )
    goto frame_failed;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resume frame FR.  CL points  to  the  next  (candidate)  clause.   First
indexing  is  activated  to find the next real candidate.  If this fails
the entire frame has failed, so we can continue at `frame_failed'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resume_frame:
  ARGP = argFrameP(FR, 0);
  Undo(FR->mark);			/* backtrack before clause indexing */

  if ( !(CL = findClause(CL, ARGP, FR, DEF, &deterministic)) )
    goto frame_failed;

  if ( deterministic )
    set(FR, FR_CUT);

  SetBfr(FR->backtrackFrame);
  aTop = aFloor;			/* reset to start, for interrupts */

  { Clause clause = CL->clause;

    PC = clause->codes;
    lTop = (LocalFrame) argFrameP(FR, clause->variables);
  }

  NEXT_INSTRUCTION;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deep backtracking part of the system.  This code handles the failure  of
the goal associated with `frame'.  This would have been simple if we had
not  to  update  the clause references.  The main control loop will walk
along the backtrack frame links until either it reaches the top goal  or
finds a frame that really has a backtrack point left (the sole fact that
a  frame  is backtrackframe does not guaranty it still has alternatives:
the alternative clause might be retracted).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

frame_failed:				MARK(FAIL);

  for(;;)
  { DEF = FR->predicate;

#ifdef O_PROFILE
    if (LD->statistics.profiling)
      DEF->profile_fails++;
#endif

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { switch( tracePort(FR, FR->backtrackFrame, FAIL_PORT, PC) )
      { case ACTION_RETRY:
	  goto retry;
	case ACTION_IGNORE:
	  Sfputs("ignore not (yet) implemented here\n", Sdout);
      }
    }
#endif /*O_DEBUGGER*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update references due to failure of this frame.  The references of  this
frame's  clause are already updated.  All frames that can be reached via
the parent links and are  created  after  the  backtrack  frame  can  be
visited for dereferencing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( false(DEF, FOREIGN) )
      leaveDefinition(DEF);
#if O_DEBUGGER
    if ( true(FR, FR_WATCHED) )
      frameFinished(FR);
#endif

    if ( !FR->backtrackFrame )			/* top goal failed */
    { register LocalFrame fr = FR->parent;

      for(; fr; fr = fr->parent)
        leaveFrame(fr);

      QF = QueryFromQid(qid);
      set(QF, PL_Q_DETERMINISTIC);

      fail;
    }

    { register LocalFrame fr = FR->parent;

      environment_frame = FR = FR->backtrackFrame;

      for( ; fr > FR; fr = fr->parent )
        leaveFrame(fr);
    }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
References except for this frame are OK again.  First fix the references
for this frame if it is a Prolog frame.  This  cannot  be  in  the  loop
above as we need to put CL on the next clause.  Dereferencing the clause
might free it!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
resume_from_body:

    DEF = FR->predicate;
    if ( true(FR, FR_CUT) )
      continue;
    if ( false(DEF, FOREIGN) && !(CL = CL->next) )
      continue;

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { Undo(FR->mark);			/* data backtracking to get nice */
					/* tracer output */

      switch( tracePort(FR, BFR, REDO_PORT, NULL) )
      { case ACTION_FAIL:	continue;
	case ACTION_IGNORE:	CL = NULL;
				goto exit_builtin;
	case ACTION_RETRY:	goto retry;
      }
    }
#endif /*O_DEBUGGER*/
    
    LD->statistics.inferences++;
#ifdef O_PROFILE
    if ( LD->statistics.profiling )
    { DEF->profile_fails++;		/* fake a failure! */
      DEF->profile_redos++;
    }
#endif /* O_PROFILE */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finaly restart.  If it is a Prolog frame this is the same as  restarting
as  resuming  a  frame after unification of the head failed.  If it is a
foreign frame we have to set BFR and do data backtracking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( is_signalled() )
    { PL_handle_signals();
      if ( exception_term )
	goto b_throw;
    }

    if ( (unsigned long)usedStack(global) < LD->stacks.global.gced_size )
      LD->stacks.global.gced_size = (unsigned long)usedStack(global);
    if ( (unsigned long)usedStack(trail) < LD->stacks.trail.gced_size )
      LD->stacks.trail.gced_size = (unsigned long)usedStack(trail);

    if ( false(DEF, FOREIGN) )
      goto resume_frame;

    SetBfr(FR->backtrackFrame);
    Undo(FR->mark);

    goto call_builtin;
  }
} /* end of PL_next_solution() */

#if O_COMPILE_OR
word
pl_alt(word h)
{ GET_LD

  switch( ForeignControl(h) )
  { case FRG_REDO:
    { int skip = ForeignContextInt(h);
      DEBUG(9, Sdprintf("$alt/1: skipping %ld codes\n", ForeignContextInt(h)));
      environment_frame->programPointer += skip;
    case FRG_CUTTED:
      succeed;
    }

    case FRG_FIRST_CALL:
    default:
      DEBUG(0, Sdprintf("*** [%ld] $alt/0: control = %d, context = %d\n",
			levelFrame(environment_frame),
			ForeignControl(h), ForeignContextInt(h)));
      assert(0);
      fail;
  }
}
#endif /* O_COMPILE_OR */
