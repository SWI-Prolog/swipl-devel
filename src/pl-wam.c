/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Virtual machine instruction interpreter
*/

/*#define O_SECURE 1*/
/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-itf.h"

#ifdef O_RLC
#include <console.h>
#endif

#if sun
#include <prof.h>			/* in-function profiling */
#else
#define MARK(label)
#endif

forwards void		copyFrameArguments(LocalFrame, LocalFrame, int);
forwards inline bool	callForeign(const Definition, LocalFrame);
forwards void		leaveForeignFrame(LocalFrame);
forwards inline void    Trail(Word, LocalFrame);

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
  int h_functor_n[256];
  int h_list;
  int b_functor_n[256];
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
#if O_COMPILE_ARITH
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
  countArray("B_REAL",	 	counting.b_real_n);  
  countArray("B_STRING", 	counting.b_string_n);  
  countOne(  "H_NIL", 		counting.h_nil);
  countArray("H_VAR", 		counting.h_var_n);  
  countArray("B_VAR", 		counting.b_var_n);  
  countArray("B_ARGVAR", 	counting.b_argvar_n);  
  countArray("H_FIRSTVAR", 	counting.h_firstvar_n);  
  countArray("B_FIRSTVAR", 	counting.b_firstvar_n);  
  countArray("B_ARGFIRSTVAR", 	counting.b_argfirstvar_n);  
  countOne(  "H_VOID", 		counting.h_void);
  countOne(  "B_VOID", 		counting.b_void);
  countArray("H_FUNCTOR", 	counting.h_functor_n);  
  countOne(  "H_LIST", 		counting.h_list);  
  countArray("B_FUNCTOR", 	counting.b_functor_n);  
  countOne(  "I_POP", 		counting.i_pop);
  countArray("I_POPN", 		counting.i_pop_n);  
  countOne(  "I_ENTER", 	counting.i_enter);
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
  countOne(  "I_FAIL",		countOne.i_fail);
  countOne(  "I_TRUE",		countOne.i_true);

  succeed;
}

static void
countHeader()
{ int m;

  Putf("%13s: ", "Instruction");
  for(m=0; m < 20; m++)
    Putf("%8d", m);
  Putf("\n");
  for(m=0; m<(15+20*8); m++)
    Putf("=");
  Putf("\n");
}  

static void
countArray(s, array)
char *s;
int *array;
{ int n, m;

  for(n=255; array[n] == 0; n--) ;
  Putf("%13s: ", s);
  for(m=0; m <= n; m++)
    Putf("%8d", array[m]);
  Putf("\n");
}

static void
countOne(s, i)
char *s;
int i;
{ Putf("%13s: %8d\n", s, i);
}

#define COUNT_N(name)  { counting.name[*PC]++; }
#define COUNT_2N(name) { counting.name[*PC]++; counting.name[PC[1]+256]++; }
#define COUNT(name)    { counting.name++; }
#else /* ~COUNTING */
#define COUNT_N(name)
#define COUNT_2N(name)
#define COUNT(name)
#endif /* COUNTING */


#include "pl-index.c"
#include "pl-alloc.c"

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

static inline bool
callForeign(const Definition def, LocalFrame frame)
{ int argc = def->functor->arity;
  word result;
  Word argv[20]; /* this must be as big as the number args we stuff in it 
                  * (switch() below can handle upto case : 15) */
  Func function;
  Lock top = pTop;
  bool gc_save;

  { Word a, *ap;
    int n;

    a = argFrameP(frame, 0);
    lTop = (LocalFrame) argFrameP(a, argc);
    for(ap = argv, n = argc; n > 0; n--, a++, ap++)
      deRef2(a, *ap)
  }

  DEBUG(7, Sdprintf("Calling built in %s\n", predicateName(def)) );

  SECURE(
  int n;
  DEBUG(5, Sdprintf("argc = %d, argv = 0x%x\n", argc, (unsigned long) argv));
  for(n = 0; n < argc; n++)
  { checkData(argv[n], FALSE);
    lockp(&argv[n]);
  }
  );

  function = def->definition.function;

#define A(n) argv[n]
#define F (*function)

  gc_status.blocked++;
  if ( (gc_save = true(def, GC_SAFE)) )
    lockp(&frame);
#if O_SHIFT_STACKS
  else
    shift_status.blocked++;
#endif

  if ( true(def, NONDETERMINISTIC) )
  { word B = (word) frame->clause;

  switch(argc)
  { case 0:  result = F(B); break;
    case 1:  result = F(A(0), B); break;
    case 2:  result = F(A(0), A(1), B); break;
    case 3:  result = F(A(0), A(1), A(2), B); break;
    case 4:  result = F(A(0), A(1), A(2), A(3), B); break;
    case 5:  result = F(A(0), A(1), A(2), A(3), A(4), B); break;
    case 6:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), B); break;
    case 7:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), B); break;
    case 8:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			B); break;
    case 9:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), B); break;
    case 10: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), A(9), B); break;
#if !mips				/* MIPS doesn't handle that many */
    case 11: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), A(9), A(10), B); break;
    case 12: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), A(9), A(10), A(11), B); break;
    case 13: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), A(9), A(10), A(11), A(12), B); break;
    case 14: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), A(9), A(10), A(11), A(12), A(13), B); break;
    case 15: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			A(8), A(9), A(10), A(11), A(12), A(13), A(14),
			B); break;
#endif
    default:	return sysError("Too many arguments to foreign function");
  }
  } else /* deterministic predicate call */
  { switch(argc)
    { case 0:  result = F(); break;
      case 1:  result = F(A(0)); break;
      case 2:  result = F(A(0), A(1)); break;
      case 3:  result = F(A(0), A(1), A(2)); break;
      case 4:  result = F(A(0), A(1), A(2), A(3)); break;
      case 5:  result = F(A(0), A(1), A(2), A(3), A(4)); break;
      case 6:  result = F(A(0), A(1), A(2), A(3), A(4), A(5)); break;
      case 7:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6)); break;
      case 8:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7));
      			break;
      case 9:  result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8)); break;
      case 10: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8), A(9)); break;
#if !mips				/* MIPS doesn't handle that many */
      case 11: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8), A(9), A(10)); break;
      case 12: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8), A(9), A(10), A(11)); break;
      case 13: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8), A(9), A(10), A(11), A(12)); break;
      case 14: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8), A(9), A(10), A(11), A(12), A(13)); break;
      case 15: result = F(A(0), A(1), A(2), A(3), A(4), A(5), A(6), A(7),
			  A(8), A(9), A(10), A(11), A(12), A(13), A(14));
			break;
#endif
      default:	return sysError("Too many arguments to foreign function");
    }
  }
  
  if ( gc_save )
    unlockp(&frame);
#if O_SHIFT_STACKS
  else
    shift_status.blocked--;
#endif
  gc_status.blocked--;

#undef F
#undef A

  SECURE(
  int n;
  DEBUG(5, Sdprintf("argc = %d, argv = 0x%x\n", argc, (unsigned long) argv));
  for(n=argc-1; n >= 0; n--)
  { unlockp(&argv[n]);
    checkData(argv[n], FALSE);
  }
  );

  if ( top != pTop )
  { if ( pTop > top )
    { warning("%s: left %d foreign data marks",
	      predicateName(def), pTop - top);
      pTop = top;
    } else
    { warning("%s: popped %d foreign data marks too many",
	      predicateName(def), top - pTop);
    }
  }

  if ( result == FALSE )
  { frame->clause = NULL;
    fail;
  } else if ( result == TRUE )
  { frame->clause = NULL;
    succeed;
  } else
  { if ( true(def, NONDETERMINISTIC) )
    { if ( !result & FRG_MASK )
      { warning("Illegal return value from foreign predicate %s: 0x%x",
				    predicateName(def), result);
	fail;
      }
      frame->clause = (ClauseRef) result;
      succeed;
    }
    warning("Deterministic foreign predicate %s returns 0x%x",
			    predicateName(def), result);
    fail;
  }
}

static void
leaveForeignFrame(LocalFrame fr)
{ Definition def = fr->predicate;
  Func f = def->definition.function;
  word context = (word) fr->clause | FRG_CUT;

#define U ((Word) NULL)
  DEBUG(5, Sdprintf("Cut %s, context = 0x%lx\n",
		    predicateName(def), context));

  switch(def->functor->arity)
  { case 0:	(*f)(context);					return;
    case 1:	(*f)(U, context);				return;
    case 2:	(*f)(U, U, context);				return;
    case 3:	(*f)(U, U, U, context);				return;
    case 4:	(*f)(U, U, U, U, context);			return;
    case 5:	(*f)(U, U, U, U, U, context);			return;
    case 6:	(*f)(U, U, U, U, U, U, context);		return;
    case 7:	(*f)(U, U, U, U, U, U, U, context);		return;
    case 8:	(*f)(U, U, U, U, U, U, U, U, context);		return;
    case 9:	(*f)(U, U, U, U, U, U, U, U, U, context);	return;
    case 10:	(*f)(U, U, U, U, U, U, U, U, U, U, context);	return;
    default:	sysError("Too many arguments (%d) to leaveForeignFrame()",
			 def->functor->arity);
  }
#undef U
}


		 /*******************************
		 *	     TRAILING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trail an assignment.  This function  is  now   local  to  this module to
exploit inlining facilities provided  by   good  C-compilers.  Note that
-when using dynamic stacks-, the  assignment   should  be  made *before*
calling Trail()!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
Trail(Word p, LocalFrame fr)
{ if ( fr && p > fr->mark.globaltop && p < gTop )
    return;

  (tTop++)->address = p;
  SECURE(assert(!isTrailValueP(p)));
  verifyStack(trail);
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
TrailAssignment(Word p)
{ Word old = allocGlobal(1);

  *old = *p;				/* save the old value on the global */
  (tTop++)->address = p;
  (tTop++)->address = makeTrailValueP(old);
  verifyStack(trail);
}

#ifdef __WIN32__
void
#else
inline void
#endif
do_undo(mark *m)
{ TrailEntry tt = tTop;

  while(tt > m->trailtop)
  { tt--;
    if ( isTrailValueP(tt->address) )
    { word val = *trailValueP(tt->address);

      tt--;
      *tt->address = val;
    } else
      setVar(*tt->address);
  }
  tTop = tt;
  gTop = m->globaltop;
}

#endif /*O_DESTRUCTIVE_ASSIGNMENT*/

		/********************************
		*          UNIFICATION          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify is the general unification procedure.   This  raw  routine  should
only be called by interpret as it does not undo bindings made during the
unification  in  case  the  unification fails.  pl_unify() (implementing
=/2) does undo bindings and should be used by foreign predicates.

Unification depends on the datatypes available in the system and will in
general need updating if new types are added.  It should be  noted  that
unify()  is  not  the only place were unification happens.  Other points
are:
  - various of the virtual machine instructions
  - various macros, for example APPENDLIST and CLOSELIST
  - unifyAtomic(), unifyFunctor(): unification of atomic data.
  - various builtin predicates. They should be flagged some way.

The Gould does not accept the construct (word)t1 = *t1.  This implies we
have to define extra variables, slowing down execution a bit (on the SUN
this trick saves about 10% on this function).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef TAGGED_LVALUE
#define w1 ((word)t1)
#define w2 ((word)t2)
#endif

bool
unify(register Word t1, register Word t2, LocalFrame fr)
{
#ifndef TAGGED_LVALUE
  register word w1, w2;
#endif

  deRef(t1);  
  deRef(t2);

  if (isVar(*t1) )
  { if (isVar(*t2) )
    { if (t1 < t2)		/* always point downwards */
      { *t2 = makeRef(t1);
        Trail(t2, fr);
	succeed;
      }
      if (t1 == t2)
	succeed;
      *t1 = makeRef(t2);
      Trail(t1, fr);
      succeed;
    }
    *t1 = *t2;
    Trail(t1, fr);
    succeed;
  }
  if (isVar(*t2) )
  { *t2 = *t1;
    Trail(t2, fr);
    succeed;
  }

  if ( (w1 = *t1) == (w2 = *t2) )
    succeed;
  if ( mask(w1) != mask(w2) )
    fail;

  if ( mask(w1) != 0 )
  { if ( !isIndirect(w1) )
      fail;
#if O_STRING
    if ( isString(w1) && isString(w2) && equalString(w1, w2) )
      succeed;
#endif /* O_STRING */
    if ( isReal(w1) && isReal(w2) && valReal(w1) == valReal(w2) )
      succeed;
    fail;
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now both w1 and w2 can still represent a term or an atom.  If  both  are
atoms  they are not the same atom.  We can do a quick and dirty test for
atom as it is not a variable, nor a masked type.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { register int arity;
    register FunctorDef fd;

    if ( pointerIsAtom(w1) || 
	 pointerIsAtom(w2) ||
	 (fd = functorTerm(w1)) != functorTerm(w2) )
      fail;

    arity = fd->arity;
    t1 = argTermP(w1, 0);
    t2 = argTermP(w2, 0);
    for(; arity > 0; arity--, t1++, t2++)
      if ( !unify(t1, t2, fr) )
	fail;
  }

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unify_atomic(p, a) is normally called through unifyAtomic(). It  unifies
a  term,  represented  by  a pointer to it, with an atomic value.  It is
intended for foreign language functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unify_atomic(register Word p, word a)
{ deRef(p);

  if (*p == a)
    succeed;

  if (isVar(*p) )
  { *p = a;
    Trail(p, environment_frame);
    succeed;
  }

  if (isIndirect(a) && isIndirect(*p) )
  { if (isReal(a) && isReal(*p) && valReal(a) == valReal(*p))
      succeed;
#if O_STRING
    if (isString(a) && isString(*p) && equalString(a, *p))
      succeed;
#endif /* O_STRING */
  }

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify a (pointer to a) term with a functor (is name/arity pair).  If the
term is instantiated to a term of the name and arity  indicated  by  the
functor  this  call just succeeds.  If the term is a free variable it is
bound to a term whose arguments are all variables.  Otherwise this  call
fails.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unifyFunctor(register Word term, register FunctorDef functor)
{ if (functor->arity == 0)
    return unifyAtomic(term, functor->name);

  deRef(term);

  if (isVar(*term) )
  { *term = globalFunctor(functor);
    Trail(term, environment_frame);
    succeed;
  }
  if (isTerm(*term) && functorTerm(*term) == functor)
    succeed;

  fail;
}

#if O_BLOCK
		/********************************
		*         BLOCK SUPPORT         *
		*********************************/

static LocalFrame
findBlock(LocalFrame fr, Word block)
{ for(; fr; fr = fr->parent)
  { if ( fr->predicate == PROCEDURE_block3->definition &&
	 pl_unify(argFrameP(fr, 0), block) )
      return fr;
  }

  warning("Can't find block");

  return NULL;
}

#endif /*O_BLOCK*/

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

#ifdef O_SECURE
#define SetBfr(fr)		(BFR = (fr))
#else
#define SetBfr(fr) \
    do { \
         assert(!(fr) || (fr)->mark.trailtop != INVALID_TRAILTOP); \
         BFR = (fr); \
       } while(0)
#endif

#if O_SHIFT_STACKS
#define register
#endif

bool
PL_call_predicate(Module Context, bool debug, Procedure proc, Word *argv)
{ register   LocalFrame FR;		/* current frame */
  register   Word	ARGP;		/* current argument pointer */
  register   Code	PC;		/* program counter */
  LocalFrame BFR;			/* last backtrack frame */
  Definition DEF;			/* definition of current procedure */
  bool	     deterministic;		/* clause found deterministically */
#define	     CL (FR->clause)		/* clause of current frame */

  LocalFrame lSave   = lTop;		/* call-back status info */
  Lock	     pSave   = pTop;
  LocalFrame envSave = environment_frame;
  Word *     aSave   = aTop;

#if O_LABEL_ADDRESSES
  static void *jmp_table[] =
  { &&I_NOP_LBL,
    &&I_ENTER_LBL,
    &&I_CALL_LBL,
    &&I_DEPART_LBL,
    &&I_EXIT_LBL,
    &&B_FUNCTOR_LBL,
    &&H_FUNCTOR_LBL,
    &&I_POP_LBL,
    &&I_POPN_LBL,
    &&B_VAR_LBL,
    &&H_VAR_LBL,
    &&B_CONST_LBL,
    &&H_CONST_LBL,
    &&H_REAL_LBL,
#if O_STRING
    &&H_STRING_LBL,
#endif /* O_STRING */

    &&B_FIRSTVAR_LBL,
    &&H_FIRSTVAR_LBL,
    &&B_VOID_LBL,
    &&H_VOID_LBL,
    &&B_ARGFIRSTVAR_LBL,
    &&B_ARGVAR_LBL,

    &&H_NIL_LBL,
    &&H_LIST_LBL,

    &&B_VAR0_LBL,
    &&B_VAR1_LBL,
    &&B_VAR2_LBL,

    &&I_USERCALL0_LBL,
    &&I_USERCALLN_LBL,
    &&I_CUT_LBL,
    &&I_APPLY_LBL,

#if O_COMPILE_ARITH
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

    &&B_REAL_LBL,
    &&B_STRING_LBL,
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
    NULL
  };

#define VMI(Name, Count, Msg)	Name ## _LBL: Count; DEBUG(8, Sdprintf Msg);
#if VMCODE_IS_ADDRESS
#define NEXT_INSTRUCTION	goto *(void *)((int)(*PC++))
#else
#define NEXT_INSTRUCTION	goto *jmp_table[*PC++]
#endif

#else /* O_LABEL_ADDRESSES */

#define VMI(Name, Count, Msg)	case Name: Count; DEBUG(8, Sdprintf Msg);
#define NEXT_INSTRUCTION	goto next_instruction

#endif /* O_LABEL_ADDRESSES */

#if VMCODE_IS_ADDRESS
  interpreter_jmp_table = jmp_table;	/* make it globally known */
#endif /* VMCODE_IS_ADDRESS */

  DEBUG(1, { extern int Output;		/* --atoenne-- */
	     FunctorDef f = proc->definition->functor;

	     if ( Output )
	     { int n;

	       Putf("Interpret: %s(", stringAtom(f->name));
	       for(n=0; n < f->arity; n++)
	       { if ( n > 0 )
		   Putf(", ");
		 pl_write(argv[n]);
	       }
	       Putf(")\n");
	     } else
	       Sdprintf("Interpret goal in unitialized environment.\n");
	   });

  /* Allocate a local stack frame */

  gc_status.blocked++;

  lTop = (LocalFrame) addPointer(lTop, sizeof(word));
  verifyStack(local);
  varFrame(lTop, -1) = (word) environment_frame;

  FR = lTop;
  FR->parent = (LocalFrame) NULL;

#define RESTORE_REGS() do { lTop = lSave; \
			    aTop = aSave; \
			    environment_frame = envSave; \
			    gc_status.blocked--; \
			    assert(pSave == pTop); } while(0)

#if O_SHIFT_STACKS
  lockp(&FR);
  lockp(&BFR);
  lockp(&ARGP);
  lockp(&lSave);
  lockp(&envSave);

#define RETURN(val) do \
	            { unlockp(&envSave); \
		      unlockp(&lSave); \
		      unlockp(&ARGP); \
		      unlockp(&BFR); \
		      unlockp(&FR); \
		      RESTORE_REGS(); \
		      return(val); \
		    } while(0)
#else
#define RETURN(val) do \
	            { RESTORE_REGS(); \
		      return(val); } while(0)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the procedure definition associated with `Goal'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

					/* machine registers */
  { int n, arity;

    DEF   = proc->definition;
    arity = DEF->functor->arity;

					/* fill frame arguments */
    ARGP = argFrameP(FR, 0);
    for( n = arity; n-- > 0; argv++ )
      *ARGP++ = isVar(**argv) ? makeRef(*argv) : **argv;

					/* find definition and clause */
    if ( (CL = DEF->definition.clauses) == NULL &&
         false(DEF, DYNAMIC) )
    { lTop = (LocalFrame) argFrameP(FR, arity);
      DEF = trapUndefined(DEF);
      CL = DEF->definition.clauses;
    }
					/* context module */
    if ( true(DEF, TRANSPARENT) )
    { if ( Context )
	FR->context = Context;
      else if ( environment_frame )
	FR->context = environment_frame->context;
      else
	FR->context = DEF->module;
    } else
      FR->context = DEF->module;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finally fill all the slots of  the  frame  and  initialise  the  machine
registers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    clearFlags(FR);
    setLevelFrame(FR, !parentFrame(FR) ? 0L : levelFrame(parentFrame(FR)) + 1);
    if ( !debug )
      set(FR, FR_NODEBUG);
    FR->backtrackFrame = (LocalFrame) NULL;
    FR->predicate = DEF;
    DoMark(FR->mark);
    SetBfr(FR);
    environment_frame = FR;


#ifdef O_PROFILE
    if (statistics.profiling)
      DEF->profile_calls++;
#endif /* O_PROFILE */

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { LocalFrame lTopSave = lTop;
      int action;

      lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
      action = tracePort(FR, CALL_PORT);
      lTop = lTopSave;
      switch(action)
      { case ACTION_FAIL:	RETURN(FALSE);
	case ACTION_IGNORE:	RETURN(TRUE);
      }
    }
#endif /*O_DEBUGGER*/

    if ( true(DEF, FOREIGN) )
    { bool rval;

      FR->clause = FIRST_CALL;
      rval = callForeign(DEF, FR);
#if O_DEBUGGER      
      if ( debugstatus.debugging )
      { LocalFrame lTopSave = lTop;
	int action;

	lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	action = tracePort(FR, rval ? EXIT_PORT : FAIL_PORT);
	lTop = lTopSave;
	switch(action)
	{ case ACTION_FAIL:	rval = FALSE;
	  			break;
	  case ACTION_IGNORE:	rval = TRUE;
	}
      }
#endif /*O_DEBUGGER*/
      RETURN(rval);			/* need trace call */
    }

    ARGP = argFrameP(FR, 0);
    if ( !(CL = firstClause(ARGP, DEF, &deterministic)) )
    { bool rval = FALSE;
#if O_DEBUGGER
      if ( debugstatus.debugging )
      { LocalFrame lTopSave = lTop;
	int action;

	lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	action = tracePort(FR, FAIL_PORT);
	lTop = lTopSave;
	switch(action)
	{ case ACTION_FAIL:	rval = FALSE;
	  			break;
	  case ACTION_IGNORE:	rval = TRUE;
	}
      }
#endif /*O_DEBUGGER*/
      RETURN(rval);			/* need trace call */
    }

    if ( deterministic )
      set(FR, FR_CUT);
    DEF->references++;

    { Clause clause = CL->clause;

      PC = clause->codes;
      lTop = (LocalFrame) argFrameP(FR, clause->variables);
    }
  }

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
  switch( *PC++ )
#endif
  { VMI(I_NOP, COUNT(i_nop), ("i_nop\n"))
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
        c = (word) ATOM_nil;

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
Real in the head. This is unlikely, but some poeple seem to use it. We have
to copy the real on the global stack as the user might retract the clause:
(this is a bit silly programming, but it should not crash)
    x(3.4).
    run :- x(X), retractall(x(_)), Y is X * 2, assert(x(Y)).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_REAL, COUNT(h_real), ("h_real %d\n", *PC))	MARK(HREAL);
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ *k = globalReal(valReal((word)*PC++));
	  Trail(k, FR);
	  NEXT_INSTRUCTION;
	}
	if (isReal(*k) && valReal(*k) == valReal((word)*PC++))
	  NEXT_INSTRUCTION;
	CLAUSE_FAILED;
      }

#if O_STRING
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
String in the head. See H_REAL and H_CONST for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(H_STRING, COUNT(h_string), ("h_string %d\n", *PC)) MARK(HSTR);
      { register Word k;

	deRef2(ARGP++, k);
	if (isVar(*k))
	{ word str = *PC++;
	  *k = globalString(valString(str));
	  Trail(k, FR);
	  NEXT_INSTRUCTION;
	}
	if ( isString(*k) )
	{ word str = *PC++;
	  if ( equalString(*k, str) )
	    NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;
      }
#endif /* O_STRING */

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_REAL and B_STRING need to copy the value on the global  stack  because
the XR-table might be freed due to a retract.  We should write fast copy
algorithms,   especially   for   the   expensive   globalReal(valReal())
construct.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_REAL, COUNT_N(b_real_n), ("b_real %d\n", *PC)) MARK(BREAL);
      { *ARGP++ = globalReal(valReal((word)*PC++));
	NEXT_INSTRUCTION;
      }

    VMI(B_STRING, COUNT_N(b_string_n), ("b_string %d\n", *PC)) MARK(BSTRING);
      { *ARGP++ = globalString(valString((word)*PC++));
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the head which is not an anonymous one and is not used for
the first time.  Invoke general unification between the argument pointer
and the variable, whose offset is given relative to  the  frame.   Note:
this once was done in place to avoid a function call.  It turns out that
using a function call is faster (at least on SUN_3).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_VAR, COUNT_N(h_var_n), ("h_var %d\n", *PC)) MARK(HVAR);
      { if (unify(varFrameP(FR, *PC++), ARGP++, FR) )
	  NEXT_INSTRUCTION;
	CLAUSE_FAILED;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variable in the body which is not an anonymous one, is  not  used  for
the  first  time  and is nested in a term (with B_FUNCTOR).  We now know
that *ARGP is a variable,  so  we  either  copy  the  value  or  make  a
reference.   The  difference between this one and B_VAR is the direction
of the reference link in case *k turns out to be variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_ARGVAR, COUNT_N(b_argvar_n), ("b_argvar %d\n", *PC)) MARK(BAVAR);
      { register Word k;

	deRef2(varFrameP(FR, *PC++), k);	
	if (isVar(*k))
	{ if (ARGP < k)
	  { setVar(*ARGP);
	    *k = makeRef(ARGP++);
	    Trail(k, FR);
	    NEXT_INSTRUCTION;
	  }
	  *ARGP++ = makeRef(k);		/* both on global stack! */
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

#define BODY_VAR(n)   { register Word k; \
			deRef2(varFrameP(FR, (n)), k); \
			*ARGP++ = (isVar(*k) ? makeRef(k) : *k); \
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
A variable in the head, which is not anonymous, but encountered for  the
first  time.  So we know that the variable is still a variable.  Copy or
make a reference.  Trailing is not needed as  we  are  writing  in  this
frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(H_FIRSTVAR, COUNT_N(h_firstvar_n), ("h_firstvar %d\n", *PC))
      MARK(HFVAR);
      { varFrame(FR, *PC++) = (isVar(*ARGP) ? makeRef(ARGP++)
					       : *ARGP++);
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
	varFrame(FR, *PC++) = makeRef(ARGP++);
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
      { register Word k = varFrameP(FR, *PC++);

	setVar(*k);
	*ARGP++ = makeRef(k);
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
      { register FunctorDef fdef;

	fdef = (FunctorDef) *PC++;

	*aTop++ = ARGP + 1;
	verifyStack(argument);
        deRef(ARGP);
	if (isVar(*ARGP) )
	{ *ARGP = globalFunctor(fdef);
	  Trail(ARGP, FR);
	  ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	if (isTerm(*ARGP) && functorTerm(*ARGP) == fdef)
	{ ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;	    
    VMI(H_LIST, COUNT(h_list), ("h_list\n")) MARK(HLIST);
	*aTop++ = ARGP + 1;
	verifyStack(argument);
	deRef(ARGP);
	if (isVar(*ARGP) )
	{ 
#if O_SHIFT_STACKS
  	  if ( gTop + 3 > gMax )
	  { growStacks(FR, PC, FALSE, TRUE, FALSE);
	    STACKVERIFY( if (gTop + 3 > gMax) outOf((Stack)&stacks.global) );
	  }
#else
	  STACKVERIFY( if (gTop + 3 > gMax) outOf((Stack)&stacks.global) );
#endif
	  *ARGP = (word) gTop;
	  Trail(ARGP, FR);
	  *gTop++ = (word) FUNCTOR_dot2;
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
    VMI(B_FUNCTOR, COUNT_N(b_functor_n), ("b_functor %d\n", *PC)) MARK(BFUNC);
      { register FunctorDef fdef = (FunctorDef) *PC++;

	*ARGP = (word) gTop;
	*aTop++ = ++ARGP;
	verifyStack(argument);
	*gTop++ = (word) fdef;
	ARGP = gTop;
	gTop += fdef->arity;
	verifyStack(global);

	NEXT_INSTRUCTION;
      }
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop the saved argument pointer (see H_FUNCTOR and B_FUNCTOR).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(I_POP, COUNT(i_pop), ("pop\n")) MARK(POP);
      { ARGP = *--aTop;
	NEXT_INSTRUCTION;
      }
    VMI(I_POPN, COUNT_N(i_pop_n), ("popn %d\n", *PC)) MARK(POPN);
      { aTop -= *PC++;
	ARGP = *aTop;
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
      { ARGP = argFrameP(lTop, 0);

#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ tracePort(FR, UNIFY_PORT);
	  SetBfr(FR);
	} else
#endif /*O_DEBUGGER*/
	{ if ( true(FR, FR_CUT ) )
	  { SetBfr(FR->backtrackFrame);
	  } else
	  { SetBfr(FR);
	  }
	}

	NEXT_INSTRUCTION;
      }

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
	{ BODY_FAILED;
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
	SetBfr(debugstatus.debugging ? blockfr : blockfr->backtrackFrame);
#else
	SetBfr(blockfr->backtrackFrame);
#endif
	for(fr = FR; fr > blockfr; fr = fr->parent)
	{ set(fr, FR_CUT);
	  fr->backtrackFrame = BFR;
	}

	DEBUG(3, Sdprintf("BFR = %d\n", (Word)BFR - (Word)lBase) );

	if ( unify(argFrameP(blockfr, 2), rval, environment_frame) ) /*???*/
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
	{ BODY_FAILED;
	}
	
#ifdef O_DEBUGGER
	SetBfr(debugstatus.debugging ? cutfr : cutfr->backtrackFrame);
#else
	SetBfr(cutfr->backtrackFrame);
#endif
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

	set(FR, FR_CUT);
	for(fr = BFR; fr > FR; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > FR; fr2 = fr2->parent)
	  { DEBUG(3, Sdprintf("discard frame of %s\n",
			      predicateName(fr2->predicate)));
	    leaveFrame(fr2);
	    fr2->clause = NULL;
	  }
	}
#ifdef O_DEBUGGER
	SetBfr(debugstatus.debugging ? FR : FR->backtrackFrame);
#else
	SetBfr(FR->backtrackFrame);
#endif

	DEBUG(3, Sdprintf("BFR = %d\n", (Word)BFR - (Word)lBase) );
	lTop = (LocalFrame) argFrameP(FR, CL->clause->variables);
	ARGP = argFrameP(lTop, 0);

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
	if ( cbfr < FR )
	  cbfr = FR;

	for(fr = BFR; fr > cbfr; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > cbfr; fr2 = fr2->parent)
	  { DEBUG(3, Sdprintf("discard %d: ", (Word)fr2 - (Word)lBase) );
	    DEBUG(3, writeFrameGoal(fr2, 2); pl_nl() );
	    leaveFrame(fr2);
	    fr2->clause = NULL;
	  }
	}

	DEBUG(3, Putf("BFR at "); writeFrameGoal(BFR, 2); pl_nl() );
	{ int nvar = (true(cbfr->predicate, FOREIGN)
				? cbfr->predicate->functor->arity
				: cbfr->clause->clause->variables);
	  lTop = (LocalFrame) argFrameP(cbfr, nvar);
	  ARGP = argFrameP(lTop, 0);
	}
        SetBfr(obfr);

        NEXT_INSTRUCTION;
      }

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
Arithmic is compiled using a stack  machine.   ARGP  is  used  as  stack
pointer and the stack is arithmic stack is allocated on top of the local
stack,  starting  at  the  argument  field of the next slot of the stack
(where ARGP points to when processing the body anyway).

Arguments to functions are pushed on the stack  starting  at  the  left,
thus `add1(X, Y) :- Y is X + 1' translates to:

    I_ENTER	% enter body
    B_VAR 0	% push X via ARGP
    B_CONST 0	% push `1' via ARGP
    A_FUNC2 N	% execute arithmic function 'N' (+/2), leaving X+1 on
		% the stack
    A_IS 	% unify top of stack ('X+1') with Y
    EXIT	% leave the clause

a_func0:	% executes arithmic function without arguments, pushing
		% its value on the stack
a_func1:	% unary function. Changes the top of the stack.
a_func2:	% binary function. Pops two values and pushes one.

Note that we do not call `ar_func0(*PC++, &ARGP)' as ARGP is a register
variable.  Also, for compilers that do register allocation it is unwise
to give the compiler a hint to put ARGP not into a register.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_FUNC0, COUNT_N(a_func0), ("a_func0 %d\n", *PC)) MARK(A_FUNC0);
      {	Word argp = ARGP;
	if ( ar_func_n(*PC++, 0, &argp) == FALSE )
	  BODY_FAILED;
	ARGP = argp;
				DEBUG(8, Sdprintf("ARGP = 0x%lx; top = ",
						(unsigned long)ARGP);
					 pl_write(ARGP-1);
					 Sdprintf("\n"));
	NEXT_INSTRUCTION;
      }

    VMI(A_FUNC1, COUNT_N(a_func1), ("a_func1 %d\n", *PC)) MARK(A_FUNC1);
      {	Word argp = ARGP;
	if ( ar_func_n(*PC++, 1, &argp) == FALSE )
	  BODY_FAILED;
	ARGP = argp;
				DEBUG(8, Sdprintf("ARGP = 0x%lx; top = ",
						(unsigned long)ARGP);
					 pl_write(ARGP-1);
					 Sdprintf("\n"));
	NEXT_INSTRUCTION;
      }

    VMI(A_FUNC2, COUNT_N(a_func2), ("a_func2 %d\n", *PC)) MARK(A_FUNC2);
      {	Word argp = ARGP;
				DEBUG(8, Sdprintf("ARGP = 0x%lx; top = ",
						(unsigned long)ARGP);
					 pl_write(ARGP-2); Sdprintf(" & ");
					 pl_write(ARGP-1);
					 Sdprintf("\n"));
	if ( ar_func_n(*PC++, 2, &argp) == FALSE )
	  BODY_FAILED;
	ARGP = argp;
				DEBUG(8, Sdprintf("ARGP = 0x%lx; top = ",
						(unsigned long)ARGP);
					 pl_write(ARGP-1);
					 Sdprintf("\n"));
	NEXT_INSTRUCTION;
      }

    VMI(A_FUNC, COUNT_N(a_func), ("a_func %d %d\n",*PC,PC[1])) MARK(A_FUNC);
      {	Word argp = ARGP;
	if ( ar_func_n(*PC++, *PC++, &argp) == FALSE )
	  BODY_FAILED;
	ARGP = argp;
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of the arithmic comparison predicates (<, >, =<,  >=,  =:=).
Both sides are pushed on the stack, so we just compare the two values on
the  top  of  this  stack  and  backtrack  if  they  do  not suffice the
condition.  Example translation: `a(Y) :- b(X), X > Y'

    ENTER
    B_FIRSTVAR 1	% Link X from B's frame to a new var in A's frame
    CALL 0		% call b/1
    B_VAR 1		% Push X
    B_VAR 0		% Push Y
    A_GT		% compare
    EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_LT, COUNT(a_lt), ("a_lt\n")) MARK(A_LT);
      { ARGP -= 2;
	if ( compareNumbers(ARGP, ARGP+1, LT) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }

    VMI(A_LE, COUNT(a_le), ("a_le\n")) MARK(A_LE);
      { ARGP -= 2;
	if ( compareNumbers(ARGP, ARGP+1, LE) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }

    VMI(A_GT, COUNT(a_gt), ("a_gt\n")) MARK(A_GT);
      { ARGP -= 2;
	if ( compareNumbers(ARGP, ARGP+1, GT) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }

    VMI(A_GE, COUNT(a_ge), ("a_ge\n")) MARK(A_GE);
      { ARGP -= 2;
	if ( compareNumbers(ARGP, ARGP+1, GE) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }

    VMI(A_EQ, COUNT(a_eq), ("a_eq\n")) MARK(A_EQ);
      { ARGP -= 2;
	if ( compareNumbers(ARGP, ARGP+1, EQ) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }

    VMI(A_NE, COUNT(a_ne), ("a_ne\n")) MARK(A_NE);
      { ARGP -= 2;
	if ( compareNumbers(ARGP, ARGP+1, NE) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of is/2. Unify the two pushed  values. Order does not matter
here. We need to open part of a frame to ensure trailing works properly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_IS, COUNT(a_is), ("a_is\n")) MARK(A_IS);
      { int rval;

	DoMark(lTop->mark);
	ARGP -= 2;
	rval = unify(ARGP, ARGP+1, lTop);

	if ( rval )
	  NEXT_INSTRUCTION;
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
    VMI(I_USERCALL0, COUNT(i_usercall0), ("user_call0\n")) MARK(USRCL0);
      { word goal;
	int arity;
	Word args, a;
	int n;
	register LocalFrame next;
	Module module;
	FunctorDef functor;
	int callargs;

	next = lTop;
	a = argFrameP(next, 0);		/* get the (now) instantiated */
	deRef(a);			/* variable */

	module = NULL;
	if ((a = stripModule(a, &module)) == (Word) NULL)
	  FRAME_FAILED;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the functor definition associated with the goal as well as the
arity and a pointer to the argument vector of the goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	if ( isAtom(goal = *a) )
	{ if ( *a == (word) ATOM_cut )
	    goto i_cut;
	  functor = lookupFunctorDef((Atom) goal, 0);
	  arity   = 0;
	  args    = NULL;
	} else if ( isTerm(goal) )
	{ args    = argTermP(goal, 0);
	  functor = functorTerm(goal);
	  arity   = functor->arity;
	} else
	{ warning("call/1 or variable as subclause: Illegal goal");
	  FRAME_FAILED;
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
	  functor = lookupFunctorDef((Atom) goal, callargs);
	  args    = NULL;
	} else if ( isTerm(goal) )
	{ functor = functorTerm(goal);
	  arity   = functor->arity;
	  functor = lookupFunctorDef(functor->name, arity + callargs);
	  args    = argTermP(goal, 0);
	} else
	{ warning("call/%d: Illegal goal", callargs+1);
	  FRAME_FAILED;
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
	  { Word a;

	    deRef2(args, a);
	    *ARGP = (isVar(*a) ? makeRef(a) : *a);
	  }
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If-then-else is a contraction of C_MARK and C_OR.  This contraction  has
been  made  to help the decompiler distinguis between (a ; b) -> c and a
-> b ; c, which would otherwise only be  possible  to  distinguis  using
look-ahead.

The   asm("nop")  is a  tricky.    The  problem  is    that  C_NOT and
C_IFTHENELSE are the same instructions.   The one is generated on \+/1
and the  other    on (Cond ->   True    ;   False).   Their  different
virtual-machine   id is  used   by  the   decompiler.  Now,   as   the
VMCODE_IS_ADDRESS is in effect,  these two instruction would  become
the same.  The asm("nop") ensures  they have the same *functionality*,
but a  *different* address.  If your  machine does't like nop,  define
the  macro ASM_NOP  in your md-file  to do something that 1)  has  *no
effect* and 2) is *not optimised* away by the compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_NOT, {}, ("c_not %d\n", *PC))
#if VMCODE_IS_ADDRESS
#ifdef ASM_NOP
      ASM_NOP
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

NOTE: as of SWI-Prolog 2.0.2, the call  to $alt/1 is `inlined'.  We just
build the frame for $alt/1 and then   continue execution.  This is ok as
the first call of $alt/1 simply succeeds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_OR, COUNT_N(c_or), ("c_or %d\n", *PC)) MARK(C_OR);
      { int skip = *PC++;
	Word a;

	*ARGP++ = consNum(skip);	/* push amount to skip (as B_CONST) */
	DEBUG(9, Sdprintf("$alt(%d)\n", skip));
	next = lTop;
	next->flags = FR->flags;
	next->predicate = PROCEDURE_alt1->definition;
	next->programPointer = PC;
	next->context = MODULE_system;

#if NO_INLINE_C_OR			/* old code.  keep for debugging */
        DEF  = next->predicate;
	goto normal_call;
#else
	STACKVERIFY( if (next > lMax) outOf((Stack)&stacks.local) );
	next->backtrackFrame = BFR;
	next->parent = FR;
	incLevel(next);
	clear(next, FR_CUT|FR_SKIPPED);
	statistics.inferences++;
	DoMark(next->mark);
	a = argFrameP(next, 0);		/* see callForeign() */
	lTop = (LocalFrame)argFrameP(a, 1);
					/* callForeign() here */
	next->clause = (ClauseRef) ForeignRedoVal(skip);
	SetBfr(next);
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
#endif /*NO_INLINE_C_OR*/
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
    { Word vars[2];
      int nvars;
      Procedure fproc;

      VMI(I_CALL_FV0, COUNT(i_call_fv0), ("i_call_fv0")) MARK(CFV0);
      { fproc = (Procedure) *PC++;
	nvars = 0;

	goto common_call_fv;
      }

      VMI(I_CALL_FV1, COUNT(i_call_fv1), ("i_call_fv1")) MARK(CFV1);
      { fproc = (Procedure) *PC++;
	nvars = 1;
	deRef2(varFrameP(FR, *PC++), vars[0]);
	goto common_call_fv;
      }

      VMI(I_CALL_FV2, COUNT(i_call_fv2), ("i_call_fv2")) MARK(CFV2);
      { fproc = (Procedure) *PC++;
	nvars = 2;
	deRef2(varFrameP(FR, *PC++), vars[0]);
	deRef2(varFrameP(FR, *PC++), vars[1]);

      common_call_fv:
	{ Definition def = fproc->definition;
	  Func f = def->definition.function;
	  int rval;
	  int i;

	  if ( !f )
	  { def = trapUndefined(def);
	    f = def->definition.function;
	  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we are debugging, just build a normal  frame and do the normal thing,
so the inline call is expanded to a normal call and may be traced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	  if ( 
#ifdef O_DEBUGGER
	       debugstatus.debugging ||
#endif
	       false(def, FOREIGN) )
	  { for(i=0; i<nvars; i++)
	      *ARGP++ = (isVar(*vars[i]) ? makeRef(vars[i]) : *vars[i]);
	  
	    next = lTop;
	    next->flags = FR->flags;
	    if ( true(DEF, HIDE_CHILDS) ) /* parent has hide_childs */
	      set(next, FR_NODEBUG);
	    DEF = def;
	    next->context = FR->context;

	    goto normal_call;
	  } else
	  { LocalFrame oldtop = lTop;
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We must create a frame and mark the  stacks for two reasons: undo if the
foreign call fails *AND*  make  sure   Trail()  functions  properly.  We
increase lTop too to prepare for asynchronous interrupts.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	    statistics.inferences++;
	    next = lTop;
	    lTop = (LocalFrame) argFrameP(next, 0);
	    if ( true(def, TRANSPARENT) )
	      next->context = FR->context;
	    else
	      next->context = def->module;
	    next->predicate = def;
	    next->parent    = FR;
#ifdef O_PROFILE
	    if ( statistics.profiling )
	      def->profile_calls++;
#endif /* O_PROFILE */
	    environment_frame = next;
	    DoMark(next->mark);

	    switch(nvars)
	    { case 0:
		rval = (*f)();
	        break;
	      case 1:
		rval = (*f)(vars[0]);
	        break;
	      case 2:
	      default:
		rval = (*f)(vars[0], vars[1]);
	        break;
	    }

	    environment_frame = FR;
	    lTop = oldtop;

	    if ( rval )
	    { NEXT_INSTRUCTION;
	    }

	    DoUndo(next->mark);
	    statistics.inferences++;	/* is a redo! */
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
      { Atom functor;
	word list;
	Module module = (Module) NULL;
	Word gp;

	next = lTop;
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )
	  set(next, FR_NODEBUG);

	ARGP = argFrameP(next, 0); deRef(ARGP); gp = ARGP;
	ARGP = argFrameP(next, 1); deRef(ARGP); list = *ARGP;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Obtain the functor of the actual goal from the first argument  and  copy
the arguments of this term in the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	
	if ((gp = stripModule(gp, &module)) == (Word) NULL)
	  FRAME_FAILED;
	next->context = module;
	goal = *gp;

	ARGP = argFrameP(next, 0);

	if (isAtom(goal) )
	{ functor = (Atom) goal;
	  arity = 0;
	} else if (isTerm(goal) )
	{ functor = functorTerm(goal)->name;
	  arity   = functorTerm(goal)->arity;
	  args    = argTermP(goal, 0);
	  for(n=0; n<arity; n++, ARGP++, args++)
	  { deRef2(args, a);
	    *ARGP = (isVar(*a) ? makeRef(a) : *a);
	  }
	} else
	{ warning("apply/2: Illegal goal");
	  FRAME_FAILED;
	}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scan the list and add the elements to the argument vector of the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	while(!isNil(list) )
	{ if (!isList(list) )
	  { warning("apply/2: Illegal argument list");
	    FRAME_FAILED;
	  }
	  args = argTermP(list, 0);
	  deRef(args);
	  *ARGP++ = (isVar(*args) ? makeRef(args) : *args);
	  arity++;
	  if (arity > MAXARITY)
	  { warning("apply/2: arity too high");
	    FRAME_FAILED;
	  }
	  args = argTermP(list, 1);
	  deRef(args);
	  list = *args;
	}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the associated procedure (see I_CALL for module handling), save the
program pointer and jump to the common part.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	{ FunctorDef fdef;

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
      VMI(I_DEPART, COUNT(i_depart), ("depart %d\n", *PC)) MARK(DEPART);
#if TAILRECURSION
	if ( true(FR, FR_CUT) && BFR <= FR
#ifdef O_DEBUGGER
	     && !debugstatus.debugging
#endif
	   )
	{ leaveDefinition(DEF);

	  if ( true(DEF, HIDE_CHILDS) )
	    set(FR, FR_NODEBUG);
	  
	  FR->predicate = DEF = ((Procedure) *PC++)->definition;
	  copyFrameArguments(lTop, FR, DEF->functor->arity);

	  goto depart_continue;
	}
#endif
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
	STACKVERIFY( if (next > lMax) outOf((Stack)&stacks.local) );

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise those slots of the frame that are common to Prolog predicates
and foreign ones.  There might be some possibilities for optimisation by
delaying these initialisations till they are really  needed  or  because
the information they are calculated from is destroyed.  This probably is
not worthwile.

Note: we are working above `lTop' here!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	next->backtrackFrame = BFR;
	next->parent         = FR;
	next->predicate	     = DEF;		/* TBD */
	next->programPointer = PC;		/* save PC in child */
	environment_frame = FR = next;		/* open the frame */

      depart_continue:

#ifdef O_DEBUGLOCAL
      {	Word ap = argFrameP(FR, DEF->functor->arity);
	int n;
	
	for(n=50; --n; )
	  *ap++ = (word)(((char*)ATOM_nil) + 1);
      }
#endif

#if tos
	{ static int tick;

	  if ( (++tick & 0x7f) == 0 )
	  { if ( kbhit() )
	      TtyAddChar(getch());
	  }	    
	}
#endif
	incLevel(FR);
	clear(FR, FR_CUT|FR_SKIPPED);

	statistics.inferences++;
	DoMark(FR->mark);

#if O_RLC				/* Windows readline console */
	{ if ( (++statistics.inferences % 0x7ff) == 0 )
	    rlc_check_intr();		/* check for interrupt (^C) */
	}
#endif

#ifdef O_PROFILE
	if (statistics.profiling)
	  DEF->profile_calls++;
#endif /* O_PROFILE */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Undefined   predicate detection and   handling.  trapUndefined() takes
care of  linking from the  public  modules  or  calling  the exception
handler.

Note that DEF->definition is  a  union  of  the clause  or C-function.
Testing is suffices to find out that the predicate is defined.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	if ( !DEF->definition.clauses && false(DEF, DYNAMIC) )	
	{ lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	  FR->predicate = DEF = trapUndefined(DEF);
	}

	if ( false(DEF, TRANSPARENT) )
	  FR->context = DEF->module;
	if ( false(DEF, SYSTEM) )
	  clear(FR, FR_NODEBUG);

#if O_DYNAMIC_STACKS
	if ( gc_status.requested )
	{ lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	  garbageCollect(FR);
	}
#else /*O_DYNAMIC_STACKS*/
#if O_SHIFT_STACKS
      { int gshift = narrowStack(global);
	int lshift = narrowStack(local);
	int tshift = narrowStack(trail);

	if ( gshift || lshift || tshift )
	{ lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);

	  if ( gshift || tshift )
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
	    growStacks(FR, NULL, lshift, gshift, tshift);
	}
      }
#else /*O_SHIFT_STACKS*/
	if ( narrowStack(global) || narrowStack(trail) )
	{ lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	  garbageCollect(FR);
	}
#endif /*O_SHIFT_STACKS*/
#endif /*O_DYNAMIC_STACKS*/

#if O_DEBUGGER
	if ( debugstatus.debugging )
	{ lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
	  CL = DEF->definition.clauses;
	  switch(tracePort(FR, CALL_PORT))
	  { case ACTION_FAIL:	goto frame_failed;
	    case ACTION_IGNORE: goto exit_builtin;
	  }
	}
#endif /*O_DEBUGGER*/

	if ( true(DEF, FOREIGN) )
	{ CL = (ClauseRef) FIRST_CALL;
	call_builtin:			/* foreign `redo' action */

	  if ( callForeign(DEF, FR) )
	    goto exit_builtin;

	  goto frame_failed;
	} 

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a normal Prolog predicate.  Just load the  machine  registers  with
values  found  in  the  clause, give a referecence to the clause and set
`lTop' to point to the first location after the current frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	ARGP = argFrameP(FR, 0);
	DEF->references++;

	DEBUG(9, Sdprintf("Searching clause ... "));

	if ( !(CL = firstClause(ARGP, DEF, &deterministic)) )
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
	  checkData(argFrameP(FR, n), FALSE);
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
      {
    exit_builtin:
				MARK(I_EXIT);
	if ( FR->parent == (LocalFrame) NULL )
	  goto top_exit;

	if ( FR->clause )
	{ if ( FR > BFR )
	    SetBfr(FR);
	} else
	{ if ( BFR <= FR )
	  { if ( BFR == FR )
	      SetBfr(FR->backtrackFrame);
	    lTop = FR;
	  }
	}
	goto normal_exit;

    VMI(I_EXIT, COUNT(i_exit), ("exit ")) MARK(EXIT);
	if (FR->parent == (LocalFrame) NULL)
	{ 
	top_exit:
	  { LocalFrame fr, fr2;

	    set(FR, FR_CUT);		/* execute I_CUT */
	    for(fr = BFR; fr > FR; fr = fr->backtrackFrame)
	    { for(fr2 = fr; fr2->clause && fr2 > FR; fr2 = fr2->parent)
	      { DEBUG(3, Sdprintf("discard %d\n", (Word)fr2 - (Word)lBase) );
		leaveFrame(fr2);
		fr2->clause = NULL;
	      }
	    }
	  }
#if O_DEBUGGER
	  if (debugstatus.debugging)
	  { CL = NULL;

	    switch(tracePort(FR, EXIT_PORT))
	    { case ACTION_RETRY:	goto retry;
	      case ACTION_FAIL:		set(FR, FR_CUT);
					FRAME_FAILED;
	    }
	  }
#endif /*O_DEBUGGER*/
	  leaveFrame(FR);
	  RETURN(TRUE);
	}

	if ( false(FR, FR_CUT) )
	{ if ( FR > BFR )			/* alternatives */
	    SetBfr(FR);
	} else
	{ if ( BFR <= FR )			/* deterministic */
	  { if ( BFR == FR )
	      SetBfr(FR->backtrackFrame);
	    lTop = FR;
	    leaveDefinition(DEF);
	  }
	}

    normal_exit:
#if O_DEBUGGER
	if (debugstatus.debugging)
	{ int action;
	  LocalFrame lSave = lTop;

	  if ( lTop < (LocalFrame)argFrameP(FR, DEF->functor->arity) )
	    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
	  action = tracePort(FR, EXIT_PORT);

	  switch(action)
	  { case ACTION_RETRY:	goto retry;
	    case ACTION_FAIL:	set(FR, FR_CUT);
				FRAME_FAILED;
	  }

	  lTop = lSave;
	}
#endif /*O_DEBUGGER*/

	PC = FR->programPointer;
	environment_frame = FR = FR->parent;
	DEF = FR->predicate;
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
      }	  
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TRACER RETRY ACTION

To retry we should first undo all actions done since the start  of  this
frame  by  resetting  the  global  stack and calling Undo(). The current
frame becomes the backtrack frame for the new childs.

Foreign functions can now just be restarted.  For Prolog  ones  we  will
create  a  dummy  clause before the first one and proceed as with normal
backtracking.

BUG: Clause reference counts should be  updated  properly.   Needs  some
detailed study!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

retry:					MARK(RETRY);
  DoUndo(FR->mark);
  SetBfr(FR);
  clear(FR, FR_CUT);
#if O_DEBUGGER
  if (debugstatus.debugging)
  { LocalFrame lTopSave = lTop;
    lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
    tracePort(FR, CALL_PORT);
    lTop = lTopSave;
  }
#endif /*O_DEBUGGER*/
  if ( false(DEF, FOREIGN) )
  { struct clause_ref zero;		/* fake a clause */
    struct clause zclause;

    clear(&zclause, ERASED);		/* avoid destruction */
    zero.next   = DEF->definition.clauses;
    zero.clause = &zclause;
    CL = &zero;

    CLAUSE_FAILED;
  }
  FR->clause = FIRST_CALL;
  goto call_builtin;

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
  DoUndo(FR->mark);		/* backtrack before clause indexing */

  if ( !(CL = findClause(CL, ARGP, DEF, &deterministic)) )
    goto frame_failed;

  if ( deterministic )
    set(FR, FR_CUT);

  SetBfr(FR->backtrackFrame);
  aTop = aBase;

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
    if (statistics.profiling)
      DEF->profile_fails++;
#endif

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { switch( tracePort(FR, FAIL_PORT) )
      { case ACTION_RETRY:	goto retry;
	case ACTION_IGNORE:	Putf("ignore not (yet) implemented here\n");
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

    if ( !FR->backtrackFrame )			/* top goal failed */
    { register LocalFrame fr = FR->parent;

      for(; fr; fr = fr->parent)
        leaveFrame(fr);

      RETURN(FALSE);
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
    if ( false(DEF, FOREIGN) )
      CL = CL->next;

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { DoUndo(FR->mark);			/* data backtracking to get nice */
					/* tracer output */

      switch( tracePort(FR, REDO_PORT) )
      { case ACTION_FAIL:	continue;
	case ACTION_IGNORE:	CL = NULL;
				goto exit_builtin;
	case ACTION_RETRY:	goto retry;
      }
    }
#endif /*O_DEBUGGER*/
    
    statistics.inferences++;
#ifdef O_PROFILE
    if ( statistics.profiling )
      DEF->profile_redos++;
#endif /* O_PROFILE */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finaly restart.  If it is a Prolog frame this is the same as  restarting
as  resuming  a  frame after unification of the head failed.  If it is a
foreign frame we have to set BFR and do data backtracking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( false(DEF, FOREIGN) )
    { if ( true(FR, FR_CUT) || !CL )
	continue;
      goto resume_frame;
    }

    SetBfr(FR->backtrackFrame);
    DoUndo(FR->mark);

    goto call_builtin;
  }
} /* end of interpret() */


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
copyFrameArguments(LocalFrame from, LocalFrame to, register int argc)
{ register Word ARGD, ARGS;
  register word k;
  int argc_save;

  if ( (argc_save = argc) == 0 )
    return;

  ARGS = argFrameP(from, 0);
  ARGD = argFrameP(to, 0);
  for( ;argc-- > 0; ARGS++, ARGD++)	/* dereference the block */
  { if ( !isRef(k = *ARGS) )
      continue;
    if ( (long)unRef(k) < (long)to )	/* to older frame */
      continue;
    if ( isVar(*unRef(k)) )
    { *unRef(k) = makeRef(ARGD);
      setVar(*ARGS);
      continue;
    }
    *ARGS = *unRef(k);
  }
    
  ARGS = argFrameP(from, 0);
  ARGD = argFrameP(to, 0);
  argc = argc_save;
  while(argc-- > 0)			/* now copy them */
    *ARGD++ = *ARGS++;  
}

#if O_COMPILE_OR
word
pl_alt(Word skip, word h)
{ switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      SECURE( if (!isInteger(*skip)) sysError("pl_alt()") );
      ForeignRedo(valNum(*skip));
    case FRG_REDO:
      DEBUG(9, Sdprintf("$alt/1: skipping %ld codes\n", ForeignContext(h)) );
      environment_frame->programPointer += ForeignContext(h);
      succeed;
    case FRG_CUTTED:
    default:
      succeed;
  }
}
#endif /* O_COMPILE_OR */
