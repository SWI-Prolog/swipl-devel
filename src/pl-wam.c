/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Virtual machine instruction interpreter
*/

#include "pl-incl.h"

#if sun
#include <prof.h>			/* in-function profiling */
#else
#define MARK(label)
#endif

forwards void	copyFrameArguments P((LocalFrame, LocalFrame, int));
forwards inline bool	callForeign P((const Procedure, LocalFrame));
forwards void	leaveForeignFrame P((LocalFrame));

#if COUNTING

forwards void	countHeader P((void));
forwards void	countArray P((char *, int *));
forwards void	countOne P((char *, int));

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
  int i_cut;
  int i_usercall;
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
  countArray("H_SINT",		counting.h_sint);
  countArray("B_SINT",		counting.b_sint);
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
  countOne(  "I_CUT", 		counting.i_cut);
  countOne(  "I_USERCALL", 	counting.i_usercall);
  countOne(  "I_APPLY", 	counting.i_apply);
  countOne(  "I_DEPART", 	counting.i_depart);
  countOne(  "I_CALL", 		counting.i_call);
  countOne(  "I_EXIT", 		counting.i_exit);

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

static
void
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
callForeign(proc, frame)
Procedure proc;
register LocalFrame frame;
{ int argc = proc->functor->arity;
  word result;
  Word argv[10];
  Func function;

  { register Word a, *ap;
    register int n;

    a = argFrameP(frame, 0);
    lTop = (LocalFrame) argFrameP(a, argc);
    for(ap = argv, n = argc; n > 0; n--, a++, ap++)
      deRef2(a, *ap)
  }

  DEBUG(7, printf("Calling built in %s\n", procedureName(proc)) );

  SECURE(
  int n;
  for(n = 0; n < argc; n++)
    checkData(argv[n]);
  );

  function = proc->definition->definition.function;

#define A(n) argv[n]
#define F (*function)
#define B ((word) frame->clause)

  gc_status.blocked++;
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
  gc_status.blocked--;

#undef B
#undef F
#undef A

  SECURE(
  int n;
  for(n=0;n<argc; n++)
    checkData(argv[n]);
  );

  if ( result == FALSE )
  { frame->clause = NULL;
    fail;
  } else if ( result == TRUE )
  { frame->clause = NULL;
    succeed;
  } else
  { if ( true(proc->definition, NONDETERMINISTIC) )
    { if ( !result & FRG_MASK )
      { warning("Illegal return value from foreign predicate %s: 0x%x",
				    procedureName(proc), result);
	fail;
      }
      frame->clause = (Clause) result;
      succeed;
    }
    warning("Deterministic foreign predicate %s returns 0x%x",
			    procedureName(proc), result);
    fail;
  }
}

static void
leaveForeignFrame(fr)
register LocalFrame fr;
{ if ( true(fr->procedure->definition, NONDETERMINISTIC) )
  { register Procedure proc = fr->procedure;
    register Func f = proc->definition->definition.function;
    register word context = (word) fr->clause | FRG_CUT;

#define U ((Word) NULL)
    DEBUG(5, printf("Cut %s, context = 0x%lx\n", procedureName(proc), context));
    switch(proc->functor->arity)
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
      default:	sysError("Too many arguments (%d) to leaveForeignFrame()");
    }
  }
#undef U
}


		/********************************
		*          INTERPRETER          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			 MACHINE REGISTERS

  - PROC
    Current procedure running.
  - DEF
    Definition structure of current procedure.
  - PC
    Virtual machine `program counter': pointer to the next byte code  to
    interpret.
  - XR
    External referecence pointer.  Pointer to an  array  holding  atoms, 
    integers,  reals,  strings functors  and  procedures  used  by  the
    current clause.  Each entry of this array is a 4 byte entity (a `word').
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
#define SetBfr(fr) 		(BFR = (fr))

bool
interpret(Context, Goal, debug)
Module Context;
word Goal;
bool debug;
{ register LocalFrame FR;		/* current frame */
  register Word	      ARGP;		/* current argument pointer */
  register Code	      PC;		/* program counter */
	   Word	      XR;		/* current external ref. table */
	   LocalFrame BFR;		/* last backtrack frame */
  	   Procedure  PROC;		/* current procedure */  
  	   Definition DEF;		/* definition of current procedure */
	   bool	      deterministic;    /* clause found deterministically */
#define		      CL (FR->clause)	/* clause of current frame */

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
    &&H_CONST0_LBL,
    &&H_CONST1_LBL,
    &&H_CONST2_LBL,

    &&H_LIST_LBL,
    &&H_FUNCTOR0_LBL,
    &&H_FUNCTOR1_LBL,
    &&H_FUNCTOR2_LBL,

    &&B_VAR0_LBL,
    &&B_VAR1_LBL,
    &&B_VAR2_LBL,

    &&H_SINT_LBL,
    &&B_SINT_LBL,

    &&I_USERCALL_LBL,
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
    &&B_STRING_LBL
  };

#define VMI(Name, Count, Msg)	Name ## _LBL: Count; DEBUG(8, printf Msg);
#if O_VMCODE_IS_ADDRESS
#define NEXT_INSTRUCTION	goto *(void *)((int)(*PC++))
#else
#define NEXT_INSTRUCTION	goto *jmp_table[*PC++]
#endif

#else /* O_LABEL_ADDRESSES */

#define VMI(Name, Count, Msg)	case Name: Count; DEBUG(8, printf Msg);
#define NEXT_INSTRUCTION	goto next_instruction

#endif /* O_LABEL_ADDRESSES */

#if O_VMCODE_IS_ADDRESS
  interpreter_jmp_table = jmp_table;	/* make it globally known */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If the assertion below fails, addresses cannot be  stored in VM codes.
Add #define O_VMCODE_IS_ADDRESS 0 to your md.h file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  DEBUG(0, { int i;

	     for(i=0; i<=I_HIGHEST; i++)
	       assert((long) jmp_table[i] >= (1 << (sizeof(code)*8)));
	   });
#endif /* O_VMCODE_IS_ADDRESS */

  DEBUG(1, { extern int Output;		/* --atoenne-- */
	     if ( Output )
	     { Putf("Interpret: ");
	       pl_write(&Goal);
	       Putf("\n");
	     } else
	       printf("Interpret goal in unitialized environment.\n");
	   });

  /* Allocate a local stack frame */

  FR = lTop;
  FR->parent = (LocalFrame) NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the procedure definition associated with `Goal'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { Word gp;
    word g;
    Module module = Context;
    FunctorDef functor;

    if ((gp = stripModule(&Goal, &module)) == (Word) NULL)
      fail;

    g = *gp;
    if ( isAtom(g) )
    { functor = lookupFunctorDef((Atom)g, 0);
    } else if ( isTerm(g) )
    { int arity;
      Word a, args = argTermP(g, 0);

      functor = functorTerm(g);
      arity = functor->arity;
      ARGP = argFrameP(FR, 0);

      for(; arity-- > 0; args++)
      { deRef2(args, a);
	*ARGP++ = (isVar(*a) ? makeRef(a) : *a);
      }
    } else
      return warning("Illegal goal while called from C");

    PROC = resolveProcedure(functor, module);
    DEF = PROC->definition;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finally fill all the slots of  the  frame  and  initialise  the  machine
registers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    FR->context = (true(DEF, TRANSPARENT) ? module : DEF->module);
    clearFlags(FR);
    setLevelFrame(FR, !parentFrame(FR) ? 0L : levelFrame(parentFrame(FR)) + 1);
    if ( !debug )
      set(FR, FR_NODEBUG);
    FR->backtrackFrame = (LocalFrame) NULL;
    FR->procedure = PROC;
    FR->clause = (Clause) NULL;
    SetBfr(FR);
    Mark(FR->mark);
    environment_frame = FR;

    if ( (CL = DEF->definition.clauses) == (Clause) NULL )
    { trapUndefined(PROC);
      DEF = PROC->definition;		/* may have changed! */
    }

    if ( debugstatus.debugging )
    { LocalFrame lTopSave = lTop;
      int action;

      lTop = (LocalFrame) argFrameP(FR, PROC->functor->arity);
      action = tracePort(FR, CALL_PORT);
      lTop = lTopSave;
      switch(action)
      { case ACTION_FAIL:	fail;
	case ACTION_IGNORE:	succeed;
      }
    }

    if ( true(DEF, FOREIGN) )
    { FR->clause = FIRST_CALL;

      return callForeign(PROC, FR);
    }

    ARGP = argFrameP(FR, 0);
    if ( (CL = findClause(CL, ARGP, DEF, &deterministic)) == NULL )
      fail;
    if ( deterministic )
      set(FR, FR_CUT);
    CL->references++;
    PC = CL->codes;
    XR = CL->externals;
    lTop = (LocalFrame) argFrameP(FR, CL->variables);
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
constant from the external reference array XR.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { word c;
    register Word k;					MARK(HCONST);

    VMI(H_CONST,	COUNT_N(h_const_n),	("h_const %d\n", *PC))
	c = XR[*PC++];
	goto common_hconst;
    VMI(H_CONST0,	COUNT(h_const_n[0]),	("h_const 0\n"))
	c = XR[0];
	goto common_hconst;
    VMI(H_CONST1,	COUNT(h_const_n[1]),	("h_const 1\n"))
        c = XR[1];
	goto common_hconst;
    VMI(H_CONST2,	COUNT(h_const_n[2]),	("h_const 2\n"))
        c = XR[2];
        goto common_hconst;
    VMI(H_SINT,		COUNT_N(h_sint),	("h_sint %d\n", *PC))
        c = consNumFromCode(*PC++);
        goto common_hconst;
    VMI(H_NIL,		COUNT(h_nil),		("h_nil\n"))
        c = (word) ATOM_nil;

  common_hconst:
        deRef2(ARGP++, k);
        if (isVar(*k))
	{ Trail(k);
	  *k = c;
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
	{ Trail(k);
	  *k = globalReal(valReal(XR[*PC++]));
	  NEXT_INSTRUCTION;
	}
	if (isReal(*k) && valReal(*k) == valReal(XR[*PC++]))
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
	{ word str = XR[*PC++];
	  Trail(k);
	  *k = globalString(valString(str));
	  NEXT_INSTRUCTION;
	}
	if ( isString(*k) )
	{ word str = XR[*PC++];
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
      { *ARGP++ = XR[*PC++];
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_REAL and B_STRING need to copy the value on the global  stack  because
the XR-table might be freed due to a retract.  We should write fast copy
algorithms,   especially   for   the   expensive   globalReal(valReal())
construct.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_REAL, COUNT_N(b_real_n), ("b_real %d\n", *PC)) MARK(BREAL);
      { *ARGP++ = globalReal(valReal(XR[*PC++]));
	NEXT_INSTRUCTION;
      }

    VMI(B_STRING, COUNT_N(b_string_n), ("b_string %d\n", *PC)) MARK(BSTRING);
      { *ARGP++ = globalString(valString(XR[*PC++]));
	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A small integer in the body. As B_CONST.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(B_SINT, COUNT_N(b_sint), ("b_sint %d\n", *PC)) MARK(B_SINT);
      { *ARGP++ = consNumFromCode(*PC++);
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
      { if (unify(varFrameP(FR, *PC++), ARGP++) )
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
	    Trail(k);
	    *k = makeRef(ARGP++);
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

	fdef = (FunctorDef) XR[*PC++];

	common_functor:
	*aTop++ = ARGP + 1;
	verifyStack(argument);
        deRef(ARGP);
	if (isVar(*ARGP) )
	{ Trail(ARGP);
	  *ARGP = globalFunctor(fdef);
	  ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	if (isTerm(*ARGP) && functorTerm(*ARGP) == fdef)
	{ ARGP = argTermP(*ARGP, 0);
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;	    
    VMI(H_FUNCTOR0, COUNT(h_functor_n[0]), ("h_functor 0\n")) MARK(HFUNC0);
	fdef = (FunctorDef) XR[0];
	goto common_functor;
    VMI(H_FUNCTOR1, COUNT(h_functor_n[1]), ("h_functor 0\n")) MARK(HFUNC1);
	fdef = (FunctorDef) XR[1];
	goto common_functor;
    VMI(H_FUNCTOR2, COUNT(h_functor_n[2]), ("h_functor 0\n")) MARK(HFUNC2);
	fdef = (FunctorDef) XR[2];
	goto common_functor;
    VMI(H_LIST, COUNT(h_list), ("h_list\n")) MARK(HLIST);
	*aTop++ = ARGP + 1;
	verifyStack(argument);
	deRef(ARGP);
	if (isVar(*ARGP) )
	{ STACKVERIFY( if (gTop + 3 > gMax) outOf((Stack)&stacks.global) );
	  Trail(ARGP);
	  *ARGP = (word) gTop;
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
      { register FunctorDef fdef = (FunctorDef) XR[*PC++];

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

	if ( debugstatus.debugging )
	{ tracePort(FR, UNIFY_PORT);
	  SetBfr(FR);
	} else
	{ if ( true(FR, FR_CUT ) )
	  { SetBfr(FR->backtrackFrame);
	  } else
	  { SetBfr(FR);
	  }
	}

	NEXT_INSTRUCTION;
      }
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
    i_cut:			/* from I_USERCALL */
    VMI(I_CUT, COUNT(i_cut), ("cut frame %d\n", REL(FR))) MARK(CUT);
      { LocalFrame fr;
	register LocalFrame fr2;

	set(FR, FR_CUT);
	for(fr = BFR; fr > FR; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > FR; fr2 = fr2->parent)
	  { DEBUG(3, printf("discard %d\n", (Word)fr2 - (Word)lBase) );
	    leaveFrame(fr2);
	    fr2->clause = (Clause) NULL;
	  }
	}
	SetBfr(debugstatus.debugging ? FR : FR->backtrackFrame);

	DEBUG(3, printf("BFR = %d\n", (Word)BFR - (Word)lBase) );
	lTop = (LocalFrame) argFrameP(FR, CL->variables);
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
C_JMP skips the amount stated in the pointed XR table value.   The  PC++
could be compiled out, but this is a bit more neath.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_JMP, COUNT_N(c_jmp), ("c_jmp %d\n", *PC)) MARK(C_JMP);
      { PC += *PC;
	PC++;

	NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_MARK saves the value of BFR (current backtrack frame) into a local
frame slot reserved by the compiler. 
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

If the saved backtrack point is older than the current  frame  use  this
frame  as  basis.   This  avoids  us to dereference the currently active
frame.

All frames created since what becomes now the  backtrack  point  can  be
discarded.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_CUT, COUNT_N(c_cut), ("c_cut %d\n", *PC)) MARK(C_CUT);
      { LocalFrame obfr = (LocalFrame) varFrame(FR, *PC++);
	LocalFrame fr;
	register LocalFrame fr2;

	if ( obfr < FR )
	  obfr = FR;

	for(fr = BFR; fr > obfr; fr = fr->backtrackFrame)
	{ for(fr2 = fr; fr2->clause && fr2 > obfr; fr2 = fr2->parent)
	  { DEBUG(3, printf("discard %d: ", (Word)fr2 - (Word)lBase) );
	    DEBUG(3, writeFrameGoal(fr2, 2); pl_nl() );
	    leaveFrame(fr2);
	    fr2->clause = (Clause) NULL;
	  }
	}

        SetBfr(obfr);
	DEBUG(3, Putf("BFR at "); writeFrameGoal(BFR, 2); pl_nl() );
	{ int nvar = (true(BFR->procedure->definition, FOREIGN)
				? BFR->procedure->functor->arity
				: BFR->clause->variables);
	  lTop = (LocalFrame) argFrameP(BFR, nvar);
	  ARGP = argFrameP(lTop, 0);
	}

        NEXT_INSTRUCTION;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_END is a dummy instruction to help the decompiler to fin the end of  A
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
    A_IS 1	% unify top of stack ('X+1') with Y
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
				DEBUG(8, printf("ARGP = 0x%x; top = ", ARGP);
					 pl_write(ARGP-1);
					 printf("\n"));
	NEXT_INSTRUCTION;
      }

    VMI(A_FUNC1, COUNT_N(a_func1), ("a_func1 %d\n", *PC)) MARK(A_FUNC1);
      {	Word argp = ARGP;
	if ( ar_func_n(*PC++, 1, &argp) == FALSE )
	  BODY_FAILED;
	ARGP = argp;
				DEBUG(8, printf("ARGP = 0x%x; top = ", ARGP);
					 pl_write(ARGP-1);
					 printf("\n"));
	NEXT_INSTRUCTION;
      }

    VMI(A_FUNC2, COUNT_N(a_func2), ("a_func2 %d\n", *PC)) MARK(A_FUNC2);
      {	Word argp = ARGP;
				DEBUG(8, printf("ARGP = 0x%x; top = ", ARGP);
					 pl_write(ARGP-2); printf(" & ");
					 pl_write(ARGP-1);
					 printf("\n"));
	if ( ar_func_n(*PC++, 2, &argp) == FALSE )
	  BODY_FAILED;
	ARGP = argp;
				DEBUG(8, printf("ARGP = 0x%x; top = ", ARGP);
					 pl_write(ARGP-1);
					 printf("\n"));
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
Translation of is/2.  Unify the  two  pushed  values.   Order  does  not
matter here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    VMI(A_IS, COUNT(a_is), ("a_is\n")) MARK(A_IS);
      { ARGP -= 2;
	if ( unify(ARGP, ARGP+1) == FALSE )
	  BODY_FAILED;
	NEXT_INSTRUCTION;
      }
#endif /* O_COMPILE_ARITH */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_USERCALL is generated by the compiler if a variable is encountered  as
a  subclause.   Note that the compount statement opened here is encloses
also  I_APPLY  and  I_CALL.   This  allows  us  to  use  local  register
variables,  but  still  jump to the `normal_call' label to do the common
part of all these three virtual machine instructions.

I_USERCALL has the task of  analysing  the  goal:  it  should  fill  the
->procedure  slot of the new frame and save the current program counter.
It also is responsible of filling the argument part of  the  environment
frame with the arguments of the term.

BUG: have to find out how to proceed in case of failure (I am afraid the
`goto frame_failed' is a bit dangerous here).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(I_USERCALL, COUNT(i_usercall), ("user_call\n")) MARK(USRCLL);
      { word goal;
	int arity;
	Word args, a;
	int n;
	register LocalFrame next;
	Module module = (Module) NULL;
	FunctorDef functor;

	next = lTop;			/* open next frame */
	next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )
	  set(next, FR_NODEBUG);
	a = argFrameP(next, 0);		/* get the (now) instantiated */
	deRef(a);			/* variable */

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
	} else if ( isTerm(goal) )
	{ args = argTermP(goal, 0);
	  functor = functorTerm(goal);
	  arity = functor->arity;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now scan the argument vector of the goal and fill the arguments  of  the
frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	  ARGP = argFrameP(next, 0);
	  for(; arity-- > 0; ARGP++, args++)
	  { Word a;

	    deRef2(args, a);
	    *ARGP = (isVar(*a) ? makeRef(a) : *a);
	  }
	} else
	{ warning("Illegal goal");
	  FRAME_FAILED;
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the associated procedure.  First look in the specified module.   If
the function is not there then look in the user module.  Finally specify
the context module environment for the goal. This is not necessary if it
will  be  specified  correctly  by  the goal started.  Otherwise tag the
frame and write  the  module  name  just  below  the  frame.   See  also
contextModule().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	PROC = resolveProcedure(functor, module);
	DEF = PROC->definition;
	next->procedure = PROC;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Save the program counter (note that  I_USERCALL  has  no  argument)  and
continue as with a normal call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	next->programPointer = PC;
	next->context = module;
	goto normal_call;
	
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
O_VMCODE_IS_ADDRESS is in effect,  these two instruction would  become
the same.  The asm("nop") ensures  they have the same *functionality*,
but a  *different* address.  If your  machine does't like nop,  define
the  macro ASM_NOP  in your md-file  to do something that 1)  has  *no
effect* and 2) is *not optimised* away by the compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_NOT, {}, ("c_not %d\n", *PC))
#if O_VMCODE_IS_ADDRESS
#ifdef ASM_NOP
      ASM_NOP
#else
      asm("nop");
#endif
#endif
    VMI(C_IFTHENELSE, COUNT_2N(c_ifthenelse), ("c_ifthenelse %d\n", *PC))
      MARK(C_ITE);
      { varFrame(FR, *PC++) = (word) BFR;	/* C_MARK */

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

Note that this one is enclosed in the compound statement of  I_USERCALL,
I_APPLY,  I_CALL  and I_DEPART to allow sharing of the register variable
`next' with them and thus make the `goto common_call' valid.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    VMI(C_OR, COUNT_N(c_or), ("c_or %d\n", *PC)) MARK(C_OR);
      { *ARGP++ = consNum(*PC++);	/* push amount to skip (as B_CONST) */
	
	PROC = PROCEDURE_alt1;		/* fill the frame arguments */
	DEF  = PROC->definition;
	next = lTop;
	next->flags = FR->flags;
	next->procedure = PROC;
	next->programPointer = PC;
	next->context = MODULE_system;

	goto normal_call;
      }
#endif /* O_COMPILE_OR */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_APPLY is the code generated by the Prolog goal $apply/2 (see reference
manual for the definition of apply/2).  We expect a term  in  the  first
argument  of  the  frame  and a list in the second, comtaining aditional
arguments.  Most comments of I_USERCALL apply to I_APPLY as well.   Note
that  the two arguments are copied in local variables as they will later
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
	  PROC = resolveProcedure(fdef, module);
	  DEF = PROC->definition;
	  next->procedure = PROC;
	  next->programPointer = PC;
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
	if ( true(FR, FR_CUT) && BFR <= FR && !debugstatus.debugging )
	{ leaveClause(CL);

	  if ( true(DEF, HIDE_CHILDS) )
	    set(FR, FR_NODEBUG);

	  PROC = FR->procedure = (Procedure) XR[*PC++];
	  DEF = PROC->definition;

	  copyFrameArguments(lTop, FR, PROC->functor->arity);

	  goto depart_continue;
	}
#endif
      VMI(I_CALL, COUNT(i_call), ("call %d\n", *PC)) MARK(CALL);
        next = lTop;
        next->flags = FR->flags;
	if ( true(DEF, HIDE_CHILDS) )		/* parent has hide_childs */
	  set(next, FR_NODEBUG);
	PROC = next->procedure = (Procedure)XR[*PC++];
	DEF = PROC->definition;
	next->programPointer = PC;		/* save PC in child */
	next->context = FR->context;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the common part of the call variations.  By now the following is
true:

  - arguments, nodebug, programPointer		filled
  - context					filled with context for
						transparent predicate
  - PROC, DEF					filled
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
	next->parent = FR;
	environment_frame = FR = next;	/* open the frame */

      depart_continue:
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
	Mark(FR->mark);

#if O_PROFILE
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
	{ lTop = (LocalFrame) argFrameP(FR, PROC->functor->arity);
	  trapUndefined(PROC);
	  DEF = PROC->definition;
	}

	if ( false(DEF, TRANSPARENT) )
	  FR->context = DEF->module;
	if ( false(DEF, SYSTEM) )
	  clear(FR, FR_NODEBUG);

#if O_DYNAMIC_STACKS
	if ( gc_status.requested )
	{ lTop = (LocalFrame) argFrameP(FR, PROC->functor->arity);
	  garbageCollect(FR);
	}
#else
	if ( gMax - gTop < 1024L || tMax - tTop < 1024L )
	{ lTop = (LocalFrame) argFrameP(FR, PROC->functor->arity);
	  garbageCollect(FR);
	}
#endif	

	if ( debugstatus.debugging )
	{ lTop = (LocalFrame) argFrameP(FR, PROC->functor->arity);
	  CL = (Clause) NULL;
	  switch(tracePort(FR, CALL_PORT))
	  { case ACTION_FAIL:	goto frame_failed;
	    case ACTION_IGNORE: goto exit_builtin;
	  }
	}

	if ( true(DEF, FOREIGN) )
	{ CL = (Clause) FIRST_CALL;

	  call_builtin:				/* foreign `redo' action */

	  if (callForeign(PROC, FR) == TRUE)
	    goto exit_builtin;
	  goto frame_failed;
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a normal Prolog predicate.  Just load the  machine  registers  with
values  found  in  the  clause, give a referecence to the clause and set
`lTop' to point to the first location after the current frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	ARGP = argFrameP(FR, 0);

	DEBUG(9, printf("Searching clause ... "));

	if ( (CL = findClause(DEF->definition.clauses, ARGP, DEF,
			      &deterministic)) == NULL )
	{ DEBUG(9, printf("No clause matching index.\n"));
	  FRAME_FAILED;
	}
	DEBUG(9, printf("Clauses found.\n"));

	if ( deterministic )
	  set(FR, FR_CUT);
	CL->references++;
	XR = CL->externals;
	PC = CL->codes;
	lTop = (LocalFrame)(ARGP + CL->variables);

	SECURE(
	int argc; int n;
	argc = PROC->functor->arity;
	for(n=0; n<argc; n++)
	  checkData(argFrameP(FR, n) );
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
	{ leaveClause(CL);
    top_exit:
	  if (debugstatus.debugging)
	  { CL = (Clause) NULL;
	    switch(tracePort(FR, EXIT_PORT) )
	    { case ACTION_RETRY:	goto retry;
	      case ACTION_FAIL:		set(FR, FR_CUT);
					goto frame_failed;
	    }
	  }
	  succeed;
	}

	if ( false(FR, FR_CUT) )
	{ if ( FR > BFR )			/* alternatives */
	    SetBfr(FR);
	} else
	{ if ( BFR <= FR )			/* deterministic */
	  { if ( BFR == FR )
	      SetBfr(FR->backtrackFrame);
	    lTop = FR;
	    leaveClause(CL);
	  }
	}

    normal_exit:
	if (debugstatus.debugging)
	{ switch(tracePort(FR, EXIT_PORT) )
	  { case ACTION_RETRY:	goto retry;
	    case ACTION_FAIL:	set(FR, FR_CUT);	/* references !!! */
				goto frame_failed;
	  }
	}

	PC = FR->programPointer;
	environment_frame = FR = FR->parent;
	PROC = FR->procedure;
	DEF = PROC->definition;
	XR = CL->externals;
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
  Undo(FR->mark);
  SetBfr(FR);
  clear(FR, FR_CUT);
  if (debugstatus.debugging)
  { LocalFrame lTopSave = lTop;
    lTop = (LocalFrame) argFrameP(FR, PROC->functor->arity);
    tracePort(FR, CALL_PORT);
    lTop = lTopSave;
  }
  if ( false(DEF, FOREIGN) )
  { struct clause zero;				/* fake a clause */

    clear(&zero, ERASED);			/* avoid destruction */
    zero.next = DEF->definition.clauses;
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
  DEBUG(9, printf("body_failed\n"));
  if ( BFR > FR )
  { environment_frame = FR = BFR;
    goto resume_from_body;
  }

clause_failed:
  { register Clause next;

    next = CL->next;
    leaveClause(CL);
    CL = next;
  }
  if ( true(FR, FR_CUT) )
    goto frame_failed;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resume frame FR.  CL points  to  the  next  (candidate)  clause.   First
indexing  is  activated  to find the next real candidate.  If this fails
the entire frame has failed, so we cab continue at `frame_failed'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resume_frame:
  ARGP = argFrameP(FR, 0);
  Undo(FR->mark);		/* backtrack before clause indexing */

  if ( (CL = findClause(CL, ARGP, DEF, &deterministic)) == NULL )
    goto frame_failed;

  if ( deterministic )
    set(FR, FR_CUT);
  else
    clear(FR, FR_CUT);

  SetBfr(FR->backtrackFrame);
  CL->references++;
  XR = CL->externals;
  PC = CL->codes;
  aTop = aBase;
  lTop = (LocalFrame) argFrameP(FR, CL->variables);

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
  { 
#if O_PROFILE
	if (statistics.profiling)
	  FR->procedure->definition->profile_fails++;
#endif /* O_PROFILE */

    if ( debugstatus.debugging )
    { switch( tracePort(FR, FAIL_PORT) )
      { case ACTION_RETRY:	PROC = FR->procedure;
				DEF = PROC->definition;
				goto retry;
	case ACTION_IGNORE:	Putf("ignore not (yet) implemented here\n");
      }
    }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update references due to failure of this frame.  The references of  this
frame's  clause are already updated.  All frames that can be reached via
the parent links and are  created  after  the  backtrack  frame  can  be
visited for dereferencing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( !FR->backtrackFrame )			/* top goal failed */
    { register LocalFrame fr = FR->parent;

      for(; fr; fr = fr->parent)
        leaveFrame(fr);

      gc_status.segment = NULL;

      fail;
    }

    { register LocalFrame fr = FR->parent;

      environment_frame = FR = FR->backtrackFrame;

      for( ; fr > FR; fr = fr->parent )
        leaveFrame(fr);
    }

    { register LocalFrame bfr = FR->backtrackFrame;

      if ( bfr < gc_status.segment )
        gc_status.segment = bfr;
    }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
References except for this frame are OK again.  First fix the references
for this frame if it is a Prolog frame.  This  cannot  be  in  the  loop
above as we need to put CL on the next clause.  Dereferencing the clause
might free it!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
resume_from_body:

    PROC = FR->procedure;
    DEF = PROC->definition;
    if ( false(DEF, FOREIGN) )
    { register Clause next;

      next = CL->next;
      leaveClause(CL);
      CL = next;
    }

    if ( debugstatus.debugging )
    { Undo(FR->mark);			/* data backtracking to get nice */
					/* tracer output */

      switch( tracePort(FR, REDO_PORT) )
      { case ACTION_FAIL:	continue;
	case ACTION_IGNORE:	CL = (Clause) NULL;
				goto exit_builtin;
	case ACTION_RETRY:	goto retry;
      }
    }
    
    statistics.inferences++;
#if O_PROFILE
    if ( statistics.profiling )
      FR->procedure->definition->profile_redos++;
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
    Undo(FR->mark);

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
    location  int  the  `to' fram, indicating sharing variables.  In the
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
copyFrameArguments(from, to, argc)
LocalFrame from;
LocalFrame to;
register int argc;
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
pl_alt(skip, h)
Word skip;
word h;
{ switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      SECURE( if (!isInteger(*skip)) sysError("pl_alt()") );
      ForeignRedo(valNum(*skip));
    case FRG_REDO:
      DEBUG(8, printf("$alt/1: skipping %d codes\n", ForeignContext(h)) );
      environment_frame->programPointer += ForeignContext(h);
      succeed;
    case FRG_CUTTED:
    default:
      succeed;
  }
}
#endif /* O_COMPILE_OR */
