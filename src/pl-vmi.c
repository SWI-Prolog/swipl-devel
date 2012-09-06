/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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
Implementation of the Virtual Machine Instructions (VMI).

Each VMI has the structure below. Using this structure we are completely
flexible in how we implement the instruction   and  we can easily create
the derived tables, which is done by the program mkvmi.c.

	VMI(Name, Flags, #Args, (ArgType, ...))
	{
	}

Within the scope of this file,   the following virtual machine variables
are available.

	* FR
	Current environment frame

	* NFR
	Next frame (used to share in various calling instructions)

	* BFR
	Backtrack frame: current choicepoint

	* PC
	Program Counter

	* ARGP
	Argument pointer

	* CL
	Running clause (= FR->clause)

	* DEF
	Running definition

Virtual machine instructions can return with one of:

	* NEXT_INSTRUCTION
	Proceed

	* CLAUSE_FAILED
	Failed unifying the head: backtrack to next clause

	* BODY_FAILED
	Failure of in-body instructions (I_FAIL, B_UNIFY_EXIT, ...)

	* FRAME_FAILED
	Other failures: deep backtracking.

	* VMI_GOTO(VMI)
	Continue executing another virtual instruction.  Note this is
	called GOTO as it is a jump rather than a call.

Virtual machine instruction names.  Prefixes:

  I_	General instructions
  B_	Body specific version
  H_	Head specific versin
  A_	Arithmetic compilation specific
  C_	Control (compilation of ;/2, etc.)
  S_   Supervisor instructions.  See pl-supervisor.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
				ISSUES


Sometime multiple instructions share variables. This   is done using the
code below, so we can at  least   identify  them easily. Ultimately, the
dependencies must be removed, probably  mostly   by  moving the reusable
code into functions.

	BEGIN_SHAREDVARS
	Decls
	VMI(...)
	VMI(...)
	END_SHAREDVARS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define BEGIN_SHAREDVARS {
#define END_SHAREDVARS   }


		 /*******************************
		 *	 ATTRIBUTED VARS	*
		 *******************************/

#ifdef O_ATTVAR
#define CHECK_WAKEUP \
	if ( unlikely(LD->alerted & ALERT_WAKEUP) ) \
	{ LD->alerted &= ~ALERT_WAKEUP; \
	  if ( *valTermRef(LD->attvar.head) ) \
	    goto wakeup; \
	}
#else
#define CHECK_WAKEUP (void)0
#endif


		 /*******************************
		 *	    DEBUGGING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
D_BREAK implements break-points in the  code.   A  break-point is set by
replacing  an  instruction  by  a   D_BREAK  instruction.  The  orininal
instruction is saved in a table. replacedBreak() fetches it.

We simply switch to trace-mode, which will   trap the tracer on the next
breakable instruction (which is what D_BREAK is supposed to replace).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(D_BREAK, 0, 0, ())
{
#if O_DEBUGGER
  if ( debugstatus.debugging )
  { debugstatus.tracing = TRUE;		/* HACK: avoid printMessage() */
    tracemode(TRUE, NULL);		/* in tracemode() */
    DEBUG(1, Sdprintf("Hit break\n"));
  }
#if VMCODE_IS_ADDRESS
  { void *c = (void *)replacedBreak(PC-1);

    goto *c;
  }
#else
  thiscode = replacedBreak(PC-1);
  goto resumebreak;
#endif
#endif /*O_DEBUGGER*/
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_NOP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_NOP, 0, 0, ())
{ NEXT_INSTRUCTION;
}

		 /*******************************
		 *	     HEAD UNIFY		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_CONST is used for an atomic constant in   the head of the clause. ARGP
points to the current argument to  be   matched.  ARGP is derefenced and
unified with a constant argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_CONST, 0, 1, (CA1_DATA))
{ word c;
  Word k;

  IF_WRITE_MODE_GOTO(B_CONST);

  c = (word)*PC++;
  deRef2(ARGP, k);
  if ( *k == c )
  { ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( canBind(*k) )
  { if ( !hasGlobalSpace(0) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(0, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, k);
    }
    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_NIL is used for [] in the head.  See H_CONST for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_NIL, 0, 0, ())
{ word c;
  Word k;

  IF_WRITE_MODE_GOTO(B_NIL);

  c = ATOM_nil;
  deRef2(ARGP, k);
  if ( *k == c )
  { ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( canBind(*k) )
  { if ( !hasGlobalSpace(0) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(0, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, k);
    }
    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_INTEGER: Long integer in  the  head.   Note  that  small  integers are
handled through H_CONST. Copy to the  global   stack  if the argument is
variable, compare the numbers otherwise.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_INTEGER, 0, 1, (CA1_INTEGER))
{ Word k;

  IF_WRITE_MODE_GOTO(B_INTEGER);

  deRef2(ARGP, k);
  if ( canBind(*k) )
  { Word p;
    word c;
    union
    { int64_t val;
      word w[WORDS_PER_INT64];
    } cvt;
    Word vp = cvt.w;

    if ( !hasGlobalSpace(2+WORDS_PER_INT64) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(2+WORDS_PER_INT64, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, k);
    }

    p = gTop;
    gTop += 2+WORDS_PER_INT64;
    c = consPtr(p, TAG_INTEGER|STG_GLOBAL);

    cvt.val = (int64_t)(intptr_t)*PC++;
    *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
    cpInt64Data(p, vp);
    *p = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);

    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  } else if ( isBignum(*k) && valBignum(*k) == (intptr_t)*PC++ )
  { ARGP++;
    NEXT_INSTRUCTION;
  }

  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_INT64: 64-bit integer in the head. Only applicable for 32-bit hardware
as this is the same as H_INTEGER on 64-bit hardware.

TBD: Compile conditionally
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_INT64, 0, WORDS_PER_INT64, (CA1_INT64))
{ Word k;

  IF_WRITE_MODE_GOTO(B_INT64);

  deRef2(ARGP, k);
  if ( canBind(*k) )
  { Word p;
    word c;

    if ( !hasGlobalSpace(2+WORDS_PER_INT64) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(2+WORDS_PER_INT64, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, k);
    }

    p = gTop;
    gTop += 2+WORDS_PER_INT64;
    c = consPtr(p, TAG_INTEGER|STG_GLOBAL);

    *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
    cpInt64Data(p, PC);
    *p = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);

    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  } else if ( isBignum(*k) )
  { Word vk = valIndirectP(*k);
    size_t i;

    for(i=0; i<WORDS_PER_INT64; i++)
    { if ( *vk++ != (word)*PC++ )
	CLAUSE_FAILED;
    }
    ARGP++;
    NEXT_INSTRUCTION;
  }

  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_FLOAT: Float in the head. The  float   follows  the instruction and is
represented as a native C-double.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_FLOAT, 0, WORDS_PER_DOUBLE, (CA1_FLOAT))
{ Word k;

  IF_WRITE_MODE_GOTO(B_FLOAT);

  deRef2(ARGP, k);
  if ( canBind(*k) )
  { Word p;
    word c;

    if ( !hasGlobalSpace(2+WORDS_PER_DOUBLE) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(2+WORDS_PER_DOUBLE, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, k);
    }

    p = gTop;
    gTop += 2+WORDS_PER_DOUBLE;
    c = consPtr(p, TAG_FLOAT|STG_GLOBAL);

    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
    cpDoubleData(p, PC);
    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);

    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  } else if ( isFloat(*k) )
  { Word p = valIndirectP(*k);

    switch(WORDS_PER_DOUBLE) /* depend on compiler to clean up */
    { case 2:
	if ( *p++ != *PC++ )
	  CLAUSE_FAILED;
      case 1:
	if ( *p++ == *PC++ )
	{ ARGP++;
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;
      default:
	assert(0);
    }
  }

  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_MPZ and H_STRING are used for an mpz   number  and string in the head.
They are both implemented using the generic  code for indirects, but the
decompiler must be able to recognise  the   instruction  which is why we
have two instructions.

TBD:	Deal with multiple identical instructions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{ SEPERATE_VMI;
  VMI_GOTO(H_STRING);
}


VMI(H_STRING, 0, VM_DYNARGC, (CA1_STRING))
{ Word k;

  IF_WRITE_MODE_GOTO(B_STRING);

  deRef2(ARGP, k);
  if ( canBind(*k) )
  { word c;
    size_t sz = gsizeIndirectFromCode(PC);

    if ( !hasGlobalSpace(sz) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(sz, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, k);
    }

    c = globalIndirectFromCode(&PC);
    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( isIndirect(*k) && equalIndirectFromCode(*k, &PC) )
  { ARGP++;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_VOID: A singleton variable in the   head.  Just increment the argument
pointer. Also generated for non-singleton   variables appearing on their
own in the head and  encountered  for   the  first  time.  Note that the
compiler suppresses H_VOID when there are   no other instructions before
I_ENTER or I_EXIT.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_VOID, 0, 0, ())
{ ARGP++;
  NEXT_INSTRUCTION;
}


VMI(H_VOID_N, 0, 1, (CA1_INTEGER))
{ ARGP += (int)*PC++;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_VAR: A variable in the head which is   not an anonymous one and is not
used for the first time. Invoke general unification between the argument
pointer and the variable, whose offset is given relative to the frame.

When in write-mode, ARGP points  to   uninitialised  data  on the global
stack that must be treated as a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_VAR, 0, 1, (CA1_VAR))
{ Word k = varFrameP(FR, (int)*PC++);
  int rc;

  if ( umode == uwrite )
  { if ( LD->prolog_flag.occurs_check == OCCURS_CHECK_FALSE )
    { deRef(k);
      if ( isVar(*k) )
      { if ( k > ARGP )			/* k on local stack */
	{ if ( tTop+1 > tMax )
	  { int rc;

	    SAVE_REGISTERS(qid);
	    rc = ensureTrailSpace(1);
	    LOAD_REGISTERS(qid);
	    if ( rc != TRUE )
	    { raiseStackOverflow(rc);
	      THROW_EXCEPTION;
	    }
	    k = varFrameP(FR, (int)PC[-1]);
	    deRef(k);
	  }
	  setVar(*ARGP);
	  Trail(k, makeRefG(ARGP));
	} else
	{ *ARGP = makeRefG(k);		/* ARGP on global, so k also */
	}
      } else if ( isAttVar(*k) )
      { *ARGP = makeRefG(k);
      } else
      { *ARGP = *k;
      }

      ARGP++;
      NEXT_INSTRUCTION;
    } else
    { setVar(*ARGP);
    }
  }

  SAVE_REGISTERS(qid);
  rc = unify_ptrs(k, ARGP, ALLOW_GC|ALLOW_SHIFT PASS_LD);
  LOAD_REGISTERS(qid);
  if ( rc )
  { ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( exception_term )
    THROW_EXCEPTION;
  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_FIRSTVAR: A variable  in  the  head,   which  is  not  anonymous,  but
encountered for the first time. So we know  that the variable is still a
variable. Copy or make a reference.  Trailing   is  not needed as we are
writing in this frame. As ARGP is pointing   in the argument list, it is
on the local stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_FIRSTVAR, 0, 1, (CA1_VAR))
{ if ( umode == uwrite )
  { setVar(*ARGP);
    varFrame(FR, *PC++) = makeRefG(ARGP);
  } else
  { varFrame(FR, *PC++) = (needsRef(*ARGP) ? makeRef(ARGP) : *ARGP);
  }
  ARGP++;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_FUNCTOR: A functor in the head. If  the current argument is a variable
we instantiate it with a  new  term,   all  whose  arguments  are set to
variables. Otherwise we check the functor  definition. In both case ARGP
is pushed on the  argument  stack  and   set  to  point  to the leftmost
argument of the  term.  Note  that   the  instantiation  is  trailed  as
dereferencing might have caused we are now pointing in a parent frame or
the global stack (should we check? Saves trail! How often?).

H_RFUNCTOR: Right-most functor.  Achieves right-argument optimization of
the argument stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_FUNCTOR, 0, 1, (CA1_FUNC))
{ pushArgumentStack((Word)((intptr_t)(ARGP + 1)|umode));
  VMI_GOTO(H_RFUNCTOR);
}

VMI(H_RFUNCTOR, 0, 1, (CA1_FUNC))
{ functor_t f;
  Word p;

  IF_WRITE_MODE_GOTO(B_RFUNCTOR);

  f = (functor_t) *PC++;
  deRef2(ARGP, p);
  if ( canBind(*p) )
  { int arity = arityFunctor(f);
    Word ap;
    word c;

    if ( !hasGlobalSpace(1+arity) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(1+arity, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      deRef2(ARGP, p);
    }

    ap = gTop;
    gTop += 1+arity;
    c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
    *ap++ = f;
    ARGP = ap;
    while(--arity>=0)			/* must clear if we want to do GC */
      setVar(*ap++);
    bindConst(p, c);
    umode = uwrite;
    NEXT_INSTRUCTION;
  }
  if ( hasFunctor(*p, f) )
  { ARGP = argTermP(*p, 0);
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_LIST:  As H_FUNCTOR, but using ./2 as predefined functor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_LIST, 0, 0, ())
{ pushArgumentStack((Word)((intptr_t)(ARGP + 1)|umode));

  VMI_GOTO(H_RLIST);
}


VMI(H_RLIST, 0, 0, ())
{ Word p;
  word w;

  IF_WRITE_MODE_GOTO(B_RLIST);

  deRef2(ARGP, p);
  w = *p;

  switch(tag(w))
  { case TAG_COMPOUND:
      if ( valueTerm(w)->definition == FUNCTOR_dot2 )
      { ARGP = argTermP(w, 0);
	NEXT_INSTRUCTION;
      }
      CLAUSE_FAILED;
    case TAG_VAR:
    case TAG_ATTVAR:
    { Word ap;
      word c;

      if ( !hasGlobalSpace(3) )
      { int rc;

	SAVE_REGISTERS(qid);
	rc = ensureGlobalSpace(3, ALLOW_GC);
	LOAD_REGISTERS(qid);
	if ( rc != TRUE )
	{ raiseStackOverflow(rc);
	  THROW_EXCEPTION;
	}
	deRef2(ARGP, p);
      }

      ap = gTop;
      gTop += 3;
      c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
      *ap++ = FUNCTOR_dot2;
      setVar(ap[0]);			/* must clear for GC */
      setVar(ap[1]);
      bindConst(p, c);
      ARGP = ap;
      umode = uwrite;
      NEXT_INSTRUCTION;
    }
    default:
      CLAUSE_FAILED;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_POP: Pop the saved argument pointer pushed by H_FUNCTOR and H_LIST.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_POP, 0, 0, ())
{ ARGP = *--aTop;
  umode = ((int)(uintptr_t)ARGP & uwrite);
  ARGP = (Word)((intptr_t)ARGP&~uwrite);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_LIST_FF: [Var1|Var2] in the head where both   Var1 and Var2 appear for
the  first  time.  This  appears    quite  commonly  in  list-processing
predicates:

	pred([], ...).
	pred([H|T], ...) :-
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_LIST_FF, 0, 2, (CA1_VAR,CA1_VAR))
{ Word p;

  if ( umode == uwrite )
  { p = ARGP;
    goto write;
  } else
  { deRef2(ARGP, p);

    if ( isList(*p) )
    { p = argTermP(*p, 0);
      varFrame(FR, *PC++) = (needsRef(*p) ? makeRef(p) : *p);
      p++;
      varFrame(FR, *PC++) = (needsRef(*p) ? makeRef(p) : *p);
    } else if ( canBind(*p) )
    { word c;
      Word ap;

    write:
      if ( !hasGlobalSpace(3) )
      { int rc;

	SAVE_REGISTERS(qid);
	rc = ensureGlobalSpace(3, ALLOW_GC);
	LOAD_REGISTERS(qid);
	if ( rc != TRUE )
	{ raiseStackOverflow(rc);
	  THROW_EXCEPTION;
	}
	if ( umode == uwrite )
	  p = ARGP;
	else
	  deRef2(ARGP, p);
      }

      ap = gTop;
      gTop = ap+3;
      c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
      *ap++ = FUNCTOR_dot2;
      setVar(*ap); varFrame(FR, *PC++) = makeRefG(ap); ap++;
      setVar(*ap); varFrame(FR, *PC++) = makeRefG(ap);
      if ( umode == uwrite )
	*p = c;
      else
	bindConst(p, c);
    } else
    { CLAUSE_FAILED;
    }
  }

  ARGP++;
  NEXT_INSTRUCTION;
}


		 /*******************************
		 *	 BODY UNIFICATION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_CONST, B_NIL: An atomic constant in the body of a clause. We know that
ARGP is pointing to a not yet   instantiated  argument of the next frame
and therefore can just fill the argument. Trailing is not needed as this
is above the stack anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_CONST, 0, 1, (CA1_DATA))
{ *ARGP++ = (word)*PC++;
  NEXT_INSTRUCTION;
}

VMI(B_NIL, 0, 0, ())
{ *ARGP++ = ATOM_nil;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_INTEGER: Long following PC for integers   that  cannot be expressed as
tagged integer.

TBD:	Merge the code writing longs to the stack
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_INTEGER, 0, 1, (CA1_INTEGER))
{ Word p;
  union
  { int64_t val;
    word w[WORDS_PER_INT64];
  } cvt;
  Word vp = cvt.w;

  if ( !hasGlobalSpace(2+WORDS_PER_INT64) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureGlobalSpace(2+WORDS_PER_INT64, ALLOW_GC);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  p = gTop;
  gTop += 2+WORDS_PER_INT64;

  cvt.val = (int64_t)(intptr_t)*PC++;
  *ARGP++ = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
  cpInt64Data(p, vp);
  *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_INT64: 64-bit (int64_t) in the body.  See H_INT64
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_INT64, 0, WORDS_PER_INT64, (CA1_INT64))
{ Word p;
  size_t i;

  if ( !hasGlobalSpace(2+WORDS_PER_INT64) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureGlobalSpace(2+WORDS_PER_INT64, ALLOW_GC);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  p = gTop;
  gTop += 2+WORDS_PER_INT64;

  *ARGP++ = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
  for(i=0; i<WORDS_PER_INT64; i++)
    *p++ = (word)*PC++;
  *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_FLOAT: Float in the  body.  PC  is   followed  by  a  double in native
representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_FLOAT, 0, WORDS_PER_DOUBLE, (CA1_FLOAT))
{ Word p;

  if ( !hasGlobalSpace(2+WORDS_PER_DOUBLE) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureGlobalSpace(2+WORDS_PER_DOUBLE, ALLOW_GC);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  p = gTop;
  gTop += 2+WORDS_PER_DOUBLE;

  *ARGP++ = consPtr(p, TAG_FLOAT|STG_GLOBAL);
  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  cpDoubleData(p, PC);
  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_MPZ: MPZ number in body
B_STRING: string in body

Both copy following indirect to the  global   stack.  See also H_MPZ and
H_STRING.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{ SEPERATE_VMI;
  VMI_GOTO(B_STRING);
}

VMI(B_STRING, 0, VM_DYNARGC, (CA1_STRING))
{ size_t sz = gsizeIndirectFromCode(PC);

  if ( !hasGlobalSpace(sz) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureGlobalSpace(sz, ALLOW_GC);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  *ARGP++ = globalIndirectFromCode(&PC);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_ARGVAR: A variable in the body which is   not an anonymous one, is not
used for the first time and is nested in a term (with B_FUNCTOR). We now
know that *ARGP is a variable, so  we   either  copy the value or make a
reference. The difference between this one and B_VAR is the direction of
the reference link in case *k turns out to be variable.

ARGP is pointing into the term on the global stack we are creating.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_ARGVAR, 0, 1, (CA1_VAR))
{ Word k = varFrameP(FR, *PC++);

  deRef(k);
  if ( isVar(*k) )
  { if ( ARGP < k )
    { if ( tTop+1 > tMax )
      { int rc;

	SAVE_REGISTERS(qid);
	rc = ensureTrailSpace(1);
	LOAD_REGISTERS(qid);
	if ( rc != TRUE )
	{ raiseStackOverflow(rc);
	  assert(exception_term);
	  THROW_EXCEPTION;
	}
	k = varFrameP(FR, (int)PC[-1]);
	deRef(k);
      }
      setVar(*ARGP);
      Trail(k, makeRefG(ARGP++));
      NEXT_INSTRUCTION;
    }
    *ARGP++ = makeRefG(k);	/* both on global stack! */
#ifdef O_ATTVAR
  } else if ( isAttVar(*k) )
  { *ARGP++ = makeRefG(k);
#endif
  } else
  { *ARGP++ = *k;
  }

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_VAR, B_VAR<N>: A variable in the body   which  is not an anonymous one
and is not used for  the  first  time.   We  now  know  that  *ARGP is a
variable, so we either copy the value   or make a reference. Trailing is
not needed as we are writing above the stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_VAR0, 0, 0, ())
{ *ARGP++ = linkVal(varFrameP(FR, VAROFFSET(0)));
  NEXT_INSTRUCTION;
}

VMI(B_VAR1, 0, 0, ())
{ *ARGP++ = linkVal(varFrameP(FR, VAROFFSET(1)));
  NEXT_INSTRUCTION;
}

VMI(B_VAR2, 0, 0, ())
{ *ARGP++ = linkVal(varFrameP(FR, VAROFFSET(2)));
  NEXT_INSTRUCTION;
}

VMI(B_VAR, 0, 1, (CA1_VAR))
{ int n = (int)*PC++;

  *ARGP++ = linkVal(varFrameP(FR, n));
  NEXT_INSTRUCTION;
}


#ifdef O_COMPILE_IS
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_UNIFY_VAR, B_UNIFY_EXIT: Unification in the body. We compile A = Term
into one of the following:

    Normal:			    A is firstvar:
	B_UNIFY_VAR <A>			B_UNIFY_FIRSTVAR <A>
	<head unify instructions>	<body unify instructions>
	B_UNIFY_EXIT			B_UNIFY_EXIT

We  need  B_UNIFY_FIRSTVAR  for   the   debugger    as   well   as   for
clearUninitialisedVarsFrame() in pl-gc.c. When in   debug mode we simply
create a frame for =/2 and call it.

TBD: B_UNIFY_CONST <var>, <const>
     B_UNIFY_VAR <var1>, <var2>

Note  that  the  B_UNIFY_FIRSTVAR  assumes  write   mode,  but  this  is
unimportant because the compiler generates write (B_*) instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_FIRSTVAR, 0, 1, (CA1_VAR))
{ ARGP = varFrameP(FR, (int)*PC++);
  setVar(*ARGP);			/* needed for GC */
  goto unify_var_cont;
}


VMI(B_UNIFY_VAR, 0, 1, (CA1_VAR))
{ ARGP = varFrameP(FR, (int)*PC++);

unify_var_cont:
#if O_DEBUGGER
  if ( debugstatus.debugging )
  { Word k = ARGP;

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(k);
    setVar(*ARGP);
    umode = uwrite;			/* must write for GC to work */
    NEXT_INSTRUCTION;
  }
#endif

  umode = uread;			/* needed? */
  NEXT_INSTRUCTION;
}


VMI(B_UNIFY_EXIT, VIF_BREAK, 0, ())
{ ARGP = argFrameP(lTop, 0);

#if O_DEBUGGER
  if ( debugstatus.debugging )
  { NFR = lTop;
    DEF = GD->procedures.equals2->definition;
    setNextFrameFlags(NFR, FR);
    goto normal_call;
  }
#endif

  CHECK_WAKEUP;				/* only for non-first-var */
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	B_UNIFY_[FV][FV] VAR1 VAR2
Unify two variables.  F stands for a first-var; V for any other var
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_FF, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { setVar(*v1);
    setVar(*v2);
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = linkVal(v2);
    goto debug_equals2;
  }
#endif

  setVar(*v1);
  *v2 = makeRefL(v1);

  NEXT_INSTRUCTION;
}


VMI(B_UNIFY_FV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { setVar(*v1);
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = linkVal(v2);
    goto debug_equals2;
  }
#endif

  *v1 = linkVal(v2);

  NEXT_INSTRUCTION;
}


VMI(B_UNIFY_VV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ int rc;
  Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = linkVal(v2);
  debug_equals2:
    NFR = lTop;
    DEF = GD->procedures.equals2->definition;
    setNextFrameFlags(NFR, FR);
    goto normal_call;
  }
#endif

  SAVE_REGISTERS(qid);
  rc = unify_ptrs(v1, v2, ALLOW_GC|ALLOW_SHIFT PASS_LD);
  LOAD_REGISTERS(qid);
  if ( rc )
  { CHECK_WAKEUP;
    NEXT_INSTRUCTION;
  }
  if ( exception_term )
    THROW_EXCEPTION;

  BODY_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_UNIFY_FC: Unify first variable with a constant.  Always succeeds, no
need for wakeup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_FC, VIF_BREAK, 2, (CA1_VAR, CA1_DATA))
{ Word v1 = varFrameP(FR, (int)*PC++);
  word c = (word)*PC++;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { setVar(*v1);
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = c;
    goto debug_equals2;
  }
#endif

  *v1 = c;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_UNIFY_VC: Unify a variable (not first) with a constant in the body.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_VC, VIF_BREAK, 2, (CA1_VAR, CA1_DATA))
{ Word k = varFrameP(FR, (int)*PC++);
  word c = (word)*PC++;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(k);
    *ARGP++ = c;
    goto debug_equals2;
  }
#endif

  deRef(k);
  if ( *k == c )
    NEXT_INSTRUCTION;
  if ( canBind(*k) )
  { if ( !hasGlobalSpace(0) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(0, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
      k = varFrameP(FR, (int)PC[-2]);
      deRef(k);
    }

    bindConst(k, c);
    CHECK_WAKEUP;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_EQ_VV: translation of	Var1 == Var2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_EQ_VV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);
  int rc;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = linkVal(v2);
  debug_eq_vv:
    NFR = lTop;
    DEF = GD->procedures.strict_equal2->definition;
    setNextFrameFlags(NFR, FR);
    goto normal_call;
  }
#endif

  if ( (rc=compareStandard(v1, v2, TRUE PASS_LD)) == 0 )
    NEXT_INSTRUCTION;
  if ( rc == CMP_ERROR )
    THROW_EXCEPTION;

  BODY_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_EQ_VC Var == constant
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_EQ_VC, VIF_BREAK, 2, (CA1_VAR,CA1_DATA))
{ Word v1 = varFrameP(FR, (int)*PC++);
  word c  = (word)*PC++;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = c;
    goto debug_eq_vv;
  }
#endif

  deRef(v1);
  if ( *v1 == c )
    NEXT_INSTRUCTION;

  BODY_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_NEQ_VV: translation of Var1 \== Var2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_NEQ_VV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);
  int rc;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = linkVal(v2);
  debug_neq_vv:
    NFR = lTop;
    DEF = GD->procedures.not_strict_equal2->definition;
    setNextFrameFlags(NFR, FR);
    goto normal_call;
  }
#endif

  if ( (rc=compareStandard(v1, v2, TRUE PASS_LD)) == 0 )
    BODY_FAILED;
  if ( rc == CMP_ERROR )
    THROW_EXCEPTION;

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_NEQ_VC Var == constant
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_NEQ_VC, VIF_BREAK, 2, (CA1_VAR,CA1_DATA))
{ Word v1 = varFrameP(FR, (int)*PC++);
  word c  = (word)*PC++;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkVal(v1);
    *ARGP++ = c;
    goto debug_neq_vv;
  }
#endif

  deRef(v1);
  if ( *v1 != c )
    NEXT_INSTRUCTION;

  BODY_FAILED;
}


#endif /*O_COMPILE_IS*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_ARGFIRSTVAR: A variable in the body nested  in a term, encountered for
the first time. We now know both   *ARGP and the variable are variables.
ARGP points to the argument of a term on the global stack. The reference
should therefore go from k to ARGP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_ARGFIRSTVAR, 0, 1, (CA1_VAR))
{ setVar(*ARGP);
  varFrame(FR, *PC++) = makeRefG(ARGP++);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_FIRSTVAR: A variable in the body, encountered   for the first time. We
now know both *ARGP and the variable  are variables. We set the variable
to be a variable (it is uninitialised   memory) and make a reference. No
trailing needed as we are writing in this and the next frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_FIRSTVAR, 0, 1, (CA1_VAR))
{ Word k = varFrameP(FR, *PC++);

  setVar(*k);
  *ARGP++ = makeRefL(k);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_VOID: A singleton variable in  the  body.   Ensure  the  argument is a
variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_VOID, 0, 0, ())
{ setVar(*ARGP++);
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_FUNCTOR: A functor in the body. As we   don't  expect ARGP to point to
initialised memory while in body mode  we   just  allocate the term, but
don't initialise the arguments to variables. Allocation is done in place
to avoid a function call.

B_RFUNCTOR: right-argument recursive version of B_FUNCTOR
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_FUNCTOR, 0, 1, (CA1_FUNC))
{ pushArgumentStack(ARGP+1);
  VMI_GOTO(B_RFUNCTOR);
}


VMI(B_RFUNCTOR, 0, 1, (CA1_FUNC))
{ functor_t f = (functor_t) *PC++;
  int arity = arityFunctor(f);
  Word ap;

  if ( !hasGlobalSpace(1+arity) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureGlobalSpace(1+arity, ALLOW_GC);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  *ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
  ARGP = gTop;
  *ARGP++ = f;
  for(ap=ARGP; arity-->0;)		/* must clear if we want to do GC */
    setVar(*ap++);
  gTop = ap;

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_LIST: Same as B_FUNCTOR for ./2
B_RLIST: Right-argument recursive B_LIST
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_LIST, 0, 0, ())
{ pushArgumentStack(ARGP+1);
  VMI_GOTO(B_RLIST);
}


VMI(B_RLIST, 0, 0, ())
{ if ( !hasGlobalSpace(3) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureGlobalSpace(3, ALLOW_GC);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  *ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
  ARGP = gTop;
  *ARGP++ = FUNCTOR_dot2;
  setVar(ARGP[0]);
  setVar(ARGP[1]);
  gTop = ARGP+2;

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_POPF: Pop the saved argument pointer (see H_FUNCTOR and B_FUNCTOR).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_POP, 0, 0, ())
{ ARGP = *--aTop;
  NEXT_INSTRUCTION;
}


		 /*******************************
		 *	       ENTER		*
		 *******************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_ENTER: Enter the body of the clause.   This instruction is left out if
the clause has no body. The basic task   of  this instruction is to move
ARGP from the argument part of this frame  into the argument part of the
child frame to be built. `BFR' (the last frame with alternatives) is set
to  this  frame  if  this  frame  has  alternatives,  otherwise  to  the
backtrackFrame of this frame.

If this frame has no alternatives it is possible to  put  the  backtrack
frame  immediately  on  the backtrack frame of this frame.  This however
makes debugging much more  difficult  as  the  system  will  do  a  deep
backtrack without showing the fail ports explicitely.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_ENTER, VIF_BREAK, 0, ())
{ ARGP = argFrameP(lTop, 0);

  if ( unlikely(LD->alerted) )
  {
#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;

      SAVE_REGISTERS(qid);
      clearUninitialisedVarsFrame(FR, PC);
      action = tracePort(FR, BFR, UNIFY_PORT, PC PASS_LD);
      LOAD_REGISTERS(qid);

      switch( action )
      { case ACTION_RETRY:
	  goto retry;
	case ACTION_FAIL:
	  FRAME_FAILED;
	case ACTION_ABORT:
	  THROW_EXCEPTION;
      }
    }
#endif /*O_DEBUGGER*/

    CHECK_WAKEUP;
  }
  NEXT_INSTRUCTION;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CONTEXT is used by  non-meta  predicates   that  are  compiled  into a
different  module  using  <module>:<head>  :-    <body>.  The  I_CONTEXT
instruction immediately follows the I_ENTER. The argument is the module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CONTEXT, 0, 1, (CA1_MODULE))
{ Module m = (Module)*PC++;

  setContextModule(FR, m);

  NEXT_INSTRUCTION;
}

		 /*******************************
		 *	       CALLING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALL forms the normal  code  generated   by  the  compiler for calling
predicates. The arguments are already written   in the frame starting at
`lTop'.

The task of I_CALL is to  save  necessary  information  in  the  current
frame,  fill  the next frame and initialise the machine registers.  Then
execution can continue at `next_instruction'
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALL, VIF_BREAK, 1, (CA1_PROC))
{ Procedure proc = (Procedure) *PC++;

  NFR = lTop;
  setNextFrameFlags(NFR, FR);
  DEF = proc->definition;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the common part of the call variations.  By now the following is
true:

  - NFR			Points to new frame
  - arguments		filled
  - DEF			filled with predicate to call
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

normal_call:

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise those slots of the frame that are common to Prolog predicates
and foreign ones.  There might be some possibilities for optimisation by
delaying these initialisations till they are really  needed  or  because
the information they are calculated from is destroyed.  This probably is
not worthwile.

Note: we are working above `lTop' here!!   We restore this as quickly as
possible to be able to call-back to Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  NFR->parent         = FR;
  NFR->predicate      = DEF;		/* TBD */
  NFR->programPointer = PC;		/* save PC in child */
  NFR->clause         = NULL;		/* for save atom-gc */
  environment_frame = FR = NFR;		/* open the frame */

  if ( unlikely(addPointer(lTop, LOCAL_MARGIN) > (void*)lMax) )
  { int rc;

    lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
    SAVE_REGISTERS(qid);
    rc = ensureLocalSpace(LOCAL_MARGIN, ALLOW_SHIFT);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { rc = raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

depart_continue:
retry_continue:
#ifdef O_LOGICAL_UPDATE
  FR->generation     = GD->generation;
#endif
#ifdef O_PROFILE
  FR->prof_node = NULL;
#endif
  LD->statistics.inferences++;

#ifdef O_DEBUGLOCAL
{ Word ap = argFrameP(FR, DEF->functor->arity);
  int n;

  for(n=50; --n; )
    *ap++ = (word)(((char*)ATOM_nil) + 1);
}
#endif

  if ( unlikely(LD->alerted) )
  {					/* play safe */
    lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);

    if ( is_signalled(PASS_LD1) )
    { SAVE_REGISTERS(qid);
      handleSignals(PASS_LD1);
      LOAD_REGISTERS(qid);
      if ( exception_term )
      { CL = NULL;

	enterDefinition(DEF);
					/* The catch is not yet installed, */
					/* so we ignore it */
	if ( FR->predicate == PROCEDURE_catch3->definition )
	  set(FR, FR_CATCHED);

	THROW_EXCEPTION;
      }
    }

#ifdef O_PROFILE
    if ( LD->profile.active )
      FR->prof_node = profCall(DEF PASS_LD);
#endif

#ifdef O_LIMIT_DEPTH
    { unsigned int depth = levelFrame(FR);

      if ( depth > depth_reached )
	depth_reached = depth;
      if ( depth > depth_limit )
      { DEBUG(2, Sdprintf("depth-limit\n"));

	if ( debugstatus.debugging )
	  newChoice(CHP_DEBUG, FR PASS_LD);
	FRAME_FAILED;
      }
    }
#endif

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int rc;

      lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
      SAVE_REGISTERS(qid);
      DEF = getProcDefinedDefinition(DEF PASS_LD);
      LOAD_REGISTERS(qid);
      if ( FR->predicate != DEF )		/* auto imported/loaded */
      { FR->predicate = DEF;
#ifdef O_PROFILE
        if ( FR->prof_node )
	  profSetHandle(FR->prof_node, DEF);
#endif
        goto retry_continue;
      }
      set(FR, FR_INBOX);

      SAVE_REGISTERS(qid);
      rc = tracePort(FR, BFR, CALL_PORT, NULL PASS_LD);
      LOAD_REGISTERS(qid);
      switch( rc )
      { case ACTION_FAIL:   FRAME_FAILED;
	case ACTION_IGNORE: VMI_GOTO(I_EXIT);
	case ACTION_ABORT:  THROW_EXCEPTION;
	case ACTION_RETRY:
	  if ( debugstatus.retryFrame )
	    goto retry;			/* otherwise retrying the call-port */
					/* is a no-op */
      }
    }
#endif /*O_DEBUGGER*/
  }

  PC = DEF->codes;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_DEPART: implies it is the last subclause   of  the clause. This is the
entry point for last call optimisation.

(*) Handling undefined predicates here is very tricky because we need to
destroy de clause pointer before   leaveDefinition() to get asynchronous
atom-gc ok, but this destroys  the  environment   for  normal  GC if the
undefined predicate trapping code starts a GC. Therefore, undefined code
runs normal I_CALL. This isn't too  bad,   as  it only affects the first
call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_DEPART, VIF_BREAK, 1, (CA1_PROC))
{ if ( (void *)BFR <= (void *)FR && truePrologFlag(PLFLAG_LASTCALL) )
  { Procedure proc = (Procedure) *PC++;

    if ( !proc->definition->impl.any &&	/* see (*) */
	 false(proc->definition, PROC_DEFINED) )
    { PC--;
      VMI_GOTO(I_CALL);
    }

    if ( true(FR, FR_WATCHED) )
    { LocalFrame lSave = lTop;
      lTop = (LocalFrame)ARGP;		/* just pushed arguments, so top */
      SAVE_REGISTERS(qid);
      frameFinished(FR, FINISH_EXIT PASS_LD);
      LOAD_REGISTERS(qid);
      lTop = lSave;
    }

    FR->clause = NULL;			/* for save atom-gc */
    leaveDefinition(DEF);
    DEF = proc->definition;
    if ( true(DEF, P_TRANSPARENT) )
    { FR->context = contextModule(FR);
      FR->level++;
      clear(FR, FR_CLEAR_NEXT);
      set(FR, FR_CONTEXT);
    } else
    { setNextFrameFlags(FR, FR);
    }
    if ( true(DEF, HIDE_CHILDS) )
      set(FR, FR_HIDE_CHILDS);

    FR->predicate = DEF;
    copyFrameArguments(lTop, FR, DEF->functor->arity PASS_LD);

    END_PROF();
    START_PROF(DEPART_CONTINUE, "DEPART_CONTINUE");
    goto depart_continue;
  }

  VMI_GOTO(I_CALL);
}


#ifdef O_CALL_AT_MODULE
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_DEPARTATM: procedure-module, context-module, procedure
See I_CALLATM for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
VMI(I_DEPARTATM, VIF_BREAK, 3, (CA1_MODULE, CA1_MODULE, CA1_PROC))
{ PC++;						/* Ignore :-qualifier */
  VMI_GOTO(I_DEPARTM);
}
#endif


VMI(I_DEPARTM, VIF_BREAK, 2, (CA1_MODULE, CA1_PROC))
{ if ( (void *)BFR > (void *)FR || !truePrologFlag(PLFLAG_LASTCALL) )
  { VMI_GOTO(I_CALLM);
  } else
  { Module m = (Module)*PC++;

    setContextModule(FR, m);
    VMI_GOTO(I_DEPART);
  }
}


		 /*******************************
		 *	       EXIT		*
		 *******************************/

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_EXIT: End of clauses for normal Prolog clauses.

TBD: Insert a layer in between, so   this  never has to handle call-back
from C.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_EXIT, VIF_BREAK, 0, ())
{ LocalFrame leave;

  if ( unlikely(LD->alerted) )
  {
#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;

      SAVE_REGISTERS(qid);
      action = tracePort(FR, BFR, EXIT_PORT, PC PASS_LD);
      LOAD_REGISTERS(qid);

      switch( action )
      { case ACTION_RETRY:
	  goto retry;
	case ACTION_FAIL:
	  discardChoicesAfter(FR, FINISH_CUT PASS_LD);
	  FRAME_FAILED;
	case ACTION_ABORT:
	  THROW_EXCEPTION;
      }

      if ( BFR && BFR->type == CHP_DEBUG && BFR->frame == FR )
	BFR = BFR->parent;
    }
#endif /*O_DEBUGGER*/
  }

  if ( (void *)BFR <= (void *)FR )	/* deterministic */
  { leave = true(FR, FR_WATCHED) ? FR : NULL;
    FR->clause = NULL;			/* leaveDefinition() destroys clause */
    leaveDefinition(DEF);		/* dynamic pred only */
    lTop = FR;
    DEBUG(3, Sdprintf("Deterministic exit of %s, lTop = #%ld\n",
		      predicateName(FR->predicate), loffset(lTop)));
  } else
  { leave = NULL;
    clear(FR, FR_INBOX);
  }

  PC = FR->programPointer;
  environment_frame = FR = FR->parent;
  DEF = FR->predicate;
  ARGP = argFrameP(lTop, 0);
  Profile(profResumeParent(FR->prof_node PASS_LD));
  if ( leave )
  { SAVE_REGISTERS(qid);
    frameFinished(leave, FINISH_EXIT PASS_LD);
    LOAD_REGISTERS(qid);
  }

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_EXITFACT: generated to close a fact. The   reason for not generating a
plain I_EXIT is first of all that the actual sequence should be I_ENTER,
I_EXIT,  and  just  optimising   to    I_EXIT   looses   the  unify-port
interception. Second, there should be some room for optimisation here.

The exit_checking_wakeup is referenced from I_FEXITNDET.   If there is a
wakeup and our current goal is  deterministic,   we  first pop it (i.e.,
some sort of last-call optimization).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_EXITFACT, 0, 0, ())
{ if ( unlikely(LD->alerted) )
  {
#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;

      SAVE_REGISTERS(qid);
      action = tracePort(FR, BFR, UNIFY_PORT, PC PASS_LD);
      LOAD_REGISTERS(qid);

      switch( action )
      { case ACTION_RETRY:
	  goto retry;
	case ACTION_ABORT:
	  THROW_EXCEPTION;
      }
    }
#endif /*O_DEBUGGER*/
  exit_checking_wakeup:
#ifdef O_ATTVAR
    if ( LD->alerted & ALERT_WAKEUP )
    { LD->alerted &= ~ALERT_WAKEUP;

      if ( *valTermRef(LD->attvar.head) )
      { PC = SUPERVISOR(exit);
	goto wakeup;
      }
    }
#endif
  }

  VMI_GOTO(I_EXIT);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Created for the return of the toplevel query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_EXITQUERY, 0, 0, ())
{ assert(!FR->parent);

  QF = QueryFromQid(qid);		/* may be shifted: recompute */
  QF->solutions++;

  assert(FR == &QF->top_frame);

  if ( BFR == &QF->choice )		/* No alternatives */
  { set(QF, PL_Q_DETERMINISTIC);
    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
    FR->clause = NULL;

    if ( true(FR, FR_WATCHED) )
    { SAVE_REGISTERS(qid);
      frameFinished(FR, FINISH_EXIT PASS_LD);
      LOAD_REGISTERS(qid);
    }
  }

#ifdef O_PROFILE
  if ( LD->profile.active )
  { LocalFrame parent = parentFrame(FR);

    if ( parent )
      profResumeParent(parent->prof_node PASS_LD);
    else
      profResumeParent(NULL PASS_LD);
  }
#endif

  QF->foreign_frame = PL_open_foreign_frame();
  assert(LD->exception.throw_environment == &throw_env);
  LD->exception.throw_environment = throw_env.parent;

  succeed;
}



		 /*******************************
		 *	      CONTROL		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CUT: !. Task is to detroy  all   choicepoints  newer  then the current
frame. If we are in  debug-mode  we   create  a  new  CHP_DEBUG frame to
provide proper debugger output.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CUT, VIF_BREAK, 0, ())
{
#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { int rc;
    Choice ch;
    mark m;

    SAVE_REGISTERS(qid);
    rc = tracePort(FR, BFR, CUT_CALL_PORT, PC PASS_LD);
    LOAD_REGISTERS(qid);
    switch( rc )
    { case ACTION_RETRY:
	goto retry;
      case ACTION_FAIL:
	FRAME_FAILED;
      case ACTION_ABORT:
	THROW_EXCEPTION;
    }

    if ( (ch = findStartChoice(FR, BFR)) )
    { m = ch->mark;
      SAVE_REGISTERS(qid);
      dbg_discardChoicesAfter(FR PASS_LD);
      LOAD_REGISTERS(qid);
      lTop = (LocalFrame) argFrameP(FR, CL->value.clause->variables);
      ch = newChoice(CHP_DEBUG, FR PASS_LD);
      ch->mark = m;
    } else
    { dbg_discardChoicesAfter(FR PASS_LD);
      lTop = (LocalFrame) argFrameP(FR, CL->value.clause->variables);
    }
    ARGP = argFrameP(lTop, 0);
    if ( exception_term )
      THROW_EXCEPTION;

    SAVE_REGISTERS(qid);
    rc = tracePort(FR, BFR, CUT_EXIT_PORT, PC PASS_LD);
    LOAD_REGISTERS(qid);
    switch( rc )
    { case ACTION_RETRY:
	goto retry;
      case ACTION_FAIL:
	FRAME_FAILED;
      case ACTION_ABORT:
	THROW_EXCEPTION;
    }
  } else
#endif
  { SAVE_REGISTERS(qid);
    discardChoicesAfter(FR, FINISH_CUT PASS_LD);
    LOAD_REGISTERS(qid);
    lTop = (LocalFrame) argFrameP(FR, CL->value.clause->variables);
    ARGP = argFrameP(lTop, 0);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_JMP skips the amount stated in the pointed argument. The PC++ could be
compiled out, but this is a bit more neath.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_JMP, 0, 1, (CA1_JUMP))
{ PC += *PC;
  PC++;

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_OR: Create choice-point in the clause.  Argument is the amount to skip
if the choice-point needs to be activated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_OR, 0, 1, (CA1_JUMP))
{ size_t skip = *PC++;
  Choice ch;

  if ( addPointer(lTop, sizeof(struct choice)) > (void*)lMax )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = ensureLocalSpace(sizeof(*ch), ALLOW_SHIFT);
    LOAD_REGISTERS(qid);
    if ( rc != TRUE )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  ch = newChoice(CHP_JUMP, FR PASS_LD);
  ch->value.PC = PC+skip;
  ARGP = argFrameP(lTop, 0);

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_IFTHEN saves the value of BFR (current   backtrack frame) into a local
frame slot reserved by the compiler. Note  that the variable to hold the
local-frame pointer is  *not*  reserved   in  clause->variables,  so the
garbage collector won't see it. We use  a term-reference because using a
relative address simplifies the stack-shifter.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_SOFTIFTHEN, 0, 1, (CA1_CHP))
{ SEPERATE_VMI;
  VMI_GOTO(C_IFTHEN);
}

VMI(C_IFTHEN, 0, 1, (CA1_CHP))
{ varFrame(FR, *PC++) = consTermRef(BFR);

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_IFTHENELSE: contraction of C_IFTHEN and C_OR.  This contraction has been
made to help the decompiler distinguis between (a ; b) -> c and a -> b ;
c,  which  would  otherwise  only  be    possible  to  distinguis  using
look-ahead.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_NOT, 0, 2, (CA1_CHP,CA1_JUMP))
{ SEPERATE_VMI;
  VMI_GOTO(C_IFTHENELSE);
}


VMI(C_IFTHENELSE, 0, 2, (CA1_CHP,CA1_JUMP))
{ varFrame(FR, *PC++) = consTermRef(BFR); /* == C_IFTHEN */

  VMI_GOTO(C_OR);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_VAR is generated by the compiler to ensure the  instantiation  pattern
of  the  variables  is  the  same after finishing both paths of the `or'
wired in the clause.  Its task is to make the n-th variable slot of  the
current frame to be a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_VAR, 0, 1, (CA1_VAR))
{ setVar(varFrame(FR, *PC++));

  NEXT_INSTRUCTION;
}


VMI(C_VAR_N, 0, 2, (CA1_VAR,CA1_INTEGER))
{ Word vp = varFrameP(FR, *PC++);
  size_t count = *PC++;

  while(count--)
    setVar(*vp++);

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_CUT  destroys  all  backtrack  points    created  after  the  C_IFTHEN
instruction in this clause. It assumes the  value of BFR has been stored
in the nth-variable slot of the current local frame.

We can dereference all frames that are older that the old backtrackframe
and older than this frame.

All frames created since what becomes now the  backtrack  point  can  be
discarded.

C_LCUT  results  from  !'s  encountered  in    the   condition  part  of
if->then;else and \+ (which  is  (g->fail;true)).   It  should  cut  all
choices created since the mark, but not   the mark itself. The test-case
is  a  :-  \+  (b,  !,  fail),    which   should  succeed.  The  current
implementation  walks  twice  over  the    choice-points,  but  cuts  in
conditions should be rare (I hope :-).

C_LSCUT does the same for the condition   part  of soft-cut (*->). Here,
the choice argument is the new choice  created by the disjunction, so we
must get its parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
BEGIN_SHAREDVARS
  Choice och;
  LocalFrame fr;
  Choice ch;

VMI(C_LSCUT, 0, 1, (CA1_CHP))
{ och = (Choice) valTermRef(varFrame(FR, *PC));
  och = och->parent;
  goto c_lcut_cont;
}

VMI(C_LCUT, 0, 1, (CA1_CHP))
{ och = (Choice) valTermRef(varFrame(FR, *PC));
c_lcut_cont:
  PC++;

  for(ch=BFR; ch; ch = ch->parent)
  { if ( ch->parent == och )
    { och = ch;
      goto c_cut;
    }
  }
  assert(BFR == och);			/* no choicepoint yet */
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CUTCHP cuts all  choice-points  after   the  specified  argument. This
instruction is generated for $cut(Var), used by prolog_cut_to(Choice).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CUTCHP, 0, 0, ())
{ Word a = argFrameP(FR, 0);

#define valid_choice(ch) \
	(  (int)ch->type >= 0 && (int)ch->type <= CHP_DEBUG && \
	   onStack(local, ch->frame) \
	)

  deRef(a);
  if ( isInteger(*a) && storage(*a) == STG_INLINE )
  { intptr_t i = valInt(*a);
    och = ((Choice)((Word)lBase + i));

    if ( !(och >= (Choice)lBase && och < (Choice)lTop) ||
	 !valid_choice(och) )
    { PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_choice, consTermRef(a));
      THROW_EXCEPTION;
    }

    goto c_cut;
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_choice, consTermRef(a));
    THROW_EXCEPTION;
  }
}


VMI(C_SCUT, 0, 1, (CA1_CHP))
{ NEXT_INSTRUCTION;
}

VMI(C_LCUTIFTHEN, 0, 1, (CA1_CHP))
{ SEPERATE_VMI;
  VMI_GOTO(C_CUT);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_CUT implements -> (and most of  *->).   Unfortunately  we have to loop
twice because calling discardFrame() in the   first  loop can invalidate
the stacks and make GC calls from frameFinished() invalid.

It might be possible to fix this by updating pointers in the first loop,
but that is complicated and might  well  turn   out  to  be slower as it
involves more write operations.

See also discardChoicesAfter();
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_CUT, 0, 1, (CA1_CHP))
{ och = (Choice) valTermRef(varFrame(FR, *PC));
  PC++;					/* cannot be in macro! */
c_cut:
  if ( !och || FR > och->frame )	/* most recent frame to keep */
    fr = FR;
  else
    fr = och->frame;

  assert(BFR>=och);
  for(ch=BFR; ch > och; ch = ch->parent)
  { LocalFrame fr2;
    LocalFrame delto;

    if ( ch->parent && ch->parent->frame > fr )
      delto = ch->parent->frame;
    else
      delto = fr;

    DEBUG(3, Sdprintf("Discarding %s\n", chp_chars(ch)));

    for(fr2 = ch->frame;
	fr2 > delto;
	fr2 = fr2->parent)
    { assert(fr2->clause || true(fr2->predicate, FOREIGN));

      if ( true(fr2, FR_WATCHED) )
      { char *lSave = (char*)lBase;

	BFR = ch;
	SAVE_REGISTERS(qid);
	frameFinished(fr2, FINISH_CUT PASS_LD);
	LOAD_REGISTERS(qid);
	if ( lSave != (char*)lBase )	/* shifted */
	{ intptr_t offset = (char*)lBase - lSave;

	  fr2 = addPointer(fr2, offset);
	  ch  = addPointer(ch,  offset);
	  assert(ch == BFR);
	  och = addPointer(och, offset);
	  fr  = addPointer(fr,  offset);
	  delto = addPointer(delto, offset);
	}
	if ( exception_term )
	  THROW_EXCEPTION;
      }

      discardFrame(fr2 PASS_LD);
    }

    if ( ch->parent == och )
      DiscardMark(ch->mark);
  }
  assert(och == ch);
  BFR = ch;

  if ( (void *)och > (void *)fr )
  { lTop = (LocalFrame)(och+1);
  } else
  { int nvar = (true(fr->predicate, FOREIGN)
			? fr->predicate->functor->arity
			: fr->clause->value.clause->variables);
    lTop = (LocalFrame) argFrameP(fr, nvar);
  }

  ARGP = argFrameP(lTop, 0);

  DEBUG(3, Sdprintf(" --> BFR = #%ld, lTop = #%ld\n",
		    loffset(BFR), loffset(lTop)));
  NEXT_INSTRUCTION;
}
END_SHAREDVARS


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_SOFTIF: A *-> B ; C is translated to C_SOFIF <A> C_SOFTCUT <B> C_JMP
end <C>.

C_SOFTIF <choice-var> <skip> is  the  same   as  C_OR  <skip>, storing a
reference to the choice-point in <choice-var>

See pl-comp.c and C_SOFTCUT implementation for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
VMI(C_SOFTIF, 0, 2, (CA1_CHP,CA1_JUMP))
{ varFrame(FR, *PC++) = consTermRef(lTop);	/* see C_SOFTCUT */

  VMI_GOTO(C_OR);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_SOFTCUT: Handle the commit-to of A *-> B; C. Simply invalidate the
choicepoint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
VMI(C_SOFTCUT, 0, 1, (CA1_CHP))
{ word ch_ref = varFrame(FR, *PC);
  Choice ch = (Choice) valTermRef(ch_ref);
  Choice bfr = BFR;

  PC++;
  if ( bfr == ch )
  { BFR = bfr->parent;
  } else
  { for(; bfr; bfr=bfr->parent)
    { if ( bfr->parent == ch )
      { bfr->parent = ch->parent;
	break;
      }
    }
  }
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_END is a dummy instruction to help the decompiler to find the end of A
-> B. (Note that a :- (b ->  c),  d  ==   a  :-  (b  ->  c, d) as far as
semantics. They are different terms however.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_END, 0, 0, ())
{ NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_FAIL is equivalent to fail/0. Used to implement \+/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_FAIL, 0, 0, ())
{ BODY_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_FAIL: Translation of fail/0. Same as C_FAIL, but we make a normal call
when in debug-mode, so we can trace the call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_FAIL, VIF_BREAK, 0, ())
{
#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { NFR = lTop;
    setNextFrameFlags(NFR, FR);
    DEF = lookupProcedure(FUNCTOR_fail0, MODULE_system)->definition;

    goto normal_call;
  }
#endif
  BODY_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_TRUE: Translation of true/0.  See also I_FAIL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_TRUE, VIF_BREAK, 0, ())
{
#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { NFR = lTop;
    setNextFrameFlags(NFR, FR);
    DEF = lookupProcedure(FUNCTOR_true0, MODULE_system)->definition;

    goto normal_call;
  }
#endif
  NEXT_INSTRUCTION;
}


BEGIN_SHAREDVARS
functor_t fpred;
Word p;
/** var(@Term)
*/

VMI(I_VAR, VIF_BREAK, 1, (CA1_VAR))
{ p = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( unlikely(debugstatus.debugging) )
  { fpred = FUNCTOR_var1;
  debug_pred1:

    NFR = lTop;
    setNextFrameFlags(NFR, FR);
    DEF  = lookupProcedure(fpred, MODULE_system)->definition;
    ARGP = argFrameP(NFR, 0);
    *ARGP++ = linkVal(p);

    goto normal_call;
  }
#endif

  deRef(p);
  if ( canBind(*p) )
    NEXT_INSTRUCTION;
  BODY_FAILED;
}


/** nonvar(@Term)
*/

VMI(I_NONVAR, VIF_BREAK, 1, (CA1_VAR))
{ p = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( unlikely(debugstatus.debugging) )
  { fpred = FUNCTOR_nonvar1;
    goto debug_pred1;
  }
#endif

  deRef(p);
  if ( !canBind(*p) )
    NEXT_INSTRUCTION;
  BODY_FAILED;
}

END_SHAREDVARS

		 /*******************************
		 *	    SUPERVISORS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_VIRGIN: Fresh, unused predicate. Any new   predicate  is created using
this supervisor (see resetProcedure()). The task of this is to

	* Resolve the definition (i.e. auto-import or auto-load if
	not defined).
	* Check the indexing opportunities and install the proper
        supervisor (see pl-index.c).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_VIRGIN, 0, 0, ())
{ lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);
  SAVE_REGISTERS(qid);
  DEF = getProcDefinedDefinition(DEF PASS_LD);
  LOAD_REGISTERS(qid);

  if ( FR->predicate != DEF )		/* auto imported/loaded */
  { FR->predicate = DEF;
#ifdef O_PROFILE
    if ( FR->prof_node )
      profSetHandle(FR->prof_node, DEF);
#endif
    goto retry_continue;
  }

  if ( createSupervisor(DEF) )
  { PC = DEF->codes;
    NEXT_INSTRUCTION;
  } else				/* TBD: temporary */
  { assert(0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_UNDEF: Undefined predicate. We could make   two  instructions, one for
trapping and one not, but the disadvantage of that is that switching the
unknown flag of the module has no   immediate consequences. Hence we use
one instruction and dynamic checking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_UNDEF, 0, 0, ())
{ switch( getUnknownModule(DEF->module) )
  { case UNKNOWN_ERROR:
    { fid_t fid;
      Definition caller;

      if ( FR->parent )
	caller = FR->parent->predicate;
      else
	caller = NULL;

      lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
      newChoice(CHP_DEBUG, FR PASS_LD);

      SAVE_REGISTERS(qid);
      if ( (fid = PL_open_foreign_frame()) )
      { PL_error(NULL, 0, NULL, ERR_UNDEFINED_PROC, DEF, caller);
	PL_close_foreign_frame(fid);
      }
      LOAD_REGISTERS(qid);

      enterDefinition(DEF);		/* will be left in exception code */

      THROW_EXCEPTION;
    }
    case UNKNOWN_WARNING:
    { fid_t fid;

      lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
      SAVE_REGISTERS(qid);
      if ( (fid = PL_open_foreign_frame()) )
      { term_t pred = PL_new_term_ref();

	if ( !unify_definition(MODULE_user, pred, DEF, 0, GP_NAMEARITY) )
	{ printMessage(ATOM_warning,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_FUNCTOR, FUNCTOR_existence_error2,
		           PL_ATOM, ATOM_procedure,
		           PL_TERM, pred,
			 PL_VARIABLE);
	}
	PL_close_foreign_frame(fid);
      }
      if ( exception_term )
	THROW_EXCEPTION;
      /*FALLTHROUGH*/
    }
    case UNKNOWN_FAIL:
    default:
      FRAME_FAILED;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generic calling code.  This needs to be specialised.

Note that FR->clause is still NULL. We need to keep that while doing the
possible stack-expansion in ENSURE_LOCAL_SPACE(), which   is  why we use
the temporary variable `cl' for storing the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_STATIC, 0, 0, ())
{ ClauseRef cl;
  struct clause_choice chp;

  ARGP = argFrameP(FR, 0);
  lTop = (LocalFrame)ARGP+DEF->functor->arity;

  DEBUG(9, Sdprintf("Searching clause ... "));

  if ( !(cl = firstClause(ARGP, FR, DEF, &chp PASS_LD)) )
  { DEBUG(9, Sdprintf("No clause matching index.\n"));
    if ( debugstatus.debugging )
      newChoice(CHP_DEBUG, FR PASS_LD);

    FRAME_FAILED;
  }
  DEBUG(9, Sdprintf("Clauses found.\n"));

  PC = cl->value.clause->codes;
  lTop = (LocalFrame)(ARGP + cl->value.clause->variables);
  ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);
  CL = cl;

  if ( chp.cref )
  { Choice ch = newChoice(CHP_CLAUSE, FR PASS_LD);
    ch->value.clause = chp;
  } else if ( debugstatus.debugging )
    newChoice(CHP_DEBUG, FR PASS_LD);

  DEBUG(CHK_SECURE,
	{ int argc; int n;
	  argc = DEF->functor->arity;
	  for(n=0; n<argc; n++)
	    checkData(argFrameP(FR, n));
	});

  umode = uread;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_DYNAMIC: Dynamic predicate. Dynamic predicates   must  use the dynamic
indexing and need to lock the predicate. This VMI can also handle static
code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_DYNAMIC, 0, 0, ())
{ enterDefinition(DEF);

  VMI_GOTO(S_STATIC);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_THREAD_LOCAL: Get thread-local definition
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_THREAD_LOCAL, 0, 0, ())
{ FR->predicate = DEF = getProcDefinition__LD(DEF PASS_LD);

  assert(DEF->codes);
  PC = DEF->codes;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_MULTIFILE: Multifile predicate.  These need to be aware of new
clauses that can be added at runtime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_MULTIFILE, 0, 0, ())
{ VMI_GOTO(S_STATIC);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_TRUSTME: Trust this clause. Generated   for  single-clause supervisors
and for the last one of a disjunction.

TBD: get rid of clause-references
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_TRUSTME, 0, 1, (CA1_CLAUSEREF))
{ ClauseRef cref = (ClauseRef)*PC++;

  ARGP = argFrameP(FR, 0);
  TRUST_CLAUSE(cref);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_ALLCLAUSES: Simply try the clauses one-by-one. This works for all code
and is the ultimate fallback of the indexing code.  The supervisor code
is

	S_ALLCLAUSES
	S_NEXTCLAUSE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

BEGIN_SHAREDVARS
ClauseRef cref;

VMI(S_ALLCLAUSES, 0, 0, ())		/* Uses CHP_JUMP */
{ cref = DEF->impl.clauses.first_clause;

next_clause:
  ARGP = argFrameP(FR, 0);
  for(; cref; cref = cref->next)
  { if ( visibleClause(cref->value.clause, FR->generation) )
    { TRY_CLAUSE(cref, cref->next, PC);
    }
  }

  FRAME_FAILED;
}


VMI(S_NEXTCLAUSE, 0, 0, ())
{ cref = CL->next;

  if ( debugstatus.debugging && !debugstatus.suspendTrace )
  { ARGP = argFrameP(FR, 0);
    lTop = (LocalFrame)ARGP + FR->predicate->functor->arity;

    for(; cref; cref = cref->next)
    { if ( visibleClause(cref->value.clause, FR->generation) )
      {	LocalFrame fr;
	CL = cref;

	if ( (fr = dbgRedoFrame(FR, CHP_CLAUSE PASS_LD)) )
	{ int action;

	  SAVE_REGISTERS(qid);
	  action = tracePort(fr, BFR, REDO_PORT, NULL PASS_LD);
	  LOAD_REGISTERS(qid);

	  switch( action )
	  { case ACTION_FAIL:
	      FRAME_FAILED;
	    case ACTION_IGNORE:
	      VMI_GOTO(I_EXIT);
	    case ACTION_RETRY:
	      goto retry_continue;
	    case ACTION_ABORT:
	      THROW_EXCEPTION;
	  }
	}

        break;
      }
    }
  }

  PC--;
  goto next_clause;
}
END_SHAREDVARS


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_LIST: Predicate consisting of two clauses, one of them using [] and
the other [_|_].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_LIST, 0, 2, (CA1_CLAUSEREF, CA1_CLAUSEREF))
{ ClauseRef cref;
  Word k;

  ARGP = argFrameP(FR, 0);
  deRef2(ARGP, k);
  if ( isList(*k) )
    cref = (ClauseRef)PC[1];
  else if ( isNil(*k) )
    cref = (ClauseRef)PC[0];
  else if ( canBind(*k) )
  { PC = SUPERVISOR(staticp) + 1;
    VMI_GOTO(S_STATIC);
  } else
    FRAME_FAILED;

  PC += 2;

  TRUST_CLAUSE(cref);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Meta-predicate  argument  qualification.  S_MQUAL    qualifies  the  Nth
argument. S_LMQUAL does the same and resets   the  context module of the
frame to be definition module of   the  predicate, such that unqualified
calls refer again to  the  definition   module.  This  sequence  must be
processed as part of the supervisor,   notably before creating the first
choicepoint.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_MQUAL, 0, 1, (CA1_VAR))
{ int arg = (int)*PC++;
  int rc;

  SAVE_REGISTERS(qid);
  rc = m_qualify_argument(FR, arg PASS_LD);
  LOAD_REGISTERS(qid);
  if ( rc != TRUE )
  { raiseStackOverflow(rc);
    THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;
}


VMI(S_LMQUAL, 0, 1, (CA1_VAR))
{ int arg = (int)*PC++;
  int rc;

  SAVE_REGISTERS(qid);
  rc = m_qualify_argument(FR, arg PASS_LD);
  LOAD_REGISTERS(qid);
  if ( rc != TRUE )
  { raiseStackOverflow(rc);
    THROW_EXCEPTION;
  }
  setContextModule(FR, FR->predicate->module);

  NEXT_INSTRUCTION;
}


		 /*******************************
		 *	    ARITHMETIC		*
		 *******************************/

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
  A_IS		% unify Y with numeric result
  I_EXIT	% leave the clause

  a_func0:	% executes arithmic function without arguments, pushing
	        % its value on the stack
  a_func1:	% unary function. Changes the top of the stack.
  a_func2:	% binary function. Pops two values and pushes one.

Note that we do not call `ar_func0(*PC++, &ARGP)' as ARGP is a register
variable.  Also, for compilers that do register allocation it is unwise
to give the compiler a hint to put ARGP not into a register.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_ENTER: Ensure the alignment of ARGP   allows  for efficient pushing of
the number structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_ENTER, 0, 0, ())
{ AR_BEGIN();
  NEXT_INSTRUCTION;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_INTEGER: Push long integer following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_INTEGER, 0, 1, (CA1_INTEGER))
{ Number n = allocArithStack(PASS_LD1);

  n->value.i = (intptr_t) *PC++;
  n->type    = V_INTEGER;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_INT64: Push int64_t following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_INT64, 0, WORDS_PER_INT64, (CA1_INT64))
{ Number n = allocArithStack(PASS_LD1);
  Word p = &n->value.w[0];

  cpInt64Data(p, PC);
  n->type    = V_INTEGER;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_MPZ: Push mpz integer following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{
#ifdef O_GMP
  Number n = allocArithStack(PASS_LD1);
  Word p = (Word)PC;
  int size;

  p++;				/* skip indirect header */
  n->type = V_MPZ;
  n->value.mpz->_mp_size  = (int)*p++;
  n->value.mpz->_mp_alloc = 0;	/* avoid de-allocating */
  size = sizeof(mp_limb_t) * abs(n->value.mpz->_mp_size);
  n->value.mpz->_mp_d = (void*)p;

  p += (size+sizeof(word)-1)/sizeof(word);
  PC = (Code)p;
#endif
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_DOUBLE: Push double following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_DOUBLE, 0, WORDS_PER_DOUBLE, (CA1_FLOAT))
{ Number n = allocArithStack(PASS_LD1);
  Word p = &n->value.w[0];

  cpDoubleData(p, PC);
  n->type       = V_FLOAT;
  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_VAR: Push a variable.  This can be any term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

BEGIN_SHAREDVARS
  int offset;

VMI(A_VAR, 0, 1, (CA1_VAR))
{ Number n;
  Word p, p2;

  offset = (int)*PC++;

a_var_n:
  p = varFrameP(FR, offset);
  deRef2(p, p2);

					/* speedup common case a bit */
  if ( tagex(*p2) == (TAG_INTEGER|STG_INLINE) )
  { n = allocArithStack(PASS_LD1);
    n->value.i = valInt(*p2);
    n->type = V_INTEGER;
    NEXT_INSTRUCTION;
    /*NOTREACHED*/
  }

  switch(tag(*p2))
  { case TAG_INTEGER:
      n = allocArithStack(PASS_LD1);
      get_integer(*p2, n);
      NEXT_INSTRUCTION;
    case TAG_FLOAT:
      n = allocArithStack(PASS_LD1);
      n->value.f = valFloat(*p2);
      n->type = V_FLOAT;
      NEXT_INSTRUCTION;
    default:
    { intptr_t lsafe = (char*)lTop - (char*)lBase;
      fid_t fid;
      number result;
      int rc;

      SAVE_REGISTERS(qid);
      lTop = (LocalFrame)argFrameP(lTop, 1); /* for is/2.  See below */
      if ( (fid = PL_open_foreign_frame()) )
      { rc = valueExpression(consTermRef(p), &result PASS_LD);
	PL_close_foreign_frame(fid);
      } else
      { rc = FALSE;
      }
      lTop = addPointer(lBase, lsafe);
      LOAD_REGISTERS(qid);

      if ( rc )
      { pushArithStack(&result PASS_LD);
	NEXT_INSTRUCTION;
      } else
      { resetArithStack(PASS_LD1);
	THROW_EXCEPTION;
      }
    }
  }
}

VMI(A_VAR0, 0, 0, ())
{ offset = VAROFFSET(0);
  goto a_var_n;
}

VMI(A_VAR1, 0, 0, ())
{ offset = VAROFFSET(1);
  goto a_var_n;
}

VMI(A_VAR2, 0, 0, ())
{ offset = VAROFFSET(2);
  goto a_var_n;
}
END_SHAREDVARS



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_FUNC, function, #args
A_FUNC0, function
A_FUNC1, function
A_FUNC2, function

TBD: Keep knowledge on #argument in function!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

BEGIN_SHAREDVARS
  int an;
  code fn;

VMI(A_FUNC0, 0, 1, (CA1_AFUNC))
{ an = 0;
  fn = *PC++;
  goto common_an;
}

VMI(A_FUNC1, 0, 1, (CA1_AFUNC))
{ an = 1;
  fn = *PC++;
  goto common_an;
}

VMI(A_FUNC2, 0, 1, (CA1_AFUNC))
{ an = 2;
  fn = *PC++;
  goto common_an;
}

VMI(A_FUNC, 0, 2, (CA1_AFUNC, CA1_INTEGER))
{ int rc;

  fn = *PC++;
  an = (int) *PC++;

common_an:
  SAVE_REGISTERS(qid);
  rc = ar_func_n((int)fn, an PASS_LD);
  LOAD_REGISTERS(qid);
  if ( !rc )
  { resetArithStack(PASS_LD1);
    THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;
}
END_SHAREDVARS


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_ADD: Shorthand for A_FUNC2 pl_ar_add()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_ADD, 0, 0, ())
{ Number argv = argvArithStack(2 PASS_LD);
  int rc;
  number r;

  SAVE_REGISTERS(qid);
  rc = pl_ar_add(argv, argv+1, &r);
  LOAD_REGISTERS(qid);
  popArgvArithStack(2 PASS_LD);
  if ( rc )
  { pushArithStack(&r PASS_LD);
    NEXT_INSTRUCTION;
  }

  resetArithStack(PASS_LD1);
  THROW_EXCEPTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_MUL: Shorthand for A_FUNC2 ar_mul()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_MUL, 0, 0, ())
{ Number argv = argvArithStack(2 PASS_LD);
  int rc;
  number r;

  SAVE_REGISTERS(qid);
  rc = ar_mul(argv, argv+1, &r);
  LOAD_REGISTERS(qid);
  popArgvArithStack(2 PASS_LD);
  if ( rc )
  { pushArithStack(&r PASS_LD);
    NEXT_INSTRUCTION;
  }

  resetArithStack(PASS_LD1);
  THROW_EXCEPTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_ADD_FC: Simple case A is B + <int>, where   A is a firstvar and B is a
normal variable. This case is very   common,  especially with relatively
small integers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_ADD_FC, VIF_BREAK, 3, (CA1_VAR, CA1_VAR, CA1_INTEGER))
{ Word rp  = varFrameP(FR, *PC++);	/* A = */
  Word np  = varFrameP(FR, *PC++);	/* B + */
  intptr_t add = (intptr_t)*PC++;	/* <int> */

  deRef(np);

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { Word expr;

    if ( !hasGlobalSpace(3) )
    { int rc;

      SAVE_REGISTERS(qid);
      rc = ensureGlobalSpace(3, ALLOW_GC);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }

      np = varFrameP(FR, PC[-2]);
      rp = varFrameP(FR, PC[-3]);
    }

    expr = gTop;
    gTop += 3;
    expr[0] = FUNCTOR_plus2;
    expr[1] = linkVal(np);
    expr[2] = consInt(add);

    ARGP = argFrameP(lTop, 0);
    setVar(*rp);
    *ARGP++ = linkVal(rp);
    *ARGP++ = consPtr(expr, TAG_COMPOUND|STG_GLOBAL);
    NFR = lTop;
    DEF = GD->procedures.is2->definition;
    setNextFrameFlags(NFR, FR);
    goto normal_call;
  }
#endif

  if ( tagex(*np) == (TAG_INTEGER|STG_INLINE) )
  { intptr_t v = valInt(*np);
    int64_t r = v+add;			/* tagged ints never overflow */
    word w = consInt(r);

    if ( valInt(w) == r )
    { *rp = w;
    } else				/* but their sum might not fit */
    { int rc;

      SAVE_REGISTERS(qid);
      rc = put_int64(&w, r, ALLOW_GC|ALLOW_SHIFT PASS_LD);
      LOAD_REGISTERS(qid);
      if ( rc != TRUE )
	THROW_EXCEPTION;
      *rp = w;
    }
    NEXT_INSTRUCTION;
  } else
  { number n;
    word w;
    fid_t fid;
    int rc;

    SAVE_REGISTERS(qid);
    if ( (fid = PL_open_foreign_frame()) )	/* Still needed? */
    { rc = valueExpression(pushWordAsTermRef(np), &n PASS_LD);
      popTermRef();
      if ( rc )
      { ensureWritableNumber(&n);
	if ( (rc=ar_add_ui(&n, add)) )
	{ if ( (rc=put_number(&w, &n, ALLOW_GC PASS_LD)) != TRUE )
	    rc = raiseStackOverflow(rc);
	}
	clearNumber(&n);
      }
      PL_close_foreign_frame(fid);
    } else
      rc = FALSE;
    LOAD_REGISTERS(qid);
    if ( !rc )
      THROW_EXCEPTION;

    rp = varFrameP(FR, PC[-3]);		/* may have shifted */
    *rp = w;

    NEXT_INSTRUCTION;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of the arithmic comparison predicates (<, >, =<,  >=,  =:=).
Both sides are pushed on the stack, so we just compare the two values on
the  top  of  this  stack  and  backtrack  if  they  do  not suffice the
condition.  Example translation: `a(Y) :- b(X), X > Y'

	I_ENTER
	B_FIRSTVAR 1	% Link X from B's frame to a new var in A's frame
	I_CALL b/1	% call b/1
	A_ENTER		% Enter arithmetic mode
	A_VAR 1		% Push X
	A_VAR 0		% Push Y
	A_GT		% compare
	EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

BEGIN_SHAREDVARS
  Number n1, n2;
  int cmp;
  int rc;

#define CMP_FAST(op) \
  n1 = argvArithStack(2 PASS_LD); \
  n2 = n1 + 1; \
  if ( n1->type == n2->type ) \
  { switch(n1->type) \
    { case V_INTEGER: \
        rc = n1->value.i op n2->value.i; \
	goto a_cmp_out; \
      case V_FLOAT: \
        rc = n1->value.f op n2->value.f; \
	goto a_cmp_out; \
      default: \
        ; \
    } \
  }


VMI(A_LT, VIF_BREAK, 0, ())		/* A < B */
{ CMP_FAST(<)
  cmp = LT;
acmp:
  rc = ar_compare(n1, n2, cmp);
a_cmp_out:
  popArgvArithStack(2 PASS_LD);
  AR_END();
  if ( rc )
    NEXT_INSTRUCTION;
  BODY_FAILED;
}

VMI(A_LE, VIF_BREAK, 0, ())		/* A =< B */
{ CMP_FAST(<=);
  cmp = LE;
  goto acmp;
}

VMI(A_GT, VIF_BREAK, 0, ())		/* A > B */
{ CMP_FAST(>);
  cmp = GT;
  goto acmp;
}

VMI(A_GE, VIF_BREAK, 0, ())		/* A >= B */
{ CMP_FAST(>=);
  cmp = GE;
  goto acmp;
}

VMI(A_EQ, VIF_BREAK, 0, ())		/* A =:= B */
{ CMP_FAST(==);
  cmp = EQ;
  goto acmp;
}

VMI(A_NE, VIF_BREAK, 0, ())		/* A \=:= B */
{ CMP_FAST(!=);
  cmp = NE;
  goto acmp;
}
END_SHAREDVARS


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_IS: Translation of is/2. The stack has two pushed values: the variable
for the result (a word) and the number holding the result. For example:

   a(X) :- X is sin(3).

  I_ENTER
  B_VAR 0		push left argument of is/2
  A_INTEGER 3		push integer as number
  A_FUNC <sin>		run function on it
  A_IS			bind value
  I_EXIT
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_IS, VIF_BREAK, 0, ())		/* A is B */
{ Number n = argvArithStack(1 PASS_LD);
  Word k;

  ARGP = argFrameP(lTop, 0);	/* 1-st argument */
  deRef2(ARGP, k);

  if ( canBind(*k) )
  { word c;
    int rc;

    if ( n->type == V_INTEGER )		/* Speedup the 99% case a bit */
    { c = consInt(n->value.i);
      if ( valInt(c) == n->value.i &&
	   hasGlobalSpace(0) )
      { rc = TRUE;
	bindConst(k, c);
	goto a_is_ok;
      }
    }

  can_bind:
    ARGP++;				/* new_args must become 1 in */
    SAVE_REGISTERS(qid);		/* get_vmi_state() */
    rc = put_number(&c, n, ALLOW_GC PASS_LD);
    LOAD_REGISTERS(qid);
    ARGP--;

    if ( rc == TRUE )
    { deRef2(ARGP, k);			/* may be shifted */
      if ( !isTerm(c) )
      { bindConst(k, c);
      } else
      { SAVE_REGISTERS(qid);
	rc = unify_ptrs(k, &c, ALLOW_GC|ALLOW_SHIFT PASS_LD);
	LOAD_REGISTERS(qid);
	if ( rc == FALSE )
	{ popArgvArithStack(1 PASS_LD);
	  AR_END();
	  if ( exception_term )
	    THROW_EXCEPTION;
	  BODY_FAILED;
	}
      }
    } else
    { raiseStackOverflow(rc);
      popArgvArithStack(1 PASS_LD);
      AR_END();
      THROW_EXCEPTION;
    }

  a_is_ok:
    popArgvArithStack(1 PASS_LD);
    AR_END();

    CHECK_WAKEUP;
    NEXT_INSTRUCTION;
  } else
  { int rc;

    if ( isInteger(*k) && intNumber(n) )
    { number left;

      get_integer(*k, &left);
      rc = (cmpNumbers(&left, n) == 0);
      clearNumber(&left);
    } else if ( isFloat(*k) && floatNumber(n) )
    { rc = (valFloat(*k) == n->value.f);
#ifdef O_GMP
    } else if ( n->type == V_MPQ )
    { goto can_bind;
#endif
    } else
    { rc = FALSE;
    }

    popArgvArithStack(1 PASS_LD);
    AR_END();
    if ( rc )
      NEXT_INSTRUCTION;
  }

  BODY_FAILED;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_FIRSTVAR_IS: Deal with the very common case that the local variable is
a firstvar of the current frame. There   are numerous advantages to this
case: we know the left-side is a var, we  do not need to trail and we do
not need to check for attvar wakeup.

TBD: link with following B_VAR? How  frequent?   Likely  very: we are in
body mode and in many cases the result is used only once.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_FIRSTVAR_IS, VIF_BREAK, 1, (CA1_VAR)) /* A is B */
{ Number n = argvArithStack(1 PASS_LD);
  word w;
  int rc;

  SAVE_REGISTERS(qid);
  if ( (rc = put_number(&w, n, ALLOW_GC PASS_LD)) != TRUE )
    rc = raiseStackOverflow(rc);
  LOAD_REGISTERS(qid);

  popArgvArithStack(1 PASS_LD);
  AR_END();

  if ( rc )
  { *varFrameP(FR, *PC++) = w;
    NEXT_INSTRUCTION;
  } else
    THROW_EXCEPTION;
}


		 /*******************************
		 *	     FOREIGN		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	I_FOPEN
	I_FCALLDET0-10 f/n
	I_FEXITDET
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_PROF_PENTIUM
#define PROF_FOREIGN \
	{ END_PROF(); \
	  START_PROF(DEF->prof_index, DEF->prof_name); \
	}
#else
#define PROF_FOREIGN (void)0
#endif

BEGIN_SHAREDVARS
volatile word rc;			/* make gcc quiet on non-initialised */
volatile fid_t ffr_id;

VMI(I_FOPEN, 0, 0, ())
{ FliFrame ffr;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
    BFR = newChoice(CHP_DEBUG, FR PASS_LD);
    ffr = (FliFrame)lTop;
  } else
#endif
  { ffr = (FliFrame)argFrameP(FR, DEF->functor->arity);
  }

  lTop = (LocalFrame)(ffr+1);
  ffr->size = 0;
  NoMark(ffr->mark);
  ffr->parent = fli_context;
  ffr->magic = FLI_MAGIC;
  fli_context = ffr;
  ffr_id = consTermRef(ffr);
  SAVE_REGISTERS(qid);

  NEXT_INSTRUCTION;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_FCALLDETVA:  Call  deterministic  foreign    function  using  P_VARARG
conventions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_FCALLDETVA, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  struct foreign_context context;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  context.context = 0L;
  context.engine  = LD;
  context.control = FRG_FIRST_CALL;

  PROF_FOREIGN;
  rc = (*f)(h0, DEF->functor->arity, &context);
  VMI_GOTO(I_FEXITDET);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_FCALLDET0 .. I_FCALLDET10: Call deterministic   foreign function using
a1, a2, ... calling conventions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_FCALLDET0, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;

  PROF_FOREIGN;
  rc = (*f)();
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET1, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET2, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET3, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET4, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET5, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET6, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET7, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET8, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET9, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FCALLDET10, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8, h0+9);
  VMI_GOTO(I_FEXITDET);
}


VMI(I_FEXITDET, 0, 0, ())
{ FliFrame ffr = (FliFrame)valTermRef(ffr_id);

  LOAD_REGISTERS(qid);
  PC += 3;
  DEBUG(CHK_SECURE, assert(PC[-1] == encode(I_FEXITDET)));
  fli_context = ffr->parent;

  switch(rc)
  { case TRUE:
      if ( exception_term )		/* false alarm */
	PL_clear_exception();
      goto exit_checking_wakeup;
    case FALSE:
      if ( exception_term )
	THROW_EXCEPTION;
      FRAME_FAILED;
    default:
    { fid_t fid = PL_open_foreign_frame();
      term_t ex = PL_new_term_ref();

      PL_put_intptr(ex, rc);
      PL_error(NULL, 0, NULL, ERR_DOMAIN,
	       ATOM_foreign_return_value, ex);
      PL_close_foreign_frame(fid);
      THROW_EXCEPTION;
    }
  }
}
END_SHAREDVARS

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Non-deterministic foreign calls. This  is   compiled  into the following
supervisor code:

	I_FOPENNDET
	I_FCALLNDETVA func
	I_FEXITNDET
	I_FREDO

On determistic success or failure I_FEXITNDET ends the clause. Otherwise
it writes the context in CL  and   creates  a jump-choicepoint that will
take it to I_FREDO. I_FREDO updates the context structure and jumps back
to the I_FCALLNDETVA (PC -= 4);
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

BEGIN_SHAREDVARS
volatile intptr_t rc;				/* make compiler happy */
struct foreign_context context;
volatile fid_t ffr_id;

VMI(I_FOPENNDET, 0, 0, ())
{ Choice ch;
  FliFrame ffr;

  context.context = 0L;
  context.engine  = LD;
  context.control = FRG_FIRST_CALL;

foreign_redo:
  lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
  ch = newChoice(CHP_JUMP, FR PASS_LD);
  ch->value.PC = PC+3;

  ffr = (FliFrame)(ch+1);
  lTop = (LocalFrame)(ffr+1);
  ffr->size = 0;
  NoMark(ffr->mark);
  ffr->parent = fli_context;
  ffr->magic = FLI_MAGIC;
  fli_context = ffr;
  ffr_id = consTermRef(ffr);
  SAVE_REGISTERS(qid);

  NEXT_INSTRUCTION;
}


VMI(I_FCALLNDETVA, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, DEF->functor->arity, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET0, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;

  PROF_FOREIGN;
  rc = (*f)(&context);
  VMI_GOTO(I_FEXITNDET);
}

VMI(I_FCALLNDET1, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET2, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  rc = (*f)(h0, h0+1, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET3, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET4, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET5, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET6, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET7, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET8, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET9, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FCALLNDET10, 0, 1, (CA1_FOREIGN))
{ Func f = (Func)*PC++;
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  PROF_FOREIGN;
  rc = (*f)(h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8, h0+9, &context);
  VMI_GOTO(I_FEXITNDET);
}


VMI(I_FEXITNDET, 0, 0, ())
{ FliFrame ffr = (FliFrame) valTermRef(ffr_id);

  LOAD_REGISTERS(qid);
  PC += 3;				/* saved at in I_FOPENNDET */
  fli_context = ffr->parent;

  switch(rc)
  { case TRUE:
      if ( exception_term )		/* false alarm */
      { exception_term = 0;
	setVar(*valTermRef(exception_bin));
      }
      DEBUG(CHK_SECURE, assert(BFR->value.PC == PC));
#ifdef O_DEBUGGER
      if ( unlikely(debugstatus.debugging) )
	BFR->type = CHP_DEBUG;
      else
#endif
	BFR = BFR->parent;
      FR->clause = NULL;
      goto exit_checking_wakeup;
    case FALSE:
      if ( exception_term )
	THROW_EXCEPTION;
      DEBUG(CHK_SECURE, assert(BFR->value.PC == PC));
#ifdef O_DEBUGGER
      if ( unlikely(debugstatus.debugging) )
	BFR->type = CHP_DEBUG;
      else
#endif
	BFR = BFR->parent;
      FRAME_FAILED;
    default:
    { /* TBD: call debugger */

      if ( (rc & FRG_REDO_MASK) == REDO_INT )
      { rc = (word)(((intptr_t)rc)>>FRG_REDO_BITS);
      } else
      { rc &= ~FRG_REDO_MASK;
      }
      CL = (ClauseRef)rc;
      lTop = (LocalFrame)(BFR+1);
      goto exit_checking_wakeup;
    }
  }
}

VMI(I_FREDO, 0, 0, ())
{ if ( is_signalled(PASS_LD1) )
  { SAVE_REGISTERS(qid);
    handleSignals(PASS_LD1);
    LOAD_REGISTERS(qid);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  context.context = (word)FR->clause;
  context.control = FRG_REDO;
  context.engine  = LD;
  PC -= 4;
  goto foreign_redo;
}

END_SHAREDVARS


		 /*******************************
		 *     EXCEPTIONS & CLEANUP	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLCLEANUP: Part of setup_call_catcher_cleanup(:Setup, :Goal,
?Catcher, :Cleanup). Simply set a flag on the frame and call the 1-st
argument. See also I_CATCH.

I_EXITCLEANUP  is  at  the  end  of   call_cleanup/3.  If  there  is  no
choice-point created this is the final exit. If this frame has no parent
(it is the entry of PL_next_solution()),

setup_call_catcher_cleanup(:Setup :Goal, -Reason, :Cleanup)
is tranalated into

  i_enter
  <setup>
  i_callcleanup
  i_exitcleanup
  i_exit

We set FR_WATCHED to get a cleanup call if the frame fails or is cutted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLCLEANUP, 0, 0, ())
{ if ( !mustBeCallable(consTermRef(argFrameP(FR, 3)) PASS_LD) )
    THROW_EXCEPTION;

  newChoice(CHP_CATCH, FR PASS_LD);
  set(FR, FR_WATCHED);
				/* = B_VAR1 */
  *argFrameP(lTop, 0) = linkVal(argFrameP(FR, 1));

  VMI_GOTO(I_USERCALL0);
}


/* (*) Work around a bug in the LLVM.  Just calling a dummy function avoids
   a crash here!?  If we do not use GCC's threaded-code support it appears
   that the bug is gone too.  LLVM's support for this GCC extension is poor
   anyway: it runs, but is *much* slower than the switch.
*/

VMI(I_EXITCLEANUP, 0, 0, ())
{
#if defined(__llvm__) && defined(VMCODE_IS_ADDRESS) /* (*) */
  llvm_dummy();
#endif

  while( BFR && BFR->type == CHP_DEBUG )
    BFR = BFR->parent;

  if ( BFR->frame == FR && BFR->type == CHP_CATCH )
  { DEBUG(3, Sdprintf(" --> BFR = #%ld\n", loffset(BFR->parent)));
    for(BFR = BFR->parent; BFR > (Choice)FR; BFR = BFR->parent)
    { if ( BFR->frame > FR )
	NEXT_INSTRUCTION;		/* choice from setup of setup_call_catcher_cleanup/4 */
      assert(BFR->type == CHP_DEBUG);
    }

    SAVE_REGISTERS(qid);
    frameFinished(FR, FINISH_EXITCLEANUP PASS_LD);
    LOAD_REGISTERS(qid);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;			/* goto i_exit? */
}


#if O_CATCHTHROW
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CATCH has to fake a choice-point to   make  it possible to undo before
starting the recover action. Otherwise it simply   has to call the first
argument.  Catch is defined as:

catch(Goal, Pattern, Recover) :-
  $catch.

which is translated to:
  I_ENTER
  I_CATCH
  I_EXITCATCH
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CATCH, 0, 0, ())
{ if ( BFR->frame == FR && BFR == (Choice)argFrameP(FR, 3) )
  { assert(BFR->type == CHP_DEBUG);
    BFR->type = CHP_CATCH;
  } else
    newChoice(CHP_CATCH, FR PASS_LD);

				  /* = B_VAR0 */
  *argFrameP(lTop, 0) = linkVal(argFrameP(FR, 0));
  VMI_GOTO(I_USERCALL0);
}


VMI(I_EXITCATCH, VIF_BREAK, 0, ())
{ if ( BFR->frame == FR && BFR == (Choice)argFrameP(FR, 3) )
  { assert(BFR->type == CHP_CATCH);
    BFR = BFR->parent;
    set(FR, FR_CATCHED);
  }

  VMI_GOTO(I_EXIT);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The B_THROW code is the implementation for   throw/1.  The call walks up
the stack, looking for a frame running catch/3 on which it can unify the
exception code. It then cuts all  choicepoints created since throw/3. If
throw/3 is not found, it sets  the   query  exception  field and returns
failure. Otherwise, it will simulate an I_USERCALL0 instruction: it sets
the FR and lTop as if it  was   running  the  throw/3 predicate. Then it
pushes the recovery goal from throw/3 and jumps to I_USERCALL0.

Note that exceptions are placed on the stack using PL_raise_exception(),
which uses duplicate_term() and  freezeGlobal()   to  make the exception
term immune for undo operations.

(*) If the exception is not caught, we  try to print it and enable trace
mode. However, we should be careful about   this  if the exception is an
out-of-stack exception because the trace runs in Prolog and is likely to
run fatally out of stack if we start the tracer immediately. That is the
role of trace_if_space(). As long as there is no space, the exception is
unwinded until there is space. Unfortunately,   this  means that some of
the context of the exception is lost. Note that  we need to run GC if we
ran out of global stack because  the   stack  is  frozen to preserve the
exception ball.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_THROW, 0, 0, ())
{ term_t catchfr_ref;
  int start_tracer;

  PL_raise_exception(argFrameP(lTop, 0) - (Word)lBase);
  THROW_EXCEPTION;				/* sets origin */

b_throw:
  QF  = QueryFromQid(qid);
  aTop = QF->aSave;
  assert(exception_term);

  if ( lTop < (LocalFrame)argFrameP(FR, FR->predicate->functor->arity) )
    lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);

  DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));
  DEBUG(MSG_THROW,
	{ fid_t fid = PL_open_foreign_frame();
	  Sdprintf("[%d] Throwing (from line %d): ",
		   PL_thread_self(), throwed_from_line);
	  PL_write_term(Serror, exception_term, 1200, 0);
	  Sdprintf("\n");
	  PL_discard_foreign_frame(fid);
	});

  SAVE_REGISTERS(qid);
  catchfr_ref = findCatcher(FR, LD->choicepoints, exception_term PASS_LD);
  LOAD_REGISTERS(qid);
  DEBUG(MSG_THROW,
	{ if ( catchfr_ref )
	  { LocalFrame fr = (LocalFrame)valTermRef(catchfr_ref);
	    Sdprintf("[%d]: found catcher at %ld\n",
		     PL_thread_self(), (long)levelFrame(fr));
	  } else
	  { Sdprintf("[%d]: not caught\n", PL_thread_self());
	  }
	});

  DEBUG(CHK_SECURE,
	{ SAVE_REGISTERS(qid);
	  checkData(valTermRef(exception_term));
	  checkStacks(NULL);
	  LOAD_REGISTERS(qid);
	});

  if ( debugstatus.suspendTrace == FALSE )
  { SAVE_REGISTERS(qid);
    exception_hook(FR, catchfr_ref PASS_LD);
    LOAD_REGISTERS(qid);
  }

#if O_DEBUGGER
  start_tracer = FALSE;
  if ( !catchfr_ref &&
       !PL_same_term(exception_term, exception_printed) &&
       false(QueryFromQid(qid), PL_Q_CATCH_EXCEPTION) )
  { int rc;

    SAVE_REGISTERS(qid);
    rc = isCaughtInOuterQuery(qid, exception_term PASS_LD);
    LOAD_REGISTERS(qid);

    if ( !rc )
    { atom_t a;

      SAVE_REGISTERS(qid);
      if ( LD->outofstack == (Stack)&LD->stacks.global )
	garbageCollect();
      LD->critical++;			/* do not handle signals */
      if ( PL_is_functor(exception_term, FUNCTOR_error2) &&
	   truePrologFlag(PLFLAG_DEBUG_ON_ERROR) )
      { debugmode(TRUE, NULL);
	if ( !trace_if_space() )		/* see (*) */
	{ start_tracer = TRUE;
	} else
	{ trimStacks(FALSE PASS_LD);	/* restore spare stacks */
	  printMessage(ATOM_error, PL_TERM, exception_term);
	}
      } else if ( !(PL_get_atom(exception_term, &a) && a == ATOM_aborted) )
      { printMessage(ATOM_error,
		     PL_FUNCTOR_CHARS, "unhandled_exception", 1,
		       PL_TERM, exception_term);
      }
      LD->critical--;
      LOAD_REGISTERS(qid);
    }
  }

  if ( debugstatus.debugging )
  { for( ;
	 FR && FR > (LocalFrame)valTermRef(catchfr_ref);
	 PC = FR->programPointer,
	 FR = FR->parent )
    { Choice ch = findStartChoice(FR, LD->choicepoints);
      void *l_top;

      environment_frame = FR;
      ARGP = argFrameP(FR, 0);	/* otherwise GC might see `new' arguments */

      if ( ch )
      { int printed = PL_same_term(exception_printed, exception_term);
	term_t chref = consTermRef(ch);
	int rc;

	lTop = (LocalFrame)(BFR+1);
	DEBUG(CHK_SECURE,
	      { SAVE_REGISTERS(qid);
		checkStacks(NULL);
		LOAD_REGISTERS(qid);
	      });
	SAVE_REGISTERS(qid);
	dbg_discardChoicesAfter((LocalFrame)ch PASS_LD);
	LOAD_REGISTERS(qid);
	ch = (Choice)valTermRef(chref);
	Undo(ch->mark);
	PL_put_term(LD->exception.pending, exception_term);
	if ( printed )
	  PL_put_term(exception_printed, exception_term);

	DEBUG(CHK_SECURE,
	      { SAVE_REGISTERS(qid);
	        checkStacks(NULL);
		LOAD_REGISTERS(qid);
		ch = (Choice)valTermRef(chref);
	      });

	SAVE_REGISTERS(qid);
	rc = tracePort(FR, ch, EXCEPTION_PORT, PC PASS_LD);
	LOAD_REGISTERS(qid);

	switch( rc )
	{ case ACTION_RETRY:
	    PL_clear_exception();
	    SAVE_REGISTERS(qid);
	    discardChoicesAfter(FR, FINISH_CUT PASS_LD);
	    resumeAfterException();	/* reinstantiate spare stacks */
	    LOAD_REGISTERS(qid);
	    DEF = FR->predicate;
	    FR->clause = NULL;
	    goto retry_continue;
	  case ACTION_ABORT:
	    THROW_EXCEPTION;
	}

	setVar(*valTermRef(LD->exception.pending));
      }

      l_top = argFrameP(FR, FR->predicate->functor->arity);
      if ( l_top < (void*)(BFR+1) )
        l_top = (void*)(BFR+1);
      lTop = l_top;

      SAVE_REGISTERS(qid);
      dbg_discardChoicesAfter(FR PASS_LD);
      LOAD_REGISTERS(qid);
      discardFrame(FR PASS_LD);
      if ( true(FR, FR_WATCHED) )
      { SAVE_REGISTERS(qid);
	frameFinished(FR, FINISH_EXCEPT PASS_LD);
	LOAD_REGISTERS(qid);
      }

      if ( start_tracer )		/* See (*) */
      {	if ( LD->outofstack == (Stack)&LD->stacks.global )
	{ SAVE_REGISTERS(qid);
	  garbageCollect();
	  LOAD_REGISTERS(qid);
	}

	DEBUG(1, Sdprintf("g+l+t free = %ld+%ld+%ld\n",
			  spaceStack(global),
			  spaceStack(local),
			  spaceStack(trail)));

	if ( trace_if_space() )
	{ start_tracer = FALSE;
	  SAVE_REGISTERS(qid);
	  LD->critical++;		/* do not handle signals */
	  trimStacks(FALSE PASS_LD);
	  printMessage(ATOM_error, PL_TERM, exception_term);
	  LD->critical--;
	  LOAD_REGISTERS(qid);
	}
      }
    }
  } else
#endif /*O_DEBUGGER*/
  { DEBUG(3, Sdprintf("Unwinding for exception\n"));

    for( ; FR && FR > (LocalFrame)valTermRef(catchfr_ref); FR = FR->parent )
    { Choice ch;

      environment_frame = FR;
      SAVE_REGISTERS(qid);
      ch = dbg_discardChoicesAfter(FR PASS_LD);
      LOAD_REGISTERS(qid);
      if ( ch )
	Undo(ch->mark);

      discardFrame(FR PASS_LD);
      if ( true(FR, FR_WATCHED) )
      { SAVE_REGISTERS(qid);
	frameFinished(FR, FINISH_EXCEPT PASS_LD);
	LOAD_REGISTERS(qid);
      }
      DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));
    }
  }

					/* re-fetch (test cleanup(clean-5)) */
  DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));

  if ( catchfr_ref )
  { Choice ch;
    word w;

    assert(FR == (LocalFrame)valTermRef(catchfr_ref));

    SAVE_REGISTERS(qid);
    ch = dbg_discardChoicesAfter(FR PASS_LD);
    LOAD_REGISTERS(qid);
    assert(ch && ch->type == CHP_CATCH);
    environment_frame = FR;
    Undo(ch->mark);
					/* re-unify */
    PL_unify(consTermRef(argFrameP(FR, 1)), exception_term);
    lTop = (LocalFrame) argFrameP(FR, 3); /* above the catch/3 */
    if ( (w=uncachableException(exception_term PASS_LD)) )
    { Word p = gTop;

      if ( !hasGlobalSpace(3) )
	fatalError("Cannot wrap abort exception\n");

      argFrame(lTop, 0) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
      p[0] = FUNCTOR_drecover_and_rethrow2;
      p[1] = argFrame(FR, 2);
      p[2] = w;
      gTop = p+3;
    } else
    { argFrame(lTop, 0) = argFrame(FR, 2);  /* copy recover goal */
    }
    *valTermRef(exception_printed) = 0;   /* consider it handled */
    *valTermRef(exception_bin)     = 0;
    exception_term		   = 0;

    PC = findCatchExit();
    { word lSafe = consTermRef(lTop);
      lTop = (LocalFrame)argFrameP(lTop, 1);
      ARGP = (Word)lTop;
      SAVE_REGISTERS(qid);
      resumeAfterException();		/* trim/GC to recover space */
      LOAD_REGISTERS(qid);
      lTop = (LocalFrame)valTermRef(lSafe);
    }

    VMI_GOTO(I_USERCALL0);
  } else
  { QF = QueryFromQid(qid);		/* may be shifted */
    set(QF, PL_Q_DETERMINISTIC);
    FR = environment_frame = &QF->top_frame;
    lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);

    Undo(QF->choice.mark);
    QF->foreign_frame = PL_open_foreign_frame();
    QF->exception = PL_copy_term_ref(exception_term);

    if ( false(QF, PL_Q_PASS_EXCEPTION) )
    { *valTermRef(exception_bin)     = 0;
      exception_term		     = 0;
      *valTermRef(exception_printed) = 0; /* consider it handled */
    }

    resumeAfterException();
    if ( PL_pending(SIG_GC) )
      garbageCollect();
    QF = QueryFromQid(qid);		/* may be shifted: recompute */

    assert(LD->exception.throw_environment == &throw_env);
    LD->exception.throw_environment = throw_env.parent;

    fail;
  }

}
#endif /*O_CATCHTHROW*/


		 /*******************************
		 *	    META-CALLING	*
		 *******************************/

BEGIN_SHAREDVARS
  Module module;
  functor_t functor;
  int arity;
  Word args;

#ifdef O_CALL_AT_MODULE
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLATM: procedure-module, context-module, procedure
The procedure-module is provided to support the compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLATM, VIF_BREAK, 3, (CA1_MODULE, CA1_MODULE, CA1_PROC))
{ PC++;
  VMI_GOTO(I_CALLM);
}

VMI(I_DEPARTATMV, VIF_BREAK, 3, (CA1_MODULE, CA1_VAR, CA1_PROC))
{ if ( (void *)BFR > (void *)FR || !truePrologFlag(PLFLAG_LASTCALL) )
  { VMI_GOTO(I_CALLATMV);
  } else
  { Word ap;
    int iv;

    PC++;
    iv = (int)*PC++;

    ap = varFrameP(FR, iv);
    deRef(ap);
    if ( isTextAtom(*ap) )
    { Module m = lookupModule(*ap);

      setContextModule(FR, m);
      VMI_GOTO(I_DEPART);
    } else
    { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_module,
	       pushWordAsTermRef(ap));
      popTermRef();
      THROW_EXCEPTION;
    }
  }
}


VMI(I_CALLATMV, VIF_BREAK, 3, (CA1_MODULE, CA1_VAR, CA1_PROC))
{ Word ap;
  int iv;
  Procedure proc;

  PC++;
  iv = (int)*PC++;
  proc = (Procedure)*PC++;

  ap = varFrameP(FR, iv);
  deRef(ap);
  if ( isTextAtom(*ap) )
  { module = lookupModule(*ap);
    DEF = proc->definition;
    NFR = lTop;

    goto mcall_cont;
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_module,
	     pushWordAsTermRef(ap));
    popTermRef();
    THROW_EXCEPTION;
  }

  VMI_GOTO(I_CALLM);
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLM deals with qualified calls. The unfortunate  task is to sort out
the context module for calling a transparent  procedure. This job is the
same as the end of I_USERCALL
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLM, VIF_BREAK, 2, (CA1_MODULE, CA1_PROC))
{ module = (Module)*PC++;
  DEF    = ((Procedure)*PC++)->definition;
  NFR = lTop;

  goto mcall_cont;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_USERCALL0 is generated by the compiler if a variable is encountered as
a subclause. Note that the compount   statement  opened here is encloses
also I_CALL. This allows us to use   local register variables, but still
jump to the `normal_call' label to do the common part of all these three
virtual machine instructions.

I_USERCALL0 has the task of  analysing  the   goal:  it  should fill the
->procedure slot of the new frame and  save the current program counter.
It also is responsible of filling the   argument part of the environment
frame with the arguments of the term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_USERCALL0, VIF_BREAK, 0, ())
{ word goal;
  Word a;

  module = NULL;
  NFR = lTop;
  a = argFrameP(NFR, 0);		/* get the goal */
  a = stripModule(a, &module PASS_LD);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the functor definition associated with the goal as well as the
arity and a pointer to the argument vector of the goal.

If the goal is not a  simple  goal,   the  body  is  compiled to `local'
clause. The compilation mode is presented to compileClause() by the NULL
`head'. This compilation-mode  constructs  a   stack-frame  whose  first
argument is the  goal-term,  followed   by  large  structures (compound,
string) from the arguments, followed  by   the  normal  local variables,
followed by the VM codes and, the   clause structure and finally a dummy
list for the clause-chain  (ClauseRef)  used   for  the  frames ->clause
field.

The clause data is discarded automatically  if the frame is invalidated.
Note that compilation does not give contained   atoms a reference as the
atom is referenced by the goal-term anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( isTextAtom(goal = *a) )
  { functor = lookupFunctorDef(goal, 0);
    arity   = 0;
    args    = NULL;
  } else if ( isTerm(goal) )
  { FunctorDef fd;

    functor = functorTerm(goal);
    if ( functor == FUNCTOR_colon2 )
      goto call_type_error;

    fd = valueFunctor(functor);
    if ( false(fd, CONTROL_F) )
    { args    = argTermP(goal, 0);
      arity   = fd->arity;
    } else
    { Clause cl;
      int rc;

      DEBUG(1, { term_t g = pushWordAsTermRef(a);
		 LocalFrame ot = lTop;
		 lTop += 100;
		 pl_writeln(g);
		 popTermRef();
		 lTop = ot;
	       });
      lTop = NFR;
      setNextFrameFlags(NFR, FR);
      rc = compileClause(&cl, NULL, a, PROCEDURE_dcall1, module PASS_LD);
      if ( rc == FALSE )
	THROW_EXCEPTION;
      if ( rc == LOCAL_OVERFLOW )
      { size_t room = roomStack(local);
	term_t lTopH = consTermRef(lTop);

	lTop = (LocalFrame)argFrameP(NFR, 1);
	SAVE_REGISTERS(qid);
	rc = ensureLocalSpace(room*2, ALLOW_SHIFT);
	LOAD_REGISTERS(qid);
	lTop = (LocalFrame)valTermRef(lTopH);
	if ( rc != TRUE )
	{ raiseStackOverflow(rc);
	  THROW_EXCEPTION;
	}
	VMI_GOTO(I_USERCALL0);
      }

      DEF		  = NFR->predicate;
      DEBUG(CHK_SECURE, assert(DEF == PROCEDURE_dcall1->definition));
      NFR->parent	  = FR;
      NFR->programPointer = PC;
#ifdef O_PROFILE
      NFR->prof_node      = FR->prof_node;
#endif
#ifdef O_LOGICAL_UPDATE
      cl->generation.erased = ~(gen_t)0;
      cl->generation.created = NFR->generation = GD->generation;
#endif
      PC = cl->codes;

      enterDefinition(DEF);
      environment_frame = FR = NFR;
      ARGP = argFrameP(lTop, 0);

      NEXT_INSTRUCTION;
    }
  } else
  {
  call_type_error:
    PL_error(NULL, 0, NULL, ERR_TYPE,
	     ATOM_callable, pushWordAsTermRef(argFrameP(NFR, 0)));
    popTermRef();
    THROW_EXCEPTION;
  }
  goto i_usercall_common;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_USERCALLN: translation of call(Goal, Arg1, ...)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_USERCALLN, VIF_BREAK, 1, (CA1_INTEGER))
{ Word a;
  int callargs = (int)*PC++;
  word goal;

  NFR = lTop;
  a = argFrameP(NFR, 0);		/* get the (now) instantiated */
  deRef(a);			/* variable */

  module = NULL;
  a = stripModule(a, &module PASS_LD);

  if ( isTextAtom(goal = *a) )
  { arity   = 0;
    functor = lookupFunctorDef(goal, callargs);
    args    = NULL;
  } else if ( isTerm(goal) )
  { FunctorDef fdef = valueFunctor(functorTerm(goal));

    arity   = fdef->arity;
    functor = lookupFunctorDef(fdef->name, arity + callargs);
    args    = argTermP(goal, 0);
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE,
	     ATOM_callable, pushWordAsTermRef(argFrameP(NFR, 0)));
    popTermRef();
    THROW_EXCEPTION;
  }

  if ( arity != 1 )
  { int i, shift = arity - 1;

    a = argFrameP(NFR, 1);	/* pointer to 1-st arg */

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
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now scan the argument vector of the goal and fill the arguments  of  the
frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if ( arity > 0 )
  { if ( arity > MAXARITY )
    { fid_t fid;

      lTop = (LocalFrame)argFrameP(NFR, 1);
      fid = PL_open_foreign_frame();
      PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_arity);
      PL_close_foreign_frame(fid);
      THROW_EXCEPTION;
    }

    ARGP = argFrameP(NFR, 0);

    for(; arity-- > 0; ARGP++, args++)
      *ARGP = linkVal(args);
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find  the  associated  procedure  and  its  definition.  The  latter  is
unfortunate, but we need to know the transparent status of the predicate
to be called to get the context module   right. We must build a complete
environment before we can call trapUndefined() to make shift/GC happy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  DEF = resolveProcedure(functor, module)->definition;

mcall_cont:
  setNextFrameFlags(NFR, FR);
  if ( !DEF->impl.any && false(DEF, PROC_DEFINED) )
  { term_t nref = consTermRef(NFR);
    NFR->parent         = FR;
    NFR->predicate      = DEF;		/* TBD */
    NFR->programPointer = PC;		/* save PC in child */
    NFR->clause         = NULL;
#ifdef O_PROFILE
    NFR->prof_node      = NULL;
#endif
    setNextFrameFlags(NFR, FR);
    environment_frame = FR = NFR;
    lTop = (LocalFrame)argFrameP(NFR, DEF->functor->arity);

    SAVE_REGISTERS(qid);
    DEF = trapUndefined(DEF PASS_LD);
    LOAD_REGISTERS(qid);
    NFR = (LocalFrame)valTermRef(nref);

    FR = FR->parent;
#ifdef O_PLMT
  } else if ( true(DEF, P_THREAD_LOCAL) )
  { DEF = getProcDefinition__LD(DEF PASS_LD);
#endif
  }

  if ( true(DEF, P_TRANSPARENT) )
    setContextModule(NFR, module);

  goto normal_call;
}
END_SHAREDVARS
