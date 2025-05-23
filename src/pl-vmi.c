/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2025, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
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
Implementation of the Virtual Machine Instructions (VMI).

Each VMI has the structure below. Using this structure we are completely
flexible in how we implement the instruction   and  we can easily create
the derived tables, which is done by the program mkvmi.c.

	VMI(Name, Flags, #Args, (ArgType, ...))
	{
	}
	END_VMI

There are also, for shared code, "helper instructions". These take a similar
form to literal instructions, with the form:

	VMH(Name, #Args, (ArgType, ...), (ArgName, ...))
	{
	}
	END_VMH

These allow function-style calls from other instructions, but only in a goto
(i.e. tailcall-like) usage.

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

	* SOLUTION_RETURN(value)
	Return the given value from PL_next_solution().

	* CLAUSE_FAILED
	Failed unifying the head: backtrack to next clause

	* BODY_FAILED
	Failure of in-body instructions (I_FAIL, B_UNIFY_EXIT, ...)

	* FRAME_FAILED
	Other failures: deep backtracking.

	* VMI_GOTO(VMI)
	Continue executing another virtual instruction.  Note this is
	called GOTO as it is a jump rather than a call.

	* VMH_GOTO(VMH, arg...)
	Continue executing a "helper instruction", using the given arguments.

	* TRACE_RETRY		(only when O_DEBUGGER)
	Retry the current goal.

	* ATTVAR_WAKEUP		(only when O_ATTVAR)
	Wakeup for attributed variable handling.

Virtual machine instruction names.  Prefixes:

  I_	General instructions
  B_	Body specific version
  H_	Head specific versin
  A_	Arithmetic compilation specific
  C_	Control (compilation of ;/2, etc.)
  S_    Supervisor instructions.  See pl-supervisor.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FASTCOND_FAILED \
	{ if ( !LD->fast_condition )   \
	  { BODY_FAILED;	       \
	  } else		       \
	  { PC = LD->fast_condition;   \
	    LD->fast_condition = NULL; \
	    NEXT_INSTRUCTION;          \
	  }                            \
	}

		 /*******************************
		 *	 LOCAL ALLOCATION	*
		 *******************************/

/* Note that lTop can be > lMax when calling ENSURE_LOCAL_SPACE() */

#define ENSURE_LOCAL_SPACE(bytes, ifnot) \
	if ( unlikely(lTop > lMax || !hasLocalSpace(bytes)) ) \
	{ int rc; \
	  SAVE_REGISTERS(QID); \
	  rc = growLocalSpace(bytes, ALLOW_SHIFT); \
	  LOAD_REGISTERS(QID); \
	  if ( rc != true ) \
	  { rc = raiseStackOverflow(rc); \
	    ifnot; \
	  } \
	}

#define IF_WRITE_MODE_GOTO(label) \
	if ( UMODE == uwrite ) VMI_GOTO(label)

#define TRUST_CLAUSE(cref) \
	UMODE = uread; \
	CL    = cref; \
	lTop  = (LocalFrame)(ARGP + cref->value.clause->variables); \
	ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION); \
	if ( debugstatus.debugging ) \
	  newChoice(CHP_DEBUG, FR); \
	PC    = cref->value.clause->codes; \
	NEXT_INSTRUCTION;
#define TRY_CLAUSE(cref, cond, altpc) \
	UMODE = uread; \
	CL    = cref; \
	lTop  = (LocalFrame)(ARGP + cref->value.clause->variables); \
	ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION); \
	if ( cond ) \
	{ Choice ch = newChoice(CHP_JUMP, FR); \
	  ch->value.pc = altpc; \
	} else if ( debugstatus.debugging ) \
	{ newChoice(CHP_DEBUG, FR); \
	} \
	PC    = cref->value.clause->codes; \
	NEXT_INSTRUCTION;

		 /*******************************
		 *	 ATTRIBUTED VARS	*
		 *******************************/

#ifdef O_ATTVAR
#define CHECK_WAKEUP \
	if ( unlikely(LD->alerted & ALERT_WAKEUP) ) \
	{ LD->alerted &= ~ALERT_WAKEUP; \
	  if ( *valTermRef(LD->attvar.head) ) \
	    ATTVAR_WAKEUP; \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attributed variable handling, originally from pl-wam.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMH(wakeup, 0, (), ())
{ DEBUG(1, Sdprintf("Activating wakeup\n"));
  NFR = lTop;
  setNextFrameFlags(NFR, FR);
  SAVE_REGISTERS(QID);
  DEF = GD->procedures.dwakeup1->definition;
  LOAD_REGISTERS(QID);
  ARGP = argFrameP(NFR, 0);
  ARGP[0] = *valTermRef(LD->attvar.head);
  setVar(*valTermRef(LD->attvar.head));
  setVar(*valTermRef(LD->attvar.tail));

  VMH_GOTO(normal_call);
}
END_VMH

#else
#define CHECK_WAKEUP (void)0
#endif


		 /*******************************
		 *	MAKE STACK SPACE	*
		 *******************************/

#define ENSURE_GLOBAL_SPACE(cells, onchange) \
	ENSURE_STACK_SPACE(cells, 0, onchange)
#define ENSURE_STACK_SPACE(g, t, onchange) \
	if ( !hasStackSpace(g, t) )			\
	{ GROW_STACK_SPACE(g, t);			\
	  onchange;					\
	}
#define GROW_STACK_SPACE(g, t) \
	do \
	{ int __rc;					\
	  SAVE_REGISTERS(QID);				\
	  __rc = ensureStackSpace(g, t);		\
	  LOAD_REGISTERS(QID);				\
	  if ( __rc != true )				\
	    THROW_EXCEPTION;				\
	} while(0)

/* Can be used for debugging to always force GC at a place */
#define FAKE_GC(onchange) \
	{ int __rc;					\
	  static int __cnt = 0;				\
	  Sdprintf("Forcing GC at %s:%d (cnt=%d)\n",	\
		   __FILE__, __LINE__, ++__cnt);	\
	  if ( __cnt == 131 ) trap_gdb();		\
	  SAVE_REGISTERS(QID);				\
	  __rc = garbageCollect(GC_GLOBAL_OVERFLOW);	\
	  LOAD_REGISTERS(QID);				\
	  if ( __rc != true )				\
	  { raiseStackOverflow(__rc);			\
	    THROW_EXCEPTION;				\
	  }						\
	  onchange;					\
	}

#define globaliseVar(p)               \
	do { Word __v = gTop++;       \
	     setVar(*__v);            \
	     Trail(p, makeRefG(__v)); \
	   } while(0)

#define globaliseFirstVar(p)          \
	do { Word __v = gTop++;       \
	     setVar(*__v);            \
	     *p = makeRefG(__v);      \
	   } while(0)


		 /*******************************
		 *	    DEBUGGING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
D_BREAK implements break-points in the  code.   A  break-point is set by
replacing  an  instruction  by  a   D_BREAK  instruction.  The  original
instruction is saved in a table. replacedBreak() fetches it.

We simply switch to trace-mode, which will   trap the tracer on the next
breakable instruction (which is what D_BREAK is supposed to replace).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(D_BREAK, 0, 0, ())
{ code c = replacedBreak(PC-1);
  break_action a;
  int pop;				/* arithmetic stack to pop */

  if (LD->fast_condition != NULL)
  { /* Upgrade fast choicepoint to real one in case the D_BREAK involves
       replacing the current instruction with something that fails but
       is not itself a fast condition
    */
    Choice ch;

    if ( !hasLocalSpace(sizeof(struct choice)) )
    { int rc;

      SAVE_REGISTERS(QID);
      rc = growLocalSpace(sizeof(*ch), ALLOW_SHIFT);
      LOAD_REGISTERS(QID);
      if ( rc != true )
      { raiseStackOverflow(rc);
	THROW_EXCEPTION;
      }
    }

    ch = newChoice(CHP_JUMP, FR);
    ch->value.pc = LD->fast_condition;
    ARGP = argFrameP(lTop, 0);
    LD->fast_condition = NULL;
  }


  switch(c)
  { case I_ENTER:
      ARGP = argFrameP(lTop, 0);	/* enter body mode */
      break;
  }

  DEBUG(CHK_SECURE, checkStacks(NULL));

  LD->query->next_environment = lTop;
  SAVE_REGISTERS(QID);
  setLTopInBody();
  DEBUG(0, memset(lTop, 0xbf, sizeof(word)*100));
  DEBUG(CHK_SECURE, checkStacks(NULL));
  a = callBreakHook(FR, BFR, PC-1, decode(c), &pop);
  switch ( a )
  { case BRK_TRACE:
      tracemode(true, NULL);
      break;
    case BRK_DEBUG:
      debugmode(true, NULL);
      break;
    default:
      break;
  }
  LOAD_REGISTERS(QID);
  lTop = LD->query->next_environment;
  LD->query->next_environment = NULL;
  DEBUG(CHK_SECURE, checkStacks(NULL));

  switch ( a )
  { case BRK_ERROR:
    case BRK_TRACE:
    case BRK_DEBUG:
    case BRK_CONTINUE:
      break;
    case BRK_CALL:
      if ( pop )			/* reset arithmetic stack */
      { popArgvArithStack(pop);
	AR_END();
      }
      if ( decode(c) == I_ENTER )
      { Clause cl = FR->clause->value.clause;
					/* I_ENTER replaces the entire body */
	PC = &cl->codes[cl->code_size-1];
      } else
      { PC = stepPC(PC-1);		/* skip the old calling instruction */
      }
      updateAlerted(LD);
      VMI_GOTO(I_CALL1);
  }
  updateAlerted(LD);
  if ( a == BRK_ERROR )
    VMH_GOTO(b_throw);

  VMI_GOTO_CODE(c);
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			TRACER RETRY ACTION

By default, retries the  current  frame.  If   another  frame  is  to be
retried, place the frame-reference, which  should   be  a  parent of the
current frame, in debugstatus.retryFrame and jump to this label. This is
implemented by returning retry(Frame) of the prolog_trace_interception/3
hook.

First, the system will leave any parent  frames. Next, it will undo back
to the call-port and finally, restart the clause.

(*) We use FINISH_EXTERNAL_EXCEPT_UNDO to ensure we undo all marks
    created after the frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_DEBUGGER
VMH(retry, 0, (), ())
{ LocalFrame rframe0, rframe;
  Choice ch;

  if ( debugstatus.retryFrame )
    rframe = (LocalFrame)valTermRef(debugstatus.retryFrame);
  else
    rframe = FR;
  debugstatus.retryFrame = 0;
  rframe0 = rframe;

  /* Find a frame with a choicepoint at the start */
  for( ; rframe; rframe = rframe->parent )
  { if ( (ch = findStartChoice(rframe, BFR)) )
      break;
  }

  if ( rframe )
  { term_t rframe_ref = consTermRef(rframe);

    if ( rframe0 != rframe )
    { DEBUG(MSG_TRACE_RETRY,
	    Sdprintf("[No retry-information for requested frame]\n"));
    }

    DEBUG(MSG_TRACE_RETRY,
	  Sdprintf("[Retrying frame %d running %s]\n",
		   (Word)rframe - (Word)lBase,
		   predicateName(rframe->predicate)));

    SAVE_REGISTERS(QID);
    discardChoicesAfter(rframe, FINISH_EXTERNAL_EXCEPT_UNDO); /* See (*) */
    LOAD_REGISTERS(QID);
    rframe = (LocalFrame)valTermRef(rframe_ref);

    rframe->clause = NULL;
    environment_frame = FR = rframe;
    DEF = FR->predicate;
    clear(FR, FR_SKIPPED);
    exception_term = 0;

    VMH_GOTO(depart_or_retry_continue);
  } else
  { Sdprintf("[Could not find retry-point]\n");
    SAVE_REGISTERS(QID);
    abortProlog();				/* What else? */
    LOAD_REGISTERS(QID);
    THROW_EXCEPTION;
  }
}
END_VMH
#endif /*O_DEBUGGER*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_NOP
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_NOP, 0, 0, ())
{ NEXT_INSTRUCTION;
}
END_VMI

		 /*******************************
		 *	     HEAD UNIFY		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_ATOM is used for an atom in the head of the clause. ARGP points to the
current argument to be matched. ARGP is   derefenced  and unified with a
constant argument. This is the same as   H_SMALLINT, except that we must
mark atoms if AGC is in progress because the AGC marker may already have
visited our stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_ATOM, 0, 1, (CA1_DATA))
{ word c;
  IF_WRITE_MODE_GOTO(B_ATOM);

  c = code2atom(*PC++);
  pushVolatileAtom(word2atom(c));
  VMH_GOTO(h_const, c);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_SMALLINT is used for  small integer  in the head  of the clause.  ARGP
points to the current argument to  be   matched.  ARGP is derefenced and
unified with a constant argument.

The argument is the raw value
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_SMALLINT, 0, 1, (CA1_INTEGER))
{ IF_WRITE_MODE_GOTO(B_SMALLINT);

  scode i = (scode)*PC++;
  VMH_GOTO(h_const, consInt(i));
}
END_VMI

VMI(H_SMALLINTW, 0, CODES_PER_WORD, (CA1_WORD))
{
#if CODES_PER_WORD > 1
  IF_WRITE_MODE_GOTO(B_SMALLINTW);
  word w;
  PC = code_get_word(PC,&w);
  VMH_GOTO(h_const, consInt((sword)w));
#else
  assert(0);
  NEXT_INSTRUCTION;
#endif
}
END_VMI

VMH(h_const, 1, (word), (c))
{ Word k = ARGP;

  deRef(k);
  if ( *k == c )
  { ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( canBind(*k) )
  { if ( !hasGlobalSpace(0) )
    { GROW_STACK_SPACE(0,0);
      k = ARGP;
      deRef(k);
    }
    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}
END_VMH


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_NIL is used for [] in the head.  See H_ATOM for details.
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
  { ENSURE_GLOBAL_SPACE(0, deRef2(ARGP, k));
    bindConst(k, c);
    ARGP++;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_FLOAT: Float in the head. The  float   follows  the instruction and is
represented as a native C-double.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_FLOAT, 0, CODES_PER_DOUBLE, (CA1_FLOAT))
{ Word k;

  IF_WRITE_MODE_GOTO(B_FLOAT);

  deRef2(ARGP, k);
  if ( canBind(*k) )
  { Word p;
    word c;

    ENSURE_GLOBAL_SPACE(2+WORDS_PER_DOUBLE, deRef2(ARGP, k));

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

    if ( memcmp(p, PC, sizeof(double)) == 0 )
    { PC += CODES_PER_DOUBLE;
      ARGP++;
      NEXT_INSTRUCTION;
    }
  }

  CLAUSE_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_MPZ and H_STRING are used for an mpz   number  and string in the head.
They are both implemented using the generic  code for indirects, but the
decompiler must be able to recognise  the   instruction  which is why we
have two instructions.

TBD:	Deal with multiple identical instructions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{ SEPARATE_VMI1;
  VMI_GOTO(H_STRING);
}
END_VMI

VMI(H_MPQ, 0, VM_DYNARGC, (CA1_MPQ))
{ SEPARATE_VMI2;
  VMI_GOTO(H_STRING);
}
END_VMI

VMI(H_STRING, 0, VM_DYNARGC, (CA1_STRING))
{ Word k;

  IF_WRITE_MODE_GOTO(B_STRING);

  deRef2(ARGP, k);
  if ( canBind(*k) )
  { size_t sz = gsizeIndirectFromCode(PC);

    ENSURE_GLOBAL_SPACE(sz, deRef2(ARGP, k));
    struct word_and_Code retval = VM_globalIndirectFromCode(PC);
    PC = retval.code;
    bindConst(k, retval.word);
    ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( isIndirect(*k) )
  { struct word_and_Code retval = VM_equalIndirectFromCode(*k, PC);
    if ( retval.word )
    { PC = retval.code;
      ARGP++;
      NEXT_INSTRUCTION;
    }
  }
  CLAUSE_FAILED;
}
END_VMI


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
END_VMI


VMI(H_VOID_N, 0, 1, (CA1_INTEGER))
{ ARGP += *PC++;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_VAR: A variable in the head which is   not an anonymous one and is not
used for the first time. Invoke general unification between the argument
pointer and the variable, whose offset is given relative to the frame.

When in write-mode, ARGP points  to   uninitialised  data  on the global
stack that must be treated as a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_VAR, 0, 1, (CA1_VAR))
{ Word k = varFrameP(FR, (size_t)*PC++);
  int rc;

  if ( UMODE == uwrite )
  { if ( LD->prolog_flag.occurs_check == OCCURS_CHECK_FALSE )
    { deRef(k);
      if ( isVar(*k) )
      { if ( k > ARGP )			/* k on local stack */
	{ if ( tTop+1 > tMax )
	  { int rc;

	    SAVE_REGISTERS(QID);
	    rc = ensureTrailSpace(1);
	    LOAD_REGISTERS(QID);
	    if ( rc != true )
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

  /* First try the simple case.  do_unify() either completes
   * (true or false) or returns a memory overflow code.  In
   * the latter case we just try again, protected for GC
   */
  if ( LD->prolog_flag.occurs_check == OCCURS_CHECK_FALSE )
  { int rc = do_unify(k, ARGP);
    if ( rc == true )
    { ARGP++;
      NEXT_INSTRUCTION;
    }
    if ( rc == false )
      CLAUSE_FAILED;
  }

  SAVE_REGISTERS(QID);
  rc = unify_ptrs(k, ARGP, ALLOW_GC|ALLOW_SHIFT);
  LOAD_REGISTERS(QID);
  if ( rc )
  { ARGP++;
    NEXT_INSTRUCTION;
  }
  if ( exception_term )
    THROW_EXCEPTION;
  CLAUSE_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_FIRSTVAR: A variable  in  the  head,   which  is  not  anonymous,  but
encountered for the first time. So we know  that the variable is still a
variable. Copy or make a reference.  Trailing   is  not needed as we are
writing in this frame.  ARGP is walking the argument list, but is always
in some compound term as H_FIRSTVAR is not generated for plain variables
in the head.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_FIRSTVAR, 0, 1, (CA1_FVAR))
{ if ( UMODE == uwrite )
  { setVar(*ARGP);
    varFrame(FR, *PC++) = makeRefG(ARGP);
  } else
  { varFrame(FR, *PC++) = (needsRef(*ARGP) ? makeRefG(ARGP) : *ARGP);
  }
  ARGP++;
  NEXT_INSTRUCTION;
}
END_VMI


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
{ pushArgumentStack((Word)((intptr_t)(ARGP + 1)|UMODE));
  VMI_GOTO(H_RFUNCTOR);
}
END_VMI

VMI(H_RFUNCTOR, 0, 1, (CA1_FUNC))
{ functor_t f;
  Word p;

  IF_WRITE_MODE_GOTO(B_RFUNCTOR);

  f = (functor_t) *PC++;
  deRef2(ARGP, p);
  if ( canBind(*p) )
  { size_t arity = arityFunctor(f);
    Word ap;
    word c;

    ENSURE_GLOBAL_SPACE(1+arity, deRef2(ARGP, p));
    ap = gTop;
    gTop += 1+arity;
    c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
    *ap++ = f;
    ARGP = ap;
    while(arity-->0)			/* must clear if we want to do GC */
      setVar(*ap++);
    bindConst(p, c);
    UMODE = uwrite;
    NEXT_INSTRUCTION;
  }
  if ( hasFunctor(*p, f) )
  { ARGP = argTermP(*p, 0);
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_LIST:  As H_FUNCTOR, but using ./2 as predefined functor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_LIST, 0, 0, ())
{ pushArgumentStack((Word)((intptr_t)(ARGP + 1)|UMODE));

  VMI_GOTO(H_RLIST);
}
END_VMI


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

      ENSURE_GLOBAL_SPACE(3, deRef2(ARGP, p));
      ap = gTop;
      gTop += 3;
      c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
      *ap++ = FUNCTOR_dot2;
      setVar(ap[0]);			/* must clear for GC */
      setVar(ap[1]);
      bindConst(p, c);
      ARGP = ap;
      UMODE = uwrite;
      NEXT_INSTRUCTION;
    }
    default:
      CLAUSE_FAILED;
  }
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_POP: Pop the saved argument pointer pushed by H_FUNCTOR and H_LIST.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_POP, 0, 0, ())
{ ARGP = *--aTop;
  UMODE = ((int)(uintptr_t)ARGP & uwrite);
  ARGP = (Word)((intptr_t)ARGP&~uwrite);
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
H_LIST_FF: [Var1|Var2] in the head where both   Var1 and Var2 appear for
the  first  time.  This  appears    quite  commonly  in  list-processing
predicates:

	pred([], ...).
	pred([H|T], ...) :-
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(H_LIST_FF, 0, 2, (CA1_FVAR,CA1_FVAR))
{ Word p;

  if ( UMODE == uwrite )
  { p = ARGP;
    goto write;
  } else
  { deRef2(ARGP, p);

    if ( isList(*p) )
    { p = argTermP(*p, 0);
      varFrame(FR, *PC++) = (needsRef(*p) ? makeRefG(p) : *p);
      p++;
      varFrame(FR, *PC++) = (needsRef(*p) ? makeRefG(p) : *p);
    } else if ( canBind(*p) )
    { word c;
      Word ap;

    write:
      ENSURE_GLOBAL_SPACE(3,
			  { if ( UMODE == uwrite )
			      p = ARGP;
			    else
			      deRef2(ARGP, p);
			  });
      ap = gTop;
      gTop = ap+3;
      c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
      *ap++ = FUNCTOR_dot2;
      setVar(*ap); varFrame(FR, *PC++) = makeRefG(ap); ap++;
      setVar(*ap); varFrame(FR, *PC++) = makeRefG(ap);
      if ( UMODE == uwrite )
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
END_VMI


		 /*******************************
		 *	 BODY UNIFICATION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_ATOM, B_SMALLINT, B_NIL: An atomic constant in the body of a clause. We
know that ARGP is pointing to a not yet instantiated argument of the next
frame and therefore can just fill the argument. Trailing is not needed as
this is above the stack anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_ATOM, VIF_LCO, 1, (CA1_DATA))
{ word c = code2atom(*PC++);
  pushVolatileAtom(word2atom(c));
  *ARGP++ = c;
  NEXT_INSTRUCTION;
}
END_VMI

VMI(B_SMALLINT, VIF_LCO, 1, (CA1_INTEGER))
{ scode i = (scode)*PC++;
  *ARGP++ = consInt(i);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(B_SMALLINTW, VIF_LCO, CODES_PER_WORD, (CA1_WORD))
{
#if CODES_PER_WORD > 1
  word w;
  PC = code_get_word(PC,&w);
  *ARGP++ = consInt((sword)w);
#else
  assert(0);
#endif
  NEXT_INSTRUCTION;
}
END_VMI

VMI(B_NIL, VIF_LCO, 0, ())
{ *ARGP++ = ATOM_nil;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_FLOAT: Float in the  body.  PC  is   followed  by  a  double in native
representation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_FLOAT, 0, CODES_PER_DOUBLE, (CA1_FLOAT))
{ Word p;

  ENSURE_GLOBAL_SPACE(2+WORDS_PER_DOUBLE, (void)0);
  p = gTop;
  gTop += 2+WORDS_PER_DOUBLE;

  *ARGP++ = consPtr(p, TAG_FLOAT|STG_GLOBAL);
  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  cpDoubleData(p, PC);
  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_MPZ: MPZ number in body
B_STRING: string in body

Both copy following indirect to the  global   stack.  See also H_MPZ and
H_STRING.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{ SEPARATE_VMI1;
  VMI_GOTO(B_STRING);
}
END_VMI

VMI(B_MPQ, 0, VM_DYNARGC, (CA1_MPQ))
{ SEPARATE_VMI2;
  VMI_GOTO(B_STRING);
}
END_VMI

VMI(B_STRING, 0, VM_DYNARGC, (CA1_STRING))
{ size_t sz = gsizeIndirectFromCode(PC);

  ENSURE_GLOBAL_SPACE(sz, (void)0);
  struct word_and_Code retval = VM_globalIndirectFromCode(PC);
  PC = retval.code;
  *ARGP++ = retval.word;
  NEXT_INSTRUCTION;
}
END_VMI


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

	SAVE_REGISTERS(QID);
	rc = ensureTrailSpace(1);
	LOAD_REGISTERS(QID);
	if ( rc != true )
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
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_VAR, B_VAR<N>: A variable in the body   which  is not an anonymous one
and is not used for  the  first  time.   We  now  know  that  *ARGP is a
variable, so we either copy the value   or make a reference. Trailing is
not needed as we are writing above the stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_VAR0, VIF_LCO, 0, ())
{ VMH_GOTO(bvar_cont, VAROFFSET(0));
}
END_VMI

VMI(B_VAR1, VIF_LCO, 0, ())
{ VMH_GOTO(bvar_cont, VAROFFSET(1));
}
END_VMI

VMI(B_VAR2, VIF_LCO, 0, ())
{ VMH_GOTO(bvar_cont, VAROFFSET(2));
}
END_VMI

VMI(B_VAR, VIF_LCO, 1, (CA1_VAR))
{ VMH_GOTO(bvar_cont, (int)*PC++);
}
END_VMI

VMH(bvar_cont, 1, (int), (voffset))
{ Word p;
  p = varFrameP(FR, voffset);
  if ( unlikely(isVar(*p)) )
  { ENSURE_GLOBAL_SPACE(1, p = varFrameP(FR, voffset));
    globaliseVar(p);
    *ARGP++ = *p;
  } else
  { *ARGP++ = linkValI(p);
  }
  NEXT_INSTRUCTION;
}
END_VMH

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
create a frame for =/2 and call it. Note that the `slow unify' mode must
be consistently applied in B_UNIFY_VAR and B_UNIFY_EXIT, which is why we
copy the global value into a local variable.

TBD: B_UNIFY_CONST <var>, <const>
     B_UNIFY_VAR <var1>, <var2>

Note  that  the  B_UNIFY_FIRSTVAR  assumes  write   mode,  but  this  is
unimportant because the compiler generates write (B_*) instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_FIRSTVAR, VIF_BREAK, 1, (CA1_FVAR))
{ ARGP = varFrameP(FR, (int)*PC++);
  setVar(*ARGP);			/* needed for GC */
  VMH_GOTO(unify_var_cont);
}
END_VMI


VMI(B_UNIFY_VAR, VIF_BREAK, 1, (CA1_VAR))
{ ARGP = varFrameP(FR, (int)*PC++);
  VMH_GOTO(unify_var_cont);
}
END_VMI

VMH(unify_var_cont, 0, (), ())
{ if ( (SLOW_UNIFY=LD->slow_unify) )
  { Word k = ARGP;

    if ( isVar(*k) )
    { ENSURE_GLOBAL_SPACE(1, k = ARGP);
      globaliseVar(k);
    }

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *k;
    setVar(*ARGP);
    UMODE = uwrite;			/* must write for GC to work */
    NEXT_INSTRUCTION;
  }

  UMODE = uread;			/* needed? */
  NEXT_INSTRUCTION;
}
END_VMH


VMI(B_UNIFY_EXIT, 0, 0, ())
{ ARGP = argFrameP(lTop, 0);
  if ( SLOW_UNIFY )
  { NFR = lTop;
    DEF = GD->procedures.equals2->definition;
    setNextFrameFlags(NFR, FR);
    VMH_GOTO(normal_call);
  }

  CHECK_WAKEUP;				/* only for non-first-var */
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	B_UNIFY_[FV][FV] VAR1 VAR2
Unify two variables.  F stands for a first-var; V for any other var
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_FF, VIF_BREAK, 2, (CA1_FVAR,CA1_FVAR))
{ ENSURE_GLOBAL_SPACE(2, (void)0);
  Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);
  Word v  = gTop++;

  setVar(*v);
  *v1 = makeRefG(v);

  if ( LD->slow_unify )
  { v = gTop++;
    setVar(*v);
    *v2 = makeRefG(v);

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *v1;
    *ARGP++ = *v2;
    VMH_GOTO(debug_equals2);
  } else
  { *v2 = *v1;
  }

  NEXT_INSTRUCTION;
}
END_VMI


/* B_UNIFY_VF is the same as B_UNIFY_FV, but the arguments
 * are swapped by the compiler.  The distinction is needed
 * to allow the decompiler return the correct argument order.
 * Having swapped V1=V2 is hard to compensate for in the
 * GUI tracer.
 */

VMI(B_UNIFY_VF, VIF_BREAK, 2, (CA1_FVAR,CA1_VAR))
{ SEPARATE_VMI1;
  VMI_GOTO(B_UNIFY_FV);
}
END_VMI


VMI(B_UNIFY_FV, VIF_BREAK, 2, (CA1_FVAR,CA1_VAR))
{ ENSURE_GLOBAL_SPACE(2, (void)0);
  Word f = varFrameP(FR, (int)*PC++);
  Word v = varFrameP(FR, (int)*PC++);

  if ( isVar(*v) )
    globaliseVar(v);

  if ( LD->slow_unify )
  { globaliseFirstVar(f);
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *f;
    *ARGP++ = *v;
    VMH_GOTO(debug_equals2);
  }

  *f = linkValI(v);

  NEXT_INSTRUCTION;
}
END_VMI


VMI(B_UNIFY_VV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ int rc;
  Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);

  if ( LD->slow_unify )
  { if ( isVar(*v1) || isVar(*v2) )
    { ENSURE_GLOBAL_SPACE(2, { v1 = varFrameP(FR, PC[-2]);
			       v2 = varFrameP(FR, PC[-1]);
			     });
      if ( isVar(*v1) )
	globaliseVar(v1);
      if ( isVar(*v2) )
	globaliseVar(v2);
    }

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *v1;
    *ARGP++ = *v2;
    VMH_GOTO(debug_equals2);
  }

  SAVE_REGISTERS(QID);
  rc = unify_ptrs(v1, v2, ALLOW_GC|ALLOW_SHIFT);
  LOAD_REGISTERS(QID);
  if ( rc )
  { CHECK_WAKEUP;
    NEXT_INSTRUCTION;
  }
  if ( exception_term )
    THROW_EXCEPTION;

  BODY_FAILED;
}
END_VMI

VMH(debug_equals2, 0, (), ())
{ NFR = lTop;
  DEF = GD->procedures.equals2->definition;
  setNextFrameFlags(NFR, FR);
  VMH_GOTO(normal_call);
}
END_VMH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_UNIFY_FC: Unify first variable with a constant.  Always succeeds, no
need for wakeup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_FC, VIF_BREAK, 2, (CA1_FVAR, CA1_DATA))
{ Word f = varFrameP(FR, (int)*PC++);
  word c = code2word(*PC++);

  if ( LD->slow_unify )
  { ENSURE_GLOBAL_SPACE(1, f = varFrameP(FR, PC[-2]));
    globaliseFirstVar(f);
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *f;
    *ARGP++ = c;
    VMH_GOTO(debug_equals2);
  }

  *f = c;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_UNIFY_VC: Unify a variable (not first) with a constant in the body.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_UNIFY_VC, VIF_BREAK, 2, (CA1_VAR, CA1_DATA))
{ Word k = varFrameP(FR, (int)*PC++);
  word c = code2word(*PC++);

  if ( LD->slow_unify )
  { if ( isVar(*k) )
    { ENSURE_GLOBAL_SPACE(1, k = varFrameP(FR, (int)PC[-2]));
      globaliseVar(k);
    }
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *k;
    *ARGP++ = c;
    VMH_GOTO(debug_equals2);
  }

  deRef(k);
  if ( *k == c )
    NEXT_INSTRUCTION;
  if ( canBind(*k) )
  { ENSURE_GLOBAL_SPACE(0,
			{ k = varFrameP(FR, (int)PC[-2]);
			  deRef(k);
			});
    bindConst(k, c);
    CHECK_WAKEUP;
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_EQ_VV: translation of	Var1 == Var2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_EQ_VV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);
  int rc;

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { if ( isVar(*v1) || isVar(*v2) )
    { ENSURE_GLOBAL_SPACE(2, { v1 = varFrameP(FR, (int)PC[-2]);
			       v2 = varFrameP(FR, (int)PC[-1]);
			     });
      if ( isVar(*v1) ) globaliseVar(v1);
      if ( isVar(*v2) ) globaliseVar(v2);
    }
    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *v1;
    *ARGP++ = *v2;
    VMH_GOTO(debug_eq_vv);
  }
#endif

  if ( (rc=compareStandard(v1, v2, true)) == 0 )
    NEXT_INSTRUCTION;
  if ( rc == CMP_ERROR )
    THROW_EXCEPTION;

  FASTCOND_FAILED;
}
END_VMI

VMH(debug_eq_vv, 0, (), ())
{ NFR = lTop;
  DEF = GD->procedures.strict_equal2->definition;
  setNextFrameFlags(NFR, FR);
  VMH_GOTO(normal_call);
}
END_VMH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_EQ_VC Var == constant
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_EQ_VC, VIF_BREAK, 2, (CA1_VAR,CA1_DATA))
{ Word v1 = varFrameP(FR, (int)*PC++);
  word c  = code2word(*PC++);

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { if ( isVar(*v1) )
    { ENSURE_GLOBAL_SPACE(1, v1 = varFrameP(FR, (int)PC[-2]));
      globaliseVar(v1);
    }

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = *v1;
    *ARGP++ = c;
    VMH_GOTO(debug_eq_vv);
  }
#endif

  deRef(v1);
  if ( *v1 == c )
    NEXT_INSTRUCTION;

  FASTCOND_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_NEQ_VV: translation of Var1 \== Var2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_NEQ_VV, VIF_BREAK, 2, (CA1_VAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (int)*PC++);
  Word v2 = varFrameP(FR, (int)*PC++);
  int rc;

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { if ( isVar(*v1) || isVar(*v2) )
    { ENSURE_GLOBAL_SPACE(2, { v1 = varFrameP(FR, (int)PC[-2]);
			       v2 = varFrameP(FR, (int)PC[-1]);
			     });
      if ( isVar(*v1) )
	globaliseVar(v1);
      if ( isVar(*v2) )
	globaliseVar(v2);
    }

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkValI(v1);
    *ARGP++ = linkValI(v2);
    VMH_GOTO(debug_neq_vv);
  }
#endif

  if ( (rc=compareStandard(v1, v2, true)) == 0 )
    FASTCOND_FAILED;
  if ( rc == CMP_ERROR )
    THROW_EXCEPTION;

  NEXT_INSTRUCTION;
}
END_VMI

VMH(debug_neq_vv, 0, (), ())
{ NFR = lTop;
  DEF = GD->procedures.not_strict_equal2->definition;
  setNextFrameFlags(NFR, FR);
  VMH_GOTO(normal_call);
}
END_VMH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_NEQ_VC Var == constant
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_NEQ_VC, VIF_BREAK, 2, (CA1_VAR,CA1_DATA))
{ Word v1 = varFrameP(FR, (int)*PC++);
  word c  = code2word(*PC++);

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { if ( isVar(*v1) )
    { ENSURE_GLOBAL_SPACE(1, v1 = varFrameP(FR, (int)PC[-2]));
      globaliseVar(v1);
    }

    ARGP = argFrameP(lTop, 0);
    *ARGP++ = linkValI(v1);
    *ARGP++ = c;
    VMH_GOTO(debug_neq_vv);
  }
#endif

  deRef(v1);
  if ( *v1 != c )
    NEXT_INSTRUCTION;

  FASTCOND_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
arg/3 special cases
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


VMI(B_ARG_CF, VIF_BREAK, 3, (CA1_INTEGER,CA1_VAR,CA1_FVAR))
{ ENSURE_GLOBAL_SPACE(2, (void)0);

  PC += 3;
  VMH_GOTO(arg3_fast,
	   NULL,
	   (scode)PC[-3],
	   varFrameP(FR, PC[-2]),
	   varFrameP(FR, PC[-1]));
}
END_VMI

VMH(arg3_fast, 4, (Word, scode, Word, Word), (aidx, ai, aterm, aarg))
{ if ( isVar(*aterm) )
  { globaliseVar(aterm);	/* instantiation error, go slow route */
  } else
  { deRef(aterm);
    if ( isTerm(*aterm) && likely(truePrologFlag(PLFLAG_VMI_BUILTIN)) )
    { size_t arity = arityTerm(*aterm);
      if ( ai > 0 && ai <= arity )
      { *aarg = linkValI(argTermP(*aterm, ai-1));
	NEXT_INSTRUCTION;
      }
    }
  }
  VMH_GOTO(arg3_slow, aidx, ai, aterm, aarg);
}
END_VMH

VMH(arg3_slow, 4, (Word, scode, Word, Word), (aidx, ai, aterm, aarg))
{ globaliseFirstVar(aarg);
  ARGP = argFrameP(lTop, 0);
  *ARGP++ = aidx ? *aidx : consInt(ai);
  *ARGP++ = *aterm;
  *ARGP++ = *aarg;

  NFR = lTop;
  DEF = GD->procedures.arg3->definition;
  setNextFrameFlags(NFR, FR);
  VMH_GOTO(normal_call);
}
END_VMH

VMI(B_ARG_VF, VIF_BREAK, 3, (CA1_VAR,CA1_VAR,CA1_FVAR))
{ Word aidx, aidx0, aterm, aarg;

  ENSURE_GLOBAL_SPACE(3, (void)0);

  aidx0 = varFrameP(FR, *PC++);
  aterm = varFrameP(FR, *PC++);
  aarg  = varFrameP(FR, *PC++);

  if ( isVar(*aidx0) ) globaliseVar(aidx0);
  if ( isVar(*aterm) ) globaliseVar(aterm);

  deRef2(aidx0, aidx);
  if ( isTaggedInt(*aidx) )
  { VMH_GOTO(arg3_fast, aidx, (scode)valInt(*aidx), aterm, aarg);
  }

  aidx = aidx0;
  VMH_GOTO(arg3_slow, aidx, 0, aterm, aarg);
}
END_VMI

#endif /*O_COMPILE_IS*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_ARGFIRSTVAR: A variable in the body nested  in a term, encountered for
the first time. We now know both   *ARGP and the variable are variables.
ARGP points to the argument of a term on the global stack. The reference
should therefore go from k to ARGP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_ARGFIRSTVAR, 0, 1, (CA1_FVAR))
{ setVar(*ARGP);
  varFrame(FR, *PC++) = makeRefG(ARGP++);
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_FIRSTVAR: A variable in the body, encountered   for the first time. We
now know both *ARGP and the variable  are variables. We set the variable
to be a variable (it is uninitialised   memory) and make a reference. No
trailing needed as we are writing in this and the next frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_FIRSTVAR, 0, 1, (CA1_FVAR))
{ Word k = varFrameP(FR, *PC++);
  Word v;
  word w;

  ENSURE_GLOBAL_SPACE(1, k = varFrameP(FR, PC[-1]));
  v = gTop++;
  setVar(*v);
  w = makeRefG(v);
  *k = w;
  *ARGP++ = w;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_VOID: A singleton variable in  the  body.   Ensure  the  argument is a
variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_VOID, VIF_LCO, 0, ())
{ setVar(*ARGP++);
  NEXT_INSTRUCTION;
}
END_VMI


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
END_VMI


VMI(B_RFUNCTOR, 0, 1, (CA1_FUNC))
{ functor_t f = (functor_t) *PC++;
  size_t arity = arityFunctor(f);
  Word ap;

  ENSURE_GLOBAL_SPACE(1+arity, (void)0);
  *ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
  ARGP = gTop;
  *ARGP++ = f;
  for(ap=ARGP; arity-->0;)		/* must clear if we want to do GC */
    setVar(*ap++);
  gTop = ap;

  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_LIST: Same as B_FUNCTOR for ./2
B_RLIST: Right-argument recursive B_LIST
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_LIST, 0, 0, ())
{ pushArgumentStack(ARGP+1);
  VMI_GOTO(B_RLIST);
}
END_VMI


VMI(B_RLIST, 0, 0, ())
{ ENSURE_GLOBAL_SPACE(3, (void)0);
  *ARGP = consPtr(gTop, TAG_COMPOUND|STG_GLOBAL);
  ARGP = gTop;
  *ARGP++ = FUNCTOR_dot2;
  setVar(ARGP[0]);
  setVar(ARGP[1]);
  gTop = ARGP+2;

  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
B_POP: Pop the argument pointer pushed by B_FUNCTOR.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_POP, 0, 0, ())
{ ARGP = *--aTop;
  NEXT_INSTRUCTION;
}
END_VMI


		 /*******************************
		 *   SINGLE SIDED UNIFICATION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translation of Picat like rule matching.  Handles

  - Head ?=> Body
    Head is matched against the goal without further unifying Goal.
    On match, non-deterministically select this clause.
  - Head => Body
    Head is matched against the goal without further unifying Goal.
    On match, deterministically select this clause.

The clause must start with I_CHP, so there   is surely a choice point at
the start of the predicate and we can   check  the trail stack to verify
the match was made without unification.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CHP, 0, 0, ())
{ if ( BFR->frame != FR )
    newChoice(CHP_DEBUG, FR);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(I_SSU_CHOICE, 0, 0, ())
{ if ( tTop > BFR->mark.trailtop.as_ptr )
    CLAUSE_FAILED;

  VMI_GOTO(I_ENTER);
}
END_VMI

VMI(I_SSU_COMMIT, 0, 0, ())
{ if ( tTop > BFR->mark.trailtop.as_ptr )
    CLAUSE_FAILED;

  clear(FR, FR_SSU_DET);

  if ( !debugstatus.debugging )
  { lTop = (LocalFrame)BFR;
    BFR = BFR->parent;
  } else
  { BFR->type = CHP_DEBUG;
  }

  VMI_GOTO(I_ENTER);
}
END_VMI

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
  { Coverage(FR, UNIFY_PORT);
#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;

      SAVE_REGISTERS(QID);
      clearUninitialisedVarsFrame(FR, PC);
      action = tracePort(FR, BFR, UNIFY_PORT, PC);
      LOAD_REGISTERS(QID);
      VMH_GOTO(debug_unify_continue, action);
    }
#endif /*O_DEBUGGER*/
    CHECK_WAKEUP;
  }
  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CONTEXT is used by  non-meta  predicates   that  are  compiled  into a
different  module  using  <module>:<head>  :-    <body>.  The  I_CONTEXT
instruction immediately follows the I_ENTER. The argument is the module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CONTEXT, 0, 1, (CA1_MODULE))
{ Module m = code2ptr(Module, *PC++);

  setContextModule(FR, m);

  NEXT_INSTRUCTION;
}
END_VMI

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

VMI(I_CALL, VIF_BREAK, 1, (CA1_LPROC))
{ Procedure proc = code2ptr(Procedure, *PC++);

  NFR = lTop;
  setNextFrameFlags(NFR, FR);
  DEF = proc->definition;
  VMH_GOTO(normal_call);
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is the common part of the call variations.  By now the following is
true:

  - NFR			Points to new frame
  - arguments		filled
  - DEF			filled with predicate to call
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMH(normal_call, 0, (), ())

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise those slots of the frame that are common to Prolog predicates
and foreign ones.  There might be some possibilities for optimisation by
delaying these initialisations till they are really  needed  or  because
the information they are calculated from is destroyed.  This probably is
not worthwile.

Note: we are working above `lTop' here!!   We restore this as quickly as
possible to be able to call-back to Prolog.

(*) If we do not resolve here,  the handleSignals() may resolve the same
procedure and deallocate our temporary version if threading is not used.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
{ NFR->parent         = FR;
  setFramePredicate(NFR, DEF);		/* TBD */
  NFR->programPointer = PC;		/* save PC in child */
  NFR->clause         = NULL;		/* for save atom-gc */
  environment_frame = FR = NFR;		/* open the frame */

  if ( unlikely(!hasLocalSpace(LOCAL_MARGIN)) )
  { int rc;

    lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
    SAVE_REGISTERS(QID);
    rc = growLocalSpace(LOCAL_MARGIN, ALLOW_SHIFT);
    LOAD_REGISTERS(QID);
    if ( rc != true )
    { rc = raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  VMH_GOTO(depart_or_retry_continue);
}
END_VMH

VMH(depart_or_retry_continue, 0, (), ())
{
  setGenerationFrame(FR);
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
    PC = DEF->codes;

					/* we need the autoloader and get back */
    if ( DEF->codes[0] == encode(S_VIRGIN) &&
	 !DEF->impl.any.defined && isoff(DEF, PROC_DEFINED) )
    { PC = DEF->codes;
      NEXT_INSTRUCTION;
    }

    if ( is_signalled() )
    { SAVE_REGISTERS(QID);
      DEF = getProcDefinedDefinition(DEF); /* see (*) above */
      LOAD_REGISTERS(QID);
      if ( FR->predicate != DEF )		/* auto imported/loaded */
      { setFramePredicate(FR, DEF);
	setGenerationFrame(FR);
      }
      if ( isoff(DEF, P_SIG_ATOMIC) )
      { SAVE_REGISTERS(QID);
	handleSignals();
	LOAD_REGISTERS(QID);

	if ( exception_term )
	{ CL = NULL;

	  enterDefinition(DEF);
					  /* The catch is not yet installed, */
					  /* so we ignore it */
	  if ( FR->predicate == PROCEDURE_catch3->definition )
	    set(FR, FR_CAUGHT);

	  THROW_EXCEPTION;
	}
      }
    }
    if ( UNDO_SCHEDULED(LD) )
    { int rc;

      SAVE_REGISTERS(QID);
      rc = run_undo_hooks();
      LOAD_REGISTERS(QID);
      if ( !rc )
	THROW_EXCEPTION;
    }

    Profile(FR->prof_node = profCall(DEF));
    Coverage(FR, CALL_PORT);

#ifdef O_LIMIT_DEPTH
    { size_t depth = levelFrame(FR);

      if ( depth > LD->depth_info.reached )
	LD->depth_info.reached = depth;
      if ( depth > LD->depth_info.limit )
      { DEBUG(2, Sdprintf("depth-limit\n"));

	if ( debugstatus.debugging )
	  newChoice(CHP_DEBUG, FR);
	FRAME_FAILED;
      }
    }
#endif

#ifdef O_INFERENCE_LIMIT
    if ( LD->statistics.inferences >= LD->inference_limit.limit )
    { lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
      SAVE_REGISTERS(QID);
      raiseInferenceLimitException();
      LOAD_REGISTERS(QID);
      if ( exception_term )
	THROW_EXCEPTION;
    }
#endif

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int rc;

      lTop = (LocalFrame) argFrameP(FR, DEF->functor->arity);
      SAVE_REGISTERS(QID);
      DEF = getProcDefinedDefinition(DEF);
      LOAD_REGISTERS(QID);
      if ( FR->predicate != DEF )		/* auto imported/loaded */
      { setFramePredicate(FR, DEF);
	setGenerationFrame(FR);
#ifdef O_PROFILE
	if ( FR->prof_node )
	  profSetHandle(FR->prof_node, DEF);
#endif
	VMH_GOTO(depart_or_retry_continue);
      }
      set(FR, FR_INBOX);

      SAVE_REGISTERS(QID);
      rc = tracePort(FR, BFR, CALL_PORT, NULL);
      LOAD_REGISTERS(QID);
      VMH_GOTO(debug_call_continue, rc);
    }
#endif /*O_DEBUGGER*/
  } /* end of if (LD->alerted) */

  PC = DEF->codes;
  NEXT_INSTRUCTION;
}
END_VMH

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

VMI(I_DEPART, VIF_BREAK, 1, (CA1_LPROC))
{ Procedure proc = code2ptr(Procedure, *PC++);

  if ( (void *)BFR <= (void *)FR &&
       truePrologFlag(PLFLAG_LASTCALL) &&
       ( proc->definition->impl.any.defined ||
	 ison(proc->definition, PROC_DEFINED)) )
  { if ( ison(FR, FR_WATCHED) )
    { LD->query->next_environment = lTop;
      lTop = (LocalFrame)ARGP;		/* just pushed arguments, so top */
      SAVE_REGISTERS(QID);
      frameFinished(FR, FINISH_EXIT);
      LOAD_REGISTERS(QID);
      lTop = LD->query->next_environment;
      LD->query->next_environment = NULL;
      if ( exception_term )
	THROW_EXCEPTION;
    }

    FR->clause = NULL;			/* for save atom-gc */
    leaveDefinition(DEF);
    DEF = proc->definition;
    if ( ison(DEF, P_TRANSPARENT) )
    { FR->context = contextModule(FR); /* must be  before setting FR_CONTEXT */
      FR->level++;
      FR->flags = ((FR->flags & ~(FR_LCO_CLEAR|FR_CLEAR_ALWAYS)) | FR_CONTEXT);
    } else
    { lcoSetNextFrameFlags(FR);
    }
    if ( ison(DEF, HIDE_CHILDS) )
      set(FR, FR_HIDE_CHILDS);

    setFramePredicate(FR, DEF);
    copyFrameArguments(lTop, FR, DEF->functor->arity);

    VMH_GOTO(depart_or_retry_continue);
  }

  NFR = lTop;
  lcoSetNextFrameFlags2(NFR, FR);
  DEF = proc->definition;
  VMH_GOTO(normal_call);
}
END_VMI


#ifdef O_CALL_AT_MODULE
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_DEPARTATM: procedure-module, context-module, procedure
See I_CALLATM for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
VMI(I_DEPARTATM, VIF_BREAK, 3, (CA1_MODULE, CA1_MODULE, CA1_PROC))
{ PC++;						/* Ignore :-qualifier */
  VMI_GOTO(I_DEPARTM);
}
END_VMI
#endif


VMI(I_DEPARTM, VIF_BREAK, 2, (CA1_MODULE, CA1_PROC))
{ if ( (void *)BFR > (void *)FR || !truePrologFlag(PLFLAG_LASTCALL) )
  { VMI_GOTO(I_CALLM);
  } else
  { Module m = code2ptr(Module, *PC++);

    setContextModule(FR, m);
    VMI_GOTO(I_DEPART);
  }
}
END_VMI


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
{ if ( unlikely(LD->alerted != 0) )
  { if ( (LD->alerted&ALERT_BUFFER) )
    { LD->alerted &= ~ALERT_BUFFER;
      release_string_buffers_from_frame(FR);
    }
    Coverage(FR, EXIT_PORT);

#if O_DEBUGGER
    if ( debugstatus.debugging )
    { int action;

      SAVE_REGISTERS(QID);
      action = tracePort(FR, BFR, EXIT_PORT, PC);
      LOAD_REGISTERS(QID);

      VMH_GOTO(debug_exit_continue, action);
    }
#endif /*O_DEBUGGER*/
  }

  VMH_GOTO(exit_continue);
}
END_VMI

VMH(exit_continue, 0, (), ())
{ LocalFrame leave;

  if ( (void *)BFR <= (void *)FR )	/* deterministic */
  { leave = ison(FR, FR_WATCHED) ? FR : NULL;
    FR->clause = NULL;			/* leaveDefinition() destroys clause */
    leaveDefinition(DEF);		/* dynamic pred only */
    lTop = FR;
    DEBUG(3, Sdprintf("Deterministic exit of %s, lTop = #%zd\n",
		      predicateName(FR->predicate), loffset(lTop)));
  } else
  { leave = NULL;
    clear(FR, FR_INBOX);
    if ( ison(FR, FR_DET|FR_DETGUARD) )
    { SAVE_REGISTERS(QID);
      determinism_error(FR, BFR, ATOM_nondet);
      LOAD_REGISTERS(QID);
      if ( exception_term )
	THROW_EXCEPTION;
    }
  }

  PC = FR->programPointer;
  environment_frame = FR = FR->parent;
  DEF = FR->predicate;
  ARGP = argFrameP(lTop, 0);
  Profile(profResumeParent(FR->prof_node));
  if ( leave )
  { SAVE_REGISTERS(QID);
    frameFinished(leave, FINISH_EXIT);
    LOAD_REGISTERS(QID);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;
}
END_VMH


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

      SAVE_REGISTERS(QID);
      action = tracePort(FR, BFR, UNIFY_PORT, PC);
      LOAD_REGISTERS(QID);

      switch( action )
      { case PL_TRACE_ACTION_RETRY:
	  TRACE_RETRY;
	case PL_TRACE_ACTION_ABORT:
	  THROW_EXCEPTION;
      }
    }
#endif /*O_DEBUGGER*/
    Coverage(FR, UNIFY_PORT);
  }
  VMH_GOTO(exit_checking_wakeup);
}
END_VMI

VMH(exit_checking_wakeup, 0, (), ())
{
#ifdef O_ATTVAR
  if ( LD->alerted & ALERT_WAKEUP )
  { LD->alerted &= ~ALERT_WAKEUP;

    if ( *valTermRef(LD->attvar.head) )
    { PC = SUPERVISOR(exit);
      ATTVAR_WAKEUP;
    }
  } else
#endif
  if ( LD->yield.frequency )
  { uint64_t itrunc = LD->statistics.inferences&(~(uint64_t)0xf);

    if ( itrunc % LD->yield.frequency == 0 &&
	 LD->yield.fired != itrunc &&
	 GD->procedures.heartbeat0->definition->impl.clauses.first_clause &&
	 !exception_term )
    { LD->yield.fired = itrunc;
      PC = SUPERVISOR(exit);		/* See VMH(wakeup) */
      NFR = lTop;
      setNextFrameFlags(NFR, FR);
      SAVE_REGISTERS(QID);
      DEF = GD->procedures.heartbeat0->definition;
      LOAD_REGISTERS(QID);
      ARGP = argFrameP(NFR, 0);

      VMH_GOTO(normal_call);
    }
  }

  VMI_GOTO(I_EXIT);
}
END_VMH


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Created for the return of the toplevel query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_EXITQUERY, 0, 0, ())
{ assert(!FR->parent);

  QF = QueryFromQid(QID);		/* may be shifted: recompute */
  QF->solutions++;

  assert(FR == &QF->top_frame);

  if ( BFR == &QF->choice )		/* No alternatives */
  { set(QF, PL_Q_DETERMINISTIC);
    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
    FR->clause = NULL;

    if ( ison(FR, FR_WATCHED) )
    { SAVE_REGISTERS(QID);
      frameFinished(FR, FINISH_EXIT);
      LOAD_REGISTERS(QID);
    }
  }

#ifdef O_PROFILE
  if ( LD->profile.active )
  { LocalFrame parent = parentFrame(FR);

    if ( parent )
      profResumeParent(parent->prof_node);
    else
      profResumeParent(NULL);
  }
#endif

  QF->foreign_frame = PL_open_foreign_frame();
#if !O_VMI_FUNCTIONS
  assert(LD->exception.throw_environment == &THROW_ENV);
  LD->exception.throw_environment = THROW_ENV.parent;
#endif

#define DET_EXIT (PL_Q_DETERMINISTIC|PL_Q_EXT_STATUS)
  SOLUTION_RETURN((QF->flags&DET_EXIT)==DET_EXIT ? PL_S_LAST : true);
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_YIELD: Yield control from this engine. This  VMI fetches the first and
second argument. The second must be an integer. PL_next_solution() exits
with code, providing Term through PL_yielded. Calling PL_next_solution()
again resumes the VM.

'$engine_yield'(Term, Code) :-
	'$yield'.

'$yield' translates to I_YIELD
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_YIELD, VIF_BREAK, 0, ())
{ Word p;

  QF = QueryFromQid(QID);
  if ( !(QF->flags&PL_Q_ALLOW_YIELD) )
  { PL_error(NULL, 0, "not an engine", ERR_PERMISSION_VMI, "I_YIELD");
    THROW_EXCEPTION;
  }
  if ( (void*)BFR < (void*)FR )
    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
  ARGP = argFrameP(lTop, 0);
  SAVE_REGISTERS(QID);
  DEBUG(CHK_SECURE, checkStacks(NULL));

  QF->foreign_frame = PL_open_foreign_frame();
  QF->yield.term = PL_new_term_ref();
  p = argFrameP(FR, 0);
  if ( isVar(*p) )
  { ENSURE_GLOBAL_SPACE(1, p = argFrameP(FR, 0));
    QF = QueryFromQid(QID);
    globaliseVar(p);
  }
  *valTermRef(QF->yield.term) = linkValI(p);
  DEBUG(CHK_SECURE, checkStacks(NULL));

  p = argFrameP(FR, 1);
  deRef(p);

  if ( isTaggedInt(*p) )
  { sword code = valInt(*p);
#if !O_VMI_FUNCTIONS
    assert(LD->exception.throw_environment == &THROW_ENV);
    LD->exception.throw_environment = THROW_ENV.parent;
#endif

    assert(code >= INT_MIN && code <= INT_MAX);
    SOLUTION_RETURN((int)code);
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE,
	     ATOM_integer, pushWordAsTermRef(argFrameP(FR, 1)));
    popTermRef();
    THROW_EXCEPTION;
  }
}
END_VMI

		 /*******************************
		 *	      LCO CALLS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Last call optimization instructions. Such a block is defined as follows:

    L_NOLCO Ln
    L_VAR t,f
    ...
    I_TCALL (or I_LCALL proc)
Ln: <normal sequence>
    I_DEPART proc
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(L_NOLCO, 0, 1, (CA1_JUMP))
{ size_t jmp = *PC++;

  if ( (void *)BFR <= (void *)FR && truePrologFlag(PLFLAG_LASTCALL) )
    NEXT_INSTRUCTION;

  PC += jmp;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
L_VAR(to,from)  moves  a  variable  in  the  current  frame.  Note  that
variables either have a value, are 0 (B_VOID)  or are a reference to the
global stack. We want to dereference to keep the reference chains short,
but we must not create a reference to   the B_VOID 0-variable. This is a
simplified version of linkVal().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(L_VAR, 0, 2, (CA1_FVAR,CA1_VAR))
{ Word v1 = varFrameP(FR, (size_t)*PC++);
  Word v2 = varFrameP(FR, (size_t)*PC++);
  word w = *v2;

  while(isRef(w))
  { v2 = unRef(w);
    if ( needsRef(*v2) )
      break;
    w = *v2;
  }

  *v1 = w;
  NEXT_INSTRUCTION;
}
END_VMI

VMI(L_VOID, 0, 1, (CA1_FVAR))
{ Word v1 = varFrameP(FR, (size_t)*PC++);

  setVar(*v1);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(L_ATOM, 0, 2, (CA1_FVAR,CA1_DATA))
{ Word v1 = varFrameP(FR, (size_t)*PC++);
  word  c = code2atom(*PC++);
  pushVolatileAtom(word2atom(c));
  *v1 = c;
  NEXT_INSTRUCTION;
}
END_VMI

VMI(L_NIL, 0, 1, (CA1_FVAR))
{ Word v1 = varFrameP(FR, (size_t)*PC++);

  *v1 = ATOM_nil;
  NEXT_INSTRUCTION;
}
END_VMI

VMI(L_SMALLINT, 0, 2, (CA1_FVAR,CA1_INTEGER))
{ Word v1 = varFrameP(FR, (size_t)*PC++);
  code  i = *PC++;
  *v1 = consInt((scode)i);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(L_SMALLINTW, 0, 1+CODES_PER_WORD, (CA1_FVAR,CA1_WORD))
{
#if CODES_PER_WORD > 1
  Word v1 = varFrameP(FR, (size_t)*PC++);
  word w;
  PC = code_get_word(PC, &w);
  *v1 = consInt((sword)w);
#else
  assert(0);
#endif
  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actual tail call. The arguments for the   new predicate have been set by
now. The number of arguments may be  both   more  and  less than for the
running frame. As we may need to perform   callbacks  this is a bit of a
problem.

  - If we perform the callbacks giving the current GC context we go
    wrong if the new predicate has more arguments than the current
    clause has `prolog_vars`.
  - If we first change the context to the new predicate we can perform
    the callbacks, but we still need to fix the calling context.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_LCALL, 0, 1, (CA1_LPROC))
{ Procedure proc = code2ptr(Procedure, *PC++);
  Module ctx0 = contextModule(FR);

  leaveDefinition(DEF);
  FR->clause = NULL;
  DEF = proc->definition;
  setFramePredicate(FR, DEF);
  lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);

  if ( ison(FR, FR_WATCHED) )
  { SAVE_REGISTERS(QID);
    frameFinished(FR, FINISH_EXIT);
    LOAD_REGISTERS(QID);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  if ( !DEF->impl.any.defined && isoff(DEF, PROC_DEFINED) )
  { SAVE_REGISTERS(QID);
    DEF = getProcDefinedDefinition(DEF);
    LOAD_REGISTERS(QID);
  }

  if ( ison(DEF, P_TRANSPARENT) )
  { FR->context = ctx0;
    FR->level++;
    clear(FR, FR_CLEAR_NEXT);
    set(FR, FR_CONTEXT);
  } else
  { lcoSetNextFrameFlags(FR);
  }
  if ( ison(DEF, HIDE_CHILDS) )
    set(FR, FR_HIDE_CHILDS);

  setFramePredicate(FR, DEF);
  VMH_GOTO(depart_or_retry_continue);
}
END_VMI

VMI(I_TCALL, 0, 0, ())
{ if ( ison(FR, FR_WATCHED) )
  { SAVE_REGISTERS(QID);
    frameFinished(FR, FINISH_EXIT);
    LOAD_REGISTERS(QID);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  tcallSetNextFrameFlags(FR);
  FR->clause = NULL;
  if ( ison(DEF, HIDE_CHILDS) )
    set(FR, FR_HIDE_CHILDS);

  VMH_GOTO(depart_or_retry_continue);
}
END_VMI

		 /*******************************
		 *	      CONTROL		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_DET sets the determinism guard for  this predicate, implying that this
predicate shall succeed deterministically.  It is bound to '$'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_DET, VIF_BREAK, 0, ())
{ set(FR, FR_DETGUARD|FR_DETGUARD_SET);

  VMI_GOTO(I_CUT);
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CUT: !. Task  is to destroy all choicepoints newer  then the current
frame. If  we are  in debug-mode  we create a  new CHP_DEBUG  frame to
provide proper debugger output.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CUT, VIF_BREAK, 0, ())
{ clear(FR, FR_SSU_DET);

  if ( (LocalFrame)BFR <= FR )
    NEXT_INSTRUCTION;

#ifdef O_DEBUGGER
  if ( debugstatus.debugging )
  { int action;

    SAVE_REGISTERS(QID);
    action = tracePort(FR, BFR, CUT_CALL_PORT, PC);
    LOAD_REGISTERS(QID);
    VMH_GOTO(debug_cut_call_continue, action);
  } else
#endif
  { SAVE_REGISTERS(QID);
    discardChoicesAfter(FR, FINISH_CUT);
    LOAD_REGISTERS(QID);
    lTop = (LocalFrame) argFrameP(FR, CL->value.clause->variables);
    ARGP = argFrameP(lTop, 0);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_JMP skips the amount stated in the pointed argument. The PC++ could be
compiled out, but this is a bit more neath.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_JMP, 0, 1, (CA1_JUMP))
{ PC += *PC;
  PC++;

  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_OR: Create choice-point in the clause.  Argument is the amount to skip
if the choice-point needs to be activated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_OR, 0, 1, (CA1_JUMP))
{ size_t skip = *PC++;
  Choice ch;

  if ( unlikely(!hasLocalSpace(sizeof(struct choice))) )
  { int rc;

    SAVE_REGISTERS(QID);
    rc = growLocalSpace(sizeof(*ch), ALLOW_SHIFT);
    LOAD_REGISTERS(QID);
    if ( rc != true )
    { raiseStackOverflow(rc);
      THROW_EXCEPTION;
    }
  }

  ch = newChoice(CHP_JUMP, FR);
  ch->value.pc = PC+skip;
  ARGP = argFrameP(lTop, 0);

  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_IFTHEN saves the value of BFR (current   backtrack frame) into a local
frame slot reserved by the compiler. Note  that the variable to hold the
local-frame pointer is  *not*  reserved   in  clause->variables,  so the
garbage collector won't see it. We use  a term-reference because using a
relative address simplifies the stack-shifter.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_SOFTIFTHEN, 0, 1, (CA1_CHP))
{ SEPARATE_VMI1;
  VMI_GOTO(C_IFTHEN);
}
END_VMI

VMI(C_IFTHEN, 0, 1, (CA1_CHP))
{ varFrame(FR, *PC++) = consTermRef(BFR);

  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_DET  is  like  C_NOT,  starting  a  block   that  is  asserted  to  be
deterministic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_DET, 0, 2, (CA1_CHP,CA1_JUMP))
{ SEPARATE_VMI1;
  VMI_GOTO(C_IFTHENELSE);
}
END_VMI

VMI(C_DETTRUE, 0, 1, (CA1_CHP))
{ Choice och = (Choice) valTermRef(varFrame(FR, *PC));
  Choice ch = BFR;

  if ( ch->parent == och )
  { DEBUG(0, assert(ch->type == CHP_JUMP));
    BFR = och;
    DEBUG(0, assert(PC[1] == encode(C_JMP)));
    PC += PC[2];
    PC += 3;
    NEXT_INSTRUCTION;
  }

  SAVE_REGISTERS(QID);
  det_goal_error(FR, PC-1, ATOM_nondet);
  LOAD_REGISTERS(QID);
  if ( exception_term )
    THROW_EXCEPTION;

  VMI_GOTO(C_CUT);
}
END_VMI

VMI(C_DETFALSE, 0, 0, ())
{ SAVE_REGISTERS(QID);
  det_goal_error(FR, PC-1, ATOM_fail);
  LOAD_REGISTERS(QID);
  if ( exception_term )
    THROW_EXCEPTION;

  BODY_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_IFTHENELSE: contraction of C_IFTHEN and C_OR.  This contraction has been
made to help the decompiler distinguis between (a ; b) -> c and a -> b ;
c,  which  would  otherwise  only  be    possible  to  distinguis  using
look-ahead.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_NOT, 0, 2, (CA1_CHP,CA1_JUMP))
{ SEPARATE_VMI2;
  VMI_GOTO(C_IFTHENELSE);
}
END_VMI


VMI(C_IFTHENELSE, 0, 2, (CA1_CHP,CA1_JUMP))
{ varFrame(FR, *PC++) = consTermRef(BFR); /* == C_IFTHEN */

  VMI_GOTO(C_OR);
}
END_VMI

VMI(C_FASTCOND, 0, 2, (CA1_CHP,CA1_JUMP))
{ size_t skip;

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
    VMI_GOTO(C_IFTHENELSE);
#endif

  varFrame(FR, *PC++) = consTermRef(BFR); /* == C_IFTHEN */
  skip = *PC++;
  LD->fast_condition = PC+skip;
  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Commit the fast cut. This is normally   a  no-op, unless C_FASTCOND or a
D_BREAK  turned  this  into  a  normal    choice  point.  In  that  case
LD->fast_condition is NULL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_FASTCUT, 0, 1, (CA1_CHP))
{ if ( LD->fast_condition == NULL )
    VMI_GOTO(C_CUT);

  PC++;
  LD->fast_condition = NULL;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_VAR is generated by the compiler to ensure the  instantiation  pattern
of  the  variables  is  the  same after finishing both paths of the `or'
wired in the clause.  Its task is to make the n-th variable slot of  the
current frame to be a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_VAR, 0, 1, (CA1_FVAR))
{ setVar(varFrame(FR, *PC++));

  NEXT_INSTRUCTION;
}
END_VMI


VMI(C_VAR_N, 0, 2, (CA1_FVAR,CA1_INTEGER))
{ Word vp = varFrameP(FR, *PC++);
  size_t count = *PC++;

  while(count--)
    setVar(*vp++);

  NEXT_INSTRUCTION;
}
END_VMI


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
must cut its parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_LSCUT, 0, 1, (CA1_CHP))
{ word chref = varFrame(FR, *PC++);
  Choice ch;

  if ( (intptr_t)chref < 0 )
    chref = (word)-(intptr_t)chref;

  ch = (Choice)valTermRef(chref);
  VMH_GOTO(c_lcut_cont, ch->parent);
}
END_VMI

VMI(C_LCUT, 0, 1, (CA1_CHP))
{ VMH_GOTO(c_lcut_cont, (Choice) valTermRef(varFrame(FR, *PC++)));
}
END_VMI

VMH(c_lcut_cont, 1, (Choice), (och))
{ Choice ch;

  for(ch=BFR; ch; ch = ch->parent)
  { if ( ch->parent == och )
    { VMH_GOTO(c_cut, ch);
    }
  }
  assert(BFR == och);			/* no choicepoint yet */
  NEXT_INSTRUCTION;
}
END_VMH


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CUTCHP cuts all  choice-points  after   the  specified  argument. This
instruction is generated for $cut(Var), used by prolog_cut_to(Choice).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CUTCHP, 0, 0, ())
{ Word a = argFrameP(FR, 0);
  Choice och;

  deRef(a);
  if ( isTaggedInt(*a) )
  { sword i = valInt(*a);
    och = ((Choice)((Word)lBase + i));

    if ( !existingChoice(och) )
    { PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_choice, consTermRef(a));
      THROW_EXCEPTION;
    }

    DEBUG(MSG_CUT, Sdprintf("prolog_cut_to/1 for %s on [%d] %s\n",
			    chp_chars(och),
			    levelFrame(FR), predicateName(FR->predicate)));

    VMH_GOTO(c_cut, och);
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_choice, consTermRef(a));
    THROW_EXCEPTION;
  }
}
END_VMI


VMI(C_SCUT, 0, 0, ())
{ NEXT_INSTRUCTION;
}
END_VMI

VMI(C_LCUTIFTHEN, 0, 1, (CA1_CHP))
{ SEPARATE_VMI1;
  VMI_GOTO(C_CUT);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code deals with ->, *-> and prolog_cut_to/1 (I_CUTCHP)

(*) Normally, committing destroys frames created   after  a choice point
that is related to the running frame. In that case we can simply destroy
all frames that are  more  recent  that   the  choice  point.  With  the
ancestral however, it is possible that  we   must  keep  frames. We must
destroy all frames that are not  reachable   by  the continuation of the
current frame, nor one of the older choice points.

Using in_continuation() seems a bit slower, but the test is rather quick
and avoids some decisions as well. Timing   on a few syntatic cases show
no significant performance difference and even   a  small gain using the
implementation below.

TBD: Merge with dbgDiscardChoicesAfter()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_CUT, 0, 1, (CA1_CHP))
{ Choice och = (Choice) valTermRef(varFrame(FR, *PC));
  PC++;					/* cannot be in macro! */
  VMH_GOTO(c_cut, och);
}
END_VMI

VMH(c_cut, 1, (Choice), (och))
{ Choice ch;

  assert(BFR>=och);
  for(ch=BFR; ch > och; ch = ch->parent)
  { LocalFrame fr2;

    DEBUG(MSG_CUT, Sdprintf("Discarding choice %s for %s\n",
			    chp_chars(ch), predicateName(ch->frame->predicate)));

    for(fr2 = ch->frame;
	!in_continuation(fr2, FR, ch->parent);	/* See (*) */
	fr2 = fr2->parent)
    { DEBUG(MSG_CUT, Sdprintf("Discarding [%d] %s\n",
			      levelFrame(fr2), predicateName(fr2->predicate)));

      assert(fr2->clause || ison(fr2->predicate, P_FOREIGN));

      if ( ison(fr2, FR_WATCHED) )
      { char *lSave = (char*)lBase;

	BFR = ch;
	SAVE_REGISTERS(QID);
	frameFinished(fr2, FINISH_CUT);
	LOAD_REGISTERS(QID);
	if ( lSave != (char*)lBase )	/* shifted */
	{ intptr_t offset = (char*)lBase - lSave;

	  fr2 = addPointer(fr2, offset);
	  ch  = addPointer(ch,  offset);
	  assert(ch == BFR);
	  och = addPointer(och, offset);
	}
	if ( exception_term )
	  THROW_EXCEPTION;
      }

      discardFrame(fr2);
      killFrame(fr2);				/* kill for markPredicates() */
    }

    if ( ch->parent == och )
      DiscardMark(ch->mark);
  }
  assert(och == ch);
  BFR = ch;

  if ( (void *)och > (void *)FR )
  { lTop = (LocalFrame)(och+1);
  } else
  { int nvar = (ison(FR->predicate, P_FOREIGN)
			? (int)FR->predicate->functor->arity
			: FR->clause->value.clause->variables);
    lTop = (LocalFrame) argFrameP(FR, nvar);
  }

  ARGP = argFrameP(lTop, 0);

  DEBUG(MSG_CUT, Sdprintf(" --> BFR = #%ld, lTop = #%zd\n",
			  loffset(BFR), loffset(lTop)));
  NEXT_INSTRUCTION;
}
END_VMH


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_SOFTIF: A *-> B ; C is translated to C_SOFIF <A> C_SOFTCUT <B> C_JMP
end <C>.

C_SOFTIF <choice-var> <skip> is  the  same   as  C_OR  <skip>, storing a
reference to the choice-point in <choice-var>

See pl-comp.c and C_SOFTCUT implementation for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
VMI(C_SOFTIF, 0, 2, (CA1_CHP,CA1_JUMP))
{ varFrame(FR, *PC++) = consTermRef(lTop);	/* see C_SOFTCUT */

  DEBUG(MSG_CUT, Sdprintf("Creating *-> choice at %p (%zd)\n",
			  lTop, loffset(lTop)));
  VMI_GOTO(C_OR);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_SOFTCUT: Handle the commit-to of A *->   B;  C. We must invalidate the
choice point, but note that C_SOFTCUT is  executed multiple times if the
condition succeeds multiple times. Also, C_LSCUT   must find this choice
point to know how far to cut,  so   we  cannot  simply delete the choice
point. Instead, a positive term reference implies we did not yet execute
the soft cut. In this case we  delete   the  choice  and make the slot a
negative term reference to the parent.   A second execution of C_SOFTCUT
with a negative term reference is simply ignored.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_SOFTCUT, 0, 1, (CA1_CHP))
{ Word chref = varFrameP(FR, *PC++);

  if ( (intptr_t)*chref > 0 )
  { Choice ch = (Choice)valTermRef((term_t)*chref);
    Choice bfr = BFR;

    DEBUG(MSG_SOFTCUT,
	  Sdprintf("Killing choice %s from %s\n",
		   chp_chars(ch), print_addr(chref, NULL)));

    if ( bfr == ch )
    { BFR = bfr->parent;
      *chref = (word)-(intptr_t)consTermRef(BFR);
    } else
    { for(; bfr >= ch; bfr=bfr->parent)
      { if ( bfr->parent == ch )
	{ bfr->parent = ch->parent;
	  *chref = (word)-(intptr_t)consTermRef(ch->parent);
	  break;
	}
      }
    }
  } else
  { DEBUG(MSG_SOFTCUT,
	  Sdprintf("Already killed at %s\n", print_addr(chref, NULL)));
  }

  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_END is a dummy instruction to help the decompiler to find the end of A
-> B. (Note that a :- (b ->  c),  d  ==   a  :-  (b  ->  c, d) as far as
semantics. They are different terms however.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_END, 0, 0, ())
{ NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C_FAIL is equivalent to fail/0. Used to implement \+/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(C_FAIL, 0, 0, ())
{ BODY_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_FAIL: Translation of fail/0. Same as C_FAIL, but we make a normal call
when in debug-mode, so we can trace the call.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_FAIL, VIF_BREAK, 0, ())
{
#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { NFR = lTop;
    setNextFrameFlags(NFR, FR);
    DEF = lookupDefinition(FUNCTOR_fail0, MODULE_system);

    VMH_GOTO(normal_call);
  }
#endif
  BODY_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_TRUE: Translation of true/0.  See also I_FAIL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_TRUE, VIF_BREAK, 0, ())
{
#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { NFR = lTop;
    setNextFrameFlags(NFR, FR);
    DEF = lookupDefinition(FUNCTOR_true0, MODULE_system);

    VMH_GOTO(normal_call);
  }
#endif
  NEXT_INSTRUCTION;
}
END_VMI



/** var(@Term)
*/

VMI(I_VAR, VIF_BREAK, 1, (CA1_VAR))
{ Word p = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { VMH_GOTO(debug_pred1, FUNCTOR_var1, p);
  }
#endif

  deRef(p);
  if ( canBind(*p) )
    NEXT_INSTRUCTION;
  FASTCOND_FAILED;
}
END_VMI

VMH(debug_pred1, 2, (functor_t, Word), (fpred, p))
{ if ( isVar(*p) )
  { ENSURE_GLOBAL_SPACE(1, p = varFrameP(FR, (int)PC[-1]));
    globaliseVar(p);
  }

  NFR = lTop;
  setNextFrameFlags(NFR, FR);
  DEF  = lookupDefinition(fpred, MODULE_system);
  ARGP = argFrameP(NFR, 0);
  *ARGP++ = *p;

  VMH_GOTO(normal_call);
}
END_VMH

/** nonvar(@Term)
*/

VMI(I_NONVAR, VIF_BREAK, 1, (CA1_VAR))
{ Word p = varFrameP(FR, (int)*PC++);

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { VMH_GOTO(debug_pred1, FUNCTOR_nonvar1, p);
  }
#endif

  deRef(p);
  if ( !canBind(*p) )
    NEXT_INSTRUCTION;
  FASTCOND_FAILED;
}
END_VMI

/** integer(@Term), atom(@Term), etc.
*/

#ifndef O_DEBUGGER
#define TYPE_TEST(functor, test)           \
	Word p = varFrameP(FR, (int)*PC++);\
	deRef(p);                          \
	if ( test(*p) )			   \
	  NEXT_INSTRUCTION;                \
	FASTCOND_FAILED;
#else
#define TYPE_TEST(functor, test)		\
	Word p = varFrameP(FR, (int)*PC++);	\
	if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )	\
	{ VMH_GOTO(debug_pred1, functor, p);	\
	}					\
	deRef(p);				\
	if ( test(*p) )				\
	  NEXT_INSTRUCTION;			\
	FASTCOND_FAILED;
#endif

VMI(I_INTEGER, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_integer1, isInteger);
}
END_VMI

VMI(I_RATIONAL, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_rational1, isRational);
}
END_VMI

VMI(I_FLOAT, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_float1, isFloat);
}
END_VMI

VMI(I_NUMBER, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_number1, isNumber);
}
END_VMI

VMI(I_ATOMIC, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_atomic1, isAtomic);
}
END_VMI

VMI(I_ATOM, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_atom1, isTextAtom);
}
END_VMI

VMI(I_STRING, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_string1, isString);
}
END_VMI

VMI(I_COMPOUND, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_compound1, isTerm);
}
END_VMI

VMI(I_CALLABLE, VIF_BREAK, 1, (CA1_VAR))
{ TYPE_TEST(FUNCTOR_callable1, isCallable);
}
END_VMI


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

  if ( !DEF->impl.any.defined && isoff(DEF, PROC_DEFINED) )
  { SAVE_REGISTERS(QID);
    DEF = getProcDefinedDefinition(DEF);
    LOAD_REGISTERS(QID);

    setFramePredicate(FR, DEF);
    setGenerationFrame(FR);
#ifdef O_PROFILE
    if ( FR->prof_node )
      profSetHandle(FR->prof_node, DEF);
#endif
    if ( DEF->impl.any.defined )
      VMH_GOTO(depart_or_retry_continue);
#ifdef O_PLMT
  } else if ( ison(DEF, P_THREAD_LOCAL) )
  { DEF = getLocalProcDefinition(DEF);
    setFramePredicate(FR, DEF);
    setGenerationFrame(FR);
#endif
  }

  if ( setDefaultSupervisor(DEF) )
  { PC = DEF->codes;
    NEXT_INSTRUCTION;
  } else				/* TBD: temporary */
  { assert(0);
    FRAME_FAILED;			/* avoid compiler warning */
  }
}
END_VMI


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
      newChoice(CHP_DEBUG, FR);

      SAVE_REGISTERS(QID);
      if ( (fid = PL_open_foreign_frame()) )
      { PL_error(NULL, 0, NULL, ERR_UNDEFINED_PROC, DEF, caller);
	PL_close_foreign_frame(fid);
      }
      LOAD_REGISTERS(QID);

      enterDefinition(DEF);		/* will be left in exception code */

      THROW_EXCEPTION;
    }
    case UNKNOWN_WARNING:
    { fid_t fid;

      lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
      SAVE_REGISTERS(QID);
      if ( (fid = PL_open_foreign_frame()) )
      { term_t pred;
	int rc;

	rc = ( (pred=PL_new_term_ref()) &&
	       unify_definition(MODULE_user, pred, DEF, 0, GP_NAMEARITY) &&
	       printMessage(ATOM_warning,
			    PL_FUNCTOR, FUNCTOR_error2,
			      PL_FUNCTOR, FUNCTOR_existence_error2,
				PL_ATOM, ATOM_procedure,
				PL_TERM, pred,
			      PL_VARIABLE) );
	(void)rc;			/* checking for exception term below */
	PL_close_foreign_frame(fid);
      }
      if ( exception_term )
	THROW_EXCEPTION;
      /*FALLTHROUGH*/
    }
    case UNKNOWN_FAIL:
    default:
      if ( debugstatus.debugging )
	newChoice(CHP_DEBUG, FR);
      FRAME_FAILED;
  }
}
END_VMI


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

  if ( !(cl = firstClause(ARGP, FR, DEF, &chp)) )
  { DEBUG(9, Sdprintf("No clause matching index.\n"));
    if ( debugstatus.debugging ||
	 ison(FR, FR_SSU_DET|FR_DET|FR_DETGUARD) )
      newChoice(CHP_DEBUG, FR);

    FRAME_FAILED;
  }
  DEBUG(9, Sdprintf("Clauses found.\n"));

  PC = cl->value.clause->codes;
  ENSURE_LOCAL_SPACE(LOCAL_MARGIN+cl->value.clause->variables*sizeof(word),
		     THROW_EXCEPTION);
  lTop = (LocalFrame)(ARGP + cl->value.clause->variables);
  CL = cl;

  if ( chp.cref )
  { Choice ch = newChoice(CHP_CLAUSE, FR);
    ch->value.clause = chp;
  } else if ( debugstatus.debugging )
    newChoice(CHP_DEBUG, FR);

  DEBUG(CHK_SECURE,
	{ int argc; int n;
	  argc = DEF->functor->arity;
	  for(n=0; n<argc; n++)
	    checkData(argFrameP(FR, n));
	});

  UMODE = uread;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_DYNAMIC: Dynamic predicate. Dynamic predicates   must  use the dynamic
indexing and need to lock the predicate. This VMI can also handle static
code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_DYNAMIC, 0, 0, ())
{ enterDefinition(DEF);

  SEPARATE_VMI1;
  VMI_GOTO(S_STATIC);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_THREAD_LOCAL: Get thread-local definition
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_THREAD_LOCAL, 0, 0, ())
{ DEF = getLocalProcDefinition(DEF);
  setFramePredicate(FR, DEF);
  setGenerationFrame(FR);

  assert(DEF->codes);
  PC = DEF->codes;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Supervisor for an  incremental  dynamic  predicate.   Must  call  the  C
equivalent   of   '$idg_add_dyncall'/1.   We   must    create   a   term
M:<functor>(var  ...)  for  abstract  dynamic  or  M:<functor>(arg  ...)
otherwise.

TBD: If we really want  to  go   for  performance  we could consider not
creating the term and follow the trie   nodes  directly. Most likely not
worth the trouble.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_INCR_DYNAMIC, 0, 0, ())
{ enterDefinition(DEF);
  atom_t current = word2atom(*valTermRef(LD->tabling.idg_current));
  trie *ctrie;

  if ( current &&
       (ctrie = symbol_trie(current)) &&
       ctrie->data.IDG )
  { fid_t fid;

    lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
    SAVE_REGISTERS(QID);
    if ( (fid = PL_open_foreign_frame()) )
    { Word ap;
      word w;
      term_t t = PL_new_term_ref();

      if ( !(ap = allocGlobal(4+DEF->functor->arity)) )
	THROW_EXCEPTION;

      w = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
      ap[0] = FUNCTOR_colon2;
      ap[1] = DEF->module->name;
      ap[2] = consPtr(&ap[3], TAG_COMPOUND|STG_GLOBAL);
      ap[3] = DEF->functor->functor;
      ap += 4;

      if ( DEF->tabling && DEF->tabling->abstract != (size_t)-1 )
      { size_t i;

	if ( DEF->tabling->abstract != 0 )
	  Sdprintf("%% WARNING: Only abstract(0) is supported\n");
	for(i=0; i<DEF->functor->arity; i++)
	  setVar(ap[i]);
      } else
      { size_t i;
	Word fp;

	LOAD_REGISTERS(QID);
	SAVE_REGISTERS(QID);
	fp = argFrameP(FR, 0);

	for(i=0; i<DEF->functor->arity; i++, fp++)
	{ Word fa;

	  deRef2(fp, fa);
	  if ( isVar(*fa) && onStack(local, fa) )
	  { setVar(ap[i]);
	    LTrail(fa);
	    *fa = makeRefG(&ap[i]);
	  } else
	  { ap[i] = needsRef(*fa) ? makeRefG(fa) : *fa;
	  }
	}
      }

      *valTermRef(t) = w;

      idg_add_dyncall(DEF, ctrie, t);
      PL_close_foreign_frame(fid);
    }
    LOAD_REGISTERS(QID);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  VMI_GOTO(S_STATIC);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_WRAP: Call a wrapped predicate from a closure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_WRAP, 0, 0, ())
{ Code codes = DEF->impl.wrapped.supervisor;

  if ( codes[0] == encode(S_VIRGIN) )
  { PL_LOCK(L_PREDICATE);
    codes = createSupervisor(DEF->impl.wrapped.predicate);
    MEMORY_BARRIER();
    DEF->impl.wrapped.supervisor = codes;
    PL_UNLOCK(L_PREDICATE);
  }

  DEF = DEF->impl.wrapped.predicate;
  setFramePredicate(FR, DEF);
  setGenerationFrame(FR);

  PC = codes;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_MULTIFILE: Multifile predicate.  These need to be aware of new
clauses that can be added at runtime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_MULTIFILE, 0, 0, ())
{ SEPARATE_VMI2;
  VMI_GOTO(S_STATIC);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_TRUSTME: Trust this clause. Generated   for  single-clause supervisors
and for the last one of a disjunction.

TBD: get rid of clause-references
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_TRUSTME, 0, 1, (CA1_CLAUSEREF))
{ ClauseRef cref = code2ptr(ClauseRef, *PC++);

  ARGP = argFrameP(FR, 0);
  TRUST_CLAUSE(cref);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_CALLWRAPPER: Trust the wrapper clause.  The implementation is the same
as S_TRUSTME, but we use a different instruction to find the wrapper and
add additional information.  Arguments:

  - 0: The clause to call (in a wrapper predicate $wrap$P)
  - 1: The closure blob
  - 2: Name of the wrapper (an atom)

Note that we actually execute the  wrapper   predicate  and  we must set
`DEF` and `FR->predicate` to the wrapper predicate to make sure GC works
properly.  If  not,  wrapping   a    foreign   predicate   will  confuse
setStartOfVMI().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_CALLWRAPPER, 0, 3, (CA1_CLAUSEREF,CA1_DATA,CA1_DATA))
{ ClauseRef cref = code2ptr(ClauseRef, *PC);

  PC += 3;
  ARGP = argFrameP(FR, 0);
  FR->predicate = DEF = cref->value.clause->predicate;
  TRUST_CLAUSE(cref);
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
S_LIST(ArgN, NilClause, ListClause):
Predicate consisting of two clauses, one of them using [] and
the other [_|_] for argument ArgN (0-based)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_LIST, 0, 3, (CA1_INTEGER, CA1_CLAUSEREF, CA1_CLAUSEREF))
{ ClauseRef cref;
  Word k;

  ARGP = argFrameP(FR, 0);
  deRef2(ARGP+PC[0], k);
  if ( isList(*k) )
    cref = code2ptr(ClauseRef, PC[2]);
  else if ( isNil(*k) )
    cref = code2ptr(ClauseRef, PC[1]);
  else if ( canBind(*k) )
  { PC = SUPERVISOR(staticp) + 1;
    VMI_GOTO(S_STATIC);
  } else
    FRAME_FAILED;

  PC += 3;

  TRUST_CLAUSE(cref);
}
END_VMI


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

  SAVE_REGISTERS(QID);
  rc = m_qualify_argument(FR, arg);
  LOAD_REGISTERS(QID);
  if ( rc != true )
  { if ( rc != false )
      raiseStackOverflow(rc);
    THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;
}
END_VMI


VMI(S_LMQUAL, 0, 1, (CA1_VAR))
{ int arg = (int)*PC++;
  int rc;

  SAVE_REGISTERS(QID);
  rc = m_qualify_argument(FR, arg);
  LOAD_REGISTERS(QID);
  if ( rc != true )
  { if ( rc != false )
      raiseStackOverflow(rc);
    THROW_EXCEPTION;
  }
  setContextModule(FR, FR->predicate->module);

  NEXT_INSTRUCTION;
}
END_VMI


VMI(S_SSU_DET, 0, 0, ())
{ set(FR, FR_SSU_DET);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(S_DET, 0, 0, ())
{ set(FR, FR_DET);
  NEXT_INSTRUCTION;
}
END_VMI

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

#define AR_THROW_EXCEPTION		\
	do { resetArithStack(); \
	     AR_CLEANUP();		\
	     THROW_EXCEPTION;		\
	   } while(0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_ENTER: Prepare for arithmetic operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_ENTER, 0, 0, ())
{ memset(&__PL_ar_ctx, 0, sizeof(__PL_ar_ctx));
  __PL_ar_ctx.alloc_buf = __PL_ar_buf;
  AR_BEGIN();
  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_INTEGER: Push long integer following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_INTEGER, 0, 1, (CA1_INTEGER))
{ Number n = allocArithStack();

  n->value.i = (scode) *PC++;
  n->type    = V_INTEGER;
  NEXT_INSTRUCTION;
}
END_VMI

VMI(A_INTEGERW, 0, CODES_PER_WORD, (CA1_WORD))
{
#if CODES_PER_WORD > 1
  Number n = allocArithStack();

  word w;
  PC = code_get_word(PC,&w);
  n->value.i = (sword)w;
  n->type    = V_INTEGER;
#else
  assert(0);
#endif
  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_MPZ: Push mpz integer following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{
#ifdef O_BIGNUM
  Number n = allocArithStack();

  n->type = V_MPZ;
  PC = get_mpz_from_code(PC, n->value.mpz);
#endif
  NEXT_INSTRUCTION;
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_MPQ: Push mpq integer following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_MPQ, 0, VM_DYNARGC, (CA1_MPQ))
{
#ifdef O_BIGNUM
  Number n = allocArithStack();

  n->type = V_MPQ;
  PC = get_mpq_from_code(PC, n->value.mpq);
#endif
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_DOUBLE: Push double following PC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_DOUBLE, 0, CODES_PER_DOUBLE, (CA1_FLOAT))
{ Number n = allocArithStack();
  Word p = &n->value.w[0];

  cpDoubleData(p, PC);
  n->type       = V_FLOAT;
  NEXT_INSTRUCTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_VAR: Push a variable.  This can be any term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


VMI(A_VAR, 0, 1, (CA1_VAR))
{ VMH_GOTO(a_var_n, (int)*PC++);
}
END_VMI

VMH(a_var_n, 1, (int), (offset))
{ Number n;
  Word p, p2;
  p = varFrameP(FR, offset);
  deRef2(p, p2);

					/* speedup common case a bit */
  if ( tagex(*p2) == (TAG_INTEGER|STG_INLINE) )
  { n = allocArithStack();
    n->value.i = valInt(*p2);
    n->type = V_INTEGER;
    NEXT_INSTRUCTION;
    /*NOTREACHED*/
  }

  switch(tag(*p2))
  { case TAG_INTEGER:
      n = allocArithStack();
      get_rational(*p2, n);
      NEXT_INSTRUCTION;
    case TAG_FLOAT:
      n = allocArithStack();
      n->value.f = valFloat(*p2);
      n->type = V_FLOAT;
      NEXT_INSTRUCTION;
    default:
    { intptr_t lsafe = (char*)lTop - (char*)lBase;
      fid_t fid;
      number result;
      int rc;

      SAVE_REGISTERS(QID);
      lTop = (LocalFrame)argFrameP(lTop, 1); /* for is/2.  See below */
      if ( (fid = PL_open_foreign_frame()) )
      { rc = evalExpression(consTermRef(p), &result);
	PL_close_foreign_frame(fid);
      } else
      { rc = false;
      }
      lTop = addPointer(lBase, lsafe);
      LOAD_REGISTERS(QID);

      if ( rc )
      { pushArithStack(&result);
	NEXT_INSTRUCTION;
      } else
      { AR_THROW_EXCEPTION;
      }
    }
  }
}
END_VMH

VMI(A_VAR0, 0, 0, ())
{ VMH_GOTO(a_var_n, VAROFFSET(0));
}
END_VMI

VMI(A_VAR1, 0, 0, ())
{ VMH_GOTO(a_var_n, VAROFFSET(1));
}
END_VMI

VMI(A_VAR2, 0, 0, ())
{ VMH_GOTO(a_var_n, VAROFFSET(2));
}
END_VMI



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_FUNC, function, #args
A_FUNC0, function
A_FUNC1, function
A_FUNC2, function

TBD: Keep knowledge on #argument in function!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


VMI(A_FUNC0, 0, 1, (CA1_AFUNC))
{ VMH_GOTO(common_an, *PC++, 0);
}
END_VMI

VMI(A_FUNC1, 0, 1, (CA1_AFUNC))
{ VMH_GOTO(common_an, *PC++, 1);
}
END_VMI

VMI(A_FUNC2, 0, 1, (CA1_AFUNC))
{ VMH_GOTO(common_an, *PC++, 2);
}
END_VMI

VMI(A_FUNC, 0, 2, (CA1_AFUNC, CA1_INTEGER))
{ PC += 2;
  VMH_GOTO(common_an, PC[-2], (int)PC[-1]);
}
END_VMI

VMH(common_an, 2, (code, int), (fn, an))
{ int rc;
  SAVE_REGISTERS(QID);
  rc = ar_func_n((int)fn, an);
  LOAD_REGISTERS(QID);
  if ( !rc )
    AR_THROW_EXCEPTION;

  NEXT_INSTRUCTION;
}
END_VMH

VMI(A_ROUNDTOWARDS_A, 0, 1, (CA1_INTEGER))
{ int mode = (int)*PC++;
  Number n = allocArithStack();

  n->value.i = __PL_ar_ctx.femode = fegetround();
  n->type = V_INTEGER;
  set_rounding(mode);

  NEXT_INSTRUCTION;
}
END_VMI


VMI(A_ROUNDTOWARDS_V, 0, 1, (CA1_VAR))
{ Word p = varFrameP(FR, (size_t)*PC++);
  int rm;

  deRef(p);
  if ( isAtom(*p) && atom_to_rounding(word2atom(*p), &rm) )
  { Number n = allocArithStack();

    n->value.i = __PL_ar_ctx.femode = fegetround();
    n->type = V_INTEGER;
    set_rounding(rm);
    NEXT_INSTRUCTION;
  } else
  { resetArithStack();
    THROW_EXCEPTION;
  }
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_ADD: Shorthand for A_FUNC2 pl_ar_add()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_ADD, 0, 0, ())
{ Number argv = argvArithStack(2);
  int rc;
  number r;

  SAVE_REGISTERS(QID);
  rc = pl_ar_add(argv+1, argv, &r);
  LOAD_REGISTERS(QID);
  popArgvArithStack(2);
  if ( rc )
  { pushArithStack(&r);
    NEXT_INSTRUCTION;
  }

  AR_THROW_EXCEPTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_MUL: Shorthand for A_FUNC2 ar_mul()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_MUL, 0, 0, ())
{ Number argv = argvArithStack(2);
  int rc;
  number r;

  SAVE_REGISTERS(QID);
  rc = ar_mul(argv+1, argv, &r);
  LOAD_REGISTERS(QID);
  popArgvArithStack(2);
  if ( rc )
  { pushArithStack(&r);
    NEXT_INSTRUCTION;
  }

  AR_THROW_EXCEPTION;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_ADD_FC: Simple case A is B + <int>, where   A is a firstvar and B is a
normal variable. This case is very   common,  especially with relatively
small integers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_ADD_FC, VIF_BREAK, 3, (CA1_FVAR, CA1_VAR, CA1_INTEGER))
{ Word rp  = varFrameP(FR, *PC++);	/* A = */
  Word np  = varFrameP(FR, *PC++);	/* B + */
  intptr_t add = (intptr_t)*PC++;	/* <int> */

  deRef(np);

#ifdef O_DEBUGGER
  if ( unlikely(!truePrologFlag(PLFLAG_VMI_BUILTIN)) )
  { Word expr;

    ENSURE_GLOBAL_SPACE(4,
			{ np = varFrameP(FR, PC[-2]);
			  rp = varFrameP(FR, PC[-3]);
			});
    expr = gTop;
    gTop += 3;
    expr[0] = FUNCTOR_plus2;
    bArgVar(&expr[1], np);
    expr[2] = consInt(add);

    ARGP = argFrameP(lTop, 0);
    globaliseFirstVar(rp);
    *ARGP++ = *rp;
    *ARGP++ = consPtr(expr, TAG_COMPOUND|STG_GLOBAL);
    NFR = lTop;
    DEF = GD->procedures.is2->definition;
    setNextFrameFlags(NFR, FR);
    VMH_GOTO(normal_call);
  }
#endif

  if ( isTaggedInt(*np) )
  { sword v = valInt(*np);
    int64_t r = v+add;			/* tagged ints never overflow */
    word w = consInt(r);

    if ( valInt(w) == r )
    { *rp = w;
    } else				/* but their sum might not fit */
    { int rc;

      SAVE_REGISTERS(QID);
      rc = put_int64(&w, r, ALLOW_GC|ALLOW_SHIFT);
      LOAD_REGISTERS(QID);
      if ( rc != true )
	THROW_EXCEPTION;
      *rp = w;
    }
    NEXT_INSTRUCTION;
  } else
  { number n;
    word w;
    fid_t fid;
    int rc;

    SAVE_REGISTERS(QID);
    if ( (fid = PL_open_foreign_frame()) )	/* Still needed? */
    { rc = evalExpression(pushWordAsTermRef(np), &n);
      popTermRef();
      if ( rc )
      { ensureWritableNumber(&n);
	if ( (rc=ar_add_si(&n, add)) )
	{ if ( (rc=put_number(&w, &n, ALLOW_GC)) != true )
	    rc = raiseStackOverflow(rc);
	}
	clearNumber(&n);
      }
      PL_close_foreign_frame(fid);
    } else
      rc = false;
    LOAD_REGISTERS(QID);
    if ( !rc )
      THROW_EXCEPTION;

    rp = varFrameP(FR, PC[-3]);		/* may have shifted */
    *rp = w;

    NEXT_INSTRUCTION;
  }
}
END_VMI


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


#define CMP_FAST(op) \
  Number n1 = argvArithStack(2); \
  Number n2 = n1 + 1; \
  if ( n1->type == n2->type ) \
  { switch(n1->type) \
    { case V_INTEGER: \
	VMH_GOTO(a_cmp_out, n1->value.i op n2->value.i); \
      case V_FLOAT: \
	VMH_GOTO(a_cmp_out, n1->value.f op n2->value.f); \
      default: \
	; \
    } \
  }
#define CMP(opname) \
  CMP_FAST(opname ## _C); \
  VMH_GOTO(a_cmp_out, ar_compare(n1, n2, opname))


VMI(A_LT, VIF_BREAK, 0, ())		/* A < B */
{ CMP(LT);
}
END_VMI

VMH(a_cmp_out, 1, (int), (rc))
{ popArgvArithStack(2);
  AR_END();
  if ( rc )
    NEXT_INSTRUCTION;
  FASTCOND_FAILED;
}
END_VMH

VMI(A_LE, VIF_BREAK, 0, ())		/* A =< B */
{ CMP(LE);
}
END_VMI

VMI(A_GT, VIF_BREAK, 0, ())		/* A > B */
{ CMP(GT);
}
END_VMI

VMI(A_GE, VIF_BREAK, 0, ())		/* A >= B */
{ CMP(GE);
}
END_VMI

VMI(A_EQ, VIF_BREAK, 0, ())		/* A =:= B */
{ CMP(EQ);
}
END_VMI

VMI(A_NE, VIF_BREAK, 0, ())		/* A \=:= B */
{ CMP(NE);
}
END_VMI


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
{ Number n = argvArithStack(1);
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
      { rc = true;
	bindConst(k, c);
	goto a_is_ok;
      }
    }

    ARGP++;				/* new_args must become 1 in */
    SAVE_REGISTERS(QID);		/* get_vmi_state() */
    rc = put_number(&c, n, ALLOW_GC);
    LOAD_REGISTERS(QID);
    ARGP--;

    if ( rc == true )
    { deRef2(ARGP, k);			/* may be shifted */
      if ( !isTerm(c) )
      { bindConst(k, c);
      } else
      { SAVE_REGISTERS(QID);
	rc = unify_ptrs(k, &c, ALLOW_GC|ALLOW_SHIFT);
	LOAD_REGISTERS(QID);
	if ( rc == false )
	{ popArgvArithStack(1);
	  AR_END();
	  if ( exception_term )
	    THROW_EXCEPTION;
	  BODY_FAILED;
	}
      }
    } else
    { raiseStackOverflow(rc);
      popArgvArithStack(1);
      AR_END();
      THROW_EXCEPTION;
    }

  a_is_ok:
    popArgvArithStack(1);
    AR_END();

    CHECK_WAKEUP;
    NEXT_INSTRUCTION;
  } else
  { int rc;

    if ( isRational(*k) && ratNumber(n) )
    { number left;

      get_rational(*k, &left);
      rc = (cmpNumbers(&left, n) == CMP_EQUAL);
      clearNumber(&left);
    } else if ( isFloat(*k) && floatNumber(n) )
    { Word ak = valIndirectP(*k);

      rc = memcmp((char*)&n->value.f, ak, sizeof(n->value.f)) == 0;
    } else
    { rc = false;
    }

    popArgvArithStack(1);
    AR_END();
    if ( rc )
      NEXT_INSTRUCTION;
  }

  BODY_FAILED;
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A_FIRSTVAR_IS: Deal with the very common case that the local variable is
a firstvar of the current frame. There   are numerous advantages to this
case: we know the left-side is a var, we  do not need to trail and we do
not need to check for attvar wakeup.

TBD: link with following B_VAR? How  frequent?   Likely  very: we are in
body mode and in many cases the result is used only once.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(A_FIRSTVAR_IS, VIF_BREAK, 1, (CA1_FVAR)) /* A is B */
{ Number n = argvArithStack(1);
  word w;
  int rc;

  SAVE_REGISTERS(QID);
  if ( (rc = put_number(&w, n, ALLOW_GC)) != true )
    rc = raiseStackOverflow(rc);
  LOAD_REGISTERS(QID);

  popArgvArithStack(1);
  AR_END();

  if ( rc )
  { *varFrameP(FR, *PC++) = w;
    NEXT_INSTRUCTION;
  } else
    THROW_EXCEPTION;
}
END_VMI


		 /*******************************
		 *	     FOREIGN		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deterministic foreign  code calls are translated  to "I_FCALLDETVA f",
which  both  inlines  the  old I_FOPEN  and  I_FEXITDET  instructions.
Without the  "VA" calling  conventions, we produce  "I_FCALLDET<N> f",
I_FEXITDET.  The I_FEXITDET  is not used, but avoids  having to repeat
the I_FCALLDET<N> in several switches on instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if 0			/* inlined with I_FCALLDETVA and I_FCALLDET<N> */
			/* NO_VMI to avoid mkvmi to see this */
NO_VMI(I_FOPEN, 0, 0, ())
{ FliFrame ffr = vmi_fopen(FR, DEF);
  FFR_ID = consTermRef(ffr);
  SAVE_REGISTERS(QID);

  NEXT_INSTRUCTION;
}
END_VMI
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_FCALLDETVA:  Call  deterministic  foreign    function  using  P_VARARG
conventions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_FCALLDETVA, 0, 1, (CA1_FOREIGN))
{ typedef foreign_t (*va_func)(term_t av, size_t ac, control_t ctx);
  va_func f = code2ptr(va_func, *PC++);
  term_t h0 = consTermRef(argFrameP(FR, 0));

  vmi_fopen(FR, DEF);		/* inline I_FOPEN */

  FNDET_CONTEXT.control   = FRG_FIRST_CALL;
  FNDET_CONTEXT.context   = 0L;
  FNDET_CONTEXT.predicate = DEF;

  SAVE_REGISTERS(QID);
  VMH_GOTO(I_FEXITDET, (*f)(h0, DEF->functor->arity, &FNDET_CONTEXT));
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_FCALLDET0 .. I_FCALLDET10: Call deterministic   foreign function using
a1, a2, ... calling conventions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_FCALLDET0, 0, 1, (CA1_FOREIGN))
{ Func0 f = code2ptr(Func0, *PC++);

  vmi_fopen(FR, DEF); /* inline I_FOPEN */
  SAVE_REGISTERS(QID);

  VMH_GOTO(I_FEXITDET, (*f)());
}
END_VMI

#define FCALL_DETN(ac, ...)				\
  Func##ac f = code2ptr(Func##ac, *PC++);		\
  term_t h0 = consTermRef(argFrameP(FR, 0));		\
  vmi_fopen(FR, DEF); /* inline I_FOPEN */		\
  PC++;							\
  SAVE_REGISTERS(QID);					\
  VMH_GOTO(I_FEXITDET, (*f)(__VA_ARGS__));

VMI(I_FCALLDET1, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(1, h0);
}
END_VMI


VMI(I_FCALLDET2, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(2, h0, h0+1);
}
END_VMI


VMI(I_FCALLDET3, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(3, h0, h0+1, h0+2);
}
END_VMI


VMI(I_FCALLDET4, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(4, h0, h0+1, h0+2, h0+3);
}
END_VMI


VMI(I_FCALLDET5, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(5, h0, h0+1, h0+2, h0+3, h0+4);
}
END_VMI


VMI(I_FCALLDET6, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(6, h0, h0+1, h0+2, h0+3, h0+4, h0+5);
}
END_VMI


VMI(I_FCALLDET7, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(7, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6);
}
END_VMI


VMI(I_FCALLDET8, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(8, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7);
}
END_VMI


VMI(I_FCALLDET9, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(9, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8);
}
END_VMI


VMI(I_FCALLDET10, 0, 1, (CA1_FOREIGN))
{ FCALL_DETN(10, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8, h0+9);
}
END_VMI


VMI(I_FEXITDET, 0, 0, ())
{ assert(0);
  THROW_EXCEPTION; /* should never happen as we jump to the helper */
}
END_VMI

VMH(I_FEXITDET, 1, (foreign_t), (rc))
{ LOAD_REGISTERS(QID);

  while ( (void*)fli_context > (void*)FR )
    fli_context = fli_context->parent;

  switch(rc)
  { case true:
      if ( exception_term )		/* false alarm */
	PL_clear_foreign_exception(FR);
      VMH_GOTO(exit_checking_wakeup);
    case false:
      if ( exception_term )
	THROW_EXCEPTION;
      FRAME_FAILED;
    default:
      error_foreign_return_code(rc);
      THROW_EXCEPTION;
  }
}
END_VMH

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

VMI(I_FOPENNDET, 0, 0, ())
{ FNDET_CONTEXT.control   = FRG_FIRST_CALL;
  FNDET_CONTEXT.context   = 0L;
  FNDET_CONTEXT.predicate = DEF;

  VMH_GOTO(foreign_redo);
}
END_VMI

VMH(foreign_redo, 0, (), ())
{ Choice ch;
  FliFrame ffr;

  environment_frame = FR;
  lTop = (LocalFrame)argFrameP(FR, DEF->functor->arity);
  ch = newChoice(CHP_JUMP, FR);
  ch->value.pc = PC+3;

  ffr = (FliFrame)(ch+1);
  lTop = (LocalFrame)(ffr+1);
  ffr->size = 0;
  ffr->no_free_before = (size_t)-1;
  NoMark(ffr->mark);
  ffr->parent = fli_context;
  FLI_SET_VALID(ffr);
  fli_context = ffr;
  FFR_ID = consTermRef(ffr);
  SAVE_REGISTERS(QID);

  NEXT_INSTRUCTION;
}
END_VMH

/* Resume after a foreign predicate returned using PL_yield_address()
 * PC is pointing at I_FREDO;
 */

VMH(foreign_resume, 0, (), ())
{ DEBUG(0, assert(*PC == encode(I_FREDO)));
  PC -= 3;
  DEBUG(0, assert(*PC == encode(I_FCALLNDETVA)));

  /* save instead? */
  for(FliFrame ffr=fli_context; ffr; ffr=ffr->parent)
  { if ( (void*)ffr->parent < (void*)FR )
    { FFR_ID = consTermRef(ffr);
      break;
    }
  }

  uintptr_t wcl = (uintptr_t)FR->clause;
  assert((wcl&FRG_REDO_MASK) == YIELD_PTR);
  FNDET_CONTEXT.control = FRG_RESUME;
  FNDET_CONTEXT.context = wcl & ~FRG_REDO_MASK;
  SAVE_REGISTERS(QID);
  NEXT_INSTRUCTION;
}
END_VMH

VMI(I_FCALLNDETVA, 0, 1, (CA1_FOREIGN))
{ typedef foreign_t (*ndet_func)(term_t h0, size_t arity, struct foreign_context*);
  ndet_func f = code2ptr(ndet_func, *PC++);
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;

  VMH_GOTO(I_FEXITNDET, (*f)(h0, DEF->functor->arity, &FNDET_CONTEXT));
}
END_VMI


VMI(I_FCALLNDET0, 0, 1, (CA1_FOREIGN))
{ NdetFunc0 f = code2ptr(NdetFunc0, *PC++);

  VMH_GOTO(I_FEXITNDET, (*f)(&FNDET_CONTEXT));
}
END_VMI

#define FCALL_NDETN(ac, ...)						\
  NdetFunc##ac f = code2ptr(NdetFunc##ac, *PC++);			\
  term_t h0 = argFrameP(FR, 0) - (Word)lBase;				\
  VMH_GOTO(I_FEXITNDET, (*f)(__VA_ARGS__, &FNDET_CONTEXT));


VMI(I_FCALLNDET1, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(1, h0);
}
END_VMI


VMI(I_FCALLNDET2, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(2, h0, h0+1);
}
END_VMI


VMI(I_FCALLNDET3, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(3, h0, h0+1, h0+2);
}
END_VMI


VMI(I_FCALLNDET4, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(4, h0, h0+1, h0+2, h0+3);
}
END_VMI


VMI(I_FCALLNDET5, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(5, h0, h0+1, h0+2, h0+3, h0+4);
}
END_VMI


VMI(I_FCALLNDET6, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(6, h0, h0+1, h0+2, h0+3, h0+4, h0+5);
}
END_VMI


VMI(I_FCALLNDET7, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(7, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6);
}
END_VMI


VMI(I_FCALLNDET8, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(8, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7);
}
END_VMI


VMI(I_FCALLNDET9, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(9, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8);
}
END_VMI


VMI(I_FCALLNDET10, 0, 1, (CA1_FOREIGN))
{ FCALL_NDETN(10, h0, h0+1, h0+2, h0+3, h0+4, h0+5, h0+6, h0+7, h0+8, h0+9);
}
END_VMI


VMI(I_FEXITNDET, 0, 0, ())
{ THROW_EXCEPTION;
}
END_VMI

VMH(I_FEXITNDET, 1, (foreign_t), (rc))
{ FliFrame ffr;

  LOAD_REGISTERS(QID);
  PC += 3;				/* saved at in I_FOPENNDET */
  DEBUG(0, assert(*PC == encode(I_FREDO)));

  switch(rc)
  { case true:
      ffr = (FliFrame) valTermRef(FFR_ID);
      fli_context = ffr->parent;
      if ( exception_term )		/* false alarm */
	PL_clear_foreign_exception(FR);
      DEBUG(CHK_SECURE, assert(BFR->value.pc == PC));
#ifdef O_DEBUGGER
      if ( unlikely(debugstatus.debugging) )
	BFR->type = CHP_DEBUG;
      else
#endif
	BFR = BFR->parent;
      FR->clause = NULL;
      VMH_GOTO(exit_checking_wakeup);
    case false:
      ffr = (FliFrame) valTermRef(FFR_ID);
      fli_context = ffr->parent;
      FR->clause = NULL;
      if ( exception_term )
	THROW_EXCEPTION;
      DEBUG(CHK_SECURE, assert(BFR->value.pc == PC));
#ifdef O_DEBUGGER
      if ( unlikely(debugstatus.debugging) )
	BFR->type = CHP_DEBUG;
      else
#endif
	BFR = BFR->parent;
      FRAME_FAILED;
    default:
    { /* TBD: call debugger */
      if ( exception_term )		/* false alarm */
	PL_clear_foreign_exception(FR);

      CL = word2ptr(ClauseRef, rc);

      if ( (rc&YIELD_PTR) )
      { fid_t fid = PL_open_foreign_frame();

	if ( !fid )
	  THROW_EXCEPTION;

	QF = QueryFromQid(QID);
	if ( !(QF->flags&PL_Q_ALLOW_YIELD) )
	{ PL_error(NULL, 0, "not an async query", ERR_PERMISSION_YIELD);
	  THROW_EXCEPTION;
	}
	SAVE_REGISTERS(QID);
	QF->foreign_frame = fid;
	QF->yield.term = YIELD_TERM_FOREIGN;
	DEBUG(MSG_YIELD, Sdprintf("Foreign yield\n"));
#if !O_VMI_FUNCTIONS
	assert(LD->exception.throw_environment == &THROW_ENV);
	LD->exception.throw_environment = THROW_ENV.parent;
#endif
	SOLUTION_RETURN(PL_S_YIELD);
      } else
      { ffr = (FliFrame) valTermRef(FFR_ID);
	fli_context = ffr->parent;
	lTop = (LocalFrame)(BFR+1);
	VMH_GOTO(exit_checking_wakeup);
      }
    }
  }
}
END_VMH

VMI(I_FREDO, 0, 0, ())
{ DEBUG(0, assert(ison(DEF, P_FOREIGN)));

  if ( unlikely(LD->alerted) && is_signalled() )
  { if ( isoff(DEF, P_SIG_ATOMIC) )
    { SAVE_REGISTERS(QID);
      handleSignals();
      LOAD_REGISTERS(QID);
      if ( exception_term )
	THROW_EXCEPTION;
    }
  }

  uintptr_t wcl = (uintptr_t)FR->clause;
  switch(wcl & FRG_REDO_MASK)
  { case REDO_INT:
      FNDET_CONTEXT.control = FRG_REDO;
      FNDET_CONTEXT.context = (uintptr_t)((intptr_t)(wcl) >> FRG_REDO_BITS);
      break;
    case REDO_PTR:
      FNDET_CONTEXT.control = FRG_REDO;
      FNDET_CONTEXT.context = wcl;
      break;
    default:
      assert(0);
  }

  PC -= 4;				     /* Back to I_FCALL* */
  VMH_GOTO(foreign_redo);
}
END_VMI



		 /*******************************
		 *     EXCEPTIONS & CLEANUP	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLCLEANUP verifies the signature of the   cleanup  handler and calls
the Goal for

  - call_cleanup(Goal, Cleanup)
  - setup_call_cleanup(Setup, Goal, Cleanup)
  - setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup)

Which of these predicates it is working for  is dictated by the arity of
the running predicate.

I_EXITCLEANUP follows I_CALLCLEANUP. If there is no choice-point created
this is the final exit.

This group of predicates calls   '$call_cleanup'/0,  which is translated
into the sequence of these two VM   instructions.  This call must be the
last of the predicate.

We set FR_CLEANUP to get a cleanup call if the frame fails or is cutted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLCLEANUP, 0, 0, ())
{ Word p;
  size_t arity = FR->predicate->functor->arity;
  size_t arg_goal = arity == 2 ? 1 : 2;

		    /* last arg is cleanup */
  if ( !mustBeCallable(consTermRef(argFrameP(FR, arity-1))) )
    THROW_EXCEPTION;

  newChoice(CHP_CATCH, FR);
  set(FR, FR_CLEANUP);

  p = argFrameP(FR, arg_goal-1);
  if ( isVar(*p) )
  { PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    THROW_EXCEPTION;
  }
				/* = B_VAR1 */
  *argFrameP(lTop, 0) = linkValI(p);

  VMI_GOTO(I_CALL1);
}
END_VMI


VMI(I_EXITCLEANUP, 0, 0, ())
{ while( BFR && BFR->type == CHP_DEBUG )
    BFR = BFR->parent;

  if ( BFR->frame == FR && BFR->type == CHP_CATCH )
  { DEBUG(3, Sdprintf(" --> BFR = #%zd\n", loffset(BFR->parent)));
    for(BFR = BFR->parent; BFR > (Choice)FR; BFR = BFR->parent)
    { if ( BFR->type == CHP_DEBUG )
	continue;
      if ( BFR->frame > FR )
	NEXT_INSTRUCTION;		/* choice from setup of setup_call_catcher_cleanup/4 */
    }

    SAVE_REGISTERS(QID);
    frameFinished(FR, FINISH_EXITCLEANUP);
    LOAD_REGISTERS(QID);
    if ( exception_term )
      THROW_EXCEPTION;
  }

  NEXT_INSTRUCTION;			/* goto i_exit? */
}
END_VMI


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

I_EXITCATCH verifies finishes the catch/3 call   if  there are no choice
points created by the guarded goal. This is   the case if the first real
choicepoint is our `CHP_CATCH` choicepoint. If we   are in debug mode we
restore it as a debug choicepoint, else we discard it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CATCH, 0, 0, ())
{ Word p = argFrameP(FR, 0);

  if ( BFR->frame == FR && BFR == (Choice)argFrameP(FR, 3) )
  { assert(BFR->type == CHP_DEBUG);
    BFR->type = CHP_CATCH;
  } else
    newChoice(CHP_CATCH, FR);

  if ( isVar(*p) )
  { PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    THROW_EXCEPTION;
  }
				  /* = B_VAR0 */
  *argFrameP(lTop, 0) = linkValI(p);
  VMI_GOTO(I_CALL1);
}
END_VMI


VMI(I_EXITCATCH, 0, 0, ())
{ Choice bfr = BFR;

  while(bfr && bfr->type == CHP_DEBUG)
    bfr = bfr->parent;

  if ( bfr->frame == FR && bfr == (Choice)argFrameP(FR, 3) )
  { assert(bfr->type == CHP_CATCH);
    if ( debugstatus.debugging )
    { bfr->type = CHP_DEBUG;
      BFR = bfr;
    } else
    { BFR = bfr->parent;
    }
    set(FR, FR_CAUGHT);
  }

  VMI_GOTO(I_EXIT);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The B_THROW code is the implementation for   throw/1.  The call walks up
the stack, looking for a frame running catch/3 on which it can unify the
exception code. It then cuts all  choicepoints created since throw/3. If
throw/3 is not found, it sets  the   query  exception  field and returns
failure. Otherwise, it will simulate an I_CALL1 instruction: it sets
the FR and lTop as if it  was   running  the  throw/3 predicate. Then it
pushes the recovery goal from throw/3 and jumps to I_CALL1.

Note that exceptions are placed on the stack using PL_raise_exception(),
which uses duplicate_term() and  freezeGlobal()   to  make the exception
term immune for undo operations.

This huge  VM instruction is  broken into a  number of VMH  helpers to
make it  easier to track the  dependencies and follow the  flow.  When
using VMI  functions, these are  simple tail calls and  otherwise they
are  `goto`  instructions  that  will   be  removed  by  the  compiler
optimizer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(B_THROW, 0, 0, ())
{ PL_raise_exception(consTermRef(argFrameP(lTop, 0)));
  THROW_EXCEPTION;				/* sets origin */
}
END_VMI

VMH(b_throw, 0, (), ())
{ term_t catchfr_ref;
  Stack outofstack;

  LD->fast_condition = NULL;		/* A_FUNC exceptions */
  QF  = QueryFromQid(QID);
  aTop = QF->aSave;
  assert(exception_term);
  outofstack = LD->outofstack;
  LD->outofstack = NULL;
  fid_t fid;

  if ( lTop < (LocalFrame)argFrameP(FR, FR->predicate->functor->arity) )
    lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);

  DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));
  DEBUG(MSG_THROW,
	{ fid_t fid = PL_open_foreign_frame();
	  Sdprintf("[%d] Throwing (from line %d): ",
		   PL_thread_self(), THROWED_FROM_LINE);
	  PL_write_term(Serror, exception_term, 1200, 0);
	  Sdprintf("\n");
	  PL_discard_foreign_frame(fid);
	});

  if ( has_emergency_space(&LD->stacks.local, sizeof(struct localFrame)) )
  { fid = open_foreign_frame();
  } else		/* fatal */
  { fid = 0;		/* Silence Clang; outOfStack() does not return */
    LD->outofstack = (Stack)&LD->stacks.local;
    outOfStack(LD->outofstack, STACK_OVERFLOW_THROW);
    assert(0);
  }

  /* find the catching frame and try to rewrite the exception */
  for(int rewritten=0; rewritten<=1; rewritten++)
  { SAVE_REGISTERS(QID);
    catchfr_ref = findCatcher(fid, FR, LD->choicepoints, exception_term);
    LOAD_REGISTERS(QID);
    DEBUG(MSG_THROW,
	  { if ( catchfr_ref )
	    { LocalFrame fr = (LocalFrame)valTermRef(catchfr_ref);
	      Sdprintf("[%d]: found catcher at %u\n",
		       PL_thread_self(), levelFrame(fr));
	    } else
	    { Sdprintf("[%d]: not caught\n", PL_thread_self());
	    }
	  });

    DEBUG(CHK_SECURE,
	  { SAVE_REGISTERS(QID);
	    checkData(valTermRef(exception_term));
	    checkStacks(NULL);
	    LOAD_REGISTERS(QID);
	  });

    /* Try to rewrite "normal" exceptions.  This is in particular done
       to decorate exceptions with context information.  We do this at
       most once.
     */
    if ( debugstatus.suspendTrace == false && !rewritten &&
	 !uncachableException(exception_term) &&		/* unwind(_) */
	 !resourceException(exception_term) )
    { bool rc;

      SAVE_REGISTERS(QID);
      rc = exception_hook(QID, consTermRef(FR), catchfr_ref);
      LOAD_REGISTERS(QID);

      if ( rc )
      { DEBUG(MSG_THROW,
	      Sdprintf("Exception was rewritten to: ");
	      PL_write_term(Serror, exception_term, 1200, 0);
	      Sdprintf(" (retrying)\n"));

	PL_rewind_foreign_frame(fid);
	if ( catchfr_ref )
	  clear((LocalFrame)valTermRef(catchfr_ref), FR_CAUGHT);
	continue;
      }
    }
    break;
  } /* Ends "rewritten" loop */
  PL_close_foreign_frame(fid);
  VMH_GOTO(b_throw_debug, catchfr_ref, outofstack);
}
END_VMH

/* By now, we  have checked the basic environment,  found the catching
 * frame and (optionally) finished  rewriting the exception.  The next
 * task to  figure out whether or  not we want to  start the debugger.
 * We want to start the debugger if the exception is not caught in the
 * current query.
 *
 * In fact, we start the debugger, unless there is insufficient space.
 * In that  case we set  `start_tracer` and start the  debugger during
 * stack unwinding when enough stack space has been recovered.
 */

VMH(b_throw_debug, 2, (term_t, Stack), (catchfr_ref, outofstack))
{ bool start_tracer;

#if O_DEBUGGER
  start_tracer = false;
  if ( !catchfr_ref &&
       !PL_same_term(exception_term, exception_printed) &&
       isoff(QueryFromQid(QID), PL_Q_CATCH_EXCEPTION) )
  { term_t rc;

    SAVE_REGISTERS(QID);
    rc = isCaughtInOuterQuery(QID, exception_term);
    DEBUG(MSG_THROW, Sdprintf("Caught in outer: %s\n", rc ? "YES" : "NO"));
    LOAD_REGISTERS(QID);

    if ( !rc )					/* uncaught exception */
    { SAVE_REGISTERS(QID);
      if ( PL_is_functor(exception_term, FUNCTOR_error2) &&
	   truePrologFlag(PLFLAG_DEBUG_ON_ERROR) )
      { DEBUG(MSG_THROW,
	      Sdprintf("Uncaught error(Formal,Context): enable debug mode\n"));
	debugmode(true, NULL);
	if ( !trace_if_space() )		/* see (*) */
	{ start_tracer = true;
	} else
	{ DEBUG(MSG_THROW,
		Sdprintf("No space for debugging, print (l+g+t) = (%zd+%zd+%zd)\n",
			 roomStack(local),roomStack(global),roomStack(trail)));
	  if ( !printMessage(ATOM_error, PL_TERM, exception_term) )
	  { Sdprintf("Could not print message for exception:\n\t");
	    PL_write_term(Suser_error, exception_term, 1200,
			  PL_WRT_QUOTED|PL_WRT_NEWLINE);
	  }
	  PL_put_term(exception_printed, exception_term);
	}
      } else if ( print_unhandled_exception(QID, exception_term) )
      {	PL_put_term(exception_printed, exception_term);
      }
      LOAD_REGISTERS(QID);
    }
  }

  if ( debugstatus.debugging )
    VMH_GOTO(b_throw_unwind_debug, catchfr_ref, outofstack,
	     start_tracer, PL_TRACE_ACTION_NONE);
  else
    VMH_GOTO(b_throw_unwind, catchfr_ref, outofstack);
}
END_VMH

/* Unwind the stack in debug mode
 */

VMH(b_throw_unwind_debug, 4, (term_t, Stack, bool, int), (catchfr_ref, outofstack, start_tracer, action))
{ Choice ch;

  if ( action == PL_TRACE_ACTION_NONE )
  { assert(FR == environment_frame);
  } else
  { FR = environment_frame;
    ch = findStartChoice(FR, BFR);
    goto trace_resume_exception;
  }

  for( ;
       FR && FR > (LocalFrame)valTermRef(catchfr_ref);
       PC = FR->programPointer, FR = FR->parent )
  { ch = findStartChoice(FR, BFR);
    environment_frame = FR;
    ARGP = argFrameP(FR, 0);		/* otherwise GC sees `new' arguments */
    LD->statistics.inferences++;	/* box exit, needed for GC */

    if ( ch )
    { bool printed = PL_same_term(exception_printed, exception_term);
      term_t chref = consTermRef(ch);

      lTop = (LocalFrame)(BFR+1);
      DEBUG(CHK_SECURE,
	    { SAVE_REGISTERS(QID);
	      checkStacks(NULL);
	      LOAD_REGISTERS(QID);
	    });
      SAVE_REGISTERS(QID);
      dbg_discardChoicesAfter((LocalFrame)ch, FINISH_EXTERNAL_EXCEPT);
      LOAD_REGISTERS(QID);
      ch = (Choice)valTermRef(chref);
      Undo(ch->mark);
      DiscardMark(ch->mark);
      clearLocalVariablesFrame(FR);
      PL_put_term(LD->exception.pending, exception_term);
      if ( printed )
	PL_put_term(exception_printed, exception_term);

      DEBUG(CHK_SECURE,
	    { SAVE_REGISTERS(QID);
	      checkStacks(NULL);
	      LOAD_REGISTERS(QID);
	      ch = (Choice)valTermRef(chref);
	    });

      SAVE_REGISTERS(QID);
      action = tracePort(FR, ch, EXCEPTION_PORT, PC);
      LOAD_REGISTERS(QID);

    trace_resume_exception:
      switch( action )
      { case PL_TRACE_ACTION_RETRY:
	  SAVE_REGISTERS(QID);
	  discardChoicesAfter(FR, FINISH_CUT);
	  resumeAfterException(true, outofstack);
	  LOAD_REGISTERS(QID);
	  DEF = FR->predicate;
	  FR->clause = NULL;
	  VMH_GOTO(depart_or_retry_continue);
	case PL_TRACE_ACTION_ABORT:
	  THROW_EXCEPTION;
	case PL_TRACE_ACTION_YIELD:
	  LD->trace.yield.exception.catchfr_ref = catchfr_ref;
	  LD->trace.yield.exception.outofstack  = outofstack;
	  SAVE_REGISTERS(QID);
	  SOLUTION_RETURN(debug_yield(EXCEPTION_PORT));
      }

      setVar(*valTermRef(LD->exception.pending));
    }

    /* discard as much as we can from the local stack */
    SAVE_REGISTERS(QID);
    dbg_except_unwind_ltop();
    dbg_except_discard_frame();
    LOAD_REGISTERS(QID);

    if ( start_tracer && dbg_except_start_tracer() )
      start_tracer = false;
  }

  if ( start_tracer )
  { Sdprintf("Failed to print resource exception due to lack of space\n");
    SAVE_REGISTERS(QID);
    PL_write_term(Serror, exception_term, 1200, PL_WRT_QUOTED|PL_WRT_NEWLINE);
    LOAD_REGISTERS(QID);
  }

  VMH_GOTO(b_throw_resume, catchfr_ref, outofstack);
}
END_VMH
#endif /*O_DEBUGGER*/

/* Normal (nodebug)  version of  the stack unwinding.   This is  a lot
 * simpler.
 */

VMH(b_throw_unwind, 2, (term_t, Stack), (catchfr_ref, outofstack))
{ DEBUG(3, Sdprintf("Unwinding for exception\n"));

  for( ;
       FR && FR > (LocalFrame)valTermRef(catchfr_ref);
       PC = FR->programPointer, FR = FR->parent )
  { environment_frame = FR;
    ARGP = argFrameP(FR, 0);		/* otherwise GC sees `new' arguments */
    LD->statistics.inferences++;	/* box exit, needed for GC */

    SAVE_REGISTERS(QID);
    dbg_discardChoicesAfter(FR, FINISH_EXTERNAL_EXCEPT_UNDO);
    LOAD_REGISTERS(QID);

    lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);
    while ( (void*)fli_context > (void*)FR )
      fli_context = fli_context->parent;
    discardFrame(FR);
    if ( ison(FR, FR_WATCHED) )
    { SAVE_REGISTERS(QID);
      frameFinished(FR, FINISH_EXCEPT);
      LOAD_REGISTERS(QID);
    }
    DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));
  }

  VMH_GOTO(b_throw_resume, catchfr_ref, outofstack);
}
END_VMH

/* We completed unwinding the  stack.  `b_throw_resume` either resumes
 * by  executing the  3rd argument  of a  catch/3 predicate  or causes
 * PL_next_solution() to return if there is no catcher.
 */

VMH(b_throw_resume, 2, (term_t, Stack), (catchfr_ref, outofstack))
{					/* re-fetch (test cleanup(clean-5)) */
  DEBUG(CHK_SECURE, checkData(valTermRef(exception_term)));
  LD->statistics.inferences++;		/* box exit, needed for GC */


  if ( catchfr_ref )
  { word w;

    assert(FR == (LocalFrame)valTermRef(catchfr_ref));

    SAVE_REGISTERS(QID);
    dbg_discardChoicesAfter(FR, FINISH_EXTERNAL_EXCEPT_UNDO);
    LOAD_REGISTERS(QID);
    environment_frame = FR;
					/* re-unify */
    PL_unify(consTermRef(argFrameP(FR, 1)), exception_term);
    lTop = (LocalFrame) argFrameP(FR, 3); /* above the catch/3 */
    if ( (w=uncachableException(exception_term)) )
    { Word p = gTop;

      if ( !has_emergency_space(&LD->stacks.global, 3*sizeof(word)) )
	fatalError("Cannot wrap abort exception\n");

      argFrame(lTop, 0) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
      p[0] = FUNCTOR_drecover_and_rethrow2;
      p[1] = argFrame(FR, 2);
      p[2] = w;
      gTop = p+3;
    } else
    { argFrame(lTop, 0) = argFrame(FR, 2);  /* copy recover goal */
    }

    PC = findCatchExit();
    { LD->query->next_environment = lTop;
      ARGP = argFrameP(lTop, 1);
      SAVE_REGISTERS(QID);
      resumeAfterException(true, outofstack);
      LOAD_REGISTERS(QID);
      LD->query->next_environment = NULL;
      DEBUG(CHK_SECURE, checkData(argFrameP(lTop, 0)));
    }

    VMI_GOTO(I_CALL1);
  } else
  { QF = QueryFromQid(QID);		/* may be shifted */
    set(QF, PL_Q_DETERMINISTIC);
    FR = environment_frame = &QF->top_frame;
    lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);

    Undo(QF->choice.mark);
    DiscardMark(QF->choice.mark);
    QF->foreign_frame = PL_open_foreign_frame();
    QF->exception = PL_copy_term_ref(exception_term);

    SAVE_REGISTERS(QID);
    resumeAfterException(isoff(QF, PL_Q_PASS_EXCEPTION), outofstack);
    LOAD_REGISTERS(QID);
    if ( PL_pending(SIG_GC) )
    { SAVE_REGISTERS(QID);
      garbageCollect(LD->gc.stats.request);
      LOAD_REGISTERS(QID);
    }
    QF = QueryFromQid(QID);		/* may be shifted: recompute */

    DEBUG(MSG_THROW,
	  { Sdprintf("Leaving exception after callback: ");
	    PL_write_term(Serror, QF->exception, 1200, 0);
	    Sdprintf("\n");
	  });

#if !O_VMI_FUNCTIONS
    assert(LD->exception.throw_environment == &THROW_ENV);
    LD->exception.throw_environment = THROW_ENV.parent;
#endif
    SOLUTION_RETURN((QF->flags & PL_Q_EXT_STATUS) ? PL_S_EXCEPTION : false);
  }
}
END_VMH
#endif /*O_CATCHTHROW*/


		 /*******************************
		 *	    META-CALLING	*
		 *******************************/


#ifdef O_CALL_AT_MODULE
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLATM: procedure-module, context-module, procedure
The procedure-module is provided to support the decompiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLATM, VIF_BREAK, 3, (CA1_MODULE, CA1_MODULE, CA1_PROC))
{ PC++;
  VMI_GOTO(I_CALLM);
}
END_VMI

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
    { Module m = lookupModule(word2atom(*ap));

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
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This instruction deals with  @(Callable,  Module),   where  Module  is a
variable. The module argument can be NULL.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLATMV, VIF_BREAK, 3, (CA1_MODULE, CA1_VAR, CA1_PROC))
{ Word ap;
  int iv;
  Procedure proc;

  PC++;
  iv = (int)*PC++;
  proc = code2ptr(Procedure, *PC++);

  ap = varFrameP(FR, iv);
  deRef(ap);
  if ( isTextAtom(*ap) )
  { Module module = lookupModule(word2atom(*ap));
    DEF = proc->definition;
    NFR = lTop;

    VMH_GOTO(mcall_cont, module);
  } else
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_module,
	     pushWordAsTermRef(ap));
    popTermRef();
    THROW_EXCEPTION;
  }
}
END_VMI

#endif/*O_CALL_AT_MODULE*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLM deals with qualified calls. The unfortunate  task is to sort out
the context module for calling a transparent  procedure. This job is the
same as the end of I_CALL1
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLM, VIF_BREAK, 2, (CA1_MODULE, CA1_PROC))
{ Module module = code2ptr(Module, *PC++);
  DEF    = code2ptr(Procedure, *PC++)->definition;
  NFR = lTop;

  VMH_GOTO(mcall_cont, module);
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALL1 is generated by the compiler if a variable is encountered as
a subclause. Note that the compount   statement  opened here is encloses
also I_CALL. This allows us to use   local register variables, but still
jump to the `normal_call' label to do the common part of all these three
virtual machine instructions.

I_CALL1 has the task of  analysing  the   goal:  it  should fill the
->procedure slot of the new frame and  save the current program counter.
It also is responsible of filling the   argument part of the environment
frame with the arguments of the term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALL1, VIF_BREAK, 0, ())
{ Word a;

  NFR = lTop;
  a = argFrameP(NFR, 0);		/* get the goal */

  DEBUG(MSG_CALL,
	{ term_t g = pushWordAsTermRef(a);
	  LocalFrame ot = lTop;
	  lTop += 100;
	  PL_write_term(Serror, g, 1200, PL_WRT_NEWLINE);
	  popTermRef();
	  lTop = ot;
	});
  DEBUG(CHK_SECURE, checkStacks(NULL));

  VMH_GOTO(i_metacall_common, a, 0, true);
}
END_VMI

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

Creating a `local clause'  is  needed   for  control  structures and for
meta-calling call/N for N >  8  as   there  are  no real predicates that
provide a backup. In  the  case  we   are  under  reset/3,  we can never
generate a local clause while  passing  call(<call/N>)   for  N  >  8 to
'$meta_call'/1 however leads to an infinite loop. For now we generate an
undefined predicate for call/N.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
VMH(i_metacall_common, 3, (Word, int, bool), (a, callargs, is_call0))
{ word goal;
  int arity = 0;
  functor_t functor = -1;
  Word args;
  Module module = NULL;
  closure *clsp = NULL;

  if ( !(a = stripModule(a, &module, 0)) )
    THROW_EXCEPTION;
  goal = *a;

  if ( isAtom(goal) )
  { Atom ap = atomValue(goal);

    if ( ison(ap->type, PL_BLOB_TEXT) || goal == ATOM_nil )
    { functor = lookupFunctorDef(word2atom(goal), callargs);
      arity   = 0;
      args    = NULL;
    } else if ( ap->type == &_PL_closure_blob )
    { clsp = (closure*)ap->name;
    } else
    { VMH_GOTO(call_type_error);
    }
  } else if ( isTerm(goal) )
  { FunctorDef fd;
    Functor gt = valueTerm(goal);

    functor = word2functor(gt->definition);
    if ( is_call0 && functor == FUNCTOR_colon2 )
      VMH_GOTO(call_type_error);

    fd = valueFunctor(functor);
    if ( !isCallableAtom(fd->name) )
    { Atom ap = atomValue(fd->name);

      if ( is_call0 && ap->type == &_PL_closure_blob )
	clsp = (closure*)ap->name;
      else
	VMH_GOTO(call_type_error);
    }

    args  = gt->arguments;
    arity = (int)fd->arity;
    if ( arity + callargs > MAXARITY )
      VMH_GOTO(max_arity_overflow);
    if (!is_call0)
      functor = lookupFunctorDef(fd->name, arity + callargs);

    if ( is_call0 ) /* checks unique to the I_CALL1 case */
    { if ( isoff(fd, CONTROL_F) &&
	   !(fd->name == ATOM_call && fd->arity > 8) )
      { /* common case, nothing to do */
      } else if ( ison(FR, FR_INRESET) )
      { if ( isoff(fd, CONTROL_F) && fd->name != ATOM_call )
	{ /* arity > 8 will raise existence error */
	} else
	{ DEF = GD->procedures.dmeta_call1->definition;
	  VMH_GOTO(mcall_cont, module);
	}
      } else
      { Clause cl;
	int rc;

	if ( fd->functor == FUNCTOR_at_sign2 &&
	     !checkCallAtContextInstantiation(a) )
	  THROW_EXCEPTION;

	lTop = NFR;
	setNextFrameFlags(NFR, FR);
	rc = compileClause(&cl, NULL, a, PROCEDURE_dcall1, module, 0, 0);
	switch(rc)
	{ case true:
	    break;
	  case false:
	    THROW_EXCEPTION;
	  case LOCAL_OVERFLOW:
	  case CHECK_INTERRUPT:
	  { term_t lTopH = consTermRef(lTop);

	    LD->query->next_environment = NFR;
	    lTop = (LocalFrame)argFrameP(NFR, 1);
	    ARGP = (Word)lTop;
	    SAVE_REGISTERS(QID);
	    if ( rc == LOCAL_OVERFLOW )
	    { size_t room = roomStack(local);
	      rc = growLocalSpace(room*2, ALLOW_SHIFT);
	    } else
	    { rc = (PL_handle_signals() >= 0); /* rc = false: exception */
	    }
	    LOAD_REGISTERS(QID);
	    LD->query->next_environment = NULL;
	    lTop = (LocalFrame)valTermRef(lTopH);
	    if ( rc != true )
	    { if ( rc )		/* rc < 0 (*_OVERFLOW): raise overflow exception */
		raiseStackOverflow(rc);
	      THROW_EXCEPTION;
	    }
	    VMI_GOTO(I_CALL1);
	  }
	  default:
	    assert(0);
	}

	DEF		  = NFR->predicate;
	DEBUG(CHK_SECURE, assert(DEF == PROCEDURE_dcall1->definition));
	NFR->parent	  = FR;
	NFR->programPointer = PC;
#ifdef O_PROFILE
	NFR->prof_node      = FR->prof_node;
#endif
#ifdef O_LOGICAL_UPDATE
	cl->generation.erased = GEN_INFINITE;
	cl->generation.created = global_generation();
	setGenerationFrame(NFR);
#endif
	PC = cl->codes;

	enterDefinition(DEF);
	environment_frame = FR = NFR;
	ARGP = argFrameP(lTop, 0);

	NEXT_INSTRUCTION;
      }
    }
  } else
  { VMH_GOTO(call_type_error);
  }

  if ( !is_call0 && arity != 1 )
  { int i, shift = arity - 1;

    a = argFrameP(NFR, 1);	/* pointer to 1-st arg */

    if ( shift > 0 )
    { for(i=callargs-1; i>=0; i--)
      { if ( isRef(a[i]) )
	{ Word a1 = unRef(a[i]);

	  if ( a1 >= a && a1 < a+arity )
	    a[i+shift] = makeRefG(a1+shift);
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
	    a[i+shift] = makeRefG(a1+shift);
	  else
	    a[i+shift] = a[i];
	} else
	  a[i+shift] = a[i];
      }
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now scan the argument vector of the goal and fill the arguments  of  the
frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if ( arity > 0 )
  { ARGP = argFrameP(NFR, 0);
					/* args is pointer into term: ok */
    for(; arity-- > 0; ARGP++, args++)
      *ARGP = linkValI(args);
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find  the  associated  procedure  and  its  definition.  The  latter  is
unfortunate, but we need to know the transparent status of the predicate
to be called to get the context module   right. We must build a complete
environment before we can call trapUndefined() to make shift/GC happy.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  assert(clsp != NULL || functor >= 0);
  if ( clsp )
  { DEF = &clsp->def;
  } else
  { DEF = resolveProcedure(functor, module)->definition;
  }

  VMH_GOTO(mcall_cont, module);
}
END_VMH

VMH(call_type_error, 0, (), ())
{ DEBUG(CHK_SECURE, checkStacks(NULL));
  PL_error(NULL, 0, NULL, ERR_TYPE,
	   ATOM_callable, pushWordAsTermRef(argFrameP(NFR, 0)));
  popTermRef();
  THROW_EXCEPTION;
}
END_VMH

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I_CALLN: translation of call(Goal, Arg1, ...)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLN, VIF_BREAK, 1, (CA1_INTEGER))
{ Word a;
  int callargs = (int)*PC++;

  NFR = lTop;
  a = argFrameP(NFR, 0);		/* get the (now) instantiated */
  deRef(a);				/* variable */

  VMH_GOTO(i_metacall_common, a, callargs, false);
}
END_VMI

VMH(max_arity_overflow, 0, (), ())
{ fid_t fid;
  lTop = (LocalFrame)argFrameP(NFR, 1);
  fid = PL_open_foreign_frame();
  PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_max_procedure_arity);
  PL_close_foreign_frame(fid);
  THROW_EXCEPTION;
}
END_VMH

VMH(mcall_cont, 1, (Module), (module))
{ setNextFrameFlags(NFR, FR);
  if ( !DEF->impl.any.defined && isoff(DEF, PROC_DEFINED) )
  { term_t nref = consTermRef(NFR);
    NFR->parent         = FR;
    setFramePredicate(NFR, DEF);	/* TBD */
    setGenerationFrame(NFR);
    NFR->programPointer = PC;		/* save PC in child */
    NFR->clause         = NULL;
#ifdef O_PROFILE
    NFR->prof_node      = NULL;
#endif
    setNextFrameFlags(NFR, FR);
    environment_frame = FR = NFR;
    lTop = (LocalFrame)argFrameP(NFR, DEF->functor->arity);

    SAVE_REGISTERS(QID);
    DEF = trapUndefined(DEF);
    LOAD_REGISTERS(QID);
    NFR = (LocalFrame)valTermRef(nref);

    FR = FR->parent;
  }

  if ( ison(DEF, P_TRANSPARENT) )
    setContextModule(NFR, module);

  VMH_GOTO(normal_call);
}
END_VMH


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reset(:Goal, ?Ball, -Continuation) :-
    '$reset'.

Where '$reset' maps to

    I_RESET
    I_EXITRESET

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_RESET, 0, 0, ())
{ Word p = argFrameP(FR, 0);

  if ( isVar(*p) )
  { PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    THROW_EXCEPTION;
  }
  set(FR, FR_INRESET);
			  /* = B_VAR0 */
  *argFrameP(lTop, 0) = linkValI(p);
  VMI_GOTO(I_CALL1);
}
END_VMI


VMI(I_EXITRESET, 0, 0, ())
{ Word p = argFrameP(FR, 2);

  deRef(p);
  if ( canBind(*p) )
  { ENSURE_GLOBAL_SPACE(0,
			{ p = argFrameP(FR, 2);
			  deRef(p);
			});
    bindConst(p, consInt(0));
    NEXT_INSTRUCTION;
  } else
  { PL_uninstantiation_error(pushWordAsTermRef(p));
    popTermRef();
    THROW_EXCEPTION;
  }
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$call_continuation(Cont)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(I_CALLCONT, 0, 1, (CA1_VAR))
{ Word cp = varFrameP(FR, (int)*PC++);

  deRef(cp);
  if ( hasFunctor(*cp, FUNCTOR_call1) )
  { *ARGP++ = linkValI(argTermP(*cp, 0));
    VMI_GOTO(I_CALL1);
  } else
  { term_t cont = pushWordAsTermRef(cp);
    Code pc;

    SAVE_REGISTERS(QID);
    pc = push_continuation(cont, FR, PC);
    LOAD_REGISTERS(QID);
    popTermRef();

    if ( pc )
    { PC = pc;
      FR = environment_frame;
      DEF = FR->predicate;
      ARGP = argFrameP(lTop, 0);
      NEXT_INSTRUCTION;
    } else
    { THROW_EXCEPTION;
    }
  }
}
END_VMI

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shift(Ball) :-
    '$shift'(Ball).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


VMI(I_SHIFT, 0, 1, (CA1_VAR))
{ VMH_GOTO(shift_common, false);
}
END_VMI

VMH(shift_common, 1, (int), (shift_for_copy))
{ Word ballp;
  term_t ball;
  Code pc;
  fid_t fid;

  ballp = varFrameP(FR, (int)*PC++);
  ball  = pushWordAsTermRef(ballp);

  SAVE_REGISTERS(QID);
  fid = PL_open_foreign_frame();
  pc = shift(ball, shift_for_copy);
  PL_close_foreign_frame(fid);
  LOAD_REGISTERS(QID);

  popTermRef();

  if ( pc )
  { PC = pc;
    FR = environment_frame;
    DEF = FR->predicate;
    ARGP = argFrameP(lTop, 0);
    NEXT_INSTRUCTION;
  } else
  { THROW_EXCEPTION;
  }
}
END_VMH

VMI(I_SHIFTCP, 0, 1, (CA1_VAR))
{ VMH_GOTO(shift_common, true);
}
END_VMI


		 /*******************************
		 *	  COMPILED TRIES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trie_gen_compiled(Trie, Key)
trie_gen_compiled(Trie, Key, Value)

This is the supervisor for trie_gen_compiled/2,3.   It compiles the trie
on demand and then calls the compiled   clause  that belongs to the same
predicate.

Instead of passing a trie we can also   pass  the dbref for the compiled
trie clause or `fail` to express the  trie   is  empty.  This is used to
achieve thread-safe deletion of answer tries.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

VMI(S_TRIE_GEN, 0, 0, ())
{ Word tp = argFrameP(FR, 0);
  atom_t dbref;
  ClauseRef cref;

  deRef(tp);
  dbref = word2atom(*tp);
  if ( !isAtom(dbref) )
  {
  trie_gen_type_error:
    SAVE_REGISTERS(QID);
    PL_type_error("trie_or_clause", pushWordAsTermRef(argFrameP(FR, 0)));
    popTermRef();
    LOAD_REGISTERS(QID);
    THROW_EXCEPTION;
  }

  if ( dbref == ATOM_fail )
    FRAME_FAILED;

  if ( !(cref = clause_clref(dbref)) )
  { trie *t;

    if ( !(t = symbol_trie(dbref)) )
      goto trie_gen_type_error;

    TRIE_STAT_INC(t, gen_call);
    if ( !(dbref=t->clause) )
    { if ( t->value_count == 0 )
	FRAME_FAILED;

      LD->query->next_environment = lTop;
      lTop = (LocalFrame)ARGP;
      SAVE_REGISTERS(QID);
      dbref = compile_trie(FR->predicate, t);
      LOAD_REGISTERS(QID);
      LD->query->next_environment = NULL;
      if ( dbref == ATOM_error )
	THROW_EXCEPTION;
    }

    if ( dbref == ATOM_fail )
      FRAME_FAILED;

    cref = clause_clref(dbref);
  }

  ARGP = argFrameP(FR, 0);
  TRUST_CLAUSE(cref);
}
END_VMI


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trie enumeration instructions. The trie  enumeration   is  executed by a
single clause that starts with an  T_TRIE_GEN instruction. The state for
generating  terms  from  the  trie  is   maintained  in  the  associated
environment frame (FR):

  argFrame(0)	The term (key)
  argFrame(1)   The node value
  argFrame(2)   Current term
  argFrame(3)   Current arg in current term (1..)
  argFrame(4)   Stack of current locations as
		'$argp'(TermP, ArgN, Parent).
  argFrame(5..)	Variables (counting from 1..)

TBD:

  - Resize stack on TrailAssignment(TrieCurrentP)
  - The various instructions are very similar to the H_* instructions.
    They only manage TrieCurrentP instead of ARGP. This should be merged
    somehow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define TrieTermP     argFrameP(FR, TRIE_ARGS)
#define TrieOffset    argFrameP(FR, TRIE_ARGS+1)
#define TrieCurrentP  (isRef(*TrieTermP) \
			 ? unRef(*TrieTermP) \
			 : argTermP(*TrieTermP, valInt(*TrieOffset)-1))
#define TrieArgStackP argFrameP(FR, TRIE_ARGS+2)
#define TrieVarP(n)   argFrameP(FR, (TRIE_VAR_OFFSET-1+(n)))
#define TRIE_TRY \
	do \
	{ intptr_t skip = *PC++;			     \
	  Choice ch;					     \
	  ENSURE_LOCAL_SPACE(sizeof(*ch), THROW_EXCEPTION);  \
	  ch = newChoice(CHP_JUMP, FR);		     \
	  ch->value.pc = PC+skip;			     \
	} while(0)
#define TrieNextArg() \
	do \
	{ ENSURE_GLOBAL_SPACE(0, (void)0); \
	  TrailAssignment(TrieOffset); \
	  (*TrieOffset = consInt(valInt(*TrieOffset)+1)); \
	} while(0)
#define TriePushArgP() \
	do \
	{ Word _astack = gTop; \
	  assert(isTerm(*TrieTermP)); \
	  gTop += 4; \
	  assert(gTop < gMax); \
	  _astack[0] = FUNCTOR_targp3; \
	  _astack[1] = *TrieTermP; \
	  _astack[2] = *TrieOffset; \
	  _astack[3] = *TrieArgStackP; \
	  TrailAssignment(TrieArgStackP); \
	  *TrieArgStackP = consPtr(_astack, TAG_COMPOUND|STG_GLOBAL); \
	} while(0)
#define TriePopArgP() \
	do  \
	{ Word _astack = valPtr(*TrieArgStackP); \
	  assert(_astack[0] == FUNCTOR_targp3); \
	  *TrieTermP     = _astack[1]; \
	  *TrieOffset    = _astack[2]; \
	  *TrieArgStackP = _astack[3]; \
	} while(0)
#define UnwindTrieArgP() \
	do \
	{ Word _as;				\
	  if ( *TrieArgStackP != ATOM_nil )	\
	  { _as = valPtr(*TrieArgStackP);	\
	    assert(_as[0] == FUNCTOR_targp3);	\
	    while ( _as[3] != ATOM_nil )	\
	    { assert(_as[0] == FUNCTOR_targp3);	\
	      _as = valPtr(_as[3]);		\
	    }					\
	    TrailAssignment(TrieTermP);		\
	    TrailAssignment(TrieArgStackP);	\
	    *TrieTermP     = _as[1];		\
	    *TrieArgStackP = ATOM_nil;		\
	  }					\
	} while(0)

VMI(T_TRIE_GEN2, 0, 0, ())
{ Word ap;
  size_t nvars = FR->clause->value.clause->prolog_vars - TRIE_VAR_OFFSET;

  DEBUG(CHK_SECURE, checkStacks(NULL));
  DEBUG(MSG_TRIE_VM, Sdprintf("T_TRIE_GEN: %zd vars\n", nvars));

  setVar(argFrame(FR, 2));
  *TrieTermP     = ATOM_nil;
  *TrieOffset    = consInt(1);
  *TrieArgStackP = ATOM_nil;
  if ( nvars )
  { Word vp = TrieVarP(1);
    for( ; nvars-- > 0; vp++)
      setVar(*vp);
  }

  ENSURE_GLOBAL_SPACE(2, (void)0);
  ap = gTop;
  ap[0] = FUNCTOR_plus1;
  setVar(ap[1]);
  gTop += 2;

  /* argFrameP(FR, 0) is the trie */
  unify_ptrs(&ap[1], argFrameP(FR, 1), 0);

  *TrieTermP = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);

  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_TRIE_GEN3, 0, 0, ())
{ Word ap;
  size_t nvars = FR->clause->value.clause->prolog_vars - TRIE_VAR_OFFSET;

  DEBUG(CHK_SECURE, checkStacks(NULL));
  DEBUG(MSG_TRIE_VM, Sdprintf("T_TRIE_GEN: %zd vars\n", nvars));

  *TrieTermP     = ATOM_nil;
  *TrieOffset    = consInt(1);
  *TrieArgStackP = ATOM_nil;
  if ( nvars )
  { Word vp = TrieVarP(1);
    for( ; nvars-- > 0; vp++)
      setVar(*vp);
  }

  ENSURE_GLOBAL_SPACE(3, (void)0);
  ap = gTop;
  ap[0] = FUNCTOR_plus2;
  setVar(ap[1]);
  setVar(ap[2]);
  gTop += 3;

  /* argFrameP(FR, 0) is the trie */
  unify_ptrs(&ap[1], argFrameP(FR, 1), 0);
  unify_ptrs(&ap[2], argFrameP(FR, 2), 0);

  *TrieTermP = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);

  NEXT_INSTRUCTION;
}
END_VMI


VMI(T_VALUE, 0, 0, ())
{ ENSURE_GLOBAL_SPACE(0, (void)0);	/* allows for 3 trailed assignments */

  TrailAssignment(TrieOffset);
  UnwindTrieArgP();
  *TrieOffset = consInt(2);

  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_DELAY, 0, 1, (CA1_TRIE_NODE))
{ trie_node *answer = code2ptr(trie_node*, *PC++);
  atom_t atrie;

  ENSURE_STACK_SPACE(16, 12, (void)0);
  UnwindTrieArgP();

  if ( answer )
  { trie *trie = get_trie_from_node(answer);
    atrie = trie_symbol(trie);
  } else
  { atrie = ATOM_nil;
  }

  tbl_push_delay(atrie, argTermP(*TrieTermP, 0), answer);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_TRY_FUNCTOR, 0, 2, (CA1_JUMP,CA1_FUNC))
{ TRIE_TRY;
  VMI_GOTO(T_FUNCTOR);
}
END_VMI
VMI(T_FUNCTOR, 0, 1, (CA1_FUNC))
{ functor_t f = (functor_t) *PC++;
  Word p;

  DEBUG(1, checkStacks(NULL));

  DEBUG(MSG_TRIE_VM,
	{ Sdprintf("T_FUNCTOR %s ", functorName(f));
	  PL_write_term(Serror, consTermRef(TrieArgStackP), 999, PL_WRT_NEWLINE);
	});

  deRef2(TrieCurrentP, p);
  if ( canBind(*p) )
  { size_t arity = arityFunctor(f);
    Word ap;
    word c;

    /* 4 extra for the '$targp3' cell for TriePushArgP() */
    ENSURE_STACK_SPACE(1+arity+4+6, 6, deRef2(TrieCurrentP, p));
    ap = gTop;
    gTop += 1+arity;
    c = consPtr(ap, TAG_COMPOUND|STG_GLOBAL);
    TriePushArgP();
    DEBUG(MSG_TRIE_VM,
	  { Sdprintf("Push: ");
	    PL_write_term(Serror, consTermRef(TrieArgStackP), 999,
			  PL_WRT_NEWLINE);
	  });
    TrailAssignment(TrieTermP);
    TrailAssignment(TrieOffset);
    *TrieTermP  = c;
    *TrieOffset = consInt(1);
    *ap++ = f;
    while(arity-->0)			/* must clear if we want to do GC */
      setVar(*ap++);
    bindConst(p, c);
    DEBUG(1, checkStacks(NULL));
    NEXT_INSTRUCTION;
  }
  if ( isTerm(*p) )
  { Functor term = valueTerm(*p);

    if ( term->definition == f )
    { ENSURE_GLOBAL_SPACE(4, deRef2(TrieCurrentP, p));
      TriePushArgP();
      TrailAssignment(TrieTermP);
      TrailAssignment(TrieOffset);
      *TrieTermP  = *p;
      *TrieOffset = consInt(1);
      NEXT_INSTRUCTION;
    }
  }
  DEBUG(1, checkStacks(NULL));
  CLAUSE_FAILED;
}
END_VMI

VMI(T_POP, 0, 0, ())
{ ENSURE_GLOBAL_SPACE(0, (void)0);	/* allows for 3 trailed assignments */
  TrailAssignment(TrieTermP);
  TrailAssignment(TrieOffset);
  TrailAssignment(TrieArgStackP);
  TriePopArgP();
  TrieNextArg();

  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_POPN, 0, 1, (CA1_INTEGER))
{ intptr_t n = (intptr_t)*PC++;

  ENSURE_GLOBAL_SPACE(0, (void)0);
  TrailAssignment(TrieTermP);
  TrailAssignment(TrieOffset);
  TrailAssignment(TrieArgStackP);
  while(n-->0)
    TriePopArgP();
  TrieNextArg();

  DEBUG(MSG_TRIE_VM,
	{ Sdprintf("POPN: ");
	  PL_write_term(Serror, consTermRef(TrieTermP),  999, 0);
	  Sdprintf(" ARG: ");
	  PL_write_term(Serror, consTermRef(TrieOffset), 999, PL_WRT_NEWLINE);
	});

  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_TRY_VAR, 0, 2, (CA1_JUMP,CA1_INTEGER))
{ TRIE_TRY;
  VMI_GOTO(T_VAR);
}
END_VMI
VMI(T_VAR, 0, 1, (CA1_INTEGER))
{ intptr_t offset = (intptr_t)*PC++;		/* offset = 1.. */
  Word vp = TrieVarP(offset);

  DEBUG(MSG_TRIE_VM,
	{ Sdprintf("VAR: %lld: ", offset);
	  Sdprintf("TermP: ");
	  PL_write_term(Serror, consTermRef(TrieTermP),  999, 0);
	  Sdprintf(" Offset: ");
	  PL_write_term(Serror, consTermRef(TrieOffset), 999, PL_WRT_NEWLINE);
	  PL_write_term(Serror, consTermRef(vp), 999, 0);
	  Sdprintf(" val = ");
	  PL_write_term(Serror, pushWordAsTermRef(TrieCurrentP), 999,
			PL_WRT_NEWLINE);
	  popTermRef();
	});

  if ( isVar(*vp) )
  { DEBUG(MSG_TRIE_VM, Sdprintf("First var %zd\n", offset));
    Trail(vp, linkValI(TrieCurrentP));
  } else
  { int rc;

    DEBUG(MSG_TRIE_VM, Sdprintf("Next var %zd\n", offset));
    SAVE_REGISTERS(QID);
    rc = unify_ptrs(vp, TrieCurrentP, ALLOW_GC);
    LOAD_REGISTERS(QID);
    if ( !rc )
    { if ( exception_term )
	THROW_EXCEPTION;
      CLAUSE_FAILED;
    }
  }

  TrieNextArg();
  NEXT_INSTRUCTION;
}
END_VMI

/* This does the same  as unify_key() for `TAG_ATTVAR|STG_STATIC`.  We
   wrap the attributes  into a tuple, where the first  argument is the
   current location (`vi->address` in  unify_key()), the second is the
   attributed variable  and the  3rd is the  location where  where the
   attributes  will be  build.  See  T_FUNCTOR for  the equivalent  of
   pushing the functor.
 */

VMI(T_TRY_ATTVARA, 0, 2, (CA1_JUMP,CA1_INTEGER))
{ TRIE_TRY;
  VMI_GOTO(T_ATTVARA);
}
END_VMI
VMI(T_ATTVARA, 0, 1, (CA1_INTEGER))
{
#if O_TRIE_ATTVAR
  intptr_t offset = (intptr_t)*PC++;		/* offset = 1.. */
  ENSURE_STACK_SPACE(6+4+6, 6, (void)0);
  Word vp = TrieVarP(offset);

  DEBUG(MSG_TRIE_VM, Sdprintf("T_ATTVARA %zd\n", offset));
  if ( isVar(*vp) )		/* first encounter */
  { Word gp, p;
    word tuple;			/* att(Target,AttVar,Atts) */

    gp = gTop;
    register_attvar(gp);
    gp[1] = consPtr(&gp[5], TAG_ATTVAR|STG_GLOBAL);
    gp[2] = FUNCTOR_att3;
    gp[3] = linkValI(TrieCurrentP);
    gp[4] = makeRefG(&gp[1]);
    setVar(gp[5]);
    gTop += 6;
    tuple = consPtr(&gp[2], TAG_COMPOUND|STG_GLOBAL);
    Trail(vp, tuple);
    TriePushArgP();
    TrailAssignment(TrieTermP);
    TrailAssignment(TrieOffset);
    *TrieTermP  = tuple;
    *TrieOffset = consInt(3);
    deRef2(&gp[3], p);
    if ( isVar(*p) )		  /* write mode */
      Trail(p, makeRefG(&gp[1])); /* link to attributed var */
    DEBUG(MSG_TRIE_VM, pl_writeln(consTermRef(vp)));
  } else			/* 2nd or later encounter */
  { Functor vi = valueTerm(*vp);
    assert(vi->definition == FUNCTOR_att3);

    if ( isVar(*TrieCurrentP) )
    { DEBUG(MSG_TRIE_VM,
	    Sdprintf("T_ATTVARA (not first) write mode\n"));
      Trail(TrieCurrentP, vi->arguments[1]);
    } else
    { int rc;

      SAVE_REGISTERS(QID);
      rc = unify_ptrs(&vi->arguments[0], TrieCurrentP, ALLOW_GC);
      LOAD_REGISTERS(QID);
      if ( !rc )
      { if ( exception_term )
	  THROW_EXCEPTION;
	CLAUSE_FAILED;
      }
    }

    TrieNextArg();
  }
#endif /*O_TRIE_ATTVAR*/
  NEXT_INSTRUCTION;
}
END_VMI

VMH(t_attvarz, 1, (bool), (istable))
{
#if O_TRIE_ATTVAR
  intptr_t offset = (intptr_t)*PC++;		/* offset = 1.. */
  DEBUG(MSG_TRIE_VM, Sdprintf("T_ATTVARZ %zd\n", offset));

  ENSURE_STACK_SPACE(0,0,(void)0);
  Word vp = TrieVarP(offset);
  Functor vi = valueTerm(*vp);
  assert(vi->definition == FUNCTOR_att3);
  Word p2, av;
  deRef2(&vi->arguments[0], p2);
  deRef2(&vi->arguments[1], av);
  if ( p2 != av )
  { DEBUG(MSG_TRIE_VM, Sdprintf("T_ATTVARZ %zd: read mode\n", offset));
    if ( !isVar(*p2) )
    { if ( istable )
      { TrailAssignment(p2);
	*p2 = vi->arguments[1]; /* must be a reference */
      } else
      {	assignAttVar(unRef(vi->arguments[1]), p2);
      }
    } else
    { Trail(p2, vi->arguments[1]);
    }
  }
#endif /*O_TRIE_ATTVAR*/
  NEXT_INSTRUCTION;
}
END_VMH

VMI(T_TRY_ATTVARZ, 0, 2, (CA1_JUMP,CA1_INTEGER))
{ TRIE_TRY;
  VMI_GOTO(T_ATTVARZ);
}
END_VMI
VMI(T_ATTVARZ, 0, 1, (CA1_INTEGER))
{ VMH_GOTO(t_attvarz, false);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_TRY_ATTVARZT, 0, 2, (CA1_JUMP,CA1_INTEGER))
{ TRIE_TRY;
  VMI_GOTO(T_ATTVARZT);
}
END_VMI
VMI(T_ATTVARZT, 0, 1, (CA1_INTEGER))
{ VMH_GOTO(t_attvarz, true);
  NEXT_INSTRUCTION;
}
END_VMI

VMI(T_TRY_FLOAT, 0, 1+CODES_PER_DOUBLE, (CA1_JUMP,CA1_FLOAT))
{ TRIE_TRY;
  VMI_GOTO(T_FLOAT);
}
END_VMI
VMI(T_FLOAT, 0, CODES_PER_DOUBLE, (CA1_FLOAT))
{ Word k;

  deRef2(TrieCurrentP, k);
  if ( canBind(*k) )
  { Word p;
    word c;

    ENSURE_GLOBAL_SPACE(2+WORDS_PER_DOUBLE, deRef2(TrieCurrentP, k));
    p = gTop;
    gTop += 2+WORDS_PER_DOUBLE;
    c = consPtr(p, TAG_FLOAT|STG_GLOBAL);

    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
    cpDoubleData(p, PC);
    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);

    bindConst(k, c);
    TrieNextArg();
    NEXT_INSTRUCTION;
  } else if ( isFloat(*k) )
  { Code p = (Code)valIndirectP(*k);

    switch(CODES_PER_DOUBLE) /* depend on compiler to clean up */
    { case 2:
	if ( *p++ != *PC++ )
	  CLAUSE_FAILED;
      case 1:
	if ( *p++ == *PC++ )
	{ TrieNextArg();
	  NEXT_INSTRUCTION;
	}
	CLAUSE_FAILED;
      default:
	assert(0);
    }
  }

  CLAUSE_FAILED;
}
END_VMI


VMI(T_TRY_MPZ, 0, VM_DYNARGC, (CA1_JUMP,CA1_MPZ))
{ TRIE_TRY;
  VMI_GOTO(T_MPZ);
}
END_VMI
VMI(T_MPZ, 0, VM_DYNARGC, (CA1_MPZ))
{ SEPARATE_VMI1;
  VMI_GOTO(T_STRING);
}
END_VMI

VMI(T_TRY_STRING, 0, VM_DYNARGC, (CA1_JUMP,CA1_STRING))
{ TRIE_TRY;
  VMI_GOTO(T_STRING);
}
END_VMI
VMI(T_STRING, 0, VM_DYNARGC, (CA1_STRING))
{ Word k;

  deRef2(TrieCurrentP, k);
  if ( canBind(*k) )
  { size_t sz = gsizeIndirectFromCode(PC);

    ENSURE_GLOBAL_SPACE(sz, deRef2(TrieCurrentP, k));
    struct word_and_Code retval = VM_globalIndirectFromCode(PC);
    PC = retval.code;
    bindConst(k, retval.word);
    TrieNextArg();
    NEXT_INSTRUCTION;
  } else if ( isIndirect(*k) )
  { struct word_and_Code retval = VM_equalIndirectFromCode(*k, PC);
    if ( retval.word )
    { PC = retval.code;
      TrieNextArg();
      NEXT_INSTRUCTION;
    }
  }

  CLAUSE_FAILED;
}
END_VMI


VMI(T_TRY_ATOM, 0, 2, (CA1_JUMP,CA1_DATA))
{ TRIE_TRY;
  VMI_GOTO(T_ATOM);
}
END_VMI

VMI(T_ATOM, 0, 1, (CA1_DATA))
{ atom_t c = code2atom(*PC++);
  DEBUG(MSG_TRIE_VM, Sdprintf("T_ATOM %s\n", PL_atom_chars(c)));
  pushVolatileAtom(c);
  VMH_GOTO(t_const, c);
}
END_VMI

VMI(T_TRY_SMALLINT, 0, 2, (CA1_JUMP,CA1_INTEGER))
{ TRIE_TRY;
  VMI_GOTO(T_SMALLINT);
}
END_VMI

VMI(T_SMALLINT, 0, 1, (CA1_INTEGER))
{ scode i = (scode)*PC++;
  DEBUG(MSG_TRIE_VM, Sdprintf("T_SMALLINT %lld\n", (long long)i));
  VMH_GOTO(t_const, consInt(i));
}
END_VMI

VMI(T_TRY_SMALLINTW, 0, 1+CODES_PER_WORD, (CA1_JUMP,CA1_WORD))
{ TRIE_TRY;
  VMI_GOTO(T_SMALLINTW);
}
END_VMI

VMI(T_SMALLINTW, 0, CODES_PER_WORD, (CA1_WORD))
{ word w;

#if CODES_PER_WORD == 1
  SEPARATE_VMI1;		/* Might collapse with T_SMALLINT */
#endif
  PC = code_get_word(PC, &w);
  DEBUG(MSG_TRIE_VM, Sdprintf("T_SMALLINT %lld\n", (long long)w));
  VMH_GOTO(t_const, consInt((sword)w));
}
END_VMI

VMH(t_const, 1, (word), (c))
{ Word k;
  deRef2(TrieCurrentP, k);
  if ( *k == c )
  { TrieNextArg();
    NEXT_INSTRUCTION;
  }
  if ( canBind(*k) )
  { ENSURE_GLOBAL_SPACE(0, deRef2(TrieCurrentP, k));
    bindConst(k, c);
    TrieNextArg();
    NEXT_INSTRUCTION;
  }
  CLAUSE_FAILED;
}
END_VMH

VMI(T_CHECKWAKEUP, 0, 0, ())
{ DEBUG(CHK_SECURE, checkStacks(NULL));
  CHECK_WAKEUP;
  NEXT_INSTRUCTION;
}
END_VMI;

		 /*******************************
		 *	   BACKTRACKING		*
		 *******************************/

/* CLAUSE_FAILED
   We get here if unification of the head failed.
 */

VMH(unify_backtrack, 0, (), ())
{ QF = QueryFromQid(QID);	/* ARGP is pushed an unknown amount */
  aTop = QF->aSave;

  VMH_GOTO(shallow_backtrack);
}
END_VMH

/* BODY_FAILED
   We get here if execution of the body failed.  This happens if some
   body goal is translated into a VM instruction and this instruction
   fails.
 */
VMH(shallow_backtrack, 0, (), ())
{ Choice ch = BFR;

  if ( FR == ch->frame )
  { Undo(ch->mark);

    if ( ch->type == CHP_JUMP )
    { DiscardMark(ch->mark);
      PC   = ch->value.pc;
      BFR  = ch->parent;
      lTop = (LocalFrame)ch;
      ARGP = argFrameP(lTop, 0);

      NEXT_INSTRUCTION;
    } else if ( ch->type == CHP_CLAUSE )
    { ARGP = argFrameP(FR, 0);
      if ( !(CL = nextClause(&ch->value.clause, ARGP, FR, DEF)) )
	FRAME_FAILED;		/* can happen if scan-ahead was too short */
      Word nTop = argFrameP(FR, CL->value.clause->variables);
      PC = CL->value.clause->codes;
      UMODE = uread;

      if ( f_hasSpace(nTop, lMax, LOCAL_MARGIN, 1) )
      { if ( (Word)ch != nTop ) /* Choice point needs to move */
	{ Choice nch = (Choice)nTop;
	  memmove(nch, ch, sizeof(*ch));
	  BFR = ch = nch;
	}

	if ( ch->value.clause.cref )
	{ lTop = (LocalFrame)(ch+1);
	  NEXT_INSTRUCTION;
	} else if ( unlikely(debugstatus.debugging) )
	{ ch->type = CHP_DEBUG;
	  lTop = (LocalFrame)(ch+1);
	  NEXT_INSTRUCTION;
	}

	DiscardMark(ch->mark);
	BFR = ch->parent;
	lTop = (LocalFrame)ch;
	NEXT_INSTRUCTION;
      } else	       /* We need GC/shift to move the choice point */
      { struct clause_choice chp;

	DiscardMark(ch->mark);
	BFR = ch->parent;
	chp = ch->value.clause;
	lTop = (LocalFrame)nTop;
	ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);

	if ( chp.cref )
	{ ch = newChoice(CHP_CLAUSE, FR);
	  ch->value.clause = chp;
	} else if ( unlikely(debugstatus.debugging) )
	{ ch = newChoice(CHP_DEBUG, FR);
	}
	NEXT_INSTRUCTION;
      }
    }
  }
  VMH_GOTO(deep_backtrack, PL_TRACE_ACTION_NONE);
}
END_VMH

/* We get  here from FRAME_FAILED  if there  is no matching  clause, a
 * foreign predicate failed or a debugger fail was initiated.  We also
 * get here if the body of an executing clause failed and there are no
 * CHP_JUMP or alternative clauses left.
 *
 * In debug mode, there is normally a CHP_DEBUG choicepoint created to
 * allow the  tracer to  backtrack to  the state at  the start  of the
 * frame.
 */

VMH(deep_backtrack, 1, (int), (trace_action))
{ DEBUG(MSG_BACKTRACK, Sdprintf("BACKTRACKING\n"));

#define LEAVE_FAILED_FRAME(fr) \
  do							      \
  { leaveFrame(FR);					      \
    if ( ison(FR, FR_WATCHED|FR_SSU_DET|FR_DET|FR_DETGUARD) ) \
    { SAVE_REGISTERS(QID);				      \
      frameFailed(FR);					      \
      LOAD_REGISTERS(QID);				      \
      if ( exception_term )				      \
	THROW_EXCEPTION;				      \
    }							      \
  } while(0)

next_choice:
  if ( unlikely(debugstatus.debugging) )
  { if ( trace_action != PL_TRACE_ACTION_NONE ) /* TODO: What if debugstatus */
      goto yield_fail_resume;			/* was changed? */

    if ( BFR->type == CHP_DEBUG && isDebugFrame(BFR->frame, FAIL_PORT) )
    { for(; (void *)FR > (void *)BFR; FR = FR->parent)
	LEAVE_FAILED_FRAME(FR);
      if ( FR == BFR->frame )
      { DEBUG(MSG_BACKTRACK,
	      Sdprintf("FAIL on %s\n", predicateName(FR->predicate)));

	Undo(BFR->mark);
	DiscardMark(BFR->mark);
	BFR = BFR->parent;

	environment_frame = FR;
	FR->clause = NULL;
	lTop = (LocalFrame)argFrameP(FR, FR->predicate->functor->arity);
	SAVE_REGISTERS(QID);
	trace_action = tracePort(FR, BFR, FAIL_PORT, NULL);
	LOAD_REGISTERS(QID);

      yield_fail_resume:
	switch( trace_action )
	{ case PL_TRACE_ACTION_RETRY:
	    environment_frame = FR;
	    DEF = FR->predicate;
	    clear(FR, FR_CAUGHT|FR_SKIPPED);
	    VMH_GOTO(depart_or_retry_continue);
	  case PL_TRACE_ACTION_ABORT:
	    THROW_EXCEPTION;
	  case PL_TRACE_ACTION_YIELD:
	    SAVE_REGISTERS(QID);
	    SOLUTION_RETURN(debug_yield(FAIL_PORT));
	  default:
	    LEAVE_FAILED_FRAME(FR);
	    environment_frame = FR = FR->parent;
	    trace_action = PL_TRACE_ACTION_NONE;
	    goto next_choice;
	}
      }
    }
  }

  for(; (void *)FR > (void *)BFR; FR = FR->parent)
    LEAVE_FAILED_FRAME(FR);

  environment_frame = FR = BFR->frame;
  Undo(BFR->mark);
  QF = QueryFromQid(QID);
  aTop = QF->aSave;
  DEF  = FR->predicate;
#ifdef O_DEBUG_BACKTRACK
  last_choice = BFR->type;
#endif

  if ( unlikely(LD->alerted) )
  { if ( LD->alerted & ALERT_BUFFER )
    { LD->alerted &= ~ALERT_BUFFER;
      release_string_buffers_from_frame(FR);
    }
    if ( UNDO_SCHEDULED(LD) )
    { int rc;

      lTop = (LocalFrame)(BFR+1);
      FR->clause = NULL;
      if ( LD->mark_bar != NO_MARK_BAR )
	LD->mark_bar = gTop;
      SAVE_REGISTERS(QID);
      rc = run_undo_hooks();
      LOAD_REGISTERS(QID);
      if ( !rc )
	THROW_EXCEPTION;
    }
    Profile(profFail(BFR->prof_node));
  }

  switch(BFR->type)
  { case CHP_JUMP:
      DEBUG(MSG_BACKTRACK,
	    Sdprintf("    REDO #%zd: Jump in %s\n",
		     loffset(FR),
		     predicateName(DEF)));
      PC   = BFR->value.pc;
      DiscardMark(BFR->mark);
      lTop = (LocalFrame)(BFR);
      BFR  = BFR->parent;
      ARGP = argFrameP(lTop, 0);
      LD->statistics.inferences++;
      if ( unlikely(LD->alerted) )
      {
#ifdef O_INFERENCE_LIMIT
	if ( LD->statistics.inferences >= LD->inference_limit.limit )
	{ SAVE_REGISTERS(QID);
	  raiseInferenceLimitException();
	  LOAD_REGISTERS(QID);
	  if ( exception_term )
	    THROW_EXCEPTION;
	}
#endif
#ifdef O_DEBUGGER
	if ( debugstatus.debugging && !debugstatus.suspendTrace  )
	{ LocalFrame fr = dbgRedoFrame(FR, CHP_JUMP);

	  if ( fr )
	  { int action;

	    SAVE_REGISTERS(QID);
	    action = tracePort(fr, BFR, REDO_PORT, BFR->value.pc);
	    LOAD_REGISTERS(QID);

	    if ( ison(FR->predicate, P_FOREIGN) &&
		 ( action == PL_TRACE_ACTION_FAIL ||
		   action == PL_TRACE_ACTION_IGNORE ||
		   action == PL_TRACE_ACTION_RETRY ||
		   action == PL_TRACE_ACTION_ABORT
		 ) )
	      discardForeignFrame(FR);

	    LD->trace.yield.redo.is_jump = true;
	    VMH_GOTO(debug_redo_continue, action);
	  }
	}
#endif
      }
      NEXT_INSTRUCTION;
    case CHP_CLAUSE:			/* try next clause */
    { Clause clause;
      struct clause_choice chp = BFR->value.clause;

      DEBUG(MSG_BACKTRACK,
	    Sdprintf("    REDO #%zd: Clause in %s\n",
		     loffset(FR),
		     predicateName(DEF)));
      ARGP = argFrameP(FR, 0);
      DiscardMark(BFR->mark);
      BFR = BFR->parent;
      if ( !(CL = nextClause(&chp, ARGP, FR, DEF)) )
	goto next_choice;	  /* Can happen of look-ahead was too short */

      clause = CL->value.clause;
      PC     = clause->codes;
      lTop   = (LocalFrame)argFrameP(FR, clause->variables);
      UMODE  = uread;

      DEBUG(CHK_SECURE, assert(LD->mark_bar >= gBase && LD->mark_bar <= gTop));

      if ( unlikely(LD->alerted) )
      { if ( is_signalled() )
	{ SAVE_REGISTERS(QID);
	  handleSignals();
	  LOAD_REGISTERS(QID);
	  if ( exception_term )
	    THROW_EXCEPTION;
	}

#ifdef O_INFERENCE_LIMIT
	if ( LD->statistics.inferences >= LD->inference_limit.limit )
	{ SAVE_REGISTERS(QID);
	  raiseInferenceLimitException();
	  LOAD_REGISTERS(QID);
	  if ( exception_term )
	    THROW_EXCEPTION;
	}
#endif

#ifdef O_DEBUGGER
	if ( debugstatus.debugging && !debugstatus.suspendTrace  )
	{ LocalFrame fr = dbgRedoFrame(FR, CHP_CLAUSE);

	  if ( fr )
	  { int action;

	    SAVE_REGISTERS(QID);
	    clearLocalVariablesFrame(FR);
	    action = tracePort(fr, BFR, REDO_PORT, NULL);
	    LOAD_REGISTERS(QID);

	    LD->trace.yield.redo.is_jump = false;
	    LD->trace.yield.redo.chp = chp;
	    VMH_GOTO(debug_redo_continue, action);
	  } else if ( !chp.cref )
	  { newChoice(CHP_DEBUG, FR);
	  }
	}
#endif
      }

      if ( chp.cref )
      { Choice ch = newChoice(CHP_CLAUSE, FR);
	ch->value.clause = chp;
      }
			/* require space for the args of the next frame */
      ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);
      NEXT_INSTRUCTION;
    }
    case CHP_TOP:			/* Query toplevel */
    { DEBUG(MSG_BACKTRACK,
	    Sdprintf("    REDO #%zd: %s: TOP\n",
		     loffset(FR),
		     predicateName(DEF)));
      DiscardMark(BFR->mark);
      QF = QueryFromQid(QID);
      set(QF, PL_Q_DETERMINISTIC);
      QF->foreign_frame = PL_open_foreign_frame();
#if !O_VMI_FUNCTIONS
      assert(LD->exception.throw_environment == &THROW_ENV);
      LD->exception.throw_environment = THROW_ENV.parent;
#endif
      SOLUTION_RETURN(false);
    }
    case CHP_CATCH:			/* catch/3 & setup_call_cleanup/3 */
      DEBUG(MSG_BACKTRACK,
	    Sdprintf("    REDO #%zd: %s: CATCH\n",
		     loffset(FR),
		     predicateName(DEF)));
	    if ( ison(BFR->frame, FR_WATCHED) )
      { DiscardMark(BFR->mark);
	environment_frame = FR = BFR->frame;
	lTop = (LocalFrame)(BFR+1);
	FR->clause = NULL;
	if ( ison(BFR->frame, FR_CLEANUP) )
	{ SAVE_REGISTERS(QID);
	  callCleanupHandler(BFR->frame, FINISH_FAIL);
	  LOAD_REGISTERS(QID);
	} else
	{ set(BFR->frame, FR_CAUGHT);
	}
	if ( exception_term )
	  THROW_EXCEPTION;
      } else
      { set(BFR->frame, FR_CAUGHT);
      }
      DiscardMark(BFR->mark);
      BFR = BFR->parent;
      goto next_choice;
    case CHP_DEBUG:			/* Just for debugging purposes */
      DEBUG(MSG_BACKTRACK,
	    Sdprintf("    REDO #%zd: %s: DEBUG\n",
		     loffset(FR),
		     predicateName(DEF)));
      if ( ison(FR, FR_SSU_DET|FR_DET|FR_DETGUARD) &&
	   BFR->frame == FR )
      { bool rc;
	SAVE_REGISTERS(QID);
	rc = ssu_or_det_failed(FR);
	LOAD_REGISTERS(QID);
	if ( !rc )
	{ assert(exception_term);
	  THROW_EXCEPTION;
	}
      }
      DiscardMark(BFR->mark);
      BFR = BFR->parent;
      goto next_choice;
  }
  assert(0);
  SOLUTION_RETURN(false);
}
END_VMH

		 /*******************************
		 *      YIELD BASED DEBUG       *
		 *******************************/

/* The   helpers   below   implement   continuations   after   calling
 * tracePort().   These  are placed  in  separate  helpers to  support
 * _yielding_ from  the debugger.   The strategy is  the same  for all
 * helpers:
 *
 *   - We jump to the helper after calling tracePort()
 *   - If tracePort returned PL_TRACE_ACTION_YIELD, we yield
 *   - If PL_next_solution() is resumed, we get called again
 *     through VMH(debug_resume), now with the real action.
 */

VMH(debug_call_continue, 1, (int), (action))
{ switch( action )
  { case PL_TRACE_ACTION_FAIL:   FRAME_FAILED;
    case PL_TRACE_ACTION_IGNORE: VMI_GOTO(I_EXIT);
    case PL_TRACE_ACTION_ABORT:  THROW_EXCEPTION;
    case PL_TRACE_ACTION_YIELD:
      SAVE_REGISTERS(QID);
      SOLUTION_RETURN(debug_yield(CALL_PORT));
    case PL_TRACE_ACTION_RETRY:
      if ( debugstatus.retryFrame )
	TRACE_RETRY;		/* otherwise retrying the call-port */
  }				/* is a no-op */

  PC = DEF->codes;
  NEXT_INSTRUCTION;
}
END_VMH

VMH(debug_exit_continue, 1, (int), (action))
{ switch( action )
  { case PL_TRACE_ACTION_RETRY:
      TRACE_RETRY;
    case PL_TRACE_ACTION_FAIL:
      discardChoicesAfter(FR, FINISH_CUT);
      FRAME_FAILED;
    case PL_TRACE_ACTION_ABORT:
      THROW_EXCEPTION;
    case PL_TRACE_ACTION_YIELD:
      SAVE_REGISTERS(QID);
      SOLUTION_RETURN(debug_yield(EXIT_PORT));
  }				/* is a no-op */

  if ( BFR && BFR->type == CHP_DEBUG && BFR->frame == FR )
    BFR = BFR->parent;

  VMH_GOTO(exit_continue);
}
END_VMH

VMH(debug_unify_continue, 1, (int), (action))
{ switch( action )
  { case PL_TRACE_ACTION_RETRY:
      TRACE_RETRY;
    case PL_TRACE_ACTION_FAIL:
      FRAME_FAILED;
    case PL_TRACE_ACTION_ABORT:
      THROW_EXCEPTION;
    case PL_TRACE_ACTION_YIELD:
      SAVE_REGISTERS(QID);
      SOLUTION_RETURN(debug_yield(UNIFY_PORT));
  }
  CHECK_WAKEUP;
  NEXT_INSTRUCTION;
}
END_VMH

VMH(debug_redo_continue, 1, (int), (action))
{ switch( action )
  { case PL_TRACE_ACTION_FAIL:
      FRAME_FAILED;
    case PL_TRACE_ACTION_IGNORE:
      VMI_GOTO(I_EXIT);
    case PL_TRACE_ACTION_RETRY:
      if ( LD->trace.yield.redo.is_jump )
	TRACE_RETRY;
      else
	VMH_GOTO(depart_or_retry_continue);
    case PL_TRACE_ACTION_ABORT:
      THROW_EXCEPTION;
    case PL_TRACE_ACTION_YIELD:
      SAVE_REGISTERS(QID);
      SOLUTION_RETURN(debug_yield(REDO_PORT));
  }

  if ( !LD->trace.yield.redo.is_jump )
  { struct clause_choice chp = LD->trace.yield.redo.chp;

    Clause clause = CL->value.clause;
    PC            = clause->codes;
    lTop          = (LocalFrame)argFrameP(FR, clause->variables);
    UMODE         = uread;

    if ( chp.cref )
    { Choice ch = newChoice(CHP_CLAUSE, FR);
      ch->value.clause = chp;
    } else
    { newChoice(CHP_DEBUG, FR);
    }
    ENSURE_LOCAL_SPACE(LOCAL_MARGIN, THROW_EXCEPTION);
  }

  NEXT_INSTRUCTION;
}
END_VMH

VMH(debug_cut_call_continue, 1, (int), (action))
{ Choice ch;
  mark m;

  switch( action )
  { case PL_TRACE_ACTION_RETRY:
      TRACE_RETRY;
    case PL_TRACE_ACTION_FAIL:
      FRAME_FAILED;
    case PL_TRACE_ACTION_ABORT:
      THROW_EXCEPTION;
    case PL_TRACE_ACTION_YIELD:
      SAVE_REGISTERS(QID);
      SOLUTION_RETURN(debug_yield(CUT_CALL_PORT));
  }

  if ( (ch = findStartChoice(FR, BFR)) )
  { m = ch->mark;
    SAVE_REGISTERS(QID);
    dbg_discardChoicesAfter(FR, FINISH_CUT);
    LOAD_REGISTERS(QID);
    lTop = (LocalFrame) argFrameP(FR, CL->value.clause->variables);
    ch = newChoice(CHP_DEBUG, FR);
    ch->mark = m;
  } else
  { dbg_discardChoicesAfter(FR, FINISH_CUT);
    lTop = (LocalFrame) argFrameP(FR, CL->value.clause->variables);
  }
  ARGP = argFrameP(lTop, 0);
  if ( exception_term )
    THROW_EXCEPTION;

  SAVE_REGISTERS(QID);
  action = tracePort(FR, BFR, CUT_EXIT_PORT, PC);
  LOAD_REGISTERS(QID);

  VMH_GOTO(debug_cut_exit_continue, action);
}
END_VMH

VMH(debug_cut_exit_continue, 1, (int), (action))
{ switch( action )
  { case PL_TRACE_ACTION_RETRY:
      TRACE_RETRY;
    case PL_TRACE_ACTION_FAIL:
      FRAME_FAILED;
    case PL_TRACE_ACTION_ABORT:
      THROW_EXCEPTION;
    case PL_TRACE_ACTION_YIELD:
      SAVE_REGISTERS(QID);
      SOLUTION_RETURN(debug_yield(CUT_EXIT_PORT));
  }

  NEXT_INSTRUCTION;
}
END_VMH

/* We come here  from PL_next_solution() initial code  if the debugger
 * caused  PL_next_solution()  to  return with  PL_S_YIELD_DEBUG.   We
 * simply dispatch to the relevant debug continuation.
 */

VMH(debug_resume, 0, (), ())
{ int port = LD->trace.yield.port;
  int action = LD->trace.yield.resume_action;
  LD->trace.yield.port = NO_PORT;
  LD->trace.yield.resume_action = PL_TRACE_ACTION_NONE;
  if ( LD->trace.yield.nodebug )
  { SAVE_REGISTERS(QID);
    tracemode(false, NULL);
    debugmode(DBG_OFF, NULL);
    LOAD_REGISTERS(QID);
  }
  switch( port )
  { case CALL_PORT:
      VMH_GOTO(debug_call_continue, action);
    case EXIT_PORT:
      VMH_GOTO(debug_exit_continue, action);
    case FAIL_PORT:
      VMH_GOTO(deep_backtrack, action);
    case REDO_PORT:
      VMH_GOTO(debug_redo_continue, action);
    case UNIFY_PORT:
      VMH_GOTO(debug_unify_continue, action);
    case CUT_CALL_PORT:
      VMH_GOTO(debug_cut_call_continue, action);
    case CUT_EXIT_PORT:
      VMH_GOTO(debug_cut_exit_continue, action);
    case EXCEPTION_PORT:
    { term_t catchfr_ref = LD->trace.yield.exception.catchfr_ref;
      Stack outofstack = LD->trace.yield.exception.outofstack;
      bool start_tracer = LD->trace.yield.exception.start_tracer;
      memset(&LD->trace.yield.exception, 0,
	     sizeof(LD->trace.yield.exception));
      VMH_GOTO(b_throw_unwind_debug, catchfr_ref, outofstack, start_tracer,
	       action);
    }
    default:
      assert(0);
      NEXT_INSTRUCTION;		/* keep compiler happy */
  }
}
END_VMH
