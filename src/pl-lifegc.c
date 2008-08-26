/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

#define MARK_ALT_CLAUSES 1		/* also walk and mark alternate clauses */

		 /*******************************
		 *	     MARK STACKS	*
		 *******************************/

typedef struct mark_state
{ FliFrame flictx;			/* foreign context for early reset */
  GCTrailEntry reset_entry;		/* Walk trail stack for early reset */
} mark_state;

static void	early_reset_choicepoint(mark_state *state, Choice ch ARG_LD);


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: Compile with  -DGC_COUNTING  to   get  gc_statistics/1  as defined
below. This predicate is NOT THREAD-SAFE!!!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef GC_COUNTING
typedef struct life_count
{ int64_t	marked_envs;		/* environments marked */
  int64_t	marked_cont;		/* continuations followed */
  int64_t	c_scanned;		/* Scanned clauses */
  int64_t	vm_scanned;		/* #VM codes scanned */
} life_count;

static life_count counts;
#define COUNT(f) counts.f++

static
PRED_IMPL("gc_statistics", 1, gc_statistics, 0)
{ int rc = PL_unify_term(A1,
			 PL_FUNCTOR_CHARS, "gc", 4,
			   PL_INT64, counts.marked_envs,
			   PL_INT64, counts.marked_cont,
			   PL_INT64, counts.c_scanned,
			   PL_INT64, counts.vm_scanned);
		       
  memset(&counts, 0, sizeof(counts));

  return rc;
}

#else
#define COUNT(f) ((void)0)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mark_local_variable()

As long as we are a reference link along the local stack, keep marking.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
mark_local_variable(Word p ARG_LD)
{ while ( tagex(*p) == (TAG_REFERENCE|STG_LOCAL) )
  { Word p2;

    p2 = unRef(*p);
    ldomark(p);
    if ( is_marked(p2) )
      return;
    p = p2;
  }

  if ( isGlobalRef(*p) )
    mark_variable(p PASS_LD);
  else
    ldomark(p);
}


static void
mark_arguments(LocalFrame fr ARG_LD)
{ Word sp = argFrameP(fr, 0);
  int slots = fr->predicate->functor->arity;

  for( ; slots-- > 0; sp++ )
  { if ( !is_marked(sp) )
      mark_local_variable(sp PASS_LD);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
walk_and_mark(walk_state *state, Code PC, code end, Code until)
    Walk along the byte code starting at PC and continuing until either
    it finds instruction `end' or the `until' address in code.  Returns
    the next instruction to process,

See decompileBody for details on handling the branch instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct walk_state
{ LocalFrame frame;			/* processing node */
  int flags;				/* general flags */
  Code c0;				/* start of code list */
  Word envtop;				/* just above environment */
  int unmarked;				/* left when marking alt clauses */
#ifdef MARK_ALT_CLAUSES
  Word ARGP;				/* head unify instructions */
  int  adepth;				/* ARGP nesting */
#endif
} walk_state;

#define GCM_CLEAR	0x1		/* Clear uninitialised data */
#define GCM_ALTCLAUSE	0x2		/* Marking alternative clauses */

static inline void
mark_frame_var(walk_state *state, code v ARG_LD)
{ Word sp = varFrameP(state->frame, v);

  if ( sp < state->envtop && !is_marked(sp) )
  { mark_local_variable(sp PASS_LD);
    state->unmarked--;
  }
}


static inline void
clear_frame_var(walk_state *state, Code PC ARG_LD)
{ if ( (state->flags & GCM_CLEAR) )
  { LocalFrame fr = state->frame;
    DEBUG(3, Sdprintf("Clear var %d at %d\n", 
		      PC[0]-VAROFFSET(0), (PC-state->c0)-1));
#ifdef O_SECURE
    { Word vp = varFrameP(fr, PC[0]);

      if ( !isVar(*vp & ~MARK_MASK) )
      { Sdprintf("ERROR: [%ld] %s: Wrong clear of var %d, PC=%d\n",
		 levelFrame(fr), predicateName(fr->predicate),
		 PC[0]-VAROFFSET(0),
		 (PC-state->c0)-1);
      }
    } 
#else
    setVar(varFrame(fr, PC[0]));
#endif
  }
}


static inline void
mark_argp(walk_state *state ARG_LD)
{
#ifdef MARK_ALT_CLAUSES
  if ( state->adepth == 0 )
  { if ( !is_marked(state->ARGP) )
    { mark_local_variable(state->ARGP PASS_LD);
      state->unmarked--;
    }
    state->ARGP++;
  }
#endif
}


static Code
walk_and_mark(walk_state *state, Code PC, code end ARG_LD)
{ code op;

  COUNT(marked_cont);

  for( ; ; PC += (codeTable[op].arguments))
  { op = decode(*PC++);

  again:
    DEBUG(3, Sdprintf("\t%s\n", codeTable[op].name));
    COUNT(vm_scanned);
    if ( op == end )
    { PC--;
      return PC;
    }

    switch( op )
    {
#if O_DEBUGGER
      case D_BREAK:
	op = decode(replacedBreak(PC-1));
        goto again;
#endif
					/* dynamically sized objects */
      case H_STRING:			/* only skip the size of the */
      case H_MPZ:
      case A_MPZ:
	mark_argp(state PASS_LD);
	/*FALLTHROUGH*/
      case B_STRING:			/* string + header */
      case B_MPZ:
      { word m = *PC;
	PC += wsizeofInd(m)+1;
	assert(codeTable[op].arguments == VM_DYNARGC);
	PC -= VM_DYNARGC;		/* compensate for for-step */
	break;
      }

      case I_EXITQUERY:
      case I_EXITFACT:
      case I_FEXITDET:
      case I_FEXITNDET:
      case I_FREDO:
      case S_TRUSTME:			/* Consider supervisor handling! */
      case S_LIST:
      case S_NEXTCLAUSE:
	return PC-1;

      case C_JMP:			/* unconditional jump */
	if ( (state->flags & GCM_ALTCLAUSE) )
	  break;
	PC += (int)PC[0]+1;
        op = decode(*PC++);
        goto again;
					/* Control-structures */
      case C_OR:
	if ( (state->flags & GCM_ALTCLAUSE) )
	  break;
      { Code alt = PC+PC[0]+1;
	DEBUG(3, Sdprintf("C_OR at %d\n", PC-state->c0-1));
	PC++;				/* skip <n> */
	walk_and_mark(state, PC, C_JMP PASS_LD);
	PC = alt;
	op = decode(*PC++);
        goto again;
      }
      case C_NOT:
	if ( (state->flags & GCM_ALTCLAUSE) )
	  break;
      { Code alt = PC+PC[1]+2;
	DEBUG(3, Sdprintf("C_NOT at %d\n", PC-state->c0-1));
	PC += 2;			/* skip the two arguments */
	walk_and_mark(state, PC, C_CUT PASS_LD);
	PC = alt;
	op = decode(*PC++);
        goto again;
      }
      case C_SOFTIF:
      case C_IFTHENELSE:
	if ( (state->flags & GCM_ALTCLAUSE) )
	  break;
      { Code alt = PC+PC[1]+2;
	DEBUG(3, Sdprintf("C_IFTHENELSE at %d\n", PC-state->c0-1));
	PC += 2;			/* skip the 'MARK' variable and jmp */
	walk_and_mark(state, PC, C_JMP PASS_LD);
	PC = alt;
	op = decode(*PC++);
        goto again;
      }

					/* variable access */
      case B_UNIFY_FIRSTVAR:
      case B_UNIFY_VAR:
	state->ARGP = varFrameP(state->frame, PC[0]);
        state->adepth = 0;
	if ( op == B_UNIFY_VAR )
	  break;
	/*FALLTHROUGH*/
      case B_FIRSTVAR:			/* reset uninitialised */
      case B_ARGFIRSTVAR:
      case A_FIRSTVAR_IS:
      case C_VAR:
	clear_frame_var(state, PC PASS_LD);
	break;
      case H_LIST_FF:
	mark_argp(state PASS_LD);
        /*FALLTHROUGH*/
      case B_UNIFY_FF:
	clear_frame_var(state, PC+0 PASS_LD);
	clear_frame_var(state, PC+1 PASS_LD);
	break;
      case B_UNIFY_FV:
	clear_frame_var(state, PC+0 PASS_LD);
	mark_frame_var(state, PC[1] PASS_LD);
	break;
      case B_UNIFY_VV:
      case B_EQ_VV:
	mark_frame_var(state, PC[0] PASS_LD);
        mark_frame_var(state, PC[1] PASS_LD);
	break;

      { size_t index;			/* mark variable access */

	case B_ARGVAR:
	case A_VAR:
	case B_VAR:	    index = *PC;		goto var_common;
	case A_VAR0:
	case B_VAR0:	    index = VAROFFSET(0);	goto var_common;
	case A_VAR1:
	case B_VAR1:	    index = VAROFFSET(1);	goto var_common;
	case A_VAR2:
	case B_VAR2:	    index = VAROFFSET(2);	var_common:
	  mark_frame_var(state, index PASS_LD);
	  break;
      }
	case I_CALLCLEANUP:
	  mark_frame_var(state, VAROFFSET(1) PASS_LD); /* main goal */
	  break;
	case I_EXITCLEANUP:
	  mark_frame_var(state, VAROFFSET(2) PASS_LD); /* The ball */
	  mark_frame_var(state, VAROFFSET(3) PASS_LD); /* cleanup goal */
	  break;
	case I_EXITCATCH:
	  mark_frame_var(state, VAROFFSET(1) PASS_LD); /* The ball */
	  mark_frame_var(state, VAROFFSET(2) PASS_LD); /* recovery goal */
	  break;
#ifdef MARK_ALT_CLAUSES
	case H_VAR:
	  mark_frame_var(state, PC[0] PASS_LD);
	  /*FALLTHROUGH*/
	case H_FIRSTVAR:
	case H_CONST:
	case H_NIL:
	case H_INTEGER:
	case H_INT64:
	case H_FLOAT:
	  mark_argp(state PASS_LD);
	  break;
	case H_FUNCTOR:
	case H_LIST:
	  mark_argp(state PASS_LD);
	  state->adepth++;
	  break;
	case H_VOID:
	  assert(state->adepth == 0);
	  state->ARGP++;
	  break;
	case H_POP:
	case B_POP:
	  state->adepth--;
	  break;
	case B_UNIFY_EXIT:
	case I_ENTER:
	  assert(state->adepth==0);
	  break;
#endif /*MARK_ALT_CLAUSES*/
    }
  }

  return PC;
}


#ifdef MARK_ALT_CLAUSES
static void
mark_alt_clauses(LocalFrame fr, ClauseRef cref ARG_LD)
{ Word sp = argFrameP(fr, 0);
  int argc = fr->predicate->functor->arity;
  int i;
  walk_state state;
  state.unmarked = 0;

  for(i=0; i<argc; i++ )
  { if ( !is_marked(&sp[i]) )
      state.unmarked++;
  }

  if ( !state.unmarked )
    return;

  state.frame	     = fr;
  state.flags        = GCM_ALTCLAUSE;
  state.adepth       = 0;
  state.ARGP	     = argFrameP(fr, 0);
  state.envtop	     = state.ARGP + argc;

  DEBUG(2, Sdprintf("Scanning clauses for %s\n", predicateName(fr->predicate)));
  for(; cref && state.unmarked > 0; cref=cref->next)
  { if ( visibleClause(cref->clause, fr->generation) )
    { COUNT(c_scanned);
      state.c0 = cref->clause->codes;
      DEBUG(3, Sdprintf("Scanning clause %p\n", cref->clause));
      walk_and_mark(&state, state.c0, I_EXIT PASS_LD);
    }

    state.adepth     = 0;
    state.ARGP	     = argFrameP(fr, 0);
  }
}

#else /*MARK_ALT_CLAUSES*/

static void
mark_alt_clauses(LocalFrame fr, ClauseRef cref ARG_LD)
{ mark_arguments(fr PASS_LD);
}

#endif /*MARK_ALT_CLAUSES*/

static void
early_reset_choicepoint(mark_state *state, Choice ch ARG_LD)
{ LocalFrame fr = ch->frame;
  Word top;

  while((char*)state->flictx > (char*)ch)
  { FliFrame fli = state->flictx;

    state->reset_entry = mark_foreign_frame(fli, state->reset_entry);
    state->flictx = fli->parent;
  }

  if ( ch->type == CHP_CLAUSE )
  { top = argFrameP(fr, fr->predicate->functor->arity);
  } else
  { assert(ch->type == CHP_TOP || (void *)ch > (void *)fr);
    top = (Word)ch;
  }

  state->reset_entry = early_reset_vars(&ch->mark, top, state->reset_entry PASS_LD);
  needsRelocation(&ch->mark.trailtop);
  alien_into_relocation_chain(&ch->mark.trailtop,
			      STG_TRAIL, STG_LOCAL PASS_LD);
  SECURE(trailtops_marked--);
}


static QueryFrame mark_environments(mark_state *state, LocalFrame fr, Code PC ARG_LD);

static void
mark_choicepoints(mark_state *state, Choice ch ARG_LD)
{ for(; ch; ch=ch->parent)
  { early_reset_choicepoint(state, ch PASS_LD);

    switch(ch->type)
    { case CHP_JUMP:
	mark_environments(state, ch->frame, ch->value.PC PASS_LD);
	break;
      case CHP_CLAUSE:
      { LocalFrame fr = ch->frame;

	mark_alt_clauses(fr, ch->value.clause PASS_LD);
        if ( false(fr, FR_MARKED) )
	{ set(fr, FR_MARKED);
	  COUNT(marked_envs);
	  check_call_residue(fr PASS_LD);
	  mark_environments(state, fr->parent, fr->programPointer PASS_LD);
	}
	break;
      }
      case CHP_DEBUG:
      case CHP_CATCH:
	mark_environments(state, ch->frame, NULL PASS_LD);
	break;
      case CHP_TOP:
	break;
    }
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) We need to mark  the  top   frame  to  deal  with foreign predicates
calling  Prolog  back  that  can  leak    term-handles   of  the  parent
environment. This came from Roberto Bagnara   and  was simplfied to this
program, which must write foo(0).

test :- c_bind(X), writeln(X).
bind(X) :- X = foo(0), garbage_collect.

static foreign_t
bind(term_t arg)
{ predicate_t pred = PL_predicate("bind", 1, "user");

  return PL_call_predicate(NULL, PL_Q_NORMAL, pred, arg);
}

install_t
install()
{ PL_register_foreign("c_bind", 1, bind, 0);
}
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
mark_environments(mark_state *state, LocalFrame fr, Code PC ARG_LD)
{ QueryFrame qf = NULL;

  while(fr)
  { walk_state state;

    if ( false(fr, FR_MARKED) )
    { set(fr, FR_MARKED);
      state.flags = GCM_CLEAR;

      COUNT(marked_envs);
      check_call_residue(fr PASS_LD);
    } else
    { state.flags = 0;
    }

    if ( true(fr->predicate, FOREIGN) || PC == NULL )
    { DEBUG(2, Sdprintf("Marking arguments for [%d] %s\n",
			levelFrame(fr), predicateName(fr->predicate)));
      mark_arguments(fr PASS_LD);
    } else
    { state.frame    = fr;
      state.unmarked = slotsInFrame(fr, PC);
      state.envtop   = argFrameP(fr, state.unmarked);
      state.c0       = fr->clause->clause->codes;

      DEBUG(2, Sdprintf("Walking code for [%d] %s from PC=%d\n",
			levelFrame(fr), predicateName(fr->predicate),
			PC-state.c0));
	
      walk_and_mark(&state, PC, I_EXIT PASS_LD);
    }

    if ( !(state.flags&GCM_CLEAR) )	/* from choicepoint */
      return NULL;

    if ( fr->parent )
    { PC = fr->programPointer;
      fr = fr->parent;
    } else
    { qf = queryOfFrame(fr);

      if ( qf->saved_environment )
	mark_arguments(qf->saved_environment PASS_LD); /* (*) */

      break;
    }
  }

  return qf;
}



static QueryFrame
mark_query_stacks(mark_state *state, LocalFrame fr, Choice ch, Code PC ARG_LD)
{ QueryFrame qf;

  qf = mark_environments(state, fr, PC PASS_LD);
  mark_choicepoints(state, ch PASS_LD);

  return qf;
}


static void
mark_stacks(LocalFrame fr, Choice ch)
{ GET_LD
  QueryFrame qf=NULL;
  mark_state state;
  Code PC = NULL;

  memset(&state, 0, sizeof(state));
  state.reset_entry = (GCTrailEntry)tTop - 1;
  state.flictx = fli_context;
  trailcells_deleted = 0;

  while(fr)
  { DEBUG(1, Sdprintf("Marking query %p\n", qf));
    qf = mark_query_stacks(&state, fr, ch, PC PASS_LD);

    if ( qf )
    { fr = qf->saved_environment;
      ch = qf->saved_bfr;
      PC = qf->saved_PC;
    } else
      break;
  }

  for( ; state.flictx; state.flictx = state.flictx->parent)
    state.reset_entry = mark_foreign_frame(state.flictx, state.reset_entry);

  DEBUG(2, Sdprintf("Trail stack garbage: %ld cells\n", trailcells_deleted));
}
