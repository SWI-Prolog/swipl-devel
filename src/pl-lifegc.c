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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark the stacks based on reachable data instead of all data.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum a_type
{ A_FRAME,
  A_CHOICE
} a_type;


typedef struct restart
{ struct restart *next;
  Code PC;				/* The restart address */
} restart;


typedef struct a_node
{ struct a_node *next;
  a_type type;
  union
  { struct
    { LocalFrame ptr;
      Code PC;				/* continuation PC from parent */
      restart *restart_list;
      ClauseRef alt_clause;		/* frame has alternate clause */
    } frame;
    struct
    { Choice ptr;
    } choice;
  } value;
} a_node;

#define NODE_PTR(n) (void*)((n)->value.frame.ptr) /* anonymous pointer */

typedef struct visited
{ Code		offset;			/* smallest PC */
  intptr_t       *bits;			/* array of values */
} visited;

typedef struct mark_state
{ a_node  *agenda;			/* frames & choicepoint agenda */
  a_node  *free_nodes;			/* node pool */
  restart *free_restarts;		/* restart pool */
  intptr_t visit_buf[10];		/* Visited for small cases */
} mark_state;


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
  int64_t	multi_envs;		/* environments with continuations */
  int64_t	marked_cont;		/* continuations followed */
  int64_t	c_scanned;		/* Scanned clauses */
  int64_t	vm_scanned;		/* #VM codes scanned */
  int64_t	vm_aborted;		/* scans stopped short */
  int64_t	a_searched;		/* #agenda searches */
  int64_t	a_scanned;		/* #agenda links followed */
} life_count;

static life_count counts;
#define COUNT(g) g

static
PRED_IMPL("gc_statistics", 1, gc_statistics, 0)
{ double as = (double)counts.a_scanned/(double)counts.a_searched;

  int rc = PL_unify_term(A1,
			 PL_FUNCTOR_CHARS, "gc", 7,
			   PL_INT64, counts.marked_envs,
			   PL_INT64, counts.multi_envs,
			   PL_INT64, counts.marked_cont,
			   PL_INT64, counts.c_scanned,
			   PL_INT64, counts.vm_scanned,
			   PL_INT64, counts.vm_aborted,
			   PL_FLOAT, as);
		       
  memset(&counts, 0, sizeof(counts));

  return rc;
}

#else
#define COUNT(g) ((void)0)
#endif

		 /*******************************
		 *	MEMORY MANAGEMENT	*
		 *******************************/

static a_node *
new_node(mark_state *state)
{ a_node *n;

  if ( (n=state->free_nodes) )
  { state->free_nodes = n->next;
    return n;
  }

  n = malloc(sizeof(*n));
  return n;
}


static void
free_node(mark_state *state, a_node *n)
{ n->next = state->free_nodes;
  state->free_nodes = n;
}


static void
free_nodes(mark_state *state)
{ a_node *n = state->free_nodes;
  a_node *n2;

  state->free_nodes = NULL;

  for(; n; n=n2)
  { n2 = n->next;
    free(n);
  }
}


static restart *
new_restart(mark_state *state)
{ restart *r;

  if ( (r=state->free_restarts) )
  { state->free_restarts = r->next;
    return r;
  }

  r = malloc(sizeof(*r));
  return r;
}


static void
free_restart(mark_state *state, restart *r)
{ r->next = state->free_restarts;
  state->free_restarts = r;
}


static void
free_restarts(mark_state *state)
{ restart *r = state->free_restarts;
  restart *r2;

  state->free_restarts = NULL;

  for(; r; r=r2)
  { r2 = r->next;
    free(r);
  }
}


		 /*******************************
		 *	 AGENDA MANAGEMENT	*
		 *******************************/

static int
insert_node(mark_state *state, a_node *n, a_node **address)
{ a_node **p = &state->agenda;
  a_node *n2;
  
  COUNT(counts.a_searched++);

  for(; *p; COUNT(counts.a_scanned++))
  { a_node *o = *p;
    
    if ( NODE_PTR(o) > NODE_PTR(n) )
    { p = &o->next;
    } else if ( NODE_PTR(o) < NODE_PTR(n) )
    { break;
    } else
    { if ( address )
	*address = o;
      return FALSE;
    }    
  }

  n2 = new_node(state);
  memcpy(n2, n, sizeof(*n2));
  n2->next = *p;
  *p = n2;
  if ( address )
    *address = n2;

  return TRUE;
}


static int
a_add_frame(mark_state *state, LocalFrame fr, a_node **node)
{ a_node n;

  n.type = A_FRAME;
  n.value.frame.ptr = fr;
  n.value.frame.restart_list = NULL;
  n.value.frame.PC = NULL;
  n.value.frame.alt_clause = NULL;
  return insert_node(state, &n, node);
}


static int
a_add_choice(mark_state *state, Choice ch, a_node **node)
{ a_node n;

  n.type = A_CHOICE;
  n.value.choice.ptr = ch;
  return insert_node(state, &n, node);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add a restart address. The first is   kept in value.frame.PC and used to
reset uninitialised variables in the environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
add_restart(mark_state *state, a_node *n, Code PC)
{ if ( n->value.frame.PC )
  { restart *r;

    r = new_restart(state);
    r->PC = PC;
    r->next = n->value.frame.restart_list;
    n->value.frame.restart_list = r;
  } else
  { n->value.frame.PC = PC;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Avoid scanning the same code twice:

     - create empty bitmap for code offsets from lowest PC ... code_size
     - Call try_visit() on candidate locations:
     	- Choice
	- Continuation after I_CALL, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_visited(mark_state *state, visited *v, a_node *n)
{ if ( n->value.frame.restart_list )
  { LocalFrame fr = n->value.frame.ptr;
    Clause cl = fr->clause->clause;

    COUNT(counts.multi_envs++);
      
    if ( cl->code_size < 2*sizeof(intptr_t)*8 )
    { v->offset  = cl->codes;
      v->bits    = state->visit_buf;
      v->bits[0] = v->bits[1] = 0;
    } else
    { Code first;
      size_t size, bsize;
      restart *r;

      first = n->value.frame.PC;
      for( r=n->value.frame.restart_list; r; r = r->next)
      { if ( r->PC < first )
	  first = r->PC;
      }
      v->offset = first;
      
      size  = cl->code_size + (v->offset - cl->codes);
      bsize = ((size+sizeof(intptr_t)-1)/sizeof(intptr_t))*sizeof(intptr_t);
      if ( bsize <= sizeof(state->visit_buf) )
	v->bits = state->visit_buf;
      else
	v->bits = malloc(bsize);
      memset(v->bits, 0, bsize);
    }
  } else
    v->bits = NULL;
}


static void
free_visited(mark_state *state, visited *v)
{ if ( v->bits && v->bits != state->visit_buf )
    free(v->bits);
}


static int
try_visit(visited *v, Code PC)
{ if ( v->bits )
  { size_t  at  = PC-v->offset;
    size_t  iat = at/(sizeof(intptr_t)*8);
    intptr_t bit = (intptr_t)1<<(int)(at%(sizeof(intptr_t)*8));

    if ( v->bits[iat] & bit )
    { COUNT(counts.vm_aborted++);
      return FALSE;
    } else
    { v->bits[iat] |= bit;
      return TRUE;
    }
  }

  return TRUE;
}


static void
mark_choice(mark_state *state, Choice ch)
{ a_node *fr_node;

  if ( ch->type != CHP_TOP )
    a_add_frame(state, ch->frame, &fr_node);

  switch(ch->type)
  { case CHP_JUMP:
      DEBUG(1,
	    LocalFrame fr = fr_node->value.frame.ptr;
	    Sdprintf("[%d] %s: Add restart %d\n",
		     levelFrame(fr), predicateName(fr->predicate),
		     ch->value.PC-fr->clause->clause->codes));

      add_restart(state, fr_node, ch->value.PC);
      break;
    case CHP_CLAUSE:
      fr_node->value.frame.alt_clause = ch->value.clause;
      break;
    case CHP_FOREIGN:
    case CHP_TOP:
    case CHP_CATCH:
    case CHP_DEBUG:
    case CHP_NONE:
      break;
  }

  if ( ch->parent )
    a_add_choice(state, ch->parent, NULL);
}


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
  visited  visited;			/* bitmap to check where we have been */
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

  COUNT(counts.marked_cont++);

  for( ; ; PC += (codeTable[op].arguments))
  { op = decode(*PC++);

  again:
    COUNT(counts.vm_scanned++);
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
	mark_argp(state PASS_LD);
	/*FALLTHROUGH*/
      case B_STRING:			/* string + header */
      case B_MPZ:
      { word m = *PC;
	PC += wsizeofInd(m);
	break;
      }

      case I_EXITFACT:
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
	if ( !try_visit(&state->visited, PC) )
	  return PC;
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
	if ( !try_visit(&state->visited, PC) )
	  return PC;
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
	if ( !try_visit(&state->visited, PC) )
	  return PC;
	op = decode(*PC++);
        goto again;
      }

					/* possible continuation point */
      case I_USERCALL0:
      case I_USERCALLN:
      case I_APPLY:
	if ( !try_visit(&state->visited, PC) )
	  return PC;
        break;
      case I_CALL:
      case I_DEPART:
	if ( !try_visit(&state->visited, PC+1) )
	  return PC;
        break;

					/* variable access */
      case B_FIRSTVAR:			/* reset uninitialised */
      case B_ARGFIRSTVAR:
      case C_VAR:
	if ( (state->flags & GCM_CLEAR) )
	{ LocalFrame fr = state->frame;
	  DEBUG(3, Sdprintf("Clear var %d at %d\n", 
			    PC[0]-VAROFFSET(0), (PC-state->c0)-1));
#ifdef O_SECURE
	  if ( !isVar(varFrame(fr, PC[0]) & ~MARK_MASK) )
	  { sysError("ERROR: [%ld] %s: Wrong clear of var %d, PC=%d\n",
		     levelFrame(fr), predicateName(fr->predicate),
		     PC[0]-VAROFFSET(0),
		     (PC-state->c0)-1);
	  } 
#else
	  setVar(varFrame(fr, PC[0]));
#endif
	}
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
	case I_EXITCLEANUP:		/* TBD: we don't have to mark main goal! */
	  mark_arguments(state->frame PASS_LD);
	  state->unmarked = 0;
	  break;
	case I_EXITCATCH:
	  mark_frame_var(state, VAROFFSET(1) PASS_LD); /* The ball */
	  mark_frame_var(state, VAROFFSET(2) PASS_LD); /* recovery goal */
	  break;
	case I_CALL_FV2:
	  mark_frame_var(state, PC[2] PASS_LD);
	  /*FALLTHROUGH*/
	case I_CALL_FV1:
	  mark_frame_var(state, PC[1] PASS_LD);
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
	  if ( state->adepth == 0 )
	    state->ARGP++;
	  break;
	case I_POPF:
	  state->adepth--;
	  break;
	case I_ENTER:
	  assert(state->adepth==0);
	  break;
#endif /*MARK_ALT_CLAUSES*/
    }
  }

  return PC;
}


static void
mark_life_data(mark_state *mstate, a_node *node ARG_LD)
{ restart *r;
  LocalFrame fr = node->value.frame.ptr;
  Code PC;

  if ( (PC=node->value.frame.PC) )
  { walk_state state;

    state.frame    = fr;
    state.c0       = fr->clause->clause->codes;
    state.flags    = GCM_CLEAR;
    state.unmarked = slotsInFrame(fr, PC);
    state.envtop   = argFrameP(fr, state.unmarked);
    init_visited(mstate, &state.visited, node);

    DEBUG(1, Sdprintf("[%ld] %s: continuation walk from PC=%d\n",
		      levelFrame(fr), predicateName(fr->predicate),
		      PC-state.c0));

    walk_and_mark(&state, node->value.frame.PC, I_EXIT PASS_LD);

    while( (r=node->value.frame.restart_list) )
    { node->value.frame.restart_list = r->next;
      
      DEBUG(1, Sdprintf("[%ld] %s: choice walk from PC=%d\n",
			levelFrame(fr), predicateName(fr->predicate),
			r->PC-state.c0));
      state.flags = 0;
      walk_and_mark(&state, r->PC, I_EXIT PASS_LD);
      free_restart(mstate, r);
    }

    free_visited(mstate, &state.visited);
  } else
  { mark_arguments(fr PASS_LD);
  }
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
  state.visited.bits = NULL;
  state.adepth       = 0;
  state.ARGP	     = argFrameP(fr, 0);
  state.envtop	     = state.ARGP + argc;

  DEBUG(2, Sdprintf("Scanning clauses for %s\n", predicateName(fr->predicate)));
  for(; cref && state.unmarked > 0; cref=cref->next)
  { if ( visibleClause(cref->clause, fr->generation) )
    { COUNT(counts.c_scanned++);
      state.c0 = cref->clause->codes;
      DEBUG(3, Sdprintf("Scanning clause %p\n", cref->clause));
      walk_and_mark(&state, state.c0, I_EXIT PASS_LD);
    }

    state.adepth     = 0;
    state.ARGP	     = argFrameP(fr, 0);
  }
}
#endif /*MARK_ALT_CLAUSES*/


#if 0					/* debugging */
static void				/* Only used for comparison */
mark_all_data(LocalFrame fr, Code PC ARG_LD)
{ Word sp = argFrameP(fr, 0);
  int slots = slotsInFrame(fr, PC);

  for( ; slots-- > 0; sp++ )
  { if ( !is_marked(sp) )
    { if ( isGlobalRef(*sp) )
	mark_variable(sp PASS_LD);
      else
	ldomark(sp);  
    }
  }
}
#endif

static void
set_unmarked_to_gced(LocalFrame fr, Code PC)
{ Word sp = argFrameP(fr, 0);
  int slots = slotsInFrame(fr, PC);

  for( ; slots-- > 0; sp++ )
  { if ( !is_marked(sp) )
    { if ( isGlobalRef(*sp) )
      { DEBUG(1, char b[64];
	      Sdprintf("[%ld] %s: GC VAR(%d) (=%s)\n",
		       levelFrame(fr), predicateName(fr->predicate),
		       sp-argFrameP(fr, 0),
		       print_val(*sp, b)));
	if ( LIFE_GC == 1 )
	{ *sp = ATOM_garbage_collected;
	} else
	{ GET_LD
	  mark_variable(sp PASS_LD);
	}
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actually mark the frame, concerning life data.  There are various cases:

    * If this is a foreign frame
	- keep all arguments;
    * If there is a clause choice
	- keep all arguments (TBD: we could check code of alt clauses)
	- keep life data from choices and continuation(s)
    * If there is no clause choice
	- keep life data from choices and continuation(s)

(*) This isn't clear. Obviously we   there  are multiple active children
and the old algorithm for   clearUninitialisedVarsFrame() used the first
one it finds, I.e. the one from the   active  parent check or the latest
choicepoint encountered. Does the first reference guarantee us to do the
same?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
mark_frame(mark_state *state, a_node *node ARG_LD)
{ LocalFrame fr = node->value.frame.ptr;

  DEBUG(1, Sdprintf("Marking [%ld] %s\n",
		    levelFrame(fr), predicateName(fr->predicate)));

  assert(false(fr, FR_MARKED));
  set(fr, FR_MARKED);

  COUNT(counts.marked_envs++);

  if ( fr->parent )
  { a_node *pn_node;

    a_add_frame(state, fr->parent, &pn_node);
    if ( false(fr->parent->predicate, FOREIGN) )
    { DEBUG(2, Sdprintf("%s of [%d] %s from [%d] %s to %d\n",
			(pn_node->value.frame.PC ? "Added restart" : "Set PC"),
			levelFrame(fr->parent),
			predicateName(fr->parent->predicate),
			levelFrame(fr),
			predicateName(fr->predicate),
			fr->programPointer - fr->parent->clause->clause->codes));

      add_restart(state, pn_node, fr->programPointer);
    }
  }

  if ( true(fr->predicate, FOREIGN) )
  { mark_arguments(fr PASS_LD);
  } else
  { mark_life_data(state, node PASS_LD);
    if ( node->value.frame.alt_clause )
    {
#ifdef MARK_ALT_CLAUSES
      mark_alt_clauses(fr, node->value.frame.alt_clause PASS_LD);
#else
      mark_arguments(fr PASS_LD);
#endif
    }
    set_unmarked_to_gced(fr, node->value.frame.PC);
  }

#ifdef O_CALL_RESIDUE
  if ( fr->predicate == PROCEDURE_call_residue_vars2->definition )
  { if ( !LD->gc.marked_attvars )
    { mark_attvars();
      LD->gc.marked_attvars = TRUE;
    }
  }
#endif    
}


static QueryFrame
mark_query_stacks(mark_state *state, LocalFrame fr, Choice ch ARG_LD)
{ a_node *node;
  QueryFrame qf = NULL;

  a_add_frame(state, fr, NULL);
  a_add_choice(state, ch, NULL);

  while((node=state->agenda))
  { state->agenda = node->next;

    switch(node->type)
    { case A_FRAME:
      { LocalFrame fr = node->value.frame.ptr;

	mark_frame(state, node PASS_LD);
	if ( !fr->parent )
	  qf = QueryOfTopFrame(fr);
        
        break;
      }
      case A_CHOICE:
	mark_choice(state, node->value.choice.ptr);
        break;
    }
    
    free_node(state, node);
  }

  assert(qf->magic == QID_MAGIC);
  return qf;
}


static void
mark_stacks(LocalFrame fr, Choice ch)
{ GET_LD
  QueryFrame qf=NULL, pqf=NULL, top = NULL;
  GCTrailEntry te = (GCTrailEntry)tTop - 1;
  FliFrame flictx = fli_context;
  Choice ch0 = ch;
  mark_state state;

  memset(&state, 0, sizeof(state));
  trailcells_deleted = 0;

  while(fr)
  { DEBUG(1, Sdprintf("Marking query %p\n", qf));
    qf = mark_query_stacks(&state, fr, ch PASS_LD);

    if ( pqf )
    { pqf->parent = qf;
    } else if ( !top )
    { top = qf;
    } 
    pqf = qf;

    fr = qf->saved_environment;
    ch = qf->saved_bfr;
  }
  if ( qf )
    qf->parent = NULL;			/* topmost query */

  free_nodes(&state);
  free_restarts(&state);

  te = mark_choicepoints(ch0, te, &flictx);
  for(qf=top; qf; qf=qf->parent)
    te = mark_choicepoints(qf->saved_bfr, te, &flictx);

  for( ; flictx; flictx = flictx->parent)
    te = mark_foreign_frame(flictx, te);

  DEBUG(2, Sdprintf("Trail stack garbage: %ld cells\n", trailcells_deleted));
}
