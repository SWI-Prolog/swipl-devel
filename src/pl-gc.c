/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

#ifdef SECURE_GC
#define O_DEBUG 1
#define O_SECURE 1
#endif
#include "pl-incl.h"

#ifndef HAVE_MEMMOVE			/* Note order!!!! */
#define memmove(dest, src, n) bcopy(src, dest, n)
#endif
#undef ulong
#define ulong unsigned long

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is based on

    Karen Appleby, Mats Carlsson, Seif Haridi and Dan Sahlin
    ``Garbage Collection for Prolog Based on WAM''
    Communications of the ACM, June 1988, vol. 31, No. 6, pages 719-741.

Garbage collection is invoked if the WAM  interpreter  is  at  the  call
port.   This  implies  the current environment has its arguments filled.
For the moment we assume the other  reachable  environments  are  filled
completely.   There  is  room  for some optimisations here.  But we will
exploit these later.

The sole fact that the garbage collector can  only  be  invoked  if  the
machinery  is  in a well known phase of the execution is irritating, but
sofar I see no solutions around this, nor have had any indications  from
other  Prolog implementors or the literature that this was feasible.  As
a consequence however, we should start the garbage collector well before
the system runs out of memory.

In theory, we could have the compiler calculating the maximum amount  of
global   stack   data  created  before  the  next  `save  point'.   This
unfortunately is not possible for the trail stack, which  also  benifits
from  a  garbage  collection pass.  Furthermore, there is the problem of
foreign code creating global stack data (=../2, name/2, read/1, etc.).


		  CONSEQUENCES FOR THE VIRTUAL MACHINE

The virtual machine interpreter now should ensure the stack  frames  are
in  a predicatable state.  For the moment, this implies that all frames,
except for the current one (which only has its arguments filled)  should
be  initialised fully.  I'm not yet sure whether we can't do better, but
this is simple and safe and allows us to  debug  the  garbage  collector
first before starting on the optimisations.


		CONSEQUENCES FOR THE DATA REPRESENTATION

The garbage collector needs two bits on each cell of `Prolog  data'.   I
decided  to  use the low order two bits for this.  The advantage of this
that pointers to word aligned data are not affected (at least on 32 bits
machines.  Unfortunately, you will have to use 4 bytes alignment  on  16
bits  machines  now  as  well).   This demand only costs us two bits for
integers, which are now shifted two bits to the left when stored on  the
stack.   The  normal  Prolog machinery expects the lower two bits of any
Prolog data object to be zero.  The  garbage  collection  part  must  be
carefull to strip of these two bits before operating on the data.

Finally, for the compacting phase we should be able to scan  the  global
stack  both  upwards  and downwards while identifying the objects in it.
This implies reals are  now  packed  into  two  words  and  strings  are
surrounded by a word at the start and end, indicating their length.

			      DEBUGGING

Debugging a garbage collector is a difficult job.  Bugs --like  bugs  in
memory  allocation--  usually  cause  crashes  long  after  the  garbage
collection has finished.   To  simplify  debugging  a  large  number  of
actions  are  counted  during garbage collection.  At regular points the
consistency between these counts  is  verified.   This  causes  a  small
performance degradation, but for the moment is worth this I think.

If the O_SECURE cpp flag is set  some  additional  expensive  consistency
checks  that need considerable amounts of memory and cpu time are added.
Garbage collection gets about 3-4 times as slow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Marking, testing marks and extracting values from GC masked words.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GC_MASK		(MARK_MASK|FIRST_MASK)
#define VALUE_MASK	(~GC_MASK)

#if O_SECURE
char tmp[256];				/* for calling print_val(), etc. */
#define check_relocation(p) do_check_relocation(p, __FILE__, __LINE__)
#define recordMark(p)   { if ( (p) < gTop ) *mark_top++ = (p); }
#else
#define recordMark(p)
#define needsRelocation(p) { needs_relocation++; }
#define check_relocation(p)
#endif

#define ldomark(p)	{ *(p) |= MARK_MASK; }
#define domark(p)	{ if ( marked(p) ) \
			    sysError("marked twice: 0x%p (*= 0x%lx), gTop = 0x%p", p, *(p), gTop); \
			  *(p) |= MARK_MASK; \
			  total_marked++; \
			  recordMark(p); \
			  DEBUG(4, Sdprintf("marked(0x%p)\n", p)); \
			}
#define unmark(p)	(*(p) &= ~MARK_MASK)
#define marked(p)	(*(p) & MARK_MASK)

#define mark_first(p)	(*(p) |= FIRST_MASK)
#define unmark_first(p)	(*(p) &= ~FIRST_MASK)
#define is_first(p)	(*(p) & FIRST_MASK)
#define is_ref(w)	isRef(w)

#define get_value(p)	(*(p) & VALUE_MASK)
#define set_value(p, w)	{ *(p) &= GC_MASK; *(p) |= w; }
#define val_ptr2(w, s)	((Word)((ulong)valPtr2((w), (s)) & ~0x3L))
#define val_ptr(w)	val_ptr2((w), storage(w))

#define inShiftedArea(area, shift, ptr) \
	((char *)ptr >= (char *)LD->stacks.area.base + shift && \
	 (char *)ptr <  (char *)LD->stacks.area.max + shift )
#define topPointerOnStack(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.max)

#define onGlobal(p)	onStackArea(global, p) /* onStack()? */
#define onLocal(p)	onStackArea(local, p)
#define onTrail(p)	topPointerOnStack(trail, p)

#ifndef offset
#define offset(s, f) ((int)(&((struct s *)NULL)->f))
#endif

#define ttag(x)		(((word)(x))&TAG_TRAILMASK)

		 /*******************************
		 *     FUNCTION PROTOTYPES	*
		 *******************************/

forwards void		mark_variable(Word);
forwards void		sweep_foreign(void);
forwards QueryFrame	mark_environments(LocalFrame, Code PC);
forwards void		update_relocation_chain(Word, Word);
forwards void		into_relocation_chain(Word, int stg);
forwards void		alien_into_relocation_chain(void *addr,
						    int orgst, int stg);
forwards void		compact_trail(void);
forwards void		sweep_mark(mark *);
forwards void		sweep_trail(void);
forwards bool		is_downward_ref(Word);
forwards bool		is_upward_ref(Word);
forwards void		compact_global(void);

#if O_SECURE
forwards int		cmp_address(const void *, const void *);
forwards void		do_check_relocation(Word, char *file, int line);
forwards void		needsRelocation(void *);
forwards bool		scan_global(int marked);
forwards void		check_mark(mark *m);
#endif

		/********************************
		*           GLOBALS             *
		*********************************/

#define	total_marked	   (LD->gc._total_marked)
#define	trailcells_deleted (LD->gc._trailcells_deleted)
#define	relocation_chains  (LD->gc._relocation_chains)
#define	relocation_cells   (LD->gc._relocation_cells)
#define	relocated_cells	   (LD->gc._relocated_cells)
#define	needs_relocation   (LD->gc._needs_relocation)
#define	local_marked	   (LD->gc._local_marked)
#define	marks_swept	   (LD->gc._marks_swept)
#define	marks_unswept	   (LD->gc._marks_unswept)
#define	alien_relocations  (LD->gc._alien_relocations)
#define local_frames	   (LD->gc._local_frames)
#define choice_count	   (LD->gc._choice_count)
#if O_SECURE
static long trailtops_marked;		/* # marked trailtops */
static Word *mark_base;			/* Array of marked cells addresses */
static Word *mark_top;			/* Top of this array */
static Table check_table = NULL;	/* relocation address table */
#endif

#undef LD
#define LD LOCAL_LD

#if O_SECURE
		/********************************
		*           DEBUGGING           *
		*********************************/

static void
needsRelocation(void *addr)
{ GET_LD

  needs_relocation++;

  addHTable(check_table, addr, (Void) TRUE);
}


static char *
print_adr(Word adr, char *buf)
{ GET_LD
  char *name;
  Word base;

  if ( onGlobal(adr) )
  { name = "global";
    base = gBase;
  } else if ( onLocal(adr) )
  { name = "local";
    base = (Word) lBase;
  } else if ( onTrail(adr) )
  { name = "trail";
    base = (Word) tBase;
  } else
  { Ssprintf(buf, "%p", adr);
    return buf;
  }

  Ssprintf(buf, "%p=%s(%d)", adr, name, adr-base);
  return buf;
}


static char *
print_val(word val, char *buf)
{ GET_LD
  char *tag_name[] = { "var", "float", "int", "atom",
		       "string", "list", "term", "ref" };
  char *stg_name[] = { "static/inline/trail", "global", "local", "reserved" };

  Ssprintf(buf, "%s at %s(%ld)",
	   tag_name[tag(val)],
	   stg_name[storage(val) >> 3],
	   val >> LMASK_BITS);
  if ( val & MARK_MASK )
    strcat(buf, "M");
  if ( val & FIRST_MASK )
    strcat(buf, "F");

  return buf;
}

static void
do_check_relocation(Word addr, char *file, int line)
{ GET_LD
  Symbol s;

  if ( !(s=lookupHTable(check_table, addr)) )
  { char buf1[256];
    char buf2[256];
    sysError("%s:%d: Address %s (%s) was not supposed to be relocated",
	     file, line, print_adr(addr, buf1), print_val(*addr, buf2));
    return;
  }

  if ( !s->value )
  { sysError("%s:%d: Relocated twice: 0x%lx", file, line, addr);
    return;
  }

  s->value = FALSE;
}

#endif /* O_SECURE */

		/********************************
		*          UTILITIES            *
		*********************************/

static inline int
isGlobalRef(word w)
{ return storage(w) == STG_GLOBAL;
}


static inline int
offset_cell(Word p)
{ word m = *p;				/* was get_value(p) */
  int offset;

  if ( storage(m) == STG_LOCAL )
    offset = wsizeofInd(m) + 1;
  else
    offset = 0;

  return offset;
}


static inline Word
previous_gcell(Word p)
{ p--;
  return p - offset_cell(p);
}


static inline word
makePtr(Word ptr, int tag)
{ GET_LD
  int stg;

  if ( onStackArea(global, ptr) )
    stg = STG_GLOBAL;
  else if ( onStackArea(local, ptr) )
    stg = STG_LOCAL;
  else
  { assert(onStackArea(trail, ptr));
    stg = STG_TRAIL;
  }

  return consPtr(ptr, tag|stg);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clear the FR_MARKED flags left after traversing all reachable frames.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
unmark_environments(PL_local_data_t *ld, LocalFrame fr)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { if ( false(fr, FR_MARKED) )
      return NULL;
    clear(fr, FR_MARKED);
    ld->gc._local_frames--;
    
    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
      return (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
  }
}


static void
unmark_choicepoints(PL_local_data_t *ld, Choice ch)
{ for( ; ch; ch = ch->parent )
  { ld->gc._choice_count--;
    unmark_environments(ld, ch->frame);
  }
}


static void
unmark_stacks(PL_local_data_t *ld, LocalFrame fr, Choice ch)
{ QueryFrame qf;

  for( ; fr; fr = qf->saved_environment, ch = qf->saved_bfr )
  { qf = unmark_environments(ld, fr);
    assert(qf->magic == QID_MAGIC);
    unmark_choicepoints(ld, ch);
  }
}



		/********************************
		*            MARKING            *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void mark_variable(start)
     Word start;

After the marking phase has been completed, the following statements are
supposed to hold:

    - All non-garbage cells on the local- and global stack are
      marked.
    - `total_marked' equals the size of the global stack AFTER
      compacting (e.i. the amount of non-garbage) in words.
    - `needs_relocation' holds the total number of references from the
      argument- and local variable fields of the local stack and the
      internal global stack references that need be relocated. This
      number is only used for consistency checking with the relocation
      statistic obtained during the compacting phase.

The marking algorithm forms a two-state machine. While going deeper into
the reference tree, the pointers are reversed  and the FIRST_MASK is set
to indicate the choice points created by   complex terms with arity > 1.
Also the actual mark bit is set on the   cells. If a leaf is reached the
process reverses, restoring the  old  pointers.   If  a  `first' mark is
reached we are either finished, or have reached a choice point, in which
case  the  alternative  is  the  cell   above  (structures  are  handled
last-argument-first).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FORWARD		goto forward
#define BACKWARD	goto backward

static void
mark_variable(Word start)
{ GET_LD
  Word current;				/* current cell examined */
  word val;				/* old value of current cell */
  Word next;				/* cell to be examined */

  DEBUG(3, Sdprintf("marking 0x%p\n", start));

  if ( marked(start) )
    sysError("Attempt to mark twice");

  local_marked++;
  current = start;
  mark_first(current);
  val = get_value(current);  
  total_marked--;			/* do not count local stack cell */
  FORWARD;

forward:				/* Go into the tree */
  if ( marked(current) )		/* have been here */
    BACKWARD;
  domark(current);

  switch(tag(val))
  { case TAG_REFERENCE:
    { next = unRef(val);		/* address pointing to */
      if ( next < gBase )
	sysError("REF pointer to 0x%p\n", next);
      needsRelocation(current);
      if ( is_first(next) )		/* ref to choice point. we will */
        BACKWARD;			/* get there some day anyway */
      val  = get_value(next);		/* invariant */
      set_value(next, makeRef(current));/* create backwards pointer */
      DEBUG(5, Sdprintf("Marking REF from 0x%p to 0x%p\n", current, next));
      current = next;			/* invariant */
      FORWARD;
    }
    case TAG_COMPOUND:
    { int args;

      SECURE(assert(storage(val) == STG_GLOBAL));
      next = valPtr2(val, STG_GLOBAL);
      needsRelocation(current);
      if ( marked(next) )
	BACKWARD;			/* term has already been marked */
      args = arityFunctor(((Functor)next)->definition) - 1;
      DEBUG(5, Sdprintf("Marking TERM %s/%d at 0x%p\n",
			stringAtom(nameFunctor(((Functor)next)->definition)),
			args+1, next));
      domark(next);
      for( next += 2; args > 0; args--, next++ )
	mark_first(next);
      next--;				/* last cell of term */
      val = get_value(next);		/* invariant */
					/* backwards pointer (NO ref!) */
      set_value(next, makePtr(current, TAG_COMPOUND));
      current = next;
      FORWARD;
    }
    case TAG_INTEGER:
      if ( storage(val) == STG_INLINE )
	BACKWARD;
    case TAG_STRING:
    case TAG_FLOAT:			/* indirects */
    { next = valPtr2(val, STG_GLOBAL);

      SECURE(assert(storage(val) == STG_GLOBAL));
      needsRelocation(current);
      if ( marked(next) )		/* can be referenced from multiple */
        BACKWARD;			/* places */
      domark(next);
      DEBUG(3, Sdprintf("Marked indirect data type, size = %ld\n",
			offset_cell(next) + 1));
      total_marked += offset_cell(next);
    }
  }
  BACKWARD;

backward:  				/* reversing backwards */
  while( !is_first(current) )
  { word w = get_value(current);

    next = valPtr(w);
    set_value(current, val);
    if ( isRef(w) )
      val = makeRef(current);
    else
      val = makePtr(current-1, TAG_COMPOUND);
    current= next;
  }

  unmark_first(current);
  if ( current == start )
    return;

  { word tmp;

    tmp = get_value(current);
    set_value(current, val);		/* restore old value */
    current--;
    val = get_value(current);		/* invariant */
    set_value(current, tmp);
    FORWARD;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
References from foreign code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
mark_term_refs()
{ GET_LD
  FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
  { Word sp = refFliP(fr, 0);
    int n = fr->size;

    for( ; n-- > 0; sp++ )
    { if ( !marked(sp) )		/* can this be marked?? */
      { if ( isGlobalRef(*sp) )
	  mark_variable(sp);
	else
	  ldomark(sp);      
      }
    }
  }
}


static void
mark_foreign_trail_refs()
{ GET_LD
  FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
  { needsRelocation(&fr->mark.trailtop);
    alien_into_relocation_chain(&fr->mark.trailtop, STG_TRAIL, STG_LOCAL);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
clearUninitialisedVarsFrame(LocalFrame fr, Code PC);

Assuming the clause associated will resume   execution  at PC, determine
the variables that are not yet initialised and set them to be variables.
This  avoids  the  garbage  collector    considering  the  uninitialised
variables.

[Q] wouldn't it be better to track  the variables that *are* initialised
and consider the others to be not?  Might   take more time, but might be
more reliable and simpler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
clearUninitialisedVarsFrame(LocalFrame fr, Code PC)
{ if ( PC != NULL )
  { code c;

    for( ; ; PC += (codeTable[c].arguments + 1))
    { c = decode(*PC);

    again:
      switch( c )
      {
#if O_DEBUGGER
	case D_BREAK:
	  c = decode(replacedBreak(PC));
	  goto again;
#endif
	case H_INDIRECT:		/* only skip the size of the */
	case B_INDIRECT:		/* string + header */
	{ word m = PC[1];
	  PC += wsizeofInd(m)+1;
	  break;
	}
	case I_EXIT:
	case I_EXITFACT:
	  return;
	case C_JMP:
	  PC += (int)PC[1]+2;
	  c = decode(*PC);
	  goto again;
	case B_FIRSTVAR:
	case B_ARGFIRSTVAR:
	case C_VAR:
#if O_SECURE	  
	  if ( varFrameP(fr, PC[1]) <
	       argFrameP(fr, fr->predicate->functor->arity) )
	    sysError("Reset instruction on argument");
	  assert(varFrame(fr, PC[1]) != QID_MAGIC);
#endif
	  setVar(varFrame(fr, PC[1]));
	  break;
      }
    }
  }
}


static inline int
slotsInFrame(LocalFrame fr, Code PC)
{ Definition def = fr->predicate;

  if ( !PC || true(def, FOREIGN) || !fr->clause )
    return def->functor->arity;

  return fr->clause->clause->prolog_vars;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark environments and all data that can   be  reached from them. Returns
the QueryFrame that started this environment,  which provides use access
to the parent `foreign' environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
mark_environments(LocalFrame fr, Code PC)
{ if ( !fr )
    return NULL;

  for( ; ; )
  { int slots;
    Word sp;
#if O_SECURE
    int oslots;
#endif

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    
    DEBUG(3, Sdprintf("Marking [%ld] %s\n",
		      levelFrame(fr), predicateName(fr->predicate)));

    clearUninitialisedVarsFrame(fr, PC);

    slots  = slotsInFrame(fr, PC);
#if O_SECURE
    oslots = slots;
#endif
    sp = argFrameP(fr, 0);
    for( ; slots-- > 0; sp++ )
    { if ( !marked(sp) )
      { if ( isGlobalRef(*sp) )
	  mark_variable(sp);
	else
	  ldomark(sp);      
      }
    }

    PC = fr->programPointer;
    if ( fr->parent != NULL )
      fr = fr->parent;
    else
      return (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark the choicepoints. This function walks   along the environments that
can be reached from  the  choice-points.   In  addition,  it deletes all
trail-references  that  will   be   overruled    by   the   choice-point
stack-reference anyway.

When using setarg/3 (O_DESTRUCTIVE_ASSIGNMENT),   destrctive assignments
are stored on the trail-stack as  two   entries.  The first entry is the
normal trail-pointer, while the  second   is  flagged  with TAG_TRAILVAL
(0x1). When undoing, the tail is scanned backwards and if a tagged value
is encountered, this value is restored  at   the  location  of the first
trail-cell.

If the has become  garbage,  we  can   destroy  both  cells.  Note  that
mark_trail() already has marked the replaced value,  so that will not be
garbage collected during this pass. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static GCTrailEntry
mark_choicepoints(Choice ch, GCTrailEntry te)
{ GET_LD

  for( ; ch; ch = ch->parent )
  { LocalFrame fr = ch->frame;
    GCTrailEntry tm = (GCTrailEntry)ch->mark.trailtop;
    Word top;

    if ( ch->type == CHP_CLAUSE )
      top = argFrameP(fr, fr->predicate->functor->arity);
    else
    { assert(ch->type == CHP_TOP || (void *)ch > (void *)fr);
      top = (Word)ch;
    }

    for( ; te >= tm; te-- )		/* early reset of vars */
    { 
#if O_DESTRUCTIVE_ASSIGNMENT
      if ( ttag(te->address) != TAG_TRAILVAL )
#endif
      { Word tard = val_ptr(te->address);

	if ( tard >= top )		/* above local stack */
	{ SECURE(assert(ttag(te[1].address) != TAG_TRAILVAL));
	  te->address = 0;
	  trailcells_deleted++;
	} else if ( !marked(tard) )	/* garbage */
	{ setVar(*tard);
	  SECURE(assert(*tard != QID_MAGIC));
#if O_DESTRUCTIVE_ASSIGNMENT
	  if ( ttag(te[1].address) == TAG_TRAILVAL)
	  { te[1].address = 0;
	    trailcells_deleted++;
	  }
#endif
	  DEBUG(3, Sdprintf("Early reset of 0x%p\n", te->address));
	  te->address = 0;
	  trailcells_deleted++;
	}
      }
    }

    needsRelocation(&ch->mark.trailtop);
    alien_into_relocation_chain(&ch->mark.trailtop, STG_TRAIL, STG_LOCAL);
    SECURE(trailtops_marked--);

    mark_environments(fr,
		      ch->type == CHP_JUMP ? ch->value.PC : NULL);
  }

  return te;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mark_stacks() marks all  data  that  is   reachable  from  any  frame or
choicepoint. In addition, it  will  do   `early  reset'  on variables of
choicepoints that will be  reset  anyway   if  backtracking  reaches the
choicepoint. Also, it  will  insert  all   trailtops  of  marks  in  the
relocation chains. A small problem is  the   top-goal  of  a query, This
frame may not be a  choicepoint,  but   its  mark  is  needed anyhow for
PL_close_query(), so it has to be relocated.  `te' in the function below
has to be updated as none of these variables should be reset
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
mark_stacks(LocalFrame fr, Choice ch)
{ GET_LD
  QueryFrame query;
  GCTrailEntry te = (GCTrailEntry)tTop - 1;

  trailcells_deleted = 0;

  for( ; fr; fr = query->saved_environment, ch = query->saved_bfr )
  { query = mark_environments(fr, NULL);
    te    = mark_choicepoints(ch, te);

    assert(query->magic == QID_MAGIC);
  }
  
  DEBUG(2, Sdprintf("Trail stack garbage: %ld cells\n", trailcells_deleted));
}


#ifdef O_DESTRUCTIVE_ASSIGNMENT
static void
mark_trail()
{ GET_LD
  GCTrailEntry te = (GCTrailEntry)tTop - 1;

  for( ; te >= (GCTrailEntry)tBase; te-- )
  { Word gp;

    if ( ttag(te->address) == TAG_TRAILVAL )
    { gp = val_ptr(te->address);

      DEBUG(2, Sdprintf("mark_trail(): trailed value from %p at %p (*=%p)\n",
			&te->address, gp, *gp));

      assert(onGlobal(gp));
      if ( !marked(gp) )
      { mark_variable(gp);
	total_marked++;			/* fix counters */
	local_marked--;
      }
    }
  }
}
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/


#if O_SECURE
static int
cmp_address(const void *vp1, const void *vp2)
{ GET_LD
  Word p1 = *((Word *)vp1);
  Word p2 = *((Word *)vp2);

  return p1 > p2 ? 1 : p1 == p2 ? 0 : -1;
}
#endif


static void
mark_phase(LocalFrame fr, Choice ch)
{ GET_LD
  total_marked = 0;

  mark_term_refs();
#ifdef O_DESTRUCTIVE_ASSIGNMENT
  mark_trail();
#endif
  mark_stacks(fr, ch);
  mark_foreign_trail_refs();
#if O_SECURE
  if ( !scan_global(TRUE) )
    sysError("Global stack corrupted after GC mark-phase");
  qsort(mark_base, mark_top - mark_base, sizeof(Word), cmp_address);
#endif

  DEBUG(2, { long size = gTop - gBase;
	     Sdprintf("%ld referenced cell; %ld garbage (gTop = 0x%p)\n",
		      total_marked, size - total_marked, gTop);
	   });
}


		/********************************
		*          COMPACTING           *
		*********************************/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Relocation chain management

A relocation chain is a linked chain of cells, whose elements all should
point to `dest' after it is unwound.  SWI-Prolog knows about a number of
different pointers.  This routine is supposed  to  restore  the  correct
pointer.  The following types are identified:

    source	types
    local	address values (gTop references)
    		term, reference and indirect pointers
    trail	address values (reset addresses)
    global	term, reference and indirect pointers

To do this, a pointer of the same  type  is  stored  in  the  relocation
chain.

    update_relocation_chain(current, dest)
	This function checks whether current is the head of a relocation
	chain.  As we know `dest' is the place  `current'  is  going  to
	move  to,  we  can reverse the chain and have all pointers in it
	pointing to `dest'.

	We must clear the `first' bit on the field.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
update_relocation_chain(Word current, Word dest)
{ GET_LD
  Word head = current;
  word val = get_value(current);

  DEBUG(3, Sdprintf("unwinding relocation chain at 0x%p to 0x%p\n",
		    current, dest));

  do
  { int tag;

    unmark_first(current);
    current = valPtr(val);
    tag = tag(val);
    val = get_value(current);
    set_value(current, makePtr(dest, tag));
    relocated_cells++;
  } while( is_first(current) );

  set_value(head, val);
  relocation_chains--;
}


static void
into_relocation_chain(Word current, int stg)
{ GET_LD
  Word head;
  word val = get_value(current);
  
  head = valPtr(val);			/* FIRST/MASK already gone */
  set_value(current, get_value(head));
  set_value(head, consPtr(current, stg|tag(val)));

  DEBUG(2, Sdprintf("Into relocation chain: 0x%p (head = 0x%p)\n",
		    current, head));

  if ( is_first(head) )
    mark_first(current);
  else
  { mark_first(head);
    relocation_chains++;
  }

  relocation_cells++;
}


static void
alien_into_relocation_chain(void *addr, int orgst, int stg)
{ GET_LD
  void **ptr = (void **)addr;
  
  *ptr = (void *)consPtr(*ptr, orgst);
  into_relocation_chain(addr, stg);

  alien_relocations++;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trail stack compacting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
compact_trail(void)
{ GET_LD
  GCTrailEntry dest, current;
  
	/* compact the trail stack */
  for( dest = current = (GCTrailEntry)tBase; current < (GCTrailEntry)tTop; )
  { if ( is_first(&current->address) )
      update_relocation_chain(&current->address, &dest->address);
#if O_SECURE
    else
    { Symbol s;
      if ( (s=lookupHTable(check_table, current)) != NULL &&
	   s->value == (void *)TRUE )
        sysError("0x%p was supposed to be relocated (*= 0x%p)",
		 current, current->address);
    }
#endif

    if ( current->address )
      *dest++ = *current++;
    else
      current++;
  }
  if ( is_first(&current->address) )
    update_relocation_chain(&current->address, &dest->address);

  tTop = (TrailEntry)dest;

  if ( relocated_cells != relocation_cells )
    sysError("After trail: relocation cells = %ld; relocated_cells = %ld\n",
	     relocation_cells, relocated_cells);
} 


static void
tag_trail()
{ GET_LD
  TrailEntry te;

  for(te = tBase; te < tTop; te++)
  { if ( te->address )
    { word mask = ttag(te->address);
      int stg;

      if ( onLocal(te->address) )
	stg = STG_LOCAL;
      else
	stg = STG_GLOBAL;

      te->address = (Word)consPtr(te->address, stg|mask);
    }
  }
}


static void
untag_trail()
{ GET_LD
  TrailEntry te;

  for(te = tBase; te < tTop; te++)
  { if ( te->address )
    { word mask = ttag(te->address);

      te->address = (Word)((word)valPtr((word)te->address)|mask);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sweep a mark. This is a bit tricky as the global-stack pointer may point
to  a  garbage  global  cell.  Therefore  we  have  to  find  the  first
non-garbage one. Unfortunately, the cell may  already be in a relocation
chain (in which case `first' is true). In  this case it is not a garbage
cell. Hence the `goto found'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
sweep_mark(mark *m)
{ GET_LD
  Word gm, prev;

  marks_swept++;
  gm = m->globaltop;

  for(;;)
  { if ( gm == gBase )
    { m->globaltop = (Word)consPtr(gm, STG_GLOBAL);
      alien_relocations++;
      break;
    }
    if ( is_first(gm-1) )
      goto found;
    prev = previous_gcell(gm);
    if ( marked(prev) )
    {
    found:
      m->globaltop = gm, STG_GLOBAL;
      DEBUG(3, Sdprintf("gTop mark from choice point: "));
      needsRelocation(&m->globaltop);
      alien_into_relocation_chain(&m->globaltop, STG_GLOBAL, STG_LOCAL);
      break;
    }
    gm = prev;
  }
}


static void
sweep_foreign()
{ GET_LD
  FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
  { Word sp = refFliP(fr, 0);
    int n = fr->size;

    sweep_mark(&fr->mark);
    for( ; n-- > 0; sp++ )
    { if ( marked(sp) )
      {	unmark(sp);
	if ( isGlobalRef(get_value(sp)) )
	{ local_marked--;
	  check_relocation(sp);
	  into_relocation_chain(sp, STG_LOCAL);
	}
      }
    }
  }
}


static void
unsweep_mark(mark *m)
{ GET_LD
  m->trailtop  = (TrailEntry)valPtr2((word)m->trailtop,  STG_TRAIL);
  m->globaltop = valPtr2((word)m->globaltop, STG_GLOBAL);

  SECURE(check_mark(m));

  marks_unswept++;
}


static void
unsweep_foreign()
{ GET_LD
  FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
    unsweep_mark(&fr->mark);
}


static void
unsweep_choicepoints(Choice ch)
{ for( ; ch ; ch = ch->parent)
    unsweep_mark(&ch->mark);
}


static QueryFrame
unsweep_environments(LocalFrame fr)
{ while(fr->parent)
    fr = fr->parent;

  return (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
}


static void
unsweep_stacks(LocalFrame fr, Choice ch)
{ QueryFrame query;

  for( ; fr; fr = query->saved_environment, ch = query->saved_bfr )
  { query = unsweep_environments(fr);
    unsweep_choicepoints(ch);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sweeping the local and trail stack to insert necessary pointers  in  the
relocation chains.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
sweep_trail(void)
{ GET_LD
  GCTrailEntry te = (GCTrailEntry)tTop - 1;

  for( ; te >= (GCTrailEntry)tBase; te-- )
  { if ( te->address )
    {
#ifdef O_DESTRUCTIVE_ASSIGNMENT
      if ( ttag(te->address) == TAG_TRAILVAL )
      { needsRelocation(&te->address);
	into_relocation_chain(&te->address, STG_TRAIL);
      } else
#endif
      if ( storage(te->address) == STG_GLOBAL )
      { needsRelocation(&te->address);
	into_relocation_chain(&te->address, STG_TRAIL);
      }
    }
  }
}


static QueryFrame
sweep_environments(LocalFrame fr, Code PC)
{ GET_LD

  if ( !fr )
    return NULL;

  for( ; ; )
  { int slots;
    Word sp;

    if ( false(fr, FR_MARKED) )
      return NULL;
    clear(fr, FR_MARKED);

    slots = slotsInFrame(fr, PC);

    sp = argFrameP(fr, 0);
    for( ; slots > 0; slots--, sp++ )
    { if ( marked(sp) )
      { unmark(sp);
	if ( isGlobalRef(get_value(sp)) )
	{ local_marked--;
	  check_relocation(sp);
	  into_relocation_chain(sp, STG_LOCAL);
	}
      }
    }

    PC = fr->programPointer;
    if ( fr->parent != NULL )
      fr = fr->parent;
    else
      return (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
  }
}


static void
sweep_choicepoints(Choice ch)
{ for( ; ch ; ch = ch->parent)
  { sweep_environments(ch->frame,
		       ch->type == CHP_JUMP ? ch->value.PC : NULL);
    sweep_mark(&ch->mark);
  }
}


static void
sweep_stacks(LocalFrame fr, Choice ch)
{ GET_LD
  QueryFrame query;
  
  for( ; fr; fr = query->saved_environment, ch = query->saved_bfr )
  { query = sweep_environments(fr, NULL);
    sweep_choicepoints(ch);

    if ( !query )			/* we've been here */
      break;
  }

  if ( local_marked != 0 )
    sysError("local_marked = %ld", local_marked);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
All preparations have been made now, and the actual  compacting  of  the
global  stack  may  start.   The  marking phase has calculated the total
number of words (cells) in the global stack that are none-garbage.

In the first phase, we will  walk  along  the  global  stack  from  it's
current  top towards the bottom.  During this phase, `current' refers to
the current element we are processing, while `dest' refers to the  place
this  element  will  be  after  the compacting phase is completed.  This
invariant is central and should be maintained carefully while processing
alien objects as strings and reals, which happen to have a  non-standard
size.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
is_downward_ref(Word p)
{ GET_LD
  word val = get_value(p);

  switch(tag(val))
  { case TAG_INTEGER:
      if ( storage(val) == STG_INLINE )
	fail;
    case TAG_STRING:
    case TAG_FLOAT:
    case TAG_REFERENCE:
    case TAG_COMPOUND:
      return val_ptr(val) < p;
  }

  fail;
}


static bool
is_upward_ref(Word p)
{ GET_LD
  word val = get_value(p);

  switch(tag(val))
  { case TAG_INTEGER:
      if ( storage(val) == STG_INLINE )
	fail;
    case TAG_STRING:
    case TAG_FLOAT:
    case TAG_REFERENCE:
    case TAG_COMPOUND:
      return val_ptr(val) > p;
  }

  fail;
}


static void
compact_global(void)
{ GET_LD
  Word dest, current;
#if O_SECURE
  Word *v = mark_top;
#endif

  DEBUG(2, Sdprintf("Scanning global stack downwards\n"));

  dest = gBase + total_marked;			/* first FREE cell */
  for( current = gTop; current >= gBase; current-- )
  { long offset = (marked(current) || is_first(current)
		   			? 0 : offset_cell(current));
    current -= offset;

    if ( marked(current) )
    {
#if O_SECURE
      if ( current != *--v )
        sysError("Marked cell at 0x%p (*= 0x%p); gTop = 0x%p; should be 0x%p",
		 current, *current, gTop, *v);
#endif
      dest -= offset + 1;
      DEBUG(3, Sdprintf("Marked cell at 0x%p (size = %ld; dest = 0x%p)\n",
			current, offset+1, dest));
      if ( is_first(current) )
	update_relocation_chain(current, dest);
      if ( is_downward_ref(current) )
      { check_relocation(current);
	into_relocation_chain(current, STG_GLOBAL);
      }
    } else
    { if ( is_first(current) )
	update_relocation_chain(current, dest);	/* gTop refs from marks */
    }
  }

#if O_SECURE
  if ( v != mark_base )
  { for( v--; v >= mark_base; v-- )
    { Sdprintf("Expected marked cell at 0x%p, (*= 0x%lx)\n", *v, **v);
    }
    sysError("v = 0x%p; mark_base = 0x%p", v, mark_base);
  }
#endif

  if ( dest != gBase )
    sysError("Mismatch in down phase: dest = 0x%p, gBase = 0x%p\n",
	     dest, gBase);
  if ( relocation_cells != relocated_cells )
    sysError("After down phase: relocation_cells = %ld; relocated_cells = %ld",
	     relocation_cells, relocated_cells);

  DEBUG(2, Sdprintf("Scanning global stack upwards\n"));
  dest = gBase;
  for(current = gBase; current < gTop; )
  { if ( marked(current) )
    { long l, n;

      if ( is_first(current) )
	update_relocation_chain(current, dest);

      if ( (l = offset_cell(current)) == 0 )	/* normal cells */
      { *dest = *current;
        if ( is_upward_ref(current) )
	{ check_relocation(current);
          into_relocation_chain(dest, STG_GLOBAL);
	}
	unmark(dest);
	dest++;
	current++;
      } else					/* indirect values */
      { Word cdest, ccurrent;

	l++;
	
	for( cdest=dest, ccurrent=current, n=0; n < l; n++ )
	  *cdest++ = *ccurrent++;
      
	unmark(dest);
	dest += l;
	current += l;
      }

    } else
      current += offset_cell(current) + 1;
  }

  if ( dest != gBase + total_marked )
    sysError("Mismatch in up phase: dest = 0x%p, gBase+total_marked = 0x%p\n",
	     dest, gBase + total_marked );

  DEBUG(3, { Word p = dest;		/* clear top of stack */
	     while(p < gTop)
	       *p++ = 0xbfbfbfbfL;
	   });

  gTop = dest;
}

static void
collect_phase(LocalFrame fr, Choice ch)
{ GET_LD
 
  DEBUG(2, Sdprintf("Sweeping foreign references\n"));
  sweep_foreign();
  DEBUG(2, Sdprintf("Sweeping trail stack\n"));
  sweep_trail();
  DEBUG(2, Sdprintf("Sweeping local stack\n"));
  sweep_stacks(fr, ch);
  DEBUG(2, Sdprintf("Compacting global stack\n"));
  compact_global();

  unsweep_foreign();
  unsweep_stacks(fr, ch);

  if ( relocation_chains != 0 )
    sysError("relocation chains = %ld", relocation_chains);
  if ( relocated_cells != relocation_cells ||
       relocated_cells != needs_relocation )
    sysError("relocation cells = %ld; relocated_cells = %ld, "
	     "needs_relocation = %ld\n\t",
	     relocation_cells, relocated_cells, needs_relocation);
}


		/********************************
		*             MAIN              *
		*********************************/

#if O_DYNAMIC_STACKS
void
considerGarbageCollect(Stack s)
{ GET_LD
  if ( s->gc && trueFeature(GC_FEATURE) && !gc_status.requested )
  { long used  = (char *)s->top   - (char *)s->base;
    long free  = (char *)s->limit - (char *)s->top;
    long limit = (char *)s->limit - (char *)s->base;

    if ( used > s->factor*s->gced_size + s->small )
    { gc_status.requested = TRUE;
    } else if ( free < limit/8 && used > s->gced_size + limit/32 ) 
    { gc_status.requested = TRUE;
    }

    DEBUG(1, if ( gc_status.requested)
	     { Sdprintf("%s overflow: Posted garbage collect request\n",
			s->name);
	     });
  }
}
#endif /* O_DYNAMIC_STACKS */


#if O_SECURE || O_DEBUG || defined(O_MAINTENANCE)
static bool
scan_global(int marked)
{ GET_LD
  Word current;
  int errors = 0;
  long cells = 0;

  for( current = gBase; current < gTop; current += (offset_cell(current)+1) )
  { cells++;
    if ( (!marked && marked(current)) || is_first(current) )
    { warning("Illegal cell in global stack (up) at 0x%p (*= 0x%p)",
	      current, *current);
      if ( isAtom(*current) )
	warning("0x%p is atom %s", current, stringAtom(*current));
      if ( isTerm(*current) )
	warning("0x%p is term %s/%d",
		current,
		stringAtom(nameFunctor(functorTerm(*current))),
		arityTerm(*current));
      if ( ++errors > 10 )
      { Sdprintf("...\n");
        break;
      }
    }
  }

  for( current = gTop - 1; current >= gBase; current-- )
  { cells --;
    current -= offset_cell(current);
    if ( (!marked && marked(current)) || is_first(current) )
    { warning("Illegal cell in global stack (down) at 0x%p (*= 0x%p)",
	      current, *current);
      if ( ++errors > 10 )
      { Sdprintf("...\n");
        break;
      }
    }
  }

  if ( !errors && cells != 0 )
    sysError("Different count of cells upwards and downwards: %ld\n", cells);

  return errors == 0;
}


static word key;
static int checked;

static void
check_mark(mark *m)
{ GET_LD

  assert(onStackArea(trail,  m->trailtop));
  assert(onStackArea(global, m->globaltop));
}


static QueryFrame
check_environments(LocalFrame fr, Code PC)
{ GET_LD

  if ( fr == NULL )
    return NULL;

  for(;;)
  { int slots, n;
    Word sp;

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    local_frames++;
    clearUninitialisedVarsFrame(fr, PC);

    assert(onStack(local, fr));

    DEBUG(3, Sdprintf("Check [%ld] %s:",
		      levelFrame(fr),
		      predicateName(fr->predicate)));

    slots = slotsInFrame(fr, PC);
    sp = argFrameP(fr, 0);
    for( n=0; n < slots; n++ )
    { key += checkData(&sp[n]);
    }
    checked += slots;
    DEBUG(3, Sdprintf(" 0x%lx\n", key));

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else
    { QueryFrame qf = (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
      DEBUG(3, Sdprintf("*** Query %s\n", predicateName(qf->frame.predicate)));
      return qf;
    }
  }
}


static void
check_choicepoints(Choice ch)
{ GET_LD

  for( ; ch; ch = ch->parent )
  { if ( !ch->parent )
      assert(ch->type == CHP_TOP);
    choice_count++;
    check_mark(&ch->mark);
    check_environments(ch->frame,
		       ch->type == CHP_JUMP ? ch->value.PC : NULL);
  }
}


word
check_foreign()
{ GET_LD
  FliFrame ff;
  word key = 0L;

  for(ff = fli_context; ff; ff = ff->parent )
  { Word sp = refFliP(ff, 0);
    int n = ff->size;

    for(n=0 ; n < ff->size; n++ )
      key += checkData(&sp[n]);

    check_mark(&ff->mark);
  }

  return key;
}


#ifdef O_DESTRUCTIVE_ASSIGNMENT
static word
check_trail()
{ GET_LD
  TrailEntry te = tTop - 1;
  word key = 0;

  for( ; te >= tBase; te-- )
  { Word gp;

    if ( isTrailVal(te->address) )
    { gp = trailValP(te->address);

      assert(onGlobal(gp));
      key += checkData(gp);
    }
  }

  return key;
}
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/


word
checkStacks(LocalFrame frame, Choice choice)
{ GET_LD
  LocalFrame fr;
  Choice ch;
  QueryFrame qf;

  if ( !frame )
    frame = environment_frame;
  if ( !choice )
    choice = LD->choicepoints;

  local_frames = 0;
  choice_count = 0;
  key = 0L;

  for( fr = frame, ch=choice;
       fr;
       fr = qf->saved_environment, ch = qf->saved_bfr )
  { qf = check_environments(fr, NULL);
    assert(qf->magic == QID_MAGIC);

    check_choicepoints(ch);
  }

  SECURE(trailtops_marked = choice_count);

  unmark_stacks(LD, frame, choice);

  assert(local_frames == 0);
  assert(choice_count == 0);

  key += check_foreign();
#ifdef O_DESTRUCTIVE_ASSIGNMENT
  key += check_trail();
#endif

  return key;
}

#endif /*O_SECURE || O_DEBUG*/


void
garbageCollect(LocalFrame fr, Choice ch)
{ GET_LD
  long tgar, ggar;
  double t = CpuTime(CPU_USER);
  int verbose = trueFeature(TRACE_GC_FEATURE);
  sigset_t mask;

  DEBUG(0, verbose = TRUE);

  if ( gc_status.blocked || !trueFeature(GC_FEATURE) )
    return;

  blockSignals(&mask);
  blockGC();				/* avoid recursion due to */
  gc_status.requested = FALSE;		/* printMessage() */

  gc_status.active = TRUE;
  finish_foreign_frame(PASS_LD1);
  if ( verbose )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "gc", 1,
		   PL_CHARS, "start");

#ifdef O_PROFILE
  PROCEDURE_garbage_collect0->definition->profile_calls++;
#endif
#if O_SECURE
  if ( !scan_global(FALSE) )
    sysError("Stack not ok at gc entry");

  key = checkStacks(fr, ch);

  if ( check_table == NULL )
    check_table = newHTable(256);
  else
    clearHTable(check_table);

  mark_base = mark_top = malloc(usedStack(global));
#endif

  needs_relocation  = 0;
  relocation_chains = 0;
  relocation_cells  = 0;
  relocated_cells   = 0;
  local_marked	    = 0;

  requireStack(global, sizeof(word));
  requireStack(trail, sizeof(struct trail_entry));
  setVar(*gTop);
  tTop->address = 0;

  tag_trail();
  mark_phase(fr, ch);
  tgar = trailcells_deleted * sizeof(struct trail_entry);
  ggar = (gTop - gBase - total_marked) * sizeof(word);
  gc_status.trail_gained  += tgar;
  gc_status.global_gained += ggar;
  gc_status.collections++;

  DEBUG(2, Sdprintf("Compacting trail ... "));
  compact_trail();

  collect_phase(fr, ch);
  untag_trail();
#if O_SECURE
  assert(trailtops_marked == 0);
  if ( !scan_global(FALSE) )
    sysError("Stack not ok after gc; gTop = 0x%p", gTop);
  free(mark_base);
#endif

  t = CpuTime(CPU_USER) - t;
  gc_status.time += t;
  trimStacks();
  LD->stacks.global.gced_size = usedStack(global);
  LD->stacks.trail.gced_size  = usedStack(trail);
  gc_status.active = FALSE;

  SECURE(if ( checkStacks(fr, ch) != key )
	 { Sdprintf("Stack checksum failure\n");
	   trap_gdb();
	 } else if ( verbose )
	   Sdprintf("(OK) "));

  if ( verbose )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "gc", 1,
		   PL_FUNCTOR_CHARS, "done", 7,
		     PL_LONG, ggar,
		     PL_LONG, tgar,
		     PL_DOUBLE, (double)t,
		     PL_LONG, usedStack(global),
		     PL_LONG, usedStack(trail),
		     PL_LONG, roomStack(global),
		     PL_LONG, roomStack(trail));
  unblockGC();
  unblockSignals(&mask);
}

word
pl_garbage_collect(term_t d)
{ GET_LD
  LocalFrame fr = environment_frame;
  Choice ch = LD->choicepoints;

#if O_DEBUG
  int ol = GD->debug_level;
  int nl;

  if ( !PL_get_integer(d, &nl) )
    return warning("garbage_collect/1: instantiation fault");
  GD->debug_level = nl;
#endif
  finish_foreign_frame(PASS_LD1);
  garbageCollect(fr, ch);
#if O_DEBUG
  GD->debug_level = ol;
#endif
  succeed;
}


void
blockGC()
{ GET_LD
  gc_status.blocked++;
#if O_SHIFT_STACKS
  shift_status.blocked++;
#endif
}


void
unblockGC()
{ GET_LD
  gc_status.blocked--;
#if O_SHIFT_STACKS
  shift_status.blocked--;
#endif
}


#if O_SHIFT_STACKS

		 /*******************************
		 *	   STACK-SHIFTER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the Prolog runtime stacks presuming they have shifted by the
the specified offset.

Memory management description.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
extern char *chp_chars(Choice ch);
#endif

static inline void
update_pointer(void *p, long offset)
{ GET_LD
  char **ptr = (char **)p;

  if ( *ptr )
    *ptr += offset;
}


		 /*******************************
		 *	   LOCAL STACK		*
		 *******************************/

static void
update_mark(mark *m, long gs, long ts)
{ GET_LD

  if ( ts ) update_pointer(&m->trailtop, ts);
  if ( gs ) update_pointer(&m->globaltop, gs);
}


/* Update pointer if it contains a pointer in the local stack.  Used for
   updating PC, as this might point to a locally compiled clause by
   I_USERCALL0.
*/

static inline void
update_local_pointer(void *p, long ls)
{ char **ptr = (char **)p;

  if ( inShiftedArea(local, ls, *ptr) )
  { update_pointer(p, ls);
  }
}


static QueryFrame
update_environments(LocalFrame fr, Code PC, long ls, long gs, long ts)
{ GET_LD
  if ( fr == NULL )
    return NULL;

  for(;;)
  { assert(inShiftedArea(local, ls, fr));

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    local_frames++;
    
    DEBUG(2,
	  Sdprintf("Shifting frame 0x%p [%ld] %s ... ",
		 fr, levelFrame(fr), predicateName(fr->predicate)));

    if ( ls )				/* update frame pointers */
    { update_pointer(&fr->parent, ls);
      clearUninitialisedVarsFrame(fr, PC);

      update_local_pointer(&fr->programPointer, ls);
					/* I_USERCALL0 compiled clause */
      if ( fr->predicate == PROCEDURE_dcall1->definition )
      { update_pointer(&fr->clause, ls);
	update_pointer(&fr->clause->clause, ls);
	update_pointer(&fr->clause->clause->codes, ls);
      }

					/* update saved BFR's from C_MARK */
      if ( PC && false(fr->predicate, FOREIGN) )
      { Clause cl = fr->clause->clause;
	unsigned int marks;

	if ( (marks = cl->marks) )
	{ Word sp = argFrameP(fr, cl->prolog_vars);
	
	  DEBUG(2, Sdprintf("(%d marks)", marks));

	  for( ; marks-- > 0; sp++ )
	    update_pointer(sp, ls);
	}
      }
    }

    DEBUG(2, Sdprintf("ok\n"));

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
    { QueryFrame query = (QueryFrame)addPointer(fr, -offset(queryFrame,frame));

      if ( ls )
      { update_pointer(&query->saved_bfr, ls);
	update_pointer(&query->saved_environment, ls);
	update_pointer(&query->registers.fr, ls);
      }
      
      return query;
    }
  }
}


static void
update_choicepoints(Choice ch, long ls, long gs, long ts)
{ GET_LD

  for( ; ch; ch = ch->parent )
  { DEBUG(1, Sdprintf("Updating choicepoint %s for %s ... ",
		      chp_chars(ch),
		      predicateName(ch->frame->predicate)));

    if ( ls )
    { update_pointer(&ch->frame, ls);
      update_pointer(&ch->parent, ls);
      if ( ch->type == CHP_JUMP )
	update_local_pointer(&ch->value.PC, ls);
    }
    update_mark(&ch->mark, gs, ts);
    update_environments(ch->frame,
		        ch->type == CHP_JUMP ? ch->value.PC : NULL,
			ls, gs, ts);
    choice_count++;
    DEBUG(1, Sdprintf("ok\n"));
  }
}


		 /*******************************
		 *	  ARGUMENT STACK	*
		 *******************************/

static void
update_argument(long ls, long gs)
{ GET_LD
  Word *p = aBase;
  Word *t = aTop;

  for( ; p < t; p++ )
  { if ( onGlobal(*p) )
    { *p = addPointer(*p, gs);
    } else
    { assert(onLocal(*p));
      *p = addPointer(*p, ls);
    }
  }
}


		 /*******************************
		 *	  TRAIL STACK	*
		 *******************************/

static void
update_trail(long ls, long gs)
{ GET_LD
  TrailEntry p = tBase;
  TrailEntry t = tTop;

  for( ; p < t; p++ )
  { if ( onGlobal(trailValP(p->address)) )
    { update_pointer(&p->address, gs);
    } else
    { assert(onLocal(p->address));
      update_pointer(&p->address, ls);
    }
  }
}


		 /*******************************
		 *	  FOREIGN FRAMES	*
		 *******************************/

static void
update_foreign(long ts, long ls, long gs)
{ GET_LD
  FliFrame fr = addPointer(fli_context, ls);

  for( ; fr; fr = fr->parent )
  { update_mark(&fr->mark, gs, ts);
    update_pointer(&fr->parent, ls);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry-point.   Update the  stacks to  reflect  their current  positions.
This function should be called *after*  the  stacks have been relocated.
Note that these functions are  only used  if  there is no virtual memory
way to reach at dynamic stacks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define updateStackHeader(name, offset) \
	{ LD->stacks.name.base  = addPointer(LD->stacks.name.base,  offset); \
	  LD->stacks.name.top   = addPointer(LD->stacks.name.top,   offset); \
	  LD->stacks.name.max   = addPointer(LD->stacks.name.max,   offset); \
	  LD->stacks.name.limit = addPointer(LD->stacks.name.limit, offset); \
	}


static void
update_stacks(LocalFrame frame, Choice choice, Code PC,
	      void *lb, void *gb, void *tb)
{ GET_LD
  long ls, gs, ts;

  ls = (long) lb - (long) lBase;
  gs = (long) gb - (long) gBase;
  ts = (long) tb - (long) tBase;

  DEBUG(2, Sdprintf("ls+gs+ts = %ld %ld %ld ... ", ls, gs, ts));

  if ( ls || gs || ts )
  { LocalFrame fr;
    Choice ch;
    QueryFrame qf;

    local_frames = 0;
    choice_count = 0;
    
    for( fr = addPointer(frame, ls),
	 ch = addPointer(choice, ls)
       ; fr
       ; fr = qf->saved_environment,
	 ch = qf->saved_bfr,
	 PC = NULL
       )
    { qf = update_environments(fr, PC, ls, gs, ts);

      update_choicepoints(ch, ls, gs, ts);
    }

    DEBUG(2, Sdprintf("%d frames, %d choice-points ...",
		      local_frames, choice_count));

    frame  = addPointer(frame, ls);
    choice = addPointer(choice, ls);
    unmark_stacks(LD, frame, choice);

    assert(local_frames == 0);
    assert(choice_count == 0);

    if ( gs || ls )
    { update_argument(ls, gs);
      update_trail(ls, gs);
    }
    update_foreign(ts, ls, gs);

    updateStackHeader(local,  ls);
    updateStackHeader(global, gs);
    updateStackHeader(trail,  ts);

    base_addresses[STG_LOCAL]  = (unsigned long)lBase;
    base_addresses[STG_GLOBAL] = (unsigned long)gBase;
    base_addresses[STG_TRAIL]  = (unsigned long)tBase;
  }

  if ( ls )
  { update_pointer(&environment_frame, ls);
    update_pointer(&fli_context, ls);
    update_pointer(&LD->choicepoints, ls);
  }
  if ( gs )
  { update_pointer(&LD->mark_bar, gs);
  } 
}


static long
nextStackSize(Stack s)
{ GET_LD
  long size  = diffPointers(s->max, s->base);
  long grow  = ROUND(size/2, 8192);
  long limit = diffPointers(s->limit, s->base);

  if ( size + grow > limit )
  { if ( size + grow > (limit*3)/2 )
      outOfStack(s, STACK_OVERFLOW_THROW);

    outOfStack(s, STACK_OVERFLOW_SIGNAL);
    grow = limit - size;
    if ( grow < 32 * 1024 )
      grow = 32 * 1024;
    s->limit = addPointer(s->base, size+grow);
  }

  size += grow;

  return size;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry point from interpret()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GL_SEPARATION sizeof(word)

int
growStacks(LocalFrame fr, Choice ch, Code PC, int l, int g, int t)
{ GET_LD
  
  if ( !fr )
    fr = environment_frame;
  if ( !ch )
    ch = LD->choicepoints;

  if ( PC != NULL )			/* for now, only at the call-port */
    fail;

  if ( (l || g || t) && !shift_status.blocked )
  { TrailEntry tb = tBase;
    Word gb = gBase;
    LocalFrame lb = lBase;
    long lsize = sizeStack(local);
    long gsize = sizeStack(global);
    long tsize = sizeStack(trail);
    double time = CpuTime(CPU_USER);
    int verbose = trueFeature(TRACE_GC_FEATURE);
    
    DEBUG(0, verbose = TRUE);

    if ( verbose )
    { int i = 0;

      Sdprintf("Expanding ");
      if ( l ) Sdprintf("%s%s", i++ ? "and " : "", "local ");
      if ( g ) Sdprintf("%s%s", i++ ? "and " : "", "global ");
      if ( t ) Sdprintf("%s%s", i++ ? "and " : "", "trail ");
      Sdprintf("stacks ");
    }

    finish_foreign_frame(PASS_LD1);

    if ( !fr )
      fr = environment_frame;
    if ( !ch )
      ch = LD->choicepoints;

    SECURE(key = checkStacks(fr, ch));

    if ( t )
    { tsize = nextStackSize((Stack) &LD->stacks.trail);
      tb = xrealloc(tb, tsize);
      shift_status.trail_shifts++;
    }

    if ( g || l )
    { long loffset = gsize + GL_SEPARATION;
      assert(lb == addPointer(gb, loffset));	

      if ( g )
      { gsize = nextStackSize((Stack) &LD->stacks.global);
	shift_status.global_shifts++;
      }
      if ( l )
      { lsize = nextStackSize((Stack) &LD->stacks.local);
	shift_status.local_shifts++;
      }

      gb = xrealloc(gb, lsize + gsize + GL_SEPARATION);
      lb = addPointer(gb, gsize + GL_SEPARATION);
      if ( g )				/* global enlarged; move local */
	memmove(lb,   addPointer(gb, loffset), lsize);
	     /* dest, src,                     size */
    }
      
    if ( verbose )
    { Sdprintf("to (l+g+t) = %d+%d+%d Kbytes ... ",
	   lsize / 1024,
	   gsize / 1024,
	   tsize / 1024);
    }

#define PrintStackParms(stack, name, newbase, newsize) \
	{ Sdprintf("%6s: 0x%08lx ... 0x%08lx --> 0x%08lx ... 0x%08lx\n", \
		 name, \
		 (unsigned long) LD->stacks.stack.base, \
		 (unsigned long) LD->stacks.stack.max, \
		 (unsigned long) newbase, \
		 (unsigned long) addPointer(newbase, newsize)); \
	}


    DEBUG(0, { Sputchar('\n');
	       PrintStackParms(global, "global", gb, gsize);
	       PrintStackParms(local, "local", lb, lsize);
	       PrintStackParms(trail, "trail", tb, tsize);
	     });
		    
    DEBUG(1, Sdprintf("Updating stacks ..."));
    update_stacks(fr, ch, PC, lb, gb, tb);

    LD->stacks.local.max  = addPointer(LD->stacks.local.base,  lsize);
    LD->stacks.global.max = addPointer(LD->stacks.global.base, gsize);
    LD->stacks.trail.max  = addPointer(LD->stacks.trail.base,  tsize);

    SetHTop(LD->stacks.local.max);
    SetHTop(LD->stacks.trail.max);

    time = CpuTime(CPU_USER) - time;
    shift_status.time += time;
    SECURE(if ( checkStacks(NULL, NULL) != key )
	   { Sdprintf("Stack checksum failure\n");
	     trap_gdb();
	   });
    if ( verbose )
    { Sdprintf("%.2f sec.\n", time);
    }

    succeed;
  }

  fail;
}

#endif /*O_SHIFT_STACKS*/

#ifdef O_ATOMGC

		 /*******************************
		 *	      ATOM-GC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  routine  markAtomsOnStacks(PL_local_data_t  *ld)  marks  all  atoms
reachable  from  the  global  stack,    environments,  choicepoints  and
term-references  using  markAtom().  It  is    designed   to  allow  for
asynchronous calling, even from different   threads (hence the argument,
although the thread examined should be stopped). 

Asynchronous calling is in general not  possible,   but  here we make an
exception. markAtom() is supposed  to  test   for  and  silently  ignore
non-atoms. Basically, this implies we can   mark a few atoms incorrectly
from the interrupted frame, but in   the context of multi-threading this
is a small price to pay.

Otherwise  this  routine  is  fairly  trivial.   It  is  modelled  after
checkStacks(), a simple routine for  checking stack-consistency that has
to walk along all reachable data as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG_ATOMGC
extern IOSTREAM * atomLogFd;		/* for error messages */

static long
loffset(void *p)
{ if ( p == NULL )
    return 0;

  assert((long)p % sizeof(word) == 0);
  return (Word)p-(Word)lBase;
}
#endif

static void
markAtomsOnGlobalStack(PL_local_data_t *ld)
{ Word gbase = ld->stacks.global.base;
  Word gtop  = ld->stacks.global.top;
  Word current;

  for(current = gbase; current < gtop; current += (offset_cell(current)+1) )
  { if ( isAtom(*current) )
      markAtom(*current);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is much like  check_environments(),  but   as  we  might  be called
asynchronously, we have to be a bit careful about the first frame (if PC
== NULL). The interpreter will  set  the   clause  field  to NULL before
opening the frame, and we only have   to  consider the arguments. If the
frame has a clause we must consider all variables of this clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
mark_atoms_in_environments(PL_local_data_t *ld, LocalFrame fr)
{ Code PC = NULL;

  if ( fr == NULL )
    return NULL;

  for(;;)
  { int slots, n;
    Word sp;

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
#ifdef O_DEBUG_ATOMGC
    if ( atomLogFd )
      Sfprintf(atomLogFd,
	       "Marking atoms from [%d] %s\n",
	       levelFrame(fr),
	       predicateName(fr->predicate));
#endif
    ld->gc._local_frames++;
    clearUninitialisedVarsFrame(fr, PC);

    if ( true(fr->predicate, FOREIGN) ||
	 !fr->clause )
      slots = fr->predicate->functor->arity;
    else
      slots = fr->clause->clause->prolog_vars;

    sp = argFrameP(fr, 0);
    for( n=0; n < slots; n++, sp++ )
    { if ( isAtom(*sp) )
	markAtom(*sp);
    }

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else
    { QueryFrame qf = (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
      return qf;
    }
  }
}


static void
markAtomsInTermReferences(PL_local_data_t *ld)
{ FliFrame   ff = ld->foreign_environment;
  LocalFrame fr = ld->environment;
  
					/* see finish_foreign_frame() */
  if ( (void *)fr < (void *)ff )
  { Word ltop = (Word)ld->stacks.local.top;
    
    ff->size = ltop - (Word)addPointer(ff, sizeof(struct fliFrame));
  }
  
  for(; ff; ff = ff->parent )
  { Word sp = refFliP(ff, 0);
    int n = ff->size;

    for(n=0 ; n < ff->size; n++ )
    { if ( isAtom(sp[n]) )
	markAtom(sp[n]);
    }
  }
}


static void
markAtomsInEnvironments(PL_local_data_t *ld)
{ QueryFrame qf;
  LocalFrame fr;
  Choice ch;

  ld->gc._local_frames = 0;

  for( fr = ld->environment,
       ch = ld->choicepoints
     ; fr
     ; fr = qf->saved_environment,
       ch = qf->saved_bfr
     )
  { qf = mark_atoms_in_environments(ld, fr);
    assert(qf->magic == QID_MAGIC);

    for(; ch; ch = ch->parent)
    {
#ifdef O_DEBUG_ATOMGC
      if ( atomLogFd )
	Sfprintf(atomLogFd, "Marking atoms from choicepoint #%ld on %s\n",
		 local(ch), predicateName(ch->frame->predicate));
#endif
      mark_atoms_in_environments(ld, ch->frame);
    }
  }

  unmark_stacks(ld, ld->environment, ld->choicepoints);

  assert(ld->gc._local_frames == 0);
}


void
markAtomsOnStacks(PL_local_data_t *ld)
{ markAtomsOnGlobalStack(ld);
  markAtomsInEnvironments(ld);
  markAtomsInTermReferences(ld);
}

#endif /*O_ATOMGC*/

