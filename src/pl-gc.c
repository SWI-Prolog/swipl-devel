/*  $Id$


    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Garbage Collection
*/

#ifdef SECURE_GC
#define O_DEBUG 1
#define O_SECURE 1
#endif
#include "pl-incl.h"

#ifndef HAVE_MEMMOVE			/* Note order!!!! */
#define memmove(dest, src, n) bcopy(src, dest, n)
#endif

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
this is simple and save and allows us to  debug  the  garbage  collector
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
#ifdef AVOID_0X80000000_BIT
#define is_ref(w)	(mask((word)(w)) == REF_MASK)
#else
#define is_ref(w)	isRef(w)
#endif

#define get_value(p)	(*(p) & VALUE_MASK)
#define set_value(p, w)	{ *(p) &= GC_MASK; *(p) |= w; }

#define inShiftedArea(area, shift, ptr) \
	((char *)ptr >= (char *)stacks.area.base + shift && \
	 (char *)ptr <  (char *)stacks.area.max + shift )
#define topPointerOnStack(name, addr) \
	((char *)(addr) >= (char *)stacks.name.base && \
	 (char *)(addr) <  (char *)stacks.name.max)

#define onGlobal(p)	onStackArea(global, p)
#define onLocal(p)	onStackArea(local, p)
#define onTrail(p)	topPointerOnStack(trail, p)

		 /*******************************
		 *     FUNCTION PROTOTYPES	*
		 *******************************/

forwards void		mark_variable(Word);
forwards void		mark_foreign(void);
forwards void		sweep_foreign(void);
forwards void		clear_uninitialised(LocalFrame, Code);
forwards QueryFrame	mark_environments(LocalFrame);
forwards TrailEntry	mark_choicepoints(LocalFrame, TrailEntry);
forwards void		mark_stacks(LocalFrame);
forwards void		mark_phase(LocalFrame);
forwards void		update_relocation_chain(Word, Word);
forwards void		into_relocation_chain(Word);
forwards void		compact_trail(void);
forwards void		sweep_mark(mark *);
forwards void		sweep_trail(void);
forwards LocalFrame	sweep_environments(LocalFrame);
forwards void		sweep_choicepoints(LocalFrame);
forwards void		sweep_stacks(LocalFrame);
forwards void		sweep_local(LocalFrame);
forwards bool		is_downward_ref(Word);
forwards bool		is_upward_ref(Word);
forwards void		compact_global(void);
forwards void		collect_phase(LocalFrame);

#if O_SECURE
forwards int		cmp_address(const void *, const void *);
forwards void		do_check_relocation(Word, char *file, int line);
forwards void		needsRelocation(Word);
forwards bool		scan_global(void);
#endif

		/********************************
		*           GLOBALS             *
		*********************************/

static long total_marked;	/* # marked global cells */
static long trailcells_deleted;	/* # garbage trailcells */
static long relocation_chains;	/* # relocation chains (debugging) */
static long relocation_cells;	/* # relocation cells */
static long relocated_cells;	/* # relocated cells */
static long needs_relocation;	/* # cells that need relocation */
static long local_marked;	/* # cells marked local -> global ptrs */
static long relocation_refs;	/* # refs that need relocation */
static long relocation_indirect;/* # indirects */
#if O_SHIFT_STACKS || O_SECURE
static long local_frames;	/* frame count for debugging */
static long trailtops_marked;
#endif


#if O_SECURE
		/********************************
		*           DEBUGGING           *
		*********************************/

static Word *mark_base;			/* Array of marked cells addresses */
static Word *mark_top;			/* Top of this array */
static Table check_table = NULL;	/* relocation address table */

#if 0
int
trap_gdb()				/* intended to put a break on */
{
}
#endif

static void
needsRelocation(Word addr)
{ needs_relocation++;

  addHTable(check_table, addr, (Void) TRUE);
}

static void
do_check_relocation(Word addr, char *file, int line)
{ Symbol s;
  if ( (s=lookupHTable(check_table, addr)) == NULL )
  { sysError("%s:%d: Address 0x%lx was not supposed to be relocated",
	     file, line, addr);
    return;
  }

  if ( s->value == FALSE )
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
{ if ( is_ref(w) )
    return onGlobal(unRef(w));

  if ( isIndirect(w) || isPointer(w) )
    return onGlobal(unMask(w));

  return 0;
}


static inline int
offset_cell(Word p)
{ word m = *p & DATA_TAG_MASK;		/* was get_value(p) */

  if ( m )
  { if ( m == REAL_MASK )
    { DEBUG(3, Sdprintf("REAL at 0x%p (m = 0x%lx)\n", p, m));
      return 1;
    }
    if ( m == STRING_MASK )
    { word w = get_value(p);
      long l = ((w) << DMASK_BITS) >> (DMASK_BITS+LMASK_BITS);
      DEBUG(3, Sdprintf("STRING ``%s'' at 0x%p (w = 0x%lx)\n",
			(char *) p, p+1, w));
      return allocSizeString(l) / sizeof(word) - 1;
    }
  }

  return 0;
}

static inline Word
previous_gcell(Word p)
{ p--;
  return p - offset_cell(p);
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

The marking algorithm forms a two-state  machine.   While  going  deeper
into  the reference tree, the pointers are reversed and the first bit is
set to indicate the choiche points created by complex terms with arity >
1.  Also the actuall mark bit is set  on  the  cells.   If  a  leafe  is
reached  the process reverses, restoring the old pointers.  If a `first'
mark is reached we are either finished, or have reached a choice  point,
in  which case the alternative is the cell above (structures are handled
last-argument-first).

Mark the tree of global stack cells,  referenced by the local stack word
`start'.  Things  are  a  bit  more  difficult  than  described  in  the
literature above as SWI-Prolog does not use   a  structure to describe a
general Prolog object, but just a  32   bits  long. This has performance
advantages as we can exploit  things   like  using  negative numbers for
references. It has some disadvantages here as we have to distinguis some
more cases. Strings and reals  on   the  stacks  complicate matters even
more.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FORWARD		goto forward
#define BACKWARD	goto backward

static void
mark_variable(Word start)
{ register Word current;		/* current cell examined */
  register word val;			/* old value of current cell */
  register Word next;			/* cell to be examined */

  DEBUG(3, Sdprintf("marking 0x%p\n", start));

  if ( marked(start) )
    sysError("Attempth to mark twice");

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

  if ( is_ref(val) )
  { next = unRef(val);			/* address pointing to */
    if ( next < gBase )
      sysError("REF pointer to 0x%p\n", next);
    needsRelocation(current);
    relocation_refs++;
    if ( is_first(next) )		/* ref to choice point. we will */
      BACKWARD;				/* get there some day anyway */
    val  = get_value(next);		/* invariant */
    set_value(next, makeRef(current));	/* create backwards pointer */
    DEBUG(5, Sdprintf("Marking REF from 0x%p to 0x%p\n", current, next));
    current = next;			/* invariant */
    FORWARD;
  }
  /* This is isTerm(); but that is not selective enough (TROUBLE) */
  if ( isPointer(val) &&
       isPointer(get_value((Word)val)) &&
       ((FunctorDef)get_value((Word)val))->type == FUNCTOR_TYPE )
  { int args;

    assert(val && !onGlobal(get_value((Word)val)));

    needsRelocation(current);
    next = (Word) val;			/* address of term on global stack */
    if ( next < gBase || next >= gTop )
      sysError("TERM pointer to 0x%p\n", next);
    if ( marked(next) )
      BACKWARD;				/* term has already been marked */
    args = functorTerm(val)->arity - 1;	/* members to flag first */
    DEBUG(5, Sdprintf("Marking TERM %s/%d at 0x%p\n", stringAtom(functorTerm(val)->name), args+1, next));
    domark(next);
    for( next += 2; args > 0; args--, next++ )
      mark_first(next);
    next--;				/* last cell of term */
    val = get_value(next);		/* invariant */
    set_value(next, (word)current);	/* backwards pointer (NO ref!) */
    current = next;
    FORWARD;
  }
  if ( isIndirect(val) )		/* string or real pointer */
  { next = (Word) unMask(val);

    if ( next < gBase )
      sysError("INDIRECT pointer from 0x%p to 0x%p\n", current, next);
    needsRelocation(current);
    relocation_indirect++;
    if ( marked(next) )			/* can be referenced from multiple */
      BACKWARD;				/* places */
    domark(next);
    DEBUG(3, Sdprintf("Marked indirect data type, size = %ld\n",
		    offset_cell(next) + 1));
    total_marked += offset_cell(next);
  }
  BACKWARD;

backward:  				/* reversing backwards */
  if ( !is_first(current) )
  { if ( is_ref(get_value(current)) )	/* internal cell */
    { next = unRef(get_value(current));
      set_value(current, val);		/* restore its value */
      val  = makeRef(current);		/* invariant */
      current = next;			/* invariant */
      BACKWARD;
    } else				/* first cell of term */
    { next = (Word) get_value(current);
      set_value(current, val);		/* elements of term ok now */
      val = (word)(current - 1);	/* invariant */
      current = next;
      BACKWARD;
    }
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
mark_foreign()
{ FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
  { Word sp = refFliP(fr, 0);
    int n = fr->size;

    DEBUG(1, Sdprintf("Marking %d PL_term_refs\n", n));

    assert(onStackArea(trail, fr->mark.trailtop));

    needsRelocation((Word) &fr->mark.trailtop);
    into_relocation_chain((Word) &fr->mark.trailtop);

    for( ; n-- > 0; sp++ )
    { if ( !marked(sp) )
      { if ( isGlobalRef(*sp) )
	  mark_variable(sp);
	else
	  ldomark(sp);      
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
clear_uninitialised(LocalFrame fr, Code PC);

Assuming the clause associated will resume   execution  at PC, determine
the variables that are not yet initialised and set them to be variables.
This  avoids  the  garbage  collector    considering  the  uninitialised
variables.

There are 5 instructions  that  assume   their  argument  variable to be
uninstantiated.  The instruction C_JMP is used   by if-then-else as well
as \+/1 (converted to (cond -> fail  ;   true))  to jump over the `else'
branch.  If we hit a C_JMP, we   will  have initialised all variables in
the `cond -> if' part and  the variable balancing instructions guarantee
the same variables will be initialised in the `else' brance.

[Q] wouldn't it be better to track  the variables that *are* initialised
and consider the others to be not?  Might   take more time, but might be
more reliable and simpler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
clear_uninitialised(LocalFrame fr, Code PC)
{ if ( PC != NULL )
  { Code branch_end = NULL;

    for( ; ; PC += (codeTable[decode(*PC)].arguments + 1))
    { switch(decode(*PC))
      { case I_EXIT:
	  return;
	case C_JMP:
	  if ( PC >= branch_end )
	    branch_end = PC + PC[1] + 2;
	  break;
	case B_FIRSTVAR:
	case B_ARGFIRSTVAR:
	case C_VAR:
#if O_SECURE	  
	  if ( varFrameP(fr, PC[1]) <
	       argFrameP(fr, fr->predicate->functor->arity) )
	    sysError("Reset instruction on argument");
#endif
	  if ( PC >= branch_end )
	  {
#if O_SECURE
	    assert(varFrame(fr, PC[1]) != QID_MAGIC);
#endif
	    setVar(varFrame(fr, PC[1]));
	  }
	  break;
      }
      if ( decode(*PC) > I_HIGHEST )
	sysError("GC: Illegal WAM instruction: %d", decode(*PC));
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Marking the environments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef offset
#define offset(s, f) ((int)(&((struct s *)NULL)->f))
#endif

static QueryFrame
mark_environments(LocalFrame fr)
{ Code PC = NULL;

  if ( !fr )
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

    clear_uninitialised(fr, PC);

    slots   = (PC == NULL ? fr->predicate->functor->arity : slotsFrame(fr));
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

#ifndef O_DESTRUCTIVE_ASSIGNMENT
#define isTrailValueP(x) 0
#endif

static TrailEntry
mark_choicepoints(LocalFrame bfr, TrailEntry te)
{ for( ; bfr; bfr = bfr->backtrackFrame )
  { Word top = argFrameP(bfr, bfr->predicate->functor->arity);

    for( ; te >= bfr->mark.trailtop; te-- )	/* early reset of vars */
    { if ( !isTrailValueP(te->address) )
      { if ( te->address >= top )
	{ te->address = (Word) NULL;
	  trailcells_deleted++;
	} else if ( !marked(te->address) )
	{ setVar(*te->address);
#if O_SECURE
	  assert(*te->address != QID_MAGIC);
#endif
	  DEBUG(3, Sdprintf("Early reset of 0x%p\n", te->address));
	  te->address = (Word) NULL;
	  trailcells_deleted++;
	}
      }
    }

    set(bfr, FR_CHOICEPT);
    assert(bfr->mark.trailtop != INVALID_TRAILTOP);
    needsRelocation((Word)&bfr->mark.trailtop);
    into_relocation_chain((Word)&bfr->mark.trailtop);
    SECURE(trailtops_marked--);

    mark_environments(bfr);
  }

  return te;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mark_stacks() will mark all data that  is   reachable  from any frame or
choicepoint. In addition, it  will  do   `early  reset'  on variables of
choicepoints that will be  reset  anyway   if  backtracking  reaches the
choicepoint. Also, it  will  insert  all   trailtops  of  marks  in  the
relocation chains. A small problem is  the   top-goal  of  a query, This
frame may not be a  choicepoint,  but   its  mark  is  needed anyhow for
PL_close_query(), so it has to be relocated.  `te' in the function below
has to be updated as none of these variables should be reset
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
mark_stacks(LocalFrame fr)
{ QueryFrame query;
  TrailEntry te = tTop - 1;

  trailcells_deleted = 0;

  for( ; fr; fr = query->saved_environment )
  { query = mark_environments(fr);
    assert(query->magic == QID_MAGIC);
    te    = mark_choicepoints(fr, te);

    if ( false(&query->frame, FR_CHOICEPT) ) /* top one is always choicept */
    { LocalFrame bfr = &query->frame;

      set(bfr, FR_CHOICEPT);
      assert(te >= bfr->mark.trailtop - 1);
      te = bfr->mark.trailtop - 1;
      needsRelocation((Word)&bfr->mark.trailtop);
      into_relocation_chain((Word)&bfr->mark.trailtop);
      SECURE(trailtops_marked--);
    }
  }
  
  DEBUG(2, Sdprintf("Trail stack garbage: %ld cells\n", trailcells_deleted));
}


#ifdef O_DESTRUCTIVE_ASSIGNMENT
static void
mark_trail()
{ TrailEntry te = tTop - 1;

  for( ; te >= tBase; te-- )
  { Word gp;

    if ( isTrailValueP(te->address) )
    { gp = trailValueP(te->address);

      assert(onGlobal(gp));
      if ( !marked(gp) )
      { local_marked--;			/* fix counters */
	total_marked++;
	mark_variable(gp);
      }
    }
  }
}
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/


#if O_SECURE
static int
cmp_address(const void *vp1, const void *vp2)
{ Word p1 = *((Word *)vp1);
  Word p2 = *((Word *)vp2);

  return p1 > p2 ? 1 : p1 == p2 ? 0 : -1;
}
#endif

static void
mark_phase(LocalFrame fr)
{ total_marked = 0;

  mark_stacks(fr);
  mark_foreign();

#if O_SECURE
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
{ if ( is_first(current) )
  { Word head = current;
    word val = get_value(current);

    DEBUG(3, Sdprintf("unwinding relocation chain at 0x%p to 0x%p\n",
		      current, dest));

    do
    { unmark_first(current);
      if ( is_ref(val) )
      { current = unRef(val);
        val = get_value(current);
	DEBUG(3, Sdprintf("Ref from 0x%p\n", current));
        set_value(current, makeRef(dest));
      } else if ( isIndirect(val) )
      { current = (Word)unMask(val);
        val = get_value(current);
        DEBUG(3, Sdprintf("Indirect link from 0x%p\n", current));
        set_value(current, (word)dest | INDIRECT_MASK);
      } else
      { current = (Word) val;
        val = get_value(current);
        DEBUG(3, Sdprintf("Pointer from 0x%p\n", current));
        set_value(current, (word)dest);
      }
      relocated_cells++;
    } while( is_first(current) );

    set_value(head, val);
    relocation_chains--;
  }
}


static void
into_relocation_chain(Word current)
{ Word head;
  word val = get_value(current);
  
  if ( is_ref(val) )
  { head = unRef(val);
    set_value(current, get_value(head));
    set_value(head, makeRef(current));
    relocation_refs--;
  } else if ( isIndirect(val) )
  { head = (Word)unMask(val);
    set_value(current, get_value(head));
    set_value(head, (word)current | INDIRECT_MASK);
    relocation_indirect--;
  } else
  { head = (Word) val;
    set_value(current, get_value(head));
    set_value(head, (word)current);
  }
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trail stack compacting.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
compact_trail(void)
{ TrailEntry dest, current;
  
	/* compact the trail stack */
  for( dest = current = tBase; current < tTop; )
  { if ( is_first((Word) current) )
      update_relocation_chain((Word) current, (Word) dest);
#if O_SECURE
    else
    { Symbol s;
      if ( (s=lookupHTable(check_table, current)) != NULL && s->value == TRUE )
        sysError("0x%p was supposed to be relocated (*= 0x%p)",
		 current, current->address);
    }
#endif

    if ( current->address != (Word) NULL )
      *dest++ = *current++;
    else
      current++;
  }
  if ( is_first((Word) current) )
    update_relocation_chain((Word) current, (Word) dest);

  tTop = dest;

  if ( relocated_cells != relocation_cells )
    sysError("After trail: relocation cells = %ld; relocated_cells = %ld\n",
	relocation_cells, relocated_cells);
} 

static void
sweep_mark(mark *m)
{ Word gm, prev;

  gm = m->globaltop;
  for(;;)
  { if ( gm == gBase )
    { m->globaltop = gm;
      break;
    }
    prev = previous_gcell(gm);
    if ( marked(prev) )
    { m->globaltop = gm;
      DEBUG(3, Sdprintf("gTop mark from choice point: "));
      needsRelocation((Word) &m->globaltop);
      into_relocation_chain((Word) &m->globaltop);
      break;
    }
    gm = prev;
  }
}


static void
sweep_foreign()
{ FliFrame fr = fli_context;

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
	  into_relocation_chain(sp);
	}
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sweeping the local and trail stack to insert necessary pointers  in  the
relocation chains.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
sweep_trail(void)
{ TrailEntry te = tTop - 1;

  for( ; te >= tBase; te-- )
  {
#ifdef O_DESTRUCTIVE_ASSIGNMENT
    if ( isTrailValueP(te->address) )
    { relocation_indirect++;
      needsRelocation((Word) &te->address);
      into_relocation_chain((Word) &te->address);
    } else
#endif
    if ( onGlobal(te->address) )
    { needsRelocation((Word) &te->address);
      into_relocation_chain((Word) &te->address);
    }
  }
}


static LocalFrame
sweep_environments(LocalFrame fr)
{ Code PC = NULL;

  if ( fr == (LocalFrame) NULL )
    return (LocalFrame) NULL;

  for( ; ; )
  { int slots;
    Word sp;

    if ( false(fr, FR_MARKED) )
      return (LocalFrame) NULL;
    clear(fr, FR_MARKED);

    if ( false(fr, FR_CHOICEPT) )
    { fr->mark.trailtop = INVALID_TRAILTOP;
      fr->mark.globaltop = INVALID_GLOBALTOP;
      SECURE(trailtops_marked--);
    } else
      clear(fr, FR_CHOICEPT);

    slots   = (PC == NULL ? fr->predicate->functor->arity : slotsFrame(fr));

    sp = argFrameP(fr, 0);
    for( ; slots > 0; slots--, sp++ )
    { if ( marked(sp) )
      { unmark(sp);
	if ( isGlobalRef(get_value(sp)) )
	{ local_marked--;
	  check_relocation(sp);
	  into_relocation_chain(sp);
	}
      }
    }

    PC = fr->programPointer;
    if ( fr->parent != NULL )
      fr = fr->parent;
    else
      return parentFrame(fr);	/* Prolog --> C --> Prolog calls */
  }
}


static void
sweep_choicepoints(LocalFrame bfr)
{ for( ; bfr != (LocalFrame)NULL; bfr = bfr->backtrackFrame )
  { sweep_environments(bfr);
    sweep_mark(&bfr->mark);
  }
}

static void
sweep_stacks(LocalFrame fr)
{ LocalFrame pfr;

  if ( (pfr = sweep_environments(fr)) != NULL )
    sweep_stacks(pfr);

  sweep_choicepoints(fr);
}


static void
sweep_local(LocalFrame fr)
{ sweep_stacks(fr);

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

On win32s, the reference mask-bit  is   used  for distinguishing between
real and string on the global  stack   (indirect  data). This implies we
have to test isIndirect() before is_ref() here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
is_downward_ref(Word p)
{ word val = get_value(p);
  Word ptr;

  if ( is_ref(val) )
  { DEBUG(5, if ( unRef(val) < p ) Sdprintf("REF: "));
    return unRef(val) < p;
  }
  if ( isVar(val) || isInteger(val) )
    fail;
  if ( isIndirect(val) )
  { ptr = (Word) unMask(val);

    assert(onGlobal(ptr));

    if ( ptr < p )
    { DEBUG(5, Sdprintf("INDIRECT: "));
      succeed;
    }

    fail;
  }

  ptr = (Word) val;
  if ( ptr < p && ptr >= gBase )
  { assert(marked(ptr));

    succeed;
  }

  fail;
}

static bool
is_upward_ref(Word p)
{ word val = get_value(p);
  Word ptr;

  if ( is_ref(val) )
    return unRef(val) > p;
  if ( isVar(val) || isInteger(val) )
    fail;
  if ( isIndirect(val) )
  { ptr = (Word) unMask(val);

    assert(onGlobal(ptr));

    if ( ptr > p )
      succeed;

    fail;
  }

  ptr = (Word) val;
  if ( ptr > p && ptr < gTop )
  { assert(marked(ptr));
    succeed;
  }

  fail;
}

static void
compact_global(void)
{ Word dest, current;
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
      update_relocation_chain(current, dest);
      if ( is_downward_ref(current) )
      { check_relocation(current);
	into_relocation_chain(current);
      }
    } else
    { update_relocation_chain(current, dest);	/* gTop refs from marks */
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

      update_relocation_chain(current, dest);

      if ( (l = offset_cell(current)) == 0 )	/* normal cells */
      { *dest = *current;
        if ( is_upward_ref(current) )
	{ check_relocation(current);
          into_relocation_chain(dest);
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
collect_phase(LocalFrame fr)
{
  DEBUG(2, Sdprintf("Sweeping foreign references\n"));
  sweep_foreign();
  DEBUG(2, Sdprintf("Sweeping trail stack\n"));
  sweep_trail();
  DEBUG(2, Sdprintf("Sweeping local stack\n"));
  sweep_local(fr);
  DEBUG(2, Sdprintf("Compacting global stack\n"));
  compact_global();

  if ( relocation_chains != 0 )
    sysError("relocation chains = %ld", relocation_chains);
  if ( relocated_cells != relocation_cells ||
       relocated_cells != needs_relocation ||
       relocation_refs != 0 || relocation_indirect != 0)
    sysError("relocation cells = %ld; relocated_cells = %ld, "
	     "needs_relocation = %ld\n\t"
	     "relocation_refs = %ld, relocation_indirect = %ld",
	     relocation_cells, relocated_cells, needs_relocation,
	     relocation_refs, relocation_indirect);
}


		/********************************
		*             MAIN              *
		*********************************/

#if O_DYNAMIC_STACKS
void
considerGarbageCollect(Stack s)
{ if ( s->gc && trueFeature(GC_FEATURE) )
  { if ( (char *)s->top - (char *)s->base > (long)(s->factor*s->gced_size + s->small) )
    { DEBUG(1, Sdprintf("%s overflow: Posted garbage collect request\n",
			s->name));
      gc_status.requested = TRUE;
    }
  }
}
#endif /* O_DYNAMIC_STACKS */


#if O_SECURE
static bool
scan_global()
{ Word current;
  int errors = 0;
  long cells = 0;

  for( current = gBase; current < gTop; current += (offset_cell(current)+1) )
  { cells++;
    if ( onGlobal(*current) && !isTerm(*current) )
      sysError("Pointer on global stack is not a term");
    if ( marked(current) || is_first(current) )
    { warning("Illegal cell in global stack (up) at 0x%p (*= 0x%p)", current, *current);
      if ( isAtom(*current) )
	warning("0x%p is atom %s", current, stringAtom(*current));
      if ( isTerm(*current) )
	warning("0x%p is term %s/%d", current,
				       stringAtom(functorTerm(*current)->name),
				       functorTerm(*current)->arity);
      if ( ++errors > 10 )
      { Sdprintf("...\n");
        break;
      }
    }
  }

  for( current = gTop - 1; current >= gBase; current-- )
  { cells --;
    current -= offset_cell(current);
    if ( marked(current) || is_first(current) )
    { warning("Illegal cell in global stack (down) at 0x%p (*= 0x%p)", current, *current);
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
{ if ( m->trailtop == INVALID_TRAILTOP )
  { assert(m->globaltop == INVALID_GLOBALTOP);
  } else
  { assert(onTrail(m->trailtop));
    assert(onGlobal(m->globaltop));
  }
}


static QueryFrame
check_environments(fr)
LocalFrame fr;
{ Code PC = NULL;

  if ( fr == NULL )
    return NULL;

  for(;;)
  { int slots, n;
    Word sp;

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    local_frames++;
    clear_uninitialised(fr, PC);

    check_mark(&fr->mark);

    DEBUG(3, Sdprintf("Check [%ld] %s:",
		      levelFrame(fr),
		      predicateName(fr->predicate)));

    slots   = (PC == NULL ? fr->predicate->functor->arity : slotsFrame(fr));
    sp = argFrameP(fr, 0);
    for( n=0; n < slots; n++ )
    { key += checkData(&sp[n], FALSE);
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
check_choicepoints(bfr)
LocalFrame bfr;
{ for( ; bfr; bfr = bfr->backtrackFrame )
  { check_environments(bfr);
  }
}


static LocalFrame
lunmark_environments(fr)
LocalFrame fr;
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { if ( false(fr, FR_MARKED) )
      return NULL;
    clear(fr, FR_MARKED);
    local_frames--;
    
    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
      return parentFrame(fr);
  }
}


static void
lunmark_choicepoints(bfr)
LocalFrame bfr;
{ for( ; bfr; bfr = bfr->backtrackFrame )
    lunmark_environments(bfr);
}


word
checkStacks(frame)
LocalFrame frame;
{ LocalFrame fr, fr2;
  QueryFrame qf;

  if ( !frame )
    frame = environment_frame;

  local_frames = 0;
  key = 0L;

  for( fr = frame; fr; fr = qf->saved_environment )
  { qf = check_environments(fr);
    assert(qf->magic == QID_MAGIC);

    check_choicepoints(fr->backtrackFrame);
  }

  SECURE(trailtops_marked = local_frames);

  for( fr = frame; fr; fr = fr2 )
  { fr2 = lunmark_environments(fr);

    lunmark_choicepoints(fr->backtrackFrame);
  }

  assert(local_frames == 0);

  { FliFrame ff;

    for(ff = fli_context; ff; ff = ff->parent )
      check_mark(&ff->mark);
  }

  return key;
}

#endif /*O_SECURE || O_DEBUG*/


void
garbageCollect(LocalFrame fr)
{ long tgar, ggar;
  real t = CpuTime();
  int verbose = trueFeature(TRACE_GC_FEATURE);

  DEBUG(0, verbose = TRUE);

#if O_SECURE
  key = checkStacks(fr);
#endif

  if ( gc_status.blocked || !trueFeature(GC_FEATURE) )
    return;
  gc_status.requested = FALSE;

  gc_status.active = TRUE;
  finish_foreign_frame();
  if ( verbose )
  { Putf("Garbage collect ... ");
    pl_flush();
  }
#ifdef O_PROFILE
  PROCEDURE_garbage_collect0->definition->profile_calls++;
#endif
#if O_SECURE
  if ( !scan_global() )
    sysError("Stack not ok at gc entry");

  key = checkStacks(fr);

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
  relocation_refs   = 0;
  relocation_indirect = 0;

  STACKVERIFY( if ( gTop + 1 >= gMax ) outOf((Stack) &stacks.global) );
  setVar(*gTop);
  STACKVERIFY( if ( tTop + 1 >= tMax ) outOf((Stack) &stacks.trail) );
  tTop->address = NULL;

  mark_phase(fr);
#ifdef O_DESTRUCTIVE_ASSIGNMENT
  mark_trail();
#endif
  tgar = trailcells_deleted * sizeof(struct trail_entry);
  ggar = (gTop - gBase - total_marked) * sizeof(word);
  gc_status.trail_gained  += tgar;
  gc_status.global_gained += ggar;
  gc_status.collections++;

  DEBUG(2, Sdprintf("Compacting trail ... "));
  compact_trail();

  collect_phase(fr);
#if O_SECURE
  assert(trailtops_marked == 0);
  if ( !scan_global() )
    sysError("Stack not ok after gc; gTop = 0x%p", gTop);
  free(mark_base);
#endif

  gc_status.time += CpuTime() - t;
  trimStacks();

  SECURE(if ( checkStacks(fr) != key )
	 { Sdprintf("Stack checksum failure\n");
	   trap_gdb();
	 } else
	   Putf("(OK) "));

  if ( verbose )
  { Putf("(gained %ld+%ld; used: %d+%d; free: %d+%d bytes)\n",
	 ggar, tgar,
	 usedStack(global), usedStack(trail),
	 roomStack(global), roomStack(trail));
  }

  gc_status.active = FALSE;
}

word
pl_garbage_collect(term_t d)
{ LocalFrame fr = environment_frame;

#if O_DEBUG
  int ol = status.debugLevel;
  int nl;

  if ( !PL_get_integer(d, &nl) )
    return warning("garbage_collect/1: instantiation fault");
  status.debugLevel = nl;
#endif
  finish_foreign_frame();
  garbageCollect(fr);
#if O_DEBUG
  status.debugLevel = ol;
#endif
  succeed;
}

void
resetGC(void)
{ gc_status.requested = FALSE;
  gc_status.blocked = 0;
  gc_status.collections = gc_status.global_gained = gc_status.trail_gained = 0;
  gc_status.time = 0.0;

#if O_SHIFT_STACKS
  shift_status.local_shifts = 0;
  shift_status.global_shifts = 0;
  shift_status.trail_shifts = 0;
  shift_status.blocked = 0;
  shift_status.time = 0.0;
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update a mark (either held by foreign code or on the local stack).  Just
update both pointers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
update_pointer(void *p, long offset)
{ char **ptr = ((char **)p);

  if ( *ptr )
    *ptr += offset;
}



static void
update_mark(mark *m, long gs, long ts)
{ DEBUG(3, Sdprintf("Updating mark\n"));

  update_pointer(&m->trailtop, ts);
  update_pointer(&m->globaltop, gs);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update a variable.  The variable may either live on the local- or global
stack.  If it is a reference  it might  be  both to  a global- and local
address.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
update_variable(Word sp, long ls, long gs)
{ if ( is_ref(*sp) )
  { if ( onGlobal(unRef(*sp)) )
      *sp = makeRef(addPointer(unRef(*sp), gs));
    else if ( onLocal(unRef(*sp)) )
      *sp = makeRef(addPointer(unRef(*sp), ls));
    else
      DEBUG(0, Sdprintf("Reference 0x%p neither to local- nor global stack\n",
		      unRef(*sp)));
    return;
  }

  if ( gs )
  { if ( isPointer(*sp) )
    { if ( onGlobal(*sp) )
	*sp = (word) addPointer(*sp, gs);
    } else if ( isIndirect(*sp) )
    { if ( onGlobal(unMask(*sp)) )
	*sp = ((word) addPointer(unMask(*sp), gs)) | INDIRECT_MASK;
    }
  }
}


		 /*******************************
		 *	   LOCAL STACK		*
		 *******************************/

static void update_choicepoints(LocalFrame, long, long, long);

static LocalFrame
update_environments(LocalFrame fr, Code PC, long ls, long gs, long ts)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { int slots;
    Word sp;
    
    assert(inShiftedArea(local, ls, fr));

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    local_frames++;
    
    DEBUG(2,
	  Sdprintf("Shifting frame 0x%p [%ld] %s ... ",
		 fr, levelFrame(fr), predicateName(fr->predicate)));

    if ( ls )				/* update frame pointers */
    { if ( fr->parent )
	fr->parent = (LocalFrame) addPointer(fr->parent, ls);
      if ( fr->backtrackFrame )
	fr->backtrackFrame = (LocalFrame) addPointer(fr->backtrackFrame, ls);
    }

    update_mark(&fr->mark, gs, ts);	/* trail and global marks */

    if ( gs || ls )			/* update variables */
    { clear_uninitialised(fr, PC);

      slots = (PC == NULL ? fr->predicate->functor->arity : slotsFrame(fr));
      sp = argFrameP(fr, 0);
      DEBUG(2, Sdprintf("\n\t%d slots in 0x%p ... 0x%p ... ",
		      slots, sp, sp+slots));
      for( ; slots > 0; slots--, sp++ )
	update_variable(sp, ls, gs);
      
					/* update saved BFR's from C_MARK */
      if ( ls && PC && false(fr->predicate, FOREIGN) )
      { Clause cl = fr->clause->clause;
	int saved_bfrs = cl->variables - cl->prolog_vars;
	
	for( ; saved_bfrs-- > 0; sp++ )
	  update_pointer(sp, ls);
      }
    }

    DEBUG(2, Sdprintf("ok\n"));

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
    { QueryFrame query = (QueryFrame)addPointer(fr, -offset(queryFrame,frame));

      if ( ls )
      { update_pointer(&query->bfr, ls);
	update_pointer(&query->saved_environment, ls);
	update_pointer(&query->registers.fr, ls);
	update_pointer(&query->registers.bfr, ls);
      }
      
      return query->saved_environment;	/* parent frame */
    }
  }
}


static void
update_choicepoints(LocalFrame bfr, long ls, long gs, long ts)
{ for( ; bfr; bfr = bfr->backtrackFrame )
  { DEBUG(1, Sdprintf("Updating choicepoints from 0x%p [%ld] %s ... ",
		      bfr, levelFrame(bfr), predicateName(bfr->predicate)));
    update_environments(bfr, NULL, ls, gs, ts);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clear the marks set by update_environments().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
unmark_environments(LocalFrame fr)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { if ( false(fr, FR_MARKED) )
      return NULL;
    clear(fr, FR_MARKED);
    local_frames--;
    
    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
      return parentFrame(fr);
  }
}


static void
unmark_choicepoints(LocalFrame bfr)
{ for( ; bfr; bfr = bfr->backtrackFrame )
    unmark_environments(bfr);
}

		 /*******************************
		 *	   GLOBAL STACK		*
		 *******************************/

#if O_DEBUG
Word
findGRef(n)
int n;
{ Word p = gBase;
  Word t = gTop;
  Word to = &gBase[n];

  for(; p < t; p++)
    if ( is_ref(*p) && unRef(*p) == to )
      return p;

  return NULL;
}
#endif

static void
update_global(long gs)
{ Word p = addPointer(gBase, gs);
  Word t = addPointer(gTop, gs);

  for( ; p < t; p++ )
  { int offset = offset_cell(p);

    if ( offset )
      p += offset;
    else
      update_variable(p, 0, gs);
  }
}


		 /*******************************
		 *	    TRAIL STACK		*
		 *******************************/

static void
update_trail(long ts, long ls, long gs)
{ TrailEntry p = addPointer(tBase, ts);
  TrailEntry t = addPointer(tTop, ts);

  for( ; p < t; p++ )			/* correct for setarg/3? */
  { if ( onGlobal(p->address) )
    { p->address = addPointer(p->address, gs);
    } else
    { assert(onLocal(p->address));
      p->address = addPointer(p->address, ls);
    }
  }
}

		 /*******************************
		 *	  ARGUMENT STACK	*
		 *******************************/

static void
update_argument(long ls, long gs)
{ Word *p = aBase;
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
		 *	  FOREIGN FRAMES	*
		 *******************************/

static void
update_foreign(long ts, long ls, long gs)
{ FliFrame fr = addPointer(fli_context, ls);

  for( ; fr; fr = fr->parent )
  { Word sp = refFliP(fr, 0);
    int n = fr->size;

    for( ; n-- > 0; sp++ )
      update_variable(sp, ls, gs);

    update_mark(&fr->mark, gs, ts);
    DEBUG(2, if ( gs ) Sdprintf("Updated foreign mark at 0x%x\n", &fr->mark));
    if ( fr->parent )
      fr->parent = addPointer(fr->parent, ls);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry-point.   Update the  stacks to  reflect  their current  positions.
This function should be called *after*  the  stacks have been relocated.
Note that these functions are  only used  if  there is no virtual memory
way to reach at dynamic stacks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define updateStackHeader(name, offset) \
	{ stacks.name.base = addPointer(stacks.name.base, offset); \
	  stacks.name.top  = addPointer(stacks.name.top,  offset); \
	  stacks.name.max  = addPointer(stacks.name.max,  offset); \
	}


static LocalFrame
updateStacks(frame, PC, lb, gb, tb)
LocalFrame frame;
Code PC;
Void lb, gb, tb;			/* bases addresses */
{ long ls, gs, ts;
  LocalFrame fr, fr2;

  ls = (long) lb - (long) lBase;
  gs = (long) gb - (long) gBase;
  ts = (long) tb - (long) tBase;

  DEBUG(2, Sdprintf("ls+gs+ts = %ld %ld %ld ... ", ls, gs, ts));

  if ( ls || gs || ts )
  { local_frames = 0;

    for(fr = addPointer(frame, ls); fr; fr = fr2, PC = NULL)
    { fr2 = update_environments(fr, PC, ls, gs, ts);

      update_choicepoints(fr->backtrackFrame, ls, gs, ts);
      DEBUG(1, if ( fr2 )
	         Sdprintf("Update frames of C-parent at 0x%p\n", fr2));
    }

    DEBUG(2, Sdprintf("%d frames ...", local_frames));

    for(fr = addPointer(frame, ls); fr; fr = fr2)
    { fr2 = unmark_environments(fr);

      unmark_choicepoints(fr->backtrackFrame);
    }
    assert(local_frames == 0);

    if ( gs )
      update_global(gs);
    if ( gs || ls )
    { update_trail(ts, ls, gs);
      update_argument(ls, gs);
    }
    update_foreign(ts, ls, gs);

    updateStackHeader(local,  ls);
    updateStackHeader(global, gs);
    updateStackHeader(trail,  ts);
  }

  if ( ls )
  { update_pointer(&environment_frame, ls);
    update_pointer(&fli_context, ls);
  }

  return addPointer(frame, ls);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry point from interpret()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GL_SEPARATION sizeof(word)

static long
nextStackSize(s)
Stack s;
{ long size = (char *) s->max - (char *) s->base;

  if ( size == s->limit )
    outOf(s);

  size = ROUND((size * 3) / 2, 4096);

  return size > s->limit ? s->limit : size;
}


int
growStacks(LocalFrame fr, Code PC, int l, int g, int t)
{ if ( fr == NULL || PC != NULL )	/* for now, only at the call-port */
    fail;

  if ( (l || g || t) && !shift_status.blocked )
  { TrailEntry tb = tBase;
    Word gb = gBase;
    LocalFrame lb = lBase;
    long lsize = sizeStack(local);
    long gsize = sizeStack(global);
    long tsize = sizeStack(trail);
    real time = CpuTime();
    int verbose = trueFeature(TRACE_GC_FEATURE);
    
    DEBUG(0, verbose = TRUE);

    if ( verbose )
    { int i = 0;

      Putf("Expanding ");
      if ( l ) Putf("%s%s", i++ ? "and " : "", "local ");
      if ( g ) Putf("%s%s", i++ ? "and " : "", "global ");
      if ( t ) Putf("%s%s", i++ ? "and " : "", "trail ");
      Putf("stacks ");
    }

    finish_foreign_frame();

    if ( !fr )
      fr = environment_frame;

    SECURE(key = checkStacks(fr));

    if ( t )
    { tsize = nextStackSize((Stack) &stacks.trail);
      tb = xrealloc(tb, tsize);
      shift_status.trail_shifts++;
    }

    if ( g || l )
    { long loffset = gsize + GL_SEPARATION;
      assert(lb == addPointer(gb, loffset));	

      if ( g )
      { gsize = nextStackSize((Stack) &stacks.global);
	shift_status.global_shifts++;
      }
      if ( l )
      { lsize = nextStackSize((Stack) &stacks.local);
	shift_status.local_shifts++;
      }

      gb = xrealloc(gb, lsize + gsize + GL_SEPARATION);
      lb = addPointer(gb, gsize + GL_SEPARATION);
      if ( g )				/* global enlarged; move local */
	memmove(lb,   addPointer(gb, loffset), lsize);
	     /* dest, src,                     size */
    }
      
    if ( verbose )
    { Putf("to (l+g+t) = %d+%d+%d Kbytes ... ", lsize, gsize, tsize);
      pl_flush();
    }

#define PrintStackParms(stack, name, newbase, newsize) \
	{ Sdprintf("%6s: 0x%08lx ... 0x%08lx --> 0x%08lx ... 0x%08lx\n", \
		 name, \
		 (unsigned long) stacks.stack.base, \
		 (unsigned long) stacks.stack.max, \
		 (unsigned long) newbase, \
		 (unsigned long) addPointer(newbase, newsize)); \
	}


    DEBUG(0, { Sputchar('\n');
	       PrintStackParms(global, "global", gb, gsize);
	       PrintStackParms(local, "local", lb, lsize);
	       PrintStackParms(trail, "trail", tb, tsize);
	     });
		    
    DEBUG(1, Sdprintf("Updating stacks ..."));
    fr = updateStacks(fr, PC, lb, gb, tb);

    stacks.local.max  = addPointer(stacks.local.base,  lsize);
    stacks.global.max = addPointer(stacks.global.base, gsize);
    stacks.trail.max  = addPointer(stacks.trail.base,  tsize);

    SetHTop(stacks.local.max);
    SetHTop(stacks.trail.max);

    shift_status.time += CpuTime() - time;
    if ( verbose )
    { Putf("done\n");
    }

    SECURE(if ( checkStacks(fr) != key )
	   { Sdprintf("Stack checksum failure\n");
	     trap_gdb();
	   });

    succeed;
  }

  fail;
}

#endif /*O_SHIFT_STACKS*/


