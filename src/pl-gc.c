/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Garbage Collection
*/

/*#define O_DEBUG 1*/
/*#define O_SECURE 1*/
#include "pl-incl.h"

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
#define recordMark(p)	{ if ( (p) < gTop ) *mark_top++ = (p); }
#else
#define recordMark(p)
#define needsRelocation(p) { needs_relocation++; }
#define check_relocation(p)
#endif

#define ldomark(p)	{ *(p) |= MARK_MASK; }
#define domark(p)	{ if ( marked(p) ) sysError("marked twice: 0x%lx (*= 0x%lx), gTop = 0x%lx", p, *(p), gTop); \
			  *(p) |= MARK_MASK; \
			  total_marked++; \
			  recordMark(p); \
			  DEBUG(4, printf("marked(0x%lx)\n", p)); \
			}
#define unmark(p)	(*(p) &= ~MARK_MASK)
#define marked(p)	(*(p) & MARK_MASK)

#define mark_first(p)	(*(p) |= FIRST_MASK)
#define unmark_first(p)	(*(p) &= ~FIRST_MASK)
#define is_first(p)	(*(p) & FIRST_MASK)

#define get_value(p)	(*(p) & VALUE_MASK)
#define set_value(p, w)	{ *(p) &= GC_MASK; *(p) |= w; }

#define inShiftedArea(area, shift, ptr) \
	((char *)ptr >= (char *)stacks.area.base + shift && \
	 (char *)ptr <  (char *)stacks.area.max + shift )

#define onGlobal(p)	onStackArea(global, p)
#define onLocal(p)	onStackArea(local, p)
#define isGlobalRef(w)	(  ((isIndirect(w) || isPointer(w)) \
				&& onGlobal(unMask(w))) \
			|| (isRef(w) && onGlobal(unRef(w))))

#define L_MARK 	  0	/* Foreign reference marks */
#define L_WORD	  1
#define L_POINTER 2

forwards long		offset_cell P((Word));
forwards Word		previous_gcell P((Word));
forwards void		mark_variable P((Word));
forwards void		mark_foreign P((void));
forwards void		clear_uninitialised P((LocalFrame, Code));
forwards LocalFrame	mark_environments P((LocalFrame));
forwards void		mark_choicepoints P((LocalFrame));
forwards void		mark_stacks P((LocalFrame));
forwards void		mark_phase P((LocalFrame));
forwards void		update_relocation_chain P((Word, Word));
forwards void		into_relocation_chain P((Word));
forwards void		compact_trail P((void));
forwards void		sweep_mark P((mark *));
forwards void		sweep_foreign P((void));
forwards void		sweep_trail P((void));
forwards LocalFrame	sweep_environments P((LocalFrame));
forwards void		sweep_choicepoints P((LocalFrame));
forwards void		sweep_stacks P((LocalFrame));
forwards void		sweep_local P((LocalFrame));
forwards bool		is_downward_ref P((Word));
forwards bool		is_upward_ref P((Word));
forwards void		compact_global P((void));
forwards void		collect_phase P((LocalFrame));

#if O_SECURE
forwards int		cmp_address P((const void *, const void *));
forwards void		check_relocation P((Word));
forwards void		needsRelocation P((Word));
forwards bool		scan_global P((void));
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

#if O_SECURE
		/********************************
		*           DEBUGGING           *
		*********************************/

static Word *mark_base;			/* Array of marked cells addresses */
static Word *mark_top;			/* Top of this array */
static Table check_table = NULL;	/* relocation address table */

static void
needsRelocation(addr)
Word addr;
{ needs_relocation++;

  addHTable(check_table, addr, (Void) TRUE);
}

static void
check_relocation(addr)
Word addr;
{ Symbol s;
  if ( (s=lookupHTable(check_table, addr)) == NULL )
  { sysError("Address 0x%lx was not supposed to be relocated", addr);
    return;
  }

  if ( s->value == FALSE )
  { sysError("Relocated twice: 0x%lx", addr);
    return;
  }

  s->value = FALSE;
}
#endif /* O_SECURE */

		/********************************
		*          UTILITIES            *
		*********************************/

static long
offset_cell(p)
Word p;
{ word w = get_value(p);

  if ( (w & DATA_TAG_MASK) == REAL_MASK )
  { DEBUG(3, printf("REAL at 0x%lx (w = 0x%lx)\n", p, w));
    return 1;
  }
  if ( (w & DATA_TAG_MASK) == STRING_MASK )
  { long l = ((w) << DMASK_BITS) >> (DMASK_BITS+LMASK_BITS);
    DEBUG(3, printf("STRING ``%s'' at 0x%lx (w = 0x%lx)\n", p, p+1, w));
    return allocSizeString(l) / sizeof(word) - 1;
  }
  
  return 0;
}

static Word
previous_gcell(p)
Word p;
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

Mark the tree of global stack cells, referenced by the local stack  word
`start'.  Things are a bit more difficult than described in the liteture
above as SWI-Prolog does not use  a  structure  to  describe  a  general
Prolog object, but just a 32 bits long.  This has performance advantages
as we can exploit things like using negative numbers for references.  It
has  some  disadvantages  here as we have to distinguis some more cases.
Strings and reals on the stacks complicate matters even more.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FORWARD		goto forward
#define BACKWARD	goto backward

static void
mark_variable(start)
Word start;
{ register Word current;		/* current cell examined */
  register word val;			/* old value of current cell */
  register Word next;			/* cell to be examined */

  DEBUG(3, printf("marking 0x%lx\n", start));

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

  if ( isRef(val) )
  { next = unRef(val);			/* address pointing to */
    if ( next < gBase )
      sysError("REF pointer to 0x%lx\n", next);
    needsRelocation(current);
    relocation_refs++;
    if ( is_first(next) )		/* ref to choice point. we will */
      BACKWARD;				/* get there some day anyway */
    val  = get_value(next);		/* invariant */
    set_value(next, makeRef(current));	/* create backwards pointer */
    DEBUG(5, printf("Marking REF from 0x%lx to 0x%lx\n", current, next));
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
      sysError("TERM pointer to 0x%lx\n", next);
    if ( marked(next) )
      BACKWARD;				/* term has already been marked */
    args = functorTerm(val)->arity - 1;	/* members to flag first */
    DEBUG(5, printf("Marking TERM %s/%d at 0x%lx\n", stringAtom(functorTerm(val)->name), args+1, next));
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
      sysError("INDIRECT pointer from 0x%lx to 0x%lx\n", current, next);
    needsRelocation(current);
    relocation_indirect++;
    if ( marked(next) )			/* can be referenced from multiple */
      BACKWARD;				/* places */
    domark(next);
    DEBUG(3, printf("Marked indirect data type, size = %ld\n",
		    offset_cell(next) + 1));
    total_marked += offset_cell(next);
  }
  BACKWARD;

backward:  				/* reversing backwards */
  if ( !is_first(current) )
  { if ( isRef(get_value(current)) )	/* internal cell */
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
{ Lock l;

  for( l = pBase; l < pTop; l++ )
  { switch( l->type )
    { case L_WORD:
	{ Word sp = (Word) (l->value << 2);

	  if ( isGlobalRef(*sp) )
	  { DEBUG(5, printf("Marking foreign value at 0x%lx\n"));
	    mark_variable(sp);
	  }

	  break;
	}
      case L_POINTER:
	{ Word *sp = (Word *) (l->value << 2);

	  if ( !marked(*sp) && isGlobalRef(**sp) )
	  { DEBUG(5, printf("Marking foreign pointer at 0x%lx\n"));
	    mark_variable(*sp);
	  }

	  break;
	}
      case L_MARK:
	break;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Marking the environments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
clear_uninitialised(fr, PC)
LocalFrame fr;
Code PC;
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
	  if ( varFrameP(fr, PC[1]) < argFrameP(fr, fr->procedure->functor->arity) )
	    sysError("Reset instruction on argument");
	  if ( PC >= branch_end )
	    setVar(varFrame(fr, PC[1]));
	  break;
      }
      if ( decode(*PC) > I_HIGHEST )
	sysError("GC: Illegal WAM instruction: %d", decode(*PC));
    }
  }
}


static LocalFrame
mark_environments(fr)
LocalFrame fr;
{ Code PC = NULL;

  if ( fr == (LocalFrame) NULL )
    return (LocalFrame) NULL;

  for( ; ; )
  { int slots;
    Word sp;
    
    if ( true(fr, FR_MARKED) )
      return (LocalFrame) NULL;		/* from choicepoints only */
    set(fr, FR_MARKED);
    
    DEBUG(3, printf("Marking [%ld] %s\n",
		levelFrame(fr), procedureName(fr->procedure)));

    clear_uninitialised(fr, PC);

    slots = (PC == NULL ? fr->procedure->functor->arity : slotsFrame(fr));
    sp = argFrameP(fr, 0);
    for( ; slots > 0; slots--, sp++ )
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
      return parentFrame(fr);	/* Prolog --> C --> Prolog calls */
  }
}

static void
mark_choicepoints(bfr)
LocalFrame bfr;
{ TrailEntry te = tTop - 1;

  trailcells_deleted = 0;

  for( ; bfr != (LocalFrame)NULL; bfr = bfr->backtrackFrame )
  { Word top = argFrameP(bfr, bfr->procedure->functor->arity);

    for( ; te >= bfr->mark.trailtop; te-- )	/* early reset of vars */
    { if ( te->address >= top )
      { te->address = (Word) NULL;
        trailcells_deleted++;
      } else if ( !marked(te->address) )
      { setVar(*te->address);
        DEBUG(3, printf("Early reset of 0x%lx\n", te->address));
        te->address = (Word) NULL;
	trailcells_deleted++;
      }
    }
    needsRelocation((Word)&bfr->mark.trailtop);
    into_relocation_chain((Word)&bfr->mark.trailtop);

    mark_environments(bfr);
  }
  
  DEBUG(2, printf("Trail stack garbage: %ld cells\n", trailcells_deleted));
}

static void
mark_stacks(fr)
LocalFrame fr;
{ LocalFrame pfr;

  if ( (pfr = mark_environments(fr)) != NULL )
    mark_stacks(pfr);

  mark_choicepoints(fr);
}

#if O_SECURE
#ifdef __STDC__
static int
cmp_address(vp1, vp2)
const void *vp1, *vp2;
{ Word p1 = *((Word *)vp1);
  Word p2 = *((Word *)vp2);

  return p1 > p2 ? 1 : p1 == p2 ? 0 : -1;
}

#else

static int
cmp_address(p1, p2)
Word *p1, *p2;
{ return *p1 > *p2 ? 1 : *p1 == *p2 ? 0 : -1;
}
#endif
#endif

static void
mark_phase(fr)
LocalFrame fr;
{ total_marked = 0;

#if GC_FOREIGN
  mark_foreign();
#endif
  mark_stacks(fr);

#if O_SECURE
  qsort(mark_base, mark_top - mark_base, sizeof(Word), cmp_address);
#endif

  DEBUG(2, { long size = gTop - gBase;
	     printf("%ld referenced cell; %ld garbage (gTop = 0x%lx)\n",
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
update_relocation_chain(current, dest)
Word current, dest;
{ if ( is_first(current) )
  { Word head = current;
    word val = get_value(current);

    DEBUG(3, printf("unwinding relocation chain at 0x%lx to 0x%lx\n", current, dest));

    do
    { unmark_first(current);
      if ( isRef(val) )
      { current = unRef(val);
        val = get_value(current);
	DEBUG(3, printf("Ref from 0x%lx\n", current));
        set_value(current, makeRef(dest));
      } else if ( isIndirect(val) )
      { current = (Word)unMask(val);
        val = get_value(current);
        DEBUG(3, printf("Indirect link from 0x%lx\n", current));
        set_value(current, (word)dest | INDIRECT_MASK);
      } else
      { current = (Word) val;
        val = get_value(current);
        DEBUG(3, printf("Pointer from 0x%lx\n", current));
        set_value(current, (word)dest);
      }
      relocated_cells++;
    } while( is_first(current) );

    set_value(head, val);
    relocation_chains--;
  }
}


static void
into_relocation_chain(current)
Word current;
{ Word head;
  word val = get_value(current);
  
  if ( isRef(val) )
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
  DEBUG(2, printf("Into relocation chain: 0x%lx (head = 0x%lx)\n",
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
compact_trail()
{ TrailEntry dest, current;
  Lock l;
  
#if GC_FOREIGN
	/* get foreign references into the relocation chains */
  for( l = pBase; l < pTop; l++ )
  { if ( l->type == L_MARK )
    { mark *m = (mark *) (l->value << 2);
    
      if ( m->trailtop > tTop )
        sysError("Illegal trail mark from foreign code");

      DEBUG(5, printf("Foreign mark: "));
      needsRelocation((Word) &m->trailtop);
      into_relocation_chain((Word) &m->trailtop);
    }
  }
#endif

	/* compact the trail stack */
  for( dest = current = tBase; current < tTop; )
  { if ( is_first((Word) current) )
      update_relocation_chain((Word) current, (Word) dest);
#if O_SECURE
    else
    { Symbol s;
      if ( (s=lookupHTable(check_table, current)) != NULL && s->value == TRUE )
        sysError("0x%lx was supposed to be relocated (*= 0x%lx)",
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
sweep_mark(m)
mark *m;
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
      DEBUG(3, printf("gTop mark from choice point: "));
      needsRelocation((Word) &m->globaltop);
      into_relocation_chain((Word) &m->globaltop);
      break;
    }
    gm = prev;
  }
}


#if GC_FOREIGN
static void
sweep_foreign()
{ Lock l;

  for( l = pBase; l < pTop; l++ )
  { switch( l->type )
    { case L_WORD:
	{ Word sp = (Word) (l->value << 2);
	  
	  unmark(sp);
	  if ( isGlobalRef(get_value(sp)) )
	  { DEBUG(5, printf("Foreign value: "));
	    check_relocation(sp);
	    into_relocation_chain(sp);
	  }
	  break;
	}
      case L_POINTER:
	{ Word *sp = (Word *) (l->value << 2);
	
	  if ( marked(*sp) && isGlobalRef(get_value(*sp)) )
	  { DEBUG(5, printf("Foreign pointer: "));
	    check_relocation(*sp);
	    into_relocation_chain(*sp);
	  }

	  break;
	}
      case L_MARK:
	DEBUG(5, printf("Foreign mark: "));
	sweep_mark((mark *) (l->value << 2));
	break;
    }
  }
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sweeping the local and trail stack to insert necessary pointers  in  the
relocation chains.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
sweep_trail()
{ register TrailEntry te = tTop - 1;

  for( ; te >= tBase; te-- )
  { if ( onGlobal(te->address) )
    { needsRelocation((Word) &te->address);
      into_relocation_chain((Word) &te->address);
    }
  }
}


static LocalFrame
sweep_environments(fr)
LocalFrame fr;
{ Code PC = NULL;

  if ( fr == (LocalFrame) NULL )
    return (LocalFrame) NULL;

  for( ; ; )
  { int slots;
    Word sp;

    if ( false(fr, FR_MARKED) )
      return (LocalFrame) NULL;
    clear(fr, FR_MARKED);

    slots = (PC == NULL ? fr->procedure->functor->arity : slotsFrame(fr));
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
sweep_choicepoints(bfr)
LocalFrame bfr;
{ for( ; bfr != (LocalFrame)NULL; bfr = bfr->backtrackFrame )
  { sweep_environments(bfr);
    sweep_mark(&bfr->mark);
  }
}

static void
sweep_stacks(fr)
LocalFrame fr;
{ LocalFrame pfr;

  if ( (pfr = sweep_environments(fr)) != NULL )
    sweep_stacks(pfr);

  sweep_choicepoints(fr);
}


static void
sweep_local(fr)
LocalFrame fr;
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
is_downward_ref(p)
Word p;
{ word val = get_value(p);

  if ( isRef(val) )
  { DEBUG(5, if ( unRef(val) < p ) printf("REF: "));
    return unRef(val) < p;
  }
  if ( isVar(val) || isInteger(val) )
    fail;
  if ( isIndirect(val) )
  { DEBUG(5, if ( (Word)unMask(val) < p ) printf("INDIRECT: "));
    return (Word)unMask(val) < p;
  }

  DEBUG(5, if ( (Word)val < p && (Word)val >= gBase ) printf("TERM: "));
  if ( (Word)val < p && (Word)val >= gBase && !marked((Word)val) )
    sysError("Pointer to term should be marked (down)");

  return (Word)val < p && (Word)val >= gBase;
}

static bool
is_upward_ref(p)
Word p;
{ word val = get_value(p);

  if ( isRef(val) )
    return unRef(val) > p;
  if ( isVar(val) || isInteger(val) )
    fail;
  if ( isIndirect(val) )
    return (Word)unMask(val) > p;

  if ( (Word)val > p && (Word)val < gTop && !marked((Word)val) )
    sysError("Pointer to term should be marked (up) \n\
	     p = 0x%lx, val = 0x%lx, *val = 0x%lx, gTop = 0x%lx",
	     p, val, *((Word)val), gTop);

  return (Word)val > p && (Word)val < gTop;
}

static void
compact_global()
{ Word dest, current;
#if O_SECURE
  Word *v = mark_top;
#endif

  DEBUG(2, printf("Scanning global stack downwards\n"));

  dest = gBase + total_marked;			/* first FREE cell */
  for( current = gTop; current >= gBase; current-- )
  { long offset = (marked(current) || is_first(current) ? 0
						        : offset_cell(current));
    current -= offset;

    if ( marked(current) )
    {
#if O_SECURE
      if ( current != *--v )
        sysError("Marked cell at 0x%lx (*= 0x%lx); gTop = 0x%lx; should have been 0x%lx", current, *current, gTop, *v);
#endif
      dest -= offset + 1;
      DEBUG(3, printf("Marked cell at 0x%lx (size = %ld; dest = 0x%lx)\n",
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
    { printf("Expected marked cell at 0x%lx, (*= 0x%lx)\n", *v, **v);
    }
    sysError("v = 0x%lx; mark_base = 0x%lx", v, mark_base);
  }
#endif

  if ( dest != gBase )
    sysError("Mismatch in down phase: dest = 0x%lx, gBase = 0x%lx\n",
							dest, gBase);
  if ( relocation_cells != relocated_cells )
    sysError("After down phase: relocation_cells = %ld; relocated_cells = %ld",
					relocation_cells, relocated_cells);

  DEBUG(2, printf("Scanning global stack upwards\n"));
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
    sysError("Mismatch in up phase: dest = 0x%lx, gBase + total_marked = 0x%lx\n", dest, gBase + total_marked );

  gTop = dest;
}

static void
collect_phase(fr)
LocalFrame fr;
{
#if GC_FOREIGN
  DEBUG(2, printf("Sweeping foreign references\n"));
  sweep_foreign();
#endif
  DEBUG(2, printf("Sweeping trail stack\n"));
  sweep_trail();
  DEBUG(2, printf("Sweeping local stack\n"));
  sweep_local(fr);
  DEBUG(2, printf("Compacting global stack\n"));
  compact_global();

  if ( relocation_chains != 0 )
    sysError("relocation chains = %ld", relocation_chains);
  if ( relocated_cells != relocation_cells ||
       relocated_cells != needs_relocation ||
       relocation_refs != 0 || relocation_indirect != 0)
    sysError("relocation cells = %ld; relocated_cells = %ld, needs_relocation = %ld\n\trelocation_refs = %ld, relocation_indirect = %ld",
	relocation_cells, relocated_cells, needs_relocation,
	relocation_refs, relocation_indirect);
}


		/********************************
		*             MAIN              *
		*********************************/

static long gsmall = 200000L;
static long tsmall = 100000L;

word
pl_collect_parms(g, t)
Word g, t;
{ if ( !isInteger(*g) || !isInteger(*t) )
    return warning("$collect_parms/2: instantiation fault");

  gsmall = valNum(*g);
  tsmall = valNum(*t);

  succeed;
}


#if O_DYNAMIC_STACKS
void
considerGarbageCollect(s)
Stack s;
{ if ( s->gc )
  { if ( s->top - s->base > 2 * s->gced_size + s->small )
    { DEBUG(1, printf("%s overflow: Posted garbage collect request\n",
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
    { warning("Illegal cell in global stack (up) at 0x%lx (*= 0x%lx)", current, *current);
      if ( isAtom(*current) )
	warning("0x%lx is atom %s", current, stringAtom(*current));
      if ( isTerm(*current) )
	warning("0x%lx is term %s/%d", current,
				       stringAtom(functorTerm(*current)->name),
				       functorTerm(*current)->arity);
      if ( ++errors > 10 )
      { printf("...\n");
        break;
      }
    }
  }

  for( current = gTop - 1; current >= gBase; current-- )
  { cells --;
    current -= offset_cell(current);
    if ( marked(current) || is_first(current) )
    { warning("Illegal cell in global stack (down) at 0x%lx (*= 0x%lx)", current, *current);
      if ( ++errors > 10 )
      { printf("...\n");
        break;
      }
    }
  }

  if ( !errors && cells != 0 )
    sysError("Different count of cells upwards and downwards: %ld\n", cells);

  return errors == 0;
}
#endif


void
garbageCollect(fr)
LocalFrame fr;
{ long tgar, ggar;
  real t = CpuTime();

  if ( gc_status.blocked )
    return;
  gc_status.requested = FALSE;

  gc_status.active = TRUE;
  DEBUG(0, printf("Garbage collect ... "));
#if O_PROFILE
  PROCEDURE_garbage_collect0->definition->profile_calls++;
#endif
#if O_SECURE
  if ( !scan_global() )
    sysError("Stack not ok at gc entry");

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
  tgar = trailcells_deleted * sizeof(struct trail_entry);
  ggar = (gTop - gBase - total_marked) * sizeof(word);
  gc_status.trail_gained  += tgar;
  gc_status.global_gained += ggar;
  gc_status.collections++;

  DEBUG(2, printf("Compacting trail ... "));
  compact_trail();

  collect_phase(fr);
#if O_SECURE
  if ( !scan_global() )
    sysError("Stack not ok after gc; gTop = 0x%lx", gTop);
  free(mark_base);
#endif
  DEBUG(0, printf("(gained %ld+%ld; used: %ld+%ld; free: %ld+%ld bytes)\n",
		  ggar, tgar,
		  usedStack(global), usedStack(trail),
		  roomStack(global), roomStack(trail)));
  gc_status.time += CpuTime() - t;
  stacks.global.gced_size = usedStack(global);
  stacks.trail.gced_size  = usedStack(trail);

  pl_trim_stacks();
  gc_status.active = FALSE;
}

word
pl_garbage_collect(d)
Word d;
{ LocalFrame fr = environment_frame;
#if O_DEBUG
  int ol = status.debugLevel;

  if ( !isInteger(*d) )
    return warning("garbage_collect/1: instantiation fault");
  status.debugLevel = (int) valNum(*d);
#endif
  gc_status.blocked--;
  garbageCollect(fr);
  gc_status.blocked++;
#if O_DEBUG
  status.debugLevel = ol;
#endif
  succeed;
}

void
resetGC()
{ gc_status.requested = FALSE;
  gc_status.blocked = 0;
  gc_status.collections = gc_status.global_gained = gc_status.trail_gained = 0;
  gc_status.time = 0.0;
}

		/********************************
		*      LOCKING FOREIGN DATA     *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are various types of foreign data that  are  of  interest  to  the
garbage  collector.  First of all the internal code both supports `word'
and `Word'.  Both can refer to the stacks.  Their value  is  non-garbage
and  should  be  relocated similar to local stack variables.  Second are
marks (Mark() and Undo()). Marks kept by foreign code should be  treated
equal to marks kept in the choicepoints on the local stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
lockw(p)
Word p;
{ Lock l = pTop++;

  verifyStack(lock);
  l->type  = L_WORD;
  l->value = (unsigned) p >> 2;
}

void
lockp(ptr)
void *ptr;
{ Word *p = ptr;
  Lock l = pTop++;

  verifyStack(lock);
  l->type  = L_POINTER;
  l->value = (unsigned) p >> 2;
}
  
void
lockMark(m)
mark *m;
{ Lock l = pTop++;

  verifyStack(lock);
  l->type  = L_MARK;
  l->value = (unsigned) m >> 2;
}

void
unlockw(p)
Word p;
{ if ( ((--pTop)->value << 2) != (unsigned)p )
    sysError("Mismatch in lock()/unlock()\n");
}

void
unlockp(ptr)
void *ptr;
{ Word *p = ptr;

  if ( ((--pTop)->value << 2) != (unsigned)p )
    sysError("Mismatch in lock()/unlock()\n");
}

void
unlockMark(m)
mark *m;
{ if ( pTop == pBase )
    warning("Lock stack underflow");
  else if ( ((--pTop)->value << 2) != (unsigned)m )
    sysError("Mismatch in lock()/unlock()\n");
}


#if O_SHIFT_STACKS

/* Development work ... */


		 /*******************************
		 *	   STACK-SHIFTER	*
		 *******************************/

static int local_frames;		/* count for debugging */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the Prolog runtime stacks presuming they have shifted by the
the specified offset.

Memory management description.





- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update a mark (either held by foreign code or on the local stack).  Just
update both pointers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
update_pointer(p, offset)
void *p;
long offset;
{ char **ptr = ((char **)p);

  if ( *ptr )
    *ptr += offset;
}



static void
update_mark(m, gs, ts)
mark *m;
long gs, ts;
{ DEBUG(3, printf("Updating mark\n"));

  update_pointer(&m->trailtop, ts);
  update_pointer(&m->globaltop, gs);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update a variable.  The variable may either live on the local- or global
stack.  If it is a reference  it might  be  both to  a global- and local
address.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
update_variable(sp, ls, gs)
Word sp;
long ls, gs;
{ if ( isRef(*sp) )
  { if ( onGlobal(unRef(*sp)) )
      *sp = makeRef(addPointer(unRef(*sp), gs));
    else if ( onLocal(unRef(*sp)) )
      *sp = makeRef(addPointer(unRef(*sp), ls));
    else
      DEBUG(0, printf("Reference 0x%x neither to local- nor global stack\n",
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


#if O_SECURE
		 /*******************************
		 *	      CHECKING		*
		 *******************************/

static LocalFrame unmark_environments P((LocalFrame));
static void unmark_choicepoints P((LocalFrame));

static LocalFrame
check_environments(fr)
LocalFrame fr;
{ Code PC = NULL;

  if ( fr == NULL )
    return NULL;

  for(;;)
  { int slots;
    Word sp;
    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    local_frames++;
    clear_uninitialised(fr, PC);

    slots = (PC == NULL ? fr->procedure->functor->arity : slotsFrame(fr));
    sp = argFrameP(fr, 0);
    for( ; slots > 0; slots--, sp++ )
      checkData(sp, FALSE);

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else
      return (LocalFrame) varFrame(fr, -1);
  }
}


static void
check_choicepoints(bfr)
LocalFrame bfr;
{ for( ; bfr; bfr = bfr->backtrackFrame )
  { check_environments(bfr);
  }
}

checkStacks(frame)
LocalFrame frame;
{ LocalFrame fr, fr2;

  if ( !frame )
    frame = environment_frame;

  local_frames = 0;

  for( fr = frame; fr; fr = fr2 )
  { fr2 = check_environments(fr);

    check_choicepoints(fr->backtrackFrame);
  }

  for( fr = frame; fr; fr = fr2 )
  { fr2 = unmark_environments(fr);

    unmark_choicepoints(fr->backtrackFrame);
  }

  assert(local_frames == 0);
}



#endif /* O_SECURE */

		 /*******************************
		 *	   LOCAL STACK		*
		 *******************************/

static void update_choicepoints P((LocalFrame, long, long, long));

static LocalFrame
update_environments(fr, ls, gs, ts)
LocalFrame fr;
long ls, gs, ts;
{ Code PC = NULL;

  if ( fr == NULL )
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
	  printf("Shifting frame 0x%x [%ld] %s ... ",
		 fr, levelFrame(fr), procedureName(fr->procedure));
	  fflush(stdout));

    if ( ls )				/* update frame pointers */
    { if ( fr->parent )
	fr->parent = (LocalFrame) addPointer(fr->parent, ls);
      if ( fr->backtrackFrame )
	fr->backtrackFrame = (LocalFrame) addPointer(fr->backtrackFrame, ls);
    }

    update_mark(&fr->mark, gs, ts);	/* trail and global marks */

    if ( gs || ls )			/* update variables */
    { clear_uninitialised(fr, PC);

      slots = (PC == NULL ? fr->procedure->functor->arity : slotsFrame(fr));
      sp = argFrameP(fr, 0);
      DEBUG(2, printf("\n\t%d slots in 0x%x ... 0x%x ... ",
		      slots, sp, sp+slots));
      for( ; slots > 0; slots--, sp++ )
	update_variable(sp, ls, gs);
    }

    DEBUG(2, printf("ok\n"));

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
    { LocalFrame parent = (LocalFrame) varFrame(fr, -1);

      if ( parent )
      { parent = addPointer(parent, ls);
	varFrame(fr, -1) = (word) parent;
      }

      return parent;
    }
  }
}


static void
update_choicepoints(bfr, ls, gs, ts)
LocalFrame bfr;
long ls, gs, ts;
{ for( ; bfr; bfr = bfr->backtrackFrame )
  { DEBUG(1, printf("Updating choicepoints from 0x%x [%ld] %s ... ",
		    bfr, levelFrame(bfr), procedureName(bfr->procedure)));
    update_environments(bfr, ls, gs, ts);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clear the marks set by update_environments().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
unmark_environments(fr)
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
unmark_choicepoints(bfr)
LocalFrame bfr;
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
    if ( isRef(*p) && unRef(*p) == to )
      return p;

  return NULL;
}
#endif

static void
update_global(gs)
long gs;
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
update_trail(ts, ls, gs)
long ts, ls, gs;			/* trail-, local- and global offsets */
{ TrailEntry p = addPointer(tBase, ts);
  TrailEntry t = addPointer(tTop, ts);

  for( ; p < t; p++ )
  { if ( onGlobal(p->address) )
    { p->address = addPointer(p->address, gs);
    } else
    { assert(onLocal(p->address));
      p->address = addPointer(p->address, ls);
    }
  }
}

		/********************************
		*           LOCK STACK		*
		********************************/

static void
update_locks(ls, gs, ts)
long ls, gs, ts;
{ Lock l = pBase;
  int w=0, p=0, m=0;

  DEBUG(1, printf("Locked references ...", pTop - pBase); fflush(stdout));

  for(l = pBase; l < pTop; l++)
  { switch(l->type)
    { case L_WORD:
	w++;
        assert(0);
	break;
      case L_POINTER:
	p++;
        { char **address = (char **)(l->value << 2);

	  if	  ( onStackArea(local, *address) )
	    *address += ls;
	  else if ( onStackArea(global, *address) )
	    *address += gs;
	  else if ( onStackArea(trail, *address) )
	    *address += ts;
	  else 
	    assert(*address == NULL);
	}
	break;
      case L_MARK:
	m++;
	update_mark((mark *)(l->value << 2), gs, ts);
	break;
    }
  }

  DEBUG(1, printf("w+p+m = %d %d %d (ok)\n", w, p, m));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry-point.   Update the  stacks to  reflect  their current  positions.
This function should be called *after*  the  stacks have been relocated.
Note that these functions are  only used  if  there is no vitrual memory
way to reach at dynamic stacks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define updateStackHeader(name, offset) \
	{ stacks.name.base = addPointer(stacks.name.base, offset); \
	  stacks.name.top  = addPointer(stacks.name.top,  offset); \
	  stacks.name.max  = addPointer(stacks.name.max,  offset); \
	}


LocalFrame
updateStacks(frame, lb, gb, tb)
LocalFrame frame;
Void lb, gb, tb;			/* bases addresses */
{ long ls, gs, ts;
  LocalFrame fr, fr2;

  ls = (long) lb - (long) lBase;
  gs = (long) gb - (long) gBase;
  ts = (long) tb - (long) tBase;

  DEBUG(2, printf("ls+gs+ts = %ld %ld %ld ... ", ls, gs, ts); fflush(stdout));

  if ( ls || gs || ts )
  { local_frames = 0;

    for(fr = addPointer(frame, ls); fr; fr = fr2)
    { fr2 = update_environments(fr, ls, gs, ts);

      update_choicepoints(fr->backtrackFrame, ls, gs, ts);
      DEBUG(1, if ( fr2 ) printf("Update frames of C-parent\n"));
    }

    DEBUG(2, printf("%d frames ...", local_frames); fflush(stdout));

    for(fr = addPointer(frame, ls); fr; fr = fr2)
    { fr2 = unmark_environments(fr);

      unmark_choicepoints(fr->backtrackFrame);
    }
    assert(local_frames == 0);

    if ( gs )
      update_global(gs);
    if ( gs || ls )
      update_trail(ts, ls, gs);

    update_locks(ls, gs, ts);

    updateStackHeader(local,  ls);
    updateStackHeader(global, gs);
    updateStackHeader(trail,  ts);
  }

  return addPointer(frame, ls);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry point from interpret()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GL_SEPARATION sizeof(word)

#define nextStackSize(name) ROUND((sizeStack(name) * 3) / 2, 4096)

LocalFrame
growStacks(fr, l, g, t)
LocalFrame fr;
int l, g, t;
{ if ( l || g || t )
  { TrailEntry tb = tBase;
    Word gb = gBase;
    LocalFrame lb = lBase;
    long lsize = sizeStack(local);
    long gsize = sizeStack(global);
    long tsize = sizeStack(trail);

    SECURE(checkStacks(fr));

    DEBUG(0,
	  printf("growStacks(0x%x, %c%c%c) l+g+t = %ld %ld %ld ...",
		 fr,
		 l ? 'l' : '-',
		 g ? 'g' : '-',
		 t ? 't' : '-',
		 lsize, gsize, tsize);
	  fflush(stdout));

    if ( t )
    { tsize = nextStackSize(trail);
      tb = realloc(tb, tsize);
      statistics.trail_shifts++;
    }

    if ( g || l )
    { long loffset = gsize + GL_SEPARATION;
      assert(lb == addPointer(gb, loffset));	

      if ( g )
      { gsize = nextStackSize(global);
	statistics.global_shifts++;
      }
      if ( l )
      { lsize = nextStackSize(local);
	statistics.local_shifts++;
      }

      gb = realloc(gb, lsize + gsize + GL_SEPARATION);
      lb = addPointer(gb, gsize + GL_SEPARATION);
      if ( g )				/* global enlarged; move local */
	bcopy(addPointer(gb, loffset), lb, lsize);
    }
      
#define PrintStackParms(stack, name, newbase, newsize) \
	{ printf("%6s: 0x%07x ... 0x%07x --> 0x%07x ... 0x%07x\n", \
		 name, stacks.stack.base, stacks.stack.max, \
		 newbase, addPointer(newbase, newsize)); \
	}


    DEBUG(0, { putchar('\n');
	       PrintStackParms(local, "local", lb, lsize);
	       PrintStackParms(global, "global", gb, gsize);
	       PrintStackParms(trail, "trail", tb, tsize);
	     });
		    
    DEBUG(1, printf("Updating stacks ..."); fflush(stdout));
    fr = updateStacks(fr, lb, gb, tb);
    DEBUG(0, printf("ok\n"));

    stacks.local.max  = addPointer(stacks.local.base,  lsize);
    stacks.global.max = addPointer(stacks.global.base, gsize);
    stacks.trail.max  = addPointer(stacks.trail.base,  tsize);

    SetHTop(stacks.local.max);
    SetHTop(stacks.trail.max);
  }

  SECURE(checkStacks(fr));

  return fr;
}

#endif /*O_SHIFT_STACKS*/


