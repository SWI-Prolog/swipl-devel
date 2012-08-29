/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemake@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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

#ifdef SECURE_GC
#define O_DEBUG 1
#endif
#include "pl-incl.h"
#include "os/pl-cstack.h"
#include "pentium.h"
#include "pl-inline.h"

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

The virtual machine interpreter now should   ensure the stack frames are
in a predictable state. For the moment,   this  implies that all frames,
except for the current one (which only  has its arguments filled) should
be initialised fully. I'm not yet sure   whether we can't do better, but
this is simple and safe and  allows   us  to debug the garbage collector
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

If the CHK_SECURE prolog_debug flag  is set  some  additional  expensive
consistency checks that need considerable amounts of memory and cpu time
are added. Garbage collection gets about 3-4 times as slow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Marking, testing marks and extracting values from GC masked words.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GC_MASK		(MARK_MASK|FIRST_MASK)
#define VALUE_MASK	(~GC_MASK)

#if O_DEBUG
char tmp[256];				/* for calling print_val(), etc. */
#define check_relocation(p) do_check_relocation(p, __FILE__, __LINE__ PASS_LD)
#define relocated_cell(p) do_relocated_cell(p PASS_LD)
#define recordMark(p)   DEBUG(CHK_SECURE, \
			      { if ( (char*)(p) < (char*)lBase ) \
				{ assert(onStack(global, p)); \
				  *mark_top++ = (p); \
				} \
			      })
#else
#define recordMark(p)
#define needsRelocation(p) { needs_relocation++; }
#define relocated_cell(p)  { relocated_cells++; }
#define check_relocation(p)
#define markLocal(p) (local_marked++)
#define processLocal(p) (local_marked--)
#endif

#define ldomark(p)	{ *(p) |= MARK_MASK; }
#define domark(p)	{ if ( is_marked(p) ) \
			    sysError("marked twice: %p (*= 0x%lx), gTop = %p", p, *(p), gTop); \
			  DEBUG(3, char b[64]; Sdprintf("\tdomarked(%p = %s)\n", p, print_val(*p, b))); \
			  *(p) |= MARK_MASK; \
			  total_marked++; \
			  recordMark(p); \
			}
#define unmark(p)	(*(p) &= ~MARK_MASK)

#define mark_first(p)	(*(p) |= FIRST_MASK)
#define unmark_first(p)	(*(p) &= ~FIRST_MASK)
#define is_ref(w)	isRef(w)

#define get_value(p)	(*(p) & VALUE_MASK)
#define set_value(p, w)	{ *(p) &= GC_MASK; *(p) |= w; }
#define val_ptr2(w, s)	((Word)((uintptr_t)valPtr2((w), (s)) & ~(uintptr_t)0x3))
#define val_ptr(w)	val_ptr2((w), storage(w))

#define inShiftedArea(area, shift, ptr) \
	((char *)ptr >= (char *)LD->stacks.area.base + shift && \
	 (char *)ptr <  (char *)LD->stacks.area.max + shift )
#define topPointerOnStack(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.max)

#define onGlobal(p)	onGlobalArea(p) /* onStack()? */
#define onLocal(p)	onStackArea(local, p)
#define onTrail(p)	topPointerOnStack(trail, p)

#ifndef offset
#define offset(s, f) ((size_t)(&((struct s *)NULL)->f))
#endif

#define ttag(x)		(((word)(x))&TAG_TRAILMASK)


		 /*******************************
		 *	     TYPES		*
		 *******************************/

typedef struct vm_state
{ LocalFrame	frame;			/* Current frame */
  Choice	choice;			/* Last choicepoint */
  Code		pc;			/* Current PC */
  Code		pc_start_vmi;		/* PC at start of current VMI */
  Word		argp;			/* Argument pointer */
  Word		argp0;			/* Arg-pointer for nested term */
  int		adepth;			/* FUNCTOR/POP nesting depth */
  LocalFrame	lSave;			/* Saved local top */
  int		save_argp;		/* Need to safe ARGP? */
  int		in_body;		/* Current frame is executing a body */
  int		new_args;		/* #new arguments */
  int		uwrite_count;		/* #UWRITE marked ARGP cells */
} vm_state;


		 /*******************************
		 *     FUNCTION PROTOTYPES	*
		 *******************************/

forwards void		mark_variable(Word ARG_LD);
static void		mark_local_variable(Word p ARG_LD);
forwards void		sweep_foreign(void);
static void		sweep_global_mark(Word *m ARG_LD);
forwards void		update_relocation_chain(Word, Word ARG_LD);
forwards void		into_relocation_chain(Word, int stg ARG_LD);
forwards void		alien_into_relocation_chain(void *addr,
						    int orgst, int stg
						    ARG_LD);
forwards void		compact_trail(void);
forwards void		sweep_mark(mark * ARG_LD);
forwards void		sweep_trail(void);
forwards bool		is_downward_ref(Word ARG_LD);
forwards bool		is_upward_ref(Word ARG_LD);
forwards void		compact_global(void);
static Code		startOfVMI(QueryFrame qf);
static void		get_vmi_state(QueryFrame qf, vm_state *state);
static size_t		tight(Stack s ARG_LD);

#if O_DEBUG
forwards int		cmp_address(const void *, const void *);
forwards void		do_check_relocation(Word, char *file, int line ARG_LD);
forwards void		needsRelocation(void *);
forwards void		check_mark(mark *m);
static int		check_marked(const char *s);
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
#define start_map	   (LD->gc._start_map)
#if O_DEBUG
#define trailtops_marked   (LD->gc._trailtops_marked)
#define mark_base	   (LD->gc._mark_base)
#define mark_top	   (LD->gc._mark_top)
#define check_table	   (LD->gc._check_table)
#define local_table	   (LD->gc._local_table)
#define relocated_check	   (LD->gc._relocated_check)
#endif

#undef LD
#define LD LOCAL_LD

		/********************************
		*           DEBUGGING           *
		*********************************/

#if defined(O_DEBUG) || defined(O_MAINTENANCE)

static char *
print_adr(Word adr, char *buf)
{ GET_LD
  char *name;
  Word base;
  static char tmp[256];

  if ( !buf )
    buf = tmp;

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
  static const char *tag_name[] = { "var", "attvar", "float", "int", "atom",
				    "string", "term", "ref" };
  static const char *stg_name[] = { "static", "global", "local", "reserved" };
  static char tmp[256];
  char *o;

  if ( !buf )
    buf = tmp;
  o = buf;

  if ( val & (MARK_MASK|FIRST_MASK) )
  { *o++ = '[';
    if ( val & MARK_MASK )
      *o++ = 'M';
    if ( val & FIRST_MASK )
      *o++ = 'F';
    *o++ = ']';
    val &= ~(word)(MARK_MASK|FIRST_MASK);
  }

  if ( isVar(val) )
  { strcpy(o, "VAR");
  } else if ( isTaggedInt(val) )
  { Ssprintf(o, "int(%ld)", valInteger(val));
  } else if ( isAtom(val) )
  { const char *s = stringAtom(val);
    if ( strlen(s) > 10 )
    { strncpy(o, s, 10);
      strcat(o, "...");
    } else
    { strcpy(o, s);
    }
  } else if ( tagex(val) == (TAG_ATOM|STG_GLOBAL) )
  { FunctorDef fd = valueFunctor(val);

    Ssprintf(o, "functor %s/%d", stringAtom(fd->name), fd->arity);
  } else
  { size_t offset = (val>>(LMASK_BITS-2))/sizeof(word);

    if ( storage(val) == STG_GLOBAL )
      offset -= gBase - (Word)base_addresses[STG_GLOBAL];

    Ssprintf(o, "%s at %s(%ld)",
	     tag_name[tag(val)],
	     stg_name[storage(val) >> 3],
	     (long)offset);
  }

  return buf;
}

#endif /*O_DEBUG*/

#if O_DEBUG

#define RELOC_NEEDS   ((void*)1)
#define RELOC_CHAINED ((void*)2)
#define RELOC_UPDATED ((void*)3)

static void
needsRelocation(void *addr)
{ GET_LD

  needs_relocation++;

  DEBUG(CHK_SECURE, addHTable(check_table, addr, RELOC_NEEDS));
}


static void
do_check_relocation(Word addr, char *file, int line ARG_LD)
{ if ( DEBUGGING(CHK_SECURE) )
  { Symbol s;

    if ( !(s=lookupHTable(check_table, addr)) )
    { char buf1[256];
      char buf2[256];
      sysError("%s:%d: Address %s (%s) was not supposed to be relocated",
	       file, line, print_adr(addr, buf1), print_val(*addr, buf2));
      return;
    }

    if ( s->value != RELOC_NEEDS )
    { sysError("%s:%d: Relocated twice: 0x%lx", file, line, addr);
      return;
    }

    s->value = RELOC_CHAINED;
  }
}


static void
do_relocated_cell(Word addr ARG_LD)
{ if ( DEBUGGING(CHK_SECURE) )
  { if ( relocated_check )		/* we cannot do this during the */
    { Symbol s;				/* final up-phase because the addresses */
					/* have already changed */
      if ( !(s=lookupHTable(check_table, addr)) )
      { char buf1[64];

        sysError("Address %s was not supposed to be updated",
	         print_adr(addr, buf1));
        return;
      }

      if ( s->value == RELOC_UPDATED )
      { char buf1[64];

        sysError("%s: updated twice", print_adr(addr, buf1));
        return;
      }

      s->value = RELOC_UPDATED;
    }
  }

  relocated_cells++;
}


static void
printNotRelocated()
{ GET_LD
  TableEnum e = newTableEnum(check_table);
  Symbol s;

  Sdprintf("Not relocated cells:\n");

  while((s=advanceTableEnum(e)))
  { if ( s->value == RELOC_CHAINED )
    { Word p = s->name;
      char buf1[64];

      Sdprintf("\t%s\n", print_adr(p, buf1));
    }
  }

  freeTableEnum(e);
}


static void
markLocal(Word addr)
{ GET_LD

  local_marked++;

  DEBUG(CHK_SECURE,
	{ Symbol s;
	  if ( (s = lookupHTable(local_table, addr)) )
	    assert(0);
	  addHTable(local_table, addr, (void*)TRUE);
	});
}


static void
processLocal(Word addr)
{ GET_LD

  local_marked--;

  DEBUG(CHK_SECURE,
	{ Symbol s;

	  if ( (s = lookupHTable(local_table, addr)) )
	  { assert(s->value == (void*)TRUE);
	    s->value = (void*)FALSE;
	  } else
	  { assert(0);
	  }
	});
}

#endif /* O_DEBUG */

		/********************************
		*          UTILITIES            *
		*********************************/

QueryFrame
queryOfFrame(LocalFrame fr)
{ QueryFrame qf;

  assert(!fr->parent);

  qf = (QueryFrame)((char*)fr - offset(queryFrame, top_frame));
  assert(qf->magic = QID_MAGIC);

  return qf;
}


static inline int
isGlobalRef(word w)
{ return storage(w) == STG_GLOBAL;
}


static inline size_t
offset_cell(Word p)
{ word m = *p;				/* was get_value(p) */
  size_t offset;

  if ( unlikely(storage(m) == STG_LOCAL) )
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
makePtr(Word ptr, int tag ARG_LD)
{ int stg;

  if ( onGlobalArea(ptr) )
    stg = STG_GLOBAL;
  else if ( onStackArea(local, ptr) )
    stg = STG_LOCAL;
  else
  { assert(onTrailArea(ptr));
    stg = STG_TRAIL;
  }

  return consPtr(ptr, tag|stg);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clear the mask (FR_MARKED or FR_MARKED_PRED) flags left after traversing
all reachable frames.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
unmark_environments(PL_local_data_t *ld, LocalFrame fr, uintptr_t mask)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { if ( false(fr, mask) )
      return NULL;
    clear(fr, mask);
    ld->gc._local_frames--;

    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
      return queryOfFrame(fr);
  }
}


static void
unmark_choicepoints(PL_local_data_t *ld, Choice ch, uintptr_t mask)
{ for( ; ch; ch = ch->parent )
  { ld->gc._choice_count--;
    unmark_environments(ld, ch->frame, mask);
  }
}


static void
unmark_stacks(PL_local_data_t *ld, LocalFrame fr, Choice ch,
	      uintptr_t mask)
{ QueryFrame qf;

  while(fr)
  { qf = unmark_environments(ld, fr, mask);
    assert(qf->magic == QID_MAGIC);
    unmark_choicepoints(ld, ch, mask);
    if ( qf->parent )
    { QueryFrame pqf = qf->parent;

      if ( !(fr = pqf->registers.fr) )
	fr = qf->saved_environment;
      ch = qf->saved_bfr;
    } else
      break;
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
mark_variable(Word start ARG_LD)
{ Word current;				/* current cell examined */
  word val;				/* old value of current cell */
  Word next;				/* cell to be examined */

  DEBUG(MSG_GC_MARK_VAR,
	char b[64];
	Sdprintf("marking %p (=%s)\n", start, print_val(*start, b)));

  if ( is_marked(start) )
    sysError("Attempt to mark twice");

  if ( onStackArea(local, start) )
  { markLocal(start);
    total_marked--;			/* do not count local stack cell */
  }
  current = start;
  mark_first(current);
  val = get_value(current);
  FORWARD;

forward:				/* Go into the tree */
  if ( is_marked(current) )		/* have been here */
    BACKWARD;
  domark(current);

  switch(tag(val))
  { case TAG_REFERENCE:
    { next = unRef(val);		/* address pointing to */
      DEBUG(CHK_SECURE, assert(onStack(global, next)));
      needsRelocation(current);
      if ( is_first(next) )		/* ref to choice point. we will */
        BACKWARD;			/* get there some day anyway */
      val  = get_value(next);		/* invariant */
      set_value(next, makeRef(current));/* create backwards pointer */
      DEBUG(MSG_GC_MARK_VAR_WALK,
	    Sdprintf("Marking REF from %p to %p\n", current, next));
      current = next;			/* invariant */
      FORWARD;
    }
#ifdef O_ATTVAR
    case TAG_ATTVAR:
    { DEBUG(CHK_SECURE, assert(storage(val) == STG_GLOBAL));
      next = valPtr2(val, STG_GLOBAL);
      DEBUG(CHK_SECURE, assert(onStack(global, next)));
      needsRelocation(current);
      if ( is_marked(next) )
	BACKWARD;			/* term has already been marked */
      val  = get_value(next);		/* invariant */
					/* backwards pointer */
      set_value(next, makePtr(current, TAG_ATTVAR PASS_LD));
      DEBUG(MSG_GC_MARK_VAR_WALK,
	    Sdprintf("Marking ATTVAR from %p to %p\n", current, next));
      current = next;			/* invariant */
      FORWARD;
    }
#endif
    case TAG_COMPOUND:
    { int args;

      DEBUG(CHK_SECURE, assert(storage(val) == STG_GLOBAL));
      next = valPtr2(val, STG_GLOBAL);
      DEBUG(CHK_SECURE, assert(onStack(global, next)));
      needsRelocation(current);
      if ( is_marked(next) )
	BACKWARD;			/* term has already been marked */
      args = arityFunctor(((Functor)next)->definition) - 1;
      DEBUG(MSG_GC_MARK_VAR_WALK,
	    Sdprintf("Marking TERM %s/%d at %p\n",
		     stringAtom(nameFunctor(((Functor)next)->definition)),
		     args+1, next));
      domark(next);
      for( next += 2; args > 0; args--, next++ )
      { DEBUG(CHK_SECURE, assert(!is_first(next)));
	mark_first(next);
      }
      next--;				/* last cell of term */
      val = get_value(next);		/* invariant */
					/* backwards pointer (NO ref!) */
      set_value(next, makePtr(current, TAG_COMPOUND PASS_LD));
      current = next;
      FORWARD;
    }
    case TAG_INTEGER:
      if ( storage(val) == STG_INLINE )
	BACKWARD;
    case TAG_STRING:
    case TAG_FLOAT:			/* indirects */
    { next = valPtr2(val, STG_GLOBAL);

      DEBUG(CHK_SECURE, assert(storage(val) == STG_GLOBAL));
      DEBUG(CHK_SECURE, assert(onStack(global, next)));
      needsRelocation(current);
      if ( is_marked(next) )		/* can be referenced from multiple */
        BACKWARD;			/* places */
      domark(next);
      DEBUG(MSG_GC_MARK_VAR_WALK,
	    Sdprintf("Marked indirect data type, size = %ld\n",
		     offset_cell(next) + 1));
      total_marked += offset_cell(next);
    }
  }
  BACKWARD;

backward:				/* reversing backwards */
  while( !is_first(current) )
  { word w = get_value(current);
    int t = (int)tag(w);

    assert(onStack(global, current));

    next = valPtr(w);
    set_value(current, val);
    switch(t)
    { case TAG_REFERENCE:
	val = makeRef(current);
        break;
      case TAG_COMPOUND:
	val = makePtr(current-1, t PASS_LD);
        break;
      case TAG_ATTVAR:
	val = makePtr(current, t PASS_LD);
        break;
      default:
	assert(0);
    }
    current= next;
  }

  unmark_first(current);
  if ( current == start )
    return;

  DEBUG(CHK_SECURE, assert(onStack(global, current)));
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
#if O_DEBUG
  long gmarked = 0;
  long lmarked = 0;
#endif

  DEBUG(MSG_GC_MARK_TERMREF, Sdprintf("Marking term references ...\n"));

  for( ; fr; fr = fr->parent )
  { Word sp = refFliP(fr, 0);
    int n = fr->size;

    DEBUG(MSG_GC_MARK_TERMREF,
	  Sdprintf("Marking foreign frame %ld (size=%d)\n",
		   (Word)fr-(Word)lBase, n));

    assert(fr->magic == FLI_MAGIC);
    for( ; n-- > 0; sp++ )
    { if ( !is_marked(sp) )
      { if ( isGlobalRef(*sp) )
	{ DEBUG(MSG_GC_MARK_TERMREF, gmarked++);
	  mark_variable(sp PASS_LD);
	} else
	{ DEBUG(MSG_GC_MARK_TERMREF, lmarked++);
	  mark_local_variable(sp PASS_LD);
	}
      }
    }

    DEBUG(CHK_SECURE, check_marked("After marking foreign frame"));
  }

  DEBUG(MSG_GC_MARK_TERMREF,
	Sdprintf("Marked %ld global and %ld local term references\n",
		 gmarked, lmarked));
}


#ifdef O_GVAR

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Dealing  with  nb_setval/2  and   nb_getval/2  non-backtrackable  global
variables as defined  in  pl-gvar.c.  We   cannot  mark  and  sweep  the
hash-table itself as the  reversed   pointers  cannot  address arbitrary
addresses returned by allocHeapOrHalt(). Therefore we   turn all references to
the  global  stack  into  term-references  and  rely  on  the  available
mark-and-sweep for foreign references.

If none of the global  variable  refers   to  the  global stack we could
`unfreeze' the global stack, except  we   may  have used nb_setarg/3. We
could enhance on this by introducing  a   `melt-bar'  set  to the lowest
location which we assigned using nb_setarg/3.   If backtracking takes us
before  that  point  we  safely  know  there  are  no  terms  left  with
nb_setarg/3  assignments.  As  the  merged   backtrackable  global  vars
implementation also causes freezing of the  stacks it it uncertain there
is much to gain with this approach.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static fid_t
gvars_to_term_refs(Word **saved_bar_at)
{ GET_LD
  fid_t fid = PL_open_foreign_frame();

  if ( LD->gvar.nb_vars && LD->gvar.grefs > 0 )
  { TableEnum e = newTableEnum(LD->gvar.nb_vars);
    int found = 0;
    Symbol s;

    while( (s=advanceTableEnum(e)) )
    { word w = (word)s->value;

      if ( isGlobalRef(w) )
      { term_t t = PL_new_term_ref_noshift();

	assert(t);
	*valTermRef(t) = w;
	found++;
      }
    }

    freeTableEnum(e);
    assert(LD->gvar.grefs == found);

    DEBUG(MSG_GC_MARK_GVAR,
	  Sdprintf("Found %d global vars on global stack. "
		   "stored in frame %p\n", found, fli_context));
  }

  if ( LD->frozen_bar )
  { Word *sb;

    assert((Word)lTop + 1 <= (Word)lMax);
    sb = (Word*)lTop;
    lTop = (LocalFrame)(sb+1);
    *sb = LD->frozen_bar;
    *saved_bar_at = sb;
  } else
  { *saved_bar_at = NULL;
  }

  return fid;
}


static void
term_refs_to_gvars(fid_t fid, Word *saved_bar_at)
{ GET_LD

  if ( saved_bar_at )
  { assert((void *)(saved_bar_at+1) == (void*)lTop);
    LD->frozen_bar = valPtr2((word)*saved_bar_at, STG_GLOBAL);

    assert(onStack(global, LD->frozen_bar) || LD->frozen_bar == gTop);
    lTop = (LocalFrame) saved_bar_at;
  }

  if ( LD->gvar.grefs > 0 )
  { FliFrame fr = (FliFrame) valTermRef(fid);
    Word fp = (Word)(fr+1);
    TableEnum e = newTableEnum(LD->gvar.nb_vars);
    int found = 0;
    Symbol s;

    while( (s=advanceTableEnum(e)) )
    { Word p = (Word)&s->value;

      if ( isGlobalRef(*p) )
      { *p = *fp++;
	found++;
      }
    }
    assert(found == fr->size);

    freeTableEnum(e);
  }

  PL_close_foreign_frame(fid);
}

#else /*O_GVAR*/

#define gvars_to_term_refs() 0
#define term_refs_to_gvars(f) (void)0

#endif /*O_GVAR*/

#define UWRITE 0x1
#define LARGP  0x2

static fid_t
argument_stack_to_term_refs(vm_state *state)
{ if ( state->save_argp )
  { GET_LD
    fid_t fid = PL_open_foreign_frame();
    Word *ap;

    pushArgumentStack(LD->query->registers.argp);
    assert(LARGP != TAG_ATTVAR && LARGP != TAG_REFERENCE);

    state->uwrite_count = 0;
    for(ap=aBase; ap<aTop; ap++)
    { Word adr = *ap;

      if ( onGlobal(adr) )
      { term_t t = PL_new_term_ref_noshift();

	if ( (word)adr & UWRITE )
	{ adr = (Word)((word)adr & ~UWRITE);
	  *valTermRef(t) = consPtr(adr, STG_GLOBAL|TAG_ATTVAR);
	  state->uwrite_count++;
	} else
	{ *valTermRef(t) = consPtr(adr, STG_GLOBAL|TAG_REFERENCE);
	}
	DEBUG(CHK_SECURE, checkData(adr));
      } else
      { assert(adr >= (Word)lBase);
	*ap = (Word)((word)adr | LARGP);
      }
    }

    return fid;
  }

  return 0;
}


static void
term_refs_to_argument_stack(vm_state *state, fid_t fid)
{ if ( fid )
  { GET_LD
    FliFrame fr = (FliFrame) valTermRef(fid);
    Word fp = (Word)(fr+1);
    Word *ap;
    int uwc = 0;

    for(ap=aBase; ap<aTop; ap++)
    { Word adr = *ap;

      if ( (word)adr & LARGP )
      { *ap = (Word)((word)adr & ~LARGP);
      } else
      { word w = *fp++;
	word mask = (tag(w) == TAG_ATTVAR ? UWRITE : 0);

	if ( mask )
	  uwc++;
	*ap = (Word)((word)valPtr(w)|mask);
      }
    }
    assert(fp == (Word)(fr+1) + fr->size);
    assert(uwc == state->uwrite_count);

    DEBUG(CHK_SECURE,
	  { if ( onStackArea(local, LD->query->registers.argp) )
	      assert(LD->query->registers.argp == aTop[-1]);
	  });

    LD->query->registers.argp = *--aTop;
    PL_close_foreign_frame(fid);
  }
}


#ifdef O_CALL_RESIDUE
static void
mark_attvars()
{ GET_LD
  Word gp;

  for( gp = gBase; gp < gTop; gp += (offset_cell(gp)+1) )
  { if ( isAttVar(*gp) && !is_marked(gp) )
    { DEBUG(MSG_GC_MARK_ATTVAR, Sdprintf("mark_attvars(): marking %p\n", gp));
      mark_variable(gp PASS_LD);
    }
  }
}
#endif /*O_CALL_RESIDUE*/


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

    for( ; ; PC = stepPC(PC))
    { c = decode(*PC);

    again:
      switch( c )
      { case I_EXIT:			/* terminate code list */
	case I_EXITFACT:
	case I_EXITCATCH:
	case I_EXITQUERY:
	case I_FEXITDET:
	case I_FEXITNDET:
	case I_FREDO:
	case S_TRUSTME:
	case S_LIST:
	  return;

	case C_JMP:			/* jumps */
	  PC += (int)PC[1]+2;
	  c = decode(*PC);
	  goto again;

	case H_FIRSTVAR:		/* Firstvar assignments */
	case B_FIRSTVAR:
	case B_ARGFIRSTVAR:
	case A_FIRSTVAR_IS:
	case B_UNIFY_FIRSTVAR:
	case C_VAR:
	  DEBUG(CHK_SECURE,
		{ if ( varFrameP(fr, PC[1]) <
		       argFrameP(fr, fr->predicate->functor->arity) )
		    sysError("Reset instruction on argument");
		  /*assert(varFrame(fr, PC[1]) != QID_MAGIC); is possible */
		});
	  setVar(varFrame(fr, PC[1]));
	  break;
       case C_VAR_N:
       { size_t var = PC[1];
	 size_t count = PC[2];

	 while(count--)
	 { setVar(varFrame(fr, var));
	   var++;
	 }
	 break;
       }
       case H_LIST_FF:
       case B_UNIFY_FF:
          setVar(varFrame(fr, PC[1]));
          setVar(varFrame(fr, PC[2]));
          break;
       case B_UNIFY_FV:
       case B_UNIFY_FC:
       case A_ADD_FC:
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

  return fr->clause->value.clause->prolog_vars;
}


static inline void
check_call_residue(LocalFrame fr ARG_LD)
{
#ifdef O_CALL_RESIDUE
  if ( fr->predicate == PROCEDURE_call_residue_vars2->definition )
  { if ( !LD->gc.marked_attvars )
    { mark_attvars();
      LD->gc.marked_attvars = TRUE;
    }
  }
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If multiple TrailAssignment() calls happen on  the same address within a
choicepoint we only need to keep the  first. Therefore we scan the trail
for this choicepoint from the mark to the  top and mark (using the FIRST
mark) the (global stack) addresses trailed. If we find one marked we can
delete the trail entry. To  avoid  a   second  scan  we store the marked
addresses on the argument stack.

Note that this additional scan of a section   of the trail stack is only
required if there are  at  least   two  trailed  assignments  within the
trail-ranged described by the choicepoint.

As far as I can  see  the  only   first-marks  in  use  at this time are
references to the trail-stack and we use   the first marks on the global
stack.

Older versions used the argument stack. We   now use the segmented cycle
stack to avoid allocation issues on the argument stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_DESTRUCTIVE_ASSIGNMENT
static inline void
push_marked(Word p ARG_LD)
{ pushSegStack(&LD->cycle.vstack, p, Word);
}


static void
popall_marked(ARG1_LD)
{ Word p;

  while( popSegStack(&LD->cycle.vstack, &p, Word) )
  { unmark_first(p);
  }
}


static void
mergeTrailedAssignments(GCTrailEntry top, GCTrailEntry mark,
			int assignments ARG_LD)
{ GCTrailEntry te;
  LD->cycle.vstack.unit_size = sizeof(Word);

  DEBUG(MSG_GC_ASSIGNMENTS,
	Sdprintf("Scanning %d trailed assignments\n", assignments));

  for(te=mark; te <= top; te++)
  { Word p = val_ptr(te->address);

    if ( ttag(te[1].address) == TAG_TRAILVAL )
    { assignments--;
      if ( is_first(p) )
      {	DEBUG(MSG_GC_ASSIGNMENTS_MERGE,
	      Sdprintf("Delete duplicate trailed assignment at %p\n", p));
	te->address = 0;
	te[1].address = 0;
	trailcells_deleted += 2;
      } else
      { mark_first(p);
	push_marked(p PASS_LD);
      }
    } else
    { if ( is_first(p) )
      { te->address = 0;
	trailcells_deleted++;
      }
    }
  }

  popall_marked(PASS_LD1);
  assert(assignments == 0);
}
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark the choicepoints. This function walks   along the environments that
can be reached from  the  choice-points.   In  addition,  it deletes all
trail-references  that  will   be   overruled    by   the   choice-point
stack-reference anyway.

When using setarg/3 (O_DESTRUCTIVE_ASSIGNMENT),  destructive assignments
are stored on the trail-stack as  two   entries.  The first entry is the
normal trail-pointer, while the  second   is  flagged  with TAG_TRAILVAL
(0x1). When undoing, the tail is scanned backwards and if a tagged value
is encountered, this value is restored  at   the  location  of the first
trail-cell.

If the trail cell  has  become  garbage,   we  can  destroy  both cells,
otherwise we must mark the value.

Early reset of trailed  assignments  is   another  issue.  If  a trailed
location has not yet been  marked  it   can  only  be accessed by frames
*after* the undo to this choicepoint took   place.  Hence, we can do the
undo now and remove  the  cell   from  the  trailcell, saving trailstack
space. For a trailed assignment this means   we should restore the value
with the trailed value. Note however that  the trailed value has already
been marked. We however can remove  this   mark  as it will be re-marked
should it be accessible and otherwise it really is garbage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static GCTrailEntry
early_reset_vars(mark *m, Word top, GCTrailEntry te ARG_LD)
{ GCTrailEntry tm = (GCTrailEntry)m->trailtop;
  GCTrailEntry te0 = te;
  int assignments = 0;
  Word gKeep = (LD->frozen_bar > m->globaltop ? LD->frozen_bar : m->globaltop);

  for( ; te >= tm; te-- )		/* early reset of vars */
  {
#if O_DESTRUCTIVE_ASSIGNMENT
    if ( isTrailVal(te->address) )
    { Word tard = val_ptr(te[-1].address);

      if ( tard >= top || (tard >= gKeep && tard < gMax) )
      { te->address = 0;
	te--;
	te->address = 0;
	trailcells_deleted += 2;
      } else if ( is_marked(tard) )
      { Word gp = val_ptr(te->address);

	assert(onGlobal(gp));
	assert(!is_first(gp));
	if ( !is_marked(gp) )
	{ DEBUG(MSG_GC_ASSIGNMENTS_MARK,
		char b1[64]; char b2[64]; char b3[64];
		Sdprintf("Marking assignment at %s (%s --> %s)\n",
			 print_adr(tard, b1),
			 print_val(*gp, b2),
			 print_val(*tard, b3)));

	  mark_variable(gp PASS_LD);
	  assert(is_marked(gp));
	}

	assignments++;
	te--;
      } else
      { Word gp = val_ptr(te->address);

	DEBUG(MSG_GC_RESET,
	      char b1[64]; char b2[64]; char b3[64];
	      Sdprintf("Early reset of assignment at %s (%s --> %s)\n",
		       print_adr(tard, b1),
		       print_val(*tard, b2),
		       print_val(*gp, b3)));

	assert(onGlobal(gp));
	*tard = *gp;
	unmark(tard);

	te->address = 0;
	te--;
	te->address = 0;
	trailcells_deleted += 2;
      }
    } else
#endif
    { Word tard = val_ptr(te->address);

      if ( tard >= top )		/* above local stack */
      { DEBUG(CHK_SECURE, assert(ttag(te[1].address) != TAG_TRAILVAL));
	te->address = 0;
	trailcells_deleted++;
      } else if ( tard > gKeep && tard < gMax )
      { te->address = 0;
	trailcells_deleted++;
      } else if ( !is_marked(tard) )
      { DEBUG(MSG_GC_RESET,
	      char b1[64]; char b2[64];
	      Sdprintf("Early reset at %s (%s)\n",
		       print_adr(tard, b1), print_val(*tard, b2)));
	setVar(*tard);
	te->address = 0;
	trailcells_deleted++;
      }
    }
  }

#if O_DESTRUCTIVE_ASSIGNMENT
  if ( assignments >= 1 )
    mergeTrailedAssignments(te0, tm, assignments PASS_LD);
#endif

  return te;
}


static GCTrailEntry
mark_foreign_frame(FliFrame fr, GCTrailEntry te ARG_LD)
{ DEBUG(CHK_SECURE, assert(fr->magic == FLI_MAGIC));

  if ( isRealMark(fr->mark) )
  { te = early_reset_vars(&fr->mark, (Word)fr, te PASS_LD);

    DEBUG(MSG_GC_MARK_FOREIGN, Sdprintf("Marking foreign frame %p\n", fr));
    needsRelocation(&fr->mark.trailtop);
    check_relocation((Word)&fr->mark.trailtop);
    DEBUG(CHK_SECURE, assert(isRealMark(fr->mark)));
    alien_into_relocation_chain(&fr->mark.trailtop,
				STG_TRAIL, STG_LOCAL PASS_LD);

  }

  return te;
}


#define MARK_ALT_CLAUSES 1		/* also walk and mark alternate clauses */

		 /*******************************
		 *	     MARK STACKS	*
		 *******************************/

typedef struct mark_state
{ vm_state     *vm_state;		/* Virtual machine locations */
  FliFrame	flictx;			/* foreign context for early reset */
  GCTrailEntry	reset_entry;		/* Walk trail stack for early reset */
} mark_state;


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

#define NO_ADEPTH 1234567

static void	early_reset_choicepoint(mark_state *state, Choice ch ARG_LD);
static void	mark_alt_clauses(LocalFrame fr, ClauseRef cref ARG_LD);


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


static void
mark_new_arguments(vm_state *state ARG_LD)
{ Word sp = argFrameP(state->lSave, 0);
  int slots = state->new_args;

  for( ; slots-- > 0; sp++ )
  { DEBUG(CHK_SECURE, assert(*sp != FLI_MAGIC));
    if ( !is_marked(sp) )
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

static inline void
mark_frame_var(walk_state *state, code v ARG_LD)
{ Word sp = varFrameP(state->frame, v);

  if ( sp < state->envtop && !is_marked(sp) )
  { mark_local_variable(sp PASS_LD);
    state->unmarked--;
  }
}


static inline void
clear_frame_var(walk_state *state, code var, Code PC)
{ if ( (state->flags & GCM_CLEAR) )
  { LocalFrame fr = state->frame;
    DEBUG(MSG_GC_CLEAR,
	  Sdprintf("Clear var %d at %d\n",
		   var-VAROFFSET(0), (PC-state->c0)-1));
#ifdef O_DEBUG
    if ( DEBUGGING(CHK_SECURE) )
    { Word vp = varFrameP(fr, PC[0]);

      if ( !isVar(*vp & ~MARK_MASK) )
      { Sdprintf("ERROR: [%ld] %s: Wrong clear of var %d, PC=%d\n",
		 levelFrame(fr), predicateName(fr->predicate),
		 var-VAROFFSET(0),
		 (PC-state->c0)-1);
      }
    } else
    { setVar(varFrame(fr, var));
    }
#else
    setVar(varFrame(fr, var));
#endif
  }
}


static inline void
mark_argp(walk_state *state ARG_LD)
{
#ifdef MARK_ALT_CLAUSES
  if ( state->adepth == 0 )
  { if ( state->ARGP < state->envtop && !is_marked(state->ARGP) )
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
    DEBUG(MSG_GC_WALK,
	  Sdprintf("\t%s at %d\n", codeTable[op].name, PC-state->c0-1));
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
	mark_argp(state PASS_LD);
	/*FALLTHROUGH*/
      case B_STRING:			/* string + header */
      case A_MPZ:
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
      case S_TRUSTME:			/* Consider supervisor handling! */
      case S_LIST:
	return PC-1;
      case S_NEXTCLAUSE:
	mark_alt_clauses(state->frame, state->frame->clause->next PASS_LD);
        return PC-1;
      case I_FREDO:
	mark_arguments(state->frame PASS_LD);
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
	DEBUG(MSG_GC_WALK, Sdprintf("C_OR at %d\n", PC-state->c0-1));
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
	DEBUG(MSG_GC_WALK, Sdprintf("C_NOT at %d\n", PC-state->c0-1));
	PC += 2;			/* skip the two arguments */
	walk_and_mark(state, PC, C_CUT PASS_LD);
	DEBUG(MSG_GC_WALK, Sdprintf("C_NOT-ALT at %d\n", alt-state->c0));
	PC = alt;
	op = decode(*PC++);
        goto again;
      }
      case C_SOFTIF:
      case C_IFTHENELSE:
	if ( (state->flags & GCM_ALTCLAUSE) )
	  break;
      { Code alt = PC+PC[1]+2;
	DEBUG(MSG_GC_WALK, Sdprintf("C_IFTHENELSE at %d\n", PC-state->c0-1));
	PC += 2;			/* skip the 'MARK' variable and jmp */
	walk_and_mark(state, PC, C_JMP PASS_LD);
	PC = alt;
	op = decode(*PC++);
        goto again;
      }
      case C_IFTHEN:
      case C_SOFTIFTHEN:
	if ( (state->flags & GCM_ALTCLAUSE) )
	  break;
      { PC = walk_and_mark(state, PC+1, C_END PASS_LD);
	PC++;				/* skip C_END */
	op = decode(*PC++);
        goto again;
      }

					/* variable access */

      case B_UNIFY_VAR:			/* Var = Term */
	mark_frame_var(state, PC[0] PASS_LD);
        state->adepth = NO_ADEPTH;
	break;
      case B_UNIFY_FIRSTVAR:
	state->adepth = NO_ADEPTH;	/* never need to mark over ARGP */
        /*FALLTHROUGH*/
      case B_FIRSTVAR:
      case B_ARGFIRSTVAR:
      case A_FIRSTVAR_IS:
      case B_UNIFY_FC:
      case C_VAR:
	clear_frame_var(state, PC[0], PC);
	break;
      case C_VAR_N:
      { size_t var = PC[0];
	size_t count = PC[1];

	while(count--)
	  clear_frame_var(state, var++, PC);
	break;
      }
      case H_LIST_FF:
	mark_argp(state PASS_LD);
        /*FALLTHROUGH*/
      case B_UNIFY_FF:
	clear_frame_var(state, PC[0], PC);
	clear_frame_var(state, PC[1], PC);
	break;
      case A_ADD_FC:
      case B_UNIFY_FV:
	clear_frame_var(state, PC[0], PC);
	mark_frame_var(state, PC[1] PASS_LD);
	break;
      case B_UNIFY_VV:
      case B_EQ_VV:
      case B_NEQ_VV:
	mark_frame_var(state, PC[0] PASS_LD);
        mark_frame_var(state, PC[1] PASS_LD);
	break;
      case I_VAR:
      case I_NONVAR:
	mark_frame_var(state, PC[0] PASS_LD);
        break;

      { size_t index;			/* mark variable access */

	case B_UNIFY_VC:
	case B_EQ_VC:
	case B_NEQ_VC:
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
	case I_CUTCHP:
	  mark_frame_var(state, VAROFFSET(1) PASS_LD); /* choice-point */
	  break;
#ifdef O_CALL_AT_MODULE
	case I_CALLATM:
	case I_DEPARTATMV:
	  mark_frame_var(state, PC[1] PASS_LD);
	  break;
#endif
#ifdef MARK_ALT_CLAUSES
	case H_FIRSTVAR:
	  if ( (state->flags & GCM_CLEAR) )
	  { clear_frame_var(state, PC[0], PC);
	    break;
	  }
	  mark_argp(state PASS_LD);
	  break;
	case H_VAR:
	  mark_frame_var(state, PC[0] PASS_LD);
	  /*FALLTHROUGH*/
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
	  /*FALLTHROUGH*/
	case B_FUNCTOR:
	case B_LIST:
	  state->adepth++;
	  break;
	case H_VOID:
	  if ( state->adepth == 0 )
	    state->ARGP++;
	  break;
	case H_VOID_N:
	  if ( state->adepth == 0 )
	    state->ARGP += PC[0];
	  break;
	case H_POP:
	case B_POP:
	  state->adepth--;
	  break;
	case B_UNIFY_EXIT:
	  assert(state->adepth == 0 || state->adepth == NO_ADEPTH);
	  break;
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

  DEBUG(MSG_GC_WALK,
	Sdprintf("Scanning clauses for %s\n", predicateName(fr->predicate)));
  for(; cref && state.unmarked > 0; cref=cref->next)
  { if ( visibleClause(cref->value.clause, fr->generation) )
    { COUNT(c_scanned);
      state.c0 = cref->value.clause->codes;
      DEBUG(MSG_GC_WALK, Sdprintf("Scanning clause %p\n", cref->value.clause));
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

    state->reset_entry = mark_foreign_frame(fli, state->reset_entry PASS_LD);
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
  DEBUG(CHK_SECURE, trailtops_marked--);
}


static QueryFrame mark_environments(mark_state *state,
				    LocalFrame fr, Code PC ARG_LD);

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

	mark_alt_clauses(fr, ch->value.clause.cref PASS_LD);
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
environment. This came from Roberto Bagnara   and was simplified to this
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

(**)  If  we  are  in  head-unification  mode,  (saved)ARGP  are  nicely
relocated. However, we must also ensure that the term in which it points
is not GC-ed. This applies  for   head-arguments  as well as B_UNIFY_VAR
instructions. See get_vmi_state().

(***) When debugging, we must  avoid   GC-ing  local variables of frames
that  are  watched  by  the  debugger.    FR_WATCHED  is  also  used  by
setup_call_cleanup/3. We avoid full marking here. Maybe we should use an
alternate flag for these two cases?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
mark_environments(mark_state *mstate, LocalFrame fr, Code PC ARG_LD)
{ QueryFrame qf = NULL;

  while ( fr )
  { walk_state state;

    if ( false(fr, FR_MARKED) )
    { set(fr, FR_MARKED);
      state.flags = GCM_CLEAR;

      COUNT(marked_envs);
      check_call_residue(fr PASS_LD);
    } else
    { state.flags = 0;
    }

    if ( true(fr->predicate, FOREIGN) || PC == NULL || !fr->clause )
    { DEBUG(MSG_GC_MARK_ARGS,
	    Sdprintf("Marking arguments for [%d] %s\n",
		     levelFrame(fr), predicateName(fr->predicate)));
      mark_arguments(fr PASS_LD);
    } else
    { Word argp0;
      state.frame    = fr;
      state.unmarked = slotsInFrame(fr, PC);
      state.envtop   = argFrameP(fr, state.unmarked);
      state.c0       = fr->clause->value.clause->codes;

      if ( fr == mstate->vm_state->frame &&
	   PC == mstate->vm_state->pc_start_vmi )
      { argp0        = mstate->vm_state->argp0;
	state.ARGP   = mstate->vm_state->argp;
	state.adepth = mstate->vm_state->adepth;
	mark_new_arguments(mstate->vm_state PASS_LD);
      } else
      { argp0 = NULL;
      }

      DEBUG(MSG_GC_WALK,
	    Sdprintf("Walking code for [%d] %s from PC=%d\n",
		     levelFrame(fr), predicateName(fr->predicate),
		     PC-state.c0));

      walk_and_mark(&state, PC, I_EXIT PASS_LD);

      if ( argp0 && !is_marked(argp0) )		/* see (**) */
      { assert(onStackArea(local, argp0));
	mark_local_variable(argp0 PASS_LD);
      }

      if ( true(fr, FR_WATCHED) &&		/* (***) */
	   fr->predicate != PROCEDURE_setup_call_catcher_cleanup4->definition )
      { int slots;
	Word sp;

	slots  = slotsInFrame(fr, PC);
	sp = argFrameP(fr, 0);
	for( ; slots-- > 0; sp++ )
	{ if ( !is_marked(sp) )
	    mark_local_variable(sp PASS_LD);
	}
      }
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
mark_stacks(vm_state *vmstate)
{ GET_LD
  QueryFrame qf=NULL;
  mark_state state;
  LocalFrame fr = vmstate->frame;
  Choice ch     = vmstate->choice;
  Code PC       = vmstate->pc_start_vmi;

  memset(&state, 0, sizeof(state));
  state.vm_state     = vmstate;
  state.reset_entry  = (GCTrailEntry)tTop - 1;
  state.flictx       = fli_context;
  trailcells_deleted = 0;

  while(fr)
  { vm_state sub_state;

    DEBUG(MSG_GC_MARK_QUERY, Sdprintf("Marking query %p\n", qf));
    qf = mark_query_stacks(&state, fr, ch, PC PASS_LD);

    if ( qf->parent )			/* same code in checkStacks() */
    { QueryFrame pqf = qf->parent;

      if ( (fr = pqf->registers.fr) )
      { get_vmi_state(pqf, &sub_state);
	sub_state.choice = qf->saved_bfr;

	state.vm_state = &sub_state;
	PC = sub_state.pc_start_vmi;
      } else
      { fr = qf->saved_environment;
	PC = NULL;
      }
      ch = qf->saved_bfr;
    } else
      break;
  }

  for( ; state.flictx; state.flictx = state.flictx->parent)
    state.reset_entry = mark_foreign_frame(state.flictx,
					   state.reset_entry PASS_LD);

  DEBUG(MSG_GC_STATS,
	Sdprintf("Trail stack garbage: %ld cells\n", trailcells_deleted));
}


#if O_DEBUG
static int
cmp_address(const void *vp1, const void *vp2)
{ Word p1 = *((Word *)vp1);
  Word p2 = *((Word *)vp2);

  return p1 > p2 ? 1 : p1 == p2 ? 0 : -1;
}
#endif


static void
mark_phase(vm_state *state)
{ GET_LD
  total_marked = 0;

  DEBUG(CHK_SECURE, check_marked("Before mark_term_refs()"));
  mark_term_refs();
  mark_stacks(state);

  DEBUG(CHK_SECURE,
	{ if ( !scan_global(TRUE) )
	    sysError("Global stack corrupted after GC mark-phase");
	  qsort(mark_base, mark_top - mark_base, sizeof(Word), cmp_address);
	});

  DEBUG(MSG_GC_STATS,
	{ intptr_t size = gTop - gBase;
	  Sdprintf("%ld referenced cell; %ld garbage (gTop = %p)\n",
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
update_relocation_chain(Word current, Word dest ARG_LD)
{ Word head = current;
  word val = get_value(current);

  DEBUG(MSG_GC_RELOC,
	Sdprintf("unwinding relocation chain at %p to %p\n",
		 current, dest));

  do
  { int tag;

    unmark_first(current);
    current = valPtr(val);
    tag = (int)tag(val);
    val = get_value(current);
    DEBUG(MSG_GC_RELOC,
	  { FliFrame f;

	    f = addPointer(current, - offset(fliFrame, mark.trailtop));
	    if ( onStack(local, f) && f->magic == FLI_MAGIC )
	      Sdprintf("Updating trail-mark of foreign frame at %p\n", f);
	  });
    set_value(current, makePtr(dest, tag PASS_LD));
    relocated_cell(current);
  } while( is_first(current) );

  set_value(head, val);
  relocation_chains--;
}


static void
into_relocation_chain(Word current, int stg ARG_LD)
{ Word head;
  word val = get_value(current);

  head = valPtr(val);			/* FIRST/MASK already gone */
  set_value(current, get_value(head));
  set_value(head, consPtr(current, stg|tag(val)));

  DEBUG(MSG_GC_RELOC,
	Sdprintf("Into relocation chain: %p (head = %p)\n",
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
alien_into_relocation_chain(void *addr, int orgst, int stg ARG_LD)
{ void **ptr = (void **)addr;

  *ptr = (void *)consPtr(*ptr, orgst);
  into_relocation_chain(addr, stg PASS_LD);

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
      update_relocation_chain(&current->address, &dest->address PASS_LD);
#if O_DEBUG
    else if ( DEBUGGING(CHK_SECURE) )
    { Symbol s;
      if ( (s=lookupHTable(check_table, current)) != NULL &&
	   s->value == (void *)TRUE )
        sysError("%p was supposed to be relocated (*= %p)",
		 current, current->address);
    }
#endif

    if ( current->address )
      *dest++ = *current++;
    else
      current++;
  }
  if ( is_first(&current->address) )
    update_relocation_chain(&current->address, &dest->address PASS_LD);

  tTop = (TrailEntry)dest;

  if ( relocated_cells != relocation_cells )
    sysError("After trail: relocation cells = %ld; relocated_cells = %ld\n",
	     relocation_cells, relocated_cells);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{tag,untag}_trail() are used to turn  the   native  pointers used on the
trail-stack into tagged ones as  used  on   the  other  stacks,  to make
pointer reversal in the relocation chains uniform.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
tag_trail()
{ GET_LD
  TrailEntry te;

  for( te = tTop; --te >= tBase; )
  { Word p = te->address;
    int stg;

    if ( isTrailVal(p) )
    { Word p2 = trailValP(p);

      DEBUG(CHK_SECURE, assert(onStack(global, p2)));
      te->address = (Word)consPtr(p2, STG_GLOBAL|TAG_TRAILVAL);
      //DEBUG(SECURE_CHK, assert(te == tBase || !isTrailVal(te[-1].address)));
    } else
    { if ( onLocal(te->address) )
      { stg = STG_LOCAL;
      } else
      { DEBUG(CHK_SECURE, assert(onGlobalArea(te->address)));
	stg = STG_GLOBAL;
      }

      te->address = (Word)consPtr(te->address, stg);
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
Make a hole. This is used by functions   doing a scan on the global data
after marking. By creating a large cell   (disguised  as a string) other
functions doing a scan can skip large portions.

bottom points to the bottom of the  garbage   and  top to the top *cell*
that is garbage.  I.e., the total size of the cell is (top+1)-bottom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_STRLEN wsizeofInd(~(word)0)

static Word
make_gc_hole(Word bottom, Word top)
{ if ( top - bottom > 4 )
  { size_t wsize = top - bottom - 1;
    Word bt = bottom;
    word hdr;

    while(wsize > MAX_STRLEN)
    { Word t1  = bt+MAX_STRLEN+1;

      hdr = mkIndHdr(MAX_STRLEN, TAG_STRING);
      *t1 = *bt = hdr;
      DEBUG(MSG_GC_HOLE,
	    Sdprintf("Created Garbage hole %p..%p\n", bt, t1+1));
      bt = t1+1;
      wsize = top - bt - 1;
    }

    hdr = mkIndHdr(wsize, TAG_STRING); /* limited by size of string? */
    *top = *bt = hdr;

    DEBUG(MSG_GC_HOLE,
	  Sdprintf("Created Garbage hole %p..%p, size %ld\n",
		   bt, top+1, (long)wsize));
  }

  return bottom;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sweep a mark. *m is a top-of-global   pointer,  i.e. it points the first
free place in the global stack. Simply   updating is not good enough, as
this part may be garbage. Hence, we  have   to  scan  until we find real
data.

Note that initPrologStacks writes a dummy   marked cell below the global
stack, so this routine needs not to check   for the bottom of the global
stack. This almost doubles the performance of this critical routine.

(*) This function does a check for  the first non-garbage cell, which is
a linear scan. If the are many   marks (choice-points and foreign marks)
and a lot  of  garbage,  this   becomes  very  costly.  Therefore, after
skipping a region the region  is  filled   with  variables  that cary as
offset the location of the  target   non-garbage  location.  If scanning
finds one of these cells, we simply fetch the value and go to `done'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define consVar(w) (((intptr_t)(w)<<LMASK_BITS) | TAG_VAR)
#define valVar(w)  ((intptr_t)(w) >> LMASK_BITS)

static void
sweep_global_mark(Word *m ARG_LD)
{ Word gm;

  DEBUG(CHK_SECURE, assert(onStack(local, m)));
  gm = *m;
  if ( is_marked_or_first(gm-1) )
    goto done;				/* quit common easy case */

  for(;;)
  { Word prev = gm-1;

    while( !(*prev & (MARK_MASK|FIRST_MASK|STG_LOCAL)) )
    { if ( tag(*prev) == TAG_VAR && *prev != 0 )
      { gm = gBase + valVar(*prev);
	goto done;			/* (*) */
      }
      prev--;
    }
    gm = prev+1;

    if ( is_marked_or_first(prev) )
    {
    found:
      { size_t off = gm-gBase;
	word w = consVar(off);
	Word p;

	for(p = gm+1; p<(*m); p++)
	  *p = w;			/* (*) */
      }

    done:
      *m = gm;
      DEBUG(MSG_GC_RELOC, Sdprintf("gTop mark from choice point: "));
      needsRelocation(m);
      check_relocation((Word)m);
      alien_into_relocation_chain(m, STG_GLOBAL, STG_LOCAL PASS_LD);

      return;
    } else				/* a large cell */
    { size_t offset;

      DEBUG(CHK_SECURE, assert(storage(*prev) == STG_LOCAL));
      offset = wsizeofInd(*prev)+1;	/* = offset for a large cell */
      prev -= offset;
      if ( is_marked_or_first(prev) )
	goto found;
    }
    gm = prev;
  }
}


static inline void
sweep_mark(mark *m ARG_LD)
{ marks_swept++;
  sweep_global_mark(&m->globaltop PASS_LD);
  if ( m->saved_bar > gTop )
    m->saved_bar = gTop;
  sweep_global_mark(&m->saved_bar PASS_LD);
}


static void
sweep_foreign()
{ GET_LD
  FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
  { Word sp = refFliP(fr, 0);
    int n = fr->size;

    if ( isRealMark(fr->mark) )
      sweep_mark(&fr->mark PASS_LD);
    for( ; n-- > 0; sp++ )
    { if ( is_marked(sp) )
      {	unmark(sp);
	if ( isGlobalRef(get_value(sp)) )
	{ processLocal(sp);
	  check_relocation(sp);
	  into_relocation_chain(sp, STG_LOCAL PASS_LD);
	}
      }
    }
  }
}


static void
unsweep_mark(mark *m ARG_LD)
{ m->trailtop  = (TrailEntry)valPtr2((word)m->trailtop,  STG_TRAIL);
  m->globaltop = valPtr2((word)m->globaltop, STG_GLOBAL);
  m->saved_bar = valPtr2((word)m->saved_bar, STG_GLOBAL);

  DEBUG(CHK_SECURE, check_mark(m));

  marks_unswept++;
}


static void
unsweep_foreign(ARG1_LD)
{ FliFrame fr = fli_context;

  for( ; fr; fr = fr->parent )
  { if ( isRealMark(fr->mark) )
      unsweep_mark(&fr->mark PASS_LD);
  }
}


static void
unsweep_choicepoints(Choice ch ARG_LD)
{ for( ; ch ; ch = ch->parent)
    unsweep_mark(&ch->mark PASS_LD);
}


static QueryFrame
unsweep_environments(LocalFrame fr)
{ while(fr->parent)
    fr = fr->parent;

  return queryOfFrame(fr);
}


static void
unsweep_stacks(vm_state *state ARG_LD)
{ QueryFrame query;
  LocalFrame fr = state->frame;
  Choice ch = state->choice;

  for( ; fr; fr = query->saved_environment, ch = query->saved_bfr )
  { query = unsweep_environments(fr);
    unsweep_choicepoints(ch PASS_LD);
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
	check_relocation(&te->address);
	into_relocation_chain(&te->address, STG_TRAIL PASS_LD);
      } else
#endif
      if ( storage(te->address) == STG_GLOBAL )
      { needsRelocation(&te->address);
	check_relocation(&te->address);
	into_relocation_chain(&te->address, STG_TRAIL PASS_LD);
      }
    }
  }
}



static void
sweep_frame(LocalFrame fr, int slots ARG_LD)
{ Word sp;

  sp = argFrameP(fr, 0);
  for( ; slots > 0; slots--, sp++ )
  { if ( is_marked(sp) )
    { unmark(sp);
      if ( isGlobalRef(get_value(sp)) )
      { processLocal(sp);
	check_relocation(sp);
	into_relocation_chain(sp, STG_LOCAL PASS_LD);
      }
    } else
    { if ( isGlobalRef(*sp) )
      { DEBUG(MSG_GC_SWEEP, char b[64];
	      Sdprintf("[%ld] %s: GC VAR(%d) (=%s)\n",
		       levelFrame(fr), predicateName(fr->predicate),
		       sp-argFrameP(fr, 0),
		       print_val(*sp, b)));
	*sp = ATOM_garbage_collected;
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

    if ( false(fr, FR_MARKED) )
      return NULL;
    clear(fr, FR_MARKED);

    slots = slotsInFrame(fr, PC);

    DEBUG(MSG_GC_SWEEP,
	  Sdprintf("Sweep %d arguments for [%d] %s\n",
		   slots, levelFrame(fr), predicateName(fr->predicate)));

    sweep_frame(fr, slots PASS_LD);

    if ( fr->parent )
    { PC = fr->programPointer;
      fr = fr->parent;
    } else
    { QueryFrame qf = queryOfFrame(fr);

      return qf;
    }
  }
}


static void
sweep_choicepoints(Choice ch ARG_LD)
{ for( ; ch ; ch = ch->parent)
  { sweep_environments(ch->frame,
		       ch->type == CHP_JUMP ? ch->value.PC : NULL);
    sweep_mark(&ch->mark PASS_LD);
  }
}


static void
sweep_new_arguments(vm_state *state ARG_LD)
{ if ( state->new_args )
  { Word sp = argFrameP(state->lSave, 0);
    int slots = state->new_args;

    for( ; slots-- > 0; sp++ )
    { assert(is_marked(sp));
      unmark(sp);
      if ( isGlobalRef(get_value(sp)) )
      { processLocal(sp);
	check_relocation(sp);
	into_relocation_chain(sp, STG_LOCAL PASS_LD);
      }
    }
  }
}


static void
sweep_stacks(vm_state *state)
{ GET_LD
  LocalFrame fr = state->frame;
  Choice ch = state->choice;
  Code PC = state->pc_start_vmi;

  sweep_new_arguments(state PASS_LD);

  while( fr )
  { QueryFrame qf = sweep_environments(fr, PC);
    assert(qf->magic == QID_MAGIC);

    sweep_choicepoints(ch PASS_LD);
    if ( qf->parent )
    { QueryFrame pqf = qf->parent;

      if ( (fr = pqf->registers.fr) )
      { PC = startOfVMI(pqf);
      } else
      { fr = qf->saved_environment;
	PC = NULL;
      }
      ch = qf->saved_bfr;
    } else
      break;
  }

  if ( local_marked != 0 )
  {
#ifdef O_DEBUG
    if ( DEBUGGING(CHK_SECURE) )
    { TableEnum e = newTableEnum(local_table);
      Symbol s;

      Sdprintf("FATAL: unprocessed local variables:\n");

      while((s=advanceTableEnum(e)))
      { if ( s->value )
	{ Word p = s->name;
	  char buf1[64];
	  char buf2[64];

	  Sdprintf("\t%s (*= %s)\n", print_adr(p, buf1), print_val(*p, buf2));
	}
      }

      freeTableEnum(e);
    }
#endif
    sysError("local_marked = %ld", local_marked);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
All preparations have been made now, and the actual  compacting  of  the
global  stack  may  start.   The  marking phase has calculated the total
number of words (cells) in the global stack that are non-garbage.

In the first phase, we will  walk  along  the  global  stack  from  it's
current  top towards the bottom.  During this phase, `current' refers to
the current element we are processing, while `dest' refers to the  place
this  element  will  be  after  the compacting phase is completed.  This
invariant is central and should be maintained carefully while processing
alien objects as strings and reals, which happen to have a  non-standard
size.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
is_downward_ref(Word p ARG_LD)
{ word val = get_value(p);

  switch(tag(val))
  { case TAG_INTEGER:
      if ( storage(val) == STG_INLINE )
	fail;
    case TAG_ATTVAR:
    case TAG_STRING:
    case TAG_FLOAT:
    case TAG_REFERENCE:
    case TAG_COMPOUND:
    { Word d = val_ptr(val);

      DEBUG(CHK_SECURE, assert(d >= gBase));

      return d < p;
    }
  }

  fail;
}


static bool
is_upward_ref(Word p ARG_LD)
{ word val = get_value(p);

  switch(tag(val))
  { case TAG_INTEGER:
      if ( storage(val) == STG_INLINE )
	fail;
    case TAG_ATTVAR:
    case TAG_STRING:
    case TAG_FLOAT:
    case TAG_REFERENCE:
    case TAG_COMPOUND:
    { Word d = val_ptr(val);

      DEBUG(CHK_SECURE, assert(d < gTop));

      return d > p;
    }
  }

  fail;
}


#if O_DEBUG

static int
check_marked(const char *s)
{ GET_LD
  intptr_t m = 0;
  Word current;
  intptr_t cells = 0;

  for( current = gBase; current < gTop; current += (offset_cell(current)+1) )
  { cells++;
    if ( is_marked(current) )
    { m += (offset_cell(current)+1);
    }
  }

  if ( m == total_marked )
    return TRUE;

  if ( m != total_marked )
    Sdprintf("**** ERROR: size: %ld != %ld (%s) ****\n",
	     m, total_marked, s);

  return FALSE;
}

#endif /* O_DEBUG */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
current points to the bottom of the  first garbage cell. Skip downwards,
returning a pointer to the bottom of the   garbage  or the bottom of the
global stack. If the found garbage  hole   is  big enough, create a cell
that represents a large garbage string,  so   the  up-phase  can skip it
quickly.

Note that below the bottom of the stack   there  is a dummy marked cell.
See also sweep_global_mark().

It looks tempting to use the  down-references   in  GC-ed  areas left by
sweep_global_mark(), but this does not work   because these cells can be
inserted into new relocation chains while  sweeping the remainder of the
data-areas :-( I tried, but this caused  a crash in Back52. After adding
a check in into_relocation_chain() I discovered   that the above was the
reason for the failure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Word
downskip_combine_garbage(Word current, Word dest ARG_LD)
{ Word top_gc = current + offset_cell(current);

  for(current-- ; ; current-- )
  { if ( (*current & (MARK_MASK|FIRST_MASK|STG_LOCAL)) )
    { if ( is_marked(current) )
      { DEBUG(MSG_GC_HOLE, Sdprintf("Normal-non-GC cell at %p\n", current));
	return make_gc_hole(current+1, top_gc);
      } else if ( is_first(current) )
      { update_relocation_chain(current, dest PASS_LD);
      } else					/* large cell */
      { size_t offset;

	DEBUG(CHK_SECURE, assert(storage(*current) == STG_LOCAL));
	offset = wsizeofInd(*current)+1;	/* = offset for a large cell */
	current -= offset;			/* start large cell */
	if ( is_marked(current) )
	{ DEBUG(MSG_GC_HOLE,
		Sdprintf("Large-non-GC cell at %p, size %d\n",
			 current, offset+1));
	  return make_gc_hole(current+offset+1, top_gc);
	} else if ( is_first(current) )
	{ update_relocation_chain(current, dest PASS_LD);
	}
      }
    }
  }

  return make_gc_hole(gBase, top_gc);
}


static void
compact_global(void)
{ GET_LD
  Word dest, current;
  Word base = gBase, top;
#if O_DEBUG
  Word *v = mark_top;
#endif

  DEBUG(MSG_GC_PROGRESS, Sdprintf("Scanning global stack downwards\n"));

  dest = base + total_marked;			/* first FREE cell */
  for( current = gTop; current >= base; current-- )
  { if ( is_marked(current) )
    { marked_large_cell:
      DEBUG(CHK_SECURE,
	    { if ( current != *--v )
		sysError("Marked cell at %p (*= %p); gTop = %p; should be %p",
			 current, *current, gTop, *v);
	    });
      dest--;
      DEBUG(MSG_GC_RELOC,
	    Sdprintf("Marked cell at %p (dest = %p)\n", current, dest));
      if ( is_first(current) )
	update_relocation_chain(current, dest PASS_LD);
      if ( is_downward_ref(current PASS_LD) )
      { check_relocation(current);
	into_relocation_chain(current, STG_GLOBAL PASS_LD);
      }
    } else if ( is_first(current) )
    { first_large_cell:
      update_relocation_chain(current, dest PASS_LD);	/* gTop refs from marks */
    } else if ( storage(*current) == STG_LOCAL ) /* large cell */
    { size_t offset = offset_cell(current);

      assert(offset > 0);
      current -= offset;		/* start large cell */
      if ( is_marked(current) )
      { dest -= offset;
	goto marked_large_cell;
      } else if ( is_first(current) )
      { goto first_large_cell;
      }	else
      { DEBUG(MSG_GC_HOLE, Sdprintf("Downskip from indirect\n"));
	current = downskip_combine_garbage(current, dest PASS_LD);
      }
    } else
    { DEBUG(MSG_GC_HOLE, Sdprintf("Downskip from normal cell\n"));
      current = downskip_combine_garbage(current, dest PASS_LD);
    }
  }

  DEBUG(CHK_SECURE,
	{ if ( v != mark_base )
	  { for( v--; v >= mark_base; v-- )
	    { Sdprintf("Expected marked cell at %p, (*= 0x%lx)\n", *v, **v);
	    }
	    sysError("v = %p; mark_base = %p", v, mark_base);
	  }
	});

  if ( dest != base )
    sysError("Mismatch in down phase: dest = %p, gBase = %p\n",
	     dest, gBase);
  if ( relocation_cells != relocated_cells )
  { DEBUG(CHK_SECURE, printNotRelocated());
    sysError("After down phase: relocation_cells = %ld; relocated_cells = %ld",
	     relocation_cells, relocated_cells);
  }

  DEBUG(CHK_SECURE, check_marked("Before up"));
  DEBUG(CHK_SECURE, relocated_check=FALSE);	/* see do_relocated_cell() */
  DEBUG(MSG_GC_PROGRESS, Sdprintf("Scanning global stack upwards\n"));

  dest = base;
  top = gTop;
  for(current = gBase; current < top; )
  { if ( is_marked(current) )
    { intptr_t l, n;

      if ( is_first(current) )
	update_relocation_chain(current, dest PASS_LD);

      if ( (l = offset_cell(current)) == 0 )	/* normal cells */
      { *dest = *current;
        if ( is_upward_ref(current PASS_LD) )
	{ check_relocation(current);
          into_relocation_chain(dest, STG_GLOBAL PASS_LD);
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
    { DEBUG(MSG_GC_HOLE,
	    if ( offset_cell(current) > 2 )
	      Sdprintf("Skipping garbage cell %p..%p, size %d\n",
		       current, current + offset_cell(current),
		       offset_cell(current)-1));
      current += offset_cell(current) + 1;
    }
  }

  if ( dest != gBase + total_marked )
    sysError("Mismatch in up phase: dest = %p, gBase+total_marked = %p\n",
	     dest, gBase + total_marked );

  DEBUG(CHK_SECURE,
	{ Word p = dest;		/* clear top of stack */
	  while(p < gTop)
	    *p++ = 0xbfbfbfbfL;
	});

  gTop = dest;
}


static void
collect_phase(vm_state *state, Word *saved_bar_at)
{ GET_LD

  DEBUG(CHK_SECURE, check_marked("Start collect"));

  DEBUG(MSG_GC_PROGRESS, Sdprintf("Sweeping foreign references\n"));
  sweep_foreign();
  DEBUG(MSG_GC_PROGRESS, Sdprintf("Sweeping trail stack\n"));
  sweep_trail();
  DEBUG(MSG_GC_PROGRESS, Sdprintf("Sweeping local stack\n"));
  sweep_stacks(state);
  if ( saved_bar_at )
  { DEBUG(2, Sdprintf("Sweeping frozen bar\n"));
    sweep_global_mark(saved_bar_at PASS_LD);
  }
  DEBUG(MSG_GC_PROGRESS, Sdprintf("Compacting global stack\n"));
  compact_global();

  unsweep_foreign(PASS_LD1);
  unsweep_stacks(state PASS_LD);

  assert(marks_swept==marks_unswept);
  if ( relocation_chains != 0 )
    sysError("relocation chains = %ld", relocation_chains);
  if ( relocated_cells != relocation_cells ||
       relocated_cells != needs_relocation )
    sysError("relocation cells = %ld; relocated_cells = %ld, "
	     "needs_relocation = %ld\n\t",
	     relocation_cells, relocated_cells, needs_relocation);
}

		 /*******************************
		 *	      VM-STATE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When using SAVE_REGISTERS(qid) in pl-vmi.c, the   PC  is either pointing
inside or pointing to the next instruction.   Here, we find the start of
the instruction for SHIFT/GC. We assume that   if  this is a first-write
instruction,  the  writing  has  not  yet  been    done.   If  it  is  a
read-intruction, we often have to be able to redo the read to compensate
for the possible shift inside the code protected by SAVE_REGISTERS().

The situation is more complicated. We need to know the depth in which we
are in *_functor...i_pop sequences. We always need to mark all arguments
of the first frame (well, this can be more subtle, but I really doubt we
want to try).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static void
setStartOfVMI(vm_state *state)
{ LocalFrame fr = state->frame;

  if ( fr->clause && false(fr->predicate, FOREIGN) && state->pc )
  { Clause clause = fr->clause->value.clause;
    Code PC, ep, next;

    if ( fr->predicate == PROCEDURE_dcall1->definition )
      state->in_body = TRUE;		/* There is no head code */

    PC = clause->codes;
    ep = PC + clause->code_size;

    for( ; PC < ep; PC = next )
    { code op;

      next = stepPC(PC);

      if ( next >= state->pc )
      {
#ifdef O_DEBUG
        size_t where  = PC - clause->codes;
	size_t where0 = state->pc - clause->codes;

	{ GET_LD
	  if ( truePrologFlag(PLFLAG_TRACE_GC) )
	  { Sdprintf("At PC=%ld(%ld) of "
		     "%d-th clause of %s (ARGP=%d; adepth=%d)\n",
		     where, where0,
		     clauseNo(fr->predicate, clause),
		     predicateName(fr->predicate),
		     (state->argp - argFrameP(fr, 0)),
		     state->adepth);
	  }
	}
#endif

	state->pc_start_vmi = PC;
	return;
      }

      op = fetchop(PC);
      switch(op)
      { case H_STRING:
	case H_MPZ:
	case H_LIST_FF:
	case H_FIRSTVAR:
	case H_VAR:
	case H_CONST:
	case H_NIL:
	case H_INTEGER:
	case H_INT64:
	case H_FLOAT:
	case H_VOID:
	  if ( state->adepth == 0 )
	    state->argp++;
	  break;
	case H_VOID_N:
	  if ( state->adepth == 0 )
	    state->argp += PC[1];
	  break;

	case B_UNIFY_VAR:
	case B_UNIFY_FIRSTVAR:
	  state->argp = varFrameP(state->frame, PC[1]);
	  assert(state->adepth == 0);
	  break;
	case H_FUNCTOR:
	case H_LIST:
	  if ( state->adepth == 0 )
	    state->argp0 = state->argp++;
	  /*FALLTHROUGH*/
	case B_FUNCTOR:
	case B_LIST:
	  state->adepth++;
	  break;
	case H_POP:
	case B_POP:
	  if ( --state->adepth == 0 )
	    state->argp0 = NULL;
	  break;
	case B_UNIFY_EXIT:
	  assert(state->adepth == 0);
	  break;
	case I_ENTER:
	  state->in_body = TRUE;
	  assert(state->adepth==0);
      }
    }
  }

  state->pc_start_vmi = NULL;
}


static Code
startOfVMI(QueryFrame qf)
{ vm_state state;

  state.frame  = qf->registers.fr;
  state.adepth = 0;
  state.argp   = argFrameP(state.frame, 0);
  state.argp0  = NULL;
  state.pc     = qf->registers.pc;

  setStartOfVMI(&state);

  return state.pc_start_vmi;
}


static void
get_vmi_state(QueryFrame qf, vm_state *state)
{ GET_LD

  state->choice	     = LD->choicepoints;
  state->lSave	     = lTop;
  state->in_body     = FALSE;
  state->adepth	     = 0;
  state->new_args    = 0;

  if ( qf && qf->registers.fr )
  { state->frame     = qf->registers.fr;

    if ( lTop <= state->frame )
    { int arity = state->frame->predicate->functor->arity;
      lTop = (LocalFrame)argFrameP(state->frame, arity);
      assert(!state->frame->clause);
    }

    state->argp		= argFrameP(state->frame, 0);
    state->argp0	= NULL;
    state->pc           = qf->registers.pc;
    state->save_argp    = (state->frame->clause != NULL);
    setStartOfVMI(state);

    if ( state->in_body )
    { Word ap = qf->registers.argp;
      Word *at = aTop;
      Word *ab = qf->aSave;

      for(;;)
      { if ( ap > (Word)lBase )
	{ assert(ap >= argFrameP(state->frame, 0));

	  if ( ap > argFrameP(lTop, 0) )
	  { state->new_args = (int)(ap - argFrameP(lTop, 0));
	    lTop = (LocalFrame)ap;
	  }
	  break;
	}
	if ( at > ab )
	{ uintptr_t uwrite = 0x1;	/* TBD: Share with def in pl-wam.c */
	  ap = *--at;
	  ap = (Word)((intptr_t)ap&~uwrite); /* see H_POP */
	} else
	  break;
      }
    }
  } else
  { state->frame        = environment_frame;
    state->pc           = NULL;
    state->pc_start_vmi = NULL;
    state->save_argp	= FALSE;
    if ( state->frame)
      state->argp       = argFrameP(state->frame, 0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we need to restore lTop if   we  are called from the body mode
because lTop is pointing to the new stack frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
restore_vmi_state(vm_state *state)
{ GET_LD

  lTop = state->lSave;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Used in D_BREAK to safely set lTop,  so the debugger won't overwrite the
stack-frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
setLTopInBody(void)
{ GET_LD
  vm_state state;

  get_vmi_state(LD->query, &state);
}


		/********************************
		*	    GC's MAIN           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If s == NULL, consider all stacks

(*) Do not  consider  GC  if  there   are  no  inferences.  This  avoids
repetetive GC calls while building large   structures  from foreign code
that calls PL_handle_signals() from time to   time  to enable interrupts
and call GC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
considerGarbageCollect(Stack s)
{ GET_LD

  if ( truePrologFlag(PLFLAG_GC) )
  { if ( PL_pending(SIG_GC) )
      return TRUE;

    if ( s == NULL )
    { return (considerGarbageCollect((Stack)&LD->stacks.global) ||
	      considerGarbageCollect((Stack)&LD->stacks.trail));
    } else
    { if ( s->gc )
      { size_t used  = usedStackP(s);	/* amount in actual use */
	size_t limit = sizeStackP(s);	/* amount we want to grow to */
	size_t space = limit - used;

	if ( LD->outofstack == s )
	{ DEBUG(MSG_GC_SCHEDULE, Sdprintf("GC: request on low space\n"));
	  return PL_raise(SIG_GC);
	}

	if ( LD->gc.inferences == LD->statistics.inferences &&
	     !LD->exception.processing )
	{ s->gced_size = used;		/* (*) */
	  return FALSE;
	}

	if ( used > s->factor*s->gced_size + s->small )
	{ DEBUG(MSG_GC_SCHEDULE,
		Sdprintf("GC: request on %s, factor=%d, last=%ld, small=%ld\n",
			 s->name, s->factor, s->gced_size, s->small));
	  return PL_raise(SIG_GC);
	} else if ( space < limit/8 &&
		    used > s->gced_size + limit/32 )
	{ DEBUG(MSG_GC_SCHEDULE, Sdprintf("GC: request on low space\n"));
	  return PL_raise(SIG_GC);
	}

	DEBUG(MSG_GC_SCHEDULE,
	      if ( PL_pending(SIG_GC) )
	      { Sdprintf("%s overflow: Posted garbage collect request\n",
			 s->name);
	      });
      }
    }
  }

  return FALSE;
}


#if O_DEBUG || defined(O_MAINTENANCE)
#define INTBITS (sizeof(int)*8)
#define REGISTER_STARTS 0x2

#if O_DEBUG
static void
alloc_start_map()
{ GET_LD
  size_t gsize = gTop+1-gBase;
  size_t ints = (gsize+INTBITS-1)/INTBITS;

  start_map = malloc(ints*sizeof(int));
  memset(start_map, 0, ints*sizeof(int));
}
#endif

static void
set_start(Word m ARG_LD)
{ size_t i = m-gBase;
  int bit = i % INTBITS;
  size_t at  = i / INTBITS;

  start_map[at] |= 1<<(bit-1);
}


static int
is_start(Word m ARG_LD)
{ size_t i = m-gBase;
  int bit = i % INTBITS;
  size_t at  = i / INTBITS;

  return (start_map[at] & 1<<(bit-1)) != 0;
}


bool
scan_global(int flags)
{ GET_LD
  Word current, next;
  int errors = 0;
  intptr_t cells = 0;
  int marked = (flags & TRUE);
  int regstart = start_map && (flags & REGISTER_STARTS) != 0;

  for( current = gBase; current < gTop; current += (offset_cell(current)+1) )
  { size_t offset;

    if ( regstart )
      set_start(current PASS_LD);
    cells++;

    if ( tagex(*current) == (TAG_VAR|STG_RESERVED) )
      Sdprintf("read varref at %p\n", current);

    if ( (!marked && is_marked(current)) || is_first(current) )
    { char pbuf[256];
      char vbuf[256];

      Sdprintf("!Illegal cell in global stack (up) at %s (*= %s)\n",
	       print_adr(current, pbuf), print_val(*current, vbuf));
      trap_gdb();

      if ( ++errors > 10 )
      { Sdprintf("...\n");
        break;
      }
    }

    offset = offset_cell(current);
    next = current+offset+1;
    if ( offset > 0 )
    { if ( offset_cell(next-1) != offset )
      { errors++;
	Sdprintf("ERROR: Illegal indirect cell on global stack at %p-%p\n"
		 "       tag=%d, offset=%ld\n",
		 current, next, tag(*current), (long)offset);
	trap_gdb();
      }
    } else if ( !marked )
    { if ( isRef(*current) )
      { if ( !onStack(global, unRef(*current)) )
	{ char b1[64], b2[64];

	  Sdprintf("ERROR: ref at %s not on global (*=%s)\n",
		   print_adr(current, b1), print_val(*current, b2));
	  trap_gdb();
	}
      }
    }
  }
  if ( regstart )
    set_start(gTop PASS_LD);

  for( current = gTop - 1; current >= gBase; current-- )
  { cells--;
    current -= offset_cell(current);
    if ( (!marked && is_marked(current)) || is_first(current) )
    { Sdprintf("!Illegal cell in global stack (down) at %p (*= %p)\n",
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


static void
check_mark(mark *m)
{ GET_LD

  assert(onTrailArea(m->trailtop));
  assert(onGlobalArea(m->globaltop));
  assert(onGlobalArea(m->saved_bar));
  assert(m->saved_bar <= m->globaltop);
  if ( start_map )
  { assert(is_start(m->globaltop PASS_LD));
    assert(is_start(m->saved_bar PASS_LD));
  }
}


static QueryFrame
check_environments(LocalFrame fr, Code PC, Word key)
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

    DEBUG(MSG_GC_CHECK,
	  Sdprintf("Check [%ld] %s (PC=%d):",
		   levelFrame(fr),
		   predicateName(fr->predicate),
		   (false(fr->predicate, FOREIGN) && PC)
		   ? (PC-fr->clause->value.clause->codes)
		   : 0));

    slots = slotsInFrame(fr, PC);
    sp = argFrameP(fr, 0);
    for( n=0; n < slots; n++ )
    { *key += checkData(&sp[n]);
    }
    DEBUG(MSG_GC_CHECK, Sdprintf(" 0x%lx\n", key));

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else
    { QueryFrame qf = queryOfFrame(fr);
      DEBUG(MSG_GC_CHECK,
	    Sdprintf("*** Query %s\n", predicateName(qf->frame.predicate)));
      return qf;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unfortunately the key returned by check_choicepoints() is not constant
due to `early reset' optimisation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
check_choicepoints(Choice ch)
{ GET_LD
  word key = 0L;

  for( ; ch; ch = ch->parent )
  { if ( !ch->parent )
      assert(ch->type == CHP_TOP);
    choice_count++;
    check_mark(&ch->mark);
    check_environments(ch->frame,
		       ch->type == CHP_JUMP ? ch->value.PC : NULL,
		       &key);
  }

  return key;
}


word
check_foreign()
{ GET_LD
  FliFrame ff;
  word key = 0L;

  for(ff = fli_context; ff; ff = ff->parent )
  { Word sp = refFliP(ff, 0);
    int n = ff->size;

    assert(ff->magic == FLI_MAGIC);
    if ( ff->parent )
    { assert(ff->parent < ff);
      assert(onStack(local, ff->parent));
    }

    for(n=0 ; n < ff->size; n++ )
      key += checkData(&sp[n]);

    if ( isRealMark(ff->mark) )
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
      assert(te > tBase);
      te--;
      assert(!isTrailVal(te->address));
#ifdef O_DEBUG
    } else if ( DEBUGGING(CHK_SECURE) )
    { if ( onGlobalArea(te->address) )
      { if ( !onStack(global, te->address) )
	{ char b1[64], b2[64], b3[64];

	  Sdprintf("Trail entry at %s not on global stack: %s (*=%s)\n",
		   print_adr((Word)te, b1),
		   print_adr(te->address, b2),
		   print_val(*te->address, b3));
	}
      }
#endif
    }
  }

  return key;
}
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/


static word
check_new_arguments(vm_state *state)
{ word key = 0L;

  if ( state->new_args )
  { Word sp = argFrameP(state->lSave, 0);
    int slots = state->new_args;

    for( ; slots-- > 0; sp++ )
      key += checkData(sp);
  }

  return key;
}



word
checkStacks(void *state_ptr)
{ GET_LD
  LocalFrame fr;
  Choice ch;
  QueryFrame qf;
  Code PC;
  word key = 0L;
  vm_state state_buf;
  vm_state *state;

  if ( state_ptr )
  { state = state_ptr;
  } else
  { state = &state_buf;
    get_vmi_state(LD->query, state);
  }

  assert(scan_global(FALSE));

  local_frames = 0;
  choice_count = 0;

  key += check_new_arguments(state);
  fr = state->frame;
  ch = state->choice;
  PC = state->pc_start_vmi;
  while(fr)
  { qf = check_environments(fr, PC, &key);
    assert(qf->magic == QID_MAGIC);

    DEBUG(MSG_GC_CHECK, Sdprintf("%ld\n", key));
    check_choicepoints(ch);		/* Do not update key; see above */
    if ( qf->parent )			/* same code in mark_stacks() */
    { QueryFrame pqf = qf->parent;

      assert(pqf->magic == QID_MAGIC);
      if ( (fr = pqf->registers.fr) )
      { PC = startOfVMI(pqf);
      } else
      { fr = qf->saved_environment;
	PC = NULL;
      }
      ch = qf->saved_bfr;
    } else
      break;
  }

  DEBUG(CHK_SECURE, trailtops_marked = choice_count);

  unmark_stacks(LD, state->frame, state->choice, FR_MARKED);

  assert(local_frames == 0);
  assert(choice_count == 0);

  key += check_foreign();
  DEBUG(MSG_GC_CHECK, Sdprintf("Foreign: %ld\n", key));
#ifdef O_DESTRUCTIVE_ASSIGNMENT
  /*key +=*/ check_trail();
#endif

  if ( state == &state_buf )
    restore_vmi_state(state);

  DEBUG(MSG_GC_CHECK, Sdprintf("Final: %ld\n", key));
  return key;
}


static
PRED_IMPL("$check_stacks", 1, check_stacks, 0)
{ char *s = NULL;

  if ( PL_get_atom_chars(A1, &s) )
    Sdprintf("[thread %d] Checking stacks [%s] ...",
	     PL_thread_self(), s);

  checkStacks(NULL);
  if ( s )
     Sdprintf(" (done)\n");

  succeed;
}

#endif /* O_DEBUG */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
About synchronisation with atom-gc (AGC). GC can run fully concurrent in
different threads as it only  affects   the  runtime stacks. AGC however
must sweep the other threads. It can only do so if these are in a fairly
sane state, which isn't the case during GC.  So:

We keep the number of threads doing GC in GD->gc.active, a variable that
is incremented and decremented using  the   L_GC  mutex. This same mutex
guards AGC as a whole. This  means  that   if  AGC  is working, GC can't
start. If AGC notices at the start  a   GC  is working, it sets the flag
GD->gc.agc_waiting and returns. If the last   GC  stops, and notices the
system wants to do AGC it raised a request for AGC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
enterGC(ARG1_LD)
{
#ifdef O_PLMT
  PL_LOCK(L_GC);
  GD->gc.active++;
  PL_UNLOCK(L_GC);
  LD->gc.active = TRUE;
#endif
}

static void
leaveGC(ARG1_LD)
{
#ifdef O_PLMT
  LD->gc.active = FALSE;
  PL_LOCK(L_GC);
  if ( --GD->gc.active == 0 && GD->gc.agc_waiting )
  { GD->gc.agc_waiting = FALSE;
    PL_raise(SIG_ATOM_GC);
  }
  PL_UNLOCK(L_GC);
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns: < 0: (local) overflow; TRUE: ok; FALSE: shifted;

If gcEnsureSpace() returns overflow or out-of-stack, it has restored the
given vm-state.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
gcEnsureSpace(vm_state *state ARG_LD)
{ int rc = TRUE;
  size_t lneeded = 0;

  if ( LD->gvar.grefs )
    lneeded += sizeof(struct fliFrame) + LD->gvar.grefs*sizeof(word);
  if ( LD->frozen_bar )
    lneeded += sizeof(Word);
  if ( state->save_argp )
    lneeded += sizeof(struct fliFrame) + (aTop+1-aBase)*sizeof(word);

  if ( (char*)lTop + lneeded > (char*)lMax )
  { if ( (char*)lTop + lneeded > (char*)lMax + LD->stacks.local.spare )
    { int rc2;

      restore_vmi_state(state);
      if ( (rc2=ensureLocalSpace(lneeded, ALLOW_SHIFT)) != TRUE )
	return rc2;
      rc = FALSE;
    } else
    { enableSpareStack((Stack)&LD->stacks.local);
    }
  }
  if ( gTop+1 > gMax )
  { assert(LD->stacks.global.spare);
    enableSpareStack((Stack)&LD->stacks.global);
  }
  if ( tTop+1 > tMax )
  { assert(LD->stacks.trail.spare);
    enableSpareStack((Stack)&LD->stacks.trail);
  }

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
garbageCollect()  returns  one  of  TRUE    (ok),   FALSE  (blocked)  or
LOCAL_OVERFLOW if the local stack  cannot accomodate the term-references
for saving ARGP and global variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
garbageCollect(void)
{ GET_LD
  vm_state state;
  intptr_t tgar, ggar;
  double t = ThreadCPUTime(LD, CPU_USER);
  int verbose = truePrologFlag(PLFLAG_TRACE_GC);
  int no_mark_bar;
  int rc;
  fid_t gvars, astack;
  Word *saved_bar_at;
#ifdef O_PROFILE
  struct call_node *prof_node = NULL;
#endif

  END_PROF();
  START_PROF(P_GC, "P_GC");

  if ( gc_status.blocked || !truePrologFlag(PLFLAG_GC) )
    return FALSE;

#ifdef O_MAINTENANCE
  save_backtrace("GC");
#endif

  get_vmi_state(LD->query, &state);
  if ( (rc=gcEnsureSpace(&state PASS_LD)) < 0 )
  { return rc;
  } else if ( rc == FALSE )		/* shifted; reload */
  { get_vmi_state(LD->query, &state);
  }

  enterGC(PASS_LD1);
#ifndef UNBLOCKED_GC
  blockSignals(&LD->gc.saved_sigmask);
#endif
  blockGC(0 PASS_LD);			/* avoid recursion due to */
  PL_clearsig(SIG_GC);

  gc_status.active = TRUE;

  if ( (no_mark_bar=(LD->mark_bar == NO_MARK_BAR)) )
    LD->mark_bar = gTop;		/* otherwise we cannot relocate */

  if ( verbose )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "gc", 1,
		   PL_CHARS, "start");

#ifdef O_PROFILE
  if ( LD->profile.active )
    prof_node = profCall(GD->procedures.dgarbage_collect1->definition PASS_LD);
#endif

#if O_DEBUG
  if ( DEBUGGING(CHK_SECURE) )
  { alloc_start_map();
    if ( !scan_global(FALSE|REGISTER_STARTS) )
      sysError("Stack not ok at gc entry");
    checkStacks(&state);
    free(start_map);
    start_map = NULL;

    if ( check_table == NULL )
    { check_table = newHTable(256);
      local_table = newHTable(256);
    } else
    { clearHTable(check_table);
      clearHTable(local_table);
    }

    mark_base = mark_top = malloc(usedStack(global));
    relocated_check = TRUE;
  }
#endif

  needs_relocation  = 0;
  relocation_chains = 0;
  relocation_cells  = 0;
  relocated_cells   = 0;
  local_marked	    = 0;
  marks_swept	    = 0;
  marks_unswept	    = 0;
  LD->gc.marked_attvars = FALSE;

  assert(gTop+1 <= gMax);
  assert(tTop+1 <= tMax);
  setVar(*gTop);
  tTop->address = 0;

  astack = argument_stack_to_term_refs(&state);
  gvars = gvars_to_term_refs(&saved_bar_at);
  DEBUG(CHK_SECURE, check_foreign());
  tag_trail();
  mark_phase(&state);
  tgar = trailcells_deleted * sizeof(struct trail_entry);
  ggar = (gTop - gBase - total_marked) * sizeof(word);
  gc_status.global_gained += ggar;
  gc_status.trail_gained  += tgar;
  gc_status.collections++;

  DEBUG(MSG_GC_PROGRESS, Sdprintf("Compacting trail\n"));
  compact_trail();

  collect_phase(&state, saved_bar_at);
  untag_trail();
  term_refs_to_gvars(gvars, saved_bar_at);
  term_refs_to_argument_stack(&state, astack);

  assert(LD->mark_bar <= gTop);

  DEBUG(CHK_SECURE,
	{ assert(trailtops_marked == 0);
	  if ( !scan_global(FALSE) )
	    sysError("Stack not ok after gc; gTop = %p", gTop);
	  free(mark_base);
	});

  t = ThreadCPUTime(LD, CPU_USER) - t;
  gc_status.time += t;
  LD->stacks.global.gced_size = usedStack(global);
  LD->stacks.trail.gced_size  = usedStack(trail);
  gc_status.global_left      += usedStack(global);
  gc_status.trail_left       += usedStack(trail);

  DEBUG(CHK_SECURE,
	{ memset(gTop, 0xFB, (char*)gMax-(char*)gTop);
	  memset(tTop, 0xFB, (char*)tMax-(char*)tTop);
	  checkStacks(&state);
	});

  if ( verbose )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "gc", 1,
		   PL_FUNCTOR_CHARS, "done", 7,
		     PL_INTPTR, ggar,
		     PL_INTPTR, tgar,
		     PL_DOUBLE, (double)t,
		     PL_INTPTR, usedStack(global),
		     PL_INTPTR, usedStack(trail),
		     PL_INTPTR, roomStack(global),
		     PL_INTPTR, roomStack(trail));

  trimStacks(LD->trim_stack_requested PASS_LD);

#ifdef O_PROFILE
  if ( prof_node && LD->profile.active )
    profExit(prof_node PASS_LD);
#endif

  restore_vmi_state(&state);
  if ( no_mark_bar )
    LD->mark_bar = NO_MARK_BAR;
  gc_status.active = FALSE;
  unblockGC(0 PASS_LD);
#ifndef UNBLOCKED_GC
  unblockSignals(&LD->gc.saved_sigmask);
#endif
  LD->gc.inferences = LD->statistics.inferences;
  leaveGC(PASS_LD1);

  assert(!LD->query ||
	 !LD->query->registers.fr ||
	 state.frame == LD->query->registers.fr);
  shiftTightStacks();

  return TRUE;
}

word
pl_garbage_collect(term_t d)
{
#if O_DEBUG
  int ol = GD->debug_level;
  int nl;

  if ( d )
  { if ( !PL_get_integer_ex(d, &nl) )
      fail;
    GD->debug_level = nl;
  }
#endif
  garbageCollect();
#if O_DEBUG
  GD->debug_level = ol;
#endif
  succeed;
}


void
blockGC(int flags ARG_LD)
{ if ( !(flags & ALLOW_GC) )
    gc_status.blocked++;
  if ( !(flags & ALLOW_SHIFT) )
    LD->shift_status.blocked++;
}


void
unblockGC(int flags ARG_LD)
{ if ( !(flags & ALLOW_GC) )
    gc_status.blocked--;
  if ( !(flags & ALLOW_SHIFT) )
    LD->shift_status.blocked--;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
makeMoreStackSpace(int overflow, int flags)

Used in loops where the  low-level   implementation  does  not allow for
stack-shifts.  Returns TRUE or FALSE and raises an exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
makeMoreStackSpace(int overflow, int flags)
{ GET_LD

  if ( overflow == MEMORY_OVERFLOW )
    return raiseStackOverflow(overflow);

  if ( LD->exception.processing )
  { if ( overflow == GLOBAL_OVERFLOW &&
	 enableSpareStack((Stack)&LD->stacks.global) )
      return TRUE;
    if ( overflow == TRAIL_OVERFLOW &&
	 enableSpareStack((Stack)&LD->stacks.trail) )
      return TRUE;
    if ( overflow == LOCAL_OVERFLOW &&
	 enableSpareStack((Stack)&LD->stacks.local) )
      return TRUE;
  }

  if ( LD->gc.inferences != LD->statistics.inferences &&
       (flags & ALLOW_GC) &&
       garbageCollect() )
    return TRUE;

  if ( (flags & ALLOW_SHIFT) )
  { size_t l=0, g=0, t=0;
    int rc;

    switch(overflow)
    { case LOCAL_OVERFLOW:  l = 1; break;
      case GLOBAL_OVERFLOW: g = 1; break;
      case TRAIL_OVERFLOW:  t = 1; break;
      default:
	return raiseStackOverflow(overflow);
    }

    if ( (rc = growStacks(l, g, t)) == TRUE )
      return rc;
    else if ( rc < 0 )
      return raiseStackOverflow(rc);
  }

  return raiseStackOverflow(overflow);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ensureGlobalSpace(size_t cell, int flags)

Makes sure we have the requested amount of space on the global stack. If
the space is not available, first try GC; than try shifting the stacks.

Returns TRUE, FALSE or *_OVERFLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ensureGlobalSpace(size_t cells, int flags)
{ GET_LD

  cells += BIND_GLOBAL_SPACE;
  if ( gTop+cells <= gMax && tTop+BIND_TRAIL_SPACE <= tMax )
    return TRUE;

  if ( LD->exception.processing || LD->gc.status.active == TRUE )
  { enableSpareStack((Stack)&LD->stacks.global);
    enableSpareStack((Stack)&LD->stacks.trail);

    if ( gTop+cells <= gMax && tTop+BIND_TRAIL_SPACE <= tMax )
      return TRUE;
  }

  if ( !flags )
    goto nospace;

  if ( (flags & ALLOW_GC) && considerGarbageCollect(NULL) )
  { garbageCollect();

    if ( gTop+cells <= gMax && tTop+BIND_TRAIL_SPACE <= tMax )
      return TRUE;
  }

  { size_t gmin;
    size_t tmin;

    if ( gTop+cells > gMax || tight((Stack)&LD->stacks.global PASS_LD) )
      gmin = cells*sizeof(word);
    else
      gmin = 0;

    if ( tight((Stack)&LD->stacks.trail PASS_LD) )
      tmin = BIND_TRAIL_SPACE*sizeof(struct trail_entry);
    else
      tmin = 0;

    growStacks(0, gmin, tmin);
    if ( gTop+cells <= gMax && tTop+BIND_TRAIL_SPACE <= tMax )
      return TRUE;
  }

nospace:
  if ( gTop+cells > gMax )
    return GLOBAL_OVERFLOW;
  else
    return TRAIL_OVERFLOW;
}


int
ensureTrailSpace(size_t cells)
{ GET_LD

  if ( tTop+cells <= tMax )
    return TRUE;

  if ( LD->exception.processing || LD->gc.status.active == TRUE )
  { enableSpareStack((Stack)&LD->stacks.trail);

    if ( tTop+cells <= tMax )
      return TRUE;
  }

  if ( considerGarbageCollect(NULL) )
  { garbageCollect();

    if ( tTop+cells <= tMax )
      return TRUE;
  }

  { size_t tmin = cells*sizeof(struct trail_entry);

    growStacks(0, 0, tmin);
    if ( tTop+cells <= tMax )
      return TRUE;
  }

  return TRAIL_OVERFLOW;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ensureLocalSpace() ensures sufficient local stack space.

NOTE: This is often called from ENSURE_LOCAL_SPACE(), while already lTop
> lMax. The stack-shifter must be able to deal with this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ensureLocalSpace(size_t bytes, int flags)
{ GET_LD

  if ( addPointer(lTop, bytes) <= (void*)lMax )
    return TRUE;

  if ( LD->exception.processing || LD->gc.status.active == TRUE )
  { enableSpareStack((Stack)&LD->stacks.local);
    if ( addPointer(lTop, bytes) <= (void*)lMax )
      return TRUE;
  }

  if ( !flags )
    goto nospace;

  growStacks(bytes, 0, 0);
  if ( addPointer(lTop, bytes) <= (void*)lMax )
    return TRUE;

nospace:
  return LOCAL_OVERFLOW;
}


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

#define update_pointer(p, offset) \
	do { if ( *p ) *p = addPointer(*p,offset); } while(0)


		 /*******************************
		 *	   LOCAL STACK		*
		 *******************************/

static void
update_mark(mark *m, intptr_t gs, intptr_t ts)
{ if ( ts )
    update_pointer(&m->trailtop, ts);
  if ( gs )
  { update_pointer(&m->globaltop, gs);
    update_pointer(&m->saved_bar, gs);
  }
}


/* Update pointer if it contains a pointer in the local stack.  Used for
   updating PC, as this might point to a locally compiled clause by
   I_USERCALL0.
*/

static inline void
update_local_pointer(Code *p, intptr_t ls)
{ GET_LD

  if ( onStackArea(local, *p) )
  { DEBUG(MSG_SHIFT_POINTER, Sdprintf(" (local ptr %p)", *p));
    update_pointer(p, ls);
  }
}


static inline void
update_lg_pointer(Word *p, intptr_t ls, intptr_t gs ARG_LD)
{ if ( onStackArea(local, *p) )
  { update_pointer(p, ls);
  } else if ( onGlobalArea(*p) )
  { update_pointer(p, gs);
  }
}


static QueryFrame
update_environments(LocalFrame fr, intptr_t ls, intptr_t gs)
{ GET_LD
  if ( fr == NULL )
    return NULL;

  for(;;)
  { assert(inShiftedArea(local, ls, fr));

    if ( true(fr, FR_MARKED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED);
    local_frames++;

    DEBUG(MSG_SHIFT_FRAME,
	  Sdprintf("Shifting frame %p [%ld] %s ... ",
		   fr, levelFrame(fr), predicateName(fr->predicate)));

    if ( ls )				/* update frame pointers */
    { update_pointer(&fr->parent, ls);

      update_local_pointer(&fr->programPointer, ls);
					/* I_USERCALL0 compiled clause */
      if ( fr->predicate == PROCEDURE_dcall1->definition && fr->clause )
      { assert(onStackArea(local, fr->clause));
	update_pointer(&fr->clause, ls);
	update_pointer(&fr->clause->value.clause, ls);
      } else
      { assert(!onStackArea(local, fr->clause));
      }

      DEBUG(MSG_SHIFT_FRAME, Sdprintf("ok\n"));
    }

    if ( fr->parent )
      fr = fr->parent;
    else				/* Prolog --> C --> Prolog calls */
    { QueryFrame query = queryOfFrame(fr);

      if ( ls )
      { update_pointer(&query->parent, ls);
        update_pointer(&query->saved_bfr, ls);
        update_pointer(&query->saved_ltop, ls);
	update_pointer(&query->saved_environment, ls);
	update_pointer(&query->registers.fr, ls);
	update_local_pointer(&query->registers.pc, ls);
      }
      if ( ls || gs )
      { update_lg_pointer(&query->registers.argp, ls, gs PASS_LD);
      }

      return query;
    }
  }
}


static void
update_choicepoints(Choice ch, intptr_t ls, intptr_t gs, intptr_t ts)
{ GET_LD

  for( ; ch; ch = ch->parent )
  { if ( ls )
    { update_pointer(&ch->frame, ls);
      update_pointer(&ch->parent, ls);
      if ( ch->type == CHP_JUMP )
	update_local_pointer(&ch->value.PC, ls);
    }
    update_mark(&ch->mark, gs, ts);

    DEBUG(MSG_SHIFT_FRAME, Sdprintf("Updated %s for %s ... ",
		      chp_chars(ch),
		      predicateName(ch->frame->predicate)));

    update_environments(ch->frame, ls, gs);
    choice_count++;
    DEBUG(MSG_SHIFT_FRAME, Sdprintf("ok\n"));
  }
}


		 /*******************************
		 *	  ARGUMENT STACK	*
		 *******************************/

static void
update_argument(intptr_t ls, intptr_t gs)
{ GET_LD
  Word *p = aBase;
  Word *t = aTop;

  for( ; p < t; p++ )
  { Word ptr = *p;

    DEBUG(CHK_SECURE, assert(onGlobal(ptr) || onLocal(ptr)));

    if ( ptr > (Word)lBase )
      *p = addPointer(ptr, ls);
    else
      *p = addPointer(ptr, gs);
  }
}


		 /*******************************
		 *	  TRAIL STACK	*
		 *******************************/

static void
update_trail(TrailEntry tb, intptr_t ls, intptr_t gs)
{ GET_LD
  TrailEntry p = tb;			/* new base */
  TrailEntry t = tb+(tTop-tBase);	/* new top */

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
update_foreign(intptr_t ts, intptr_t ls, intptr_t gs)
{ GET_LD
  FliFrame fr = addPointer(fli_context, ls);

  for( ; fr; fr = fr->parent )
  { if ( isRealMark(fr->mark) )
      update_mark(&fr->mark, gs, ts);
    update_pointer(&fr->parent, ls);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update global variables. As our pointers   areoffsets  to the stacks, we
don't actually need to update the variables   themselves.  We do need to
update the frozen bar however.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
update_gvars(intptr_t gs)
{ GET_LD

  if ( LD->frozen_bar )
  { update_pointer(&LD->frozen_bar, gs);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Entry-point.   Update the  stacks to  reflect  their current  positions.
This function should be called *after*  the  stacks have been relocated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define updateStackHeader(name, offset) \
  { LD->stacks.name.base    = addPointer(LD->stacks.name.base,    offset); \
    LD->stacks.name.top     = addPointer(LD->stacks.name.top,     offset); \
    LD->stacks.name.max     = addPointer(LD->stacks.name.max,     offset); \
  }


static void
update_stacks(vm_state *state, void *lb, void *gb, void *tb)
{ GET_LD
  intptr_t ls, gs, ts;

  ls = (intptr_t) lb - (intptr_t) lBase;
  gs = (intptr_t) gb - (intptr_t) gBase;
  ts = (intptr_t) tb - (intptr_t) tBase;

  DEBUG(MSG_SHIFT_PROGRESS,
	Sdprintf("update_stacks(): ls+gs+ts = %ld %ld %ld\n", ls, gs, ts));

  if ( ls || gs || ts )
  { LocalFrame fr;
    Choice ch;
    QueryFrame qf;

    local_frames = 0;
    choice_count = 0;

    if ( ls )
    {					/* sometimes on local stack */
      update_local_pointer(&state->pc, ls);
      update_local_pointer(&state->pc_start_vmi, ls);
					/* always on local stack */
      update_pointer(&state->frame, ls);
      update_pointer(&state->choice, ls);
      update_pointer(&state->lSave, ls);
      update_pointer(&LD->query, ls);
    }

    for( fr = state->frame,
	 ch = state->choice
       ; fr
       ;
       )
    { qf = update_environments(fr, ls, gs);
      assert(qf->magic == QID_MAGIC);

      update_choicepoints(ch, ls, gs, ts);

      if ( qf->parent )
      { QueryFrame pqf = qf->parent;

	if ( (fr = pqf->registers.fr) )
	{ fr = addPointer(fr, ls);	/* parent is not yet shifted */
	} else
	{ fr = qf->saved_environment;
	}
	ch = qf->saved_bfr;
      } else
	break;
    }

    DEBUG(MSG_SHIFT_PROGRESS,
	  Sdprintf("%d frames, %d choice-points ...",
		   local_frames, choice_count));

    unmark_stacks(LD, state->frame, state->choice, FR_MARKED);

    assert(local_frames == 0);
    assert(choice_count == 0);

    if ( gs || ls )
    { update_argument(ls, gs);
      update_trail(tb, ls, gs);
    }
    update_foreign(ts, ls, gs);
    if ( gs )
      update_gvars(gs);

    updateStackHeader(local,  ls);
    updateStackHeader(global, gs);
    updateStackHeader(trail,  ts);

    base_addresses[STG_LOCAL]  = (uintptr_t)lBase;
    base_addresses[STG_GLOBAL] = (uintptr_t)(gBase-1); /* MARK_MASK */
    base_addresses[STG_TRAIL]  = (uintptr_t)tBase;
  }

  if ( ls )
  { update_pointer(&LD->environment,         ls);
    update_pointer(&LD->foreign_environment, ls);
    update_pointer(&LD->choicepoints,        ls);
  }
  if ( gs && LD->mark_bar != NO_MARK_BAR )
  { update_pointer(&LD->mark_bar, gs);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
nextStackSize() computes the size to use for s, given it should at least
have minfree space after the stack  expansion.   We  want stacks to grow
along a fixed set of sizes to maximize reuse of abandoned stacks.

Note that we allocate local and  global   stacks  in one chunk, so their
combined size should come from a fixed maximum.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef K
#undef MB
#define K * 1024
#define MB * (1024L * 1024L)

size_t
nextStackSizeAbove(size_t n)
{ size_t size;

#ifdef O_DEBUG
  if ( DEBUGGING(CHK_SECURE) )
  { static int got_incr = FALSE;
    static size_t increment = 0;
    GET_LD

    if ( !got_incr )
    { char *incr = getenv("PL_STACK_INCREMENT"); /* 1: random */

      if ( incr )
        increment = atol(incr);
      got_incr = TRUE;
    }

    if ( increment )
    { size_t sz;

      if ( increment == 1 )
      {
#ifdef __WINDOWS__
	sz = n+rand()%10000;
#else
	sz = n+rand_r(&LD->gc.incr_seed)%10000;
#endif
      } else
      { sz = n+increment;
      }

      return sz & ~(size_t)(sizeof(word)-1); /* align on words */
    }
  }
#endif

  if ( n < 4 MB )
  { size = 8192;
    while ( size <= n )
      size *= 2;
  } else
  { size = 4 MB;

    while ( size <= n )
    { if ( (size + size/2) > n )
	return size + size/2;

      size *= 2;
    }
  }
					/* enforce real limit */
  if ( size > (size_t)(MAXTAGGEDPTR+1) )
    size = (size_t)(MAXTAGGEDPTR+1);
  if ( size < n )
    return 0;				/* still too small */

  return size;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return next size for the stack that ensures minfree bytes of free space.
We add another s->min_free to give some freedom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

size_t
nextStackSize(Stack s, size_t minfree)
{ size_t size;

  if ( minfree == GROW_TRIM )
  { size = nextStackSizeAbove(usedStackP(s) + s->min_free + s->def_spare);
    if ( size > (size_t)sizeStackP(s) )
      size = sizeStackP(s);
  } else
  { if ( s->top > s->max )
      minfree += (char*)s->top - (char*)s->max;

    size = nextStackSizeAbove(sizeStackP(s) +
			      minfree + s->min_free + s->def_spare);

    if ( size >= s->size_limit + s->size_limit/2 )
    { if ( minfree == 1 && roomStackP(s) > (ssize_t)minfree )
	size = sizeStackP(s);		/* tight-stack request */
      else
	size = 0;			/* passed limit */
    }
  }

  return size;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Stack shifter entry point. The arguments l, g and t request expansion of
the local, global and trail-stacks. Non-0 versions   ask the stack to be
modified. Positive values enlarge the stack to the next size that has at
least the specified value free space (i.e. min-free).

GROW_TRIM cause the stack to  shrink  to   the  value  nearest above the
current usage and the minimum free stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
new_stack_size(Stack s, size_t *request, size_t *newsize)
{ if ( *request )
  { size_t new;

    if ( !(new = nextStackSize(s, *request)) )
      return s->overflow_id;
    *newsize = new;
    if ( new == sizeStackP(s) )
    { *request = 0;
      return FALSE;			/* no change */
    }

    return TRUE;
  } else
  { *newsize = sizeStackP(s);

    return FALSE;
  }
}


static int
grow_stacks(size_t l, size_t g, size_t t ARG_LD)
{ sigset_t mask;
  size_t lsize=0, gsize=0, tsize=0;
  vm_state state;
  Stack fatal = NULL;	/* stack we couldn't expand due to lack of memory */
  int rc;
#if O_DEBUG
  word key=0;
#endif

  if ( (rc=new_stack_size((Stack)&LD->stacks.trail,  &t, &tsize))<0 ||
       (rc=new_stack_size((Stack)&LD->stacks.global, &g, &gsize))<0 ||
       (rc=new_stack_size((Stack)&LD->stacks.local,  &l, &lsize))<0 )
  { DEBUG(MSG_STACK_OVERFLOW, Sdprintf("Reached stack-limit\n"));
    return rc;
  }

  if ( !(l || g || t) )
    return TRUE;			/* not a real request */

  if ( LD->shift_status.blocked )
    return FALSE;

  enterGC(PASS_LD1);			/* atom-gc synchronisation */
  blockSignals(&mask);
  blockGC(0 PASS_LD);			/* avoid recursion due to */
  PL_clearsig(SIG_GC);

  get_vmi_state(LD->query, &state);
  DEBUG(CHK_SECURE,
	{ gBase++;
	  checkStacks(&state);
	  gBase--;
	});

  { TrailEntry tb = tBase;
    Word gb = gBase;
    LocalFrame lb = lBase;
    double time, time0 = ThreadCPUTime(LD, CPU_USER);
    int verbose = truePrologFlag(PLFLAG_TRACE_GC);

    if ( verbose )
    { const char *prefix;
      int tid = PL_thread_self();

      if ( Serror->position && Serror->position->linepos > 0 )
	prefix = "\n% ";
      else
	prefix = "% ";

      if ( tid != 1 )
	Sdprintf("%s[%d] SHIFT: l:g:t = %ld:%ld:%ld ...",
		 prefix, tid, (long)l, (long)g, (long)t);
      else
	Sdprintf("%sSHIFT: l:g:t = %ld:%ld:%ld ...",
		 prefix, (long)l, (long)g, (long)t);
    }

    DEBUG(CHK_SECURE,
	  { gBase++;
	    if ( !scan_global(FALSE) )
	      sysError("Stack not ok at shift entry");
	    key = checkStacks(&state);
	    gBase--;
	  });

    if ( t )
    { void *nw;

      if ( (nw = stack_realloc(tb, tsize)) )
      { LD->shift_status.trail_shifts++;
	tb = nw;
      } else
      { fatal = (Stack)&LD->stacks.trail;
	tsize = sizeStack(trail);
      }
    }

    if ( g || l )
    { size_t ogsize, olsize;
      void *nw;

      assert(*gb == MARK_MASK);		/* see initPrologStacks() */
      ogsize = sizeStack(global);
      olsize = sizeStack(local);
      assert(lb == addPointer(gb, ogsize));

      if ( gsize < ogsize )		/* TBD: Only copy life-part */
	memmove(addPointer(gb, gsize), lb, olsize);

      if ( (nw = stack_realloc(gb, lsize + gsize)) )
      { if ( g )
	  LD->shift_status.global_shifts++;
	if ( l )
	  LD->shift_status.local_shifts++;

	gb = nw;
	lb = addPointer(gb, gsize);
	if ( gsize > ogsize )
	  memmove(lb, addPointer(gb, ogsize), olsize);
      } else
      { if ( g )
	  fatal = (Stack)&LD->stacks.global;
	else
	  fatal = (Stack)&LD->stacks.local;

	gsize = sizeStack(global);
	lsize = sizeStack(local);
	DEBUG(MSG_STACK_OVERFLOW, Sdprintf("realloc() failed\n"));
      }
    }

#define PrintStackParms(stack, name, newbase, newsize) \
	{ void *newmax = addPointer(newbase, newsize); \
	  Sdprintf("%-6s: %p ... %p --> ", \
		   name, \
		   LD->stacks.stack.base, \
		   LD->stacks.stack.max); \
	  if ( LD->stacks.stack.base == newbase && \
	       (void*)LD->stacks.stack.max == newmax ) \
	  { Sdprintf("(no change)\n"); \
	  } else \
	  { Sdprintf("%p ... %p\n", newbase, newmax); \
	  } \
	}

    if ( verbose )
    { DEBUG(0, { Sputchar('\n');
		 PrintStackParms(global, "global", gb, gsize);
		 PrintStackParms(local, "local", lb, lsize);
		 PrintStackParms(trail, "trail", tb, tsize);
	       });
    }

    DEBUG(MSG_SHIFT_PROGRESS, Sdprintf("Updating stacks ..."));
    gBase++; gb++;
    update_stacks(&state, lb, gb, tb);
    gBase--; gb--;

    LD->stacks.local.max  = addPointer(LD->stacks.local.base,  lsize);
    LD->stacks.global.max = addPointer(LD->stacks.global.base, gsize);
    LD->stacks.trail.max  = addPointer(LD->stacks.trail.base,  tsize);

    time = ThreadCPUTime(LD, CPU_USER) - time0;
    LD->shift_status.time += time;
    DEBUG(CHK_SECURE,
	  { gBase++;
	    if ( checkStacks(&state) != key )
	    { Sdprintf("Stack checksum failure\n");
	      trap_gdb();
	    }
	    gBase--;
	  });
    if ( verbose )
    { Sdprintf("l+g+t = %lld+%lld+%lld (%.3f sec)\n",
	       (int64_t)lsize, (int64_t)gsize, (int64_t)tsize, time);
    }
  }

  DEBUG(CHK_SECURE,
	{ gBase++;
	  checkStacks(&state);
	  gBase--;
	});
  restore_vmi_state(&state);
  unblockGC(0 PASS_LD);
  unblockSignals(&mask);
  leaveGC(PASS_LD1);

  if ( fatal )
    return fatal->overflow_id;

  return TRUE;
}


static void
include_spare_stack(Stack s, size_t *request)
{ if ( *request && *request != GROW_TRIM )
    *request += s->def_spare - s->spare;

  if ( s->spare )
  { s->max = addPointer(s->max, s->spare);
    s->spare = 0;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns one of TRUE:  Stacks  are   resized;  FALSE:  stack-shifting  is
blocked or *_OVERFLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
growStacks(size_t l, size_t g, size_t t)
{ GET_LD
  int rc;

#ifdef O_MAINTENANCE
  save_backtrace("SHIFT");
#endif

  gBase--;
  include_spare_stack((Stack)&LD->stacks.local,  &l);
  include_spare_stack((Stack)&LD->stacks.global, &g);
  include_spare_stack((Stack)&LD->stacks.trail,  &t);

  rc = grow_stacks(l, g, t PASS_LD);

  trim_stack((Stack)&LD->stacks.trail);
  trim_stack((Stack)&LD->stacks.global);
  trim_stack((Stack)&LD->stacks.local);
  gBase++;

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) Some programs have a lot of global and hardly any trail requirement.
This means we gets lots of GCs for trail, which works fine, but they are
expensive due to the size of the global stack. As long as we do not have
generational GC, we make the trail free space proportional to the global
stack usage. This too isn't ideal; it is possible that simply nothing is
trailed and therefore it can be low.   Ideally, I think that the margins
should depend on the percentage of the   time spent in GC's triggered by
the stack. One of the problems we are   faced with is that not all OS'es
allow us to get per-thread CPU statistics.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
tight(Stack s ARG_LD)
{ size_t min_room  = sizeStackP(s)/3;
  size_t spare_gap = s->def_spare - s->spare;

  if ( s == (Stack)&LD->stacks.trail )	/* See (*) */
  { min_room += sizeStack(global)/6;
    DEBUG(MSG_GC_SCHEDULE, Sdprintf("Trail min_room = %ld\n", min_room));
  }

  if ( min_room < s->min_free )
    min_room = s->min_free;

  if ( (size_t)roomStackP(s) < min_room + spare_gap )
    return 1;

  return 0;
}


int
shiftTightStacks(void)
{ GET_LD
  size_t l = tight((Stack)&LD->stacks.local PASS_LD);
  size_t g = tight((Stack)&LD->stacks.global PASS_LD);
  size_t t = tight((Stack)&LD->stacks.trail PASS_LD);

  if ( (l|g|t) )
    return growStacks(l, g, t);

  return TRUE;
}


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

static intptr_t
loffset(void *p)
{ GET_LD
  if ( p == NULL )
    return 0;

  assert((intptr_t)p % sizeof(word) == 0);
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

    if ( fr->predicate == PROCEDURE_dcall1->definition &&
	 fr->clause )
      forAtomsInClause(fr->clause->value.clause, markAtom);

    if ( true(fr->predicate, FOREIGN) ||
	 !fr->clause )
      slots = fr->predicate->functor->arity;
    else
      slots = fr->clause->value.clause->prolog_vars;

    sp = argFrameP(fr, 0);
    for( n=0; n < slots; n++, sp++ )
    { if ( isAtom(*sp) )
	markAtom(*sp);
    }

    PC = fr->programPointer;
    if ( fr->parent )
      fr = fr->parent;
    else
     return queryOfFrame(fr);
  }
}


static void
markAtomsInTermReferences(PL_local_data_t *ld)
{ FliFrame   ff = ld->foreign_environment;

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
		 loffset(ch), predicateName(ch->frame->predicate));
#endif
      mark_atoms_in_environments(ld, ch->frame);
    }
  }

  unmark_stacks(ld, ld->environment, ld->choicepoints, FR_MARKED);

  assert(ld->gc._local_frames == 0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
markAtomsOnStacks()  is  called   asynchronously    (Unix)   or  between
SuspendThread()/ResumeThread() from another thread in  Windows. Its task
is to mark all atoms that  are   references  from  the Prolog stacks. It
should not make any assumptions  on   the  initialised  variables in the
stack-frames, but it is allowed to mark atoms from uninitialised data as
this causes some atoms not to  be   GC-ed  this  time (maybe better next
time).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
markAtomsOnStacks(PL_local_data_t *ld)
{ assert(!ld->gc.status.active);

  DEBUG(MSG_AGC, save_backtrace("AGC"));
#ifdef O_MAINTENANCE
  save_backtrace("AGC");
#endif
#ifdef ATOMIC_REFERENCES
  markAtom(ld->atoms.unregistering);	/* see PL_unregister_atom() */
#endif
  markAtomsOnGlobalStack(ld);
  markAtomsInEnvironments(ld);
  markAtomsInTermReferences(ld);
  markAtomsFindall(ld);
  markAtomsThreadMessageQueue(ld);
}

#endif /*O_ATOMGC*/

#ifdef O_CLAUSEGC
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is much like  check_environments(),  but   as  we  might  be called
asynchronously, we have to be a bit careful about the first frame (if PC
== NULL). The interpreter will  set  the   clause  field  to NULL before
opening the frame, and we only have   to  consider the arguments. If the
frame has a clause we must consider all variables of this clause.

This  routine  is  used   by    garbage_collect_clauses/0   as  well  as
start_consult/1. In the latter case, only  predicates in this sourcefile
are marked.

Predicates marked with P_FOREIGN_CREF are   foreign  predicates that use
the frame->clause choicepoint info for  storing the clause-reference for
the next clause. Amoung these are retract/1, clause/2, etc.

(*) we must *not* use  getProcDefinition()  here   because  we  are in a
signal handler and thus the locking there for thread-local predicates is
not safe. That is no problem however,  because we are only interested in
static predicates. Note that clause/2,  etc.   use  the choice point for
searching clauses and thus chp->cref may become NULL if all clauses have
been searched.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static QueryFrame
mark_predicates_in_environments(PL_local_data_t *ld, LocalFrame fr)
{ if ( fr == NULL )
    return NULL;

  for(;;)
  { Definition def;

    if ( true(fr, FR_MARKED_PRED) )
      return NULL;			/* from choicepoints only */
    set(fr, FR_MARKED_PRED);
    ld->gc._local_frames++;

					/* P_FOREIGN_CREF: clause, etc. choicepoints */
    if ( true(fr->predicate, P_FOREIGN_CREF) && fr->clause )
    { ClauseChoice chp = (ClauseChoice)fr->clause;
      ClauseRef cref;

      if ( chp && (cref=chp->cref) )
	def = cref->value.clause->procedure->definition; /* See (*) above */
      else
	def = NULL;
    } else
      def = fr->predicate;

    if ( def &&
	 false(def, DYNAMIC) &&
	 def->references == 0 )		/* already done */
    { if ( GD->procedures.reloading )
      { ListCell cell;			/* startConsult() */

	for(cell=GD->procedures.reloading->procedures; cell; cell=cell->next)
	{ Procedure proc = cell->value;

	  if ( proc->definition == def )
	  { DEBUG(MSG_CLAUSE_GC, Sdprintf("Marking %s\n", predicateName(def)));
	    def->references++;
	    GD->procedures.active_marked++;
	    break;
	  }
	}
      } else				/* pl_garbage_collect_clauses() */
      { if ( true(def, NEEDSCLAUSEGC) )
	{ DEBUG(MSG_CLAUSE_GC, Sdprintf("Marking %s\n", predicateName(def)));
	  def->references++;
	}
      }
    }

    if ( fr->parent )
      fr = fr->parent;
    else
      return queryOfFrame(fr);
  }
}


void
markPredicatesInEnvironments(PL_local_data_t *ld)
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
  { qf = mark_predicates_in_environments(ld, fr);
    assert(qf->magic == QID_MAGIC);

    for(; ch; ch = ch->parent)
    { mark_predicates_in_environments(ld, ch->frame);
    }
  }

  unmark_stacks(ld, ld->environment, ld->choicepoints, FR_MARKED_PRED);

  assert(ld->gc._local_frames == 0);
}


#endif /*O_CLAUSEGC*/

BeginPredDefs(gc)
#if O_DEBUG || defined(O_MAINTENANCE)
  PRED_DEF("$check_stacks", 1, check_stacks, 0)
#endif
#ifdef GC_COUNTING
  PRED_DEF("gc_statistics", 1, gc_statistics, 0)
#endif
EndPredDefs
