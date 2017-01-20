/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, University of Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-copyterm.h"
#define AC_TERM_WALK_LR 1
#include "pl-termwalk.c"


		 /*******************************
		 *	    COPY TERM		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy-term uses the GC marks to mark the state of variables and terms:

	  MARK_MASK   FIRST_MASK
	      0		  0		Virgin
	      1		  0		Visited
	      1		  1		Visited twice (share in copy)
	      0		  1		Ground (share with origin)

Places where we put marks:

	- variables		(virgin/visited/shared)
	- value of attvar	(virgin/visited/shared)
	- definition of term	(virgin/visited/shared/ground)

There   are   two   marking     algorithms:   mark_for_duplicate()   for
duplicate_term/2 that does not try to share with the original and a more
extensive mark_for_copy() that classifies terms as ground. The latter is
a bottom-up process and thus requires pushing the processed nodes on the
stack for re-visit. The algorithm is carefully   designed  to use only a
single cell on a segmented cell for each node processed. This means that
the required stack size is at most 1/2th   of the size of the term being
copied.

mark_for_duplicate() could quite easily return  the required stack-size,
avoiding stack-resizing during the actual copy.  This is much harder for
mark_for_copy() and I doubt that this   makes  much difference in actual
applications.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define VAR_MARK	((0x1<<LMASK_BITS)|TAG_VAR)
#define BOTH_MASK	(MARK_MASK|FIRST_MASK)

#define visited(w)	((w) & BOTH_MASK)
#define visited_once(w) (((w) & BOTH_MASK) == MARK_MASK)
#define virgin(w)	(!visited(w))
#define shared(w)	(((w) & BOTH_MASK) == BOTH_MASK)
#define ground(w)	(((w) & BOTH_MASK) == FIRST_MASK)
#define set_visited(w)	(w |= MARK_MASK)
#define set_shared(w)	(w |= BOTH_MASK)
#define set_ground(w)	(w &= ~MARK_MASK, w |= FIRST_MASK)

#define COPY_SHARE	0x01			/* Share ground terms */
#define COPY_ATTRS	0x02			/* do copy attributes */

static int
mark_for_duplicate(Word p, int flags ARG_LD)
{ term_agenda agenda;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  {
  again:
    switch(tag(*p))
    { case TAG_ATTVAR:
      { if ( flags & COPY_ATTRS )
	{ p = valPAttVar(*p);
	  goto again;
	}
	/*FALLTHROUGH*/
      }
      case TAG_VAR:
      { if ( virgin(*p) )
	  set_visited(*p);
        else if ( visited_once(*p) )
	  set_shared(*p);
        break;
      }
      case TAG_COMPOUND:
      { Functor t = valueTerm(*p);
	int arity = arityFunctor(t->definition);

	if ( virgin(t->definition) )
	{ set_visited(t->definition);
	} else
	{ if ( visited_once(t->definition) )
	    set_shared(t->definition);
	  break;
	}

	if ( !pushWorkAgenda(&agenda, arity, t->arguments) )
	  return MEMORY_OVERFLOW;
	continue;
      }
    }
  }
  clearTermAgenda(&agenda);

  return TRUE;
}


/* unshare_attvar() ensures that even ground attvar structures are not
   shared as ground.  We assume that the att/3 structure is not shared
   with anything else.  The shared/unshared distinction is lost during
   the ground marking.
*/

static void
unshare_attvar(Word p ARG_LD)
{ for(;;)
  { deRef(p);

    if ( isTerm(*p) )
    { Functor t = valueTerm(*p);
      word fd = (t->definition & ~BOTH_MASK);

      if ( fd == FUNCTOR_att3 )
      {	t->definition = fd | MARK_MASK;
	p = &t->arguments[2];
      }
    } else
    { break;
    }
  }
}


static int
can_share(Word p ARG_LD)
{
again:
  switch(tag(*p))
  { case TAG_VAR:
    case TAG_ATTVAR:
      return FALSE;
    case TAG_REFERENCE:
      p = unRef(*p);
      goto again;
    case TAG_COMPOUND:
    { Functor t = valueTerm(*p);
      return ground(t->definition);
    }
    default:
      return TRUE;
  }
}


static void
update_ground(Word p ARG_LD)
{ Functor t = valueTerm(*p);
  int arity = arityFunctor(t->definition);
  Word a = &t->arguments[arity];
  int ground = TRUE;

  while(--a >= t->arguments)
  { if ( !can_share(a PASS_LD) )
    { ground = FALSE;
      break;
    }
  }

  if ( ground )
    set_ground(t->definition);
}


static int
pushForMark(segstack *stack, Word p, int wr)
{ word w = ((word)p)|wr;

  return pushSegStack(stack, w, word);
}

static void
popForMark(segstack *stack, Word *pp, int *wr)
{ word w = 0;

  popSegStack(stack, &w, word);
  *wr = w & (word)0x1;
  *pp = (Word)(w & ~(word)0x1);
}


static int
mark_for_copy(Word p, int flags ARG_LD)
{ Word start = p;
  int walk_ref = FALSE;
  Word buf[1024];
  segstack stack;

  initSegStack(&stack, sizeof(Word), sizeof(buf), buf);

  for(;;)
  { switch(tag(*p))
    { case TAG_ATTVAR:
      { if ( flags & COPY_ATTRS )
	{ if ( !pushForMark(&stack, p, walk_ref) )
	  { clearSegStack(&stack);
	    return MEMORY_OVERFLOW;
	  }
	  walk_ref = TRUE;
	  p = valPAttVar(*p);
	  continue;
	}
	/*FALLTHROUGH*/
      }
      case TAG_VAR:
      { if ( virgin(*p) )
	  set_visited(*p);
        else if ( visited_once(*p) )
	  set_shared(*p);
        break;
      }
      case TAG_REFERENCE:
      { if ( !pushForMark(&stack, p, walk_ref) )
	{ clearSegStack(&stack);
	  return MEMORY_OVERFLOW;
	}
	walk_ref = TRUE;
	deRef(p);
	continue;
      }
      case TAG_COMPOUND:
      { Functor t = valueTerm(*p);
	int arity = arityFunctor(t->definition);

	if ( virgin(t->definition) )
	{ set_visited(t->definition);
	} else
	{ if ( visited_once(t->definition) )
	    set_shared(t->definition);
	  break;
	}

	if ( arity >= 1 )
	{ if ( !pushForMark(&stack, p, walk_ref) )
	  { clearSegStack(&stack);
	    return MEMORY_OVERFLOW;
	  }
	  walk_ref = FALSE;
	  p = &t->arguments[arity-1];		/* last argument */
	  continue;
	}
      }
    }

    if ( p == start )
    { clearSegStack(&stack);
      return TRUE;
    }

    while ( walk_ref )
    { popForMark(&stack, &p, &walk_ref);
      if ( isAttVar(*p) )
      { Word ap = valPAttVar(*p);

	unshare_attvar(ap PASS_LD);
      }
      if ( p == start )
      { clearSegStack(&stack);
	return TRUE;
      }
    }

    p--;
    if ( tagex(*p) == (TAG_ATOM|STG_GLOBAL) )
    { popForMark(&stack, &p, &walk_ref);
      update_ground(p PASS_LD);
    }
  }
}


		 /*******************************
		 *	      UNMARKING		*
		 *******************************/

static void
cp_unmark(Word p, int flags ARG_LD)
{ term_agenda agenda;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { again:

    switch(tag(*p))
    { case TAG_ATTVAR:
      { if ( flags & COPY_ATTRS )
	{ p = valPAttVar(*p);
	  goto again;
	}
      }
      case TAG_VAR:
      { *p &= ~BOTH_MASK;
        continue;
      }
      case TAG_COMPOUND:
      { Functor f = valueTerm(*p);

	if ( visited(f->definition) )
	{ f->definition &= ~BOTH_MASK;

	  pushWorkAgenda(&agenda, arityFunctor(f->definition), f->arguments);
	  continue;
	}
      }
    }
  }

  clearTermAgenda(&agenda);
}


static void
initCyclicCopy(ARG1_LD)
{ LD->cycle.lstack.unit_size = sizeof(Word);
}

static int
TrailCyclic(Word p ARG_LD)
{ return pushSegStack(&LD->cycle.lstack, p, Word);
}

static inline void
exitCyclicCopy(int flags ARG_LD)
{ Word p;

  while(popSegStack(&LD->cycle.lstack, &p, Word))
  { if ( isRef(*p) )
    { Word p2 = unRef(*p);

      if ( *p2 == VAR_MARK )		/* sharing variables */
      { setVar(*p2);
	setVar(*p);
      } else
      { *p = *p2 | MARK_MASK;		/* cyclic terms */
      }
    } else
    { Word old;

      popSegStack(&LD->cycle.lstack, &old, Word);

      if ( !(flags&COPY_ATTRS) )
      { Word p2 = valPAttVar(*p & ~BOTH_MASK);

	assert(*p2 == VAR_MARK);
	setVar(*p2);
      }

      *p = consPtr(old, STG_GLOBAL|TAG_ATTVAR);
    }
  }
}


static int
copy_term(Word from, Word to, int flags ARG_LD)
{ term_agendaLR agenda;
  int rc = TRUE;

  initTermAgendaLR(&agenda, 1, from, to);
  while( nextTermAgendaLR(&agenda, &from, &to) )
  {
  again:

    switch(tag(*from))
    { case TAG_REFERENCE:
      { Word p2 = unRef(*from);

	if ( *p2 == VAR_MARK )		/* reference to a copied variable */
	{ *to = makeRef(p2);
	} else
	{ from = p2;			/* normal reference */
	  goto again;
	}

	continue;
      }
      case TAG_VAR:
      { if ( shared(*from) )
	{ *to = VAR_MARK;
	  *from = makeRef(to);
	  TrailCyclic(from PASS_LD);
	} else
	{ setVar(*to);
	}

	continue;
      }
      case TAG_ATTVAR:
	if ( flags&COPY_ATTRS )
	{ Word p = valPAttVar(*from);

	  if ( isAttVar(*p) )		/* already copied */
	  { *to = makeRefG(p);
	  } else
	  { Word attr;

	    if ( !(attr = alloc_attvar(PASS_LD1)) )
	    { rc = GLOBAL_OVERFLOW;
	      goto out;
	    }
	    TrailCyclic(p PASS_LD);
	    TrailCyclic(from PASS_LD);
	    *from = consPtr(attr, STG_GLOBAL|TAG_ATTVAR);
	    *to = makeRefG(attr);

	    from = p;
	    to = &attr[1];
	    goto again;
	  }
	} else
	{ if ( shared(*from) )
	  { Word p = valPAttVar(*from & ~BOTH_MASK);

	    if ( *p == VAR_MARK )
	    { *to = makeRef(p);
	    } else
	    { *to = VAR_MARK;
	      *from = consPtr(to, STG_GLOBAL|TAG_ATTVAR)|BOTH_MASK;
	      TrailCyclic(p PASS_LD);
	      TrailCyclic(from PASS_LD);
	    }
	  } else
	  { setVar(*to);
	  }
	}
	continue;
      case TAG_COMPOUND:
      { Functor ff = valueTerm(*from);

	if ( isRef(ff->definition) )
	{ *to = consPtr(unRef(ff->definition), TAG_COMPOUND|STG_GLOBAL);
	  continue;
	}

	if ( ground(ff->definition) )
	{ *to = *from;
	  continue;
	}

	if ( shared(ff->definition) )
	{ int arity = arityFunctor(ff->definition);
	  Functor ft;

	  if ( !(ft = (Functor)allocGlobalNoShift(arity+1)) )
	  { rc = GLOBAL_OVERFLOW;
	    goto out;
	  }
	  ft->definition = ff->definition & ~BOTH_MASK;
	  ff->definition = makeRefG((Word)ft);
	  TrailCyclic(&ff->definition PASS_LD);
	  *to = consPtr(ft, TAG_COMPOUND|STG_GLOBAL);

	  if ( pushWorkAgendaLR(&agenda, arity, ff->arguments, ft->arguments) )
	    continue;
	  rc = MEMORY_OVERFLOW;
	  goto out;
	} else				/* unshared term */
	{ int arity = arityFunctor(ff->definition);
	  Functor ft;

	  if ( !(ft = (Functor)allocGlobalNoShift(arity+1)) )
	  { rc = GLOBAL_OVERFLOW;
	    goto out;
	  }
	  ft->definition = ff->definition & ~BOTH_MASK;
	  *to = consPtr(ft, TAG_COMPOUND|STG_GLOBAL);

	  if ( pushWorkAgendaLR(&agenda, arity, ff->arguments, ft->arguments) )
	    continue;
	  rc = MEMORY_OVERFLOW;
	  goto out;
	}
      }
      default:
	*to = *from;
        continue;
    }
  }

out:
  clearTermAgendaLR(&agenda);
  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Both from and to  point  to  locations   on  the  global  stack. From is
deferenced and to is a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
do_copy_term(Word from, Word to, int flags ARG_LD)
{ int rc;

again:
  switch(tag(*from))
  { case TAG_VAR:
      return TRUE;
    case TAG_REFERENCE:
      from = unRef(*from);
      goto again;
    case TAG_ATTVAR:
    case TAG_COMPOUND:
      break;
    default:
      *to = *from;
      return TRUE;
  }

  if ( flags & COPY_SHARE )
  { DEBUG(0, { mark_for_copy(from, flags PASS_LD);
	       cp_unmark(from, flags PASS_LD);
	       checkData(from);
	     });
    mark_for_copy(from, flags PASS_LD);
  } else
  { mark_for_duplicate(from, flags PASS_LD);
  }
  initCyclicCopy(PASS_LD1);
  rc = copy_term(from, to, flags PASS_LD);
  exitCyclicCopy(flags PASS_LD);
  cp_unmark(from, flags PASS_LD);
/*DEBUG(0, if ( rc == TRUE )		May lead to "Reference to higher address"
	   { checkData(from);
             checkData(to);
	   });
*/

  return rc;
}


static int
copy_term_refs(term_t from, term_t to, int flags ARG_LD)
{ for(;;)
  { fid_t fid;
    int rc;
    Word dest, src;

    if ( !(fid = PL_open_foreign_frame()) )
      return FALSE;			/* no space */

    if ( !(dest = allocGlobal(1)) )	/* make a variable on the global */
    { PL_close_foreign_frame(fid);
      return FALSE;			/* stack */
    }
    setVar(*dest);
    *valTermRef(to) = makeRef(dest);
    src = valTermRef(from);

    rc = do_copy_term(src, dest, flags PASS_LD);

    if ( rc < 0 )			/* no space for copy */
    { PL_discard_foreign_frame(fid);
      PL_put_variable(to);		/* gc consistency */
      if ( !makeMoreStackSpace(rc, ALLOW_SHIFT|ALLOW_GC) )
	return FALSE;
    } else
    { PL_close_foreign_frame(fid);
      DEBUG(CHK_SECURE,
	    { checkData(valTermRef(from));
	      checkData(valTermRef(to));
	      checkStacks(NULL);
	    });
      return TRUE;		/* if do_copy_term() == FALSE --> not-ground */
    }
  }
}


int
duplicate_term(term_t in, term_t copy ARG_LD)
{ return copy_term_refs(in, copy, COPY_ATTRS PASS_LD);
}

		 /*******************************
		 *	  FAST HEAP TERMS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below copies a  term  to   the  heap  (program space) just like
PL_record(). The representation is particularly   suited  for copying it
back to the stack really quickly: the memory can simply be copied to the
global stack, after which a quick series of relocations is performed.

This  representations  is   particularly    suited   for   re-activating
continuations as needed for tabling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define REL_END (~(unsigned int)0)

static int
needs_relocation(word w)
{ return ( isTerm(w) || isRef(w) || isIndirect(w) || isAtom(w) );
}

#if SIZEOF_VOIDP == 8
#define PTR_SHIFT (LMASK_BITS+1)
#else
#define PTR_SHIFT LMASK_BITS
#endif

static word
relocate_down(word w, size_t offset)
{ if ( isAtom(w) )
  { PL_register_atom(w);
    return w;
  } else
  { return (((w>>PTR_SHIFT)-offset)<<PTR_SHIFT) | tagex(w);
  }
}

static word
relocate_up(word w, size_t offset ARG_LD)
{ if ( isAtom(w) )
  { pushVolatileAtom(w);
    return w;
  } else
  { return (((w>>PTR_SHIFT)+offset)<<PTR_SHIFT) | tagex(w);
  }
}


fastheap_term *
term_to_fastheap(term_t t ARG_LD)
{ term_t copy = PL_new_term_ref();
  Word gcopy, gtop, p, o;
  size_t relocations=0;
  fastheap_term *fht;
  unsigned int *r;
  size_t last_rel = 0;
  size_t offset;
  size_t indirect_cells = 0;
  Word indirects;

  if ( !duplicate_term(t, copy PASS_LD) )
    return NULL;
  gcopy = valTermRef(copy);
  gtop  = gTop;
  deRef(gcopy);					/* term at gcopy .. gTop */

  for(p=gcopy; p<gtop; p++)
  { if ( needs_relocation(*p) )
    { if ( isIndirect(*p) )
      { Word ip = addressIndirect(*p);
	indirect_cells += wsizeofInd(*ip)+2;
      }
      relocations++;
    }
  }

  if ( !(fht = malloc(sizeof(fastheap_term) +
		      ((char*)gtop-(char *)gcopy) +
		      indirect_cells * sizeof(word) +
		      (relocations+1) * sizeof(unsigned int))) )
  { PL_resource_error("memory");
    return NULL;
  }

  fht->data_len    = (gtop-gcopy) + indirect_cells;
  fht->data        = addPointer(fht, sizeof(fastheap_term));
  fht->relocations = addPointer(fht->data, fht->data_len*sizeof(word));
  indirects        = fht->data + (gtop-gcopy);

  offset = gcopy-gBase;
  for(p=gcopy, o=fht->data, r=fht->relocations; p<gtop; p++)
  { if ( needs_relocation(*p) )
    { size_t this_rel = p-gcopy;

      if ( isIndirect(*p) )
      { Word ip = addressIndirect(*p);
	size_t sz = wsizeofInd(*ip)+2;
	size_t go = gBase - (Word)base_addresses[STG_GLOBAL];

	memcpy(indirects, ip, sz*sizeof(word));
	*o++ = ((go+indirects-fht->data)<<PTR_SHIFT) | tagex(*p);
	indirects += sz;
      } else
      { *o++ = relocate_down(*p, offset);
      }
      *r++ = this_rel-last_rel;
      last_rel = this_rel;
    } else
    { *o++ = *p;
    }
  }
  *r++ = REL_END;

  return fht;
}


void
free_fastheap(fastheap_term *fht)
{ unsigned int *r;
  Word p = fht->data;

  for(r = fht->relocations; *r != REL_END; r++)
  { p += *r;
    if ( isAtom(*p) )
      PL_unregister_atom(*p);
  }

  free(fht);
}


int
put_fastheap(fastheap_term *fht, term_t t ARG_LD)
{ Word p, o;
  size_t offset;
  unsigned int *r;

  if ( !hasGlobalSpace(fht->data_len) )
  { int rc;

    if ( (rc=ensureGlobalSpace(fht->data_len, ALLOW_GC|ALLOW_SHIFT)) != TRUE )
      return raiseStackOverflow(rc);
  }

  o = gTop;
  memcpy(o, fht->data, fht->data_len*sizeof(word));

  offset = o-gBase;
  for(r = fht->relocations, p=o; *r != REL_END; r++)
  { p += *r;
    *p = relocate_up(*p, offset PASS_LD);
  }

  gTop += fht->data_len;
  *valTermRef(t) = makeRefG(o);

  return TRUE;
}


		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

static
PRED_IMPL("copy_term", 2, copy_term, 0)
{ PRED_LD

  if ( PL_is_atomic(A1) )
  { return PL_unify(A1, A2);
  } else
  { term_t copy = PL_new_term_ref();

    if ( copy_term_refs(A1, copy, COPY_SHARE|COPY_ATTRS PASS_LD) )
      return PL_unify(copy, A2);

    fail;
  }
}


static
PRED_IMPL("duplicate_term", 2, duplicate_term, 0)
{ PRED_LD

  if ( PL_is_atomic(A1) )
  { return PL_unify(A1, A2);
  } else
  { term_t copy = PL_new_term_ref();

    if ( duplicate_term(A1, copy PASS_LD) )
      return PL_unify(copy, A2);

    fail;
  }
}


static
PRED_IMPL("copy_term_nat", 2, copy_term_nat, 0)
{ PRED_LD
  term_t copy = PL_new_term_ref();

  if ( copy_term_refs(A1, copy, COPY_SHARE PASS_LD) )
    return PL_unify(copy, A2);

  fail;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(copyterm)
  PRED_DEF("copy_term", 2, copy_term, PL_FA_ISO)
  PRED_DEF("duplicate_term", 2, duplicate_term, 0)
  PRED_DEF("copy_term_nat", 2, copy_term_nat, 0)
EndPredDefs
