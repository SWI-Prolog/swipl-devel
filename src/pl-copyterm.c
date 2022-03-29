/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2020, University of Amsterdam
			      CWI, Amsterdam
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
#include "pl-prims.h"
#include "pl-copyterm.h"
#include "pl-attvar.h"
#include "pl-pro.h"
#include "pl-gc.h"
#define AC_TERM_WALK_LRD 1
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
					Copy (copy_term/4)

Places where we put marks:

	- variables		(virgin/visited/shared/copy)
	- value of attvar	(virgin/visited/shared/copy)
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

For copy_term/4 we first mark all variables  of the first argument using
set_copy(). In mark_for_copy() we process these  variables and leave the
virgin variables virgin. In  the  final   copy  we  simply  share virgin
variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define VAR_MARK	((0x1<<LMASK_BITS)|TAG_VAR)
#define BOTH_MASK	(MARK_MASK|FIRST_MASK)

#define visited(w)	((w) & BOTH_MASK)
#define visited_once(w) (((w) & BOTH_MASK) == MARK_MASK)
#define virgin(w)	(!visited(w))
#define shared(w)	(((w) & BOTH_MASK) == BOTH_MASK)
#define is_ground(w)	(((w) & BOTH_MASK) == FIRST_MASK)
#define must_copy(w)	(((w) & BOTH_MASK) == FIRST_MASK)
#define set_visited(w)	(w |= MARK_MASK)
#define set_shared(w)	(w |= BOTH_MASK)
#define set_ground(w)	(w &= ~MARK_MASK, w |= FIRST_MASK)
#define set_copy(w)	(w &= ~MARK_MASK, w |= FIRST_MASK)
#define clear_marks(w)	(w &= ~BOTH_MASK)

#define COPY_SHARE	0x01			/* Share ground terms */
#define COPY_ATTRS	0x02			/* do copy attributes */
#define COPY_ABSTRACT	0x04			/* Abstract compounds */
#define COPY_MARKED	0x08			/* Only copy marked variables */
#define COPYING_ATTVAR  0x10			/* See mark_for_copy() */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mark_vars() is a helper for copy_term/4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define mark_vars(t, set) LDFUNC(mark_vars, t, set)

static int
mark_vars(DECL_LD term_t t, int set)
{ int rc = TRUE;
  term_agenda agenda;
  Word p = valTermRef(t);

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { switch(tag(*p))
    { case TAG_ATTVAR:
      case TAG_VAR:
	if ( set )
	  set_copy(*p);
        else
	  clear_marks(*p);
        break;
      case TAG_COMPOUND:
      {	if ( !visited(*p) )
	{ Functor t = valueTerm(*p);
	  int arity = arityFunctor(t->definition);

	  set_visited(*p);
	  if ( !pushWorkAgenda(&agenda, arity, t->arguments) )
	  { rc = MEMORY_OVERFLOW;
	    goto end_loop;
	  }
	}
      }
    }
  }
end_loop:
  clearTermAgenda(&agenda);

  p = valTermRef(t);
  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { switch(tag(*p))
    { case TAG_ATTVAR:
      case TAG_VAR:
	if ( rc != TRUE )
	  clear_marks(*p);
        break;
      case TAG_COMPOUND:
      { if ( visited(*p) )
	{ clear_marks(*p);

	  Functor t = valueTerm(*p);
	  int arity = arityFunctor(t->definition);
	  if ( !pushWorkAgenda(&agenda, arity, t->arguments) )
	    fatalError("No memory for cleaning\n");
	}
      }
    }
  }

  return rc;
}


#define mark_for_duplicate(p, flags) LDFUNC(mark_for_duplicate, p, flags)
static int
mark_for_duplicate(DECL_LD Word p, int flags)
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
	{ clearTermAgenda(&agenda);
	  return MEMORY_OVERFLOW;		/* out of memory */
	}
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

#define unshare_attvar(p) \
	LDFUNC(unshare_attvar, p)

static void
unshare_attvar(DECL_LD Word p)
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


#define can_share(p, flags) \
	LDFUNC(can_share, p, flags)

static int
can_share(DECL_LD Word p, int flags)
{
again:
  switch(tag(*p))
  { case TAG_VAR:
    case TAG_ATTVAR:
      if ( (flags&COPY_MARKED) &&
	   virgin(*p) &&
	   !(flags&COPYING_ATTVAR) )
	return TRUE;
      return FALSE;
    case TAG_REFERENCE:
      p = unRef(*p);
      goto again;
    case TAG_COMPOUND:
    { Functor t = valueTerm(*p);
      return is_ground(t->definition);
    }
    default:
      return TRUE;
  }
}


#define update_ground(p, flags) \
	LDFUNC(update_ground, p, flags)

static void
update_ground(DECL_LD Word p, int flags)
{ Functor t = valueTerm(*p);
  int arity = arityFunctor(t->definition);
  Word a = &t->arguments[arity];
  int ground = TRUE;

  while(--a >= t->arguments)
  { if ( !can_share(a, flags) )
    { ground = FALSE;
      break;
    }
  }

  DEBUG(MSG_COPYTERM, Sdprintf("  --> ground=%d\n", ground));

  if ( ground )
    set_ground(t->definition);
}


#define MC_MASK      0x3
#define MC_WALK_REF  0x1
#define MC_WALK_COPY 0x2

static int
pushForMark(segstack *stack, Word p, int wr)
{ word w = ((word)p)|wr;

  return pushSegStack(stack, w, word);
}

static void
popForMark(segstack *stack, Word *pp, int *wr)
{ word w = 0;

  popSegStack(stack, &w, word);
  *wr = w & (word)MC_MASK;
  *pp = (Word)(w & ~(word)MC_MASK);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We must copy an attvar if

  - COPY_ATTRS holds and we are not in copy_term/4
  - We are in copy_term/4 and
    - We are inside copying an attvar
    - The variable is marked for copying
    -
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
must_copy_attvar(word w, int flags, int mode)
{ if ( flags & (COPY_ATTRS|COPY_MARKED) )
  { if ( flags & COPY_MARKED )
    { if ( mode & MC_WALK_COPY )
	return TRUE;
      if ( must_copy(w) )
	return !!(flags & COPY_ATTRS);
      else
	return FALSE;
    } else
    { return TRUE;
    }
  } else
  { return FALSE;
  }
}


#define mark_for_copy(p, flags) \
	LDFUNC(mark_for_copy, p, flags)

static int
mark_for_copy(DECL_LD Word p, int flags)
{ Word start = p;
  int mode = 0;
  Word buf[1024];
  segstack stack;

  initSegStack(&stack, sizeof(Word), sizeof(buf), buf);

#define COPY_ALWAYS (!(flags&COPY_MARKED) || (mode&MC_WALK_COPY))

  for(;;)
  { switch(tag(*p))
    { case TAG_ATTVAR:
      { if ( must_copy_attvar(*p, flags, mode) )
	{ if ( !pushForMark(&stack, p, mode) )
	  { clearSegStack(&stack);
	    return MEMORY_OVERFLOW;
	  }
	  mode |= (MC_WALK_REF|MC_WALK_COPY);
	  p = valPAttVar(*p & ~BOTH_MASK);
	  continue;
	}
	/*FALLTHROUGH*/
      }
      case TAG_VAR:
      { if ( virgin(*p) && COPY_ALWAYS )
	  set_visited(*p);
        else if ( visited_once(*p) )
	  set_shared(*p);
	else if ( must_copy(*p) )
	{ clear_marks(*p);
	  set_visited(*p);
	}
        break;
      }
      case TAG_REFERENCE:
      { if ( !pushForMark(&stack, p, mode) )
	{ clearSegStack(&stack);
	  return FALSE;
	}
	mode |= MC_WALK_REF;
	deRef(p);
	continue;
      }
      case TAG_COMPOUND:
      { Functor t = valueTerm(*p);
	int arity = arityFunctor(t->definition);

	if ( virgin(t->definition) )
	{ DEBUG(MSG_COPYTERM,
		Sdprintf("Visit %s at %s\n",
			 functorName(t->definition),
			 print_addr(&t->definition, NULL)));
	  set_visited(t->definition);
	} else
	{ if ( visited_once(t->definition) )
	    set_shared(t->definition);
	  break;
	}

	if ( arity >= 1 )
	{ if ( !pushForMark(&stack, p, mode) )
	  { clearSegStack(&stack);
	    return FALSE;
	  }
	  mode &= ~MC_WALK_REF;
	  p = &t->arguments[arity-1];		/* last argument */
	  continue;
	}
      }
    }

    if ( p == start )
    { clearSegStack(&stack);
      return TRUE;
    }

    while ( mode&MC_WALK_REF )
    { popForMark(&stack, &p, &mode);
      if ( isAttVar(*p) )
      { Word ap = valPAttVar(*p & ~BOTH_MASK);

	unshare_attvar(ap);
      }
      if ( p == start )
      { clearSegStack(&stack);
	return TRUE;
      }
    }

    p--;
    if ( tagex(*p) == (TAG_ATOM|STG_GLOBAL) )	/* functor (compound) */
    { int f = flags;

      DEBUG(MSG_COPYTERM,
	    Sdprintf("Functor %s; mode=0x%x\n",
		     functorName(*p & ~BOTH_MASK), mode));

      if ( mode&MC_WALK_COPY )
	f |= COPYING_ATTVAR;
      popForMark(&stack, &p, &mode);
      update_ground(p, f);
    }
  }

#undef COPY_ALWAYS
}


		 /*******************************
		 *	      UNMARKING		*
		 *******************************/

#define cp_unmark(p, flags) LDFUNC(cp_unmark, p, flags)
static void
cp_unmark(DECL_LD Word p, int flags)
{ term_agenda agenda;

  initTermAgenda(&agenda, 1, p);
  while((p=nextTermAgenda(&agenda)))
  { again:

    switch(tag(*p))
    { case TAG_ATTVAR:
      { if ( flags & COPY_MARKED )
	  *p &= ~BOTH_MASK;
	if ( flags & COPY_ATTRS )
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

	  DEBUG(MSG_COPYTERM,
		Sdprintf("Unmarking %s at %s\n",
			 functorName(f->definition),
			 print_addr(&f->definition, NULL)));

	  if ( !pushWorkAgenda(&agenda,
			       arityFunctor(f->definition), f->arguments) )
	    fatalError("Failed to revert copy markers");
	  continue;
	}
      }
    }
  }

  clearTermAgenda(&agenda);
}


#define initCyclicCopy(_) LDFUNC(initCyclicCopy, _)
static void
initCyclicCopy(DECL_LD)
{ LD->cycle.lstack.unit_size = sizeof(Word);
}

#define TrailCyclic(p) LDFUNC(TrailCyclic, p)
static int
TrailCyclic(DECL_LD Word p)
{ return pushSegStack(&LD->cycle.lstack, p, Word);
}

#define exitCyclicCopy(flags) LDFUNC(exitCyclicCopy, flags)
static inline void
exitCyclicCopy(DECL_LD int flags)
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
    { Word old = NULL;			/* Silence compiler */

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


#define copy_term(from, to, abstract, flags) \
	LDFUNC(copy_term, from, to, abstract, flags)

static int
copy_term(DECL_LD Word from, Word to, size_t abstract, int flags)
{ term_agendaLRD agenda;
  int rc = TRUE;
  size_t aleft = (size_t)-1;

  initTermAgendaLRD(&agenda, 1, from, to);
  while( nextTermAgendaLRD(&agenda, &from, &to) )
  { if ( agenda.work.depth == 1 )
      aleft = abstract;

  again:
    switch(tag(*from))
    { case TAG_REFERENCE:
      { Word p2 = unRef(*from);

	if ( *p2 == VAR_MARK )		/* reference to a copied variable */
	{ *to = makeRefG(p2);
	} else
	{ from = p2;			/* normal reference */
	  goto again;
	}

	continue;
      }
      case TAG_VAR:
      { if ( shared(*from) )
	{ *to = VAR_MARK;
	  *from = makeRefG(to);
	  TrailCyclic(from);
	} else if ( (flags&COPY_ABSTRACT) )
	{ *to = makeRefG(from);
	} else if ( virgin(*from) )	/* copy_term/4 */
	{ *to = makeRefG(from);
	} else
	{ setVar(*to);
	}

	continue;
      }
      case TAG_ATTVAR:
	if ( flags&COPY_ATTRS )
	{ Word p = valPAttVar(*from & ~BOTH_MASK);

	  if ( isAttVar(*p) )		/* already copied */
	  { *to = makeRefG(p);
	  } else if ( !(flags&COPY_MARKED) || must_copy(*from) )
	  { Word attr;

	    if ( !(attr = alloc_attvar()) )
	    { rc = GLOBAL_OVERFLOW;
	      goto out;
	    }
	    TrailCyclic(p);
	    TrailCyclic(from);
	    *from = consPtr(attr, STG_GLOBAL|TAG_ATTVAR);
	    *to = makeRefG(attr);

	    from = p;
	    to = &attr[1];
	    goto again;
	  } else
	  { *to = makeRefG(from);	/* copy_term/4 */
	  }
	} else
	{ if ( shared(*from) )
	  { Word p = valPAttVar(*from & ~BOTH_MASK);

	    if ( *p == VAR_MARK )
	    { *to = makeRefG(p);
	    } else
	    { *to = VAR_MARK;
	      *from = consPtr(to, STG_GLOBAL|TAG_ATTVAR)|BOTH_MASK;
	      TrailCyclic(p);
	      TrailCyclic(from);
	    }
	  } else if ( flags & COPY_MARKED )
	  { *to = makeRefG(from);
	  } else
	  { setVar(*to);
	  }
	}
	continue;
      case TAG_COMPOUND:
      { Functor ff = valueTerm(*from);

	if ( aleft == 0 )
	{ setVar(*to);
	  continue;
	} else
	{ if ( aleft != (size_t)-1 )
	    aleft--;
	}

	if ( isRef(ff->definition) )
	{ *to = consPtr(unRef(ff->definition), TAG_COMPOUND|STG_GLOBAL);
	  continue;
	}

	if ( is_ground(ff->definition) )
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
	  TrailCyclic(&ff->definition);
	  *to = consPtr(ft, TAG_COMPOUND|STG_GLOBAL);

	  if ( pushWorkAgendaLRD(&agenda, arity, ff->arguments, ft->arguments) )
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

	  if ( pushWorkAgendaLRD(&agenda, arity, ff->arguments, ft->arguments) )
	    continue;
	  rc = MEMORY_OVERFLOW;
	  goto out;
	}
      }
      case TAG_ATOM:
	pushVolatileAtom(*from);
        /*FALLTHROUGH*/
      default:
	*to = *from;
        continue;
    }
  }

out:
  clearTermAgendaLRD(&agenda);
  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Both from and to  point  to  locations   on  the  global  stack. From is
deferenced and to is a variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define do_copy_term(from, to, abstract, flags) \
	LDFUNC(do_copy_term, from, to, abstract, flags)

static int
do_copy_term(DECL_LD Word from, Word to, int abstract, int flags)
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
    case TAG_ATOM:
      pushVolatileAtom(*from);
      /*FALLTHROUGH*/
    default:
      *to = *from;
      return TRUE;
  }

  if ( (flags&COPY_SHARE) )
  { DEBUG(CHK_SECURE, { mark_for_copy(from, flags);
			cp_unmark(from, flags);
			checkData(from);
		      });
    if ( (rc=mark_for_copy(from, flags)) != TRUE )
    { cp_unmark(from, flags);
      return rc;
    }
  } else if ( !(flags&COPY_ABSTRACT) )
  { if ( (rc=mark_for_duplicate(from, flags)) != TRUE )
    { cp_unmark(from, flags);
      return rc;
    }
  }
  initCyclicCopy();
  rc = copy_term(from, to, abstract, flags);
  exitCyclicCopy(flags);
  cp_unmark(from, flags);
  DEBUG(0,
	if ( rc == TRUE )	     // May lead to "Reference to higher address"
	{ checkData(from);
	  checkData(to);
	});

  return rc;
}


#define copy_term_refs(from, to, vars, abstract, flags) \
	LDFUNC(copy_term_refs, from, to, vars, abstract, flags)

static int
copy_term_refs(DECL_LD term_t from, term_t to, term_t vars,
	       size_t abstract, int flags)
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
    *valTermRef(to) = makeRefG(dest);
    src = valTermRef(from);

    if ( vars )
    { if ( mark_vars(vars, TRUE) != TRUE )
	return PL_no_memory();
    }
    rc = do_copy_term(src, dest, abstract, flags);

    if ( rc < 0 )			/* no space for copy */
    { PL_discard_foreign_frame(fid);
      PL_put_variable(to);		/* gc consistency */
      if ( vars )
      { if ( mark_vars(vars, FALSE) != TRUE )
	  return PL_no_memory();
      }
      if ( !makeMoreStackSpace(rc, ALLOW_SHIFT|ALLOW_GC) )
	return FALSE;
      DEBUG(CHK_SECURE, checkStacks(NULL));
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
duplicate_term(DECL_LD term_t in, term_t copy)
{ return copy_term_refs(in, copy, 0, (size_t)-1, COPY_ATTRS);
}


int
size_abstract_term(DECL_LD term_t in, term_t copy, size_t abstract)
{ return copy_term_refs(in, copy, 0, abstract, COPY_ATTRS|COPY_ABSTRACT);
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

#define relocate_up(w, offset) LDFUNC(relocate_up, w, offset)
static word
relocate_up(DECL_LD word w, size_t offset)
{ if ( isAtom(w) )
  { pushVolatileAtom(w);
    return w;
  } else
  { return (((w>>PTR_SHIFT)+offset)<<PTR_SHIFT) | tagex(w);
  }
}


fastheap_term *
term_to_fastheap(DECL_LD term_t t)
{ term_t copy = PL_new_term_ref();
  Word gcopy, gtop, p, o;
  size_t relocations=0;
  fastheap_term *fht;
  unsigned int *r;
  size_t last_rel = 0;
  size_t offset;
  size_t indirect_cells = 0;
  Word indirects;

  if ( !duplicate_term(t, copy) )
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
put_fastheap(DECL_LD fastheap_term *fht, term_t t)
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
    *p = relocate_up(*p, offset);
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

    if ( copy_term_refs(A1, copy, 0, (size_t)-1, COPY_SHARE|COPY_ATTRS) )
      return PL_unify(copy, A2);

    fail;
  }
}


#define copy_term_4(vs0, t0, vs, t, flags) \
	LDFUNC(copy_term_4, vs0, t0, vs, t, flags)

static int
copy_term_4(DECL_LD term_t vs0, term_t t0, term_t vs, term_t t, int flags)
{ term_t term, copy;

  return ( (term = PL_new_term_ref()) &&
	   (copy = PL_new_term_ref()) &&
	   PL_cons_functor(term, FUNCTOR_minus2, vs0, t0) &&
	   copy_term_refs(term, copy, vs0, (size_t)-1,
			  COPY_SHARE|COPY_MARKED|flags) &&
	   PL_get_arg(1, copy, term) &&
	   PL_unify(vs, term) &&
	   PL_get_arg(2, copy, term) &&
	   PL_unify(t, term) );
}

static
PRED_IMPL("copy_term", 4, copy_term, 0)
{ return copy_term_4(A1, A2, A3, A4, COPY_ATTRS);
}

static
PRED_IMPL("copy_term_nat", 4, copy_term_nat, 0)
{ return copy_term_4(A1, A2, A3, A4, 0);
}


static
PRED_IMPL("duplicate_term", 2, duplicate_term, 0)
{ PRED_LD

  if ( PL_is_atomic(A1) )
  { return PL_unify(A1, A2);
  } else
  { term_t copy = PL_new_term_ref();

    if ( duplicate_term(A1, copy) )
      return PL_unify(copy, A2);

    fail;
  }
}


static
PRED_IMPL("copy_term_nat", 2, copy_term_nat, 0)
{ PRED_LD
  term_t copy = PL_new_term_ref();

  if ( copy_term_refs(A1, copy, 0, (size_t)-1, COPY_SHARE) )
    return PL_unify(copy, A2);

  fail;
}


static
PRED_IMPL("size_abstract_term", 3, size_abstract_term, 0)
{ PRED_LD
  size_t abstract;

  if ( PL_get_size_ex(A1, &abstract) )
  { term_t copy = PL_new_term_ref();

    if ( size_abstract_term(A2, copy, abstract) )
      return PL_unify(copy, A3);
  }

  return FALSE;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(copyterm)
  PRED_DEF("copy_term",          2, copy_term,          PL_FA_ISO)
  PRED_DEF("copy_term",          4, copy_term,          0)
  PRED_DEF("duplicate_term",     2, duplicate_term,     0)
  PRED_DEF("copy_term_nat",      2, copy_term_nat,      0)
  PRED_DEF("copy_term_nat",      4, copy_term_nat,      0)
  PRED_DEF("size_abstract_term", 3, size_abstract_term, 0)
EndPredDefs
