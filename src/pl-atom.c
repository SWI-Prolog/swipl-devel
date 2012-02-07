/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "os/pl-ctype.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation issues
---------------------

There are two parts in the atom   administration. One is a dynamic array
(called buffer) atom_array, which is there to   find  the atoms back. An
atom as it appears is a intptr_t   of the form (n<<LMASK_BITS)|TAG_ATOM. The
atom  structure  is  located  by  getting  the  n-th  pointer  from  the
atom_array dynamic array.  See atomValue() for translating the intptr_t into
the address of the structure.

Next, there is a hash-table, which is a normal `open' hash-table mapping
char * to the atom structure. This   thing is dynamically rehashed. This
table is used by lookupAtom() below.

Atom garbage collection
-----------------------

There is no such thing, but below is an outline of what in entails.

There are various categories of atoms:

	# Built-in atoms
	These are used directly in the C-source of the system and cannot
	be removed. These are the atoms upto a certain number. This
	number is sizeof(atoms)/sizeof(char *).

	# Foreign referenced atoms
	These are references hold in foreign code by means of
	PL_new_atom() or other calls returning an atom. The system has
	no way to determine the lifetime of them. Most probably the best
	approach is to offer a locking/unlocking flag to deal this type
	of atoms. The lock/unlock may be nested. The proposed functions
	are:

		PL_register_atom(atom_t atom)
		PL_unregister_atom(atom_t atom)

	# References from the Prolog stacks
	Reference counting is unacceptable here, which implies a pass
	similar to the normal garbage-collector is required to deal with
	them. It might be worthwhile to include the atom
	garbage-collection in the normal garbage collector.

	# References from other structures
	Various of the structures contain or may contain atom
	references.  There are two options: lock/unlock them using
	PL_register_atom() on creation/destruction of the structure
	or enumerate them and flag the atoms.  The choice depends a
	bit on the structure.  FunctorDef for example is not garbage
	collected itself, so simply locking it makes sence.

	# References from compiled code and records
	Again both aproaches are feasible.  Reference counting is
	easy, except that the destruction of clauses and records
	will be more slowly as the code needs to be analysed for
	atom-references.

Reclaiming
----------

To reclaim an atom, it needs to be   deleted from the hash-table, a NULL
pointer should be set in the dynamic array and the structure needs to be
disposed using unalloc(). Basically, this is not a hard job.

The dynamic array will get holes   and  registerAtom() should first spot
for a hole (probably using a globally   defined index that points to the
first location that might be a hole   to avoid repetive scanning for the
array while there is no place anyway).   It cannot be shrunk, unless all
atoms above a certain index are gone. There is simply no way to find all
atom references (that why we need the PL_register_atom()).

In an advanced form, the hash-table could be shrunk, but it is debatable
whether this is worth the trouble. So, alltogether the system will waist
an average 1.5 machine word per reclaimed  atom, which will be reused as
the atom-space grows again.


Status
------

Some prelimary parts of atom garbage   collection  have been implemented
and flagged using #ifdef O_ATOMGC  ...   #endif.  The foreign code hooks
have been defined as well.


Atom GC and multi-threading
---------------------------

This is a hard problem. I think the   best  solution is to add something
like PL_thread_signal_async(int tid, void (*f)(void)) and call this from
the invoking thread on all other threads.   These  thread will then scan
their stacks and mark any references from their. Next they can carry on,
as intptr_t as the invoking thread keeps   the  atom mutex locked during the
whole atom garbage collection process. This   implies  the thread cannot
create any atoms as intptr_t as the collection is going on.

We do have to define some mechanism to   know  all threads are done with
their marking.

Don't know yet about Windows.  They   can't  do anything asynchronously.
Maybe they have ways to ensure  all   other  threads  are sleeping for a
while, so we can control the whole  process from the invoking thread. If
this is the case we could also do this in Unix:

	thread_kill(<thread>, SIG_STOP);
	<mark from thread>;
	thread_kill(<thread>, SIG_CONT);

Might be wise  to  design  the  marking   routine  suitable  to  take  a
PL_local_data term as argument, so it can be called from any thread.

All this will only work if we can call the atom garbage synchronously!!!

Measures to allow for asynchronous atom GC
------------------------------------------

	* lookupAtom() returns a referenced atom
	If not, it can be collected right-away!  Actually this might be
	a good idea anyway to avoid foreign-code that caches atoms from
	having to be updated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	rehashAtoms();

#define atom_buckets GD->atoms.buckets
#define atomTable    GD->atoms.table

#if O_DEBUG
#define lookups GD->atoms.lookups
#define	cmps	GD->atoms.cmps
#endif

#define LOCK()   PL_LOCK(L_ATOM)
#define UNLOCK() PL_UNLOCK(L_ATOM)
#undef LD
#define LD LOCAL_LD

		 /*******************************
		 *	      TYPES		*
		 *******************************/

static PL_blob_t text_atom =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE|PL_BLOB_TEXT,		/* unique representation of text */
  "text"
};


static PL_blob_t unregistered_blob_atom =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY|PL_BLOB_TEXT,
  "unregistered"
};


void
PL_register_blob_type(PL_blob_t *type)
{ PL_LOCK(L_MISC);			/* cannot use L_ATOM */

  if ( !type->registered )
  { if ( !GD->atoms.types )
    { GD->atoms.types = type;
      type->atom_name = ATOM_text;	/* avoid deadlock */
      type->registered = TRUE;
    } else
    { PL_blob_t *t = GD->atoms.types;

      while(t->next)
	t = t->next;

      t->next = type;
      type->rank = t->rank+1;
      type->registered = TRUE;
      type->atom_name = PL_new_atom(type->name);
    }

  }

  PL_UNLOCK(L_MISC);
}


PL_blob_t *
PL_find_blob_type(const char *name)
{ PL_blob_t *t;

  PL_LOCK(L_MISC);
  for(t = GD->atoms.types; t; t = t->next)
  { if ( streq(name, t->name) )
      break;
  }
  PL_UNLOCK(L_MISC);

  return t;
}



int
PL_unregister_blob_type(PL_blob_t *type)
{ size_t index;
  int i, last=FALSE;
  PL_blob_t **t;
  int discarded = 0;

  PL_LOCK(L_MISC);
  for(t = &GD->atoms.types; *t; t = &(*t)->next)
  { if ( *t == type )
    { *t = type->next;
      type->next = NULL;
    }
  }
  PL_UNLOCK(L_MISC);

  PL_register_blob_type(&unregistered_blob_atom);

  LOCK();
  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom atom = b[index];

      if ( atom && atom->type == type )
      { atom->type = &unregistered_blob_atom;

	atom->name = "<discarded blob>";
	atom->length = strlen(atom->name);

	discarded++;
      }
    }
  }
  UNLOCK();

  return discarded == 0 ? TRUE : FALSE;
}


		 /*******************************
		 *      BUILT-IN ATOM TABLE	*
		 *******************************/

#define ATOM(s) s

typedef const char * ccharp;
static const ccharp atoms[] = {
#include "pl-atom.ic"
  ATOM((char *)NULL)
};
#undef ATOM

/* Note that we use PL_malloc_uncollectable() here because the pointer in
   our block is not the real memory pointer.  Probably it is better to
   have two pointers; one to the allocated memory and one with the
   necessary offset.
*/

static void
putAtomArray(size_t where, Atom a)
{ int idx = MSB(where);

  assert(where >= 0);

  if ( !GD->atoms.array.blocks[idx] )
  { PL_LOCK(L_MISC);
    if ( !GD->atoms.array.blocks[idx] )
    { size_t bs = (size_t)1<<idx;
      Atom *newblock = PL_malloc_uncollectable(bs*sizeof(Atom));

      GD->atoms.array.blocks[idx] = newblock-bs;
    }
    PL_UNLOCK(L_MISC);
  }

  GD->atoms.array.blocks[idx][where] = a;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
It might be wise to  provide  for   an  option  that does not reallocate
atoms. In that case accessing a GC'ed   atom  causes a crash rather then
another atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerAtom(Atom a)
{ size_t index;
#ifdef O_ATOMGC				/* try to find a hole! */
  int i;
  int last = FALSE;

  for(index=GD->atoms.no_hole_before, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { if ( b[index] == NULL )
      { a->atom = (index<<LMASK_BITS)|TAG_ATOM;
	b[index] = a;
	GD->atoms.no_hole_before = index+1;

	return;
      }
    }
  }
  GD->atoms.no_hole_before = index+1;
#else
  index = GD->atoms.highest;
#endif /*O_ATOMGC*/

  a->atom = (index<<LMASK_BITS)|TAG_ATOM;
  if ( indexAtom(a->atom) != index )	/* TBD: user-level exception */
    fatalError("Too many (%d) atoms", index);
  putAtomArray(index, a);
  GD->atoms.highest = index+1;
}


static size_t
paddingBlob(PL_blob_t *type)
{ if ( true(type, PL_BLOB_TEXT) )
  { return true(type, PL_BLOB_WCHAR) ? sizeof(pl_wchar_t) : sizeof(char);
  } else
  { return 0;
  }
}


		 /*******************************
		 *	  GENERAL LOOKUP	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) AGC starting. As we cannot run AGC if   we  are not in a safe state,
AGC is started using a   software interrupt using PL_raise(SIG_ATOM_GC).
Earlier versions only fired the signal   at exactly (last+margin) atoms,
but it is possible the signal is not  handled due to the thread dying or
the thread starting an indefinite  wait.   Therefore  we keep signalling
every 128 new atoms. Sooner or later   some  actually active thread will
pick up the request and process it.

PL_handle_signals() decides on the actual invocation of atom-gc and will
treat the signal as bogus if agc has already been performed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
lookupBlob(const char *s, size_t length, PL_blob_t *type, int *new)
{ unsigned int v0, v;
  Atom a;

  if ( !type->registered )		/* avoid deadlock */
    PL_register_blob_type(type);
  v0 = MurmurHashAligned2(s, length, MURMUR_SEED);

  LOCK();
  v  = v0 & (atom_buckets-1);
  DEBUG(MSG_HASH_STAT, lookups++);

  if ( true(type, PL_BLOB_UNIQUE) )
  { if ( false(type, PL_BLOB_NOCOPY) )
    { for(a = atomTable[v]; a; a = a->next)
      { DEBUG(MSG_HASH_STAT, cmps++);
	if ( length == a->length &&
	     type == a->type &&
	     memcmp(s, a->name, length) == 0 )
	{
#ifdef O_ATOMGC
	  if ( indexAtom(a->atom) >= GD->atoms.builtin )
	  { if ( a->references++ == 0 )
	      GD->atoms.unregistered--;
	  }
#endif
          UNLOCK();
	  *new = FALSE;
	  return a->atom;
	}
      }
    } else
    { for(a = atomTable[v]; a; a = a->next)
      { DEBUG(MSG_HASH_STAT, cmps++);

	if ( length == a->length &&
	     type == a->type &&
	     s == a->name )
	{
#ifdef O_ATOMGC
	  if ( a->references++ == 0 )
	    GD->atoms.unregistered--;
#endif
          UNLOCK();
	  *new = FALSE;
	  return a->atom;
	}
      }
    }
  }

  a = allocHeapOrHalt(sizeof(struct atom));
  a->length = length;
  a->type = type;
  if ( false(type, PL_BLOB_NOCOPY) )
  { if ( true(type, PL_BLOB_TEXT) )
    { size_t pad = paddingBlob(type);

      a->name = PL_malloc_atomic(length+pad);
      memcpy(a->name, s, length);
      memset(a->name+length, 0, pad);
      GD->statistics.atom_string_space += length+pad;
    } else
    { a->name = PL_malloc(length);
      memcpy(a->name, s, length);
      GD->statistics.atom_string_space += length;
    }
  } else
  { a->name = (char *)s;
  }
#ifdef O_TERMHASH
  a->hash_value = v0;
#endif
#ifdef O_ATOMGC
  a->references = 1;
#endif
  registerAtom(a);
  if ( true(type, PL_BLOB_UNIQUE) )
  { a->next       = atomTable[v];
    atomTable[v]  = a;
  }
  GD->statistics.atoms++;

#ifdef O_ATOMGC
  if ( GD->atoms.margin != 0 &&
       GD->atoms.unregistered >= GD->atoms.non_garbage + GD->atoms.margin )
  { intptr_t x = GD->atoms.unregistered - (GD->atoms.non_garbage + GD->atoms.margin);

    if ( x % 128 == 0 )			/* see (*) above */
      PL_raise(SIG_ATOM_GC);
  }
#endif

  if ( atom_buckets * 2 < GD->statistics.atoms )
    rehashAtoms();

  UNLOCK();

  *new = TRUE;
  if ( type->acquire )
    (*type->acquire)(a->atom);

  return a->atom;
}


word
lookupAtom(const char *s, size_t length)
{ int new;

  return lookupBlob(s, length, &text_atom, &new);
}


		 /*******************************
		 *	      ATOM-GC		*
		 *******************************/

#ifdef O_ATOMGC

#ifdef O_DEBUG_ATOMGC
static char *tracking;
IOSTREAM *atomLogFd;

void
_PL_debug_register_atom(atom_t a,
			const char *file, int line, const char *func)
{ int i = indexAtom(a);
  int mx = entriesBuffer(&atom_array, Atom);
  Atom atom;

  assert(i>=0 && i<mx);
  atom = fetchBuffer(&atom_array, i, Atom);

  atom->references++;
  if ( atomLogFd && strprefix(atom->name, tracking) )
    Sfprintf(atomLogFd, "%s:%d: %s(): ++ (%d) for `%s' (#%d)\n",
	     file, line, func, atom->references, atom->name, i);
}


void
_PL_debug_unregister_atom(atom_t a,
			  const char *file, int line, const char *func)
{ int i = indexAtom(a);
  int mx = entriesBuffer(&atom_array, Atom);
  Atom atom;

  assert(i>=0 && i<mx);
  atom = fetchBuffer(&atom_array, i, Atom);

  assert(atom->references >= 1);
  atom->references--;
  if ( atomLogFd && strprefix(atom->name, tracking) )
    Sfprintf(atomLogFd, "%s:%d: %s(): -- (%d) for `%s' (#%d)\n",
	     file, line, func, atom->references, atom->name, i);
}


Atom
_PL_debug_atom_value(atom_t a)
{ GET_LD
  int i = indexAtom(a);
  Atom atom = fetchBuffer(&atom_array, i, Atom);

  if ( !atom )
  { char buf[32];

    Sdprintf("*** No atom at index (#%d) ***", i);
    trap_gdb();

    atom = allocHeapOrHalt(sizeof(*atom));
    Ssprintf(buf, "***(#%d)***", i);
    atom->name = store_string(buf);
    atom->length = strlen(atom->name);
  }

  return atom;
}


word
pl_track_atom(term_t which, term_t stream)
{ char *s;

  if ( tracking )
    remove_string(tracking);
  tracking = NULL;
  atomLogFd = NULL;

  if ( PL_get_nil(stream) )
    succeed;

  if ( !PL_get_chars(which, &s, CVT_LIST) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, which);
  if ( !PL_get_stream_handle(stream, &atomLogFd) )
    fail;

  tracking = store_string(s);

  succeed;
}
#endif /*O_DEBUG_ATOMGC*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
lockAtoms() discards all currently defined atoms for garbage collection.
To be used after loading the program,   so we won't traverse the program
atoms each pass.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
lockAtoms(void)
{ GD->atoms.builtin      = GD->atoms.highest;
  GD->atoms.unregistered = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark an atom from the stacks.  We must be prepared to handle fake-atoms!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
markAtom(atom_t a)
{ size_t i = indexAtom(a);
  Atom ap;

  if ( i >= GD->atoms.highest )
    return;				/* not an atom */
  if ( i < GD->atoms.builtin )
    return;				/* locked range */

  ap = fetchAtomArray(i);

  if ( ap )
  {
#ifdef O_DEBUG_ATOMGC
    if ( atomLogFd )
      Sfprintf(atomLogFd, "Marked `%s' at (#%d)\n", ap->name, i);
#endif
    ap->references |= ATOM_MARKED_REFERENCE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destroyAtom()  actually  discards  an  atom.  The  code  marked  (*)  is
sometimes inserted to debug atom-gc. The   trick  is to create xxxx<...>
atoms that should *not* be subject to AGC.   If we find one collected we
know we trapped a bug.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
destroyAtom(Atom *ap, uintptr_t mask)
{ Atom a = *ap;
  Atom *ap2 = &atomTable[a->hash_value & mask];

  if ( a->type->release )
  { if ( !(*a->type->release)(a->atom) )
      return FALSE;
  } else if ( GD->atoms.gc_hook )
  { if ( !(*GD->atoms.gc_hook)(a->atom) )
      return FALSE;				/* foreign hooks says `no' */
  }

#if 0
  if ( strncmp(a->name, "xxxx", 4) == 0 )	/* (*) see above */
  { Sdprintf("Deleting %s\n", a->name);
    assert(0);
  }
#endif

#ifdef O_DEBUG_ATOMGC
  if ( atomLogFd )
    Sfprintf(atomLogFd, "Deleted `%s'\n", a->name);
#endif

  if ( true(a->type, PL_BLOB_UNIQUE) )
  { for( ; ; ap2 = &(*ap2)->next )
    { assert(*ap2);		/* MT: TBD: failed a few times!? */

      if ( *ap2 == a )
      { *ap2 = a->next;
        break;
      }
    }
  }

  *ap = NULL;			/* delete from index array */
  if ( false(a->type, PL_BLOB_NOCOPY) )
  { size_t slen = a->length+paddingBlob(a->type);
    GD->statistics.atom_string_space -= slen;
    GD->statistics.atom_string_space_freed += slen;
    PL_free(a->name);
  }
  freeHeap(a, sizeof(*a));

  return TRUE;
}


static size_t
collectAtoms(void)
{ int hole_seen = FALSE;
  size_t reclaimed = 0;
  size_t index;
  int i, last=FALSE;

  for(index=GD->atoms.builtin, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b[index];

      if ( !a )
      { if ( !hole_seen )
	{ hole_seen = TRUE;
	  GD->atoms.no_hole_before = index;
	}
	continue;
      }

      if ( a->references == 0 )
      { if ( destroyAtom(&b[index], atom_buckets-1) )
	{ reclaimed++;
	  if ( !hole_seen )
	  { hole_seen = TRUE;
	    GD->atoms.no_hole_before = index;
	  }
	}
      } else
      { a->references &= ~ATOM_MARKED_REFERENCE;
      }
    }
  }

  return reclaimed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_garbage_collect_atoms() realised the atom   garbage  collector (AGC).
This is a tricky beast that   needs  careful synchronisation with normal
GC. These issues are described with enterGC() in pl-gc.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_garbage_collect_atoms()
{ GET_LD
  int64_t oldcollected;
  int verbose;
  double t;
  sigset_t set;
  size_t reclaimed;

  PL_LOCK(L_GC);
  if ( gc_status.blocked )		/* Tricky things; avoid problems. */
  { PL_UNLOCK(L_GC);
    succeed;
  }

#ifdef O_PLMT
  if ( GD->gc.active )			/* GC in progress: delay */
  { DEBUG(MSG_AGC, Sdprintf("GC active; delaying AGC\n"));
    GD->gc.agc_waiting = TRUE;
    PL_UNLOCK(L_GC);
    succeed;
  }
#endif

  gc_status.blocked++;			/* avoid recursion */

  if ( (verbose = truePrologFlag(PLFLAG_TRACE_GC)) )
  {
#ifdef O_DEBUG_ATOMGC
/*
    access_level_t old;
    Sdprintf("Starting ATOM-GC.  Stack:\n");
    old = setAccessLevel(ACCESS_LEVEL_SYSTEM);
    backTrace(5);
    setAccessLevel(old);
*/
#endif
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "agc", 1,
		   PL_CHARS, "start");
  }

  PL_LOCK(L_THREAD);
  PL_LOCK(L_AGC);
  LOCK();
  blockSignals(&set);
  t = CpuTime(CPU_USER);
  markAtomsOnStacks(LD);
#ifdef O_PLMT
  markAtomsThreads();
  forThreadLocalData(markAtomsOnStacks, 0);
#endif
  oldcollected = GD->atoms.collected;
  reclaimed = collectAtoms();
  GD->atoms.collected += reclaimed;
  GD->statistics.atoms -= reclaimed;
  GD->atoms.unregistered -= reclaimed;
  GD->atoms.non_garbage = GD->atoms.unregistered;
  t = CpuTime(CPU_USER) - t;
  GD->atoms.gc_time += t;
  GD->atoms.gc++;
  unblockSignals(&set);
  UNLOCK();
  PL_UNLOCK(L_AGC);
  PL_UNLOCK(L_THREAD);
  gc_status.blocked--;
  PL_UNLOCK(L_GC);

  if ( verbose )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "agc", 1,
		   PL_FUNCTOR_CHARS, "done", 3,
		     PL_INT64, GD->atoms.collected - oldcollected,
		     PL_INT, GD->statistics.atoms,
		     PL_DOUBLE, (double)t);

  succeed;
}


PL_agc_hook_t
PL_agc_hook(PL_agc_hook_t new)
{ PL_agc_hook_t old = GD->atoms.gc_hook;
  GD->atoms.gc_hook = new;

  return old;
}


#endif /*O_ATOMGC*/

#undef PL_register_atom
#undef PL_unregister_atom

void
resetAtoms()
{
}


void
PL_register_atom(atom_t a)
{
#ifdef O_ATOMGC
  size_t index = indexAtom(a);

  if ( index >= GD->atoms.builtin )
  { Atom p;

    LOCK();
    p = fetchAtomArray(index);
    if ( p->references++ == 0 )
      GD->atoms.unregistered--;
    UNLOCK();
  }
#endif
}

void
PL_unregister_atom(atom_t a)
{
#ifdef O_ATOMGC
  size_t index = indexAtom(a);

  if ( index >= GD->atoms.builtin )
  { Atom p;

    LOCK();
    p = fetchAtomArray(index);
    if ( --p->references == 0 )
      GD->atoms.unregistered++;
    if ( p->references == (unsigned)-1 )
    { Sdprintf("OOPS: -1 references to '%s'\n", p->name);
      trap_gdb();
    }
    UNLOCK();
  }
#endif
}

#define PL_register_atom error		/* prevent using them after this */
#define PL_unregister_atom error

		 /*******************************
		 *	    REHASH TABLE	*
		 *******************************/

static void
rehashAtoms(void)
{ Atom *oldtab   = atomTable;
  int   oldbucks = atom_buckets;
  uintptr_t mask;
  size_t index;
  int i, last=FALSE;

  atom_buckets *= 2;
  mask = atom_buckets-1;
  atomTable = allocHeapOrHalt(atom_buckets * sizeof(Atom));
  memset(atomTable, 0, atom_buckets * sizeof(Atom));

  DEBUG(MSG_HASH_STAT,
	Sdprintf("rehashing atoms (%d --> %d)\n", oldbucks, atom_buckets));

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b[index];

      if ( a )
      { size_t v = a->hash_value & mask;

	a->next = atomTable[v];
	atomTable[v] = a;
      }
    }
  }

  freeHeap(oldtab, oldbucks * sizeof(Atom));
}


word
pl_atom_hashstat(term_t idx, term_t n)
{ GET_LD
  long i, m;
  Atom a;

  if ( !PL_get_long(idx, &i) || i < 0 || i >= (long)atom_buckets )
    fail;
  for(m = 0, a = atomTable[i]; a; a = a->next)
    m++;

  return PL_unify_integer(n, m);
}


static void
registerBuiltinAtoms(void)
{ int size = sizeof(atoms)/sizeof(char *) - 1;
  Atom a;
  const ccharp *s;

  GD->atoms.builtin_array = PL_malloc(size * sizeof(struct atom));
  GD->statistics.atoms = size;

  for(s = atoms, a = GD->atoms.builtin_array; *s; s++, a++)
  { size_t len = strlen(*s);
    unsigned int v0 = MurmurHashAligned2(*s, len, MURMUR_SEED);
    unsigned int v = v0 & (atom_buckets-1);

    a->name       = (char *)*s;
    a->length     = len;
    a->type       = &text_atom;
#ifdef O_ATOMGC
    a->references = 0;
#endif
#ifdef O_TERMHASH
    a->hash_value = v0;
#endif
    a->next       = atomTable[v];
    atomTable[v]  = a;
    registerAtom(a);
  }
}


#if O_DEBUG
static void
exitAtoms(int status, void *context)
{ (void)status;
  (void)context;

  Sdprintf("hashstat: %d lookupAtom() calls used %d strcmp() calls\n",
	   lookups, cmps);
}
#endif


void
initAtoms(void)
{ LOCK();
  if ( !atomTable )			/* Atom hash table */
  { atom_buckets = ATOMHASHSIZE;
    atomTable = allocHeapOrHalt(atom_buckets * sizeof(Atom));
    memset(atomTable, 0, atom_buckets * sizeof(Atom));

    GD->atoms.highest = 1;
    GD->atoms.no_hole_before = 1;
    registerBuiltinAtoms();
#ifdef O_ATOMGC
    GD->atoms.margin = 10000;
    lockAtoms();
#endif
    PL_register_blob_type(&text_atom);

    DEBUG(MSG_HASH_STAT, PL_on_halt(exitAtoms, NULL));
  }
  UNLOCK();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cleanupAtoms() is called at shutdown. There are three possible scenarios
these days: (1) do not cleanup at  all, (2) cleanup the main structures,
leaving the rest to GC or (3) cleanup the whole thing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cleanupAtoms(void)
{ int i;
  int builtin_count = sizeof(atoms)/sizeof(char *) - 1;
  Atom builtin_start = GD->atoms.builtin_array;
  Atom builtin_end   = builtin_start+builtin_count;
  Atom *ap0;

  for(i=0; (ap0=GD->atoms.array.blocks[i]); i++)
  { size_t bs = (size_t)1<<i;
    size_t upto = (size_t)2<<i;
    Atom *ap, *ep;

    ap0 += bs;
    ap = ap0;
    ep = ap+bs;
    if ( upto > GD->atoms.highest )
      ep -= upto-GD->atoms.highest;

    for(; ap<ep; ap++)
    { if ( *ap )
      { Atom a = *ap;

	if ( !(a>=builtin_start && a<builtin_end) )
	{ if ( a->type->release )
	    (*a->type->release)(a->atom);
	  else if ( GD->atoms.gc_hook )
	    (*GD->atoms.gc_hook)(a->atom);

	  if ( false(a->type, PL_BLOB_NOCOPY) )
	    PL_free(a->name);
	  freeHeap(a, sizeof(*a));
	}
      }
    }

    GD->atoms.array.blocks[i] = NULL;
    PL_free(ap0);
  }

  PL_free(builtin_start);

  for(i=0; i<256; i++)			/* char-code -> char-atom map */
  { atom_t *p;

    if ( (p=GD->atoms.for_code[i]) )
    { GD->atoms.for_code[i] = NULL;
      PL_free(p);
    }
  }

  if ( atomTable )
  { freeHeap(atomTable, atom_buckets * sizeof(Atom));
    atomTable = NULL;
  }
}


static word
current_blob(term_t a, term_t type, frg_code call, intptr_t state ARG_LD)
{ atom_t type_name = 0;
  size_t index;
  int i, last=0;

  switch( call )
  { case FRG_FIRST_CALL:
    { PL_blob_t *bt;

      if ( PL_is_blob(a, &bt) )
      { if ( type )
	  return PL_unify_atom(type, bt->atom_name);
	else if ( false(bt, PL_BLOB_TEXT) )
	  fail;

	succeed;
      }
      if ( !PL_is_variable(a) )
	return FALSE;

      index = 1;
      break;
    }
    case FRG_REDO:
      index = state;
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  if ( type )
  { if ( !PL_is_variable(type) &&
	 !PL_get_atom_ex(type, &type_name) )
      fail;
  }

  PL_LOCK(L_AGC);
  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom atom = b[index];

      if ( atom &&
	   atom->atom != ATOM_garbage_collected )
      { if ( type )
	{ if ( type_name && type_name != atom->type->atom_name )
	    continue;

	  PL_unify_atom(type, atom->type->atom_name);
	} else if ( false(atom->type, PL_BLOB_TEXT) )
	  continue;

	PL_unify_atom(a, atom->atom);
	PL_UNLOCK(L_AGC);
	ForeignRedoInt(index+1);
      }
    }
  }
  PL_UNLOCK(L_AGC);

  return FALSE;
}


static
PRED_IMPL("current_blob", 2, current_blob, PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return current_blob(A1, A2, CTX_CNTRL, CTX_INT PASS_LD);
}


static
PRED_IMPL("current_atom", 1, current_atom, PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return current_blob(A1, 0, CTX_CNTRL, CTX_INT PASS_LD);
}


/** blob(@Term, ?Type) is semidet

Type-test for a blob.
*/

static
PRED_IMPL("blob", 2, blob, 0)
{ PRED_LD
  PL_blob_t *bt;

  if ( PL_is_blob(A1, &bt) )
    return PL_unify_atom(A2, bt->atom_name);

  return FALSE;
}


static
PRED_IMPL("$atom_references", 2, atom_references, 0)
{ PRED_LD
  atom_t atom;

  if ( PL_get_atom_ex(A1, &atom) )
  { Atom av = atomValue(atom);

    return PL_unify_integer(A2, av->references);
  }

  fail;
}

		 /*******************************
		 *	 ATOM COMPLETION	*
		 *******************************/

#define ALT_SIZ 80		/* maximum length of one alternative */
#define ALT_MAX 256		/* maximum number of alternatives */
#define stringMatch(m)	((m)->name->name)

typedef struct match
{ Atom		name;
  size_t	length;
} *Match;


static bool
allAlpha(const char *s, size_t len)
{ for( ; --len>=0; s++)
  { if ( !*s || !isAlpha(*s) )
      fail;
  }
  succeed;
}


static int
extendAtom(char *prefix, bool *unique, char *common)
{ size_t index;
  int i, last=FALSE;
  bool first = TRUE;
  size_t lp = strlen(prefix);

  *unique = TRUE;

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b[index];

      if ( a && a->type == &text_atom &&
	   strprefix(a->name, prefix) &&
	   strlen(a->name) < LINESIZ )
      { if ( first == TRUE )
	{ strcpy(common, a->name+lp);
	  first = FALSE;
	} else
	{ char *s = common;
	  char *q = a->name+lp;
	  while( *s && *s == *q )
	    s++, q++;
	  *s = EOS;
	  *unique = FALSE;
	}
      }
    }
  }

  return !first;
}


word
pl_complete_atom(term_t prefix, term_t common, term_t unique)
{ char *p;
  bool u;
  char buf[LINESIZ];
  char cmm[LINESIZ];

  if ( !PL_get_chars(prefix, &p, CVT_ALL|CVT_EXCEPTION) )
    fail;
  strcpy(buf, p);

  if ( extendAtom(p, &u, cmm) )
  { GET_LD

    strcat(buf, cmm);
    if ( PL_unify_list_codes(common, buf) &&
	 PL_unify_atom(unique, u ? ATOM_unique
				 : ATOM_not_unique) )
      succeed;
  }

  fail;
}


static int
compareMatch(const void *m1, const void *m2)
{ return strcmp(stringMatch((Match)m1), stringMatch((Match)m2));
}


static bool
extend_alternatives(char *prefix, struct match *altv, int *altn)
{ size_t index;
  int i, last=FALSE;

  *altn = 0;
  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b[index];

      if ( a && a->type == &text_atom &&
	   strprefix(a->name, prefix) &&
	   a->length < ALT_SIZ &&
	   allAlpha(a->name, a->length) )
      { Match m = &altv[(*altn)++];

	m->name = a;
	m->length = a->length;
	if ( *altn > ALT_MAX )
	  goto out;
      }
    }
  }

out:
  qsort(altv, *altn, sizeof(struct match), compareMatch);

  succeed;
}


word
pl_atom_completions(term_t prefix, term_t alternatives)
{ GET_LD
  char *p;
  char buf[LINESIZ];
  struct match altv[ALT_MAX];
  int altn;
  int i;
  term_t alts = PL_copy_term_ref(alternatives);
  term_t head = PL_new_term_ref();

  if ( !PL_get_chars(prefix, &p, CVT_ALL|CVT_EXCEPTION) )
    fail;
  strcpy(buf, p);

  extend_alternatives(buf, altv, &altn);

  for(i=0; i<altn; i++)
  { if ( !PL_unify_list(alts, head, alts) ||
	 !PL_unify_atom(head, altv[i].name->atom) )
      fail;
  }

  return PL_unify_nil(alts);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Completeness generation for the GNU readline library. This function uses
a state variable to indicate  the   generator  should maintain/reset its
state. Horrible!

We must use thread-local data here.  Worse   is  we can't use the normal
Prolog one as there might  not  be   a  Prolog  engine associated to the
thread.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_PLMT
#include <pthread.h>
static pthread_key_t key;
#endif

#define is_signalled() unlikely(LD && LD->signal.pending != 0)

static int
alnum_text(PL_chars_t *txt)
{ switch(txt->encoding)
  { case ENC_ISO_LATIN_1:
    { const unsigned char *s = (const unsigned char *)txt->text.t;
      const unsigned char *e = &s[txt->length];

      for(; s<e; s++)
      { if ( !isAlpha(*s) )
	  return FALSE;
      }
      return TRUE;
    }
    case ENC_WCHAR:
    { const pl_wchar_t *s = (const pl_wchar_t*)txt->text.w;
      const pl_wchar_t *e = &s[txt->length];

      for(; s<e; s++)
      { if ( !isAlphaW(*s) )
	  return FALSE;
      }
      return TRUE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


static int
atom_generator(PL_chars_t *prefix, PL_chars_t *hit, int state)
{ GET_LD
  size_t index;
  int i, last=FALSE;

#ifdef O_PLMT
  if ( !key )
    pthread_key_create(&key, NULL);
#endif

  if ( !state )
  { index = 1;
  } else
  {
#ifdef O_PLMT
    index = (size_t)pthread_getspecific(key);
#else
    index = LD->atoms.generator;
#endif
  }

  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    Atom *b = GD->atoms.array.blocks[i];

    if ( upto >= GD->atoms.highest )
    { upto = GD->atoms.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b[index];

      if ( is_signalled() )		/* Notably allow windows version */
	PL_handle_signals();		/* to break out on ^C */

      if ( a && get_atom_ptr_text(a, hit) &&
	 hit->length < ALT_SIZ &&
	 PL_cmp_text(prefix, 0, hit, 0, prefix->length) == 0 &&
	 alnum_text(hit) )
      {
#ifdef O_PLMT
        pthread_setspecific(key, (void *)(index+1));
#else
        LD->atoms.generator = index+1;
#endif
        return TRUE;
      }
    }
  }

  return FALSE;
}


char *
PL_atom_generator(const char *prefix, int state)
{ PL_chars_t txt, hit;

  PL_init_text(&txt);
  txt.text.t   = (char *)prefix;
  txt.encoding = ENC_ISO_LATIN_1;
  txt.length   = strlen(prefix);

  while ( atom_generator(&txt, &hit, state) )
  { if ( hit.encoding == ENC_ISO_LATIN_1 )
      return hit.text.t;		/* text is from atoms, thus static */
    state = TRUE;
  }

  return NULL;
}


pl_wchar_t *
PL_atom_generator_w(const pl_wchar_t *prefix,
		    pl_wchar_t *buffer,
		    size_t buflen,
		    int state)
{ PL_chars_t txt, hit;

  PL_init_text(&txt);
  txt.text.w   = (pl_wchar_t *)prefix;
  txt.encoding = ENC_WCHAR;
  txt.length   = wcslen(prefix);

  for( ; atom_generator(&txt, &hit, state); state = TRUE )
  { if ( buflen > hit.length+1 )
    { if ( hit.encoding == ENC_WCHAR )
      { wcscpy(buffer, hit.text.w);
      } else
      { const unsigned char *s = (const unsigned char *)hit.text.t;
	const unsigned char *e = &s[hit.length];
	pl_wchar_t *o;

	for(o=buffer; s<e;)
	  *o++ = *s++;
	*o = EOS;
      }

      return buffer;
    }
  }

  return NULL;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(atom)
  PRED_DEF("current_blob",  2, current_blob, PL_FA_NONDETERMINISTIC)
  PRED_DEF("current_atom", 1, current_atom, PL_FA_NONDETERMINISTIC)
  PRED_DEF("blob", 2, blob, 0)
  PRED_DEF("$atom_references", 2, atom_references, 0)
EndPredDefs
