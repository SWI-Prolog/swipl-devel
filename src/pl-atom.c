/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
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
atom   as   it   appears   is   of   type   word   and   of   the   form
(n<<LMASK_BITS)|TAG_ATOM. The atom structure is   located by getting the
n-th pointer from the atom_array  dynamic   array.  See  atomValue() for
translating the word into the address of the structure.

Next, there is a hash-table, which is a normal `open' hash-table mapping
char * to the atom structure. This   thing is dynamically rehashed. This
table is used by lookupAtom() below.

Atom garbage collection
-----------------------

There are various categories of atoms:

	* Built-in atoms
	These are used directly in the C-source of the system and cannot
	be removed. These are the atoms upto a certain number. This
	number is sizeof(atoms)/sizeof(char *).

	* Foreign referenced atoms
	These are references hold in foreign code by means of
	PL_new_atom() or other calls returning an atom. The system has
	no way to determine the lifetime of them.  Foreign code must
	keep track of references to atoms using these two functions:

	      - PL_register_atom(atom_t atom)
	      - PL_unregister_atom(atom_t atom)

	* References from the Prolog stacks
	Reference counting is unacceptable here, which implies we must
	mark atoms that are accessible from the stacks.  This is done
	by markAtomsOnStacks().

	* References from other structures
	Various of the structures contain or may contain atom
	references.  There are two options: lock/unlock them using
	PL_register_atom() on creation/destruction of the structure
	or enumerate them and flag the atoms.  Currently, we use
	registration everywhere, except for temporary structures
	used by findall/3 and message queues, which are marked
	by markAtomsThreads().

	* References from compiled code and records
	This uses PL_register_atom(), except for the cases mentioned
	above.

Reclaiming
----------

To reclaim an atom, it is deleted   from  the hash-table, a NULL pointer
should be set in the dynamic array and the structure is disposed.

The dynamic array gets holes and  we   remember  the  first free hole to
avoid too much search. Alternatively, we could  turn the holes into some
form of linked list, for example by   encoding an integer that expresses
the location of the next hole. We   cannot  shrink the array, unless all
atoms above a certain index are gone.

Atom GC and multi-threading
---------------------------

This is a hard problem. Atom-GC cannot   run  while some thread performs
normal GC because the pointer relocation makes it extremely hard to find
referenced atoms. Otherwise, ask all  threads   to  mark their reachable
atoms and run collectAtoms() to reclaim the unreferenced atoms.

On Unix, we signal all threads. Upon receiving the signal, they mark all
accessible atoms and continue.  On  Windows,   we  have  no asynchronous
signals, so we silence the threads one-by-one   and do the marking. This
is realised by forThreadLocalData().  Note  that   this  means  only one
thread is collecting  in  Windows.  This   could  be  enhanced  by using
multiple threads during the collection phase.

Note that threads can mark their atoms and continue execution because:

  - If a marked atom is no longer needed it is merely not reclaimed this
    time (but might be in the next collection).
  - If a new atom is referenced from the stack it is either a
    - builtin atom (no problem)
    - an atom from a structure using reference counting (is referenced
      by this structure, so no problem, unless the reference count drops
      to zero in PL_unregister_atom().  See PL_unregister_atom() for
      handling the no-locking case.
    - It is created.  This case blocks on L_ATOM being locked from
      lookupBlob().
  - Finally, message queues and bags as used by findall/3 complicate
    the issue.  An atom sent to these structures subsequently may
    become inaccessible from the stack (the normal case for findall/3,
    which backtracks).  If the atom is copied back from the structure
    to the stack ('$collect_findall_bag'/3 or thread_get_message/1,2),
    the atom can no longer be marked from the structure but is added
    to the stacks without locking.   We resolve this issue as follows:
    - Run marking from queues/bags after marking the stack.  This
      ensures that atoms added to the these structures get marked,
      also if the atom is no longer on the stack.
    - If an atom is copied to the stack from such a structure while
      AGC is running, we are ok, because this is merely the same issue
      as atoms living on the stack.  TBD: redesign the structures such
      that they can safely be walked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	rehashAtoms(void);

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
      Atom *newblock;

      if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(Atom))) )
	outOfCore();

      memset(newblock, 0, bs*sizeof(Atom));
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
	  {
#ifdef ATOMIC_REFERENCES
	    if ( ATOMIC_INC(&a->references) == 1 )
	      ATOMIC_DEC(&GD->atoms.unregistered);
#else
	    if ( ++a->references == 1 )
	      GD->atoms.unregistered--;
#endif
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
#ifdef ATOMIC_REFERENCES
	  if ( ATOMIC_INC(&a->references) == 1 )
	    ATOMIC_DEC(&GD->atoms.unregistered);
#else
	  if ( a->references++ == 0 )
	    GD->atoms.unregistered--;
#endif
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
  { if ( GD->statistics.atoms % 128 == 0 ) /* see (*) above */
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

  if ( ap && !(ap->references & ATOM_MARKED_REFERENCE) )
  {
#ifdef O_DEBUG_ATOMGC
    if ( atomLogFd )
      Sfprintf(atomLogFd, "Marked `%s' at (#%d)\n", ap->name, i);
#endif
#ifdef ATOMIC_REFERENCES
    ATOMIC_OR(&ap->references, ATOM_MARKED_REFERENCE);
#else
    ap->references |= ATOM_MARKED_REFERENCE;
#endif
  }
}

void
unmarkAtoms(void)
{ size_t index;
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

      if ( a && (a->references & ATOM_MARKED_REFERENCE) )
      {
#ifdef ATOMIC_REFERENCES
        ATOMIC_AND(&a->references, ~ATOM_MARKED_REFERENCE);
#else
        a->references &= ~ATOM_MARKED_REFERENCE;
#endif
      }
    }
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
  size_t unregistered = 0;
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
      {
#ifdef ATOMIC_REFERENCES
	ATOMIC_AND(&a->references, ~ATOM_MARKED_REFERENCE);
#else
	a->references &= ~ATOM_MARKED_REFERENCE;
#endif
        if ( a->references == 0 )
	  unregistered++;
      }
    }
  }

  GD->atoms.unregistered = GD->atoms.non_garbage = unregistered;

  return reclaimed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_garbage_collect_atoms() realised the atom   garbage  collector (AGC).

Issues around the design of the atom  garbage collector are explained at
the start of this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

foreign_t
pl_garbage_collect_atoms(void)
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
    Sdprintf("Starting ATOM-GC.  Stack:\n");
    PL_backtrace(5, 0);
*/
#endif
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "agc", 1,
		   PL_CHARS, "start");
  }

  PL_LOCK(L_THREAD);
  PL_LOCK(L_AGC);
  PL_LOCK(L_STOPTHEWORLD);
  LOCK();
  GD->atoms.gc_active = TRUE;
  blockSignals(&set);
  t = CpuTime(CPU_USER);
  unmarkAtoms();
  markAtomsOnStacks(LD);
#ifdef O_PLMT
  forThreadLocalDataUnsuspended(markAtomsOnStacks, 0);
  markAtomsMessageQueues();
#endif
  oldcollected = GD->atoms.collected;
  reclaimed = collectAtoms();
  GD->atoms.gc_active = FALSE;
  GD->atoms.collected += reclaimed;
  GD->statistics.atoms -= reclaimed;
  t = CpuTime(CPU_USER) - t;
  GD->atoms.gc_time += t;
  GD->atoms.gc++;
  unblockSignals(&set);
  UNLOCK();
  PL_UNLOCK(L_STOPTHEWORLD);
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(un)register atoms. If  possible,  this   is  implemented  using  atomic
operations. This should be safe because:

    - When we register an atom, it must be referenced from somewhere
      else.
    - When we unregister an atom, it must have at least one reference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
register_atom(Atom p)
{
#ifdef ATOMIC_REFERENCES
  if ( ATOMIC_INC(&p->references) == 1 )
    ATOMIC_DEC(&GD->atoms.unregistered);
#else
  LOCK();
  if ( p->references++ == 0 )
    GD->atoms.unregistered--;
  UNLOCK();
#endif
}


void
PL_register_atom(atom_t a)
{
#ifdef O_ATOMGC
  size_t index = indexAtom(a);

  if ( index >= GD->atoms.builtin )
  { Atom p = fetchAtomArray(index);

    register_atom(p);
  }
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign code reduces the reference count. This is safe, unless we are in
the following scenario:

  - A threads has done its atom-marking during a GC and is continued.
  - Now, it fetches an atom from foreign code and the foreign code calls
    PL_unregister_atom() which drops the reference count to zero. We can
    now get into the position where the atom is no longer accessible
    from foreign code and has not be seen while marking atoms from the
    stack.

The locking version of this code  is   not  a  problem, as the reference
count cannot be dropped as  long  as   AGC  is  running. In the unlocked
version, we need  to  replace  1   by  ATOM_MARKED_REFERENCE  if  AGC is
running. We can be a bit sloppy here:  if   we  do this while AGC is not
running we merely prevent the atom to be  collected in the next AGC. The
next AGC resets the flag  and  thus   the  atom  becomes a candidate for
collection afterwards.  So, basically we must do something like this:

  if ( agc_running )
  { do
    { unsigned int oldref = p->references;
      unsigned int newref = oldref == 1 ? ATOM_MARKED_REFERENCE : oldref-1;
    } while( !compare_and_swap(&p->references, oldref, newref) );
  } else
  { atomic_dec(&p->references);
  }

But, this fails because AGC might kick in between agc_running was tested
FALSE the atomic decrement. This is  fixed   by  putting the atom we are
unregistering  in  LD->atoms.unregistered  and  mark    this  atom  from
markAtomsOnStacks().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
PL_unregister_atom(atom_t a)
{
#ifdef O_ATOMGC
  size_t index = indexAtom(a);

  if ( index >= GD->atoms.builtin )
  { Atom p;
    unsigned int refs;

    p = fetchAtomArray(index);
#ifdef ATOMIC_REFERENCES
    if ( GD->atoms.gc_active )
    { unsigned int oldref, newref;

      do
      { oldref = p->references;
	newref = oldref == 1 ? ATOM_MARKED_REFERENCE : oldref-1;
      } while( !COMPARE_AND_SWAP(&p->references, oldref, newref) );
      refs = newref;

      if ( newref == ATOM_MARKED_REFERENCE )
	ATOMIC_INC(&GD->atoms.unregistered);
    } else
    { GET_LD

      if ( LD )
	LD->atoms.unregistering = a;
      if ( (refs=ATOMIC_DEC(&p->references)) == 0 )
	ATOMIC_INC(&GD->atoms.unregistered);
    }
#else
    LOCK();
    if ( (refs = --p->references) == 0 )
      GD->atoms.unregistered++;
    UNLOCK();
#endif
    if ( refs == (unsigned int)-1 )
    { Sdprintf("OOPS: -1 references to '%s'\n", p->name);
      trap_gdb();
    }
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

      if ( a && true(a->type, PL_BLOB_UNIQUE) )
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
static int
exitAtoms(int status, void *context)
{ (void)status;
  (void)context;

  Sdprintf("hashstat: %d lookupAtom() calls used %d strcmp() calls\n",
	   lookups, cmps);

  return 0;
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

#define is_signalled() unlikely(LD && LD->signal.pending != 0)

typedef struct match
{ Atom		name;
  size_t	length;
} *Match;


static inline int
completion_candidate(Atom a)
{ return (a->references || indexAtom(a->atom) < GD->atoms.builtin);
}


static int
is_identifier_text(PL_chars_t *txt)
{ if ( txt->length == 0 )
    return FALSE;

  switch(txt->encoding)
  { case ENC_ISO_LATIN_1:
    { const unsigned char *s = (const unsigned char *)txt->text.t;
      const unsigned char *e = &s[txt->length];

      if ( !f_is_prolog_atom_start(*s) )
	return FALSE;

      for(s++; s<e; s++)
      { if ( !f_is_prolog_identifier_continue(*s) )
	  return FALSE;
      }
      return TRUE;
    }
    case ENC_WCHAR:
    { const pl_wchar_t *s = (const pl_wchar_t*)txt->text.w;
      const pl_wchar_t *e = &s[txt->length];

      if ( !f_is_prolog_atom_start(*s) )
	return FALSE;

      for(s++; s<e; s++)
      { if ( !f_is_prolog_identifier_continue(*s) )
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
	   completion_candidate(a) &&
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


/** '$complete_atom'(+Prefix, -Common, -Unique) is semidet.

True when Prefix can be extended based on currently defined atoms.

@arg Common is a code list consisting of the characters from Prefix
     and the common text for all possible completions
@arg Unique is either =unique= or =not_unique=.  In the second case,
     this implies that there are longer atoms that have the prefix
     Common.
@see '$atom_completions'/2.
@bug This version only handles ISO Latin 1 text
*/

static
PRED_IMPL("$complete_atom", 3, complete_atom, 0)
{ PRED_LD
  term_t prefix = A1;
  term_t common = A2;
  term_t unique = A3;

  char *p;
  bool u;
  char buf[LINESIZ];
  char cmm[LINESIZ];

  if ( !PL_get_chars(prefix, &p, CVT_ALL|CVT_EXCEPTION) )
    fail;
  strcpy(buf, p);

  if ( extendAtom(p, &u, cmm) )
  { strcat(buf, cmm);
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


static int
extend_alternatives(PL_chars_t *prefix, struct match *altv, int *altn)
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
      PL_chars_t hit;

      if ( index % 256 == 0 && PL_handle_signals() < 0 )
	return FALSE;			/* interrupted */

      if ( a && completion_candidate(a) &&
	   get_atom_ptr_text(a, &hit) &&
	   hit.length < ALT_SIZ &&
	   PL_cmp_text(prefix, 0, &hit, 0, prefix->length) == 0 &&
	   is_identifier_text(&hit) )
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

  return TRUE;
}


/** '$atom_completions'(+Prefix, -Alternatives:list(atom)) is det.

True when Alternatives is a list of   all  atoms that have prefix Prefix
and are considered completion  candidates.   Completions  candidates are
atoms that

  - Are built-in or referenced from some static datastructure
  - All characters are legal characters for unquoted atoms
  - The atom is at most 80 characters long
*/

static
PRED_IMPL("$atom_completions", 2, atom_completions, 0)
{ PRED_LD
  term_t prefix = A1;
  term_t alternatives = A2;

  PL_chars_t p_text;
  struct match altv[ALT_MAX];
  int altn;
  int i;
  term_t alts = PL_copy_term_ref(alternatives);
  term_t head = PL_new_term_ref();

  if ( !PL_get_text(prefix, &p_text, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  if ( !extend_alternatives(&p_text, altv, &altn) )
    return FALSE;			/* interrupt */

  for(i=0; i<altn; i++)
  { if ( !PL_unify_list(alts, head, alts) ||
	 !PL_unify_atom(head, altv[i].name->atom) )
      return FALSE;
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

      if ( a && completion_candidate(a) &&
	   get_atom_ptr_text(a, hit) &&
	   hit->length < ALT_SIZ &&
	   PL_cmp_text(prefix, 0, hit, 0, prefix->length) == 0 &&
	   is_identifier_text(hit) )
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
		 *	   SPECIAL ATOMS	*
		 *******************************/

/* This code provides forward compatibility between 6.0 and 7.0
   for shared objects that acts as plugin.
*/

static const atom_t special_atoms[] =
{ ATOM_nil,				/* 0: [] */
  ATOM_dot				/* 1: .(_|_) or '$cons'(_,_) */
};


const atom_t *
_PL_atoms(void)
{ return special_atoms;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(atom)
  PRED_DEF("current_blob",  2, current_blob, PL_FA_NONDETERMINISTIC)
  PRED_DEF("current_atom", 1, current_atom, PL_FA_NONDETERMINISTIC)
  PRED_DEF("blob", 2, blob, 0)
  PRED_DEF("$atom_references", 2, atom_references, 0)
  PRED_DEF("$atom_completions", 2, atom_completions, 0)
  PRED_DEF("$complete_atom", 3, complete_atom, 0)
EndPredDefs
