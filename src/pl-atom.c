/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
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
#include "os/pl-ctype.h"
#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation issues
---------------------

There are two parts in the atom   administration. One is a dynamic array
(called buffer) atom_array, which is there to   find  the atoms back. An
atom   as   it   appears   is   of   type   word   and   of   the   form
(n<<LMASK_BITS)|TAG_ATOM. The atom structure is   located by getting the
n-th pointer from the atom_array  dynamic   array.  See  atomValue() for
translating the word into the address of the structure.

Next, there is a hash-table. Thanks to the work by Keri Harris, the atom
table now uses a lock-free algorithm.  This works as follows:

  - Atoms have a ->next pointer, organizing them as an open hash table.
  - The head pointers for the hash-buckets are in a struct atom_table,
    of which the most recent is in GD->atoms.table.  This structure
    contains a pointer to older atom_tables (before resizing).  A
    resize allocates a new struct atom_table, copies all atoms
    (updating Atom->next) and makes the new atom-table current.
    Lookup and creation work reliable during this process because
    - If the bucket scan accidentally finds the right atom, great.
    - If not, but the atom table has changed we retry, now using
      the new table where all atoms are properly linked again.
    - If the resize is in progress though, we may not find the
      atom.  In that case we create a new one.  As our hash-table
      is still too small, we lock and call rehashAtoms().  So,
      when we get to linking the new atom into the hash-table,
      we will find the table is old, destroy the atom and redo.

  - The creation of an atom needs to guarantee that it is added
    to the latest table and only added once.  We do this by creating
    a RESERVED atom and fully preparing it.  Now, if we managed to
    CAS it into the bucket, we know we are the only one that created
    the atom and we make the atom VALID, so others can find it.  If
    not, we redo the lookup, possibly finding that someone created it
    for us.  It is possible that another thread inserted another atom
    into the same bucket.  In that case we have bad luck and must
    recreate the atom.  Hash collisions should be fairly rare.

Atom garbage collection
-----------------------

There are various categories of atoms:

	* Built-in atoms
	These are used directly in the C-source of the system and cannot
	be removed. These are the atoms upto a certain number. This
	number is GD->atoms.builtin

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

Atoms are reclaimed by collectAtoms(), which is a two pass process.

  - First, the atom-array is scanned and unmarked atoms without
    references are handed to invalidateAtom(), which
    - Uses CAS to reset VALID
    - Removes the atom from the hash bucket
    - adds atom to chain of invalidated atoms
    - sets length to 0;
  - In the second pass, we call destroyAtom() for all atoms in
    the invalidated chain.  destroyAtom() only destroys
    the atom if pl_atom_bucket_in_use() returns FALSE.  This serves
    two purposes:
      - Garantee that Atom->name is valid
      - Avoid a race between looking up the atom and destroying it:

	thread1                      thread2
	--------------------------------------------
	lookupBlob(s, length, type)
	  v = hash(s)
	  a = atoms[v]
	  length == a.length
	  type == a.type
	  memcmp(s, a.name)
	  // we have a match!
				     AGC
				       invalidate atom
				       free atom
				     lookupBlob(s2)
				       v = hash(s2)
				       not found so insert at atoms[v]
	  CAS ref -> ref+1

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
atoms and run collectAtoms() to reclaim the unreferenced atoms. The lock
LD->thread.scan_lock is used to ensure garbage   collection does not run
concurrently with atom garbage collection.

Atom-GC asynchronously walks  the  stacks  of   all  threads  and  marks
everything  that  looks  `atom-like',   i.e.,    our   collector   is  a
`conservative' collector. While agc is running the VM will mark atoms as
it pushes them onto the stacks. See e.g.,   the H_ATOM VMI. An atom that
is unregistered (PL_unregister_atom()) just before   AGC  starts may not
get marked this way. This is fixed by setting LD->atoms.unregistering.

Note that threads can mark their atoms and continue execution because:

  - If a marked atom is no longer needed it is merely not reclaimed this
    time (but might be in the next collection).
  - New atoms are added to the stacks using pushVolatileAtom(), which
    marks the atom if AGC is active.
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

static int	rehashAtoms(void);
static void	considerAGC(void);

static inline int
bump_atom_references(Atom a, unsigned int ref)
{ for(;;)
  { unsigned int nref = ref+1;

    if ( ATOM_REF_COUNT(nref) == 0 )
      return TRUE;			/* reached max references */

    if ( COMPARE_AND_SWAP(&a->references, ref, nref) )
    { if ( ATOM_REF_COUNT(ref) == 0 )
	ATOMIC_DEC(&GD->atoms.unregistered);
      return TRUE;
    } else
    { ref = a->references;
      if ( !ATOM_IS_VALID(ref) )
	return FALSE;
    }
  }
}

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
{ if ( !type->registered )
  { PL_LOCK(L_MISC);

    if ( !type->registered )
    { if ( !GD->atoms.types )
      { GD->atoms.types = type;
      } else
      { PL_blob_t *t = GD->atoms.types;

	while(t->next)
	  t = t->next;

	t->next = type;
	type->rank = t->rank+1;
      }
      if ( true(type, PL_BLOB_TEXT) )
      { if ( true(type, PL_BLOB_WCHAR) )
	  type->padding = sizeof(pl_wchar_t);
	else
	  type->padding = sizeof(char);
      }

      if ( !GD->atoms.initialised )
	type->registered = TRUE;
      if ( !type->atom_name )
	type->atom_name = PL_new_atom(type->name);
      type->registered = TRUE;
    }

    PL_UNLOCK(L_MISC);
  }
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

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom atom = b + index;
      unsigned int refs = atom->references;
      PL_blob_t *btype = atom->type;

      if ( ATOM_IS_VALID(refs) && btype == type &&
	   bump_atom_references(atom, refs) )
      { atom->type = &unregistered_blob_atom;

	atom->name = "<discarded blob>";
	atom->length = strlen(atom->name);
	discarded++;
	PL_unregister_atom(atom->atom);
      }
    }
  }

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

#ifdef O_PLMT

#define acquire_atom_table(t, b) \
  { LD->thread.info->access.atom_table = GD->atoms.table; \
    t = LD->thread.info->access.atom_table->table; \
    b = LD->thread.info->access.atom_table->buckets; \
  }

#define release_atom_table() \
  { LD->thread.info->access.atom_table = NULL; \
    LD->thread.info->access.atom_bucket = NULL; \
  }

#define acquire_atom_bucket(b) \
  { LD->thread.info->access.atom_bucket = (b); \
  }

#define release_atom_bucket() \
  { LD->thread.info->access.atom_bucket = NULL; \
  }

#else

#define acquire_atom_table(t, b) \
  { t = GD->atoms.table->table; \
    b = GD->atoms.table->buckets; \
  }

#define release_atom_table() (void)0

#define acquire_atom_bucket(b) (void)0

#define release_atom_bucket(b) (void)0

#endif

/* Note that we use PL_malloc_uncollectable() here because the pointer in
   our block is not the real memory pointer.  Probably it is better to
   have two pointers; one to the allocated memory and one with the
   necessary offset.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
It might be wise to  provide  for   an  option  that does not reallocate
atoms. In that case accessing a GC'ed   atom  causes a crash rather then
another atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
allocateAtomBlock(int idx)
{ if ( !GD->atoms.array.blocks[idx] )
  { size_t bs = (size_t)1<<idx;
    size_t i;
    Atom newblock;

    if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(struct atom))) )
      outOfCore();

    memset(newblock, 0, bs*sizeof(struct atom));
    for(i=0; i<bs; i++)
    { newblock[i].type = ATOM_TYPE_INVALID;
      newblock[i].name = "<virgin>";
    }
    if ( !COMPARE_AND_SWAP(&GD->atoms.array.blocks[idx],
			   NULL, newblock-bs) )
      PL_free(newblock);		/* done by someone else */
  }
}

static Atom
reserveAtom(void)
{ size_t index;
#ifdef O_ATOMGC				/* try to find a hole! */
  int i;
  int last = FALSE;
  Atom a;
  unsigned int ref;
  int idx;

  for(index=GD->atoms.no_hole_before, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { a = b + index;
      ref = a->references;

      if ( ATOM_IS_FREE(ref) &&
	   COMPARE_AND_SWAP(&a->references, ref, ATOM_RESERVED_REFERENCE) )
      { assert(a->type == ATOM_TYPE_INVALID);
	GD->atoms.no_hole_before = index+1;
        a->atom = (index<<LMASK_BITS)|TAG_ATOM;

	return a;
      }
    }
  }
  GD->atoms.no_hole_before = index+1;
#endif /*O_ATOMGC*/

  for(;;)
  { index = GD->atoms.highest;
    idx = MSB(index);
    assert(index >= 0);

    if ( !GD->atoms.array.blocks[idx] )
      allocateAtomBlock(idx);

    a = &GD->atoms.array.blocks[idx][index];
    ref = a->references;

    if ( ATOM_IS_FREE(ref) &&
	 COMPARE_AND_SWAP(&a->references, ref, ATOM_RESERVED_REFERENCE) )
    { COMPARE_AND_SWAP(&GD->atoms.highest, index, index+1);
      a->atom = (index<<LMASK_BITS)|TAG_ATOM;

      return a;
    }
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

(**)   Without   this    check,    some     threads    may    pass   the
PL_LOCK(L_REHASH_ATOMS) around rehashAtoms() and create   their atom. If
they manage to register the atom in   the old table before rehashAtoms()
activates the new table the insertion   is successful, but rehashAtoms()
may not have moved the atom to the new   table. Now we will repeat if we
bypassed the LOCK as either GD->atoms.rehashing is TRUE or the new table
is activated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
same_name(const Atom a, const char *s, size_t length, const PL_blob_t *type)
{ if ( false(type, PL_BLOB_NOCOPY) )
    return memcmp(s, a->name, length) == 0;
  else
    return s == a->name;
}


word
lookupBlob(const char *s, size_t length, PL_blob_t *type, int *new)
{ GET_LD
  unsigned int v0, v, ref;
  Atom *table;
  int buckets;
  Atom a, head;

  if ( !type->registered )		/* avoid deadlock */
    PL_register_blob_type(type);
  v0 = MurmurHashAligned2(s, length, MURMUR_SEED);

redo:

  acquire_atom_table(table, buckets);

  v  = v0 & (buckets-1);
  head = table[v];
  acquire_atom_bucket(table+v);
  DEBUG(MSG_HASH_STAT, GD->atoms.lookups++);

  if ( true(type, PL_BLOB_UNIQUE) )
  { for(a = table[v]; a; a = a->next)
    { DEBUG(MSG_HASH_STAT, GD->atoms.cmps++);
      ref = a->references;
      if ( ATOM_IS_RESERVED(ref) &&
	   length == a->length &&
	   type == a->type &&
	   same_name(a, s, length, type) )
      { if ( !ATOM_IS_VALID(ref) )
	  goto redo;
#ifdef O_ATOMGC
        if ( indexAtom(a->atom) >= GD->atoms.builtin &&
	     !likely(bump_atom_references(a, ref)) )
	  break;			/* atom was just GCed.  Re-create */
#endif
        *new = FALSE;
	release_atom_table();
	release_atom_bucket();
	return a->atom;
      }
    }
  }

  if ( GD->atoms.table->buckets * 2 < GD->statistics.atoms )
  { int rc;

    PL_LOCK(L_REHASH_ATOMS);
    rc = rehashAtoms();
    PL_UNLOCK(L_REHASH_ATOMS);

    if ( !rc )
      outOfCore();
  }

  if ( !( table == GD->atoms.table->table && head == table[v] ) )
    goto redo;

  a = reserveAtom();
  a->length = length;
  a->type = type;
  if ( false(type, PL_BLOB_NOCOPY) )
  { if ( type->padding )
    { size_t pad = type->padding;

      a->name = PL_malloc_atomic(length+pad);
      memcpy(a->name, s, length);
      memset(a->name+length, 0, pad);
      ATOMIC_ADD(&GD->statistics.atom_string_space, length+pad);
    } else
    { a->name = PL_malloc(length);
      memcpy(a->name, s, length);
      ATOMIC_ADD(&GD->statistics.atom_string_space, length);
    }
  } else
  { a->name = (char *)s;
  }

#ifdef O_TERMHASH
  a->hash_value = v0;
#endif

  if ( true(type, PL_BLOB_UNIQUE) )
  { a->next = table[v];
    if ( !( COMPARE_AND_SWAP(&table[v], head, a) &&
	    !GD->atoms.rehashing &&	/* See (**) above */
            table == GD->atoms.table->table ) )
    { if ( false(type, PL_BLOB_NOCOPY) )
        PL_free(a->name);
      a->type = ATOM_TYPE_INVALID;
      a->name = "<race>";
      MemoryBarrier();
      a->references = 0;
      goto redo;
    }
  }

#ifdef O_ATOMGC
  a->references = 1 | ATOM_VALID_REFERENCE | ATOM_RESERVED_REFERENCE;
#endif

  *new = TRUE;
  if ( type->acquire )
    (*type->acquire)(a->atom);

  release_atom_table();
  release_atom_bucket();

  if ( ATOMIC_INC(&GD->statistics.atoms) % 128 == 0 )
    considerAGC();

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

  if ( ATOM_IS_VALID(ap->references) && !ATOM_IS_MARKED(ap->references) )
  {
#ifdef O_DEBUG_ATOMGC
    if ( atomLogFd )
      Sfprintf(atomLogFd, "Marked `%s' at (#%d)\n", ap->name, i);
#endif
    ATOMIC_OR(&ap->references, ATOM_MARKED_REFERENCE);
  }
}

void
unmarkAtoms(void)
{ size_t index;
  int i, last=FALSE;

  for(index=GD->atoms.builtin, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;

      if ( ATOM_IS_MARKED(a->references) )
      { ATOMIC_AND(&a->references, ~ATOM_MARKED_REFERENCE);
      }
    }
  }
}


void
maybe_free_atom_tables(void)
{
  AtomTable t = GD->atoms.table;
  while ( t )
  { AtomTable t2 = t->prev;
    if ( t2 && !pl_atom_table_in_use(t2) )
    { t->prev = t2->prev;
      freeHeap(t2->table, t2->buckets * sizeof(Atom));
      freeHeap(t2, sizeof(atom_table));
    }
    t = t->prev;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destroyAtom()  actually  discards  an  atom.  The  code  marked  (*)  is
sometimes inserted to debug atom-gc. The   trick  is to create xxxx<...>
atoms that should *not* be subject to AGC.   If we find one collected we
know we trapped a bug.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ATOM_NAME_MUST_FREE 0x1

static Atom invalid_atoms = NULL;

static int
invalidateAtom(Atom a, unsigned int ref)
{ Atom *ap;

#define ATOM_PRE_DESTROY_REFERENCE \
	(ATOM_DESTROY_REFERENCE|ATOM_RESERVED_REFERENCE)

  if ( !COMPARE_AND_SWAP(&a->references, ref, ATOM_PRE_DESTROY_REFERENCE) )
  { return FALSE;
  }

  if ( a->type->release )
  { if ( !(*a->type->release)(a->atom) )
    { COMPARE_AND_SWAP(&a->references, ATOM_PRE_DESTROY_REFERENCE, ref);
      return FALSE;
    }
  } else if ( GD->atoms.gc_hook )
  { if ( !(*GD->atoms.gc_hook)(a->atom) )
    { COMPARE_AND_SWAP(&a->references, ATOM_PRE_DESTROY_REFERENCE, ref);
      return FALSE;				/* foreign hooks says `no' */
    }
  }

  a->references = ATOM_DESTROY_REFERENCE;

#ifdef O_DEBUG_ATOMGC
  if ( atomLogFd )
    Sfprintf(atomLogFd, "Invalidated `%s'\n", a->name);
#endif

  if ( true(a->type, PL_BLOB_UNIQUE) )
  { AtomTable table;
    uintptr_t mask;

  redo:
    table = GD->atoms.table;
    mask = table->buckets-1;
    ap = &table->table[a->hash_value & mask];

    if ( *ap == a )
    { if ( !COMPARE_AND_SWAP(&table->table[a->hash_value & mask], a, a->next) )
      { goto redo;
      }
    }
    else
    { for( ; ; ap = &(*ap)->next )
      { assert(*ap);		// MT: TBD: failed a few times!?

        if ( *ap == a )
        { *ap = a->next;
          break;
        }
      }
    }
  }

  if ( false(a->type, PL_BLOB_NOCOPY) )
  { size_t slen = a->length + a->type->padding;
    ATOMIC_SUB(&GD->statistics.atom_string_space, slen);
    ATOMIC_ADD(&GD->statistics.atom_string_space_freed, slen);
    a->next_invalid = (uintptr_t)invalid_atoms | ATOM_NAME_MUST_FREE;
  } else
  { a->next_invalid = (uintptr_t)invalid_atoms;
  }
  invalid_atoms = a;
  a->length = 0;

  return TRUE;
}

static int
destroyAtom(Atom a, Atom **buckets)
{ unsigned int v;
  AtomTable t;
  size_t index;

  while ( buckets && *buckets )
  { t = GD->atoms.table;
    while ( t )
    { v = a->hash_value & (t->buckets-1);
      if ( *buckets == t->table+v )
      { return FALSE;
      }
      t = t->prev;
    }
    buckets++;
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

  if ( a->next_invalid & ATOM_NAME_MUST_FREE )
  { PL_free(a->name);
  }

  a->name = "<reclaimed>";
  a->type = ATOM_TYPE_INVALID;
  MemoryBarrier();
  a->references = 0;

  index = indexAtom(a->atom);
  if ( GD->atoms.no_hole_before > index )
    GD->atoms.no_hole_before = index;

  return TRUE;
}


static size_t
collectAtoms(void)
{ size_t reclaimed = 0;
  size_t unregistered = 0;
  size_t index;
  int i, last=FALSE;
  Atom temp, next, prev = NULL;	 /* = NULL to keep compiler happy */

  for(index=GD->atoms.builtin, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;
      unsigned int ref = a->references;

      if ( !ATOM_IS_VALID(ref) )
      { continue;
      }

      if ( !ATOM_IS_MARKED(ref) && (ATOM_REF_COUNT(ref) == 0) )
      { invalidateAtom(a, ref);
      } else
      {	ATOMIC_AND(&a->references, ~ATOM_MARKED_REFERENCE);
        if ( ATOM_REF_COUNT(ref) == 0 )
	  unregistered++;
      }
    }
  }

  Atom** buckets = pl_atom_buckets_in_use();

  temp = invalid_atoms;
  while ( temp && temp == invalid_atoms )
  { next = (Atom)(temp->next_invalid & ~ATOM_NAME_MUST_FREE);
    if ( destroyAtom(temp, buckets) )
    { reclaimed++;
      invalid_atoms = next;
    }
    prev = temp;
    temp = next;
  }
  while ( temp )
  { next = (Atom)(temp->next_invalid & ~ATOM_NAME_MUST_FREE);
    if ( destroyAtom(temp, buckets) )
    { reclaimed++;
      prev->next_invalid = ((uintptr_t)next | (prev->next_invalid & ATOM_NAME_MUST_FREE));
    } else
    { prev = temp;
    }
    temp = next;
  }

  if ( buckets )
    PL_free(buckets);
  maybe_free_atom_tables();

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
  int verbose = truePrologFlag(PLFLAG_TRACE_GC) && !LD->in_print_message;
  double t;
  sigset_t set;
  size_t reclaimed;
  int rc = TRUE;

  if ( GD->cleaning != CLN_NORMAL )	/* Cleaning up */
    return TRUE;

  if ( !COMPARE_AND_SWAP(&GD->atoms.gc_active, FALSE, TRUE) )
    return TRUE;

  if ( verbose )
  { if ( !printMessage(ATOM_informational,
		       PL_FUNCTOR_CHARS, "agc", 1,
		         PL_CHARS, "start") )
    { GD->atoms.gc_active = FALSE;
      return FALSE;
    }
  }

  PL_LOCK(L_REHASH_ATOMS);
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
  GD->atoms.collected += reclaimed;
  ATOMIC_SUB(&GD->statistics.atoms, reclaimed);
  t = CpuTime(CPU_USER) - t;
  GD->atoms.gc_time += t;
  GD->atoms.gc++;
  unblockSignals(&set);
  PL_UNLOCK(L_REHASH_ATOMS);

  if ( verbose )
    rc = printMessage(ATOM_informational,
		      PL_FUNCTOR_CHARS, "agc", 1,
		        PL_FUNCTOR_CHARS, "done", 3,
		          PL_INT64, GD->atoms.collected - oldcollected,
		          PL_INT, GD->statistics.atoms,
		          PL_DOUBLE, (double)t);

  GD->atoms.gc_active = FALSE;

  return rc;
}


PL_agc_hook_t
PL_agc_hook(PL_agc_hook_t new)
{ PL_agc_hook_t old = GD->atoms.gc_hook;
  GD->atoms.gc_hook = new;

  return old;
}


static void
considerAGC(void)
{ if ( GD->atoms.margin != 0 &&
       GD->atoms.unregistered >= GD->atoms.non_garbage + GD->atoms.margin )
  { signalGCThread(SIG_ATOM_GC);
  }
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
register_atom(volatile Atom p)
{ for(;;)
  { unsigned int ref  = p->references;
    unsigned int nref = ref+1;

    if ( ATOM_REF_COUNT(nref) != 0 )
    { if ( COMPARE_AND_SWAP(&p->references, ref, nref) )
      { if ( ATOM_REF_COUNT(nref) == 1 )
	  ATOMIC_DEC(&GD->atoms.unregistered);
	return;
      }
    } else
    { return;
    }
  }
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


static char *
dbgAtomName(Atom a, char *enc, char **buf)
{ if ( a->type == &text_atom )
  { if ( enc ) *enc = 'L';
    return a->name;
  } else if ( isUCSAtom(a) )
  { if ( enc ) *enc = 'W';
    return a->name;
  } else
  { size_t len = 0;
    IOSTREAM *fd = Sopenmem(buf, &len, "w");
    (a->type->write)(fd, a->atom, 0);
    Sclose(fd);
    return *buf;
  }
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
  unsigned int refs;

  if ( index >= GD->atoms.builtin )
  { Atom p;

    p = fetchAtomArray(index);
    if ( !ATOM_IS_VALID(p->references) )
    { Sdprintf("OOPS: PL_unregister_atom('%s'): invalid atom\n", p->name);
      trap_gdb();
    }

    if ( unlikely(ATOM_REF_COUNT(p->references+1) == 0) )
      return;

    if ( GD->atoms.gc_active )
    { unsigned int oldref, newref;

      do
      { oldref = p->references;
	newref = oldref - 1;

	if ( ATOM_REF_COUNT(newref) == 0 )
	  newref |= ATOM_MARKED_REFERENCE;
      } while( !COMPARE_AND_SWAP(&p->references, oldref, newref) );
      refs = ATOM_REF_COUNT(newref);
    } else
    { GET_LD

      if ( HAS_LD )
	LD->atoms.unregistering = a;
      if ( (refs=ATOM_REF_COUNT(ATOMIC_DEC(&p->references))) == 0 )
	ATOMIC_INC(&GD->atoms.unregistered);
    }
    if ( refs == ATOM_REF_COUNT((unsigned int)-1) )
    { char fmt[100];
      char *enc;
      char *buf = NULL;

      strcpy(fmt, "OOPS: PL_unregister_atom('%Ls'): -1 references\n");
      enc = strchr(fmt, '%')+1;

      Sdprintf(fmt, dbgAtomName(p, enc, &buf));
      if ( buf )
	PL_free(buf);
      trap_gdb();
    }
  }
#endif
}


		 /*******************************
		 *	      CHECK		*
		 *******************************/

#ifdef O_DEBUG

static int
findAtomSelf(Atom a)
{ GET_LD
  Atom *table;
  int buckets;
  Atom head, ap;
  unsigned int v;

redo:
  acquire_atom_table(table, buckets);
  v = a->hash_value & (buckets-1);
  head = table[v];
  acquire_atom_bucket(table+v);

  for(ap=head; ap; ap = ap->next )
  { if ( ap == a )
    { release_atom_table();
      release_atom_bucket();
      return TRUE;
    }
  }

  if ( !( table == GD->atoms.table->table && head == table[v] ) )
    goto redo;

  return FALSE;
}


int
checkAtoms_src(const char *file, int line)
{ size_t index;
  int i, last=FALSE;
  int errors = 0;

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;

      if ( ATOM_IS_VALID(a->references) )
      { if ( !a->type || !a->name || (int)ATOM_REF_COUNT(a->references) < 0 )
	{ size_t bs = (size_t)1<<i;
	  Sdprintf("%s%d: invalid atom %p at index %zd in "
		   "block at %p (size %d)\n",
		   file, line, a, index, b+bs, bs);
	  errors++;
	  trap_gdb();
	}

	if ( true(a->type, PL_BLOB_UNIQUE) )
	{ if ( !findAtomSelf(a) )
	  { Sdprintf("%s%d: cannot find self: %p\n", file, line, a);
	  }
	}
      }
    }
  }

  return errors;
}

#endif /*O_DEBUG*/


		 /*******************************
		 *	    REHASH TABLE	*
		 *******************************/

static int
rehashAtoms(void)
{ AtomTable newtab;
  uintptr_t mask;
  size_t index;
  int i, last=FALSE;

  if ( GD->cleaning != CLN_NORMAL )
    return TRUE;			/* no point anymore and foreign ->type */
					/* pointers may have gone */

  if ( GD->atoms.table->buckets * 2 >= GD->statistics.atoms )
    return TRUE;

  if ( !(newtab = allocHeap(sizeof(*newtab))) )
    return FALSE;
  newtab->buckets = GD->atoms.table->buckets * 2;
  if ( !(newtab->table = allocHeapOrHalt(newtab->buckets * sizeof(Atom))) )
  { freeHeap(newtab, sizeof(*newtab));
    return FALSE;
  }
  memset(newtab->table, 0, newtab->buckets * sizeof(Atom));
  newtab->prev = GD->atoms.table;
  mask = newtab->buckets-1;

  DEBUG(MSG_HASH_STAT,
	Sdprintf("rehashing atoms (%d --> %d)\n",
		 GD->atoms.table->buckets, newtab->buckets));

  GD->atoms.rehashing = TRUE;

  for(index=1, i=0; !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { volatile Atom a = b + index;
      unsigned int ref;
redo:
      ref = a->references;
      if ( ATOM_IS_RESERVED(ref) )
      { if ( !ATOM_IS_VALID(ref) )
	  goto redo;
	if ( true(a->type, PL_BLOB_UNIQUE) )
	{ size_t v;
	  v = a->hash_value & mask;
	  a->next = newtab->table[v];
	  newtab->table[v] = a;
	}
      }
    }
  }

  GD->atoms.table = newtab;
  GD->atoms.rehashing = FALSE;

  return TRUE;
}


word
pl_atom_hashstat(term_t idx, term_t n)
{ GET_LD
  long i, m;
  int buckets;
  Atom *table;
  Atom a;

  acquire_atom_table(table, buckets);

  if ( !PL_get_long(idx, &i) || i < 0 || i >= (long)buckets )
  { release_atom_table();
    fail;
  }
  for(m = 0, a = table[i]; a; a = a->next)
    m++;

  release_atom_table();

  return PL_unify_integer(n, m);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetListAtoms() resets the atom '[|]' to point  to '.' and switches the
type for '[]' back to  the  normal   text_atom  type.  This is needed to
switch to traditional mode if the atom table has been initialised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
resetListAtoms(void)
{ Atom a = atomValue(ATOM_dot);

  if ( strcmp(a->name, ".") != 0 )
  { Atom *ap2 = &GD->atoms.table->table[a->hash_value & (GD->atoms.table->buckets-1)];
    unsigned int v;
    static char *s = ".";

    DEBUG(0, Sdprintf("Resetting list constructor to ./2\n"));

    for( ; ; ap2 = &(*ap2)->next )
    { assert(*ap2);		/* MT: TBD: failed a few times!? */

      if ( *ap2 == a )
      { *ap2 = a->next;
	goto modify;
      }
    }
    assert(0);

  modify:
    a->name   = s;
    a->length = strlen(s);
    a->hash_value = MurmurHashAligned2(s, a->length, MURMUR_SEED);
    v = a->hash_value & (GD->atoms.table->buckets-1);

    a->next      = GD->atoms.table->table[v];
    GD->atoms.table->table[v] = a;
  }

  a = atomValue(ATOM_nil);
  a->type = &text_atom;

  return TRUE;
}


static void
registerBuiltinAtoms(void)
{ int size = sizeof(atoms)/sizeof(char *) - 1;
  Atom a;
  const ccharp *sp;
  size_t index;
  int idx;

  GD->atoms.builtin_array = PL_malloc(size * sizeof(struct atom));
  GD->statistics.atoms = size;

  for( sp = atoms, index = GD->atoms.highest; *sp; sp++, index++ )
  { const char *s = *sp;
    size_t len = strlen(s);
    unsigned int v0, v;

    idx = MSB(index);

    if ( !GD->atoms.array.blocks[idx] )
    { allocateAtomBlock(idx);
    }

    if ( *s == '.' && len == 1 && !GD->options.traditional )
    { s = "[|]";
      len = strlen(s);
    }

    v0 = MurmurHashAligned2(s, len, MURMUR_SEED);
    v  = v0 & (GD->atoms.table->buckets-1);

    a = &GD->atoms.array.blocks[idx][index];
    a->atom       = (index<<LMASK_BITS)|TAG_ATOM;
    a->name       = (char *)s;
    a->length     = len;
    a->type       = &text_atom;
#ifdef O_ATOMGC
    a->references = ATOM_VALID_REFERENCE | ATOM_RESERVED_REFERENCE;
#endif
#ifdef O_TERMHASH
    a->hash_value = v0;
#endif
    a->next       = GD->atoms.table->table[v];
    GD->atoms.table->table[v]  = a;

    GD->atoms.no_hole_before = index+1;
    GD->atoms.highest = index+1;
  }
}


#if O_DEBUG
static int
exitAtoms(int status, void *context)
{ (void)status;
  (void)context;

  Sdprintf("hashstat: %d lookupAtom() calls used %d strcmp() calls\n",
	   GD->atoms.lookups, GD->atoms.cmps);

  return 0;
}
#endif


void
do_init_atoms(void)
{ PL_LOCK(L_INIT_ATOMS);
  if ( !GD->atoms.initialised )			/* Atom hash table */
  { GD->atoms.table = allocHeapOrHalt(sizeof(*GD->atoms.table));
    GD->atoms.table->buckets = ATOMHASHSIZE;
    GD->atoms.table->table = allocHeapOrHalt(ATOMHASHSIZE * sizeof(Atom));
    memset(GD->atoms.table->table, 0, ATOMHASHSIZE * sizeof(Atom));
    GD->atoms.table->prev = NULL;

    GD->atoms.highest = 1;
    GD->atoms.no_hole_before = 1;
    registerBuiltinAtoms();
#ifdef O_ATOMGC
    GD->atoms.margin = 10000;
    lockAtoms();
#endif
    text_atom.atom_name = ATOM_text;
    PL_register_blob_type(&text_atom);

    DEBUG(MSG_HASH_STAT, PL_on_halt(exitAtoms, NULL));
#ifdef O_RESERVED_SYMBOLS
    initReservedSymbols();
#endif
    GD->atoms.initialised = TRUE;
  }
  PL_UNLOCK(L_INIT_ATOMS);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cleanupAtoms() is called at shutdown. There are three possible scenarios
these days: (1) do not cleanup at  all, (2) cleanup the main structures,
leaving the rest to GC or (3) cleanup the whole thing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cleanupAtoms(void)
{ AtomTable table;
  size_t index;
  int i, last=FALSE;

  for(index=GD->atoms.builtin, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;
      unsigned int ref = a->references;

      if ( !ATOM_IS_VALID(ref) )
        continue;

      if ( a->type->release )
        (*a->type->release)(a->atom);
      else if ( GD->atoms.gc_hook )
        (*GD->atoms.gc_hook)(a->atom);

      if ( false(a->type, PL_BLOB_NOCOPY) )
        PL_free(a->name);
    }
  }

  i = 0;
  while( GD->atoms.array.blocks[i] )
  { size_t bs = (size_t)1<<i;
    PL_free(GD->atoms.array.blocks[i++] + bs);
  }

  for(i=0; i<256; i++)			/* char-code -> char-atom map */
  { atom_t *p;

    if ( (p=GD->atoms.for_code[i]) )
    { GD->atoms.for_code[i] = NULL;
      PL_free(p);
    }
  }

  table = GD->atoms.table;
  while ( table )
  { AtomTable prev = table->prev;
    freeHeap(table->table, table->buckets * sizeof(Atom));
    freeHeap(table, sizeof(atom_table));
    table = prev;
  }
  if ( GD->atoms.table )
  { GD->atoms.table = NULL;
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
	  return FALSE;

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
      return TRUE;
  }

  if ( type )
  { if ( !PL_is_variable(type) &&
	 !PL_get_atom_ex(type, &type_name) )
      return FALSE;
  }

  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom atom = b + index;
      unsigned int refs = atom->references;
      PL_blob_t *btype = atom->type;
      int rc;

      if ( ATOM_IS_VALID(refs) && btype &&
	   (!type_name || type_name == btype->atom_name) &&
	   bump_atom_references(atom, refs) )
      { DEBUG(CHK_SECURE,	/* avoid trap through linkVal__LD() check */
	      if ( atom->atom == ATOM_garbage_collected )
	      { PL_unregister_atom(atom->atom);
	        continue;
	      });

	if ( type )
	{ if ( !type_name )
	  { if ( !PL_unify_atom(type, btype->atom_name) )
	    { PL_unregister_atom(atom->atom);
	      return FALSE;
	    }
	  }
	} else if ( false(btype, PL_BLOB_TEXT) )
	{ PL_unregister_atom(atom->atom);
	  continue;
	}

	rc = PL_unify_atom(a, atom->atom);
	PL_unregister_atom(atom->atom);
	if ( rc )
	  ForeignRedoInt(index+1);
	else
	  return rc;
      }
    }
  }

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

    return PL_unify_integer(A2, ATOM_REF_COUNT(av->references));
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


/* An atom without references cannot be part of the program
*/

static int
global_atom(Atom a)
{ return ( ATOM_REF_COUNT(a->references) != 0 ||
	   indexAtom(a->atom) < GD->atoms.builtin );
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
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;

      if ( ATOM_IS_VALID(a->references) && a->type == &text_atom &&
	   global_atom(a) &&
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
  size_t len;
  bool u;
  char buf[LINESIZ];
  char cmm[LINESIZ];

  if ( !PL_get_nchars(prefix, &len, &p, CVT_ALL|CVT_EXCEPTION) ||
       len >= sizeof(buf) )
    return FALSE;
  strcpy(buf, p);

  if ( extendAtom(p, &u, cmm) )
  { strcat(buf, cmm);
    if ( PL_unify_list_codes(common, buf) &&
	 PL_unify_atom(unique, u ? ATOM_unique
				 : ATOM_not_unique) )
      return TRUE;
  }

  return FALSE;
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
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;
      PL_chars_t hit;

      if ( index % 256 == 0 && PL_handle_signals() < 0 )
	return FALSE;			/* interrupted */

      if ( ATOM_IS_VALID(a->references) &&
	   global_atom(a) &&
	   get_atom_ptr_text(a, &hit) &&
	   hit.length < ALT_SIZ &&
	   PL_cmp_text(prefix, 0, &hit, 0, prefix->length) == 0 &&
	   is_identifier_text(&hit) )
      { Match m = &altv[(*altn)++];

	m->name = a;
	m->length = a->length;
	if ( *altn >= ALT_MAX )
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
static pthread_once_t key_created = PTHREAD_ONCE_INIT;
static pthread_key_t key;

static void
atom_generator_create_key(void)
{ pthread_key_create(&key, NULL);
}
#endif

static int
atom_generator(PL_chars_t *prefix, PL_chars_t *hit, int state)
{ GET_LD
  size_t index;
  int i, last=FALSE;

#ifdef O_PLMT
  if ( !LD )
    pthread_once(&key_created, atom_generator_create_key);
#endif

  if ( !state )
  { index = 1;
  } else
  { if ( HAS_LD )
      index = LD->atoms.generator;
#ifdef O_PLMT
    else
      index = (size_t)pthread_getspecific(key);
#endif
  }

  for(i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    size_t high = GD->atoms.highest;
    Atom b = GD->atoms.array.blocks[i];

    if ( upto >= high )
    { upto = high;
      last = TRUE;
    }

    for(; index<upto; index++)
    { Atom a = b + index;

      if ( is_signalled(PASS_LD1) )	/* Notably allow windows version */
      { if ( PL_handle_signals() < 0 )	/* to break out on ^C */
	  return FALSE;
      }

      if ( ATOM_IS_VALID(a->references) && global_atom(a) &&
	   get_atom_ptr_text(a, hit) &&
	   hit->length < ALT_SIZ &&
	   PL_cmp_text(prefix, 0, hit, 0, prefix->length) == 0 &&
	   is_identifier_text(hit) )
      { if ( HAS_LD )
	  LD->atoms.generator = index+1;
#ifdef O_PLMT
	else
	  pthread_setspecific(key, (void *)(index+1));
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
  PRED_DEF("$atom_completions", 2, atom_completions, 0)
  PRED_DEF("$complete_atom", 3, complete_atom, 0)
EndPredDefs
