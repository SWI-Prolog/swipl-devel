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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-ctype.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation issues
---------------------

There are two parts in the atom   administration. One is a dynamic array
(called buffer) atom_array, which is there to   find  the atoms back. An
atom as it appears is a long   of the form (n<<LMASK_BITS)|TAG_ATOM. The
atom  structure  is  located  by  getting  the  n-th  pointer  from  the
atom_array dynamic array.  See atomValue() for translating the long into
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
as long as the invoking thread keeps   the  atom mutex locked during the
whole atom garbage collection process. This   implies  the thread cannot
create any atoms as long as the collection is going on.

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
It might be wise to  provide  for   an  option  that does not reallocate
atoms. In that case accessing a GC'ed   atom  causes a crash rather then
another atom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerAtom(Atom a)
{ int n = entriesBuffer(&atom_array, Atom);
#ifdef O_ATOMGC				/* try to find a hole! */
  Atom *ap = baseBuffer(&atom_array, Atom);
  Atom *ep = ap+n;
  Atom *p;

  for(p = &ap[GD->atoms.no_hole_before]; p < ep; p++)
  { if ( *p == NULL )
    { n = p - ap;
      *p = a;
      a->atom = (n<<LMASK_BITS)|TAG_ATOM;
      if ( indexAtom(a->atom) != (unsigned long)n )
      {	/* TBD: user-level exception */
	fatalError("Too many (%d) atoms", n);
      }
      GD->atoms.no_hole_before = n+1;

      return;
    }
  }
  GD->atoms.no_hole_before = n+1;
#endif /*O_ATOMGC*/

  a->atom = (n<<LMASK_BITS)|TAG_ATOM;

  addBuffer(&atom_array, a, Atom);
}


		 /*******************************
		 *	  GENERAL LOOKUP	*
		 *******************************/

word
lookupAtom(const char *s, unsigned int length)
{ int v0 = unboundStringHashValue(s, length);
  int v = v0 & (atom_buckets-1);
  ulong oldheap;
  Atom a;

  LOCK();
  DEBUG(0, lookups++);

  for(a = atomTable[v]; a; a = a->next)
  { DEBUG(0, cmps++);
    if ( length == a->length &&
	 memcmp(s, a->name, length) == 0 )
    { 
#ifdef O_ATOMGC
      PL_LOCK(L_MISC);			/* used by PL_(un)register_atom() */
      a->references++;
      PL_UNLOCK(L_MISC);
#endif
      UNLOCK();
      return a->atom;
    }
  }
  oldheap = GD->statistics.heap;
  a = allocHeap(sizeof(struct atom));
  a->length = length;
  a->name = allocHeap(length+1);
  memcpy(a->name, s, length);
  a->name[length] = EOS;
#ifdef O_HASHTERM
  a->hash_value = v0;
#endif
#ifdef O_ATOMGC
  a->references = 1;
#endif
  registerAtom(a);
  a->next       = atomTable[v];
  atomTable[v]  = a;
  GD->statistics.atoms++;

#ifdef O_ATOMGC
  if ( GD->atoms.margin > 0 &&
       GD->statistics.atoms == GD->atoms.non_garbage + GD->atoms.margin )
    PL_raise(SIG_ATOM_GC);
#endif
    
  if ( atom_buckets * 2 < GD->statistics.atoms )
    rehashAtoms();

  GD->statistics.atomspace += (GD->statistics.heap - oldheap);

  UNLOCK();
  
  return a->atom;
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
{ int i = indexAtom(a);
  Atom atom = fetchBuffer(&atom_array, i, Atom);

  if ( !atom )
  { char buf[32];

    Sdprintf("*** No atom at index (#%d) ***", i);
    trap_gdb();

    atom = allocHeap(sizeof(*atom));
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

void
lockAtoms()
{ GD->atoms.builtin     = entriesBuffer(&atom_array, Atom);
  GD->atoms.non_garbage = GD->atoms.builtin;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark an atom from the stacks.  We must be prepared to handle fake-atoms!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
markAtom(atom_t a)
{ unsigned int i = indexAtom(a);
  Atom ap;

  if ( i >= entriesBuffer(&atom_array, Atom) )
    return;				/* not an atom */

  ap = fetchBuffer(&atom_array, i, Atom);

  if ( ap )
  {
#ifdef O_DEBUG_ATOMGC
    if ( atomLogFd )
      Sfprintf(atomLogFd, "Marked `%s' at (#%d)\n", ap->name, i);
#endif
    ap->references |= ATOM_MARKED_REFERENCE;
  }
}


void
collectAtoms()
{ int n = GD->atoms.builtin;
  Atom *ap = &baseBuffer(&atom_array, Atom)[n];
  int mx = entriesBuffer(&atom_array, Atom);
  int holes = 0;

  for( ; n < mx; ap++, n++)
  { Atom a = *ap;

    if ( !a )
    { if ( holes++ == 0 )
	GD->atoms.no_hole_before = n;
      continue;
    }

    if ( a->references == 0 &&
	 (!GD->atoms.gc_hook || (*GD->atoms.gc_hook)(a->atom)) )
    { Atom *ap2 = &atomTable[a->hash_value & (atom_buckets-1)];

					/* delete from hash-table */
      for( ; ; ap2 = &(*ap2)->next )
      { assert(*ap2);
	
	if ( *ap2 == a )
	{ *ap2 = a->next;
	  break;
	}
      }
      
      *ap = NULL;			/* delete from index array */
      GD->atoms.collected++;
      GD->statistics.atoms--;
      if ( holes++ == 0 )
	GD->atoms.no_hole_before = n;
#ifdef O_DEBUG_ATOMGC
      if ( atomLogFd )
	Sfprintf(atomLogFd, "Deleted `%s' at (#%d)\n", a->name, n);
#endif
      freeHeap(a->name, a->length+1);
      freeHeap(a, sizeof(*a));
    }

    a->references &= ~ATOM_MARKED_REFERENCE;
  }
}


word
pl_garbage_collect_atoms()
{ int verbose = trueFeature(TRACE_GC_FEATURE);
  long oldcollected = GD->atoms.collected;
  long oldheap = GD->statistics.heap;
  long freed;
  double t;
  sigset_t set;

  if ( gc_status.blocked )		/* Tricky things; avoid problems. */
    succeed;

  LOCK();
  if ( GD->atoms.gc_active )
  { UNLOCK();
    succeed;
  }
  GD->atoms.gc_active = TRUE;
  UNLOCK();				/* for the printMessage() */

  if ( verbose )
  {
#ifdef O_DEBUG_ATOMGC
/*
    Sdprintf("Starting ATOM-GC.  Stack:\n");
    systemMode(TRUE);
    backTrace(NULL, 5);
    systemMode(FALSE);
*/
#endif
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "agc", 1,
		   PL_CHARS, "start");
  }

  PL_LOCK(L_THREAD);
  LOCK();
  blockSignals(&set);
  t = CpuTime(CPU_USER);
  markAtomsOnStacks(LD);
#ifdef O_PLMT
  threadMarkAtomsOtherThreads();
#endif
  collectAtoms();
  GD->atoms.non_garbage = GD->statistics.atoms;
  t = CpuTime(CPU_USER) - t;
  GD->atoms.gc_time += t;
  GD->atoms.gc++;
  GD->atoms.gc_active = FALSE;
  freed = oldheap - GD->statistics.heap;
  GD->statistics.atomspacefreed += freed;
  GD->statistics.atomspace -= freed;
  unblockSignals(&set);
  UNLOCK();
  PL_UNLOCK(L_THREAD);
  
  if ( verbose )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "agc", 1,
		   PL_FUNCTOR_CHARS, "done", 3,
		     PL_LONG, GD->atoms.collected - oldcollected,
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
{ GD->atoms.gc_active = FALSE;
}


void
PL_register_atom(atom_t a)
{
#ifdef O_ATOMGC
  PL_LOCK(L_MISC);
  atomValue(a)->references++;
  PL_UNLOCK(L_MISC);
#endif
}


void
PL_unregister_atom(atom_t a)
{
#ifdef O_ATOMGC
  Atom p = atomValue(a);

  PL_LOCK(L_MISC);
  assert(p->references > 0);
  p->references--;
  PL_UNLOCK(L_MISC);
#endif
}

#define PL_register_atom error		/* prevent using them after this */
#define PL_unregister_atom error

		 /*******************************
		 *	    REHASH TABLE	*
		 *******************************/

static void
rehashAtoms()
{ Atom *oldtab   = atomTable;
  int   oldbucks = atom_buckets;
  long i, mx = entriesBuffer(&atom_array, Atom);

  startCritical;
  atom_buckets *= 2;
  atomTable = allocHeap(atom_buckets * sizeof(Atom));
  memset(atomTable, 0, atom_buckets * sizeof(Atom));
  
  DEBUG(0, Sdprintf("rehashing atoms (%d --> %d)\n", oldbucks, atom_buckets));

  for(i=0; i<mx; i++)
  { Atom a = baseBuffer(&atom_array, Atom)[i];
    int v = a->hash_value & (atom_buckets-1);

    a->next = atomTable[v];
    atomTable[v] = a;
  }

  freeHeap(oldtab, oldbucks * sizeof(Atom));
  endCritical;
}


word
pl_atom_hashstat(term_t idx, term_t n)
{ int i, m;
  Atom a;
  
  if ( !PL_get_integer(idx, &i) || i < 0 || i >= atom_buckets )
    fail;
  for(m = 0, a = atomTable[i]; a; a = a->next)
    m++;

  return PL_unify_integer(n, m);
}


static void
registerBuiltinAtoms()
{ int size = sizeof(atoms)/sizeof(char *) - 1;
  Atom a = allocHeap(size * sizeof(struct atom));
  const ccharp *s;

  GD->statistics.atoms = size;

  for(s = atoms; *s; s++, a++)
  { int len = strlen(*s);
    int v0 = unboundStringHashValue(*s, len);
    int v = v0 & (atom_buckets-1);

    a->name       = (char *)*s;
    a->length     = len;
#ifdef O_HASHTERM
    a->hash_value = v0;
#endif
    a->next       = atomTable[v];
    atomTable[v]  = a;
    registerAtom(a);
  }
}


#if O_DEBUG
static void
exitAtoms(int status, void *arg)
{ Sdprintf("hashstat: %d lookupAtom() calls used %d strcmp() calls\n",
	   lookups, cmps);
}
#endif


void
initAtoms(void)
{ LOCK();
  if ( !atomTable )
  { initMemAlloc();
    atom_buckets = ATOMHASHSIZE;
    atomTable = allocHeap(atom_buckets * sizeof(Atom));

    memset(atomTable, 0, atom_buckets * sizeof(Atom));
    initBuffer(&atom_array);
    registerBuiltinAtoms();
#ifdef O_ATOMGC
    GD->atoms.margin     = 10000;
    lockAtoms();
#endif

    DEBUG(0, PL_on_halt(exitAtoms, NULL));
  }
  UNLOCK();
}


void
cleanupAtoms(void)
{ discardBuffer(&atom_array);
}


word
pl_current_atom2(term_t a, term_t refs, word h)
{ unsigned int i;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_is_atom(a) )
      {
#ifdef O_ATOMGC
	if ( refs )
	{ Atom ap = atomValue(a);

	  return PL_unify_integer(refs,
				  ap->references & ~ATOM_MARKED_REFERENCE);
	}
#endif
	succeed;
      }
      if ( !PL_is_variable(a) )
	return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_atom, a);

      i = 0;
      break;
    case FRG_REDO:
      i = ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for( ; i < entriesBuffer(&atom_array, Atom); i++ )
  { Atom atom;

    if ( (atom = baseBuffer(&atom_array, Atom)[i]) )
    { 
#ifdef O_ATOMGC
      if ( refs &&
	   !PL_unify_integer(refs, atom->references & ~ATOM_MARKED_REFERENCE) )
	continue;
#endif
      PL_unify_atom(a, atom->atom);
      ForeignRedoInt(i+1);
    }
  }

  fail;
}


word
pl_current_atom(term_t a, word h)
{ return pl_current_atom2(a, 0, h);
}


		 /*******************************
		 *	 ATOM COMPLETION	*
		 *******************************/

#define ALT_SIZ 80		/* maximum length of one alternative */
#define ALT_MAX 256		/* maximum number of alternatives */
#define stringMatch(m)	((m)->name->name)

typedef struct match
{ Atom	name;
  int	length;
} *Match;


static bool
allAlpha(register char *s)
{ for( ; *s; s++)
  { if ( !isAlpha(*s) )
      fail;
  }
  succeed;
}


static int
extendAtom(char *prefix, bool *unique, char *common)
{ long i, mx = entriesBuffer(&atom_array, Atom);
  Atom a;
  bool first = TRUE;
  int lp = (int) strlen(prefix);
  
  *unique = TRUE;

  for(i=0; i<mx; i++)
  { a = baseBuffer(&atom_array, Atom)[i];

    if ( a && strprefix(a->name, prefix) )
    { if ( strlen(a->name) >= LINESIZ )
	continue;
      if ( first == TRUE )
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

  return !first;
}


word
pl_complete_atom(term_t prefix, term_t common, term_t unique)
{ char *p;
  bool u;
  char buf[LINESIZ];
  char cmm[LINESIZ];
    
  if ( !PL_get_chars(prefix, &p, CVT_ALL) )
    return warning("$complete_atom/3: instanstiation fault");
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


static bool
extend_alternatives(char *prefix, struct match *altv, int *altn)
{ long i, mx = entriesBuffer(&atom_array, Atom);
  Atom a;
  char *as;
  int l;

  *altn = 0;
  for(i=0; i<mx; i++)
  { if ( !(a = baseBuffer(&atom_array, Atom)[i]) )
      continue;
    
    as = a->name;
    if ( strprefix(as, prefix) &&
	 allAlpha(as) &&
	 (l = (int)strlen(as)) < ALT_SIZ )
    { Match m = &altv[(*altn)++];
      m->name = a;
      m->length = l;
      if ( *altn > ALT_MAX )
	break;
    }
  }
  
  qsort(altv, *altn, sizeof(struct match), compareMatch);

  succeed;
}


word
pl_atom_completions(term_t prefix, term_t alternatives)
{ char *p;
  char buf[LINESIZ];
  struct match altv[ALT_MAX];
  int altn;
  int i;
  term_t alts = PL_copy_term_ref(alternatives);
  term_t head = PL_new_term_ref();

  if ( !PL_get_chars(prefix, &p, CVT_ALL) )
    return warning("$atom_completions/2: instanstiation fault");
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
state. Horrible! We use the thread-local   structure to store the state,
so multiple Prolog threads can use this routine.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define is_signalled() (LD->pending_signals != 0)

char *
PL_atom_generator(const char *prefix, int state)
{ long i, mx = entriesBuffer(&atom_array, Atom);

  if ( !state )
    i = 0;
  else
    i = LD->atoms.generator;

  for(; i<mx; i++)
  { Atom a;

    if ( !(a = baseBuffer(&atom_array, Atom)[i]) )
      continue;
    
    if ( is_signalled() )		/* Notably allow windows version */
      PL_handle_signals();		/* to break out on ^C */

    if ( strprefix(a->name, prefix) &&
	 allAlpha(a->name) &&
	 strlen(a->name) < ALT_SIZ )
    { LD->atoms.generator = i+1;
      return a->name;
    }
  }

  return NULL;
}


