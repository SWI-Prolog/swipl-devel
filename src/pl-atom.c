/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: atom management
    MT-status: SAFE
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

static void
registerAtom(Atom a)
{ int n = entriesBuffer(&atom_array, Atom);
    
  a->atom = (n<<LMASK_BITS)|TAG_ATOM;

  addBuffer(&atom_array, a, Atom);
}


		 /*******************************
		 *	  GENERAL LOOKUP	*
		 *******************************/

word
lookupAtom(const char *s)
{ int v0 = unboundStringHashValue(s);
  int v = v0 & (atom_buckets-1);
  Atom a;

  LOCK();
  DEBUG(0, lookups++);

  for(a = atomTable[v]; a; a = a->next)
  { DEBUG(0, cmps++);
    if (streq(s, a->name) )
    { UNLOCK();
      return a->atom;
    }
  }
  a = (Atom)allocHeap(sizeof(struct atom));
  a->name       = store_string(s);
#ifdef O_HASHTERM
  a->hash_value = v0;
#endif
  registerAtom(a);
  a->next       = atomTable[v];
  atomTable[v]  = a;
  GD->statistics.atoms++;

  if ( atom_buckets * 2 < GD->statistics.atoms )
    rehashAtoms();
  UNLOCK();
  
  return a->atom;
}


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
  { int v0 = unboundStringHashValue(*s);
    int v = v0 & (atom_buckets-1);

    a->name       = (char *)*s;
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
{ static int done = FALSE;

  LOCK();
  if ( !done )
  { done = TRUE;

    initMemAlloc();
    atom_buckets = ATOMHASHSIZE;
    atomTable = allocHeap(atom_buckets * sizeof(Atom));

    memset(atomTable, 0, atom_buckets * sizeof(Atom));
    initBuffer(&atom_array);
    registerBuiltinAtoms();

    DEBUG(0, PL_on_halt(exitAtoms, NULL));
  }
  UNLOCK();
}


word
pl_current_atom(term_t a, word h)
{ unsigned int i;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( PL_is_atom(a) )
	succeed;
      if ( !PL_is_variable(a) )
	return PL_error("current_atom", 1, NULL, ERR_DOMAIN, ATOM_atom, a);

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
      PL_unify_atom(a, atom->atom);

    ForeignRedoInt(i+1);
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
	 PL_unify_atom(unique, u ? ATOM_unique : ATOM_not_unique) )
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

char *
PL_atom_generator(char *prefix, int state)
{ long i, mx = entriesBuffer(&atom_array, Atom);

  if ( !state )
    i = 0;
  else
    i = LD->atoms.generator;

  for(; i<mx; i++)
  { Atom a;

    if ( !(a = baseBuffer(&atom_array, Atom)[i]) )
      continue;
    
    if ( strprefix(a->name, prefix) &&
	 allAlpha(a->name) &&
	 strlen(a->name) < ALT_SIZ )
    { LD->atoms.generator = i+1;
      return a->name;
    }
  }

  return NULL;
}


