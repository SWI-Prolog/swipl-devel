/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: atom management
*/

#include "pl-incl.h"
#include "pl-ctype.h"

static Atom atomTable[ATOMHASHSIZE];

Atom
lookupAtom(s)
char *s;
{ int v = stringHashValue(s, ATOMHASHSIZE);
  register Atom a;

  for(a = atomTable[v]; a && !isRef((word)a); a = a->next)
  { if (streq(s, a->name) )
      return a;
  }
  a = (Atom)allocHeap(sizeof(struct atom));
  a->next = atomTable[v];
  a->type = ATOM_TYPE;
  a->name = store_string(s);
  atomTable[v] = a;
  statistics.atoms++;

  return a;
}


word
pl_atom_hashstat(i, n)
Word i, n;
{ int m;
  register Atom a;

  if ( !isInteger(*i) || valNum(*i) < 0 || valNum(*i) >= ATOMHASHSIZE )
    fail;
  for(m = 0, a = atomTable[valNum(*i)]; a && !isRef((word)a); a = a->next)
    m++;

  return unifyAtomic(n, consNum(m));
}


struct atom atoms[] = {
#include "pl-atom.ic"
{ (Atom)NULL,	ATOM_TYPE,	(char *)NULL }
};

/* Note that the char * of the atoms is copied to the data segment.  This
   is done because some functions temporary change the char string associated
   with an atom (pl_concat_atom()) and GCC puts char constants in the text
   segment.
*/

void
initAtoms()
{ register int n;

  { register Atom *a;
    for(n=0, a=atomTable; n < (ATOMHASHSIZE-1); n++, a++)
      *a = (Atom) makeRef(a+1);
  }

  { register Atom a;
    register int v;

    for( a = &atoms[0]; a->name; a++)
    { a->name = store_string(a->name);
      v = stringHashValue(a->name, ATOMHASHSIZE);
      a->next = atomTable[v];
      atomTable[v] = a;
      statistics.atoms++;
    }
  }    
}


word
pl_current_atom(a, h)
Word a;
word h;
{ Atom atom;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( isAtom(*a) ) succeed;
      if ( !isVar(*a) ) fail;

      atom = atomTable[0];
      break;
    case FRG_REDO:
      atom = (Atom) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; atom; atom = atom->next)
  { while(isRef((word)atom) )
    { atom = *((Atom *)unRef(atom));
      if (atom == (Atom) NULL)
	fail;
    }
    if (unifyAtomic(a, atom) == FALSE)
      continue;

    return_next_table(Atom, atom);
  }

  fail;
}

#ifdef O_EXTEND_ATOMS

#define ALT_MAX 256		/* maximum number of alternatives */
#define ALT_SIZ 80		/* maximum length of one alternative */

#define IS_MODULE	0x1
#define IS_FUNCTOR	0x2
#define IS_PROCEDURE	0x4

#define stringMatch(m)	(stringAtom((m)->name))

typedef struct match *Match;

struct match
{ Atom	name;
  short length;
  short	flags;
};

forwards bool 	allAlpha P((char *));
forwards short	type_flags P((Atom));
forwards void	listAtoms P((int, Match));

char *
extendAtom(prefix, unique)
char *prefix;
bool *unique;
{ Atom a = atomTable[0];
  bool first = TRUE;
  static char common[LINESIZ];
  int lp = (int) strlen(prefix);

  *unique = TRUE;

  for(; a; a = a->next)
  { while( isRef((word)a) )
    { a = *((Atom *)unRef(a));
      if ( a == (Atom)NULL)
	goto out;
    }
    if ( strprefix(stringAtom(a), prefix) )
    { if ( strlen(stringAtom(a)) >= LINESIZ )
	continue;
      if ( first == TRUE )
      { strcpy(common, stringAtom(a)+lp);
	first = FALSE;
      } else
      { char *s = common;
	char *q = stringAtom(a)+lp;
	while( *s && *s == *q )
	  s++, q++;
	*s = EOS;
	*unique = FALSE;
      }
    }
  }

out:
  return first == TRUE ? (char *)NULL : common;
}


word
pl_complete_atom(prefix, common, unique)
Word prefix, common, unique;
{ char *p, *s;
  bool u;
  char buf[LINESIZ];
    
  if ( (p = toString(*prefix)) == NULL )
    return warning("$complete_atom/3: instanstiation fault");
  strcpy(buf, p);
  if ( (s = extendAtom(p, &u)) != NULL )
  { strcat(buf, s);
    TRY(unifyStringWithList(buf, common));
    return unifyAtomic(unique, u ? ATOM_unique : ATOM_not_unique);
  }

  fail;
}


static int
compareMatch(m1, m2)
Match m1, m2;
{ return strcmp(stringMatch(m1), stringMatch(m2));
}


static short
type_flags(name)
Atom name;
{ short flags;

  flags = 0;
  if ( isCurrentModule(name) != (Module) NULL )
    flags |= IS_MODULE;
  if ( atomIsFunctor(name) >= 0 )
  { flags |= IS_FUNCTOR;
    if ( atomIsProcedure(name) )
      flags |= IS_PROCEDURE;
  }

  return flags;
}


static void
listAtoms(nm, vm)
int nm;
Match vm;
{ int maxlength = 8;
  int columns, colwidth;
  int rows, spaces;
  int n, m, r, c;
 
  for(n = 0; n < nm; n++ )
    if ( vm[n].length+3 > maxlength )
      maxlength = vm[n].length+3;

  columns = 78 / maxlength;
  if ( columns == 0 )
    columns = 1;
  colwidth = 78 / columns;
  rows = (nm + columns - 1) / columns;

  for(r = 0; r < rows; r++)
  { for(c = 0; c < columns; c++)
    { n = c * rows + r;
      if ( n >= nm )
        continue;
      Putf("%s", stringMatch(&vm[n]));
      spaces = colwidth - vm[n].length;
      if ( vm[n].flags & IS_MODULE )
      { Put(':');
	spaces--;
      }
      if ( vm[n].flags & IS_PROCEDURE )
      { Put('*');
	spaces--;
      } else
      { if ( vm[n].flags & IS_FUNCTOR )
	{ Put('/');
	  spaces--;
	}
      }
      for( m=0; m < spaces; m++ )
        Put(' ');
    }
    Put('\n');
  }
}

static bool
allAlpha(s)
register char *s;
{ for( ; *s; s++)
   if ( !isAlpha(*s) )
     fail;

  succeed;
}

bool
extend_alternatives(prefix, altv, altn)
char *prefix;
struct match *altv;
int *altn;
{ Atom a = atomTable[0];
  char *as;
  int l;

  *altn = 0;
  for(; a; a=a->next)
  { while( a && isRef((word)a) )
      a = *((Atom *)unRef(a));
    if ( a == (Atom) NULL )
      break;
    
    as = stringAtom(a);
    if ( strprefix(as, prefix) &&
	 allAlpha(as) &&
	 (l = (int)strlen(as)) < ALT_SIZ )
    { Match m = &altv[(*altn)++];
      m->name = a;
      m->length = l;
      m->flags = type_flags(m->name);
      if ( *altn > ALT_MAX )
	break;
    }
  }
  
  qsort(altv, *altn, sizeof(struct match), compareMatch);

  succeed;
}

bool
extendAlternatives(prefix)
char *prefix;
{ struct match altv[ALT_MAX];
  int altn;

  extend_alternatives(prefix, altv, &altn);
  listAtoms(altn, altv);

  succeed;
}

word
pl_atom_completions(prefix, alts)
Word prefix, alts;
{ char *p;
  char buf[LINESIZ];
  struct match altv[ALT_MAX];
  int altn;
  int i;

  if ( (p = toString(*prefix)) == NULL )
    return warning("$atom_completions/2: instanstiation fault");
  strcpy(buf, p);

  extend_alternatives(buf, altv, &altn);
  
  for(i=0; i<altn; i++)
  { TRY(unifyFunctor(alts, FUNCTOR_dot2));
    TRY(unifyAtomic(argTermP(*alts, 0), altv[i].name));
    alts = argTermP(*alts, 1);
    deRef(alts);
  }
  return unifyAtomic(alts, ATOM_nil);
} 

#endif O_EXTEND_ATOMS
