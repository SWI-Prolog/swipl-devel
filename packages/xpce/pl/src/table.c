/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

typedef struct asymbol *ASymbol;
typedef struct table   *Table;

struct asymbol
{ Atom		atom;
  PceName	name;
  ASymbol	next;
};

struct table
{ ASymbol      *symbols;
  int		allocated;
  int	        size;
  int		mask;
};

static struct table atom_to_name;
static struct table name_to_atom;

#define AtomKey(t, a) (((a)>>5) & (t)->mask)
#define NameKey(t, a) (((unsigned long)(a)>>2) & (t)->mask)

static void
rehashTable(Table t, int aton)
{ ASymbol *old   = t->symbols;
  int oldentries = t->allocated;
  int n;
  
  t->allocated *= 2;
  t->mask = t->allocated - 1;
  t->symbols    = malloc(t->allocated * sizeof(ASymbol));
  memset(t->symbols, 0, t->allocated * sizeof(ASymbol));

  for(n=0; n<oldentries; n++)
  { ASymbol s = old[n];
    ASymbol n;
    
    for( ; s; s = n )
    { int k;

      n = s->next;

      if ( aton )
	k = AtomKey(t, s->atom);
      else
	k = NameKey(t, s->name);
      
      s->next = t->symbols[k];
      t->symbols[k] = s;
    }
  }

  free(old);
}

static PceName
atomToName(Atom a)
{ int k = AtomKey(&atom_to_name, a);
  ASymbol s = atom_to_name.symbols[k];
  PceName name;
  unsigned int len;
  const char *text;
  
  for( ; s; s = s->next )
  { if ( s->atom == a )
      return s->name;
  }
       
  PL_register_atom(a);
  text = PL_atom_nchars(a, &len);
  name = cToPceName_n(text, len);
  s = pceAlloc(sizeof(struct asymbol));
  s->atom = a;
  s->name = name;
  s->next = atom_to_name.symbols[k];
  atom_to_name.symbols[k] = s;
  if ( ++atom_to_name.size > 2*atom_to_name.allocated )
    rehashTable(&atom_to_name, TRUE);

  return name;
}


static Atom
CachedNameToAtom(PceName name)
{ int k = NameKey(&name_to_atom, name);
  ASymbol s = name_to_atom.symbols[k];
  Atom a;
  unsigned int len;
  char *text;

  for( ; s; s = s->next )
  { if ( s->name == name )
      return s->atom;
  }
       
  text = pceCharArrayToC(name, &len);
  a = PL_new_atom_nchars(len, text);
  s = pceAlloc(sizeof(struct asymbol));
  s->atom = a;
  s->name = name;
  s->next = name_to_atom.symbols[k];
  name_to_atom.symbols[k] = s;
  if ( ++name_to_atom.size > 2*name_to_atom.allocated )
    rehashTable(&name_to_atom, FALSE);

  return a;
}


static void
initTable(Table t)
{ t->allocated = 1024;
  t->size      = 0;
  t->mask      = t->allocated-1;
  t->symbols   = malloc(t->allocated * sizeof(ASymbol));
  memset(t->symbols, 0, t->allocated * sizeof(ASymbol));
}


static void
initNameAtomTable()
{ initTable(&atom_to_name);
  initTable(&name_to_atom);
}
