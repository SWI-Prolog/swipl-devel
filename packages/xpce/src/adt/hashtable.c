/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>

static status	unlinkHashTable(HashTable ht);

static __inline void
assign_symbol_value(HashTable ht, Symbol symbol, Any value)
{ if ( ht->refer == NAME_both || ht->refer == NAME_value )
    assignField((Instance)(ht), &symbol->value, value); 
  else
    symbol->value = value;
}

static __inline void
assign_symbol_name(HashTable ht, Symbol symbol, Any name)
{ if ( ht->refer == NAME_both || ht->refer == NAME_name )
    assignField((Instance)(ht), &symbol->name, name); 
  else
    symbol->name = name;
}


#if USE_PRIMES
static int
nextBucketSize(n)
int n;
{ if ( !(n % 2) )
    n++;

  for(;; n += 2)
  { int m;
    int to = isqrt(n);

    for(m=3 ; m <= to; m += 2)
      if ( !(n % m) )
	break;

    if ( m > to )
      return n;
  }
}

#else

static int
nextBucketSize(int n)
{ int m;

  for(m=2; m<n; m<<=1);
    return m;
}
#endif


HashTable
createHashTable(Int buckets, Name refer)
{ HashTable ht = alloc(sizeof(struct hash_table));

  initHeaderObj(ht, ClassHashTable);
  initialiseHashTable(ht, buckets);
  ht->refer = refer;			/* is a protected object */
  createdObject(ht, NAME_new);

  return ht;
}


status
freeHashTable(HashTable ht)
{ unlinkHashTable(ht);
  unalloc(sizeof(struct hash_table), ht);

  succeed;
}


status
initialiseHashTable(HashTable ht, Int buckets)
{ int n = isDefault(buckets) ? 5 : valInt(buckets);
  Symbol s;

  ht->refer = NAME_both;
  n = nextBucketSize(n);
  ht->size    = ZERO;
  ht->buckets = n;
  ht->symbols = alloc(n * sizeof(struct symbol));

  for(s = ht->symbols; s < &ht->symbols[ht->buckets]; s++)
    s->name = s->value = NULL;

  succeed;
}


static status
unlinkHashTable(HashTable ht)
{ if ( ht->symbols != NULL )
  { if ( ht->refer != NAME_none )
      clearHashTable(ht);

    unalloc(ht->buckets * sizeof(struct symbol), ht->symbols);
    ht->symbols = NULL;
  }
  
  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
load/save format:

<hashtable>		::= <pce-slots> {<symbol>} 'X'

<symbol>		::= 's' <key> <value>
<key>			::= <any>
<value>			::= <any>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static status
storeHashTable(HashTable ht, FileObj file)
{ TRY(storeSlotsObject(ht, file));

  for_hash_table(ht, s,
	       { storeCharFile(file, 's');
		 storeObject(s->name, file);
		 storeObject(s->value, file);
	       });

  storeCharFile(file, 'X');

  succeed;
}


static status
loadHashTable(HashTable ht, IOSTREAM *fd, ClassDef def)
{ int buckets;
  Symbol s;

  loadSlotsObject(ht, fd, def);
  
  buckets = (isNil(ht->size) ? 5 : ((valInt(ht->size) * 4) / 3 + 4));
  buckets = nextBucketSize(buckets);
  if ( isNil(ht->refer) || isOn(ht->refer) )
    assign(ht, refer, NAME_both);

  assign(ht, size, ZERO);
  ht->buckets = buckets;
  ht->symbols = alloc(buckets * sizeof(struct symbol));

  for(s = ht->symbols; s < &ht->symbols[ht->buckets]; s++)
    s->name = s->value = NULL;

  for(;;)
  { int c;

    switch(c=Sgetc(fd))
    { case 's':
	{ Any key, value;

	  TRY( key   = loadObject(fd) );
	  TRY( value = loadObject(fd) );
	  if ( restoreVersion < 8 && instanceOfObject(ht, ClassChainTable) )
	    appendChainTable((ChainTable) ht, key, value);
	  else
	    appendHashTable(ht, key, value);

	  break;
	}
      case 'X':
	succeed;
      default:
	return errorPce(LoadFile, NAME_illegalCharacter,
			toInt(c), toInt(Stell(fd)));
    }
  }
}


static status
convertOldSlotHashTable(HashTable ht, Name slot, Any value)
{ if ( slot == NAME_unique && value == OFF )
  { errorPce(ht, NAME_loadMessage,
	     CtoString("Migrating to a chain_table"));
    ht->class = ClassChainTable;	/* Brrrrrr! */

    succeed;
  } else
  { Any argv[2];

    argv[0] = slot;
    argv[1] = value;
    return vm_send(ht, NAME_convertOldSlot, ht->class->super_class, 2, argv);
  }
}


#ifndef O_RUNTIME
static int
checkMemberHashTable(const HashTable ht, const Any name, const Any value)
{ int hashkey = hashKey(name, ht->buckets);
  Symbol s = &ht->symbols[hashkey];
  int shifts=0;

  for(;;)
  { if ( s->name == name )
    { assert(s->value == value);
      return shifts;
    }
    if ( !s->name )
      fail;
    shifts++;
    if ( ++hashkey == ht->buckets )
    { hashkey = 0;
      s = ht->symbols;
    } else
      s++;
  }

  fail;
}

static status
infoHashTable(HashTable ht)
{ Symbol s = ht->symbols;
  int n = ht->buckets;
  int shifts = 0;
  int members = 0;

  for( ; --n >= 0; s++ )
  { if ( s->name )
    { shifts += checkMemberHashTable(ht, s->name, s->value);
      members++;
    }
  }

  Cprintf("%s: %d buckets holding %d members, %d shifts\n",
	  pp(ht), ht->buckets, members, shifts);
  
  succeed;
}
#endif /*O_RUNTIME*/


static status
bucketsHashTable(HashTable ht, Int buckets)
{ int    bs    = valInt(buckets);
  Symbol old   = ht->symbols;
  int    size  = ht->buckets;
  Name	 refer = ht->refer;
  int    n;
  Symbol s;

  bs = max(bs, (4*valInt(ht->size))/3);
  bs = nextBucketSize(bs);
  ht->size    = ZERO;
  ht->buckets = bs;
  ht->symbols = alloc(bs * sizeof(struct symbol));
  ht->refer   = NAME_none;

  for( n=ht->buckets, s=ht->symbols; n-- > 0; s++ )
    s->name = s->value = NULL;

  for( n = size, s = old; n-- > 0; s++)
    if ( s->name )
      appendHashTable(ht, s->name, s->value);
  
  ht->refer = refer;
  unalloc(size * sizeof(struct symbol), old);

  COUNT(hash_resizes++);

  succeed;
}


status
appendHashTable(HashTable ht, Any name, Any value)
{ int hashkey;
  Symbol s;

  if ( 4 * valInt(ht->size) + 5 > 3 * ht->buckets )
    bucketsHashTable(ht, toInt(2*ht->buckets));

  hashkey = hashKey(name, ht->buckets);
  s       = &ht->symbols[hashkey];

  for(;;)
  { if ( s->name == name )
    { assign_symbol_value(ht, s, value);
      succeed;
    }
    if ( s->name == NULL )
    { s->name = s->value = NIL;
      assign_symbol_name(ht, s, name);
      assign_symbol_value(ht, s, value);
      assign(ht, size, add(ht->size, ONE));
      succeed;
    } 
    if ( ++hashkey == ht->buckets )
    { hashkey = 0;
      s = ht->symbols;
    } else
      s++;
  }
  /*NOTREACHED*/
}


#define EMPTY(ht, i) \
  { assign_symbol_name(ht, &ht->symbols[i], NIL); \
    assign_symbol_value(ht, &ht->symbols[i], NIL); \
    ht->symbols[i].name = ht->symbols[i].value = NULL; \
  }

status
deleteHashTable(HashTable ht, Any name)
{ int i = hashKey(name, ht->buckets);
  int j, r;

  while( ht->symbols[i].name && ht->symbols[i].name != name )
    if ( ++i == ht->buckets )
      i = 0;
  if ( !ht->symbols[i].name )
    fail;				/* not in table */

  assign(ht, size, sub(ht->size, ONE));

  EMPTY(ht, i);				/* R1 */
  j = i;

  for(;;)
  { if ( ++i == ht->buckets )		/* R2 */
      i = 0;

    if ( !ht->symbols[i].name )
      succeed;
    
    r = hashKey(ht->symbols[i].name, ht->buckets);
    if ( (i >= r && r > j) || (r > j && j > i) || (j > i && i >= r) )
      continue;

    ht->symbols[j] = ht->symbols[i];
    ht->symbols[i].name = ht->symbols[i].value = NULL;
    j = i;
  }
}


status
clearHashTable(HashTable ht)
{ int n;
  Symbol s;

  for(n=0, s = ht->symbols; n<ht->buckets; n++, s++)
  { assign_symbol_name(ht, s, NIL);
    assign_symbol_value(ht, s, NIL);
    s->name = s->value = NULL;
  }

  ht->size = ZERO;

  succeed;
}


		/********************************
		*      ACCESS TO C-SLOTS	*
		********************************/

static status
emptyHashTable(HashTable ht)
{ return ht->size == ZERO ? SUCCEED : FAIL;
}


static Int
getBucketsHashTable(HashTable ht)
{ answer(toInt(ht->buckets));
}


		/********************************
		*             FOR		*
		********************************/

static status
forAllHashTable(HashTable ht, Code code, Bool safe)
{ int n, size = ht->buckets;
  Symbol s;

  if ( safe == OFF )
  { for(n=size, s=ht->symbols; n-->0; s++)
      if ( s->name )
	TRY(forwardCode(code, s->name, s->value, 0));
  } else
  { Symbol symbols = (Symbol)alloca(sizeof(struct symbol) * valInt(ht->size));
    Symbol q = symbols;

    for(n=size, s=ht->symbols; n-- > 0; s++)
      if ( s->name )
      	*q++ = *s;

    for(n=valInt(ht->size), q=symbols; n-- > 0; q++)
      if ( (nonObject(q->name) || !isFreedObj(q->name)) &&
	   (nonObject(q->value) || !isFreedObj(q->value)) )
	TRY(forwardCode(code, q->name, q->value, 0));
  }

  succeed;
}


static status
forSomeHashTable(HashTable ht, Code code, Bool safe)
{ int n, size = ht->buckets;
  Symbol s;

  if ( safe == OFF )
  { for(n=size, s=ht->symbols; n-->0; s++)
      if ( s->name )
	forwardCode(code, s->name, s->value, 0);
  } else
  { Symbol symbols = (Symbol)alloca(sizeof(struct symbol) * valInt(ht->size));
    Symbol q = symbols;

    for(n=size, s=ht->symbols; n-- > 0; s++)
      if ( s->name )
      	*q++ = *s;

    for(n=valInt(ht->size), q=symbols; n-- > 0; q++)
      if ( (nonObject(q->name) || !isFreedObj(q->name)) &&
	   (nonObject(q->value) || !isFreedObj(q->value)) )
	forwardCode(code, q->name, q->value, 0);
  }

  succeed;
}


static Any
getFindKeyHashTable(HashTable ht, Code code)
{ int n, size = ht->buckets;
  Symbol s;

  for(n=size, s=ht->symbols; n-->0; s++)
    if ( s->name )
      if ( forwardCode(code, s->name, s->value, 0) )
	answer(s->name);

  fail;
}


static Any
getFindValueHashTable(HashTable ht, Code code)
{ int n, size = ht->buckets;
  Symbol s;

  for(n=size, s=ht->symbols; n-->0; s++)
    if ( s->name )
      if ( forwardCode(code, s->name, s->value, 0) )
	answer(s->value);

  fail;
}


#if O_COUNT

static status
printStatisticsHashTable(HashTable ht)
{ Cprintf("Total hash_table statistics:\n");
  Cprintf("\t# resizes:    %d\n", hash_resizes);
  Cprintf("\t# lookups:    %d\n", hash_lookups);
  Cprintf("\t# mismatches: %d\n", hash_cmp_failed);

  succeed;
}


static Int
getShiftsHashTable(HashTable ht)
{ long old_cmps = hash_cmp_failed;
  int n, size = ht->buckets;
  Symbol s;

  for(n=size, s=ht->symbols; n-->0; s++)
    if ( s->name )
      getMemberHashTable(ht, s->name);

  answer(toInt(hash_cmp_failed - old_cmps));
}

#endif /* O_COUNT */

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_actionAcode_safeADboolD[] =
        { "action=code", "safe=[bool]" };
static char *T_append[] =
        { "key=any", "value=any" };
static char *T_convertOldSlot[] =
        { "name", "any" };

/* Instance Variables */

static vardecl var_hashTable[] =
{ IV(NAME_refer, "{none,name,value,both}", IV_NONE,
     NAME_oms, "Determines which fields create references (internal)"),
  IV(NAME_size, "int", IV_GET,
     NAME_cardinality, "Number of symbols in table"),
  IV(NAME_buckets, "alien:int", IV_NONE,
     NAME_cardinality, "Number of buckets in the table"),
  IV(NAME_symbols, "alien:Symbol", IV_NONE,
     NAME_storage, "Array of symbols")
};

/* Send Methods */

static senddecl send_hashTable[] =
{ SM(NAME_initialise, 1, "buckets=[int]", initialiseHashTable,
     DEFAULT, "Create from buckets"),
  SM(NAME_unlink, 0, NULL, unlinkHashTable,
     DEFAULT, "Clear table"),
  SM(NAME_append, 2, T_append, appendHashTable,
     NAME_add, "Append association to table"),
  SM(NAME_convertOldSlot, 2, T_convertOldSlot, convertOldSlotHashTable,
     NAME_compatibility, "File <-object conversion"),
  SM(NAME_clear, 0, NULL, clearHashTable,
     NAME_delete, "Delete all entries"),
  SM(NAME_delete, 1, "key=any", deleteHashTable,
     NAME_delete, "Delete all matching symbol"),
  SM(NAME_forAll, 2, T_actionAcode_safeADboolD, forAllHashTable,
     NAME_iterate, "Run code on all values; demand acceptance ([safe])"),
  SM(NAME_forSome, 2, T_actionAcode_safeADboolD, forSomeHashTable,
     NAME_iterate, "Run code on all values ([safe])"),
#ifndef O_RUNTIME
  SM(NAME_info, 0, NULL, infoHashTable,
     NAME_statistics, "Check consistency and print statistics"),
#endif
#if O_COUNT
  SM(NAME_printStatistics, 0, NULL, printStatisticsHashTable,
     NAME_statistics, "Print statistics on all tables"),
#endif
  SM(NAME_buckets, 1, "int", bucketsHashTable,
     NAME_storage, "Number of buckets in the table"),
  SM(NAME_empty, 0, NULL, emptyHashTable,
     NAME_test, "Test if hash_table has no elements")
};

/* Get Methods */

static getdecl get_hashTable[] =
{ GM(NAME_member, 1, "value=any", "key=any", getMemberHashTable,
     NAME_lookup, "Get associated value"),
  GM(NAME_findKey, 1, "key=any", "test=code", getFindKeyHashTable,
     NAME_search, "Find key accepted by code"),
  GM(NAME_findValue, 1, "value=any", "test=code", getFindValueHashTable,
     NAME_search, "Find value accepted by code"),
#if O_COUNT
  GM(NAME_shifts, 0, "int", NULL, getShiftsHashTable,
     NAME_statistics, "Number of shifted entries"),
#endif
  GM(NAME_buckets, 0, "buckets=int", NULL, getBucketsHashTable,
     NAME_storage, "Number of buckets in the table")
};

/* Resources */

#define rc_hashTable NULL
/*
static classvardecl rc_hashTable[] =
{ 
};
*/

/* Class Declaration */

static Name hashTable_termnames[] = { NAME_buckets };

ClassDecl(hashTable_decls,
          var_hashTable, send_hashTable, get_hashTable, rc_hashTable,
          1, hashTable_termnames,
          "$Rev$");


status
makeClassHashTable(Class class)
{ declareClass(class, &hashTable_decls);

  setLoadStoreFunctionClass(class, loadHashTable, storeHashTable);

  succeed;
}
