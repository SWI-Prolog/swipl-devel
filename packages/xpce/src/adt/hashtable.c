/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>

static status	unlinkHashTable(HashTable ht);

#define ASSIGN(ht, field, value) \
	if ( ht->refer == ON ) \
	  assignField((Instance)(ht), (Any *) &(field), (Any)value); \
	else \
	  field = (value)

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
createHashTable(Int buckets, Bool refer)
{ HashTable ht = alloc(sizeof(struct hash_table));

  initHeaderObj(ht, ClassHashTable);
  ht->refer = NIL;
  initialiseHashTable(ht, buckets);
  assign(ht, refer, refer);
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

  assign(ht, refer, ON);

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
  { if ( ht->refer == ON )
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
loadHashTable(HashTable ht, FILE *fd, ClassDef def)
{ int buckets;
  Symbol s;

  loadSlotsObject(ht, fd, def);
  
  buckets = (isNil(ht->size) ? 5 : ((valInt(ht->size) * 4) / 3 + 4));
  buckets = nextBucketSize(buckets);
  if ( isNil(ht->refer) )
    assign(ht, refer, ON);

  assign(ht, size, ZERO);
  ht->buckets = buckets;
  ht->symbols = alloc(buckets * sizeof(struct symbol));

  for(s = ht->symbols; s < &ht->symbols[ht->buckets]; s++)
    s->name = s->value = NULL;

  for(;;)
  { int c;

    switch(c=getc(fd))
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
			toInt(c), toInt(ftell(fd)));
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
  Bool	 refer = ht->refer;
  int    n;
  Symbol s;

  bs = max(bs, (4*valInt(ht->size))/3);
  bs = nextBucketSize(bs);
  ht->size    = ZERO;
  ht->buckets = bs;
  ht->symbols = alloc(bs * sizeof(struct symbol));
  ht->refer   = OFF;

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
    { ASSIGN(ht, s->value, value);
      succeed;
    }
    if ( s->name == NULL )
    { s->name = s->value = NIL;
      ASSIGN(ht, s->name, name);
      ASSIGN(ht, s->value, value);
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
  { ASSIGN(ht, ht->symbols[i].name, NIL); \
    ASSIGN(ht, ht->symbols[i].value, NIL); \
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
  { ASSIGN(ht, s->name, NIL);
    ASSIGN(ht, s->value, NIL);
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
  { Symbol symbols = alloca(sizeof(struct symbol) * valInt(ht->size));
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
  { Symbol symbols = alloca(sizeof(struct symbol) * valInt(ht->size));
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

status
makeClassHashTable(Class class)
{ sourceClass(class, makeClassHashTable, __FILE__, "$Revision$");

  localClass(class, NAME_refer, NAME_oms, "bool", NAME_none,
	     "If @off, registers no references (internal)");
  localClass(class, NAME_size, NAME_cardinality,  "int", NAME_get,
	     "Number of symbols in table");
  localClass(class, NAME_buckets, NAME_cardinality, "alien:int", NAME_none,
	     "Number of buckets in the table");
  localClass(class, NAME_symbols, NAME_storage, "alien:Symbol", NAME_none,
	     "Array of symbols");

  termClass(class, "hash_table", 1, NAME_buckets);
  setLoadStoreFunctionClass(class, loadHashTable, storeHashTable);
/*setCloneFunctionClass(class, cloneHashTable);
  setChangedFunctionClass(class, changedHashTable);
*/

  sendMethod(class, NAME_initialise, DEFAULT, 1, "buckets=[int]",
	     "Create from buckets",
	     initialiseHashTable);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Clear table",
	     unlinkHashTable);
  sendMethod(class, NAME_append, NAME_add, 2, "key=any", "value=any",
	     "Append association to table",
	     appendHashTable);
  sendMethod(class, NAME_clear, NAME_delete, 0,
	     "Delete all entries",
	     clearHashTable);
  sendMethod(class, NAME_empty, NAME_test, 0,
	     "Test if hash_table has no elements",
	     emptyHashTable);
  sendMethod(class, NAME_delete, NAME_delete, 1, "key=any",
	     "Delete all matching symbol",
	     deleteHashTable);
  sendMethod(class, NAME_buckets, NAME_storage, 1, "int",
	     "Number of buckets in the table",
	     bucketsHashTable);
  sendMethod(class, NAME_convertOldSlot, NAME_compatibility, 2, "name", "any",
	     "File <-object conversion",
	     convertOldSlotHashTable);
#if O_COUNT
  sendMethod(class, NAME_printStatistics, NAME_statistics, 0,
	     "Print statistics on all tables",	
	     printStatisticsHashTable);
#endif
#ifndef O_RUNTIME
  sendMethod(class, NAME_info, NAME_statistics, 0,
	     "Check consistency and print statistics",
	     infoHashTable);
#endif


  sendMethod(class, NAME_forAll, NAME_iterate, 2, "action=code", "safe=[bool]",
	     "Run code on all values; demand acceptance ([safe])",
	     forAllHashTable);
  sendMethod(class, NAME_forSome, NAME_iterate, 2,
	     "action=code", "safe=[bool]",
	     "Run code on all values ([safe])",
	     forSomeHashTable);

  getMethod(class, NAME_member, NAME_lookup, "value=any", 1, "key=any",
	    "Get associated value",
	    getMemberHashTable);
  getMethod(class, NAME_buckets, NAME_storage, "buckets=int", 0,
	    "Number of buckets in the table",
	    getBucketsHashTable);
  getMethod(class, NAME_findKey, NAME_search, "key=any", 1, "test=code",
	    "Find key accepted by code",
	    getFindKeyHashTable);
  getMethod(class, NAME_findValue, NAME_search, "value=any", 1, "test=code",
	    "Find value accepted by code",
	    getFindValueHashTable);

#if O_COUNT
  getMethod(class, NAME_shifts, NAME_statistics, "int", 0,
	    "Number of shifted entries",
	    getShiftsHashTable);
#endif

  succeed;
}
