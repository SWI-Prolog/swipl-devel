/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define NO_BUILT_IN_DECL
#include <h/kernel.h>
#include <h/trace.h>

#define BENCHNAMES 0			/* include benchmark code */

static void	insertName(Name);
static void	deleteName(Name);
static Name	getLookupName(Class, CharArray);
static status	registerName(Name n);


		/********************************
		*         BUILTIN NAMES		*
		********************************/

#define BUILTIN_NAME(s) { 0L, 0L, NULL, 0, s },

NewClass(bname)
  ulong str_header;
  char *text;
End;

struct bname builtin_names[] =
{
#include <h/names.ic>
  { 0L, 0L, NULL, 0, NULL }
};


void
trapGdb(void)
{
}


static int
nextBucketSize(n)
int n;
{ n *= 2;

  if ( !(n % 2) )
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



		/********************************
		*          HASH-TABLE		*
		********************************/

static int	buckets = 2048;		/* initial size */
static int      names = 0;		/* number of names */
static Name    *name_table;
static int	builtins;		/* number of builtin names */

static inline int
stringHashValue(String s)
{ unsigned int value = 0;
  unsigned int shift = 5;
  int size = s->size;
  char8 *t = s->s_text8;

  if ( isstr16(s) )
    size *= 2;
    
  while(--size >= 0)
  { unsigned int c = *t++;
    
    c -= 'a';
    value ^= c << shift;
    shift += 3;
    if ( shift > 24 )
      shift = 1;
  }

  return value % buckets;
}


static Int
getHashValueName(Name name)
{ answer(toInt(stringHashValue(&name->data)));
}


static Name
getBucketValueName(Name name, Int bucket)
{ if ( valInt(bucket) < buckets && name_table[valInt(bucket)] )
    answer(name_table[valInt(bucket)]);

  fail;
}



static void
rehashNames(void)
{ int old_buckets = buckets;
  Name *old_table = name_table;
  Name *nm;
  int n;

  buckets = nextBucketSize(buckets);
  DEBUG((Name)NAME_name, printf("Rehashing names ... "); fflush(stdout));
  name_table = malloc(buckets * sizeof(Name));
  for(n=buckets, nm = name_table; n-- > 0; nm++)
    *nm = NULL;

  names = 0;
  for(n=old_buckets, nm=old_table; n-- > 0; nm++)
    if ( *nm ) 
      insertName(*nm);

  DEBUG((Name)NAME_name, printf("done\n"));

  free(old_table);
}


static void
insertName(Name name)
{ Name *nm;
  Name *end;

  if ( 5*names > 3*buckets )
    rehashNames();

  nm = &name_table[stringHashValue(&name->data)];
  end = &name_table[buckets];

  while( *nm != NULL )
  { if ( ++nm == end )
      nm = name_table;
  }

  *nm = name;
  names++;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deletion algorithm from Donald E. Knuth, ``The Art of Computer
Programming'', Volume 3 page 527.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
deleteName(Name name)
{ int hashkey = stringHashValue(&name->data);
  Name *j, *i = &name_table[hashkey];
  Name *end = &name_table[buckets];

  while(*i && *i != name)
  { if ( ++i == end )
      i = name_table;
  }
  assert(*i);
  
  *i = NULL;				/* R1 */
  j = i;

  for(;;)
  { Name *r;

    if ( ++i == end )			/* R2 */
      i = name_table;

    if ( *i == NULL )			/* R3 */
    { names--;
      return;
    }

    r = &name_table[stringHashValue(&(*i)->data)];
    if ( (i >= r && r > j) || (r > j && j > i) || (j > i && i >= r) )
      continue;

    *j = *i;
    *i = NULL;				/* R1 and to R2 */
    j = i;
  }
}


void
initNamesPass1(void)
{ Name name;

  allocRange(builtin_names, sizeof(builtin_names));

  for( name=(Name)builtin_names; name->data.s_text != NULL; name++)
  { name->data.size = strlen(name->data.s_text);
    name->data.encoding = ENC_ASCII;
    name->data.b16 = FALSE;
  }
}


void
initNamesPass2(void)
{ int n;
  Name name;

  buckets = nextBucketSize(buckets);	/* initialise to first valid value */
  name_table = malloc(buckets * sizeof(Name));
  for(n=0; n < buckets; n++)
    name_table[n] = NULL;

  for(n = 0, name=(Name)builtin_names; name->data.s_text != NULL; name++, n++)
  { initHeaderObj(name, ClassName);
    registerName(name);
    createdObject(name, (Name)NAME_new);
  }

  builtins = n;

  DEBUG_BOOT(checkNames(TRUE));
}



		/********************************
		*       CHECK CONSISTENCY	*
		********************************/

static int shifts;

void
checkNames(int prt)
{ int n;
  int cnt = 0;
  shifts = 0;
  

  for(n=0; n < buckets; n++)
  { Name name = name_table[n];

    if ( name != NULL )
    { cnt++;
      assert(isProperObject(name));
      assert(isName(name));		/* checks F_ISNAME */
      assert(classOfObject(name) == ClassName);
      assert(isProtectedObj(name));
      assert(name->data.s_text != NULL);
      assert(getLookupName(NULL, (CharArray) name) == name);
    }
  }

  if ( prt )
    printf("%d names in %d buckets. %d shifts\n",
	   names, buckets, shifts);

  assert(cnt == names);
}


		/********************************
		*         CREATE/CONVERT	*
		********************************/


status
initialiseName(Name n, CharArray value)
{ initialiseCharArray((CharArray) n, value);
  
  if ( inBoot )
    return registerName(n);
  else
    return qadSendv(n, (Name)NAME_register, 0, NULL);
}


static status
unlinkName(Name n)
{ assert(0);				/* names cannot be unlinked! */
  fail;
}


static status
registerName(Name n)
{ insertName(n);
  setFlag(n, F_PROTECTED|F_ISNAME);

  succeed;
}


static Name
getLookupName(Class class, CharArray value)
{ int hashkey = stringHashValue(&value->data);
  Name *name = &name_table[hashkey];

  while(*name)
  { if ( str_eq(&(*name)->data, &value->data) )
      answer(*name);

    shifts++;				/* debugging */
    if ( ++hashkey == buckets )
    { hashkey = 0;
      name = name_table;
    } else
      name++;
  }

  fail;
}


static Name
getConvertName(Any ctx, Any val)
{ if ( instanceOfObject(val, ClassCharArray) )
  { CharArray ca = val;

    return StringToName(&ca->data);
  } else
  { char *s;

    TRY(s = toCharp(val));
    answer(CtoName(s));
  }
}


static Name
getModifyName(Name n, Name n2)
{ Name name;

  if ( (name = getLookupName(ClassName, (CharArray) n2)) )
    answer(name);

  answer(newObject(ClassName, n2, 0));
}


static Name
getCopyName(Name n)
{ answer(n);
}

		/********************************
		*       MODIFYING NAMES		*
		********************************/

static int
isBuiltInName(Name n)
{ return n >= (Name)&builtin_names[0] &&
         n <  (Name)&builtin_names[builtins];
}


static status
ValueName(Name n, CharArray val)
{ Name existing;

  DEBUG((Name)NAME_name, printf("Converting %s --> ", strName(n)); fflush(stdout));

  if ( (existing = getLookupName(classOfObject(n), val)) )
  { if ( existing != n )
      return errorPce(n, (Name)NAME_nameAlreadyExists);
    succeed;
  }

  deleteName(n);
  if ( !isBuiltInName(n) )
    str_unalloc(&n->data);
  str_cphdr(&n->data, &val->data);
  str_alloc(&n->data);
  str_ncpy(&n->data, 0, &val->data, 0, val->data.size);
  insertName(n);

  DEBUG((Name)NAME_name, printf("%s\n", strName(n)));

  succeed;
}


static status
syntaxName(Name n, Name casemap, Int ws)
{ String s = &n->data;
  int size = s->size;
  int i;
  StringObj str;

  for(i=0; i<size; i++)
  { wchar c = str_fetch(s, i);

    if ( isupper(c) || c == '%' || c == '.' )
      succeed;
  }

  str = newObject(ClassString, name_procent_s, n, 0);
  upcaseString(str);
  if ( notDefault(ws) )
  { s = &str->data;
    size = s->size;

    for(i=0; i<size; i++)
      if ( str_fetch(s, i) == syntax.word_separator )
	str_store(s, i, valInt(ws));
  }

  TRY(ValueName(n, (CharArray) str));

  return doneObject(str);
}


status
forNamePce(Pce pce, Code code)
{ Name *a = alloca(sizeof(Name) * names);
  Name *i=name_table, *o = a;
  int nms = names;			/* might change: copy */
  int n;

  for(; i < &name_table[buckets]; i++)
  { if ( *i )
      *o++ = *i;
  }

  for(n = 0, i = a; n < nms; n++, i++)
  { if ( !forwardCodev(code, 1, (Any*)i) )
      fail;
  }

  succeed;
}

		/********************************
		*       RAW MODIFICATIONS	*
		********************************/

Name
getCapitaliseName(Name n)
{ return (Name) getCapitaliseCharArray((CharArray) n);
}


Name
getLabelNameName(Name n)
{ return (Name) getLabelNameCharArray((CharArray) n);
}


Name
getDeleteSuffixName(Name n, Name suffix)
{ return (Name) getDeleteSuffixCharArray((CharArray) n, (CharArray)suffix);
}


Name
getExternalName(Name n)			/* map to lower  */
{ if ( syntax.uppercase )
    return (Name) getDowncaseCharArray((CharArray)n);
  
  return n;
}

		/********************************
		*          C-CONVERSIONS	*
		********************************/

#ifdef BENCHNAMES
static int str_eq_failed;
#endif

Name
StringToName(String s)
{ int hashkey = stringHashValue(s);
  Name *namep;

  for( namep = &name_table[hashkey]; *namep; )
  { if ( str_eq(&(*namep)->data, s) )
      answer(*namep);
#ifdef BENCHNAMES
    else
      str_eq_failed++;
#endif
  if ( ++hashkey == buckets )
    { hashkey = 0;
      namep = name_table;
    } else
      namep++;
  }

  if ( inBoot )
  { Name name = alloc(sizeof(struct name));
    initHeaderObj(name, ClassName);

    str_cphdr(&name->data, s);
    str_alloc(&name->data);
    str_ncpy(&name->data, 0, s, 0, s->size);
    registerName(name);
    createdObject(name, (Name)NAME_new);

    answer(name);
  } else
  { CharArray scratch = StringToScratchCharArray(s);
    Name name;

    Mode(MODE_SYSTEM, name = newObject(ClassName, scratch, 0));

    doneScratchCharArray(scratch);
    return name;
  }
}


Name
CtoName(const char *text)
{ if ( text )
  { string s;

    s.size = strlen(text);
    s.encoding = ENC_ASCII;
    s.b16 = FALSE;
    s.s_text8 = (char8 *)text;

    return StringToName(&s);
  } else
    fail;
}


Name
CtoKeyword(const char *s)
{ if ( syntax.uppercase )
  { CharBuf(buf, strlen(s));
    char *q;

    for(q=buf; *s; s++)
    { if ( islower(*s) )
	*q++ = toupper(*s);
      else if ( *s == '_' )
	*q++ = syntax.word_separator;
      else
	*q++ = *s;
    }
    *q = EOS;

    return CtoName(buf);
  }

  return CtoName(s);
}


char *
saveStringName(Name n)
{ if ( isProperObject(n) && instanceOfObject(n, ClassName) )
    return save_string(n->data.s_text8);
  else
  { char buf[100];

    sprintf(buf, "0x%lx", (unsigned long)n);
    return save_string(buf);
  }
}

		/********************************
		*           ANALYSIS		*
		********************************/

#ifdef BENCHNAMES

static Int
GetBucketsName(Name name)
{ answer(toInt(buckets));
}


static Int
GetBenchName(Name name, Int count)
{ int cnt = valInt(count);
  int n;

  str_eq_failed = 0;
  
  for(;;)
  { for(n=0; n<buckets; n++)
    { Name nm;

      if ( (nm = name_table[n]) )
      { if ( cnt-- > 0 )
	  StringToName(&nm->data);
	else
	  answer(toInt(str_eq_failed));
      }
    }
  }
}

#endif /*BENCHNAMES*/

status
makeClassName(Class class)
{ sourceClass(class, makeClassName, __FILE__, "$Revision$");

  termClass(class, "name", 1, NAME_value);
  cloneStyleClass(class, (Name) NAME_none);

  sendMethod(class, (Name)NAME_initialise, DEFAULT, 1, "text=char_array",
	     "Create a name from name",
	     initialiseName);
  sendMethod(class, (Name)NAME_unlink, DEFAULT, 0,
	     "Trap error",
	     unlinkName);
  sendMethod(class, (Name)NAME_register, (Name)NAME_oms, 0,
	     "Register in unique table",
	     registerName);
  sendMethod(class, (Name)NAME_equal, (Name)NAME_compare, 1, "name",
	     "Test if names represent same text",
	     equalObject);
  sendMethod(class, (Name)NAME_Value, (Name)NAME_internal, 1, "char_array",
	     "Modify name, preserving identity",
	     ValueName);
  sendMethod(class, (Name)NAME_syntax, (Name)NAME_internal, 2, "{uppercase}", "char",
	     "Upcase and map word separator",
	     syntaxName);

  getMethod(class, (Name)NAME_lookup, (Name)NAME_oms, "name", 1, "char_array",
	    "Perform lookup in current name-table",
	    getLookupName);
  getMethod(class, (Name)NAME_convert, (Name)NAME_conversion, "name", 1, "any",
	    "Convert `text-convertable'",
	    getConvertName);
  getMethod(class, (Name)NAME_modify, DEFAULT, "name", 1, "char_array",
	    "Lookup or create argument name",
	    getModifyName);
  getMethod(class, (Name)NAME_copy, DEFAULT, "name", 0,
	    "The name itself",
	    getCopyName);

  getMethod(class, (Name)NAME_hashValue, (Name)NAME_statistics, "int", 0,
	    "Its hash-value",
	    getHashValueName);
  getMethod(class, (Name)NAME_bucketValue, (Name)NAME_statistics, "name", 1, "0..",
	    "Name at indicated bucket",
	    getBucketValueName);

#ifdef BENCHNAMES
  getMethod(class, (Name)NAME_Buckets, (Name)NAME_statistics, "int", 0,
	    "Number of buckets in name-table",
	    GetBucketsName);
  getMethod(class, (Name)NAME_Bench, (Name)NAME_statistics, "int", 1, "int",
	     "Lookup text of this name <n> times",	
	     GetBenchName);
#endif /*BENCHNAMES*/

  succeed;
}
