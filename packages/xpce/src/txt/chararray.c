/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

		/********************************
		*         CREATE/CONVERT	*
		********************************/


status
initialiseCharArray(CharArray n, CharArray value)
{ str_cphdr(&n->data, &value->data);
  str_alloc(&n->data);
  memcpy(n->data.s_text8, value->data.s_text8, str_datasize(&n->data));

  succeed;
}


static status
unlinkCharArray(CharArray n)
{ str_unalloc(&n->data);

  succeed;
}


static CharArray
getConvertCharArray(Any ctx, Any val)
{ string s;

  TRY(toString(val, &s));
  answer(stringToCharArray(&s));
}


Name
getValueCharArray(CharArray n)
{ answer(StringToName(&n->data));
}


		/********************************
		*             TESTS		*
		********************************/

status
equalCharArray(CharArray n1, CharArray n2)
{ if ( str_eq(&n1->data, &n2->data) )
    succeed;

  fail;
}


static status
prefixCharArray(CharArray n1, CharArray n2) /* n2 is prefix of n1 */
{ if ( str_prefix(&n1->data, &n2->data) )
    succeed;

  fail;
}


status
suffixCharArray(CharArray n, CharArray s)
{ if ( str_suffix(&n->data, &s->data) )
    succeed;

  fail;
}


static status
largerCharArray(CharArray n1, CharArray n2)
{ if ( str_cmp(&n1->data, &n2->data) > 0 )
    succeed;
  fail;
}


static status
smallerCharArray(CharArray n1, CharArray n2)
{ if ( str_cmp(&n1->data, &n2->data) < 0 )
    succeed;
  fail;
}


static status
subCharArray(CharArray n1, CharArray n2, Bool ign_case)
{ if ( ign_case != ON )
  { if ( str_sub(&n1->data, &n2->data) )
      succeed;
  } /*else
  { if ( str_icasesub(&n1->data, &n2->data) )
      succeed;
  }*/

  fail;
}

		/********************************
		*         MODIFICATIONS		*
		********************************/

static CharArray
getModifyCharArray(CharArray n, CharArray n2)
{ answer(answerObject(classOfObject(n), n2, 0));
}


static CharArray
ModifiedCharArray(CharArray n, String buf)
{
  Class class = classOfObject(n);

  if ( class == ClassName )
    return (CharArray) StringToName(buf);
  else if ( class == ClassString)
    return (CharArray) StringToString(buf);	/* ??? */
  else
  { CharArray scratch = StringToScratchCharArray(buf);
    CharArray rval = get(n, NAME_modify, scratch, 0);

    doneScratchCharArray(scratch);
    answer(rval);
  }
}


CharArray
getCopyCharArray(CharArray n)
{ answer(ModifiedCharArray(n, &n->data));
}


CharArray
getCapitaliseCharArray(CharArray n)
{ if ( n->data.size == 0 )
    answer(n);
  else
  { String d = &n->data;
    int size = d->size;
    LocalString(buf, d, size);
    int i=1, o=1;

    str_store(buf, 0, toupper(str_fetch(d, 0)));

    for(; i < size; i++, o++)
    { wchar c = str_fetch(d, i);
      
      if ( iswordsep(c) )
      { if ( ++i < size )
	  str_store(buf, o, toupper(str_fetch(d, i)));
      } else
	str_store(buf, o, tolower(c));
    }

    buf->size = o;
    answer(ModifiedCharArray(n, buf));
  }
}


CharArray
getLabelNameCharArray(CharArray n)
{ String s = &n->data;
  int size = s->size;
  int i, l = 0, u = 0;

  if ( size == 0 )
    return n;

  for(i=0; i < size; i++)
  { l |= isupper(str_fetch(s, i));
    u |= islower(str_fetch(s, i));

    if ( (u && l) || (!u && !l) )		/* mixed case or no letters */
      return n;
  }

  { LocalString(buf, s, size);
    int o = 0;

    i = 0;
    str_store(buf, o, toupper(str_fetch(s, i)));
    i++, o++;

    for( ; i < size; i++, o++ )
    { if ( iswordsep(str_fetch(s, i)) )
      { str_store(buf, o, ' ');
	i++, o++;
	str_store(buf, o, toupper(str_fetch(s, i)));
      } else
	str_store(buf, o, tolower(str_fetch(s, i)));
    }

    answer(ModifiedCharArray(n, buf));
  }
}


CharArray
getDowncaseCharArray(CharArray n)
{ String s = &n->data;
  int size = s->size;
  LocalString(buf, s, size);
  int i;

  for(i=0; i<size; i++)
    str_store(buf, i, tolower(str_fetch(s, i)));
  buf->size = size;

  answer(ModifiedCharArray(n, buf));
}


static CharArray
getUpcaseCharArray(CharArray n)
{ String s = &n->data;
  int size = s->size;
  LocalString(buf, s, size);
  int i;

  for(i=0; i<size; i++)
    str_store(buf, i, toupper(str_fetch(s, i)));
  buf->size = size;

  answer(ModifiedCharArray(n, buf));
}


CharArray
getAppendCharArray(CharArray n1, CharArray n2)
{ String s1 = &n1->data;
  String s2 = &n2->data;
  LocalString(buf, s1, s1->size + s2->size);
  int n;

  buf->size = s1->size + s2->size;
  memcpy(buf->s_text8, s1->s_text8, (n=str_datasize(s1)));
  memcpy(&buf->s_text8[n], s2->s_text8, str_datasize(s2));

  answer(ModifiedCharArray(n1, buf));
}


static CharArray
getAppendCharArrayv(CharArray ca, int argc, CharArray *argv)
{ int l = ca->data.size;
  int i;

  for( i=0; i<argc; i++ )
    l += argv[i]->data.size;
       
  { LocalString(buf, &ca->data, l);

    if ( isstr8(&ca->data) )
    { char8 *d = buf->s_text8;
      
      memcpy(d, ca->data.s_text8, ca->data.size * sizeof(char8));
      d += ca->data.size;

      for( i=0; i<argc; i++ )
      { memcpy(d, argv[i]->data.s_text8, argv[i]->data.size*sizeof(char8));
	d += argv[i]->data.size;
      }
    } else
    { char16 *d = buf->s_text16;
      
      memcpy(d, ca->data.s_text16, ca->data.size * sizeof(char16));
      d += ca->data.size;

      for( i=0; i<argc; i++ )
      { memcpy(d, argv[i]->data.s_text16, argv[i]->data.size*sizeof(char16));
	d += argv[i]->data.size;
      }
    }

    buf->size = l;
    answer(ModifiedCharArray(ca, buf));
  }
}


CharArray
getDeleteSuffixCharArray(CharArray n, CharArray s)
{ if ( suffixCharArray(n, s) )
  { string buf;

    str_cphdr(&buf, &n->data);
    buf.s_text = n->data.s_text;
    buf.size = n->data.size - s->data.size;

    answer(ModifiedCharArray(n, &buf));
  }

  fail;
}


CharArray
getEnsureSuffixCharArray(CharArray n, CharArray s)
{ if ( suffixCharArray(n, s) )
    answer(n);

  answer(getAppendCharArray(n, s));
}


CharArray
getDeletePrefixCharArray(CharArray n, CharArray s)
{ if ( prefixCharArray(n, s) )
  { string buf;

    str_cphdr(&buf, &n->data);
    buf.size = n->data.size - s->data.size;
    if ( isstr8(&buf) )
      buf.s_text8 = &n->data.s_text8[s->data.size];
    else
      buf.s_text16 = &n->data.s_text16[s->data.size];

    answer(ModifiedCharArray(n, &buf));
  }

  fail;
}


static CharArray
getSubCharArray(CharArray n, Int start, Int end)
{ string s;
  int x, y;
  int len = n->data.size;

  x = valInt(start);
  y = (isDefault(end) ? len : valInt(end));
  if ( x < 0 || y > len || x > y )
    fail;

  str_cphdr(&s, &n->data);
  s.size = y-x;
  if ( isstr8(&n->data) )
    s.s_text8 = &n->data.s_text8[x];
  else
    s.s_text16 = &n->data.s_text16[x];
  
  answer(ModifiedCharArray(n, &s));
}


		/********************************
		*          READING GETS		*
		********************************/

Int
getSizeCharArray(Any n)
{ CharArray c = n;

  answer(toInt(c->data.size));
}


static Int
getCharacterCharArray(CharArray n, Int idx)
{ int i = valInt(idx);

  if ( i < 0 || i >= n->data.size )	
    fail;

  answer(toInt(str_fetch(&n->data, i)));
}


static Int
getIndexCharArray(CharArray n, Int chr, Int here)
{ wchar c = valInt(chr);
  int h;

  h = (isDefault(here) ? 0 : valInt(here));
  if ( (h = str_next_index(&n->data, h, c)) >= 0 )
    answer(toInt(h));

  fail;
}


static Int
getRindexCharArray(CharArray n, Int chr, Int here)
{ wchar c = valInt(chr);
  int h, len = n->data.size;

  h = (isDefault(here) ? (len - 1) : valInt(here));
  if ( (h = str_next_rindex(&n->data, h, c)) >= 0 )
    answer(toInt(h));

  fail;
}


static Int
getLineNoCharArray(CharArray name, Int caret)
{ int here = (isDefault(caret) ? name->data.size : valInt(caret));

  answer(toInt(str_lineno(&name->data, here)));
}


static Vector
getScanCharArray(CharArray n, CharArray fmt)
{ if ( isstr8(&n->data) && isstr8(&fmt->data) )
  { Any argv[SCAN_MAX_ARGS];
    Int argc;

    TRY(argc = scanstr(n->data.s_text8, fmt->data.s_text8, argv));
    
    answer(answerObjectv(ClassVector, valInt(argc), argv));
  } else
  { errorPce(n, NAME_notSupportedForChar16);
    fail;
  }
}


static Name
getCompareCharArray(CharArray n1, CharArray n2)
{ int rval;

  if ( (rval = str_cmp(&n1->data, &n2->data)) < 0 )
    answer(NAME_smaller);
  else if ( rval == 0 )
    answer(NAME_equal);
  else
    answer(NAME_larger);

  assert(0);				/* should not get here! */
  fail;
}


		/********************************
		*          C-CONVERSIONS	*
		********************************/

#define SCRATCH_CHAR_ARRAYS	(10)

static struct char_array scratch_char_arrays[SCRATCH_CHAR_ARRAYS];

void
initCharArrays(void)
{ CharArray ca;
  int n;

  allocRange(scratch_char_arrays, sizeof(scratch_char_arrays));

  for(ca=scratch_char_arrays, n = 0; n < SCRATCH_CHAR_ARRAYS; ca++, n++)
  { initHeaderObj(ca, ClassCharArray);
    setProtectedObj(ca);
    createdObject(ca, NAME_new);
  }
}


CharArray
CtoScratchCharArray(const char *s)
{ CharArray name = scratch_char_arrays;
  int n;

  for(n = 0; n < SCRATCH_CHAR_ARRAYS; n++, name++)
    if ( name->data.s_text8 == NULL )
    { name->data.s_text8 = (char *) s;
      name->data.b16 = FALSE;
      name->data.size = strlen(s);
      name->data.encoding = ENC_ASCII;
      return name;
    }

  NOTREACHED;
  fail;
}


CharArray
StringToScratchCharArray(const String s)
{ CharArray name = scratch_char_arrays;
  int n;

  for(n = 0; n < SCRATCH_CHAR_ARRAYS; n++, name++)
    if ( name->data.s_text8 == NULL )
    { str_cphdr(&name->data, s);
      name->data.s_text = s->s_text;
      return name;
    }

  NOTREACHED;
  fail;
}


void
doneScratchCharArray(CharArray n)
{ n->data.s_text = NULL;
}


CharArray
CtoCharArray(char *s)
{ CharArray name = CtoScratchCharArray(s);
  CharArray rval = answerObject(ClassCharArray, name, 0);
  
  doneScratchCharArray(name);
  return rval;
}


CharArray
stringToCharArray(String s)
{ CharArray name = StringToScratchCharArray(s);
  CharArray rval = answerObject(ClassCharArray, name, 0);
  
  doneScratchCharArray(name);
  return rval;
}


status
makeClassCharArray(Class class)
{ sourceClass(class, makeClassCharArray, __FILE__, "$Revision$");

  localClass(class, NAME_header, NAME_internal, "alien:str_h", NAME_none,
	     "Header info (packed)");
  localClass(class, NAME_text, NAME_internal, "alien:wchar *", NAME_none,
	     "Text represented (8- or 16-bits chars)");

  termClass(class, "char_array", 1, NAME_value);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "text=char_array",
	     "Create from other char_array",
	     initialiseCharArray);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Free the char *",
	     unlinkCharArray);
  sendMethod(class, NAME_equal, NAME_compare, 1, "char_array",
	     "Test if names represent same text",
	     equalCharArray);

  sendMethod(class, NAME_suffix, NAME_test, 1, "suffix=char_array",
	     "Test if receiver has suffix argument",
	     suffixCharArray);
  sendMethod(class, NAME_prefix, NAME_test, 1, "prefix=char_array",
	     "Test if receiver has prefix argument",
	     prefixCharArray);
  sendMethod(class, NAME_larger, NAME_compare, 1, "than=char_array",
	     "Test if I'm alphabetically after arg",
	     largerCharArray);
  sendMethod(class, NAME_smaller, NAME_compare, 1, "than=char_array",
	     "Test if I'm alphabetically before arg",
	     smallerCharArray);
  sendMethod(class, NAME_sub, NAME_test, 2,
	     "sub=char_array", "ignore_case=[bool]",
	     "Test if argument is a substring",
	     subCharArray);

  getMethod(class, NAME_convert, NAME_conversion, "char_array", 1, "any",
	    "Convert `text-convertable'",
	    getConvertCharArray);
  getMethod(class, NAME_value, NAME_conversion, "name", 0,
	    "Value as a name",
	    getValueCharArray);
  getMethod(class, NAME_modify, NAME_internal, "char_array", 1, "char_array",
	    "New instance of my class",
	    getModifyCharArray);

  getMethod(class, NAME_size, NAME_cardinality, "int", 0,
	    "Number of characters in the text",
	    getSizeCharArray);
  getMethod(class, NAME_compare, NAME_compare, "{smaller,equal,larger}", 1,
	    "char_array",
	    "Alphabetical comparison",
	    getCompareCharArray);
  getMethod(class, NAME_character, NAME_content, "char", 1, "int",
	    "ASCII value of 0-based nth character",
	    getCharacterCharArray);
  getMethod(class, NAME_index, NAME_parse, "int", 2, "of=char", "from=[int]",
	    "Get 0-based index starting at pos (forwards)",
	    getIndexCharArray);
  getMethod(class, NAME_rindex, NAME_parse, "int", 2, "of=char", "from=[int]",
	    "Get 0-based index starting at pos (backwards)",
	    getRindexCharArray);
  getMethod(class, NAME_lineNo, NAME_line, "line=int", 1, "index=[int]",
	    "Get 1-based line number at which index is",
	    getLineNoCharArray);
  getMethod(class, NAME_scan, NAME_parse, "vector", 1, "format=char_array",
	    "C-scanf like parsing of string",
	    getScanCharArray);
  getMethod(class, NAME_append, NAME_content, "char_array", 1,
	    "char_array ...",
	    "Concatenation of me and the argument(s)",
	    getAppendCharArrayv);
  getMethod(class, NAME_sub, NAME_content, "char_array", 2,
	    "start=int", "end=[int]",
	    "Get substring from 0-based start and end",
	    getSubCharArray);

  getMethod(class, NAME_downcase, NAME_case, "char_array", 0,
	    "Map all upercase letters to lowercase",
	    getDowncaseCharArray);
  getMethod(class, NAME_upcase, NAME_case, "char_array", 0,
	    "Map all lowercase letters to uppercase",
	    getUpcaseCharArray);
  getMethod(class, NAME_capitalise, NAME_case, "char_array", 0,
	    "Capitalised version of name",
	    getCapitaliseCharArray);
  getMethod(class, NAME_labelName, NAME_case, "char_array", 0,
	    "Default name used for labels",
	    getLabelNameCharArray);
  getMethod(class, NAME_deleteSuffix, NAME_content, "char_array", 1,
	    "suffix=char_array",
	    "Delete specified suffix",
	    getDeleteSuffixCharArray);
  getMethod(class, NAME_deletePrefix, NAME_content, "char_array", 1,
	    "prefix=char_array",
	    "Delete specified prefix",
	    getDeletePrefixCharArray);
  getMethod(class, NAME_ensureSuffix, NAME_content, "char_array", 1,
	    "suffix=char_array",
	    "Append suffix if not already there",
	    getEnsureSuffixCharArray);
  getMethod(class, NAME_copy, NAME_copy, "char_array", 0,
	    "Copy representing the same text",
	    getCopyCharArray);
  getMethod(class, NAME_printName, DEFAULT, "char_array", 0,
	    "Equivalent to <-self",
	    getSelfObject);

  initCharArrays();

  succeed;
}
