/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <h/kernel.h>
#include <h/unix.h>			/* storeCharpFile() prototype */

static CharArray	stringToCharArray(String s);

		/********************************
		*         CREATE/CONVERT	*
		********************************/


status
initialiseCharArray(CharArray n, CharArray value)
{ str_cphdr(&n->data, &value->data);
  str_alloc(&n->data);
  if ( value->data.readonly )
    n->data.s_text8 = value->data.s_text8;
  else
    memcpy(n->data.s_text8, value->data.s_text8, str_datasize(&n->data));

  succeed;
}


static status
unlinkCharArray(CharArray n)
{ str_unalloc(&n->data);

  succeed;
}


static status
cloneCharArray(CharArray str, CharArray clone)
{ clonePceSlots(str, clone);
  clone->data = str->data;
  str_alloc(&clone->data);
  memcpy(clone->data.s_text8, str->data.s_text8, str_datasize(&str->data));

  succeed;
}


static status
storeCharArray(CharArray s, FileObj file)
{ TRY(storeSlotsObject(s, file));
  return storeCharpFile(file, (char *)s->data.s_text8); /* TBD: full store! */
}


static status
loadCharArray(CharArray s, IOSTREAM *fd, ClassDef def)
{ unsigned char *data;

  TRY(loadSlotsObject(s, fd, def));
  if ( (data = (unsigned char *)loadCharp(fd)) )
  { String str = &s->data;
    
    str_inithdr(str, ENC_ASCII);
    str->size     = strlen((char *)data);
    str->s_text8  = data;
  }

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


/* n2 is prefix of n1 */

status
prefixCharArray(CharArray n1, CharArray n2, Bool ign_case)
{ if ( ign_case == ON )
    return str_icase_prefix(&n1->data, &n2->data);
  else
    return str_prefix(&n1->data, &n2->data);
}


status
suffixCharArray(CharArray n, CharArray s, Bool ign_case)
{ if ( ign_case == ON )
    return str_icase_suffix(&n->data, &s->data);
  else
    return str_suffix(&n->data, &s->data);
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
  } else
  { if ( str_icasesub(&n1->data, &n2->data) )
      succeed;
  }

  fail;
}

		/********************************
		*         MODIFICATIONS		*
		********************************/

static CharArray
getModifyCharArray(CharArray n, CharArray n2)
{ answer(answerObject(classOfObject(n), n2, EAV));
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
    CharArray rval = get(n, NAME_modify, scratch, EAV);

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
  int i;

  if ( size == 0 )
    return n;

  { LocalString(buf, s, size);
    int o = 0;
    int c = str_fetch(s, 0);

    i = 0;
    str_store(buf, o, toupper(c));
    i++, o++;

    for( ; i < size; i++, o++ )
    { c = str_fetch(s, i);

      if ( iswordsep(c) )
      { str_store(buf, o, ' ');
#if 0
	if ( ++i < size )
	{ o++;
	  str_store(buf, o, toupper(str_fetch(s, i)));
	}
#endif
      } else
	str_store(buf, o, c);
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


static CharArray
getStripCharArray(CharArray n, Name how)
{ String s = &n->data;
  int size = s->size;
  LocalString(buf, s, size);
  int i=0, o=0, lnb=0;

  if ( isDefault(how) )
    how = NAME_canonise;

  if ( how == NAME_canonise || how == NAME_leading || how == NAME_both )
  { for(; i<size && islayout(str_fetch(s, i)); i++)
      ;
  }
  for( ; i<size; i++)
  { int c = str_fetch(s, i);

    str_store(buf, o++, c);
    if ( !islayout(c) )
      lnb = o;
    else if ( how == NAME_canonise )
    { for( ; i+1<size && islayout(str_fetch(s, i+1)); i++)
	;
    }
  }
  if ( how == NAME_canonise || how == NAME_trailing || how == NAME_both )
    buf->size = lnb;
  else
    buf->size = o;

  answer(ModifiedCharArray(n, buf));
}


static Chain
getSplitCharArray(CharArray in, CharArray br)
{ String s1 = &in->data;
  int size = s1->size;
  int i=0, last=0;
  Chain ch = answerObject(ClassChain, EAV);
  string buf;

  str_cphdr(&buf, s1);

  if ( notDefault(br) )			/* given pattern */
  { String b = &br->data;

    while( i<=size-b->size )
    { if ( str_prefix_offset(s1, i, b) )
      { if ( isstr8(s1) )
	  buf.s_text8 = s1->s_text8+last;
	else
	  buf.s_text16 = s1->s_text16+last;

	buf.size = i-last;
	appendChain(ch, ModifiedCharArray(in, &buf));

	i = last = i+b->size;
      } else
	i++;
    }
  } else
  { for(; i<size && islayout(str_fetch(s1, i)); i++) /* strip leading */
      ;
    last = i;

    while( i<size )
    { if ( islayout(str_fetch(s1, i)) )
      { if ( isstr8(s1) )
	  buf.s_text8 = s1->s_text8+last;
	else
	  buf.s_text16 = s1->s_text16+last;

	buf.size = i-last;
	appendChain(ch, ModifiedCharArray(in, &buf));

	while(i < size && islayout(str_fetch(s1, i)))
	  i++;
	last = i;
	if ( i == size )		/* trailing blanks */
	  answer(ch);
      } else
	i++;
    }
  }
	   
  if ( isstr8(s1) )
    buf.s_text8 = s1->s_text8+last;
  else
    buf.s_text16 = s1->s_text16+last;

  buf.size = size-last;
  appendChain(ch, ModifiedCharArray(in, &buf));

  answer(ch);
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
      
      cpdata(d, ca->data.s_text16, char16, ca->data.size);
      d += ca->data.size;

      for( i=0; i<argc; i++ )
      { cpdata(d, argv[i]->data.s_text16, char16, argv[i]->data.size);
	d += argv[i]->data.size;
      }
    }

    buf->size = l;
    answer(ModifiedCharArray(ca, buf));
  }
}


CharArray
getDeleteSuffixCharArray(CharArray n, CharArray s)
{ if ( suffixCharArray(n, s, OFF) )
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
{ if ( suffixCharArray(n, s, OFF) )
    answer(n);

  answer(getAppendCharArray(n, s));
}


static CharArray
getDeletePrefixCharArray(CharArray n, CharArray s)
{ if ( prefixCharArray(n, s, OFF) )
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


		 /*******************************
		 *	 BASE-64 ENCODING	*
		 *******************************/

static int
base64_char(unsigned int in)
{ if ( in < 26 ) return 'A'+in;
  if ( in < 52 ) return 'a'+in-26;
  if ( in < 62 ) return '0'+in-52;
  if ( in == 62 ) return '+';
  assert(in == 63);
  return '/';
}


static unsigned long
base64_code(unsigned int in)
{ if ( in == '+' ) return 62;
  if ( in == '/' ) return 63;
  if ( in <  '0' ) return ~0L;
  if ( in <= '9' ) return in - '0' + 52;
  if ( in <  'A' ) return ~0L;
  if ( in <= 'Z' ) return in - 'A';
  if ( in <  'a' ) return ~0L;
  if ( in <= 'z' ) return in - 'a' + 26;
  return ~0L;
}


static CharArray
getBase64EncodeCharArray(CharArray in)
{ String s = &in->data;
  int size = s->size;
  int triples = (size+2)/3;
  LocalString(buf, s, triples*4);
  int i, o=0;
  unsigned long v;

  for(i=0; i+2<size;)
  { v = (str_fetch(s, i)<<16) + (str_fetch(s, i+1)<<8) + str_fetch(s, i+2);
    i += 3;
    str_store(buf, o++, base64_char((v>>18)&0x3f));
    str_store(buf, o++, base64_char((v>>12)&0x3f));
    str_store(buf, o++, base64_char((v>> 6)&0x3f));
    str_store(buf, o++, base64_char((v>> 0)&0x3f));
  }

  if ( size - i == 2 )
  { v = (str_fetch(s, i)<<16) + (str_fetch(s, i+1)<<8);
    str_store(buf, o++, base64_char((v>>18)&0x3f));
    str_store(buf, o++, base64_char((v>>12)&0x3f));
    str_store(buf, o++, base64_char((v>> 6)&0x3f));
    str_store(buf, o++, '=');
  } else if ( size - i == 1)
  { v = (str_fetch(s, i)<<16);
    str_store(buf, o++, base64_char((v>>18)&0x3f));
    str_store(buf, o++, base64_char((v>>12)&0x3f));
    str_store(buf, o++, '=');
    str_store(buf, o++, '=');
  }

  buf->size = o;
  answer(ModifiedCharArray(in, buf));
}


static CharArray
getBase64DecodeCharArray(CharArray in)
{ String s = &in->data;
  int size = s->size;
  LocalString(buf, s, (size/4)*3);
  int i, o = 0;
  unsigned long v = 0L;
    
  for(i=0; i+3<size; )
  { int c;

    v = (base64_code(str_fetch(s, i)) << 18) |
	(base64_code(str_fetch(s, i+1)) << 12);
    i += 2;
    c = str_fetch(s, i++);
    if ( c == '=' )
    { i++;				/* skip last (must be =) */
      str_store(buf, o++, (v>>16) & 0xff);
      break;
    }
    v |= base64_code(c) << 6;
    c = str_fetch(s, i++);
    if ( c == '=' )
    { str_store(buf, o++, (v>>16) & 0xff);
      str_store(buf, o++, (v>>8) & 0xff);
      break;
    }
    v |= base64_code(c);
    if ( v == ~0L )
      fail;
    str_store(buf, o++, (v>>16) & 0xff);
    str_store(buf, o++, (v>>8) & 0xff);
    str_store(buf, o++, (v>>0) & 0xff);
  }

  if ( i != size || v == ~0L )
    fail;

  buf->size = o;
  answer(ModifiedCharArray(in, buf));
}


		 /*******************************
		 *	      AS-FILE		*
		 *******************************/

static CharArray
getReadAsFileCharArray(CharArray n, Int from, Int size)
{ int f = valInt(from);
  int s = valInt(size);

  if ( f < 0 || s < 0 || f > n->data.size )
    fail;

  if ( f == 0 && s >= n->data.size )
    answer(n);
  else
  { string str;

    if ( f+s > n->data.size )
      s = n->data.size - f;

    str_cphdr(&str, &n->data);
    str.size = s;
    if ( isstr8(&n->data) )
      str.s_text8 = &n->data.s_text8[f];
    else
      str.s_text16 = &n->data.s_text16[f];
    
    answer((CharArray)StringToString(&str));
  }
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

    TRY(argc = scanstr((char *)n->data.s_text8,
		       (char *)fmt->data.s_text8,
		       argv));
    
    answer(answerObjectv(ClassVector, valInt(argc), argv));
  } else
  { errorPce(n, NAME_notSupportedForChar16);
    fail;
  }
}


static Name
getCompareCharArray(CharArray n1, CharArray n2, Bool ignore_case)
{ int rval;

  if ( ignore_case == ON )
    rval = str_icase_cmp(&n1->data, &n2->data);
  else
    rval = str_cmp(&n1->data, &n2->data);

  if ( rval < 0 )
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

static CharArray scratch_char_arrays;

void
initCharArrays(void)
{ CharArray ca;
  int n;
  int size = sizeof(struct char_array) * SCRATCH_CHAR_ARRAYS;

  scratch_char_arrays = alloc(size);
  memset(scratch_char_arrays, 0, size);

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
    { str_inithdr(&name->data, ENC_ASCII);
      name->data.size = strlen(s);
      name->data.s_text8 = (unsigned char *) s;

      return name;
    }

  initCharArrays();			/* handle the crash better */
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

  initCharArrays();			/* handle the crash better */
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
  CharArray rval = answerObject(ClassCharArray, name, EAV);
  
  doneScratchCharArray(name);
  return rval;
}


static CharArray
stringToCharArray(String s)
{ CharArray name = StringToScratchCharArray(s);
  CharArray rval = answerObject(ClassCharArray, name, EAV);
  
  doneScratchCharArray(name);
  return rval;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_compare[] =
        { "char_array", "ignore_case=[bool]" };
static char *T_ofAchar_fromADintD[] =
        { "of=char", "from=[int]" };
static char *T_gsub[] =
        { "start=int", "end=[int]" };
static char *T_cmpcase[] =
        { "text=char_array", "ignore_case=[bool]" };
static char *T_readAsFile[] =
        { "from=int", "size=int" };

/* Instance Variables */

static vardecl var_charArray[] =
{ IV(NAME_header, "alien:str_h", IV_NONE,
     NAME_internal, "Header info (packed)"),
  IV(NAME_text, "alien:wchar *", IV_NONE,
     NAME_internal, "Text represented (8- or 16-bits chars)")
};

/* Send Methods */

static senddecl send_charArray[] =
{ SM(NAME_initialise, 1, "text=char_array", initialiseCharArray,
     DEFAULT, "Create from other char_array"),
  SM(NAME_unlink, 0, NULL, unlinkCharArray,
     DEFAULT, "Free the char *"),
  SM(NAME_equal, 1, "char_array", equalCharArray,
     NAME_compare, "Test if names represent same text"),
  SM(NAME_larger, 1, "than=char_array", largerCharArray,
     NAME_compare, "Test if I'm alphabetically after arg"),
  SM(NAME_smaller, 1, "than=char_array", smallerCharArray,
     NAME_compare, "Test if I'm alphabetically before arg"),
  SM(NAME_prefix, 2, T_cmpcase, prefixCharArray,
     NAME_test, "Test if receiver has prefix argument"),
  SM(NAME_sub, 2, T_cmpcase, subCharArray,
     NAME_test, "Test if argument is a substring"),
  SM(NAME_suffix, 2, T_cmpcase, suffixCharArray,
     NAME_test, "Test if receiver has suffix argument")
};

/* Get Methods */

static getdecl get_charArray[] =
{ GM(NAME_printName, 0, "char_array", NULL, getSelfObject,
     DEFAULT, "Equivalent to <-self"),
  GM(NAME_size, 0, "int", NULL, getSizeCharArray,
     NAME_cardinality, "Number of characters in the text"),
  GM(NAME_capitalise, 0, "char_array", NULL, getCapitaliseCharArray,
     NAME_case, "Capitalised version of name"),
  GM(NAME_downcase, 0, "char_array", NULL, getDowncaseCharArray,
     NAME_case, "Map all uppercase letters to lowercase"),
  GM(NAME_labelName, 0, "char_array", NULL, getLabelNameCharArray,
     NAME_case, "Default name used for labels"),
  GM(NAME_upcase, 0, "char_array", NULL, getUpcaseCharArray,
     NAME_case, "Map all lowercase letters to uppercase"),
  GM(NAME_strip, 1, "char_array", "[{canonise,leading,trailing,both}]", getStripCharArray,
     NAME_content, "Strip leading/trailing blanks"),
  GM(NAME_compare, 2, "{smaller,equal,larger}", T_compare, getCompareCharArray,
     NAME_compare, "Alphabetical comparison"),
  GM(NAME_append, 1, "char_array", "char_array ...", getAppendCharArrayv,
     NAME_content, "Concatenation of me and the argument(s)"),
  GM(NAME_character, 1, "char", "int", getCharacterCharArray,
     NAME_content, "ASCII value of 0-based nth character"),
  GM(NAME_deletePrefix, 1, "char_array", "prefix=char_array", getDeletePrefixCharArray,
     NAME_content, "Delete specified prefix"),
  GM(NAME_deleteSuffix, 1, "char_array", "suffix=char_array", getDeleteSuffixCharArray,
     NAME_content, "Delete specified suffix"),
  GM(NAME_ensureSuffix, 1, "char_array", "suffix=char_array", getEnsureSuffixCharArray,
     NAME_content, "Append suffix if not already there"),
  GM(NAME_sub, 2, "char_array", T_gsub, getSubCharArray,
     NAME_content, "Get substring from 0-based start and end"),
  GM(NAME_convert, 1, "char_array", "any", getConvertCharArray,
     NAME_conversion, "Convert `text-convertible'"),
  GM(NAME_value, 0, "name", NULL, getValueCharArray,
     NAME_conversion, "Value as a name"),
  GM(NAME_copy, 0, "char_array", NULL, getCopyCharArray,
     NAME_copy, "Copy representing the same text"),
  GM(NAME_split, 1, "chain", "separator=[char_array]", getSplitCharArray,
     NAME_content, "Split text using separator"),
  GM(NAME_modify, 1, "char_array", "char_array", getModifyCharArray,
     NAME_internal, "New instance of my class"),
  GM(NAME_lineNo, 1, "line=int", "index=[int]", getLineNoCharArray,
     NAME_line, "Get 1-based line number at which index is"),
  GM(NAME_index, 2, "int", T_ofAchar_fromADintD, getIndexCharArray,
     NAME_parse, "Get 0-based index starting at pos (forwards)"),
  GM(NAME_rindex, 2, "int", T_ofAchar_fromADintD, getRindexCharArray,
     NAME_parse, "Get 0-based index starting at pos (backwards)"),
  GM(NAME_scan, 1, "vector", "format=char_array", getScanCharArray,
     NAME_parse, "C-scanf like parsing of string"),
  GM(NAME_base64Encode, 0, "char_array", NULL, getBase64EncodeCharArray,
     NAME_mime, "Perform base-64 encoding on the argument"),
  GM(NAME_base64Decode, 0, "char_array", NULL, getBase64DecodeCharArray,
     NAME_mime, "Perform base-64 decoding on the argument"),
  GM(NAME_readAsFile, 2, "char_array", T_readAsFile, getReadAsFileCharArray,
     NAME_stream, "Read data from object using pce_open/3"),
  GM(NAME_sizeAsFile, 0, "characters=int", NULL, getSizeCharArray,
     NAME_stream, "Support pce_open/3")
};

/* Resources */

#define rc_charArray NULL
/*
static classvardecl rc_charArray[] =
{ 
};
*/

/* Class Declaration */

static Name charArray_termnames[] = { NAME_value };

ClassDecl(charArray_decls,
          var_charArray, send_charArray, get_charArray, rc_charArray,
          1, charArray_termnames,
          "$Rev$");

status
makeClassCharArray(Class class)
{ declareClass(class, &charArray_decls);

  setCloneFunctionClass(class, cloneCharArray);
  setLoadStoreFunctionClass(class, loadCharArray, storeCharArray);

  initCharArrays();

  succeed;
}
