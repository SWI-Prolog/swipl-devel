/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>

static status	appendString(StringObj, CharArray);
static status	setString(StringObj str, String s);
static status	CsetString(StringObj str, char *txt);

StringObj
StringToString(String s)
{ CharArray c = StringToScratchCharArray(s);
  StringObj str = answerObject(ClassString, name_procent_s, c, 0);
  doneScratchCharArray(c);

  return str;
}


StringObj
CtoString(char *s)
{ CharArray c = CtoScratchCharArray(s);
  StringObj str =  answerObject(ClassString, name_procent_s, c, 0);
  doneScratchCharArray(c);

  return str;
}


StringObj
staticCtoString(const char *s)
{ CharArray c = CtoScratchCharArray(s);
  StringObj str;

  c->data.readonly = TRUE;
  str = answerObject(ClassString, name_procent_s, c, 0);
  doneScratchCharArray(c);

  return str;
}


StringObj
CtoTempString(char *s)
{ CharArray c = CtoScratchCharArray(s);
  StringObj str =  tempObject(ClassString, name_procent_s, c, 0);
  doneScratchCharArray(c);

  return str;
}


static StringObj
getModifyString(StringObj str, CharArray value)
{ answer(answerObject(classOfObject(str), name_procent_s, value, 0));
}


status
initialiseStringv(StringObj str, CharArray fmt, int argc, Any *argv)
{ if ( isDefault(fmt) )
  { str_inithdr(&str->data, ENC_ASCII);
    str->data.size = 0;
    str_alloc(&str->data);
  } else if ( (Name) fmt == name_procent_s &&
	      argc == 1 && instanceOfObject(argv[0], ClassCharArray) )
  { CharArray v = argv[0];

    str_cphdr(&str->data, &v->data);
    if ( v->data.readonly )
    { str->data.s_text8 = v->data.s_text8;

      DEBUG(NAME_readOnly, Cprintf("Shared %s\n", pp(str)));
    } else
    { str_alloc(&str->data);
      memcpy(str->data.s_text8, v->data.s_text8, str_datasize(&v->data));
    }
  } else
    TRY(str_writefv(&str->data, fmt, argc, argv));

  succeed;
}


static status
bitsPerCharacterString(StringObj str, Int bits)
{ String s = &str->data;

  if ( valInt(bits) == 8 )
  { if ( !isstr8(s) )
    { string s2 = *s;

      s2.b16 = FALSE;
      s2.size *= 2;
      setString(str, &s2);
    }
    succeed;
  } else if ( valInt(bits) == 16 )
  { if ( !isstr16(s) )
    { string s2 = *s;

      s2.b16 = TRUE;
      s2.size /= 2;
      setString(str, &s2);
    }

    succeed;
  } else
    return errorPce(bits, NAME_unexpectedType, CtoString("8 or 16"));
}


static StringObj
getCopyString(StringObj s)
{ answer(answerObject(classOfObject(s), name_procent_s, s, 0));
}


static StringObj
convertString(Class class, Any obj)
{ if ( instanceOfObject(obj, ClassString) ) 
    answer((StringObj) obj);
  else if ( instanceOfObject(obj, ClassCharArray) )
    answer(answerObject(ClassString, name_procent_s, obj, 0));
  else
  { char *s = toCharp(obj);

    if ( s != NULL )
      answer(CtoString(s));
    else
      fail;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load/store a string to/from file. Format:

<string>	::= <charp>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeString(StringObj s, FileObj file)
{ TRY(storeSlotsObject(s, file));
  return storeCharpFile(file, s->data.s_text8); /* TBD: full store! */
}


static status
loadString(StringObj s, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(s, fd, def));
  CsetString(s, loadCharp(fd));

  succeed;
}


static status
formatString(StringObj s, CharArray fmt, int argc, Any *argv)
{ char buf[FORMATSIZE];

  TRY(swritefv(buf, fmt, argc, argv));

  return CsetString(s, buf);
}


status
valueString(StringObj s1, CharArray s2)
{ if ( equalCharArray((CharArray) s1, s2) )
    succeed;

  return setString(s1, &s2->data);
}


static status
prependString(StringObj s1, StringObj s2)
{ return str_insert_string(s1, ZERO, &s2->data);
}


static status
ensureNlString(StringObj s1, CharArray s2)
{ if ( s1->data.size > 0 && str_fetch(&s1->data, s1->data.size-1) != '\n' )
    str_insert_string(s1, DEFAULT, str_nl(&s1->data));

  if ( notDefault(s2) )
    return appendString(s1, s2);

  succeed;
}


static status
ensureSuffixString(StringObj s, CharArray suff, Bool ign_case)
{ if ( !suffixCharArray((CharArray) s, suff, ign_case) )
    appendString(s, suff);

  succeed;
}


static status
newlineString(StringObj s, Int times)
{ int tms;

  if ( isDefault(times) )
    times = ONE;
  tms = valInt(times);

  { String nl = str_nl(&s->data);
    LocalString(buf, &s->data, nl->size * tms);
    int i;

    for(i=0; i<tms; i++)
      str_ncpy(buf, i * nl->size, nl, 0, nl->size);
    buf->size = nl->size * tms;

    return str_insert_string(s, DEFAULT, buf);
  }
}


status
insertCharacterString(StringObj str, Int chr, Int where, Int times)
{ int tms = isDefault(times) ? 1 : valInt(times);

  { LocalString(buf, &str->data, tms);
    int i;

    for(i=0; i<tms; i++)
      str_store(buf, i, valInt(chr));
    buf->size = tms;
    str_insert_string(str, where, buf);
  }

  succeed;
}


static status
appendString(StringObj s1, CharArray s2)
{ return str_insert_string(s1, DEFAULT, &s2->data);
}


static status
stripString(StringObj str, Name where)
{ String s = &str->data;
  int size = s->size;
  int from = 0;
  int to = size;
  string buf;

  if ( where != NAME_trailing )
  { while( from < size && islayout(str_fetch(s, from)))
      from++;
  }

  if ( where != NAME_leading )
  { while( to > from && islayout(str_fetch(s, to-1)) )
      to--;
  }

  str_cphdr(&buf, s);
  buf.s_text = str_textp(s, from);
  buf.size = to - from;

  return setString(str, &buf);
}


static status
untabifyString(StringObj str, Any tabs)
{ Int n;

  if ( isDefault(tabs) )
    tabs = toInt(8);

  if ( instanceOfObject(tabs, ClassVector) )
  { int size = valInt(((Vector)tabs)->size);
    Any *elements = ((Vector)tabs)->elements;
    int maxtab = -1;
    int n;

    for(n = 0; n<size; n++)
    { if ( !isInteger(elements[n]) )
	return errorPce(elements[n], NAME_unexpectedType, TypeInt);
      if ( n <= maxtab )
	return errorPce(str, NAME_badTabStopVector);
      maxtab = n;
    }

    { int size = str->data.size;
      String s = &str->data;
      LocalString(buf, s, size + maxtab);
      int i=0, o=0, col=0;

      for( ; i < size; i++ )
      { wchar c = str_fetch(s, i);

	if ( c == '\t' )
	{ int destcol = col+1;

	  for(n=0; n<size; n++)
	  { if ( valInt(elements[n]) >= destcol )
	    { destcol = valInt(elements[n]);
	      break;
	    }
	  }

	  do
	  { str_store(buf, o++, ' ');
	    col++;
	  } while ( col != destcol );
	} else
	{ str_store(buf, o++, c);
	  if ( c == '\n' )
	    col = 0;
	  else
	    col++;
	}
      }
      buf->size = o;

      return setString(str, buf);
    }
  } else if ( (n = checkType(tabs, TypeInt, NIL)) )
  { int size = str->data.size;
    int d = valInt(n);
    String s = &str->data;
    int tabs = str_count_chr(s, 0, size, '\t');
    LocalString(buf, s, size + d * tabs);
    int i=0, o=0, col=0;

    for( ; i < size; i++ )
    { wchar c = str_fetch(s, i);

      if ( c == '\t' )
      { do
	{ str_store(buf, o++, ' ');
	  col++;
	} while ( col % d );
      } else
      { str_store(buf, o++, c);
	if ( c == '\n' )
	  col = 0;
	else
	  col++;
      }
    }
    buf->size = o;

    return setString(str, buf);
  }

  fail;
}


static void
prepareWriteString(StringObj s)
{ if ( s->data.readonly )
    setString(s, &s->data);
}


status
upcaseString(StringObj s)
{ prepareWriteString(s);

  str_upcase(&s->data, 0, s->data.size);
  return setString(s, &s->data);
}


static status
downcaseString(StringObj s)
{ prepareWriteString(s);

  str_downcase(&s->data, 0, s->data.size);
  return setString(s, &s->data);
}


static status
truncateString(StringObj s, Int n)
{ return deleteString(s, n, DEFAULT);
}


static status
translateString(StringObj str, Int c1, Int c2)
{ wchar f = valInt(c1);
  int changed = 0;
  String s = &str->data;
  int size = s->size;
  int i = 0;

  prepareWriteString(str);

  if ( notNil(c2) )
  { wchar t = valInt(c2);

    for(;;)
    { if ( (i = str_next_index(s, i, f)) >= 0 )
      { str_store(s, i++, t);
	changed++;
      } else
	break;
    }

    if ( changed )
      setString(str, &str->data);	/* forward changes */
  } else				/* delete c1's */
  { LocalString(buf, s, size);
    int o = 0;

    for(;;)
    { int ni;

      if ( (ni = str_next_index(s, i, f)) >= 0 )
      { str_ncpy(buf, o, s, i, ni-i);
	o += ni-i;
	i = ni+1;
	changed++;
      } else
	break;
    }
    if ( changed )
    { str_ncpy(buf, o, s, i, size-i);
      o += size-i;
      buf->size = o;

      setString(str, buf);
    }
  }

  succeed;
}


static status
characterString(StringObj str, Int index, Int chr)
{ int i = valInt(index);
  wchar c = valInt(chr);

  if ( i <  0 || i >= str->data.size )
    fail;

  if ( str_fetch(&str->data, i) != c )
  { prepareWriteString(str);
    str_store(&str->data, i, c);
    setString(str, &str->data);
  }

  succeed;
}


status
deleteString(StringObj str, Int start, Int length)
{ String s = &str->data;
  int size = s->size;
  int f = valInt(start);
  int e = (isDefault(length) ? size : valInt(length)) + f - 1;
  int d;

  if ( f <  0    ) s = 0;
  if ( f >= size ) succeed;
  if ( e <  f    ) succeed;
  if ( e >= size )
    e = size - 1;
  d = e - f + 1;

  { LocalString(buf, s, size-d);

    str_ncpy(buf, 0, s, 0, f);
    str_ncpy(buf, f, s, e+1, size - (e+1));
    buf->size = size-d;

    setString(str, buf);
  }

  succeed;
}


status
insertString(StringObj s1, Int n, CharArray s2)
{ return str_insert_string(s1, n, &s2->data);
}


 		/********************************
		*    NON-PCE-TYPE MANIPULATION  *
		*********************************/

static status
setString(StringObj str, String s)
{ Class class = classOfObject(str);

  if ( str->data.s_text != s->s_text ||
       str_allocsize(&str->data) != str_allocsize(s) ||
       str->data.readonly )
  { string s2 = *s;

    DEBUG(NAME_readOnly,
	  if ( str->data.readonly )
	    Cprintf("Copying %s", pp(str)));

    str_alloc(&s2);
    memcpy(s2.s_text8, s->s_text8, str_datasize(s));
    str_unalloc(&str->data);
    str->data = s2;
  } else
    str->data = *s;

  if ( notNil(class->changed_messages) )
    changedObject(str, NAME_text, 0);

  succeed;
}


static status
CsetString(StringObj str, char *txt)
{ int l = strlen(txt);
  string s;

  s.size = l;
  s.encoding = ENC_ASCII;
  s.b16 = 0;
  s.pad = 0;
  s.s_text8 = (char8*) txt;

  return setString(str, &s);
}


status
str_insert_string(StringObj str, Int where, String s)
{ int sz = str->data.size;
  LocalString(buf, &str->data, sz + s->size);
  int p = (isDefault(where) ? sz : valInt(where));

  if ( p < 0  ) p = 0;
  if ( p > sz ) p = sz;

  str_ncpy(buf, 0, &str->data, 0, p);
  str_ncpy(buf, p, s, 0, s->size);
  str_ncpy(buf, p+s->size, &str->data, p, str->data.size - p);
  buf->size = sz + s->size;

  return setString(str, buf);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_insert[] =
        { "at=[int]", "text=char_array" };
static char *T_character[] =
        { "at=int", "char=char" };
static char *T_insertCharacter[] =
        { "char=char", "at=[0..]", "times=[0..]" };
static char *T_formatADchar_arrayD_argumentAany_XXX[] =
        { "format=[char_array]", "argument=any ..." };
static char *T_translate[] =
        { "from=char", "into=char*" };
static char *T_delete[] =
        { "from=int", "length=[int]" };
static char *T_ensureSuffix[] =
	{ "text=char_array", "ignore_case=[bool]" };

/* Instance Variables */

#define var_string NULL
/*
vardecl var_string[] =
{ 
};
*/

/* Send Methods */

static senddecl send_string[] =
{ SM(NAME_initialise, 2, T_formatADchar_arrayD_argumentAany_XXX, initialiseStringv,
     DEFAULT, "Create a string, initialise as ->format"),
  SM(NAME_downcase, 0, NULL, downcaseString,
     NAME_case, "Change all letters in string to lower case"),
  SM(NAME_upcase, 0, NULL, upcaseString,
     NAME_case, "Change all letters in string to upper case"),
  SM(NAME_append, 1, "text=char_array", appendString,
     NAME_content, "Append to the string"),
  SM(NAME_character, 2, T_character, characterString,
     NAME_content, "Change character at 0-based index"),
  SM(NAME_delete, 2, T_delete, deleteString,
     NAME_content, "Delete range from 0-based start and length"),
  SM(NAME_ensureNl, 1, "[char_array]", ensureNlString,
     NAME_content, "Ensure string has trailing newline [and append string]"),
  SM(NAME_ensureSuffix, 2, T_ensureSuffix, ensureSuffixString,
     NAME_content, "Ensure string has indicated suffix"),
  SM(NAME_insert, 2, T_insert, insertString,
     NAME_content, "Insert string at 0-based index"),
  SM(NAME_insertCharacter, 3, T_insertCharacter, insertCharacterString,
     NAME_content, "Insert times character(s) at location"),
  SM(NAME_newline, 1, "times=[0..]", newlineString,
     NAME_content, "Append a newline to string"),
  SM(NAME_prepend, 1, "char_array", prependString,
     NAME_content, "Add argument at the beginning"),
  SM(NAME_strip, 1, "[{leading,trailing}]", stripString,
     NAME_content, "Strip leading/trailing blanks"),
  SM(NAME_translate, 2, T_translate, translateString,
     NAME_content, "Map occurrences of 1-st arg into 2-nd arg"),
  SM(NAME_truncate, 1, "int", truncateString,
     NAME_content, "Truncate string to argument characters"),
  SM(NAME_value, 1, "text=char_array", valueString,
     NAME_copy, "Set the contents of the string"),
  SM(NAME_bitsPerCharacter, 1, "int", bitsPerCharacterString,
     NAME_encoding, "8- or 16-bits per character"),
  SM(NAME_format, 2, T_formatADchar_arrayD_argumentAany_XXX, formatString,
     NAME_format, "Format (like printf) in string"),
  SM(NAME_untabify, 1, "tabs=[int|vector]", untabifyString,
     NAME_indentation, "Replace tab characters by spaces")
};

/* Get Methods */

static getdecl get_string[] =
{ GM(NAME_convert, 1, "string", "any", convertString,
     DEFAULT, "Convert name, int, real, etc."),
  GM(NAME_modify, 1, "string", "char_array", getModifyString,
     DEFAULT, "Make modified version"),
  GM(NAME_copy, 0, "string", NULL, getCopyString,
     NAME_copy, "Copy with the same text")
};

/* Resources */

#define rc_string NULL
/*
static resourcedecl rc_string[] =
{ 
};
*/

/* Class Declaration */

static Name string_termnames[] = { NAME_value };

ClassDecl(string_decls,
          var_string, send_string, get_string, rc_string,
          1, string_termnames,
          "$Rev$");

status
makeClassString(Class class)
{ declareClass(class, &string_decls);
  setLoadStoreFunctionClass(class, loadString, storeString);

  succeed;
}

