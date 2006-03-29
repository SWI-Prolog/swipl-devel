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
#include <h/text.h>		/* text_buffer */
#include <rgx/regex.h>

forwards Int getRegisterStartRegex(Regex, Int);

NewClass(regex)
  CharArray		    pattern;	/* Pattern matched */
  Name			    syntax;	/* basic, extended, advanced */
  Bool			    ignore_case;/* @on --> case insensitive */
  Int			    re_flags;	/* REG_* FLAGS */
  regex_t		   *compiled;	/* compiled regex */
  regmatch_t		   *registers;	/* \0-\9 matches */
End;


static status
initialiseRegex(Regex re, CharArray pattern,
		Bool case_sensitive,
		Name syntax)
{ if ( isDefault(pattern) )
    pattern = (CharArray)NAME_;
  if ( isDefault(syntax) )
    syntax = NAME_advanced;
  assign(re, pattern, pattern);

  if ( case_sensitive == OFF )
    assign(re, ignore_case, ON);
  else					/* @default, @on */
    assign(re, ignore_case, OFF);
  assign(re, syntax, syntax);

  re->compiled  = NULL;
  re->registers = NULL;

  succeed;
}


static void
unlink_registers(Regex re)
{ if ( re->registers != NULL )
  { pceFree(re->registers);
    re->registers = NULL;
  }
}


static void
unlink_compiled(Regex re)
{ if ( re->compiled != NULL )
  { regfree(re->compiled);
    pceFree(re->compiled);

    re->compiled = NULL;
  }
}


static status
unlinkRegex(Regex re)
{ unlink_registers(re);
  unlink_compiled(re);

  succeed;
}


static status
storeRegex(Regex re, FileObj file)
{ return storeSlotsObject(re, file);
}


static status
loadRegex(Regex re, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(re, fd, def));

  re->registers	  	  = NULL;
  re->compiled 		  = NULL;

  succeed;
}


static status
cloneRegex(Regex re, Regex clone)
{ clonePceSlots(re, clone);

  re->registers		  = NULL;
  re->compiled 		  = NULL;

  succeed;
}


static Regex
getConvertRegex(Class class, Any name)
{ answer(answerObject(ClassRegex, name, EAV));
}


static status
patternRegex(Regex re, StringObj pattern)
{ assign(re, pattern, pattern);

  unlink_registers(re);
  unlink_compiled(re);

  succeed;
}


status
ignoreCaseRegex(Regex re, Bool val)
{ if ( re->ignore_case != val )
  { assign(re, ignore_case, val);

    unlink_registers(re);
    unlink_compiled(re);		/* force recompilation */
  }

  succeed;
}


static status
syntaxRegex(Regex re, Name syntax)
{ if ( re->syntax != syntax )
  { assign(re, syntax, syntax);

    unlink_registers(re);
    unlink_compiled(re);		/* force recompilation */
  }

  succeed;
}


status
compileRegex(Regex re, Bool optimize)
{ succeed;				/* backward compatibility */
}


static status
error_regex(Regex re, int rc)
{ if ( rc != REG_NOMATCH )
  { char buf[1024];

    regerror(rc, re->compiled, buf, sizeof(buf));

    return errorPce(re, NAME_syntaxError, CtoName(buf));
  }

  fail;					/* normal match failure */
}


#define RE_MATCH	0x0001
#define RE_SEARCH	0x0002

static status
ensure_compiled_regex(Regex re, int flags)
{ int myflags = REG_NLANCH;		/* make ^$ match around \n */

  if ( re->ignore_case == ON )
    myflags |= REG_ICASE;
  if ( (flags & RE_MATCH) )
    myflags |= REG_BOSONLY;

  if ( re->syntax == NAME_basic )
    myflags |= REG_BASIC;
  else if ( re->syntax == NAME_extended )
    myflags |= REG_EXTENDED;
  else
    myflags |= REG_ADVANCED;

  if ( !re->compiled ||
       isNil(re->re_flags) ||
       valInt(re->re_flags) != myflags )
  { int rc;
    wchar_t *ws;
    size_t len;

    unlink_compiled(re);
    unlink_registers(re);

    ws = charArrayToWC(re->pattern, &len);
    re->compiled = pceMalloc(sizeof(regex_t));
    rc = re_compileW(re->compiled, ws, len, myflags);

    if ( rc == REG_OKAY )
    { re->registers = pceMalloc(sizeof(regmatch_t)*(re->compiled->re_nsub+1));
      assign(re, re_flags, toInt(myflags));
    } else
    { error_regex(re, rc);
      pceFree(re->compiled);
      re->compiled = NULL;

      fail;
    }
  }

  succeed;
}




#define IDXOFF		0x1000
#define IDX2PTR(i)	(charW*)((i)*sizeof(charW)+IDXOFF)
#define PTR2IDX(p)	((long)(p)-IDXOFF)/sizeof(charW)


static int
re_fetch_string(const charW *at, void *closure)
{ String str = closure;
  long idx = PTR2IDX(at);

  return str_fetch(str, idx);
}


static int
re_fetch_textbuffer(const charW *at, void *closure)
{ TextBuffer tb = closure;
  long idx = PTR2IDX(at);

  return fetch_textbuffer(tb, idx);
}


static int
re_fetch_fragment(const charW *at, void *closure)
{ Fragment f = closure;
  long idx = PTR2IDX(at);

  return fetch_textbuffer(f->textbuffer, idx+f->start);
}


status
search_string_regex(Regex re, String s)
{ int rc;

  if ( !ensure_compiled_regex(re, RE_SEARCH) )
    fail;
  
  rc = re_execW(re->compiled, IDX2PTR(0), s->size,
		re_fetch_string, s,
		NULL,
		re->compiled->re_nsub+1, re->registers, 0);

  if ( rc == 0 )
    succeed;
  else
    return error_regex(re, rc);
}



static status
search_regex(Regex re, Any obj, Int start, Int end, int flags)
{ int from = isDefault(start) ? 0 : valInt(start);
  int len, to, eflags = 0;
  int (*fetch)(const charW*, void*);
  void *closure;

  if ( instanceOfObject(obj, ClassCharArray) )
  { CharArray ca = obj;			/* TBD: 16-bit? */
    String s = &ca->data;

    len = s->size;
    fetch = re_fetch_string;
    closure = s;
  } else if ( instanceOfObject(obj, ClassTextBuffer) )
  { TextBuffer tb = obj;
    
    len = tb->size;
    fetch = re_fetch_textbuffer;
    closure = tb;

  } else if ( instanceOfObject(obj, ClassFragment) )
  { Fragment frag = obj;

    len = frag->length;
    fetch = re_fetch_fragment;
    closure = frag;
  } else
    fail;
   
  if ( isDefault(end) )
  { to = len;
  } else
  { to = valInt(end);
    if ( to < 0   ) to = 0;
    if ( to > len ) to = len;
  }

  if ( isDefault(start) )
  { from = 0;
  } else
  { from = valInt(start);
    if ( from < 0   ) from = 0;
    if ( from > len ) from = len;
  }

  if ( from <= to )			/* forwards */
  { int rc;

					  /* partial match, check ends */
    if ( from > 0 && (*fetch)(IDX2PTR(from-1), closure) != '\n' )
      eflags |= REG_NOTBOL;
    if ( to < len && (*fetch)(IDX2PTR(to), closure) != '\n' )
      eflags |= REG_NOTEOL;

    if ( !ensure_compiled_regex(re, flags) ) /* RE_SEARCH/RE_MATCH */
      fail;

    rc = re_execW(re->compiled, IDX2PTR(from), to-from,
		  fetch, closure,
		  NULL,
		  re->compiled->re_nsub+1, re->registers, eflags);
    if ( rc == 0 )
    { if ( from != 0 )
      { int n;

	for(n=0; n <= re->compiled->re_nsub; n++)
	{ re->registers[n].rm_so += from;
	  re->registers[n].rm_eo += from;
	}
      }
      succeed;
    } else
      return error_regex(re, rc);
  } else				/* backward search */
  { int rc;
    int here;
    int match = -1;

    if ( !ensure_compiled_regex(re, RE_MATCH) )
      fail;

    if ( from < len && (*fetch)(IDX2PTR(from), closure) != '\n' )
      eflags |= REG_NOTEOL;

    for(here = from; here >= to; here--)
    { eflags &= ~REG_NOTBOL;

      if ( here > 0 && (*fetch)(IDX2PTR(here-1), closure) != '\n' )
	eflags |= REG_NOTBOL;

      rc = re_execW(re->compiled, IDX2PTR(here), from-here,
		    fetch, closure,
		    NULL,
		    re->compiled->re_nsub+1, re->registers, eflags);
      switch(rc)
      { case REG_NOMATCH:
	{ int n;

	  if ( match == -1 )
	    continue;
	  rc = re_execW(re->compiled, IDX2PTR(match), from-match,
			fetch, closure,
			NULL,
			re->compiled->re_nsub+1, re->registers, 0);
	  assert(rc == REG_OKAY);
	adjust:
	  if ( flags == RE_MATCH )
	  { if ( match+re->registers[0].rm_eo != from )
	      fail;
	  }

	  for(n=0; n <= re->compiled->re_nsub; n++)
	  { re->registers[n].rm_so += match;
	    re->registers[n].rm_eo += match;
	  }

	  succeed;
	}
	case REG_OKAY:
	  match = here;
	  if ( here == to )
	    goto adjust;
	  continue;
	default:
	  return error_regex(re, rc);
      }
    }
    fail;			/* no match */
  }
}


status
searchRegex(Regex re, Any obj, Int start, Int end)
{ return search_regex(re, obj, start, end, RE_SEARCH);
}


static Int
getSearchRegex(Regex re, Any obj, Int start, Int end)
{ TRY(searchRegex(re, obj, start, end));

  answer(getRegisterStartRegex(re, ZERO));
}


Int
getMatchRegex(Regex re, Any obj, Int start, Int end)
{ if ( search_regex(re, obj, start, end, RE_MATCH) )
    answer(toInt(re->registers[0].rm_eo -
		 re->registers[0].rm_so));
   
  fail;
}


status
matchRegex(Regex re, Any obj, Int start, Int end)
{ return getMatchRegex(re, obj, start, end) ? SUCCEED : FAIL;
}


#define validRegister(re, n) ((n) >= 0 && \
			      re->compiled && \
			      (n) <= re->compiled->re_nsub)


static Int
getRegisterStartRegex(Regex re, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);

  if ( !validRegister(re, n) )
    fail;

  answer(toInt(re->registers[n].rm_so));
}


Int
getRegisterEndRegex(Regex re, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);

  if ( !validRegister(re, n) )
    fail;

  answer(toInt(re->registers[n].rm_eo));
}


static Int
getRegisterSizeRegex(Regex re, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);

  if ( !validRegister(re, n) )
    fail;

  answer(toInt(re->registers[n].rm_eo - re->registers[n].rm_so));
}


static CharArray
getRegisterValueRegex(Regex re, Any obj, Int which, Type type)
{ int n = isDefault(which) ? 0 : valInt(which);
  Any argv[2];
  Any rval;

  if ( !validRegister(re, n) )
    fail;

  argv[0] = toInt(re->registers[n].rm_so);
  argv[1] = toInt(re->registers[n].rm_eo);

  rval = getv(obj, NAME_sub, 2, argv);
  if ( rval && notDefault(type) )
    answer(checkType(rval, type, obj));

  answer(rval);
}


static status
registerValueRegex(Regex re, Any obj, CharArray value, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);
  Any argv[2];
  int start, len, shift;

  if ( !validRegister(re, n) )
    fail;

  start = re->registers[n].rm_so;
  len   = re->registers[n].rm_eo - start;
  shift = valInt(getSizeCharArray(value)) - len;

  argv[0] = toInt(start);
  argv[1] = toInt(len);

  if ( sendv(obj, NAME_delete, 2, argv) &&
       (argv[1] = value) &&
       sendv(obj, NAME_insert, 2, argv) )
  { for(n=0; n <= re->compiled->re_nsub; n++)
    { if ( re->registers[n].rm_so > start )
	re->registers[n].rm_so += shift;
      if ( re->registers[n].rm_eo >= start )
	re->registers[n].rm_eo += shift;
    }
    
    succeed;
  }

  fail;
}


static status
replaceRegex(Regex re, Any obj, CharArray value)
{ String s = &value->data;
  LocalString(buf, s->iswide, FORMATSIZE);
  int o, i, size = s->size;
  CharArray repl;
  status rval;

  for(i=o=0; i<size; i++)
  { wint_t c = str_fetch(s, i);

    if ( c == '\\' && isdigit(str_fetch(s, i+1)) )
    { CharArray ca;
      Int reg = toInt(str_fetch(s, i+1) - '0');

      if ( (ca = getRegisterValueRegex(re, obj, reg, DEFAULT)) )
      { str_ncpy(buf, o, &ca->data, 0, ca->data.size);
	o += ca->data.size;
	i++;
	continue;
      } else
	errorPce(re, NAME_noRegexRegister, reg, 0);
    } 
    str_store(buf, o, c);
    o++;
  }
  buf->size = o;
  
  repl = StringToScratchCharArray(buf);
  rval = registerValueRegex(re, obj, repl, ZERO);
  doneScratchCharArray(repl);

  return rval;
}


static Int
getRegistersRegex(Regex re)
{ TRY(compileRegex(re, DEFAULT));

  answer(toInt(re->compiled->re_nsub));
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/

static status
forAllRegex(Regex re, Any obj, Code code, Int from, Int to)
{ if ( isDefault(from) )
    from = ZERO;

  while( searchRegex(re, obj, from, to) )
  { int oe = re->registers[0].rm_eo;
    int ne;

    TRY(forwardCode(code, re, obj, EAV));

    ne = re->registers[0].rm_eo;
    if ( ne == valInt(from) && oe == valInt(from) )
      from = toInt(ne+1);
    else
      from = toInt(ne);
  }

  succeed;
}


static CharArray
getPrintNameRegex(Regex re)
{ answer(re->pattern);
}


static StringObj
getQuoteRegex(Regex re, CharArray ca)
{ String s = &ca->data;
  int size = s->size;
  LocalString(buf, s->iswide, LINESIZE);
  int i, o=0;

  if ( str_fetch(s, 0) == '^' )
    str_store(buf, o++, '\\');

  for( i=0; i < size; i++)
  { wint_t c = str_fetch(s, i);

    switch(c)
    { case '?':
      case '[':
      case ']':
      case '\\':
      case '*':
      case '+':
      case '.':
      case '{':
      case '}':
	str_store(buf, o++, '\\');
	break;
      case '$':
	if ( i == size-1 )
	  str_store(buf, o++, '\\');
	break;
    }

    str_store(buf, o++, c);
  }

  buf->size = o;

  answer(StringToString(buf));
}


static status
filePatternRegex(Regex re, CharArray file_pattern)
{ char *s = strName(file_pattern);
  LocalArray(char, tmp, strlen(s)*2+2);
  char *q = tmp;

  for(; *s; s++)
  { switch(*s)
    { case '*':
	*q++ = '.';
        *q++ = '*';
        continue;
      case '?':
	*q++ = '.';
        continue;
      case '\\':
	*q++ = *s++;
        *q++ = *s;
        continue;
      case '[':
	do
	{ *q++ = *s;
	} while(*s && *s++ != ']');
	continue;
      case '{':
	*q++ = '\\';
        *q++ = '(';
        for( ; *s && *s != '}'; s++ )
	{ if ( *s == ',' )
	  { *q++ = '\\';
	    *q++ = '|';
	  } else
	    *q++ = *s;
	}
	if ( *s == '}' )
	{ *q++ = '\\';
	  *q++ = ')';
	}
	continue;
      case '.':
	*q++ = '\\';
	*q++ = *s;
        continue;
      default:
	*q++ = *s;
    }
  }
  *q++ = '$';
  *q = EOS;

  return patternRegex(re, CtoString(tmp));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
	{ "pattern=[char_array]", "case_sensitive=[bool]",
	  "syntax=[{basic,extended,advanced}]"
	};
static char *T_forAll[] =
        { "in=char_array|text_buffer|fragment",
	  "action=code", "from=[int]", "to=[int]" };
static char *T_inAchar_arrayOtext_bufferOfragment_startADintD_endADintD[] =
        { "in=char_array|text_buffer|fragment", "start=[int]", "end=[int]" };
static char *T_gregisterValue[] =
        { "in=object", "register=[0..9]", "type=[type]" };
static char *T_replace[] =
        { "in=object", "value=char_array" };
static char *T_sregisterValue[] =
        { "in=object", "value=char_array", "register=[0..9]" };

/* Instance Variables */

static vardecl var_regex[] =
{ SV(NAME_pattern, "char_array", IV_GET|IV_STORE, patternRegex,
     NAME_pattern, "Pattern to search for"),
  SV(NAME_syntax, "{basic,extended,advanced}", IV_GET|IV_STORE, syntaxRegex,
     NAME_pattern, "Regex syntax used"),
  SV(NAME_ignoreCase, "bool", IV_GET|IV_STORE, ignoreCaseRegex,
     NAME_pattern, "Ignore case while matching?"),
  IV(NAME_flags, "int*", IV_NONE,
     NAME_internal, "REG_* flags"),
  IV(NAME_compiled, "alien:regex_t*", IV_NONE,
     NAME_internal, "Buffer holding compiled pattern"),
  IV(NAME_registers, "alien:regmatch_t*", IV_NONE,
     NAME_internal, "Registers for (sub-)matches")
};

/* Send Methods */

static senddecl send_regex[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseRegex,
     DEFAULT, "Create regex from pattern"),
  SM(NAME_unlink, 0, NULL, unlinkRegex,
     DEFAULT, "Deallocate private storage"),
  SM(NAME_compile, 1, "optimise=[bool]", compileRegex,
     NAME_compilation, "Compile expression (optimized)"),
  SM(NAME_forAll, 4, T_forAll, forAllRegex,
     NAME_iterate, "Run code on each match"),
  SM(NAME_filePattern, 1, "file_pattern=char_array", filePatternRegex,
     NAME_pattern, "Translate file-pattern to regex and ->pattern"),
  SM(NAME_registerValue, 3, T_sregisterValue, registerValueRegex,
     NAME_registers, "Replace indicated register"),
  SM(NAME_replace, 2, T_replace, replaceRegex,
     NAME_registers, "Replace last match which char_array"),
  SM(NAME_match, 3, T_inAchar_arrayOtext_bufferOfragment_startADintD_endADintD, matchRegex,
     NAME_search, "Match regex at indicated position"),
  SM(NAME_search, 3, T_inAchar_arrayOtext_bufferOfragment_startADintD_endADintD, searchRegex,
     NAME_search, "Search regex in object in range [start,end)")
};

/* Get Methods */

static getdecl get_regex[] =
{ GM(NAME_convert, 1, "regex", "char_array", getConvertRegex,
     NAME_conversion, "Converts char_array to regex(pattern)"),
  GM(NAME_quote, 1, "quoted=string", "pattern=char_array", getQuoteRegex,
     NAME_pattern, "Quoted version of argument"),
  GM(NAME_registerEnd, 1, "index=int", "register=[0..9]", getRegisterEndRegex,
     NAME_registers, "End index of register or match"),
  GM(NAME_registerSize, 1, "characters=int", "register=[0..9]", getRegisterSizeRegex,
     NAME_registers, "# characters in register or match"),
  GM(NAME_registerStart, 1, "index=int", "register=[0..9]", getRegisterStartRegex,
     NAME_registers, "Start index of register or match"),
  GM(NAME_registerValue, 3, "register=any", T_gregisterValue, getRegisterValueRegex,
     NAME_registers, "String of specified register or match"),
  GM(NAME_registers, 0, "registers=int", NULL, getRegistersRegex,
     NAME_registers, "Number of \\( ... \\) pairs in pattern"),
  GM(NAME_match, 3, "length=int", T_inAchar_arrayOtext_bufferOfragment_startADintD_endADintD, getMatchRegex,
     NAME_search, "Length of match at indicated position"),
  GM(NAME_search, 3, "start=int", T_inAchar_arrayOtext_bufferOfragment_startADintD_endADintD, getSearchRegex,
     NAME_search, "->search and return start-position of match"),
  GM(NAME_printName, 0, "char_array", NULL, getPrintNameRegex,
     NAME_textual, "Same as <-pattern (support text_item)")
};

/* Resources */

#define rc_regex NULL
/*
static classvardecl rc_regex[] =
{ 
};
*/

/* Class Declaration */

static Name regex_termnames[] = { NAME_pattern };

ClassDecl(regex_decls,
          var_regex, send_regex, get_regex, rc_regex,
          1, regex_termnames,
          "$Rev$");

status
makeClassRegex(Class class)
{ declareClass(class, &regex_decls);
  setLoadStoreFunctionClass(class, loadRegex, storeRegex);
  setCloneFunctionClass(class, cloneRegex);

  succeed;
}

