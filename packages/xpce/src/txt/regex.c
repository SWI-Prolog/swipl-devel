/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/text.h>		/* text_buffer */
#include <gnu/gregex.h>

forwards Int getRegisterStartRegex(Regex, Int);

#define BYTEWIDTH 8
#define FASTMAPSIZE (1<<BYTEWIDTH)

NewClass(regex)
  CharArray		    pattern;	/* Pattern matched */
  struct re_pattern_buffer *compiled;	/* Compiled regex */
  struct re_registers	   *registers;	/* \0-\9 matches */
End;


static status
initialiseRegex(Regex re, CharArray pattern)
{ assign(re, pattern, pattern);

  re->registers    = NULL;
  re->compiled     = alloc(sizeof(struct re_pattern_buffer));
  memset(re->compiled, 0, sizeof(struct re_pattern_buffer));

  succeed;
}


static void
unlink_registers(Regex re)
{ if ( re->registers != NULL )
  {
    if ( re->registers->start )
      free(re->registers->start);	/* malloc'ed by gnu-regex.c */
    if ( re->registers->end )
      free(re->registers->end);		/* same */
    unalloc(sizeof(struct re_registers), re->registers);
    re->registers = NULL;
  }
}


static void
unlink_compiled_buffer(Regex re)
{ if ( re->compiled != NULL )
  { if ( re->compiled->buffer != NULL )
    { free(re->compiled->buffer);		/* malloc'ed by gnu-regex.c */
      re->compiled->buffer = NULL;
    }
    if ( re->compiled->fastmap != NULL )
    { free(re->compiled->fastmap);
      re->compiled->fastmap = NULL;
    }
  }
}


static void
unlink_compiled(Regex re)
{ if ( re->compiled != NULL )
  { unlink_compiled_buffer(re);
    unalloc(sizeof(struct re_pattern_buffer), re->compiled);
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
loadRegex(Regex re, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(re, fd, def));

  re->registers	  	  = NULL;
  re->compiled 		  = alloc(sizeof(struct re_pattern_buffer));
  memset(re->compiled, 0, sizeof(struct re_pattern_buffer));

  succeed;
}


static status
cloneRegex(Regex re, Regex clone)
{ clonePceSlots(re, clone);

  re->registers		  = NULL;
  re->compiled 		  = alloc(sizeof(struct re_pattern_buffer));
  memset(re->compiled, 0, sizeof(struct re_pattern_buffer));

  succeed;
}


static Regex
getConvertRegex(Class class, Any name)
{ answer(answerObject(ClassRegex, name, 0));
}


static status
patternRegex(Regex re, StringObj pattern)
{ assign(re, pattern, pattern);

  unlink_registers(re);
  unlink_compiled_buffer(re);

  succeed;
}


static char upcase[0400] = 
{ 000, 001, 002, 003, 004, 005, 006, 007,
  010, 011, 012, 013, 014, 015, 016, 017,
  020, 021, 022, 023, 024, 025, 026, 027,
  030, 031, 032, 033, 034, 035, 036, 037,
  040, 041, 042, 043, 044, 045, 046, 047,
  050, 051, 052, 053, 054, 055, 056, 057,
  060, 061, 062, 063, 064, 065, 066, 067,
  070, 071, 072, 073, 074, 075, 076, 077,
  0100, 0101, 0102, 0103, 0104, 0105, 0106, 0107,
  0110, 0111, 0112, 0113, 0114, 0115, 0116, 0117,
  0120, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
  0130, 0131, 0132, 0133, 0134, 0135, 0136, 0137,
  0140, 0101, 0102, 0103, 0104, 0105, 0106, 0107,
  0110, 0111, 0112, 0113, 0114, 0115, 0116, 0117,
  0120, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
  0130, 0131, 0132, 0173, 0174, 0175, 0176, 0177,
  0200, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
  0210, 0211, 0212, 0213, 0214, 0215, 0216, 0217,
  0220, 0221, 0222, 0223, 0224, 0225, 0226, 0227,
  0230, 0231, 0232, 0233, 0234, 0235, 0236, 0237,
  0240, 0241, 0242, 0243, 0244, 0245, 0246, 0247,
  0250, 0251, 0252, 0253, 0254, 0255, 0256, 0257,
  0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267,
  0270, 0271, 0272, 0273, 0274, 0275, 0276, 0277,
  0300, 0301, 0302, 0303, 0304, 0305, 0306, 0307,
  0310, 0311, 0312, 0313, 0314, 0315, 0316, 0317,
  0320, 0321, 0322, 0323, 0324, 0325, 0326, 0327,
  0330, 0331, 0332, 0333, 0334, 0335, 0336, 0337,
  0340, 0341, 0342, 0343, 0344, 0345, 0346, 0347,
  0350, 0351, 0352, 0353, 0354, 0355, 0356, 0357,
  0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
  0370, 0371, 0372, 0373, 0374, 0375, 0376, 0377
};


static Bool
getIgnoreCaseRegex(Regex re)
{ answer(re->compiled->translate == NULL ? OFF : ON);
}


static status
ignoreCaseRegex(Regex re, Bool val)
{ if ( getIgnoreCaseRegex(re) != val )
  { re->compiled->translate = (val == OFF ? NULL : upcase);
    unlink_registers(re);
    unlink_compiled_buffer(re);		/* force recompilation */
  }

  succeed;
}


status
compileRegex(Regex re, Bool optimize)
{ if ( re->compiled->buffer == NULL )
  { char *error;

    re->compiled->allocated = 0;
    re->compiled->fastmap   = (optimize == ON ? pceMalloc(FASTMAPSIZE) : NULL);

    re_set_syntax(RE_SYNTAX_EMACS);
    if ( (error = (char *) re_compile_pattern(re->pattern->data.s_text,
					      re->pattern->data.size,
					      re->compiled)) != NULL )
      return errorPce(re, NAME_syntaxError, CtoName(error));

    re->registers = alloc(sizeof(struct re_registers));
    re->registers->start = re->registers->end = NULL;
    re->compiled->regs_allocated = REGS_UNALLOCATED;
  }

  succeed;
}


status
search_regex(Regex re, char *str1, int size1, char *str2, int size2,
	     int start, int end)
{ TRY(compileRegex(re, size1+size2 > 100 ? ON : OFF));

  if ( end < start )
  { for(;;)
    { switch( re_search_2(re->compiled,
			  str1, size1, str2, size2,
			  start, end-start, re->registers, start) )
      { case -1:
	  fail;
	case -2:
	  return errorPce(re, NAME_internalError);
	default:
	  if ( re->registers->end[0] > start )
	  { DEBUG(NAME_regex, Cprintf("Match to %d\n", re->registers->end[0]));

	    if ( --start < 0 )
	      fail;
	    continue;
	  }

	  succeed;
      }
    }
  } else
  { switch( re_search_2(re->compiled,
			str1, size1, str2, size2,
			start, end-start, re->registers, end) )
    { case -1:
	fail;
      case -2:
	return errorPce(re, NAME_internalError);
      default:
	succeed;
    }
  }
}


status
searchRegex(Regex re, Any obj, Int start, Int end)
{ int from = isDefault(start) ? 0 : valInt(start);

  if ( instanceOfObject(obj, ClassCharArray) )
  { CharArray ca = obj;			/* TBD: 16-bit? */
    char *s = ca->data.s_text;
    int ls = ca->data.size;

    return search_regex(re, s, ls, NULL, 0,
			from, isDefault(end) ? ls : valInt(end));
  } else if ( instanceOfObject(obj, ClassTextBuffer) )
  { TextBuffer tb = obj;

    if ( tb->buffer.b16 )
      return search_regex(re,
			  (char *)&tb->tb_buffer16[0],
			  tb->gap_start*2,
			  (char *)&tb->tb_buffer16[tb->gap_end+1],
			  (tb->size - tb->gap_start)*2,
			  from*2,
			  (isDefault(end) ? tb->size : valInt(end))*2);
    else
      return search_regex(re,
			  &tb->tb_buffer8[0],
			  tb->gap_start,
			  &tb->tb_buffer8[tb->gap_end+1],
			  tb->size - tb->gap_start,
			  from,
			  isDefault(end) ? tb->size : valInt(end));
  } else if ( instanceOfObject(obj, ClassFragment) )
  { Fragment frag = obj;
    TextBuffer tb = frag->textbuffer;
    status rval;

    if ( tb->buffer.b16 )
      rval = search_regex(re,
			  (char *)tb->tb_buffer16, tb->gap_start*2,
			  (char *)&tb->tb_buffer16[tb->gap_end+1],
			  (tb->size - tb->gap_start)*2,
			  (from+frag->start)*2,
			  (isDefault(end) ? frag->start + frag->length
			  		  : valInt(end)+frag->start)*2);
    else
      rval = search_regex(re,
			  tb->tb_buffer8, tb->gap_start,
			  &tb->tb_buffer8[tb->gap_end+1],
			  tb->size - tb->gap_start,
			  from+frag->start,
			  isDefault(end) ? frag->start + frag->length
			  		 : valInt(end)+frag->start);

    if ( rval )
    { int n;

      for(n=0; re->registers->start[n] >= 0; n++)
      { re->registers->start[n] -= frag->start;
	re->registers->end[n] -= frag->start;
      }
    }

    return rval;
  }
   
  fail;
}


static Int
getSearchRegex(Regex re, Any obj, Int start, Int end)
{ TRY(searchRegex(re, obj, start, end));

  answer(getRegisterStartRegex(re, ZERO));
}


static Int
match_regex(Regex re, char *str1, int size1, char *str2, int size2, int start, int end)
{ int n;
  TRY(compileRegex(re, size1+size2 > 100 ? ON : OFF));

  if ( end >= start )			/* forward */
  { switch( (n = re_match_2(re->compiled,
			    (unsigned char *)str1, size1,
			    (unsigned char *)str2, size2,
			    start, re->registers, end)) )
    { case -1:
	fail;
      case -2:
	{ errorPce(re, NAME_internalError);
	  fail;
	}
      default:
	return toInt(n);
    }
  } else				/* backward */
  { int s = start;
    int n;

    for(s = start; s >= end; s--)
    { DEBUG(NAME_regex, Cprintf("Match in %d .. %d\n", s, start));
      switch( (n = re_match_2(re->compiled,
			      (unsigned char *)str1, size1,
			      (unsigned char *)str2, size2,
			      s, re->registers, start)) )
      { case -1:
	  continue;
	case -2:
	  { errorPce(re, NAME_internalError);
	    fail;
	  }
	default:
	  if ( s + n == start )
	    return toInt(n);
      }
    }
    fail;
  }
}


Int
getMatchRegex(Regex re, Any obj, Int start, Int end)
{ int from = isDefault(start) ? 0 : valInt(start);

  if ( instanceOfObject(obj, ClassCharArray) )
  { CharArray ca = obj;
    char *s = ca->data.s_text;
    int ls = ca->data.size;

    return match_regex(re, s, ls, NULL, 0,
		       from, isDefault(end) ? ls : valInt(end));
  } else if ( instanceOfObject(obj, ClassTextBuffer) )
  { TextBuffer tb = obj;

    if ( tb->buffer.b16 )
      return match_regex(re,
			 (char *)tb->tb_buffer16, tb->gap_start*2,
			 (char *)&tb->tb_buffer16[tb->gap_end+1],
			 (tb->size - tb->gap_start)*2,
			 from*2,
			 (isDefault(end) ? tb->size : valInt(end))*2);
    else
      return match_regex(re,
			 tb->tb_buffer8, tb->gap_start,
			 &tb->tb_buffer8[tb->gap_end+1],
			 tb->size - tb->gap_start,
			 from, isDefault(end) ? tb->size : valInt(end));
  } else if ( instanceOfObject(obj, ClassFragment) )
  { Fragment frag = obj;
    TextBuffer tb = frag->textbuffer;
    Int rval;

    if ( tb->buffer.b16 )
      rval = match_regex(re,
			 (char *)tb->tb_buffer16, tb->gap_start*2,
			 (char *)&tb->tb_buffer16[tb->gap_end+1],
			 (tb->size - tb->gap_start)*2,
			 (from+frag->start)*2,
			 (isDefault(end) ? frag->start + frag->length
			 		 : valInt(end)+frag->start)*2);
    else
      rval = match_regex(re,
			 tb->tb_buffer8, tb->gap_start,
			 &tb->tb_buffer8[tb->gap_end+1],
			 tb->size - tb->gap_start,
			 from+frag->start,
			 isDefault(end) ? frag->start + frag->length
			 		: valInt(end)+frag->start);
    
    if ( rval )
    { int n;

      for(n=0; re->registers->start[n] >= 0; n++)
      { re->registers->start[n] -= frag->start;
	re->registers->end[n] -= frag->start;
      }
    }

    answer(rval);
  }
   
  fail;
}


status
matchRegex(Regex re, Any obj, Int start, Int end)
{ return getMatchRegex(re, obj, start, end) ? SUCCEED : FAIL;
}


#define validRegister(re, n) ((n) >= 0 && \
			      re->registers && \
			      re->registers->start[(n)] >= 0)


static Int
getRegisterStartRegex(Regex re, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);

  if ( !validRegister(re, n) )
    fail;

  answer(toInt(re->registers->start[n]));
}


Int
getRegisterEndRegex(Regex re, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);

  if ( !validRegister(re, n) )
    fail;

  answer(toInt(re->registers->end[n]));
}


static Int
getRegisterSizeRegex(Regex re, Int which)
{ int n = isDefault(which) ? 0 : valInt(which);

  if ( !validRegister(re, n) )
    fail;

  answer(toInt(re->registers->end[n] - re->registers->start[n]));
}


static CharArray
getRegisterValueRegex(Regex re, Any obj, Int which, Type type)
{ int n = isDefault(which) ? 0 : valInt(which);
  Any argv[2];
  Any rval;

  if ( !validRegister(re, n) )
    fail;

  argv[0] = toInt(re->registers->start[n]);
  argv[1] = toInt(re->registers->end[n]);

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

  start = re->registers->start[n];
  len   = re->registers->end[n] - start;
  shift = valInt(getSizeCharArray(value)) - len;

  argv[0] = toInt(start);
  argv[1] = toInt(len);

  if ( sendv(obj, NAME_delete, 2, argv) &&
       (argv[1] = value) &&
       sendv(obj, NAME_insert, 2, argv) )
  { for(n=0; re->registers->start[n] >= 0; n++)
    { if ( re->registers->start[n] > start )
	re->registers->start[n] += shift;
      if ( re->registers->end[n] >= start )
	re->registers->end[n] += shift;
    }
    
    succeed;
  }

  fail;
}


static status
replaceRegex(Regex re, Any obj, CharArray value)
{ String s = &value->data;
  LocalString(buf, s, FORMATSIZE);
  int o, i, size = s->size;
  CharArray repl;
  status rval;

  for(i=o=0; i<size; i++)
  { wchar c = str_fetch(s, i);

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
  { int oe = re->registers->end[0];
    int ne;

    TRY(forwardCode(code, re, obj, 0));

    ne = re->registers->end[0];
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
  LocalString(buf, s, LINESIZE);
  int i, o=0;

  if ( str_fetch(s, 0) == '^' )
    str_store(buf, o++, '\\');

  for( i=0; i < size; i++)
  { wchar c = str_fetch(s, i);

    switch(c)
    { case '?':
      case '[':
      case ']':
      case '\\':
      case '*':
      case '+':
      case '.':
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



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_forAll[] =
        { "in=char_array|text_buffer|fragment", "action=code", "from=[int]", "to=[int]" };
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
{ IV(NAME_pattern, "char_array", IV_GET,
     NAME_pattern, "Pattern to search for"),
  IV(NAME_compiled, "alien:struct re_pattern_buffer *", IV_NONE,
     NAME_compilation, "Buffer holding compiled pattern"),
  IV(NAME_registers, "alien:struct re_registers *", IV_NONE,
     NAME_registers, "Registers for (sub-)matches")
};

/* Send Methods */

static senddecl send_regex[] =
{ SM(NAME_initialise, 1, "pattern=char_array", initialiseRegex,
     DEFAULT, "Create regex from pattern"),
  SM(NAME_unlink, 0, NULL, unlinkRegex,
     DEFAULT, "Deallocate private storage"),
  SM(NAME_compile, 1, "optimise=[bool]", compileRegex,
     NAME_compilation, "Compile expression (optimized)"),
  SM(NAME_forAll, 4, T_forAll, forAllRegex,
     NAME_iterate, "Run code on each match"),
  SM(NAME_ignoreCase, 1, "ignore=bool", ignoreCaseRegex,
     NAME_pattern, "@on: case is ignored during match"),
  SM(NAME_pattern, 1, "pattern=char_array", patternRegex,
     NAME_pattern, "Set pattern searched for"),
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
  GM(NAME_ignoreCase, 0, "ignore=bool", NULL, getIgnoreCaseRegex,
     NAME_pattern, "@on if case is ignored during match"),
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
static resourcedecl rc_regex[] =
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

