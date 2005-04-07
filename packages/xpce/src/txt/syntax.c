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
#include <h/unix.h>

static HashTable SyntaxTables;		/* @syntax_tables */

#define FLAGS_SIZE(t)   (sizeof(unsigned short) * valInt(t->size))
#define CONTEXT_SIZE(t) (sizeof(char)  * valInt(t->size))

static status
initialiseSyntaxTable(SyntaxTable t, Name name, SyntaxTable def)
{ unsigned short *flags;
  char *context;

  if ( isDefault(name) )
    name = NIL;

  if ( notDefault(def) )
  { flags = def->table;
    context = def->context;
    assign(t, sentence_end, def->paragraph_end);
    assign(t, paragraph_end, def->paragraph_end);
  } else
  { flags = char_flags;
    context = char_context;
    assign(t, sentence_end,
	   newObject(ClassRegex, CtoName("[.?!]\\s"), EAV));
    assign(t, paragraph_end,
	   newObject(ClassRegex, CtoName("\\s*\n"), EAV));
  }
    
  assign(t, name, name);
  assign(t, size, toInt(256));

  t->table   = alloc(FLAGS_SIZE(t));
  t->context = alloc(CONTEXT_SIZE(t));
  memcpy(t->table,   flags,   FLAGS_SIZE(t));
  memcpy(t->context, context, CONTEXT_SIZE(t));

  if ( notNil(name) )
    appendHashTable(SyntaxTables, name, t);

  succeed;
}


#ifndef WORDS_BIGENDIAN
static void
swapBytesTable(SyntaxTable t)
{
#define swapchr(x, y)	{ unsigned char z; z=x; x=y; y=z; }
  { unsigned char *s;
    int i = FLAGS_SIZE(t);

    for(s = (unsigned char *)t->table; i > 0; s += 2, i-= 2)
    { swapchr(s[0], s[1]);
    }
  }
}
#else /*WORDS_BIGENDIAN*/
#define swapBytesTable(t)
#endif /*WORDS_BIGENDIAN*/


static status
storeSyntaxTable(SyntaxTable t, FileObj file)
{ TRY(storeSlotsObject(t, file));
  swapBytesTable(t);
  Sfwrite(t->table,   sizeof(char), FLAGS_SIZE(t), file->fd);
  swapBytesTable(t);
  Sfwrite(t->context, sizeof(char), CONTEXT_SIZE(t), file->fd);

  succeed;
}


static status
loadSyntaxTable(SyntaxTable t, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(t, fd, def));

  t->table   = alloc(FLAGS_SIZE(t));
  t->context = alloc(CONTEXT_SIZE(t));
  Sfread(t->table,   sizeof(char), FLAGS_SIZE(t), fd);
  Sfread(t->context, sizeof(char), CONTEXT_SIZE(t), fd);

  swapBytesTable(t);

  succeed;
}


static SyntaxTable
getLookupSyntaxTable(Any ctx, Name name)
{ answer(getMemberHashTable(SyntaxTables, name));
}


static SyntaxTable
getConvertSyntaxTable(Any ctx, Name name)
{ SyntaxTable t;

  if ( (t = getLookupSyntaxTable(ctx, name)) )
    answer(t);

  answer(answerObject(ClassSyntaxTable, name, EAV));
}


static status
unlinkSyntaxTable(SyntaxTable t)
{ if ( t->table )
  { unalloc(FLAGS_SIZE(t), t->table);
    t->table = NULL;
  }
  if ( t->context )
  { unalloc(CONTEXT_SIZE(t), t->context);
    t->context = NULL;
  }

  if ( notNil(t->name) )
    deleteHashTable(SyntaxTables, t->name);

  succeed;
}


static status
copySyntaxTable(SyntaxTable t1, SyntaxTable t2)
{ unlinkSyntaxTable(t1);

  assign(t1, size, t2->size);

  t1->table   = alloc(FLAGS_SIZE(t1));
  t1->context = alloc(CONTEXT_SIZE(t1));
  memcpy(t1->table,   t2->table,   FLAGS_SIZE(t1));
  memcpy(t1->context, t2->context, CONTEXT_SIZE(t1));

  succeed;
}


static unsigned short
nameToCode(Name name)
{ if (      name == NAME_uppercaseLetter ) return UC;
  else if ( name == NAME_lowercaseLetter ) return LC;
  else if ( name == NAME_digit )           return DI;
  else if ( name == NAME_wordSeparator )   return WS;
  else if ( name == NAME_symbol )          return SY;
  else if ( name == NAME_openBracket )     return OB;
  else if ( name == NAME_closeBracket )    return CB;
  else if ( name == NAME_endOfLine )       return EL;
  else if ( name == NAME_whiteSpace )      return BL;
  else if ( name == NAME_stringQuote )     return QT;
  else if ( name == NAME_punctuation )     return PU;
  else if ( name == NAME_endOfString )     return EB;
  else if ( name == NAME_commentStart )    return CS;
  else if ( name == NAME_commentEnd )      return CE;
  else if ( name == NAME_letter )          return (UC|LC);
  else if ( name == NAME_word )            return (UC|LC|DI|WS|SY);
  else if ( name == NAME_layout )	   return (EL|BL);
  else					   fail;
}


static status
syntaxSyntaxTable(SyntaxTable t, Int chr, Name kind, Int context)
{ t->table[valInt(chr)]   = nameToCode(kind);
  t->context[valInt(chr)] = isDefault(context) ? 0 : valInt(context);

  if ( notDefault(context) )
  { if ( kind == NAME_openBracket )
    { t->table[valInt(context)]   = CB;
      t->context[valInt(context)] = valInt(chr);
    } else if ( kind == NAME_closeBracket )
    { t->table[valInt(context)]   = OB;
      t->context[valInt(context)] = valInt(chr);
    } else if ( kind == NAME_commentStart )
    { t->table[valInt(context)]   = CS;
      t->context[valInt(chr)]     = 1;
      t->context[valInt(context)] = 2;
    } else if ( kind == NAME_commentEnd )
    { t->table[valInt(context)]   = CE;
      t->context[valInt(chr)]     = 4;
      t->context[valInt(context)] = 8;
    }
  }

  succeed;
}


static status
addSyntaxSyntaxTable(SyntaxTable t, Int chr, Name kind, Int context)
{ t->table[valInt(chr)]   |= nameToCode(kind);

  if ( notDefault(context) )
  { if ( kind == NAME_openBracket )
    { t->table[valInt(context)]   = CB;
      t->context[valInt(context)] = valInt(chr);
      t->context[valInt(chr)]     = valInt(context);
    } else if ( kind == NAME_closeBracket )
    { t->table[valInt(context)]   = OB;
      t->context[valInt(context)] = valInt(chr);
      t->context[valInt(chr)]     = valInt(context);
    } else if ( kind == NAME_commentStart )
    { t->table[valInt(context)]   |= CS;
      t->context[valInt(chr)]     |= 1;
      t->context[valInt(context)] |= 2;
    } else if ( kind == NAME_commentEnd )
    { t->table[valInt(context)]   |= CE;
      t->context[valInt(chr)]     |= 4;
      t->context[valInt(context)] |= 8;
    } else
      t->context[valInt(chr)]  |= valInt(context);
  }

  succeed;
}


static status
contextSyntaxTable(SyntaxTable t, Int chr, Int context)
{ t->context[valInt(chr)] = isDefault(context) ? 0 : valInt(context);

  succeed;
}


static Int
getContextSyntaxTable(SyntaxTable t, Int chr)
{ if ( t->context[valInt(chr)] )
    answer(toInt(t->context[valInt(chr)]));

  fail;
}


static Any
getSyntaxSyntaxTable(SyntaxTable t, Int chr)
{ Any argv[20];
  int argc = 0;
  int code = t->table[valInt(chr)];

  if ( code & UC )	argv[argc++] = NAME_uppercaseLetter;
  if ( code & LC )	argv[argc++] = NAME_lowercaseLetter;
  if ( code & DI )	argv[argc++] = NAME_digit;
  if ( code & WS )	argv[argc++] = NAME_wordSeparator;
  if ( code & SY )	argv[argc++] = NAME_symbol;
  if ( code & OB )	argv[argc++] = NAME_openBracket;
  if ( code & CB )	argv[argc++] = NAME_closeBracket;
  if ( code & EL )	argv[argc++] = NAME_endOfLine;
  if ( code & BL )	argv[argc++] = NAME_whiteSpace;
  if ( code & QT )	argv[argc++] = NAME_stringQuote;
  if ( code & PU )	argv[argc++] = NAME_punctuation;
  if ( code & EB )	argv[argc++] = NAME_endOfString;
  if ( code & CS )	argv[argc++] = NAME_commentStart;
  if ( code & CE )	argv[argc++] = NAME_commentEnd;

  switch(argc)
  { case 0:
      fail;
    case 1:
      answer(argv[0]);
    default:
      answer(answerObjectv(ClassChain, argc, argv));
  }
}


static status
hasSyntaxSyntaxTable(SyntaxTable t, Int chr, Name name)
{ if ( tischtype(t, valInt(chr), nameToCode(name)) )
    succeed;
  
  fail;
}


static Name
getCommentStartSyntax(SyntaxTable t, Int len)
{ if ( isDefault(len) || len == ONE )
  { int i;

    for(i=0; i<valInt(t->size); i++)
      if ( tiscommentstart(t, i) )
      { char buf[2];
	buf[0] = (char) i;
	buf[1] = EOS;
	answer(CtoName(buf));
      }
  } else
  { int i1, i2;

    for(i1=0; i1<valInt(t->size); i1++)
      if ( tiscommentstart1(t, i1) )
	 for(i2=0; i2<valInt(t->size); i2++)
	   if ( tiscommentstart2(t, i2) )
	   { char buf[3];
	     buf[0] = (char) i1;
	     buf[1] = (char) i2;
	     buf[2] = EOS;
	     answer(CtoName(buf));
	   }
  }

  fail;
}


static Name
getCommentEndSyntax(SyntaxTable t, Int len)
{ if ( isDefault(len) || len == ONE )
  { int i;

    for(i=0; i<valInt(t->size); i++)
      if ( tiscommentend(t, i) )
      { char buf[2];
	buf[0] = (char) i;
	buf[1] = EOS;
	answer(CtoName(buf));
      }
  } else
  { int i1, i2;

    for(i1=0; i1<valInt(t->size); i1++)
      if ( tiscommentend1(t, i1) )
	 for(i2=0; i2<valInt(t->size); i2++)
	   if ( tiscommentend2(t, i2) )
	   { char buf[3];
	     buf[0] = (char) i1;
	     buf[1] = (char) i2;
	     buf[2] = EOS;
	     answer(CtoName(buf));
	   }
  }

  fail;
}


status
makeClassSyntaxTable(Class class)
{ defineType("syntax_name",
	     "{uppercase_letter,lowercase_letter,digit,word_separator,"
	     "symbol,open_bracket,close_bracket,end_of_line,white_space,"
	     "string_quote,punctuation,end_of_string,"
	     "comment_start,comment_end,"
	     "letter,word,layout}");

  sourceClass(class, makeClassSyntaxTable, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name*", NAME_get,
	     "Name of this syntax table");
  localClass(class, NAME_size, NAME_storage, "int", NAME_get,
	     "Size of the table");
  localClass(class, NAME_sentenceEnd, NAME_syntax, "regex", NAME_both,
	     "Regular expression for end of sentence");
  localClass(class, NAME_paragraphEnd, NAME_syntax, "regex", NAME_both,
	     "Regular expression for end of paragraph");
  localClass(class, NAME_table, NAME_storage, "alien:ushort *", NAME_none,
	     "Type-flags");
  localClass(class, NAME_context, NAME_storage, "alien:char *", NAME_none,
	     "Context information");

  termClass(class, "syntax_table", 1, NAME_name);
  setLoadStoreFunctionClass(class, loadSyntaxTable, storeSyntaxTable);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "name=[name]*", "prototype=[syntax_table]",
	     "Create name and default table",
	     initialiseSyntaxTable);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Free alien tables",
	     unlinkSyntaxTable);
  sendMethod(class, NAME_copy, NAME_copy, 1, "syntax_table",
	     "Copy contents of argument table",
	     copySyntaxTable);
  sendMethod(class, NAME_syntax, NAME_syntax, 3,
	     "character=char", "category=syntax_name", "context=[char]",
	     "Modify syntax entry for char",
	     syntaxSyntaxTable);
  sendMethod(class, NAME_addSyntax, NAME_syntax, 3,
	     "character=char", "category=syntax_name", "context=[char]",
	     "Add syntax-type for char",
	     addSyntaxSyntaxTable);
  sendMethod(class, NAME_hasSyntax, NAME_test, 2,
	     "character=char", "category=syntax_name",
	     "Test if char has syntax",
	     hasSyntaxSyntaxTable);
  sendMethod(class, NAME_context, NAME_syntax, 2,
	     "character=char", "context=char",
	     "Set context for character",
	     contextSyntaxTable);

  getMethod(class, NAME_context, NAME_syntax, "context=char", 1,
	    "character=char",
	    "Context character for char",
	    getContextSyntaxTable);
  getMethod(class, NAME_syntax, NAME_syntax, "category=name|chain", 1,
	    "character=char",
	    "Syntax for given character",
	    getSyntaxSyntaxTable);
  getMethod(class, NAME_lookup, NAME_oms, "syntax_table", 1, "name",
	    "Lookup in @syntax_tables",
	    getLookupSyntaxTable);
  getMethod(class, NAME_convert, NAME_conversion, "syntax_table", 1, "name",
	    "Convert table-name into table",
	    getConvertSyntaxTable);
  getMethod(class, NAME_commentStart, NAME_syntax, "name", 1, "[1..2]",
	    "Name holding 1- or 2 character comment-start sequence",
	    getCommentStartSyntax);
  getMethod(class, NAME_commentEnd, NAME_syntax, "name", 1, "[1..2]",
	    "Name holding 1- or 2 character comment-end sequence",
	    getCommentEndSyntax);

  SyntaxTables = globalObject(NAME_syntaxTables, ClassHashTable, EAV);
  DefaultSyntaxTable = globalObject(NAME_defaultSyntaxTable, class,
				    NAME_default, EAV);

  succeed;
}

		/********************************
		*            TABLES		*
		********************************/


unsigned short syntax_spec_code[] = {
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/* ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_   16-31 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   32-47 */
BL|EL,  0, QT,  0,  0,  0,  0,  0, OB, CB,  0,  0,  0, SY, PU,  0, 
/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   48-63 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, CS,  0, CE,  0, 
/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   64-79 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   80-95 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, WS, 
/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   96-111 */
    0,  0,  0,  0, DI,  0,  0,  0,  0,  0,  0,  0, LC,  0, EL,  0, 
/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  ^?   112-127 */
    0,  0,  0,  0,  0, UC,  0, AN,  0,  0,  0,  0,  0,  0,  0,  0, 
			  /* 128-255 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 };

#undef XL
#undef xl
#define XL (UC|XD)
#define xl (LC|XD)

unsigned short char_flags[] = {
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
   EB, CT, CT, CT, CT, CT, CT, CT, CT, BL, EL, CT, BL, BL, CT, CT, 
/* ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_   16-31 */
   CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, 
/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   32-47 */
   BL, PU, QT, PU, PU, PU, PU, QT, OB, CB, PU, PU, PU, PU, PU, PU, 
/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   48-63 */
   DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, PU, PU, PU, PU, PU, PU, 
/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   64-79 */
   PU, XL, XL, XL, XL, XL, XL, UC, UC, UC, UC, UC, UC, UC, UC, UC, 
/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   80-95 */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, OB, PU, CB, PU, WS, 
/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   96-111 */
   PU, xl, xl, xl, xl, xl, xl, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  ^?   112-127 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, OB, PU, CB, PU, CT, 
			  /* 128-255 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC };

char char_context[] = {
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/* ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_   16-31 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   32-47 */
    0,  0,  0,  0,  0,  0,  0,  0,')','(',  0,  0,  0,  0,  0,  0, 
/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   48-63 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   64-79 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   80-95 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,']',  0,'[',  0,  0, 
/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   96-111 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  ^?   112-127 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,'}',  0,'{',  0,  0, 
			  /* 128-255 */
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 };
