/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <h/lang.h>
#include <math.h>

#define A_NONE		0		/* no input */
#define A_FILE		1		/* input is a file */
#define A_CHAR_ARRAY	2		/* input is a char_array */
#define A_TEXT_BUFFER	3		/* input in a text_buffer */

NewClass(tokeniser)
  SyntaxTable	syntax;			/* syntax declarations */
  Any		source;			/* input device */
  Chain		stack;			/* push-back */
  HashTable	symbols;		/* (Partial) symbols */
  int		line;			/* current line-no */
  int		access;			/* access-functions */
  int		caret;			/* current location */
End;

static status	closeTokeniser P((Tokeniser));

#define IsEof(c)	((c) == EOF)

/* Problems:

	* UNGETC has to do two chars!
*/

static status
initialiseTokeniser(Tokeniser t, SyntaxTable syntax)
{ assign(t, syntax,  syntax);
  assign(t, symbols, newObject(ClassHashTable, 0));

  t->access = A_NONE;
  t->line = t->caret = 0;

  succeed;
}


static status
cloneTokeniser(Tokeniser t, Tokeniser clone)
{ clonePceSlots(t, clone);
  assign(clone, source, NIL);
  t->access = A_NONE;
  t->line = t->caret = 0;
  
  succeed;
}

		 /*******************************
		 *	 READ ALIEN SLOTS	*
		 *******************************/

Int
getLineTokeniser(Tokeniser t)
{ answer(toInt(t->line));
}


Int
getCaretTokeniser(Tokeniser t)
{ answer(toInt(t->caret));
}


		 /*******************************
		 *	 HANLING <-SOURCE	*
		 *******************************/

Tokeniser
getOpenTokeniser(Tokeniser t, Any source)
{ if ( notNil(t->source) )
  { t = getCloneObject(t);
    assert(t);
  }

  assign(t, source, source);
  t->line  = 1;
  t->caret = 0;

  if ( instanceOfObject(source, ClassFile) )
  { if ( !send(t->source, NAME_open, NAME_read, 0) )
    { assign(t, source, NIL);
      fail;
    }
    t->access = A_FILE;
  } else if ( instanceOfObject(source, ClassCharArray) )
  { t->access = A_CHAR_ARRAY;
  } else if ( instanceOfObject(source, ClassTextBuffer) )
  { t->access = A_TEXT_BUFFER;
  }

  answer(t);
}


static status
closeTokeniser(Tokeniser t)
{ switch(t->access)
  { case A_FILE:
      send(t->source, NAME_close, 0);
  }

  assign(t, source, NIL);
  t->access = A_NONE;

  succeed;
}


static int
GETC(Tokeniser t)
{ int c;

  switch(t->access)
  { case A_FILE:
    { FileObj f = t->source;
      c = getc(f->fd);
      break;
    }
    case A_CHAR_ARRAY:
    { CharArray ca = t->source;
      String s = &ca->data;

      c = (t->caret < s->size ? str_fetch(&ca->data, t->caret) : EOF);
      break;
    }
    case A_TEXT_BUFFER:
    { TextBuffer tb = t->source;
      c = fetch_textbuffer(tb, t->caret);
    }
    default:
      return EOF;
  }

  if ( tisendsline(t->syntax, c) )
    t->line++;
  t->caret++;

  return c;
}


static void
UNGETC(Tokeniser t, int c)
{ if ( t->caret > 0 )
  { switch(t->access)
    { case A_FILE:
      { FileObj f = t->source;

	ungetc(c, f->fd);
      }
    }

    if ( tisendsline(t->syntax, c) )
      t->line--;
    t->caret--;
  }
}


static Int
getCharacterTokeniser(Tokeniser t)
{ int c = GETC(t);

  if ( !IsEof(c) )
    answer(toInt(c));

  fail;
}


static status
characterTokeniser(Tokeniser t, Int c)
{ UNGETC(t, valInt(c));

  succeed;
}


Int
getPeekTokeniser(Tokeniser t)
{ Int chr;

  if ( (chr = getCharacterTokeniser(t)) )
    characterTokeniser(t, chr);

  answer(chr);
}

		 /*******************************
		 *	    SYMBOLS		*
		 *******************************/

status
symbolTokeniser(Tokeniser t, Name symb)	/* only need 2++ characters!? */
{ String s = &symb->data;
  int size = s->size;

  if ( size > 1 )
  { int i;

    for(i=0; i<size; i++)
    { if ( !tisalnum(t->syntax, str_fetch(s, i)) )
      { string s2;

	str_cphdr(&s2, s);
	for(i=1; i<=size; i++)
	{ s2.size = i;
	  appendHashTable(t->symbols, StringToName(&s2), ON);
	}

	succeed;
      }
    }
  }

  succeed;
}


		 /*******************************
		 *	    SYNTAX ERRORS	*
		 *******************************/

static status
syntaxErrorTokeniser(Tokeniser t, CharArray msg)
{ errorPce(t, NAME_sourceError, t->source, toInt(t->line), msg);

  succeed;
}


static Any
getReportToTokeniser(Tokeniser t)
{ if ( notNil(t->source) )
    answer(t->source);

  fail;
}


		 /*******************************
		 *	    TOKENISER		*
		 *******************************/


static status
tokenTokeniser(Tokeniser t, Any token)
{ if ( isNil(t->stack) )
    assign(t, stack, newObject(ClassChain, 0));

  return appendChain(t->stack, token);
}


static Any
getTokenTokeniser(Tokeniser t)
{ int c;
  SyntaxTable s = t->syntax;

  if ( notNil(t->stack) && !emptyChain(t->stack) )
  { Any token = getDeleteHeadChain(t->stack);

    answer(token);
  }
    
  if ( isNil(t->source) )
  { errorPce(t, NAME_notOpen);
    fail;
  }

					/* skip whitespace and comment */
  for(;;)
  { do
    { c = GETC(t);
    } while(tislayout(s, c));

    if ( tiscommentstart(s, c) )	/* 1 character comment */
    { do
      { if ( IsEof(c = GETC(t)) )
	{ send(t, NAME_syntaxError, CtoName("End of file in comment"));
	  fail;
	}
      } while( !tiscommentend(s, c) );

      continue;
    } else if ( tiscommentstart1(s, c) ) /* 2 character comment */
    { char c2 = GETC(t);

      if ( tiscommentstart2(s, c2) )
      { char c1 = GETC(t);
	char c2 = GETC(t);

	while( !tiscommentend1(s, c1) || !tiscommentend2(s, c2) )
	{ c1 = c2;
	  if ( IsEof(c2 = GETC(t)) )
	  { send(t, NAME_syntaxError, CtoName("End of file in comment"));
	    fail;
	  }
	}

	continue;
      } else
      { UNGETC(t, c2);
      }
    }

    break;
  }


  DEBUG(NAME_tokeniser, printf("Found char = %c at %d\n", c, t->caret));

  if ( IsEof(c) )
    return EndOfFile;

  if ( tisquote(s, c) )			/* strings */
  { char buf[LINESIZE];
    char *q = buf;
    char open = c;

    for(;;)
    { if ( IsEof(c = GETC(t)) )
      { send(t, NAME_syntaxError, CtoName("End of file in string"));
	fail;
      }
	
      if ( tisstringescape(s, open, c) )
      { if ( c == open )		/* escape as double "" or '' */
	{ char c2 = GETC(t);
	  
	  if ( c2 == open )
	  { *q++ = c;
	    continue;
	  } else
	  { UNGETC(t, c2);
	    *q = EOS;
	    answer(CtoString(buf));
	  }
	} else	
	{ char c2;

	  if ( IsEof(c2 = GETC(t)) )
	  { send(t, NAME_syntaxError, CtoName("End of file in string"));
	    fail;
	  }
	  if ( c2 != open )
	    *q++ = c;
	  *q++ = c2;
	  continue;
	}
      }

      if ( c == open )
      { *q = EOS;
        answer(CtoString(buf));
      } else
	*q++ = c;
    }
  } else if ( tisdigit(s, c) )		/* int, real */
  { char buf[LINESIZE];
    char *q = buf;
    int is_int = TRUE;

    do
    { *q++ = c;

      c = GETC(t);
    } while ( tisdigit(s, c) );
    
    if ( c == '.' )
    { char c2 = GETC(t);

      if ( tisdigit(s, c2) )
      { *q++ = c;
        c = c2;
        is_int = FALSE;
	do
	{ *q++ = c;
	  c = GETC(t);
	} while( tisdigit(s, c) );
      } else
      { UNGETC(t, c2);
	goto num_out;
      }
    }

    if ( c == 'e' || c == 'E' )
    { char c2 = GETC(t);

      if ( tisdigit(s, c2) )
      { *q++ = c;
        c = c2;
        is_int = FALSE;
	do
	{ *q++ = c;
	  c = GETC(t);
	} while( tisdigit(s, c) );
      } else
      { UNGETC(t, c2);
	goto num_out;
      }
    }
  num_out:
    UNGETC(t, c);

    *q = EOS;
    if ( is_int )
    { char *e;
      long f = strtol(buf, &e, 10);
      if ( e != q )
      { DEBUG(NAME_tokeniser, 
	      printf("Num = '%s' (%ld), e = %d, q = %d\n",
		     buf, f, e-buf, q-buf));
	send(t, NAME_syntaxError, CtoName("Illegal number"));
	fail;
      }
      answer(toInt(f));
    } else
    { char *e;
      double f = StrTod(buf, &e);
      if ( e != q )
      { DEBUG(NAME_tokeniser,
	      printf("Num = '%s' (%f), e = %d, q = %d\n",
		     buf, f, e-buf, q-buf));
	send(t, NAME_syntaxError, CtoName("Illegal number"));
	fail;
      }
      answer(CtoReal(f));
    }
  } else if ( tisalnum(s, c) )		/* atom */
  { char buf[LINESIZE];
    char *q = buf;

    do
    { *q++ = c;
      c = GETC(t);
    } while ( tisalnum(s, c) );
    *q = EOS;
    UNGETC(t, c);

    return CtoName(buf);
  } else				/* singleton */
  { char buf[LINESIZE];
    char *s = buf;
    Name symb, symbol;

    *s++ = c;
    *s = EOS;
    symb = CtoName(buf);

    if ( isNil(t->symbols) || !getMemberHashTable(t->symbols, symb) )
      answer(symb);
    
    do
    { symbol = symb;
      *s++ = c;
      *s   = EOS;
      c = GETC(t);
      if ( !tischtype(t->syntax, c, PU) )
	break;
      symb = CtoName(buf);
    } while( getMemberHashTable(t->symbols, symb) );

    UNGETC(t, c);
    answer(symbol);
  }
}
	  

status
makeClassTokeniser(Class class)
{ sourceClass(class, makeClassTokeniser, __FILE__, "$Revision$");

  localClass(class, NAME_syntax, NAME_syntax, "syntax_table", NAME_both,
	     "Syntax used");
  localClass(class, NAME_source, NAME_input, "file|char_array|text_buffer*",
	     NAME_get, "Input source");
  localClass(class, NAME_stack, NAME_readAhead, "chain*", NAME_get,
	     "Stack of pushed-back tokens");
  localClass(class, NAME_symbols, NAME_syntax, "hash_table", NAME_get,
	     "Table with punctuation-character symbols");
  localClass(class, NAME_access, NAME_input, "alien:int", NAME_none,
	     "Internal access context");
  localClass(class, NAME_line, NAME_report, "alien:int", NAME_none,
	     "Current line number");
  localClass(class, NAME_caret, NAME_input, "alien:int", NAME_none,
	     "Point for random_access devices");

  termClass(class, "tokeniser", 2, NAME_source, NAME_syntax);
  setCloneFunctionClass(class, cloneTokeniser);
  cloneStyleVariableClass(class, NAME_syntax,  NAME_reference);
  cloneStyleVariableClass(class, NAME_symbols, NAME_reference);
  cloneStyleVariableClass(class, NAME_source,  NAME_reference);
  cloneStyleVariableClass(class, NAME_stack,   NAME_nil);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "syntax=[syntax_table]",
	     "Create from syntax",
	     initialiseTokeniser);
  sendMethod(class, NAME_close, NAME_input, 0,
	     "Close source",
	     closeTokeniser);
  sendMethod(class, NAME_symbol, NAME_syntax, 1, "symbol=name",
	     "Declare name to be a symbol",
	     symbolTokeniser);
  sendMethod(class, NAME_token, NAME_readAhead, 1, "token=any",
	     "Push back a token",
	     tokenTokeniser);
  sendMethod(class, NAME_syntaxError, NAME_report, 1, "message=char_array",
	     "Generate syntax-error warning",
	     syntaxErrorTokeniser);
  sendMethod(class, NAME_character, NAME_readAhead, 1, "char",
	     "Unget (push back) character",
	     characterTokeniser);

  getMethod(class, NAME_token, NAME_parse, "token=any", 0,
	    "Read next token",
	    getTokenTokeniser);
  getMethod(class, NAME_character, NAME_parse, "char", 0,
	    "Read next character",
	    getCharacterTokeniser);
  getMethod(class, NAME_peek, NAME_parse, "char", 0,
	    "Peek at next character",
	    getPeekTokeniser);
  getMethod(class, NAME_reportTo, NAME_report, "any", 0,
	    "Report errors to the <-source",
	    getReportToTokeniser);
  getMethod(class, NAME_open, NAME_input, "tokeniser", 1,
	    "file|char_array|text_buffer*",
	    "Open input for tokenising",
	    getOpenTokeniser);
  getMethod(class, NAME_caret, NAME_report, "int", 0,
	    "Current character-index",
	    getCaretTokeniser);
  getMethod(class, NAME_line, NAME_report, "int", 0,
	    "Current line-number (1-based)",
	    getLineTokeniser);

  EndOfFile = globalObject(NAME_endOfFile, ClassConstant,
			   NAME_endOfFile,
			   CtoString("End-of-file marker"),
			   0);

  succeed;
}
