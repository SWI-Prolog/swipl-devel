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

NewClass(tokeniser)
  SyntaxTable	syntax;			/* syntax declarations */
  Any		source;			/* input device */
  Int		line;			/* current line-no */
  Name		status;			/* {open,closed} */
  Name		kind;			/* {sequential,random_access} */
  Chain		stack;			/* push-back */
  HashTable	symbols;		/* (Partial) symbols */
  int		caret;			/* current location (random_access) */
End;

static status	closeTokeniser P((Tokeniser));
static status	sourceTokeniser(Tokeniser t, Any source);

#define IsEof(c)	((c) == (char)EOF)

/* Problems:

	* Text access
	* UNGETC has to do two chars!
*/

static status
initialiseTokeniser(Tokeniser t, Any source, SyntaxTable syntax)
{ assign(t, syntax,  syntax);
  assign(t, stack,   newObject(ClassChain, 0));
  assign(t, symbols, newObject(ClassHashTable, 0));

  return sourceTokeniser(t, source);
}


		 /*******************************
		 *	 HANLING <-SOURCE	*
		 *******************************/

static status
sourceTokeniser(Tokeniser t, Any source)
{ if ( t->source != source )
  { closeTokeniser(t);
    assign(t, source, source);
    assign(t, status, NAME_closed);
    assign(t, line, ONE);
    t->caret = 0;
    if ( instanceOfObject(source, ClassFile) )
      assign(t, kind, NAME_sequential);
    else
      assign(t, kind, NAME_randomAccess);
  }

  succeed;
}


static status
openTokeniser(Tokeniser t)
{ if ( t->status != NAME_open )
  { TRY(send(t->source, NAME_open, NAME_read, 0));
    assign(t, status, NAME_open);
  }

  succeed;
}


static status
closeTokeniser(Tokeniser t)
{ if ( t->status != NAME_closed )
  { if ( notNil(t->source) )
      TRY(send(t->source, NAME_close, 0));
    assign(t, status, NAME_closed);
  }

  succeed;
}


static char
GETC(Tokeniser t)
{ FileObj f = t->source;
  char c;

  c = getc(f->fd);
  if ( tisendsline(t->syntax, c) )
    assign(t, line, inc(t->line));
  t->caret++;

  return c;
}


static void
UNGETC(Tokeniser t, char c)
{ FileObj f = t->source;

  ungetc(c, f->fd);
  if ( tisendsline(t->syntax, c) )
    assign(t, line, dec(t->line));
  t->caret--;
}


static Int
getCharacterTokeniser(Tokeniser t)
{ char c = GETC(t);

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
{ errorPce(t, NAME_sourceError, t->source, t->line, msg);

  succeed;
}


		 /*******************************
		 *	    TOKENISER		*
		 *******************************/


static status
tokenTokeniser(Tokeniser t, Any token)
{ return appendChain(t->stack, token);
}


static Any
getTokenTokeniser(Tokeniser t)
{ char c;
  SyntaxTable s = t->syntax;

  if ( !emptyChain(t->stack) )
  { Any token = getHeadChain(t->stack);

    deleteHeadChain(t->stack);
    answer(token);
  }
    

  TRY(openTokeniser(t));

					/* skip whitespace and comment */
  for(;;)
  { do
    { c = GETC(t);
    } while(tislayout(s, c));

    if ( tiscommentstart(s, c) )	/* 1 character comment */
    { do
      { if ( IsEof(c = GETC(t)) )
	{ errorPce(t, NAME_sourceError, t->source, t->line,
		   CtoName("End of file in comment"));
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
	  { errorPce(t, NAME_sourceError, t->source, t->line,
		     CtoName("End of file in comment"));
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
  { fail;				/* TBD: special token (@eof) ? */
  }

  if ( tisquote(s, c) )			/* strings */
  { char buf[LINESIZE];
    char *q = buf;
    char open = c;

    for(;;)
    { if ( IsEof(c = GETC(t)) )
      { errorPce(t, NAME_sourceError, t->source, t->line,
		 CtoName("End of file in string"));
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
	  { errorPce(t, NAME_sourceError, t->source, t->line,
		     CtoName("End of file in string"));
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
      { printf("Num = '%s' (%ld), e = %d, q = %d\n", buf, f, e-buf, q-buf);
	errorPce(t, NAME_sourceError, t->source, t->line,
		 CtoName("Illegal number"));
	fail;
      }
      answer(toInt(f));
    } else
    { char *e;
      double f = StrTod(buf, &e);
      if ( e != q )
      { printf("Num = '%s' (%f), e = %d, q = %d\n", buf, f, e-buf, q-buf);
	errorPce(t, NAME_sourceError, t->source, t->line,
		 CtoName("Illegal number"));
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
  localClass(class, NAME_source, NAME_input, "file", NAME_get,
	     "Input source");
  localClass(class, NAME_line, NAME_report, "int", NAME_both,
	     "Current line number");
  localClass(class, NAME_status, NAME_input, "{open,closed}", NAME_get,
	     "Status of input device");
  localClass(class, NAME_kind, NAME_input,
	     "{sequential,random_access}", NAME_get,
	     "Kind of input device");
  localClass(class, NAME_stack, NAME_readAhead, "chain", NAME_get,
	     "Stack of pushed-back tokens");
  localClass(class, NAME_symbols, NAME_syntax, "hash_table", NAME_get,
	     "Table with punctuation-character symbols");
  localClass(class, NAME_caret, NAME_input, "alien:int", NAME_none,
	     "Point for random_access devices");

  termClass(class, "tokeniser", 2, NAME_source, NAME_syntax);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "input=file", "syntax=[syntax_table]",
	     "Create for source with syntax",
	     initialiseTokeniser);
  sendMethod(class, NAME_open, NAME_input, 0,
	     "Open source",
	     openTokeniser);
  sendMethod(class, NAME_close, NAME_input, 0,
	     "Close source",
	     closeTokeniser);
  sendMethod(class, NAME_source, NAME_input, 1,
	     "source=file|char_array|text_buffer",
	     "Assign specified source",
	     sourceTokeniser);
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

  succeed;
}
