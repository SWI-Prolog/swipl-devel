/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>
#include <h/unix.h>

forwards Int	getMatchingQuoteTextBuffer(TextBuffer, Int, Name);
forwards int	match(TextBuffer, int, String, int, int);
forwards int	room(TextBuffer, int, int);
forwards status capitalise_textbuffer(TextBuffer, int, int);
forwards status clear_textbuffer(TextBuffer);
forwards status downcase_textbuffer(TextBuffer, int, int);
forwards void	end_change(TextBuffer, int);
forwards Int    getSizeTextBuffer(TextBuffer);
forwards status store_textbuffer(TextBuffer, int, wchar);
forwards status transpose_textbuffer(TextBuffer, int, int, int, int);
forwards status upcase_textbuffer(TextBuffer, int, int);
forwards status save_textbuffer(TextBuffer, int, int, SourceSink);
forwards status insert_file_textbuffer(TextBuffer, int, int, SourceSink);
forwards status shift_fragments(TextBuffer, long, long);
forwards void	start_change(TextBuffer, int);
forwards status insert_textbuffer_shift(TextBuffer, int, int, String, int);

#define ALLOC (256)		/* increment allocation by this amount */
#define ROUND(n, r)		( (((n) + (r)-1) / (r)) * (r) )
#define NormaliseIndex(tb, i)	( i < 0 ? 0 : i > tb->size ? tb->size : i)
#define Swap(a, b)		{ int _tmp = (a); (a) = (b); (b) = _tmp; }
#define Before(i1, i2)		{ if ( i1 > i2 ) Swap(i1, i2); }
#define fetch(i)		fetch_textbuffer(tb, i)
#define istb8(tb)		isstr8(&(tb)->buffer)
#define Address(tb, i)		(istb8(tb) ? &(tb)->tb_buffer8[(i)] \
					   : (char8 *)&(tb)->tb_buffer16[(i)])
#define Index(tb, p) ((tb)->gap_start <= (p) ? \
		(tb)->gap_end + ((p) - (tb)->gap_start) + 1 : (p) )


static status
initialiseTextBuffer(TextBuffer tb, CharArray ca)
{ initialiseSourceSink((SourceSink)tb);

  assign(tb, first_fragment, NIL);
  assign(tb, last_fragment,  NIL);
  assign(tb, editors,	     newObject(ClassChain, 0));
  obtainClassVariablesObject(tb);	/* dubious: subclassing? */

  tb->undo_buffer = NULL;
  tb->tb_buffer8  = NULL;
  if ( notDefault(ca) )
    str_cphdr(&tb->buffer, &ca->data);
  else
    str_cphdr(&tb->buffer, str_nl(NULL)); /* ASCII */

  clear_textbuffer(tb);			/* (re)initialise buffer */

  if ( notDefault(ca) )
    insertTextBuffer(tb, ZERO, ca, ONE);
  assign(tb, modified, OFF);

  succeed;
}


static status
unlinkTextBuffer(TextBuffer tb)
{ Any editor;

  for_chain(tb->editors, editor,
	    send(ReceiverOfEditor(editor), NAME_lostTextBuffer, 0));
  clearChain(tb->editors);

  while( notNil(tb->first_fragment) )	/* destroy fragments */
    freeObject(tb->first_fragment);

  if ( tb->tb_buffer8 != NULL )		/* deallocate the buffer */
  { pceFree(tb->tb_buffer8);
    tb->tb_buffer8 = NULL;
  }

  if ( tb->undo_buffer != NULL )
  { destroyUndoBuffer(tb->undo_buffer);
    tb->undo_buffer = NULL;
  }

  succeed;
}


static TextBuffer
getConvertTextBuffer(Any ctx, Editor e)
{ answer(e->text_buffer);
}


static status
storeTextBuffer(TextBuffer tb, FileObj file)
{ int unitsize = (istb8(tb) ? sizeof(char8) : sizeof(char16));

  TRY(storeSlotsObject(tb, file));
  storeIntFile(file, toInt(tb->size));

  fwrite(Address(tb, 0),
	 unitsize,
	 tb->gap_start,
	 file->fd);
  fwrite(Address(tb, tb->gap_end+1),
	 unitsize,
	 tb->size - tb->gap_start,
	 file->fd);

  return checkErrorFile(file);
}


static status
loadTextBuffer(TextBuffer tb, IOSTREAM *fd, ClassDef def)
{ TRY( loadSlotsObject(tb, fd, def) );

  if ( isNil(tb->syntax) )
    assign(tb, syntax, getClassVariableValueObject(tb, NAME_syntax));

  assign(tb, editors, newObject(ClassChain, 0));
  tb->size = loadWord(fd);
  tb->allocated = ROUND(tb->size, ALLOC);
  str_cphdr(&tb->buffer, str_nl(NULL));	/* ASCII */
  tb->tb_buffer8 = pceMalloc(tb->allocated);
  Sfread(Address(tb, 0), sizeof(char), tb->size, fd); /* TBD */
  tb->gap_start = tb->size;
  tb->gap_end = tb->allocated - 1;

  if ( tb->lines == 0 )
  { tb->lines = -1;			/* indicate invalid */
    tb->lines = count_lines_textbuffer(tb, 0, tb->size);
  }

  tb->changed_start = tb->size;
  tb->changed_end = 0;  
  CmodifiedTextBuffer(tb, OFF);

  succeed;
}


static status
cloneTextBuffer(TextBuffer tb, TextBuffer clone)
{ clonePceSlots(tb, clone);

  clone->undo_buffer = NULL;
  clone->tb_buffer8 = pceMalloc(clone->allocated);
  memcpy(clone->tb_buffer8, tb->tb_buffer8, clone->allocated);
  clone->changed_start = clone->size;
  clone->changed_end = 0;

  succeed;
}


		 /*******************************
		 *	      EDITOR		*
		 *******************************/

static status
attachTextBuffer(TextBuffer tb, Editor e)
{ return appendChain(tb->editors, e);
}


static status
detachTextBuffer(TextBuffer tb, Editor e)
{ deleteChain(tb->editors, e);

  succeed;
}


		/********************************
		*         REPORT ERRORS		*
		********************************/

static status
reportTextBuffer(TextBuffer tb, Name kind, CharArray fmt, int argc, Any *argv)
{ ArgVector(av, argc + 2);
  Any editor;

  av[0] = kind;
  av[1] = fmt;
  copyArgs(argc, argv, &av[2]);

  for_chain(tb->editors, editor, sendv(editor, NAME_report, argc+2, av));
  succeed;
}


		/********************************
		*       FORWARDING CHANGES      *
		*********************************/

status
changedTextBuffer(TextBuffer tb)
{ Cell cell;

  if ( tb->changed_start <= tb->changed_end )
  { Any av[2];

    av[0] = toInt(tb->changed_start);
    av[1] = toInt(tb->changed_end);

    for_cell(cell, tb->editors)
      qadSendv(cell->value, NAME_ChangedRegion, 2, av);
  }

  tb->changed_start = tb->size;
  tb->changed_end = 0;

  succeed;
}


status
ChangedRegionTextBuffer(TextBuffer tb, Int start, Int end)
{ int s = valInt(start);
  int e = valInt(end);

  Before(s, e);
  start_change(tb, s);
  end_change(tb, e);

  return changedTextBuffer(tb);
}


status
ChangedFragmentListTextBuffer(TextBuffer tb)
{ Cell cell;

  for_cell(cell, tb->editors)
    qadSendv(cell->value, NAME_ChangedFragmentList, 0, NULL);

  succeed;
}


		/********************************
		*    OBJECT LEVEL OPERATIONS    *
		*********************************/

status
clearTextBuffer(TextBuffer tb)
{ clear_textbuffer(tb);

  return changedTextBuffer(tb);
}


status
insertFileTextBuffer(TextBuffer tb, Int where, SourceSink file, Int times)
{ if ( isDefault(times) )
    times = ONE;

  if ( valInt(times) <= 0 )
    succeed;

  if ( insert_file_textbuffer(tb, valInt(where), valInt(times), file) )
    return changedTextBuffer(tb);

  fail;
}


status
insertTextBuffer(TextBuffer tb, Int where, CharArray ca, Int times)
{ if ( isDefault(times) )
    times = ONE;

  insert_textbuffer(tb,
		    valInt(where),
		    valInt(times),
		    &ca->data);

  return changedTextBuffer(tb);
}


status
appendTextBuffer(TextBuffer tb, CharArray ca, Int times)
{ if ( isDefault(times) )
    times = ONE;

  insert_textbuffer_shift(tb,
			  tb->size,
			  valInt(times),
			  &ca->data,
			  FALSE);	/* don't shift fragments! */

  return changedTextBuffer(tb);
}


static status
formatTextBuffer(TextBuffer tb, CharArray fmt, int argc, Any *argv)
{ string s;

  TRY(str_writefv(&s, fmt, argc, argv));
  insert_textbuffer_shift(tb, tb->size, 1, &s, FALSE);
					/* don't shift fragments! */
  str_unalloc(&s);

  return changedTextBuffer(tb);
}


status
deleteTextBuffer(TextBuffer tb, Int where, Int times)
{ if ( isDefault(times) )
    times = ONE;

  delete_textbuffer(tb, valInt(where), valInt(times));

  return changedTextBuffer(tb);
}


status
saveTextBuffer(TextBuffer tb, SourceSink file, Int from, Int len)
{ int clear_modified = (isDefault(from) && isDefault(len));

  if ( isDefault(from) )
    from = ZERO;
  if ( isDefault(len) )
    len = toInt(tb->size);

  if ( save_textbuffer(tb, valInt(from), valInt(len), file) )
  { if ( clear_modified )
      CmodifiedTextBuffer(tb, OFF);
    succeed;
  }

  fail;
}


static Int
getSizeTextBuffer(TextBuffer tb)
{ return toInt(tb->size);
}


status
CmodifiedTextBuffer(TextBuffer tb, Bool val)
{ if ( tb->modified != val )
    return sendv(tb, NAME_modified, 1, (Any *) &val);

  succeed;
}


static status
modifiedTextBuffer(TextBuffer tb, Bool val)
{ if ( tb->modified != val )
  { Cell cell;
    assign(tb, modified, val);

    if ( val == OFF )
      checkpointUndoTextBuffer(tb);

    for_cell(cell, tb->editors)
      forwardModifiedEditor(cell->value, val);
  }

  succeed;
}


static Int
getCharacterTextBuffer(TextBuffer tb, Int where)
{ int c = fetch_textbuffer(tb, valInt(where));

  if ( c >= 0 )
    answer(toInt(c));

  fail;
}


status
characterTextBuffer(TextBuffer tb, Int where, Int c)
{ TRY(store_textbuffer(tb, valInt(where), valInt(c)));

  return changedTextBuffer(tb);
}


status
transposeTextBuffer(TextBuffer tb, Int f1, Int t1, Int f2, Int t2)
{ transpose_textbuffer(tb, valInt(f1), valInt(t1), valInt(f2), valInt(t2));

  return changedTextBuffer(tb);
}


status
downcaseTextBuffer(TextBuffer tb, Int from, Int len)
{ downcase_textbuffer(tb, valInt(from), valInt(len));
  
  return changedTextBuffer(tb);
}


status
upcaseTextBuffer(TextBuffer tb, Int from, Int len)
{ upcase_textbuffer(tb, valInt(from), valInt(len));
  
  return changedTextBuffer(tb);
}


status
capitaliseTextBuffer(TextBuffer tb, Int from, Int len)
{ capitalise_textbuffer(tb, valInt(from), valInt(len));
  
  return changedTextBuffer(tb);
}


Int
getScanTextBuffer(TextBuffer tb, Int from, Name unit, Int amount, Name az)
{ if ( isDefault(amount) )
    amount = ONE;
  if ( isDefault(az) )
    az = (valInt(amount) >= 0 ? NAME_end : NAME_start);

  return toInt(scan_textbuffer(tb,
  			       valInt(from), 
  			       unit,
  			       valInt(amount),
  			       az == NAME_start ? 'a' : 'z'));
}


static status
contentsTextBuffer(TextBuffer tb, CharArray ca)
{ clearTextBuffer(tb);

  return appendTextBuffer(tb, ca, ONE);
}


static StringObj
getSubTextBuffer(TextBuffer tb, Int from, Int to)
{ string s;
  int f = (isDefault(from) ? 0 : valInt(from));
  int t = (isDefault(to) ? tb->size : valInt(to));

  str_sub_text_buffer(tb, &s, f, t-f);
  answer(StringToString(&s));
}


StringObj
getContentsTextBuffer(TextBuffer tb, Int from, Int length)
{ if ( isDefault(from) )
    from = ZERO;

  return getSubTextBuffer(tb, from,
			  isDefault(length) ? DEFAULT : add(from, length));
}


static status
forAllFragmentsTextBuffer(TextBuffer tb, Code msg)
{ int size = 0;
  Fragment f;

  for(f = tb->first_fragment; notNil(f); f = f->next)
    size++;

  { ArgVector(argv, size);
    int i = 0;

    for(f = tb->first_fragment; notNil(f); f = f->next)
      argv[i++] = f;
    for(i=0; i<size; i++)
    { if ( !isFreedObj(argv[i]) )
	TRY(forwardCodev(msg, 1, &argv[i]));
    }
  }

  succeed;
}


static Chain
getFindAllFragmentsTextBuffer(TextBuffer tb, Code msg)
{ Chain result = answerObject(ClassChain, 0);
  Fragment f;

  for(f = tb->first_fragment; notNil(f); f = f->next)
  { if ( isDefault(msg) || forwardCodev(msg, 1, (Any *)&f) )
      appendChain(result, f);
  }

  answer(result);
}


static Fragment
getFindFragmentTextBuffer(TextBuffer tb, Code msg)
{ Fragment f;

  for(f = tb->first_fragment; notNil(f); f = f->next)
  { if ( forwardCodev(msg, 1, (Any *)&f) )
      answer(f);
  }

  fail;
}


static Int
getFindTextBuffer(TextBuffer tb, Int from, StringObj str,
		  Int times, Name start, Bool exactcase, Bool wordmode)
{ char az;
  int result;
  int ec, wm;

  if ( isDefault(times) ) 
    times = ONE;
  az = (isDefault(start) ? (valInt(times) >= 0 ? 'z' : 'a')
			 : start == NAME_start ? 'a' : 'z');
  ec = (exactcase == ON || isDefault(exactcase) ? TRUE : FALSE);
  wm = (wordmode == OFF || isDefault(wordmode)  ? FALSE : TRUE);

  result = find_textbuffer(tb,
			   valInt(from),
			   &str->data,
			   valInt(times),
			   az, ec, wm);
  if ( result < 0 )
    fail;

  answer(toInt(result));
}

		/********************************
		*           SCANNING            *
		*********************************/

/*  Find a position by scanning the text. The scan starts at `from' and
    returns the index resulting from skipping `amount' `units'.  A unit
    consists of the unit itself and a separator (with the exception of
    characters that have a zero length separator between them). One can
    scan for the start of a unit or for the end of the unit. The start
    of the unit returns the first index of the unit. The end returns the
    first index of the separator between this unit and the next. Below
    are the definitions of the units recognised currently.

    NAME_character
    	A character unit has no start, nor an end.
    NAME_word
    	A word consists of a contiguous string of `alNum' characters
    	(digits + letters + _). A separator is a contiguous string of
    	'non-alNum' characters.
    NAME_line
        A line consists of a (possible empty) string of non-'\n'
        characters. A separator is exactly one '\n'.
    NAME_sentence
        A sentence separator is a seqence [.!?], followed by a non-empty
        sequence of blank characters.  The sentence separator is a sequence
        of any character except for the sentence separator sequence.
    NAME_paragraph
	A paragraph separator is sequence of blank lines.
    NAME_term
        A term if either:
	    a) a series of alnum characters
	    b) a quoted string
	    c) a string with matching brackets at its start end end
	    d) case a), immediately followed by case c).	       

    Negative amount scans backwards.  The returned index is in the range
    [0, size).
*/


static status
ends_sentence(TextBuffer tb, int here)
{ return matchRegex(tb->syntax->sentence_end, tb, toInt(here), DEFAULT);
}


status
parsep_line_textbuffer(TextBuffer tb, int here)
{ int rval = matchRegex(tb->syntax->paragraph_end, tb, toInt(here), DEFAULT);

  DEBUG(NAME_paragraph,
	Cprintf("parsep_line_textbuffer(%s, %d) --> %s\n",
		pp(tb), here, rval ? "yes" : "no"));

  return rval;
}


int
scan_textbuffer(TextBuffer tb, int from, Name unit, int amount, int az)
{ int here;
  int size = tb->size;
  SyntaxTable syntax = tb->syntax;
  
  here = from;
  
  if ( equalName(unit, NAME_character) )
  { here = from + amount;	/* 'az' does not count (no separator) */
    return NormaliseIndex(tb, here);
  } else if ( equalName(unit, NAME_word) )
  { if ( az == 'a' )
    { if ( amount <= 0 )
      { for( ; here > 0 && amount <= 0; amount++ )
	{ while( !tisalnum(syntax, fetch(here)) && here > 0 ) here--;
	  while( tisalnum(syntax, fetch(here)) && here > 0 ) here--;
	}
	return (here == 0 ? here : here+1);
      } else
      { for( ; here < size && amount > 0; amount-- )
	{ while( tisalnum(syntax, fetch(here)) && here < size ) here++;
	  while( !tisalnum(syntax, fetch(here)) && here < size ) here++;
	}
	return here;
      }
    } else	/* 'z' mode */
    { if ( amount >= 0 )
      { for( ; here < size && amount >= 0; amount-- )
	{ while( !tisalnum(syntax, fetch(here)) && here < size ) here++;
	  while( tisalnum(syntax, fetch(here)) && here < size ) here++;
	}
	return here;
      } else
      { for( ; here > 0 && amount < 0; amount-- )
	{ while( tisalnum(syntax, fetch(here)) && here > 0 ) here--;
	  while( !tisalnum(syntax, fetch(here)) && here > 0 ) here--;
	}
	return here == 0 ? here : here+1;
      }
    } 	   
  } else if ( equalName(unit, NAME_line) )
  { if ( az == 'a' )		/* return first char of line */
    { if ( amount <= 0 )
      { for( ; here >= 0 && amount <= 0; amount++ )
	{ if ( tisendsline(syntax, fetch(here)) )
	    here--;
	  while( here >= 0 && !tisendsline(syntax, fetch(here)) )
	    here--;
	}
	return (here < 0 ? 0 : here + 1);
      } else
      { for( ; here <= size && amount > 0; amount--)
	{ while( here <= size && !tisendsline(syntax, fetch(here)) )
	    here++;
	  here++;
	}
	return (here >= size ? size : here);
      }
    } else /* 'z' case */
    { if ( amount >= 0 )
      { for( ; ; amount-- )
	{ while( here <= size && !tisendsline(syntax, fetch(here)) )
	    here++;
	  if ( here >= size || amount == 0 )
	    return here > size ? size : here;
	  here++;
	} 
      } else
      { for( ; ; amount++ )
	{ while( here > 0 && !tisendsline(syntax, fetch(here)) )
	    here--;
	  if ( here <= 0 || amount == 0 )
	    return here < 0 ? 0 : here;
	  here--;
	}
      }             
    }
  } else if ( equalName(unit, NAME_sentence) )
  { if ( az == 'z' )
    { if ( amount >= 0 )
      { for( ; here < size && amount >= 0; amount-- )
	{ here++;
	  while( here < size && !ends_sentence(tb, here) ) here++;
	}
	return here;
      } else
      { while(here > 0 && !ends_sentence(tb, here) ) here--;
	for( ; here > 0 && amount < 0; amount++ )
	{ here--;
	  while(here > 0 && !ends_sentence(tb, here) ) here--;
	}
	return here;
      }
    } else		/* 'a' case */
    { if ( amount <= 0 )
      { for( ; here > 0 && amount <= 0; amount++ )
	{ here--;
	  while(here > 0 && tislayout(syntax, fetch(here)) ) here--;
	  while(here > 0 && !ends_sentence(tb, here) ) here--;
	}
	while( here < size && tislayout(syntax, fetch(here)) ) here++;
	return here;
      } else
      { for( ; here < size && amount > 0; amount-- )
	{ here++;
	  while( here < size && !ends_sentence(tb, here) ) here++;
	}
	while( here < size && tislayout(syntax, fetch(here)) ) here++;
	return here;
      }
    }
  } else if ( equalName(unit, NAME_paragraph) )
  { if ( az == 'z' )
    { if ( amount >= 0 )
      { here = scan_textbuffer(tb, here, NAME_line, 0, 'a');
	for( ; here < size && amount >= 0; amount-- )
	{ while( here < size && parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, 1, 'a');
	  while( here < size && !parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, 1, 'a');
	}
	return here;
      } else
      { for( ; here > 0 && amount < 0; amount++ )
	{ here--;
	  while( here > 0 && !parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, -1, 'a');
	  while( here > 0 && parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, -1, 'a');
	}
	return here;
      }
    } else		/* the 'a' case */
    { if ( amount > 0 )
      { for( ; here < size && amount > 0; amount-- )
	{ while( here < size && !parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, 1, 'a');
	  while( here < size && parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, 1, 'a');
	}
	return here;
      } else
      { for( ; here > 0 && amount <= 0; amount++ )
	{ here--;
	  while( here > 0 && parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, -1, 'a');
	  while( here > 0 && !parsep_line_textbuffer(tb, here) )
	    here = scan_textbuffer(tb, here, NAME_line, -1, 'a');
	}
	if ( parsep_line_textbuffer(tb, here) )
	  here = scan_textbuffer(tb, here, NAME_line, 1, 'a');
	return here;
      }
    }
  } else if ( equalName(unit, NAME_term) )
  { if ( amount > 0 )	/* forwards */
    { for( ; here < size && amount > 0; amount-- )
      { while( here < size && !tischtype(syntax, fetch(here), AN|OB|QT) )
	  here++;
	if ( amount == 1 && az == 'a' )
	  return here;
	while( here < size && tisalnum(syntax, fetch(here)) )
	  here++;
	if ( tisquote(syntax, fetch(here)) )
	{ Int h = getMatchingQuoteTextBuffer(tb, toInt(here), NAME_forward);
	  if ( h == FAIL ) return here;
	  here = valInt(h) + 1;
	} else if ( tisopenbrace(syntax, fetch(here)) )
	{ Int h = getMatchingBracketTextBuffer(tb, toInt(here), DEFAULT);
	  if ( h == FAIL ) return here;
	  here = valInt(h) + 1;
	}
      }
      return here;
    } else if ( amount < 0 )	/* backwards */
    { for( ; here > 0 && amount < 0; amount++ )
      { here--;
	while( here > 0 && !tischtype(syntax, fetch(here), AN|CB|QT) )
	  here--;
	if ( amount == -1 && az == 'z' )
	  return here+1;
	if ( tisquote(syntax, fetch(here)) )
	{ Int h = getMatchingQuoteTextBuffer(tb, toInt(here), NAME_backward);
	  if ( h == FAIL ) return here;
	  here = valInt(h);
	} else if ( tisclosebrace(syntax, fetch(here)) )
	{ Int h = getMatchingBracketTextBuffer(tb, toInt(here), DEFAULT);
	  if ( h == FAIL ) return here;
	  here = valInt(h);
	}
	if ( tisalnum(syntax, fetch(here-1)) )
	  while( here > 0 && tisalnum(syntax, fetch(here-1)) )
	    here--;
      }
      return here;
    }
    return here;
  } else
    fail;
}


static Int
getMatchingQuoteTextBuffer(TextBuffer tb, Int idx, Name direction)
{ long i = valInt(idx);
  int c = fetch(i);
  SyntaxTable syntax = tb->syntax;

  if ( !tisquote(tb->syntax, c) )	/* must start on quote */
    fail;

  if ( direction == NAME_forward )
  { long i0 = i;
    int quoteisescape = tisstringescape(syntax, c, c);

    for(i++; i<tb->size; i++)
    { int c2 = fetch(i);

      if ( c2 == c )
      { if ( quoteisescape && i+1 < tb->size && fetch(i+1) == c )
	{ i++;				/* Prolog 'Can''t' syntax! */
	  continue;
	}
	if ( i-1 > i0 &&
	     (c2=fetch(i-1)) != c &&
	     tisstringescape(syntax, c, c2) )
	  continue;
	  
	answer(toInt(i));
      }
    }
  } else /* if ( direction == NAME_backward ) */
  { for(i--; i>=0; i--)
    { int c2 = fetch(i);
      
      if ( c2 == c )
      { if ( i == 0 )
	  answer(toInt(i));
	if ( tisstringescape(syntax, c, fetch(i-1)) )
	{ if ( tisstringescape(syntax, c, c) )
	    i--;			/* Prolog 'Can''t' syntax! */
	} else
	  answer(toInt(i));
      }
    }
  }

  fail;
}


		 /*******************************
		 *        SYNTAX HANDLING	*
		 *******************************/

#define SST_PLAIN	0x100		/* syntax-state */
#define SST_COMMENT1	0x200		/* 1-character comment-string */
#define SST_COMMENT2	0x400		/* 2-character comment-string */
#define SST_STRING	0x800		/* string (low-order is start) */

typedef int (*scan_callback_t)(TextBuffer, long, long, int);

static int
scan_syntax_textbuffer(TextBuffer tb,
		       long from, long to,
		       scan_callback_t *callback,
		       int flags)
{ long here = from;			/* current position */
  SyntaxTable syntax = tb->syntax;	/* syntax-table */
  int state = SST_PLAIN;		/* initial/current state */
  int tokenstart;
  
  for(; here < to; here++)
  { int c = fetch(here);

					/* strings */
    if ( tisquote(syntax, c) )
    { int quoteisescape = tisstringescape(syntax, c, c);

					/* Prolog 0char syntax */
      if ( c == '\'' && syntax->name == NAME_prolog && here > 0 )
      { int c0 = fetch(here-1);

	if ( isdigit(c0) )		/* or <digit><number> */
	  continue;
      }

      state = SST_STRING|c;
      tokenstart = here;

      for(here++; here<to; here++)
      { int c2 = fetch(here);

	if ( c2 == c )
	{ if ( quoteisescape && here+1 < tb->size && fetch(here+1) == c )
	  { here++;
	    continue;
	  }
	  if ( here-1 > tokenstart &&
	       (c2=fetch(here-1)) != c &&
	       tisstringescape(syntax, c, c2) )
	    continue;

	  state = SST_PLAIN;
	  goto cont;
	}
      }
    } else if ( tiscommentstart(syntax, c) ) /* COMMENT (1) */
    { tokenstart = here;

      for(here++ ; here < to; here++ )
      { int c = fetch(here);

	if ( tiscommentend(syntax, c) )
	  goto cont;
      }

      state = SST_COMMENT1;
    } else if ( tiscommentstart1(syntax, c) &&
		tiscommentstart2(syntax, fetch(here+1)) )
    { for( here += 4; here < to; here++ )
      { int c = fetch(here - 1);
	  
	if ( tiscommentend2(syntax, c) )
	{ c = fetch(here - 2);
	  if ( tiscommentend1(syntax, c) )
	    goto cont;
	}
      }
      state = SST_COMMENT2;
    }

  cont:
    ;
  }

  return state;
}


static Name
getScanSyntaxTextBuffer(TextBuffer tb, Int f, Int t)
{ int from = NormaliseIndex(tb, valInt(f));
  int to   = NormaliseIndex(tb, valInt(t));
  int s;

  if ( to == tb->size )
    to--;

  s = scan_syntax_textbuffer(tb, from, to, NULL, 0);
  switch(s&0xff00)
  { case SST_PLAIN:
      answer(NAME_code);
    case SST_COMMENT1:
    case SST_COMMENT2:
      answer(NAME_comment);
    case SST_STRING:
      answer(NAME_string);
    default:
      assert(0);
      fail;
  }
} 


static status
inStringTextBuffer(TextBuffer tb, Int pos, Int from)
{ long idx = valInt(pos);
  long here = (isDefault(from) ? 0L : valInt(from));
  SyntaxTable syntax = tb->syntax;

  for( ; here <= idx; here++)
  { int c = fetch(here);

    if ( tisquote(syntax, c) )
    { Int match;

      DEBUG(NAME_inString, Cprintf("here = %ld (idx = %ld)\n", here, idx));

					/* Prolog 0'char syntax */
      if ( c == '\'' && syntax->name == NAME_prolog && here > 0 )
      { int c0 = fetch(here-1);

	if ( isdigit(c0) )
	{ if ( c0 == '0' && idx == here+1 )
	    succeed;
	  continue;
	}
      }

      if ( (match = getMatchingQuoteTextBuffer(tb, toInt(here), NAME_forward)))
      { DEBUG(NAME_inString, Cprintf("Matching: %ld\n", valInt(match)));

	if ( (here = valInt(match)) >= idx )
	  succeed;
      } else
	succeed;
    }
  }

  fail;
}

#define MAXBRACKETS 1000		/* ??? */

Int
getMatchingBracketTextBuffer(TextBuffer tb, Int idx, Int bracket)
{ int i = valInt(idx);
  wchar stack[MAXBRACKETS];
  int depth = 1;
  int ic;
  SyntaxTable syntax = tb->syntax;
  int c;

  c = (notDefault(bracket) ? valInt(bracket) : fetch(i));
  stack[0] = c;

  if ( tisopenbrace(syntax, c) )
    ic = 1; 
  else if ( tisclosebrace(syntax, c) )
    ic = -1;
  else
    fail;

  for( i += ic; i >= 0 && i < tb->size; i += ic )
  { c = fetch(i);

    if ( tisopenbrace(syntax, c) )
    { if ( ic > 0 )
	stack[depth] = c;
      depth += ic;
      if ( ic < 0 && !tismatching(syntax, c, stack[depth]) )
      { errorPce(tb, NAME_mismatchedBracket);
	fail;
      }
    } else if ( tisclosebrace(syntax, c) )
    { if ( ic < 0 )
	stack[depth] = c;
      depth -= ic;
      if ( ic > 0 && !tismatching(syntax, c, stack[depth]) )
      { errorPce(tb, NAME_mismatchedBracket);
	fail;
      }
    } else if ( tisquote(syntax, c) )
    { Int mb = getMatchingQuoteTextBuffer(tb, toInt(i),
					  ic > 0 ? NAME_forward
					         : NAME_backward);
      if ( mb )
	i = valInt(mb);
      else
	fail;
    }

    if ( depth == 0 )
      answer(toInt(i));
  }
  
  fail;
}


Int
getSkipBlanksTextBuffer(TextBuffer tb, Int where, Name direction, Bool skipnl)
{ long pos = valInt(where);
  long size = tb->size;

  pos = NormaliseIndex(tb, pos);

  if ( isDefault(skipnl) )
    skipnl = ON;
  if ( isDefault(direction) )
    direction = NAME_forward;

  if ( direction == NAME_forward )
  { if ( skipnl == OFF )
    { while( pos < size && tisblank(tb->syntax, fetch(pos)) )
        pos++;
    } else
    { while( pos < size && tislayout(tb->syntax, fetch(pos)) )
	pos++;
    }
  } else
  { if ( skipnl == OFF )
    { while( pos > 0 && tisblank(tb->syntax, fetch(pos-1)) )
        pos--;
    } else
    { while( pos > 0 && tislayout(tb->syntax, fetch(pos-1)) )
        pos--;
    }
  }

  return toInt(pos);
}


static Int
getSkipCommentTextBuffer(TextBuffer tb, Int where, Int to, Bool layouttoo)
{ long pos = valInt(where);
  long end = (isDefault(to) ? tb->size : valInt(to));
  int fwd = (end >= pos);
    
  pos = NormaliseIndex(tb, pos);
  end = NormaliseIndex(tb, end);

  if ( fwd )				/* forward */
  { for(;;)
    { if ( pos < 0 )
	answer(toInt(tb->size));

      if ( layouttoo != OFF )
      { for( ; pos < end && tislayout(tb->syntax, fetch(pos)); pos++ )
	  ;
      }

      if ( tiscommentstart(tb->syntax, fetch(pos)) )
      { for( ; pos < end; pos++ )
	{ int c = fetch(pos);

	  if ( tiscommentend(tb->syntax, c) )
	    break;
	}
	continue;
      }

      if ( tiscommentstart1(tb->syntax, fetch(pos)) &&
	   tiscommentstart2(tb->syntax, fetch(pos+1)) )
      { for( pos += 4; pos < end; pos++ )
	{ int c = fetch(pos - 1);
	  
	  if ( tiscommentend2(tb->syntax, c) )
	  { c = fetch(pos - 2);
	    if ( tiscommentend1(tb->syntax, c) )
	      break;
	  }
	}

	continue;
      }
      break;
    }
  } else				/* backwards */
  { for(;;)
    { wchar c;

    again:

      if ( pos < end )
	answer(ZERO);

      if ( layouttoo != OFF )
	for(; pos >= end && tislayout(tb->syntax, c=fetch(pos))
	                 && !tiscommentend(tb->syntax, c)
	    ; pos-- )
	  ;

      if ( tiscommentend(tb->syntax, fetch(pos)) )
      { long possave = pos;

	for( pos--;
	     pos >= end && !tiscommentstart(tb->syntax, c=fetch(pos));
	     pos-- )
	  if ( tiscommentend(tb->syntax, c) )
	  { pos = possave;
	    if ( tislayout(tb->syntax, fetch(pos)) )
	    { pos--;
	      goto again;
	    } else
	      goto next;
	  }

	if ( pos > end )
	  pos--;
	continue;
      }

      next:
      if ( tiscommentend2(tb->syntax, fetch(pos)) &&
	   tiscommentend1(tb->syntax, fetch(pos-1)) )
      { for( pos -= 4;
	     pos >= end && !(tiscommentstart1(tb->syntax, fetch(pos+1)) &&
	    	             tiscommentstart2(tb->syntax, fetch(pos+2)));
	     pos-- )
	  ;
	continue;
      }
      break;
    }
  }

  if ( (fwd && pos > end) || (!fwd && pos < end) )
    pos = end;

  answer(toInt(NormaliseIndex(tb, pos)));
}


static status
inCommentTextBuffer(TextBuffer tb, Int pos, Int from)
{ int idx = valInt(pos);
  int here = (isDefault(from) ? 0 : valInt(from));
  SyntaxTable syntax = tb->syntax;

  while(here <= idx)
  { int c = fetch(here);

    if ( tisquote(syntax, c) )
    { Int h = getMatchingQuoteTextBuffer(tb, toInt(here), NAME_forward);

      if ( !h )
	succeed;
      here = valInt(h) + 1;
      continue;
    }

    if ( tiscommentstart(syntax, c) ||
	 (tiscommentstart1(syntax, c) &&
	  tiscommentstart2(syntax, fetch(here+1))) )
    { here = valInt(getSkipCommentTextBuffer(tb, toInt(here), DEFAULT, OFF));
      if ( here >= idx )
	succeed;
    }

    here++;
  }

  fail;
}


static status
forAllCommentsTextBuffer(TextBuffer tb, Code msg, Int from, Int to)
{ int here = (isDefault(from) ? 0 : valInt(from));
  int end  = (isDefault(to)   ? tb->size : valInt(to));
  SyntaxTable syntax = tb->syntax;

  if ( here < 0 )			/* normalise the indices */
    here = 0;
  if ( end > tb->size )
    end = tb->size;

  while(here < end)
  { int c = fetch(here);

    if ( tisquote(syntax, c) )
    { Int h = getMatchingQuoteTextBuffer(tb, toInt(here), NAME_forward);

      if ( !h )
	succeed;
      here = valInt(h) + 1;
      continue;
    }

    if ( tiscommentstart(syntax, c) ||
	 (tiscommentstart1(syntax, c) &&
	  tiscommentstart2(syntax, fetch(here+1))) )
    { int endc = valInt(getSkipCommentTextBuffer(tb, toInt(here),
						 DEFAULT, OFF));

      forwardReceiverCode(msg, tb, toInt(here), toInt(endc), 0);

      here = endc;
    }

    here++;
  }

  succeed;
}


Int
getLineNumberTextBuffer(TextBuffer tb, Int i)
{ int e = (isDefault(i) ? tb->size : valInt(i));

  answer(toInt(count_lines_textbuffer(tb, 0, e) + 1));
}
  

int
find_textbuffer(TextBuffer tb, int here, String str,
		int times, char az, int ec, int wm)
{ int hit = FALSE;
  int where = here;

  if ( times < 0 )
  { for( ; here >= 0 && times < 0; times++ )
    { for( ; here >= 0; here-- )
      { if ( match(tb, here, str, ec, wm) )
	{ hit = TRUE;
	  where = here;
	  break;
	}
      }
    }
  } else if ( times > 0 )
  { int size = tb->size;

    for( ; here < size && times > 0; times-- )
    { for( ; here < size; here++ )
      { if ( match(tb, here, str, ec, wm) )
        { hit = TRUE;
          where = here;
          break;
	}
      }
    }
  } else
    return here;

  return hit ? (az == 'a' ? where : where + str->size) : -1;
}


static int
match(TextBuffer tb, int here, String s, int ec, int wm)
{ int l = s->size;
  int i;

  if ( wm && (tisalnum(tb->syntax, fetch(here-1)) ||
	      tisalnum(tb->syntax, fetch(here+l))) )
    return FALSE;

  if ( ec )
  { for( i=0; i < l; i++ )
    { if ( fetch(here++) != str_fetch(s, i) )
	return FALSE;
    }
  } else
  { for( i=0; i < l; i++ )
    { wchar c1 = fetch(here++);
      wchar c2 = str_fetch(s, i);

      if ( tolower(c1) != tolower(c2) )
	return FALSE;
    }
  }

  return TRUE;
}


		/********************************
		*            FILLING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill a line.  The text starts at `from' (possibly  holding  blanks)  the
first  non-blank  character  is at column sc and rm is the right margin.
The return value  is  the  start  position  of  the  next  line  in  the
textbuffer.  Characters after to are always left untouched.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_WORDS 1000

static void
distribute_spaces(TextBuffer tb, int spaces, int nbreaks, long int *breaks)
{ int s = spaces / (nbreaks-1);
  int n, m;
  int *extra = (int *)alloca(nbreaks * sizeof(int));
  String space = str_spc(&tb->buffer);

  DEBUG(NAME_justify, Cprintf("%d spaces (each %d)\n", spaces, s));

  for(n=0; n < nbreaks-1; n++)		/* give them equal size */
    extra[n] = s;
  extra[n] = 0;				/* not in last (is the newline) */
					/* distribute from the center */
  spaces -= s*(nbreaks-1);
  for(m = nbreaks / 2, n = 0; spaces > 0; n++, spaces--)
  { int i = m + (n%2 == 0 ? n/2 : -(n/2));

    if ( i >= nbreaks-1 ) i = nbreaks - 2;
    if ( i < 0 ) i = 0;
    extra[i]++;
    DEBUG(NAME_justify, Cprintf("\tadding one at break %d\n", i));
  }

  for(n=0, m=0; n<nbreaks; n++)
  { breaks[n] += m;
    if ( extra[n] )
    { insert_textbuffer(tb, breaks[n], extra[n], space);
      m += extra[n];
    }
  }
}


long
fill_line_textbuffer(TextBuffer tb, long int here, long int to,
		     int sc, int rm, int justify)
{ int col = sc;
  long breaks[MAX_WORDS];
  int nbreaks = 0;
  int last_break_col = 0;
  long i;
  String nl = str_nl(&tb->buffer);
  String space = str_spc(&tb->buffer);

  DEBUG(NAME_fill, Cprintf("fill_line(tb, %ld, %ld, %d, %d)\n",
			   here, to, sc, rm));

					/* delete leading white space */
  for( i = here; i < to && tislayout(tb->syntax, fetch(i)); i++ )
    ;
  if ( i-here > 0 )
  { delete_textbuffer(tb, here, i-here);
    to -= i-here;
    DEBUG(NAME_fill, Cprintf("deleted %ld leading blanks\n", i-here));
  }


  for(;;)				
  { 					/* copy string of non-blanks */
    for( ; here < to && !tislayout(tb->syntax, fetch(here)); here++ )
      col++;
    DEBUG(NAME_fill,
	  Cprintf("Word to %ld; col = %d; here-1 = %c, here = %d, to=%ld\n",
		  here, col, fetch(here-1), fetch(here), to));

    if ( col > rm )			/* trapped margin */
    { if ( nbreaks > 0 )
      { store_textbuffer(tb, breaks[nbreaks-1], '\n');
	if ( justify && last_break_col < rm )
	  distribute_spaces(tb, rm - last_break_col, nbreaks, breaks);
        return breaks[nbreaks-1] + 1;
      } else
      { if ( here == to )		/* end of buffer: add a newline */
	  insert_textbuffer(tb, here, 1, nl);
	else
          store_textbuffer(tb, here, '\n');
	return here+1;
      }        
    }
    
    if ( here >= to )
      return here;

    breaks[nbreaks] = here;
    if ( nbreaks < MAX_WORDS-1 )
      nbreaks++;			/* avoid crash */
    last_break_col = col;
    if ( fetch(here) != ' ' )
      store_textbuffer(tb, here, ' ');
    here++, col++;
    if ( ends_sentence(tb, here-2) )	/* sentence: one more space */
    { DEBUG(NAME_fill, Cprintf("End-sentence at %ld\n", here-2));
      if ( fetch(here) != ' ' )
      { insert_textbuffer(tb, here, 1, space);
	to++;
      }
      here++; col++;
    }

    for( i = here; i < to && tislayout(tb->syntax, fetch(i)); i++ )
      ;
    if ( i-here > 0 )
    { delete_textbuffer(tb, here, i-here);
      to -= i-here;
      DEBUG(NAME_fill, Cprintf("deleted %ld blanks\n", i-here));
    }

    if ( here >= to )
      return here;
  }
}

		/********************************
		*            SORTING		*
		********************************/

static int
compare_lines(const void *s, const void *t)
{ return strcmp(*((char **)s), *((char **)t));
}


status
sortTextBuffer(TextBuffer tb, Int from, Int to)
{ int f, t, ln, i, n;
  char *buf, **lines;
  char *bp, **lp;
  int bufsize;

  if ( isDefault(from) )
    from = ZERO;
  if ( isDefault(to) )
    to = toInt(tb->size);

  from = getScanTextBuffer(tb, from, NAME_line, ZERO, NAME_start);
  to   = getScanTextBuffer(tb, to, NAME_line, ZERO, NAME_start);
  f = valInt(from);
  t = valInt(to);
  ln = count_lines_textbuffer(tb, f, t+1); /* <=t below */

  if ( ln > 1 )				/* TBD (16B) */
  { bufsize = t - f + 1;
    lines = alloc((ln+1) * sizeof(char *));
    buf   = alloc(bufsize);

    for(bp=buf, lp=lines, i=f, *lp++=bp; i <= t; i++, bp++)
    { *bp = fetch_textbuffer(tb, i);

      if ( tisendsline(tb->syntax, *bp) )
      { *bp = EOS;
	*lp++ = bp+1;
        Cprintf("nl %-4d at %d\n", lp-lines, i);
      }
    }

    qsort(lines, ln, sizeof(char *), compare_lines);

    delete_textbuffer(tb, f, t-f);
    for(n=0; n<ln; n++)
    { String nl = str_nl(&tb->buffer);
      string s;

      str_set_ascii(&s, lines[n]);
      insert_textbuffer(tb, f, 1, &s);
      f += s.size;
      insert_textbuffer(tb, f, 1, nl);
      f++;
    }

    unalloc((ln+1) * sizeof(char *), lines);
    unalloc(bufsize, buf);
  }

  return changedTextBuffer(tb);
}

		 /*******************************
		 *	    LINE COUNTS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Counting lines and finding lines.  This   is  in  some applications done
quite often on long buffers and  therefore   we  have  written this at a
rather low level. 

count_lines_textbuffer()       finds the number of newlines in a region.
start_of_line_n_textbuffer()   finds the character index of the nth-1 line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
count_lines_textbuffer(TextBuffer tb, int f, int t)
{ int lines = 0;
  SyntaxTable syntax = tb->syntax;

  f = NormaliseIndex(tb, f);
  t = NormaliseIndex(tb, t);

  if ( f == 0 && t == tb->size && tb->lines >= 0 )
    return tb->lines;			/* use the total count */

  if ( istb8(tb) )
  { char8 *b = tb->tb_buffer8;
    int end1 = min(tb->gap_start, t);

    for( ; f<end1; f++)
    { if ( tisendsline(syntax, b[f]) )
      { lines++;
	Cprintf("count-nl %-4d at %d\n", lines, f);
      }
    }
    b += tb->gap_end - tb->gap_start + 1;
    for( ; f<t; f++)
    { if ( tisendsline(syntax, b[f]) )
      { lines++;
	Cprintf("count-nl %-4d at %d\n", lines, f);
      }
    }
  } else
  { char16 *b = tb->tb_buffer16;
    int end1 = min(tb->gap_start, t);

    for( ; f<end1; f++)
    { if ( tisendsline(syntax, b[f]) )
	lines++;
    }
    b += tb->gap_end - tb->gap_start + 1;
    for( ; f<t; f++)
    { if ( tisendsline(syntax, b[f]) )
	lines++;
    }
  }

  return lines;
}


int
start_of_line_n_textbuffer(TextBuffer tb, int lineno)
{ int i;
  SyntaxTable syntax = tb->syntax;

  if ( --lineno <= 0 )
    return 0;

  if ( istb8(tb) )
  { char8 *b = tb->tb_buffer8;

    for(i=0 ; i<tb->gap_start; i++)
    { if ( tisendsline(syntax, b[i]) )
      { if ( --lineno <= 0 )
	  return i+1;
      }
    }
    b += tb->gap_end - tb->gap_start + 1;
    for( ; i<tb->size; i++)
    { if ( tisendsline(syntax, b[i]) )
      { if ( --lineno <= 0 )
	  return i+1;
      }
    }
  } else
  { char16 *b = tb->tb_buffer16;

    for(i=0 ; i<tb->gap_start; i++)
    { if ( tisendsline(syntax, b[i]) )
      { if ( --lineno <= 0 )
	  return i+1;
      }
    }
    b += tb->gap_end - tb->gap_start + 1;
    for( ; i<tb->size; i++)
    { if ( tisendsline(syntax, b[i]) )
      { if ( --lineno <= 0 )
	  return i+1;
      }
    }
  }

  return tb->size;
}

		/********************************
		*     PRIMITIVE OPERATIONS      *
		*********************************/

int
fetch_textbuffer(TextBuffer tb, int where)
{ int idx;

  if ( where < 0 || where >= tb->size )
    return EOB;
  idx = Index(tb, where);

  return istb8(tb) ? (wchar)tb->tb_buffer8[idx] : (wchar)tb->tb_buffer16[idx];
}
  

static status
store_textbuffer(TextBuffer tb, int where, wchar c)
{ long idx;
  int old;

  if ( where < 0 || where >= tb->size )
    fail;
  idx = Index(tb, where);

  if ( istb8(tb) )
    old = tb->tb_buffer8[idx];
  else
    old = tb->tb_buffer16[idx];
    
  if ( old == c )
    succeed;
  if ( tisendsline(tb->syntax, old) )
    tb->lines--;
  if ( tisendsline(tb->syntax, c) )
    tb->lines++;

  start_change(tb, where);
  register_change_textbuffer(tb, where, 1);

  if ( istb8(tb) )
    tb->tb_buffer8[idx] = c;
  else
    tb->tb_buffer16[idx] = c;

  end_change(tb, where+1);
  CmodifiedTextBuffer(tb, ON);

  succeed;
}


status
change_textbuffer(TextBuffer tb, int where, void *s, int len)
{ int w, n;

  if ( len < 0 || where < 0 || where+len > tb->size )
    fail;

  register_change_textbuffer(tb, where, len);
  
  if ( istb8(tb) )
  { char8 *s2 = s;

    for( w=where, n=0; n < len; n++, w++ )
    { long i = Index(tb, w);
      if ( tb->tb_buffer8[i] != s2[n] )
      { if ( tisendsline(tb->syntax, tb->tb_buffer8[i]) )
	  tb->lines--;
	if ( tisendsline(tb->syntax, s2[n]) )
	  tb->lines++;
	tb->tb_buffer8[i] = s2[n];
      }
    }
  } else
  { char16 *s2 = s;

    for( w=where, n=0; n < len; n++, w++ )
    { long i = Index(tb, w);
      if ( tb->tb_buffer16[i] != s2[n] )
      { if ( tisendsline(tb->syntax, tb->tb_buffer16[i]) )
	  tb->lines--;
	if ( tisendsline(tb->syntax, s2[n]) )
	  tb->lines++;
	tb->tb_buffer16[i] = s2[n];
      }
    }
  }

  start_change(tb, where);
  end_change(tb, where+len);
  CmodifiedTextBuffer(tb, ON);

  succeed;
}


static void
mirror_textbuffer(TextBuffer tb, int f, int t)
{ if ( istb8(tb) )
  { for( ; f < t; f++, t-- )
      Swap(tb->tb_buffer8[f], tb->tb_buffer8[t])
  } else
  { for( ; f < t; f++, t-- )
      Swap(tb->tb_buffer16[f], tb->tb_buffer16[t])
  }
}


static status
transpose_textbuffer(TextBuffer tb, int f1, int t1, int f2, int t2)
{ Before(f1, t1);
  Before(f2, t2);

  f1 = NormaliseIndex(tb, f1);
  t1 = NormaliseIndex(tb, t1);
  f2 = NormaliseIndex(tb, f2);
  t2 = NormaliseIndex(tb, t2);

  if ( f1 > f2 )
  { Swap(f1, f2);
    Swap(t1, t2);
  }
  if ( t1 > f2 )
    fail;

  register_change_textbuffer(tb, f1, t2-f1);

  room(tb, t2, 0);			/* move gap out of the way */
  t1--; t2--;
  mirror_textbuffer(tb, f1, t2);
  mirror_textbuffer(tb, f1, f1+t2-f2);
  mirror_textbuffer(tb, t2+f1-t1, t2);
  mirror_textbuffer(tb, f1+t2-f2+1, t2+f1-t1-1);

  start_change(tb, f1);
  end_change(tb, t2+1);
  CmodifiedTextBuffer(tb, ON);

  succeed;
}


static status
downcase_textbuffer(TextBuffer tb, int from, int len)
{ for( ; from < tb->size && len > 0; len--, from++ )
  { wchar c;

    if ( tisupper(tb->syntax, (c=fetch(from))) )
      store_textbuffer(tb, from, tolower(c));
  }
      
  succeed;
}


static status
upcase_textbuffer(TextBuffer tb, int from, int len)
{ for( ; from < tb->size && len > 0; len--, from++ )
  { wchar c;

    if ( tislower(tb->syntax, (c=fetch(from))) )
      store_textbuffer(tb, from, toupper(c));
  }
      
  succeed;
}


static status
capitalise_textbuffer(TextBuffer tb, int from, int len)
{ wchar b = ' ';

  for( ; from < tb->size && len > 0; len--, from++ )
  { char c = fetch(from);

    if ( !tisalnum(tb->syntax, b) )
    { if ( tislower(tb->syntax, c) )
	store_textbuffer(tb, from, toupper(c));
    } else
    { if ( tisupper(tb->syntax, c) )
	store_textbuffer(tb, from, tolower(c));
    }

    b = c;
  }
      
  succeed;
}


static status
save_textbuffer(TextBuffer tb, int from, int len, SourceSink file)
{ int unitsize = (istb8(tb) ? sizeof(char8) : sizeof(char16));
  IOSTREAM *fd;
  status rval;

  room(tb, tb->size, 0);		/* move the gap to the end */

  if ( !(fd = Sopen_object(file, "wr")) )
    fail;				/* error message? */

  from = NormaliseIndex(tb, from);
  if ( (from + len) > tb->size )
    len = tb->size - from;

  Sfwrite(Address(tb, from), unitsize, len, fd);

  rval = checkErrorSourceSink(file, fd);
  Sclose(fd);

  return rval;
}
  

status
str_sub_text_buffer(TextBuffer tb, String s, int start, int len)
{ int idx;

  if ( start < 0 )
    start = 0;
  else if ( start > tb->size )
    start = tb->size-1;

  if ( len < 0 )
    len = 0;
  else if ( start + len > tb->size )
    len = tb->size - start;
  
  if ( start < tb->gap_start && start+len > tb->gap_start )
    room(tb, start + len, 1);
  
  str_cphdr(s, &tb->buffer);
  s->size = len;
  
  if ( start < tb->gap_start )
    idx = start;
  else
    idx = tb->gap_end + (start - tb->gap_start) + 1;

  if ( isstr8(s) )
    s->s_text8 = &tb->tb_buffer8[idx];
  else
    s->s_text16 = &tb->tb_buffer16[idx];

  succeed;
}



/*  Insert the contents of file `file' into the text buffer at position
    `where' `times' times. Returns SUCCEED if everything was ok, FAIL
    otherwise.
*/

static int
insert_file_textbuffer(TextBuffer tb, int where, int times, SourceSink file)
{ int size;
  char8 *addr;
  int unitsize = (istb8(tb) ? sizeof(char8) : sizeof(char16));
  int grow, here;
  IOSTREAM *fd;
  status rval;

  if ( times <= 0 )
    succeed;

  if ( !(fd = Sopen_object(file, "rr")) )
    fail;
  size = Ssize(fd);
  if ( !istb8(tb) )
  { if ( size % 2 )
      errorPce(tb, NAME_oddDataSize);
    size /= 2;
  }

  room(tb, where, times*size);
  start_change(tb, tb->gap_start);
  addr = Address(tb, tb->gap_start);

  size = Sfread(addr, unitsize, size, fd);
  rval = checkErrorSourceSink(file, fd);
  Sclose(fd);
  TRY(rval);

  grow = times*size;
  register_insert_textbuffer(tb, where, grow);

  tb->gap_start += size;
  tb->size += size;
  times--;
  while(times-- > 0)
  { memmove(Address(tb, tb->gap_start), addr, istb8(tb) ? size : size*2);
    tb->gap_start += size;
    tb->size += size;
  }
  end_change(tb, tb->gap_start);

					/* update <-lines */
  for(here=where; here<where+grow; here++)
  { if ( tisendsline(tb->syntax, fetch(here)) )
      tb->lines++;
  }

  shift_fragments(tb, where, grow);
  CmodifiedTextBuffer(tb, ON);
    
  succeed;
}


static status
insert_textbuffer_shift(TextBuffer tb, int where, int times,
			String s, int shift)
{ int grow;
  int unitsize;
  int size;
  int here;

  if ( istb8(tb) )
  { unitsize = sizeof(char8);
    size = (isstr8(s) ? s->size : s->size * 2);
  } else
  { unitsize = sizeof(char16);
    size = (isstr8(s) ? s->size/2 : s->size);
  }

  grow = times * size;
  where = NormaliseIndex(tb, where);
  room(tb, where, grow);

  register_insert_textbuffer(tb, where, grow);
  start_change(tb, tb->gap_start);
  while(times-- > 0)
  { memmove(Address(tb, tb->gap_start), s->s_text, istb8(tb) ? size : size*2);
    tb->gap_start += size;
    tb->size += size;
  }
  end_change(tb, tb->gap_start);  

  for(here=where; here<where+grow; here++)
  { if ( tisendsline(tb->syntax, fetch(here)) )
      tb->lines++;
  }

  if ( shift )
    shift_fragments(tb, where, grow);

  CmodifiedTextBuffer(tb, ON);

  succeed;
}


status
insert_textbuffer(TextBuffer tb, int where, int times, String s)
{ return insert_textbuffer_shift(tb, where, times, s, TRUE);
}


static status
clear_textbuffer(TextBuffer tb)
{ register_delete_textbuffer(tb, 0, tb->size);

  if ( tb->tb_buffer8 != NULL )
    pceFree(tb->tb_buffer8);
  
  start_change(tb, 0);
  end_change(tb, tb->size);

  tb->size = 0;
  tb->lines = 0;
  tb->allocated = ALLOC;
  tb->tb_buffer8 = pceMalloc(istb8(tb) ? ALLOC : ALLOC*2);

  tb->gap_start = 0;
  tb->gap_end = tb->allocated - 1;
  
  while( notNil(tb->first_fragment) )		/* destroy fragments */
    freeObject(tb->first_fragment);
  CmodifiedTextBuffer(tb, ON);

  succeed;
}


status
delete_textbuffer(TextBuffer tb, int where, int length)
{ if ( length < 0 )				/* delete backwards */
  { if ( where + length < 0 )			/* passed start: normalise */
      length = -where;

    where += length;
    length = -length;
  }

  if ( where + length > tb->size )		/* normalise on end */
    length = tb->size - where;

  if ( length == 0 )				/* out of bounds: ignore */
    succeed;

  room(tb, where, 0);				/* move the gap here */
  register_delete_textbuffer(tb, where, length);

  start_change(tb, where);
  tb->gap_end += length;
  tb->size -= length;
  end_change(tb, tb->size);

  shift_fragments(tb, where, -length);
  CmodifiedTextBuffer(tb, ON);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Shift the fragments after an insert/delete.  Insert is easy as this occurs
either before, inside or after the fragment.  Delete is more complicated.
The cases are:

Text: +++++++++++++++++++++++++++++++++++++++++++++++++++++
Frag:				-------
1)	       11111
2)			      2222
3)				   333
4)			      44444444444
5)				     5555555
6)						6666666
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
shift_fragments(TextBuffer tb, long int from, long int shift)
{ Fragment f;
  Cell cell;

  DEBUG(NAME_shift, Cprintf("Start shift: from = %ld, shift = %ld\n",
			    from, shift));

  if ( shift > 0 )			/* insert */
  { for(f=tb->first_fragment; notNil(f); f = f->next)
    { if ( from < f->start ||
	   (from == f->start && !(f->attributes & FRAG_INCLUDES_START)) )
	f->start += shift;
      else if ( from < f->start + f->length ||
		(from == f->start + f->length &&
		 (f->attributes & FRAG_INCLUDES_END)) )
	f->length += shift;
    }
  } else				/* delete */
  { int to = from - shift;
    Fragment next;

    for(f=tb->first_fragment; notNil(f); f = next)
    { int oldlen = f->length;

      next = f->next;
      DEBUG(NAME_shift, Cprintf("%s: start = %ld, length = %ld --> ",
				pp(f), f->start, f->length));
      if ( to < f->start )			/* 1 */
	f->start += shift;
      else
      { if ( from > f->start )			/* 3,5,6 */
	{ if ( from < f->start + f->length )	/* 3,5 */
	  { if ( to < f->start + f->length )	/* 3 */
	      f->length += shift;
	    else				/* 5 */
	      f->length += (to - (f->start + f->length)) + shift;
	  }
						/* 6 */
	} else					/* 2,4 */
	{ if ( to < f->start + f->length )	/* 2 */
	  { int reduce = to - f->start;

	    f->length -= reduce;
	    f->start -= -shift - reduce;
	  } else				/* 4 */
	  { f->length = 0;
	    f->start = from;
	  }
	}
      }

      DEBUG(NAME_shift, Cprintf("start = %ld, length = %ld\n",
				f->start, f->length));

      if ( f->length == 0 && oldlen != 0 )
      { DEBUG(NAME_shift, Cprintf("Invoking %s->emptied\n", pp(f)));
	send(f, NAME_emptied, 0);
      }
    }
  }

  for_cell(cell, tb->editors)
    send(cell->value, NAME_InsertEditor, toInt(from), toInt(shift), 0);

  succeed;
}


static void
start_change(TextBuffer tb, int where)
{ if ( tb->changed_start > where )
    tb->changed_start = where;
}


static void
end_change(TextBuffer tb, int where)
{ if ( tb->changed_end < where )
    tb->changed_end = where;
}


/*  Ensures the gap starts at `where' and is at least `grow' bytes long.

 ** Tue Apr  4 17:23:28 1989  jan@swivax.UUCP (Jan Wielemaker)  */

static int
room(TextBuffer tb, int where, int grow)
{ int shift;

  if ( grow + tb->size > tb->allocated )
  { int s = ROUND(tb->size + grow, ALLOC);
    int ag = tb->allocated - tb->gap_end - 1;

    shift = s - tb->allocated;
    tb->tb_buffer8 = pceRealloc(tb->tb_buffer8, istb8(tb) ? s : s*2);
    tb->allocated = s;
    
    memmove(Address(tb, tb->gap_end + 1 + shift),
	    Address(tb, tb->gap_end + 1),
	    istb8(tb) ? ag : ag*2);
    tb->gap_end += shift;
  }

  shift = where - tb->gap_start;
  if ( shift < 0 )				/* move gap towards start */
  { memmove(Address(tb, tb->gap_end + shift + 1),
	    Address(tb, where),
	    istb8(tb) ? -shift : 2 * -shift);
  } else if ( shift > 0 )			/* move gap towards end */
  { memmove(Address(tb, tb->gap_start),
	    Address(tb, tb->gap_end + 1),
	    istb8(tb) ? shift : 2 * shift);
  }    
  tb->gap_start += shift;			/* move the gap pointers */
  tb->gap_end += shift;

  succeed;
}

#if 0

		 /*******************************
		 *	    EVENT VIEW		*
		 *******************************/

#define endFragment(f) ((f)->start + (f)->length)

static void
forCharsInTextBuffer(TextBuffer tb, TextEventFunction f, text_event *ev)
{ Fragment *open_fragments = alloca(sizeof(Fragment) * 4);
  Fragment current_fragment, next_close_fragment = NULL;
  int open_allocated = 4;
  int open_count = 0;
  int size = tb->size;
  int i;

  current_fragment = tb->first_fragment; /* sorted by start-index */
  if ( isNil(current_fragment) )
    current_fragment = NULL;

  for(i=0; i<size; i++)
  { int n, m;

    if ( current_fragment && current_fragment->start == i )
    { ev->type = TXT_FRAGMENT_START;
      ev->value.fragment = current_fragment;
      (*f)(ev);
      
					/* ensure space */
      if ( open_count + 1 > open_allocated )
      { Fragment *new = alloca(sizeof(Fragment)*open_allocated*2);
	
	memcpy(new, open_fragments, sizeof(Fragment)*open_allocated);
	open_allocated *= 2;
	open_fragments = new;
      }

					/* insert fragment by close index */
      for( n=0;
	   n<open_count &&
	   endFragment(open_fragments[n]) > endFragment(current_fragment);
	   n++)
	;
      for(m=open_count; m > n; m--)
	open_fragments[m+1] = open_fragments[m];
      open_fragments[n] = current_fragment;
      next_close_fragment = open_fragments[open_count++];
      current_fragment = notNil(current_fragment->next)
				? current_fragment->next
				: (Fragment)NULL;
    }

    if ( next_close_fragment && endFragment(next_close_fragment) == i )
    { ev->type = TXT_FRAGMENT_END;
      ev->value.fragment = next_close_fragment;
      (*f)(ev);

      for( n=0; open_fragments[n] != next_close_fragment; n++ )
	;
      open_count--;
      for( ; n < open_count; n++ )
	open_fragments[n] = open_fragments[n+1];
      if ( open_count > 0 )
	next_close_fragment = open_fragments[open_count-1];
      else
	next_close_fragment = NULL;
    }

    ev->type = TXT_FRAGMENT_CHAR;
    ev->value.character = fetch_textbuffer(tb, i);

    (*f)(ev);
  }
}

#endif /*0*/

		 /*******************************
		 *	 ASFILE INTERFACE	*
		 *******************************/

static status
writeAsFileTextBuffer(TextBuffer tb, Int where, CharArray txt)
{ if ( isDefault(where) )
    where = toInt(tb->size);

  return insertTextBuffer(tb, where, txt, ONE);
}


static status
truncateAsFileTextBuffer(TextBuffer tb)
{ return clearTextBuffer(tb);
}


static Int
getSizeAsFileTextBuffer(TextBuffer tb)
{ answer(toInt(tb->size));
}


static StringObj
getReadAsFileTextBuffer(TextBuffer tb, Int from, Int size)
{ return getContentsTextBuffer(tb, from, size);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_writeAsFile[] =
        { "at=[int]", "text=char_array" };
static char *T_character[] =
        { "at=int", "character=char" };
static char *T_delete[] =
        { "at=int", "characters=[int]" };
static char *T_insertFile[] =
        { "at=int", "data=source_sink", "times=[int]" };
static char *T_insert[] =
        { "at=int", "text=char_array", "times=[int]" };
static char *T_format[] =
        { "format=char_array", "argument=any ..." };
static char *T_transpose[] =
        { "from1=int", "to1=int", "from2=int", "to2=int" };
static char *T_contents[] =
        { "from=[int]", "size=[int]" };
static char *T_fromADintD_toADintD[] =
        { "from=[int]", "to=[int]" };
static char *T_matchingBracket[] =
        { "from=int", "bracket=[char]" };
static char *T_matchingQuote[] =
        { "from=int", "direction={forward,backward}" };
static char *T_find[] =
        { "from=int", "for=string", "times=[int]",
	  "return=[{start,end}]", "exact_case=[bool]", "word=[bool]" };
static char *T_fromAint_sizeAint[] =
        { "from=int", "size=int" };
static char *T_skipComment[] =
        { "from=int", "to=[int]", "skip_layout=[bool]" };
static char *T_skipLayout[] =
        { "from=int",
	  "direction=[{forward,backward}]", "skip_newline=[bool]" };
static char *T_scan[] =
        { "from=int",
	  "unit={character,word,line,sentence,paragraph,term}",
	  "times=[int]", "return=[{start,end}]" };
static char *T_save[] =
        { "in=file", "from=[int]", "size=[int]" };
static char *T_forAllComments[] =
	{ "message=code", "from=[int]", "to=[int]" };
static char *T_indexAint_startADintD[] =
        { "index=int", "start=[int]" };
static char *T_report[] =
        { "kind={status,inform,progress,done,warning,error}", "format=[char_array]", "argument=any ..." };
static char *T_append[] =
        { "text=char_array", "times=[int]" };

/* Instance Variables */

static vardecl var_textBuffer[] =
{ IV(NAME_firstFragment, "fragment*", IV_GET,
     NAME_fragment, "First fragment (lowest start index)"),
  IV(NAME_lastFragment, "fragment*", IV_GET,
     NAME_fragment, "Last fragment (highest start index)"),
  IV(NAME_editors, "chain", IV_GET,
     NAME_organisation, "Editors displaying this buffer"),
  SV(NAME_modified, "bool", IV_GET|IV_STORE, modifiedTextBuffer,
     NAME_modified, "Has buffer been modified"),
  SV(NAME_undoBufferSize, "bytes=int", IV_GET|IV_STORE, undoBufferSizeTextBuffer,
     NAME_modified, "Size of the undo-buffer in characters"),
  IV(NAME_syntax, "syntax_table", IV_BOTH,
     NAME_language, "Description of the used syntax"),
  IV(NAME_changedStart, "alien:int", IV_NONE,
     NAME_repaint, "Start of changes since last repaint"),
  IV(NAME_changedEnd, "alien:int", IV_NONE,
     NAME_repaint, "End of changes since last repaint"),
  IV(NAME_gapStart, "alien:int", IV_NONE,
     NAME_storage, "Start of gap in buffer"),
  IV(NAME_gapEnd, "alien:int", IV_NONE,
     NAME_storage, "End of gap in buffer"),
  IV(NAME_size, "alien:int", IV_NONE,
     NAME_cardinality, "Number of valid characters in buffer"),
  IV(NAME_lines, "alien:int", IV_NONE,
     NAME_cardinality, "Number of newlines in the buffer"),
  IV(NAME_allocated, "alien:int", IV_NONE,
     NAME_storage, "Total size of buffer"),
  IV(NAME_undoBuffer, "alien:UndoBuffer", IV_NONE,
     NAME_storage, "Record undo information here"),
  IV(NAME_stringHeader, "alien:str_h", IV_NONE,
     NAME_storage, "Encoding description"),
  IV(NAME_buffer, "alien:char *", IV_NONE,
     NAME_storage, "Actual storage bin")
};

/* Send Methods */

static senddecl send_textBuffer[] =
{ SM(NAME_initialise, 1, "contents=[char_array]", initialiseTextBuffer,
     DEFAULT, "Create from initial contents"),
  SM(NAME_unlink, 0, NULL, unlinkTextBuffer,
     DEFAULT, "Destroy the text"),
  SM(NAME_capitalise, 2, T_fromAint_sizeAint, capitaliseTextBuffer,
     NAME_case, "Capitalise (start, length)"),
  SM(NAME_downcase, 2, T_fromAint_sizeAint, downcaseTextBuffer,
     NAME_case, "Bring (start, length) to lowercase"),
  SM(NAME_upcase, 2, T_fromAint_sizeAint, upcaseTextBuffer,
     NAME_case, "Bring (start, length) to uppercase"),
  SM(NAME_append, 2, T_append, appendTextBuffer,
     NAME_edit, "Append string (n-times)"),
  SM(NAME_character, 2, T_character, characterTextBuffer,
     NAME_edit, "Change character at index to ASCII value"),
  SM(NAME_clear, 0, NULL, clearTextBuffer,
     NAME_edit, "Delete all contents (and fragments)"),
  SM(NAME_contents, 1, "char_array", contentsTextBuffer,
     NAME_edit, "Set the contents (deletes fragments)"),
  SM(NAME_delete, 2, T_delete, deleteTextBuffer,
     NAME_edit, "Delete characters from index"),
  SM(NAME_insert, 3, T_insert, insertTextBuffer,
     NAME_edit, "Insert string at index (n-times)"),
  SM(NAME_transpose, 4, T_transpose, transposeTextBuffer,
     NAME_edit, "Transpose [from1, to1) with [from2, to2)"),
  SM(NAME_insertFile, 3, T_insertFile, insertFileTextBuffer,
     NAME_file, "Insert file at index (n-times)"),
  SM(NAME_save, 3, T_save, saveTextBuffer,
     NAME_file, "Save (from, length) to file"),
  SM(NAME_format, 2, T_format, formatTextBuffer,
     NAME_format, "Append formatted text"),
  SM(NAME_forAllFragments, 1, "code", forAllFragmentsTextBuffer,
     NAME_iterate, "Iterate code over all fragments"),
  SM(NAME_inComment, 2, T_indexAint_startADintD, inCommentTextBuffer,
     NAME_language, "Test if first index is in comment"),
  SM(NAME_forAllComments, 3, T_forAllComments, forAllCommentsTextBuffer,
     NAME_iterate, "Iterate code over all comments"),
  SM(NAME_inString, 2, T_indexAint_startADintD, inStringTextBuffer,
     NAME_language, "Test if first index is in string constant"),
  SM(NAME_checkPointUndo, 0, NULL, checkpointUndoTextBuffer,
     NAME_modified, "Set `no-change' checkpoint in undo buffer"),
  SM(NAME_markUndo, 0, NULL, markUndoTextBuffer,
     NAME_modified, "Mark undo point"),
  SM(NAME_resetUndo, 0, NULL, resetUndoTextBuffer,
     NAME_modified, "Clear the undo-buffer"),
  SM(NAME_undo, 0, NULL, undoTextBuffer,
     NAME_modified, "Undo operations backto last mark"),
  SM(NAME_attach, 1, "editor", attachTextBuffer,
     NAME_organisation, "Attach the given editor"),
  SM(NAME_detach, 1, "editor", detachTextBuffer,
     NAME_organisation, "Detach the given editor"),
  SM(NAME_report, 3, T_report, reportTextBuffer,
     NAME_report, "Report message (send to <-editors)"),
  SM(NAME_sort, 2, T_fromADintD_toADintD, sortTextBuffer,
     NAME_sort, "Sort [from, to) alphabetically by line"),
  SM(NAME_truncateAsFile, 0, NULL, truncateAsFileTextBuffer,
     NAME_stream, "Implements handling a buffer as a file"),
  SM(NAME_writeAsFile, 2, T_writeAsFile, writeAsFileTextBuffer,
     NAME_stream, "Implements handling a buffer as a file")
};

/* Get Methods */

static getdecl get_textBuffer[] =
{ GM(NAME_convert, 1, "text_buffer", "editor", getConvertTextBuffer,
     DEFAULT, "Return `editor <-text_buffer'"),
  GM(NAME_length, 0, "int", NULL, getSizeTextBuffer,
     NAME_cardinality, "Equivalent to <-size (# characters)"),
  GM(NAME_size, 0, "int", NULL, getSizeTextBuffer,
     NAME_cardinality, "Number of characters in buffer"),
  GM(NAME_findAllFragments, 1, "matching=chain", "test=[code]", getFindAllFragmentsTextBuffer,
     NAME_fragment, "New chain holding fragments accepted by code"),
  GM(NAME_findFragment, 1, "fragment", "test=code", getFindFragmentTextBuffer,
     NAME_fragment, "First fragment that accepts code"),
  GM(NAME_matchingBracket, 2, "index=int", T_matchingBracket, getMatchingBracketTextBuffer,
     NAME_language, "Find bracket matching bracket at index"),
  GM(NAME_matchingQuote, 2, "index=int", T_matchingQuote, getMatchingQuoteTextBuffer,
     NAME_language, "Find matching string-quote"),
  GM(NAME_skipComment, 3, "index=int", T_skipComment, getSkipCommentTextBuffer,
     NAME_language, "Skip comments and optionally white space"),
  GM(NAME_skipLayout, 3, "index=int", T_skipLayout, getSkipBlanksTextBuffer,
     NAME_language, "Skip white-space in either direction"),
  GM(NAME_lineNumber, 1, "line=int", "index=[int]", getLineNumberTextBuffer,
     NAME_line, "Get line number (1-based) for character index"),
  GM(NAME_scan, 4, "index=int", T_scan, getScanTextBuffer,
     NAME_parse, "Scan textual units"),
  GM(NAME_character, 1, "char", "at=int", getCharacterTextBuffer,
     NAME_read, "ASCII value of character at index"),
  GM(NAME_contents, 2, "string", T_contents, getContentsTextBuffer,
     NAME_read, "New string holding text (from, length)"),
  GM(NAME_sub, 2, "string", T_fromADintD_toADintD, getSubTextBuffer,
     NAME_read, "New string holding text [from, to)"),
  GM(NAME_find, 6, "index=int", T_find, getFindTextBuffer,
     NAME_search, "Search for a string"),
  GM(NAME_readAsFile, 2, "string", T_fromAint_sizeAint,getReadAsFileTextBuffer,
     NAME_stream, "Implement reading as a file"),
  GM(NAME_sizeAsFile, 0, "characters=int", NULL, getSizeAsFileTextBuffer,
     NAME_stream, "Implement seek when using as a file"),
  GM(NAME_scanSyntax, 2, "name", T_fromADintD_toADintD,getScanSyntaxTextBuffer,
     NAME_language, "Find syntactical state of position")
};

/* Resources */

static classvardecl rc_textBuffer[] =
{ RC(NAME_syntax, "[syntax_table]", "default",
     "Syntax definition"),
  RC(NAME_undoBufferSize, "int", "10000",
     "Memory allocated to store undo")
};

/* Class Declaration */

static Name textBuffer_termnames[] = { NAME_string };

ClassDecl(textBuffer_decls,
          var_textBuffer, send_textBuffer, get_textBuffer, rc_textBuffer,
          0, textBuffer_termnames,
          "$Rev$");

status
makeClassTextBuffer(Class class)
{ declareClass(class, &textBuffer_decls);

  setLoadStoreFunctionClass(class, loadTextBuffer, storeTextBuffer);
  saveStyleVariableClass(class, NAME_editors, NAME_nil);
  setCloneFunctionClass(class, cloneTextBuffer);
  cloneStyleVariableClass(class, NAME_editors, NAME_referenceChain);

  succeed;
}
