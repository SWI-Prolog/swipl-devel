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
forwards int	count_lines_textbuffer(TextBuffer, int, int);
forwards status capitalise_textbuffer(TextBuffer, int, int);
forwards status clear_textbuffer(TextBuffer);
forwards status downcase_textbuffer(TextBuffer, int, int);
forwards void	end_change(TextBuffer, int);
forwards Int    getSizeTextBuffer(TextBuffer);
forwards status store_textbuffer(TextBuffer, int, wchar);
forwards status transpose_textbuffer(TextBuffer, int, int, int, int);
forwards status upcase_textbuffer(TextBuffer, int, int);
forwards status save_textbuffer(TextBuffer, int, int, FileObj);
forwards status insert_file_textbuffer(TextBuffer, int, int, FileObj);
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
{ assign(tb, first_fragment, NIL);
  assign(tb, last_fragment, NIL);
  assign(tb, editors, newObject(ClassChain, 0));
  assign(tb, undo_buffer_size, DEFAULT);
  assign(tb, syntax, DEFAULT);
  obtainResourcesObject(tb);

  tb->undo_buffer = NULL;
  tb->tb_buffer8 = NULL;
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

  for_chain(tb->editors, editor, freeObject(editor));
  clearChain(tb->editors);

  while( notNil(tb->first_fragment) )	/* destroy fragments */
    freeObject(tb->first_fragment);

  if ( tb->tb_buffer8 != NULL )		/* deallocate the buffer */
  { free(tb->tb_buffer8);
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
loadTextBuffer(TextBuffer tb, FILE *fd, ClassDef def)
{ TRY( loadSlotsObject(tb, fd, def) );

  if ( isNil(tb->syntax) )
    assign(tb, syntax, getResourceValueObject(tb, NAME_syntax));

  assign(tb, editors, newObject(ClassChain, 0));
  tb->size = loadWord(fd);
  tb->allocated = ROUND(tb->size, ALLOC);
  str_cphdr(&tb->buffer, str_nl(NULL));	/* ASCII */
  tb->tb_buffer8 = malloc(tb->allocated);
  fread(Address(tb, 0), sizeof(char), tb->size, fd); /* TBD */
  tb->gap_start = tb->size;
  tb->gap_end = tb->allocated - 1;

  tb->changed_start = tb->size;
  tb->changed_end = 0;  
  CmodifiedTextBuffer(tb, OFF);

  succeed;
}


static status
cloneTextBuffer(TextBuffer tb, TextBuffer clone)
{ clonePceSlots(tb, clone);

  clone->undo_buffer = NULL;
  clone->tb_buffer8 = malloc(clone->allocated);
  memcpy(clone->tb_buffer8, tb->tb_buffer8, clone->allocated);
  clone->changed_start = clone->size;
  clone->changed_end = 0;
  assign(clone, editors, newObject(ClassChain, 0));

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
  { for_cell(cell, tb->editors)
      send(cell->value, NAME_ChangedRegion, toInt(tb->changed_start),
					    toInt(tb->changed_end), 0);
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
    sendv(cell->value, NAME_ChangedFragmentList, 0, NULL);

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
insertFileTextBuffer(TextBuffer tb, Int where, FileObj file, Int times)
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
saveTextBuffer(TextBuffer tb, FileObj file, Int from, Int len)
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

  str_sub_text_buffer(tb, &s, valInt(from), valInt(to)-valInt(from));
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
scan_textbuffer(TextBuffer tb, int from, Name unit, int amount, char az)
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
{ int i = valInt(idx);
  int ic = (direction == NAME_forward ? 1 : -1);
  wchar c = fetch(i);

  if ( !tisquote(tb->syntax, c) )
    fail;

  for( i += ic; i >= 0 && i < tb->size; i += ic )
    if ( fetch(i) == c && (!tisstringescape(tb->syntax, c, fetch(i-1)) ||
			   i-1 == valInt(idx)) )
      break;

  if ( fetch(i) == c )
    answer(toInt(i));

  fail;
}


static status
inStringTextBuffer(TextBuffer tb, Int pos, Int from)
{ long idx = valInt(pos);
  long here = (isDefault(from) ? 0L : valInt(from));

  while(here <= idx)
  { if ( tisquote(tb->syntax, fetch(here)) )
    { Int match;

      DEBUG(NAME_inString, Cprintf("here = %ld (idx = %ld)\n", here, idx));
      if ( (match = getMatchingQuoteTextBuffer(tb, toInt(here), NAME_forward)))
      { DEBUG(NAME_inString, Cprintf("Matching: %ld\n", valInt(match)));

	if ( (here = valInt(match)) >= idx )
	  succeed;
      } else
	succeed;
    }

    here++;
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
  char c;

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
    }
    else if ( tisquote(syntax, c) )
      for( i += ic; i >= 0 && i < tb->size; i += ic )
        if ( fetch(i) == c && !tisstringescape(syntax, c, fetch(i-1)) )
	  break;

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

  if ( fwd )			/* forward */
  { for(;;)
    { if ( pos < 0 )
	answer(toInt(tb->size));

      if ( layouttoo != OFF )
	for( ; pos < end && tislayout(tb->syntax, fetch(pos)); pos++ )
	  ;
      if ( tiscommentstart(tb->syntax, fetch(pos)) )
      { for( ; pos < end && !tiscommentend(tb->syntax, fetch(pos)); pos++ )
	  ;
	continue;
      }
      if ( tiscommentstart1(tb->syntax, fetch(pos)) &&
	   tiscommentstart2(tb->syntax, fetch(pos+1)) )
      { for( pos += 4;
	     pos < end && !(tiscommentend1(tb->syntax, fetch(pos-2)) &&
	    	            tiscommentend2(tb->syntax, fetch(pos-1)));
	     pos++ )
	  ;
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

  while(here <= idx)
  { char c;

    if ( tiscommentstart(tb->syntax, c=fetch(here)) ||
	 (tiscommentstart1(tb->syntax, c) &&
	  tiscommentstart2(tb->syntax, fetch(here+1))) )
    { here = valInt(getSkipCommentTextBuffer(tb, toInt(here), DEFAULT, OFF));
      if ( here >= idx )
	succeed;
    }

    here++;
  }

  fail;
}


Int
getLineNumberTextBuffer(TextBuffer tb, Int i)
{ int e = (isDefault(i) ? tb->size : valInt(i));
  int n, l;

  e = NormaliseIndex(tb, e);
  for( n=0,l=1; n < e; n++ )
    if ( tisendsline(tb->syntax, fetch_textbuffer(tb, n)) )
      l++;

  answer(toInt(l));
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
match(register TextBuffer tb, int here, String s, int ec, int wm)
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
  int *extra = alloca(nbreaks * sizeof(int));
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
  ln = count_lines_textbuffer(tb, f, t);

  if ( ln > 1 )				/* TBD (16B) */
  { bufsize = t - f + 1;
    lines = alloc((ln+1) * sizeof(char *));
    buf   = alloc(bufsize);

    for(bp=buf, lp=lines, i=f, *lp++=bp; i <= t; i++, bp++)
    { *bp = fetch_textbuffer(tb, i);

      if ( tisendsline(tb->syntax, *bp) )
      { *bp = EOS;
	*lp++ = bp+1;
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


		/********************************
		*     PRIMITIVE OPERATIONS      *
		*********************************/

static int
count_lines_textbuffer(TextBuffer tb, int f, int t)
{ int lines = 0;

  f = NormaliseIndex(tb, f);
  t = NormaliseIndex(tb, t);

  for( ; f < t; f++ )
    if ( tisendsline(tb->syntax, fetch(f)) )
      lines++;

  return lines;
}


int
fetch_textbuffer(register TextBuffer tb, register int where)
{ int idx;

  if ( where < 0 || where >= tb->size )
    return EOB;
  idx = Index(tb, where);

  return istb8(tb) ? (wchar)tb->tb_buffer8[idx] : (wchar)tb->tb_buffer16[idx];
}
  

static status
store_textbuffer(TextBuffer tb, int where, wchar c)
{ long idx;

  if ( where < 0 || where >= tb->size )
    fail;
  idx = Index(tb, where);

  if ( istb8(tb) )
  { if ( tb->tb_buffer8[idx] == c )
      succeed;
  } else
  { if ( tb->tb_buffer16[idx] == c )
      succeed;
  }
    
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

  if ( len < 0 || where < 0 || where+len >= tb->size )
    fail;

  register_change_textbuffer(tb, where, len);
  
  if ( istb8(tb) )
  { char8 *s2 = s;

    for( w=where, n=0; n < len; n++, w++ )
    { if ( tb->gap_start <= w )
	tb->tb_buffer8[tb->gap_end + (w - tb->gap_start) + 1] = s2[n];
      else
	tb->tb_buffer8[w] = s2[n];
    }
  } else
  { char16 *s2 = s;

    for( w=where, n=0; n < len; n++, w++ )
    { if ( tb->gap_start <= w )
	tb->tb_buffer16[tb->gap_end + (w - tb->gap_start) + 1] = s2[n];
      else
	tb->tb_buffer16[w] = s2[n];
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
save_textbuffer(TextBuffer tb, int from, int len, FileObj file)
{ int unitsize = (istb8(tb) ? sizeof(char8) : sizeof(char16));

  room(tb, tb->size, 0);		/* move the gap to the end */

  TRY(send(file, NAME_open, NAME_write, 0));

  from = NormaliseIndex(tb, from);
  if ( (from + len) > tb->size )
    len = tb->size - from;

  if ( len && fwrite(Address(tb, from), unitsize, len, file->fd) == 0 )
    return reportErrorFile(file);

  return send(file, NAME_close, 0);
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
insert_file_textbuffer(TextBuffer tb, int where, int times, FileObj file)
{ int size;
  char8 *addr;
  int unitsize = (istb8(tb) ? sizeof(char8) : sizeof(char16));

  if ( times <= 0 )
    succeed;

  TRY(openFile(file, NAME_read, DEFAULT, DEFAULT));
  size = valInt(getSizeFile(file));
  if ( !istb8(tb) )
  { if ( size % 2 )
      errorPce(tb, NAME_oddDataSize);
    size /= 2;
  }

  room(tb, where, times*size);
  start_change(tb, tb->gap_start);
  addr = Address(tb, tb->gap_start);

  size = fread(addr, unitsize, size, file->fd);
  TRY(checkErrorFile(file));
  TRY(closeFile(file));

  register_insert_textbuffer(tb, where, times*size);

  tb->gap_start += size;
  tb->size += size;
  times--;
  while(times-- > 0)
  { memmove(Address(tb, tb->gap_start), addr, istb8(tb) ? size : size*2);
    tb->gap_start += size;
    tb->size += size;
  }
  end_change(tb, tb->gap_start);

  shift_fragments(tb, where, times*size);
  CmodifiedTextBuffer(tb, ON);
    
  succeed;
}


static status
insert_textbuffer_shift(TextBuffer tb, int where, int times,
			String s, int shift)
{ int grow;
  int unitsize;
  int size;

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
{ if ( tb->tb_buffer8 != NULL )
    free(tb->tb_buffer8);

  register_delete_textbuffer(tb, 0, tb->size);
  start_change(tb, 0);
  end_change(tb, tb->size);

  tb->size = 0;
  tb->allocated = ALLOC;
  tb->tb_buffer8 = malloc(istb8(tb) ? ALLOC : ALLOC*2);

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
      if ( f->length == 0 && oldlen != 0 )
	send(f, NAME_emptied, 0);

      DEBUG(NAME_shift, Cprintf("start = %ld, length = %ld\n",
				f->start, f->length));
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
    tb->tb_buffer8 = realloc(tb->tb_buffer8, istb8(tb) ? s : s*2);
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


status
makeClassTextBuffer(Class class)
{ sourceClass(class, makeClassTextBuffer, __FILE__, "$Revision$");

  localClass(class, NAME_firstFragment, NAME_fragment, "fragment*", NAME_get,
	     "First fragment (lowest start index)");
  localClass(class, NAME_lastFragment, NAME_fragment, "fragment*", NAME_get,
	     "Last fragment (highest start index)");
  localClass(class, NAME_editors, NAME_organisation, "chain", NAME_get,
	     "Editors displaying this buffer");
  localClass(class, NAME_modified, NAME_modified, "bool", NAME_get,
	     "Has buffer been modified");
  localClass(class, NAME_undoBufferSize, NAME_modified, "bytes=int", NAME_get,
	     "Size of the undo-buffer in characters");
  localClass(class, NAME_syntax, NAME_language, "syntax_table", NAME_both,
	     "Description of the used syntax");
  localClass(class, NAME_changedStart, NAME_repaint, "alien:int", NAME_none,
	     "Start of changes since last repaint");
  localClass(class, NAME_changedEnd, NAME_repaint, "alien:int", NAME_none,
	     "End of changes since last repaint");
  localClass(class, NAME_gapStart, NAME_storage, "alien:int", NAME_none,
	     "Start of gap in buffer");
  localClass(class, NAME_gapEnd, NAME_storage, "alien:int", NAME_none,
	     "End of gap in buffer");
  localClass(class, NAME_size, NAME_cardinality, "alien:int", NAME_none,
	     "Number of valid characters in buffer");
  localClass(class, NAME_allocated, NAME_storage, "alien:int", NAME_none,
	     "Total size of buffer");
  localClass(class, NAME_undoBuffer, NAME_storage, "alien:UndoBuffer",
	     NAME_none,
	     "Record undo information here");
  localClass(class, NAME_stringHeader, NAME_storage, "alien:str_h",
	     NAME_none,
	     "Encoding description");
  localClass(class, NAME_buffer, NAME_storage, "alien:char *", NAME_none,
	     "Actual storage bin");

  termClass(class, "text_buffer", 0, NAME_string);
  setLoadStoreFunctionClass(class, loadTextBuffer, storeTextBuffer);
  saveStyleVariableClass(class, NAME_editors, NAME_nil);
  setCloneFunctionClass(class, cloneTextBuffer);
  cloneStyleVariableClass(class, NAME_editors, NAME_nil);

  storeMethod(class, NAME_undoBufferSize, undoBufferSizeTextBuffer);
  storeMethod(class, NAME_modified, modifiedTextBuffer);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "contents=[char_array]",
	     "Create from initial contents",
	     initialiseTextBuffer);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy the text",
	     unlinkTextBuffer);
  sendMethod(class, NAME_attach, NAME_organisation, 1, "editor",
	     "Attach the given editor",
	     attachTextBuffer);
  sendMethod(class, NAME_detach, NAME_organisation, 1, "editor",
	     "Detach the given editor",
	     detachTextBuffer);
  sendMethod(class, NAME_clear, NAME_edit, 0,
	     "Delete all contents (and fragments)",
	     clearTextBuffer);
  sendMethod(class, NAME_contents, NAME_edit, 1, "char_array",
	     "Set the contents (deletes fragments)",
	     contentsTextBuffer);
  sendMethod(class, NAME_insert, NAME_edit, 3,
	     "at=int", "text=char_array", "times=[int]",
	     "Insert string at index (n-times)",
	     insertTextBuffer);
  sendMethod(class, NAME_insertFile, NAME_file, 3,
	     "at=int", "data=file", "times=[int]",
	     "Insert file at index (n-times)",
	     insertFileTextBuffer);
  sendMethod(class, NAME_append, NAME_edit, 2,
	     "text=char_array", "times=[int]",
	     "Append string (n-times)",
	     appendTextBuffer);
  sendMethod(class, NAME_format, NAME_format, 2,
	     "format=char_array", "argument=any ...",
	     "Append formatted text",
	     formatTextBuffer);
  sendMethod(class, NAME_delete, NAME_edit, 2, "at=int", "characters=[int]",
	     "Delete characters from index",
	     deleteTextBuffer);
  sendMethod(class, NAME_character, NAME_edit, 2, "at=int", "character=char",
	     "Change character at index to ASCII value",
	     characterTextBuffer);
  sendMethod(class, NAME_downcase, NAME_case, 2, "from=int", "size=int",
	     "Bring (start, lenght) to lowercase",
	     downcaseTextBuffer);
  sendMethod(class, NAME_upcase, NAME_case, 2, "from=int", "size=int",
	     "Bring (start, lenght) to uppercase",
	     upcaseTextBuffer);
  sendMethod(class, NAME_capitalise, NAME_case, 2, "from=int", "size=int",
	     "Capitalise (start, lenght)",
	     capitaliseTextBuffer);
  sendMethod(class, NAME_transpose, NAME_edit, 4,
	     "from1=int", "to1=int", "from2=int", "to2=int",
	     "Transpose [from1, to1) with [from2, to2)",
	     transposeTextBuffer);
  sendMethod(class, NAME_save, NAME_file, 3,
	     "in=file", "from=[int]", "size=[int]",
	     "Save (from, length) to file",
	     saveTextBuffer);
  sendMethod(class, NAME_sort, NAME_sort, 2, "from=[int]", "to=[int]",
	     "Sort [from, to) alphabetically by line",
	     sortTextBuffer);
  sendMethod(class, NAME_undo, NAME_modified, 0,
	     "Undo operations backto last mark",
	     undoTextBuffer);
  sendMethod(class, NAME_markUndo, NAME_modified, 0,
	     "Mark undo point",
	     markUndoTextBuffer);
  sendMethod(class, NAME_resetUndo, NAME_modified, 0,
	     "Clear the undo-buffer",
	     resetUndoTextBuffer);
  sendMethod(class, NAME_checkPointUndo, NAME_modified, 0,
	     "Set `no-change' checkpoint in undo buffer",
	     checkpointUndoTextBuffer);
  sendMethod(class, NAME_inString, NAME_language, 2,
	     "index=int", "start=[int]",
	     "Test if first index is in string constant",
	     inStringTextBuffer);
  sendMethod(class, NAME_inComment, NAME_language, 2,
	     "index=int", "start=[int]",
	     "Test if first index is in comment",
	     inCommentTextBuffer);
  sendMethod(class, NAME_report, NAME_report, 3,
	     "kind={status,inform,progress,done,warning,error}",
	     "format=[char_array]", "argument=any ...",
	     "Report message (send to <-editors)",
	     reportTextBuffer);
  sendMethod(class, NAME_forAllFragments, NAME_iterate, 1, "code",
	     "Iterate code over all fragments",
	     forAllFragmentsTextBuffer);

  getMethod(class, NAME_convert, DEFAULT, "text_buffer", 1, "editor",
	    "Return `editor <-text_buffer'",
	    getConvertTextBuffer);
  getMethod(class, NAME_length, NAME_cardinality, "int", 0,
	    "Equivalent to <-size (# characters)",
	    getSizeTextBuffer);
  getMethod(class, NAME_size, NAME_cardinality, "int", 0,
	    "Number of characters in buffer",
	    getSizeTextBuffer);
  getMethod(class, NAME_findAllFragments, NAME_fragment, "matching=chain", 1,
	    "test=[code]",
	    "New chain holding fragments accepted by code",
	    getFindAllFragmentsTextBuffer);
  getMethod(class, NAME_findFragment, NAME_fragment, "fragment", 1,
	    "test=code",
	    "First fragment that accepts code",
	    getFindFragmentTextBuffer);
  getMethod(class, NAME_contents, NAME_read, "string", 2,
	    "from=[int]", "size=[int]",
	    "New string holding text (from, length)",
	    getContentsTextBuffer);
  getMethod(class, NAME_sub, NAME_read, "string", 2,
	    "from=[int]", "to=[int]",
	    "New string holding text [from, to)",
	    getSubTextBuffer);
  getMethod(class, NAME_character, NAME_read, "char", 1, "at=int",
	    "ASCII value of character at index",
	    getCharacterTextBuffer);
  getMethod(class, NAME_scan, NAME_parse, "index=int", 4,
	    "from=int", "unit={character,word,line,sentence,paragraph,term}",
	    "times=[int]", "return=[{start,end}]",
	    "Scan textual units",
	    getScanTextBuffer);
  getMethod(class, NAME_find, NAME_search, "index=int", 6,
	    "from=int", "for=string", "times=[int]",
	    "return=[{start,end}]", "exact_case=[bool]", "word=[bool]",
	    "Search for a string",
	    getFindTextBuffer);
  getMethod(class, NAME_lineNumber, NAME_line, "line=int", 1, "index=[int]",
	    "Get line number (1-based) for character index",
	    getLineNumberTextBuffer);
  getMethod(class, NAME_matchingBracket, NAME_language, "index=int", 2,
	    "from=int", "bracket=[char]",
	    "Find bracket matching bracket at index",
	    getMatchingBracketTextBuffer);
  getMethod(class, NAME_matchingQuote, NAME_language, "index=int", 2,
	    "from=int", "direction={forward,backward}",
	    "Find matching string-quote",
	    getMatchingQuoteTextBuffer);
  getMethod(class, NAME_skipComment, NAME_language, "index=int", 3,
	    "from=int", "to=[int]", "skip_layout=[bool]",
	    "Skip comments and optionally white space",
	    getSkipCommentTextBuffer);

  attach_resource(class, "undo_buffer_size", "int", "10000",
		  "Memory allocated to store undo");
  attach_resource(class, "syntax", "[syntax_table]",
		  "default",
		  "Syntax definition");

  succeed;
}

