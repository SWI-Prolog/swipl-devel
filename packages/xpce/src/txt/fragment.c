/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/text.h>
#include <h/unix.h>

static int	unlink_fragment(Fragment);
static int	link_fragment(Fragment);
static status	normaliseFragment(Fragment f);

#define normalize_tb(tb, index) \
	(index < 0        ? 0 : \
         index > tb->size ? tb->size \
	   		  : index)


static status
initialiseFragment(Fragment f, TextBuffer tb, Int s, Int l, Name style)
{ assign(f, textbuffer, tb);
  assign(f, style, style);

  f->start  = valInt(s);
  f->length = valInt(l);

  normaliseFragment(f);
  link_fragment(f);
  ChangedFragmentListTextBuffer(f->textbuffer);
  ChangedRegionTextBuffer(f->textbuffer,
			  toInt(f->start), toInt(f->start + f->length));

  succeed;
}

static status
unlinkFragment(Fragment f)
{ if ( notNil(f->textbuffer) )
  { unlink_fragment(f);
    ChangedFragmentListTextBuffer(f->textbuffer);
    ChangedRegionTextBuffer(f->textbuffer,
			    toInt(f->start), toInt(f->start + f->length));
    assign(f, textbuffer, NIL);
  }

  succeed;
}


static status
storeFragment(Fragment f, FileObj file)
{ if ( !storeSlotsObject(f, file) ||
       !storeWordFile(file, (Any) f->start) ||
       !storeWordFile(file, (Any) f->length) )
    fail;

  succeed;
}


static status
loadFragment(Fragment f, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(f, fd, def));
  if ( restoreVersion >= 10 )
  { f->start  = loadWord(fd);
    f->length = loadWord(fd);
  }

  succeed;
}


static status
convertOldSlotFragment(Fragment f, Name name, Any value)
{ if ( restoreVersion < 10 )
  { if ( name == NAME_start )
    { f->start = valInt(value);
      succeed;
    } else if ( name == NAME_length )
    { f->length = valInt(value);
      succeed;
    }
  }

  fail;
}


		/********************************
		*        LINK/UNLINK/PLACE      *
		*********************************/

/*  Unlink the fragment from the double linked chain in of fragments of the
    textbuffer.
*/

static int
unlink_fragment(Fragment f)
{ Fragment next = f->next;

  if ( notNil(f->next) )
  { assign(f->next, prev, f->prev);
    assign(f, next, NIL);
  } else
    assign(f->textbuffer, last_fragment, f->prev);

  if ( notNil(f->prev) )
  { assign(f->prev, next, next);
    assign(f, prev, NIL);
  } else
    assign(f->textbuffer, first_fragment, next);

  succeed;
}


/*  Link the fragment in the double linked fragment chain of the
    textbuffer. Note that this chain should always be kept sorted to the
    start index of the fragment.
*/

static int
link_fragment(Fragment f)
{ Fragment b;
  TextBuffer tb = f->textbuffer;

  if ( notNil(b = tb->first_fragment) )
  { if ( f->start >= tb->last_fragment->start )
    { b = tb->last_fragment;		/* at the end  */
      assign(b, next, f);
      assign(f, prev, b);
      assign(tb, last_fragment, f);

      succeed;
    }

    if ( f->start < b->start )
    { assign(f, next, b);		/* at the start */
      assign(b, prev, f);
      assign(tb, first_fragment, f);
      
      succeed;
    }

    for( ; notNil(b->next); b = b->next)
    { if ( b->next->start < f->start )
	continue;

      assign(f, next, b->next);		/* somewere in the middle */
      assign(f, prev, b);
      assign(b, next, f);
      assign(f->next, prev, f);

      succeed;
    }

    assign(b, next, f);			/* at the end (should not happen) */
    assign(f, prev, b);
    assign(tb, last_fragment, f);

    succeed;
  }
  
  assign(tb, first_fragment, f);	/* the only one */
  assign(tb, last_fragment, f);

  succeed;
}


static status
relink_fragment(Fragment f)
{ if ( (notNil(f->prev) && f->prev->start > f->start) ||
       (notNil(f->next) && f->next->start < f->start) )
  { addCodeReference(f);		/* should not drop out! */
    unlink_fragment(f);
    link_fragment(f);
    ChangedFragmentListTextBuffer(f->textbuffer);
    delCodeReference(f);
  }

  succeed;
}

		/********************************
		*         NORMALISATION         *
		*********************************/

static status
normaliseFragment(Fragment f)
{ TextBuffer tb = f->textbuffer;

  f->start = normalize_tb(tb, f->start);
  f->length = normalize_tb(tb, f->start + f->length) - f->start;

  succeed;
}

		/********************************
		*             METHODS           *
		*********************************/

static status
startFragment(Fragment f, Int start)
{ if ( valInt(start) != f->start )
  { Int oldstart = toInt(f->start);

    f->start = valInt(start);
    normaliseFragment(f);
    relink_fragment(f);
    ChangedRegionTextBuffer(f->textbuffer, oldstart, toInt(f->start+f->length));
  }

  succeed;
}


static status
lengthFragment(Fragment f, Int length)
{ if ( valInt(length) != f->length )
  { int oldl = f->length;

    f->length = valInt(length);
    normaliseFragment(f);
    ChangedRegionTextBuffer(f->textbuffer,
			    toInt(f->start + oldl),
			    toInt(f->start + f->length));
  }

  succeed;
}


static status
endFragment(Fragment f, Int end)
{ Int len = toInt(valInt(end) - f->start);

  return lengthFragment(f, len);
}


static Int
getStartFragment(Fragment f)
{ answer(toInt(f->start));
}


static Int
getLengthFragment(Fragment f)
{ answer(toInt(f->length));
}


static Int
getEndFragment(Fragment f)
{ answer(toInt(f->start + f->length));
}


static Fragment
getNextFragment(Fragment f, Code cond)
{ Fragment n = f->next;

  if ( notDefault(cond) )
  { while(notNil(n) && !forwardCodev(cond, 1, (Any *)&n))
      n = n->next;
  }

  if ( notNil(n) )
    answer(n);
  fail;
}


static Fragment
getPreviousFragment(Fragment f, Code cond)
{ Fragment n = f->prev;

  if ( notDefault(cond) )
  { while(notNil(n) && !forwardCodev(cond, 1, (Any *)&n))
      n = n->prev;
  }

  if ( notNil(n) )
    answer(n);
  fail;
}


static status
styleFragment(Fragment f, Name s)
{ if ( s != f->style )
  { assign(f, style, s);

    ChangedRegionTextBuffer(f->textbuffer,
			    toInt(f->start),
			    toInt(f->start + f->length));
  }
  
  succeed;
}


static status
overlapFragment(Fragment f, Any obj)
{
#define OVERLAPS(f, i) ( (i) >= f->start && (i) < f->start + f->length )

  if ( instanceOfObject(obj, ClassFragment) )
  { Fragment f2 = obj;
    
    if ( max(f->start, f2->start) <
	 min(f->start + f->length, f2->start + f2->length) )
      succeed;
  } else if ( instanceOfObject(obj, ClassPoint) )
  { Point p = obj;
    int x = valInt(p->x);
    int y = valInt(p->y);
	
    if ( max(f->start, x) < min(f->start + f->length, y) )
      succeed;
  } else /*if ( isInteger(obj) )*/
  { if ( OVERLAPS(f, valInt(obj)) )	  
      succeed;
  }

  fail;
}



static StringObj
getStringFragment(Fragment f)
{ int len = f->length;
  int i = f->start;
  TextBuffer tb = f->textbuffer;
  string s;

  str_sub_text_buffer(tb, &s, i, len);
  answer(StringToString(&s));
}


static StringObj
getSubFragment(Fragment f, Int start, Int end)
{ int x = valInt(start), y;
  TextBuffer tb = f->textbuffer;
  int i, len;
  string s;

  y = (isDefault(end) ? f->length : valInt(end));
  if ( x < 0 || y > f->length || x > y )
    fail;

  i = f->start + x;
  len = y - x;
  str_sub_text_buffer(tb, &s, i, len);
  answer(StringToString(&s));
}


static status
stringFragment(Fragment f, CharArray ca)
{ TextBuffer tb = f->textbuffer;
  Int start = toInt(f->start);

  deleteTextBuffer(tb, start, toInt(f->length));
  insertTextBuffer(tb, start, ca, ONE);
  startFragment(f, start);		/* TBD */
  lengthFragment(f, getSizeCharArray(ca));

  succeed;
}


static status
insertFragment(Fragment f, Int idx, CharArray txt)
{ int where = (isDefault(idx) ? f->length : valInt(idx));
  int l = f->length;

  if ( where < 0 )
    where = 0;
  else if ( where > l )
    where = l;

  insertTextBuffer(f->textbuffer, toInt(f->start + where), txt, ONE);
  f->length = l + valInt(getSizeCharArray(txt));

  succeed;
}


static status
deleteFragment(Fragment f, Int from, Int len)
{ int s = valInt(from);
  int size = f->length;
  int e = (isDefault(len) ? size : valInt(len)) + s - 1;
  int d;

  if ( s <  0    ) s = 0;
  if ( s >= size ) succeed;
  if ( e <  s    ) succeed;
  if ( e >= size )
    e = size - 1;
  d = e - s + 1;

  deleteTextBuffer(f->textbuffer, toInt(s + f->start), toInt(d));
  f->length = size - d;

  succeed;
}


status
makeClassFragment(Class class)
{ sourceClass(class, makeClassFragment, __FILE__, "$Revision$");

  localClass(class, NAME_textBuffer, NAME_whole, "text_buffer", NAME_get,
	     "Text_buffer I'm a range of");
  localClass(class, NAME_next, NAME_list, "fragment*", NAME_none,
	     "Next in double-linked chain");
  localClass(class, NAME_previous, NAME_list, "fragment*", NAME_none,
	     "Previous in double-linked chain");
  localClass(class, NAME_style, NAME_appearance, "[name]", NAME_get,
	     "Visual feedback: name of style in editor");
  localClass(class, NAME_start, NAME_dimension, "alien:long", NAME_none,
	     "Start index (0-based)");
  localClass(class, NAME_length, NAME_dimension, "alien:long", NAME_none,
	     "Length in characters");

  termClass(class, "fragment", 4, NAME_textBuffer, NAME_start,
			          NAME_length, NAME_style);
  setLoadStoreFunctionClass(class, loadFragment, storeFragment);

  storeMethod(class, NAME_style,  styleFragment);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "text=text_buffer", "start=int", "length=int", "style=[name]",
	     "Create from text_buffer, start, length, style",
	     initialiseFragment);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink from the text_buffer",
	     unlinkFragment);
  sendMethod(class, NAME_overlap, NAME_compare, 1, "int|fragment|point",
	     "Test if overlap with argument",
	     overlapFragment);
  sendMethod(class, NAME_start, NAME_dimension, 1, "int",
	     "Start index (0-based)",
	     startFragment);
  sendMethod(class, NAME_length, NAME_dimension, 1, "int",
	     "Length in characters",
	     lengthFragment);
  sendMethod(class, NAME_end, NAME_dimension, 1, "int",
	     "End (changes <-length)",
	     endFragment);
  sendMethod(class, NAME_string, NAME_contents, 1, "char_array",
	     "Replace text by argument",
	     stringFragment);
  sendMethod(class, NAME_insert, NAME_contents, 2, "[int]", "char_array",
	     "Insert text at location [append]",
	     insertFragment);
  sendMethod(class, NAME_delete, NAME_contents, 2, "from=int", "length=[int]",
	     "Delete range of characters",
	     deleteFragment);
  sendMethod(class, NAME_convertOldSlot, NAME_compatibility, 2,
	     "name", "any",
	     "Convert start and length slots",
	     convertOldSlotFragment);
  sendMethod(class, NAME_emptied, NAME_virtual, 0,
	     "Called if text is killed/deleted",
	     succeedObject);

  getMethod(class, NAME_string, NAME_contents, "string", 0,
	    "New string with contents",
	    getStringFragment);
  getMethod(class, NAME_sub, NAME_contents, "string", 2,
	    "start=int", "end=[int]",
	    "New string with contents in range",
	    getSubFragment);
  getMethod(class, NAME_start, NAME_dimension, "int", 0,
	    "Start index (0-based)",
	    getStartFragment);
  getMethod(class, NAME_length, NAME_dimension, "int", 0,
	    "Length in characters",
	    getLengthFragment);
  getMethod(class, NAME_end, NAME_dimension, "int", 0,
	    "End (<-start + <-length) of fragment",
	    getEndFragment);
  getMethod(class, NAME_next, NAME_list, "fragment", 1, "condition=[code]",
	    "Next in list for which condition is true",
	    getNextFragment);
  getMethod(class, NAME_previous, NAME_list, "fragment", 1, "condition=[code]",
	    "Previous in list for which condition is true",
	    getPreviousFragment);

  succeed;
}

