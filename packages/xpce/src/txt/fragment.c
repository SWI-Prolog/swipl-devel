/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/text.h>
#include <h/unix.h>

#define O_STATISTICS 0			/* statistics functions */

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

  f->start      = valInt(s);
  f->length     = valInt(l);
  f->attributes = 0L;

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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Link a fragment in the double-linked fragment list.  Fragments are ordered
to their start-index to speedup repaint management.  If the start index is
equal, the largest fragment is first to ensure optimal `nesting'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_STATISTICS
static int mis;

static status
dumpMisFragment(Fragment f)
{ Cprintf("mis = %d\n", mis);

  succeed;
}

#endif

static int
link_fragment(Fragment f)
{ Fragment b;
  TextBuffer tb = f->textbuffer;

  if ( notNil(b = tb->first_fragment) )
  { if ( f->start > tb->last_fragment->start )
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

    if ( (tb->last_fragment->start - f->start) < (f->start - b->start) )
    { b = tb->last_fragment;

      for( ; notNil(b); b = b->prev )
      { if ( b->start > f->start ||
	     (b->start == f->start && b->length < f->length) )
	{
#if O_STATISTICS
	  mis++;
#endif
	  continue;			/* f must be before b */
	}
	
	assign(f, next, b->next);
	assign(f, prev, b);
	if ( notNil(b->next) )
	{ assign(b, next, f);
	  assign(f->next, prev, f);
	} else
	{ assign(tb, last_fragment, f);
	  assign(b, next, f);
	}

	succeed;
      }

      b = tb->first_fragment;
      assign(f, next, b);		/* at the start */
      assign(b, prev, f);
      assign(tb, first_fragment, f);
    } else
    { for( ; notNil(b->next); b = b->next)
      { if ( b->next->start < f->start ||
	     (b->next->start == f->start && b->next->length > f->length) )
	{
#if O_STATISTICS
	  mis++;
#endif
	  continue;
	}

	assign(f, next, b->next);	/* somewere in the middle */
	assign(f, prev, b);
	assign(b, next, f);
	assign(f->next, prev, f);

	succeed;
      }

      assign(b, next, f);		/* at the end (should not happen) */
      assign(f, prev, b);
      assign(tb, last_fragment, f);
    }


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

		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static status
includeFragment(Fragment f, Name what, Bool val)
{ long mask;

  if ( what == NAME_start )
    mask = FRAG_INCLUDES_START;
  else if ( what == NAME_end )
    mask = FRAG_INCLUDES_END;
  else
    mask = FRAG_INCLUDES_START|FRAG_INCLUDES_END;

  if ( val == OFF )
    f->attributes &= ~mask;
  else
    f->attributes |= mask;

  succeed;
}


static status
doesIncludeFragment(Fragment f, Name what)
{ int rval;

  if ( what == NAME_start )
    rval = (f->attributes & FRAG_INCLUDES_START);
  else
    rval = (f->attributes & FRAG_INCLUDES_END);

  return rval ? SUCCEED : FAIL;
}


		/********************************
		*             METHODS           *
		*********************************/

#if 0					/* better? */
static status
setFragment(Fragment f, Int start, Int length)
{ if ( valInt(start) != f->start || valInt(length) != f->length )
  { int os = f->start;
    int ol = f->length;

    f->start  = valInt(start);
    f->length = valInt(length);
    normaliseFragment(f);
    ChangedRegionTextBuffer(f->textbuffer,
			    min(os, f->start),
			    max(os+ol, f->start + f->length));
  }

  succeed;
}
#endif

static status
startFragment(Fragment f, Int start, Bool moveend)
{ if ( valInt(start) != f->start )
  { int oldstart = f->start;
    int chend;

    f->start = valInt(start);

    if ( moveend == OFF )
    { f->length -= f->start - oldstart;
      chend = f->start;
    } else
      chend = f->start + f->length;

    normaliseFragment(f);
    relink_fragment(f);
    ChangedRegionTextBuffer(f->textbuffer, toInt(oldstart), toInt(chend));
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

  if ( isInteger(obj) )
  { if ( OVERLAPS(f, valInt(obj)) )	  
      succeed;
  } else if ( instanceOfObject(obj, ClassFragment) )
  { Fragment f2 = obj;
    
    if ( max(f->start, f2->start) <
	 min(f->start + f->length, f2->start + f2->length) )
      succeed;
  } else /* if ( instanceOfObject(obj, ClassPoint) ) */
  { Point p = obj;
    int x = valInt(p->x);
    int y = valInt(p->y);
	
    if ( max(f->start, x) < min(f->start + f->length, y) )
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
  int start = f->start;
  int len = f->length;
  int calen = ca->data.size;

  insertTextBuffer(tb, toInt(start), ca, ONE);
  startFragment(f, toInt(start), OFF);		/* TBD */
  lengthFragment(f, toInt(calen));
  deleteTextBuffer(tb, toInt(start + calen), toInt(len));

  succeed;
}


static status
insertFragment(Fragment f, Int idx, CharArray txt)
{ int where = (isDefault(idx) ? f->length : valInt(idx));
  int l = f->length;
  int start = f->start;

  if ( where < 0 )
    where = 0;
  else if ( where > l )
    where = l;

  insertTextBuffer(f->textbuffer, toInt(start + where), txt, ONE);
  f->start = start;			/* moves otherwise! */
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

		 /*******************************
		 *	       VISUAL		*
		 *******************************/

static Any
getContainedInFragment(Fragment f)
{ TextBuffer tb = f->textbuffer;

  if ( tb && notNil(tb) &&
       notNil(tb->editors) &&
       !emptyChain(tb->editors) )
    answer(getHeadChain(tb->editors));

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_insert[] =
        { "[int]", "char_array" };
static const char *T_delete[] =
        { "from=int", "length=[int]" };
static const char *T_start[] =
        { "int", "move_end=[bool]" };
static const char *T_convertOldSlot[] =
        { "name", "any" };
static const char *T_sub[] =
        { "start=int", "end=[int]" };
static const char *T_initialise[] =
        { "text=text_buffer", "start=int", "length=int", "style=[name]" };
static const char *T_include[] =
        { "what=[{start,end,both}]", "include=[bool]" };

/* Instance Variables */

static const vardecl var_fragment[] =
{ IV(NAME_textBuffer, "text_buffer", IV_GET,
     NAME_whole, "Text_buffer I'm a range of"),
  IV(NAME_next, "fragment*", IV_NONE,
     NAME_list, "Next in double-linked chain"),
  IV(NAME_previous, "fragment*", IV_NONE,
     NAME_list, "Previous in double-linked chain"),
  SV(NAME_style, "[name]", IV_GET|IV_STORE, styleFragment,
     NAME_appearance, "Visual feedback: name of style in editor"),
  IV(NAME_start, "alien:long", IV_NONE,
     NAME_dimension, "Start index (0-based)"),
  IV(NAME_length, "alien:long", IV_NONE,
     NAME_dimension, "Length in characters"),
  IV(NAME_attributes, "alien:long", IV_NONE,
     NAME_internal, "Various packed attributes")
};

/* Send Methods */

static const senddecl send_fragment[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseFragment,
     DEFAULT, "Create from text_buffer, start, length, style"),
  SM(NAME_unlink, 0, NULL, unlinkFragment,
     DEFAULT, "Unlink from the text_buffer"),
  SM(NAME_overlap, 1, "int|fragment|point", overlapFragment,
     NAME_compare, "Test if overlap with argument"),
  SM(NAME_convertOldSlot, 2, T_convertOldSlot, convertOldSlotFragment,
     NAME_compatibility, "Convert start and length slots"),
  SM(NAME_delete, 2, T_delete, deleteFragment,
     NAME_contents, "Delete range of characters"),
  SM(NAME_insert, 2, T_insert, insertFragment,
     NAME_contents, "Insert text at location [append]"),
  SM(NAME_string, 1, "char_array", stringFragment,
     NAME_contents, "Replace text by argument"),
  SM(NAME_end, 1, "int", endFragment,
     NAME_dimension, "End (changes <-length)"),
  SM(NAME_length, 1, "int", lengthFragment,
     NAME_dimension, "Length in characters"),
  SM(NAME_start, 2, T_start, startFragment,
     NAME_dimension, "Start index (0-based)"),
#if O_STATISTICS
  SM(NAME_dumpMap, 0, NULL, dumpMisFragment,
     NAME_internal, "Debugging: dump `mis' info"),
#endif
  SM(NAME_doesInclude, 1, "what={start,end}", doesIncludeFragment,
     NAME_update, "Test whether start or end is included"),
  SM(NAME_include, 2, T_include, includeFragment,
     NAME_update, "Define whether start and end are included"),
  SM(NAME_emptied, 0, NULL, succeedObject,
     NAME_virtual, "Called if text is killed/deleted")
};

/* Get Methods */

static const getdecl get_fragment[] =
{ GM(NAME_containedIn, 0, "editor", NULL, getContainedInFragment,
     DEFAULT, "editor object I'm contained in"),
  GM(NAME_string, 0, "string", NULL, getStringFragment,
     NAME_contents, "New string with contents"),
  GM(NAME_sub, 2, "string", T_sub, getSubFragment,
     NAME_contents, "New string with contents in range"),
  GM(NAME_end, 0, "int", NULL, getEndFragment,
     NAME_dimension, "End (<-start + <-length) of fragment"),
  GM(NAME_length, 0, "int", NULL, getLengthFragment,
     NAME_dimension, "Length in characters"),
  GM(NAME_start, 0, "int", NULL, getStartFragment,
     NAME_dimension, "Start index (0-based)"),
  GM(NAME_next, 1, "fragment", "condition=[code]", getNextFragment,
     NAME_list, "Next in list for which condition is true"),
  GM(NAME_previous, 1, "fragment", "condition=[code]", getPreviousFragment,
     NAME_list, "Previous in list for which condition is true")
};

/* Resources */

static const resourcedecl rc_fragment[] =
{ 
};

/* Class Declaration */

static Name fragment_termnames[] = { NAME_textBuffer, NAME_start, NAME_length, NAME_style };

ClassDecl(fragment_decls,
          var_fragment, send_fragment, get_fragment, rc_fragment,
          4, fragment_termnames,
          "$Rev$");

status
makeClassFragment(Class class)
{ declareClass(class, &fragment_decls);
  setLoadStoreFunctionClass(class, loadFragment, storeFragment);

  succeed;
}

