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
#include <h/graphics.h>
#include <h/text.h>
#include <h/unix.h>

static Int		getMarginWidthEditor(Editor);
static Int		getColumnEditor(Editor, Int);
static Int		getLineNumberEditor(Editor, Int);
static Int		getLengthEditor(Editor);
static Int		normalise_index(Editor, Int);
static FragmentCache	newFragmentCache(Editor);
static void		freeFragmentCache(FragmentCache);
static void		resetFragmentCache(FragmentCache, TextBuffer);
static status		CaretEditor(Editor, Int);
static status		caretEditor(Editor, Int);
static status		IsearchEditor(Editor, EventId);
static status		DabbrevExpandEditor(Editor, EventId);
static status		centerWindowEditor(Editor, Int);
static status		columnEditor(Editor, Int);
static status		ChangedRegionEditor(Editor, Int, Int);
static status		ChangedEditor(Editor);
static status		appendKill(CharArray);
static status		prependKill(CharArray);
static status		geometryEditor(Editor, Int, Int, Int, Int);
static status		ensureVisibleEditor(Editor, Int, Int);
static status		ensureCaretInWindowEditor(Editor);
static status		endIsearchEditor(Editor);
static status		updateStyleCursorEditor(Editor);
static status		selectedFragmentEditor(Editor, Fragment);
static status		showMatchingBracketEditor(Editor, Int);
static status		insertSelfFillEditor(Editor, Int, Int);
static status		scrollDownEditor(Editor, Int);
static status		selectionOriginEditor(Editor, Int);
static status		selectionExtendEditor(Editor, Int);
static status		selection_editor(Editor, Int, Int, Name);
static status		selectionToCutBufferEditor(Editor, Int);
static status		insertCutBufferEditor(Editor, Int);
static status		insertEditor(Editor e, CharArray str);
static status		lineNumberEditor(Editor, Int);
static status		saveEditor(Editor, SourceSink);
static status		newKill(CharArray);
static CharArray	killRegister(Int);
static status		tabDistanceEditor(Editor e, Int tab);
static status		isisearchingEditor(Editor e);
static status		showLabelEditor(Editor e, Bool val);
static Int		countLinesEditor(Editor e, Int from, Int to);
static status		deleteEditor(Editor e, Int from, Int to);
static status		deleteSelectionEditor(Editor e);
static status		abortIsearchEditor(Editor e);
static status		scrollUpEditor(Editor e, Int arg);
static Int		getColumnLocationEditor(Editor e, Int c, Int from);

static Timer	ElectricTimer;

#define Caret(e)	valInt(e->caret)
#define Receiver(e)	ReceiverOfEditor(e)
#define Round(n, r)	((((n) + ((r)-1)) / (r)) * (r))
#define Before(f, t)	{ if ( valInt(f) > valInt(t) ) \
			  { Int _tmp = t; t = f; f = _tmp; \
			  } \
			}
#define HasSelection(e) ((e)->mark != (e)->caret && \
			 (e)->mark_status == NAME_active)
#define Fetch(e, i)		fetch_textbuffer((e)->text_buffer, (i))
#define InRegion(i, l, h)	( (l < h && i >= l && i < h) || \
				  (l > h && i >= h && i < l) )

/* Scroll using line-parameters upto this size for the buffer */
#define MAXPRECISESCROLLING   10000
#define MAXLINEBASEDSCROLLING 25000


		/********************************
		*            CREATE		*
		********************************/

static status
initialiseEditor(Editor e, TextBuffer tb, Int w, Int h, Int tmw)
{ Int fw, fh, iw, ih, ew;
  Size sz = getClassVariableValueObject(e, NAME_size);

  if ( isDefault(tb) ) tb = newObject(ClassTextBuffer, EAV);
  if ( isDefault(tmw)) tmw = ZERO;

  assign(e, size, newObject(ClassSize, sz->w, sz->h, EAV));
  if ( notDefault(w) ) assign(e->size, w, w);
  if ( notDefault(h) ) assign(e->size, h, h);

  initialiseDevice((Device) e);		/* also obtains class-variables! */
/*assign(e, pen, getClassVariableValueObject(e, NAME_pen));*/
  assign(e, text_buffer, tb);
/*assign(e, font, getClassVariableValueObject(e, NAME_font));*/
  fw = getExFont(e->font);
  fh = getHeightFont(e->font);
  iw = toInt(valInt(e->size->w) * valInt(fw) + 2 * TXT_X_MARGIN);
  ih = toInt(valInt(e->size->h) * valInt(fh) + 2 * TXT_Y_MARGIN);

  assign(e, image, newObject(ClassTextImage, e, iw, ih, EAV));
  assign(e, scroll_bar, newObject(ClassScrollBar, e, NAME_vertical, EAV));

  if ( valInt(tmw) > 0 )
    assign(e, margin, newObject(ClassTextMargin, e, tmw, ih, EAV));
  else
    assign(e, margin, NIL);
  assign(e, text_cursor, newObject(ClassTextCursor, e->font, EAV));
  send(e->text_cursor, NAME_active, OFF, EAV);
  assign(e, caret, ZERO);
  assign(e, mark, toInt(tb->size));
  assign(e, mark_status, NAME_inactive);
  assign(e, mark_ring, newObject(ClassVector, EAV));
  fillVector(e->mark_ring, NIL, ONE, toInt(16));
  assign(e, selected_fragment, NIL);
  assign(e, selected_fragment_style, newObject(ClassStyle, EAV));
  boldStyle(e->selected_fragment_style, ON);
  assign(e, bindings, newObject(ClassKeyBinding, NIL, NAME_editor, EAV));
  assign(e, focus_function, NIL);
/*assign(e, fill_mode, getClassVariableValueObject(e, NAME_fillMode));
  assign(e, exact_case, getClassVariableValueObject(e, NAME_exactCase));*/
  assign(e, kill_location, NIL);
  assign(e, search_direction, NAME_forward);
  assign(e, search_string, NIL);
  assign(e, search_origin, ZERO);
  assign(e, search_base, ZERO);
  assign(e, selection_origin, ZERO);
  assign(e, selection_unit, NAME_character);
/*assign(e, selection_style, getClassVariableValueObject(e, NAME_selectionStyle));*/
  assign(e, editable, ON);
  assign(e, error_message, NIL);
  assign(e, left_margin, ZERO);
/*assign(e, right_margin, getClassVariableValueObject(e, NAME_rightMargin));
  assign(e, indent_increment, getClassVariableValueObject(e, NAME_indentIncrement)); */
  assign(e, auto_newline, OFF);
  assign(e, file, NIL);
  assign(e, dabbrev_target, NIL);
  assign(e, dabbrev_reject, NIL);
  assign(e, dabbrev_pos, NIL);
  assign(e, dabbrev_origin, NIL);
  assign(e, styles, newObject(ClassSheet, EAV));

  e->fragment_cache = newFragmentCache(e);

  send(e->image, NAME_cursor, getClassVariableValueObject(e, NAME_cursor), EAV);
  send(e->image, NAME_set, e->scroll_bar->area->w, ZERO, EAV);
/*tabDistanceEditor(e, getClassVariableValueObject(e, NAME_tabDistance));*/
  tabDistanceTextImage(e->image, mul(e->tab_distance, getExFont(e->font)));
  heightGraphical((Graphical) e->scroll_bar, ih);
  displayDevice(e, e->scroll_bar, DEFAULT);
  displayDevice(e, e->image, DEFAULT);
  displayDevice(e, e->text_cursor, DEFAULT);
  ew = add(e->scroll_bar->area->w,e->image->area->w);

  if ( notNil(e->margin) )
  { send(e->margin, NAME_set, ew, EAV);
    ew = add(ew, e->margin->area->w);
    displayDevice(e, e->margin, DEFAULT);
  }

  updateStyleCursorEditor(e);		/* also does position */
  send(tb, NAME_attach, e, EAV);
  geometryEditor(e, ZERO, ZERO, ew, ih);
  
  succeed;
}


static Editor
getConvertEditor(Any ctx, View v)
{ answer(v->editor);
}


static status
unlinkEditor(Editor e)
{ View view = Receiver(e);

  if ( ElectricTimer && ((Message)(ElectricTimer->message))->receiver == e )
  { stopTimer(ElectricTimer);
    assign((Message)ElectricTimer->message, receiver, NIL);
  }
   
  if ( notNil(e->text_buffer) )
  { send(e->text_buffer, NAME_detach, e, EAV);
    assign(e, text_buffer, NIL);
  }
  if ( e->fragment_cache != NULL )
  { freeFragmentCache(e->fragment_cache);
    e->fragment_cache = NULL;
  }

  unlinkDevice((Device) e);

  freeObject(e->image);			/* make sure */
  freeObject(e->scroll_bar);
  freeObject(e->text_cursor);

  if ( instanceOfObject(view, ClassView) && !isFreedObj(view) )
    send(view, NAME_free, EAV);

  succeed;
}


static status
lostTextBufferEditor(Editor e)
{ if ( !onFlag(e, F_FREED|F_FREEING) )
    send(Receiver(e), NAME_free, EAV);

  succeed;
}


		 /*******************************
		 *		REDRAW		*
		 *******************************/

static status
RedrawAreaEditor(Editor e, Area a)
{ Any obg = r_background(getClassVariableValueObject(e, NAME_background));

  RedrawAreaDevice((Device)e, a);
  if ( e->pen != ZERO )
  { int p = valInt(e->pen);
    int x, y, w, h;
    int th = valInt(e->image->area->y);

    initialiseDeviceGraphical(e, &x, &y, &w, &h);
    y += th;
    h -= th;

					/* test for overlap with border */
    if ( valInt(a->x) < p || valInt(a->y) < p ||
	 valInt(a->x) + valInt(a->w) > w - p ||
	 valInt(a->y) + valInt(a->h) > h - p )
    { r_thickness(p);
      r_dash(e->texture);

      r_box(x, y, w, h, 0, NIL);
    }
  }

  r_background(obg);

  succeed;
}

		 /*******************************
		 *	  CLONE/SAVE/LOAD	*
		 *******************************/

static status
storeEditor(Editor e, FileObj file)
{ return storeSlotsObject(e, file);
}


static status
loadFdEditor(Editor e, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(e, fd, def));

  e->fragment_cache = newFragmentCache(e);
  e->internal_mark = 0;

  succeed;
}


static status
cloneEditor(Editor e, Editor clone)
{ clonePceSlots(e, clone);
  
  e->fragment_cache = newFragmentCache(e);

  succeed;
}


		 /*******************************
		 *	    TEXT-BUFFER		*
		 *******************************/

static status
textBufferEditor(Editor e, TextBuffer tb)
{ if ( e->text_buffer != tb )
  { TextImage ti = e->image;

    selectedFragmentEditor(e, NIL);
    send(e->text_buffer, NAME_detach, e, EAV);

    assign(e, text_buffer, tb);
    assign(e, caret, ZERO);
    assign(e, mark, toInt(tb->size));
    assign(e, mark_status, NAME_inactive);
    if ( e->fragment_cache )
      resetFragmentCache(e->fragment_cache, e->text_buffer);

    send(tb, NAME_attach, e, EAV);

    ChangedEntireTextImage(ti);
    requestComputeGraphical(e, DEFAULT);
  }
  
  succeed;
}


		/********************************
		*            CURSOR		*
		********************************/

static status
showCaretAtEditor(Editor e, Int caret)
{ int x, y, w, h, b;
  int displaced = notDefault(caret);

  caret = normalise_index(e, isDefault(caret) ? e->caret : caret);
  if ( get_character_box_textimage(e->image, valInt(caret),
				   &x, &y, &w, &h, &b) )
  { x += valInt(e->image->area->x);
    y += valInt(e->image->area->y);
    w = valInt(getExFont(e->font));

    setTextCursor(e->text_cursor,
		  toInt(x), toInt(y), toInt(w), toInt(h), toInt(b));
    if ( displaced )
      requestComputeGraphical(e, NAME_showCaretAt);

    succeed;
  }

  fail;
}


static status
updateCursorEditor(Editor e)
{ return showCaretAtEditor(e, DEFAULT);
}


static status
electricCaretEditor(Editor e, Int caret, Real time)
{ TRY( showCaretAtEditor(e, caret) );

  if ( !ElectricTimer )
  { if ( isDefault(time) )
      time = CtoReal(0.5);

    ElectricTimer = globalObject(NAME_electricTimer, ClassTimer, time,
				 newObject(ClassMessage, e,
					   NAME_showCaretAt, EAV),
				 EAV);
  } else
  { assign((Message)ElectricTimer->message, receiver, e);
    if ( notDefault(time) )
      intervalTimer(ElectricTimer, time);
  }

  return startTimer(ElectricTimer, NAME_once);
}


static status
updateStyleCursorEditor(Editor e)
{ send(e->text_cursor, NAME_font, e->font, EAV);

  return updateCursorEditor(e);
}


static status
showCaretEditor(Editor e, Bool show)
{ DisplayedGraphical(e->text_cursor, show);

  succeed;
}


		/********************************
		*          SCROLLBAR		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scrollbar updates. This can all be  optimised  further, both by scanning
the text only once, avoiding fetch() while scanning and finally and most
importantly, by caching some of these values,  such as the start-line of
the view and the number of lines in the buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
bubbleScrollBarEditor(Editor e, ScrollBar sb)
{ TextBuffer tb = e->text_buffer;
  Int start = getStartTextImage(e->image, ONE);

  if ( tb->size < MAXPRECISESCROLLING )
  { return bubbleScrollBarTextImage(e->image, sb);
  } else if ( tb->size < MAXLINEBASEDSCROLLING ) /* short, work line-based */
  { Int len   = countLinesEditor(e, ZERO, toInt(tb->size));
    Int first = sub(getLineNumberEditor(e, start), ONE); /* 1-based! */
    Int view  = countLinesEditor(e, start, e->image->end);
    
    if ( tb->size > 0 &&
	 !tisendsline(tb->syntax, Fetch(e, tb->size-1)) )
      incrInt(len);			/* incomplete last line */
    if ( valInt(e->image->end) > 0 &&
	 !tisendsline(tb->syntax, Fetch(e, valInt(e->image->end)-1)) )
      incrInt(view);

    return bubbleScrollBar(sb, len, first, view);
  } else				/* long, work character-based */
  { Int len  = toInt(tb->size);
    Int view = getViewTextImage(e->image);

    return bubbleScrollBar(sb, len, start, view);
  }
}


static Int
getStartEditor(Editor e, Int line)
{ answer(getStartTextImage(e->image, line));
}


static Int
getViewEditor(Editor e)
{ answer(getViewTextImage(e->image));
}


static Int
getLengthEditor(Editor e)
{ answer(toInt(e->text_buffer->size));
}

		/********************************
		*            LABEL		*
		********************************/

static status
labelEditor(Editor e, Name lbl)
{ showLabelEditor(e, ON);

  send(e->label_text, NAME_string, lbl, EAV);
  geometryEditor(e, DEFAULT, DEFAULT, DEFAULT, DEFAULT);

  succeed;
}


static status
showLabelEditor(Editor e, Bool val)
{ if ( isNil(e->label_text) )
  { if ( val == ON )
    { assign(e, label_text,
	     newObject(ClassText, GetLabelNameName(e->name), NAME_left,
		       getClassVariableValueObject(e, NAME_labelFont), EAV));
      marginText(e->label_text, e->area->w, NAME_clip);
      displayDevice(e, e->label_text, DEFAULT);
      return geometryEditor(e, DEFAULT, DEFAULT, DEFAULT, DEFAULT);
    } else
      succeed;
  }

  if ( e->label_text->displayed != val )
  { DisplayedGraphical(e->label_text, val);
    return geometryEditor(e, DEFAULT, DEFAULT, DEFAULT, DEFAULT);
  }

  succeed;
}


static Bool
getShowLabelEditor(Editor e)
{ if ( notNil(e->label_text) )
    answer(e->label_text->displayed);

  answer(OFF);
}


		/********************************
		*             MARGIN		*
		********************************/

static status
marginWidthEditor(Editor e, Int width)
{ if ( getMarginWidthEditor(e) != width )
  { if ( isNil(e->margin) )
    { assign(e, margin, newObject(ClassTextMargin, e, width, e->area->h, EAV));
      displayDevice(e, e->margin, DEFAULT);
    } else
      setGraphical(e->margin, DEFAULT, DEFAULT, width, DEFAULT);
          
    geometryEditor(e, DEFAULT, DEFAULT, DEFAULT, DEFAULT);
  }

  succeed;
}


static Int
getMarginWidthEditor(Editor e)
{ if ( notNil(e->margin) )
    answer(e->margin->area->w);

  answer(ZERO);
}


static status
selectedFragmentEditor(Editor e, Fragment fr)
{ if ( e->selected_fragment != fr )
  { if ( notNil(e->selected_fragment) )
    { Fragment f = e->selected_fragment;

      ChangedRegionEditor(e, toInt(f->start), toInt(f->start + f->length));
    }
    assign(e, selected_fragment, fr);
    if ( notNil(fr) )
      ChangedRegionEditor(e, toInt(fr->start), toInt(fr->start + fr->length));
  }
  
  succeed;
}


static status
selectedFragmentStyleEditor(Editor e, Style style)
{ if ( e->selected_fragment_style != style )
  { assign(e, selected_fragment_style, style);
    if ( notNil(e->selected_fragment) )
    { Fragment f = e->selected_fragment;

      ChangedRegionEditor(e, toInt(f->start), toInt(f->start + f->length));
    }
  }

  succeed;
}


		/********************************
		*            GEOMETRY		*
		********************************/

static status
geometryEditor(Editor e, Int x, Int y, Int w, Int h)
{ int ix, iy, iw, ih, mx, mw, sw;
  int pen = valInt(e->pen);
  Area a = e->area;
  Any sbobj = e->image;
  int fh = valInt(getHeightFont(e->font));
  
  if ( e->badBoundingBox == ON && (isDefault(w) || isDefault(h)) )
  { Cell cell;
    clearArea(a);

				/* simplyfied computeBoundingBoxDevice() */
				/* we should consider non-displayed grs */
				/* too ... See label and scrollbar hiding */
    for_cell(cell, e->graphicals)
    { Graphical gr = cell->value;

      unionNormalisedArea(a, gr->area);
    }

    relativeMoveArea(a, e->offset);
    assign(e, badBoundingBox, OFF);
  }

  if ( isDefault(x) ) x = a->x;
  if ( isDefault(y) ) y = a->y;
  if ( isDefault(w) ) w = a->w;
  if ( isDefault(h) ) h = a->h;

  if ( valInt(w) < 50 ) w = toInt(50);

  DEBUG(NAME_editor, Cprintf("geometryEditor(%s, %d, %d, %d, %d)\n",
			     pp(e),
			     valInt(x), valInt(y),
			     valInt(w), valInt(h)));

  if ( notNil(e->label_text) && e->label_text->displayed == ON )
  { marginText(e->label_text, w, NAME_clip);
    ComputeGraphical(e->label_text);
    send(e->label_text, NAME_set, ZERO, ZERO, DEFAULT, DEFAULT, EAV);
    iy = valInt(e->label_text->area->h);
  } else
  { iy = 0;
  }

					/* make sure at least line fits! */
  if ( valInt(h) - iy - fh - 2*TXT_Y_MARGIN < 0 )
    h = toInt(iy+fh+2*TXT_Y_MARGIN);
  ih = valInt(h);

  sw = isNil(e->scroll_bar) ? 0 : valInt(getMarginScrollBar(e->scroll_bar));
  mw = notNil(e->margin) ? valInt(e->margin->area->w) : 0;
  iw = valInt(w) - abs(sw) - mw;
  
  DEBUG(NAME_editor, Cprintf("sw = %d, mw = %d, iw = %d\n",
			     sw, mw, iw));

  assign(e->size, w, div(toInt(iw), getExFont(e->font)));
  assign(e->size, h, div(h,  getHeightFont(e->font)));

  ix = (sw < 0 ? -sw : 0);
  mx = ix + iw - pen;

  if ( notNil(e->margin) )
  { if ( getClassVariableValueObject(e->margin, NAME_placement) == NAME_left )
    { mx = ix;
      ix += mw;
    } else
      sbobj = e->margin;
  }

  send(e->image, NAME_set, toInt(ix), toInt(iy), toInt(iw), toInt(ih-iy), EAV);
  if ( notNil(e->margin) )
    send(e->margin, NAME_set, toInt(mx), toInt(iy), DEFAULT, toInt(ih-iy), EAV);
  if ( notNil(e->scroll_bar) )
    placeScrollBar(e->scroll_bar, sbobj);

  return geometryDevice((Device) e, x, y, DEFAULT, DEFAULT);
}


static status
requestGeometryEditor(Editor e, Int x, Int y, Int w, Int h)
{ Any v;

  if ( notDefault(w) )
    w = mul(w, getExFont(e->font));
  else if ( notNil(e->request_compute) )
    w = mul(e->size->w, getExFont(e->font));

  if ( notDefault(h) )
    h = mul(h, getHeightFont(e->font));
  else if ( notNil(e->request_compute) )
    h = mul(e->size->h, getHeightFont(e->font));

  if ( instanceOfObject(v = Receiver(e), ClassWindow) )
    requestGeometryWindow(v, x, y, w, h);
  else
    requestGeometryGraphical(e, x, y, w, h);

  succeed;
}


static status
SizeEditor(Editor e, Size size)
{ return doSetGraphical(e, DEFAULT, DEFAULT, size->w, size->h);
}


static Size
getSizeEditor(Editor e)
{ answer(e->size);
}


static Int
getWidthEditor(Editor e)
{ answer(e->size->w);
}


static Int
getHeightEditor(Editor e)
{ answer(e->size->h);
}


		/********************************
		*            STYLES		*
		********************************/

static status
styleEditor(Editor e, Name name, Style style)
{ if ( isNil(style) )
    deleteSheet(e->styles, name);
  else
    valueSheet(e->styles, name, style);

  ChangedEditor(e);

  succeed;    
}

static status
stylesEditor(Editor e, Sheet styles)
{ assign(e, styles, styles);
  ChangedEditor(e);

  succeed;    
}


		/********************************
		*            FETCH		*
		********************************/

typedef struct fragment_cell  *FragmentCell;

struct fragment_cell
{ Fragment	fragment;		/* Fragment in the cell */
  Style		style;			/* Style that belongs to it */
  FragmentCell	next;			/* next in chain */
};


struct fragment_cache
{ FragmentCell	active;			/* list of active fragments */
  Fragment	current;		/* current fragment */
  long		index;			/* current index */
  unsigned long		attributes;		/* Current (fragment) attributes */
  FontObj	font;			/* current (fragment) font */
  Colour	colour;			/* current (fragment) colour */
  Any		background;		/* curremt (fragment) background */
  int		left_margin;		/* current left margin */
  int		right_margin;		/* current right margin */
  int		initial_state;		/* state after reset */
};


static FragmentCache
newFragmentCache(Editor e)
{ FragmentCache fc = alloc(sizeof(struct fragment_cache));

  fc->active = NULL;
  fc->initial_state = FALSE;
  resetFragmentCache(fc, e->text_buffer);

  return fc;
}


static void
freeFragmentCache(FragmentCache fc)
{ resetFragmentCache(fc, NIL);

  unalloc(sizeof(struct fragment_cache), fc);
}


static void
resetFragmentCache(FragmentCache fc, TextBuffer tb)
{ if ( !fc->initial_state )
  { FragmentCell c, c2;

    for(c=fc->active; c; c = c2)
    { c2 = c->next;
      unalloc(sizeof(struct fragment_cell), c);
    }

    fc->active        = NULL;
    fc->index         = -1;
    fc->attributes    = 0;
    fc->font	      = DEFAULT;
    fc->colour	      = DEFAULT;
    fc->background    = DEFAULT;
    fc->left_margin   = 0;
    fc->right_margin  = 0;
    fc->initial_state = TRUE;
  }

  fc->current         = (isNil(tb) ? NIL : tb->first_fragment);
}


static void
indexFragmentCache(FragmentCache fc, Editor e, long int i)
{ int changed = 0;
  FragmentCell *C, c;
  Fragment fr;

  if ( i < fc->index )			/* No incremental index when back */
    resetFragmentCache(fc, e->text_buffer);

					/* Delete those we passed */
  for(C = &fc->active; (c = *C); )
  { if ( i >= c->fragment->start + c->fragment->length )
    { *C = c->next;
      DEBUG(NAME_fragment, Cprintf("Passed %s fragment (%ld, %ld)\n",
				   pp(c->fragment->style),
				   c->fragment->start, c->fragment->length));
      unalloc(sizeof(struct fragment_cell), c);
      changed++;
    } else
      C = &c->next;
  }
					/* Add new ones entered */
  while( notNil(fr = fc->current) && i >= fr->start )
  { Style s;

    if ( i < fr->start + fr->length &&
	 (s = getValueSheet(e->styles, fr->style)) )
    { FragmentCell c = alloc(sizeof(struct fragment_cell));

      DEBUG(NAME_fragment,
	    Cprintf("Enter %s fragment (%ld, %ld) (style = %s)\n",
		    pp(fr->style),
		    fr->start, fr->length,
		    pp(s)));
      c->fragment = fr;
      c->style    = s;
      c->next     = fc->active;
      fc->active  = c;

      changed++;
    }

    fc->current = fr->next;
  }

  if ( changed )
  { FragmentCell cell;
    FontObj f = DEFAULT;
    Any bg    = DEFAULT;
    Colour c  = DEFAULT;
    long fl   = 0;			/* keep compiler happy */
    long bgl  = 0;
    long cl   = 0;
    int lm    = 0;
    int rm    = 0;			/* margins */
    unsigned long attributes = 0L;

    for( cell = fc->active; cell; cell = cell->next )
    { Style s = cell->style;

      lm += valInt(s->left_margin);
      rm += valInt(s->right_margin);

      if ( s->attributes & TXT_HIDDEN )
      { Fragment fr = cell->fragment;
	
	indexFragmentCache(fc, e, fr->start + fr->length);
	return;
      }

      attributes |= s->attributes;

      if ( notDefault(s->font) )
      { if ( isDefault(f) || cell->fragment->length < fl )
	{ f = s->font;
	  fl = cell->fragment->length;
	}
      }
      if ( notDefault(s->colour) )
      { if ( isDefault(c) || cell->fragment->length < cl )
	{ c = s->colour;
	  cl = cell->fragment->length;
	}
      }
      if ( notDefault(s->background) )
      { if ( isDefault(bg) || cell->fragment->length < bgl )
	{ bg = s->background;
	  bgl = cell->fragment->length;
	}
      }
    }

    fc->font	     = f;
    fc->colour       = c;
    fc->background   = bg;
    fc->attributes   = attributes;
    fc->right_margin = rm;
    fc->left_margin  = lm;

    DEBUG(NAME_fragment, Cprintf("---> Font: %s; attributes: 0x%lx\n",
				 pp(f), attributes));
  }

  fc->initial_state = FALSE;
  fc->index = i;
}


static void
seek_editor(Any obj, long int index)
{ Editor e = obj;

  indexFragmentCache(e->fragment_cache, e, index);
}


static long
scan_editor(Any obj, long int index, int dir, int how, int category, int *eof)
{ Editor e = obj;
  TextBuffer tb = e->text_buffer;
  SyntaxTable s = tb->syntax;
  int size = tb->size;

  *eof = FALSE;

  if ( how == TEXT_SCAN_FOR )
  { if ( dir > 0 )
    { for(; index < size; index++)
      { if ( tischtype(s, fetch_textbuffer(tb, index), category) )
	  goto out;
      }
      goto out_eof;
    } else				/* dir < 0 */
    { for(; index >= 0; index--)
      { if ( tischtype(s, fetch_textbuffer(tb, index), category) )
	  goto out;
      }
      goto out_eof;
    }
  } else				/* TEXT_SKIP_OVER */
  { if ( dir > 0 )
    { for(; index < size; index++)
      { if ( !tischtype(s, fetch_textbuffer(tb, index), category) )
	  goto out;
      }
      goto out_eof;
    } else				/* dir < 0 */
    { for(; index >= 0; index--)
      { if ( !tischtype(s, fetch_textbuffer(tb, index), category) )
	  goto out;
      }
      goto out_eof;
    }
  }
	  
out_eof:
  *eof = TRUE;
out:
  if ( index < 0 )
    index = 0;
  else if ( index > size ) 
    index = size;

  return index;
}


#define GRAPHICS_START 01		/* ^A */

static long
fetch_editor(Any obj, TextChar tc)
{ Editor e = obj;
  FragmentCache fc = e->fragment_cache;
  long index = fc->index;

  tc->value.c      = Fetch(e, index);
  tc->type	   = CHAR_ASCII;
  tc->font         = fc->font;
  tc->colour       = fc->colour;
  tc->background   = fc->background;
  tc->attributes   = fc->attributes;
  tc->index	   = index;

  if ( tc->value.c == GRAPHICS_START &&
       Fetch(e, index+2) == GRAPHICS_START &&
       hasGetMethodObject(e, NAME_diagram) )
  { int grindex = Fetch(e, index+1);
    Graphical gr = get(e, NAME_diagram, toInt(grindex), EAV);

    if ( gr )
    { tc->value.graphical = gr;
      tc->type	         = CHAR_GRAPHICAL;
    
      indexFragmentCache(e->fragment_cache, e, index+3);
      return fc->index;
    }
  }

  if ( e->mark_status != NAME_inactive &&
       InRegion(index, valInt(e->mark), valInt(e->caret)) )
  { Style s = (isisearchingEditor(e)
	       ? getClassVariableValueObject(e, NAME_isearchStyle)
	       : e->selection_style);

    if ( !s || isDefault(s) )
    { tc->attributes ^= TXT_HIGHLIGHTED;
    } else
    { tc->attributes |= s->attributes;
      if ( notDefault(s->font) )
	tc->font = s->font;
      if ( notDefault(s->colour) )
	tc->colour = s->colour;
      if ( notDefault(s->background) )
	tc->background = s->background;
    }
  }

  if ( notNil(e->selected_fragment) )
  { Fragment fr = e->selected_fragment;
    Style s = e->selected_fragment_style;

    if ( index >= fr->start && index < fr->start + fr->length )
    { tc->attributes |= s->attributes;
      if ( notDefault(s->font) )
	tc->font = s->font;
      if ( notDefault(s->colour) )
	tc->colour = s->colour;
      if ( notDefault(s->background) )
	tc->background = s->background;
    }
  }

  if ( isDefault(tc->font) )
    tc->font = e->font;

  indexFragmentCache(e->fragment_cache, e, ++index);

  return fc->index;
}


static void
margin_editor(Any obj, int *left, int *right)
{ Editor e = obj;
  FragmentCache fc = e->fragment_cache;

  *left  = fc->left_margin;
  *right = fc->right_margin;
}


static SeekFunction
getSeekFunctionEditor(Editor e)
{ answer(seek_editor);
}


static ScanFunction
getScanFunctionEditor(Editor e)
{ answer(scan_editor);
}


static FetchFunction
getFetchFunctionEditor(Editor e)
{ answer(fetch_editor);
}


static MarginFunction
getMarginFunctionEditor(Editor e)
{ answer(margin_editor);
}


static Int
getFetchEditor(Editor e, Int where)
{ answer(toInt(Fetch(e, valInt(where))));
}


static RewindFunction
getRewindFunctionEditor(Editor e)
{ answer((RewindFunction) NULL);
}




		/********************************
		*            REDRAW		*
		********************************/

static status
computeEditor(Editor e)
{ if ( notNil(e->request_compute) )
  { computeTextImage(e->image);
    ensureVisibleEditor(e, DEFAULT, DEFAULT);
    if ( e->request_compute != NAME_showCaretAt )
      updateCursorEditor(e);
    if ( notNil(e->margin) )
      changedEntireImageGraphical(e->margin);

    computeDevice(e);
  }

  succeed;
}


		/********************************
		*            WINDOW		*
		********************************/

static Int
normalise_index(Editor e, Int index)
{ if ( valInt(index) < 0 )
    return ZERO;
  if ( valInt(index) > e->text_buffer->size )
    return toInt(e->text_buffer->size);

  return index;
}


static Name
where_editor(Editor e, Int index)
{ int i = valInt(index);

  if ( i < valInt(getStartTextImage(e->image, ONE)) )
    return NAME_above;			/* above window */

  ComputeGraphical(e->image);
  if ( i < valInt(e->image->end) )
    return NAME_inside;			/* In the window */

  if ( i == e->text_buffer->size &&	/* standing on EOF that is in window */
       e->image->eof_in_window == ON )
    return NAME_inside;
  
  return NAME_below;
}


static status
ensureVisibleEditor(Editor e, Int from, Int to)
{ TextImage ti = e->image;

  from = (isDefault(from) ? e->caret : normalise_index(e, from));
  to   = (isDefault(to) ? from : normalise_index(e, to));

  if ( !(from == to && ensureVisibleTextImage(ti, from)) )
  { Before(from, to);

    if ( where_editor(e, to) == NAME_below )
    { DEBUG(NAME_scroll, Cprintf("Caret below window\n"));
      startTextImage(ti, getScanTextBuffer(e->text_buffer,
					   getStartTextImage(ti,ONE),
					   NAME_line, ONE,
					   NAME_start),
		     ZERO);
  
      if ( where_editor(e, to) == NAME_below )
      { DEBUG(NAME_scroll, Cprintf("More than one line: centering\n"));
	centerWindowEditor(e, to);
	ComputeGraphical(ti);
      }
    } else if ( valInt(to) < valInt(getStartTextImage(ti, ONE)) )
    { startTextImage(ti, getScanTextBuffer(e->text_buffer,
					   getStartTextImage(ti,ONE),
					   NAME_line, toInt(-1),
					   NAME_start),
		     ZERO);
      ComputeGraphical(ti);
      if ( valInt(to) < valInt(getStartTextImage(ti, ONE)) )
      { centerWindowEditor(e, to);
	ComputeGraphical(ti);
      }
    }
  
    if ( valInt(from) < valInt(getStartTextImage(ti, ONE)) )
    { while( valInt(from) < valInt(getStartTextImage(ti, ONE)) )
      { startTextImage(ti, getScanTextBuffer(e->text_buffer,
					     getStartTextImage(ti,ONE),
					     NAME_line, toInt(-1),
					     NAME_start),
		       ZERO);
	ComputeGraphical(ti);
      }
    }
  }

  ensureCaretInWindowEditor(e);		/* play save */

  succeed;
}


status
normaliseEditor(Editor e, Int start, Int end)
{ return ensureVisibleEditor(e, start, end); /* TBD: delete */
}


static status
ensureCaretInWindowEditor(Editor e)
{ Int start;

  ComputeGraphical(e->image);

  if ( valInt(e->caret) < valInt(start = getStartTextImage(e->image, ONE)) )
    CaretEditor(e, start);
  else
  { if ( valInt(e->caret) >= valInt(e->image->end) )
    { if ( e->image->eof_in_window == ON )
	CaretEditor(e, e->image->end);
      else
      { long ie = max(0, valInt(e->image->end) - 1);
	
	CaretEditor(e, toInt(ie));
      }
    }
  }

  return requestComputeGraphical(e->scroll_bar, DEFAULT);
}


static Int
getFirstEditor(Editor e)
{ ComputeGraphical(e->image);

  answer(getLineNumberEditor(e, getStartTextImage(e->image, ONE)));
}


static Int
countLinesEditor(Editor e, Int from, Int to)
{ answer(toInt(count_lines_textbuffer(e->text_buffer,
				      valInt(from), valInt(to))));
}


static Point
getLinesVisibleEditor(Editor e)
{ Int first = getFirstEditor(e);
  Int last;

  last = add(countLinesEditor(e, getStartTextImage(e->image, ONE),
			      e->image->end), first);

  answer(answerObject(ClassPoint, first, sub(last, ONE), EAV));
}

		/********************************
		*            FEEDBACK		*
		********************************/

Any
ReceiverOfEditor(Editor e)
{ if ( isObject(e->device) && instanceOfObject(e->device, ClassView) )
    return e->device;

  return e;
}


static status
reportEditor(Editor e, Name kind, CharArray fm, int argc, Any *argv)
{ if ( notNil(e->error_message) )
  { char msg[FORMATSIZE];
    StringObj str;

    if ( isDefault(fm) )
      fm = (CharArray) (kind == NAME_done ? NAME_done : CtoName(""));

    swritefv(msg, NULL, fm, argc, argv);
    str = CtoTempString(msg);

    forwardReceiverCode(e->error_message, Receiver(e),
			e, kind, str, EAV);
    considerPreserveObject(str);

    succeed;
  }

  return reportVisual((VisualObj)e, kind, fm, argc, argv);
}


status
forwardModifiedEditor(Editor e, Bool val)
{ abortIsearchEditor(e);

  if ( notNil(e->modified_message) )
    forwardReceiverCode(e->modified_message, Receiver(e), val, EAV);

  succeed;
}


		/********************************
		*        FUNCTION MAPPING	*
		********************************/


static Any
getKeyBindingEditor(Editor e, Name key)
{ return getFunctionKeyBinding(e->bindings, key);
}


static status
keyBindingEditor(Editor e, Name key, Any function)
{ return functionKeyBinding(e->bindings, key, function);
}


		/********************************
		*            TYPING		*
		********************************/

static status
typedEditor(Editor e, EventId id)
{ if ( notNil(e->focus_function) )
  { if ( send(e, e->focus_function, id, EAV) )
      succeed;
    else
      assign(e, focus_function, NIL);
  }

  return typedKeyBinding(e->bindings, id, Receiver(e));
}


static status
event_editor(Editor e, EventObj ev)
{ if ( isAEvent(ev, NAME_focus) )
  { if ( isAEvent(ev, NAME_activateKeyboardFocus) )
      send(e->text_cursor, NAME_active, ON, EAV);
    else if ( isAEvent(ev, NAME_deactivateKeyboardFocus) )
      send(e->text_cursor, NAME_active, OFF, EAV);

    succeed;
  }

  if ( eventDevice(e, ev) )
    succeed;

  if ( isAEvent(ev, NAME_keyboard) )
    return send(e, NAME_typed, ev, EAV);

  					/* delete mode on button down */
  if ( isDownEvent(ev) )
  { PceWindow sw = getWindowGraphical((Graphical)e);

    if ( sw && notNil(sw) && sw->keyboard_focus != (Graphical)e )
      send(e, NAME_keyboardFocus, ON, EAV);

    endIsearchEditor(e);
    assign(e, focus_function, NIL);
  }

  if ( mapWheelMouseEvent(ev, e) )
    succeed;
    
					/* @editor_recogniser is a hook */
					/* to allow for host-language */
					/* level redefinition */

  { Any recogniser = getObjectFromReferencePce(PCE, NAME_editorRecogniser);

    if ( recogniser && instanceOfObject(recogniser, ClassRecogniser) )
      return send(recogniser, NAME_event, ev, EAV);
  }
					/* Built-in version */

  if ( isAEvent(ev, NAME_button) )
  { Int where = getIndexTextImage(e->image, ev);
    Modifier select_modifier = getClassVariableValueObject(e, NAME_selectModifier);
    Modifier caret_modifier = getClassVariableValueObject(e, NAME_caretModifier);

    if ( !where )
      fail;

/*  endIsearchEditor(e); */

    if ( isDownEvent(ev) )
    { status rval = FAIL;

      focusGraphical((Graphical) e, DEFAULT, DEFAULT, DEFAULT);

      if ( isAEvent(ev, NAME_msLeftDown) )
      { if ( hasModifierEvent(ev, select_modifier) )
	{ if ( getMulticlickEvent(ev) == NAME_double )
	    assign(e, selection_unit, NAME_word);
	  else if ( getMulticlickEvent(ev) == NAME_triple )
	    assign(e, selection_unit, NAME_line);
	  else
	    assign(e, selection_unit, NAME_character);

	  rval = selectionOriginEditor(e, where);
	}

	if ( hasModifierEvent(ev, caret_modifier) &&
	     getMulticlickEvent(ev) == NAME_single )
	  rval = CaretEditor(e, where);

	return rval;
      }

      if ( isAEvent(ev, NAME_msRightDown) &&
	   hasModifierEvent(ev, select_modifier) )
	return selectionExtendEditor(e, where);
    } else
    { if ( isAEvent(ev, NAME_msMiddleUp) &&
	   hasModifierEvent(ev, select_modifier) )
      { insertCutBufferEditor(e, DEFAULT);
	succeed;
      } else if ( hasModifierEvent(ev, select_modifier) )
      { selectionExtendEditor(e, where);
	selectionToCutBufferEditor(e, DEFAULT);
      } 
    }

    if ( hasModifierEvent(ev, select_modifier) &&
	 (isAEvent(ev, NAME_msLeftDrag) || isAEvent(ev, NAME_msRightDrag)) )
      return selectionExtendEditor(e, where);
  }

  fail;
}


static status
eventEditor(Editor e, EventObj ev)
{ status rval = event_editor(e, ev);
  
  if ( rval && !isFreedObj(e) )
  { if ( isAEvent(ev, NAME_keyboard) ||
	 (isAEvent(ev, NAME_button) /*&& isDownEvent(ev)*/) )
      markUndoTextBuffer(e->text_buffer);

    if ( notNil(e->text_buffer) && notNil(e->request_compute) )
    { assign(e, caret, normalise_index(e, e->caret));
      ensureVisibleEditor(e, e->caret, e->caret);
    }
  }

  return rval;
}


		/********************************
		*         EDIT FUNCTIONS	*
		********************************/

#define UArg(arg)	  (isDefault(arg) ? 1 : valInt(arg))
#define MustBeEditable(e) TRY( verify_editable_editor(e) )


static status
verify_editable_editor(Editor e)
{ if ( e->editable == OFF )
  { send(e, NAME_report, NAME_warning, CtoName("Text is read-only"), EAV);
    fail;
  }

  succeed;
}


static status
insert_editor(Editor e, Int times, Int chr, int fill)
{ wchar c;
  TextBuffer tb = e->text_buffer;
  LocalString(s, &tb->buffer, 1);

  MustBeEditable(e);
  if ( HasSelection(e) &&
       getClassVariableValueObject(e, NAME_insertDeletesSelection) == ON )
    deleteSelectionEditor(e);

  if ( fill && e->fill_mode == ON )
    return insertSelfFillEditor(e, times, chr);

  if ( isDefault(times) )
    times = ONE;

  if ( isDefault(chr) )
  { EventObj ev = EVENT->value;

    if ( instanceOfObject(ev, ClassEvent) && isAEvent(ev, NAME_printable) )
      c = valInt(getIdEvent(ev));
    else
      return errorPce(e, NAME_noCharacter);
  } else
    c = valInt(chr);
    
  str_store(s, 0, c);
  s->size = 1;
  insert_textbuffer(e->text_buffer, Caret(e), valInt(times), s);

  if ( tisclosebrace(e->text_buffer->syntax, c) &&
       getClassVariableValueObject(e, NAME_showOpenBracket) == ON )
    showMatchingBracketEditor(e, sub(e->caret, ONE));

  succeed;
}


static status
insertSelfEditor(Editor e, Int times, Int chr)
{ return insert_editor(e, times, chr, TRUE);
}


static status
insertQuotedEditor(Editor e, Int times, Int chr)
{ return insert_editor(e, times, chr, FALSE);
}


static status
showMatchingBracketEditor(Editor e, Int arg)
{ Int here = (isDefault(arg) ? e->caret : arg);
  Int there_pos, here_bracket, there_bracket;
  TextBuffer tb = e->text_buffer;

  if ( !tischtype(tb->syntax, valInt(getFetchEditor(e, here)), OB|CB) )
  { here = sub(here, ONE);
    if ( !tisclosebrace(tb->syntax, valInt(getFetchEditor(e, here))) )
      fail;
  }

  here_bracket = getFetchEditor(e, here);
  if ( (there_pos = getMatchingBracketTextBuffer(tb, here, DEFAULT)) &&
       (there_bracket = getFetchEditor(e, there_pos)) &&
       tismatching(tb->syntax, valInt(there_bracket), valInt(here_bracket)) )
  { if ( !electricCaretEditor(e, there_pos, DEFAULT) )
    { Int sol = getScanTextBuffer(e->text_buffer, there_pos, NAME_line,
				  ZERO, NAME_start);
      Int eol = getScanTextBuffer(e->text_buffer, sol, NAME_line,
				  ZERO, NAME_end);
      Int len = toInt(valInt(eol) - valInt(sol));
      StringObj line = getContentsTextBuffer(e->text_buffer, sol, len);
      send(e, NAME_report, NAME_status, CtoName("Matches %s"), line, EAV);
    }
  } else
    return errorPce(e, NAME_noMatchingBracket);

  succeed;
}


static status
newlineEditor(Editor e, Int arg)
{ MustBeEditable(e);
  return insert_textbuffer(e->text_buffer, Caret(e), UArg(arg),
			   str_nl(&e->text_buffer->buffer));
}


static status
openLineEditor(Editor e, Int arg)
{ Int caret = e->caret;

  MustBeEditable(e);
  insert_textbuffer(e->text_buffer, Caret(e), UArg(arg),
		    str_nl(&e->text_buffer->buffer));
  return CaretEditor(e, caret);		/* do not move the caret */
}


static status
caretEditor(Editor e, Int c)
{ if ( isDefault(c) )
    c = toInt(e->text_buffer->size);

  selection_editor(e, DEFAULT, c, DEFAULT);
  return requestComputeGraphical(e, DEFAULT);
}


static status
CaretEditor(Editor e, Int c)
{ if ( e->caret != c )
    return qadSendv(e, NAME_caret, 1, (Any *)&c);

  succeed;
}


static status
markEditor(Editor e, Int mark, Name status)
{ if ( isDefault(mark) )
    mark = e->caret;

					/* TBD: push in mark-ring */
  selection_editor(e, mark, DEFAULT, status);
  return requestComputeGraphical(e, DEFAULT);
}


static status
markStatusEditor(Editor e, Name status)
{ if ( e->mark_status != status )
    selection_editor(e, DEFAULT, DEFAULT, status);

  succeed;
}


static status
forwardCharEditor(Editor e, Int arg)
{ return CaretEditor(e, toInt(Caret(e) + UArg(arg)));
}


static status
backwardCharEditor(Editor e, Int arg)
{ return CaretEditor(e, toInt(Caret(e) - UArg(arg)));
}


static status
forwardWordEditor(Editor e, Int arg)
{ return CaretEditor(e,
		     getScanTextBuffer(e->text_buffer,
				       e->caret, NAME_word, toInt(UArg(arg)-1),
				       NAME_end));
}


static status
backwardWordEditor(Editor e, Int arg)
{ backwardCharEditor(e, ONE);
  return CaretEditor(e,
		     getScanTextBuffer(e->text_buffer,
				       e->caret, NAME_word, toInt(1-UArg(arg)),
				       NAME_start));
}


static status
beginningOfLineEditor(Editor e, Int arg)
{ Int caret;

  if ( e->image->wrap == NAME_word && 
       isDefault(arg) &&
       (caret = getBeginningOfLineCursorTextImage(e->image, e->caret)) )
  {
  } else
  { caret = getScanTextBuffer(e->text_buffer,
			      e->caret, NAME_line, toInt(1-UArg(arg)),
			      NAME_start);
  }

  return CaretEditor(e, caret);
}


static status
endOfLineEditor(Editor e, Int arg)
{ Int caret;

  if ( e->image->wrap == NAME_word && 
       isDefault(arg) &&
       (caret = getEndOfLineCursorTextImage(e->image, e->caret)) )
  {
  } else
    caret = getScanTextBuffer(e->text_buffer,
			      e->caret, NAME_line, toInt(UArg(arg)-1),
			      NAME_end);

  return CaretEditor(e, caret);
}


static status
forwardSentenceEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  e->caret,
					  NAME_sentence,
					  toInt(UArg(arg)-1),
					  NAME_end));
}


static status
backwardSentenceEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  e->caret,
					  NAME_sentence,
					  toInt(1-UArg(arg)),
					  NAME_start));
}


static status
forwardParagraphEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  e->caret,
					  NAME_paragraph,
					  toInt(UArg(arg)-1),
					  NAME_end));
}


static status
backwardParagraphEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  e->caret,
					  NAME_paragraph,
					  toInt(1-UArg(arg)),
					  NAME_start));
}


static status
forwardTermEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  e->caret,
					  NAME_term,
					  toInt(UArg(arg)),
					  NAME_end));
}


static status
backwardTermEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  e->caret,
					  NAME_term,
					  toInt(-UArg(arg)),
					  NAME_start));
}


static status
skipBlanksEditor(Editor e, Int arg)
{ TextBuffer tb  = e->text_buffer;
  Name direction = (UArg(arg) >= 0 ? NAME_forward : NAME_backward);
  Bool skipnl    = (UArg(arg) >= 4 || UArg(arg) <= -4 ? ON : OFF);

  return CaretEditor(e, getSkipBlanksTextBuffer(tb, e->caret,
						direction, skipnl));
}


static status
pointToTopOfFileEditor(Editor e, Int arg)
{ return lineNumberEditor(e, toInt(UArg(arg)));
}


static status
pointToBottomOfFileEditor(Editor e, Int arg)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  toInt(e->text_buffer->size),
					  NAME_line, toInt(1-UArg(arg)),
					  NAME_end));
}


static status
pointToTopOfWindowEditor(Editor e, Int arg)
{ return CaretEditor(e, getStartTextImage(e->image, arg));
}


static status
pointToBottomOfWindowEditor(Editor e, Int arg)
{ if ( isDefault(arg) )
    arg = ONE;

  return CaretEditor(e, getStartTextImage(e->image, neg(arg)));
}


static status
nextLineEditor(Editor e, Int arg, Int column)
{ Int caret;
  TextBuffer tb = e->text_buffer;
  int n = UArg(arg);

  if ( isDefault(column) )
    column = getColumnEditor(e, e->caret);

  caret = getScanTextBuffer(tb, e->caret, NAME_line, toInt(n), NAME_start);

  if ( valInt(caret) ==	tb->size &&
       ( caret == e->caret || Fetch(e, tb->size-1) != '\n' ) &&
       n == 1 &&
       e->editable == ON )
  { endOfLineEditor(e, DEFAULT);

    return send(e, NAME_newline, ONE, EAV);
  }

  return CaretEditor(e, getColumnLocationEditor(e, column, caret));
}


static status
previousLineEditor(Editor e, Int arg, Int column)
{ return nextLineEditor(e, toInt(-UArg(arg)), column);
}


static status
deleteCharEditor(Editor e, Int arg)
{ MustBeEditable(e);

  return delete_textbuffer(e->text_buffer, Caret(e), UArg(arg));
}


static status
backwardDeleteCharEditor(Editor e, Int arg)
{ MustBeEditable(e);
  return delete_textbuffer(e->text_buffer, Caret(e), -UArg(arg));
}


static status
cutOrDeleteCharEditor(Editor e, Int arg)
{ MustBeEditable(e);

  if ( isDefault(arg) && HasSelection(e) )
    return send(e, NAME_cut, EAV);
  else
    return send(e, NAME_deleteChar, arg, EAV);
}


static status
cutOrBackwardDeleteCharEditor(Editor e, Int arg)
{ MustBeEditable(e);

  if ( isDefault(arg) && HasSelection(e) )
    return send(e, NAME_cut, EAV);
  else
    return send(e, NAME_backwardDeleteChar, arg, EAV);
}


static status
copyEditor(Editor e)
{ StringObj s = getSelectedEditor(e);
  DisplayObj d = getDisplayGraphical((Graphical)e);

  if ( s && d )
    return send(d, NAME_copy, s, EAV);

  fail;
}


static status
cutEditor(Editor e)
{ MustBeEditable(e);

  if ( send(e, NAME_copy, EAV) )
    return deleteSelectionEditor(e);

  fail;
}


static status
pasteEditor(Editor e)
{ DisplayObj d = getDisplayGraphical((Graphical)e);
  CharArray selection;

  if ( d && (selection=get(d, NAME_paste, EAV)) )
  { if ( HasSelection(e) &&
	 getClassVariableValueObject(e, NAME_insertDeletesSelection) == ON )
      deleteSelectionEditor(e);

    return insertEditor(e, selection);
  }

  fail;
}

		 /*******************************
		 *	    CURSOR KEYS		*
		 *******************************/

static int
buttons()
{ if ( instanceOfObject(EVENT->value, ClassEvent) )
  { EventObj ev = EVENT->value;
    
    return valInt(ev->buttons);
  }

  return 0;
}


static status
caretMoveExtendSelectionEditor(Editor e, Int oldcaret)
{ if ( e->mark_status != NAME_active )
  { assign(e, selection_unit, NAME_character);
    assign(e, selection_origin, oldcaret);
  }

  selectionExtendEditor(e, e->caret);
  if ( getClassVariableValueObject(e, NAME_autoCopy) == ON )
    copyEditor(e);

  succeed;
}


static Int
getUpDownColumnEditor(Editor e)
{ if ( e->image->wrap == NAME_word )
    return getUpDownColumnTextImage(e->image, e->caret);
  else
    return getColumnEditor(e, e->caret);
}


static status
cursorUpEditor(Editor e, Int arg, Int column)
{ int bts = buttons();
  Int caret = e->caret;

  if ( isDefault(arg) )
    arg = ONE;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  if ( bts & BUTTON_control )
    backwardParagraphEditor(e, arg);
  else if ( e->image->wrap == NAME_word &&
	    (caret = getUpDownCursorTextImage(e->image, caret,
					      neg(arg), column)) )
    return CaretEditor(e, caret);
  else if ( e->text_cursor->displayed == OFF && !isisearchingEditor(e) )
    return scrollDownEditor(e, ONE);
  else
    previousLineEditor(e, arg, column);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorDownEditor(Editor e, Int arg, Int column)
{ int bts = buttons();
  Int caret = e->caret;

  if ( isDefault(arg) )
    arg = ONE;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  if ( bts & BUTTON_control )
    forwardParagraphEditor(e, arg);
  else if ( e->image->wrap == NAME_word &&
	    (caret = getUpDownCursorTextImage(e->image, caret, arg, column)) )
    return CaretEditor(e, caret);
  else if ( e->text_cursor->displayed == OFF && !isisearchingEditor(e) )
    return scrollUpEditor(e, ONE);
  else
    nextLineEditor(e, arg, column);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorLeftEditor(Editor e, Int arg)
{ int bts = buttons();
  Int caret = e->caret;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  if ( bts & BUTTON_control )
    backwardWordEditor(e, arg);
  else
    backwardCharEditor(e, arg);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorRightEditor(Editor e, Int arg)
{ int bts = buttons();
  Int caret = e->caret;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  if ( bts & BUTTON_control )
    forwardWordEditor(e, arg);
  else
    forwardCharEditor(e, arg);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorEndEditor(Editor e, Int arg)
{ int bts = buttons();
  Int caret = e->caret;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  if ( bts & BUTTON_control )
    pointToBottomOfFileEditor(e, arg);
  else
    endOfLineEditor(e, arg);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorHomeEditor(Editor e, Int arg)
{ int bts = buttons();
  Int caret = e->caret;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  if ( bts & BUTTON_control )
    pointToTopOfFileEditor(e, arg);
  else
    beginningOfLineEditor(e, arg);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorPageUpEditor(Editor e, Int arg)
{ int bts = buttons();
  Int caret = e->caret;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  scrollDownEditor(e, arg);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}


static status
cursorPageDownEditor(Editor e, Int arg)
{ int bts = buttons();
  Int caret = e->caret;

  if ( !(bts & BUTTON_shift) )
    markStatusEditor(e, NAME_inactive);

  scrollUpEditor(e, arg);

  if ( bts & BUTTON_shift )
    caretMoveExtendSelectionEditor(e, caret);

  succeed;
}

		/********************************
		*        KILLING/YANKING	*
		********************************/

static status
yankEditor(Editor e, Int times)
{ CharArray s = killRegister(ZERO);

  times = toInt(abs(UArg(times)));
  MustBeEditable(e);

  if ( s )
  { Int mark = e->caret;		/* otherwise moves at the insert */

    insertTextBuffer(e->text_buffer, e->caret, s, times);
    assign(e, mark, mark);

    succeed;
  }
  
  fail;
}


static status
killEditor(Editor e, Int from, Int to)
{ Int length;
  CharArray text;

  MustBeEditable(e);

  Before(from, to);
  length = sub(to, from);
  text = (CharArray)getContentsTextBuffer(e->text_buffer, from, length);

  if ( from == e->kill_location )
    appendKill(text);
  else if ( to == e->kill_location )
    prependKill(text);
  else
    newKill(text);

  deleteTextBuffer(e->text_buffer, from, length);
  assign(e, kill_location, from);
    
  succeed;
}


static status
grabEditor(Editor e, Int from, Int to)
{ Int length;

  Before(from, to);
  length = sub(to, from);

  newKill((CharArray) getContentsTextBuffer(e->text_buffer, from, length));
  send(e, NAME_report, NAME_status, CtoName("Grabbed"), EAV);
  assign(e, kill_location, NIL);

  succeed;
}


static status
killWordEditor(Editor e, Int arg)
{ Int end = getScanTextBuffer(e->text_buffer, e->caret,
			      NAME_word, toInt(UArg(arg)-1), NAME_end);

  MustBeEditable(e);
  return killEditor(e, e->caret, end);
}


static status
backwardKillWordEditor(Editor e, Int arg)
{ Int start = getScanTextBuffer(e->text_buffer, sub(e->caret, ONE),
				NAME_word, toInt(1-UArg(arg)), NAME_start);

  MustBeEditable(e);
  return killEditor(e, start, e->caret);
}


static status
killLineEditor(Editor e, Int arg)
{ Int end;
  Int lines;

  MustBeEditable(e);
  if ( notDefault(arg) )
    lines = arg;
  else
  { if ( tisendsline(e->text_buffer->syntax, Fetch(e, valInt(e->caret))) )
      return killEditor(e, e->caret, add(e->caret, ONE));

    if ( e->image->wrap == NAME_word &&
	 (end = getEndOfLineCursorTextImage(e->image, e->caret)) )
    { int i = valInt(end);
      TextBuffer tb = e->text_buffer;

      while(i<tb->size && Fetch(e, i) == ' ')
	i++;
      return killEditor(e, e->caret, toInt(i));
    } else
      lines = ZERO;
  }

  end = getScanTextBuffer(e->text_buffer, e->caret,
			  NAME_line, lines, NAME_end);

  return killEditor(e, e->caret, end);
}


static status
killSentenceEditor(Editor e, Int arg)
{ Int end = getScanTextBuffer(e->text_buffer,
			      e->caret,
			      NAME_sentence,
			      toInt(UArg(arg)-1),
			      NAME_end);
  MustBeEditable(e);
  return killEditor(e, e->caret, end);
}


static status
killParagraphEditor(Editor e, Int arg)
{ Int end = getScanTextBuffer(e->text_buffer,
			      e->caret,
			      NAME_paragraph,
			      toInt(UArg(arg)-1),
			      NAME_end);
  MustBeEditable(e);
  return killEditor(e, e->caret, end);
}


static status
killTermEditor(Editor e, Int arg)
{ Int end = getScanTextBuffer(e->text_buffer,
			      e->caret,
			      NAME_term,
			      toInt(UArg(arg)),
			      NAME_end);
  MustBeEditable(e);
  return killEditor(e, e->caret, end);
}


static status
killOrGrabRegionEditor(Editor e, Int arg)
{ if ( notDefault(e->mark) )
  { assign(e, mark, normalise_index(e, e->mark));
    if ( isDefault(arg) )
      return killEditor(e, e->caret, e->mark);

    return grabEditor(e, e->caret, e->mark);
  }

  send(e, NAME_report, NAME_warning, CtoName("No mark"), EAV);
  succeed;
}

		/********************************
		*          MISCELENEOUS		*
		********************************/

static status
undefinedEditor(Editor e)
{ send(e, NAME_report, NAME_warning, CtoName("Undefined"), EAV);

  succeed;
}


static status
keyboardQuitEditor(Editor e, Int arg)
{ assign(e, focus_function, NIL);
  abortIsearchEditor(e);
  markStatusEditor(e, NAME_inactive);
  send(e, NAME_report, NAME_warning, CtoName("Quit"), EAV);

  succeed;
}


static status
undoEditor(Editor e)
{ Int caret;

  if ( (caret = getUndoTextBuffer(e->text_buffer)) )
  { return CaretEditor(e, caret);
  } else
  { send(e, NAME_report, NAME_warning,
	 CtoName("No (further) undo information"), EAV);
    fail;
  }
}


static status
setMarkEditor(Editor e, Int arg)
{ if ( isDefault(arg) )
    arg = e->caret;

  markEditor(e, arg, NAME_active);

  send(e, NAME_report, NAME_status, CtoName("Mark set"), EAV);
  succeed;
}


static status
switchCaseModeEditor(Editor e, Int arg)
{ if ( isDefault(arg) )
    assign(e, exact_case, e->exact_case == ON ? OFF : ON);
  else
    assign(e, exact_case, UArg(arg) > 0 ? OFF : ON);

  send(e, NAME_report, NAME_status, CtoName("%s case"),
       e->exact_case == ON ? CtoName("Exact") : CtoName("Either"), EAV);

  succeed;
}


static status
pointToMarkEditor(Editor e)
{ return CaretEditor(e, e->mark);
}


static status
exchangePointAndMarkEditor(Editor e)
{ if ( notNil(e->mark) )
    return selection_editor(e, e->caret, e->mark, NAME_active);

  send(e, NAME_report, NAME_warning, CtoName("No mark"), EAV);
  fail;
}


static status
transposeWordEditor(Editor e)
{ Int f1, t1, f2, t2;
  Int caret = e->caret;

  MustBeEditable(e);
  backwardWordEditor(e, ONE);	f1 = e->caret;
  forwardWordEditor(e, ONE);	t1 = e->caret;
  forwardWordEditor(e, ONE);	t2 = e->caret;
  backwardWordEditor(e, ONE);	f2 = e->caret;
  if ( transposeTextBuffer(e->text_buffer, f1, t1, f2, t2) )
    CaretEditor(e, add(caret, sub(sub(t2, f2), sub(t1, f1))));  

  succeed;
}


static status
transposeLinesEditor(Editor e)
{ Int f1, t1, f2, t2;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);

  t2 = getScanTextBuffer(tb, e->caret, NAME_line, ZERO, NAME_end);
  f2 = getScanTextBuffer(tb, e->caret, NAME_line, ZERO, NAME_start);
  t1 = sub(f2, ONE);
  f1 = getScanTextBuffer(tb, t1,       NAME_line, ZERO, NAME_start);

  if ( transposeTextBuffer(tb, f1, t1, f2, t2) )
    forwardCharEditor(e, sub(f1, f2));

  succeed;
}


static status
transposeTermsEditor(Editor e)
{ Int f1, t1, f2, t2;
  TextBuffer tb = e->text_buffer;
  int caret = valInt(e->caret);

  MustBeEditable(e);

  if ( !tisblank(tb->syntax, fetch_textbuffer(tb, caret)) &&
        tisblank(tb->syntax, fetch_textbuffer(tb, caret-1)) )
    caret--;

  f2 = getScanTextBuffer(tb, e->caret, NAME_term, ONE,       NAME_start);
  t2 = getScanTextBuffer(tb, f2,       NAME_term, ONE,       NAME_end);
  t1 = getScanTextBuffer(tb, e->caret, NAME_term, toInt(-1), NAME_end);
  f1 = getScanTextBuffer(tb, t1,       NAME_term, toInt(-1), NAME_start);

  if ( transposeTextBuffer(tb, f1, t1, f2, t2) )
    CaretEditor(e, add(e->caret, sub(sub(t2,f2), sub(t1, f1))));

  succeed;
}


static status
deleteHorizontalSpaceEditor(Editor e, Int arg)
{ int f, t;
  int spaces = (isDefault(arg) ? 0 : valInt(arg));
  SyntaxTable syntax = e->text_buffer->syntax;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);
  f = t = valInt(e->caret);
  if ( f > 0 &&
       !tisblank(syntax, Fetch(e, f)) && tisblank(syntax, Fetch(e, f-1)) )
    f--, t--;
  for( ; f > 0 && tisblank(syntax, Fetch(e, f-1)); f-- )
    ;
  for( ; t < tb->size && tisblank(syntax, Fetch(e, t)); t++ )
    ;
  delete_textbuffer(tb, f, t-f);
  insert_textbuffer(tb, f, spaces, str_spc(&tb->buffer));

  return CaretEditor(e, toInt(f+spaces));
}


static status
justOneSpaceEditor(Editor e)
{ return deleteHorizontalSpaceEditor(e, ONE);
}


static status
deleteBlankLinesEditor(Editor e)
{ TextBuffer tb = e->text_buffer;
  Int to, from;

  MustBeEditable(e);

  to = getScanTextBuffer(tb,
			 getSkipBlanksTextBuffer(tb,e->caret,NAME_forward,ON),
			 NAME_line, ZERO, NAME_start);
  from = getSkipBlanksTextBuffer(tb, e->caret, NAME_backward, ON);
  
  if ( valInt(to) > valInt(from) )
  { deleteTextBuffer(tb, from, sub(to, from));
    CaretEditor(e, from);
  }  

  succeed;
}


static status
gosmacsTransposeEditor(Editor e)
{ long caret = valInt(e->caret);

  MustBeEditable(e);
  if ( caret >= 2 )
  { char c1, c2;

    c1 = Fetch(e, caret-2);
    c2 = Fetch(e, caret-1);
    characterTextBuffer(e->text_buffer, toInt(caret-2), toInt(c2));
    characterTextBuffer(e->text_buffer, toInt(caret-1), toInt(c1));

    succeed;
  }
  
  fail;
}


static status
transposeCharsEditor(Editor e)
{ long caret = valInt(e->caret);

  MustBeEditable(e);
  if ( caret >= 1 )
  { char c1, c2;

    c1 = Fetch(e, caret-1);
    c2 = Fetch(e, caret);
    characterTextBuffer(e->text_buffer, toInt(caret-1), toInt(c2));
    characterTextBuffer(e->text_buffer, toInt(caret), toInt(c1));

    succeed;
  }
  
  fail;
}

		/********************************
		*            FILES		*
		********************************/

static status
saveBufferEditor(Editor e, Int arg)
{ status rval = SUCCEED;

  if ( e->text_buffer->modified == ON && isDefault(arg) )
  { if ( notNil(e->file) )
    { if ( saveEditor(e, DEFAULT) )
      { CmodifiedTextBuffer(e->text_buffer, OFF);
	send(e, NAME_report, NAME_status,
	     CtoName("Buffer saved in %N"), e->file, EAV);
      } else
      { send(e, NAME_report, NAME_error,
	     CtoName("Failed to save buffer into %N"), e->file, EAV);
	rval = FAIL;
      }      
    } else
    { send(e, NAME_report, NAME_error, CtoName("No current file"), EAV);
      rval = FAIL;
    }
  } else
  { send(e, NAME_report, NAME_status, CtoName("No changes need saving"), EAV);
  }

  return rval;
}


		/********************************
		*        CHARACTER CASE		*
		********************************/

static status
downcaseRegionEditor(Editor e)
{ Int from = e->mark;
  Int to   = e->caret;
  
  MustBeEditable(e);
  if ( isDefault(from) )
    fail;

  Before(from, to);
  return downcaseTextBuffer(e->text_buffer, from, sub(to, from));
}


static status
upcaseRegionEditor(Editor e)
{ Int from = e->mark;
  Int to   = e->caret;
  
  MustBeEditable(e);
  if ( isDefault(from) )
    fail;

  Before(from, to);
  return upcaseTextBuffer(e->text_buffer, from, sub(to, from));
}


static status
capitaliseRegionEditor(Editor e)
{ Int from = e->mark;
  Int to   = e->caret;
  
  MustBeEditable(e);
  if ( isDefault(from) )
    fail;

  Before(from, to);
  return capitaliseTextBuffer(e->text_buffer, from, sub(to, from));
}


static status
downcasePreviousWordEditor(Editor e, Int arg)
{ Int f = getScanTextBuffer(e->text_buffer,
			    sub(e->caret, ONE), NAME_word, toInt(1-UArg(arg)),
			    NAME_start);
  MustBeEditable(e);
  return downcaseTextBuffer(e->text_buffer, f, sub(e->caret, f));
}


static status
upcasePreviousWordEditor(Editor e, Int arg)
{ Int f = getScanTextBuffer(e->text_buffer,
			    sub(e->caret, ONE), NAME_word, toInt(1-UArg(arg)),
			    NAME_start);
  MustBeEditable(e);
  return upcaseTextBuffer(e->text_buffer, f, sub(e->caret, f));
}


static status
capitalisePreviousWordEditor(Editor e, Int arg)
{ Int f = getScanTextBuffer(e->text_buffer,
			    sub(e->caret, ONE), NAME_word, toInt(1-UArg(arg)),
			    NAME_start);
  MustBeEditable(e);
  return capitaliseTextBuffer(e->text_buffer, f, sub(e->caret, f));
}


static status
downcaseWordEditor(Editor e, Int arg)
{ Int to = getScanTextBuffer(e->text_buffer,
			     e->caret, NAME_word, toInt(UArg(arg)-1),
			     NAME_end);
  MustBeEditable(e);
  downcaseTextBuffer(e->text_buffer, e->caret, sub(to, e->caret));
  return CaretEditor(e, to);
}


static status
upcaseWordEditor(Editor e, Int arg)
{ Int to = getScanTextBuffer(e->text_buffer,
			     e->caret, NAME_word, toInt(UArg(arg)-1),
			     NAME_end);
  MustBeEditable(e);
  upcaseTextBuffer(e->text_buffer, e->caret, sub(to, e->caret));
  return CaretEditor(e, to);
}


static status
capitaliseWordEditor(Editor e, Int arg)
{ Int to = getScanTextBuffer(e->text_buffer,
			     e->caret, NAME_word, toInt(UArg(arg)-1),
			     NAME_end);
  MustBeEditable(e);
  capitaliseTextBuffer(e->text_buffer, e->caret, sub(to, e->caret));
  return CaretEditor(e, to);
}


static status
toggleCharCaseEditor(Editor e)
{ long caret = valInt(e->caret);

  MustBeEditable(e);
  if ( caret >= 1 )
  { char c;

    c = Fetch(e, caret-1);
    if ( tisupper(e->text_buffer->syntax, c) )
      c = tolower(c);
    else if ( tislower(e->text_buffer->syntax, c) )
      c = toupper(c);
    else
      succeed;
    
    return characterTextBuffer(e->text_buffer, toInt(caret-1), toInt(c));
  }
  
  fail;

}


		/********************************
		*          INDENT/UNDENT        *
		*********************************/

static long
start_of_line(Editor e, Int where)
{ TextBuffer tb = e->text_buffer;

  if ( isDefault(where) )
    where = e->caret;
  where = normalise_index(e, where);

  return valInt(getScanTextBuffer(tb, where, NAME_line, ZERO, NAME_start));
}


static long
end_of_line(Editor e, Int where)
{ TextBuffer tb = e->text_buffer;

  if ( isDefault(where) )
    where = e->caret;
  where = normalise_index(e, where);

  return valInt(getScanTextBuffer(tb, where, NAME_line, ZERO, NAME_end));
}


static void
get_region_editor(Editor e, Int *from, Int *to)
{ *from = normalise_index(e, e->mark);
  *to   = normalise_index(e, e->caret);
  Before(*from, *to);
}


static status
blankLineEditor(Editor e, Int where)
{ TextBuffer tb = e->text_buffer;
  long sol = start_of_line(e, where);
  
  for( ; ; sol++)
  { char c = fetch_textbuffer(tb, sol);

    if ( tisblank(tb->syntax, c) )
      continue;
    if ( tisendsline(tb->syntax, c) )
      succeed;
    fail;
  }
}


static Int
getIndentationEditor(Editor e, Int where, Regex re)
{ TextBuffer tb = e->text_buffer;
  int col;
  Int n;
  long sol = start_of_line(e, where);
  long eoi;

  if ( isDefault(re) )
  { eoi = valInt(getSkipBlanksTextBuffer(tb, toInt(sol), NAME_forward, OFF));
  } else
  { long eol = end_of_line(e, where);
    eoi = ((n=getMatchRegex(re, tb, toInt(sol), toInt(eol))) ? sol+valInt(n)
	   						     : sol);
  }

  for(col = 0; sol < eoi; sol++)
  { switch( fetch_textbuffer(tb, sol) )
    { case '\t':
	col++;
	col = Round(col, valInt(e->tab_distance));
	continue;
      case '\b':
	col--;
	continue;
      default:
        col++;
        continue;
    }
  }

  answer(toInt(col));
}


static status
alignEditor(Editor e, Int column, Int where)
{ TextBuffer tb = e->text_buffer;
  long here = valInt(normalise_index(e, notDefault(where) ? where : e->caret));
  long txt;
  int txtcol, tabs, spaces;
  int col = valInt(column);
  int tabd = valInt(e->tab_distance);

					/* find text before `here' */
  for( txt = here-1;
       txt >= 0 && tisblank(tb->syntax, fetch_textbuffer(tb, txt));
       txt--)
    ;
  txt++;
  txtcol = valInt(getColumnEditor(e, toInt(txt)));
  DEBUG(NAME_align, Cprintf("col = %d; txt = %ld; txtcol = %d\n",
			    col, txt, txtcol));

  if ( col <= txtcol )
  { tabs = 0;
    if ( txt == 1 || tisendsline(tb->syntax, fetch_textbuffer(tb, txt-1)) )
      spaces = 0;
    else
      spaces = 1;
  } else
  { tabs   = col / tabd - txtcol / tabd;
    spaces = (tabs == 0 ? col - txtcol : col % tabd);
  }
  DEBUG(NAME_align, Cprintf("tabs = %d; spaces = %d\n", tabs, spaces));

					/* delete old indent */
  delete_textbuffer(tb, txt, here-txt);
  insert_textbuffer(tb, txt, tabs, str_tab(&tb->buffer));
  insert_textbuffer(tb, txt+tabs, spaces, str_spc(&tb->buffer));

  succeed;
}


/*  Set the indentation of a line to be `column'
*/

static status
alignOneLineEditor(Editor e, Int where, Int column)
{ TextBuffer tb = e->text_buffer;
  long sol = start_of_line(e, where);
  long sot;
  int tabs, spaces;
  int col;

  if ( isDefault(column) )
    column = e->left_margin;
  col = valInt(column);
  if ( col < 0 )
    col = 0;

  for(sot = sol;
      sot < tb->size && tisblank(tb->syntax, fetch_textbuffer(tb, sot));
      sot++)
    ;
					/* delete old indent */
  delete_textbuffer(tb, sol, sot-sol);
  tabs   = col / valInt(e->tab_distance);
  spaces = col % valInt(e->tab_distance);
  insert_textbuffer(tb, sol, tabs, str_tab(&tb->buffer));
  insert_textbuffer(tb, sol+tabs, spaces, str_spc(&tb->buffer));

  succeed;
}


static status
alignLineEditor(Editor e, Int arg)	/* align line on indentation arg */
{ MustBeEditable(e);
  return alignOneLineEditor(e, e->caret, arg);
}


static status
alignRegionEditor(Editor e, Int arg)
{ Int from, to;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);
  get_region_editor(e, &from, &to);
  e->internal_mark = valInt(to);
  while( valInt(from) < e->internal_mark )  
  { alignOneLineEditor(e, from, arg);
    from = getScanTextBuffer(tb, from, NAME_line, ONE, NAME_start);
  }

  succeed;
}


static status
indentOneLineEditor(Editor e, Int where, Int arg)
{ int col = valInt(getIndentationEditor(e, where, DEFAULT)) +
	    UArg(arg) * valInt(e->indent_increment);

  return alignOneLineEditor(e, where, toInt(col));
}


static status
indentLineEditor(Editor e, Int arg)
{ MustBeEditable(e);
  beginningOfLineEditor(e, DEFAULT);
  indentOneLineEditor(e, e->caret, arg);
  return skipBlanksEditor(e, DEFAULT);
}


static status
undentLineEditor(Editor e, Int arg)
{ return indentLineEditor(e, toInt(-UArg(arg)));
}


static status
indentRegionEditor(Editor e, Int arg)
{ Int from, to;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);
  get_region_editor(e, &from, &to);
  e->internal_mark = valInt(to);
  while( valInt(from) < e->internal_mark )  
  { indentOneLineEditor(e, from, arg);
    from = getScanTextBuffer(tb, from, NAME_line, ONE, NAME_start);
  }

  succeed;
}


static status
undentRegionEditor(Editor e, Int arg)
{ return indentRegionEditor(e, toInt(-UArg(arg)));
}

/* Insert a newline and copy the indentation of the last non-blank line.
*/

static status
newlineAndIndentEditor(Editor e, Int arg)
{ Int index;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);
  endOfLineEditor(e, DEFAULT);
  newlineEditor(e, arg);
  index = e->caret;
  do
  { index = getScanTextBuffer(tb, index, NAME_line, toInt(-1), NAME_start);
    if ( !blankLineEditor(e, index) )
    { alignLineEditor(e, getIndentationEditor(e, index, DEFAULT));
      endOfLineEditor(e, DEFAULT);
      break;
    }
  } while( index != ZERO );
  
  succeed;
}

		/********************************
		*         FILL/JUSTIFY		*
		********************************/

static status
autoFillModeEditor(Editor e, Int arg)
{ Bool val;
  if ( isDefault(arg) )
    val = (e->fill_mode == ON ? OFF : ON);
  else
    val = (UArg(arg) > 0 ? ON : OFF);
  assign(e, fill_mode, val);
  send(e, NAME_report, NAME_status,
       CtoName("%sAuto Fill"), val == ON ? CtoName("") : CtoName("No "), EAV);
  succeed;
}


static status
setFillColumnEditor(Editor e, Int arg)
{ if ( isDefault(arg) )
    send(e, NAME_report, NAME_inform,
	 CtoName("Left margin: %d, Right margin: %d"),
	 e->left_margin, e->right_margin, EAV);
  else if ( UArg(arg) > 0 )
    assign(e, right_margin, arg);
  else
    assign(e, left_margin, toInt(-UArg(arg)));

  succeed;
}


static status
fillEditor(Editor e,
	   Int from, Int to,
	   Int left_margin, Int right_margin,
	   Bool justify)
{ TextBuffer tb = e->text_buffer;
  int rm  = valInt(isDefault(right_margin) ? e->right_margin : right_margin);
  int lm  = valInt(isDefault(left_margin)  ? e->left_margin  : left_margin);
  int pos = start_of_line(e, normalise_index(e, from));
  int end;
  int ep;				/* end of paragraph */
  int col;
  int p;

  MustBeEditable(e);

  end = valInt(normalise_index(e, to));

  while( pos < end )
  { int p0 = pos;

    DEBUG(NAME_fill, Cprintf("fill: region = %d ... %d\n", pos, end)); 
    
					/* skip the separator */ 
    while( pos < end && parsep_line_textbuffer(tb, pos) )
    { pos = scan_textbuffer(tb, p=pos, NAME_line, 1, 'a');
      if ( pos <= p )			/* end of file? */
	break;
    }

    ep = scan_textbuffer(tb, pos, NAME_paragraph, 0, 'z');
    if ( fetch_textbuffer(tb, ep-1) == '\n' )
      ep--;
    ep = min(ep, end);
    e->internal_mark = ep;

    col = 0;				/* Do the first line: keep indent */
    while( pos < e->internal_mark &&
	   tisblank(tb->syntax, fetch_textbuffer(tb, pos)) )
    { if ( fetch_textbuffer(tb, pos) == '\t' )
        col = Round(col+1, valInt(e->tab_distance));
      else
      	col++;
      pos++;
    }
    DEBUG(NAME_fill, Cprintf("Filling first paragraph line from %d\n", pos));
    pos = fill_line_textbuffer(tb, pos, e->internal_mark,
			       col, rm, justify == ON);

					/* do the lines of the paragraph */
    while( pos < e->internal_mark && !parsep_line_textbuffer(tb, pos) )
    { alignOneLineEditor(e, toInt(pos), toInt(lm));
      pos = valInt(getSkipBlanksTextBuffer(tb, toInt(pos), NAME_forward, OFF));
      DEBUG(NAME_fill, Cprintf("Next paragraph line from %d\n", pos));
      pos = fill_line_textbuffer(tb, pos, e->internal_mark,
				 lm, rm, justify == ON);
    }
    DEBUG(NAME_fill,
	  Cprintf("%s end\n",
		  pos < e->internal_mark ? "Paragraph" : "Region"));

					/* correct end for inserts/deletes */
    end += e->internal_mark - ep;
    pos = max(pos, p0+1);		/* ensure progress */
  }
  changedTextBuffer(tb);		/* Not a neat place! */

  succeed;
}


static status
fillRegionEditor(Editor e)
{ TextBuffer tb = e->text_buffer;
  Int from = e->mark;
  Int to   = e->caret;
  
  from = getScanTextBuffer(tb, from, NAME_line, ZERO, NAME_start);

  Before(from, to);
  return fillEditor(e, from, to, DEFAULT, DEFAULT, OFF);
}


static status
fillParagraphEditor(Editor e, Int arg)
{ TextBuffer tb = e->text_buffer;
  Int from = getScanTextBuffer(tb, add(e->caret, ONE), NAME_paragraph,
			       ZERO, NAME_start);
  Int to   = getScanTextBuffer(tb, sub(e->caret, ONE), NAME_paragraph,
			       ZERO, NAME_end);
  Bool justify = (isDefault(arg) ? OFF : ON);

  return fillEditor(e, from, to, DEFAULT, DEFAULT, justify);
}


static status
justifyParagraphEditor(Editor e)
{ TextBuffer tb = e->text_buffer;
  Int from = getScanTextBuffer(tb, add(e->caret, ONE), NAME_paragraph,
			       ZERO, NAME_start);
  Int to   = getScanTextBuffer(tb, sub(e->caret, ONE), NAME_paragraph,
			       ZERO, NAME_end);

  return fillEditor(e, from, to, DEFAULT, DEFAULT, ON);
}


static status
justifyRegionEditor(Editor e)
{ TextBuffer tb = e->text_buffer;
  Int from = e->mark;
  Int to   = e->caret;
  
  from = getScanTextBuffer(tb, from, NAME_line, ZERO, NAME_start);

  Before(from, to);
  return fillEditor(e, from, to, DEFAULT, DEFAULT, ON);
}


static status
insertSelfFillEditor(Editor e, Int times, Int chr)
{ TextBuffer tb = e->text_buffer;
  LocalString(s, &tb->buffer, 1);
  wchar c;
  Int le;

  MustBeEditable(e);

  if ( isDefault(times) )
    times = ONE;

  if ( isDefault(chr) )
  { EventObj ev = EVENT->value;

    if ( instanceOfObject(ev, ClassEvent) && isAEvent(ev, NAME_printable) )
      c = valInt(getIdEvent(ev));
    else
      return errorPce(e, NAME_noCharacter);
  } else
    c = valInt(chr);

  str_store(s, 0, c);
  s->size = 1;
  insert_textbuffer(e->text_buffer, Caret(e), valInt(times), s);
  le = getScanTextBuffer(tb, e->caret, NAME_line, ZERO, NAME_end);

  if ( valInt(getColumnEditor(e, le)) > valInt(e->right_margin) )
    send(e, NAME_autoFill, EAV);

  if ( tisclosebrace(e->text_buffer->syntax, c) &&
       getClassVariableValueObject(e, NAME_showOpenBracket) == ON )
    showMatchingBracketEditor(e, sub(e->caret, ONE));

  succeed;
}


static status
autoFillEditor(Editor e, Int caret)
{ TextBuffer tb = e->text_buffer;
  Int from, to, lm;
  
  if ( isDefault(caret) )
    caret = e->caret;

  from = getScanTextBuffer(tb, e->caret, NAME_line, ZERO, NAME_start);
  to = getScanTextBuffer(tb, sub(e->caret, ONE), NAME_paragraph,
			 ZERO, NAME_end);
  lm = getIndentationEditor(e, from, DEFAULT);

  fillEditor(e, from, to, lm, DEFAULT, OFF);

  succeed;
}


		/********************************
		*           SEARCHING		*
		********************************/

static status
findCutBufferEditor(Editor e, Int arg)
{ StringObj str;
  Int start = normalise_index(e, e->caret);
  int hit_start;
  int ign = (e->exact_case == OFF);
  int buffer = UArg(arg) - 1;

  if ( buffer < 0 || buffer > 7 )
  { send(e, NAME_report, NAME_error,
	 CtoName("Illegal cut buffer: %d"), toInt(buffer+1));
    fail;
  }
  
  if ( (str = get(getDisplayGraphical((Graphical) e),
		  NAME_cutBuffer, toInt(buffer), EAV)) == FAIL )
  { send(e, NAME_report, NAME_warning,
	 CtoName("Failed to get cut buffer %d"), toInt(buffer+1));
    fail;
  }

  hit_start = find_textbuffer(e->text_buffer,
			      valInt(start),
			      &str->data,
			      1, 'a', !ign, FALSE);
  if ( hit_start < 0 )
  { send(e, NAME_report, NAME_warning, CtoName("Failed search: %s"), str, EAV);
    fail;
  }
    
  selection_editor(e,
		   toInt(hit_start),
		   toInt(hit_start + str->data.size),
		   NAME_highlight);
  ensureVisibleEditor(e, toInt(hit_start), toInt(hit_start + str->data.size));
  succeed;
}


static status
isisearchingEditor(Editor e)
{ if ( e->focus_function == NAME_Isearch ||
       e->focus_function == NAME_StartIsearch )
    succeed;

  fail;
}


static status
StartIsearchEditor(Editor e, EventId id)
{ Name cmd = getKeyBindingEditor(e, characterName(id));

  if ( !equalName(cmd, NAME_isearchForward) &&
       !equalName(cmd, NAME_isearchBackward) )
    assign(e, search_string, NIL);

  assign(e, focus_function, NAME_Isearch);
  return IsearchEditor(e, id);
}


static status
beginIsearchEditor(Editor e, Name direction)
{ assign(e, search_direction, direction);
  assign(e, search_base,      e->caret);
  assign(e, search_origin,    e->caret);
  assign(e, focus_function,   NAME_StartIsearch);
  selection_editor(e, e->caret, e->caret, NAME_highlight);
  send(e, NAME_report, NAME_status, CtoName("isearch %s"), direction, EAV);

  succeed;
}


static status
abortIsearchEditor(Editor e)
{ if ( isisearchingEditor(e) )
  { assign(e, focus_function, NIL);
    selection_editor(e, DEFAULT, DEFAULT, NAME_inactive);
  }
  
  succeed;
}


static status
endIsearchEditor(Editor e)
{ if ( isisearchingEditor(e) )
  { /*int caret = (e->search_direction == NAME_forward ? e->selection_end
						     : e->selection_start);

    CaretEditor(e, toInt(caret)); TBD */
    abortIsearchEditor(e);
    send(e, NAME_report, NAME_status, NAME_, EAV);
  }

  succeed;
}


static status
isearchForwardEditor(Editor e)
{ return beginIsearchEditor(e, NAME_forward);
}


static status
isearchBackwardEditor(Editor e)
{ return beginIsearchEditor(e, NAME_backward);
}


static status
extendSearchStringToWordEditor(Editor e)
{ TextBuffer tb = e->text_buffer;
  Int start = e->mark;			/* TBD */
  Int end   = e->caret;

  end = getScanTextBuffer(tb, end, NAME_word, ZERO, NAME_end);

  assign(e, search_string, getContentsTextBuffer(tb, start, sub(end, start)));
  selection_editor(e, start, end, DEFAULT);
  return ensureVisibleEditor(e, start, end);
}


static status
backwardDeleteCharSearchStringEditor(Editor e)
{ if ( notNil(e->search_string) )
  { Int size = getSizeCharArray(e->search_string);

    if ( size == ONE )
      assign(e, search_string, NIL);
    else
      deleteString(e->search_string, sub(size, ONE), DEFAULT);
  }
  
  succeed;
}


static status
executeSearchEditor(Editor e, Int chr)
{ int l, hit_start, hit_end;
  int times, start;
  int fwd = (e->search_direction == NAME_forward);
  int ign = (e->exact_case == OFF);

  if ( notDefault(chr) )
  { if ( isNil(e->search_string) )
      assign(e, search_string, newObject(ClassString, EAV));

    insertCharacterString(e->search_string, chr, DEFAULT, DEFAULT);
  }

  l     = valInt(getSizeCharArray(e->search_string));
  if ( fwd )
  { times = 1;
    start = valInt(e->mark);
  } else
  { times = -1;
    start = valInt(e->caret);
  }

  if ( isNil(e->search_string) || l == 0 )
  { send(e, NAME_report, NAME_warning, CtoName("No search string"), EAV);
    abortIsearchEditor(e);
    succeed;
  }

  if ( isDefault(chr) && e->mark != e->caret )
    start += times;

  hit_start = find_textbuffer(e->text_buffer,
			      start, 
			      &e->search_string->data,
			      times, 'a', !ign, FALSE);
  if ( hit_start < 0 )
  { send(e, NAME_report, NAME_warning,
	 CtoName("Failing ISearch: %s"), e->search_string, EAV);

    if ( notDefault(chr) )
      backwardDeleteCharSearchStringEditor(e);

    succeed;
  }
  hit_end = hit_start + l;

  if ( isDefault(chr) )
    assign(e, search_base, toInt(fwd ? hit_start : hit_end-1));

  if ( !fwd )				/* backward: swap */
  { int tmp = hit_end;
    hit_end = hit_start;
    hit_start = tmp;
  }
  selection_editor(e, toInt(hit_start), toInt(hit_end), NAME_highlight);
  ensureVisibleEditor(e, toInt(hit_start), toInt(hit_end));

  succeed;
}


static status
searchDirectionEditor(Editor e, Name dir)
{ if ( dir != e->search_direction )
  { assign(e, search_direction, dir);

    if ( dir == NAME_forward )
    { if ( valInt(e->caret) < valInt(e->mark) )
	selection_editor(e, e->caret, e->mark, DEFAULT);
      assign(e, search_base, e->mark);
    } else
    { if ( valInt(e->mark) < valInt(e->caret) )
	selection_editor(e, e->caret, e->mark, DEFAULT);
      assign(e, search_base, e->mark);
    }
  }

  succeed;
}


static status
IsearchEditor(Editor e, EventId id)
{ Int chr = id;				/* TBD: test for character */
  Name cmd = getKeyBindingEditor(e, characterName(id));

  if ( cmd == NAME_keyboardQuit )	/* abort the search */
  { selection_editor(e, e->search_origin, e->search_origin, NAME_inactive);
    assign(e, search_string, NIL);
    keyboardQuitEditor(e, DEFAULT);
    endIsearchEditor(e);

    succeed;
  }
  if ( cmd == NAME_isearchForward )
  { searchDirectionEditor(e, NAME_forward);
    return executeSearchEditor(e, DEFAULT);
  }
  if ( cmd == NAME_isearchBackward )
  { searchDirectionEditor(e, NAME_backward);
    return executeSearchEditor(e, DEFAULT);
  }
  if ( cmd == NAME_backwardDeleteChar ||
       cmd == NAME_cutOrBackwardDeleteChar )
  { backwardDeleteCharSearchStringEditor(e);
    if ( notNil(e->search_string) )
    { selection_editor(e, e->search_base, e->search_base, DEFAULT);
      executeSearchEditor(e, DEFAULT);
    } else
      endIsearchEditor(e);

    succeed;
  }
      
  if ( !isInteger(id) )
  { endIsearchEditor(e);
    fail;
  }

  switch( valInt(chr) )
  { case ESC:
      endIsearchEditor(e);
      succeed;
    case Control('W'):
      extendSearchStringToWordEditor(e);
      succeed;
    case Control('M'):
      chr = toInt(Control('J'));
    case Control('J'):
    case Control('I'):
      return executeSearchEditor(e, chr);
    case Control('L'):
    case Control('@'):
      endIsearchEditor(e);
      fail;
  }

  if ( valInt(chr) < Meta(0) &&
       tisprint(e->text_buffer->syntax, valInt(chr)) )
    return executeSearchEditor(e, chr);

  endIsearchEditor(e);
  fail;
}

		/********************************
		*            DABBREV		*
		********************************/

static Name
get_dabbrev_target(Editor e)
{ Int caret = e->caret;
  TextBuffer tb = e->text_buffer;
  Int sow = getScanTextBuffer(tb, caret, NAME_word, 0, NAME_start);
  int n;
  string s;
  
  for(n=valInt(sow); n < valInt(caret); n++)
  { if ( !tisalnum(tb->syntax, fetch_textbuffer(tb, n)) )
    { send(e, NAME_report, NAME_warning, CtoName("Not at end of word"), EAV);
      fail;
    }
  }

  assign(e, dabbrev_origin, sow);
  str_sub_text_buffer(tb, &s, valInt(sow), valInt(caret) - valInt(sow));
  answer(StringToName(&s));  
}


static status
dabbrevExpandEditor(Editor e)
{ Name target;

  MustBeEditable(e);

  TRY( target = get_dabbrev_target(e) );
  assign(e, dabbrev_target, target);
  DEBUG(NAME_editor, Cprintf("dabbrev target = %s\n", pp(target)));

  if ( notNil(e->dabbrev_reject) )
    clearChain(e->dabbrev_reject);
  else
    assign(e, dabbrev_reject, newObject(ClassChain, EAV));
  appendChain(e->dabbrev_reject, target);

  assign(e, dabbrev_pos, sub(e->caret, toInt(target->data.size+1)));
  assign(e, focus_function, NAME_DabbrevExpand);

  DEBUG(NAME_editor, Cprintf("starting DabbrevExpand\n"));

  return DabbrevExpandEditor(e, DEFAULT);
}


static Name
get_dabbrev_hit_editor(Editor e, int start)
{ TextBuffer tb = e->text_buffer;
  int size = tb->size;
  int end;
  string s;

  for(end = start; end < size; end++)
  { wchar c = fetch_textbuffer(tb, end);
    if ( !tisalnum(tb->syntax, c) )
      break;
  }
  str_sub_text_buffer(tb, &s, start, end-start);
  answer(StringToName(&s));
}


static Name
get_case_pattern(SyntaxTable syntax, String s)
{ int i, size = s->size;

  if ( tisupper(syntax, str_fetch(s, 0)) )
  { for( i=1; i < size; i++)
    { if ( tislower(syntax, str_fetch(s, i)) )
    	return NAME_capitalised;
    }
    return NAME_upper;
  }

  return NAME_lower;
}


static void
fix_case_and_insert(TextBuffer tb, int where, String insert,
		    Name pattern, int ec)
{ if ( insert->size == 0 )
    return;

  if ( ec )
  { insert_textbuffer(tb, where, 1, insert);
  } else
  { int size = insert->size;
    LocalString(copy, insert, insert->size);

    str_cpy(copy, insert);
    if ( equalName(pattern, NAME_upper) )
      str_upcase(copy, 0, size);
    else if ( equalName(pattern, NAME_capitalised) )
    { str_upcase(copy, 0, 1);
      str_downcase(copy, 1, size);
    } else
      str_downcase(copy, 0, size);

    insert_textbuffer(tb, where, 1, copy);
  }
}


static status
DabbrevExpandEditor(Editor e, EventId id)
{ int pos = valInt(e->dabbrev_pos);
  int caret = valInt(e->caret);
  String target = &e->dabbrev_target->data;
  int ec = (e->exact_case == ON);
  TextBuffer tb = e->text_buffer;
  int dir = (pos < caret ? -1 : 1);
  int hit_pos;
  Name hit;

  if ( notDefault(id) )
  { Name cmd = getKeyBindingEditor(e, characterName(id));

    if ( equalName(cmd, NAME_keyboardQuit) )
    { Int start = add(e->dabbrev_origin, toInt(target->size));

      deleteTextBuffer(tb, start, sub(e->caret, start));
      keyboardQuitEditor(e, DEFAULT);
      assign(e, focus_function, NIL);

      succeed;
    }

    if ( !equalName(cmd, NAME_dabbrevExpand) )
      fail;
  }

  for(;;)
  { Cell cell;

    DEBUG(NAME_editor, Cprintf("Starting search\n"));
    hit_pos = find_textbuffer(tb, pos, target, dir, 'a', ec, FALSE);

    if ( hit_pos < 0 )
    { if ( dir < 0 )			/* no more backwards; revert */
      { dir = -dir;
	pos = caret;
        continue;
      }

      send(e, NAME_report, NAME_warning, CtoName("No more hits"), EAV);
      assign(e, focus_function, NIL);
      succeed;
    }

    if ( hit_pos != 0 &&
	 tisalnum(tb->syntax, fetch_textbuffer(tb, hit_pos-1)) )
    { pos = hit_pos + dir;		/* hit is no start of word */
      continue;
    }

    DEBUG(NAME_editor, Cprintf("hit at %d\n", hit_pos));

    hit = get_dabbrev_hit_editor(e, hit_pos);
    DEBUG(NAME_editor, Cprintf("hit = %s\n", pp(hit)));
    pos = (dir < 0 ? hit_pos - 1 : hit_pos + target->size);

    for_cell(cell, e->dabbrev_reject)
    { Name reject = cell->value;
     
      if ( ec && reject == hit )
      	goto next;
      if ( !ec && str_icase_eq(&hit->data, &reject->data) )
      	goto next;
    }

    if ( memberChain(e->dabbrev_reject, hit) == SUCCEED )
      continue;
    appendChain(e->dabbrev_reject, hit);
    assign(e, dabbrev_pos, toInt(pos));

    DEBUG(NAME_editor, Cprintf("deleting\n"));
    deleteTextBuffer(tb, e->dabbrev_origin, sub(e->caret, e->dabbrev_origin));
    DEBUG(NAME_editor, Cprintf("inserting\n"));
    fix_case_and_insert(tb,
			valInt(e->dabbrev_origin),
			&hit->data,
			get_case_pattern(tb->syntax, target),
			str_prefix(&hit->data, target) ? TRUE : ec);
    DEBUG(NAME_editor, Cprintf("ok\n"));
    succeed;

    next:;
  } 
}

		/********************************
		*           SCROLLING		*
		********************************/

status
scrollToEditor(Editor e, Int pos)
{ if ( isDefault(pos) )
    pos = toInt(e->text_buffer->size);

  centerTextImage(e->image, pos, DEFAULT);
  return ensureCaretInWindowEditor(e);
}


static status
centerWindowEditor(Editor e, Int pos)
{ centerTextImage(e->image, normalise_index(e, pos), DEFAULT);
  ComputeGraphical(e->image);
  updateCursorEditor(e);

  succeed;
}


static status
scrollDownEditor(Editor e, Int arg)
{ Name unit = NAME_line;

  if ( isDefault(arg) )
  { arg = toInt(SCROLL_PAGE_PROM);
    unit = NAME_page;
  }

  return send(e, NAME_scrollVertical, NAME_backwards, unit, arg, EAV);
}


static status
scrollUpEditor(Editor e, Int arg)
{ Name unit = NAME_line;
  
  if ( isDefault(arg) )
  { arg = toInt(SCROLL_PAGE_PROM);
    unit = NAME_page;
  }

  return send(e, NAME_scrollVertical, NAME_forwards, unit, arg, EAV);
}


static status
scrollOneLineUpEditor(Editor e, Int arg)
{ return scrollUpEditor(e, toInt(UArg(arg)));
}


static status
scrollOneLineDownEditor(Editor e, Int arg)
{ return scrollDownEditor(e, toInt(UArg(arg)));
}


static status
lineToTopOfWindowEditor(Editor e, Int arg)
{ centerTextImage(e->image, normalise_index(e, e->caret),
		  toInt(UArg(arg) - 1));

  return ensureCaretInWindowEditor(e);
}


static status
recenterEditor(Editor e, Int arg)
{ centerTextImage(e->image, normalise_index(e, e->caret), arg);
  updateCursorEditor(e);

  succeed;
}


static status
scrollVerticalEditor(Editor e, Name dir, Name unit, Int amount)
{ TextBuffer tb = e->text_buffer;
  Int start;

  if ( unit == NAME_file )
  { if ( dir == NAME_goto )
    { if ( tb->size < MAXPRECISESCROLLING &&
	   (start = getScrollStartTextImage(e->image, dir, unit, amount)) )
      { startTextImage(e->image, start, ZERO);
	
	return ensureCaretInWindowEditor(e);
      } else if ( tb->size < MAXLINEBASEDSCROLLING )
      { int size = valInt(countLinesEditor(e, ZERO, toInt(tb->size)));
	int view = valInt(getLinesTextImage(e->image));
	int target = ((size-view)*valInt(amount))/1000;
	int cp;				/* character-position */
	
	if ( target < 0 )
	  target = 0;
	
	cp = start_of_line_n_textbuffer(tb, target+1);
	centerTextImage(e->image, toInt(cp), ONE);
	ensureCaretInWindowEditor(e);
      } else
      { long h = (long)(((double)tb->size * (double)valInt(amount)) / 1000.0);
					/* avoid integer arithmetic overflow */

	scrollToEditor(e, toInt(h));
      }
    }
  } else 
  { if ( (start = getScrollStartTextImage(e->image, dir, unit, amount)) )
    { startTextImage(e->image, start, ZERO);
	
      return ensureCaretInWindowEditor(e);
    }
  }

  succeed;
}


static status
showScrollBarEditor(Editor e, Bool show, ScrollBar sb)
{ if ( isDefault(sb) || sb == e->scroll_bar )
  { computeBoundingBoxDevice((Device)e);
    DisplayedGraphical(e->scroll_bar, show);
    geometryEditor(e, DEFAULT, DEFAULT, e->area->w, e->area->h);
  }

  succeed;
}

		/********************************
		*     SELECTION/REGION/MARK	*
		********************************/

static status
internalMarkEditor(Editor e, Int mark)
{ if ( isDefault(mark) )
    mark = e->caret;

  e->internal_mark = valInt(normalise_index(e, mark));

  succeed;
}


static Int
getInternalMarkEditor(Editor e)
{ return toInt(e->internal_mark);
}



static status
selectionOriginEditor(Editor e, Int where)
{ endIsearchEditor(e);
  assign(e, selection_origin, where);

  return selectionExtendEditor(e, where);
}


static status
selectionExtendEditor(Editor e, Int where)
{ int from = valInt(e->selection_origin);
  int to   = valInt(where);
  SyntaxTable syntax = e->text_buffer->syntax;

#define WordKind(c) (tisalnum(syntax, c) ? 1 : 0)
#define LineKind(c) (tisendsline(syntax, c) ? 1 : 0)

  if ( to < from )
  { int tmp = to;
    to = from + 1;
    from = tmp;
  }

  if ( equalName(e->selection_unit, NAME_word) )
  { for( ; from > 0 && WordKind(Fetch(e, from-1)) ; from-- )
      ;

    for( ; to < e->text_buffer->size && WordKind(Fetch(e, to)); to++)
      ;
  } else if ( equalName(e->selection_unit, NAME_line) )
  { for( ; from > 0 && !LineKind(Fetch(e, from-1)); from-- )
      ;
    if ( !LineKind(Fetch(e, to)) )
      for( ; to < e->text_buffer->size && !LineKind(Fetch(e, to)); to++ )
	;
    to++;
  }

#undef WordKind
#undef LineKind

  if ( valInt(where) < valInt(e->selection_origin) ) /* swap */
  { int tmp = from;
    from = to;
    to = tmp;
  }
  return selection_editor(e, toInt(from), toInt(to), NAME_active);
}


static status
selection_editor(Editor e, Int from, Int to, Name status)
{ if ( isDefault(from) )   from   = e->mark;
  if ( isDefault(to) )     to     = e->caret;
  if ( isDefault(status) ) status = e->mark_status;

  from = normalise_index(e, from);
  to   = normalise_index(e, to);

  if ( from != e->mark || to != e->caret || status != e->mark_status )
  { if ( e->caret != e->mark )
      ChangedRegionEditor(e, e->mark, e->caret);

    assign(e, mark, from);
    assign(e, caret, to);	/* TBD: allow redefinition of ->caret */
    assign(e, mark_status, status);

    if ( from != to )
      ChangedRegionEditor(e, from, to);
  }

  succeed;
}


static status
selectLineEditor(Editor e, Int line, Bool newline)
{ Int from, to;
  TextBuffer tb = e->text_buffer;

  if ( notDefault(line) )
    from = toInt(start_of_line_n_textbuffer(tb, valInt(line)));
  else
    from = getScanTextBuffer(tb, e->caret, NAME_line, ZERO, NAME_start);

  to = getScanTextBuffer(tb, from, NAME_line, ZERO, NAME_end);
  if ( newline == ON )
    to = add(to, ONE);

  selection_editor(e, from, to, NAME_highlight);
  return ensureVisibleEditor(e, from, to);
}


status
selectionEditor(Editor e, Int from, Int to, Name status)
{ selection_editor(e, from, to, status);
  normaliseEditor(e, e->mark, e->caret);

  succeed;
}


Point
getSelectionEditor(Editor e)
{ Int f = e->mark;
  Int t = e->caret;

  if ( f != t )
  { Before(f, t);
    answer(answerObject(ClassPoint, f, t, EAV));
  }
  
  fail;
}


static Int
getSelectionStartEditor(Editor e)
{ if ( e->mark != e->caret )
    answer(e->mark);

  fail;
}


static Int
getSelectionEndEditor(Editor e)
{ if ( e->mark != e->caret )
    answer(e->caret);

  fail;
}


StringObj
getSelectedEditor(Editor e)
{ Int f = e->mark;
  Int t = e->caret;

  if ( f != t )
  { Before(f, t);
    answer(getContentsTextBuffer(e->text_buffer, f, sub(t, f)));
  }
  
  fail;
}

		/********************************
		*        SELECTION EDITS	*
		********************************/

#define SelectionRegion(e, from, to) \
  { if ( !HasSelection(e) ) \
    { send(e, NAME_report, NAME_warning, CtoName("No selection"), EAV); \
      fail; \
    } \
    from = e->mark; \
    to   = e->caret; \
    Before(from, to); \
  }

static status
deleteSelectionEditor(Editor e)
{ Int from, to;
  status rval;

  MustBeEditable(e);
  SelectionRegion(e, from, to);
  if ( (rval = deleteTextBuffer(e->text_buffer, from, sub(to, from))) )
    selection_editor(e, from, from, NAME_inactive);

  return rval;
}


static status
indentSelectionEditor(Editor e, Int arg)
{ Int from, to;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);
  SelectionRegion(e, from, to);
  e->internal_mark = valInt(to);
  while( valInt(from) < e->internal_mark )  
  { indentOneLineEditor(e, from, arg);
    from = getScanTextBuffer(tb, from, NAME_line, ONE, NAME_start);
  }
  succeed;
}


static status
undentSelectionEditor(Editor e, Int arg)
{ return indentSelectionEditor(e, toInt(-UArg(arg)));
}


static status
fillSelectionEditor(Editor e)
{ Int from, to;
  TextBuffer tb = e->text_buffer;

  MustBeEditable(e);
  SelectionRegion(e, from, to);
  from = getScanTextBuffer(tb, from, NAME_line, ZERO, NAME_start);
  return fillEditor(e, from, to, DEFAULT, DEFAULT, OFF);
}


		/********************************
		*         X CUT BUFFERS		*
		********************************/

static status
selectionToCutBufferEditor(Editor e, Int arg)
{ int buffer = UArg(arg) - 1;

  if ( buffer < 0 || buffer > 7 )
  { send(e, NAME_report, NAME_error,
	 CtoName("Illegal cut buffer: %d"), toInt(buffer+1), EAV);
    fail;
  }

  if ( !HasSelection(e) )
    fail;				/* no selection */

  return send(getDisplayGraphical((Graphical) e), NAME_cutBuffer,
	      toInt(buffer), getSelectedEditor(e), EAV);
}


static status
insertCutBufferEditor(Editor e, Int arg)
{ StringObj str;
  int buffer = UArg(arg) - 1;

  MustBeEditable(e);

  if ( buffer < 0 || buffer > 7 )
  { send(e, NAME_report, NAME_error,
	 CtoName("Illegal cut buffer: %d"), toInt(buffer+1), EAV);
    fail;
  }
  
  if ( (str = get(getDisplayGraphical((Graphical) e),
		  NAME_cutBuffer, toInt(buffer), EAV)) == FAIL )
  { send(e, NAME_report, NAME_warning,
	 CtoName("Failed to get cut buffer %d"), toInt(buffer+1), EAV);
    fail;
  }

  return insertEditor(e, (CharArray) str);
}


		/********************************
		*         PROGRAM EDITS		*
		********************************/

static status
appendEditor(Editor e, CharArray str)
{ if ( e->left_margin != ZERO )
    alignOneLineEditor(e, getLengthEditor(e), e->left_margin);
  appendTextBuffer(e->text_buffer, str, ONE);
  if ( e->auto_newline == ON )
    newlineEditor(e, ONE);
  return CaretEditor(e, DEFAULT);

  succeed;
}


static status
insertEditor(Editor e, CharArray str)
{ return insertTextBuffer(e->text_buffer, e->caret, str, ONE);
}


status
formatEditor(Editor e, CharArray fmt, int argc, Any *argv)
{ string s;

  TRY(str_writefv(&s, fmt, argc, argv));
  insert_textbuffer(e->text_buffer, Caret(e), 1, &s);
  str_unalloc(&s);
  
  succeed;
}


static status
appendfEditor(Editor e, CharArray fmt, int argc, Any *argv)
{ string s;

  TRY(str_writefv(&s, fmt, argc, argv));
  insert_textbuffer(e->text_buffer, e->text_buffer->size, 1, &s);
  str_unalloc(&s);
  
  succeed;
}



static status
printEditor(Editor e, CharArray str)
{ insertEditor(e, str);
  if ( e->auto_newline == ON )
    newlineEditor(e, ONE);

  succeed;
}


status
clearEditor(Editor e)
{ clearTextBuffer(e->text_buffer);
  selection_editor(e, ZERO, ZERO, NAME_inactive);
  assign(e, file, NIL);

  succeed;
}


static status
deleteEditor(Editor e, Int from, Int to)
{ Before(from, to);
  return deleteTextBuffer(e->text_buffer, from, sub(to, from));
}


static status
deleteLineEditor(Editor e, Int line)
{ Int from = getScanTextBuffer(e->text_buffer,
			       ZERO, NAME_line, sub(line, ONE),
			       NAME_start);
  Int to =   getScanTextBuffer(e->text_buffer,
			       from, NAME_line, ZERO, NAME_end);
  
  return deleteTextBuffer(e->text_buffer, from, sub(add(to, ONE), from));
}


static status
replaceLineEditor(Editor e, CharArray str)
{ Int from = getScanTextBuffer(e->text_buffer,
			       e->caret, NAME_line, ZERO,
			       NAME_start);
  Int to =   getScanTextBuffer(e->text_buffer,
			       from, NAME_line, ZERO, NAME_end);

  deleteTextBuffer(e->text_buffer, from, sub(to, from));
  insertTextBuffer(e->text_buffer, from, str, ONE);
  return CaretEditor(e, from);
}


static status
sortEditor(Editor e, Int from, Int to)
{ return sortTextBuffer(e->text_buffer, from, to);
}


static StringObj
getWordEditor(Editor e, Int where)
{ Int to;
  TextBuffer tb = e->text_buffer;

  if ( isDefault(where) )
    where = e->caret;
  where = normalise_index(e, where);

  where = getScanTextBuffer(tb, where, NAME_word, ZERO, NAME_start);
  to    = getScanTextBuffer(tb, where, NAME_word, ZERO, NAME_end);

  answer(getContentsTextBuffer(e->text_buffer, where, sub(to, where)));
}


static StringObj
getLineEditor(Editor e, Int where)
{ Int to;
  TextBuffer tb = e->text_buffer;

  if ( isDefault(where) )
    where = e->caret;
  where = normalise_index(e, where);

  where = getScanTextBuffer(tb, where, NAME_line, ZERO, NAME_start);
  to    = getScanTextBuffer(tb, where, NAME_line, ZERO, NAME_end);

  answer(getContentsTextBuffer(e->text_buffer, where, sub(to, where)));
}


static StringObj
getReadLineEditor(Editor e)
{ Int to;
  StringObj rval;

  if ( e->caret == getLengthEditor(e) )
    fail;
  to = getScanTextBuffer(e->text_buffer, e->caret, NAME_line, 0, NAME_end);
  rval = getContentsTextBuffer(e->text_buffer, e->caret, sub(to, e->caret));
  CaretEditor(e, add(to, ONE));
  
  answer(rval);  
}


static StringObj
getFirstLineEditor(Editor e)
{ ComputeGraphical(e->image);

  answer(getLineEditor(e, getStartTextImage(e->image, ONE)));
}


static Int
getColumnEditor(Editor e, Int where)
{ TextBuffer tb = e->text_buffer;
  long sol;
  int col;

  if ( isDefault(where) )
    where = e->caret;
  where = normalise_index(e, where);

  sol = valInt(getScanTextBuffer(tb, where, NAME_line, 0, NAME_start));
  for(col = 0; sol < valInt(where); sol++ )
  { if ( fetch_textbuffer(tb, sol) == '\t' )
    { col++;
      col = Round(col, valInt(e->tab_distance));
    } else
      col++;
  }
  
  answer(toInt(col));
}


static Int
getColumnLocationEditor(Editor e, Int c, Int from)
{ TextBuffer tb = e->text_buffer;
  int size = tb->size;
  long pos;
  int dcol = valInt(c);
  int col;

  if ( isDefault(from) )
    from = e->caret;
  pos = valInt(getScanTextBuffer(tb, from, NAME_line, 0, NAME_start));

  for(col = 0; col < dcol && pos < size; pos++)
  { switch( fetch_textbuffer(tb, pos) )
    { case '\n':
	return toInt(pos);
      case '\t':
	col++;
	col = Round(col, valInt(e->tab_distance));
	break;
      default:
	col++;
    }
  }

  answer(toInt(pos));
}


static status
columnEditor(Editor e, Int c)
{ return CaretEditor(e, getColumnLocationEditor(e, c, e->caret));
}


static status
lineNumberEditor(Editor e, Int line)
{ return CaretEditor(e, getScanTextBuffer(e->text_buffer,
					  ZERO, NAME_line, sub(line, ONE),
					  NAME_start));
}


static Int
getLineNumberEditor(Editor e, Int where)
{ if ( isDefault(where) )
    where = e->caret;
  where = normalise_index(e, where);

  answer(getLineNumberTextBuffer(e->text_buffer, where));
}


		/********************************
		*             FILES		*
		********************************/

static status
loadEditor(Editor e, SourceSink file)
{ TextBuffer tb = e->text_buffer;
  status rval;

  clearTextBuffer(tb);
  if ( (rval = insertFileTextBuffer(tb, ZERO, file, ONE)) ) 
  { Bool editable = (send(file, NAME_access, NAME_write, EAV) ? ON : OFF);

    assign(e, file, file);
    send(e, NAME_editable, editable, EAV);
    CaretEditor(e, ZERO);
    CmodifiedTextBuffer(tb, OFF);
    resetUndoTextBuffer(tb);
  }

  return rval;
}


static status
saveEditor(Editor e, SourceSink file)
{ if ( isDefault(file) )
    file = e->file;

  if ( isNil(file) )
    fail;
  
  if ( instanceOfObject(file, ClassFile) &&
       existsFile((FileObj)file, DEFAULT) )
    TRY(send(file, NAME_backup, EAV));

  TRY(saveTextBuffer(e->text_buffer, file, DEFAULT, DEFAULT));
  assign(e, file, file);

  succeed;
}


		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
fontEditor(Editor e, FontObj font)
{ if ( e->font != font )
  { assign(e, font, font);
    setGraphical(e, DEFAULT, DEFAULT, e->size->w, e->size->h);
    updateStyleCursorEditor(e);
    ChangedEditor(e);
  }
  
  succeed;
}


static status
tabDistanceEditor(Editor e, Int tab)
{ if ( e->tab_distance != tab )
  { assign(e, tab_distance, tab);
    tabDistanceTextImage(e->image, mul(tab, getExFont(e->font)));
    ChangedEditor(e);
  }
  
  succeed;
}


static status
tabStopsEditor(Editor e, Vector v)
{ return tabStopsTextImage(e->image, v);	/* character -> pixels? */
}


static Vector
getTabStopsEditor(Editor e)
{ answer(e->image->tab_stops);
}


static status
wrapEditor(Editor e, Name wrap)
{ return send(e->image, NAME_wrap, wrap, EAV);
}


static Name
getWrapEditor(Editor e)
{ answer(e->image->wrap);
}

		/********************************
		*      CHANGE NOTIFICATIONS	*
		********************************/

static inline long
update_index_on_insert(long i, long w, long a)
{ if ( a > 0 )				/* insert */
    return i > w ? i+a : i;

  if ( i > w )				/* delete before i */
    return i > w-a ? i+a : w;

  return i;
}


static inline long
update_caret_on_insert(long i, long w, long a)
{ if ( a > 0 )				/* insert */
    return i >= w ? i+a : i;

  if ( i > w )				/* delete before i */
    return i > w-a ? i+a : w;

  return i;
}


static status
InsertEditor(Editor e, Int where, Int amount)
{ long w = valInt(where);
  long a = valInt(amount);

  assign(e, caret, toInt(update_caret_on_insert(valInt(e->caret), w, a)));
  assign(e, mark,  toInt(update_index_on_insert(valInt(e->mark),  w, a)));

#define UPDATE_C_INDEX(e, idx) \
  e->idx = update_index_on_insert(e->idx, w, a);

  UPDATE_C_INDEX(e, internal_mark);

#undef UPDATE_C_INDEX

  InsertTextImage(e->image, where, amount);
  if ( notNil(e->kill_location) )
    assign(e, kill_location, NIL);

  succeed;
}


static status
ChangedRegionEditor(Editor e, Int from, Int to)
{ Before(from, to);
  ChangedRegionTextImage(e->image, from, to);
  if ( notNil(e->kill_location) )
    assign(e, kill_location, NIL);

  succeed;
}


static status
ChangedFragmentListEditor(Editor e)
{ if ( notNil(e->selected_fragment) &&
       isFreeingObj(e->selected_fragment) ) /* HACK ... */
  { assign(e, selected_fragment, NIL);
    requestComputeGraphical(e->image, DEFAULT);
  }

  if ( notNil(e->margin) )
    requestComputeGraphical(e->margin, DEFAULT);

  resetFragmentCache(e->fragment_cache, e->text_buffer);

  succeed;
}


static status
ChangedEditor(Editor e)
{ ChangedRegionEditor(e, ZERO, getLengthEditor(e));

  succeed;
}

		 /*******************************
		 *	     DELEGATION		*
		 *******************************/

static status
referenceEditor(Editor e, Point ref)
{ return referenceGraphical((Graphical) e, ref);
}


		/********************************
		*            VISUAL		*
		********************************/

static Chain
getContainsEditor(Editor e)
{ fail;
}


static Any
getMasterEditor(Editor e)
{ if ( instanceOfObject(e->device, ClassView) )
    answer(e->device);

  answer(e);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_align[] =
        { "column=int", "index=[int]" };
static char *T_scrollVertical[] =
        { "direction={forwards,backwards,goto}",
	  "unit={file,page,line}", "amount=int" };
static char *T_showScrollBar[] =
        { "show=[bool]", "which=[scroll_bar]" };
static char *T_formatAchar_array_argumentAany_XXX[] =
        { "format=char_array", "argument=any ..." };
static char *T_style[] =
        { "fragment=name", "style=style*" };
static char *T_fromADintD_toADintD[] =
        { "from=[int]", "to=[int]" };
static char *T_fromAint_toAint[] =
        { "from=int", "to=int" };
static char *T_fill[] =
        { "from=int", "to=int", "left_margin=[int]",
	  "right_margin=[int]", "justify=[bool]" };
static char *T_indentation[] =
        { "index=[int]", "skip=[regex]" };
static char *T_electricCaret[] =
        { "index=int", "seconds=[real]" };
static char *T_int_int[] =
        { "int", "int" };
static char *T_keyBinding[] =
        { "key=name", "action=name|code" };
static char *T_selectLine[] =
        { "line=[int]", "newline=[bool]" };
static char *T_linesADintD_columnADintD[] =
        { "lines=[int]", "column=[int]" };
static char *T_initialise[] =
        { "text=[text_buffer]", "width=[int]", "height=[int]",
	  "margin=[int]" };
static char *T_timesADintD_characterADcharD[] =
        { "times=[int]", "character=[char]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_selection[] =
        { "mark=[int]", "caret=[int]",
	  "status=[{active,inactive,highlight}]"
	};
static char *T_mark[] =
        { "mark=[int]", "status=[{active,inactive,highlight}]"
	};

/* Instance Variables */

static vardecl var_editor[] =
{ SV(NAME_textBuffer, "text_buffer", IV_GET|IV_STORE, textBufferEditor,
     NAME_delegate, "Underlying text"),
  IV(NAME_image, "text_image", IV_GET,
     NAME_components, "Screen/redisplay management"),
  IV(NAME_scrollBar, "scroll_bar", IV_GET,
     NAME_components, "Scrollbar for the text"),
  IV(NAME_margin, "text_margin*", IV_GET,
     NAME_components, "Margin for annotations"),
  IV(NAME_textCursor, "text_cursor", IV_GET,
     NAME_components, "The caret"),
  IV(NAME_labelText, "text*", IV_GET,
     NAME_components, "Text object that displays the label"),
  SV(NAME_font, "font", IV_GET|IV_STORE, fontEditor,
     NAME_appearance, "Default font for the text"),
  IV(NAME_size, "size", IV_GET,
     NAME_area, "Size of editor in character units"),
  IV(NAME_caret, "int", IV_GET,
     NAME_caret, "0-based caret index"),
  IV(NAME_mark, "int", IV_GET,
     NAME_caret, "0-based mark index"),
  SV(NAME_markStatus, "{active,inactive,highlight}", IV_GET|IV_STORE,
     markStatusEditor,
     NAME_caret, "Status of the mark/region"),
  IV(NAME_markRing, "vector", IV_GET,
     NAME_caret, "Ring of old marks"),
  SV(NAME_tabDistance, "characters=int", IV_GET|IV_STORE, tabDistanceEditor,
     NAME_appearance, "Distance between tabs"),
  IV(NAME_selectionStyle, "[style]", IV_GET,
     NAME_appearance, "Feedback for the <-selection"),
  SV(NAME_selectedFragment, "fragment*", IV_GET|IV_STORE,
     selectedFragmentEditor,
     NAME_selection, "The current fragment"),
  SV(NAME_selectedFragmentStyle, "style", IV_GET|IV_STORE,
     selectedFragmentStyleEditor,
     NAME_appearance, "Style for the current fragment"),
  IV(NAME_styles, "sheet", IV_GET,
     NAME_appearance, "Style-name to style-object mapping"),
  IV(NAME_bindings, "key_binding", IV_BOTH,
     NAME_accelerator, "key_binding table"),
  IV(NAME_focusFunction, "name*", IV_BOTH,
     NAME_editContinue, "Method in focus of keystrokes"),
  IV(NAME_fillMode, "bool", IV_BOTH,
     NAME_mode, "If @on, automatically insert newlines"),
  IV(NAME_exactCase, "bool", IV_BOTH,
     NAME_case, "Search/replace uses exact case"),
  IV(NAME_killLocation, "int*", IV_NONE,
     NAME_internal, "Status for handling kill commands"),
  IV(NAME_searchDirection, "{forward,backward}", IV_NONE,
     NAME_internal, "Current direction of search"),
  IV(NAME_searchString, "string*", IV_BOTH,
     NAME_internal, "Current target of search"),
  IV(NAME_searchOrigin, "int", IV_NONE,
     NAME_internal, "Index where search started"),
  IV(NAME_searchBase, "int", IV_NONE,
     NAME_internal, "Index where last change was done"),
  IV(NAME_selectionUnit, "{character,word,line}", IV_BOTH,
     NAME_selection, "Multiclick processing for the selection"),
  SV(NAME_selectionOrigin, "int", IV_GET|IV_STORE, selectionOriginEditor,
     NAME_selection, "Where the selection started"),
  IV(NAME_editable, "bool", IV_BOTH,
     NAME_mode, "If @on, text may be changed by user"),
  IV(NAME_errorMessage, "code*", IV_BOTH,
     NAME_report, "What to do with feedback/errors"),
  IV(NAME_modifiedMessage, "code*", IV_BOTH,
     NAME_report, "Forward changes to <->modified"),
  IV(NAME_leftMargin, "int", IV_BOTH,
     NAME_fill, "Auto-indent margin width"),
  IV(NAME_rightMargin, "int", IV_BOTH,
     NAME_fill, "Auto-fill margin"),
  IV(NAME_indentIncrement, "int", IV_BOTH,
     NAME_indentation, "Indent/undent amount"),
  IV(NAME_autoNewline, "bool", IV_BOTH,
     NAME_mode, "If @on, append newline after ->append"),
  IV(NAME_file, "source_sink*", IV_GET,
     NAME_file, "Associated file"),
  IV(NAME_dabbrevTarget, "name*", IV_NONE,
     NAME_internal, "Dynamic abbreviation target"),
  IV(NAME_dabbrevReject, "chain*", IV_NONE,
     NAME_internal, "Rejected alternatives"),
  IV(NAME_dabbrevPos, "int*", IV_NONE,
     NAME_internal, "Caret index at start of dabbrev"),
  IV(NAME_dabbrevOrigin, "int*", IV_NONE,
     NAME_internal, "Caret index of start of target"),
  IV(NAME_internalMark, "alien:int", IV_NONE,
     NAME_internal, "Additional mark for internal use"),
  IV(NAME_fragmentCache, "alien:FragmentCache", IV_NONE,
     NAME_cache, "Cache to compute fragment attributes")
};

static senddecl send_editor[] =
{ SM(NAME_geometry, 4, T_geometry, geometryEditor,
     DEFAULT, "Resize the image"),
  SM(NAME_initialise, 4, T_initialise, initialiseEditor,
     DEFAULT, "Create from text_buffer, W, H and margin"),
  SM(NAME_requestGeometry, 4, T_geometry, requestGeometryEditor,
     DEFAULT, "Map size to character units"),
  SM(NAME_unlink, 0, NULL, unlinkEditor,
     DEFAULT, "Unlink from buffer, margin, etc."),
  SM(NAME_lostTextBuffer, 0, NULL, lostTextBufferEditor,
     DEFAULT, "<-text_buffer is being freed"),
  SM(NAME_keyBinding, 2, T_keyBinding, keyBindingEditor,
     NAME_accelerator, "Set a local key binding"),
  SM(NAME_style, 2, T_style, styleEditor,
     NAME_appearance, "Set style associated with name"),
  SM(NAME_styles, 1, "sheet", stylesEditor,
     NAME_appearance, "Associate new name --> style object map"),
  SM(NAME_tabStops, 1, "vector*", tabStopsEditor,
     NAME_appearance, "Set tab-stops (vector of pixels)"),
  SM(NAME_wrap, 1, "{none,character,word}", wrapEditor,
     NAME_appearance, "Wrap mode for long lines"),
  SM(NAME_Size, 1, "pixels=size", SizeEditor,
     NAME_area, "Set size in pixels (trap window resize)"),
  SM(NAME_backwardChar, 1, "[int]", backwardCharEditor,
     NAME_caret, "Move characters backward"),
  SM(NAME_backwardParagraph, 1, "[int]", backwardParagraphEditor,
     NAME_caret, "Move paragraphs backward"),
  SM(NAME_backwardSentence, 1, "[int]", backwardSentenceEditor,
     NAME_caret, "Move sentences backward"),
  SM(NAME_backwardTerm, 1, "[int]", backwardTermEditor,
     NAME_caret, "Move Prolog terms backward"),
  SM(NAME_backwardWord, 1, "[int]", backwardWordEditor,
     NAME_caret, "Move words backward"),
  SM(NAME_beginningOfLine, 1, "[int]", beginningOfLineEditor,
     NAME_caret, "Move lines backward"),
  SM(NAME_caret, 1, "index=[int]", caretEditor,
     NAME_caret, "Put the caret at 0-based index"),
  SM(NAME_column, 1, "column=int", columnEditor,
     NAME_caret, "Move caret to column at current line"),
  SM(NAME_endOfLine, 1, "[int]", endOfLineEditor,
     NAME_caret, "Move lines forward"),
  SM(NAME_exchangePointAndMark, 0, NULL, exchangePointAndMarkEditor,
     NAME_caret, "Exchange caret with mark"),
  SM(NAME_forwardChar, 1, "[int]", forwardCharEditor,
     NAME_caret, "Move characters forward"),
  SM(NAME_forwardParagraph, 1, "[int]", forwardParagraphEditor,
     NAME_caret, "Move paragraphs forward"),
  SM(NAME_forwardSentence, 1, "[int]", forwardSentenceEditor,
     NAME_caret, "Move sentences forward"),
  SM(NAME_forwardTerm, 1, "[int]", forwardTermEditor,
     NAME_caret, "Move Prolog terms forward"),
  SM(NAME_forwardWord, 1, "[int]", forwardWordEditor,
     NAME_caret, "Move words forward"),
  SM(NAME_nextLine, 2, T_linesADintD_columnADintD, nextLineEditor,
     NAME_caret, "Move lines downward; place caret at column"),
  SM(NAME_pointToBottomOfFile, 1, "[int]", pointToBottomOfFileEditor,
     NAME_caret, "Move to end of file"),
  SM(NAME_pointToBottomOfWindow, 1, "[int]", pointToBottomOfWindowEditor,
     NAME_caret, "Scroll caret to bottom of window"),
  SM(NAME_pointToMark, 0, NULL, pointToMarkEditor,
     NAME_caret, "Move to mark"),
  SM(NAME_pointToTopOfFile, 1, "[int]", pointToTopOfFileEditor,
     NAME_caret, "Move to start of file "),
  SM(NAME_pointToTopOfWindow, 1, "[int]", pointToTopOfWindowEditor,
     NAME_caret, "Move to 1st character of window"),
  SM(NAME_previousLine, 2, T_linesADintD_columnADintD, previousLineEditor,
     NAME_caret, "Move lines upward; place caret at column"),
  SM(NAME_setMark, 1, "[int]", setMarkEditor,
     NAME_caret, "Set mark at point"),
  SM(NAME_capitalisePreviousWord, 1, "[int]", capitalisePreviousWordEditor,
     NAME_case, "Capitalise n word before caret"),
  SM(NAME_capitaliseRegion, 0, NULL, capitaliseRegionEditor,
     NAME_case, "Capitalise words in region"),
  SM(NAME_capitaliseWord, 1, "[int]", capitaliseWordEditor,
     NAME_case, "Capitalise n words from caret"),
  SM(NAME_downcasePreviousWord, 1, "[int]", downcasePreviousWordEditor,
     NAME_case, "Lower-case n words before caret"),
  SM(NAME_downcaseRegion, 0, NULL, downcaseRegionEditor,
     NAME_case, "Put region in lower case"),
  SM(NAME_downcaseWord, 1, "[int]", downcaseWordEditor,
     NAME_case, "Loser-case n words after caret"),
  SM(NAME_toggleCharCase, 0, NULL, toggleCharCaseEditor,
     NAME_case, "Toggle case of character before caret"),
  SM(NAME_upcasePreviousWord, 1, "[int]", upcasePreviousWordEditor,
     NAME_case, "Uppercase n words before caret"),
  SM(NAME_upcaseRegion, 0, NULL, upcaseRegionEditor,
     NAME_case, "Put region in uppercase"),
  SM(NAME_upcaseWord, 1, "[int]", upcaseWordEditor,
     NAME_case, "Uppercase n words after caret"),
  SM(NAME_dabbrevExpand, 0, NULL, dabbrevExpandEditor,
     NAME_complete, "Dynamically expand word"),
  SM(NAME_backwardDeleteChar, 1, "[int]", backwardDeleteCharEditor,
     NAME_delete, "Delete characters backward"),
  SM(NAME_backwardKillWord, 1, "[int]", backwardKillWordEditor,
     NAME_delete, "Kill words backward"),
  SM(NAME_clear, 0, NULL, clearEditor,
     NAME_delete, "Clear the contents"),
  SM(NAME_delete, 2, T_fromAint_toAint, deleteEditor,
     NAME_delete, "Delete range [from, to)"),
  SM(NAME_deleteBlankLines, 0, NULL, deleteBlankLinesEditor,
     NAME_delete, "Delete blank lines around point"),
  SM(NAME_deleteChar, 1, "[int]", deleteCharEditor,
     NAME_delete, "Delete characters forward"),
  SM(NAME_copy, 0, NULL, copyEditor,
     NAME_selection, "Copy selection"),
  SM(NAME_cut, 0, NULL, cutEditor,
     NAME_selection, "Copy and delete selection"),
  SM(NAME_paste, 0, NULL, pasteEditor,
     NAME_selection, "Paste the clipboard value"),
  SM(NAME_cutOrDeleteChar, 1, "[int]", cutOrDeleteCharEditor,
     NAME_delete, "Cut selection or delete characters forward"),
  SM(NAME_cutOrBackwardDeleteChar, 1, "[int]", cutOrBackwardDeleteCharEditor,
     NAME_delete, "Cut selection or delete characters backward"),
  SM(NAME_deleteHorizontalSpace, 1, "[int]", deleteHorizontalSpaceEditor,
     NAME_delete, "Delete blanks around caret"),
  SM(NAME_deleteSelection, 0, NULL, deleteSelectionEditor,
     NAME_delete, "Delete the selection"),
  SM(NAME_grab, 2, T_fromAint_toAint, grabEditor,
     NAME_delete, "Add text to the kill-buffer"),
  SM(NAME_kill, 2, T_fromAint_toAint, killEditor,
     NAME_delete, "Delete text and add it to the kill-buffer"),
  SM(NAME_killLine, 1, "[int]", killLineEditor,
     NAME_delete, "Kill lines forward"),
  SM(NAME_killOrGrabRegion, 1, "[int]", killOrGrabRegionEditor,
     NAME_delete, "Kill or grab (with arg) region"),
  SM(NAME_killParagraph, 1, "[int]", killParagraphEditor,
     NAME_delete, "Kill paragraphs"),
  SM(NAME_killSentence, 1, "[int]", killSentenceEditor,
     NAME_delete, "Kill sentences forward"),
  SM(NAME_killTerm, 1, "[int]", killTermEditor,
     NAME_delete, "Kill Prolog terms"),
  SM(NAME_killWord, 1, "[int]", killWordEditor,
     NAME_delete, "Kill words forward"),
  SM(NAME_reference, 1, "point", referenceEditor,
     NAME_dialogItem, "Set reference as dialog_item"),
  SM(NAME_DabbrevExpand, 1, "event_id", DabbrevExpandEditor,
     NAME_editContinue, "Focus function"),
  SM(NAME_Isearch, 1, "event_id", IsearchEditor,
     NAME_editContinue, "Focus function"),
  SM(NAME_StartIsearch, 1, "event_id", StartIsearchEditor,
     NAME_editContinue, "Focus function"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, succeedObject,
     NAME_event, "Test if ready to accept input (true)"),
  SM(NAME_event, 1, "event", eventEditor,
     NAME_event, "Handle a general event"),
  SM(NAME_label, 1, "name", labelEditor,
     NAME_label, "Set the name of the label"),
  SM(NAME_load, 1, "file=source_sink", loadEditor,
     NAME_file, "Clear editor and load a file"),
  SM(NAME_save, 1, "file=[file]", saveEditor,
     NAME_file, "Save to current or given file"),
  SM(NAME_saveBuffer, 1, "always=[int]", saveBufferEditor,
     NAME_file, "Save to current <-file"),
  SM(NAME_fill, 5, T_fill, fillEditor,
     NAME_fill, "Fill (from, to) using left- and rightmargin [justify]"),
  SM(NAME_fillParagraph, 1, "justify=[int]", fillParagraphEditor,
     NAME_fill, "Fill paragraph around point"),
  SM(NAME_fillRegion, 0, NULL, fillRegionEditor,
     NAME_fill, "Fill paragraphs in region"),
  SM(NAME_fillSelection, 0, NULL, fillSelectionEditor,
     NAME_fill, "Fill paragraphs in selection"),
  SM(NAME_justifyParagraph, 0, NULL, justifyParagraphEditor,
     NAME_fill, "Justify paragraph around point"),
  SM(NAME_justifyRegion, 0, NULL, justifyRegionEditor,
     NAME_fill, "Justify region"),
  SM(NAME_setFillColumn, 1, "[int]", setFillColumnEditor,
     NAME_fill, "Set fill column to argument"),
  SM(NAME_appendf, 2, T_formatAchar_array_argumentAany_XXX, appendfEditor,
     NAME_format, "Formatted append (see `string ->format')"),
  SM(NAME_format, 2, T_formatAchar_array_argumentAany_XXX, formatEditor,
     NAME_format, "Formatted insert (see `string ->format')"),
  SM(NAME_align, 2, T_align, alignEditor,
     NAME_indentation, "Align here [caret] to indicated column"),
  SM(NAME_alignLine, 1, "column=[int]", alignLineEditor,
     NAME_indentation, "Align line to argument"),
  SM(NAME_alignRegion, 1, "column=[int]", alignRegionEditor,
     NAME_indentation, "Align region to argument"),
  SM(NAME_indentLine, 1, "[int]", indentLineEditor,
     NAME_indentation, "Indent line by <-indent_increment"),
  SM(NAME_indentRegion, 1, "[int]", indentRegionEditor,
     NAME_indentation, "Indent lines in region by <-indent_increment"),
  SM(NAME_indentSelection, 1, "[int]", indentSelectionEditor,
     NAME_indentation, "Indent lines in selection by <-indent_increment"),
  SM(NAME_newlineAndIndent, 1, "[int]", newlineAndIndentEditor,
     NAME_indentation, "Start a newline and indent"),
  SM(NAME_undentLine, 1, "[int]", undentLineEditor,
     NAME_indentation, "Unindent line by <-indent_increment"),
  SM(NAME_undentRegion, 1, "[int]", undentRegionEditor,
     NAME_indentation, "Unindent lines in region by <-indent_increment"),
  SM(NAME_undentSelection, 1, "[int]", undentSelectionEditor,
     NAME_indentation, "Unindent lines in selection by <-indent_increment"),
  SM(NAME_append, 1, "text=char_array", appendEditor,
     NAME_insert, "Append text (left_margin, auto_newline)"),
  SM(NAME_insert, 1, "text=char_array", insertEditor,
     NAME_insert, "Insert text at caret (moves caret)"),
  SM(NAME_insertCutBuffer, 1, "[int]", insertCutBufferEditor,
     NAME_insert, "Insert value of X cut-buffer"),
  SM(NAME_insertQuoted, 2, T_timesADintD_characterADcharD, insertQuotedEditor,
     NAME_insert, "Insert typed character n times (no fill)"),
  SM(NAME_insertSelf, 2, T_timesADintD_characterADcharD, insertSelfEditor,
     NAME_insert, "Insert typed character n times"),
  SM(NAME_insertSelfFill, 2, T_timesADintD_characterADcharD, insertSelfEditor,
     NAME_insert, "Insert char n times; adjust margins"),
  SM(NAME_autoFill, 1, "from=[int]", autoFillEditor,
     NAME_insert, "Fill after ->insert_self_fill detected a long line"),
  SM(NAME_newline, 1, "[int]", newlineEditor,
     NAME_insert, "Insert newlines"),
  SM(NAME_openLine, 1, "[int]", openLineEditor,
     NAME_insert, "Insert newlines after caret"),
  SM(NAME_print, 1, "text=string", printEditor,
     NAME_insert, "Insert text at caret (auto_newline)"),
  SM(NAME_typed, 1, "event|event_id", typedEditor,
     NAME_insert, "Process a keystroke"),
  SM(NAME_showLabel, 1, "show=bool", showLabelEditor,
     NAME_appearance, "Show/unshow the label"),
  SM(NAME_yank, 1, "[int]", yankEditor,
     NAME_insert, "Yank current kill-buffer"),
  SM(NAME_justOneSpace, 0, NULL, justOneSpaceEditor,
     NAME_layout, "Replace blanks with a single space"),
  SM(NAME_deleteLine, 1, "line=int", deleteLineEditor,
     NAME_line, "Delete given line number"),
  SM(NAME_lineNumber, 1, "line=int", lineNumberEditor,
     NAME_line, "Move caret to start of line"),
  SM(NAME_replaceLine, 1, "text=char_array", replaceLineEditor,
     NAME_line, "Replace given line number by string"),
  SM(NAME_autoFillMode, 1, "[int]", autoFillModeEditor,
     NAME_mode, "Toggle auto_fill mode"),
  SM(NAME_switchCaseMode, 1, "[int]", switchCaseModeEditor,
     NAME_mode, "Toggle exact/either case search"),
  SM(NAME_ChangedFragmentList, 0, NULL, ChangedFragmentListEditor,
     NAME_repaint, "Notify change in text_buffers fragment list"),
  SM(NAME_ChangedRegion, 2, T_int_int, ChangedRegionEditor,
     NAME_repaint, "Notify change (start, length)"),
  SM(NAME_InsertEditor, 2, T_int_int, InsertEditor,
     NAME_repaint, "Notify insert (start, length)"),
  SM(NAME_compute, 0, NULL, computeEditor,
     NAME_repaint, "Recompute the editor"),
  SM(NAME_electricCaret, 2, T_electricCaret, electricCaretEditor,
     NAME_report, "Temporary display caret at location"),
  SM(NAME_report, 3, T_report, reportEditor,
     NAME_report, "Report message (using <-error_message)"),
  SM(NAME_showCaretAt, 1, "index=[int]", showCaretAtEditor,
     NAME_report, "Display caret at indicated position"),
  SM(NAME_showCaret, 1, "show=bool", showCaretEditor,
     NAME_caret, "Show/hide the caret"),
  SM(NAME_showMatchingBracket, 1, "index=[int]", showMatchingBracketEditor,
     NAME_report, "->electric_caret on matching bracket"),
  SM(NAME_undefined, 0, NULL, undefinedEditor,
     NAME_report, "Warn binding is not defined"),
  SM(NAME_keyboardQuit, 0, NULL, keyboardQuitEditor,
     NAME_reset, "Cancel current operation"),
  SM(NAME_lineToTopOfWindow, 1, "[int]", lineToTopOfWindowEditor,
     NAME_scroll, "Scroll caret to top of window"),
  SM(NAME_normalise, 2, T_fromADintD_toADintD, normaliseEditor,
     NAME_scroll, "Try to make range visible"),
  SM(NAME_recenter, 1, "[int]", recenterEditor,
     NAME_scroll, "Scroll caret to center of window"),
  SM(NAME_scrollDown, 1, "[int]", scrollDownEditor,
     NAME_scroll, "Scroll lines (1 screen) downward"),
  SM(NAME_scrollOneLineDown, 1, "[int]", scrollOneLineDownEditor,
     NAME_scroll, "Scroll lines (1 line) downward"),
  SM(NAME_scrollOneLineUp, 1, "[int]", scrollOneLineUpEditor,
     NAME_scroll, "Scroll lines (1 line) upward"),
  SM(NAME_scrollTo, 1, "index=[int]", scrollToEditor,
     NAME_scroll, "Set start of window to index"),
  SM(NAME_scrollUp, 1, "[int]", scrollUpEditor,
     NAME_scroll, "Scroll lines (1 screen) upward"),
  SM(NAME_scrollVertical, 3, T_scrollVertical, scrollVerticalEditor,
     NAME_scroll, "Trap scroll_bar request"),
  SM(NAME_showScrollBar, 2, T_showScrollBar, showScrollBarEditor,
     NAME_scroll, "Control visibility of the <-scroll_bar"),
  SM(NAME_bubbleScrollBar, 1, "scroll_bar", bubbleScrollBarEditor,
     NAME_scroll, "Update bubble of given scroll_bar object"),
  SM(NAME_findCutBuffer, 1, "[int]", findCutBufferEditor,
     NAME_search, "Find string in X-cut buffer"),
  SM(NAME_isearchBackward, 0, NULL, isearchBackwardEditor,
     NAME_search, "Start incremental search backward"),
  SM(NAME_isearchForward, 0, NULL, isearchForwardEditor,
     NAME_search, "Start incremental search forward"),
  SM(NAME_internalMark, 1, "[int]", internalMarkEditor,
     NAME_caret, "Mark for program-use"),
  SM(NAME_selectLine, 2, T_selectLine, selectLineEditor,
     NAME_selection, "Select given line number"),
  SM(NAME_selection, 3, T_selection, selectionEditor,
     NAME_selection, "Make [from, to) the selection"),
  SM(NAME_mark, 2, T_mark, markEditor,
     NAME_selection, "Set mark and region-status"),
  SM(NAME_selectionExtend, 1, "to=int", selectionExtendEditor,
     NAME_selection, "Extend the selection"),
  SM(NAME_selectionToCutBuffer, 1, "buffer=[0..9]", selectionToCutBufferEditor,
     NAME_selection, "Copy the selection to an X-cut buffer"),
  SM(NAME_sort, 2, T_fromADintD_toADintD, sortEditor,
     NAME_sort, "Sort range [from, to) by line"),
  SM(NAME_gosmacsTranspose, 0, NULL, gosmacsTransposeEditor,
     NAME_transpose, "Transpose characters before caret"),
  SM(NAME_transposeChars, 0, NULL, transposeCharsEditor,
     NAME_transpose, "Transpose characters around caret"),
  SM(NAME_transposeLines, 0, NULL, transposeLinesEditor,
     NAME_transpose, "Transpose line with line above"),
  SM(NAME_transposeTerms, 0, NULL, transposeTermsEditor,
     NAME_transpose, "Transpose Prolog terms around point"),
  SM(NAME_transposeWord, 0, NULL, transposeWordEditor,
     NAME_transpose, "Transpose words around caret"),
  SM(NAME_undo, 0, NULL, undoEditor,
     NAME_undo, "Undo last interactive command"),
  SM(NAME_marginWidth, 1, "pixels=int", marginWidthEditor,
     NAME_visualisation, "Set width of annotation margin"),

  SM(NAME_cursorUp, 2, T_linesADintD_columnADintD, cursorUpEditor,
     NAME_event, "Handle cursor up-arrow"),
  SM(NAME_cursorDown, 2, T_linesADintD_columnADintD, cursorDownEditor,
     NAME_event, "Handle cursor down-arrow"),
  SM(NAME_cursorLeft, 1, "[int]", cursorLeftEditor,
     NAME_event, "Handle cursor left-arrow"),
  SM(NAME_cursorRight, 1, "[int]", cursorRightEditor,
     NAME_event, "Handle cursor right-arrow"),
  SM(NAME_cursorEnd, 1, "[int]", cursorEndEditor,
     NAME_event, "Handle 'end'-key"),
  SM(NAME_cursorHome, 1, "[int]", cursorHomeEditor,
     NAME_event, "Handle 'home'-key"),
  SM(NAME_cursorPageUp, 1, "[int]", cursorPageUpEditor,
     NAME_event, "Handle 'page-up'-key"),
  SM(NAME_cursorPageDown, 1, "[int]", cursorPageDownEditor,
     NAME_event, "Handle 'page-down'-key")
};

/* Get Methods */

static getdecl get_editor[] =
{ GM(NAME_contains, 0, "visual", NULL, getContainsEditor,
     DEFAULT, "Visuals contained (fails)"),
  GM(NAME_convert, 1, "editor", "view", getConvertEditor,
     DEFAULT, "The `view <-editor'"),
  GM(NAME_master, 0, "editor|view", NULL, getMasterEditor,
     DEFAULT, "Principal visual I'm part of (self or view)"),
  GM(NAME_keyBinding, 1, "action=name|code", "key=name|event_id",
     getKeyBindingEditor,
     NAME_accelerator, "Function for specified key"),
  GM(NAME_tabStops, 0, "vector*", NULL, getTabStopsEditor,
     NAME_appearance, "Vector with tab-stop positions in pixels (or @nil)"),
  GM(NAME_wrap, 0, "{none,character,word}", NULL, getWrapEditor,
     NAME_appearance, "Wrap mode for long lines"),
  GM(NAME_height, 0, "character=int", NULL, getHeightEditor,
     NAME_area, "Height in character units"),
  GM(NAME_size, 0, "characters=size", NULL, getSizeEditor,
     NAME_area, "Size in character units"),
  GM(NAME_width, 0, "characters=int", NULL, getWidthEditor,
     NAME_area, "Width in character units"),
  GM(NAME_column, 1, "column=0..", "index=[int]", getColumnEditor,
     NAME_caret, "Column point is at"),
  GM(NAME_upDownColumn, 0, "column=int", NULL, getUpDownColumnEditor,
     NAME_caret, "Saved X-infor for ->cursor_up/->cursor_down"),
  GM(NAME_indentation, 2, "column=int", T_indentation, getIndentationEditor,
     NAME_indentation, "Column of first non-blank character"),
  GM(NAME_FetchFunction, 0, "alien:FetchFunction", NULL,
     getFetchFunctionEditor,
     NAME_internal, "Pointer to C-function to fetch char"),
  GM(NAME_MarginFunction, 0, "alien:MarginFunction", NULL,
     getMarginFunctionEditor,
     NAME_internal, "Pointer to C-function to fetch margins"),
  GM(NAME_RewindFunction, 0, "alien:RewindFunction", NULL,
     getRewindFunctionEditor,
     NAME_internal, "Pointer to C-function to rewind object"),
  GM(NAME_ScanFunction, 0, "alien:ScanFunction", NULL, getScanFunctionEditor,
     NAME_internal, "Pointer to C-function to scan for char-type"),
  GM(NAME_SeekFunction, 0, "alien:SeekFunction", NULL, getSeekFunctionEditor,
     NAME_internal, "Pointer to C-function to seek to position"),
  GM(NAME_lineNumber, 1, "line=int", "index=[int]", getLineNumberEditor,
     NAME_line, "Line number point is at"),
  GM(NAME_firstLine, 0, "line=string", NULL, getFirstLineEditor,
     NAME_read, "New string with text of first window line"),
  GM(NAME_line, 1, "line=string", "index=[int]", getLineEditor,
     NAME_read, "New string with text of current line"),
  GM(NAME_readLine, 0, "line=string", NULL, getReadLineEditor,
     NAME_read, "As <-line, moves caret to next"),
  GM(NAME_word, 1, "word=string", "index=[int]", getWordEditor,
     NAME_read, "New string with text of current word"),
  GM(NAME_first, 0, "line=int", NULL, getFirstEditor,
     NAME_scroll, "Line-number (1-based) of the first line on the window"),
  GM(NAME_length, 0, "int", NULL, getLengthEditor,
     NAME_scroll, "Length of contents (# characters)"),
  GM(NAME_linesVisible, 0, "point", NULL, getLinesVisibleEditor,
     NAME_scroll, "New point with first and last visible line"),
  GM(NAME_start, 1, "int", "[int]", getStartEditor,
     NAME_scroll, "Start of nth-1 visible line (for scroll_bar)"),
  GM(NAME_view, 0, "int", NULL, getViewEditor,
     NAME_scroll, "Length of view (for scroll_bar)"),
  GM(NAME_selected, 0, "string", NULL, getSelectedEditor,
     NAME_selection, "New string with contents of selection"),
  GM(NAME_selection, 0, "point", NULL, getSelectionEditor,
     NAME_selection, "New point with start and end of selection"),
  GM(NAME_selectionEnd, 0, "int", NULL, getSelectionEndEditor,
     NAME_selection, "Index for end of selection"),
  GM(NAME_selectionStart, 0, "int", NULL, getSelectionStartEditor,
     NAME_selection, "Index for start of selection"),
  GM(NAME_internalMark, 0, "int", NULL, getInternalMarkEditor,
     NAME_caret, "Mark for program use"),
  GM(NAME_showLabel, 0, "bool", NULL, getShowLabelEditor,
     NAME_appearance, "Bool indicating if label is visible"),
  GM(NAME_marginWidth, 0, "pixels=int", NULL, getMarginWidthEditor,
     NAME_visualisation, "Width of annotation margin")
};

/* Resources */

static classvardecl rc_editor[] =
{ RC(NAME_background, "colour|pixmap", "white",
     "Colour/fill pattern of the background"),
  RC(NAME_caretModifier, "modifier", "",
     "Modify caret using this modifier"),
  RC(NAME_cursor, "cursor", UXWIN("xterm", "win_ibeam"),
     "Default cursor"),
  RC(NAME_exactCase, "bool", "@off",
     "Search/replace case"),
  RC(NAME_fillMode, "bool", "@off",
     "If @on, auto-fill"),
  RC(NAME_font, "font", "fixed",
     "Default font"),
  RC(NAME_labelFont, "font", "bold",
     "Font used to display the label"),
  RC(NAME_indentIncrement, "int", "2",
     "Indent/undent amount"),
  RC(NAME_isearchStyle, "style",
     UXWIN("when(@colour_display,\n"
	   "     style(background := green),\n"
	   "     style(background:= @grey25_image))",
	   "@_isearch_style"),
     "Style for incremental search"),
  RC(NAME_keyBinding, "string", "",
     "`Key = selector' binding list"),
  RC(NAME_pen, "0..", UXWIN("0", "1"),
     "Thickness of box around editor"),
  RC(NAME_rightMargin, "int", "72",
     "Auto-fill margin width"),
  RC(NAME_selectModifier, "modifier", "s",
     "Modify selection using this modifier"),
  RC(NAME_selectionStyle, "[style]",
     UXWIN("when(@colour_display,\n"
	   "     style(background := yellow),\n"
	   "     style(highlight := @on))",
	   "@_select_style"),
     "Style for <-selection"),
  RC(NAME_insertDeletesSelection, "bool", "@on",
     "->insert_self and ->paste delete the selection"),
  RC(NAME_caretMovesOnSelect, "bool", "@on",
     "The caret is moved if a selection is made"),
  RC(NAME_autoCopy, "bool", "@on",
     "Automatically copy selected text to the clipboard"),
  RC(NAME_showOpenBracket, "bool", "@on",
     "Show open-bracket when inserting close-bracket"),
  RC(NAME_size, "size", "size(40,20)",
     "Default size in `characters x lines'"),
  RC(NAME_tabDistance, "int", "8",
     "Distance between tab stops (characters)")
};

/* Class Declaration */

static Name editor_termnames[] =
	{ NAME_textBuffer, NAME_width, NAME_height, NAME_marginWidth };

ClassDecl(editor_decls,
          var_editor, send_editor, get_editor, rc_editor,
          4, editor_termnames,
          "$Rev$");

status
makeClassEditor(Class class)
{ declareClass(class, &editor_decls);

  setCloneFunctionClass(class, cloneEditor);
  setLoadStoreFunctionClass(class, loadFdEditor, storeEditor);
  setRedrawFunctionClass(class, RedrawAreaEditor);
  delegateClass(class, NAME_textBuffer);
  delegateClass(class, NAME_margin);

  succeed;
}

		/********************************
		*            KILLING		*
		********************************/

static Vector
TextKillRing(void)
{ static Vector	ring;			/* @text_kill_ring */

  if ( !ring )
  { ring = globalObject(NAME_textKillRing, ClassVector, EAV);
    fillVector(ring, NIL, ZERO, toInt(9));
  }

  return ring;
}


static status
appendKill(CharArray ca)
{ Vector ring = TextKillRing();
  CharArray old = getElementVector(ring, ZERO);

  if ( isNil(old) )
    elementVector(ring, ZERO, ca);
  else
    elementVector(ring, ZERO, getAppendCharArray(old, ca));

  succeed;
}


static status
prependKill(CharArray ca)
{ Vector ring = TextKillRing();
  CharArray old = getElementVector(ring, ZERO);

  if ( isNil(old) )
    elementVector(ring, ZERO, ca);
  else
    elementVector(ring, ZERO, getAppendCharArray(ca, old));

  succeed;
}


static status
newKill(CharArray ca)
{ Vector ring = TextKillRing();

  shiftVector(ring, ONE);
  elementVector(ring, ZERO, ca);

  succeed;
}


static CharArray
killRegister(Int which)
{ CharArray ca;

  if ( isDefault(which) )
    which = ZERO;

  if ( TextKillRing &&
       (ca = getElementVector(TextKillRing(), 0)) &&
       notNil(ca) )
    answer(ca);

  fail;
}
