/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status	initOffsetText(TextObj, int);
static status	initPositionText(TextObj);
static status	initAreaText(TextObj);
static status	recomputeText(TextObj t, Name what);
static status	get_char_pos_text(TextObj t, Int chr, int *X, int *Y);
static status	caretText(TextObj t, Int where);
static status	prepareEditText(TextObj t, Name selector);

#define Wrapped(t)      ((t)->wrap == NAME_wrap || \
			 (t)->wrap == NAME_wrapFixedWidth)
#define Before(x, y)    if ( valInt(x) > valInt(y) ) { Int _z = x; x=y; y=_z; }
#define MakeSel(f, t)   toInt(((valInt(t) & 0xffff) << 16) | \
			      (valInt(f) & 0xffff))
#define GetSel(s,f,t)	{ *(t) = ((valInt(s) >> 16) & 0xffff); \
			  *(f) = (valInt(s) & 0xffff); \
			}



		/********************************
		*            CREATE		*
		********************************/

static status
initialiseText(TextObj t, CharArray string, Name format, FontObj font)
{ if ( isDefault(string) )
    string = CtoCharArray("");

  initialiseGraphical(t, ZERO, ZERO, ZERO, ZERO);

  if ( notDefault(format) )
    assign(t, format,     format);
  if ( notDefault(font) )
    assign(t, font,       font);
  assign(t, underline,	  OFF);
  assign(t, string,       string);
  assign(t, margin,	  toInt(100));
  assign(t, wrap,         NAME_extend);
  assign(t, position,     newObject(ClassPoint, EAV));
  assign(t, caret,        getSizeCharArray(string));
  assign(t, show_caret,   OFF);
  assign(t, background,   NIL);
  assign(t, x_offset,	  ZERO);
  assign(t, x_caret,	  ZERO);
  assign(t, y_caret,	  ZERO);
  assign(t, selection,	  NIL);

  return recomputeText(t, NAME_position);
}

		/********************************
		*            COMPUTE		*
		********************************/

static status
computeText(TextObj t)
{ if ( notNil(t->request_compute) )
  { obtainClassVariablesObject(t);

    CHANGING_GRAPHICAL(t,
	if ( t->request_compute == NAME_position )
	  initPositionText(t);
	else if ( t->request_compute == NAME_area )
	  initAreaText(t);
	changedEntireImageGraphical(t));

    assign(t, request_compute, NIL);
  }

  succeed;
}


static status
recomputeText(TextObj t, Name what)
{ if ( notNil(t->selection) )		/* normalise the selection */
  { int from, to;
    int size = t->string->data.size;

    GetSel(t->selection, &from, &to);
    if ( from > size || to > size )
    { if ( from > size ) from = size;
      if ( from > size ) to = size;

      assign(t, selection, MakeSel(toInt(from), toInt(to)));
    }
  }

  if ( notNil(t->request_compute) && t->request_compute != what )
    computeText(t);

  return requestComputeGraphical(t, what);
}



		/********************************
		*            REDRAW		*
		********************************/

static status
RedrawAreaText(TextObj t, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(t, &x, &y, &w, &h);

  repaintText(t, x, y, w, h);
  if ( t->pen != ZERO )
  { r_thickness(valInt(t->pen));
    r_dash(t->texture);
    r_box(x, y, w, h, 0, NIL);
  }

  return RedrawAreaGraphical(t, a);
}


void
str_format(String out, const String in, const int width, const FontObj font)
{ int x = 0;
  int last_is_layout = TRUE;

  if ( isstr8(in) )			/* 8-bit string */
  { char8  *s = in->s_text8;
    char8  *e = &s[in->size];
    char8  *o = out->s_text8;
    char8 *lb = NULL;			/* last-break; */

    for(;; s++)
    { *o++ = *s;

      if ( s == e )
      { out->size = o - out->s_text8 - 1;
	return;
      }

      if ( !last_is_layout && islayout(*s) )
	lb = o-1;
      last_is_layout = islayout(*s);

      if ( *s == '\n' )
	x = 0;
      else
	x += c_width(*s, font);

      if ( x > width )
      { if ( lb )
	{ s -= o-lb-2;
	  while( islayout(*s) )
	    s++;
	  s--;
	  o = lb;
	  *o++ = '\n';
	  lb = NULL;
	  x = 0;
	}
      }
    }
  } else				/* 16-bit string */
  { char16  *s = in->s_text16;
    char16  *e = &s[in->size];
    char16  *o = out->s_text16;
    char16 *lb = NULL;			/* last-break; */

    for(;; s++)
    { *o++ = *s;

      if ( s == e )
      { out->size = o - out->s_text16 - 1;
	return;
      }

      if ( !last_is_layout && islayout(*s) )
	lb = o-1;
      last_is_layout = islayout(*s);

      if ( *s == '\n' )
	x = 0;
      else
	x += c_width(*s, font);

      if ( x > width )
      { if ( lb )
	{ s -= o-lb-2;
	  while( islayout(*s) )
	    s++;
	  s--;
	  o = lb;
	  *o++ = '\n';
	  lb = NULL;
	  x = 0;
	}
      }
    }
  }
}


static void
str_one_line(String to, String from)
{ int n;

  for(n=0; n<from->size; n++)
  { unsigned int c = str_fetch(from, n);
	
    if      ( c == '\n' ) c = 0xb6;	/* Paragraph sign */
    else if ( c == '\t' ) c = 0xbb;	/* >> */
    else if ( c == '\r' ) c = 0xab;	/* << */
    str_store(to, n, c);
  }

  to->size = from->size;
}


static void
draw_caret(int x, int y, int w, int h, int active)
{ if ( active )
  { int cx = x + w/2;

    r_fillpattern(BLACK_IMAGE, NAME_foreground);
    r_fill_triangle(cx, y, x, y+h, x+w, y+h);
  } else
  { ipoint pts[4];
    int cx = x + w/2;

    int cy = y + h/2;
    int i = 0;

    pts[i].x = cx;  pts[i].y = y;   i++;
    pts[i].x = x;   pts[i].y = cy;  i++;
    pts[i].x = cx;  pts[i].y = y+h; i++;
    pts[i].x = x+w; pts[i].y = cy;  i++;
      
    r_fillpattern(GREY50_IMAGE, NAME_foreground);
    r_fill_polygon(pts, i);
  }
}


#ifndef OL_CURSOR_SIZE
#define OL_CURSOR_SIZE	9
#endif

status
repaintText(TextObj t, int x, int y, int w, int h)
{ String s = &t->string->data;
  int b = valInt(t->border);
  int sf, st;
  int flags = 0;
  Style style = NIL;

  if ( notNil(t->background) )
  { if ( isDefault(t->background) )
      r_clear(x, y, w, h);
    else
      r_fill(x, y, w, h, t->background);
  }

  if ( t->underline == ON )
    flags |= TXT_UNDERLINED;

  x += b;
  y += b;
  w -= 2*b;
  h -= 2*b;

  if ( t->wrap == NAME_clip )
    d_clip(x, y, w, h);

  if ( notNil(t->selection) )
  { GetSel(t->selection, &sf, &st);
    style = getClassVariableValueObject(t, NAME_selectionStyle);
  }

  if ( Wrapped(t) )
  { LocalString(buf, s, s->size + MAX_WRAP_LINES);

    str_format(buf, s, valInt(t->margin), t->font);
    str_string(buf, t->font,
	       x+valInt(t->x_offset), y, w, h,
	       t->format, NAME_top, flags);
  } else
  { if ( t->wrap == NAME_clip )
    { LocalString(buf, s, s->size);

      str_one_line(buf, s);
      s = buf;
    }
    if ( notNil(t->selection) )
    { str_selected_string(s, t->font, sf, st, style,
			  x+valInt(t->x_offset), y, w, h,
			  t->format, NAME_top);
    } else
    { str_string(s, t->font,
		 x+valInt(t->x_offset), y, w, h,
		 t->format, NAME_top, flags);
    }
  }

  if ( t->wrap == NAME_clip )
    d_clip_done();

  if ( t->show_caret != OFF )
  { int fh = valInt(getAscentFont(t->font));
    int active = (t->show_caret == ON);
    Any colour = getClassVariableValueClass(ClassTextCursor,
					    active ? NAME_colour
					           : NAME_inactiveColour);
    Any old = r_colour(colour);

    draw_caret(valInt(t->x_caret) - OL_CURSOR_SIZE/2 + x - b,
	       valInt(t->y_caret) + y + fh - b - 3,
	       OL_CURSOR_SIZE, OL_CURSOR_SIZE,
	       active);

    r_colour(old);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To avoid texts moving  on type actions  a position is  maintained.  If
the string changes  the area is recomputed  relative to  this position
argument.  If the text gets a `change area'  request it will recompute
the position attribute to be used in subsequent string changes.

initAreaText()		recomputes the area from the position after the
			string or font has been changed.
initPositionText()	recomputes the position from the area after the
			area has been changed from outside.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initAreaText(TextObj t)
{ int tw, x, y, w, h;
  Point pos = t->position;
  String s = &t->string->data;
  int size = s->size;
  int b = valInt(t->border);

  if ( valInt(t->caret) < 0 )
    assign(t, caret, ZERO);
  if ( valInt(t->caret) > size )
    assign(t, caret, toInt(size));

  if ( Wrapped(t) )
  { LocalString(buf, s, s->size + MAX_WRAP_LINES);

    str_format(buf, s, valInt(t->margin), t->font);
    str_size(buf, t->font, &tw, &h);
    if ( t->wrap == NAME_wrapFixedWidth && tw < valInt(t->margin) )
      tw = valInt(t->margin);
  } else
  { if ( t->wrap == NAME_clip )
    { LocalString(buf, s, s->size);
      
      str_one_line(buf, s);
      s = buf;
    }
    str_size(s, t->font, &tw, &h);
  }

  if ( t->wrap == NAME_clip )
    w = valInt(t->area->w) - 2*b;
  else
    w = tw;

  if ( t->format == NAME_right )
  { x = valInt(pos->x) - w;
    y = valInt(pos->y);
  } else if ( t->format == NAME_center )
  { x = valInt(pos->x) - w/2;
    y = valInt(pos->y) - h/2;
  } else
  { x = valInt(pos->x);
    y = valInt(pos->y);
  }

  x -= b; y -= b; w += 2*b; h+= 2*b;

  assign(t->area, x, toInt(x));
  assign(t->area, y, toInt(y));
  assign(t->area, w, toInt(w));
  assign(t->area, h, toInt(h));

  return initOffsetText(t, tw);
}


static status
initPositionText(TextObj t)
{ int tw, x, y, w, h;
  Point pos = t->position;
  String s = &t->string->data;
  int b = valInt(t->border);

  if ( Wrapped(t) )
  { LocalString(buf, s, s->size + MAX_WRAP_LINES);

    str_format(buf, s, valInt(t->margin), t->font);
    str_size(buf, t->font, &tw, &h);
    if ( t->wrap == NAME_wrapFixedWidth && tw < valInt(t->margin) )
      tw = valInt(t->margin);
  } else
  { if ( t->wrap == NAME_clip )
    { LocalString(buf, s, s->size);

      str_one_line(buf, s);
      s = buf;
    }
    str_size(s, t->font, &tw, &h);
  }

  if ( t->wrap == NAME_clip )
    w = valInt(t->area->w) - 2*b;
  else
    w = tw;

  if ( equalName(t->format, NAME_left) )
  { x = valInt(t->area->x);
    y = valInt(t->area->y) + b;
  } else if ( equalName(t->format, NAME_right) )
  { x = valInt(t->area->x) + w;
    y = valInt(t->area->y) + b;
  } else
  { x = valInt(t->area->x) + w/2;
    y = valInt(t->area->y) + h/2;
  }

  x += b;
  y += b;				/* was missing? */
  w += 2*b;
  h += 2*b;

  assign(pos, x, toInt(x));
  assign(pos, y, toInt(y));
  assign(t->area, w, toInt(w));
  assign(t->area, h, toInt(h));

  return initOffsetText(t, tw);
}


static int
initOffsetText(TextObj t, int tw)
{ if ( t->wrap != NAME_clip )
  { int x, y;

    assign(t, x_offset, ZERO);
    get_char_pos_text(t, DEFAULT, &x, &y);
    assign(t, x_caret, toInt(x));
    assign(t, y_caret, toInt(y));
  } else
  { int x, y, w = valInt(t->area->w) - valInt(t->border), shift;
    int xoff;

    if ( tw <= w || t->caret == ZERO )
      assign(t, x_offset, ZERO);
    else if ( t->caret == getSizeCharArray(t->string) )
      assign(t, x_offset, toInt(w - tw));

    xoff = valInt(t->x_offset);

    get_char_pos_text(t, DEFAULT, &x, &y);
    if ( x >= w )
      shift = w - x;
    else if ( x < 0 )
      shift = - x;
    else
      shift = 0;

    if ( shift )
    { xoff += shift;
      x += shift;
      assign(t, x_offset, toInt(xoff));
    }
    assign(t, x_caret, toInt(x));
    assign(t, y_caret, toInt(y));
  }

  succeed;
}


static status
resizeText(TextObj t, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(t->position->x);
  int oy = valInt(t->position->y);
  int nx, ny;

  init_resize_graphical(t, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  nx = ox + rfloat((float) (valInt(t->position->x)-ox) * xf);
  ny = oy + rfloat((float) (valInt(t->position->y)-oy) * yf);
  assign(t->position, x, toInt(nx));
  assign(t->position, y, toInt(ny));
  
  return recomputeText(t, NAME_area);
}


/*  Determine the position of a character  in  pixels.   It  returns the
    coordinate  of  the  upper left corner of the character.  By default
    the position of the caret is returned.

 ** Tue Nov  8 17:24:58 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static Point
getCharacterPositionText(TextObj t, Int chr)
{ int x, y;

  get_char_pos_text(t, chr, &x, &y);

  answer(answerObject(ClassPoint, toInt(x), toInt(y), EAV));
}


static status
get_char_pos_text(TextObj t, Int chr, int *X, int *Y)
{ int caret = (isDefault(chr) ? valInt(t->caret) : valInt(chr));
  int w   = abs(valInt(t->area->w));
  int ch  = valInt(getHeightFont(t->font));
  int cx, cy = 0, lw, sl;
  int shift;
  String s = str_bits_as_font(&t->string->data, t->font, &shift);
  int b = valInt(t->border);

  if ( shift )
    caret = (shift > 0 ? caret << shift : caret >> -shift);

  if ( Wrapped(t) )
  { LocalString(buf, s, Wrapped(t) ? s->size + MAX_WRAP_LINES : 0);

    str_format(buf, s, valInt(t->margin), t->font);
    s = buf;
  } else if ( t->wrap == NAME_clip )
  { LocalString(buf, s, s->size);

    str_one_line(buf, s);
    s = buf;
  }

  if ( (sl = str_next_rindex(s, caret-1, '\n')) < 0 )
    sl = 0;
  else
  { sl++;
    cy += (str_lineno(s, sl)-1) * ch;
  }

  lw = str_width(s, sl, caret, t->font);
  w -= 2 * b;

  if ( t->format == NAME_left )
  { cx = lw;
  } else
  { int el;
    int rw;

    if ( (el = str_next_index(s, caret, '\n')) < 0 )
      el = s->size;
    rw = str_width(s, caret, el, t->font);

    if ( t->format == NAME_center )
      cx = w/2 - (lw+rw)/2 + lw;
    else				/* right */
      cx = w - rw;
  }

  *X = cx + valInt(t->x_offset) + b;
  *Y = cy + b;

  succeed;
}


Int
get_pointed_text(TextObj t, int x, int y)
{ String s = &t->string->data;
  int ch = valInt(getHeightFont(t->font));
  int b = valInt(t->border);
  int cw, w;
  int caret = 0, el;
  int line = (y-b) / ch;			/* line for caret */
  int shift;

  if ( s->size == 0 )
    answer(ZERO);

  x -= b;

  s = str_bits_as_font(s, t->font, &shift);

  /* Find the start of the line pointed at by pos. */

  while(line-- > 0)
  { int c2;

    if ( (c2 = str_next_index(s, caret, '\n')) < 0 )
      break;
    caret = c2+1;
  }
  if ( caret > s->size )
    caret = s->size;
  if ( (el = str_next_index(s, caret, '\n')) < 0 )
    el = s->size;

  /* caret = start of line, el = end of line */

  if ( t->format == NAME_left )
    w = 0;
  else
  { int lw = str_width(s, caret, el, t->font);

    if ( t->format == NAME_center )
      w = (valInt(t->area->w) - lw)/2 - b;
    else
      w = valInt(t->area->w) - lw - 2*b;
  }
  w += valInt(t->x_offset);

  if ( caret < el-1 )
  { for( cw = c_width(str_fetch(s, caret), t->font);
	 x > w + cw/2;
	 caret++, w += cw, cw = c_width(str_fetch(s, caret), t->font) )
    { if ( caret >= el )
	break;
    }
  }
  
  if ( shift )
    caret = (shift > 0 ? caret >> shift : caret << -shift);

  answer(toInt(caret));
}


static Int
getPointedText(TextObj t, Point pos)
{ int x = valInt(pos->x);
  int y = valInt(pos->y);

  return get_pointed_text(t, x, y);
}

		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
backgroundText(TextObj t, Any bg)
{ if ( t->background != bg)
  { CHANGING_GRAPHICAL(t,
		       assign(t, background, bg);
		       changedEntireImageGraphical(t));
  }

  succeed;
}


static status
underlineText(TextObj t, Bool underline)
{ if ( t->underline != underline )
  { CHANGING_GRAPHICAL(t, assign(t, underline, underline);
		       changedEntireImageGraphical(t));
  }
  
  succeed;  
}


status
transparentText(TextObj t, Bool val)
{ Any bg = (val == ON ? NIL : DEFAULT);

  return backgroundText(t, bg);
}


static Bool
getTransparentText(TextObj t)
{ answer(isNil(t->background) ? ON : OFF);
}



status
fontText(TextObj t, FontObj font)
{ if (t->font != font)
  { assign(t, font, font);
    recomputeText(t, NAME_area);
  }
  succeed;
}


static status
formatText(TextObj t, Name format)
{ if (t->format != format)
  { assign(t, format, format);
    recomputeText(t, NAME_position);
  }
  succeed;
}


status
borderText(TextObj t, Int border)
{ if (t->border != border)
  { assign(t, border, border);
    recomputeText(t, NAME_area);
  }
  succeed;
}


status
stringText(TextObj t, CharArray s)
{ if ( t->string != s )
  { prepareEditText(t, DEFAULT);

    valueString((StringObj) t->string, s);
    caretText(t, DEFAULT);
    recomputeText(t, NAME_area);
  }

  succeed;
}


status
showCaretText(TextObj t, Any val)
{ if ( t->show_caret == val )
    succeed;

  CHANGING_GRAPHICAL(t,
		     assign(t, show_caret, val);
		     changedEntireImageGraphical(t));

  succeed;
}

		 /*******************************
		 *	     SELECTION		*
		 *******************************/

static status
selectionText(TextObj t, Int from, Int to)
{ int changed = FALSE;

  if ( from == to )
    from = NIL;

  if ( isNil(from) )
  { if ( notNil(t->selection) )
    { assign(t, selection, NIL);
      changed++;
    }
  } else
  { int ofrom, oto;
    Int new;

    if ( notNil(t->selection) )
    { GetSel(t->selection, &ofrom, &oto);
    } else
      ofrom = oto = 0;
  
    if ( isDefault(from) )
      from = toInt(ofrom);
    if ( isDefault(to) )
      to = toInt(oto);
  
    Before(from, to);
    new = MakeSel(from, to);

    if ( new != t->selection )
    { assign(t, selection, MakeSel(from, to));
      changed++;
    }
  }

  if ( changed )
    changedEntireImageGraphical(t);

  succeed;
}


static Point
getSelectionText(TextObj t)
{ if ( notNil(t->selection) )
  { int from, to;

    GetSel(t->selection, &from, &to);

    answer(answerObject(ClassPoint, toInt(from), toInt(to), EAV));
  }

  fail;
}


static StringObj
getSelectedTextText(TextObj t)
{ if ( notNil(t->selection) )
  { int from, to;

    GetSel(t->selection, &from, &to);
    answer(getSubString((StringObj)t->string, toInt(from), toInt(to)));
  }

  fail;
}


static status
copyText(TextObj t)
{ StringObj s = getSelectedTextText(t);
  DisplayObj d = getDisplayGraphical((Graphical)t);

  if ( !d )
  { if ( instanceOfObject(EVENT->value, ClassEvent) )
      d = getDisplayEvent(EVENT->value);
  }

  if ( s && d )
    return send(d, NAME_copy, s, EAV);

  fail;
}


static status
deleteSelectionText(TextObj t)
{ if ( notNil(t->selection) )
  { int from, to;

    GetSel(t->selection, &from, &to);

    prepareEditText(t, DEFAULT);
    deleteString((StringObj)t->string, toInt(from), toInt(to-from));
    assign(t, selection, NIL);
    if ( valInt(t->caret) > from )
      caretText(t, toInt(from));
    recomputeText(t, NAME_area);
  }

  succeed;
}


static status
cutText(TextObj t)
{ if ( send(t, NAME_copy, EAV) )
  { int from, to;

    GetSel(t->selection, &from, &to);
    return deleteSelectionText(t);
  }

  fail;
}




		 /*******************************
		 *	     GEOMETRY		*
		 *******************************/


static status
geometryText(TextObj t, Int x, Int y, Int w, Int h)
{ Int ox = t->area->x;
  Int oy = t->area->y;
  Point p = t->position;
  Area a = t->area;
  
  if ( Wrapped(t) && notDefault(w) )
  { assign(t, margin, w);
    CHANGING_GRAPHICAL(t,
		       initAreaText(t);
		       setArea(t->area, x, y, DEFAULT, DEFAULT));
  } else
  { if ( t->wrap != NAME_clip )
      w = (Int) DEFAULT;
    geometryGraphical(t, x, y, w, DEFAULT);
  }

  assign(p, x, toInt(valInt(p->x) + valInt(a->x) - valInt(ox)));
  assign(p, y, toInt(valInt(p->y) + valInt(a->y) - valInt(oy)));
  if ( notDefault(w) )
  { int tw, h;

    if ( isDefault(t->font) )
      obtainClassVariablesObject(t);		/* resolve the font */
    str_size(&t->string->data, t->font, &tw, &h);
    initOffsetText(t, tw);
  }

  succeed;
}


static status
updateShowCaretText(TextObj t)
{ if ( t->show_caret != OFF )
  { PceWindow sw = getWindowGraphical((Graphical)t);
    int active = (sw && sw->input_focus == ON);

    showCaretText(t, active ? (Any)ON : (Any)NAME_passive); 
  }

  succeed;
}


static status
eventText(TextObj t, EventObj ev)
{ if ( eventGraphical(t, ev) )
    succeed;

  if ( isAEvent(ev, NAME_focus) )
  { if ( isAEvent(ev, NAME_obtainKeyboardFocus) )
      showCaretText(t, ON);
    else if ( isAEvent(ev, NAME_releaseKeyboardFocus) )
      showCaretText(t, OFF);

    return updateShowCaretText(t);
  }

  if ( t->show_caret == ON && isAEvent(ev, NAME_keyboard) )
    return send(t, NAME_typed, ev, EAV);

  fail;
}


static status
typedText(TextObj t, EventId id)
{ return typedKeyBinding(KeyBindingText(), id, (Graphical) t);
}


static int
start_of_line(String s, int n)
{ if ( n > 0 && str_fetch(s, n) == '\n' )
    n--;

  n = str_next_rindex(s, n, '\n') + 1; /* returns -1 on not found! */

  return n;
}


static int
end_of_line(String s, int n)
{ if ( (n = str_next_index(s, n, '\n')) < 0 )
    n = s->size;

  return n;
}


static int
forward_word(String s, int i, int n)
{ while( n-- > 0 && i < s->size )
  { while( i < s->size && !isalnum(str_fetch(s, i)) ) i++;
    while( i < s->size && isalnum(str_fetch(s, i)) ) i++;
  }
  
  return i;  
}


static int
backward_word(String s, int i, int n)
{ while( n-- > 0 && i > 0 )
  { i--;
    while( i > 0 && !isalnum(str_fetch(s, i)) ) i--;
    while( i > 0 && isalnum(str_fetch(s, i-1)) ) i--;
  }
  
  return i;  
}


		/********************************
		*   INTERACTIVE EDIT COMMANDS   *
		********************************/

#define UArg(t)	(isDefault(arg) ? 1 : valInt(arg))

static void
deselectText(TextObj t)
{ if ( notNil(t->selection) )
    selectionText(t, NIL, DEFAULT);
}


static status
caretText(TextObj t, Int where)
{ int size = t->string->data.size;

  if ( isDefault(where) || valInt(where) >= size )
  { where = toInt(size);
  } else if ( valInt(where) < 0 )
  { where = ZERO;
  }
  assign(t, caret, where);
  if ( t->show_caret == ON )
    recomputeText(t, NAME_area);
  
  succeed;
}


static status
forwardCharText(TextObj t, Int arg)
{ deselectText(t);

  return caretText(t, add(t->caret, toInt(UArg(t))));
}


static status
backwardCharText(TextObj t, Int arg)
{ deselectText(t);

  return caretText(t, sub(t->caret, toInt(UArg(t))));
}


static status
nextLineText(TextObj t, Int arg, Int column)
{ int cx, cy;
  int fw, fh;

  deselectText(t);
  fw = valInt(getExFont(t->font));
  fh = valInt(getHeightFont(t->font));
  get_char_pos_text(t, DEFAULT, &cx, &cy);
  cy += UArg(t) * fh + fh/2;
  cx  = (isDefault(column) ? cx + fw/2 : valInt(column));

  return caretText(t, get_pointed_text(t, cx, cy));
}


static status
previousLineText(TextObj t, Int arg, Int column)
{ deselectText(t);

  return nextLineText(t, toInt(-UArg(t)), column);
}


static Int
getColumnText(TextObj t)
{ int cx, cy;
  int fw;

  fw = valInt(getExFont(t->font));
  get_char_pos_text(t, DEFAULT, &cx, &cy);

  answer(toInt(cx + fw/2));
}


static status
endOfLineText(TextObj t, Int arg)
{ String s = &t->string->data;
  int caret = valInt(t->caret);
  int n;

  deselectText(t);
  caret = end_of_line(s, caret);
  for(n = UArg(t)-1; caret < t->string->data.size && n > 0; n--)
  { caret++;
    caret = end_of_line(s, caret);
  }
  return caretText(t, toInt(caret));
}


static status
beginningOfLineText(TextObj t, Int arg)
{ String s = &t->string->data;
  int caret = valInt(t->caret);
  int n;

  deselectText(t);
  caret = start_of_line(s, caret);
  for(n = UArg(t)-1; caret > 0 && n > 0; n--)
  { caret--;
    caret = start_of_line(s, caret);
  }
  return caretText(t, toInt(caret));
}


static status
forwardWordText(TextObj t, Int arg)
{ int caret = valInt(t->caret);

  deselectText(t);
  caret = forward_word(&t->string->data, caret, UArg(t));
  return caretText(t, toInt(caret));
}


static status
backwardWordText(TextObj t, Int arg)
{ int caret = valInt(t->caret);

  deselectText(t);
  caret = backward_word(&t->string->data, caret, UArg(t));
  return caretText(t, toInt(caret));
}


		 /*******************************
		 *	  EDIT COMMANDS		*
		 *******************************/

static void
prepareInsertText(TextObj t)
{ if ( !instanceOfObject(t->string, ClassString) )
    assign(t, string, newObject(ClassString, name_procent_s,
				t->string, EAV));

  if ( getClassVariableValueObject(t, NAME_insertDeletesSelection) == ON )
    deleteSelectionText(t);
}


static status
prepareEditText(TextObj t, Name selector)
{ if ( notDefault(selector) &&
       !getSendMethodClass(ClassString, selector) )
    fail;

  if ( !instanceOfObject(t->string, ClassString) )
    assign(t, string, newObject(ClassString, name_procent_s,
				t->string, EAV));

  selectionText(t, NIL, DEFAULT);
  succeed;
}


status
pasteText(TextObj t, Int buffer)
{ CharArray str;
  DisplayObj d = CurrentDisplay(t);

  TRY((str = get(d, NAME_cutBuffer, buffer, EAV)) &&
      (str = checkType(str, nameToType(NAME_charArray), NIL)));
  prepareInsertText(t);
  insertString((StringObj) t->string, t->caret, str);
  caretText(t, add(t->caret, getSizeCharArray(str)));
  doneObject(str);
  return recomputeText(t, NAME_area);
}


static status
backwardDeleteCharText(TextObj t, Int arg)
{ int caret = valInt(t->caret);
  int len  = UArg(t);
  int from = (len > 0 ? caret - len : caret);
  int size = t->string->data.size;
  
  deselectText(t);

  len = abs(len);
  if ( from < 0 )
  { len += from;
    from = 0;
  }
  if ( from + len > size )
    len = size - from;

  if ( len > 0 )
  { caretText(t, toInt(from));
    prepareEditText(t, DEFAULT);
    deleteString((StringObj) t->string, toInt(from), toInt(len));
    return recomputeText(t, NAME_area);
  }
  
  succeed;
}


static status
deleteCharText(TextObj t, Int arg)
{ return backwardDeleteCharText(t, toInt(-UArg(t)));
}


static status
cutOrDeleteCharText(TextObj t, Int arg)
{ if ( notNil(t->selection) && isDefault(arg) )
    return cutText(t);
  else
    return deleteCharText(t, arg);
}


static status
cutOrBackwardDeleteCharText(TextObj t, Int arg)
{ if ( notNil(t->selection) && isDefault(arg) )
    return cutText(t);
  else
    return backwardDeleteCharText(t, arg);
}


static status
killLineText(TextObj t, Int arg)
{ String s = &t->string->data;
  int caret = valInt(t->caret);
  int end, n;

  deselectText(t);

  if ( isDefault(arg) && str_fetch(s, caret) == '\n' )
    return deleteCharText(t, DEFAULT);

  end = end_of_line(s, caret);
  if ( notDefault(arg) )
    for( n=UArg(t); end < s->size && n > 0; n--, end++ )
      end = end_of_line(s, end);

  prepareEditText(t, DEFAULT);
  deleteString((StringObj) t->string, t->caret, toInt(end-caret));
  return recomputeText(t, NAME_area);
}


static status
clearText(TextObj t)
{ deselectText(t);

  prepareEditText(t, DEFAULT);
  deleteString((StringObj) t->string, ZERO, DEFAULT);
  caretText(t, ZERO);
  return recomputeText(t, NAME_area);
}


static status
insertText(TextObj t, Int where, CharArray str)
{ if ( isDefault(where) )
    where = t->caret;

  prepareEditText(t, DEFAULT);
  insertString((StringObj)t->string, where, str);
  caretText(t, add(where, getSizeCharArray(str)));

  return recomputeText(t, NAME_area);
}


static status
insertSelfText(TextObj t, Int times, Int chr)
{ wchar c;
  int tms;

  if ( isDefault(times) )
    times = ONE;
  tms = valInt(times);

  if ( isDefault(chr) )
  { EventObj ev = EVENT->value;

    if ( instanceOfObject(ev, ClassEvent) && isAEvent(ev, NAME_printable) )
      c = valInt(getIdEvent(ev));
    else
      return errorPce(t, NAME_noCharacter);
  } else
    c = valInt(chr);
    
  prepareInsertText(t);

  { LocalString(buf, &t->string->data, tms);
    int i;

    for(i=0; i<tms; i++)
      str_store(buf, i, c);
    buf->size = i;

    str_insert_string((StringObj) t->string, t->caret, buf);
    caretText(t, add(t->caret, times));

    return recomputeText(t, NAME_area);
  }
}


static status
newlineText(TextObj t, Int arg)
{ return insertSelfText(t, arg, toInt('\n'));
}


static status
openLineText(TextObj t, Int arg)
{ int tms = UArg(t);

  if ( tms > 0 )
  { String nl = str_nl(&t->string->data);
    LocalString(buf, &t->string->data, nl->size * tms);
    int i;

    for(i=0; i<tms; i++)
      str_ncpy(buf, i * nl->size, nl, 0, nl->size);
    buf->size = nl->size * tms;

    prepareInsertText(t);
    str_insert_string((StringObj) t->string, t->caret, buf);
    recomputeText(t, NAME_area);
  }

  succeed;
}


static status
gosmacsTransposeText(TextObj t)
{ int caret = valInt(t->caret);

  if ( caret >= 2 )
  { wchar tmp;
    String s;

    deselectText(t);
    prepareEditText(t, DEFAULT);
    s = &((StringObj)t->string)->data;
    tmp = str_fetch(s, caret-2);
    str_store(s, caret-2, str_fetch(s, caret-1));
    str_store(s, caret-1, tmp);
    return recomputeText(t, NAME_area);
  }
  
  fail;
}


static status
transposeCharsText(TextObj t)
{ int caret = valInt(t->caret);

  if ( caret >= 1 )
  { wchar tmp;
    String s;

    deselectText(t);
    prepareEditText(t, DEFAULT);
    s = &((StringObj)t->string)->data;
    tmp = str_fetch(s, caret-1);
    str_store(s, caret-1, str_fetch(s, caret));
    str_store(s, caret, tmp);
    return recomputeText(t, NAME_area);
  }
  
  fail;
}


static status
killWordText(TextObj t, Int arg)
{ int caret = valInt(t->caret);

  deselectText(t);
  prepareEditText(t, DEFAULT);
  caret = forward_word(&t->string->data, caret, UArg(t));
  deleteString((StringObj) t->string, t->caret, sub(toInt(caret), t->caret));
  return recomputeText(t, NAME_area);
}


static status
backwardKillWordText(TextObj t, Int arg)
{ Int caret = t->caret;

  deselectText(t);
  prepareEditText(t, DEFAULT);
  caret = toInt(backward_word(&t->string->data, valInt(caret), UArg(t)));
  deleteString((StringObj) t->string, caret, sub(t->caret, caret));
  caretText(t, caret);
  return recomputeText(t, NAME_area);
}


static status
formatCenterText(TextObj t)
{ deselectText(t);
  return formatText(t, NAME_center);
}


static status
formatLeftText(TextObj t)
{ deselectText(t);
  return formatText(t, NAME_left);
}


static status
formatRightText(TextObj t)
{ deselectText(t);
  return formatText(t, NAME_right);
}


		/********************************
		*	HANDLING LONG TEXT	*
		********************************/

status
lengthText(TextObj t, Int l)
{ int fw, len;
    
  if ( isDefault(t->font) )
    obtainClassVariablesObject(t);

  fw = valInt(getExFont(t->font));
  len = (valInt(l)+1) * fw;

  return marginText(t, toInt(len), NAME_clip);
}


status
marginText(TextObj t, Int width, Name wrap)
{ int changed = FALSE;

  if ( isNil(width) )
    wrap = NAME_extend;
  else if ( isDefault(wrap) )
    wrap = NAME_wrap;

  if ( t->wrap != wrap )
  { assign(t, wrap, wrap);
    changed++;
  }

  assign(t, margin, width);

  if ( Wrapped(t) )
    changed++;
  else if ( wrap == NAME_clip )
    setGraphical(t, DEFAULT, DEFAULT, width, DEFAULT);

  if ( changed )
    recomputeText(t, NAME_area);

  succeed;
}


		/********************************
		*          LOAD-STORE		*
		********************************/

static status
loadText(TextObj t, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(t, fd, def));
  if ( restoreVersion <= 6 && t->pen != ZERO )
    assign(t, pen, ZERO);

  if ( isNil(t->wrap) )
    assign(t, wrap, NAME_extend);
  if ( isNil(t->margin) )
    assign(t, margin, toInt(100));
  if ( isNil(t->border) )
    assign(t, border, ZERO);
  if ( isNil(t->underline) )
    assign(t, underline, OFF);
    
  succeed;
}


static status
convertOldSlotText(TextObj t, Name slot, Any value)
{ if ( slot == NAME_transparent && isNil(t->background) )
    assign(t, background, (value == ON ? NIL : DEFAULT));

  succeed;
}

		 /*******************************
		 *	    DELEGATION		*
		 *******************************/

static status
catchAllText(TextObj t, Name sel, int argc, Any* argv)
{ if ( qadSendv(t->string, NAME_hasSendMethod, 1, (Any *)&sel) ||
       prepareEditText(t, sel) )
  { status rval;

    if ( (rval = vm_send(t->string, sel, NULL, argc, argv)) )
      recomputeText(t, NAME_area);

    return rval;
  }

  return errorPce(t, NAME_noBehaviour, CtoName("->"), sel);
}


static Any
getCatchAllText(TextObj t, Name sel, int argc, Any *argv)
{ if ( qadSendv(t->string, NAME_hasGetMethod, 1, (Any *)&sel) )
    answer(vm_get(t->string, sel, NULL, argc, argv));

  errorPce(t, NAME_noBehaviour, CtoName("<-"), sel);
  fail;
} 


static status
hasSendMethodText(TextObj t, Name sel)
{ if ( hasSendMethodObject(t, sel) ||
       hasSendMethodObject(t->string, sel) ||
       getSendMethodClass(ClassString, sel) )
    succeed;

  fail;
}


static status
hasGetMethodText(TextObj t, Name sel)
{ if ( hasGetMethodObject(t, sel) ||
       hasGetMethodObject(t->string, sel) )
    succeed;

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_insert[] =
        { "at=[int]", "text=char_array" };
static char *T_resize[] =
        { "factor_x=real", "factor_y=[real]", "origin=[point]" };
static char *T_margin[] =
        { "int*", "[{wrap,wrap_fixed_width,clip}]" };
static char *T_linesADintD_columnADintD[] =
        { "lines=[int]", "column=[int]" };
static char *T_convertOldSlot[] =
        { "slot=name", "value=unchecked" };
static char *T_initialise[] =
        { "string=[char_array]", "format=[{left,center,right}]",
	  "font=[font]" };
static char *T_insertSelf[] =
        { "times=[int]", "character=[char]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_selection[] =
        { "from=[int]*", "to=[int]" };
static char *T_catchAll[] =
        { "selector=name", "argument=unchecked ..." };

/* Instance Variables */

static vardecl var_text[] =
{ IV(NAME_string, "char_array", IV_GET,
     NAME_storage, "Represented string (may contain newlines)"),
  SV(NAME_font, "font", IV_GET|IV_STORE, fontText,
     NAME_appearance, "Font used to draw the string"),
  SV(NAME_format, "{left,center,right}", IV_GET|IV_STORE, formatText,
     NAME_appearance, "Left, center or right alignment"),
  IV(NAME_margin, "int", IV_GET,
     NAME_appearance, "Margin for <->wrap equals wrap"),
  SV(NAME_underline, "bool", IV_GET|IV_STORE, underlineText,
     NAME_appearance, "Underlined text?"),
  IV(NAME_position, "point", IV_NONE,
     NAME_internal, "Avoid `walking' with alignment"),
  IV(NAME_caret, "int", IV_GET,
     NAME_caret, "Index (0-based) of caret"),
  SV(NAME_showCaret, "bool|{passive}", IV_GET|IV_STORE, showCaretText,
     NAME_appearance, "If not @off, show the caret"),
  SV(NAME_background, "[colour|pixmap]*", IV_GET|IV_STORE, backgroundText,
     NAME_appearance, "@nil: transparent; @default: cleared"),
  SV(NAME_border, "0..", IV_GET|IV_STORE, borderText,
     NAME_appearance, "Border around actual text"),
  IV(NAME_wrap, "{extend,wrap,wrap_fixed_width,clip}", IV_GET,
     NAME_appearance, "How long text is handled"),
  IV(NAME_xOffset, "int", IV_NONE,
     NAME_internal, "Horizontal scroll when nonzero length"),
  IV(NAME_xCaret, "int", IV_NONE,
     NAME_internal, "X-position of caret"),
  IV(NAME_yCaret, "int", IV_NONE,
     NAME_internal, "Y-position of caret"),
  IV(NAME_Selection, "int*", IV_NONE,
     NAME_internal, "Selected text")
};

/* Send Methods */

static senddecl send_text[] =
{ SM(NAME_event, 1, "event", eventText,
     DEFAULT, "Handle focus and keyboard events"),
  SM(NAME_geometry, 4, T_geometry, geometryText,
     DEFAULT, "Only move text"),
  SM(NAME_initialise, 3, T_initialise, initialiseText,
     DEFAULT, "Create from string, format and font"),
  SM(NAME_resize, 3, T_resize, resizeText,
     DEFAULT, "Resize text with specified factor"),
  SM(NAME_string, 1, "char_array", stringText,
     NAME_storage, "Represented string"),
  SM(NAME_formatCenter, 0, NULL, formatCenterText,
     NAME_appearance, "Set center alignment"),
  SM(NAME_formatLeft, 0, NULL, formatLeftText,
     NAME_appearance, "Set left alignment"),
  SM(NAME_formatRight, 0, NULL, formatRightText,
     NAME_appearance, "Set right alignment"),
  SM(NAME_margin, 2, T_margin, marginText,
     NAME_appearance, "Determine how long text is handled"),
  SM(NAME_length, 1, "int", lengthText,
     NAME_area, "(compatibility)"),
  SM(NAME_prefix, 0, NULL, succeedObject,
     NAME_binding, "Multi-key prefix (see class key_binding)"),
  SM(NAME_backwardChar, 1, "times=[int]", backwardCharText,
     NAME_caret, "Move caret characters backward (\\C-f)"),
  SM(NAME_backwardWord, 1, "times=[int]", backwardWordText,
     NAME_caret, "Move caret words backward (\\eb)"),
  SM(NAME_beginningOfLine, 1, "times=[int]", beginningOfLineText,
     NAME_caret, "Move caret to start of line (\\C-a)"),
  SM(NAME_caret, 1, "[int]", caretText,
     NAME_caret, "Move caret to 0-based index"),
  SM(NAME_endOfLine, 1, "times=[int]", endOfLineText,
     NAME_caret, "Move caret to end of line (\\C-e)"),
  SM(NAME_forwardChar, 1, "times=[int]", forwardCharText,
     NAME_caret, "Move caret characters forwards (\\C-f)"),
  SM(NAME_forwardWord, 1, "times=[int]", forwardWordText,
     NAME_caret, "Move caret words forward (\\ef)"),
  SM(NAME_nextLine, 2, T_linesADintD_columnADintD, nextLineText,
     NAME_caret, "Move caret lines down (\\C-n)"),
  SM(NAME_previousLine, 2, T_linesADintD_columnADintD, previousLineText,
     NAME_caret, "Move caret lines up (\\C-n)"),
  SM(NAME_convertOldSlot, 2, T_convertOldSlot, convertOldSlotText,
     NAME_compatibility, "Convert <-transparent to <-background"),
  SM(NAME_transparent, 1, "bool", transparentText,
     NAME_compatibility, "Defines <-background"),
  SM(NAME_backwardDeleteChar, 1, "times=[int]", backwardDeleteCharText,
     NAME_delete, "Delete chars backward from caret (DEL)"),
  SM(NAME_backwardKillWord, 1, "times=[int]", backwardKillWordText,
     NAME_delete, "Deletes words backward from caret (\\eDEL)"),
  SM(NAME_clear, 0, NULL, clearText,
     NAME_delete, "Wipe out all text (\\C-u)"),
  SM(NAME_deleteChar, 1, "times=[int]", deleteCharText,
     NAME_delete, "Delete characters forwards (\\C-d)"),
  SM(NAME_cutOrDeleteChar, 1, "times=[int]", cutOrDeleteCharText,
     NAME_delete, "Cur or delete characters forwards (DEL)"),
  SM(NAME_cutOrBackwardDeleteChar, 1, "times=[int]",
     cutOrBackwardDeleteCharText,
     NAME_delete, "Cut or delete characters backward (BS)"),
  SM(NAME_killLine, 1, "times=[int]", killLineText,
     NAME_delete, "Delete lines from caret \\C-k)"),
  SM(NAME_killWord, 1, "times=[int]", killWordText,
     NAME_delete, "Deletes words forward from caret (\\ed)"),
  SM(NAME_typed, 1, "event|event_id", typedText,
     NAME_event, "Handle a keystroke"),
  SM(NAME_insert, 2, T_insert, insertText,
     NAME_insert, "Insert text at position [<-caret]"),
  SM(NAME_insertSelf, 2, T_insertSelf, insertSelfText,
     NAME_insert, "Insert n-times char at caret"),
  SM(NAME_newline, 1, "times=[int]", newlineText,
     NAME_insert, "Insert newlines (RET, LFD)"),
  SM(NAME_openLine, 1, "times=[int]", openLineText,
     NAME_insert, "Insert newlines after caret (\\C-o)"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptText,
     NAME_postscript, "Create PostScript"),
  SM(NAME_compute, 0, NULL, computeText,
     NAME_repaint, "Recompute area/offset"),
  SM(NAME_paste, 1, "[0..9]", pasteText,
     NAME_selection, "Paste value of cut-buffer"),
  SM(NAME_gosmacsTranspose, 0, NULL, gosmacsTransposeText,
     NAME_transpose, "Transpose two char_array before caret"),
  SM(NAME_transposeChars, 0, NULL, transposeCharsText,
     NAME_transpose, "Transpose two char_array around caret"),
  SM(NAME_selection, 2, T_selection, selectionText,
     NAME_selection, "Make [from, to) the selection"),
  SM(NAME_copy, 0, NULL, copyText,
     NAME_selection, "Copy selection (\\C-c)"),
  SM(NAME_cut, 0, NULL, cutText,
     NAME_selection, "Copy and delete selection"),
  SM(NAME_cutOrDeleteChar, 1, "times=[int]", cutOrDeleteCharText,
     NAME_delete, "Delete characters forwards (DEL)"),
  SM(NAME_catchAll, 2, T_catchAll, catchAllText,
     NAME_delegate, "Delegate to <-string"),
  SM(NAME_hasSendMethod, 1, "name", hasSendMethodText,
     DEFAULT, "Test if text or <-string defines method"),
  SM(NAME_hasGetMethod, 1, "name", hasGetMethodText,
     DEFAULT, "Test if text or <-string defines method")
};

/* Get Methods */

static getdecl get_text[] =
{ GM(NAME_characterPosition, 1, "point", "index=[int]",
     getCharacterPositionText,
     NAME_calculate, "Convert index to position of character"),
  GM(NAME_column, 0, "pixels=int", NULL, getColumnText,
     NAME_caret, "Current X-location of caret (pixels)"),
  GM(NAME_upDownColumn, 0, "pixels=int", NULL, getColumnText,
     NAME_caret, "Current X-location of caret (pixels)"),
  GM(NAME_transparent, 0, "bool", NULL, getTransparentText,
     NAME_compatibility, "Map <-background"),
  GM(NAME_pointed, 1, "index=int", "at=point", getPointedText,
     NAME_event, "Convert position to character index"),
  GM(NAME_selectedText, 0, "string", NULL, getSelectedTextText,
     NAME_selection, "New string with contents of selection"),
  GM(NAME_selection, 0, "point", NULL, getSelectionText,
     NAME_selection, "New point with start and end of selection"),
  GM(NAME_catchAll, 2, "unchecked", T_catchAll, getCatchAllText,
     NAME_delegate, "Delegate to <-string")
};

/* Resources */

static classvardecl rc_text[] =
{ RC(NAME_pen, RC_REFINE, "0", NULL),
  RC(NAME_border, "0..", "0",
     "Space around the actual text"),
  RC(NAME_font, "font", "normal",
     "Default font"),
  RC(NAME_format, "name", "left",
     "Default adjustment: {left,center,right}"),
  RC(NAME_selectionStyle, "style",
     UXWIN("style(colour := white, background := black)",
	   "@_select_style"),
     "Style for <-selection"),
  RC(NAME_insertDeletesSelection, "bool", "@on",
     "->insert_self and ->paste delete the selection"),
  RC(NAME_keyBinding, "string", "",
     "`Key = selector' binding list")
};

/* Class Declaration */

static Name text_termnames[] = { NAME_string, NAME_format, NAME_font };

ClassDecl(text_decls,
          var_text, send_text, get_text, rc_text,
          3, text_termnames,
          "$Rev$");


status
makeClassText(Class class)
{ declareClass(class, &text_decls);
  setRedrawFunctionClass(class, RedrawAreaText);
  setLoadStoreFunctionClass(class, loadText, NULL);

  succeed;
}

