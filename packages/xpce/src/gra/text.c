/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	initOffsetText P((TextObj, int));
static status	initPositionText P((TextObj));
static status	initAreaText P((TextObj));
static status	recomputeText(TextObj t, Name what);
static status	get_char_pos_text(TextObj t, Int chr, int *X, int *Y);
static status	prepareEditText(TextObj t);

#define Wrapped(t) ((t)->wrap == NAME_wrap || (t)->wrap == NAME_wrapFixedWidth)


		/********************************
		*            CREATE		*
		********************************/

static status
initialiseText(TextObj t, CharArray string, Name format, FontObj font)
{ if ( isDefault(string) )
    string = CtoCharArray("");

  initialiseGraphical(t, ZERO, ZERO, ZERO, ZERO);

  assign(t, pen,	  ZERO);
  assign(t, format,       format);
  assign(t, string,       string);
  assign(t, margin,	  toInt(100));
  assign(t, wrap,         NAME_extend);
  assign(t, font,         font);
  assign(t, position,     newObject(ClassPoint, 0));
  assign(t, caret,        getSizeCharArray(string));
  assign(t, show_caret,   OFF);
  assign(t, background,   NIL);
  assign(t, border,	  DEFAULT);
  assign(t, x_offset,	  ZERO);
  assign(t, x_caret,	  ZERO);
  assign(t, y_caret,	  ZERO);

  return recomputeText(t, NAME_position);
}

		/********************************
		*            COMPUTE		*
		********************************/

status
computeText(TextObj t)
{ if ( notNil(t->request_compute) )
  { obtainResourcesObject(t);

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
{ if ( notNil(t->request_compute) && t->request_compute != what )
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
      { out->size = o - out->s_text8;
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
      { out->size = o - out->s_text16;
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
draw_caret(int x, int y, int w, int h, int active)
{ if ( active )
  { int cx = x + w/2;

    r_fillpattern(BLACK_IMAGE);
    r_fill_triangle(cx, y, x, y+h, x+w, y+h);
  } else
  { struct ipoint pts[4];
    int cx = x + w/2;

    int cy = y + h/2;
    int i = 0;

    pts[i].x = cx;  pts[i].y = y;   i++;
    pts[i].x = x;   pts[i].y = cy;  i++;
    pts[i].x = cx;  pts[i].y = y+h; i++;
    pts[i].x = x+w; pts[i].y = cy;  i++;
      
    r_fillpattern(GREY50_IMAGE);
    r_fill_polygon(pts, i);
  }
}


#ifndef OL_CURSOR_SIZE
#define OL_CURSOR_SIZE	9
#endif

status
repaintText(TextObj t, int x, int y, int w, int h)
{ int cw = valInt(getExFont(t->font));
  String s = &t->string->data;
  int b = valInt(t->border);

  if ( notNil(t->background) )
  { if ( isDefault(t->background) )
      r_clear(x, y, w, h);
    else
      r_fill(x, y, w, h, t->background);
  }

  x += b;
  y += b;
  w -= 2*b;
  h -= 2*b;

  if ( t->wrap == NAME_clip )
    d_clip(x, y, w, h);

  if ( Wrapped(t) )
  { LocalString(buf, s, s->size + MAX_WRAP_LINES);

    str_format(buf, s, valInt(t->margin), t->font);
    str_string(buf, t->font,
	       x+cw/2+valInt(t->x_offset), y, w-cw, h,
	       t->format, NAME_top);
  } else
    str_string(s, t->font,
	       x+cw/2+valInt(t->x_offset), y, w-cw, h,
	       t->format, NAME_top);

  if ( t->wrap == NAME_clip )
    d_clip_done();

  if ( t->show_caret != OFF )
  { int fh = valInt(getAscentFont(t->font));
    int active = (t->show_caret == ON);
    Any colour = getResourceValueClass(ClassTextCursor,
				       active ? NAME_colour
					      : NAME_inactiveColour);
    Any old = r_colour(colour);

    draw_caret(valInt(t->x_caret) - OL_CURSOR_SIZE/2 + x - b - 2,
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
{ int tw, x, y, w, h, fw;
  Point pos = t->position;
  String s = &t->string->data;
  int size = s->size;
  int b = valInt(t->border);

  if ( valInt(t->caret) < 0 )
    assign(t, caret, ZERO);
  if ( valInt(t->caret) > size )
    assign(t, caret, toInt(size));

  fw = valInt(getExFont(t->font));
  if ( Wrapped(t) )
  { LocalString(buf, s, s->size + MAX_WRAP_LINES);

    str_format(buf, s, valInt(t->margin), t->font);
    str_size(buf, t->font, &tw, &h);
    if ( t->wrap == NAME_wrapFixedWidth && tw < valInt(t->margin) - fw )
      tw = valInt(t->margin) - fw;
  } else
    str_size(s, t->font, &tw, &h);

  if ( t->wrap == NAME_clip )
    w = valInt(t->area->w) - 2*b;
  else
    w = tw + fw;

  if ( equalName(t->format, NAME_right) )
  { x = valInt(pos->x) - w + fw/2;
    y = valInt(pos->y);
  } else if ( equalName(t->format, NAME_center) )
  { x = valInt(pos->x) - w/2;
    y = valInt(pos->y) - h/2;
  } else
  { x = valInt(pos->x) - fw/2;
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
{ int tw, x, y, w, h, fw = valInt(getExFont(t->font));
  Point pos = t->position;
  String s = &t->string->data;
  int b = valInt(t->border);

  if ( Wrapped(t) )
  { LocalString(buf, s, s->size + MAX_WRAP_LINES);

    str_format(buf, s, valInt(t->margin), t->font);
    str_size(buf, t->font, &tw, &h);
    if ( t->wrap == NAME_wrapFixedWidth && tw < valInt(t->margin) - fw )
      tw = valInt(t->margin) - fw;
  } else
    str_size(s, t->font, &tw, &h);

  if ( t->wrap == NAME_clip )
    w = valInt(t->area->w) - 2*b;
  else
    w = tw + fw;

  if ( equalName(t->format, NAME_left) )
  { x = valInt(t->area->x) + fw/2;
    y = valInt(t->area->y) + b;
  } else if ( equalName(t->format, NAME_right) )
  { x = valInt(t->area->x) + w - fw/2;
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
  { int x, y, w = valInt(t->area->w), shift;
    int fw = valInt(getExFont(t->font));
    int xoff;

    if ( tw + fw/2 <= w || t->caret == ZERO )
      assign(t, x_offset, ZERO);
    else if ( t->caret == getSizeCharArray(t->string) )
      assign(t, x_offset, toInt(w - (tw + fw)));

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

  answer(answerObject(ClassPoint, toInt(x), toInt(y), 0));
}


static status
get_char_pos_text(TextObj t, Int chr, int *X, int *Y)
{ int caret = (isDefault(chr) ? valInt(t->caret) : valInt(chr));
  int w   = abs(valInt(t->area->w));
  int cw2 = valInt(getExFont(t->font))/2;
  int ch  = valInt(getHeightFont(t->font));
  int cx, cy = 0, lw, sl;
  int shift;
  String s = str_bits_as_font(&t->string->data, t->font, &shift);
  LocalString(buf, s, Wrapped(t) ? s->size + MAX_WRAP_LINES : 0);
  int b = valInt(t->border);

  if ( shift )
    caret = (shift > 0 ? caret << shift : caret >> -shift);

  if ( Wrapped(t) )
  { str_format(buf, s, valInt(t->margin), t->font);
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
  { cx = cw2 + lw;
  } else
  { int el;
    int rw;

    if ( (el = str_next_index(s, caret, '\n')) < 0 )
      el = s->size;
    rw = str_width(s, caret, el, t->font);

    if ( t->format == NAME_center )
      cx = w/2 - (lw+rw)/2 + lw;
    else				/* right */
      cx = w - cw2 - rw;
  }

  *X = cx + valInt(t->x_offset) + b;
  *Y = cy + b;

  succeed;
}


static Int
get_pointed_text(TextObj t, int x, int y)
{ String s = &t->string->data;
  int ch = valInt(getHeightFont(t->font));
  int cw2 = valInt(getExFont(t->font))/2;
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
    w = cw2;
  else
  { int lw = str_width(s, caret, el, t->font);

    if ( t->format == NAME_center )
      w = (valInt(t->area->w) - lw)/2 - b;
    else
      w = valInt(t->area->w) - lw - cw2 - 2*b;
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


status
transparentText(TextObj t, Bool val)
{ Any bg = (val == ON ? NIL : DEFAULT);

  return backgroundText(t, bg);
}


Bool
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
{ if ( !equalCharArray(t->string, s) )
  { prepareEditText(t);

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
	recomputeText(t, NAME_area));

  succeed;
}


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
      obtainResourcesObject(t);		/* resolve the font */
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
    return send(t, NAME_typed, ev, 0);

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

status
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
{ return caretText(t, add(t->caret, toInt(UArg(t))));
}


static status
backwardCharText(TextObj t, Int arg)
{ return caretText(t, sub(t->caret, toInt(UArg(t))));
}


static status
nextLineText(TextObj t, Int arg, Int column)
{ int cx, cy;
  int fw, fh;

  fw = valInt(getExFont(t->font));
  fh = valInt(getHeightFont(t->font));
  get_char_pos_text(t, DEFAULT, &cx, &cy);
  cy += UArg(t) * fh + fh/2;
  cx  = (isDefault(column) ? cx + fw/2 : valInt(column));

  return caretText(t, get_pointed_text(t, cx, cy));
}


static status
previousLineText(TextObj t, Int arg, Int column)
{ return nextLineText(t, toInt(-UArg(t)), column);
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

  caret = forward_word(&t->string->data, caret, UArg(t));
  return caretText(t, toInt(caret));
}


static status
backwardWordText(TextObj t, Int arg)
{ int caret = valInt(t->caret);

  caret = backward_word(&t->string->data, caret, UArg(t));
  return caretText(t, toInt(caret));
}


static status
prepareEditText(TextObj t)
{ if ( !instanceOfObject(t->string, ClassString) )
    assign(t, string, newObject(ClassString, name_procent_s,
				t->string, 0));

  succeed;
}


status
pasteText(TextObj t, Int buffer)
{ CharArray str;
  DisplayObj d = CurrentDisplay(t);

  TRY((str = get(d, NAME_cutBuffer, buffer, 0)) &&
      (str = checkType(str, nameToType(NAME_charArray), NIL)));
  prepareEditText(t);
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
  
  len = abs(len);
  if ( from < 0 )
  { len += from;
    from = 0;
  }
  if ( from + len > size )
    len = size - from;

  if ( len > 0 )
  { caretText(t, toInt(from));
    prepareEditText(t);
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
killLineText(TextObj t, Int arg)
{ String s = &t->string->data;
  int caret = valInt(t->caret);
  int end, n;

  if ( isDefault(arg) && str_fetch(s, caret) == '\n' )
    return deleteCharText(t, DEFAULT);

  end = end_of_line(s, caret);
  if ( notDefault(arg) )
    for( n=UArg(t); end < s->size && n > 0; n--, end++ )
      end = end_of_line(s, end);

  prepareEditText(t);
  deleteString((StringObj) t->string, t->caret, toInt(end-caret));
  return recomputeText(t, NAME_area);
}


static status
clearText(TextObj t)
{ prepareEditText(t);
  deleteString((StringObj) t->string, ZERO, DEFAULT);
  caretText(t, ZERO);
  return recomputeText(t, NAME_area);
}


static status
insertText(TextObj t, Int where, CharArray str)
{ if ( isDefault(where) )
    where = t->caret;

  prepareEditText(t);
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
    
    prepareEditText(t);

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
  { String nl = str_nl();
    LocalString(buf, &t->string->data, nl->size * tms);
    int i;

    for(i=0; i<tms; i++)
      str_ncpy(buf, i * nl->size, nl, 0, nl->size);
    buf->size = nl->size * tms;

    prepareEditText(t);
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

    prepareEditText(t);
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

    prepareEditText(t);
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

  prepareEditText(t);
  caret = forward_word(&t->string->data, caret, UArg(t));
  deleteString((StringObj) t->string, t->caret, sub(toInt(caret), t->caret));
  return recomputeText(t, NAME_area);
}


static status
backwardKillWordText(TextObj t, Int arg)
{ Int caret = t->caret;

  prepareEditText(t);
  caret = toInt(backward_word(&t->string->data, valInt(caret), UArg(t)));
  deleteString((StringObj) t->string, caret, sub(t->caret, caret));
  caretText(t, caret);
  return recomputeText(t, NAME_area);
}


static status
formatCenterText(TextObj t)
{ return formatText(t, NAME_center);
}


static status
formatLeftText(TextObj t)
{ return formatText(t, NAME_left);
}


static status
formatRightText(TextObj t)
{ return formatText(t, NAME_right);
}


		/********************************
		*	HANDLING LONG TEXT	*
		********************************/

status
lengthText(TextObj t, Int l)
{ int fw, len;
    
  if ( isDefault(t->font) )
    obtainResourcesObject(t);

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
		*       DELEGATE TO STRING	*
		********************************/

static status
delegateTextv(TextObj t, Any impl, Any part, int argc, Any *argv)
{ if ( sendImplementation(impl, part, argc, argv) )
  { recomputeText(t, NAME_area);
    succeed;
  }

  fail;
}


		/********************************
		*          LOAD-STORE		*
		********************************/

static status
loadText(TextObj t, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(t, fd, def));
  if ( restoreVersion <= 6 && t->pen != ZERO )
    assign(t, pen, ZERO);

  if ( isNil(t->wrap) )
    assign(t, wrap, NAME_extend);
  if ( isNil(t->margin) )
    assign(t, margin, toInt(100));
  if ( isNil(t->border) )
    assign(t, border, ZERO);
    
  succeed;
}


static status
convertOldSlotText(TextObj t, Name slot, Any value)
{ if ( slot == NAME_transparent && isNil(t->background) )
    assign(t, background, (value == ON ? NIL : DEFAULT));

  succeed;
}


extern drawPostScriptText(TextObj t);

status
makeClassText(Class class)
{ sourceClass(class, makeClassText, __FILE__, "$Revision$");

  superClass(class, NAME_string, NAME_storage, "char_array", NAME_get, 
	     NAME_delegate,
	     "Represented string (may contain newlines)");
  localClass(class, NAME_font, NAME_appearance, "font", NAME_get,
	     "Font used to draw the string");
  localClass(class, NAME_format, NAME_appearance,
	     "{left,center,right}", NAME_get,
	     "Left, center or right alignment");
  localClass(class, NAME_margin, NAME_appearance, "int", NAME_get,
	     "Margin for <->wrap equals wrap");
  localClass(class, NAME_position, NAME_internal, "point", NAME_none,
	     "Avoid `walking' with alignment");
  localClass(class, NAME_caret, NAME_caret, "int", NAME_get,
	     "Index (0-based) of caret");
  localClass(class, NAME_showCaret, NAME_appearance, "bool|{passive}",
	     NAME_get,
	     "If not @off, show the caret");
  localClass(class, NAME_background, NAME_appearance, "[colour|pixmap]*",
	     NAME_get,
	     "@nil: transparent; @default: cleared");
  localClass(class, NAME_border, NAME_appearance, "0..", NAME_get,
	     "Border around actual text");
  localClass(class, NAME_wrap, NAME_appearance,
	     "{extend,wrap,wrap_fixed_width,clip}", NAME_get,
	     "How long text is handled");
  localClass(class, NAME_xOffset, NAME_internal, "int", NAME_none,
	     "Horizontal scroll when nonzero length");
  localClass(class, NAME_xCaret, NAME_internal, "int", NAME_none,
	     "X-position of caret");
  localClass(class, NAME_yCaret, NAME_internal, "int", NAME_none,
	     "Y-position of caret");

  termClass(class, "text", 3, NAME_string, NAME_format, NAME_font);
  setRedrawFunctionClass(class, RedrawAreaText);
  setLoadStoreFunctionClass(class, loadText, NULL);

  storeMethod(class, NAME_font,        fontText);
  storeMethod(class, NAME_format,      formatText);
  storeMethod(class, NAME_showCaret,   showCaretText);
  storeMethod(class, NAME_string,      stringText);
  storeMethod(class, NAME_background,  backgroundText);
  storeMethod(class, NAME_border,      borderText);

  sendMethod(class, NAME_initialise, DEFAULT,
	     3, "string=[char_array]", "format=[{left,center,right}]",
	        "font=[font]",
	     "Create from string, format and font",
	     initialiseText);
  sendMethod(class, NAME_delegate, NAME_delegate,
	     3, "program_object", "string", "unchecked ...",
	     "Delegate to string",
	     delegateTextv);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Only move text",
	     geometryText);
  sendMethod(class, NAME_compute, NAME_repaint, 0,
	     "Recompute area/offset",
	     computeText);
  sendMethod(class, NAME_caret, NAME_caret, 1, "[int]",
	     "Move caret to 0-based index",
	     caretText);
  sendMethod(class, NAME_resize, DEFAULT, 3,
	     "factor_x=real", "factor_y=[real]", "origin=[point]",
	     "Resize text with specified factor",
	     resizeText);
  sendMethod(class, NAME_typed, NAME_event, 1, "event_id",
	     "Handle a keystroke",
	     typedText);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Handle focus and keyboard events",
	     eventText);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptText);
  sendMethod(class, NAME_paste, NAME_selection, 1, "[0..9]",
	     "Paste value of cut-buffer",
	     pasteText);
  sendMethod(class, NAME_length, NAME_area, 1, "int",
	     "(compatibility)",
	     lengthText);
  sendMethod(class, NAME_margin, NAME_appearance, 2, "int*",
	     "[{wrap,wrap_fixed_width,clip}]",
	     "Determine how long text is handled",
	     marginText);
  sendMethod(class, NAME_insert, NAME_insert, 2, "at=[int]", "text=char_array",
	     "Insert text at position [<-caret]",
	     insertText);
			/*** INTERACTIVE ***/

  sendMethod(class, NAME_insertSelf, NAME_insert, 2,
	     "times=[int]", "character=[char]",
	     "Insert n-times char at caret",
	     insertSelfText);

#define interactiveMethod(name, func, group, doc) \
  sendMethod(class, name, group, 1, "times=[int]", doc, func)
#define noArgMethod(name, func, group, doc) \
  sendMethod(class, name, group, 0, doc, func)

  noArgMethod(NAME_formatCenter, formatCenterText,
	      NAME_appearance, "Set center alignment");
  noArgMethod(NAME_formatLeft, formatLeftText,
	      NAME_appearance, "Set left alignment");
  noArgMethod(NAME_formatRight, formatRightText,
	      NAME_appearance, "Set right alignment");
  noArgMethod(NAME_prefix, succeedObject,
	      NAME_binding, "Multi-key prefix (see class key_binding)");
  noArgMethod(NAME_clear, clearText,
	      NAME_delete, "Wipe out all text");
  noArgMethod(NAME_gosmacsTranspose, gosmacsTransposeText,
	      NAME_transpose, "Transpose two char_array before caret");
 noArgMethod(NAME_transposeChars, transposeCharsText,
	      NAME_transpose, "Transpose two char_array around caret");

  interactiveMethod(NAME_backwardChar,          backwardCharText,
                    NAME_caret, "Move caret characters backward (\\C-f)");
  interactiveMethod(NAME_endOfLine,		endOfLineText,
		    NAME_caret, "Move caret to end of line (\\C-e)");
  interactiveMethod(NAME_forwardChar,		forwardCharText,
		    NAME_caret, "Move caret characters forwards (\\C-f)");
  interactiveMethod(NAME_beginningOfLine,	beginningOfLineText,
		    NAME_caret, "Move caret to start of line (\\C-a)");
  interactiveMethod(NAME_forwardWord,		forwardWordText,
		    NAME_caret, "Move caret words forward (\\ef)");
  interactiveMethod(NAME_backwardWord,		backwardWordText,
		    NAME_caret, "Move caret words backward (\\eb)");
  interactiveMethod(NAME_backwardDeleteChar,	backwardDeleteCharText,
		    NAME_delete, "Delete chars backward from caret (DEL)");
  interactiveMethod(NAME_deleteChar,		deleteCharText,
		    NAME_delete, "Delete characters forwards (\\C-d)");
  interactiveMethod(NAME_killLine,		killLineText,
		    NAME_delete, "Delete lines from caret \\C-k)");
  interactiveMethod(NAME_newline,		newlineText,
		    NAME_insert, "Insert newlines (RET, LFD)");
  interactiveMethod(NAME_openLine,		openLineText,
		    NAME_insert, "Insert newlines after caret (\\C-o)");
  interactiveMethod(NAME_killWord,		killWordText,
		    NAME_delete, "Deletes words forward from caret (\\ed)");
  interactiveMethod(NAME_backwardKillWord,	backwardKillWordText,
		    NAME_delete, "Deletes words backward from caret (\\eDEL)");

  sendMethod(class, NAME_nextLine, NAME_caret, 2,
	     "lines=[int]", "column=[int]",
	     "Move caret lines down (\\C-n)",
	     nextLineText);
  sendMethod(class, NAME_previousLine, NAME_caret, 2,
	     "lines=[int]", "column=[int]",
	     "Move caret lines up (\\C-n)",
	     previousLineText);

  sendMethod(class, NAME_transparent, NAME_compatibility, 1, "bool",
	     "Defines <-background",
	     transparentText);
  sendMethod(class, NAME_convertOldSlot, NAME_compatibility, 2,
	     "slot=name", "value=unchecked",
	     "Convert <-transparent to <-background",
	     convertOldSlotText);

  getMethod(class, NAME_pointed, NAME_event, "index=int", 1, "at=point",
	    "Convert position to character index",
	    getPointedText);
  getMethod(class, NAME_characterPosition, NAME_calculate, "point", 1,
	    "index=[int]",
	    "Convert index to position of character",
	    getCharacterPositionText);
  getMethod(class, NAME_column, NAME_caret, "pixels=int", 0,
	    "Current X-location of caret (pixels)",
	    getColumnText);
  getMethod(class, NAME_transparent, NAME_compatibility, "bool", 0,
	    "Map <-background",
	    getTransparentText);

  attach_resource(class, "font", "font", "@screen_roman_13",
		  "Default font");
  attach_resource(class, "format", "name", "left",
		  "Default adjustment: {left,center,right}");
  attach_resource(class, "key_binding", "string", "",
		  "`Key = selector' binding list");
  attach_resource(class, "border", "0..", "0",
		  "Space around the actual text");

  succeed;
}

