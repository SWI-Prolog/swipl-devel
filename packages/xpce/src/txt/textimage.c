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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The PCE-3 editor object is now split up into a large number of  separate
objects  to  improve modilarity and reusability of various pieces of the
editor.  One of the most critical ones  is  the  textimage  object.   It
maintains the relation between a text and its graphical representation.

The textimage object understands the following method:

    ->size     Size		Resize the image to a new size (pixels)
    ->start    Integer		Defines first character to be displayed
    ->wrap     Name		none, character or word.  Defines wrap mode
    ->Inserted Start Amount	Amount characters have been inserted/
    				deleted at Start (deleted: negative argument)
    ->ChangedRegion From To	Region [From, To) has changed

The textimage extracts information from the underlying text object using
a pointer to a function that returns information on a specific character
and its attributes:
    
    void (*fetch)(Any context, TextChar chr)

Each textimage contains the bitmap  image  and  an  array  of  text_line
structures  that  describe  the  current  contents  of  the screen.  The
insert, delete and change messages are used to maintain a summary of the
things that need be checked during the update.

The update phase does the following:

	*) determine the lines that need to be changed.
	*) fill a second -class maintained- text_line structure array
	   with the information of the lines to be changed.
	*) Find for each line a similar line in the current map line.
	   A similar line is an equal line, a line that is equal with
	   the exception of underline and invert attributes or a line
	   that is equal except for few insertions/deletions or a
	   combination of the two.
	*) Find the correct updating order, such that no information
	   will be overwritten during the copying process.
	*) Update the lines.
	*) Swap the text_line structures of the class with those of the
	   text_image to get this one upto date.

This process maintains an area that is the union of everything that  has
been changed, so we can forward this to the device's update algorithm.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#define INFINITE 0x3fffffff		/* 31-bit int, but will do anyhow */
#define Round(n, r)  ((((n) + ((r)-1)) / (r)) * (r))

		/********************************
		*          LOCAL TYPES		*
		********************************/

#define END_CUT  (1)			/* Line ends due to wrap: none */
#define END_WRAP (2)			/* Line ends due to wrap */
#define END_EOF  (4)			/* Line ends due to end-of-buffer */
#define END_NL	 (8)			/* Line ends due to newline */

#define X_RIGHT	 (-1)			/* extend to right margin */

forwards long	do_fill_line(TextImage, TextLine, long);
static TextLine line_from_y(TextImage ti, int y);
forwards status reinitTextImage(TextImage ti);
static int	char_from_x(TextLine tl, int x);
static void	copy_line_attributes(TextLine from, TextLine to);
static void	copy_line_chars(TextLine from, int start, TextLine to);
static void	ascent_and_descent_graphical(Graphical gr, int *, int *);
static void	ascent_and_descent_image(Image im, int *, int *);


		/********************************
		*       NEW/ALLOC/UNALLOC	*
		********************************/

static status
initialiseTextImage(TextImage ti, Any obj, Int w, Int h)
{ initialiseGraphical(ti, ZERO, ZERO, w, h);

  assign(ti, text,	   obj);
  assign(ti, start,        ZERO);
  assign(ti, end,	   ZERO);
  assign(ti, background,   getClassVariableValueObject(ti, NAME_background));
  assign(ti, wrap,	   getClassVariableValueObject(ti, NAME_wrap));
  assign(ti, tab_distance, getClassVariableValueObject(ti, NAME_tabDistance));

  return reinitTextImage(ti);
}


static void
unalloc_textline(TextLine l)
{ if ( l->chars != NULL )
  { unalloc(l->allocated * sizeof(struct text_char), l->chars);
    l->chars = NULL;
  }
}


static void
unalloc_screen(TextScreen s)
{ int i;

  if ( s->lines != NULL )
  { for(i=0; i<s->allocated; i++)
      unalloc_textline(&s->lines[i]);
  
    unalloc(s->allocated * sizeof(struct text_line), s->lines);
    s->lines = NULL;
  }

  unalloc(sizeof(struct text_screen), s);
}


static status
unlinkTextImage(TextImage ti)
{ unlinkGraphical((Graphical) ti);

  if ( ti->map != NULL )
  { unalloc_screen(ti->map);
    ti->map = NULL;
  }

  succeed;
}


static void
ensure_lines_screen(TextScreen s, int lines)
{ if ( s->allocated < lines )
  { TextLine new;
    int chars = (s->allocated > 0 ? s->lines[0].allocated : 80);
    int n;

    if ( lines > 500 )
      errorPce(NIL, NAME_tooManyScreenLines);

    lines = Round(lines, 8);
    new = alloc(lines * sizeof(struct text_line));
    DEBUG(NAME_allocated, Cprintf("Lines at %p, %ld bytes\n",
				  new,
				  (unsigned long) lines * sizeof(struct text_line)));

    for(n = 0; n < s->allocated; n++)	/* copy old lines */
      new[n] = s->lines[n];

    for( ; n < lines; n++)		/* create new ones */
    { new[n].chars = alloc(chars * sizeof(struct text_char));
      new[n].allocated = chars;
      new[n].changed = 0;
      new[n].start = -1;
    }

    if ( s->lines )
      unalloc(s->allocated * sizeof(struct text_line), s->lines);
    s->lines = new;
    s->allocated = lines;
  }
}


static void
ensure_chars_line(TextLine l, int chars)
{ if ( l->allocated < chars )
  { TextChar new;
    int n;

    chars = Round(chars, 16);
    new = alloc(chars * sizeof(struct text_char));

    for(n = 0; n < l->allocated; n++)
      new[n] = l->chars[n];

    if ( l->chars != NULL )
      unalloc(l->allocated * sizeof(struct text_char), l->chars);
    l->allocated = chars;
    l->chars = new;
  }
}

		 /*******************************
		 *	     LOAD/SAVE		*
		 *******************************/

static status
storeTextImage(TextImage ti, FileObj file)
{ return storeSlotsObject(ti, file);
}


static status
reinitTextImage(TextImage ti)
{ Any obj = ti->text;
  Elevation z;

  assign(ti, request_compute, ON);

  ti->w		   = valInt(ti->area->w);
  ti->h		   = valInt(ti->area->h);
  ti->change_start = 0;
  ti->change_end   = INFINITE;
  ti->inserted     = 0;

  ti->seek   = (SeekFunction)   get(obj, NAME_SeekFunction, EAV);
  ti->scan   = (ScanFunction)   get(obj, NAME_ScanFunction, EAV);
  ti->fetch  = (FetchFunction)  get(obj, NAME_FetchFunction, EAV);
  ti->margin = (MarginFunction) get(obj, NAME_MarginFunction, EAV);
  ti->rewind = (RewindFunction) get(obj, NAME_RewindFunction, EAV);

  if ( !ti->seek || !ti->scan || !ti->fetch )
    return errorPce(ti, NAME_noFetchFunction, obj);
  DEBUG(NAME_SeekFunction, Cprintf("ti->seek = %p\n", ti->seek));

  ti->map                  = alloc(sizeof(struct text_screen));
  ti->map->allocated       = ti->map->length = ti->map->skip = 0;
  ti->map->lines           = NULL;

  if ( (z = getClassVariableValueObject(ti, NAME_elevation)) && notNil(z) )
    assign(ti, pen, absInt(z->height));

  return obtainClassVariablesObject(ti);
}


static status
loadTextImage(TextImage ti, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(ti, fd, def));

  return reinitTextImage(ti);
}


static status
cloneTextImage(TextImage ti, TextImage clone)
{ clonePceSlots(ti, clone);
  
  return reinitTextImage(clone);
}



		/********************************
		*        TRAPPING CHANGES	*
		********************************/

static int
update_insert(int v, int w, int a)
{ if ( a > 0 )
    return w < v ? v+a : v;
  else
  { a = -a;
    if ( w + a < v ) return v - a;
    if ( w > v ) return v;
    return w;
  }      
}


status
InsertTextImage(TextImage ti, Int where, Int amount)
{ int w = valInt(where);
  int a = valInt(amount);
  int line;

  assign(ti, start, toInt(update_insert(valInt(ti->start), w, a)));
  assign(ti, end,   toInt(update_insert(valInt(ti->end), w, a)));

  if ( ti->map->lines != NULL )
  { for(line = 0; line <= ti->map->length; line++) /* Last as well!! */
    { TextLine tl = &ti->map->lines[line];

      tl->start = update_insert(tl->start, w, a);
      tl->end   = update_insert(tl->end, w, a);
    }
  }

  if ( w < ti->change_start )
    ti->change_start = w;
  if ( a > 0 )
  { if ( w+a > ti->change_end )
      ti->change_end = w+a;
  } else
  { if ( w+1 > ti->change_end )
      ti->change_end = w+1;
  }

  requestComputeGraphical(ti, DEFAULT);

  succeed;
}


status
ChangedRegionTextImage(TextImage ti, Int from, Int to)
{ if ( valInt(from) < ti->change_start )
    ti->change_start = valInt(from);
  if ( valInt(to) > ti->change_end )
    ti->change_end = valInt(to);
  requestComputeGraphical(ti, DEFAULT);

  succeed;
}


status
ChangedEntireTextImage(TextImage ti)
{ return ChangedRegionTextImage(ti, ZERO, toInt(INFINITE));
}


		/********************************
		*         FILLING INFO 		*
		********************************/

static int
tab(TextImage ti, int x)
{ x -= TXT_X_MARGIN;
  x++;

  if ( isNil(ti->tab_stops) )
  { int td = valInt(ti->tab_distance);

    x = Round(x, td);
  } else
  { int i;

    for(i=1; i<=valInt(ti->tab_stops->size); i++)
    { int s = valInt(getElementVector(ti->tab_stops, toInt(i)));

      if ( s >= x )
	return s + TXT_X_MARGIN;
    }

    x += 5;
  }

  x += TXT_X_MARGIN;

  return x;
}


static void
fill_dimensions_line(TextLine l)
{ FontObj f = NULL;
  int ascent = 0, descent = 0;
  TextChar tc, te;

  for(tc=l->chars, te=&l->chars[l->length]; tc<te; tc++)
  { int a, d;

    switch(tc->type)
    { case CHAR_GRAPHICAL:
	ascent_and_descent_graphical(tc->value.graphical, &a, &d);
	ascent  = max(ascent, a);
	descent = max(descent, d);
	break;
      case CHAR_IMAGE:
	ascent_and_descent_image(tc->value.image, &a, &d);
        ascent  = max(ascent, a);
	descent = max(descent, d);
	break;
      case CHAR_ASCII:
	if ( tc->font != f )
	{ f = tc->font;
      
	  assert(f);
	  a = valInt(getAscentFont(f));
	  d = valInt(getDescentFont(f));
	  ascent  = max(ascent, a);
	  descent = max(descent, d);
	}
        break;
    }
  }
  
  l->base = ascent;
  l->h = ascent + descent;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The function fill_line() fills  a line description,  assuming the line
starts at index `start' and will be displayed at `y' in the bitmap.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
do_fill_line(TextImage ti, TextLine l, long index)
{ short last_break = -1;
  int last_is_space = FALSE;
  TextChar tc;
  int x, i, left_margin, right_margin;  
  long start;

  l->ends_because = 0;
  start = l->start = index;

  (*ti->seek)(ti->text, index);
  if ( ti->margin )
    (*ti->margin)(ti->text, &left_margin, &right_margin);
  else
    left_margin = right_margin = 0;

  x = TXT_X_MARGIN + left_margin;
  if ( right_margin < 0 )
    right_margin = ti->w - TXT_X_MARGIN;
  else
    right_margin = ti->w - TXT_X_MARGIN - right_margin;

  for( i = 0, tc = l->chars; ; i++, tc++)
  { if ( l->allocated <= i )
    { ensure_chars_line(l, i+1);
      tc = &l->chars[i];
    }

    index = (*ti->fetch)(ti->text, tc);
    tc->index -= start;
    tc->x = x;

    switch(tc->type)
    { case CHAR_ASCII:
	switch(tc->value.c)
	{ case EOB:
	  case '\n':
	    x = right_margin;
	    l->ends_because |= END_NL;
	    l->length = ++i;
	    l->end = index;
	    if ( tc->value.c == EOB )
	    { index--;
	      l->ends_because |= END_EOF;
	    }
	    l->w = x;
	    ensure_chars_line(l, i+1);
	    tc = &l->chars[i];
	    tc->x = x;
	    fill_dimensions_line(l);
	    return index;
	  case '\t':
	    x = tab(ti, x);
	    last_is_space = TRUE;
	    break;
	  case ' ':
	    x += c_width(tc->value.c, tc->font);
	    last_is_space = TRUE;
	    break;
	  default:
	    x += c_width(tc->value.c, tc->font);
	    if ( last_is_space )
	      last_break = i;
	    last_is_space = FALSE;
	    break;
	}
        break;
      case CHAR_GRAPHICAL:
	ComputeGraphical(tc->value.graphical);

        x += valInt(tc->value.graphical->area->w);
	if ( last_is_space )
	  last_break = i;
	last_is_space = FALSE;
	break;
      case CHAR_IMAGE:
	x += valInt(tc->value.image->size->w);
        if ( last_is_space )
	  last_break = i;
        last_is_space = FALSE;
	break;
    }
    
    if ( x >= right_margin )
    { l->ends_because |= END_WRAP;

      if ( ti->wrap == NAME_none )
      { int eof;

	l->ends_because |= END_CUT;
	l->length = i;
	l->w = tc->x = right_margin;

	index = (*ti->scan)(ti->text, index, 1, TEXT_SCAN_FOR, EL, &eof) + 1;
	l->end = index;
	if ( eof )
	  l->ends_because |= END_EOF;
      } else if ( ti->wrap == NAME_character )
      { as_char:
	if ( index - l->start <= 1 )	/* make sure at least 1 character */
	{ i++;
	  index++;
	}
	l->length = i;
	index--;
	l->end = index;
	l->w = tc->x = x;
      } else if ( ti->wrap == NAME_word )
      { if ( last_break > 0 )
	{ int eof;

	  l->length = i = last_break;
	  l->w = l->chars[i].x;
	  index = l->start + l->chars[i].index;

	  (*ti->seek)(ti->text, index);
	  index = (*ti->scan)(ti->text, index, 1, TEXT_SKIP_OVER, BL, &eof);
	  l->end = index;
	} else				/* doesn't fit on line: as character */
	{ goto as_char;
	}
      }
      break;
    }
  }

  fill_dimensions_line(l);

  return index;
}


#define equal_text_char(c1, c2) ( (c1)->value.c == (c2)->value.c && \
				  (c1)->font == (c2)->font && \
				  (c1)->colour == (c2)->colour && \
				  (c1)->background == (c2)->background && \
				  (c1)->x == (c2)->x && \
				  (c1)->attributes == (c2)->attributes )

static long
fill_line(TextImage ti, int line, long int index, short int y)
{ TextLine l;

  ensure_lines_screen(ti->map, line+1);
  l = &ti->map->lines[line];

  if ( l->start == index && l->changed < 0 &&
       (l->end < ti->change_start || l->start >= ti->change_end) )
  { if ( l->y != y )
    { l->y = y;
      l->changed = 0;
    }
    return ti->map->lines[line+1].start;
  }
  
  if ( l->y != y )
  { l->y = y;
    l->changed = 0;

    return do_fill_line(ti, l, index);
  } else
  { static struct text_line tmp;
    long idx;
    
    if ( !tmp.chars )
    { tmp.chars = alloc(80 * sizeof(struct text_char));
      tmp.allocated = 80;
    }

    idx = do_fill_line(ti, &tmp, index);
    l->start        = tmp.start;
    l->end          = tmp.end;
    l->ends_because = tmp.ends_because;
    
    if ( l->h != tmp.h || l->base != tmp.base )
    { l->changed = 0;
      copy_line_attributes(&tmp, l);
      l->y = y;				/* overruled by copy_line_attributes */
      copy_line_chars(&tmp, 0, l);

      return idx;
    } else
    { int i;
      int n = min(l->length, tmp.length);

      ensure_chars_line(l, tmp.length);
      for(i=0; i<n; i++)
      { if ( !equal_text_char(&tmp.chars[i], &l->chars[i]) )
	{ l->changed = i;
	  copy_line_chars(&tmp, i, l);
	  l->length = tmp.length;

	  return idx;
	}
      }
      if ( i < tmp.length )
      { l->changed = i;
	copy_line_chars(&tmp, i, l);
      } 
      if ( tmp.length < l->length )
	l->changed = tmp.length;
      l->length = tmp.length;

      if ( l->w != tmp.w )
      { /*Cprintf("Line changed width, ->changed = %d\n", l->changed);*/

	if ( l->chars[l->length].x == l->w )
	  l->chars[l->length].x = tmp.w;
	l->w = tmp.w;
	l->changed = l->length-1;
      } 

      return idx;
    }
  }
}


static status
updateMapTextImage(TextImage ti)
{ if ( ti->change_end > ti->change_start )
  { Bool eof_in_window = OFF;
    int line;
    short y = TXT_Y_MARGIN;
    long index = valInt(ti->start);
  
    DEBUG(NAME_text, Cprintf("Updating map from %d to %d ",
			     ti->change_start, ti->change_end));

    if ( ti->rewind )
      (*ti->rewind)(ti->text);

    for(line = 0; ; line++)
    { long next_index;

      next_index = fill_line(ti, line, index, y);
      DEBUG(NAME_text,
	    Cprintf("Line %d %4ld..%4ld (changed = %d, y=%d, h=%d)\n",
		    line, index, next_index, ti->map->lines[line].changed,
		    y, ti->map->lines[line].h));
      if ( line >= ti->map->skip )
	y += ti->map->lines[line].h;

      if ( y > ti->h - TXT_Y_MARGIN && line > 0 )
      { ti->map->length = line - ti->map->skip;
	assign(ti, end, toInt(index));
	assign(ti, eof_in_window, eof_in_window);
	ti->change_start = INFINITE;
	ti->change_end = 0;
	DEBUG(NAME_text, Cprintf("ok; eof_in_window = %s\n",
				 pp(eof_in_window)); );

	succeed;
      }

      index = next_index;
      if ( ti->map->lines[line].ends_because & END_EOF )
      	eof_in_window = ON;
    }
  }
  
  succeed;
}


		/********************************
		*          DUMP THE MAP		*
		********************************/

static void
dump_map(TextScreen map)
{ int i;

  Cprintf("skip = %d; length = %d, allocated = %d lines\n",
	  map->skip, map->length, map->allocated);

  for(i=0; i<map->skip + map->length; i++)
  { TextLine l = &map->lines[i];
    int n;
    int c;

    if ( i < map->skip )
      Cprintf("--:");
    else
      Cprintf("%2d:", i - map->skip);
    Cprintf("%4ld-%4ld at y=%3d changed = %d ",
	    l->start, l->start + l->length, l->y, l->changed);
    Cputchar((l->ends_because & END_EOF)  ? 'F' : '-');
    Cputchar((l->ends_because & END_WRAP) ? 'W' : '-');
    Cputchar((l->ends_because & END_CUT)  ? 'C' : '-');
    Cputchar((l->ends_because & END_NL)   ? 'L' : '-');
    Cprintf(": \"");
    for(n=0; n < 5 && n < l->length; n++)
    { if ( (c = l->chars[n].value.c) == '\n' )
	Cprintf("\\n");
      else if ( c == EOB )
	Cprintf("\\$");
      else
	Cputchar(c);
    }
    if ( l->length - 5 > n )
    { Cprintf(" ... ");
      n = l->length - 5;
    }
    for( ; n < l->length; n++ )
    { if ( (c = l->chars[n].value.c) == '\n' )
	Cprintf("\\n");
      else if ( c == EOB )
	Cprintf("\\$");
      else
	Cputchar(c);
    }
    Cprintf("\"\n");
  }
}

static status
dumpMapTextImage(TextImage ti)
{ dump_map(ti->map);

  succeed;
}


		/********************************
		*      PAINTING PRIMITIVES	*
		********************************/

static void
t_underline(int x, int y, int w, Colour c)
{ static int ex = 0, ey = 0, ew = 0;
  static Colour cc = NIL;

  if ( x == ex+ew && y == ey && c == cc )
  { ew += w;
  } else
  { if ( ew > 0 )
    { r_colour(cc);
      r_line(ex, ey, ex+ew, ey);
    }
    ex = x, ey = y, ew = w;
    cc = c;
  }
}


static void
t_invert(int x, int y, int w, int h)
{ static int ix=0, iy=0, iw=0, ih=0;

  if ( iw == 0 && ih == 0 )
  { ix = x, iy = y, iw = w, ih = h;
  } else
  { if ( iy == y && ih == h && ix + iw == x )
    { iw += w;
      return;
    }
  }

  r_complement(ix, iy, iw, ih);
  ix=0, iy=0, iw=0, ih=0;
}


static void
t_grey(int x, int y, int w, int h)
{ static int ix=0, iy=0, iw=0, ih=0;

  if ( iw == 0 && ih == 0 )
  { ix = x, iy = y, iw = w, ih = h;
  } else
  { if ( iy == y && ih == h && ix + iw == x )
    { iw += w;
      return;
    }
  }

  r_and(ix, iy, iw, ih, GREY50_IMAGE);
  ix=0, iy=0, iw=0, ih=0;
}


		 /*******************************
		 *	GRAPHICS PAINTING	*
		 *******************************/

static void
ascent_and_descent_graphical(Graphical gr, int *ascent, int *descent)
{ Point r;

  if ( instanceOfObject(gr, ClassDialogItem) )
  { if ( (r = qadGetv(gr, NAME_reference, 0, NULL)) )
      *ascent = valInt(r->y);
    else
      *ascent = valInt(gr->area->h);
  } else if ( onFlag(gr, F_ATTRIBUTE) &&
	      (r = getAttributeObject(gr, NAME_reference)) )
  { *ascent = valInt(r->y);
  } else
    *ascent = valInt(gr->area->h);

  if ( descent )
    *descent = valInt(gr->area->h) - *ascent;
}


static void
ascent_and_descent_image(Image im, int *ascent, int *descent)
{ if ( notNil(im->hot_spot) )
    *ascent = valInt(im->hot_spot->y);
  else
    *ascent = valInt(im->size->h);
  
  if ( descent )
    *descent = valInt(im->size->h) - *ascent;
}


static void
paint_graphical(TextImage ti, Area a, Graphical gr, int x, int base)
{ int dx, dy;
  int asc;
  Int ox = a->x;
  Int oy = a->y;

  ascent_and_descent_graphical(gr, &asc, NULL);
  dx = x    - valInt(gr->area->x);
  dy = base - (valInt(gr->area->y) + asc);

  r_offset(dx, dy);
  assign(a, x, toInt(valInt(a->x) - dx));
  assign(a, y, toInt(valInt(a->y) - dy));
  RedrawArea(gr, a);
  assign(a, x, ox);
  assign(a, y, oy);
  r_offset(-dx, -dy);
}


static void
paint_image(TextImage ti, Area a, Image im, int x, int base)
{ int asc;

  ascent_and_descent_image(im, &asc, NULL);

  DEBUG(NAME_image, Cprintf("Painting %s at %d, %d\n", pp(im), x, base));

  r_image(im,
	  0, 0,
	  x, base - asc, valInt(im->size->w), valInt(im->size->h),
	  ON);
}



		/********************************
		*            PAINTING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Paint a line from index `from' to index `to'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
paint_attributes(TextImage ti, TextLine l, int from, int to, Colour c)
{ unsigned char atts = l->chars[from].attributes;
  
  if ( atts & TXT_UNDERLINED )
  { t_underline(l->chars[from].x, l->y + l->h - 1,
		l->chars[to].x - l->chars[from].x, c);
  }
  if ( atts & TXT_HIGHLIGHTED )
  { int w = (to == l->length ? ti->w - TXT_X_MARGIN : l->chars[to].x);

    t_invert(l->chars[from].x, l->y,
	     w - l->chars[from].x, l->h);
  }
  if ( atts & TXT_GREYED )
  { t_grey(l->chars[from].x, l->y,
	   l->chars[to].x - l->chars[from].x, l->h);
  }
}


#define PutBuf(c) if ( b16 ) \
		   { *(char16 *) out = c; \
		     out += sizeof(char16); \
		   } else \
		   { *(char8 *) out = c; \
		     out += sizeof(char8); \
		   }

static void
paint_line(TextImage ti, Area a, TextLine l, int from, int to)
{ char buf[1000];
  char *out;
  int b16, n, s = from, e;
  FontObj f;
  Colour c;
  Any bg;
  unsigned char atts;
  int cx, cw;
  int pen = valInt(ti->pen);
  int rmargin = ti->w - TXT_X_MARGIN;

  DEBUG(NAME_text, Cprintf("painting line %p from %d to %d\n",
			   l, from, to));

  cx = (from == 0 ? pen : l->chars[from].x);
  cw = (to >= l->length ? rmargin : l->chars[to].x) - cx;
  r_clear(cx, l->y, cw, l->h);

  { TextChar last = &l->chars[to-1];

    if ( last->value.c == EOB )
      to--;
  }

  for( s = from; s < to; s = e )
  { int prt;
    int chr = l->chars[s].value.c;

    e = s;

    c      = l->chars[e].colour;
    bg     = l->chars[e].background;

    switch(l->chars[e].type)
    { case CHAR_GRAPHICAL:
	if ( notDefault(bg) && !instanceOfObject(bg, ClassElevation) )
	{ int x  = l->chars[s].x;
	  int tx = l->chars[s+1].x;
	  r_fill(x, l->y, tx-x, l->h, bg);
	}
	paint_graphical(ti, a,
			l->chars[e].value.graphical, 
			l->chars[e].x,
			l->y + l->base);
        e++;
	paint_attributes(ti, l, s, e, c);
	continue;
      case CHAR_IMAGE:
	if ( notDefault(bg) && !instanceOfObject(bg, ClassElevation) )
	{ int x  = l->chars[s].x;
	  int tx = l->chars[s+1].x;
	  r_fill(x, l->y, tx-x, l->h, bg);
	}
	r_colour(c);
	paint_image(ti, a,
		    l->chars[e].value.image, 
		    l->chars[e].x,
		    l->y + l->base);
	e++;
	paint_attributes(ti, l, s, e, c);
        continue;
    }

    n = 0;
    f      = l->chars[e].font;
    b16    = (f->b16 == ON);
    atts   = l->chars[e].attributes;
    out    = buf;

    PutBuf(chr);

    if ( chr == '\t' )			/* print tabs */
    { prt = FALSE;

      for(n++, e++; e < to; n++, e++)
      { if ( l->chars[e].type != CHAR_ASCII ||
	     l->chars[e].attributes != atts ||
	     l->chars[e].background != bg ||
	     l->chars[e].value.c != '\t' )
	  break;
      }
    } else if ( chr == '\n' )		/* newline */
    { prt = FALSE;

      e++;
    } else				/* real text */
    { prt = TRUE;

      for(n++, e++; e < to; n++, e++)
      { if ( l->chars[e].font != f ||
	     l->chars[e].colour != c ||
	     l->chars[e].background != bg ||
	     l->chars[e].attributes != atts ||
	     l->chars[e].value.c == '\t' ||
	     l->chars[e].value.c == '\n' )
	  break;
	
	PutBuf(l->chars[e].value.c);
      }
    }

    if ( notDefault(bg) )
    { if ( instanceOfObject(bg, ClassElevation) )
      { int f, t, x, tx;

	for(f=s-1; f>=0 && l->chars[f].background == bg; f--)
	  ;
	f++;
	for(t=e; t<l->length && l->chars[t].background == bg; t++)
	  ;

	x  = l->chars[f].x;
	tx = l->chars[t].x;
	r_3d_box(x, l->y, tx-x, l->h, 0, bg, TRUE);
      } else
      { int x  = l->chars[s].x;
	int tx = l->chars[e].x;
	r_fill(x, l->y, tx-x, l->h, bg);
      }
    }

    if ( prt )
    { r_colour(c);

      if ( b16 )
	s_print16((char16 *)buf, e - s, l->chars[s].x, l->y + l->base, f);
      else
	s_print8((char8 *)buf, e - s, l->chars[s].x, l->y + l->base, f);

      if ( atts & TXT_BOLDEN )
      { if ( b16 )
	{ s_print16((char16 *)buf, e - s, l->chars[s].x+1, l->y + l->base, f);
	  s_print16((char16 *)buf, e - s, l->chars[s].x, l->y-1 + l->base, f);
	} else
	{ s_print8((char8 *)buf, e - s, l->chars[s].x+1, l->y + l->base, f);
	  s_print8((char8 *)buf, e - s, l->chars[s].x, l->y-1 + l->base, f);
	}
      }
    }

    paint_attributes(ti, l, s, e, c);
  }

  t_underline(0, 0, 0, NIL);
}


static void
paint_area(TextImage ti, Area a, int x, int y, int w, int h)
{ int p = valInt(ti->pen);

  if ( x < ti->w - TXT_X_MARGIN && x+w >= TXT_X_MARGIN &&
       y < ti->h + TXT_Y_MARGIN && y+h >= TXT_Y_MARGIN )
  { TextLine ml = line_from_y(ti, y);
    int line = ml - &ti->map->lines[ti->map->skip];
    int ly = 0;
  
    for(line = 0; line < ti->map->length && ml->y < y+h; line++, ml++)
    { if ( ml->y + ml->h > y )
      { int f, t;

	if ( ml->y + ml->h > ti->h - TXT_Y_MARGIN )
	  break;
  
	f = char_from_x(ml, x);
	t = char_from_x(ml, x+w);
  
	paint_line(ti, a, ml, f, t+1);	/* TBD: get correct boundaries */
	ly = ml->y + ml->h;
      }
    }  

    if ( y + h > ly )
      r_clear(p, ly, ti->w-2*p, y+h-ly);
  }

  if ( y < TXT_Y_MARGIN )
    r_clear(p, p, ti->w-2*p, TXT_Y_MARGIN-p);
  if ( x+w >= ti->w - TXT_X_MARGIN )
    r_clear(ti->w - TXT_X_MARGIN, p, TXT_X_MARGIN-p, ti->h-2*p);
}


		/********************************
		*        INDEX <-> POSITION	*
		********************************/

static TextLine
line_from_y(TextImage ti, int y)
{ if ( ti->map && ti->map->lines )
  { int l = ti->map->skip;
    int h = ti->map->length - 1;
    int m;
    TextLine tl;

    if ( y < ti->map->lines[l].y )
      return &ti->map->lines[l];
    if ( y >= ti->map->lines[h].y + ti->map->lines[h].h )
      return &ti->map->lines[h];

    for(;;)
    { m = (l+h) / 2;
      tl = &ti->map->lines[m];

      if ( y >= tl->y )
      { if ( y < tl->y + tl->h )
	  return tl;
	l = (l == m ? l+1 : m);
      } else
	h = m;
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the character index from a given X-pixel coordinate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
char_from_x(TextLine tl, int x)
{ int l = 0;
  int h = tl->length - 1;
  int m;

  if ( x < tl->chars[l].x )
    return l;
  if ( x >= tl->chars[h+1].x )
    return h;

  for(;;)
  { m = (l+h) / 2;

    if ( x >= tl->chars[m].x )
    { if ( x < tl->chars[m+1].x )
	return m;
      l = (l == m ? l+1 : m);
    } else
      h = m;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the X-Y position of character   at  index `pos'.  The top-left
corner is defined to be (1,1).

In the current implementation both the Y- and X-search is linear.  These
should be changed to binary searches someday,   but  this routine is not
uterly time critical.

In X-direction, we first do a quick   test  hoping the characters in the
line are adjecent (i.e.  no characters are hidden).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
get_xy_pos(TextImage ti, Int pos, int *x, int *y)
{ int line;
  int index = valInt(pos);
  int skip;

  ComputeGraphical(ti);
  skip = ti->map->skip;
  
  for(line=0; line < ti->map->length; line++)
  { TextLine l = &ti->map->lines[skip + line];
  
					/* binary search? */
    if ( index >= l->start && index < l->end )
    { if ( x )
      { int li = index - l->start;	/* X-index in line */

	if ( li > l->length || l->chars[li].index != li ) /* Quick test */
	{ if ( l->length > 0 && li > l->chars[l->length-1].index )
	    li = l->length - 1;
	  else
	  { TextChar ch = l->chars;
	    TextChar lc = &l->chars[l->length];
	
					/* binary search! */
	    while(ch < lc && ch->index < li)
	      ch++;

	    li = ch - l->chars;
	  }
	}
      
	*x = li + 1;
      }
      if ( y )
	*y = line + 1;
      succeed;
    }
  }
  
  fail;
}


status
get_character_box_textimage(TextImage ti, int index,
			    int *x, int *y, int *w, int *h, int *b)
{ int cx, cy;

  if ( get_xy_pos(ti, toInt(index), &cx, &cy) )
  { TextLine l  = &ti->map->lines[cy-1+ti->map->skip];
    TextChar tc = &l->chars[cx-1];
    
    *x = tc->x; *y = l->y, *w = tc[1].x - tc->x; *h = l->h; *b = l->base;

    succeed;
  }

  fail;
}


static int
get_index_text_image(TextImage ti, int x, int y)
{ int line;
  int skip;

  ComputeGraphical(ti);
  skip = ti->map->skip;

  if ( y < TXT_Y_MARGIN )
    return valInt(ti->start);

  for(line=0; line < ti->map->length; line++)
  { TextLine l = &ti->map->lines[line + skip];
  
    if ( y >= l->y && y < l->y + l->h )
    { int i;

      if ( x < TXT_X_MARGIN )
        return l->start;

      for(i = 0; i < l->length; i++)
        if ( l->chars[i+1].x > x )
	  return l->start + l->chars[i].index; /* bsearch()! */
      
      return l->start + l->length - 1;
    }
  }
  
  return valInt(ti->end) - 1;
}


Int
getLinesTextImage(TextImage ti)
{ ComputeGraphical(ti);

  answer(toInt(ti->map->length));
}


static Int
getLineTextImage(TextImage ti, Int pos)
{ int cy;

  if ( get_xy_pos(ti, pos, NULL, &cy) )
    answer(toInt(cy));

  fail;  
}

		/********************************
		*            EVENTS		*
		********************************/

static status
resetTextImage(TextImage ti)
{ if ( notNil(ti->pointed) )
  { DeviceGraphical(ti->pointed, NIL);
    assign(ti, pointed, NIL);
  }

  succeed;
}


Int
getIndexTextImage(TextImage ti, EventObj ev)
{ Int X, Y;
  int x, y;
  
  get_xy_event(ev, ti, ON, &X, &Y);
  x = valInt(X);
  y = valInt(Y);

  if ( x < 0 || x > ti->w )
    fail;

  answer(toInt(get_index_text_image(ti, x, y)));
}


static status
updatePointedTextImage(TextImage ti, EventObj ev, long *where)
{ Int x, y;
  TextLine tl = NULL;
  TextChar tc = NULL;
  Graphical gr;

  if ( isAEvent(ev, NAME_areaExit) )
  { if ( notNil(ti->pointed) )
    { PceWindow sw = getWindowGraphical((Graphical) ti->device);

      if ( sw )
      { if ( sw->focus == ti->pointed )
	  focusWindow(sw, NIL, NIL, NIL, NIL);
	else if ( subGraphical(ti->pointed, sw->keyboard_focus) )
	  keyboardFocusWindow(sw, NIL);
      }
    }	     

    gr = NIL;
    
  } else
  { get_xy_event(ev, ti, ON, &x, &y);
    if ( (tl = line_from_y(ti, valInt(y))) &&
	 (tc = &tl->chars[char_from_x(tl, valInt(x))]) &&
	 tc->type == CHAR_GRAPHICAL )
    { *where = tl->start + tc->index;
      gr = tc->value.graphical;
    } else
      gr = NIL;
  }

  if ( gr != ti->pointed )
  { Name enter, exit;

    if ( allButtonsUpEvent(ev) )
    { enter = NAME_areaEnter;
      exit  = NAME_areaExit;
    } else
    { enter = NAME_areaResume;
      exit  = NAME_areaCancel;
    }

    if ( notNil(ti->pointed) )
      generateEventGraphical(ti->pointed, exit);

    assign(ti, pointed, gr);

    if ( notNil(gr) )
    { Int ty, tx = toInt(valInt(ti->area->x) + tc->x);
      int asc;

      ascent_and_descent_graphical(gr, &asc, NULL);
      ty = toInt(valInt(ti->area->y) + tl->y + tl->base - asc);

      doSetGraphical(gr, tx, ty, DEFAULT, DEFAULT);

      generateEventGraphical(ti->pointed, enter);
    }
  }

  succeed;
}


static CursorObj
getDisplayedCursorTextImage(TextImage ti)
{ if ( notNil(ti->pointed) )
  { CursorObj c;

    if ( notNil(c=qadGetv(ti->pointed, NAME_displayedCursor, 0, NULL)) )
      answer(c);

    answer(NIL);
  }

  answer(ti->cursor);
}


static status
eventTextImage(TextImage ti, EventObj ev)
{ if ( eventGraphical(ti, ev) )
  { succeed;
  } else
  { long where;

    updatePointedTextImage(ti, ev, &where);

    if ( notNil(ti->pointed) )
    { Graphical gr = ti->pointed;
      status rval;
      PceWindow sw = getWindowGraphical((Graphical) ti->device);
      Area a = gr->area;
      Int ow = a->w, oh = a->h;

      DeviceGraphical(gr, ti->device);
      DisplayedGraphical(gr, ON);
      rval = postEvent(ev, gr, DEFAULT);
      if ( sw && (sw->focus == gr || sw->keyboard_focus == gr) )
      { DisplayObj d = getDisplayGraphical((Graphical) sw);
	TextCursor tc = NIL;
	Any tcon = NIL;

	if ( sw->keyboard_focus == gr &&
	     instanceOfObject(ti->device, ClassEditor) )
	{ Editor e = (Editor) ti->device;
	  tc = e->text_cursor;
	  if ( notNil(tc) )
	    tcon = tc->active;
	  send(tc, NAME_active, OFF, EAV);
	}

	while( !onFlag(sw, F_FREED|F_FREEING) &&
	       (sw->focus == gr || sw->keyboard_focus == gr) )
	{ if ( dispatchDisplay(d) )
	    ws_discard_input("Focus on graphical in editor");
	}

	if ( notNil(tcon) && !onFlag(tc, F_FREED|F_FREEING) )
	  send(tc, NAME_active, tcon, EAV);
      }
      if ( !onFlag(gr, F_FREED|F_FREEING) &&
	   !onFlag(ti, F_FREED|F_FREEING) )
      { DeviceGraphical(gr, NIL);
	a = gr->area;

	if ( ow != a->w || oh != a->h )
	{ DEBUG(NAME_diagram, Cprintf("%s: Changed %d\n", pp(ti), where));
	  ChangedRegionTextImage(ti, toInt(where), toInt(where+1));
	}
      }

      return rval;
    }
  }

  fail;
}



		/********************************
		*            REDRAW		*
		********************************/
static status
RedrawAreaTextImage(TextImage ti, Area a)
{ int x, y, w, h;
  int bx, by, bw, bh;
  int sx, sy;
  int p = valInt(ti->pen);
  int ox = valInt(ti->area->x);
  int oy = valInt(ti->area->y);
  Any obg;

  initialiseDeviceGraphical(ti, &x, &y, &w, &h);
  bx = x, by = y, bw = w, bh = h;

  sx = valInt(a->x) - valInt(ti->area->x); if ( sx < p ) sx = p;
  sy = valInt(a->y) - valInt(ti->area->y); if ( sy < p ) sy = p;
  x += sx, w -= sx + p;
  y += sy, h -= sy + p;
  if ( w > valInt(a->w) ) w = valInt(a->w);
  if ( h > valInt(a->h) ) h = valInt(a->h);

  obg = r_background(ti->background);
  if ( sx < TXT_X_MARGIN || sx + w > ti->w - TXT_X_MARGIN ||
       sy < TXT_Y_MARGIN || sy + h > ti->h - TXT_Y_MARGIN )
  { Elevation z = getClassVariableValueObject(ti, NAME_elevation);
    
    if ( z && notNil(z) )
    { r_3d_box(bx, by, bw, bh, 0, z, FALSE);
    } else
    { r_thickness(p);
      r_dash(ti->texture);
      r_box(bx, by, bw, bh, 0, NIL);
    }
  }
  r_offset(ox, oy);
  r_thickness(1);			/* default for underlining */
  r_dash(NAME_none);
  paint_area(ti, a, sx, sy, w, h);
  r_offset(-ox, -oy);
  r_background(obg);

  return RedrawAreaGraphical(ti, a);
}


status
computeTextImage(TextImage ti)
{ if ( notNil(ti->request_compute) )
  { TextLine ml;
    int line;
    int fy = 0, ty = 0, fx = 100000, tx = ti->w - TXT_X_MARGIN;

    updateMapTextImage(ti);

    ml = &ti->map->lines[ti->map->skip];
    for(line = 0; line < ti->map->length; line++, ml++)
    { int cy = ml->y + ml->h;

      if ( cy > ti->h - TXT_Y_MARGIN )
      { if ( fy != ty )
	  ty = cy;
	break;
      }
	
      if ( ml->changed >= 0 )
      { int cx;

	if ( line == ti->map->length - 1 ) /* last line */
	  cy = ti->h - valInt(ti->pen);

	if ( fy == ty )
	{ fy = ml->y;
	  ty = cy;
	} else
	  ty = cy;

	if ( ml->changed == 0 )
	  cx = TXT_X_MARGIN;
	else
	  cx = ml->chars[ml->changed].x;
	if ( cx < fx )
	  fx = cx;

	ml->changed = -1;
      }
    }

    DEBUG(NAME_text, Cprintf("changedImageGraphical(%s, %d, %d, %d, %d)\n",
			     pp(ti), fx, fy, tx-fx, ty-fy));
    if ( ty > fy )
      changedImageGraphical(ti,
			    toInt(fx), toInt(fy), toInt(tx-fx), toInt(ty-fy));

    assign(ti, request_compute, NIL);
  }

  succeed;
}


		/********************************
		*       CHANGING PARAMETERS	*
		********************************/

status
startTextImage(TextImage ti, Int start, Int skip)
{ TextScreen map = ti->map;
  
  if ( isDefault(skip) )
    skip = ZERO;
  if ( isDefault(start) )
    start = ti->start;

  if ( ti->start != start ||
       map->skip != valInt(skip) )
  { assign(ti, start, start);

    if ( map->skip != valInt(skip) )
    { int sl = 0;
      int el = map->length + map->skip;
      short y = TXT_Y_MARGIN;

      map->skip = valInt(skip);
      
      for( ; sl < el; sl++ )
      { map->lines[sl].y = y;
	if ( sl >= map->skip )
	  y += map->lines[sl].h;
      }
    }

    return ChangedEntireTextImage(ti);
  }

  succeed;
}


static int
locate_screen_line(TextScreen map, int pos)
{ int i;

  for(i=0; i < map->skip + map->length; i++)
  { if ( pos >= map->lines[i].start &&
	 pos <  map->lines[i].end )
      return i;
  }

  return -1;				/* not in the map */
}


static void
copy_line_attributes(TextLine from, TextLine to)
{ to->y      = from->y;
  to->h      = from->h;
  to->base   = from->base;
  to->length = from->length;
  to->w      = from->w;
}


static void
copy_line_chars(TextLine from, int start, TextLine to)
{ int end = from->length+1;

  ensure_chars_line(to, end);

  for( ; start < end; start++ )
    to->chars[start] = from->chars[start];    
}


static void
copy_line(TextLine from, TextLine to)
{ copy_line_attributes(from, to);
  copy_line_chars(from, 0, to);
}


static long
paragraph_start(TextImage ti, long int pos)
{ int eof;
  long index;
  
  index = (*ti->scan)(ti->text, pos-1, -1, TEXT_SCAN_FOR, EL, &eof);

  return eof ? index : index + 1;
}


static void
shift_lines_down(TextScreen map, int from, int n)
{ int i;

  if ( map->skip + map->length + n > map->allocated )
    ensure_lines_screen(map, map->skip + map->length + n);

  for(i = map->skip + map->length + n - 1; i >= n + from; i--)
  { TextLine fl = &map->lines[i-n];
    TextLine tl = &map->lines[i];

    copy_line(fl, tl);
    tl->start   = fl->start;
    tl->end     = fl->end;
    tl->w       = fl->w;
    tl->changed = fl->changed;
  }

  map->length += n;
}


static status
center_from_screen(TextImage ti, long int pos, int line)
{ TextScreen map = ti->map;
  int l;

  if ( (l = locate_screen_line(map, pos)) >= 0 &&
        l >= line )
  { int startline = l - line;
    int skip = 0;

    while( startline > 0 &&
	   !(map->lines[startline-1].ends_because & END_NL) )
    { startline--;
      skip++;
    }
    DEBUG(NAME_center, Cprintf("Start at %ld; skip = %d\n",
			       map->lines[startline].start, skip));

    startTextImage(ti, toInt(map->lines[startline].start), toInt(skip));

    succeed;
  }

  DEBUG(NAME_center, Cprintf("Out of screen: l = %d\n", l));
  fail;
}


status
centerTextImage(TextImage ti, Int position, Int screen_line)
{ int pos = valInt(position);
  int line;
  TextScreen map = ti->map;

  ComputeGraphical(ti);
  line = (isDefault(screen_line) ? ti->map->length/2 : valInt(screen_line)-1);
  if ( line < 0 )
    line = 0;

  DEBUG(NAME_center, writef("%s: center %d at line %d\n",
			    ti, position, toInt(line)));

					/* Info on the screen: simple */
  if ( center_from_screen(ti, pos, line) )
    succeed;
  else
  { long here = pos;
    long start;

    map->length = map->skip = 0;	/* empty the map */
    ChangedEntireTextImage(ti);		/* recompute next time */

    for( ; (start = paragraph_start(ti, here)) > 0; here = start-1 )
    { long idx = start;
      int ln = 0;

      DEBUG(NAME_center, Cprintf("ParStart = %ld\n", start));
      do
      { shift_lines_down(map, ln, 1);
	idx = fill_line(ti, ln, idx, 0);
	DEBUG(NAME_center, Cprintf("Filled line %d to %ld\n", ln-1, idx));
      } while ( idx <= here &&
	        !(ti->map->lines[ln++].ends_because & END_EOF) );

      if ( center_from_screen(ti, pos, line) )
	succeed;
    }

    return startTextImage(ti, ZERO, ZERO); /* best we can do */
  }
  
}


Int
getStartTextImage(TextImage ti, Int line)
{ int ln = isDefault(line) ? 1 : valInt(line);
  TextScreen map = ti->map;
  static struct text_line tl;		/* reusable dummy line */
  
  ComputeGraphical(ti);

  if ( ln >= 0 )
  { ln--;
  } else
  { ln += map->length;
  }

  DEBUG(NAME_start, Cprintf("Looking for start of line %d\n", ln));

  if ( ln < 0 )
  { if ( -ln <= map->skip )
    { answer(toInt(map->lines[map->skip + ln].start));
    } else
    { long here = map->lines[0].start;
      long start;

      ln = -ln - map->skip;		/* lines before idx */
      do
      { long idx = start = paragraph_start(ti, here-1);
	DEBUG(NAME_start, Cprintf("start = %ld; here = %ld\n", start, here));
	do
	{ idx = do_fill_line(ti, &tl, idx);
	  DEBUG(NAME_start, Cprintf("line to %ld; ln = %d\n", idx, ln));
	  if ( --ln == 0 )
	    answer(toInt(idx));
	} while( idx < here );
	here = start;
      } while(start > 0);

      answer(ZERO);			/* start of buffer */
    }
  } else if ( ln >= map->length )
  { int li = map->skip + map->length - 1;
    long idx = (li >= 0 ? map->lines[li].start : 0);

    for( ln -= map->length - 1; ln > 0; ln-- )
    { DEBUG(NAME_start, Cprintf("ln = %d; idx = %ld\n", ln, idx));

      idx = do_fill_line(ti, &tl, idx);
      if ( tl.ends_because & END_EOF )
	break;
    }

    answer(toInt(idx));
  }

  answer(toInt(map->lines[map->skip + ln].start));
}


static status
wrapTextImage(TextImage ti, Name wrap)
{ if ( ti->wrap != wrap )
  { assign(ti, wrap, wrap);
    ChangedEntireTextImage(ti);
  }

  succeed;
}


static status
backgroundTextImage(TextImage ti, Any bg)
{ if ( ti->background != bg )
  { assign(ti, background, bg);
    changedEntireImageGraphical(ti);		/* only visual effect */
  }

  succeed;
}


status
tabDistanceTextImage(TextImage ti, Int tab)
{ if ( ti->tab_distance != tab )
  { assign(ti, tab_distance, tab);
    ChangedEntireTextImage(ti);
  }

  succeed;
}


status
tabStopsTextImage(TextImage ti, Vector v)
{ if ( isNil(v) )
    assign(ti, tab_stops, v);
  else
  { int i;

    for(i=1; i<valInt(v->size); i++)
    { Int s;

      if ( !(s = checkType(getElementVector(v, toInt(i)), TypeInt, NIL)) )
	return errorPce(v, NAME_elementType, toInt(i), TypeInt);
      elementVector(v, toInt(i), s);
    }

    assign(ti, tab_stops, v);
  }

  succeed;
}


static status
geometryTextImage(TextImage ti, Int x, Int y, Int w, Int h)
{
#define Changed(a) ( notDefault(a) && (a) != ti->area->a )

  if ( Changed(w) || Changed(h) )	/* resize */
  { geometryGraphical(ti, x, y, w, h);
    ti->w = valInt(ti->area->w);
    ti->h = valInt(ti->area->h);
    ChangedEntireTextImage(ti);
  } else
    geometryGraphical(ti, x, y, DEFAULT, DEFAULT); /* move only */
#undef Changed

  succeed;
}

		/********************************
		*         GET ATTRIBUTES	*
		********************************/

static Int
getWidthTextImage(TextImage ti)
{ answer(toInt(ti->w));
}


static Int
getHeightTextImage(TextImage ti)
{ answer(toInt(ti->h));
}


Int
getViewTextImage(TextImage ti)
{ answer(sub(ti->end, ti->start));
}


		 /*******************************
		 *    PRECISE SCROLL SUPPORT	*
		 *******************************/

#define MAXPLINES 1000

typedef struct pline
{ int y;				/* y of the physical line */
  long start;				/* start index */
} *PLine;


static TextLine
tmpLine()
{ static struct text_line tmp;

  if ( !tmp.chars )
  { tmp.chars = alloc(80 * sizeof(struct text_char));
    tmp.allocated = 80;
  }

  return &tmp;
}


status
bubbleScrollBarTextImage(TextImage ti, ScrollBar sb)
{ TextLine tmp = tmpLine();
  long index = 0;
  int start = -1;
  int view = ti->h - 2*TXT_Y_MARGIN;
  int len;
  int y=0;

  if ( ti->rewind )
    (*ti->rewind)(ti->text);

  for(;;)
  { long next_index;

    if ( start < 0 && index >= valInt(ti->start) )
      start = y;

    next_index = do_fill_line(ti, tmp, index);
    y += tmp->h;
    index = next_index;

    if ( tmp->ends_because & END_EOF )
    { len = y;
      break;
    }
  }

  return bubbleScrollBar(sb, toInt(len), toInt(start), toInt(view));
}


static status
make_pline_map(TextImage ti, PLine lines, int *size)
{ TextLine tmp = tmpLine();
  long index = 0;
  int y=0;
  int mx = *size;
  int line;

  if ( ti->rewind )
    (*ti->rewind)(ti->text);

  for(line=0; line < mx-1 ;line++)
  { lines[line].y = y;
    lines[line].start = index;
    index = do_fill_line(ti, tmp, index);
    y += tmp->h;

    if ( tmp->ends_because & END_EOF )
    { *size = ++line;
      lines[line].y = y+tmp->h;
      succeed;
    }
  }
      
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find filled line starting <lines> screenlines before <here>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
backwards_filled_line(TextImage ti, TextLine l, long here, int lines)
{ while(here > 0)
  { int i;
    long idx, idx0;

    idx0 = paragraph_start(ti, here-1);

    for(i=0, idx=idx0; ;)
    { idx = do_fill_line(ti, l, idx);
      i++;
      if ( l->end >= here )
	break;
    }

    if ( i == lines && i == 1)		/* common case */
    { succeed;
    } else if ( i >= lines )
    { int n = i+1-lines;

      for(idx=idx0; n-- > 0; )
	idx = do_fill_line(ti, l, idx);

      succeed;
    } else 
    { lines -= i;
      here = idx0;
    }
  }

  do_fill_line(ti, l, 0);
  fail;
}


static status
backwards_filled_line_from_dy(TextImage ti, TextLine l, long here, int dy)
{ while(here > 0)
  { int sy;
    long idx, idx0;

    idx0 = paragraph_start(ti, here-1);

    for(sy=0, idx=idx0; ;)		/* Heigth of the paragraph */
    { idx = do_fill_line(ti, l, idx);
      sy += l->h;
      if ( l->end >= here )
	break;
    }

    if ( sy >= dy )			/* somewhere here */
    { int skip = sy-dy;

      for(idx=idx0; skip > 0; )
      { idx = do_fill_line(ti, l, idx);
	skip -= l->h;
      }

      succeed;
    } else 
    { dy -= sy;
      here = idx0;
    }
  }

  do_fill_line(ti, l, 0);
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ensure there is enough to see.  We assume 1/3-th of the screen or 1 line
for very small screens will suffice.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
ensure_enough_visible(TextImage ti, long here)
{ int minv = (ti->h-2*TXT_Y_MARGIN)/3;
  TextLine l = tmpLine();
  int v = 0;
  long idx = here;

  for(;;)
  { idx = do_fill_line(ti, l, idx);
    v += l->h;

    if ( v >= minv )
      return here;

    if ( l->ends_because & END_EOF )
    { long end = l->start + l->length;
      
      backwards_filled_line_from_dy(ti, l, end, minv);
      return l->start;
    }
  }
}


Int
getScrollStartTextImage(TextImage ti, Name dir, Name unit, Int amount)
{ if ( unit == NAME_file )
  { if ( dir == NAME_goto )
    { struct pline lines[MAXPLINES];
      int count = MAXPLINES;
      int l;
      int h, wh = ti->h - 2*TXT_Y_MARGIN;

      if ( !make_pline_map(ti, lines, &count) )
	fail;
      h  = lines[count].y;

      if ( h>wh )
      { int yt = ((h-wh) * valInt(amount))/1000;
	
	for(l=0; l<count; l++)
	{ if ( lines[l].y >= yt )
	    break;
	}
	/*Cprintf("%d promille, h=%d, wh=%d, yt=%d, l=%d\n",
		valInt(amount), h, wh, yt, l);*/
      } else
	return ZERO;

      answer(toInt(lines[l].start));
    }
  } else
  { long idx;

    if ( unit == NAME_line )
    { if ( dir == NAME_forwards )
      { int n = valInt(amount);
	TextLine l = tmpLine();
	idx = valInt(ti->start);
  
	for( ; n-- > 0; )
	{ idx = do_fill_line(ti, l, idx);
	  if ( l->ends_because & END_EOF )
	    break;
	}
      } else				/* scrolling up */
      { int n = valInt(amount);
	TextLine l = tmpLine();
  
	backwards_filled_line(ti, l, valInt(ti->start), n);
	idx = l->start;
      }
    } else /* if ( unit	== NAME_page ) */
    { int dy = ((ti->h-2*TXT_Y_MARGIN) * valInt(amount))/1000;
      TextLine l = tmpLine();

      idx = valInt(ti->start);
  
      if ( dir == NAME_forwards )
      { for( ; dy > 0 ; )
	{ long next = do_fill_line(ti, l, idx);
	  if ( l->ends_because & END_EOF )
	    break;
	  dy -= l->h;
	  if ( dy <= 0 && idx != valInt(ti->start) )
	    break;
	  idx = next;
	}
      } else
      { backwards_filled_line_from_dy(ti, l, idx, dy);
	idx = l->start;
      }
    }

    if ( idx < 0 )
      idx = 0;
    else
      idx = ensure_enough_visible(ti, idx);

    answer(toInt(idx));
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get desired cursor position for moving a line   up or down. This is used
in word-wrap mode to get natural behaviour of the cursor up/down arrow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Int
getUpDownColumnTextImage(TextImage ti, Int here)
{ int cx, cy;

  if ( get_xy_pos(ti, here, &cx, &cy) )
  { int ly = cy-1+ti->map->skip;
    TextLine l  = &ti->map->lines[ly];
    TextChar tc = &l->chars[cx-1];

    answer(toInt(tc->x));
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Support cursor up/down movement for editors  using long paragraph and in
word-wrap mode. This is pretty tricky. Unlike   the  above stuff, we use
this routine independent from the  size  of   the  buffer,  so we cannot
affort scanning the whole buffer.

We assume the cursor is at this moment on the screen. If not, the editor
will fall back to ->next_line, based on  the represented text. If it is,
we deduce the X and Y of  the   characters  and look N lines up/down. If
this happens to be on the screen,  we   are  lucky. Otehrwise we have to
format the bits and pieces just off  the screen, to determine the proper
location. Going down, this is  easy  again.   Going  up  we will go back
paragraph-by-paragraph and count the number of  lines before the screen.
If we are far enough back, we go forward to include the proper location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Int
getUpDownCursorTextImage(TextImage ti, Int here, Int updown, Int column)
{ int cx, cy;				/* grid x-y */
  int ud = valInt(updown);

  if ( get_xy_pos(ti, here, &cx, &cy) )
  { int ly = cy-1+ti->map->skip;
    TextLine l  = &ti->map->lines[ly];
    TextChar tc = &l->chars[cx-1];
    int x = tc->x;			/* pixel-x */
    int i;

    if ( isDefault(column) )
      x = tc->x;
    else
      x = valInt(column);

    ly += ud;
    if ( ly < 0 )			/* before the screen */
    { long start = ti->map->lines[0].start;
      long idx = start;
      l = tmpLine();

      for(;;)
      { int i;
	long here;

	here = idx = paragraph_start(ti, idx);

	for(i=0; here < start; i++)	/* count screen-lines */
	{ here = do_fill_line(ti, l, here);
	  if ( l->ends_because & END_EOF )
	    break;			/* should not happen */
	}

        if ( i >= -ly )
	{ i += ly;

	  for(here=idx; i-- >= 0; )
	    here = do_fill_line(ti, l, here);

	  goto out;
	}

	if ( --idx < 0 )
	  break;
      }

      do_fill_line(ti, l, 0);
    } else if ( ly >= ti->map->length )	/* after the screen */
    { long idx = valInt(ti->end);
      int n = ly-(ti->map->length-1);

      l = tmpLine();
      while(n-- > 0)
      { idx = do_fill_line(ti, l, idx);
	if ( l->ends_because & END_EOF )
	  break;
      }
    } else				/* on the screen */
    { l  = &ti->map->lines[ly];
    }

out:
					/* fix the X-location */
    for(i = 0; i < l->length; i++)
    { if ( l->chars[i+1].x > x )
	break;
    }

    return toInt(l->start + l->chars[i].index);
  }

  fail;
}


Int
getBeginningOfLineCursorTextImage(TextImage ti, Int here)
{ int cx, cy;

  if ( get_xy_pos(ti, here, &cx, &cy) )
  { int ly = cy-1+ti->map->skip;
    TextLine l  = &ti->map->lines[ly];

    answer(toInt(l->start));
  }

  fail;
}


Int
getEndOfLineCursorTextImage(TextImage ti, Int here)
{ int cx, cy;

  if ( get_xy_pos(ti, here, &cx, &cy) )
  { int ly = cy-1+ti->map->skip;
    TextLine l  = &ti->map->lines[ly];

    answer(toInt(l->end-1));
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ensureVisibleTextImage()
    See whether we can make caret part of the screen by scrolling at most
    one screen-line up or down.  For up we start filling out the physical
    line before the screen.  For down we fill the next line.  If caret is
    on this line we discard the first N-lines to free up enough space for
    the line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ensureVisibleTextImage(TextImage ti, Int caret)
{ long here = valInt(caret);

  if ( here < valInt(ti->start) )
  { long idx = paragraph_start(ti, valInt(ti->start)-1);

    if ( here >= idx )
    { TextLine l = tmpLine();
      long next;

      for(; ; idx=next)
      { next = do_fill_line(ti, l, idx);
	if ( l->ends_because & END_EOF )
	  fail;				/* should not happen */
	if ( here >= idx && here < next )
	  return startTextImage(ti, toInt(idx), ZERO);
      }
    }
  } else
  { ComputeGraphical(ti);

    if ( here >= valInt(ti->end) && ti->eof_in_window == OFF )
    { TextLine l = tmpLine();
      long next;
  
      next = do_fill_line(ti, l, valInt(ti->end));
      if ( here < next || l->ends_because & END_EOF ) /* shift one at most */
      { TextLine last = &ti->map->lines[ti->map->length-1];
	int yt = last->y + last->h + l->h;
	int yshift = yt - (ti->h - 2*TXT_Y_MARGIN);
	int f;
  
	for(f=ti->map->skip; f<ti->map->length; f++)
	{ if ( ti->map->lines[f].y >= yshift )
	    return startTextImage(ti, toInt(ti->map->lines[f].start), ZERO);
	}
      }
    } else
      succeed;
  }

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_center[] =
        { "index=int", "line=[int]" };
static char *T_start[] =
        { "start=[int]", "skip_lines=[int]" };
static char *T_initialise[] =
        { "text=object", "width=int", "height=int" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_textImage[] =
{ IV(NAME_text, "object", IV_GET,
     NAME_storage, "Source of the text"),
  SV(NAME_background, "[colour|pixmap]", IV_GET|IV_STORE, backgroundTextImage,
     NAME_appearance, "Background colour"),
  IV(NAME_start, "int", IV_NONE,
     NAME_scroll, "Index of first character displayed"),
  IV(NAME_end, "int", IV_GET,
     NAME_scroll, "Index of last character displayed"),
  SV(NAME_wrap, "{none,character,word}", IV_GET|IV_STORE, wrapTextImage,
     NAME_appearance, "Wrap mode for long lines"),
  SV(NAME_tabDistance, "int", IV_GET|IV_STORE, tabDistanceTextImage,
     NAME_appearance, "Pixel distance between tab stops"),
  SV(NAME_tabStops, "vector*", IV_GET|IV_STORE, tabStopsTextImage,
     NAME_appearance, "Vector of tab-stops in pixels"),
  IV(NAME_pointed, "graphical*", IV_GET,
     NAME_event, "Graphical under the pointer"),
  IV(NAME_eofInWindow, "bool", IV_GET,
     NAME_repaint, "Is end-of-file inside window?"),
  IV(NAME_width, "alien:int", IV_NONE,
     NAME_area, "Width of the image"),
  IV(NAME_height, "alien:int", IV_NONE,
     NAME_area, "Height of the image"),
  IV(NAME_changeStart, "alien:int", IV_NONE,
     NAME_repaint, "Start of changes (character index)"),
  IV(NAME_changeEnd, "alien:int", IV_NONE,
     NAME_repaint, "End of changes (character index)"),
  IV(NAME_inserted, "alien:int", IV_NONE,
     NAME_repaint, "How much text was inserted/deleted"),
  IV(NAME_seek, "alien:SeekFunction", IV_NONE,
     NAME_internal, "C-Function to seek to a position"),
  IV(NAME_scan, "alien:ScanFunction", IV_NONE,
     NAME_internal, "C-Function to scan for a syntactical category"),
  IV(NAME_fetch, "alien:FetchFunction", IV_NONE,
     NAME_internal, "C-function to fetch next character from source"),
  IV(NAME_MarginFunction, "alien:MarginFunction", IV_NONE,
     NAME_internal, "C-function to fetch margins from source"),
  IV(NAME_RewindFunction, "alien:RewindFunction", IV_NONE,
     NAME_internal, "C-function to rewind input"),
  IV(NAME_map, "alien:TextScreen", IV_NONE,
     NAME_cache, "2-dimensional map of source")
};

/* Send Methods */

static senddecl send_textImage[] =
{ SM(NAME_compute, 0, NULL, computeTextImage,
     DEFAULT, "Recompute text-image if necessary"),
  SM(NAME_reset, 0, NULL, resetTextImage,
     DEFAULT, "Reset <-pointed after an abort"),
  SM(NAME_event, 1, "event", eventTextImage,
     DEFAULT, "Forward event to included graphicals"),
  SM(NAME_geometry, 4, T_geometry, geometryTextImage,
     DEFAULT, "Change image geometry"),
  SM(NAME_initialise, 3, T_initialise, initialiseTextImage,
     DEFAULT, "Create from source, width and height"),
  SM(NAME_unlink, 0, NULL, unlinkTextImage,
     DEFAULT, "Reclaim private allocated data"),
  SM(NAME_dumpMap, 0, NULL, dumpMapTextImage,
     NAME_debugging, "Dump map of the screen"),
  SM(NAME_center, 2, T_center, centerTextImage,
     NAME_scroll, "Scroll to place index at given line"),
  SM(NAME_start, 2, T_start, startTextImage,
     NAME_scroll, "Set start of screen and screenlines to skip")
};

/* Get Methods */

static getdecl get_textImage[] =
{ GM(NAME_height, 0, "int", NULL, getHeightTextImage,
     NAME_area, "Height of the image"),
  GM(NAME_width, 0, "int", NULL, getWidthTextImage,
     NAME_area, "Width of the image"),
  GM(NAME_index, 1, "int", "event", getIndexTextImage,
     NAME_event, "Character index from event (position)"),
  GM(NAME_line, 1, "int", "int", getLineTextImage,
     NAME_scroll, "Window line (row) from character index"),
  GM(NAME_lines, 0, "int", NULL, getLinesTextImage,
     NAME_scroll, "Number of lines visible"),
  GM(NAME_start, 1, "line=int", "index=[int]", getStartTextImage,
     NAME_scroll, "Character index for start of screenline"),
  GM(NAME_displayedCursor, 0, "cursor*", NULL, getDisplayedCursorTextImage,
     NAME_cursor, "Currently displayed cursor"),
  GM(NAME_view, 0, "int", NULL, getViewTextImage,
     NAME_scroll, "Number of characters visible")
};

/* Resources */

static classvardecl rc_textImage[] =
{ RC(NAME_background, "[colour|pixmap]",
     UXWIN("@default", "win_window"),
     "Background colour for the text"),
  RC(NAME_elevation, "elevation*", "when(@colour_display, 1, @nil)",
     "Elevation from the background"),
  RC(NAME_tabDistance, "int", "64",
     "Tabstop interval (pixels)"),
  RC(NAME_wrap, "{none,character,word}", "character",
     "Wrap unit for long lines"),
  RC(NAME_colour, RC_REFINE, "black", NULL)
};

/* Class Declaration */

static Name textImage_termnames[] = { NAME_text, NAME_width, NAME_height };

ClassDecl(textImage_decls,
          var_textImage, send_textImage, get_textImage, rc_textImage,
          3, textImage_termnames,
          "$Rev$");


status
makeClassTextImage(Class class)
{ declareClass(class, &textImage_decls);

  setCloneFunctionClass(class, cloneTextImage);
  setLoadStoreFunctionClass(class, loadTextImage, storeTextImage);
  setRedrawFunctionClass(class, RedrawAreaTextImage);
  solidClass(class, ON);
  cloneStyleVariableClass(class, NAME_image, NAME_nil);
  cloneStyleVariableClass(class, NAME_changedArea, NAME_nil);
  saveStyleVariableClass(class, NAME_image, NAME_nil);
  saveStyleVariableClass(class, NAME_changedArea, NAME_nil);

  succeed;
}

