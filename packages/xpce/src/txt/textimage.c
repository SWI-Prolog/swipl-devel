/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status adjustSizeImageTextImage P((TextImage));

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


#define INFINITE PCE_MAX_INT
#define Round(n, r)  ((((n) + ((r)-1)) / (r)) * (r))

		/********************************
		*          LOCAL TYPES		*
		********************************/

#define END_CUT  (1)			/* Line ends due to wrap: none */
#define END_WRAP (2)			/* Line ends due to wrap */
#define END_EOF  (4)			/* Line ends due to end-of-buffer */
#define END_NL	 (8)			/* Line ends due to newline */


forwards status fillImageTextImage P((TextImage, Bool force));
forwards long	do_fill_line P((TextImage, TextLine, long));
forwards status reinitTextImage P((TextImage ti));


		/********************************
		*       NEW/ALLOC/UNALLOC	*
		********************************/

static status
initialiseTextImage(TextImage ti, Any obj, Int w, Int h)
{ initialiseGraphical(ti, ZERO, ZERO, w, h);
  assign(ti, text,	   obj);
  assign(ti, image,        NIL);
  assign(ti, start,        ZERO);
  assign(ti, end,	   ZERO);
  assign(ti, wrap,	   getResourceValueObject(ti, NAME_wrap));
  assign(ti, tab_distance, getResourceValueObject(ti, NAME_tabDistance));

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
  if ( ti->image_map != NULL )
  { unalloc_screen(ti->image_map);
    ti->image_map = NULL;
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
    DEBUG(NAME_allocated, printf("Lines at %ld, %ld bytes\n",
				 (ulong) new,
				 (ulong) lines * sizeof(struct text_line)));

    for(n = 0; n < s->allocated; n++)	/* copy old lines */
      new[n] = s->lines[n];

    for( ; n < lines; n++)		/* create new ones */
    { new[n].chars = alloc(chars * sizeof(struct text_char));
      new[n].allocated = chars;
      new[n].changed = TRUE;
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

  assign(ti, changed_area, newObject(ClassArea, 0));
  assign(ti, request_compute, ON);

  ti->w		   = valInt(ti->area->w);
  ti->h		   = valInt(ti->area->h);
  ti->change_start = 0;
  ti->change_end   = INFINITE;
  ti->inserted     = 0;

  ti->seek  = (SeekFunction) IntToTextPointer(get(obj, NAME_SeekFunction, 0));
  ti->scan  = (ScanFunction) IntToTextPointer(get(obj, NAME_ScanFunction, 0));
  ti->fetch = (FetchFunction)IntToTextPointer(get(obj, NAME_FetchFunction,0));
  if ( !ti->seek || !ti->scan || !ti->fetch )
    return errorPce(ti, NAME_noFetchFunction, obj);
  DEBUG(NAME_SeekFunction, printf("ti->seek = 0x%lx\n", (ulong) ti->seek));

  ti->map                  = alloc(sizeof(struct text_screen));
  ti->map->allocated       = ti->map->length = ti->map->skip = 0;
  ti->map->lines           = NULL;
  ti->image_map            = alloc(sizeof(struct text_screen));
  ti->image_map->allocated = ti->image_map->length = ti->image_map->skip = 0;
  ti->image_map->lines     = NULL;

  succeed;
}


static status
loadTextImage(TextImage ti, FILE *fd, ClassDef def)
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
{ int i;
  FontObj f = NULL;
  int ascent = 0, descent = 0;

  for(i=0; i<l->length; i++)
  { if ( l->chars[i].font != f )
    { f = l->chars[i].font;
      
      if ( f != NULL )
      { ascent  = max(ascent, valInt(getAscentFont(f)));
	descent = max(descent, valInt(getDescentFont(f)));
      } else
	printf("Font is NULL???\n");
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
do_fill_line(TextImage ti, TextLine l, long int index)
{ short last_break = -1;
  int last_is_space = FALSE;
  TextChar tc;
  short x = TXT_X_MARGIN;  
  int i;

  l->changed = TRUE;
  l->ends_because = 0;

  l->start = index;

  DEBUG(NAME_fill, printf("Filling line from %ld", index); fflush(stdout));
  (*ti->seek)(ti->text, index);
  DEBUG(NAME_fill, printf(" (seek'ed)"); fflush(stdout));

  for( i = 0, tc = l->chars; ; i++, tc++)
  { if ( l->allocated <= i )
    { ensure_chars_line(l, i+1);
      tc = &l->chars[i];
    }

    index = (*ti->fetch)(ti->text, tc);
    DEBUG(NAME_fill, printf(" %c", tc->c); fflush(stdout));
    tc->x = x;

    switch(tc->c)
    { case EOB:
      case '\n':
	x += c_width(' ', tc->font);
	l->ends_because |= END_NL;
	l->length = ++i;
	l->end = index;
	if ( tc->c == EOB )
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
	if ( ++last_is_space == 1 )
	  last_break = i;
	break;
      case ' ':
	x += c_width(tc->c, tc->font);
	if ( ++last_is_space == 1 )
	  last_break = i;
	break;
      default:
	x += c_width(tc->c, tc->font);
	last_is_space = FALSE;
	break;
    }
    
    if ( x > ti->w - TXT_X_MARGIN )
    { l->ends_because |= END_WRAP;

      if ( equalName(ti->wrap, NAME_none) )
      { int eof;

	l->ends_because |= END_CUT;
	l->length = i;
	l->w = tc->x;

	index = (*ti->scan)(ti->text, index, 1, TEXT_SCAN_FOR, EL, &eof) + 1;
	l->end = index;
	if ( eof )
	{ l->ends_because |= END_EOF;
	  index--;
	}
      } else if ( equalName(ti->wrap, NAME_character) )
      { l->length = i;
	index--;
	l->end = index;
	l->w = tc->x = x;
      } else if ( equalName(ti->wrap, NAME_word) )
      { if ( last_break > 0 )
	{ int eof;

	  l->length = i = last_break;
	  index = l->start + i;
	  l->w = l->chars[last_break].x;

	  (*ti->seek)(ti->text, index);
	  index = (*ti->scan)(ti->text, index, 1, TEXT_SKIP_OVER, BL, &eof);
	  l->end = index;
	} else				/* doesn't fit on line: as character */
	{ l->length = i;
	  l->end = index;
	  l->w = tc->x = x;
	}
      }
      break;
    }
  }

  fill_dimensions_line(l);

  return index;
}


static long
fill_line(TextImage ti, int line, long int index, short int y)
{ TextLine l;

  ensure_lines_screen(ti->map, line+1);
  l = &ti->map->lines[line];

  if ( l->start == index &&
       (l->end < ti->change_start || l->start >= ti->change_end) )
  { if ( l->y != y )
    { l->y = y;
      l->changed = TRUE;
    }
    return ti->map->lines[line+1].start;
  }
  
  l->y = y;

  return do_fill_line(ti, l, index);
}


status
updateMapTextImage(TextImage ti)
{ if ( ti->change_end > ti->change_start )
  { Bool eof_in_window = OFF;
    int line;
    short y = TXT_Y_MARGIN;
    long index = valInt(ti->start);
  
    DEBUG(NAME_text, printf("Updating map from %d to %d ",
			    ti->change_start, ti->change_end);
	  fflush(stdout); );

    for(line = 0; ; line++)
    { long next_index;

      next_index = fill_line(ti, line, index, y);
      if ( line >= ti->map->skip )
	y += ti->map->lines[line].h;

      if ( y > ti->h - TXT_Y_MARGIN )
      { ti->map->length = line - ti->map->skip;
	assign(ti, end, toInt(index));
	assign(ti, eof_in_window, eof_in_window);
	ti->change_start = INFINITE;
	ti->change_end = 0;
	DEBUG(NAME_text, printf("ok; eof_in_window = %s\n",
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

  printf("skip = %d; length = %d, allocated = %d lines\n",
	 map->skip, map->length, map->allocated);

  for(i=0; i<map->skip + map->length; i++)
  { TextLine l = &map->lines[i];
    int n;
    int c;

    if ( i < map->skip )
      printf("--:");
    else
      printf("%2d:", i - map->skip);
    printf("%4ld-%4ld at y=%3d %s ",
	   l->start, l->end-1, l->y, l->changed ? "**" : "  ");
    putchar((l->ends_because & END_EOF)  ? 'F' : '-');
    putchar((l->ends_because & END_WRAP) ? 'W' : '-');
    putchar((l->ends_because & END_CUT)  ? 'C' : '-');
    putchar((l->ends_because & END_NL)   ? 'L' : '-');
    printf(": \"");
    for(n=0; n < 5 && n < l->length; n++)
    { if ( (c = l->chars[n].c) == '\n' )
	printf("\\n");
      else if ( c == EOB )
	printf("\\$");
      else
	putchar(c);
    }
    if ( l->length - 5 > n )
    { printf(" ... ");
      n = l->length - 5;
    }
    for( ; n < l->length; n++ )
    { if ( (c = l->chars[n].c) == '\n' )
	printf("\\n");
      else if ( c == EOB )
	printf("\\$");
      else
	putchar(c);
    }
    printf("\"\n");
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
t_underline(int x, int y, int w)
{ static int ex = 0, ey = 0, ew = 0;

  if ( x == ex+ew && y == ey )
  { ew += w;
  } else
  { if ( ew > 0 )
      r_line(ex, ey, ex+ew, ey);
    ex = x, ey = y, ew = w;
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


static status
t_open(TextImage ti)
{ if ( notNil(ti->image) && ti->w > 0 && ti->h > 0 )
  { Colour c;

    clearArea(ti->changed_area);
    DEBUG(NAME_text, printf("Opening image %s\n", pp(ti->image)));
    d_image(ti->image, 0, 0, ti->w, ti->h);
    
    if ( notDefault(c = getDisplayColourGraphical((Graphical)ti)) )
      r_default_colour(c);

    succeed;
  }

  fail;
}


static void
t_close(void)
{ t_underline(0, 0, 0);
  t_invert(0, 0, 0, 0);
  t_grey(0, 0, 0, 0);
  
  d_done();
}


		/********************************
		*             AREAS		*
		********************************/

static void
changed_area(TextImage ti, int x, int y, int w, int h)
{ Area a = ti->changed_area;

  if ( a->w == ZERO || a->h == ZERO )
  { a->x = toInt(x); a->y = toInt(y); a->w = toInt(w); a->h = toInt(h);
  } else
  { int ax = valInt(a->x), ay = valInt(a->y),
        aw = valInt(a->w), ah = valInt(a->h);
  
    if ( x < ax ) { aw += ax - x; ax = x; }
    if ( y < ay ) { ah += ay - y; ay = y; }
    if ( x+w > ax+aw ) { aw += x+w-ax; }
    if ( y+h > ay+ah ) { ah += y+h-ay; }

    DEBUG(NAME_text, printf("changed (%d %d %d %d) -> (%d %d %d %d)\n",
			    x, y, w, h, ax, ay, aw, ah));

    a->x = toInt(ax); a->y = toInt(ay); a->w = toInt(aw); a->h = toInt(ah);
  }
}

		/********************************
		*            PAINTING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Paint a line from index `from' to index `to'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
paint_attributes(TextImage ti, TextLine l, int from, int to)
{ unsigned char atts = l->chars[from].attributes;
  
  if ( atts & TXT_UNDERLINED )
  { t_underline(l->chars[from].x, l->y + l->h - 1,
		l->chars[to].x - l->chars[from].x);
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
paint_line(TextImage ti, TextLine l, int from, int to)
{ char buf[1000];
  char *out;
  int b16, n, s = from, e;
  FontObj f;
  Colour c;
  unsigned char atts;
  int cw;

  DEBUG(NAME_text, printf("painting line 0x%lx from %d to %d\n",
			  (ulong)l, from, to));

  cw = (to == l->length ? ti->w : l->chars[to].x) - l->chars[from].x;
  r_clear(l->chars[from].x, l->y, cw, l->h);

  { TextChar last = &l->chars[to-1];

    if ( last->c == '\n' )	/* do not paint newline */
    { if ( last->attributes & TXT_HIGHLIGHTED )
	t_invert(last->x, l->y, ti->w - TXT_X_MARGIN - last->x, l->h);

      to--;
    } else if ( last->c == EOB )
    { if ( to > 1 && (last->attributes & TXT_HIGHLIGHTED) )
	t_invert(last->x, l->y, ti->w - TXT_X_MARGIN - last->x, l->h);
      
      to--;
    }      
  }

  for( s = from; s < to; s = e )
  { e = s;
    n = 0;
    f      = l->chars[e].font;
    b16    = (f->b16 == ON);
    atts   = l->chars[e].attributes;
    c      = l->chars[e].colour;
    out    = buf;
    PutBuf(l->chars[e].c);

    if ( l->chars[e].c == '\t' )	/* print tabs */
    { for(n++, e++; e < to; n++, e++)
      { if ( l->chars[e].attributes != atts || l->chars[e].c != '\t' )
	  break;
      }
    } else				/* print anything else */
    { for(n++, e++; e < to; n++, e++)
      { if ( l->chars[e].font != f ||
	     l->chars[e].colour != c ||
	     l->chars[e].attributes != atts ||
	     l->chars[e].c == '\t' )
	  break;
	
	PutBuf(l->chars[e].c);
      }

      r_colour(c);
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

    paint_attributes(ti, l, s, e);
  }

  changed_area(ti, l->chars[from].x, l->y, cw, l->h);
}

		/********************************
		*         UPDATING LINES	*
		********************************/

static void
copy_line_attributes(TextLine from, TextLine to)
{ to->y      = from->y;
  to->h      = from->h;
  to->base   = from->base;
  to->length = from->length;
}


static void
copy_line_chars(TextLine from, int start, int end, TextLine to)
{ ensure_chars_line(to, from->length);

  for( ; start < end; start++ )
    to->chars[start] = from->chars[start];    
}


static void
copy_line(TextLine from, TextLine to)
{ copy_line_attributes(from, to);
  copy_line_chars(from, 0, from->length, to);
}


#define equal_text_char(c1, c2) \
  ( (c1)->c == (c2)->c && \
    (c1)->x == (c2)->x && \
    (c1)->attributes == (c2)->attributes && \
    (c1)->font == (c2)->font \
  )

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ml  contains what the  line should  hold;  holds  the  contents of the
screen.  After completion of this function the area described by ml of
the image bitmap is ok and il is equivalent to ml.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
update_line(TextImage ti, TextLine ml, TextLine il)
{ if ( ml->changed == FALSE )
    return;

  if ( ml->y != il->y || ml->base != il->base || ml->h != il->h )
  { copy_line(ml, il);
    paint_line(ti, il, 0, il->length);
  } else
  { int end = min(ml->length, il->length);
    int n;
    
    for( n = 0; n < end; n++ )
      if ( !equal_text_char(&ml->chars[n], &il->chars[n] ) )
        break;

    if ( n < ml->length )
    { copy_line_chars(ml, n, ml->length, il);
      il->length = ml->length;
      paint_line(ti, il, n, il->length);
    }
  }  

  ml->changed = FALSE;
}

		/********************************
		*         SHIFTING LINES	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The function shift_lines()  tries  to find  out whether the screen has
been  scrolled  some lines  up/down  or whether some lines  have  been
inserted  or deleted somewhere.     If so, it   will shift    the line
description  structures of map_image  and shift the information on the
image as well  using a copy area.   Finally it updates changed_area to
propagate the change to the device.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_SHIFT 2			/* maximum lines to shift */

static int
equal_line(TextLine l1, TextLine l2)
{ int n;

  if ( l1->length != l2->length || l1->h != l2->h )
    fail;

  for(n=0; n < l1->length; n++)
    if ( !equal_text_char(&l1->chars[n], &l2->chars[n]) )
      fail;

  succeed;
}


static int
inserted_or_deleted_lines(TextImage ti, Name id, int sl, int el)
{ int ins;
  int line;
  int skip = ti->map->skip;

  for(ins = 1; ins < el - sl - 1 && ins <= MAX_SHIFT; ins++) 
  { for(line = sl+1; line < el - ins; line++)
    { if ( id == NAME_insert )
      { DEBUG(NAME_shift, printf("equal_line(map: %d, image %d) --> ",
				 line+ins, line));
	if ( !equal_line(&ti->map->lines[line+ins+skip],
			 &ti->image_map->lines[line]) )
	  goto continue_outer;
      } else
      { DEBUG(NAME_shift, printf("equal_line(map: %d, image %d) --> ",
				 line, line+ins));
	if ( !equal_line(&ti->map->lines[line+skip],
			 &ti->image_map->lines[line+ins]) )
	  goto continue_outer;
      }
      DEBUG(NAME_shift, printf("yes\n"));
    }
    return ins;

    continue_outer:;
    DEBUG(NAME_shift, printf("no\n"));
  }

  return -1;
}


static void
rotate_lines(TextLine l, int size, int r)
{ struct text_line tmp[MAX_SHIFT+1];
  int n;

  if ( r > 0 )
  { for( n = 0; n < r; n++ )
      tmp[n] = l[size-r+n];
    for( n = 0; n < size-r; n++ )
      l[size-1-n] = l[size-1-r-n];
    for( n = 0; n < r; n++ )
      l[n] = tmp[n];
  } else
  { r = -r;

    for( n = 0; n < r; n++ )
      tmp[n] = l[n];
    for( n = 0; n < size-r; n++ )
      l[n] = l[n+r];
    for( n = 0; n < r; n++ )
      l[size-r+n] = tmp[n];
  }
}


static void
shift_y_lines(TextLine l, int size, int shift)
{ int n;

  for(n = 0; n < size; n++)
    l[n].y += shift;
}


static void
shift_lines(TextImage ti)
{ int sl, el = ti->map->length;
  int skip = ti->map->skip;
  int ins;
  TextScreen im = ti->image_map;

  if ( ti->map->length != im->length )
    return;				/* not worth the trouble */

  for(sl = 0; sl < el; sl++)		/* skip unchanged part */
  { if ( ti->map->lines[sl+skip].changed )
    { if ( equal_line(&ti->map->lines[sl+skip], &im->lines[sl]) )
      { ti->map->lines[sl+skip].changed = FALSE;
        continue;
      }
      break;
    }
  }
					/* get rid of blank lines */
  for( ; el > sl; el--)
  { if ( ti->map->lines[el-1+skip].changed )
    { if ( equal_line(&ti->map->lines[el-1+skip], &im->lines[el-1]) )
      { ti->map->lines[el-1+skip].changed = FALSE;
        continue;
      }
      break;
    }
  }
  DEBUG(NAME_shift, printf("sl = %d, el = %d\n", sl, el));
    
  if ( el - sl < 2 )
    return;

  if ( (ins=inserted_or_deleted_lines(ti, NAME_insert, sl, el)) > 0 )
  { int yfrom = im->lines[sl].y;
    int yto   = im->lines[sl+ins].y;
    int h     = im->lines[el-1].y + im->lines[el-1].h - yto;
    int shift = yto - yfrom;
    int ln;

    DEBUG(NAME_shift, printf("Insert %d lines at %d: ", ins, sl));
    DEBUG(NAME_shift, printf("copy %d -> %d, h=%d, clear(%d, %d)\n",
			    yfrom, yto, h,
			    yfrom, yto - yfrom));

#ifdef __WINDOWS__
    r_copy(0, yfrom, 0, yto, ti->w, h);
#else
    r_image(ti->image, 0, yfrom, 0, yto, ti->w, h);
    r_clear(0, yfrom, ti->w, yto - yfrom);
#endif
    rotate_lines(&im->lines[sl], el - sl, ins);
    shift_y_lines(&im->lines[sl], el - sl, shift);
    for(ln = sl; ln < sl+ins; ln++)
      im->lines[ln].length = 0;
    changed_area(ti, 0, yfrom, ti->w, h+shift);
  } else
  if ( (ins=inserted_or_deleted_lines(ti, NAME_delete, sl, el)) > 0 )
  { int yfrom = im->lines[sl+ins].y;
    int yto   = im->lines[sl].y;
    int h     = im->lines[el-1].y + im->lines[el-1].h - yfrom;
    int shift = yto - yfrom;
    int ln;

    DEBUG(NAME_shift, printf("Delete %d lines at %d: ", ins, sl));
    DEBUG(NAME_shift, printf("copy %d -> %d, h=%d, clear(%d, %d)\n",
			    yfrom, yto, h,
			    yto + h, yfrom - yto));

#ifdef __WINDOWS__
    r_copy(0, yfrom, 0, yto, ti->w, h);
#else
    r_image(ti->image, 0, yfrom, 0, yto, ti->w, h);
    r_clear(0, yto + h, ti->w, yfrom - yto);
#endif
    rotate_lines(&im->lines[sl], el - sl, -ins);
    shift_y_lines(&im->lines[sl], el - sl, shift);
    for(ln = el-ins; ln < el; ln++)
      im->lines[ln].length = 0;
    changed_area(ti, 0, yto, ti->w, h-shift);
  }
}

		/********************************
		*        INDEX <-> POSITION	*
		********************************/

static status
get_xy_pos(TextImage ti, Int pos, int *x, int *y)
{ int line;
  int index = valInt(pos);
  int skip;

  updateMapTextImage(ti);
  skip = ti->map->skip;
  
  for(line=0; line < ti->map->length; line++)
  { TextLine l = &ti->map->lines[skip + line];
  
    if ( index >= l->start && index < l->end )
    { int li = index - l->start;

      if ( li >= l->length )
        li = l->length - 1;
      
      *x = li + 1;
      *y = line + 1;
      succeed;
    }
  }
  
  fail;
}


status
get_character_box_textimage(TextImage ti, int index, int *x, int *y, int *w, int *h, int *b)
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

  updateMapTextImage(ti);
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
	  return l->start + i;
      
      return l->start + l->length - 1;
    }
  }
  
  return valInt(ti->end) - 1;
}


Int
getLinesTextImage(TextImage ti)
{ updateMapTextImage(ti);

  answer(toInt(ti->map->length));
}


static Int
getLineTextImage(TextImage ti, Int pos)
{ int cx, cy;

  if ( get_xy_pos(ti, pos, &cx, &cy) == SUCCEED )
    answer(toInt(cy));

  fail;  
}

		/********************************
		*            EVENTS		*
		********************************/

Int
getIndexTextImage(TextImage ti, EventObj ev)
{ Int x, y;

  get_xy_event(ev, ti, ON, &x, &y);
  answer(toInt(get_index_text_image(ti, valInt(x), valInt(y))));
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

  if ( isNil(ti->image) )
  { computeTextImage(ti);
    assign(ti, image, newObject(ClassImage, NIL, ZERO, ZERO, NAME_pixmap, 0));
    assign(ti->image, display, getDisplayGraphical((Graphical)ti));
    adjustSizeImageTextImage(ti);
    ensure_lines_screen(ti->image_map, ti->map->length);
    fillImageTextImage(ti, ON);
  }

  initialiseDeviceGraphical(ti, &x, &y, &w, &h);
  bx = x, by = y, bw = w, bh = h;

  sx = valInt(a->x) - valInt(ti->area->x); if ( sx < p ) sx = p;
  sy = valInt(a->y) - valInt(ti->area->y); if ( sy < p ) sy = p;
  x += sx, w -= sx + p;
  y += sy, h -= sy + p;
  if ( w > valInt(a->w) ) w = valInt(a->w);
  if ( h > valInt(a->h) ) h = valInt(a->h);

  if ( w > 0 && h > 0 ) 
  { DEBUG(NAME_textImage, printf("r_image(%s, %d, %d, %d, %d, %d, %d)\n",
				 pp(ti->image), sx, sy, x, y, w, h));
    r_image(ti->image, sx, sy, x, y, w, h);
  }

  r_thickness(p);
  r_dash(ti->texture);
  r_box(bx, by, bw, bh, 0, NIL);

  return RedrawAreaGraphical(ti, a);
}


static status
fillImageTextImage(TextImage ti, Bool force)
{ if ( t_open(ti) )			/* No image: W or H is 0 */
  { Area a;
    int line;
    TextLine ml, il;

    if ( force == ON )
    { ml = ti->map->lines + ti->map->skip;
      il = ti->image_map->lines;
      for(line = 0; line < ti->map->length; line++, ml++, il++)
      { il->y = 0;
	update_line(ti, ml, il);
      }
    } else
    { shift_lines(ti);			/* Handle scroll or line ins/del */

      ml = ti->map->lines + ti->map->skip;
      il = ti->image_map->lines;
      for(line = 0; line < ti->map->length; line++, ml++, il++)
	update_line(ti, ml, il);
      if ( ml->changed )
      { r_clear(0, ml->y, ti->w, ti->h - ml->y);
	ml->changed = FALSE;
      }
    }

    ti->image_map->length = ti->map->length;
    t_close();

    a = ti->changed_area;
    if ( valInt(a->w) > 0 && valInt(a->h) > 0 )
    { CHANGING_GRAPHICAL(ti,
			 DEBUG(NAME_text,
			       printf("Changed: (%ld %ld %ld %ld)\n",
				      valInt(a->x), valInt(a->y),
				      valInt(a->w), valInt(a->h)));
			 changedImageGraphical(ti, a->x, a->y, a->w, a->h));
    }
  }

  succeed;
}


status
computeTextImage(TextImage ti)
{ if ( notNil(ti->request_compute) )
  { updateMapTextImage(ti);

    if ( notNil(ti->image) )
    { ensure_lines_screen(ti->image_map, ti->map->length);
      fillImageTextImage(ti, OFF);	/* TBD: optimise? */
    }
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
    DEBUG(NAME_center, printf("Start at %ld; skip = %d\n",
			      map->lines[startline].start, skip));

    startTextImage(ti, toInt(map->lines[startline].start), toInt(skip));

    succeed;
  }

  DEBUG(NAME_center, printf("Out of screen: l = %d\n", l));
  fail;
}


status
centerTextImage(TextImage ti, Int position, Int screen_line)
{ int pos = valInt(position);
  int line;
  TextScreen map = ti->map;

  updateMapTextImage(ti);
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

      DEBUG(NAME_center, printf("ParStart = %ld\n", start));
      do
      { shift_lines_down(map, ln, 1);
	idx = fill_line(ti, ln, idx, 0);
	DEBUG(NAME_center, printf("Filled line %d to %ld\n", ln-1, idx));
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
  
  updateMapTextImage(ti);

  if ( ln >= 0 )
  { ln--;
  } else
  { ln += map->length;
  }

  DEBUG(NAME_start, printf("Looking for start of line %d\n", ln));

  if ( ln < 0 )
  { if ( -ln <= map->skip )
    { answer(toInt(map->lines[map->skip + ln].start));
    } else
    { long here = map->lines[0].start;
      long start;

      ln = -ln - map->skip;		/* lines before idx */
      do
      { long idx = start = paragraph_start(ti, here-1);
	DEBUG(NAME_start, printf("start = %ld; here = %ld\n", start, here));
	do
	{ idx = do_fill_line(ti, &tl, idx);
	  DEBUG(NAME_start, printf("line to %ld; ln = %d\n", idx, ln));
	  if ( --ln == 0 )
	    answer(toInt(idx));
	} while( idx < here );
	here = start;
      } while(start > 0);

      answer(ZERO);			/* start of buffer */
    }
  } else if ( ln >= map->length )
  { long idx = map->lines[map->skip + map->length - 1].start;

    for( ln -= map->length - 1; ln > 0; ln-- )
    { DEBUG(NAME_start, printf("ln = %d; idx = %ld\n", ln, idx));

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
adjustSizeImageTextImage(TextImage ti)
{ if ( notNil(ti->image) )
  { int bw = Round(ti->w, 16);
    int bh = Round(ti->h, 16);

    if ( bw > 0 && bh > 0 &&
	 (bw != valInt(ti->image->size->w) ||
	  bh != valInt(ti->image->size->h)) )
    { DEBUG(NAME_image, printf("Resizing %s to %d x %d\n",
			       pp(ti), bw, bh));
      resizeImage(ti->image, toInt(bw), toInt(bh));
      d_image(ti->image, 0, 0, bw, bh);
      r_clear(0, 0, bw, bh);
      d_done();
    }
  }
  
  succeed;
}


static status
geometryTextImage(TextImage ti, Int x, Int y, Int w, Int h)
{
#define Changed(a) ( notDefault(a) && (a) != ti->area->a )

  if ( Changed(w) || Changed(h) )	/* resize */
  { int line;

    geometryGraphical(ti, x, y, w, h);
    ti->w = valInt(ti->area->w);
    ti->h = valInt(ti->area->h);
    adjustSizeImageTextImage(ti);
    for(line = 0; line < ti->image_map->length; line++)
      ti->image_map->lines[line].length = 0;
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


status
makeClassTextImage(Class class)
{ sourceClass(class, makeClassTextImage, __FILE__, "$Revision$");

  setCloneFunctionClass(class, cloneTextImage);
  setLoadStoreFunctionClass(class, loadTextImage, storeTextImage);

  localClass(class, NAME_text, NAME_storage, "object", NAME_get,
	     "Source of the text");
  localClass(class, NAME_image, NAME_cache, "image*", NAME_get,
	     "Scratch image");
  localClass(class, NAME_start, NAME_scroll, "int", NAME_none,
	     "Index of first character displayed");
  localClass(class, NAME_end, NAME_scroll, "int", NAME_get,
	     "Index of last character displayed");
  localClass(class, NAME_wrap, NAME_appearance,
	     "{none,character,word}", NAME_get,
	     "Wrap mode for long lines");
  localClass(class, NAME_tabDistance, NAME_appearance, "int", NAME_get,
	     "Pixel distance between tab stops");
  localClass(class, NAME_tabStops, NAME_appearance, "vector*", NAME_get,
	     "Vector of tab-stops in pixels");
  localClass(class, NAME_changedArea, NAME_repaint, "area", NAME_get,
	     "Area to redisplay next");
  localClass(class, NAME_eofInWindow, NAME_repaint, "bool", NAME_get,
	     "Is end-of-file inside window?");

  localClass(class, NAME_width, NAME_area, "alien:int", NAME_none,
	     "Width of the image");
  localClass(class, NAME_height, NAME_area, "alien:int", NAME_none,
	     "Height of the image");
  localClass(class, NAME_changeStart, NAME_repaint, "alien:int", NAME_none,
	     "Start of changes (character index)");
  localClass(class, NAME_changeEnd, NAME_repaint, "alien:int", NAME_none,
	     "End of changes (character index)");
  localClass(class, NAME_inserted, NAME_repaint, "alien:int", NAME_none,
	     "How much text was inserted/deleted");
  localClass(class, NAME_seek, NAME_internal, "alien:SeekFunction", NAME_none,
	     "C-Function to seek to a position");
  localClass(class, NAME_scan, NAME_internal, "alien:ScanFunction", NAME_none,
	     "C-Function to scan for a syntactical category");
  localClass(class, NAME_fetch, NAME_internal, "alien:FetchFunction", NAME_none,
	     "C-function to fetch next character from source");
  localClass(class, NAME_map, NAME_cache, "alien:TextScreen", NAME_none,
	     "2-dimensional map of source");
  localClass(class, NAME_imageMap, NAME_cache, "alien:TextScreen", NAME_none,
	     "2-dimensional map of image");

  termClass(class, "text_image", 3, NAME_text, NAME_width, NAME_height);
  setRedrawFunctionClass(class, RedrawAreaTextImage);
  solidClass(class, ON);

  cloneStyleVariableClass(class, NAME_image, NAME_nil);
  cloneStyleVariableClass(class, NAME_changedArea, NAME_nil);
  saveStyleVariableClass(class, NAME_image, NAME_nil);
  saveStyleVariableClass(class, NAME_changedArea, NAME_nil);

  storeMethod(class, NAME_wrap,        wrapTextImage);
  storeMethod(class, NAME_tabDistance, tabDistanceTextImage);
  storeMethod(class, NAME_tabStops,    tabStopsTextImage);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "text=object", "width=int", "height=int",
	     "Create from source, width and height",
	     initialiseTextImage);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Reclaim private allocated data",
	     unlinkTextImage);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Recompute text-image if necessary",
	     computeTextImage);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Change image geometry",
	     geometryTextImage);
  sendMethod(class, NAME_start, NAME_scroll, 2,
	     "start=[int]", "skip_lines=[int]",
	     "Set start of screen and screenlines to skip",
	     startTextImage);
  sendMethod(class, NAME_center, NAME_scroll, 2, "index=int", "line=[int]",
	     "Scroll to place index at given line",
	     centerTextImage);
  sendMethod(class, NAME_dumpMap, NAME_debugging, 0,
	     "Dump map of the screen",
	     dumpMapTextImage);
	
  getMethod(class, NAME_width, NAME_area, "int", 0,
	    "Width of the image",
	    getWidthTextImage);
  getMethod(class, NAME_height, NAME_area, "int", 0,
	    "Height of the image",
	    getHeightTextImage);
  getMethod(class, NAME_view, NAME_scroll, "int", 0,
	    "Number of characters visible",
	    getViewTextImage);
  getMethod(class, NAME_lines, NAME_scroll, "int", 0,
	    "Number of lines visible",
	    getLinesTextImage);
  getMethod(class, NAME_line, NAME_scroll, "int", 1, "int",
	    "Window line (row) from character index",
	    getLineTextImage);
  getMethod(class, NAME_index, NAME_event, "int", 1, "event",
	    "Character index from event (position)",
	    getIndexTextImage);
  getMethod(class, NAME_start, NAME_scroll, "line=int", 1, "index=[int]",
	    "Character index for start of screenline",
	    getStartTextImage);

  attach_resource(class, "wrap", "{none,character,word}", "character",
		  "Wrap unit for long lines");
  attach_resource(class, "tab_distance", "int", "64",
		  "Tabstop interval (pixels)");

  succeed;
}

