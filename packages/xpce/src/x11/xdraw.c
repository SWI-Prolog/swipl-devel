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

/*
Public functions:

d_window(sw, x, y, w, h)	Initialise for drawing on specified
				window and area.  Sets clip area and
				clears the area.
d_image(i, x, y, w, h)		Initialise for drawing on specified image
				and area.  Sets clip area, but does not
d_clip(x, y, w, h)		Narrow clip area to specified area.
d_done()			Exit clip or device setup.

d_display(d)			Make some display the current one
d_flush()			Flush output to current display

  *** AREA MANAGEMENT ***

r_clear(x, y, w, h)		Clear specified area (fill with background
				color).
r_complement(x, y, w, h)	Invert the specified area.
r_and(x, y, w, h, pattern)	And an area with a pattern
r_fill(x, y, w, h, pattern)	Fill specified rectangle with a pattern.
r_fill_polygon(pts, n)		Fill a polygon
r_fill_triangle(x1, y1, x2, y2, x3, y3)

  *** GRAPHICS STATE ***

r_thickness(pen)		Set line width.
r_dash(dash)			Set dash pattern.
r_fillpattern(image, Name which)Set fill pattern
r_andpattern(image)		Set andpattern for greying
r_default_colour(colour)	Set default foreground colour
r_colour(colour)		Set foreground colour

  *** DRAWING ***

r_box(x, y, w, h, round, fill)	Draw filled box with rounded corners.
r_ellipse(x, y, w, h, fill)	Draw filled ellipse
r_line(x1, y1, x2, y2)		Draw line.
r_polygon(pts, n, close)	Draw a polygon
r_path(points, ox, oy, radius, close, fill)
				Draw a line through points
r_image(bm, sx, sy, x. y, w, h, transparent) Copy an image
r_set_pixel(x, y)		Set a pixel to the foreground color
r_clear_pixel(x, y)		Set a pixel to the background color
r_complement_pixel(x, y)	Invert a pixel

  *** READING ***

r_get_pixel(x, y)		Return pixel value at (x,y)

  *** TEXT MANAGEMENT ***

s_size(s, f, *w, *h)		Compute size of a string in a font
s_string(s, f, x, y, w, h, had, vad)
				Draw text 's' in font f at (x, y, w, h)
				adjusted horizontally and vertically
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>
#include "include.h"

static void	clip_area(int *, int *, int *, int *);
static void	r_andpattern(Image i);

		/********************************
		*       DEVICE FUNCTIONS	*
		********************************/

#define MAX_CLIP_NESTING	(50)

static int quick;			/* display quick_and_dirty */

static struct environment
{ iarea		area;			/* clip rectangle */
  int		level;			/* nesting level */
} environments[MAX_CLIP_NESTING];

static struct environment *env = environments;

typedef struct d_context *DContext;

static struct d_context
{ DContext	parent;			/* saved parent context */
  DrawContext	gcs;			/* The X GC's */                  
  Display      *display;		/* Current drawing display */     
  Drawable	drawable;		/* X Object we are drawing on */  
  Name		kind;			/* Drawable kind */               
  int		depth;			/* depth of drawable */           
  DisplayObj	pceDisplay;		/* PCE display object */          
  Image		cache;			/* Actually writing here */       
  Window	window;			/* Window we are caching for */   
  Any		default_background;	/* Default background colour */    
  Any		default_colour;		/* Colour for @default */
  int		cache_x;		/* X-offset of cache */           
  int		cache_y;		/* Y-offset of cache */           
  int		cache_w;		/* Width of cache */              
  int		cache_h;		/* Height of cache */
  int		offset_x;		/* Paint offset in X direction */
  int		offset_y;		/* Paint offset in Y direction */
  int		origin_x;		/* Origin-X relative to drawable */
  int		origin_y;
  int		fill_offset_x;		/* X-offset for filling */
  int		fill_offset_y;		/* Y-offset for filling */
  int		fixed_colours;		/* The colours are fixed */
					/* Save over d_image()/d_done() */
  Any		colour;			/* preserve current colours */
  Any		background;
} context;

#define X(x) ((x) + context.offset_x)
#define Y(y) ((y) + context.offset_y)
#define Translate(x, y)	 { (x) = X(x); (y) = Y(y); }
#define Clip(x, y, w, h) clip_area(&x, &y, &w, &h)

#include <gra/graphstate.c>

void
resetDraw(void)
{ struct environment *e;
  int i;

  for(e=environments, i=0; i<MAX_CLIP_NESTING; i++, e++)
    e->level = i;

  env = environments;

  context.fixed_colours = 0;
}


static Any
registerColour(Any *bin, Any c)
{ Any old = *bin;

  *bin = c;
  if ( isObject(old) && !isProtectedObj(old) )
  { delRefObj(old);
    if ( isVirginObj(old) )
    { freeObject(old);
      old = NIL;
    }
  }
  if ( isObject(c) && !isProtectedObj(c) )
    addRefObj(c);

  return old;
}


static void
d_push_context(void)
{ DContext ctx = alloc(sizeof(struct d_context));
  
  if ( env->level > 0 )
  { registerColour(&context.colour, context.gcs->colour);
    registerColour(&context.background, context.gcs->background);
  }

  *ctx = context;			/* structure copy! */
  context.parent = ctx;
}


static void
d_pop_context()
{ if ( context.parent != NULL )
  { DContext ctx = context.parent;

    registerColour(&context.colour, NIL);
    registerColour(&context.background, NIL);

    context = *ctx;			/* structure copy! */
    unalloc(sizeof(struct d_context), ctx);
  }
}


void
d_offset(int x, int y)
{ DEBUG(NAME_redraw, Cprintf("d_offset(%d, %d)\n", x, y));

  context.offset_x = x;
  context.offset_y = y;
}


void
r_offset(int x, int y)
{ context.offset_x += x;
  context.offset_y += y;
}


static void
d_set_filloffset()
{ int tsx, tsy;

  tsx = context.fill_offset_x + context.offset_x;
  tsy = context.fill_offset_y + context.offset_y;

  XSetTSOrigin(context.display, context.gcs->fillGC,  tsx, tsy);
  XSetTSOrigin(context.display, context.gcs->clearGC, tsx, tsy);
  XSetTSOrigin(context.display, context.gcs->workGC,  tsx, tsy);

  DEBUG(NAME_fillOffset, Cprintf("set_filloffset() to %d, %d\n", tsx, tsy));
}


void
r_filloffset(Point offset, int x0, int y0, fill_state *state)
{ state->x = context.fill_offset_x;
  state->y = context.fill_offset_y;

  if ( notNil(offset) )
  { context.fill_offset_x = valInt(offset->x) + x0;
    context.fill_offset_y = valInt(offset->y) + y0;

    d_set_filloffset();
  }
}


void
r_fillrestore(fill_state *state)
{ if ( state->x != context.fill_offset_x ||
       state->y != context.fill_offset_y )
  { context.fill_offset_x = state->x;
    context.fill_offset_y = state->y;

    d_set_filloffset();
  }
}


DisplayObj
d_display(DisplayObj d)
{ DisplayObj old = context.pceDisplay;

  if ( isDefault(d) )
    d = CurrentDisplay(NIL);

  if ( context.pceDisplay != d )
  { DisplayWsXref r;

    openDisplay(d);
    r = d->ws_ref;

    context.pceDisplay = d;
    context.display    = r->display_xref;
    context.depth      = r->depth;
    context.gcs	       = r->pixmap_context;
  }

  quick = (d->quick_and_dirty == ON);

  return old;
}


void
d_ensure_display()
{ if ( context.pceDisplay == NULL )
    d_display(CurrentDisplay(NIL));
}


void
d_flush(void)
{ XFlush(context.display);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise a picture to redraw all graphicals in the area (x y w h) in the
pictures coordinate system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);

  DEBUG(NAME_redraw,
	Cprintf("d_window(%s, %d, %d, %d, %d) (on %s)\n",
		pp(sw), x, y, w, h, pp(d)));

  if ( env->level != 0 )
    resetDraw();			/* security measure */

  d_push_context();
  d_display(d);

  context.origin_x	     = context.offset_x;
  context.origin_y	     = context.offset_y;
  context.drawable	     = (Drawable) XtWindow(widgetWindow(sw));
  context.kind		     = NAME_window;

  env++;
  if ( limit )
  { int dx, dy, dw, dh;

    compute_window(sw, &dx, &dy, &dw, &dh);
    env->area.x   = dx;
    env->area.y   = dy;
    env->area.w   = dw;
    env->area.h   = dh;
  } else
  { env->area.x   = 0;
    env->area.y   = 0;
    env->area.w   = valInt(sw->area->w);
    env->area.h   = valInt(sw->area->h);
  }

  DEBUG(NAME_redraw,
	Cprintf("area = (%d, %d, %d, %d) %s\n",
		env->area.x, env->area.y, env->area.w, env->area.h,
		clear ? "clear" : "no clear"));

  if ( limit && notNil(d->cache) && clear && sw->buffered_update != OFF )
  { NormaliseArea(x, y, w, h);
    context.cache_x = x; context.cache_y = y;
    context.cache_w = w; context.cache_h = h;
    Translate(context.cache_x, context.cache_y);
    Clip(context.cache_x, context.cache_y, context.cache_w, context.cache_h);

    if ( context.cache_w <= valInt(d->cache->size->w) &&
	 context.cache_h <= valInt(d->cache->size->h) )
    { context.offset_x -= context.cache_x;
      context.offset_y -= context.cache_y;
      env->area.x = env->area.y = 0;
      env->area.w = context.cache_w;
      env->area.h = context.cache_h;
      context.cache = d->cache;
      context.window = context.drawable;
      context.drawable = (Drawable) getXrefObject(context.cache, d);
      assign(context.cache, foreground, sw->colour);
      assign(context.cache, background, sw->background);
      context.kind = NAME_pixmap;
    } else
    { context.cache_x = context.cache_y = 0;
      context.cache = NULL;
    }
  } else
  { context.cache_x = context.cache_y = 0;
    context.cache = NULL;
  }

  d_set_filloffset();
  d_clip(x, y, w, h);

  r_background(sw->background);
  context.default_background = sw->background;
  r_default_colour(sw->colour);

  if ( clear )
    r_clear(x, y, w, h);
}


void
d_image(Image i, int x, int y, int w, int h)
{ DisplayObj d = i->display;
  Pixmap image;

  d_push_context();

  if ( isNil(d) )
  { if ( isNil(d = context.pceDisplay) || d == NULL )
      d = CurrentDisplay(i);
  }

  image = (Pixmap) getXrefObject(i, d);
  
  d_display(d);
  d_offset(0, 0);			/* Do we need this for images? */

  Translate(x, y);
  
  context.cache_x	= 0;
  context.cache_y	= 0;
  context.cache		= NULL;
  context.origin_x	= 0;
  context.origin_y	= 0;

  if ( i->kind == NAME_bitmap )
  { DisplayWsXref r = d->ws_ref;
    context.gcs = r->bitmap_context;
  }

  if ( isDefault(i->foreground) )
    context.default_colour = d->foreground;
  else
    context.default_colour = i->foreground;
  if ( isDefault(i->background) )
    context.default_background = d->background;    
  else
    context.default_background = i->background;
  context.drawable	     = (Drawable) image;
  context.kind		     = i->kind;

  if ( i->kind == NAME_pixmap )
  { r_background(context.default_background);
    r_default_colour(context.default_colour);
  }

  env++;
  env->area.x   = 0;
  env->area.y   = 0;
  env->area.w   = valInt(i->size->w);
  env->area.h   = valInt(i->size->h);

  XSetTSOrigin(context.display, context.gcs->fillGC,
	       context.fill_offset_x, context.fill_offset_y);

  d_clip(x, y, w, h);
}


static void
d_xwindow(DisplayObj d, Window win, int x, int y, int w, int h)
{ d_offset(0, 0);
  Translate(x, y);

  d_push_context();
  d_display(d);
  
  context.cache_x	     = 0;
  context.cache_y	     = 0;
  context.origin_x	     = 0;
  context.origin_y	     = 0;
  context.drawable	     = (Drawable) win;
  context.default_background = d->background;
  context.kind		     = NAME_window;

  r_background(d->background);

  env++;
  env->area.x   = 0;
  env->area.y   = 0;
  env->area.w   = w;
  env->area.h   = h;

  d_clip(x, y, w, h);
}


void
d_screen(DisplayObj d)
{ XWindowAttributes atts;
  int iw, ih;
  Window root;
  DisplayWsXref r = d->ws_ref;

  XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);
  root = atts.root;
  XGetWindowAttributes(r->display_xref, root, &atts);

  iw = atts.width; ih = atts.height;

  DEBUG(NAME_draw,
	Cprintf("d_xwindow(%s, %ld, %dx%d\n", pp(d), root, iw, ih));

  d_xwindow(d, root, 0, 0, iw, ih);
}


void
d_frame(FrameObj fr, int x, int y, int w, int h)
{ Widget widget = widgetFrame(fr);

  if ( widget )
  { d_xwindow(fr->display, XtWindow(widget), x, y, w, h);
  }
}


void
d_clip(int x, int y, int w, int h)
{ XRectangle rect;

  DEBUG(NAME_redraw, Cprintf("d_clip(%d, %d, %d, %d) -> ", x, y, w, h));
  NormaliseArea(x, y, w, h);
  Translate(x, y);
  DEBUG(NAME_redraw, Cprintf("(%d %d %d %d) -> ", x, y, w, h));
  Clip(x, y, w, h);
  DEBUG(NAME_redraw, Cprintf("(%d %d %d %d)\n", x, y, w, h));

  env++;
  env->area.x = x;
  env->area.y = y;
  env->area.w = w;
  env->area.h = h;

  rect.x      = x;
  rect.y      = y;
  rect.width  = w;
  rect.height = h;

  DEBUG(NAME_redraw, Cprintf("clip to %d %d %d %d\n", x, y, w, h));

# define CLIP(x) XSetClipRectangles(context.display, x, 0, 0, &rect, \
				    1, Unsorted)
  CLIP(context.gcs->workGC);
  CLIP(context.gcs->fillGC);
  CLIP(context.gcs->shadowGC);
  CLIP(context.gcs->reliefGC);
}


void
d_done()
{ if ( context.cache != NULL )
  { DEBUG(NAME_redraw, Cprintf("writing cache to (%d %d %d %d)\n",
			       context.cache_x, context.cache_y,
			       context.cache_w, context.cache_h));
    XCopyArea(context.display, context.drawable, context.window,
	      context.gcs->copyGC, 0, 0,
	      context.cache_w, context.cache_h,
	      context.cache_x, context.cache_y);

    context.cache = NULL;
  }

  env--;
  d_clip_done();
  if ( env->level > 0 )
  { Any c;

    if ( (c=context.parent->colour) && notNil(c) )
      r_colour(c);
    if ( (c=context.parent->background) && notNil(c) )
      r_background(c);
  }
  d_pop_context();
  DEBUG(NAME_redraw, Cprintf("After d_done(): env->level = %d\n", env->level));
}


void
d_clip_done(void)
{ env--;

  DEBUG(NAME_redraw, Cprintf("d_done()\n"));

  assert(env >= environments);		/* stack underflow */

  if ( env->level != 0 )		/* outermost: no use */
  { XRectangle rect;

    rect.x      = env->area.x;
    rect.y      = env->area.y;
    rect.width  = env->area.w;
    rect.height = env->area.h;

    CLIP(context.gcs->workGC);
    CLIP(context.gcs->fillGC);
    CLIP(context.gcs->shadowGC);
    CLIP(context.gcs->reliefGC);
  } 

#   undef CLIP
}


void
intersection_iarea(IArea a, IArea b)
{ int x, y, w, h;

  x = (a->x > b->x ? a->x : b->x);
  y = (a->y > b->y ? a->y : b->y);
  w = (a->x + a->w < b->x + b->w ? a->x + a->w : b->x + b->w) - x;
  h = (a->y + a->h < b->y + b->h ? a->y + a->h : b->y + b->h) - y;

  if ( w < 0 ) w = 0;  
  if ( h < 0 ) h = 0;

  a->x = x;
  a->y = y;
  a->w = w;
  a->h = h;
}


static void
clip_area(int *x, int *y, int *w, int *h)
{ iarea a;

  a.x = *x; a.y = *y; a.w = *w; a.h = *h;
  intersection_iarea(&a, &env->area);
  *x = a.x; *y = a.y; *w = a.w; *h = a.h;
}


static int
in_clip(int x, int y)
{ IArea a = &env->area;

  if ( x >= a->x && x < a->x + a->w && y >= a->y && y < a->h + a->h )
    return TRUE;

  return FALSE;
}

		/********************************
		*        AREA MANAGEMENT	*
		********************************/

void
r_clear(int x, int y, int w, int h)
{ NormaliseArea(x, y, w, h);
  Translate(x, y);
  Clip(x, y, w, h);

  if ( w > 0 && h > 0 )
  { DEBUG(NAME_background, Cprintf("r_clear(%d, %d, %d, %d) in %s context\n",
				   x, y, w, h, pp(context.gcs->kind)));

/*  if ( context.kind == NAME_window )
      XClearArea(context.display, context.drawable, x, y, w, h, False);
    else */
      XFillRectangle(context.display, context.drawable, context.gcs->clearGC,
		     x, y, w, h);
  }
}


void
r_complement(int x, int y, int w, int h)
{ NormaliseArea(x, y, w, h);
  Translate(x, y);
  Clip(x, y, w, h);

  if ( w > 0 && h > 0 )
    XFillRectangle(context.display, context.drawable,
		   context.gcs->complementGC,
		   x, y, w, h);
}


void
r_and(int x, int y, int w, int h, Image pattern)
{ NormaliseArea(x, y, w, h);
  Translate(x, y);
  Clip(x, y, w, h);

  if ( w > 0 && h > 0 )
  { r_andpattern(pattern);
    XFillRectangle(context.display, context.drawable,
		   context.gcs->andGC,
		   x, y, w, h);
  }
}

		/********************************
		*         GRAPHICS STATE	*
		********************************/

void
r_thickness(int pen)
{ if ( pen != context.gcs->pen )
  { XGCValues values;

					/* pen == 0 draws faster! */
    values.line_width = ((pen == 1) && quick ? 0 : pen);
    XChangeGC(context.display, context.gcs->workGC, GCLineWidth, &values);

    context.gcs->pen = pen;
  }
}


int
r_transformed(int val)
{ return val;				/* see msdraw.c */
}


static struct dashpattern
{ Name	dash;
  int	line_style;
  char *dash_list;
  int	dash_list_length;
} dash_patterns[] =
{ { NAME_none,		LineSolid,	"",				0},
  { NAME_dotted,	LineOnOffDash,	"\01\02",			2},
  { NAME_dashed,	LineOnOffDash,	"\07\07",			2},
  { NAME_dashdot,	LineOnOffDash,	"\07\03\01\07",			4},
  { NAME_dashdotted,	LineOnOffDash,	"\011\03\01\03\01\03\01\03",	8},
  { NAME_longdash,	LineOnOffDash,	"\015\07",			2},
  { 0,			0,		"",				0},
};


void
r_dash(Name name)
{ if ( name != context.gcs->dash )
  { struct dashpattern *dp = dash_patterns;

    for( ; dp->dash != 0; dp++ )
    { if ( dp->dash == name )
      { XGCValues values;

	values.line_style = dp->line_style;
	XChangeGC(context.display, context.gcs->workGC, GCLineStyle, &values);
	if ( dp->dash_list_length > 0 )
	  XSetDashes(context.display, context.gcs->workGC, 0, dp->dash_list,
		     dp->dash_list_length);

	context.gcs->dash = name;
	return;
      }
    }
    errorPce(name, NAME_badTexture);
  }
}


void
d_pen(Pen pen)
{ r_thickness(valInt(pen->thickness));
  r_dash(pen->texture);
  if ( notDefault(pen->colour) )
    r_colour(pen->colour);
}


void
r_fillpattern(Any fill, Name which)	/* image or colour */
{ DEBUG(NAME_fillPattern,
	Cprintf("r_fillpattern(%s, %s) ", pp(fill), pp(which)));

  if ( isDefault(fill) )
    fill = context.gcs->colour;
  else if ( fill == NAME_current )
    return;

  if ( context.fixed_colours && !instanceOfObject(fill, ClassImage) )
    fill = (which == NAME_foreground ? context.gcs->colour
				     : context.gcs->background);

  if ( fill != context.gcs->fill )
  { XGCValues values;
    unsigned long mask;

    DEBUG(NAME_fillPattern, Cprintf("Changing\n"));

    if ( instanceOfObject(fill, ClassImage) )
    { Image i = fill;
      Pixmap pm = (Pixmap) getXrefObject(fill, context.pceDisplay);

      if ( context.kind != NAME_bitmap && i->kind == NAME_bitmap )
      { values.stipple    = pm;
	values.fill_style = FillOpaqueStippled;
	values.foreground = context.gcs->foreground_pixel;
	values.background = context.gcs->background_pixel;
	DEBUG(NAME_fillPattern, Cprintf("fg = %ld, bg = %ld\n",
					context.gcs->foreground_pixel,
					context.gcs->background_pixel));
	mask 		  = (GCStipple|GCFillStyle|GCForeground|GCBackground);
      } else
      { values.tile       = pm;
	values.fill_style = FillTiled;
	mask		  = (GCTile|GCFillStyle);
      }
    } else				/* solid colour */
    { mask = GCForeground|GCFillStyle;
      values.foreground = getPixelColour(fill, context.pceDisplay);
      values.fill_style = FillSolid;
    }

    XChangeGC(context.display, context.gcs->fillGC, mask, &values);

					/* maintain a reference to */
    delRefObj(context.gcs->fill);	/* avoid drop-out */
    freeableObj(context.gcs->fill);
    addRefObj(fill);

    context.gcs->fill = fill;
  } else
  { DEBUG(NAME_fillPattern, Cprintf("Not changed\n"));
  }
}


void
r_arcmode(Name mode)
{ if ( mode != context.gcs->arcmode )
  { XSetArcMode(context.display, context.gcs->fillGC,
	        mode == NAME_chord ? ArcChord : ArcPieSlice);

    context.gcs->arcmode = mode;
  }
}


static void
r_andpattern(Image i)
{ Pixmap image;

  if ( i != context.gcs->and_pattern &&
       (image = (Pixmap) getXrefObject(i, context.pceDisplay)) != 0 )
  { XGCValues values;
    unsigned long mask;
    
    if ( context.kind != NAME_bitmap && i->kind == NAME_bitmap )
    { values.stipple    = image;
      values.fill_style = FillOpaqueStippled;
      mask 		= (GCStipple|GCFillStyle);
    } else
    { values.tile       = image;
      values.fill_style = FillTiled;
      mask		= (GCTile|GCFillStyle);
    }
    XChangeGC(context.display, context.gcs->andGC, mask, &values);

    context.gcs->and_pattern = i;
  }
}


void
r_fix_colours(Any fg, Any bg, ColourContext ctx)
{ ctx->foreground = context.gcs->colour;
  ctx->background = context.gcs->background;
  ctx->lock	  = context.fixed_colours;

  if ( !context.fixed_colours )
  { if ( !fg || isNil(fg) ) fg = DEFAULT;
    if ( !bg || isNil(bg) ) bg = DEFAULT;

    r_default_colour(fg);
    r_background(bg);
  }

  context.fixed_colours++;
}


void
r_unfix_colours(ColourContext ctx)
{ if ( (context.fixed_colours = ctx->lock) == 0 )
  { r_default_colour(ctx->foreground);
    r_background(ctx->background);
  }
}



Any
r_default_colour(Any c)
{ Any old = context.default_colour;
  
  if ( !context.fixed_colours )
  { if ( notDefault(c) )
      context.default_colour = c;

    r_colour(context.default_colour);
  }
  
  return old;
}


Any
r_colour(Any c)
{ Colour old = context.gcs->colour;

  if ( context.fixed_colours )
    return old;

  if ( isDefault(c) )
    c = context.default_colour;

  if ( c != context.gcs->colour )
  { if ( context.gcs->kind != NAME_bitmap )
    { XGCValues values;
      unsigned long mask;

      if ( instanceOfObject(c, ClassColour) )
      { unsigned long pixel = getPixelColour(c, context.pceDisplay);
	
	values.foreground = pixel;
	values.fill_style = FillSolid;
	mask		  = (GCForeground|GCFillStyle);

	context.gcs->foreground_pixel = pixel;
      } else
      { Pixmap pm   = (Pixmap) getXrefObject(c, context.pceDisplay);

	values.tile       = pm;
	values.fill_style = FillTiled;
	mask		  = (GCTile|GCFillStyle);
      }

      XChangeGC(context.display, context.gcs->workGC, mask, &values);
      if ( instanceOfObject(context.gcs->fill, ClassImage) &&
	   instanceOfObject(c, ClassColour))
	XChangeGC(context.display, context.gcs->fillGC, GCForeground, &values);
    } 

    old = registerColour(&context.gcs->colour, c);
  }

  return old;
}


Any
r_background(Any c)
{ Any ob = context.gcs->background;

  if ( isDefault(c) || context.fixed_colours )
    return ob;

  if ( c != context.gcs->background )
  { if ( context.gcs->kind != NAME_bitmap )
    { XGCValues values;
      unsigned long mask;

      DEBUG(NAME_background, Cprintf("Setting clearGC of %s context to %s\n",
				     pp(context.gcs->kind), pp(c)));

      if ( instanceOfObject(c, ClassColour) )
      { unsigned long pixel = getPixelColour(c, context.pceDisplay);
	
	values.foreground = pixel;
	values.fill_style = FillSolid;
	mask		  = (GCForeground|GCFillStyle);

	context.gcs->background_pixel = pixel;
      } else
      { Image i   = (Image) c;
	Pixmap pm = (Pixmap) getXrefObject(i, context.pceDisplay);

	if ( i->kind == NAME_bitmap )
	{ DisplayWsXref r = context.pceDisplay->ws_ref;

	  values.stipple    = pm;
	  values.fill_style = FillOpaqueStippled;
	  values.foreground = r->black_pixel;
	  values.background = r->white_pixel;
	  mask		    = (GCStipple|GCFillStyle|GCForeground|GCBackground);
	} else
	{ values.tile       = pm;
	  values.fill_style = FillTiled;
	  mask		    = (GCTile|GCFillStyle);
	}
      }

      ob = registerColour(&context.gcs->background, c);
      context.gcs->elevation = NIL;	/* force update */
      XChangeGC(context.display, context.gcs->clearGC, mask, &values);
    }
  }

  return ob;
}


void
r_swap_background_and_foreground()
{ if ( context.gcs->kind != NAME_bitmap )
  { Colour tc = context.gcs->background;

    r_background(context.gcs->colour);
    r_colour(tc);
  }
}


Bool
r_subwindow_mode(Bool val)
{ Bool old = context.gcs->subwindow_mode;

  if ( context.gcs->subwindow_mode != val )
  { int mode = (val == ON ? IncludeInferiors : ClipByChildren);

    XSetSubwindowMode(context.display, context.gcs->workGC,   mode);
    XSetSubwindowMode(context.display, context.gcs->fillGC,   mode);
    XSetSubwindowMode(context.display, context.gcs->copyGC,   mode);
    XSetSubwindowMode(context.display, context.gcs->bitmapGC, mode);
    XSetSubwindowMode(context.display, context.gcs->andGC,    mode);
    XSetSubwindowMode(context.display, context.gcs->opGC,     mode);

    context.gcs->subwindow_mode = val;
  }

  return old;
}


void
r_invert_mode(Bool val)
{ if ( context.gcs->invert_mode != val )
  { XGCValues values;
    int mask = GCFunction|GCPlaneMask;
    
    if ( val == ON )
    { values.function   = GXinvert;
      values.plane_mask = AllPlanes;
    } else
    { values.function   = GXcopy;
      values.plane_mask = AllPlanes;
    }

    XChangeGC(context.display, context.gcs->workGC,   mask, &values);
    XChangeGC(context.display, context.gcs->fillGC,   mask, &values);
    XChangeGC(context.display, context.gcs->copyGC,   mask, &values);
    XChangeGC(context.display, context.gcs->bitmapGC, mask, &values);
    XChangeGC(context.display, context.gcs->opGC,     mask, &values);

    context.gcs->invert_mode = val;
  }
}


		 /*******************************
		 *	     COORDINATES	*
		 *******************************/

void
r_translate(int x, int y, int *ox, int *oy)
{ Translate(x, y);

  *ox = x; 
  *oy = y;
}

		/********************************
		*            DRAWING		*
		********************************/


void
r_box(int x, int y, int w, int h, int r, Any fill)
{ int mwh, pen = context.gcs->pen;
  int drawpen;

  if ( context.gcs->dash == NAME_none && quick && pen > 0 )
    drawpen = 1;
  else
    drawpen = pen;

  Translate(x, y);
  r_thickness(drawpen);

  NormaliseArea(x, y, w, h);
  mwh = (w < h ? w : h);

  if ( mwh == 0 )
    return;

  DEBUG(NAME_redraw,
	Cprintf("r_box(%d, %d, %d, %d)\n", x, y, w, h));

  if ( r > mwh / 2 - pen)
    r = mwh / 2 - pen;

  x += drawpen / 2;
  y += drawpen / 2;
  w -= drawpen;
  h -= drawpen;
  if ( drawpen <= 0 )
    drawpen = 1;			/* ensure termination */

  if ( notNil(fill) )
    r_fillpattern(fill, NAME_background);

  if ( r <= 0 )
  { int n;

    if ( notNil(fill) )
      XFillRectangle(context.display, context.drawable, context.gcs->fillGC,
		     x, y, w, h);

    for( n = pen; n > 0; n -= drawpen )
    { XDrawRectangle(context.display, context.drawable, context.gcs->workGC,
		     x, y, w, h);

      x += drawpen;
      y += drawpen;
      w -= 2*drawpen;
      h -= 2*drawpen;
    }
  } else if ( r <= 4 )
  { XSegment s[8];			/* little rounded corners */
    int n;

    for( n = pen; n >= 0; n -= drawpen )
    { s[0].x1 = x+r;   s[0].y1 = y;     s[0].x2 = x+w-r; s[0].y2 = y;
      s[1].x1 = x+w-r; s[1].y1 = y;     s[1].x2 = x+w;   s[1].y2 = y+r;
      s[2].x1 = x+w;   s[2].y1 = y+r;   s[2].x2 = x+w;   s[2].y2 = y+h-r;
      s[3].x1 = x+w;   s[3].y1 = y+h-r; s[3].x2 = x+w-r; s[3].y2 = y+h;
      s[4].x1 = x+w-r; s[4].y1 = y+h;   s[4].x2 = x+r;   s[4].y2 = y+h;
      s[5].x1 = x+r;   s[5].y1 = y+h;   s[5].x2 = x;     s[5].y2 = y+h-r;
      s[6].x1 = x;     s[6].y1 = y+h-r; s[6].x2 = x;     s[6].y2 = y+r;
      s[7].x1 = x;     s[7].y1 = y+r;   s[7].x2 = x+r;   s[7].y2 = y;

      if ( n == pen && notNil(fill) )
      { XPoint p[8];
	int n;

	for(n=0; n<8; n++)
	{ p[n].x = s[n].x1;
	  p[n].y = s[n].y1;
	}

	XFillPolygon(context.display, context.drawable, context.gcs->fillGC,
		     p, 8, Convex, CoordModeOrigin);  
      }

      if ( n > 0 )
	XDrawSegments(context.display, context.drawable, context.gcs->workGC,
		      s, 8);

      x += drawpen;
      y += drawpen;
      w -= 2*drawpen;
      h -= 2*drawpen;
      r -= drawpen;
    }
  } else
  { XSegment s[4];			/* rounded corners */
    XArc     a[4];
    short    wh;
    int n;

    for( n = pen; n >= 0; n -= drawpen )
    { wh = r*2;

      s[0].x1 = x+r; s[0].y1 = y;   s[0].x2 = x+w-r; s[0].y2 = y;
      s[1].x1 = x+w; s[1].y1 = y+r; s[1].x2 = x+w;   s[1].y2 = y+h-r;
      s[2].x1 = x+r; s[2].y1 = y+h; s[2].x2 = x+w-r; s[2].y2 = y+h;
      s[3].x1 = x;   s[3].y1 = y+r; s[3].x2 = x;     s[3].y2 = y+h-r;

      a[0].x = x;      a[0].y = y;      a[0].width = a[0].height = wh;
      a[0].angle1 = 90*64;  a[0].angle2 = 90*64;
      a[1].x = x+w-wh; a[1].y = y;      a[1].width = a[1].height = wh;
      a[1].angle1 = 0*64;   a[1].angle2 = 90*64;
      a[2].x = x+w-wh; a[2].y = y+h-wh; a[2].width = a[2].height = wh;
      a[2].angle1 = 270*64; a[2].angle2 = 90*64;
      a[3].x = x;      a[3].y = y+h-wh; a[3].width = a[3].height = wh;
      a[3].angle1 = 180*64; a[3].angle2 = 90*64;

      r_arcmode(NAME_pieSlice);
      if ( n == pen && notNil(fill) )
      { XFillArcs(context.display, context.drawable, context.gcs->fillGC,
		  a, 4);
#define FILL(x, y, w, h) \
	XFillRectangle(context.display, context.drawable, \
		       context.gcs->fillGC, x, y, w, h)
	FILL(x,     y+r, r,    h-wh);
	FILL(x+w-r, y+r, r,    h-wh);
	FILL(x+r,   y,   w-wh, h);
#undef FILL
      }

      if ( n > 0 )
      { XDrawSegments(context.display, context.drawable, context.gcs->workGC,
		      s, 4);
	XDrawArcs(context.display, context.drawable, context.gcs->workGC,
		  a, 4);
      }

      x += drawpen;
      y += drawpen;
      w -= 2*drawpen;
      h -= 2*drawpen;
      r -= drawpen;
    }
  }
}


void
r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill)
{ if ( !shadow )
  { r_box(x, y, w, h, r, fill);
    return;
  }

  if ( shadow > h ) shadow = h;
  if ( shadow > w ) shadow = w;

  r_colour(BLACK_COLOUR);
  r_box(x+shadow, y+shadow, w-shadow, h-shadow, r, BLACK_IMAGE);
  r_colour(DEFAULT);
  r_box(x, y, w-shadow, h-shadow, r, isNil(fill) ? WHITE_IMAGE : fill);
}


#define MAX_SHADOW 10

Any
r_elevation_shadow(Elevation e)
{ if ( isDefault(e->shadow) )
  { Any bg = context.gcs->background;

    if ( instanceOfObject(bg, ClassColour) && context.gcs->depth != 1 )
      return getReduceColour(bg);
    else
      return BLACK_COLOUR;
  } else
    return e->shadow;
}


static Any
r_elevation_relief(Elevation e)
{ if ( isDefault(e->relief) )
  { Any bg = context.gcs->background;

    if ( instanceOfObject(bg, ClassColour) && context.gcs->depth != 1 )
      return getHiliteColour(bg);
    else
      return WHITE_COLOUR;
  } else
    return e->relief;
}


static void
r_elevation(Elevation e)
{ if ( context.gcs->elevation != e )
  { Any relief = r_elevation_relief(e);
    Any shadow = r_elevation_shadow(e);
  
    x11_set_gc_foreground(context.pceDisplay, relief,
			  1, &context.gcs->reliefGC);
    x11_set_gc_foreground(context.pceDisplay, shadow,
			  1, &context.gcs->shadowGC);

    context.gcs->elevation = e;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
r_elevation_fillpattern(Elevation e, int up)
    Sets the fill-pattern for the interior of elevated areas and returns
    TRUE if the interior needs to be filled.  Returns FALSE otherwise.
    The special colours `reduced' and `hilited' are interpreted as relative
    colours to the background.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
r_elevation_fillpattern(Elevation e, int up)
{ Any fill = NIL;

  if ( up && notDefault(e->colour) )
  { fill = e->colour;
  } else if ( !up && notDefault(e->background) )
  { fill = e->background;
  }

  if ( isNil(fill) )
    fail;

  if ( fill == NAME_reduced || fill == NAME_hilited )
  { Any bg = context.gcs->background;

    if ( instanceOfObject(bg, ClassColour) && context.gcs->depth != 1 )
    { if ( fill == NAME_reduced )
	fill = getReduceColour(bg);
      else
	fill = getHiliteColour(bg);
    } else
      fail;
  }
  
  r_fillpattern(fill, NAME_background);

  succeed;
}


void
r_3d_segments(int n, ISegment s, Elevation e, int light)
{ XSegment *xs = (XSegment *)alloca(sizeof(XSegment) * n);
  XSegment *xp = xs;
  ISegment p   = s;
  int i;

  r_elevation(e);
  
  for(i=0; i<n; i++, p++, xp++)
  { xp->x1 = X(p->x1);
    xp->y1 = Y(p->y1);
    xp->x2 = X(p->x2);
    xp->y2 = Y(p->y2);
  }
    
  XDrawSegments(context.display, context.drawable,
		light ? context.gcs->reliefGC : context.gcs->shadowGC,
		xs, n);
}


void
r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up)
{ int i;
  int pen = 1;
  int os;
  GC TopLeftGC, BottomRightGC;
  int xt, yt;
  int shadow = valInt(e->height);
  int fill;				/* fill interior */

  NormaliseArea(x, y, w, h);
  if ( radius > 0 )
  { int maxr = min(w,h)/2;

    if ( radius > maxr )
      radius = maxr;
  }

  if ( e->kind == NAME_shadow )
  { XSegment s[2 * MAX_SHADOW];
    int is = 0;				/* # segments */

    r_elevation(e);

    shadow = abs(shadow);
    shadow = min(shadow, min(w, h));
    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;
    r_box(x, y, w-shadow, h-shadow, radius-shadow, e->colour);
    
    xt = x, yt = y;
    Translate(xt, yt);

    if ( radius > 0 )
    { int  r = min(radius, min(w, h));
      XArc as[MAX_SHADOW * 3];
      int  ns = 0;

      w--, h--;
      for( os=0; os < shadow; os++ )
      { int ar = r - shadow + os;
	int ang /*= 90/(os+1) */;

	s[is].x1 = xt+w-os;		s[is].y1 = yt+r-shadow; /* vert */
	s[is].x2 = xt+w-os;		s[is].y2 = yt+h-r;
	is++;
	s[is].x1 = xt+r-shadow;		s[is].y1 = yt+h-os; /* hor */
	s[is].x2 = xt+w-r;		s[is].y2 = yt+h-os;
	is++;
					/* bottom-right at xt+w-r, yt+h-r */
	as[ns].x = xt+w-r-ar+1;		as[ns].y = yt+h-r-ar+1;
	as[ns].width = 			as[ns].height = ar*2;
	as[ns].angle1 = 270*64;		as[ns].angle2 = 90*64;
	ns++;
					/* top-right around xt+w-r, yt+r */
	ang = 90;
	as[ns].x = xt+w-2*ar-os;	as[ns].y = yt;
	as[ns].width = 			as[ns].height = ar*2;
	as[ns].angle1 = 0*64;		as[ns].angle2 = ang*64;
	ns++;
					/* bottom-left around xt+r, yt+h-r */
	as[ns].x = xt;			as[ns].y = yt+h-2*ar-os;
	as[ns].width = 			as[ns].height = ar*2;
	as[ns].angle1 = (270-ang)*64;	as[ns].angle2 = ang*64;
	ns++;
      }

      XDrawArcs(context.display, context.drawable,
		context.gcs->shadowGC, as, ns);
    } else
    { w -= shadow;
      h -= shadow;

      for( os=0; os < shadow; os++ )
      { s[is].x1 = xt+w+os;	s[is].y1 = yt+shadow;
	s[is].x2 = xt+w+os;	s[is].y2 = yt+h+os;
	is++;
	s[is].x1 = xt+shadow;	s[is].y1 = yt+h+os;
	s[is].x2 = xt+w+os;	s[is].y2 = yt+h+os;
	is++;
      }
    }

    XDrawSegments(context.display, context.drawable,
		  context.gcs->shadowGC, s, is);

    return;
  }
  
  if ( !up  )
    shadow = -shadow;
  fill = r_elevation_fillpattern(e, up);

					/* 3D box */
  if ( shadow )
  { r_elevation(e);

    if ( shadow > 0 )
    { TopLeftGC     = context.gcs->reliefGC;
      BottomRightGC = context.gcs->shadowGC;
    } else
    { TopLeftGC     = context.gcs->shadowGC;
      BottomRightGC = context.gcs->reliefGC;
      shadow        = -shadow;
    }
  
    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;
  
    xt = x, yt = y;
    Translate(xt, yt);
  
    if ( radius > 0 )			/* with rounded corners */
    { XSegment sr[MAX_SHADOW * 2];	/* top, left */
      XArc     ar[MAX_SHADOW * 3];	/* idem */
      XSegment ss[MAX_SHADOW * 2];	/* bottom, right */
      XArc     as[MAX_SHADOW * 3];	/* item */
      int      is=0, ir=0, ns=0, nr=0;	/* # items */
      int      os;
  
      w--, h--;

      r_arcmode(NAME_pieSlice);
      if ( fill )
      { GC FillGC = context.gcs->fillGC;
	int r2 = 2 * radius;
	int r = radius;
	XRectangle rs[3];

	ar[0].angle1 = 90*64; ar[0].angle2 = 90*64; /* top-left */
	ar[0].x = xt; ar[0].y = yt; ar[0].width = r2; ar[0].height = r2;
 
	ar[1].angle1 = 0*64; ar[1].angle2 = 90*64; /* top-right */
	ar[1].x = xt+w-r2; ar[1].y = yt; ar[1].width = r2; ar[1].height = r2;
 
	ar[2].angle1 = 180*64; ar[2].angle2 = 90*64; /* bottom-left */
	ar[2].x = xt; ar[2].y = yt+h-r2; ar[2].width = r2; ar[2].height = r2;
 
	ar[3].angle1 = 270*64; ar[3].angle2 = 90*64; /* bottom-left */
	ar[3].x = xt+w-r2; ar[3].y = yt+h-r2; ar[3].width=r2; ar[3].height=r2;
 
/*top*/	rs[0].x = xt+r; rs[0].y = yt;     rs[0].width = w-r2; rs[0].height = r;
/*bot*/	rs[1].x = xt+r; rs[1].y = yt+h-r; rs[1].width = w-r2; rs[1].height = r;
/*body*/rs[2].x = xt;   rs[2].y = yt+r;   rs[2].width = w; rs[2].height = h-r2;

        XFillArcs(context.display, context.drawable, FillGC, ar, 4);
	XFillRectangles(context.display, context.drawable, FillGC, rs, 3);
      }

      for(os=0; os<shadow; os++)
      { int r     = radius-os;
	short wh  = r*2;
  
	sr[ir].x1 = os+xt+r;	sr[ir].y1 = os+yt;	/* top */
	sr[ir].x2 = -os+xt+w-r;	sr[ir].y2 = os+yt;
	ir++;
	sr[ir].x1 = os+xt;	sr[ir].y1 = os+yt+r;	/* left */
	sr[ir].x2 = os+xt;	sr[ir].y2 = -os+yt+h-r;
	ir++;

	ss[is].x1 = -os+xt+w;   ss[is].y1 = os+yt+r;	/* right */
	ss[is].x2 = -os+xt+w;   ss[is].y2 = -os+yt+h-r;
	is++;
	ss[is].x1 = os+xt+r;    ss[is].y1 = -os+yt+h;	/* bottom */
	ss[is].x2 = os+xt+w-r;  ss[is].y2 = -os+yt+h;
	is++;

	ar[nr].x = os+xt;	ar[nr].y = os+yt; 	/* top-left */
	ar[nr].width = wh;	ar[nr].height = wh;
        ar[nr].angle1 = 90*64;  ar[nr].angle2 = 90*64;
	nr++;
	ar[nr].x = -os+xt+w-wh;	ar[nr].y = os+yt; 	/* top-right */
	ar[nr].width = wh;	ar[nr].height = wh;
        ar[nr].angle1 = 45*64;  ar[nr].angle2 = 45*64;
	nr++;
	ar[nr].x = os+xt;	ar[nr].y = -os+yt+h-wh;	/* bottom-left */
	ar[nr].width = wh;	ar[nr].height = wh;
        ar[nr].angle1 = 180*64; ar[nr].angle2 = 45*64;
	nr++;

	as[ns].x = -os+xt+w-wh;	as[ns].y = -os+yt+h-wh;	/* bottom-right */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 270*64;	as[ns].angle2 = 90*64;
	ns++;
	as[ns].x = -os+xt+w-wh;	as[ns].y = os+yt; 	/* top-right */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 0*64;   as[ns].angle2 = 45*64;
	ns++;
	as[ns].x = os+xt;	as[ns].y = -os+yt+h-wh;	/* bottom-left */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 225*64; as[ns].angle2 = 45*64;
	ns++;
      }

      XDrawSegments(context.display, context.drawable, TopLeftGC,     sr, ir);
      XDrawSegments(context.display, context.drawable, BottomRightGC, ss, is);
      XDrawArcs(    context.display, context.drawable, TopLeftGC,     ar, nr);
      XDrawArcs(    context.display, context.drawable, BottomRightGC, as, ns);

      return;				/* did the filling already */
    } else				/* no radius */
    { XSegment s[2 * MAX_SHADOW];

      for(i=0, os=0; os < shadow; os += pen)
      { s[i].x1 = xt+os;	s[i].y1 = yt+os; 	/* top-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+os;
	i++;
	s[i].x1 = xt+os;	s[i].y1 = yt+os;	/* left-side */
	s[i].x2 = xt+os;	s[i].y2 = yt+h-1-os;
	i++;
      }
      XDrawSegments(context.display, context.drawable, TopLeftGC, s, i);
      
      for(i=0, os=0; os < shadow; os += pen)
      { s[i].x1 = xt+os;	s[i].y1 = yt+h-1-os;	/* bottom-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
	i++;
	s[i].x1 = xt+w-1-os;	s[i].y1 = yt+os;	/* right-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
	i++;
      }
      XDrawSegments(context.display, context.drawable, BottomRightGC, s, i);
    }
  }  

  if ( fill )
    r_fill(x+shadow, y+shadow, w-2*shadow, h-2*shadow, NAME_current);
}


void
r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up)
{ XSegment s[MAX_SHADOW];
  int i;
  int z = valInt(e->height);

  Translate(x1, y1);
  Translate(x2, y2);

  r_elevation(e);

  if ( z < 0 )
  { z = -z;
    up = !up;
  }

  if ( z > MAX_SHADOW )
    z = MAX_SHADOW;

  if ( y1 == y2 )
  { y1 -= z; y2 -= z;
  } else
  { x1 -= z; x2 -= z;
  }

  for(i=0; i<z; i++)
  { s[i].x1 = x1, s[i].x2 = x2, s[i].y1 = y1, s[i].y2 = y2;
    if ( y1 == y2 )
      y1++, y2++;
    else
      x1++, x2++;
  }
  XDrawSegments(context.display, context.drawable,
		up ? context.gcs->reliefGC : context.gcs->shadowGC, s, i);
  for(i=0; i<z; i++)
  { s[i].x1 = x1, s[i].x2 = x2, s[i].y1 = y1, s[i].y2 = y2;
    if ( y1 == y2 )
      y1++, y2++;
    else
      x1++, x2++;
  }
  XDrawSegments(context.display, context.drawable,
		up ? context.gcs->shadowGC : context.gcs->reliefGC, s, i);
}


static inline void
step_to(int *x1, int *y1, int tx, int ty)
{ if ( tx > *x1 )
    (*x1)++;
  else if ( tx < *x1 )
    (*x1)--;

  if ( ty > *y1 )
    (*y1)++;
  else if ( ty < *y1 )
    (*y1)--;
}


void
r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3,
	      Elevation e, int up, int map)
{ XSegment s[3];
  GC topGC, botGC;
  int z, n, i, p;
  int cx, cy;

  if ( !e || isNil(e) )
  { r_triangle(x1, y1, x2, y2, x3, y3, up ? NIL : BLACK_COLOUR);
    return;
  }

  r_elevation(e);
  z = valInt(e->height);

  if ( !up )
    z = -z;

  if ( z > 0 )
  { topGC = context.gcs->reliefGC;
    botGC = context.gcs->shadowGC;
    p = z;
  } else
  { topGC = context.gcs->shadowGC;
    botGC = context.gcs->reliefGC;
    p = -z;
  }

  cx = (x1 + x2 + x3)/3;
  cy = (y1 + y2 + y3)/3;

  for( i = 0 ; p > 0; p--, i += 3 )
  { int i0=0;
    int i1=i0+1;
    int i2=i0+2;

    s[i0].x1=X(x1);    s[i0].y1=Y(y1);    s[i0].x2=X(x2);    s[i0].y2=Y(y2);
    s[i1].x1=s[i0].x2; s[i1].y1=s[i0].y2; s[i1].x2=X(x3);    s[i1].y2=Y(y3);
    s[i2].x1=s[i1].x2; s[i2].y1=s[i1].y2; s[i2].x2=s[i0].x1; s[i2].y2=s[i0].y1;

    for(n=0; n<3;)
    { int f = n;
      int light = map & (1<<n);
      
      do
      { n++;
      } while(n < 3 &&
	      ((light && (map & (1<<n))) ||
	       (!light && !(map & (1<<n)))));

      XDrawSegments(context.display, context.drawable,
		    light ? topGC : botGC, &s[f], n-f);
    }

    step_to(&x1, &y1, cx, cy);
    step_to(&x2, &y2, cx, cy);
    step_to(&x3, &y3, cx, cy);
  }

  if ( r_elevation_fillpattern(e, up) )
    r_fill_triangle(x1, y1, x2, y2, x3, y3);
}


void
r_3d_diamond(int x, int y, int w, int h, Elevation e, int up)
{ GC topGC, botGC;
  int z = valInt(e->height);
  int nox, noy, wex, wey, sox, soy, eax, eay;

  r_elevation(e);
  r_thickness(1);

  if ( !up )
    z = -z;

  if ( z > 0 )
  { topGC = context.gcs->reliefGC;
    botGC = context.gcs->shadowGC;
  } else
  { topGC = context.gcs->shadowGC;
    botGC = context.gcs->reliefGC;
    z = -z;
  }

  z = (z*3)/2;				/* actually sqrt(2) */

  DEBUG(NAME_3dDiamond,
	Cprintf("r_3d_diamond(%d, %d, %d, %d, %s, %d) -->\n\t",
		x, y, w, h, pp(e), up));

  nox = X(x) + w/2; noy = Y(y);
  wex = X(x) + w;   wey = Y(y) + h/2;
  sox = nox;        soy = Y(y) + h;
  eax = X(x);       eay = wey;

  DEBUG(NAME_3dDiamond,
	Cprintf("(%d, %d) (%d, %d) (%d, %d) (%d, %d)\n",
		nox, noy, wex, wey, sox, soy, eax, eay));

  while( z > 0 )
  { XSegment s[4];

    s[0].x1 = eax; s[0].y1 = eay; s[0].x2 = nox; s[0].y2 = noy;
    s[1].x1 = nox; s[1].y1 = noy; s[1].x2 = wex; s[1].y2 = wey;
    s[2].x1 = wex; s[2].y1 = wey; s[2].x2 = sox; s[2].y2 = soy;
    s[3].x1 = sox; s[3].y1 = soy; s[3].x2 = eax; s[3].y2 = eay;

    XDrawSegments(context.display, context.drawable, topGC, s,     2);
    XDrawSegments(context.display, context.drawable, botGC, &s[2], 2);

    if ( --z > 0 )
    { noy++;
      soy--;
      wex--;
      eax++;
    } else
    { eax++;				/* ??? */
    }
  }
      
  if ( r_elevation_fillpattern(e, up) )
  { XPoint p[4];

    p[0].x = wex; p[0].y = wey;
    p[1].x = nox; p[1].y = noy;
    p[2].x = eax; p[2].y = eay;
    p[3].x = sox; p[3].y = soy;

    XFillPolygon(context.display, context.drawable, context.gcs->fillGC,
		 p, 4, Convex, CoordModeOrigin);
  }
}


void
r_arc(int x, int y, int w, int h, int s, int e, Any fill)
{ int pen = context.gcs->pen;
  int drawpen;
  int oldpen = pen;
  int mwh2;

  Translate(x, y);
  NormaliseArea(x, y, w, h);
  mwh2 = min(w, h)/2;

  if ( pen > mwh2 )
  { pen = mwh2;
    if ( pen == 0 )
      return;
  }

  drawpen = ((context.gcs->dash == NAME_none) && quick ? 1 : pen);
  x += drawpen/2;
  y += drawpen/2;
  w -= drawpen;
  h -= drawpen;

  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_background);
    XFillArc(context.display, context.drawable, context.gcs->fillGC,
	     x, y, w, h, s, e);
  }

  if ( fill != BLACK_IMAGE )
  { int done;
    r_thickness(drawpen);
    
    for( done = 0; done < pen; done += drawpen )
    { XDrawArc(context.display, context.drawable, context.gcs->workGC,
	       x, y, w, h, s, e);
      x += drawpen;
      y += drawpen;
      w -= 2*drawpen;
      h -= 2*drawpen;
    }
  }

  if ( oldpen != drawpen )
    r_thickness(oldpen);
}


void
r_ellipse(int x, int y, int w, int h, Any fill)
{ r_arc(x, y, w, h, 0, 360*64, fill);
}


void
r_3d_ellipse(int x, int y, int w, int h, Elevation z, int up)
{ int shadow;

  if ( !z || isNil(z) )
    r_ellipse(x, y, w, h, NIL);
  
  shadow = valInt(z->height);
  if ( !up )
    shadow = -shadow;
  
  if ( shadow > MAX_SHADOW )
    shadow = MAX_SHADOW;

  if ( shadow )
  { GC TopLeftGC, BottomRightGC;
    int xt=x, yt=y;
    XArc a[MAX_SHADOW*2];
    int an, os;

    r_elevation(z);

    if ( shadow > 0 )
    { TopLeftGC     = context.gcs->reliefGC;
      BottomRightGC = context.gcs->shadowGC;
    } else
    { TopLeftGC     = context.gcs->shadowGC;
      BottomRightGC = context.gcs->reliefGC;
      shadow        = -shadow;
    }

    Translate(xt, yt);
    for(an=0, os=0; os<shadow && w>=1 && h>=1; os++)
    { a[an].x = xt+os; a[an].y = yt+os;
      a[an].width = w-2*os; a[an].height = h-2*os;
      a[an].angle1 = 45*64; a[an].angle2 = 180*64;
      an++;
    }
    XDrawArcs(context.display, context.drawable, TopLeftGC, a, an);
    for(an=0, os=0; os<shadow && w>=1 && h>=1; os++)
    { a[an].x = xt+os; a[an].y = yt+os;
      a[an].width = w-2*os; a[an].height = h-2*os;
      a[an].angle1 = 225*64; a[an].angle2 = 180*64;
      an++;
    }
    XDrawArcs(context.display, context.drawable, BottomRightGC, a, an);
  }

  if ( r_elevation_fillpattern(z, up) )
  { r_thickness(0);
    r_arc(x+shadow, y+shadow, w-2*shadow, h-2*shadow, 0, 360*64, NAME_current);
  }
}


void
r_line(int x1, int y1, int x2, int y2)
{ Translate(x1, y1);
  Translate(x2, y2);

  { int p = context.gcs->pen;		/* check whether the line is */
    int x = x1;				/* in the painted area */
    int y = y1;
    int w = x2-x1;
    int h = y2-y1;

    NormaliseArea(x, y, w, h);
    x -= p;
    y -= p;
    p *= 2;
    w += p;
    h += p;
    Clip(x, y, w, h);
    if ( w == 0 || h == 0 )
      return;
  }

  XDrawLine(context.display, context.drawable, context.gcs->workGC,
	    x1, y1, x2, y2);
}


void
r_polygon(IPoint pts, int n, int close)
{ if ( context.gcs->pen > 0 )
  { XPoint *points = (XPoint *)alloca(n * sizeof(XPoint));
    int i;

    for(i=0; i<n; i++)
    { points[i].x = X(pts[i].x);
      points[i].y = Y(pts[i].y);
    }
  
    if ( close )
    { points[i].x = points[0].x;
      points[i].y = points[0].y;
      i++;
    }

    XDrawLines(context.display, context.drawable, context.gcs->workGC,
	       points, i, CoordModeOrigin);
  }
}


void
r_path(Chain points, int ox, int oy, int radius, int closed, Image fill)
{ Cell cell;
  int npoints = valInt(getSizeChain(points));

  if ( npoints < 2 )
    return;

  if ( radius == 0 )
  { XPoint *pts = (XPoint *)alloca((npoints+1) * sizeof(XPoint));
    int i=0;

    for_cell(cell, points)
    { Point p = cell->value;
      pts[i].x = valInt(p->x) + ox; pts[i].x = X(pts[i].x);
      pts[i].y = valInt(p->y) + oy; pts[i].y = Y(pts[i].y);
      i++;
    }
    if ( closed || notNil(fill) )
    { Point p = (Point) points->head->value;
      pts[i].x = valInt(p->x) + ox; pts[i].x = X(pts[i].x);
      pts[i].y = valInt(p->y) + oy; pts[i].y = Y(pts[i].y);
      i++;
    }

    if ( notNil(fill) )
    { r_fillpattern(fill, NAME_background);
      XFillPolygon(context.display, context.drawable, context.gcs->fillGC,
		   pts, i, Complex, CoordModeOrigin);  
    }

    if ( context.gcs->pen )
      XDrawLines(context.display, context.drawable, context.gcs->workGC,
		 pts, i, CoordModeOrigin);
  } else
  { 
#if 0					/* TBD */
    XSegment *sgs = (XSegment *) alloca((npoints-1) * sizeof(XSegment));
    int pt = 0;
    int seg = 0;
    int x1, x2, x3;
    int y1, y2, y3;

    for_cell(cell, points)
    { Point p = cell->value;
      int x = X(valInt(p->x));
      int y = Y(valInt(p->y));

      if ( pt == 0 )
      { x1 = x;  y1 = y;
      } else if ( pt == 1 )
      { x2 = x1; y2 = y1;
	x1 = x;  y1 = y;
      } else
      { x3 = x2; y3 = y2;
        x2 = x1; y2 = y1;
	x1 = x;  y1 = y;	
	sgs[seg].x1 = x3; sgs[seg].y1 = y3;
	/* to be continued */
      }     	
	
      pt++;
    }
#endif
    Cprintf("Not yet implemented (r_path())\n");
  }
}


void
r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op)
{ NormaliseArea(x, y, w, h);
  Translate(x, y);
  if ( env->area.x > x ) sx += env->area.x - x;
  if ( env->area.y > y ) sy += env->area.y - y;
  Clip(x, y, w, h);

  if ( w > 0 && h > 0 )
  { GC opgc;
    Pixmap pix = (Pixmap) getXrefObject(image, context.pceDisplay);

    if ( equalName(op, NAME_copy) )
      opgc = context.gcs->copyGC;
    else
    { opgc = context.gcs->opGC;

      if ( equalName(op, NAME_or) )
      { DisplayWsXref r = context.pceDisplay->ws_ref;

	if ( context.kind != NAME_bitmap &&
	     r->black_pixel == 0 )
	  XSetFunction(context.display, opgc, GXand);
        else
	  XSetFunction(context.display, opgc, GXor);
      } else if ( equalName(op, NAME_and) )
	opgc = context.gcs->andGC;
      else /*if ( equalName(op, NAME_xor) )*/
	XSetFunction(context.display, opgc, GXxor);
    }

    XCopyArea(context.display, pix, context.drawable, opgc,
	      sx, sy, w, h, x, y);
  }
}


void
r_image(Image image,
	int sx, int sy,
	int x, int y, int w, int h,
	Bool transparent)
{ XGCValues values;
    
  if ( image->size->w == ZERO || image->size->h == ZERO )
    return;

  DEBUG(NAME_image, Cprintf("image <-kind %s on drawable kind %s\n",
			    pp(image->kind), pp(context.kind)));

  if ( (image->kind == NAME_bitmap && context.kind != NAME_bitmap) ||
       (image->kind != NAME_bitmap && context.kind == NAME_bitmap) )
  { NormaliseArea(x, y, w, h);
    Translate(x, y);
    if ( image->kind != NAME_bitmap )
    { if ( env->area.x > x ) sx += env->area.x - x;
      if ( env->area.y > y ) sy += env->area.y - y;
    }
    values.ts_x_origin = x-sx;
    values.ts_y_origin = y-sy;

    if ( notNil(image->mask) )
    { values.clip_mask = (Pixmap)getXrefObject(image->mask,
					       context.pceDisplay);
      values.clip_x_origin = x;
      values.clip_y_origin = y;
    } else
      values.clip_mask = None;

    Clip(x, y, w, h);

    if ( w > 0 && h > 0 )
    { Pixmap pix = (Pixmap) getXrefObject(image, context.pceDisplay);

      if ( image->kind == NAME_bitmap )	/* bitmap on pixmap */
      { if ( instanceOfObject(context.gcs->colour, ClassColour) )
	{ values.foreground  = context.gcs->foreground_pixel;
	  values.background  = context.gcs->background_pixel;
	  values.stipple     = pix;
	  values.fill_style  = (transparent == ON ? FillStippled
			      			  : FillOpaqueStippled);

	  XChangeGC(context.display, context.gcs->bitmapGC,
		    GCClipXOrigin|GCClipYOrigin|GCClipMask|
		    GCForeground|GCBackground|GCFillStyle|
		    GCStipple|GCTileStipXOrigin|GCTileStipYOrigin,
		    &values);

	  XFillRectangle(context.display, context.drawable,
			 context.gcs->bitmapGC,
			 x, y, w, h);
	} else /* bitmap on pixmap, pixmap colour */
	{ DisplayWsXref r = context.pceDisplay->ws_ref;
	  Pixmap tmp = XCreatePixmap(context.display,
				     XtWindow(r->shell_xref),
				     w, h, context.depth);
	  GC GCtmp;

	  if ( !tmp )
	    return;

	  r_fillpattern(context.gcs->colour, NAME_foreground);
	  XFillRectangle(context.display, tmp, context.gcs->fillGC,
			 0, 0, w, h);

	  values.clip_x_origin = values.ts_x_origin;
	  values.clip_y_origin = values.ts_y_origin;
	  values.clip_mask     = pix;

	  GCtmp = XCreateGC(context.display, context.drawable,
			    GCClipXOrigin|GCClipYOrigin|GCClipMask|
			    GCClipXOrigin|GCClipYOrigin|GCClipMask,
			    &values);

	  XCopyArea(context.display, tmp, context.drawable,
		    GCtmp, 0, 0,
		    w, h, x, y);
	  XFreePixmap(context.display, tmp);
	  XFreeGC(context.display, GCtmp);
	}
      } else				/* pixmap on bitmap */
      { unsigned long fpixel, bpixel;
	unsigned long plane = 1L;
	int i;
	
	if ( isDefault(image->foreground) )
	  assign(image, foreground, context.pceDisplay->foreground);
	if ( isDefault(image->background) )
	  assign(image, background, context.pceDisplay->background);

	fpixel = getPixelColour(image->foreground, context.pceDisplay);
	bpixel = getPixelColour(image->background, context.pceDisplay);

	if ( fpixel != bpixel )
	{ for(i=1; i++ <= (sizeof(unsigned long) * 8); plane <<= 1)
	  { if ( (fpixel & plane) != (bpixel & plane) )
	      break;
	  }
	}

        DEBUG(NAME_image, Cprintf("fpixel = %ld, bpixel = %ld, plane = %ld\n",
				  fpixel, bpixel, plane));

        if ( (fpixel & plane) == 0 )
	{ values.foreground = 0;
	  values.background = 1;
	  XChangeGC(context.display, context.gcs->copyGC,
		    GCClipXOrigin|GCClipYOrigin|GCClipMask|
		    GCForeground|GCBackground,
		    &values);
	}

	XCopyPlane(context.display,
		   (Pixmap) getXrefObject(image, context.pceDisplay),
		   context.drawable,
		   context.gcs->copyGC,
		   sx, sy, w, h, x, y, plane);

        if ( (fpixel & plane) == 0 )
	{ values.foreground = 1;
	  values.background = 0;
	  values.clip_mask = None;
	  XChangeGC(context.display, context.gcs->copyGC,
		    GCClipMask|GCForeground|GCBackground,
		    &values);
	}
      }
    }
  } else if ( transparent == ON && image->kind == NAME_bitmap )
  { r_op_image(image, sx, sy, x, y, w, h, NAME_or);
  } else
  { Pixmap pm = (Pixmap)getXrefObject(image, context.pceDisplay);

    if ( notNil(image->mask) )
    { values.clip_mask = (Pixmap)getXrefObject(image->mask,
					       context.pceDisplay);
      values.clip_x_origin = X(x);
      values.clip_y_origin = Y(y);
      XChangeGC(context.display, context.gcs->copyGC,
		GCClipXOrigin|GCClipYOrigin|GCClipMask, &values);
    }

    NormaliseArea(x, y, w, h);
    Translate(x, y);
    if ( env->area.x > x ) sx += env->area.x - x;
    if ( env->area.y > y ) sy += env->area.y - y;
    Clip(x, y, w, h);

    if ( w > 0 && h > 0 )
      XCopyArea(context.display, pm, context.drawable, context.gcs->copyGC,
		sx, sy, w, h, x, y);

    if ( notNil(image->mask) )
    { values.clip_mask = None;
      values.clip_x_origin = 0;
      values.clip_y_origin = 0;
      XChangeGC(context.display, context.gcs->copyGC,
		GCClipXOrigin|GCClipYOrigin|GCClipMask, &values);
    }
  }
}


void
r_fill(int x, int y, int w, int h, Any pattern)
{ Translate(x, y);
  Clip(x, y, w, h);
  if ( w > 0 && h > 0 )
  { r_fillpattern(pattern, NAME_foreground);
    XFillRectangle(context.display, context.drawable, context.gcs->fillGC,
		   x, y, w, h);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: this function only fills polygons up to 20 points that are convex.
This is all we need sofar (cursors and arrow heads).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
r_fill_polygon(IPoint pts, int n)
{ XPoint *points = (XPoint *) alloca(n * sizeof(XPoint));
  int i;

  for(i=0; i<n; i++)
  { points[i].x = X(pts[i].x);
    points[i].y = Y(pts[i].y);
  }
  
  XFillPolygon(context.display, context.drawable, context.gcs->fillGC,
	       points, n, Convex, CoordModeOrigin);  
}


void
r_caret(int cx, int cy, FontObj font)
{ int ch, cb, ah, cw2;
  int cw = valInt(getExFont(font));
  ipoint pts[3];

  if ( cw < 4 )
    cw = 4;
  else if ( cw > 10 )
    cw = 10;

  ch = valInt(getHeightFont(font));
  cw2 = cw/2;
  cb = cy + ch-1;
  ah = (ch+2)/3;

  r_thickness(1);
  r_dash(NAME_none);
  r_line(cx, cb-2, cx, cb-ch);
    
  pts[0].x = cx - cw2;
  pts[0].y = cb;
  pts[1].x = cx + cw2;
  pts[1].y = cb;
  pts[2].x = cx;
  pts[2].y = cb-ah;

  r_fillpattern(BLACK_IMAGE, NAME_foreground);
  r_fill_polygon(pts, 3);
}



void
r_fill_triangle(int x1, int y1, int x2, int y2, int x3, int y3)
{ XPoint p[3];

  p[0].x = X(x1);
  p[0].y = Y(y1);
  p[1].x = X(x2);
  p[1].y = Y(y2);
  p[2].x = X(x3);
  p[2].y = Y(y3);

  XFillPolygon(context.display, context.drawable, context.gcs->fillGC,
	       p, 3, Convex, CoordModeOrigin);  
}


void
r_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Any fill)
{ XSegment s[3];

  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_foreground);
    r_fill_triangle(x1, y1, x2, y2, x3, y3);
  }
  
  s[0].x1 = X(x1);   s[0].y1 = Y(y1);   s[0].x2 = X(x2);   s[0].y2 = Y(y2);
  s[1].x1 = s[0].x2; s[1].y1 = s[0].y2; s[1].x2 = X(x3);   s[1].y2 = Y(y3);
  s[2].x1 = s[1].x2; s[2].y1 = s[1].y2; s[2].x2 = s[0].x1; s[2].y2 = s[0].y1;

  XDrawSegments(context.display, context.drawable, context.gcs->workGC,
		s, 3);
}


void
r_pixel(int x, int y, Any val)
{ Translate(x, y);
  if ( in_clip(x, y) )
  { if ( context.kind == NAME_bitmap )
    { if ( isOn(val) )
    	XDrawPoint(context.display, context.drawable, context.gcs->workGC,
		   x, y);
      else
      	XDrawPoint(context.display, context.drawable, context.gcs->clearGC,
		   x, y);
    } else
    { r_colour(val);
      XDrawPoint(context.display, context.drawable, context.gcs->workGC, x, y);
    }
  }
}


void
r_complement_pixel(int x, int y)
{ Translate(x, y);
  if ( in_clip(x, y) )
    XDrawPoint(context.display, context.drawable, context.gcs->complementGC,
	       x, y);
}


		/********************************
		*         READ DRAWABLES	*
		********************************/

static Drawable  last_drawable = 0;
  
void
d_modify()
{ if ( last_drawable == context.drawable )
    last_drawable = 0;			/* discard the cache */
}


int
r_get_mono_pixel(int x, int y)
{ return r_get_pixel(x, y) == context.gcs->background_pixel ? FALSE : TRUE;
}


unsigned long
r_get_pixel(int x, int y)
{ static Display  *last_display  = NULL;
  static XImage   *image         = NULL;
  static int ix, iy, iw, ih;		/* Image coordinates */
  static int dw, dh;			/* Preferred width/height */
  int move = FALSE;

  Translate(x, y);
  if ( in_clip(x, y) )
  { if ( last_drawable != context.drawable || last_display != context.display )
    { last_drawable = context.drawable;
      last_display = context.display;
      dw = dh = 8;
      ix = iy = iw = ih = 0;
      move = TRUE;
    }
    
    if ( x < ix )     { move = TRUE; dw *= 2; ix = x - dw - 1; }
    if ( x >= ix+iw ) { move = TRUE; dw *= 2; ix = x; }
    if ( y < iy )     { move = TRUE; dh *= 2; iy = y - dh - 1; }
    if ( y >= iy+ih ) { move = TRUE; dh *= 2; iy = y; }
    
    if ( move )
    { if ( image != NULL )
        XDestroyImage(image);

      iw = dw; ih = dh;
      Clip(ix, iy, iw, ih);
      image = XGetImage(last_display, last_drawable,
			ix, iy, iw, ih, AllPlanes, ZPixmap);
    }
    
    return XGetPixel(image, x-ix, y-iy);
  }
  
  return NoPixel;
}


		/********************************
		*       TEXT MANIPULATION	*
		********************************/

#define MAX_CHAR 256
static void s_font(FontObj f);

static XCharStruct *
s_char_struct(XFontStruct *info, int c)
{ if ( info->per_char == NULL )
    return &info->max_bounds;
  else if ( info->min_byte1 == 0 && info->max_byte1 == 0 ) /* 8-bit */
  { int b2 = c % 256;

    if ( b2 < info->min_char_or_byte2 || b2 > info->max_char_or_byte2 )
      return NULL;

    return &info->per_char[b2 - info->min_char_or_byte2];
  } else				/* 16-bit font */
  { int b2 = c % 256;
    int b1 = c / 256;

    if ( b2 < info->min_char_or_byte2 || b2 > info->max_char_or_byte2 ||
	 b1 < info->min_byte1 || b1 > info->max_byte1 )
      return NULL;
    else
    { int cols = info->max_char_or_byte2 - info->min_char_or_byte2 + 1;

      return &info->per_char[((b1-info->min_byte1)*cols) +
			     b2 - info->min_char_or_byte2];
    }
  }
}


int
s_has_char(FontObj f, unsigned int c)
{ XFontStruct *info;
  XCharStruct *def;

  s_font(f);
  info = context.gcs->font_info;
  if ( (def = s_char_struct(info, c)) && def->width != 0 )
    succeed;

  fail;
}


void
f_domain(FontObj f, Name which, int *x, int *y)
{ XFontStruct *info;

  s_font(f);
  info = context.gcs->font_info;
  if ( which == NAME_x )
  { *x = info->min_char_or_byte2;
    *y = info->max_char_or_byte2;
  } else
  { *x = info->min_byte1;
    *y = info->max_byte1;
  }
}


int
s_default_char(FontObj font)
{ s_font(font);

  return context.gcs->font_info->default_char;
}


static void
s_font(FontObj f)
{ d_ensure_display();

  if ( context.gcs->font != f )
  { XpceFontInfo info;
    context.gcs->font = f;

    info = (XpceFontInfo) getXrefObject(f, context.pceDisplay);
    context.gcs->font_info = info->info;
    context.gcs->char_widths = info->widths;

    XSetFont(context.display, context.gcs->workGC,
	     context.gcs->font_info->fid);
  }
}


int
s_ascent(FontObj f)
{ s_font(f);

  return context.gcs->font_info->ascent;
}


int
s_descent(FontObj f)
{ s_font(f);

  return context.gcs->font_info->descent;
}


int
s_height(FontObj f)
{ s_font(f);

  return context.gcs->font_info->ascent + context.gcs->font_info->descent;
}


static int
lbearing(unsigned int c, XFontStruct *info)
{ XCharStruct *def = s_char_struct(info, c);

  return def ? -def->lbearing : 0;
}


#if 0
static int
rbearing(unsigned int c, XFontStruct *info)
{ XCharStruct *def = s_char_struct(info, c);

  return def ? def->rbearing : info->max_bounds.width;
}
#endif


int
c_width(unsigned int c, FontObj font)
{ s_font(font);

  if ( context.gcs->char_widths )
    return context.gcs->char_widths[c];

  return context.gcs->font_info->max_bounds.width;
}


String
str_bits_as_font(String s, FontObj f, int *shift)
{ static string s2;

  Bool b16 = getB16Font(f);
  if ( b16 == ON && isstr8(s) )
  { s2 = *s;
    s2.b16 = TRUE;
    s2.size /= 2;
    if ( shift )
      *shift = -1;
    return &s2;
  } else if ( b16 != ON && !isstr8(s) )
  { s2 = *s;
    s2.b16 = FALSE;
    s2.size *= 2;
    if ( shift )
      *shift = 1;
    return &s2;
  }
  
  if ( shift )
    *shift = 0;
  return s;
}


static int
s_width_(String s, int from, int to)
{ if ( !context.gcs->char_widths )
  { return context.gcs->font_info->max_bounds.width * (to-from);
  } else
  { cwidth *widths = context.gcs->char_widths;
    int width;
    int n = to-from;

    if ( from >= to )
      return 0;

    if ( isstr8(s) )
    { char8 *q = &s->s_text8[from];

      width = lbearing(*q, context.gcs->font_info);
#if 0
      for(; n-- > 1; q++)
	width += widths[*q];
      width += rbearing(*q, context.gcs->font_info);
#else
      for(; n-- > 0; q++)
	width += widths[*q];
#endif
    } else
    { char16 *q = &s->s_text16[from];

      width = lbearing(*q, context.gcs->font_info);
#if 0
      for(; n-- > 1; q++)
	width += widths[*q];
      width += rbearing(*q, context.gcs->font_info);
#else
      for(; n-- > 0; q++)
	width += widths[*q];
#endif
    }

    return width;
  }
}


int
str_width(String s, int from, int to, FontObj f)
{ string s2;

  s_font(f);

  if ( f->b16 == ON && isstr8(s) )
  { s2 = *s;
    s2.b16 = TRUE;
    s2.size /= 2;
    from /= 2;
    to /= 2;
    s = &s2;
  } else if ( f->b16 != ON && !isstr8(s) )
  { s2 = *s;
    s2.b16 = FALSE;
    s2.size *= 2;
    from *= 2;
    to *= 2;
    s = &s2;
  }

  if ( from < 0 )
    from = 0;
  if ( from >= s->size || to <= from )
    return 0;
  if ( to > s->size )
    to = s->size;

  return s_width_(s, from, to);
}


int
str_advance(String s, int from, int to, FontObj f)
{ if ( f ) 
    s_font(f);

  if ( !context.gcs->char_widths )
  { return context.gcs->font_info->max_bounds.width * (to-from);
  } else
  { cwidth *widths = context.gcs->char_widths;
    int width = 0;
    int n = to-from;

    if ( isstr8(s) )
    { char8 *q = &s->s_text8[from];

      for(; n-- > 0; q++)
	width += widths[*q];
    } else
    { char16 *q = &s->s_text16[from];

      for(; n-- > 0; q++)
	width += widths[*q];
    }

    return width;
  }
}


void
s_print8(char8 *s, int l, int x, int y, FontObj f)
{ if ( l > 0 )
  { Translate(x, y);
    s_font(f);
    XDrawString(context.display, context.drawable, context.gcs->workGC,
		x, y, s, l);
  }
}


void
s_print16(char16 *s, int l, int x, int y, FontObj f)
{ if ( l > 0 )
  { Translate(x, y);
    s_font(f);
    XDrawString16(context.display, context.drawable, context.gcs->workGC,
		  x, y, (XChar2b *)s, l);
  }
}


void
s_print(String s, int x, int y, FontObj f)
{ if ( isstr8(s) )
    s_print8(s->s_text8, s->size, x, y, f);
  else
    s_print16(s->s_text16, s->size, x, y, f);
}


void
s_print_aligned(String s, int x, int y, FontObj f)
{ if ( s->size > 0 )
  { s_font(f);
    
    x += lbearing(str_fetch(s, 0), context.gcs->font_info);
    Translate(x, y);

    XDrawString(context.display, context.drawable, context.gcs->workGC,
		x, y, s->s_text8, s->size);
  } 
}


static void
str_stext(String s, int f, int len, int x, int y, Style style)
{ if ( len > 0 )
  { string s2;
    Any ofg = NULL;
    int w = 0;				/* make compiler happy */

    if ( notNil(style) )
    { w = str_advance(s, f, f+len, NULL);

      if ( notDefault(style->background) )
      { int a = context.gcs->font_info->ascent;
	int b = context.gcs->font_info->descent;
	
	r_fillpattern(style->background, NAME_foreground);
	XFillRectangle(context.display, context.drawable, context.gcs->fillGC,
		       x, y-a, w, b+a);
      }
      if ( notDefault(style->colour) )
	ofg = r_colour(style->colour);
    }

    if ( context.gcs->font->b16 == ON )
    { if ( isstr8(s) )
      { s2 = *s;
	s2.size /= 2;
	s2.b16 = TRUE;
	s = &s2;
	len /= 2;
	f /= 2;
      }
      XDrawString16(context.display, context.drawable, context.gcs->workGC,
		    x, y, (XChar2b *)s->s_text16+f, len);
    } else
    { if ( isstr16(s) )
      { s2 = *s;
	s2.size *= 2;
	s2.b16 = FALSE;
	s = &s2;
	len *= 2;
	f *= 2;
      }
      XDrawString(context.display, context.drawable, context.gcs->workGC,
		  x, y, s->s_text8+f, len);
    }

    if (  notNil(style) && style->attributes & TXT_UNDERLINED )
      XDrawLine(context.display, context.drawable, context.gcs->workGC,
		x, y, x+w, y);

    if ( ofg )
      r_colour(ofg);
  }
}


static void
str_text(String s, int x, int y)
{ if ( s->size > 0 )
  { string s2;

    if ( context.gcs->font->b16 == ON )
    { if ( isstr8(s) )
      { s2 = *s;
	s2.size /= 2;
	s2.b16 = TRUE;
	s = &s2;
      }
      x += lbearing(str_fetch(s, 0), context.gcs->font_info);
      XDrawString16(context.display, context.drawable, context.gcs->workGC,
		    x, y, (XChar2b *)s->s_text16, s->size);
    } else
    { if ( isstr16(s) )
      { s2 = *s;
	s2.size *= 2;
	s2.b16 = FALSE;
	s = &s2;
      }
      x += lbearing(str_fetch(s, 0), context.gcs->font_info);
      XDrawString(context.display, context.drawable, context.gcs->workGC,
		  x, y, s->s_text8, s->size);
    }
  }
}


		/********************************
		*         MULTILINE TEXT	*
		********************************/

#define MAX_TEXT_LINES 200		/* lines in a text object */

typedef struct
{ short	x;				/* origin x offset */
  short	y;				/* origin y offset */
  short	width;				/* pixel width of line */
  short	height;				/* pixel height of line */
  string text;				/* text of the line */
} strTextLine;


static void
str_break_into_lines(String s, strTextLine *line, int *nlines)
{ int here = 0;
  int size = s->size;

  *nlines = 0;

  if ( size == 0 )			/* totally empty: report one line */
  { str_cphdr(&line->text, s);
    line->text.s_text = s->s_text;
    line->text.size = 0;
    *nlines = 1;
    return;
  }

  for( ; here < size; line++, (*nlines)++ )
  { int el;

    str_cphdr(&line->text, s);
    line->text.s_text = str_textp(s, here);

    if ( (el = str_next_index(s, here, '\n')) >= 0 )
    { line->text.size = el - here;
      here = el + 1;
      if ( here == size )		/* last char is newline: add a line */
      { line++, (*nlines)++;
	str_cphdr(&line->text, s);
	line->text.s_text = str_textp(s, here);
	line->text.size = 0;
      }
    } else
    { line->text.size = size - here;
      here = size;
    }
  }
}


static void
str_compute_lines(strTextLine *lines, int nlines, FontObj font,
		  int x, int y, int w, int h,
		  Name hadjust, Name vadjust)
{ int cy;
  int th = s_height(font);
  strTextLine *line;
  int n;

  if ( vadjust == NAME_top )
    cy = y;
  else if ( vadjust == NAME_center )
    cy = y + (h - nlines*th)/2;
  else /*if ( vadjust == NAME_bottom )*/
    cy = y + h - nlines*th;

  for( n = 0, line = lines; n++ < nlines; line++, cy += th )
  { line->y      = cy;
    line->height = th;
    line->width  = str_width(&line->text, 0, line->text.size, font);

    if ( hadjust == NAME_left )
      line->x = x;
    else if ( hadjust == NAME_center )
      line->x = x + (w - line->width)/2;
    else /*if ( hadjust == NAME_right )*/
      line->x = x + w - line->width;
  }
}


void
str_size(String s, FontObj font, int *width, int *height)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int w = 0;
  string s2;

  s_font(font);

  if ( font->b16 == ON && isstr8(s) )
  { s2 = *s;
    s2.b16 = TRUE;
    s2.size /= 2;
    s = &s2;
  } else if ( font->b16 != ON && !isstr8(s) )
  { s2 = *s;
    s2.b16 = FALSE;
    s2.size *= 2;
    s = &s2;
  }

  str_break_into_lines(s, lines, &nlines);
  for(n = 0, line = lines; n++ < nlines; line++)
  { int lw = s_width_(&line->text, 0, line->text.size);
    
    if ( w < lw )
      w = lw;
  }
  
  *width  = w;
  *height = nlines * s_height(font);
}


void
str_string(String s, FontObj font, int x, int y, int w, int h,
	   Name hadjust, Name vadjust, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;

  if ( s->size == 0 )
    return;

  Translate(x, y);
  s_font(font);
  baseline = context.gcs->font_info->ascent;
  str_break_into_lines(s, lines, &nlines);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  if ( flags & TXT_UNDERLINED )
  { r_dash(NAME_none);
    r_thickness(1);
  }

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(&line->text, line->x, line->y+baseline);
    if ( flags & TXT_UNDERLINED )
      XDrawLine(context.display, context.drawable, context.gcs->workGC,
		line->x, line->y+baseline+1,
		line->x+line->width, line->y+baseline+1);
  }
}


void
str_selected_string(String s, FontObj font,
		    int f, int t, Style style,	/* selection parameters */
		    int x, int y, int w, int h,
		    Name hadjust, Name vadjust)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;
  int here = 0;

  if ( s->size == 0 )
    return;

  Translate(x, y);
  s_font(font);
  baseline = context.gcs->font_info->ascent;
  str_break_into_lines(s, lines, &nlines);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  for(n=0, line = lines; n++ < nlines; line++)
  { int len = line->text.size;

    line->x += lbearing(str_fetch(&line->text, 0), context.gcs->font_info);

    if ( t <= here || f >= here+len )	/* outside */
      str_stext(&line->text, 0, len, line->x, line->y+baseline, NIL);
    else
    { int sf, sx, sl;

      sf = (f <= here     ?      0 : f-here);
      sl = (t >= here+len ? len-sf : t-here-sf);
      sx = str_advance(&line->text, 0, sf, NULL);
      
      str_stext(&line->text, 0,  sf, line->x,    line->y+baseline, NIL);
      str_stext(&line->text, sf, sl, line->x+sx, line->y+baseline, style);
      if ( sf+sl < len )
      { int a  = sf+sl;
	int ax = sx + str_advance(&line->text, sf, a, NULL);

	str_stext(&line->text, a, len-a, line->x+ax, line->y+baseline, NIL);
      }
    }

    here += len + 1;			/* 1 for the newline */
  }
}


		 /*******************************
		 *      POSTSCRIPT SUPPORT	*
		 *******************************/

void
ps_string(String s, FontObj font, int x, int y, int w, Name format, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;

  if ( s->size == 0 )
    return;

  s_font(font);
  ps_font(font);

  baseline = context.gcs->font_info->ascent;
  str_break_into_lines(s, lines, &nlines);
  str_compute_lines(lines, nlines, font, x, y, w, 0, format, NAME_top);

  for(n=0, line = lines; n++ < nlines; line++)
  { if ( line->text.size > 0 )
    { ps_output("~D ~D 0 ~D ~a text\n",
		line->x, line->y+baseline,
		line->width, &line->text);
      if ( flags & TXT_UNDERLINED )
      { ps_output("nodash 1 ~D ~D ~D ~D linepath draw\n",
		  line->x, line->y+baseline+2, line->width, 0);
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draws a string, just  like  str_string(),   but  underscores  the  first
character matching the accelerator (case-insensitive).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
str_draw_text_lines(int acc, FontObj font,
		    int nlines, strTextLine *lines,
		    int ox, int oy)
{ strTextLine *line;
  int n;
  int baseline = context.gcs->font_info->ascent;

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(&line->text, line->x+ox, line->y+baseline+oy);

    if ( acc )
    { int cx = line->x;
      int cn;

      cx += lbearing(str_fetch(&line->text, 0), context.gcs->font_info);

      for(cn=0; cn<line->text.size; cn++)
      { int c  = str_fetch(&line->text, cn);
	int cw = c_width(c, font);

	if ( tolower(c) == acc )
	{			/* not r_line to avoid double Translate() */
	  XDrawLine(context.display, context.drawable, context.gcs->workGC,
		    cx, line->y+baseline+1, cx+cw-2, line->y+baseline+1);
	  acc = 0;
	  break;
	}

	cx += cw;
      }
    }
  }
}


static Any
r_text_colour(Any c)
{ int oldfixed = context.fixed_colours;
  Any old;

  context.fixed_colours = 0;
  old = r_colour(c);
  context.fixed_colours = oldfixed;

  return old;
}


void
str_label(String s, int acc, FontObj font, int x, int y, int w, int h,
	   Name hadjust, Name vadjust, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  int nlines;

  if ( s->size == 0 )
    return;

  Translate(x, y);
  s_font(font);
  str_break_into_lines(s, lines, &nlines);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);
  if ( acc )
  { r_dash(NAME_none);
    r_thickness(1);
  }

  if ( flags & LABEL_INACTIVE )
  { if ( context.depth > 1 )
    { Any old = r_text_colour(WHITE_COLOUR);

      str_draw_text_lines(acc, font, nlines, lines, 1, 1);
      r_text_colour(ws_3d_grey());
      str_draw_text_lines(acc, font, nlines, lines, 0, 0);
      r_text_colour(old);
    } else
    { Any old = r_text_colour(GREY50_IMAGE);
      
      str_draw_text_lines(acc, font, nlines, lines, 0, 0);
      r_text_colour(old);
    }
  } else
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
}

