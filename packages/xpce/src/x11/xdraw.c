/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
r_fillpattern(image)		Set fill pattern
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
#include "include.h"

static void	clip_area P((int *, int *, int *, int *));
static void	r_andpattern(Image i);
static void	r_background(Any c);

		/********************************
		*       DEVICE FUNCTIONS	*
		********************************/

#define MAX_CLIP_NESTING	(50)
#define MAX_POLYGON_POINTS      (20)

static int quick;			/* display quick_and_dirty */

static struct environment
{ struct iarea	area;			/* clip rectangle */
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
  Any		default_foreground;	/* Default foreground colour */   
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
} context;

#define X(x) ((x) + context.offset_x)
#define Y(y) ((y) + context.offset_y)
#define Translate(x, y)	 { (x) = X(x); (y) = Y(y); }
#define Clip(x, y, w, h) clip_area(&x, &y, &w, &h)

void
resetDraw(void)
{ struct environment *e;
  int i;

  for(e=environments, i=0; i<MAX_CLIP_NESTING; i++, e++)
    e->level = i;

  env = environments;
}


static void
d_push_context(void)
{ DContext ctx = alloc(sizeof(struct d_context));
  
  *ctx = context;			/* structure copy! */
  context.parent = ctx;
}


static void
d_pop_context()
{ if ( context.parent != NULL )
  { DContext ctx = context.parent;

    context = *ctx;			/* structure copy! */
    unalloc(sizeof(struct d_context), ctx);
  }
}


void
d_offset(int x, int y)
{ DEBUG(NAME_redraw, printf("d_offset(%d, %d)\n", x, y));

  context.offset_x = x;
  context.offset_y = y;
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
	printf("d_window(%s, %d, %d, %d, %d)\n", pp(sw), x, y, w, h));

  if ( env->level != 0 )
    resetDraw();			/* security measure */

  d_push_context();
  d_display(d);

  context.default_foreground = sw->colour;
  context.default_background = sw->background;
  context.default_colour     = sw->colour;
  context.origin_x	     = context.offset_x;
  context.origin_y	     = context.offset_y;
  context.drawable	     = (Drawable) XtWindow(widgetWindow(sw));
  context.kind		     = NAME_window;

  r_background(sw->background);

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
	printf("area = (%d, %d, %d, %d) %s\n",
	       env->area.x, env->area.y, env->area.w, env->area.h,
	       clear ? "clear" : "no clear"));

  if ( limit && notNil(d->cache) && clear )
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

  { int tsx = context.origin_x-context.cache_x;
    int tsy = context.origin_y-context.cache_y;

    XSetTSOrigin(context.display, context.gcs->fillGC, tsx, tsy);
    XSetTSOrigin(context.display, context.gcs->clearGC, tsx, tsy);
    XSetTSOrigin(context.display, context.gcs->workGC, tsx, tsy);
  }

  d_clip(x, y, w, h);
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
    context.default_foreground = d->foreground;
  else
    context.default_foreground = i->foreground;
  if ( isDefault(i->background) )
    context.default_background = d->background;    
  else
    context.default_background = i->background;
  context.drawable	     = (Drawable) image;
  context.kind		     = i->kind;

  context.default_colour = context.default_foreground;

  if ( i->kind == NAME_pixmap )
    r_background(i->background);

  env++;
  env->area.x   = 0;
  env->area.y   = 0;
  env->area.w   = valInt(i->size->w);
  env->area.h   = valInt(i->size->h);

  XSetTSOrigin(context.display, context.gcs->fillGC, 0, 0);

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
  context.default_foreground = d->foreground;
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
  d_xwindow(d, root, 0, 0, iw, ih);
}


void
d_clip(int x, int y, int w, int h)
{ XRectangle rect;

  DEBUG(NAME_redraw, printf("d_clip(%d, %d, %d, %d) -> ", x, y, w, h));
  NormaliseArea(x, y, w, h);
  Translate(x, y);
  DEBUG(NAME_redraw, printf("(%d %d %d %d) -> ", x, y, w, h));
  Clip(x, y, w, h);
  DEBUG(NAME_redraw, printf("(%d %d %d %d)\n", x, y, w, h));

  env++;
  env->area.x = x;
  env->area.y = y;
  env->area.w = w;
  env->area.h = h;

  rect.x      = x;
  rect.y      = y;
  rect.width  = w;
  rect.height = h;

  DEBUG(NAME_redraw, printf("clip to %d %d %d %d\n", x, y, w, h));

# define CLIP(x) XSetClipRectangles(context.display, x, 0, 0, &rect, \
				    1, Unsorted)
  CLIP(context.gcs->workGC);
  CLIP(context.gcs->fillGC);
}


void
d_done()
{ if ( context.cache != NULL )
  { DEBUG(NAME_redraw, printf("writing cache to (%d %d %d %d)\n",
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
  d_pop_context();
  DEBUG(NAME_redraw, printf("After d_done(): env->level = %d\n", env->level));
}


void
d_clip_done(void)
{ env--;

  DEBUG(NAME_redraw, printf("d_done()\n"));

  assert(env >= environments);		/* stack underflow */

  if ( env->level != 0 )		/* outermost: no use */
  { XRectangle rect;

    rect.x      = env->area.x;
    rect.y      = env->area.y;
    rect.width  = env->area.w;
    rect.height = env->area.h;

    CLIP(context.gcs->workGC);
    CLIP(context.gcs->fillGC);
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
{ struct iarea a;

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
  { if ( context.kind == NAME_window )
      XClearArea(context.display, context.drawable, x, y, w, h, False);
    else
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
r_fillpattern(Any fill)		/* image or colour */
{ int pattern = instanceOfObject(fill, ClassImage);
  Image  i = (pattern ? (Image)fill : BLACK_IMAGE);
  Colour c = (pattern ? context.gcs->colour : (Colour) fill);
  Pixmap image;

  if ( (i != context.gcs->fill_pattern ||
	c != context.gcs->fill_colour) &&
       (image = (Pixmap) getXrefObject(i, context.pceDisplay)) )
  { XGCValues values;
    ulong mask;
    
    if ( context.kind != NAME_bitmap && i->kind == NAME_bitmap )
    { values.stipple    = image;
      values.fill_style = FillOpaqueStippled;
      mask 		= (GCStipple|GCFillStyle);
    } else
    { values.tile       = image;
      values.fill_style = FillTiled;
      mask		= (GCTile|GCFillStyle);
    }
    values.foreground = (pattern ? context.gcs->foreground_pixel
			 	 : getPixelColour(c, context.pceDisplay));
    mask |= GCForeground;

    XChangeGC(context.display, context.gcs->fillGC, mask, &values);

    context.gcs->fill_pattern = i;
    context.gcs->fill_colour = c;
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
    ulong mask;
    
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


Any
r_default_colour(Colour c)
{ Any old = context.default_colour;
  
  if ( notDefault(c) )
    context.default_colour = c;

  r_colour(context.default_colour);
  
  return old;
}


Colour
r_colour(Any c)
{ Colour old = context.gcs->colour;

  if ( isDefault(c) )
    c = context.default_colour;

  if ( c != context.gcs->colour )
  { if ( context.gcs->kind != NAME_bitmap )
    { XGCValues values;
      ulong mask;

      if ( instanceOfObject(c, ClassColour) )
      { ulong pixel = getPixelColour(c, context.pceDisplay);
	
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

      context.gcs->colour = c;
      XChangeGC(context.display, context.gcs->workGC, mask, &values);
      XChangeGC(context.display, context.gcs->fillGC, mask, &values);
    } 
    context.gcs->colour = c;
  }

  return old;
}


static void
r_background(Any c)
{ if ( isDefault(c) )
    c = context.default_background;

  if ( c != context.gcs->background )
  { if ( context.gcs->kind != NAME_bitmap )
    { XGCValues values;
      ulong mask;

      if ( instanceOfObject(c, ClassColour) )
      { ulong pixel = getPixelColour(c, context.pceDisplay);
	
	values.foreground = pixel;
	values.fill_style = FillSolid;
	mask		  = (GCForeground|GCFillStyle);

	context.gcs->background_pixel = pixel;
      } else
      { Pixmap pm   = (Pixmap) getXrefObject(c, context.pceDisplay);

	values.tile       = pm;
	values.fill_style = FillTiled;
	mask		  = (GCTile|GCFillStyle);
      }

      context.gcs->background = c;
      XChangeGC(context.display, context.gcs->clearGC, mask, &values);
    }
  }
}


void
r_swap_background_and_foreground()
{ if ( context.gcs->kind != NAME_bitmap )
  { Colour tc = context.gcs->background;

    r_background(context.gcs->colour);
    r_colour(tc);
  }
}


void
r_subwindow_mode(Bool val)
{ if ( context.gcs->subwindow_mode != val )
  { int mode = (val == ON ? IncludeInferiors : ClipByChildren);

    XSetSubwindowMode(context.display, context.gcs->workGC,   mode);
    XSetSubwindowMode(context.display, context.gcs->fillGC,   mode);
    XSetSubwindowMode(context.display, context.gcs->copyGC,   mode);
    XSetSubwindowMode(context.display, context.gcs->bitmapGC, mode);
    XSetSubwindowMode(context.display, context.gcs->andGC,    mode);
    XSetSubwindowMode(context.display, context.gcs->opGC,     mode);

    context.gcs->subwindow_mode = val;
  }
}


void
r_invert_mode(Bool val)
{ if ( context.gcs->invert_mode != val )
  { XGCValues values;
    int mask = GCFunction|GCPlaneMask;
    
    if ( val == ON )
    { values.function   = GXinvert;
      values.plane_mask = 1;
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
r_box(int x, int y, int w, int h, int r, Image fill)
{ int mwh, pen = context.gcs->pen;
  int drawpen = ((context.gcs->dash == NAME_none) && quick ? 1 : pen);

  Translate(x, y);
  r_thickness(drawpen);

  NormaliseArea(x, y, w, h);
  mwh = (w < h ? w : h);

  if ( mwh == 0 )
    return;

  if ( r > mwh / 2 - pen - 1)
    r = mwh / 2 - pen - 1;

  x += drawpen / 2;
  y += drawpen / 2;
  w -= drawpen;
  h -= drawpen;

  if ( notNil(fill) )
    r_fillpattern(fill);

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

void
r_3d_box(int x, int y, int w, int h, int shadow, Any fill, int up)
{ XSegment s[MAX_SHADOW * 2];
  int i;
  int pen = 1;
  int ios, os;
  GC TopLeftGC, BottomRightGC;
  int xt, yt;

  if ( up )
  { TopLeftGC     = context.gcs->reliefGC;
    BottomRightGC = context.gcs->shadowGC;
  } else
  { TopLeftGC     = context.gcs->shadowGC;
    BottomRightGC = context.gcs->reliefGC;
  }

  if ( shadow > MAX_SHADOW )
    shadow = MAX_SHADOW;

  xt = x, yt = y;
  Translate(xt, yt);

#if 0
  if ( context.gcs->depth == 1 )	/* monochrome */
  { XDrawRectangle(context.display, context.drawable, context.gcs->shadowGC,
		   xt, yt, w-1, h-1);
    ios = 1;
  } else
#endif
    ios = 0;

  for(i=0, os=ios; os < shadow; os += pen)
  { s[i].x1 = xt+os;		s[i].y1 = yt+os;	/* top-side */
    s[i].x2 = xt+w-1-os;	s[i].y2 = yt+os;
    i++;
    s[i].x1 = xt+os;		s[i].y1 = yt+os;	/* left-side */
    s[i].x2 = xt+os;		s[i].y2 = yt+h-1-os;
    i++;
  }
  XDrawSegments(context.display, context.drawable, TopLeftGC, s, i);
  
  for(i=0, os=ios; os < shadow; os += pen)
  { s[i].x1 = xt+os;		s[i].y1 = yt+h-1-os;	/* bottom-side */
    s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
    i++;
    s[i].x1 = xt+w-1-os;	s[i].y1 = yt+os;	/* right-side */
    s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
    i++;
  }
  XDrawSegments(context.display, context.drawable, BottomRightGC, s, i);
  
  if ( notNil(fill) )
    r_fill(x+shadow, y+shadow, w-2*shadow, h-2*shadow, fill);
}


void
r_arc(int x, int y, int w, int h, int s, int e, Image fill)
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
  { r_fillpattern(fill);
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
r_ellipse(int x, int y, int w, int h, Image fill)
{ r_arc(x, y, w, h, 0, 360*64, fill);
}


void
r_line(int x1, int y1, int x2, int y2)
{ Translate(x1, y1);
  Translate(x2, y2);

  XDrawLine(context.display, context.drawable, context.gcs->workGC,
	    x1, y1, x2, y2);
}


void
r_polygon(IPoint pts, int n, int close)
{ XPoint points[MAX_POLYGON_POINTS];
  int i;

  if ( n > MAX_POLYGON_POINTS-1 )
  { errorPce(NIL, NAME_polyTooManyPoints, toInt(MAX_POLYGON_POINTS));
    return;
  }

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
    { r_fillpattern(fill);
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
    printf("Not yet implemented (r_path())\n");
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
{ if ( image->size->w == ZERO || image->size->h == ZERO )
    return;

  DEBUG(NAME_image, printf("image <-kind %s on drawable kind %s\n",
			   pp(image->kind), pp(context.kind)));

  if ( (image->kind == NAME_bitmap && context.kind != NAME_bitmap) ||
       (image->kind != NAME_bitmap && context.kind == NAME_bitmap) )
  { XGCValues values;

    NormaliseArea(x, y, w, h);
    Translate(x, y);
    if ( image->kind != NAME_bitmap )
    { if ( env->area.x > x ) sx += env->area.x - x;
      if ( env->area.y > y ) sy += env->area.y - y;
    }
    values.ts_x_origin = x-sx;
    values.ts_y_origin = y-sy;
    Clip(x, y, w, h);

    if ( w > 0 && h > 0 )
    { Pixmap pix = (Pixmap) getXrefObject(image, context.pceDisplay);

      if ( image->kind == NAME_bitmap )	/* bitmap on pixmap */
      { values.foreground  = context.gcs->foreground_pixel;
	values.background  = context.gcs->background_pixel;
	values.stipple     = pix;
	values.fill_style  = (transparent == ON ? FillStippled
			      			: FillOpaqueStippled);

	XChangeGC(context.display, context.gcs->bitmapGC,
		  GCForeground|GCBackground|GCFillStyle|
		  GCStipple|GCTileStipXOrigin|GCTileStipYOrigin,
		  &values);

	XFillRectangle(context.display, context.drawable,
		       context.gcs->bitmapGC,
		       x, y, w, h);
      } else				/* pixmap on bitmap */
      { ulong fpixel = getPixelColour(image->foreground, context.pceDisplay);
	ulong bpixel = getPixelColour(image->background, context.pceDisplay);
	ulong plane = 1L;
	int i;

	if ( fpixel != bpixel )
	  for(i=1; i++ <= (sizeof(ulong) * 8); plane <<= 1)
	    if ( (fpixel & plane) != (bpixel & plane) )
	      break;

        DEBUG(NAME_image, printf("fpixel = %ld, bpixel = %ld, plane = %ld\n",
				 fpixel, bpixel, plane));

        if ( (fpixel & plane) == 0 )
	{ values.foreground = 0;
	  values.background = 1;
	  XChangeGC(context.display, context.gcs->copyGC,
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
	  XChangeGC(context.display, context.gcs->copyGC,
		    GCForeground|GCBackground,
		    &values);
	}
      }
    }
  } else if ( transparent == ON && image->kind == NAME_bitmap )
    r_op_image(image, sx, sy, x, y, w, h, NAME_or);
  else
    r_op_image(image, sx, sy, x, y, w, h, NAME_copy);
}


void
r_fill(int x, int y, int w, int h, Image pattern)
{ Translate(x, y);
  Clip(x, y, w, h);
  if ( w > 0 && h > 0 )
  { r_fillpattern(pattern);
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
{ XPoint points[MAX_POLYGON_POINTS];
  int i;

  if ( n > MAX_POLYGON_POINTS )
  { errorPce(NIL, NAME_polyTooManyPoints, toInt(MAX_POLYGON_POINTS));
    return;
  }

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
  struct ipoint pts[3];

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

  r_fillpattern(BLACK_IMAGE);
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
r_triangle(int x1, int y1, int x2, int y2, int x3, int y3)
{ XSegment s[3];

  r_fill_triangle(x1, y1, x2, y2, x3, y3);
  
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
			ix, iy, iw, ih, AllPlanes, XYPixmap);
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
{ if ( context.pceDisplay == NULL )	/* TBD */
    d_display(DEFAULT);

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


static inline int
s_width_(String s, int from, int to)
{ if ( !context.gcs->char_widths )
  { return context.gcs->font_info->max_bounds.width * (to-from);
  } else
  { cwidth *widths = context.gcs->char_widths;
    int width;
    int n = to-from;

    if ( isstr8(s) )
    { char8 *q = &s->s_text8[from];

      for(width = 0; n-- > 0; q++)
	width += widths[*q];
    } else
    { char16 *q = &s->s_text16[from];

      for(width = 0; n-- > 0; q++)
	width += widths[*q];
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


void
s_print8(char8 *s, int l, int x, int y, FontObj f)
{ if ( l > 0 )
  { s_font(f);
    XDrawString(context.display, context.drawable, context.gcs->workGC,
		x, y, s, l);
  }
}


void
s_print16(char16 *s, int l, int x, int y, FontObj f)
{ if ( l > 0 )
  { s_font(f);
    XDrawString16(context.display, context.drawable, context.gcs->workGC,
		  x, y, (XChar2b *)s, l);
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

  if ( equalName(vadjust, NAME_top) )
    cy = y;
  else if ( equalName(vadjust, NAME_center) )
    cy = y + (h - nlines*th)/2;
  else /*if ( equalName(vadjust, NAME_bottom) )*/
    cy = y + h - nlines*th;

  for( n = 0, line = lines; n++ < nlines; line++, cy += th )
  { line->y      = cy;
    line->height = th;
    line->width  = str_width(&line->text, 0, line->text.size, font);

    if ( equalName(hadjust, NAME_left) )
      line->x = x;
    else if ( equalName(hadjust, NAME_center) )
      line->x = x + (w - line->width)/2;
    else /*if ( equalName(hadjust, NAME_right) )*/
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
	   Name hadjust, Name vadjust)
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

  for(n=0, line = lines; n++ < nlines; line++)
    str_text(&line->text, line->x, line->y+baseline);
}

		 /*******************************
		 *      POSTSCRIPT SUPPORT	*
		 *******************************/

void
ps_string(String s, FontObj font, int x, int y, int w, Name format)
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
    ps_output("~D ~D 0 ~D ~a text\n",
	      line->x, line->y+baseline,
	      line->width, &line->text);
}


#if TBD
void
str_label(char8 *s, char8 acc, FontObj font, int x, int y, int w, int h,
	Name hadjust, Name vadjust)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;
  char8 *q;

  if ( s == NULL )
    s = "(null)";
  else if ( s[0] == EOS )
    return;

  Translate(x, y);
  s_font(font);
  baseline = context.gcs->font_info->ascent;
  str_break_into_lines(s, lines, &nlines);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  for(n=0, line = lines; n++ < nlines; line++)
    str_text(line->text, line->length, line->x, line->y+baseline);

  if ( acc && (q=strchr(s, acc)) )
  { int i = s - q;
    int cx, cw;
    int descent = context.gcs->font_info->descent;
    
    for(line = lines; i > line->length; line++)
      i -= line->length;

    cx = str_width(line->text, i, font);
    cw = c_width(acc, font);

    r_line(line->x + cx,      line->y + baseline + descent - 1,
	   line->x + cx + cw, line->y + baseline + descent - 1);
  }
}

#endif /*TBD*/
