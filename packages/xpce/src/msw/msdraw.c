/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <math.h>
#ifndef M_PI
#define M_PI (3.141593)
#endif

#define MAX_CLIP_DEPTH (5)		/* clip nesting depth */
#define MAX_CTX_DEPTH (10)		/* Max draw context depth */

typedef struct
{ HWND		hwnd;			/* current Windows window */
  HBITMAP	hbitmap;		/* current Image */
  PAINTSTRUCT	ps;			/* paint structure */
  HDC		hdc;			/* device context */
  HPEN		hpen;			/* Current created pen */
  HRGN		hrgn;			/* Current created region */
  HBRUSH	hbrush;			/* Currently selected brush */

  HPEN		ohpen;			/* Original pen */
  HRGN		ohrgn;			/* Original region */
  HBITMAP	ohbitmap;		/* Original bitmap */
  HBRUSH	ohbrush;		/* Original brush */
  int		stockpen;		/* Pen comes from GetStockObject() */
  HFONT		ohfont;			/* Original font */

  HBITMAP	cache;			/* background drawing */
  HDC		cached_hdc;		/* hdc of original device */
  int		cache_x;		/* X-corner of cache */
  int		cache_y;		/* Y-corner of cache */
  int		cache_w;		/* Width of cache */
  int		cache_h;		/* Height of cache */
  HBITMAP	cache_ohbitmap;		/* original bitmap handle */

  int		offset_x;		/* d_offset(), r_offset() */
  int		offset_y;		/* same */
  int		r_offset_x;		/* r_offset() */
  int		r_offset_y;		/* r_offset() */

  int		open;			/* is context opened? */

  Image		fill_pattern;		/* PCE fill-pattern image */
  Colour	colour;			/* Current colour */
  Colour	default_colour;		/* The default colour */
  Any		background;		/* Background colour */
  Any		default_background;	/* @default background */
  COLORREF	rgb;			/* RGB of colour */
  COLORREF	background_rgb;		/* RBG of background */
  int		thickness;		/* Current pen */
  Name		texture;		/* Current dash-pattern */
  int		modified_pen;		/* Pen is modified */
  FontObj	font;			/* Currently mounted font */
  WsFont	wsf;			/* Window System Font reference */
  Any		device;			/* XPCE device in use */
  DisplayObj	display;		/* The XPCE display */
  int		depth;			/* # bits/pixel */

  Elevation	elevation;		/* current elevation context */
  HPEN		relief_pen;		/* standing-edge pen */
  HPEN		shadow_pen;		/* falling edge pen */


  struct
  { RECT	orect;			/* old clipping rect */
  } clip_stack[MAX_CLIP_DEPTH];
  int		clip_depth;		/* #entries on clip stack */

} wdraw_context, *WDrawContext;

#ifdef __WIN32__
#define MoveTo(hdc, x, y) MoveToEx((hdc), (x), (y), NULL);
#define SetWindowOrg(hdc, x, y) SetWindowOrgEx((hdc), (x), (y), NULL);
#define SetViewportOrg(hdc, x, y) SetViewportOrgEx((hdc), (x), (y), NULL);
#endif /*__WIN32__*/

static int		cache = 1;	/* Do or don't */
static int		quick;		/* Prefer speed */
static wdraw_context	context; 	/* current context */
static wdraw_context	ctx_stack[MAX_CTX_DEPTH];  /* Context stack */
static int		ctx_stacked;	/* Saved frames */

static HDC		default_hdc;	/* Default context */
static HBITMAP		default_hdc_hbitmap; /* Memory for default_hdc */
static HBITMAP		default_hdc_ohbitmap; /* Original memory */
static DisplayObj	TheDisplay;	/* @display */
static int		display_depth;	/* depth of the display */

static void	r_update_pen(void);	/* Update the pen context */
static void	r_default_background(Any bg);
static COLORREF cref_colour(Colour c);
static void	push_context(void);
static void	empty_brush_cache(void);
static void	make_default_context(void);

#include <gra/graphstate.c>

static void
reset_context()
{ context.fill_pattern       = WHITE_IMAGE;
  context.font               = NIL;
  context.thickness          = 1;
  context.texture            = NAME_none;
  context.hbrush	     = 0;
  context.ohbrush	     = 0;
  context.ohpen		     = 0;
  context.ohfont	     = 0;
  context.hpen 		     = 0;
  context.stockpen	     = FALSE;
  context.colour             = BLACK_COLOUR;
  context.background         = WHITE_COLOUR;	/* is this true? */
  context.default_background = WHITE_COLOUR;
  context.rgb	             = RGB(0, 0, 0);
  context.background_rgb     = RGB(255, 255, 255);
  context.hwnd	             = 0;
  context.hbitmap            = 0;
  context.modified_pen       = FALSE;
  context.open	             = 0;
  context.hdc	             = default_hdc;
  context.display            = TheDisplay;
  context.cache	             = 0;
  context.elevation	     = NIL;
  context.relief_pen	     = 0;
  context.shadow_pen	     = 0;
  context.depth		     = display_depth;
  context.r_offset_x	     = 0;
  context.r_offset_y	     = 0;
}


void
initDraw()
{ make_default_context();
  if ( !TheDisplay )
    TheDisplay = CurrentDisplay(NIL);
  if ( !display_depth )
    display_depth = ws_depth_display(TheDisplay);

  resetDraw();

  at_pce_exit(exitDraw, ATEXIT_FILO);
}


void
resetDraw()
{ context.open  = 0;
  ctx_stacked   = 0;

  reset_context();
}


static void
make_default_context()
{ if ( !default_hdc )
  { if ( !(default_hdc = CreateCompatibleDC(NULL)) ||
	 !(default_hdc_hbitmap = ZCreateCompatibleBitmap(default_hdc,16,16)) ||
	 !(default_hdc_ohbitmap = ZSelectObject(default_hdc,
						default_hdc_hbitmap)) )
      Cprintf("WARNING: Failed to make scratch context");
					/* TBD: Must be fatal error */
  }

  resetDraw();
}


static void
remove_default_context()
{ if ( default_hdc )
  { ZSelectObject(default_hdc, default_hdc_ohbitmap);
    ZDeleteObject(default_hdc_hbitmap);
    DeleteDC(default_hdc);

    default_hdc = NULL;
  }
}


void
exitDraw()
{ DisplayObj d = TheDisplay;

  remove_default_context();
					/* Windows frames and windows */
  if ( d && notNil(d) )
  { Cell cell;

    for_cell(cell, d->frames)
      send(cell->value, NAME_uncreate, 0);
  }

  SetCursor(LoadCursor(NULL, IDC_WAIT));

  closeAllXrefs();
  resetDraw();
}


void
d_offset(int x, int y)
{ DEBUG(NAME_cache, Cprintf("d_offset(%d, %d)\n", x, y));

  context.offset_x = x;
  context.offset_y = y;

  x = -x;
  y = -y;

  if ( context.cache )
  { SetWindowOrg(context.cached_hdc, x, y);
  } else
    SetWindowOrg(context.hdc, x, y);
}


void
r_offset(int x, int y)
{ if ( x == 0 && y == 0 )
    return;				/* very common! */

  if ( !context.cache )
  { d_offset(context.offset_x + x, context.offset_y + y);
  } else
  { POINT old;
    int rval;

    context.r_offset_x += x;
    context.r_offset_y += y;

    DEBUG(NAME_offset, Cprintf("r_offset(%d, %d): vp-offset %d, %d\n",
			       x, y,
			       context.r_offset_x - context.cache_x,
			       context.r_offset_y - context.cache_y));

    rval = SetViewportOrgEx(context.hdc,
			    context.r_offset_x - context.cache_x,
			    context.r_offset_y - context.cache_y,
			    &old);
    assert(rval);
    DEBUG(NAME_offset, Cprintf("\told = %d, %d\n", old.x, old.y));
  }
}


DisplayObj
d_display(DisplayObj d)
{ DisplayObj old = context.display;

  if ( isDefault(d) )
    d = CurrentDisplay(NIL);

  if ( context.display != d )
  { openDisplay(d);
    context.display = d;
    quick = (d->quick_and_dirty == ON);
  }


  return old;
}


void
d_ensure_display()
{ if ( context.display == NULL )
    d_display(CurrentDisplay(NIL));
}


void
d_flush(void)
{
}


status
d_mswindow(PceWindow sw, IArea a, int clear)
{ push_context();

  context.hwnd           = getHwndWindow(sw);
  context.hdc            = BeginPaint(context.hwnd, &context.ps);
  context.device         = sw;
  context.default_colour = sw->colour;
  context.open++;

  if ( !IsRectEmpty(&context.ps.rcPaint) )
  { RECT *r = &context.ps.rcPaint;

    a->x = r->left   - valInt(sw->scroll_offset->x);
    a->y = r->top    - valInt(sw->scroll_offset->y);
    a->w = r->right  - r->left;
    a->h = r->bottom - r->top;


    if ( cache && clear )
    { RECT rect;
      HBRUSH hbrush;

      context.cached_hdc     = context.hdc;
      context.cache_x        = a->x;
      context.cache_y        = a->y;
      context.cache_w        = a->w + 1;
      context.cache_h        = a->h + 1;
      context.cache	     = ZCreateCompatibleBitmap(context.hdc,
						       context.cache_w,
						       context.cache_h);
      context.hdc            = CreateCompatibleDC(context.hdc);
      context.cache_ohbitmap = ZSelectObject(context.hdc, context.cache);
      context.background_rgb = cref_colour(sw->background);

      rect.left   = 0;
      rect.top    = 0;
      rect.right  = context.cache_w;
      rect.bottom = context.cache_h;
      hbrush = ZCreateSolidBrush(context.background_rgb);
      FillRect(context.hdc, &rect, hbrush);
      ZDeleteObject(hbrush);

      SetViewportOrg(context.hdc, -context.cache_x, -context.cache_y);

      DEBUG(NAME_cache, Cprintf("Created cache for %d %d %d %d\n",
				context.cache_x, context.cache_y,
				context.cache_w, context.cache_h));
    }

    r_default_background(sw->background);
    SetBkMode(context.hdc, TRANSPARENT);

    succeed;
  }

  fail;
}


void
d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit)
{ d_display(getDisplayGraphical((Graphical)sw));

  if ( !context.open++ )
  { push_context();

    context.hwnd               = getHwndWindow(sw);
    context.hdc                = BeginPaint(context.hwnd, &context.ps);
    context.default_colour     = sw->colour;
    context.background	       = sw->background;
    context.default_background = sw->background;

    if ( clear )
      r_clear(x, y, w, h);
  }
}


static void
push_context()
{ if ( context.open )
    ctx_stack[ctx_stacked++] = context;
  if ( ctx_stacked >= MAX_CTX_DEPTH )
    Cprintf("**************** ERROR: Draw Context Stack overflow\n");

  reset_context();
}


void
d_image(Image i, int x, int y, int w, int h)
{ Colour background;

  push_context();

  DEBUG(NAME_redraw, Cprintf("d_image(%s, %d, %d, %d, %d)\n",
			     pp(i), x, y, w, h));

  context.open++;
  d_display(notNil(i->display) ? i->display : DEFAULT);
  context.device = i;
  
  context.hbitmap  = (HBITMAP) getXrefObject(i, context.display);
  context.hdc      = CreateCompatibleDC(NULL);
  context.ohbitmap = ZSelectObject(context.hdc, context.hbitmap);
  context.depth    = valInt(i->depth);

  if ( x != 0 || y != 0 || w != valInt(i->size->w) || h != valInt(i->size->h) )
  { HRGN clip_region = ZCreateRectRgn(x, y, x+w, y+h);

    ZSelectObject(context.hdc, clip_region);
    ZDeleteObject(clip_region);
  }

  if ( notDefault(i->foreground) )
    context.default_colour = i->foreground;
  else
  { if ( i->kind == NAME_bitmap )
      context.default_colour = BLACK_COLOUR;
    else
      context.default_colour = context.display->foreground;
  }
  
  if ( notDefault(i->background) )
    background = i->background;
  else
  { if ( i->kind == NAME_bitmap )
      background = WHITE_COLOUR;
    else
      background = context.display->background;
  }

  SetBkMode(context.hdc, TRANSPARENT);
  r_default_background(background);
  r_colour(DEFAULT);
}


void
d_screen(DisplayObj d)
{
}


void
d_hdc(HDC hdc, Colour fg, Colour bg)
{ push_context();

  d_display(DEFAULT);
  if ( isDefault(fg) )
    fg = context.display->foreground;
  if ( isDefault(bg) )
    bg = context.display->background;

  context.open++;
  context.hdc = hdc;
  context.device = NIL;			/* anonymous device */

  r_default_background(bg);
  r_default_colour(fg);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clipping is nasty in MS-Windows.  The   task of the pair r_clip(x,y,w,h)
and r_clip_done() is to set the clipping   area  of the device (given in
the current coordinate system determined   by d_offset() and r_offset())
and revert it back to the old clipping   area.  Clipping is used both by
class device if `device<-clip_area' is  present   and  by  class text to
handle scrolled text.

In Windows, clipping is established by   creating a region and selecting
it  into  the  current  device  context.   The  region  is  in  *device*
coordinates.  Unlike for the  other   graphical  attributes, selecting a
clipping region does  *not*  return  a   handle  to  the  old situation.
Therefore we use GetClipBox() to  find   the  clipping  region before we
started clipping.  But ...  the region   returned  by GetClipBox() is in
*logical* coordinates!  Hence all the offsets.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
d_clip(int x, int y, int w, int h)
{ if ( context.clip_depth < MAX_CLIP_DEPTH )
  { HRGN hrgn;
    RECT *rect = &context.clip_stack[context.clip_depth].orect;

    GetClipBox(context.hdc, rect);

    DEBUG(NAME_clip, { Cprintf("d_clip(%d %d %d %d): ", x, y, w, h);
		       Cprintf("ClipBox = %d %d %d %d --> ",
			       rect->left, rect->top,
			       rect->right - rect->left,
			       rect->bottom - rect->top);
		     });
    
    if ( context.cache )
    { x += context.r_offset_x - context.cache_x; /* TRY ... */
      y += context.r_offset_y - context.cache_y;
    } else if ( context.hwnd )
    { POINT offset;

      GetWindowOrgEx(context.hdc, &offset);
      x -= offset.x;
      y -= offset.y;
    }

    hrgn = ZCreateRectRgn(x, y, x+w, y+h);
    ZSelectObject(context.hdc, hrgn);
    ZDeleteObject(hrgn);

    DEBUG(NAME_clip, { RECT nrect;
		       GetClipBox(context.hdc, &nrect);
		       Cprintf("%d %d %d %d\n",
			       nrect.left, nrect.top,
			       nrect.right - nrect.left,
			       nrect.bottom - nrect.top);
		     });

    context.clip_depth++;
  } else
    sysPce("Too many levels of clipping");
}


void
d_done(void)
{ if ( --context.open == 0 )
  { DEBUG(NAME_redraw, Cprintf("d_done(%s)\n",
			       context.hwnd ?
			       pp(GetWindowLong(context.hwnd, GWL_DATA)) :
			       "(image)"));

    if ( context.ohbrush )
    { ZSelectObject(context.hdc, context.ohbrush);
      context.hbrush = 0;
      context.ohbrush = 0;
    }
    empty_brush_cache();
    if ( context.hpen )
    { ZSelectObject(context.hdc, context.ohpen);
      if ( !context.stockpen )
	ZDeleteObject(context.hpen);
      context.hpen = 0;
    }
    if ( context.ohfont )
    { ZSelectObject(context.hdc, context.ohfont);
      context.ohfont = 0;
    }
    if ( context.relief_pen )
    { ZDeleteObject(context.relief_pen);
      context.relief_pen = 0;
    }
    if ( context.shadow_pen )
    { ZDeleteObject(context.shadow_pen);
      context.shadow_pen = 0;
    }

    if ( instanceOfObject(context.device, ClassWindow) )
    { if ( context.cache )
      { DEBUG(NAME_cache, Cprintf("Writing cache to window\n"));
	SetViewportOrg(context.hdc, 0, 0);
	BitBlt(context.cached_hdc,
	       context.cache_x, context.cache_y,
	       context.cache_w, context.cache_h,
	       context.hdc, 0, 0, SRCCOPY);
	ZSelectObject(context.hdc, context.cache_ohbitmap);
	ZDeleteObject(context.cache);
	DeleteDC(context.hdc);
      }
      EndPaint(context.hwnd, &context.ps);
    } else if ( instanceOfObject(context.device, ClassImage) )
    { ZSelectObject(context.hdc, context.ohbitmap);
      DeleteDC(context.hdc);
    } else				/* d_hdc() context */
    { ;
    }

    if ( ctx_stacked )
      context = ctx_stack[--ctx_stacked];
    else
      reset_context();
  }
}


void
d_clip_done(void)
{ RECT *rect;
  HRGN hrgn;
  int ox=0, oy=0;

  if ( context.clip_depth-- < 0 )
    sysPce("Clip stack underfow!");

  rect = &context.clip_stack[context.clip_depth].orect;
  DEBUG(NAME_clip,  Cprintf("d_clip_done(%d %d %d %d) --> ",
			    rect->left, rect->top,
			    rect->right - rect->left,
			    rect->bottom - rect->top));

  if ( context.cache )
  { ox = context.r_offset_x - context.cache_x;
    oy = context.r_offset_y - context.cache_y;
  } else if ( context.hwnd )
  { POINT offset;

    GetWindowOrgEx(context.hdc, &offset);
    ox = -offset.x;
    oy = -offset.y;
  }
  rect->left   += ox;
  rect->top    += oy;
  rect->right  += ox;
  rect->bottom += oy;

  hrgn = ZCreateRectRgnIndirect(rect);
  ZSelectObject(context.hdc, hrgn);
  ZDeleteObject(hrgn);

  DEBUG(NAME_clip, { RECT nrect;
		     GetClipBox(context.hdc, &nrect);
		     Cprintf("%d %d %d %d\n",
			     nrect.left, nrect.top,
			     nrect.right, nrect.bottom);
		   });
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


void
r_clear(int x, int y, int w, int h)
{ HBRUSH hbrush = ZCreateSolidBrush(context.background_rgb);
  RECT rect;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  FillRect(context.hdc, &rect, hbrush);
  ZDeleteObject(hbrush);
}


void
r_complement(int x, int y, int w, int h)
{ RECT rect;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  InvertRect(context.hdc, &rect);
}


void
r_and(int x, int y, int w, int h, Image pattern)
{ HBITMAP bm = (HBITMAP) getXrefObject(pattern, context.display);
  HBRUSH brush = ZCreatePatternBrush(bm);
  HBRUSH obrush = ZSelectObject(context.hdc, brush);

  PatBlt(context.hdc, x, y, w, h, 0xFA0089); /* P|D */

  ZSelectObject(context.hdc, obrush);
  ZDeleteObject(brush);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
It appears you cannot create a NULL_PEN using CreatePen(). As we need to
use stock pens anyway, I decided to use   one  for the very common solid
1-thick  black  pen  too,  assuming    GetStockObject()  will  use  less
resources. In this  implementation,  we   will  call  DeleteObject() for
stock-objects. Should this be done or not?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
r_update_pen()
{ if ( context.modified_pen )
  { HPEN org, old = (!context.stockpen ? context.hpen : 0);

    if ( context.thickness <= 0 )
    { context.hpen = GetStockObject(NULL_PEN);
      context.stockpen = TRUE;
    } else
    { int style;

      if ( context.texture == NAME_none )
      { if ( context.rgb == RGB(0,0,0) && context.thickness == 1 )
	{ context.hpen = GetStockObject(BLACK_PEN);
	  context.stockpen = TRUE;
	  goto out;
	} else
	  style = PS_SOLID;
      } else if ( context.texture == NAME_dotted )
	style = PS_DOT;
      else if ( context.texture == NAME_dashed )
	style = PS_DASH;
      else if ( context.texture == NAME_dashdot )
	style = PS_DASHDOT;
      else if ( context.texture == NAME_dashdotted )
	style = PS_DASHDOTDOT;
      else if ( context.texture == NAME_longdash )
	style = PS_DASH;		/* not supported */
      
      context.hpen = ZCreatePen(style, context.thickness, context.rgb);
      context.stockpen = FALSE;
    }

  out:
    org = ZSelectObject(context.hdc, context.hpen);
    if ( !context.ohpen )
      context.ohpen = org;

    if ( old )
      ZDeleteObject(old);

    context.modified_pen = FALSE;
  }
}


void
r_thickness(int pen)
{ if ( context.thickness != pen )
  { context.modified_pen = TRUE;
    context.thickness = pen;
  }
}


void
r_dash(Name dash)
{ if ( context.texture != dash )
  { context.modified_pen = TRUE;
    context.texture	 = dash;
  }
}


Any
r_colour(Any colour)
{ Any old = context.colour;

  DEBUG(NAME_colour, Cprintf("r_colour(%s)\n", pp(colour)));

  if ( isDefault(colour) )
  { assert(notDefault(context.default_colour));
    colour = context.default_colour;
  } else if ( !instanceOfObject(colour, ClassColour) )
    colour = getReplacementColourPixmap(colour);

  if ( context.colour != colour )
  { context.modified_pen = TRUE;
    context.colour       = colour;
    context.rgb		 = cref_colour(colour);
    SetTextColor(context.hdc, context.rgb);
  }

  return old;
}

#define BRUSH_CACHE_SIZE 5

typedef struct
{ Any	 object;			/* object for brush */
  HBRUSH brush;				/* associated brush */
  int	 times;				/* # times used */
} brush_cache_element;

brush_cache_element brush_cache[BRUSH_CACHE_SIZE];

static HBRUSH
lookup_brush(Any fill)
{ int i;
  brush_cache_element *e;

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
    if ( e->object == fill )
    { e->times++;
      return e->brush;
    }

  return (HBRUSH)0;
}


static void
empty_brush_cache()
{ int i;
  brush_cache_element *e;

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( e->brush )
    { ZDeleteObject(e->brush);
      e->brush = (HBRUSH)0;
    }
    e->object = NULL;
    e->times = 0;
  }
}


static void
add_brush(Any fill, HBRUSH brush)
{ int i;
  brush_cache_element *e;
  int leastusage;
  brush_cache_element *leastused;

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( !e->object )
    { e->object = fill;
      e->brush  = brush;
      e->times  = 1;
      return;
    }
  }

  leastused = brush_cache;
  leastusage = leastused->times;
  
  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( e->times < leastusage )
    { leastusage = e->times;
      leastused  = e;
    }
  }

  if ( context.hbrush == leastused->brush )
    Cprintf("%s:%d: Attempt to delete current brush", __FILE__, __LINE__);

  ZDeleteObject(leastused->brush);
  leastused->object = fill;
  leastused->brush  = brush;
  leastused->times  = 0;
}


static HashTable
winBrushTable()
{ static HashTable table;

  if ( !table )
  { table = createHashTable(toInt(16), ON);

    declareWindowsBrush(NIL,     GetStockObject(NULL_BRUSH));
    declareWindowsBrush(DEFAULT, GetStockObject(NULL_BRUSH)); /* play safe */
  }

  return table;
}


void
declareWindowsBrush(Any obj, HBRUSH brush)
{ Int b = toInt((long)brush);

  assert((HBRUSH) valInt(b) == brush);
  appendHashTable(winBrushTable(), obj, b);
}


static HBRUSH
standardWindowsBrush(Any obj)
{ Int b;

  if ( (b = getMemberHashTable(winBrushTable(), obj)) )
    return (HBRUSH) valInt(b);
  
  return 0;
}


static HBRUSH
r_fillbrush(Any fill)
{ HBRUSH hbrush;

  if ( !(hbrush = standardWindowsBrush(fill)) )
  { if ( !(hbrush = lookup_brush(fill)) )
    { if ( instanceOfObject(fill, ClassImage) )
      { HBITMAP bm = (HBITMAP) getXrefObject(fill, context.display);     

	hbrush = ZCreatePatternBrush(bm);
      } else /* instanceOfObject(fill, ClassColour) */
      { COLORREF rgb = cref_colour(fill);

	hbrush = ZCreateSolidBrush(rgb);
      }

      DEBUG(NAME_fill, Cprintf("add_brush(%s, 0x%x)\n", pp(fill), hbrush));
      add_brush(fill, hbrush);
    }
  }

  return hbrush;
}


void
r_fillpattern(Any fill)			/* colour or image */
{ if ( context.fill_pattern != fill )
  { HBRUSH new, old;
    
    DEBUG(NAME_fill, Cprintf("Selecting fill-pattern %s\n", pp(fill)));
    new = r_fillbrush(fill);
    context.hbrush = new;
    old = ZSelectObject(context.hdc, new);
    if ( !context.ohbrush )
      context.ohbrush = old;

    context.fill_pattern = fill;
  }
}


void
r_arcmode(Name mode)
{					/* handled by r_msarc() itself */
}


Any
r_default_colour(Any c)
{ Any old = context.default_colour;
  
  DEBUG(NAME_colour, Cprintf("r_default_colour(%s)\n", pp(c)));
  if ( notDefault(c) )
  { if ( !instanceOfObject(c, ClassColour) )
      c = getReplacementColourPixmap(c);

    context.default_colour = c;
  }

  assert(notDefault(context.default_colour));
  r_colour(context.default_colour);
  
  return old;
}


Any
r_background(Any c)
{ Any old = context.background;

  if ( isDefault(c) )
  { c = context.default_background;
    DEBUG(NAME_background, Cprintf("Using default background %s\n", pp(c)));
  } else if ( !instanceOfObject(c, ClassColour) )
    c = getReplacementColourPixmap(c);

  if ( context.background != c )
  { COLORREF rgb = cref_colour(c);
    
    SetBkColor(context.hdc, rgb);
    context.background     = c;
    context.background_rgb = rgb;
  }

  return old;
}


static void
r_default_background(Any bg)
{ r_background(bg);
  context.default_background = context.background;

  DEBUG(NAME_background, Cprintf("r_default_background(%s)\n", pp(bg)));
}


void
r_swap_background_and_foreground(void)
{ Colour tc = context.background;

  r_background(context.colour);
  r_colour(tc);
}


void
r_subwindow_mode(Bool val)
{
}


void
r_invert_mode(Bool val)
{
}


void
r_translate(int x, int y, int *ox, int *oy)
{
}


void
r_box(int x, int y, int w, int h, int r, Image fill)
{ if ( context.thickness > 0 || notNil(fill) )
  { if ( context.thickness > 0 || r > 1 )
    { int da = context.thickness / 2;
      int db = max(0, (context.thickness - 1) / 2);
  
      DEBUG(NAME_redraw, Cprintf("r_box(%d, %d, %d, %d, %d, %s)\n",
				 x, y, w, h, r, pp(fill)));

      DEBUG(NAME_pen, Cprintf("context.thickness = %d\n", context.thickness));
      x += da;    y += da;
      w -= da+db; h -= da+db;

      if ( w < 2 || h < 2 )
	return;				/* TBD: too small (make line) */

      r_fillpattern(fill);
      r_update_pen();

      if ( r == 0 )
      { Rectangle(context.hdc, x, y, x+w, y+h);
      } else
      { RoundRect(context.hdc, x, y, x+w, y+h, r*2, r*2);
      }
    } else
    { r_fill(x, y, w, h, fill);
    }
  }
}


void
r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill)
{ if ( !shadow )
  { r_box(x, y, w, h, r, fill);
  } else
  { if ( shadow > h ) shadow = h;
    if ( shadow > w ) shadow = w;

    r_colour(BLACK_COLOUR);
    r_box(x+shadow, y+shadow, w-shadow, h-shadow, r, BLACK_IMAGE);
    r_colour(DEFAULT);
    r_box(x, y, w-shadow, h-shadow, r, isNil(fill) ? WHITE_IMAGE : fill);
  }
}


#define MAX_SHADOW 10

static COLORREF
cref_colour(Colour c)
{ COLORREF r = (COLORREF) getXrefObject(c, context.display);
  int exact;

  exact = (r & EXACT_COLOUR_MASK);
  r &= ~EXACT_COLOUR_MASK;

  return exact ? r : GetNearestColor(context.hdc, r);
}


static void
r_elevation(Elevation e)
{ if ( context.elevation != e )
  { Any bg = context.background;
    Any relief, shadow;

    DEBUG(NAME_elevation,
	  Cprintf("r_elevation(%s) (bg=%s, depth=%d) ... ",
		  pp(e), pp(bg), context.depth));

    if ( isDefault(e->relief) )
    { if ( instanceOfObject(bg, ClassColour) && context.depth != 1 )
	relief = getHiliteColour(bg);
      else
	relief = WHITE_COLOUR;
    } else
      relief = e->relief;

    if ( isDefault(e->shadow) )
    { if ( instanceOfObject(bg, ClassColour) && context.depth != 1 )
	shadow = getReduceColour(bg);
      else
	shadow = BLACK_COLOUR;
    } else
      shadow = e->shadow;
  
    assert(instanceOfObject(shadow, ClassColour));

    if ( context.relief_pen )
      ZDeleteObject(context.relief_pen);
    if ( context.shadow_pen )
      ZDeleteObject(context.shadow_pen);

    context.relief_pen = ZCreatePen(PS_SOLID, 1, cref_colour(relief));
    context.shadow_pen = ZCreatePen(PS_SOLID, 1, cref_colour(shadow));

    DEBUG(NAME_elevation, Cprintf("ok\n"));

    context.elevation = e;
  }
}


typedef struct
{ int x1, y1;				/* start of line */
  int x2, y2;				/* end of line */
} lsegment;


typedef struct
{ int x, y;
  int width, height;
  int angle1, angle2;
} larc;


static void
draw_segments(lsegment *s, int n, HPEN pen)
{ HPEN old = ZSelectObject(context.hdc, pen);

  for( ; n > 0; n--, s++ )
  { MoveTo(context.hdc, s->x1, s->y1);
    LineTo(context.hdc, s->x2, s->y2);
  }

  ZSelectObject(context.hdc, old);
}


static short costable[91];

static int
cos64(int angle, int radius)
{ int f;

  angle /= 64;

  if ( angle <= 90 )
    f = 1;
  else if ( angle <= 180 )
    angle = 180 - angle, f = -1;
  else if ( angle <= 270 )
    angle = angle - 180, f = -1;
  else /* if ( angle <= 360 ) */
    angle = 360 - angle, f = 1;
  
  if ( !costable[angle] && angle != 90 )
  { costable[angle] = rfloat(1024.0 * cos(((float)angle * M_PI)/M_PI));
    DEBUG(NAME_arc, Cprintf("Adding cos(%d) = %d (%f)\n",
			    angle,
			    costable[angle],
			    (float) costable[angle] * 1024.0));
  }
    
  return (radius * costable[angle] * f) / 1024;
}


static int
sin64(int angle, int radius)
{ angle -= 90*64;
  if ( angle < 0 )
    angle += 360*64;

  return cos64(angle, radius);
}


void
r_arc(int x, int y, int w, int h, int s, int e, Any fill)
{ Cprintf("r_arc() not implemented yet\n");
}


static void
draw_arcs(larc *a, int n, HPEN pen)
{ HPEN old = ZSelectObject(context.hdc, pen);
  
  for( ; n > 0; n--, a++ )
  { int x1, y1, x2, y2;
    int cx = a->x + a->width/2;
    int cy = a->y + a->height/2;

    x1 = cx + cos64(a->angle1, a->width)/2;
    y1 = cy - sin64(a->angle1, a->height)/2;
    x2 = cx + cos64(a->angle1 + a->angle2, a->width)/2;
    y2 = cy - sin64(a->angle1 + a->angle2, a->height)/2;

    DEBUG(NAME_arc, Cprintf("%d %d %d %d (%d --> %d): "
			    "%d,%d --> %d, %d\n",
			    a->x, a->y, a->width, a->height,
			    a->angle1/64, a->angle2/64,
			    x1, y1, x2, y2));

    Arc(context.hdc,
	a->x, a->y, a->x + a->width, a->y + a->height,
	x1, y1, x2, y2);
  }

  ZSelectObject(context.hdc, old);
}


void
r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up)
{ int pen = 1;
  int shadow = valInt(e->height);
  HPEN top_left_pen, bottom_right_pen;

  if ( e->kind == NAME_shadow )
  { lsegment s[2 * MAX_SHADOW];
    int is = 0;				/* # segments */
    int xt, yt, os;

    r_elevation(e);

    shadow = abs(shadow);
    shadow = min(shadow, min(w, h));
    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;

    r_box(x, y, w-shadow, h-shadow, max(0, radius-shadow), e->colour);
    
    xt = x, yt = y;

    if ( radius > 0 )
    { int  r = min(radius, min(w, h));
      larc as[MAX_SHADOW * 3];
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

      draw_arcs(as, ns, context.shadow_pen);
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

    draw_segments(s, is, context.shadow_pen);

    return;
  }

  if ( !up )
    shadow = -shadow;

  if ( shadow )
  { r_elevation(e);

    if ( shadow > 0 )
    { bottom_right_pen = context.shadow_pen;
      top_left_pen     = context.relief_pen;
    } else
    { bottom_right_pen = context.relief_pen;
      top_left_pen     = context.shadow_pen;
      shadow           = -shadow;
    }

    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;

    if ( radius > 0 )			/* coloured elevation, radius > 0 */
    { lsegment sr[MAX_SHADOW * 2];	/* top, left */
      larc     ar[MAX_SHADOW * 3];	/* idem */
      lsegment ss[MAX_SHADOW * 2];	/* bottom, right */
      larc     as[MAX_SHADOW * 3];	/* item */
      int      is=0, ir=0, ns=0, nr=0;	/* # items */
      int      os;
      int      xt = x, yt = y;
  
      w--, h--;

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
        ar[nr].angle1 = 45*64;   ar[nr].angle2 = 45*64;
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
        as[ns].angle1 = 0*64;  as[ns].angle2 = 45*64;
	ns++;
	as[ns].x = os+xt;	as[ns].y = -os+yt+h-wh;	/* bottom-left */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 225*64; as[ns].angle2 = 45*64;
	ns++;
      }

      draw_segments(sr, ir, top_left_pen);
      draw_segments(ss, is, bottom_right_pen);
      draw_arcs(    ar, nr, top_left_pen);
      draw_arcs(    as, ns, bottom_right_pen);
    } else				/* coloured elevation, radius == 0 */
    { lsegment s[2 * MAX_SHADOW];
      int i, os;
      int xt = x, yt = y;

      for(i=0, os=0; os < shadow; os += pen)
      { s[i].x1 = xt+os;	s[i].y1 = yt+os; 	/* top-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+os;
	i++;
	s[i].x1 = xt+os;	s[i].y1 = yt+os;	/* left-side */
	s[i].x2 = xt+os;	s[i].y2 = yt+h-1-os;
	i++;
      }
      draw_segments(s, i, top_left_pen);
      
      for(i=0, os=0; os < shadow; os += pen)
      { s[i].x1 = xt+os;	s[i].y1 = yt+h-1-os;	/* bottom-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
	i++;
	s[i].x1 = xt+w-1-os;	s[i].y1 = yt+os;	/* right-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
	i++;
      }
      draw_segments(s, i, bottom_right_pen);
    }
  }

  if ( notDefault(e->colour) )
    r_fill(x+shadow, y+shadow, w-2*shadow, h-2*shadow, e->colour);
}


void
r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up)
{ lsegment s[MAX_SHADOW];
  int i;
  int z = valInt(e->height);

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
  draw_segments(s, i, up ? context.relief_pen : context.shadow_pen);

  for(i=0; i<z; i++)
  { s[i].x1 = x1, s[i].x2 = x2, s[i].y1 = y1, s[i].y2 = y2;
    if ( y1 == y2 )
      y1++, y2++;
    else
      x1++, x2++;
  }
  draw_segments(s, i, up ? context.shadow_pen : context.relief_pen);
}


#define X(x) x				/* less changes! */
#define Y(y) y

void
r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3,
	      Elevation e, int up, int map)
{ lsegment s[3];
  HPEN top_pen, bot_pen;
  int z = valInt(e->height);

  r_elevation(e);

  if ( !up )
    z = -z;

  if ( z > 0 )
  { top_pen = context.relief_pen;
    bot_pen = context.shadow_pen;
  } else
  { top_pen = context.shadow_pen;
    bot_pen = context.relief_pen;
  }

  s[0].x1 = X(x1);   s[0].y1 = Y(y1);   s[0].x2 = X(x2);   s[0].y2 = Y(y2);
  s[1].x1 = s[0].x2; s[1].y1 = s[0].y2; s[1].x2 = X(x3);   s[1].y2 = Y(y3);
  s[2].x1 = s[1].x2; s[2].y1 = s[1].y2; s[2].x2 = s[0].x1; s[2].y2 = s[0].y1;

  draw_segments(s,     2, top_pen);
  draw_segments(&s[2], 1, bot_pen);
}


void
r_3d_diamond(int x, int y, int w, int h, Elevation e, int up)
{ HPEN top_pen, bot_pen;
  int z = valInt(e->height);
  int nox, noy, wex, wey, sox, soy, eax, eay;

  r_elevation(e);
  r_thickness(1);

  if ( !up )
    z = -z;

  if ( z > 0 )
  { top_pen = context.relief_pen;
    bot_pen = context.shadow_pen;
  } else
  { top_pen = context.shadow_pen;
    bot_pen = context.relief_pen;
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

  for(; z > 0; z--)
  { lsegment s[4];

    s[0].x1 = eax; s[0].y1 = eay; s[0].x2 = nox; s[0].y2 = noy;
    s[1].x1 = nox; s[1].y1 = noy; s[1].x2 = wex; s[1].y2 = wey;
    s[2].x1 = wex; s[2].y1 = wey; s[2].x2 = sox; s[2].y2 = soy;
    s[3].x1 = sox; s[3].y1 = soy; s[3].x2 = eax; s[3].y2 = eay;

    draw_segments(s,     2, top_pen);
    draw_segments(&s[2], 2, bot_pen);

    noy++;
    soy--;
    wex--;
    eax++;
  }
      
  if ( (up && notDefault(e->colour)) || (!up && notDefault(e->background)))
  { POINT p[4];
    HPEN oldpen = ZSelectObject(context.hdc, GetStockObject(NULL_PEN));

    p[0].x = wex; p[0].y = wey;
    p[1].x = nox; p[1].y = noy;
    p[2].x = eax; p[2].y = eay;
    p[3].x = sox; p[3].y = soy;

    r_fillpattern(up ? e->colour : e->background);
    Polygon(context.hdc, p, 4);
    ZSelectObject(context.hdc, oldpen);
  }
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
  { HPEN top_pen, bottom_pen;
    int xt=x, yt=y;
    larc a[MAX_SHADOW*2];
    int an, os;

    r_elevation(z);

    if ( shadow > 0 )
    { top_pen    = context.relief_pen;
      bottom_pen = context.shadow_pen;
    } else
    { top_pen    = context.shadow_pen;
      bottom_pen = context.relief_pen;
      shadow     = -shadow;
    }

    for(an=0, os=0; os<shadow && w>=1 && h>=1; os++)
    { a[an].x = xt+os; a[an].y = yt+os;
      a[an].width = w-2*os; a[an].height = h-2*os;
      a[an].angle1 = 45*64; a[an].angle2 = 180*64;
      an++;
    }
    draw_arcs(a, an, top_pen);

    for(an=0, os=0; os<shadow && w>=1 && h>=1; os++)
    { a[an].x = xt+os; a[an].y = yt+os;
      a[an].width = w-2*os; a[an].height = h-2*os;
      a[an].angle1 = 225*64; a[an].angle2 = 180*64;
      an++;
    }
    draw_arcs(a, an, bottom_pen);
  }

  if ( notDefault(z->colour) )
  { r_thickness(0);
    r_ellipse(x+shadow, y+shadow, w-2*shadow, h-2*shadow, z->colour);
  }
}


void
r_msarc(int x, int y, int w, int h,	/* bounding box */
	int sx, int sy,			/* starting point */
	int ex, int ey,			/* end point */
	Name close,			/* none,pie_slice,chord */
	Any fill)			/* @nil or fill pattern */
{ if ( close == NAME_none )
  { Arc(context.hdc, x, y, x+w, y+h, sx, sy, ex, ey);
  } else if ( close == NAME_pieSlice )
  { r_fillpattern(fill);
    Pie(context.hdc, x, y, x+w, y+h, sx, sy, ex, ey);
  } else /* if ( close == NAME_chord ) */
  { r_fillpattern(fill);
    Chord(context.hdc, x, y, x+w, y+h, sx, sy, ex, ey);
  }
}


void
r_ellipse(int x, int y, int w, int h, Any fill)
{ r_fillpattern(fill);
  r_update_pen();

  DEBUG(NAME_redraw, Cprintf("r_ellipse(%d, %d, %d, %d, %s)\n",
			     x, y, w, h, pp(fill)));
  Ellipse(context.hdc, x, y, x+w, y+h);
}


void
r_line(int x1, int y1, int x2, int y2)
{ r_update_pen();
  MoveTo(context.hdc, x1, y1);
  LineTo(context.hdc, x2, y2);
}


void
r_polygon(IPoint pts, int n, int close)
{ 
}


void
r_path(Chain points, int ox, int oy, int radius, int closed, Image fill)
{ Cell cell;
  int npoints = valInt(getSizeChain(points));

  if ( npoints < 2 )
    return;

  if ( radius == 0 )
  { POINT *pts = (POINT *)alloca((npoints+1) * sizeof(POINT));
    int i=0;

    for_cell(cell, points)
    { Point p = cell->value;
      pts[i].x = valInt(p->x) + ox;
      pts[i].y = valInt(p->y) + oy;
      i++;
    }

    r_update_pen();
    if ( closed || notNil(fill) )
    { r_fillpattern(fill);
      Polygon(context.hdc, pts, i);
    } else
      Polyline(context.hdc, pts, i);
  } else
  { Cprintf("radius > 0 not yet implemented (r_path())\n");
  }
}


void
r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op)
{ HBITMAP bm = (HBITMAP) getXrefObject(image, context.display);
  HDC mhdc = CreateCompatibleDC(context.hdc);
  HBITMAP obm;
  DWORD rop;

  if ( op == NAME_copy )
    rop = SRCCOPY;
  else if ( op == NAME_or )
    rop = SRCAND;
  else if ( op == NAME_and )
    rop = SRCPAINT;
  else /*if ( op == NAME_xor )*/
    rop = SRCINVERT;    

  DEBUG(NAME_redraw,
	Cprintf("r_op_image(%s, %d, %d, %d, %d, %d, %d, %s) "
		"(bm=0x%x, mhdc=0x%x)\n",
		pp(image), sx, sy, x, y, w, h, pp(op), (long)bm, (long)mhdc));
  obm = ZSelectObject(mhdc, bm);
  BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, rop);
  if ( op == NAME_xor )
    BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, DSTINVERT);
  ZSelectObject(mhdc, obm);
  DeleteDC(mhdc);
}


void
r_image(Image image,
	int sx, int sy,
	int x, int y, int w, int h,
	Bool transparent)
{ HBITMAP bm = (HBITMAP) getXrefObject(image, context.display);
  HDC mhdc = CreateCompatibleDC(context.hdc);
  HBITMAP obm = ZSelectObject(mhdc, bm);

  DEBUG(NAME_redraw,
	Cprintf("r_image(%s, %d, %d, %d, %d, %d, %d) (bm=0x%x, mhdc=0x%x)\n",
		pp(image), sx, sy, x, y, w, h, (long)bm, (long)mhdc));
    
  if ( transparent == ON && valInt(image->depth) <= 1 )
  { HBRUSH hbrush = ZCreateSolidBrush(context.rgb);
    HBRUSH oldbrush = ZSelectObject(context.hdc, hbrush);
    COLORREF oldbk = SetBkColor(context.hdc, RGB(255,255,255));
    COLORREF oldtx = SetTextColor(context.hdc, RGB(0,0,0));

    BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, 0xB8074AL);
					/* ROP from "Programming Windows3.1" */
					/* 3-rd edition, page 633 */
    SetTextColor(context.hdc, oldtx);
    SetBkColor(context.hdc, oldbk);

    ZSelectObject(context.hdc, oldbrush);
    ZDeleteObject(hbrush);
  } else
  { BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, SRCCOPY);
  }

  ZSelectObject(mhdc, obm);
  DeleteDC(mhdc);
}


void
r_copy(int xf, int yf, int xt, int yt, int w, int h)
{ RECT toclear, source, dest;
  
  source.left = xf;
  source.right = xf + w;
  source.top = yf;
  source.bottom = yf + h;

  dest.left = xt;
  dest.right = xt + w;
  dest.top = yt;
  dest.bottom = yt + h;

  SubtractRect(&toclear, &source, &dest);

  BitBlt(context.hdc, xt, yt, w, h, context.hdc, xf, yf, SRCCOPY);
  r_clear(toclear.left,
	  toclear.top,
	  toclear.right - toclear.left,
	  toclear.bottom - toclear.top);
}


void
r_fill(int x, int y, int w, int h, Image pattern)
{ RECT rect;
  HBRUSH hbrush;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  hbrush = r_fillbrush(pattern);
  FillRect(context.hdc, &rect, hbrush);
}


void
r_fill_polygon(IPoint pts, int n)
{ POINT *points = alloca(sizeof(POINT) * n);
  HPEN oldpen = ZSelectObject(context.hdc, GetStockObject(NULL_PEN));
  int i;

  for(i=0; i<n; i++)
  { points[i].x = pts[i].x;
    points[i].y = pts[i].y;
  }

  Polygon(context.hdc, points, n);

  ZSelectObject(context.hdc, oldpen);
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
{ POINT pt[3];
  HPEN oldpen = ZSelectObject(context.hdc, GetStockObject(NULL_PEN));

  pt[0].x = x1; pt[0].y = y1;
  pt[1].x = x2; pt[1].y = y2;
  pt[2].x = x3; pt[2].y = y3;

  Polygon(context.hdc, pt, 3);
  ZSelectObject(context.hdc, oldpen);
}


void
r_triangle(int x1, int y1, int x2, int y2, int x3, int y3)
{ POINT pts[3];

  pts[0].x = x1;
  pts[0].y = y1;
  pts[1].x = x2;
  pts[1].y = y2;
  pts[2].x = x3;
  pts[2].y = y3;

  r_fillpattern(BLACK_COLOUR);
  Polygon(context.hdc, pts, sizeof(pts)/sizeof(POINT));
}


void
r_pixel(int x, int y, Any val)
{ COLORREF c;

  if ( isBoolean(val) )
  { if ( val == ON )
      c = RGB(255, 255, 255);
    else
      c = RGB(0, 0, 0);
  } else
    c = cref_colour(val);

  SetPixel(context.hdc, x, y, c);
}


void
r_complement_pixel(int x, int y)
{ r_complement(x, y, 1, 1);
}


void
d_modify(void)
{
}


unsigned long
r_get_pixel(int x, int y)
{ return (unsigned long) GetPixel(context.hdc, x, y);
}


int
r_get_mono_pixel(int x, int y)
{ return GetPixel(context.hdc, x, y) == context.background_rgb ? FALSE : TRUE;
}


static void
s_font(FontObj font)
{ if ( context.font != font )
  { WsFont wsf;
    HFONT org;

    if ( !context.hdc )
    { DEBUG(NAME_redraw, Cprintf("!! Making default context\n"));
      make_default_context();
    }
    wsf = getXrefObject(font, context.display);
  
    DEBUG(NAME_font,
	  Cprintf("s_font(%s) (hfont = 0x%x)%s\n",
		  pp(font), (int)wsf->hfont,
		  context.hdc == default_hdc ? " (default_hdc)" : ""));

    context.wsf = wsf;
    org = ZSelectObject(context.hdc, wsf->hfont);
    if ( !context.ohfont )
      context.ohfont = org;

    context.font = font;
  }
}


int
s_has_char(FontObj f, unsigned int c)
{ succeed;
}


void
f_domain(FontObj f, Name which, int *x, int *y)
{ *x = 0;
  *y = 255;
}


int
s_default_char(FontObj font)
{ return ' ';
}


int
s_ascent(FontObj f)
{ s_font(f);

  return context.wsf->ascent;
}


int
s_descent(FontObj f)
{ s_font(f);

  return context.wsf->descent;
}


int
s_height(FontObj f)
{ s_font(f);

  return context.wsf->ascent + context.wsf->descent;
}


int
c_width(unsigned int c, FontObj font)
{ s_font(font);
  
  return context.wsf->widths[c];
}

				/* should move out of window module! */


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
{ if ( !context.wsf )
  { return 0;			/* TBD */
  } else
  { cwidth *widths = context.wsf->widths;
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
    y -= s_ascent(f);
    TextOut(context.hdc, x, y, s, l);
  }
}


void
s_print16(char16 *s, int l, int x, int y, FontObj f)
{ Cprintf("16-bits characters are not (yet) supported on XPCE for Windows\n");
}


void
s_print(String s, int x, int y, FontObj f)
{ if ( isstr8(s) )
    s_print8(s->s_text8, s->size, x, y, f);
  else
    s_print16(s->s_text16, s->size, x, y, f);
}


void
str_size(String s, FontObj font, int *width, int *height)
{ if ( s->size > 0 )
  { RECT rect;
    UINT flags = DT_CALCRECT|DT_EXTERNALLEADING|DT_NOCLIP|DT_NOPREFIX;
    int rval;

    rect.left   = 0;
    rect.top    = 0;
    rect.right  = 0;
    rect.bottom = 0;

    s_font(font);
    rval = DrawText(context.hdc, s->s_text8, s->size, &rect, flags);
    DEBUG(NAME_font,
	  { char buf[32];
	    int n = min(s->size, 25);
	    strncpy(buf, s->s_text8, n);
	    buf[n] = EOS;
	    if ( s->size > 25 )
	      strcat(buf, " ...");

	    Cprintf("DrawText(\"%s\") --> %d\n", buf, rval);
	  });

    *width = rect.right;
  } else
    *width = 0;				/* looks like a Windows bug ... */

  *height = valInt(getHeightFont(font)) *
	    (str_count_chr(s, 0, s->size, '\n')+1);
}


void
str_string(String s, FontObj font,
	   int x, int y, int w, int h,
	   Name hadjust, Name vadjust)
{ RECT rect;
  UINT flags = DT_EXTERNALLEADING|DT_NOCLIP|DT_NOPREFIX;
  
  if ( hadjust == NAME_left )
    flags |= DT_LEFT;
  else if ( hadjust == NAME_center )
    flags |= DT_CENTER;
  else
    flags |= DT_RIGHT;

  if ( vadjust != NAME_top )
  { int th = valInt(getHeightFont(font)) *
             (str_count_chr(s, 0, s->size, '\n')+1);

    if ( vadjust == NAME_center )
      y += (h-th)/2;
    else
      y += h-th;
  } else
    h = 10000;				/* hack: 0x0 does not draw??? */
  if ( hadjust == NAME_left )
    w = 10000;

  rect.left   = x;
  rect.top    = y;
  rect.right  = rect.left + w;
  rect.bottom = rect.top + h;

  s_font(font);
  DEBUG(NAME_redraw, Cprintf("str_text(%s, %s, %d, %d, %d, %d, %s, %s)\n",
			     s->s_text8, pp(font), x, y, w, h,
			     pp(hadjust), pp(vadjust)));
  DrawText(context.hdc, s->s_text8, s->size, &rect, flags);
}


void
ps_string(String s, FontObj font, int x, int y, int w, Name format)
{
}


void
str_label(char8 *s, char8 acc, FontObj font, int x, int y, int w, int h,
	  Name hadjust, Name vadjust)
{
}
