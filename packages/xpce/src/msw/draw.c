/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"

#define MAX_CLIP_DEPTH (5)		/* clip nesting depth */

typedef struct
{ HWND		hwnd;			/* current Windows window */
  HBITMAP	hbitmap;		/* current Image */
  PAINTSTRUCT	ps;			/* paint structure */
  HDC		hdc;			/* device context */
  HBRUSH	hbrush;			/* Current created brush */
  HPEN		hpen;			/* Current created pen */
  HRGN		hrgn;			/* Current created region */

  HRGN		ohrgn;			/* Original region */
  HBITMAP	ohbitmap;		/* Original bitmap */

  HBITMAP	cache;			/* background drawing */
  HDC		cached_hdc;		/* hdc of original device */
  int		cache_x;		/* X-corner of cache */
  int		cache_y;		/* Y-corner of cache */
  int		cache_w;		/* Width of cache */
  int		cache_h;		/* Height of cache */
  HBITMAP	cache_ohbitmap;		/* original bitmap handle */

  int		open;			/* is context opened? */

  Image		fill_pattern;		/* PCE fill-pattern image */
  Colour	colour;			/* Current colour */
  Colour	default_colour;		/* The default colour */
  Colour	background;		/* Background colour */
  COLORREF	rgb;			/* RGB of colour */
  COLORREF	background_rgb;		/* RBG of background */
  int		thickness;		/* Current pen */
  Name		texture;		/* Current dash-pattern */
  int		modified_pen;		/* Pen is modified */
  FontObj	font;			/* Currently mounted font */
  WsFont	wsf;			/* Window System Font reference */
  Any		device;			/* XPCE device in use */
  DisplayObj	display;		/* The XPCE display */

  struct
  { RECT	orect;			/* old clipping rect */
  } clip_stack[MAX_CLIP_DEPTH];
  int		clip_depth;		/* #entries on clip stack */

} wdraw_context, *WDrawContext;


static int		cache = 1;	/* Do or don't */
static int		quick;		/* Prefer speed */
static wdraw_context	context; 	/* current context */
static wdraw_context	ctx_stack[10];  /* Context stack */
static int		ctx_stacked;	/* Saved frames */

static HDC		default_hdc;	/* Default context */
static DisplayObj	TheDisplay;	/* @display */

static void	r_update_pen(void);	/* Update the pen context */

static void
reset_context()
{ context.fill_pattern   = WHITE_IMAGE;
  context.font           = NIL;
  context.thickness      = 1;
  context.texture        = NAME_none;
  context.colour         = BLACK_COLOUR;
  context.background     = WHITE_COLOUR;	/* is this true? */
  context.rgb	         = RGB(0,0,0);
  context.background_rgb = RGB(255, 255, 255);
  context.hwnd	         = 0;
  context.hbitmap        = 0;
  context.modified_pen   = FALSE;
  context.open	         = 0;
  context.hdc	         = default_hdc;
  context.display        = TheDisplay;
  context.cache	         = 0;
}


void
initDraw()
{ if ( !default_hdc )
    default_hdc = CreateCompatibleDC(NULL);
  if ( !TheDisplay )
    TheDisplay = CurrentDisplay(NIL);

  resetDraw();
}


void
resetDraw()
{ context.open  = 0;
  ctx_stacked   = 0;

  reset_context();
}


void
d_offset(int x, int y)
{ DEBUG(NAME_cache, printf("d_offset(%d, %d)\n", x, y));

  x = -x;
  y = -y;

  if ( context.cache )
  { SetWindowOrg(context.cached_hdc, x, y);
  } else
    SetWindowOrg(context.hdc, x, y);
}


DisplayObj
d_display(DisplayObj d)
{ DisplayObj old = context.display;

  if ( isDefault(d) )
    d = CurrentDisplay(NIL);

  if ( context.display != d )
  { openDisplay(d);
    context.display = d;
  }

  quick = (d->quick_and_dirty == ON);

  return old;
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

      context.background_rgb = (COLORREF) getXrefObject(sw->background,
							getDisplayWindow(sw));
      context.cached_hdc     = context.hdc;
      context.cache_x        = a->x;
      context.cache_y        = a->y;
      context.cache_w        = a->w + 1;
      context.cache_h        = a->h + 1;
      context.cache	     = CreateCompatibleBitmap(context.hdc,
						      context.cache_w,
						      context.cache_h);
      context.hdc            = CreateCompatibleDC(context.hdc);
      context.cache_ohbitmap = SelectObject(context.hdc, context.cache);

      rect.left   = 0;
      rect.top    = 0;
      rect.right  = context.cache_w;
      rect.bottom = context.cache_h;
      hbrush = CreateSolidBrush(context.background_rgb);
      FillRect(context.hdc, &rect, hbrush);
      DeleteObject(hbrush);

      SetViewportOrg(context.hdc, -context.cache_x, -context.cache_y);

      DEBUG(NAME_cache, printf("Created cache for %d %d %d %d\n",
			       context.cache_x, context.cache_y,
			       context.cache_w, context.cache_h));
    }

    r_background(sw->background);
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

    context.hwnd           = getHwndWindow(sw);
    context.hdc            = BeginPaint(context.hwnd, &context.ps);
    context.default_colour = sw->colour;
    context.background	   = sw->background;

    if ( clear )
      r_clear(x, y, w, h);
  }
}


static void
push_context()
{ if ( context.open )
    ctx_stack[ctx_stacked++] = context;

  reset_context();
}


void
d_image(Image i, int x, int y, int w, int h)
{ Colour background;

  push_context();

  DEBUG(NAME_redraw, printf("d_image(%s, %d, %d, %d, %d)\n",
			    pp(i), x, y, w, h));

  context.open++;
  d_display(notNil(i->display) ? i->display : DEFAULT);
  context.device = i;
  
  context.hbitmap  = (HBITMAP) getXrefObject(i, context.display);
  context.hdc      = CreateCompatibleDC(NULL);
  context.ohbitmap = SelectObject(context.hdc, context.hbitmap);
  context.hrgn     = CreateRectRgn(x, y, x+w, y+h);
  context.ohrgn    = SelectObject(context.hdc, context.hrgn);

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
  r_background(background);
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

  r_background(bg);
  r_default_colour(fg);
}


void
d_clip(int x, int y, int w, int h)
{ if ( context.clip_depth < MAX_CLIP_DEPTH )
  { HRGN hrgn;
    RECT *rect = &context.clip_stack[context.clip_depth].orect;

    GetClipBox(context.hdc, rect);

    DEBUG(NAME_clip, { printf("d_clip(%d %d %d %d): ", x, y, w, h);
		       printf("ClipBox = %d %d %d %d --> ",
			      rect->left, rect->top,
			      rect->right - rect->left,
			      rect->bottom - rect->top);
		       fflush(stdout);
		     });
    
    if ( context.cache )
    { x -= context.cache_x;
      y -= context.cache_y;
    } else if ( context.hwnd )
    { POINT offset;

      GetWindowOrgEx(context.hdc, &offset);
      x -= offset.x;
      y -= offset.y;
    }

    hrgn = CreateRectRgn(x, y, x+w, y+h);
    SelectObject(context.hdc, hrgn);
    DeleteObject(hrgn);

    DEBUG(NAME_clip, { RECT nrect;
		       GetClipBox(context.hdc, &nrect);
		       printf("%d %d %d %d\n",
			      nrect.left, nrect.top,
			      nrect.right - rect->left,
			      nrect.bottom - rect->top);
		     });

    context.clip_depth++;
  } else
    sysPce("Too many levels of clipping");
}


void
d_done(void)
{ if ( --context.open == 0 )
  { DEBUG(NAME_redraw, printf("d_done(%s)\n",
			      context.hwnd ?
			        pp(GetWindowLong(context.hwnd, GWL_DATA)) :
			        "(image)"));

    if ( context.hbrush )
    { SelectObject(context.hdc, GetStockObject(WHITE_BRUSH));
      DeleteObject(context.hbrush);
      context.hbrush = 0;
    }
    if ( context.hpen )
    { SelectObject(context.hdc, GetStockObject(BLACK_PEN));
      DeleteObject(context.hpen);
      context.hpen = 0;
    }

    if ( instanceOfObject(context.device, ClassWindow) )
    { if ( context.cache )
      { DEBUG(NAME_cache, printf("Writing cache to window\n"));
	SetViewportOrg(context.hdc, 0, 0);
	BitBlt(context.cached_hdc,
	       context.cache_x, context.cache_y,
	       context.cache_w, context.cache_h,
	       context.hdc, 0, 0, SRCCOPY);
	SelectObject(context.hdc, context.cache_ohbitmap);
	DeleteObject(context.cache);
	DeleteDC(context.hdc);
      }
      EndPaint(context.hwnd, &context.ps);
    } else if ( instanceOfObject(context.device, ClassImage) )
    { SelectObject(context.hdc, context.ohrgn);
      SelectObject(context.hdc, context.ohbitmap);
      DeleteObject(context.hrgn);
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

  if ( context.clip_depth-- < 0 )
    sysPce("Clip stack underfow!");

  rect = &context.clip_stack[context.clip_depth].orect;
  if ( context.cache )
  { rect->left   -= context.cache_x;	/* viewport origin of the cache! */
    rect->right  -= context.cache_x;
    rect->top    -= context.cache_y;
    rect->bottom -= context.cache_y;
  } else if ( context.hwnd )
  { POINT offset;

    GetWindowOrgEx(context.hdc, &offset);
    rect->left   -= offset.x;
    rect->right  -= offset.x;
    rect->top    -= offset.y;
    rect->bottom -= offset.y;
  }
  hrgn = CreateRectRgnIndirect(rect);
  SelectObject(context.hdc, hrgn);
  DeleteObject(hrgn);

  DEBUG(NAME_clip, { RECT nrect;
		     GetClipBox(context.hdc, &nrect);
		     printf("d_clip_done: --> %d %d %d %d\n",
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
{ COLORREF rgb = (COLORREF) getXrefObject(context.background,
					  context.display);
  HBRUSH hbrush = CreateSolidBrush(rgb);
  RECT rect;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  FillRect(context.hdc, &rect, hbrush);
  DeleteObject(hbrush);
}


void
r_complement(int x, int y, int w, int h)
{ RECT rect;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  InvertRect(context.hdc, &rect);
/*BitBlt(context.hdc, x, y, w, h, context.hdc, 0, 0, DSTINVERT);*/
}


void
r_and(int x, int y, int w, int h, Image pattern)
{ HBITMAP bm = (HBITMAP) getXrefObject(pattern, context.display);
  HBRUSH brush = CreatePatternBrush(bm);
  HBRUSH obrush = SelectObject(context.hdc, brush);

  PatBlt(context.hdc, x, y, w, h, 0xFA0089); /* P|D */

  SelectObject(context.hdc, obrush);
  DeleteObject(brush);
}


static void
r_update_pen()
{ if ( context.modified_pen )
  { HPEN old = context.hpen;
    int style;

    if ( context.texture == NAME_none )
      style = PS_SOLID;
    else if ( context.texture == NAME_dotted )
      style = PS_DOT;
    else if ( context.texture == NAME_dashed )
      style = PS_DASH;
    else if ( context.texture == NAME_dashdot )
      style = PS_DASHDOT;
    else if ( context.texture == NAME_dashdotted )
      style = PS_DASHDOTDOT;
    else if ( context.texture == NAME_longdash )
      style = PS_DASH;		/* not supported */
    
				/* TBD: colour (only back for now) */
    context.hpen = CreatePen(style, context.thickness, context.rgb);
    SelectObject(context.hdc, context.hpen);

    if ( old )
      DeleteObject(old);

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


Colour
r_colour(Colour colour)
{ Colour old = context.colour;

  DEBUG(NAME_colour, printf("r_colour(%s)\n", pp(colour)));

  if ( isDefault(colour) )
  { assert(notDefault(context.default_colour));
    colour = context.default_colour;
  }

  if ( context.colour != colour )
  { context.modified_pen = TRUE;
    context.colour       = colour;
    context.rgb		 = (COLORREF) getXrefObject(colour, context.display);
    SetTextColor(context.hdc, context.rgb);
  }

  return old;
}


static HBRUSH
r_fillbrush(Any fill, int *stock)
{ HBRUSH hbrush;
  int s;

  if ( isNil(fill) )
  { hbrush = GetStockObject(NULL_BRUSH);
    s = TRUE;
  } else if ( instanceOfObject(fill, ClassImage) )
  { HBITMAP bm = (HBITMAP) getXrefObject(fill, context.display);     

    hbrush = CreatePatternBrush(bm);
    s = FALSE;
  } else /* instanceOfObject(fill, ClassColour) */
  { COLORREF rgb = (COLORREF) getXrefObject(fill, context.display);

    hbrush = CreateSolidBrush(rgb);
    s = FALSE;
  }

  if ( stock )
    *stock = s;

  return hbrush;
}


void
r_fillpattern(Any fill)			/* colour or image */
{ if ( context.fill_pattern != fill )
  { HBRUSH new, old = context.hbrush;
    int stock;    
    
    DEBUG(NAME_redraw, printf("Selecting fill-pattern %s\n", pp(fill)));
    new = r_fillbrush(fill, &stock);
    SelectObject(context.hdc, new);
    if ( !stock )
      context.hbrush = new;

    if ( old )
      DeleteObject(old);

    context.fill_pattern = fill;
  }
}


void
r_arcmode(Name mode)
{					/* handled by r_msarc() itself */
}


Colour
r_default_colour(Colour c)
{ Colour old = context.default_colour;
  
  DEBUG(NAME_colour, printf("r_default_colour(%s)\n", pp(c)));
  if ( notDefault(c) )
    context.default_colour = c;

  assert(notDefault(context.default_colour));
  r_colour(context.default_colour);
  
  return old;
}


static void
r_background(Colour c)
{ if ( context.background != c )
  { COLORREF rgb = (COLORREF) getXrefObject(c, context.display);
    
    SetBkColor(context.hdc, rgb);
    context.background     = c;
    context.background_rgb = rgb;
  }
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
{ int da = context.thickness / 2;
  int db = max(0, (context.thickness - 1) / 2);
  
  DEBUG(NAME_redraw, printf("r_box(%d, %d, %d, %d, %d, %s)\n",
			    x, y, w, h, r, pp(fill)));

  DEBUG(NAME_pen, printf("context.thickness = %d\n", context.thickness));
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


void
r_msarc(int x, int y, int w, int h,	/* bounding box */
	int sx, int sy,			/* starting point */
	int ex, int ey,			/* end point */
	Name close,			/* none,pie_slice,chord */
	Image fill)			/* @nil or fill pattern */
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
r_ellipse(int x, int y, int w, int h, Image fill)
{ r_fillpattern(fill);
  r_update_pen();

  DEBUG(NAME_redraw, printf("r_ellipse(%d, %d, %d, %d, %s)\n",
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
  { printf("radius > 0 not yet implemented (r_path())\n");
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
	printf("r_op_image(%s, %d, %d, %d, %d, %d, %d, %s) "
	       "(bm=0x%x, mhdc=0x%x)\n",
	       pp(image), sx, sy, x, y, w, h, pp(op), (long)bm, (long)mhdc));
  obm = SelectObject(mhdc, bm);
  BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, rop);
  if ( op == NAME_xor )
    BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, DSTINVERT);
  SelectObject(mhdc, obm);
  DeleteDC(mhdc);
}


					/* TBD: add transparent arg */

void
r_image(Image image, int sx, int sy, int x, int y, int w, int h)
{ HBITMAP bm = (HBITMAP) getXrefObject(image, context.display);
  HDC mhdc = CreateCompatibleDC(context.hdc);
  HBITMAP obm;

  DEBUG(NAME_redraw,
	printf("r_image(%s, %d, %d, %d, %d, %d, %d) (bm=0x%x, mhdc=0x%x)\n",
	       pp(image), sx, sy, x, y, w, h, (long)bm, (long)mhdc));
  obm = SelectObject(mhdc, bm);
  BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, SRCCOPY);
  SelectObject(mhdc, obm);
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
  int stock;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  hbrush = r_fillbrush(pattern, &stock);
  FillRect(context.hdc, &rect, hbrush);
  if ( !stock )
    DeleteObject(hbrush);
}


void
r_fill_polygon(IPoint pts, int n)
{ POINT *points = alloca(sizeof(POINT) * n);
  HPEN oldpen = SelectObject(context.hdc, GetStockObject(NULL_PEN));
  int i;

  for(i=0; i<n; i++)
  { points[i].x = pts[i].x;
    points[i].y = pts[i].y;
  }

  Polygon(context.hdc, points, n);

  SelectObject(context.hdc, oldpen);
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
{
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
    c = (COLORREF) getXrefObject(val, context.display);

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
  { WsFont wsf = getXrefObject(font, context.display);
  
    DEBUG(NAME_font, printf("s_font(%s) (hfont = 0x%x)\n",
			    pp(font), (int)wsf->hfont));
    context.wsf = wsf;
    SelectObject(context.hdc, wsf->hfont);
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
{ printf("16-bits characters are not (yet) supported on XPCE for Windows\n");
}


void
str_size(String s, FontObj font, int *width, int *height)
{ if ( s->size > 0 )
  { RECT rect;
    UINT flags = DT_CALCRECT|DT_EXTERNALLEADING|DT_NOCLIP|DT_NOPREFIX;

    rect.left   = 0;
    rect.top    = 0;
    rect.right  = 0;
    rect.bottom = 0;

    s_font(font);
    DrawText(context.hdc, s->s_text8, s->size, &rect, flags);

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
  DEBUG(NAME_redraw, printf("str_text(%s, %s, %d, %d, %d, %d, %s, %s)\n",
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


