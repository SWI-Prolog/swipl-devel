/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

#ifdef __WIN32__			/* timer broken in Win95 */
#define SCROLLTIMER_USE_SLEEP 1
#endif

#define OPENLOOK 1
#define O_MOTIF 1

#if OPENLOOK
#define BOXHEIGHT	6		/* boxes at top/bottom */
#define BAR_WIDTH	3		/* Width of bar-line */
#define BOX_MARGIN	2		/* Box to bar-line */
#define BUTTON_HEIGHT  15		/* Height of (1/3) button */
static  int LastOffset;			/* pointer-warping */
#endif

#define ARROW_LEFT 0			/* arrow directions */
#define ARROW_RIGHT 1
#define ARROW_UP 2
#define ARROW_DOWN 3

#define swap(x, y)	{ int z; z=x; x=y; y=z; }
#define swapInt(x, y)	{ Int z; z=x; x=y; y=z; }
#define BOUNDS(n, l, h) ((n) > (h) ? (h) : (n) < (l) ? (l) : (n))
#define MIN_BUBBLE 6			/* smallest variable bubble */

#define Repeating(sb) ((sb)->status == NAME_repeatDelay || \
		       (sb)->status == NAME_repeat)

typedef struct bubble_info *BubbleInfo;
typedef struct sb_draw_data *SbDrawData;

struct bubble_info
{ int	start;				/* start of bubble */
  int	length;				/* lenght of bubble */
  int	bar_start;			/* start of the bar */
  int   bar_length;			/* length of the bar */
};

struct sb_draw_data
{ int x, y, w, h;			/* from initialiseDeviceGraphical() */
  int vertical;				/* true if vertical bar */
  int arrow;				/* height of arrows */
  struct bubble_info bubble;		/* bubble info */
};


static void	compute_bubble(ScrollBar, struct bubble_info *,
			       int start, int min_bubble,
			       int fixed_bubble);
static status	orientationScrollBar(ScrollBar s, Name or);
static status	lookScrollBar(ScrollBar s, Name look);
static status   OpenLookRedrawAreaScrollBar(ScrollBar s, Area a);
static int	offset_event_scrollbar(ScrollBar s, EventObj ev);
static status	OpenLookEventScrollBar(ScrollBar s, EventObj ev);
static status	MotifEventScrollBar(ScrollBar s, EventObj ev);
static status	forwardScrollBar(ScrollBar s);
static Int	promilage_event_scrollbar(ScrollBar s, EventObj ev);
static status	detachTimerScrollBar(ScrollBar s);

static status
initialiseScrollBar(ScrollBar s, Any obj, Name orientation, Message msg)
{ Int w = getResourceValueObject(s, NAME_width);

  if ( !w || isDefault(w) )
    w = ws_default_scrollbar_width();

  initialiseGraphical(s, ZERO, ZERO, w, toInt(100));
  assign(s, orientation,   NAME_vertical);

  assign(s, look,	   DEFAULT);
  assign(s, placement,	   DEFAULT);
  assign(s, distance,	   DEFAULT);
  assign(s, pen,	   DEFAULT);
  
  assign(s, view,	   ZERO);	/* length of view */
  assign(s, start,	   ZERO);	/* position in object */
  assign(s, length,	   ZERO);	/* length of scrollable object */

  assign(s, bubble_start,  toInt(-1));
  assign(s, bubble_length, toInt(-1));

  assign(s, message, 	   msg);
  assign(s, object,	   obj);

  assign(s, drag,	   ON);
  assign(s, amount,	   ZERO);
  assign(s, direction,	   NAME_forwards);
  assign(s, unit,	   NAME_file);
  assign(s, status,	   NAME_inactive);

  obtainResourcesObject(s);
  if ( orientation == NAME_horizontal )
    orientationScrollBar(s, orientation);

  requestComputeGraphical(s, DEFAULT);
  
  succeed;
}


		/********************************
		*            COMPUTE		*
		********************************/

static int
arrow_height_scrollbar(ScrollBar sb)
{ if ( sb->look == NAME_motif || sb->look == NAME_win )
  { return valInt(sb->orientation == NAME_vertical ? sb->area->w
		  				   : sb->area->h);
  }
  
  return 0;
}


static status
ComputeScrollBar(ScrollBar sb)
{ if ( notNil(sb->request_compute) )
  { struct bubble_info bi;
    int arrow = arrow_height_scrollbar(sb);
    
    compute_bubble(sb, &bi, arrow, MIN_BUBBLE, FALSE);
    if ( valInt(sb->bubble_start) != bi.start ||
	 valInt(sb->bubble_length) != bi.length )
    { DEBUG(NAME_scroll, Cprintf("%s: start %ld --> %d; length %ld --> %d\n",
				 pp(sb), valInt(sb->bubble_start), bi.start,
				 valInt(sb->bubble_length), bi.length));
      assign(sb, bubble_start, toInt(bi.start));
      assign(sb, bubble_length, toInt(bi.length));

#if OPENLOOK
      if ( sb->look == NAME_openLook && Repeating(sb) )
      { struct bubble_info button_bi;

	compute_bubble(sb, &button_bi,
		       BOXHEIGHT+BOX_MARGIN, BUTTON_HEIGHT*3, TRUE);

	assign(sb, request_compute, NIL);	/* avoid loop */

	if ( sb->unit == NAME_line )
	{ int y;

	  if ( sb->direction == NAME_backwards )
	    y = button_bi.start + BUTTON_HEIGHT/2;
	  else
	    y = button_bi.start + BUTTON_HEIGHT*2 + BUTTON_HEIGHT/2;

	  if ( sb->orientation == NAME_vertical )
	    pointerGraphical((Graphical) sb,
			     answerObject(ClassPoint,
					  div(sb->area->w, TWO),
					  toInt(y), 0));
	  else
	    pointerGraphical((Graphical) sb,
			     answerObject(ClassPoint,
					  toInt(y),
					  div(sb->area->h, TWO), 0));
	} else if ( sb->unit == NAME_page )
	{ int y = -1;

	  if ( sb->direction == NAME_backwards &&
	       LastOffset >= button_bi.start )
	    y = button_bi.start - 1;
	  else if ( sb->direction == NAME_forwards &&
		    LastOffset <= button_bi.start + button_bi.length )
	    y = button_bi.start + button_bi.length + 1;

	  if ( y > 0 )
	  { if ( sb->orientation == NAME_vertical )
	      pointerGraphical((Graphical) sb,
			       answerObject(ClassPoint,
					  div(sb->area->w, TWO),
					  toInt(y), 0));
	    else
	      pointerGraphical((Graphical) sb,
			     answerObject(ClassPoint,
					  toInt(y),
					  div(sb->area->h, TWO), 0));

	    LastOffset = y;
	  }
	}

/*	assign(sb, status, NAME_inactive); */
      }
#endif /*OPENLOOK*/
      CHANGING_GRAPHICAL(sb, changedEntireImageGraphical(sb));
    }

    assign(sb, request_compute, NIL);
  }

  succeed;
}


Int
getMarginScrollBar(ScrollBar sb)
{ if ( sb->orientation == NAME_horizontal )
  { if ( memberChain(sb->placement, NAME_bottom) )
      answer(add(sb->area->h, sb->distance));
    else
      answer(minInt(add(sb->area->h, sb->distance)));
  } else				/* vertical */
  { if ( memberChain(sb->placement, NAME_right) )
      answer(add(sb->area->w, sb->distance));
    else
      answer(minInt(add(sb->area->w, sb->distance)));
  }
}


status
placeScrollBar(ScrollBar sb, Graphical gr)
{ if ( isDefault(gr) )
    gr = sb->object;

  if ( instanceOfObject(gr, ClassGraphical) )
  { if ( sb->orientation == NAME_horizontal )
    { setGraphical(sb,
		   gr->area->x,
		   memberChain(sb->placement, NAME_bottom)
		       ? add(gr->area->y, add(gr->area->h, sb->distance))
		       : sub(gr->area->y, add(sb->area->h, sb->distance)),
		   gr->area->w,
		   DEFAULT);
    } else				/* vertical */
    { setGraphical(sb,
		   memberChain(sb->placement, NAME_right)
		        ? add(gr->area->x, add(gr->area->w, sb->distance))
		        : sub(gr->area->x, add(sb->area->w, sb->distance)),
		   gr->area->y,
		   DEFAULT,
		   gr->area->h);
    }
  }

  succeed;
}


static status
computeScrollBar(ScrollBar sb)
{ if ( notNil(sb->request_compute) )
  { if ( hasSendMethodObject(sb->object, NAME_bubbleScrollBar) )
      send(sb->object, NAME_bubbleScrollBar, sb, 0);
    else if ( hasGetMethodObject(sb->object, NAME_start) &&
	      hasGetMethodObject(sb->object, NAME_view) &&
	      hasGetMethodObject(sb->object, NAME_length) )
    { Int s = getv(sb->object, NAME_start, 0, NULL);
      Int v = getv(sb->object, NAME_view, 0, NULL);
      Int l = getv(sb->object, NAME_length, 0, NULL);

      if ( s != FAIL && v != FAIL && l != FAIL )
	bubbleScrollBar(sb, l, s, v);

    }

    ComputeScrollBar(sb);
  }

  succeed;
}


static void
compute_bubble(ScrollBar s, struct bubble_info *bi,
	       int bar_start, int min_bubble, int fixed_bubble)
{ int len   = valInt(s->length);
  int start = (valInt(s->start) > len ? len : valInt(s->start));
  int view  = valInt(s->view);

					/* bar sizes */
  bi->bar_start   = bar_start;
  bi->bar_length  = (equalName(s->orientation, NAME_vertical)
		     ? valInt(s->area->h)
		     : valInt(s->area->w));
  bi->bar_length -= 2 * bi->bar_start;

					/* bubble characteristics */
  if ( fixed_bubble )
  { int free, below, above;

    if ( bi->bar_length < min_bubble )
    { bi->bar_length += 2 * bi->bar_start;
      bi->bar_start = 0;

      if ( bi->bar_length < min_bubble )
	min_bubble = bi->bar_length;
    }

    bi->length = min_bubble;
    free  = bi->bar_length - bi->length;
    below = len - (start+view);
    above = start;

    if ( (above + below) <= 0 )
      bi->start = 0;
    else
      bi->start = (free * above) / (above + below);
  } else
  { int bubble_prom = (len != 0 ? ((1000 * start) / len) : 0);
    int bubble_lp   = (len != 0 ? ((1000 * valInt(s->view)) / len) : 1000);

    bi->length = (bi->bar_length * bubble_lp)/1000 + min_bubble;
    bi->start  = ((bi->bar_length * bubble_prom) / 1000) - min_bubble/2;
  }

  bi->start    = BOUNDS(bi->start, 0, bi->bar_length-min_bubble);
  bi->start   += bi->bar_start;
  bi->length   = BOUNDS(bi->length, 0,
			(bi->bar_length + bi->bar_start - bi->start));
}


		 /*******************************
		 *		REDRAW		*
		 *******************************/

static void
sb_init_draw_data(ScrollBar s, Area a, SbDrawData d)
{ initialiseDeviceGraphical(s, &d->x, &d->y, &d->w, &d->h);
  NormaliseArea(d->x, d->y, d->w, d->h);

  d->vertical = (s->orientation == NAME_vertical);
  d->arrow = arrow_height_scrollbar(s);

  compute_bubble(s, &d->bubble, d->arrow, MIN_BUBBLE, FALSE);
}

#define X1 pts[0]
#define Y1 pts[1]
#define X2 pts[2]
#define Y2 pts[3]
#define X3 pts[4]
#define Y3 pts[5]

static void
compute_arrow_corners(int x, int y, int w, int h,
		      int which,
		      int aw, int ah,
		      int *pts)
{ switch(which)
  { case ARROW_DOWN:
    { int dx = (w-aw)/2;
      int dy = (h-ah)/2;
      
      X1 = x+dx; Y1 = y+dy; X2 = x+w-dx; Y2 = Y1; X3 = (X1+X2)/2, Y3 = y+h-dy;
      break;
    }
    case ARROW_UP:
    { int dx = (w-aw)/2;
      int dy = (h-ah)/2;

      X1 = x+dx; Y1 = y+h-dy; X2 = x+w-dx; Y2 = Y1; X3 = (X1+X2)/2, Y3 = y+dy;
      break;
    }
    case ARROW_LEFT:
    { int dx = (w-ah)/2;
      int dy = (h-aw)/2;
      
      X1 = x+w-dx, Y1 = y+dy; X2 = X1; Y2 = y+h-dy; X3 = x+dx; Y3 = (Y1+Y2)/2;
      break;
    }
    case ARROW_RIGHT:
    { int dx = (w-ah)/2;
      int dy = (h-aw)/2;

      X1 = x+dx, Y1 = y+dy; X2 = X1; Y2 = y+h-dy; X3=x+w-dx; Y3 = (Y1+Y2)/2;
    }
  }
  DEBUG(NAME_arrow, Cprintf("arrow(%d, %d, %d, %d, %d, %d)\n",
			    X1, Y1, X2, Y2, X3, Y3));
}


static Elevation
getElevationScrollBar(ScrollBar s)
{ Elevation z = getResourceValueObject(s, NAME_elevation);

					/* TBD: make default one */
  return z;
}


static void
draw_arrow(ScrollBar s, int x, int y, int w, int h, int which, int up)
{ Elevation z = getElevationScrollBar(s);

  DEBUG(NAME_arrow, Cprintf("Arrow box(%d, %d, %d, %d)\n", x, y, w, h));

  if ( s->look == NAME_win )
  { int pts[6];

    r_thickness(valInt(s->pen));

    if ( up )
      r_3d_box(x, y, w, h, 0, z, TRUE);
    else
      r_box(x, y, w, h, 0, isDefault(z->colour) ? NIL : (Any) z->colour);

    compute_arrow_corners(x, y, w, h, which, w/2, h/4, pts);
    r_fillpattern(BLACK_COLOUR);
    r_fill_triangle(X1, Y1, X2, Y2, X3, Y3);
  } else				/* motif */
  {
  }
}

#undef X1
#undef Y1
#undef X2
#undef Y2
#undef X3
#undef Y3


static void
draw_arrows(ScrollBar s, SbDrawData d)
{ int faup = TRUE;			/* first-arrow-up */
  int saup = TRUE;			/* second-arrow-up */
  int ah = d->arrow-1;

  if ( Repeating(s) && s->unit == NAME_line )
  { if ( s->direction == NAME_forwards )
      saup = FALSE;
    else
      faup = FALSE;
  }

  if ( d->vertical )
  { draw_arrow(s, d->x, d->y, d->w, ah, ARROW_UP, faup);
    draw_arrow(s, d->x, d->y + d->h - ah, d->w, ah, ARROW_DOWN, saup);
  } else
  { draw_arrow(s, d->x, d->y, ah, d->h, ARROW_LEFT, faup);
    draw_arrow(s, d->x + d->w-ah, d->y, ah, d->h, ARROW_RIGHT, saup);
  }
}


static void
draw_bubble(ScrollBar s, SbDrawData d)
{ int p = valInt(s->pen);
  Elevation z = getResourceValueObject(s, NAME_elevation);
  int x = d->x, y = d->y, w = d->w, h = d->h;
  BubbleInfo bi = &d->bubble;
  int pf=FALSE, pb=FALSE;

  if ( s->look == NAME_win &&
       Repeating(s) &&
       s->unit == NAME_page )
  { if ( s->direction == NAME_forwards )
      pf = TRUE;
    else
      pb = TRUE;
  }

  if ( d->vertical )
  { int ym, hm;

    x += p;
    w -= 2*p;

    ym = y+bi->bar_start; hm = bi->start - bi->bar_start;
    if ( pb )
      r_fill(x, ym, w, hm, BLACK_COLOUR);
    else
      r_clear(x, ym, w, hm);

    ym = y+bi->start;
    hm = bi->length;
    if ( instanceOfObject(z, ClassElevation) )
      r_3d_box(x, ym, w, hm, 0, z, TRUE);
    else
      r_fill(x, ym, w, hm, GREY50_IMAGE);

    ym += hm;
    hm = (bi->bar_start+bi->bar_length) - (bi->start+bi->length);
    if ( pf )
      r_fill(x, ym, w, hm, BLACK_COLOUR);
    else
      r_clear(x, ym, w, hm);
  } else /* if ( equalName(s->orientation, NAME_horizontal) ) */
  { int xm, wm;

    y += p;
    h -= 2*p;

    xm = x+bi->bar_start; wm = bi->start - bi->bar_start;
    if ( pb )
      r_fill(xm, y, wm, h, BLACK_COLOUR);
    else
      r_clear(xm, y, wm, h);

    xm = x+bi->start;
    wm = bi->length;
    if ( instanceOfObject(z, ClassElevation) )
      r_3d_box(x+xm, y, wm, h, 0, z, TRUE);
    else
      r_fill(x+xm, y, wm, h, GREY50_IMAGE);

    xm += wm;
    wm = (bi->bar_start+bi->bar_length) - (bi->start+bi->length);
    if ( pf )
      r_fill(xm, y, wm, h, BLACK_COLOUR);
    else
      r_clear(xm, y, wm, h);
  }
}


status
RedrawAreaScrollBar(ScrollBar s, Area a)
{ Any bg = getResourceValueObject(s, NAME_background);
  Any obg = NIL;

  if ( bg && (instanceOfObject(bg, ClassColour) ||
	      instanceOfObject(bg, ClassPixmap)) )
    obg = r_background(bg);

  if ( s->look == NAME_openLook )
    OpenLookRedrawAreaScrollBar(s, a);
  else
  { struct sb_draw_data d;

    sb_init_draw_data(s, a, &d);
    if ( d.arrow )
      draw_arrows(s, &d);
    draw_bubble(s, &d);
  }

  if ( notNil(obg) )
    r_background(obg);

  return RedrawAreaGraphical(s, a);
}


#if OPENLOOK

static Timer	ScrollBarRepeatTimer;
static Message  ScrollBarRepeatMessage;

static Timer
scrollBarRepeatTimer()
{ if ( !ScrollBarRepeatTimer )
    ScrollBarRepeatTimer = globalObject(NAME_scrollBarRepeatTimer,
					ClassTimer, CtoReal(0.08),
					( ScrollBarRepeatMessage =
					  newObject(ClassMessage, NIL,
						    NAME_repeat, 0)), 0);
  return ScrollBarRepeatTimer;
}


static status
changedBubbleScrollBar(ScrollBar s)
{ if ( s->look == NAME_openLook )
  { struct bubble_info button_bi;
    compute_bubble(s, &button_bi,
		   BOXHEIGHT+BOX_MARGIN, BUTTON_HEIGHT*3, TRUE);

    if ( s->orientation == NAME_vertical )
      changedImageGraphical(s, ZERO, toInt(button_bi.start),
			    s->area->w, toInt(button_bi.length));
    else
      changedImageGraphical(s, toInt(button_bi.start), ZERO,
			    toInt(button_bi.length), s->area->h);
  }

  succeed;
}


static status
repeatScrollBar(ScrollBar s)
{ again:

  if ( Repeating(s) )
  { unsigned long clk = mclock();

    if ( s->unit == NAME_page )
    { if ( s->direction == NAME_backwards )
      { if ( valInt(s->start) <= 0 )
	{ detachTimerScrollBar(s);
	  succeed;
	}
      } else if ( valInt(s->view) + valInt(s->start) >= valInt(s->length) )
      { detachTimerScrollBar(s);
	succeed;
      }
    }
    forwardScrollBar(s);
    synchroniseGraphical((Graphical) s, ON);
    if ( Repeating(s) )		/* synchroniseGraphical() can handle up */
    { Real t = getResourceValueObject(s, NAME_repeatInterval);
      int ct = (int)(t->value * 1000.0) - (float)(mclock() - clk);

      assign(s, status, NAME_repeat);

      if ( ct > 5 )
      {
#ifdef SCROLLTIMER_USE_SLEEP
	msleep(ct);
	goto again;
#else
        Timer tmr = scrollBarRepeatTimer();

	intervalTimer(tmr, CtoReal((float)ct / 1000.0));
	statusTimer(tmr, NAME_once);
#endif
      } else
	goto again;
    }
  }
  
  succeed;
}


static status
detachTimerScrollBar(ScrollBar s)
{ if ( ScrollBarRepeatMessage && ScrollBarRepeatMessage->receiver == s )
  { stopTimer(ScrollBarRepeatTimer);
    assign(ScrollBarRepeatMessage, receiver, NIL);

    succeed;
  }

  fail;
}


static void
attachTimerScrollBar(ScrollBar s)
{ Timer t = scrollBarRepeatTimer();

  detachTimerScrollBar(s);
  intervalTimer(t, getResourceValueObject(s, NAME_repeatDelay));
  assign(ScrollBarRepeatMessage, receiver, s);
  startTimer(t, NAME_once);
}
  

static status
unlinkScrollBar(ScrollBar s)
{ detachTimerScrollBar(s);

  return unlinkGraphical((Graphical) s);
}


static void
sb_box(int x, int y, int w, int h,
       int vertical, Elevation z, int use_shadow,
       int active)
{ if ( !vertical )
  { swap(x, y);
    swap(w, h);
  }

  if ( !z )
  { if ( use_shadow )
      r_shadow_box(x, y, w, h, 0, 1, active ? BLACK_IMAGE : NIL);
    else
      r_box(x, y, w, h, 0, active ? BLACK_IMAGE : NIL);
  } else
  { r_3d_box(x, y, w, h, 0, z, active ? FALSE : TRUE);
  }
}


static void
sb_triangle(int x1, int y1,
	    int x2, int y2,
	    int x3, int y3,
	    int vertical, Elevation z,
	    int active)
{ if ( !vertical )
  { swap(x1, y1);
    swap(x2, y2);
    swap(x3, y3);
  }

  r_fillpattern((z || active) ? BLACK_IMAGE : GREY50_IMAGE);
  r_triangle(x1, y1, x2, y2, x3, y3);
}


static status
OpenLookRedrawAreaScrollBar(ScrollBar s, Area a)
{ int x, y, w, h;
  int p = valInt(s->pen);
  int shadow = 1;
  int am = 3;				/* arrow-margin */
  struct bubble_info bar_bi;
  struct bubble_info button_bi;
  struct iarea redraw;
  Elevation z = getResourceValueObject(s, NAME_elevation);
  int boxh = BOXHEIGHT;
  int boxm = BOX_MARGIN;

  if ( isNil(z) )
    z = NULL;

  initialiseRedrawAreaGraphical(s, a, &x, &y, &w, &h, &redraw);
  r_clear(redraw.x, redraw.y, redraw.w, redraw.h);
  r_thickness(p);
  r_dash(s->texture);

  compute_bubble(s, &button_bi,
		 boxh+boxm, BUTTON_HEIGHT*3, TRUE);
  compute_bubble(s, &bar_bi,
		 boxh+boxm, button_bi.length, FALSE);
  if ( button_bi.bar_start == 0 )
    boxh = 0;

#define Fill(x, y, w, h, f) \
	if ( vertical ) r_fill(x, y, w, h, f); else r_fill(y, x, h, w, f)
#define Clear(x, y, w, h) \
	if ( vertical ) r_clear(x, y, w, h); else r_clear(y, x, h, w)

  { int vertical = (s->orientation == NAME_vertical);
    int bx, cy, bh, ch, ch3, ch9, l1y, l2y;

    if ( !vertical )
    { swap(x, y);
      swap(w, h);
    }

    x += 1, w -= 2;			/* 1 pixel border */
    bx = x + (w-BAR_WIDTH+1)/2;		/* x of the bar */

    if ( boxh > 0 )
    { sb_box(x, y, w, boxh, vertical, z, FALSE,
	     s->status == NAME_topOfFile);
      sb_box(x, y+h-boxh, w, boxh, vertical, z, FALSE,
	     s->status == NAME_bottomOfFile);
    }
    
    cy = y + bar_bi.bar_start;		/* paint the bar */
    ch = y+bar_bi.start - cy;
    Fill(bx, cy, BAR_WIDTH, ch, GREY50_IMAGE);
    cy += ch;
    ch = bar_bi.length;
    Fill(bx, cy, BAR_WIDTH, ch, BLACK_IMAGE);
    cy += ch;
    ch = y + bar_bi.bar_start + bar_bi.bar_length - cy;
    Fill(bx, cy, BAR_WIDTH, ch, GREY50_IMAGE);

    cy = y+button_bi.start;		/* paint the button */
    bh = button_bi.length;
    Clear(x, cy-1, w, bh+2);
    sb_box(x, cy, w, bh, vertical, z, TRUE, FALSE);
    ch3 = bh/3;
    l1y = cy + ch3;
    l2y = cy + bh-ch3-shadow;

    sb_box(x, l1y-shadow, w, l2y-l1y+2*shadow, vertical, z, FALSE,
	   !z && s->status == NAME_dragging);
    if ( z )
    { if ( s->status == NAME_dragging )
      { int ew = w/2;
	int ex = x + (w-ew)/2;
	int ey = l1y + (l2y-l1y-ew)/2;

	if ( vertical )
	  r_3d_ellipse(ex, ey, ew, ew, z, FALSE);
	else
	  r_3d_ellipse(ey, ex, ew, ew, z, FALSE);
      } else if ( Repeating(s) && s->unit == NAME_line )
      { int by = s->direction == NAME_forwards ? l2y : cy;

	sb_box(x, by, w, l1y-cy, vertical, z, FALSE, TRUE);
      }
    }

    ch9 = ((ch3 * 3) / 10)+1;		/* ... and the arrows */
    sb_triangle(x+am, l1y-1-ch9,
		x+w-shadow-am-1, l1y-1-ch9,
		x+(w-shadow)/2, cy+ch9,
		vertical, z,
		s->start != ZERO);
    sb_triangle(x+am, l2y+1+ch9,
		x+w-shadow-am-1, l2y+1+ch9,
		x+(w-shadow)/2, cy+bh-shadow-ch9,
		vertical, z,
		valInt(s->start) + valInt(s->view) < valInt(s->length));

    if ( !z && Repeating(s) && s->unit == NAME_line )
    { int bx = x + shadow;
      int by = s->direction == NAME_forwards ? l2y : cy;
      int bh = l1y-cy;
      int bw = w - 2*shadow;

      if ( vertical )
	r_complement(bx, by, bw, bh);
      else
	r_complement(by, bx, bh, bw);
    }
  }

  succeed;
}


static status
OpenLookEventScrollBar(ScrollBar s, EventObj ev)
{ if ( isAEvent(ev, NAME_msLeftDown) )
  { int offset = offset_event_scrollbar(s, ev);
    int vertical = (s->orientation == NAME_vertical);
    Int w = s->area->w;
    Int h = s->area->h;
    struct bubble_info button_bi;
    compute_bubble(s, &button_bi,
		   BOXHEIGHT+BOX_MARGIN, BUTTON_HEIGHT*3, TRUE);


    if ( offset <= button_bi.bar_start )
    { assign(s, unit,      NAME_file);
      assign(s, direction, NAME_goto);
      assign(s, amount,    ZERO);
      assign(s, status,	   NAME_topOfFile);
      if ( vertical )
	changedImageGraphical(s, 0, 0, w, toInt(BOXHEIGHT));
      else
	changedImageGraphical(s, 0, 0, toInt(BOXHEIGHT), h);
    } else if ( offset >= button_bi.bar_start + button_bi.bar_length )
    { assign(s, unit,      NAME_file);
      assign(s, direction, NAME_goto);
      assign(s, amount,    toInt(1000));
      assign(s, status,	   NAME_bottomOfFile);
      if ( vertical )
	changedImageGraphical(s, 0, toInt(valInt(h)-BOXHEIGHT),
			      w, toInt(BOXHEIGHT));
      else
	changedImageGraphical(s, toInt(valInt(w)-BOXHEIGHT), 0, 
			      toInt(BOXHEIGHT), h);
    } else
    { if ( offset < button_bi.start )
      { assign(s, unit,      NAME_page);
	assign(s, direction, NAME_backwards);
	assign(s, amount,    toInt(990));
	assign(s, status,    NAME_repeatDelay);
      } else if ( offset > button_bi.start + button_bi.length )
      { assign(s, unit,      NAME_page);
	assign(s, direction, NAME_forwards);
	assign(s, amount,    toInt(990));
	assign(s, status,    NAME_repeatDelay);
      } else if ( offset < button_bi.start + button_bi.length/3 )
      { assign(s, unit,      NAME_line);
	assign(s, direction, NAME_backwards);
	assign(s, amount,    ONE);
	assign(s, status,    NAME_repeatDelay);
      } else if ( offset > button_bi.start + (button_bi.length*2)/3 )
      { assign(s, unit,      NAME_line);
	assign(s, direction, NAME_forwards);
	assign(s, amount,    ONE);
	assign(s, status,    NAME_repeatDelay);
      } else
      { assign(s, unit,      NAME_file);
	assign(s, direction, NAME_goto);
	assign(s, status,    NAME_dragging);
      }
      
      if ( s->status == NAME_repeatDelay )
      { LastOffset = offset; 
	attachTimerScrollBar(s);
      }

      changedBubbleScrollBar(s);
    }
  } else if ( isAEvent(ev, NAME_msLeftDrag) )
  { if ( s->status == NAME_dragging )
    { int offset = offset_event_scrollbar(s, ev);
      struct bubble_info button_bi;
      int prom;
      int bh;

      compute_bubble(s, &button_bi,
		     BOXHEIGHT+BOX_MARGIN,
		     BUTTON_HEIGHT*3, TRUE);
      bh = button_bi.length;
      if ( button_bi.bar_length > bh )
      { prom = ((offset - button_bi.bar_start - bh/2) * 1000) /
		   (button_bi.bar_length - bh);
	prom = BOUNDS(prom, 0, 1000);

	assign(s, amount, toInt(prom));
	forwardScrollBar(s);
      }
    }
  } else if ( isAEvent(ev, NAME_msLeftUp) )
  { DEBUG(NAME_scrollBar, Cprintf("left-up received\n"));

    if ( (s->unit != NAME_file && s->status != NAME_repeat) ||
	 s->status == NAME_topOfFile ||
	 s->status == NAME_bottomOfFile )
    { forwardScrollBar(s);
      if ( instanceOfObject(s->object, ClassGraphical) )
	ComputeGraphical(s->object);
      ComputeGraphical(s);
    }

    detachTimerScrollBar(s);
    assign(s, status, NAME_inactive);
    changedEntireImageGraphical(s);
#if 0
    if ( s->status != NAME_dragging )
    { forwardScrollBar(s);
      if ( instanceOfObject(s->object, ClassGraphical) )
	ComputeGraphical(s->object);
      ComputeGraphical(s);
    }
    assign(s, status, NAME_inactive);
    detachTimerScrollBar(s);
    changedEntireImageGraphical(s);	/* too much, but for now ok */
#endif
  } else
    fail;				/* other button/event */

  succeed;
}

#endif /*OPENLOOK*/

#ifdef O_MOTIF

static status
MotifEventScrollBar(ScrollBar s, EventObj ev)
{ if ( !isAEvent(ev, NAME_button) )
    fail;

  if ( isAEvent(ev, NAME_msLeft) )
  { if ( isAEvent(ev, NAME_msLeftDown) )
    { int offset = offset_event_scrollbar(s, ev);
      Int w = s->area->w;
      Int h = s->area->h;
      int vertical = (s->orientation == NAME_vertical);
      int ah = (vertical ? valInt(w) : valInt(h));
      int len = (vertical ? valInt(h) : valInt(w));
  
      if ( offset < ah )			/* line-up */
      { assign(s, unit,      NAME_line);
	assign(s, direction, NAME_backwards);
	assign(s, amount,    ONE);
	assign(s, status,    NAME_repeatDelay);
      } else if ( offset > len - ah )	/* line-down */
      { assign(s, unit,      NAME_line);
	assign(s, direction, NAME_forwards);
	assign(s, amount,    ONE);
	assign(s, status,    NAME_repeatDelay);
      } else				/* not on the arrows */
      { struct bubble_info bi;
  
	compute_bubble(s, &bi, ah, MIN_BUBBLE, FALSE);
	
	if ( offset < bi.start )		/* page-up */
	{ assign(s, unit,      NAME_page);
	  assign(s, direction, NAME_backwards);
	  assign(s, amount,    toInt(990));
	  assign(s, status,    NAME_repeatDelay);
	} else if ( offset > bi.start + bi.length ) /* page-down */
	{ assign(s, unit,      NAME_page);
	  assign(s, direction, NAME_forwards);
	  assign(s, amount,    toInt(990));
	  assign(s, status,    NAME_repeatDelay);
	} else				/* on the bubble */
	{ assign(s, unit,      NAME_file);
	  assign(s, direction, NAME_goto);
	  assign(s, amount,    promilage_event_scrollbar(s, ev));
	  assign(s, status,    NAME_running);
	}
      }

      if ( s->status == NAME_repeatDelay )
      { attachTimerScrollBar(s);
	changedEntireImageGraphical(s);
      }
    } else if ( isAEvent(ev, NAME_msLeftDrag) && s->status == NAME_running )
    { assign(s, amount, promilage_event_scrollbar(s, ev));
      forwardScrollBar(s);
    } else if ( isAEvent(ev, NAME_msLeftUp) )
    { if ( s->unit != NAME_file && s->status != NAME_repeat )
	forwardScrollBar(s);

      assign(s, status, NAME_inactive);

      if ( detachTimerScrollBar(s) )
	changedEntireImageGraphical(s);
    }

    succeed;
  }
  
  if ( isAEvent(ev, NAME_msMiddle) )
  { if ( isAEvent(ev, NAME_msMiddleDown) )
    { assign(s, unit,      NAME_file);
      assign(s, direction, NAME_goto);
      assign(s, amount,    promilage_event_scrollbar(s, ev));
      assign(s, status,    NAME_running);
  
      forwardScrollBar(s);	
    } else if ( s->status == NAME_running &&
		s->drag == ON &&
		isAEvent(ev, NAME_msMiddleDrag) )
    { assign(s, amount,    promilage_event_scrollbar(s, ev));
      forwardScrollBar(s);
    } else if ( isAEvent(ev, NAME_msMiddleUp) )
    { assign(s, status, NAME_inactive);
    }  

    succeed;
  }

  fail;
}

#endif /*O_MOTIF*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scrollbar event handling.  Status field:

  inactive:	Doing nothing; pointer is outside the scrollbar.
  active:	Pointer is in the scrollbar, no button pressed.
  running:	A button is depressed.  Handle events to the up.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
offset_event_scrollbar(ScrollBar s, EventObj ev)
{ if ( equalName(s->orientation, NAME_horizontal) )
    return valInt(getXEvent(ev, s));
  else
    return valInt(getYEvent(ev, s));
}


static Int
promilage_event_scrollbar(ScrollBar s, EventObj ev)
{ struct bubble_info bi;
  int ah = arrow_height_scrollbar(s);
  int offset = offset_event_scrollbar(s, ev);
  int rval;

  compute_bubble(s, &bi, ah, MIN_BUBBLE, FALSE);
  rval = ((offset - bi.bar_start) * 1000) / bi.bar_length;
  
  return toInt(BOUNDS(rval, 0, 1000));
}


static status
forwardScrollBar(ScrollBar s)
{ if ( isNil(s->message) )
    succeed;

  if ( isDefault(s->message) )
  { send(s->object,
	 (equalName(s->orientation, NAME_horizontal)
	  ? NAME_scrollHorizontal
	  : NAME_scrollVertical),
	 s->direction, s->unit, s->amount, 0);
  } else
    forwardReceiverCode(s->message, s->object,
			s->direction, s->unit, s->amount, 0);
  
  succeed;
}


static status
updateCursorScrollBar(ScrollBar s, Name which)
{ Name name = (Name) NIL;

  if ( equalName(s->orientation, NAME_vertical) )
  { if ( equalName(which, NAME_active) )
      name = NAME_sbVDoubleArrow;
    else if ( equalName(which, NAME_up) )
      name = NAME_sbUpArrow;
    else if ( equalName(which, NAME_down) )
      name = NAME_sbDownArrow;
    else if ( equalName(which, NAME_goto) )
      name = NAME_sbRightArrow;
  } else
  { if ( equalName(which, NAME_active) )
      name = NAME_sbHDoubleArrow;
    else if ( equalName(which, NAME_up) )
      name = NAME_sbLeftArrow;
    else if ( equalName(which, NAME_down) )
      name = NAME_sbRightArrow;
    else if ( equalName(which, NAME_goto) )
      name = NAME_sbDownArrow;
  }

  send(s, NAME_cursor, name, 0);

  succeed;
}


static status
eventScrollBar(ScrollBar s, EventObj ev)
{ if ( s->look == NAME_x )
  { if ( equalName(s->status, NAME_inactive) )
    { if ( isAEvent(ev, NAME_areaEnter) )
      { assign(s, status,	 NAME_active);
	return updateCursorScrollBar(s, NAME_active);
      }
    } else if ( equalName(s->status, NAME_active) )
    { if ( isDownEvent(ev) )
      { if ( isAEvent(ev, NAME_msLeftDown) )
	{ assign(s, unit,      NAME_page);
	  assign(s, direction, NAME_forwards);
	  assign(s, amount,    promilage_event_scrollbar(s, ev));
	  assign(s, status,    NAME_running);
	  return updateCursorScrollBar(s, NAME_up);
	}
	if ( isAEvent(ev, NAME_msMiddleDown) )
	{ assign(s, unit,      NAME_file);
	  assign(s, direction, NAME_goto);
	  assign(s, amount,    promilage_event_scrollbar(s, ev));
	  assign(s, status,    NAME_running);
	  updateCursorScrollBar(s, NAME_goto);
	  forwardScrollBar(s);	
	  succeed;
	}
	if ( isAEvent(ev, NAME_msRightDown) )
	{ assign(s, unit,      NAME_page);
	  assign(s, direction, NAME_backwards);
	  assign(s, amount,    promilage_event_scrollbar(s, ev));
	  assign(s, status,    NAME_running);
	  return updateCursorScrollBar(s, NAME_down);
	}
      }
      if ( isAEvent(ev, NAME_areaExit) )
      { assign(s, status,    NAME_inactive);
	return updateCursorScrollBar(s, NAME_inactive);
      }
    } else if ( equalName(s->status, NAME_running) )
    { if ( s->drag == ON && isAEvent(ev, NAME_msMiddleDrag) )
      { assign(s, amount,    promilage_event_scrollbar(s, ev));
	forwardScrollBar(s);
	succeed;
      }
      if ( isUpEvent(ev) )
      { if ( !equalName(s->unit, NAME_file) )
	  forwardScrollBar(s);
	
	if ( allButtonsUpEvent(ev) )
	{ if ( insideEvent(ev, (Graphical) s) )
	  { assign(s, status, NAME_active);
	    return updateCursorScrollBar(s, NAME_active);
	  }
	  assign(s, status, NAME_inactive);
	  return updateCursorScrollBar(s, NAME_inactive);
	}
      }
    }
  } else if ( s->look == NAME_openLook )
  { if ( OpenLookEventScrollBar(s, ev) )
      succeed;
  } else if ( s->look == NAME_motif || s->look == NAME_win )
  { if ( MotifEventScrollBar(s, ev) )
      succeed;
  }

  return eventGraphical(s, ev);
}


static status
orientationScrollBar(ScrollBar s, Name or)
{ if ( s->orientation == or )
    succeed;

  CHANGING_GRAPHICAL(s, 
	swapInt(s->area->h, s->area->w);
	assign(s, orientation, or);
	changedEntireImageGraphical(s));

  succeed;
}


static status
viewScrollBar(ScrollBar s, Int n)
{ if (valInt(n) < 0)
    n = ZERO;

  if ( s->view != n )
  { assign(s, view, n);
    requestComputeGraphical(s, DEFAULT);
  }

  succeed;
}


static status
startScrollBar(ScrollBar s, Int n)
{ if (valInt(n) < 0)
    n = ZERO;

  if ( s->start != n )
  { assign(s, start, n);
    requestComputeGraphical(s, DEFAULT);
  }

  succeed;
}


static status
lengthScrollBar(ScrollBar s, Int n)
{ if (valInt(n) < 0)
    n = ZERO;

  if ( s->length != n )
  { assign(s, length, n);
    requestComputeGraphical(s, DEFAULT);
  }

  succeed;
}


status
bubbleScrollBar(ScrollBar sb, Int l, Int s, Int v)
{ if ( valInt(l) < 0 ) l = ZERO;
  if ( valInt(s) < 0 ) s = ZERO;
  if ( valInt(v) < 0 ) v = ZERO;

  assign(sb, length, l);
  assign(sb, start,  s);
  assign(sb, view,   v);
  requestComputeGraphical(sb, DEFAULT);

  succeed;
}


static status
lookScrollBar(ScrollBar s, Name look)
{ CHANGING_GRAPHICAL(s,
		     assign(s, look, look);
		     assign(s, distance, toInt(look == NAME_x ? -1 : 1));
		     changedEntireImageGraphical(s));

  succeed;
}


static status
convertLoadedObjectScrollBar(ScrollBar sb, Int ov, Int nv)
{ if ( isName(sb->placement) )
  { Chain ch = newObject(ClassChain, 0);
    static char *names[] = {"left", "right", "top", "bottom"};
    int i;

    for(i=0; i<4; i++)
    { Name place = CtoKeyword(names[i]);

      if ( send(sb->placement, NAME_sub, place, ON, 0) )
	appendChain(ch, place);
    }
    assign(sb, placement, ch);
  }

  succeed;
}

 

status
makeClassScrollBar(Class class)
{ sourceClass(class, makeClassScrollBar, __FILE__, "$Revision$");

  localClass(class, NAME_message, NAME_action, "[code]*", NAME_both,
	     "Message used to inform object");
  localClass(class, NAME_object, NAME_client, "graphical*", NAME_both,
	     "Graphical to be scrolled scrolled");
  localClass(class, NAME_placement, NAME_layout, "chain", NAME_get,
	     "Relative automatic placement");
  localClass(class, NAME_distance, NAME_layout, "int", NAME_get,
	     "Relative distance (pixels)");
  localClass(class, NAME_status, NAME_event, "name", NAME_get,
	     "Current status for event parsing");
  localClass(class, NAME_orientation, NAME_appearance,
	     "{horizontal,vertical}", NAME_get,
	     "Scroll object horizontal or vertical");
  localClass(class, NAME_view, NAME_scroll, "int", NAME_get,
	     "Length of visible part");
  localClass(class, NAME_start, NAME_scroll, "int", NAME_get,
	     "Start of visible part");
  localClass(class, NAME_length, NAME_scroll, "int", NAME_get,
	     "Total length of object");
  localClass(class, NAME_bubbleStart, NAME_internal, "int", NAME_none,
	     "Pixel position of bubble");
  localClass(class, NAME_bubbleLength, NAME_internal, "int", NAME_none,
	     "Pixel size of bubble");
  localClass(class, NAME_look, NAME_appearance,
	     "{x,open_look,motif,win}", NAME_get,
	     "Look-and-feel (only `x')");
  localClass(class, NAME_drag, NAME_event, "bool", NAME_both,
	     "If @on, messages are sent continuously");
  localClass(class, NAME_amount, NAME_internal, "int", NAME_none,
	     "Amount to scroll");
  localClass(class, NAME_direction, NAME_internal,
	     "{forwards,backwards,goto}", NAME_none,
	     "Direction in which to scroll or jump");
  localClass(class, NAME_unit, NAME_internal, "{line,page,file}", NAME_none,
	     "Unit to scroll");
  
  termClass(class, "scroll_bar",
	    3, NAME_object, NAME_orientation, NAME_message);
  setRedrawFunctionClass(class, RedrawAreaScrollBar);

  storeMethod(class, NAME_orientation, orientationScrollBar);
  storeMethod(class, NAME_view,        viewScrollBar);
  storeMethod(class, NAME_start,       startScrollBar);
  storeMethod(class, NAME_length,      lengthScrollBar);
  storeMethod(class, NAME_look,        lookScrollBar);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "object=object", "orientation={horizontal,vertical}",
	     "message=[code]*",
	     "Create from object, orientation and message",
	     initialiseScrollBar);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Recompute the scrollbar values",
	     computeScrollBar);
  sendMethod(class, NAME_place, NAME_area, 1, "[graphical]",
	     "Position scrollbar relative to object",
	     placeScrollBar);
  sendMethod(class, NAME_bubble, NAME_scroll, 3,
	     "length=int", "start=int", "view=int",
	     "Set length, start and view",
	     bubbleScrollBar);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Process a user event",
	     eventScrollBar);
#if OPENLOOK
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Stop/disconnect repeat timer",
	     unlinkScrollBar);
  sendMethod(class, NAME_repeat, NAME_scroll, 0,
	     "Repeat last action (->look: open_look)",
	     repeatScrollBar);
#endif
  sendMethod(class, NAME_convertLoadedObject, DEFAULT, 2, "int", "int",
	     "Convert placement attribute",
	     convertLoadedObjectScrollBar);

  attach_resource(class, "width", "[int]", "16",
		  "Width of the scroll_bar");
  attach_resource(class, "look", "{x,open_look,motif,win}", "x",
		  "Look-and-feel");
  attach_resource(class, "placement", "chain", "[top,left]",
		  "Relative placement");
  attach_resource(class, "distance", "int", "-1",
		  "Distance to graphical");
  attach_resource(class, "pen", "int", "1",
		  "Thickness of surrounding box");
  attach_resource(class, "repeat_delay", "real", "0.35",
		  "OpenLook: time to wait until start of repeat");
  attach_resource(class, "repeat_interval", "real", "0.06",
		  "OpenLook: interval between repeats");
  attach_resource(class, "elevation", "elevation*", "@nil",
		  "3-D effect elevation");
  attach_resource(class, "background", "[elevation|colour|pixmap]", "white",
		  "Colour of background parts");

  succeed;
}
