/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status scrollbarsWindowDecorator(WindowDecorator dw, Name bars);
static status labelWindowDecorator(WindowDecorator, CharArray, int, Any *);


static status
initialiseWindowDecorator(WindowDecorator dw, PceWindow w,
			  Name bars, Name label)
{ initialiseWindow((PceWindow) dw, DEFAULT, DEFAULT, DEFAULT);

  if ( notDefault(bars) )
    scrollbarsWindowDecorator(dw, bars);
  if ( notDefault(label) )
    send(dw, NAME_label, label, 0);

  assign(dw, window, w);
  send(w, NAME_decorate, NAME_grow, ZERO, ZERO, ZERO, ZERO, dw, 0); /* TBD */

  succeed;
}


static status
unlinkWindowDecorator(WindowDecorator dw)
{ PceWindow sw;

  if ( notNil((sw = dw->window)) )
  { assign(dw, window, NIL);
    freeObject(sw);
  }

  return unlinkWindow((PceWindow) dw);
}

		 /*******************************
		 *	   ATTACH PARTS		*
		 *******************************/


static status
horizontalScrollbarWindowDecorator(WindowDecorator dw, Bool val)
{ if ( val == ON && isNil(dw->horizontal_scrollbar) )
  { assign(dw, horizontal_scrollbar,
	   newObject(ClassScrollBar, dw->window, NAME_horizontal, 0));
    displayDevice(dw, dw->horizontal_scrollbar, DEFAULT);
    send(dw, NAME_rearrange, 0);
  } else if ( val == OFF && notNil(dw->horizontal_scrollbar) )
  { freeObject(dw->horizontal_scrollbar);
    assign(dw, horizontal_scrollbar, NIL);
    send(dw, NAME_rearrange, 0);
  }
  
  succeed;
}


static status
verticalScrollbarWindowDecorator(WindowDecorator dw, Bool val)
{ if ( val == ON && isNil(dw->vertical_scrollbar) )
  { assign(dw, vertical_scrollbar,
	   newObject(ClassScrollBar, dw->window, NAME_vertical, 0));
    displayDevice(dw, dw->vertical_scrollbar, DEFAULT);
    send(dw, NAME_rearrange, 0);
  } else if ( val == OFF && notNil(dw->vertical_scrollbar) )
  { freeObject(dw->vertical_scrollbar);
    assign(dw, vertical_scrollbar, NIL);
    send(dw, NAME_rearrange, 0);
  }
  
  succeed;
}


static status
scrollbarsWindowDecorator(WindowDecorator dw, Name bars)
{ Bool vbar = OFF;
  Bool hbar = OFF;

  if ( equalName(bars, NAME_vertical) )
    vbar = ON;
  else if ( equalName(bars, NAME_horizontal) )
    hbar = ON;
  else if ( equalName(bars, NAME_both) )
    vbar = hbar = ON;
	
  horizontalScrollbarWindowDecorator(dw, hbar);
  verticalScrollbarWindowDecorator(dw, vbar);
  
  succeed;
}


static Name
getScrollbarsWindowDecorator(WindowDecorator dw)
{ if ( notNil(dw->horizontal_scrollbar) )
  { if ( notNil(dw->vertical_scrollbar) )
      answer(NAME_both);
    answer(NAME_horizontal);
  } else
  { if ( notNil(dw->vertical_scrollbar) )
      answer(NAME_vertical);
    answer(NAME_none);
  }
}


status
requestComputeScrollbarsWindowDecorator(WindowDecorator dw)
{ if ( notNil(dw->horizontal_scrollbar) )
    requestComputeGraphical(dw->horizontal_scrollbar, DEFAULT);
  if ( notNil(dw->vertical_scrollbar) )
    requestComputeGraphical(dw->vertical_scrollbar, DEFAULT);

  succeed;
}

		 /*******************************
		 *	    ARRANGING		*
		 *******************************/


static void
compute_margins_window_decorator(WindowDecorator dw,
				 Int *lm, Int *tm, Int *rm, Int *bm)
{ int l=0, t=0, r=0, b=0;		/* margins we need */

  if ( notNil(dw->label_text) )
    t += valInt(getAreaGraphical((Graphical) dw->label_text)->h);
  if ( notNil(dw->horizontal_scrollbar) )
  { int m = valInt(getMarginScrollBar(dw->horizontal_scrollbar));
    
    if ( m > 0 )
      b += m;
    else
      t -= m;
  }
  if ( notNil(dw->vertical_scrollbar) )
  { int m = valInt(getMarginScrollBar(dw->vertical_scrollbar));
    
    if ( m > 0 )
      r += m;
    else
      l -= m;
  }
  
  *lm = toInt(l), *tm = toInt(t), *rm = toInt(r), *bm = toInt(b);
}


static status
rearrangeWindowDecorator(WindowDecorator dw)
{ Int lm, tm, rm, bm;			/* margins we need */

  compute_margins_window_decorator(dw, &lm, &tm, &rm, &bm);
  doSetGraphical(dw->window,
		 lm, tm,
		 sub(dw->area->w, add(lm, rm)),
		 sub(dw->area->h, add(tm, bm)));
  if ( notNil(dw->horizontal_scrollbar) )
    placeScrollBar(dw->horizontal_scrollbar, DEFAULT);
  if ( notNil(dw->vertical_scrollbar) )
    placeScrollBar(dw->vertical_scrollbar, DEFAULT);

  succeed;
}

		 /*******************************
		 *	     GEOMETRY		*
		 *******************************/

static status
geometryWindowDecorator(WindowDecorator dw, Int x, Int y, Int w, Int h)
{ geometryWindow((PceWindow)dw, x, y, w, h);

  send(dw, NAME_rearrange, 0);

  succeed;
}


static status
requestGeometryWindowDecorator(WindowDecorator dw, Int x, Int y, Int w, Int h)
{ Int lm, tm, rm, bm;			/* margins we need */
  Int nw, nh;

  compute_margins_window_decorator(dw, &lm, &tm, &rm, &bm);
  nw = (isDefault(w) ? w : add(w, add(lm, rm)));
  nh = (isDefault(h) ? h : add(h, add(tm, bm)));

  if ( notNil(dw->tile) )
  { setTile(dw->tile, DEFAULT, DEFAULT, nw, nh);

    if ( notNil(dw->frame) && createdFrame(dw->frame) )
    { if ( !parentGoal(VmiSend, dw->frame, NAME_fit) ) /* avoid loop */
	send(dw->frame, NAME_fit, 0);
    }
  } else
    geometryWindowDecorator(dw, x, y, nw, nh);

  succeed;
}


static status
resizeWindowDecorator(WindowDecorator dw)
{ resizeWindow((PceWindow)dw);
  resizeWindow(dw->window);

  succeed;
}

		 /*******************************
		 *	   COMMUNICATION	*
		 *******************************/

static status
displayedWindowDecorator(WindowDecorator dw, Bool val)
{ displayedGraphical(dw, val);

  return DisplayedGraphical(dw->window, val);
}


static status
ComputeDesiredSizeWindowDecorator(WindowDecorator dw)
{ return send(dw->window, NAME_ComputeDesiredSize, 0);
}


		 /*******************************
		 *	    WINDOW LABEL	*
		 *******************************/

static status
labelWindowDecorator(WindowDecorator dw, CharArray fmt, int argc, Any *argv)
{ if ( isNil(fmt) )
  { freeObject(dw->label_text);
    assign(dw, label_text, NIL);
  } else
  { char buf[LINESIZE];
    FontObj font = getResourceValueObject(dw, NAME_labelFont);

    TRY(swritefv(buf, fmt, argc, argv));
    if ( isNil(dw->label_text) )
    { assign(dw, label_text, newObject(ClassText, DEFAULT, DEFAULT, font, 0));
      displayDevice(dw, dw->label_text, DEFAULT);
    }
    transparentText(dw->label_text, ON);
    stringText(dw->label_text, (CharArray) CtoString(buf));
  }

  send(dw, NAME_rearrange, 0);

  succeed;
}


static CharArray
getLabelWindowDecorator(WindowDecorator dw)
{ if ( notNil(dw->label_text) )
    answer(dw->label_text->string);

  fail;
}


status
makeClassWindowDecorator(Class class)
{ sourceClass(class, makeClassWindowDecorator, __FILE__, "$Revision$");

  localClass(class, NAME_window, NAME_client, "window", NAME_get,
	     "Decorated window");
  localClass(class, NAME_horizontalScrollbar, NAME_scroll,
	     "scroll_bar*", NAME_get,
	     "Scrollbar for X-direction");
  localClass(class, NAME_verticalScrollbar, NAME_scroll,
	     "scroll_bar*", NAME_get,
	     "Scrollbar for Y-direction");
  localClass(class, NAME_labelText, NAME_label,
	     "text*", NAME_get,
	     "Text object to display label");

  termClass(class, "window_decorator", 3,
	    NAME_window, NAME_scrollbars, NAME_label);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "window=window", "scrollbars=[{none,vertical,horizontal,both}]",
	     "label=[char_array]",
	     "Create decoration for window",
	     initialiseWindowDecorator);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Make sure <-window is destroyed",
	     unlinkWindowDecorator);

  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Update internals",
	     geometryWindowDecorator);
  sendMethod(class, NAME_requestGeometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Handle window geometry request",
	     requestGeometryWindowDecorator);
  sendMethod(class, NAME_rearrange, NAME_layout, 0,
	     "Rearrange <-window, <-scrollbars and <-label_text",
	     rearrangeWindowDecorator);
  sendMethod(class, NAME_resize, DEFAULT, 0,
	     "Also `window ->resize' <-window",
	     resizeWindowDecorator);
  sendMethod(class, NAME_label, NAME_label, 2,
	     "format=char_array*", "argument=any ...",
	     "Define window-level label",
	     labelWindowDecorator);
  sendMethod(class, NAME_displayed, DEFAULT, 1, "bool",
	     "(Un)display window.  Take care of <-window",
	     displayedWindowDecorator);
  sendMethod(class, NAME_ComputeDesiredSize, NAME_layout, 0,
	     "Compute the desired size (delegate to <-window)",
	     ComputeDesiredSizeWindowDecorator);

  sendMethod(class, NAME_horizontalScrollbar, NAME_scroll, 1, "bool",
	     "Attach/detach horizontal scrollbar",
	     horizontalScrollbarWindowDecorator);
  sendMethod(class, NAME_verticalScrollbar, NAME_scroll, 1, "bool",
	     "Attach/detach horizontal scrollbar",
	     verticalScrollbarWindowDecorator);
  sendMethod(class, NAME_scrollbars, NAME_scroll, 1,
	     "{none,horizontal,vertical,both}",
	     "Set/remove scrollbars",
	     scrollbarsWindowDecorator);

  getMethod(class, NAME_label, NAME_label, "char_array", 0,
	    "Currently displayed label",
	    getLabelWindowDecorator);
  getMethod(class, NAME_scrollbars, NAME_scroll,
	    "{none,horizontal,vertical,both}", 0,
	    "Available scrollbars",
	    getScrollbarsWindowDecorator);

  attach_resource(class, "border", "int", "0",
		  "Distance between outside and inside");
  attach_resource(class, "pen", "int", "0",
		  "Thickness of outside line");
  attach_resource(class, "label_font", "font", "bold",
		  "Font to display label");

  succeed;
}
