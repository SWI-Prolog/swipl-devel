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
static status rearrangeWindowDecorator(WindowDecorator dw);


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


static status
showScrollBarWindowDecodaror(WindowDecorator dw, Bool show, ScrollBar sb)
{ if ( sb == dw->horizontal_scrollbar ||
       sb == dw->vertical_scrollbar )
  { DisplayedGraphical(sb, show);
    rearrangeWindowDecorator(dw);
  }

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
  if ( notNil(dw->horizontal_scrollbar) && 
       dw->horizontal_scrollbar->displayed == ON )
    placeScrollBar(dw->horizontal_scrollbar, DEFAULT);
  if ( notNil(dw->vertical_scrollbar) &&
       dw->vertical_scrollbar->displayed == ON )
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

    if ( notNil(dw->frame) )
      send(dw->frame, NAME_fit, 0);
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


static status
layoutDialogWindowDecorator(Device d, Size gap, Size bb, Size border)
{ succeed;
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
    FontObj font = getClassVariableValueObject(dw, NAME_labelFont);

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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_label[] =
        { "format=char_array*", "argument=any ..." };
static char *T_showScrollBar[] =
        { "show=[bool]", "which=[scroll_bar]" };
static char *T_layout[] =
        { "gap=[size]", "size=[size]", "border=[size]" };
static char *T_initialise[] =
        { "window=window", "scrollbars=[{none,vertical,horizontal,both}]", "label=[char_array]" };
static char *T_xADintD_yADintD_widthADintD_heightADintD[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_windowDecorator[] =
{ IV(NAME_window, "window", IV_GET,
     NAME_client, "Decorated window"),
  IV(NAME_horizontalScrollbar, "scroll_bar*", IV_GET,
     NAME_scroll, "Scrollbar for X-direction"),
  IV(NAME_verticalScrollbar, "scroll_bar*", IV_GET,
     NAME_scroll, "Scrollbar for Y-direction"),
  IV(NAME_labelText, "text*", IV_GET,
     NAME_label, "Text object to display label")
};

/* Send Methods */

static senddecl send_windowDecorator[] =
{ SM(NAME_displayed, 1, "bool", displayedWindowDecorator,
     DEFAULT, "(Un)display window.  Take care of <-window"),
  SM(NAME_geometry, 4, T_xADintD_yADintD_widthADintD_heightADintD, geometryWindowDecorator,
     DEFAULT, "Update internals"),
  SM(NAME_initialise, 3, T_initialise, initialiseWindowDecorator,
     DEFAULT, "Create decoration for window"),
  SM(NAME_requestGeometry, 4, T_xADintD_yADintD_widthADintD_heightADintD, requestGeometryWindowDecorator,
     DEFAULT, "Handle window geometry request"),
  SM(NAME_resize, 0, NULL, resizeWindowDecorator,
     DEFAULT, "Also `window ->resize' <-window"),
  SM(NAME_unlink, 0, NULL, unlinkWindowDecorator,
     DEFAULT, "Make sure <-window is destroyed"),
  SM(NAME_label, 2, T_label, labelWindowDecorator,
     NAME_label, "Define window-level label"),
  SM(NAME_ComputeDesiredSize, 0, NULL, ComputeDesiredSizeWindowDecorator,
     NAME_layout, "Compute the desired size (delegate to <-window)"),
  SM(NAME_layoutDialog, 3, T_layout, layoutDialogWindowDecorator,
     NAME_layout, "(Re)compute layout of dialog_items (ignore)"),
  SM(NAME_rearrange, 0, NULL, rearrangeWindowDecorator,
     NAME_layout, "Rearrange <-window, <-scrollbars and <-label_text"),
  SM(NAME_horizontalScrollbar, 1, "bool", horizontalScrollbarWindowDecorator,
     NAME_scroll, "Attach/detach horizontal scrollbar"),
  SM(NAME_scrollbars, 1, "{none,horizontal,vertical,both}", scrollbarsWindowDecorator,
     NAME_scroll, "Set/remove scrollbars"),
  SM(NAME_verticalScrollbar, 1, "bool", verticalScrollbarWindowDecorator,
     NAME_scroll, "Attach/detach horizontal scrollbar"),
  SM(NAME_showScrollBar, 2, T_showScrollBar, showScrollBarWindowDecodaror,
     NAME_scroll, "Control visibility of the indicated scroll_bar")
};

/* Get Methods */

static getdecl get_windowDecorator[] =
{ GM(NAME_label, 0, "char_array", NULL, getLabelWindowDecorator,
     NAME_label, "Currently displayed label"),
  GM(NAME_scrollbars, 0, "{none,horizontal,vertical,both}", NULL, getScrollbarsWindowDecorator,
     NAME_scroll, "Available scrollbars")
};

/* Resources */

static classvardecl rc_windowDecorator[] =
{ RC(NAME_border, "int", "0",
     "Distance between outside and inside"),
  RC(NAME_labelFont, "font", "bold",
     "Font to display label"),
  RC(NAME_pen, "int", "0",
     "Thickness of outside line"),
  RC(NAME_background, RC_REFINE, "@_dialog_bg",
     NULL)
};

/* Class Declaration */

static Name windowDecorator_termnames[] =
	{ NAME_window, NAME_scrollbars, NAME_label };

ClassDecl(windowDecorator_decls,
          var_windowDecorator, send_windowDecorator,
	  get_windowDecorator, rc_windowDecorator,
          3, windowDecorator_termnames,
          "$Rev$");


status
makeClassWindowDecorator(Class class)
{ return declareClass(class, &windowDecorator_decls);
}
