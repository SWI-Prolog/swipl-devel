/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

status
initialiseResizeGesture(ResizeGesture g, Name button, Modifier modifier)
{ Size ms = getResourceValueObject(g, NAME_minSize);

  initialiseGesture((Gesture) g, button, modifier);

  assign(g, h_mode, NAME_keep);
  assign(g, v_mode, NAME_keep);
  assign(g, min_size, ms != FAIL ? ms : newObject(ClassSize, 0));
  
  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
verifyResizeGesture(ResizeGesture g, EventObj ev)
{ int frac = valInt(getResourceValueObject(g, NAME_marginFraction));
  int mx   = valInt(getResourceValueObject(g, NAME_marginWidth));
  Int X, Y;
  int x, y, w, h;

  if ( isNil(ev->receiver->device) )
    fail;

  get_xy_event(ev, ev->receiver, ON, &X, &Y);
  x = valInt(X), y = valInt(Y);
  w = valInt(ev->receiver->area->w);
  h = valInt(ev->receiver->area->h);
  
  if ( x < w/frac && x < mx )		/* determine horizontal-mode */
    assign(g, h_mode, NAME_left);
  else if ( x > ((frac-1) * w)/frac && x > w - mx )
    assign(g, h_mode, NAME_right);
  else
    assign(g, h_mode, NAME_keep);

  if ( y < h/frac && y < mx )		/* determine vertical-mode */
    assign(g, v_mode, NAME_top);
  else if ( y > ((frac-1) * h)/frac && y > h - mx )
    assign(g, v_mode, NAME_bottom);
  else
    assign(g, v_mode, NAME_keep);

  if ( g->h_mode == NAME_keep && g->v_mode == NAME_keep )
    fail;

  succeed;
}

		/********************************
		*           INITIATE		*
		********************************/

static status
setCursorResizeGesture(ResizeGesture g, PceWindow sw)
{ int i;
  static struct
  { Name h_mode;
    Name v_mode;
    Name cursor;
  } cursors[] = {
  { NAME_left,	NAME_top,    NAME_topLeftCorner },
  { NAME_right, NAME_top,    NAME_topRightCorner },
  { NAME_left,  NAME_bottom, NAME_bottomLeftCorner },
  { NAME_right, NAME_bottom, NAME_bottomRightCorner },
  { NAME_keep,  NAME_top,    NAME_topSide },
  { NAME_keep,  NAME_bottom, NAME_bottomSide },
  { NAME_left,  NAME_keep,   NAME_leftSide },
  { NAME_right, NAME_keep,   NAME_rightSide }};

  for(i=0; i<8; i++)
    if ( g->h_mode == cursors[i].h_mode &&
	 g->v_mode == cursors[i].v_mode )
    { send(sw, NAME_focusCursor, cursors[i].cursor, 0);
      succeed;
    }
    
  fail;
}


static status
setPointerResizeGesture(ResizeGesture g, Graphical gr, EventObj ev)
{ Point pos;
  Name hm = g->h_mode, vm = g->v_mode;
  Int w, h, px = ZERO, py = ZERO;	/* keep compiler happy */

  w  = gr->area->w;
  h  = gr->area->h;

  if ( hm == NAME_keep || vm == NAME_keep )
  { Int cx, cy;

    get_xy_event(ev, gr, ON, &cx, &cy);
    if (      hm == NAME_keep  && vm == NAME_top )     px = cx,   py = ZERO;
    else if ( hm == NAME_keep  && vm == NAME_bottom )  px = cx,   py = h;
    else if ( hm == NAME_left  && vm == NAME_keep )    px = ZERO, py = cy;
    else if ( hm == NAME_right && vm == NAME_keep )    px = w,    py = cy;
    else
      NOTREACHED;
  } else
  { if (      hm == NAME_left  && vm == NAME_top )     px = ZERO, py = ZERO;
    else if ( hm == NAME_right && vm == NAME_top )     px = w,    py = ZERO;
    else if ( hm == NAME_left  && vm == NAME_bottom )  px = ZERO, py = h;
    else if ( hm == NAME_right && vm == NAME_bottom )  px = w,    py = h;
    else
      NOTREACHED;
  }
    
  pos = tempObject(ClassPoint, px, py, 0);
  pointerGraphical(gr, pos);
  considerPreserveObject(pos);

  succeed;
}


static status
initiateResizeGesture(ResizeGesture g, EventObj ev)
{ setCursorResizeGesture(g, ev->window);
  setPointerResizeGesture(g, ev->receiver, ev);

  succeed;
}


static status
dragResizeGesture(ResizeGesture g, EventObj ev)
{ Graphical gr = ev->receiver;
  int x, y, w, h;
  int sx, sy, sw, sh;
  int ex, ey;
  Int X, Y;
  Name hm = g->h_mode, vm = g->v_mode;
  
  x = valInt(gr->area->x);
  y = valInt(gr->area->y);
  w = valInt(gr->area->w);
  h = valInt(gr->area->h);
  get_xy_event(ev, gr, ON, &X, &Y);
  ex = valInt(X); ey = valInt(Y);

  if ( notNil(g->min_size) )
  { if ( hm == NAME_left && ex > w - valInt(g->min_size->w) )
      ex = w - valInt(g->min_size->w);
    if ( hm == NAME_right && ex < valInt(g->min_size->w) )
      ex = valInt(g->min_size->w);
    if ( vm == NAME_top && ey > h - valInt(g->min_size->h) )
      ey = h - valInt(g->min_size->h);
    if ( vm == NAME_bottom && ey < valInt(g->min_size->h) )
      ey = valInt(g->min_size->h);
  }
  if ( notNil(g->max_size) )
  { if ( hm == NAME_left && ex < w - valInt(g->max_size->w) )
      ex = w - valInt(g->max_size->w);
    if ( hm == NAME_right && ex > valInt(g->max_size->w) )
      ex = valInt(g->max_size->w);
    if ( vm == NAME_top && ey < h - valInt(g->max_size->h) )
      ey = h - valInt(g->max_size->h);
    if ( vm == NAME_bottom && ey > valInt(g->max_size->h) )
      ey = valInt(g->max_size->h);
  }

  if (      hm == NAME_left  && vm == NAME_top )
    sx = x+ex, sy = y+ey, sw = w-ex, sh = h-ey;
  else if ( hm == NAME_right && vm == NAME_top )     
    sx = x,    sy = y+ey, sw = ex,   sh = h-ey;
  else if ( hm == NAME_left  && vm == NAME_bottom )  
    sx = x+ex, sy = y,    sw = w-ex, sh = ey;
  else if ( hm == NAME_right && vm == NAME_bottom )  
    sx = x,    sy = y,    sw = ex,   sh = ey;
  else if ( hm == NAME_keep  && vm == NAME_top )     
    sx = x,    sy = y+ey, sw = w,    sh = h-ey;
  else if ( hm == NAME_keep  && vm == NAME_bottom )  
    sx = x,    sy = y,    sw = w,    sh = ey;
  else if ( hm == NAME_left  && vm == NAME_keep )    
    sx = x+ex, sy = y,    sw = w-ex, sh = h;
  else if ( hm == NAME_right && vm == NAME_keep )    
    sx = x,    sy = y,    sw = ex,   sh = h;
  else
  { NOTREACHED;
    fail;
  }

  return send(gr, NAME_doSet, toInt(sx), toInt(sy), toInt(sw), toInt(sh), 0);
}


static status
terminateResizeGesture(ResizeGesture g, EventObj ev)
{ return dragResizeGesture(g, ev);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static const vardecl var_resizeGesture[] =
{ IV(NAME_hMode, "{left,keep,right}", IV_BOTH,
     NAME_mode, "Horizontal resize mode"),
  IV(NAME_vMode, "{top,keep,bottom}", IV_BOTH,
     NAME_mode, "Vertical resize mode"),
  IV(NAME_minSize, "size*", IV_BOTH,
     NAME_constraint, "Specify minimum size of the graphical"),
  IV(NAME_maxSize, "size*", IV_BOTH,
     NAME_constraint, "Specify maximum size of the graphical")
};

/* Send Methods */

static const senddecl send_resizeGesture[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseResizeGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_drag, 1, "event", dragResizeGesture,
     NAME_event, "Changes the appropriate edges"),
  SM(NAME_initiate, 1, "event", initiateResizeGesture,
     NAME_event, "Set cursor and warp pointer"),
  SM(NAME_terminate, 1, "event", terminateResizeGesture,
     NAME_event, "Equivalent to ->drag"),
  SM(NAME_verify, 1, "event", verifyResizeGesture,
     NAME_event, "Test margins and set modes")
};

/* Get Methods */

static const getdecl get_resizeGesture[] =
{ 
};

/* Resources */

static const resourcedecl rc_resizeGesture[] =
{ RC(NAME_button, "button_name", "middle",
     "Active on which button (middle)"),
  RC(NAME_marginFraction, "int", "4",
     "Cursor must be within 1/fraction from edge"),
  RC(NAME_marginWidth, "int", "15",
     "Cursor must be within <max> from edge"),
  RC(NAME_minSize, "size", "size(3,3)",
     "Minimum size of graphical")
};

/* Class Declaration */

static Name resizeGesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(resizeGesture_decls,
          var_resizeGesture, send_resizeGesture,
	  get_resizeGesture, rc_resizeGesture,
          2, resizeGesture_termnames,
          "$Rev$");

status
makeClassResizeGesture(Class class)
{ return declareClass(class, &resizeGesture_decls);
}

