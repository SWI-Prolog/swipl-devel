/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/layout.h>

NewClass(resize_table_slice_gesture)
  ABSTRACT_GESTURE
  Name	mode;				/* column,row */
  Int	row;				/* Number of the row */
  Int	column;				/* Number of the column */
  Size	min_size;			/* Minimum size of cell */
End;


status
initialiseResizeTableSliceGesture(ResizeTableSliceGesture g, Name mode,
				  Name button, Modifier modifier)
{ Size ms = getClassVariableValueObject(g, NAME_minSize);

  initialiseGesture((Gesture) g, button, modifier);

  assign(g, mode, mode);
  assign(g, min_size, ms != FAIL ? ms : newObject(ClassSize, EAV));

  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static Table
getTableFromEvent(EventObj ev)
{ Device dev = (Device)ev->receiver;
  Table tab;

  if ( !instanceOfObject(dev, ClassDevice) ||
       !instanceOfObject((tab=(Table)dev->layout_manager), ClassTable) )
    fail;

  return tab;       
}


static status
verifyResizeTableSliceGesture(ResizeTableSliceGesture g, EventObj ev)
{ int frac = valInt(getClassVariableValueObject(g, NAME_marginFraction));
  int mx   = valInt(getClassVariableValueObject(g, NAME_marginWidth));
  Int X, Y;
  int ex, ey;
  int x, y, w, h;
  Table tab;
  Any down;
  int cn, rn;
  TableRow row;
  TableColumn col;

  if ( !(tab = getTableFromEvent(ev)) ||
       !(down = getCellFromPositionTable(tab, ev, ON)) )
    fail;

  if ( instanceOfObject(down, ClassTableCell) )
  { TableCell cell = down;

    if ( isNil(cell->column) || isNil(cell->row) )
      fail;

    cn = valInt(cell->column);
    rn = valInt(cell->row);
  } else
  { Point pt = down;

    cn = valInt(pt->x);
    rn = valInt(pt->y);
  }

  row = getRowTable(tab,    toInt(rn), ON); /* play safe */
  col = getColumnTable(tab, toInt(cn), ON); /* play safe */

  get_xy_event(ev, ev->receiver, ON, &X, &Y);
  ex = valInt(X), ey = valInt(Y);
  x = valInt(col->position);
  y = valInt(row->position);
  w = valInt(col->width);
  h = valInt(row->width);

  assign(g, row, NIL);			/* play safe */
  assign(g, column, NIL);

  if ( g->mode == NAME_column )
  { if ( ex < x+w/frac && ex < x+mx )	/* determine horizontal-mode */
    { int cmin, cmax;

      table_column_range(tab, &cmin, &cmax);
      if ( cn <= cmin )
	fail;
      assign(g, column, toInt(cn-1));
    } else if ( ex > x+((frac-1) * w)/frac && ex > x + w - mx )
    { assign(g, column, toInt(cn));
    } else
      fail;
  } else /* if ( g->mode == NAME_row ) */
  { if ( ey < y+h/frac && ey < y+mx )	/* determine vertical-mode */
    { int rmin, rmax;

      table_row_range(tab, &rmin, &rmax);
      if ( rn <= rmin )
	fail;
      assign(g, row, toInt(rn-1));
    } else if ( ey > y+((frac-1) * h)/frac && ey > y + h - mx )
    { assign(g, row, toInt(rn));
    } else
      fail;
  }

  succeed;
}

		/********************************
		*           INITIATE		*
		********************************/

static status
setCursorResizeTableSliceGesture(ResizeTableSliceGesture g, PceWindow sw)
{ Any av[1];

  if ( g->mode == NAME_column )
    av[0] = NAME_sbHDoubleArrow;
  else
    av[0] = NAME_sbVDoubleArrow;

  return sendv(sw, NAME_focusCursor, 1, av);
}


static status
setPointerResizeTableSliceGesture(ResizeTableSliceGesture g, EventObj ev)
{ Table tab;

  if ( (tab=getTableFromEvent(ev)) )
  { Int cx, cy;
    Int px = ZERO, py = ZERO;	/* keep compiler happy */
    Point pos;
  
    get_xy_event(ev, ev->receiver, ON, &cx, &cy);

    if ( g->mode == NAME_column )
    { TableColumn col = getColumnTable(tab, g->column, ON);
      
      px = add(col->position, col->width);
      py = cy;
    } else
    { TableRow row = getRowTable(tab, g->row, ON);

      py = add(row->position, row->width);
      px = cx;
    }
    
    pos = tempObject(ClassPoint, px, py, EAV);
    pointerGraphical(ev->receiver, pos);
    considerPreserveObject(pos);

    succeed;
  }

  fail;
}


static status
initiateResizeTableSliceGesture(ResizeTableSliceGesture g, EventObj ev)
{ setCursorResizeTableSliceGesture(g, ev->window);
  setPointerResizeTableSliceGesture(g, ev);

  succeed;
}


static status
dragResizeTableSliceGesture(ResizeTableSliceGesture g, EventObj ev)
{ Table tab;

  if ( (tab = getTableFromEvent(ev)) )
  { Int cx, cy;

    get_xy_event(ev, ev->receiver, ON, &cx, &cy);
    
    if ( g->mode == NAME_column )
    { TableColumn col = getColumnTable(tab, g->column, ON);
      int nw = valInt(cx) - valInt(col->position);

      nw = max(valInt(g->min_size->w), nw);
      send(tab, NAME_userResizeSlice, col, toInt(nw), EAV);
    } else
    { TableRow row = getRowTable(tab, g->row, ON);
      int nh = valInt(cy) - valInt(row->position);

      nh = max(valInt(g->min_size->h), nh);
      send(tab, NAME_userResizeSlice, row, toInt(nh), EAV);
    }

    succeed;
  }

  fail;
}


static status
terminateResizeTableSliceGesture(ResizeTableSliceGesture g, EventObj ev)
{ return dragResizeTableSliceGesture(g, ev);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
	{ "mode=[{row,column}]",
	  "button=[button_name]",
	  "modifier=[modifier]"
	};

/* Instance Variables */

static vardecl var_resizeGesture[] =
{ IV(NAME_mode,   "{column,row}", IV_BOTH,
     NAME_mode, "Resize rows or columns"),
  IV(NAME_row,    "int*",         IV_GET,
     NAME_event, "Row we are resizing"),
  IV(NAME_column, "int*",         IV_GET,
     NAME_event, "Column we are resizing"),
  IV(NAME_minSize, "size*", IV_BOTH,
     NAME_constraint, "Minimum size of the row/column")
};

/* Send Methods */

static senddecl send_resizeGesture[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseResizeTableSliceGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_drag, 1, "event", dragResizeTableSliceGesture,
     NAME_event, "Changes the appropriate edges"),
  SM(NAME_initiate, 1, "event", initiateResizeTableSliceGesture,
     NAME_event, "Set cursor and warp pointer"),
  SM(NAME_terminate, 1, "event", terminateResizeTableSliceGesture,
     NAME_event, "Equivalent to ->drag"),
  SM(NAME_verify, 1, "event", verifyResizeTableSliceGesture,
     NAME_event, "Test margins and set modes")
};

/* Get Methods */

#define get_resizeGesture NULL
/*
static getdecl get_resizeGesture[] =
{ 
};
*/

/* Resources */

static classvardecl rc_resizeGesture[] =
{ RC(NAME_button, "button_name", "middle",
     "Active on which button (middle)"),
  RC(NAME_marginFraction, "int", "4",
     "Cursor must be within 1/fraction from edge"),
  RC(NAME_marginWidth, "int", "15",
     "Cursor must be within <max> from edge"),
  RC(NAME_minSize, "size", "size(10,10)",
     "Minimum size of cell")
};

/* Class Declaration */

static Name resizeGesture_termnames[] = { NAME_mode, NAME_button, NAME_modifier };

ClassDecl(resizeGesture_decls,
          var_resizeGesture, send_resizeGesture,
	  get_resizeGesture, rc_resizeGesture,
          3, resizeGesture_termnames,
          "$Rev$");

status
makeClassResizeTableSliceGesture(Class class)
{ return declareClass(class, &resizeGesture_decls);
}

