/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

		/********************************
		*            CREATE		*
		********************************/

status
initialiseFigure(Figure f)
{ initialiseDevice((Device) f);
  assign(f, background, NIL);
  assign(f, pen,        ZERO);
  assign(f, border,	ZERO);
  assign(f, radius,	ZERO);
  assign(f, elevation,	NIL);
  assign(f, status,     NAME_allActive);

  succeed;
}

		/********************************
		*             REDRAW		*
		********************************/

Any
RedrawBoxFigure(Figure f, Area area)
{ Any rval = NIL;

  if ( f->pen != ZERO || notNil(f->background) || notNil(f->elevation) )
  { int x, y, w, h;

    initialiseDeviceGraphical(f, &x, &y, &w, &h);
    if ( f->pen == ZERO && f->radius == ZERO && isNil(f->elevation) )
    { r_fill(x, y, w, h, f->background);
      rval = f->background;
    } else
    { r_thickness(valInt(f->pen));
      r_dash(f->texture);

      if ( notNil(f->elevation) )
      { r_3d_box(x, y, w, h, valInt(f->radius), f->elevation, TRUE);
	rval = f->elevation->background;
      } else
      { r_box(x, y, w, h, valInt(f->radius), f->background);
	rval = f->background;
      }
    }
  }

  return rval;
}


status
RedrawAreaFigure(Figure f, Area area)
{ Any bg, obg;

  if ( notNil(bg = RedrawBoxFigure(f, area)) )
    obg = r_background(bg);
  else
    obg = NULL;
  
  RedrawAreaDevice((Device) f, area);

  if ( obg )
    r_background(obg);

  succeed;
}


		 /*******************************
		 *	     OUTLINE		*
		 *******************************/

static status
computeBoundingBoxFigure(Figure f)
{ if ( f->badBoundingBox == ON )
  { Area a = f->area;
    Int ox = a->x, oy = a->y, ow = a->w, oh = a->h;

    computeBoundingBoxDevice((Device) f);

    if ( f->border != ZERO )
      increaseArea(f->area, f->border);

    if ( ox != a->x || oy != a->y || ow != a->w || oh != a->h )
      changedAreaGraphical((Graphical)f, ox, oy, ow, oh);
  }

  succeed;
}


status
computeFigure(Figure f)
{ if ( notNil(f->request_compute) )
  { if ( f->pen != ZERO || notNil(f->background) )
    { CHANGING_GRAPHICAL(f, { computeGraphicalsDevice((Device) f);
			      computeLayoutDevice((Device) f);
			      computeBoundingBoxFigure(f);
			    });
    } else
    { computeGraphicalsDevice((Device) f);
      computeLayoutDevice((Device) f);
      computeBoundingBoxFigure(f);
    }

    assign(f, request_compute, NIL);
  }

  succeed;
}


		/********************************
		*           ATTRIBUTES		*
		********************************/


static status
statusFigure(Figure f, Name stat)
{ Cell cell;

  if ( stat == NAME_allActive )
  { for_cell(cell, f->graphicals)
      DisplayedGraphical(cell->value, ON);
  } else
  { for_cell(cell, f->graphicals)
    { Graphical gr = cell->value;
    
      DisplayedGraphical(gr, gr->name == stat ? ON : OFF);
    }
    assign(f, status, stat);
  }

  requestComputeDevice((Device) f, DEFAULT);

  succeed;
}


static status
nextStatusFigure(Figure f)
{ Cell cell;

  if ( f->status == NAME_allActive)
    fail;

  for_cell(cell, f->graphicals)
  { Graphical gr = cell->value;

    if ( gr->name == f->status )
    { Graphical gr2;

      if ( notNil(cell->next) )
        gr2 = (Graphical) cell->next->value;
      else
	gr2 = (Graphical) f->graphicals->head->value;

      return statusFigure(f, gr2->name);
    }      
  }

  fail;
}


static status
backgroundFigure(Figure f, Image bg)
{ if ( f->background != bg )
  { CHANGING_GRAPHICAL(f,
		       assign(f, background, bg);
		       if ( notNil(f->elevation) )
		         assign(f, elevation, getModifyElevation(f->elevation, NAME_colour,
								 isNil(bg) ? DEFAULT : bg));
		       changedEntireImageGraphical(f));
  }
  
  succeed;
}


static status
clipAreaFigure(Device f, Area a)
{ assign(f, badBoundingBox, ON);
  assign(f, clip_area, a);
  requestComputeDevice(f, DEFAULT);
  
  succeed;
}


static Area
getClipAreaFigure(Figure f)
{ answer(f->clip_area);
}


static status
elevationFigure(Figure f, Elevation e)
{ return assignGraphical(f, NAME_elevation, e);
}


static status
shadowFigure(Figure f, Int shadow)
{ return elevationFigure(f, shadow == ZERO ?
			      NIL :
			      newObject(ClassElevation, NIL,
					shadow, /* height */
					isNil(f->background) ? DEFAULT
							     : f->background,
					DEFAULT, /* edge colours */
					DEFAULT,
					NAME_shadow, 0));
}


static Int
getShadowFigure(Figure f)
{ if ( notNil(f->elevation) )
    answer(f->elevation->height);

  answer(ZERO);
}


static status
radiusFigure(Figure f, Int radius)
{ return assignGraphical(f, NAME_radius, radius);
}


static status
borderFigure(Figure f, Int border)
{ if ( f->border != border )
  { assign(f, border, border);
    requestComputeDevice((Device) f, DEFAULT);
  }

  succeed;
}


static status
displayFigure(Figure f, Graphical gr, Point pos)
{ if ( notDefault(pos) )
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);

  TRY( DeviceGraphical(gr, (Device) f) );
  return DisplayedGraphical(gr,
			    (f->status == NAME_allActive ||
			     f->status == gr->name) ? ON : OFF);
}


static status
convertOldSlotFigure(Figure f, Name slot, Any value)
{ if ( slot == NAME_shadow )
    shadowFigure(f, value);

  succeed;
}


extern status drawPostScriptFigure(Figure f);

status
makeClassFigure(Class class)
{ sourceClass(class, makeClassFigure, __FILE__, "$Revision$");

  localClass(class, NAME_status, NAME_visibility, "name", NAME_get,
	     "Name of visible graphical (or all_active)");
  localClass(class, NAME_background, NAME_appearance,
	     "image|colour*", NAME_get,
	     "Fill pattern used as background");
  localClass(class, NAME_border, NAME_appearance, "int", NAME_get,
	     "Border around graphicals");
  localClass(class, NAME_radius, NAME_appearance, "int", NAME_get,
	     "Radius of outline");
  localClass(class, NAME_elevation, NAME_appearance, "elevation*", NAME_get,
	     "Elevation from background");

  setRedrawFunctionClass(class, RedrawAreaFigure);

  storeMethod(class, NAME_status,     statusFigure);
  storeMethod(class, NAME_background, backgroundFigure);
  storeMethod(class, NAME_clipArea,   clipAreaFigure);
  storeMethod(class, NAME_border,     borderFigure);
  storeMethod(class, NAME_radius,     radiusFigure);
  storeMethod(class, NAME_elevation,  elevationFigure);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create figure",
	     initialiseFigure);
  sendMethod(class, NAME_compute, NAME_update, 0,
	     "Recompute figure (handle <-border)",
	     computeFigure);
  sendMethod(class, NAME_nextStatus, NAME_visibility, 0,
	     "Make next in <-graphicals visible",
	     nextStatusFigure);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptFigure);
  sendMethod(class, NAME_display, NAME_organisation, 2, "graphical", "[point]",
	     "Display graphical at point",
	     displayFigure);
  sendMethod(class, NAME_shadow, NAME_appearance, 1, "0..",
	     "Attach `shadow' elevation object",
	     shadowFigure);
  sendMethod(class, NAME_convertOldSlot, NAME_compatibility, 2,
	     "slot=name", "value=any",
	     "Translate old shadow into elevation",
	     convertOldSlotFigure);
	
  getMethod(class, NAME_clipArea, NAME_scroll, "area", 0,
	    "Clip area associated with figure",
	    getClipAreaFigure);
  getMethod(class, NAME_shadow, NAME_compatibility, "0..", 0,
	    "Read `elevation <-height'",
	    getShadowFigure);

  succeed;
}

