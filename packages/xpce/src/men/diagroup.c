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

static status nameDialogGroup(DialogGroup g, Name name);
static status labelDialogGroup(DialogGroup g, Any name);


		/********************************
		*            CREATE		*
		********************************/

status
initialiseDialogGroup(DialogGroup g, Name name, Name kind)
{ initialiseDevice((Device) g);

  if ( isDefault(name) )
    name = getClassNameObject(g);

  assign(g, label,	  DEFAULT);	/* see nameDialogGroup() */
  assign(g, size,	  DEFAULT);
  assign(g, border,	  getClassVariableValueObject(g, NAME_border));
  assign(g, auto_align,	  ON);
  assign(g, elevation,	  NIL);

  nameDialogGroup(g, name);

  if ( notDefault(kind) )
    return qadSendv(g, NAME_kind, 1, (Any *)&kind);

  succeed;
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

void
compute_label_size_dialog_group(DialogGroup g, int *w, int *h)
{ if ( instanceOfObject(g->label, ClassImage) )
  { Image i = g->label;

    *w = valInt(i->size->w);
    *h = valInt(i->size->h);
  } else
  { if ( instanceOfObject(g->label, ClassCharArray) )
    { CharArray ca = g->label;

      str_size(&ca->data, g->label_font, w, h);
    } else
    { *w = *h = 0;
    }
  }
}


static void
compute_label(DialogGroup g, int *x, int *y, int *w, int *h)
{ if ( notNil(g->label) )
  { int tw, th;

    compute_label_size_dialog_group(g, &tw, &th);
  
    if ( w ) *w = tw;
    if ( h ) *h = th;
    if ( y )
    { if ( g->label_format == NAME_top )
	*y = 0;
      else if ( g->label_format == NAME_bottom )
	*y = -th;
      else if ( g->label_format == NAME_hotSpot &&
		instanceOfObject(g->label, ClassImage) )
      { Image img = (Image)g->label;

	if ( notNil(img->hot_spot) )
	  *y = -valInt(img->hot_spot->y);
	else
	  *y = -th/2;
      } else /* center */
	*y = -th/2;
    }
    if ( x )
    { *x = valInt(g->radius) + valInt(getExFont(g->label_font));
    }
  } else
  { if ( x ) *x = 0;
    if ( y ) *y = 0;
    if ( w ) *w = 0;
    if ( h ) *h = 0;
  }
}


static status
computeDialogGroup(DialogGroup g)
{ if ( notNil(g->request_compute) )
  { int x, y, w, h;
    int lx, ly, lw, lh;
    Area a = g->area;
    Size border;

    obtainClassVariablesObject(g);
    border = (isDefault(g->border) ? g->gap : g->border);

    CHANGING_GRAPHICAL(g,
    { computeGraphicalsDevice((Device) g);
      compute_label(g, &lx, &ly, &lw, &lh);
      
      if ( isDefault(g->size) )		/* implicit size */
      { if ( isNil(g->layout_manager) ||
	     !qadSendv(g->layout_manager, NAME_computeBoundingBox, 0, NULL) )
	{ Cell cell;
  
	  clearArea(a);
	  for_cell(cell, g->graphicals)
	  { Graphical gr = cell->value;
	    
	    unionNormalisedArea(a, gr->area);
	  }
	}
	relativeMoveArea(a, g->offset);
  
	x = valInt(a->x) -     valInt(border->w);
	y = valInt(a->y) -     valInt(border->h);
	w = valInt(a->w) + 2 * valInt(border->w);
	h = valInt(a->h) + 2 * valInt(border->h);
      } else				/* explicit size */
      { x = valInt(g->offset->x);
	y = valInt(g->offset->y);
	w = valInt(g->size->w);
	h = valInt(g->size->h);
      }
  
      if ( w < 2*lx + lw )
	w = 2*lx + lw;

      if ( ly < 0 )
      { h -= ly;
	y += ly;
      }
  
      assign(a, x, toInt(x));
      assign(a, y, toInt(y));
      assign(a, w, toInt(w));
      assign(a, h, toInt(h));
    });

    assign(g, request_compute, NIL);
  }

  succeed;
}

		 /*******************************
		 *	       GEOMETRY		*
		 *******************************/

static status
geometryDialogGroup(DialogGroup g, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) || notDefault(h) )
  { Any size;

    if ( isDefault(w) )
      w = getWidthGraphical((Graphical) g);

    if ( isDefault(h) )
    { int ly;

      compute_label(g, NULL, &ly, NULL, NULL);
      h = getHeightGraphical((Graphical) g);
      if ( ly < 0 )
	h = toInt(valInt(h) + ly);
    }

    size = newObject(ClassSize, w, h, EAV);
    qadSendv(g, NAME_size, 1, &size);
    doneObject(size);
  }

  return geometryDevice((Device) g, x, y, w, h);
}


static status
sizeDialogGroup(DialogGroup g, Size s)
{ if ( g->size == s )
    succeed;
  if ( notDefault(g->size) &&
       notDefault(s) &&
       equalSize(g->size, s) )
    succeed;

  if ( notDefault(g->size) && notDefault(s) )
    copySize(g->size, s);
  else
    assign(g, size, s);
  
  send(g, NAME_layoutDialog, EAV);

  return requestComputeGraphical(g, DEFAULT);
}


		 /*******************************
		 *	       LAYOUT		*
		 *******************************/

static status
layoutDialogDialogGroup(DialogGroup g)
{ obtainClassVariablesObject(g);

  if ( notNil(g->layout_manager) )
  { if ( notNil(g->layout_manager->request_compute) )
      qadSendv(g->layout_manager, NAME_compute, 0, NULL);
  } else
    layoutDialogDevice((Device)g, g->gap, g->size, g->border);

  succeed;
}


static status
gapDialogGroup(DialogGroup g, Size gap)
{ if ( !equalSize(gap, g->gap) )
  { assign(g, gap, gap);
    if ( isNil(g->request_compute) && notNil(g->device) )
      send(g, NAME_layoutDialog, EAV);
  }

  succeed;
}


static status
borderDialogGroup(DialogGroup g, Size border)
{ if ( (isDefault(border) && notDefault(g->border)) ||
       (notDefault(border) && isDefault(g->border)) ||
       (notDefault(border) && notDefault(g->border) &&
	!equalSize(border, g->border)) )
  { assign(g, border, border);
    if ( isNil(g->request_compute) && notNil(g->device) )
      send(g, NAME_layoutDialog, EAV);
  }

  succeed;
}


		 /*******************************
		 *	     NAME/LABEL		*
		 *******************************/

static status
nameDialogGroup(DialogGroup g, Name name)
{ Any label;

  assign(g, name, name);

  if ( notNil(g->label) &&
       (label = get(g, NAME_labelName, name, EAV)) )
    labelDialogGroup(g, label ? label : name);

  succeed;
}


static status 
ChangedLabelDialogGroup(DialogGroup g)
{ requestComputeGraphical(g, DEFAULT);

  return changedEntireImageGraphical(g);
}


static status
labelDialogGroup(DialogGroup g, Any label)
{ if ( g->label != label )
  { assign(g, label, label);
    qadSendv(g, NAME_ChangedLabel, 0, NULL);
  }

  succeed;
}


static status
labelFontDialogGroup(DialogGroup g, FontObj font)
{ if ( g->label_font != font )
  { assign(g, label_font, font);
    qadSendv(g, NAME_ChangedLabel, 0, NULL);
  }

  succeed;
}


static status
showLabelDialogGroup(DialogGroup g, Bool show)
{ if ( (show == OFF && isNil(g->label)) ||
       (show == ON  && notNil(g->label)) )
    succeed;
  
  if ( show == OFF )
  { labelDialogGroup(g, NIL);
  } else
  { assign(g, label, DEFAULT);
    nameDialogGroup(g, g->name);
  }

  succeed;
}



status
labelFormatDialogGroup(DialogGroup g, Name fmt)
{ if ( g->label_format != fmt )
  { assign(g, label_format, fmt);
    qadSendv(g, NAME_ChangedLabel, 0, NULL);
  }

  succeed;
}


static CharArray
getLabelNameDialogGroup(DialogGroup g, Name name)
{ Any suffix, label = get(name, NAME_labelName, EAV);

  if ( label && (suffix = getClassVariableValueObject(g, NAME_labelSuffix)) )
    label = getEnsureSuffixCharArray(label, suffix);

  answer(label);
}


static status
radiusDialogGroup(DialogGroup g, Int radius)
{ return assignGraphical(g, NAME_radius, radius);
}


static status
elevationDialogGroup(DialogGroup d, Elevation e)
{ return assignGraphical(d, NAME_elevation, e);
}


		 /*******************************
		 *	       KIND		*
		 *******************************/

static status
kindDialogGroup(DialogGroup g, Name kind)
{ if ( kind == NAME_box )
  { assign(g, pen, toInt(1));
    assign(g, border, getClassVariableValueObject(g, NAME_border));
    nameDialogGroup(g, g->name);
  } else if ( kind == NAME_group )
  { assign(g, pen, toInt(0));
    assign(g, border, newObject(ClassSize, EAV));
    assign(g, label, NIL);
  } else
    fail;

  return requestComputeGraphical(g, DEFAULT);
}


		/********************************
		*             REDRAW		*
		********************************/

status
RedrawLabelDialogGroup(DialogGroup g, int acc,
		       int x, int y, int w, int h,
		       Name hadjust, Name vadjust, int flags)
{ if ( instanceOfObject(g->label, ClassImage) )
  { Image i = g->label;
    int iw = valInt(i->size->w);
    int ih = valInt(i->size->h);
    int ix, iy;

    if ( hadjust == NAME_left )
      ix = x;
    else if ( hadjust == NAME_center )
      ix = x + (w-iw)/2;
    else 
      ix = x + w-iw;
    
    if ( vadjust == NAME_top )
      iy = y;
    else if ( vadjust == NAME_center )
      iy = y + (h-ih)/2;
    else 
      iy = y + h-ih;
    
    r_image(i, 0, 0, ix, iy, iw, ih, ON);
  } else if ( instanceOfObject(g->label, ClassCharArray) )
  { CharArray label = g->label;

    str_label(&label->data, acc, g->label_font,
	      x, y, w, h,
	      hadjust, vadjust, flags);
  }

  succeed;
}


static status
RedrawAreaDialogGroup(DialogGroup g, Area a)
{ int x, y, w, h;
  int lx, ly, lw, lh;
  int eh;
  Any obg = NIL, bg = NIL;

  initialiseDeviceGraphical(g, &x, &y, &w, &h);
  compute_label(g, &lx, &ly, &lw, &lh);

  if ( g->pen != ZERO )
  { Elevation e = getClassVariableValueObject(g, NAME_elevation);

    if ( e && instanceOfObject(e, ClassElevation) )
    { int bx = x;
      int by = y-ly;
      int bw = w;
      int bh = h+ly;
      
      eh = valInt(e->height);
      r_3d_box(bx, by, bw, bh, valInt(g->radius), e, FALSE);
      bx += eh;
      by += eh;
      bw -= 2*eh;
      bh -= 2*eh;
      r_3d_box(bx, by, bw, bh, valInt(g->radius), e, TRUE);
    } else
    { eh = valInt(g->pen);
  
      r_thickness(eh);
      r_dash(g->texture);
      r_box(x, y-ly, w, h+ly, valInt(g->radius), NIL);
    }
  } else if ( notNil(g->elevation) )
  { int bx = x;
    int by = y-ly;
    int bw = w;
    int bh = h+ly;

    r_3d_box(bx, by, bw, bh, valInt(g->radius), g->elevation, TRUE);
    bg = g->elevation->background;
    eh = valInt(g->elevation->height);
  } else
    eh = 0;

  if ( notNil(g->label) && g->label != NAME_ )
  { int ex = valInt(getExFont(g->label_font));

    r_clear(x+lx-ex/2, y, lw+ex, lh);
    RedrawLabelDialogGroup(g, 0, x+lx, y, lw, lh, NAME_center, NAME_center, 0);
  }

  { Cell cell;
    Int ax = a->x, ay = a->y;
    Point offset = g->offset;
    int ox = valInt(offset->x);
    int oy = valInt(offset->y);

    d_clip(x+eh, y-ly+eh, w-2*eh, h-2*eh+ly); /* check if needed! */
    qassign(a, x, toInt(valInt(a->x) - ox));
    qassign(a, y, toInt(valInt(a->y) - oy));
    r_offset(ox, oy);

    if ( notNil(bg) )
      obg = r_background(bg);

    if ( notNil(g->layout_manager) )
      qadSendv(g->layout_manager, NAME_RedrawArea, 1, (Any*)&a);

    for_cell(cell, g->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON && overlapArea(a, gr->area) )
	RedrawArea(gr, a);
    }
    if ( notNil(obg) )
      r_background(obg);

    r_offset(-ox, -oy);
    qassign(a, x, ax);
    qassign(a, y, ay);
    d_clip_done();	     
  }

  return RedrawAreaGraphical(g, a);
}

		 /*******************************
		 *      DIALOG INTEGRATION	*
		 *******************************/

static status
WantsKeyboardFocusTextItem(DialogGroup g)
{ Cell cell;

  for_cell(cell, g->graphicals)
  { if ( qadSendv(cell->value, NAME_WantsKeyboardFocus, 0, NULL) )
      succeed;
  }

  fail;
}


status
eventDialogGroup(DialogGroup g, EventObj ev)
{ if ( isAEvent(ev, NAME_obtainKeyboardFocus) )
    return send(g, NAME_advance, EAV);
  else
    return eventDevice((Device)g, ev);
}


static Button
getDefaultButtonDialogGroup(DialogGroup g)
{ Device d;
  Cell cell;	

  for_cell(cell, g->graphicals)
  { if ( instanceOfObject(cell->value, ClassButton) )
    { Button b = cell->value;

      if ( b->default_button == ON )
	answer(b);
    }
  }

  for(d= g->device; notNil(d); d = d->device)
  { if ( hasGetMethodObject(d, NAME_defaultButton) )
      answer(get(d, NAME_defaultButton, EAV));
  }

  fail;
}


static status
applyDialogGroup(DialogGroup g, Bool always)
{ DialogItem di;
  Graphical defb;
  
  for_chain(g->graphicals, di, send(di, NAME_apply, always, EAV));
  if ( (defb = get(g, NAME_defaultButton, EAV)) )
    send(defb, NAME_active, OFF, EAV);

  succeed;
}


static status
restoreDialogGroup(DialogGroup g)
{ DialogItem di;
  Graphical defb;
  
  for_chain(g->graphicals, di, send(di, NAME_restore, EAV));
  if ( (defb = get(g, NAME_defaultButton, EAV)) )
    send(defb, NAME_active, OFF, EAV);

  succeed;
}


static status
modifiedItemDialogGroup(DialogGroup g, Graphical gr, Bool m)
{ Button b;

  if ( m == ON )
  { if ( (b = get(g, NAME_defaultButton, EAV)) )
      return send(b, NAME_active, ON, EAV);
    if ( notNil(g->device) )
      return send(g->device, NAME_modifiedItem, g, ON, EAV); /* or gr? */
  }

  fail;
}






		 /*******************************
		 *	       DIALOG		*
		 *******************************/

static status
appendDialogGroup(DialogGroup g, Graphical item, Name where)
{ return appendDialogItemDevice((Device) g, item, where);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char T_kind[] = "kind=[{box,group}]";
static char *T_initialise[] =
	{ "name=[name]", T_kind };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_append[] =
        { "item=graphical", "relative_to_last=[{below,right,next_row}]" };
static char *T_modifiedItem[] =
        { "item=graphical", "modified=bool" };

/* Instance Variables */

static vardecl var_diagroup[] =
{ IV(NAME_label, "[name|image]*", IV_GET,
     NAME_label, "Displayed label"),
  SV(NAME_labelFont, "font", IV_GET|IV_STORE, labelFontDialogGroup,
     NAME_appearance, "Font used to display textual label"),
  SV(NAME_labelFormat, "{top,center,bottom,hot_spot}", IV_GET|IV_STORE,
     labelFormatDialogGroup,
     NAME_appearance, "Alignment of label with top"),
  SV(NAME_elevation, "elevation*", IV_GET|IV_STORE, elevationDialogGroup,
     NAME_appearance, "Elevation from background"),
  SV(NAME_radius, "0..", IV_GET|IV_STORE, radiusDialogGroup,
     NAME_appearance, "Radius for the corners"),
  IV(NAME_size, "[size]", IV_GET,
     NAME_area, "Size of the contents"),
  SV(NAME_gap, "size", IV_GET|IV_STORE, gapDialogGroup,
     NAME_appearance, "Distance between the items"),
  SV(NAME_border, "[size]", IV_GET|IV_STORE, borderDialogGroup,
     NAME_appearance, "Space around the items"),
  IV(NAME_autoAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align in dialog (@on)"),
  IV(NAME_alignment, "{column,left,center,right}", IV_BOTH,
     NAME_layout, "Align in columns or right of item to the left")
};

/* Send Methods */

static senddecl send_diagroup[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseDialogGroup,
     DEFAULT, "Create a new group of dialog items"),
  SM(NAME_geometry, 4, T_geometry, geometryDialogGroup,
     DEFAULT, "Move/resize dialog group"),
  SM(NAME_position, 1, "point", positionGraphical,
     NAME_area, "Top-left corner of tab"),
  SM(NAME_x, 1, "int", xGraphical,
     NAME_area, "Left-side of object"),
  SM(NAME_y, 1, "int", yGraphical,
     NAME_area, "Top-side of object"),
  SM(NAME_size, 1, "[size]", sizeDialogGroup,
     NAME_area, "Size, @default implies minimal size"),
  SM(NAME_layoutDialog, 0, NULL, layoutDialogDialogGroup,
     NAME_layout, "(Re)compute layout of dialog_items"),
  SM(NAME_label, 1, "name|image", labelDialogGroup,
     NAME_name, "Change visual label"),
  SM(NAME_name, 1, "name", nameDialogGroup,
     NAME_name, "Change <-name, update <-label"),
  SM(NAME_kind, 1, T_kind, kindDialogGroup,
     NAME_appearance, "Set standard default appearance"),
  SM(NAME_reference, 1, "point", referenceGraphical,
     NAME_layout, "Dialog item integration"),
  SM(NAME_append, 2, T_append, appendDialogGroup,
     NAME_organisation, "Append dialog_item {below,right,next_row} last"),
  SM(NAME_compute, 0, NULL, computeDialogGroup,
     NAME_update, "Recompute area"),
  SM(NAME_ChangedLabel, 0, NULL, ChangedLabelDialogGroup,
     NAME_update, "Add label-area to the update"),
  SM(NAME_event, 1, "event", eventDialogGroup,
     NAME_event, "Process an event"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusTextItem,
     NAME_event, "Test if a member is ready for input"),
  SM(NAME_apply, 1, "always=[bool]", applyDialogGroup,
     NAME_apply, "->apply all changed items"),
  SM(NAME_modifiedItem, 2, T_modifiedItem, modifiedItemDialogGroup,
     NAME_apply, "Indicates item has changed state"),
  SM(NAME_restore, 0, NULL, restoreDialogGroup,
     NAME_apply, "->restore all items to their <-default"),
  SM(NAME_showLabel, 1, "bool", showLabelDialogGroup,
     NAME_appearance, "Whether label is visible")
};

/* Get Methods */

static getdecl get_diagroup[] =
{ GM(NAME_position, 0, "point", NULL, getPositionGraphical,
     NAME_area, "Top-left corner of tab"),
  GM(NAME_x, 0, "int", NULL, getXGraphical,
     NAME_area, "Left-side of tab"),
  GM(NAME_y, 0, "int", NULL, getYGraphical,
     NAME_area, "Top-side of tab"),
  GM(NAME_labelName, 1, "name", "name", getLabelNameDialogGroup,
     NAME_label, "Determine default-label from the name"),
  GM(NAME_defaultButton, 0, "button", NULL, getDefaultButtonDialogGroup,
     NAME_accelerator, "Current Button connected to `RET'")
};

/* Resources */

static classvardecl rc_diagroup[] =
{ RC(NAME_alignment, "{column,left,center,right}", "column",
     "Alignment in the row"),
  RC(NAME_elevation, "elevation*",
   "when(@colour_display, 1, @nil)",
     "Elevation above environment"),
  RC(NAME_radius, "0..", "0",
     "Radius for the corners"),
  RC(NAME_gap, "size", "size(15,8)",
     "Distance between items in X and Y"),
  RC(NAME_border, "[size]", "@default",
     "Distance around the items in X and Y"),
  RC(NAME_labelFont, "font", "bold",
     "Font used to display the label"),
  RC(NAME_labelFormat, "{top,center,bottom}", "center",
     "Alignment of label with top-line"),
  RC(NAME_labelSuffix, "name", "",
     "Ensured suffix of label")
};

/* Class Declaration */

ClassDecl(diagroup_decls,
          var_diagroup, send_diagroup, get_diagroup, rc_diagroup,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassDialogGroup(Class class)
{ declareClass(class, &diagroup_decls);
  setRedrawFunctionClass(class, RedrawAreaDialogGroup);

  succeed;
}

