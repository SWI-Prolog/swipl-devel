/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status nameDialogGroup(DialogGroup g, Name name);
static status labelDialogGroup(DialogGroup g, Name name);


		/********************************
		*            CREATE		*
		********************************/

status
initialiseDialogGroup(DialogGroup g, Name name)
{ initialiseDevice((Device) g);

  if ( isDefault(name) )
    name = getClassNameObject(g);

  assign(g, label_font,   DEFAULT);	/* Resource */
  assign(g, label_format, DEFAULT);	/* Resource */
  assign(g, radius,       DEFAULT);	/* Resource */
  assign(g, size,	  DEFAULT);
  assign(g, gap,	  DEFAULT);	/* Resource */
  assign(g, auto_align,	  ON);
  assign(g, alignment,	  DEFAULT);	/* Resource */

  nameDialogGroup(g, name);

  succeed;
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static void
compute_label(DialogGroup g, int *x, int *y, int *w, int *h)
{ int tw, th;

  if ( isDefault(g->label_font) )
    obtainResourcesObject(g);

  if ( g->label != NAME_ )
    str_size(&g->label->data, g->label_font, &tw, &th);
  else
    tw = th = 0;

  if ( w ) *w = tw;
  if ( h ) *h = th;
  if ( y )
  { if ( g->label_format == NAME_top )
      *y = -th;
    else if ( g->label_format == NAME_bottom )
      *y = 0;
    else /* center */
      *y = -th/2;
  }
  if ( x )
    *x = valInt(g->radius) + valInt(getExFont(g->label_font));
}


static status
computeDialogGroup(DialogGroup g)
{ if ( notNil(g->request_compute) )
  { int x, y, w, h;
    int lx, ly, lw, lh;
    Area a = g->area;

    obtainResourcesObject(g);
    computeGraphicalsDevice((Device) g);
    compute_label(g, &lx, &ly, &lw, &lh);
    
    if ( isDefault(g->size) )		/* implicit size */
    { Cell cell;

      clearArea(a);
      for_cell(cell, g->graphicals)
      { Graphical gr = cell->value;
	
	unionNormalisedArea(a, gr->area);
      }
      relativeMoveArea(a, g->offset);

      w = valInt(a->w) + 2 * valInt(g->gap->w);
      h = valInt(a->h) + 2 * valInt(g->gap->h);
    } else				/* explicit size */
    { w = valInt(g->size->w);
      h = valInt(g->size->h);
    }

    if ( ly < 0 )
      h -= ly;
    x = valInt(g->offset->x);
    y = valInt(g->offset->y) + (ly < 0 ? ly : 0);

    CHANGING_GRAPHICAL(g,
	assign(a, x, toInt(x));
	assign(a, y, toInt(y));
	assign(a, w, toInt(w));
	assign(a, h, toInt(h)));

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

    size = newObject(ClassSize, w, h, 0);
    qadSendv(g, NAME_size, 1, &size);
  }

  geometryDevice((Device) g, x, y, w, h);
  requestComputeGraphical(g, DEFAULT);

  succeed;
}


static status
sizeDialogGroup(DialogGroup g, Size s)
{ assign(g, size, s);

  if ( !parentGoal(VmiSend, g, NAME_layout) )
    send(g, NAME_layoutDialog, 0);

  return requestComputeGraphical(g, DEFAULT);
}


		 /*******************************
		 *	       LAYOUT		*
		 *******************************/

static status
layoutDialogDialogGroup(DialogGroup g)
{ obtainResourcesObject(g);

  return layoutDialogDevice((Device)g, g->gap, g->size);
}


		 /*******************************
		 *	     NAME/LABEL		*
		 *******************************/

static status
nameDialogGroup(DialogGroup g, Name name)
{ Any label = get(g, NAME_labelName, name, 0);

  assign(g, name, name);
  return labelDialogGroup(g, label ? label : name);
}


static status 
ChangedLabelDialogGroup(DialogGroup g)
{ assign(g, request_compute, ON);

  return changedEntireImageGraphical(g);
}


static status
labelDialogGroup(DialogGroup g, Name label)
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
{ Any label = get(name, NAME_labelName, 0);

  answer(label);
}


static status
radiusDialogGroup(DialogGroup g, Int radius)
{ return assignGraphical(g, NAME_radius, radius);
}


		/********************************
		*             REDRAW		*
		********************************/

static status
RedrawAreaDialogGroup(DialogGroup g, Area a)
{ int x, y, w, h;
  int lx, ly, lw, lh;
  int eh;
  int ex = valInt(getExFont(g->label_font));

  initialiseDeviceGraphical(g, &x, &y, &w, &h);
  compute_label(g, &lx, &ly, &lw, &lh);

  if ( g->pen != ZERO )
  { Elevation e = getResourceValueObject(g, NAME_elevation);

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
  } else
    eh = 0;

  r_clear(x+lx-ex/2, y, lw+ex, lh);
  str_string(&g->label->data, g->label_font,
	     x+lx, y, lw, lh, NAME_center, NAME_center);

  { Cell cell;
    Int ax = a->x, ay = a->y;
    Point offset = g->offset;
    int ox = valInt(offset->x);
    int oy = valInt(offset->y);

    d_clip(x+eh, y-ly+eh, w-2*eh, h-2*eh+ly); /* check if needed! */
    assign(a, x, toInt(valInt(a->x) - ox));
    assign(a, y, toInt(valInt(a->y) - oy));
    r_offset(ox, oy);

    for_cell(cell, g->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON && overlapArea(a, gr->area) )
	RedrawArea(gr, a);
    }
    d_clip_done();	     
    
    r_offset(-ox, -oy);
    assign(a, x, ax);
    assign(a, y, ay);
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
    return send(g, NAME_advance, 0);
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
      answer(get(d, NAME_defaultButton, 0));
  }

  fail;
}


static status
applyDialogGroup(DialogGroup g, Bool always)
{ DialogItem di;
  Graphical defb;
  
  for_chain(g->graphicals, di, send(di, NAME_apply, always, 0));
  if ( (defb = get(g, NAME_defaultButton, 0)) )
    send(defb, NAME_active, OFF, 0);

  succeed;
}


static status
restoreDialogGroup(DialogGroup g)
{ DialogItem di;
  Graphical defb;
  
  for_chain(g->graphicals, di, send(di, NAME_restore, 0));
  if ( (defb = get(g, NAME_defaultButton, 0)) )
    send(defb, NAME_active, OFF, 0);

  succeed;
}


static status
modifiedItemDialogGroup(DialogGroup g, Graphical gr, Bool m)
{ Button b;

  if ( (b = get(g, NAME_defaultButton, 0)) )
    return send(b, NAME_active, ON, 0);

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

static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_append[] =
        { "item=graphical", "relative_to_last=[{below,right,next_row}]" };
static char *T_modifiedItem[] =
        { "item=graphical", "modified=bool" };

/* Instance Variables */

static vardecl var_diagroup[] =
{ IV(NAME_label, "name", IV_GET,
     NAME_label, "Text displayed for the label"),
  SV(NAME_labelFont, "font", IV_GET|IV_STORE, labelFontDialogGroup,
     NAME_appearance, "Font used to display the label"),
  SV(NAME_labelFormat, "{top,center,bottom}", IV_GET|IV_STORE,
     labelFormatDialogGroup,
     NAME_appearance, "Alignment of label with top"),
  SV(NAME_radius, "0..", IV_GET|IV_STORE, radiusDialogGroup,
     NAME_appearance, "Radius for the corners"),
  IV(NAME_size, "[size]", IV_GET,
     NAME_geometry, "Size of the contents"),
  IV(NAME_gap, "size", IV_GET,
     NAME_appearance, "Distance between the items"),
  IV(NAME_autoAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align in dialog (@on)"),
  IV(NAME_alignment, "{column,left,center,right}", IV_BOTH,
     NAME_layout, "Align in columns or right of item to the left")
};

/* Send Methods */

static senddecl send_diagroup[] =
{ SM(NAME_initialise, 1, "name=[name]", initialiseDialogGroup,
     DEFAULT, "Create a new group of dialog items"),
  SM(NAME_geometry, 4, T_geometry, geometryDialogGroup,
     DEFAULT, "Move/resize tab"),
  SM(NAME_position, 1, "point", positionGraphical,
     NAME_geometry, "Top-left corner of tab"),
  SM(NAME_x, 1, "int", xGraphical,
     NAME_geometry, "Left-side of tab"),
  SM(NAME_y, 1, "int", yGraphical,
     NAME_geometry, "Top-side of tab"),
  SM(NAME_size, 1, "[size]", sizeDialogGroup,
     NAME_geometry, "Size, @default implies minimal size"),
  SM(NAME_layoutDialog, 0, NULL, layoutDialogDialogGroup,
     NAME_layout, "(Re)compute layout of dialog_items"),
  SM(NAME_label, 1, "name", labelDialogGroup,
     NAME_name, "Change visual label"),
  SM(NAME_name, 1, "name", nameDialogGroup,
     NAME_name, "Change <-name, update <-label"),
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
     NAME_apply, "->restore all items to their <-default")
};

/* Get Methods */

static getdecl get_diagroup[] =
{ GM(NAME_position, 0, "point", NULL, getPositionGraphical,
     NAME_geometry, "Top-left corner of tab"),
  GM(NAME_x, 0, "int", NULL, getXGraphical,
     NAME_geometry, "Left-side of tab"),
  GM(NAME_y, 0, "int", NULL, getYGraphical,
     NAME_geometry, "Top-side of tab"),
  GM(NAME_labelName, 1, "name", "name", getLabelNameDialogGroup,
     NAME_label, "Determine default-label from the name"),
  GM(NAME_defaultButton, 0, "button", NULL, getDefaultButtonDialogGroup,
     NAME_accelerator, "Current Button connected to `RET'")
};

/* Resources */

static resourcedecl rc_diagroup[] =
{ RC(NAME_alignment, "{column,left,center,right}", "column",
     "Alignment in the row"),
  RC(NAME_elevation, "elevation*",
   "when(@colour_display, 1, @nil)",
     "Elevation above environment"),
  RC(NAME_radius, "0..", "0",
     "Radius for the corners"),
  RC(NAME_gap, "size", "size(15,8)",
     "Distance between items in X and Y"),
  RC(NAME_labelFont, "font", "bold",
     "Font used to display the label"),
  RC(NAME_labelFormat, "{top,center,bottom}", "center",
     "Alignment of label with top-line"),
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

