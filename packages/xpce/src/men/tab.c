/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status nameTab(Tab t, Name name);
static status labelTab(Tab t, Name name);
static status layoutTab(Tab t);


		/********************************
		*            CREATE		*
		********************************/

status
initialiseTab(Tab t, Name name)
{ initialiseDevice((Device) t);

  if ( isDefault(name) )
    name = getClassNameObject(t);

  assign(t, label_font,   DEFAULT);	/* Resource */
  assign(t, label_size,   DEFAULT);	/* Resource */
  assign(t, label_format, DEFAULT);	/* Resource */
  assign(t, label_offset, ZERO);
  assign(t, elevation,	  DEFAULT);	/* Resource */
  assign(t, status,	  NAME_onTop);
  assign(t, size,	  DEFAULT);
  assign(t, gap,	  DEFAULT);	/* Resource */
  assign(t, auto_align,	  ON);


  nameTab(t, name);

  succeed;
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static status
computeTab(Tab t)
{ if ( notNil(t->request_compute) )
  { int x, y, w, h;
    Area a = t->area;

    obtainResourcesObject(t);
/*  layoutTab(t);	 */		/* place the items */
    
    if ( isDefault(t->size) )		/* implicit size */
    { Cell cell;

      clearArea(a);
      for_cell(cell, t->graphicals)
      { Graphical gr = cell->value;
	
	unionNormalisedArea(a, gr->area);
      }
      relativeMoveArea(a, t->offset);

      w = valInt(a->w) + 2 * valInt(t->gap->w);
      h = valInt(a->h) + 2 * valInt(t->gap->h);
    } else				/* explicit size */
    { w = valInt(t->size->w);
      h = valInt(t->size->h);
    }

    h += valInt(t->label_size->h);
    x = valInt(t->offset->x);
    y = valInt(t->offset->y) - valInt(t->label_size->h);

    CHANGING_GRAPHICAL(t,
	assign(a, x, toInt(x));
	assign(a, y, toInt(y));
	assign(a, w, toInt(w));
	assign(a, h, toInt(h)));

    assign(t, request_compute, NIL);
  }

  succeed;
}

		 /*******************************
		 *	       GEOMETRY		*
		 *******************************/

static status
geometryTab(Tab t, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) || notDefault(h) )
  { if ( isDefault(w) )
      w = getWidthGraphical((Graphical) t);
    if ( isDefault(h) )
      h = getHeightGraphical((Graphical) t);

    assign(t, size, newObject(ClassSize, w, h, 0));
  }

  geometryDevice((Device) t, x, y, w, h);
  requestComputeGraphical(t, DEFAULT);

  succeed;
}


static status
sizeTab(Tab t, Size s)
{ assign(t, size, s);

  return requestComputeGraphical(t, DEFAULT);
}


		 /*******************************
		 *	       LAYOUT		*
		 *******************************/

static status
layoutTab(Tab t)
{ obtainResourcesObject(t);

  return layoutDialogDevice((Device)t, t->gap, t->size);
}


		 /*******************************
		 *	     NAME/LABEL		*
		 *******************************/

static status
nameTab(Tab t, Name name)
{ Any label = get(t, NAME_labelName, name, 0);

  assign(t, name, name);
  return labelTab(t, label ? label : name);
}


static void
changedLabelTab(Tab t)
{ Int eh;

  assign(t, request_compute, ON);
  computeTab(t);
  eh = t->elevation->height;

  changedImageGraphical(t,
			t->label_offset, ZERO,
			t->label_size->w,
			add(t->label_size->h, eh));
}


static status
labelTab(Tab t, Name label)
{ if ( t->label != label )
  { assign(t, label, label);
    changedLabelTab(t);
  }

  succeed;
}


static status
labelFontTab(Tab t, FontObj font)
{ if ( t->label_font != font )
  { assign(t, label_font, font);
    changedLabelTab(t);
  }

  succeed;
}


static status
labelFormatTab(Tab t, Name fmt)
{ if ( t->label_format != fmt )
  { assign(t, label_format, fmt);
    changedLabelTab(t);
  }

  succeed;
}


static status
labelOffsetTab(Tab t, Int offset)
{ if ( t->label_offset != offset )
  { int chl, chr;

    chl = valInt(t->label_offset);
    chr = chl + valInt(t->label_size->w);

    assign(t, label_offset, offset);
    if ( valInt(offset) < chl )
      chl = valInt(offset);		/* shift left */
    else
      chr = valInt(offset) + valInt(t->label_size->w); /* shift right */

    changedImageGraphical(t,
			  toInt(chl), ZERO,
			  toInt(chr), t->label_size->h);
  }

  succeed;
}


static CharArray
getLabelNameTab(Tab t, Name name)
{ Any label = getLabelNameName(name);

  answer(label);
}


static status
elevationTab(Tab t, Elevation e)
{ return assignGraphical(t, NAME_elevation, e);
}


static status
statusTab(Tab t, Name stat)
{ return assignGraphical(t, NAME_status, stat);
}



		/********************************
		*             REDRAW		*
		********************************/

static status
RedrawAreaTab(Tab t, Area a)
{ int x, y, w, h;
  int lh   = valInt(t->label_size->h);
  int lw   = valInt(t->label_size->w);
  int loff = valInt(t->label_offset);
  int eh = valInt(t->elevation->height);
  int ex = valInt(getExFont(t->label_font));

  initialiseDeviceGraphical(t, &x, &y, &w, &h);

  if ( t->status == NAME_onTop )
  { r_3d_box(x, y+lh, w, h-lh, 0, t->elevation, TRUE); /* main box */
    r_3d_box(x+loff, y, lw, lh+eh, 0, t->elevation, TRUE); /* label box */
    if ( notDefault(t->elevation->colour) )
    { r_fill(x+loff+eh, y+lh, lw-2*eh, eh, t->elevation->colour);
    } else
    { r_clear(x+loff+eh, y+lh, lw-2*eh, eh);
    }

    str_string(&t->label->data, t->label_font,
	       x+loff+ex, y, lw-2*ex, lh, t->label_format, NAME_center);

    { Cell cell;
      Int ax = a->x, ay = a->y;
      Point offset = t->offset;
      int ox = valInt(offset->x);
      int oy = valInt(offset->y);

      assign(a, x, toInt(valInt(a->x) - ox));
      assign(a, y, toInt(valInt(a->y) - oy));
      r_offset(ox, oy);

      d_clip(x+eh, y+eh, w-2*eh, h-2*eh); /* check if needed! */
      for_cell(cell, t->graphicals)
      { Graphical gr = cell->value;

	if ( gr->displayed == ON && overlapArea(a, gr->area) )
	  RedrawArea(gr, a);
      }
      d_clip_done();	     

      r_offset(-ox, -oy);
      assign(a, x, ax);
      assign(a, y, ay);
    }
  } else /* if ( t->status == NAME_hidden ) */
  { r_3d_box(x+loff, y, lw, lh, 0, t->elevation, TRUE); /* integrate! */
    if ( notDefault(t->elevation->colour) )
    { r_fill(x+loff+eh, y+lh-eh, lw-2*eh, eh, t->elevation->colour);
    } else
    { r_clear(x+loff+eh, y+lh-eh, lw-2*eh, eh);
    }

    str_string(&t->label->data, t->label_font,
	       x+loff+ex, y, lw-2*ex, lh, t->label_format, NAME_center);
  }

  return RedrawAreaGraphical(t, a);
}

		 /*******************************
		 *	       DIALOG		*
		 *******************************/

static status
appendTab(Tab t, Graphical item, Name where)
{ return appendDialogItemDevice((Device) t, item, where);
}


		 /*******************************
		 *	       EVENT		*
		 *******************************/

static status
eventTab(Tab t, EventObj ev)
{ Int X, Y;
  int x, y;

  get_xy_event(ev, t, OFF, &X, &Y);
  x = valInt(X), y = valInt(Y);

  if ( y < 0 )				/* tab-bar */
  { if ( isDownEvent(ev) &&
	 y > -valInt(t->label_size->h) &&
	 x > valInt(t->label_offset) &&
	 x < valInt(t->label_offset) + valInt(t->label_size->w) )
    { send(t->device, NAME_onTop, t, 0); /* TBD: use gesture? */
      succeed;
    }

    fail;				/* pass to next one */
  }

  if ( t->status == NAME_onTop )
  { eventDevice(t, ev);
    succeed;				/* don't continue */
  }

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_append[] =
        { "item=graphical", "relative_to_last=[{below,right,next_row}]" };

/* Instance Variables */

static vardecl var_tab[] =
{ IV(NAME_label, "name", IV_GET,
     NAME_label, "Text displayed in the tab-label"),
  SV(NAME_labelFont, "font", IV_GET|IV_STORE, labelFontTab,
     NAME_appearance, "Font used to display the label"),
  IV(NAME_labelSize, "size", IV_GET,
     NAME_layout, "Size of the label-box"),
  SV(NAME_labelOffset, "int", IV_GET|IV_STORE, labelOffsetTab,
     NAME_layout, "X-Offset of label-box"),
  SV(NAME_labelFormat, "{left,center,right}", IV_GET|IV_STORE, labelFormatTab,
     NAME_appearance, "Alignment of label in box"),
  SV(NAME_elevation, "elevation*", IV_GET|IV_STORE, elevationTab,
     NAME_appearance, "Elevation from background"),
  SV(NAME_status, "{on_top,hidden}", IV_GET|IV_STORE, statusTab,
     NAME_appearance, "Currently displayed status"),
  IV(NAME_size, "[size]", IV_GET,
     NAME_geometry, "Size of the contents"),
  IV(NAME_autoAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align in dialog (@on)"),
  IV(NAME_gap, "size", IV_GET,
     NAME_appearance, "Distance between the items")
};

/* Send Methods */

static senddecl send_tab[] =
{ SM(NAME_initialise, 1, "name=[name]", initialiseTab,
     DEFAULT, "Create a new tab-entry"),
  SM(NAME_geometry, 4, T_geometry, geometryTab,
     DEFAULT, "Move/resize tab"),
  SM(NAME_event, 1, "event", eventTab,
     NAME_event, "Process event"),
  SM(NAME_position, 1, "point", positionGraphical,
     NAME_geometry, "Top-left corner of tab"),
  SM(NAME_x, 1, "int", xGraphical,
     NAME_geometry, "Left-side of tab"),
  SM(NAME_y, 1, "int", yGraphical,
     NAME_geometry, "Top-side of tab"),
  SM(NAME_size, 1, "[size]", sizeTab,
     NAME_geometry, "Size, @default implies minimal size"),
  SM(NAME_layout, 0, NULL, layoutTab,
     NAME_layout, "(Re)compute layout of dialog_items"),
  SM(NAME_label, 1, "name", labelTab,
     NAME_name, "Change visual label"),
  SM(NAME_name, 1, "name", nameTab,
     NAME_name, "Change <-name, update <-label"),
  SM(NAME_append, 2, T_append, appendTab,
     NAME_organisation, "Append dialog_item {below,right,next_row} last"),
  SM(NAME_compute, 0, NULL, computeTab,
     NAME_update, "Recompute area")
};

/* Get Methods */

static getdecl get_tab[] =
{ GM(NAME_position, 0, "point", NULL, getPositionGraphical,
     NAME_geometry, "Top-left corner of tab"),
  GM(NAME_x, 0, "int", NULL, getXGraphical,
     NAME_geometry, "Left-side of tab"),
  GM(NAME_y, 0, "int", NULL, getYGraphical,
     NAME_geometry, "Top-side of tab"),
  GM(NAME_labelName, 1, "name", "name", getLabelNameTab,
     NAME_label, "Determine default-label from the name")
};

/* Resources */

static resourcedecl rc_tab[] =
{ RC(NAME_elevation, "elevation",
     "when(@colour_display, " /* concat */
           "1, " /* concat */
     	   "elevation(tab, 2, relief := @grey50_image, shadow := black))",
     "Elevation above environment"),
  RC(NAME_gap, "size", "size(15,8)",
     "Distance between items in X and Y"),
  RC(NAME_labelFont, "font", "normal",
     "Font used to display the label"),
  RC(NAME_labelFormat, "{left,center,right}", "left",
     "Alignment of label in box"),
  RC(NAME_labelSize, "size", "size(80, 20)",
     "Size of box for label")
};

/* Class Declaration */

ClassDecl(tab_decls,
          var_tab, send_tab, get_tab, rc_tab,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassTab(Class class)
{ declareClass(class, &tab_decls);
  setRedrawFunctionClass(class, RedrawAreaTab);

  succeed;
}

