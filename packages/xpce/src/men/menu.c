/*  $Id$

    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

static FontObj	getFontMenuItemMenu(Menu m, MenuItem mi);
static MenuItem	getItemSelectionMenu(Menu m);
static status	nextMenu(Menu m);
static status	kindMenu(Menu m, Name kind);
static status	ensureSingleSelectionMenu(Menu m);
static status	multipleSelectionMenu(Menu m, Bool val);
static status	restoreMenu(Menu m);
static status	compute_popup_indicator(Menu m, MenuItem mi, int *w, int *h);
static status	modifiedMenu(Menu m, Bool val);

#define CYCLE_DROP_WIDTH 14
#define CYCLE_DROP_HEIGHT 14
#define CYCLE_TRIANGLE_WIDTH 9
#define CYCLE_TRIANGLE_HEIGHT 8
#define CYCLE_DROP_DISTANCE 5

#define MARK_DIAMOND_SIZE 14
#define MARK_BOX_SIZE     13
#define MARK_CIRCLE_SIZE  8

status
initialiseMenu(Menu m, Name name, Name kind, Code msg)
{ createDialogItem(m, name);
 
  assign(m, message,		msg);
  assign(m, members,	        newObject(ClassChain, 0));
  assign(m, multiple_selection, OFF);

  assign(m, preview,		NIL);

  assign(m, kind,		kind);
  assign(m, columns,            ONE);
  
  assign(m, left_offset,	ZERO);
  assign(m, right_offset,	ZERO);
  assign(m, label_area,		NIL);
  assign(m, item_offset,	newObject(ClassPoint, 0));
  assign(m, item_size,		newObject(ClassSize, 0));
  obtainClassVariablesObject(m);

  kindMenu(m, kind);

  return requestComputeGraphical(m, NAME_assignAccelerators);
}


static status
unlinkMenu(Menu m)
{ Cell cell;

  for_cell(cell, m->members)
  { MenuItem mi = cell->value;
    
    assign(mi, menu, NIL);
  }
  clearChain(m->members);

  return unlinkDialogItem((DialogItem) m);
}


		/********************************
		*          COMPUTING		*
		********************************/

static void
rows_and_cols(Menu m, int *rows, int *cols)
{ int size = valInt(getSizeChain(m->members));
  
  *cols = valInt(m->columns);
  *cols = min(*cols, size);
  *rows = (*cols == 0 ? 0 : (size + *cols - 1) / *cols);

  DEBUG(NAME_columns, Cprintf("%d rows; %d cols\n", *rows, *cols));
}


static int
x_gap(Menu m)
{ int iw = valInt(m->item_size->w);
  int gx = valInt(m->gap->w);
  int vw = valInt(m->value_width);
  int gap = iw + gx > vw ? gx : vw - iw;

  return gap == 0 ? -valInt(m->pen) : gap;
}


static int
y_gap(Menu m)
{ int gap = valInt(m->gap->h);

  return gap == 0 ? -valInt(m->pen) : gap;
}


void
area_menu_item(Menu m, MenuItem mi, int *x, int *y, int *w, int *h)
{ *w = valInt(m->item_size->w);
  *h = valInt(m->item_size->h);

  *x = valInt(m->item_offset->x) + valInt(m->margin);
  *y = valInt(m->item_offset->y);

  if ( m->feedback != NAME_showSelectionOnly )
  { int index = valInt(getIndexChain(m->members, mi)) - 1; /* 0-based */
    int rows, cols;
    int gx = x_gap(m);
    int gy = y_gap(m);

    *w += gx;				/* when pen != 0 or box feedback */
    *h += gy;

    rows_and_cols(m, &rows, &cols);
    if ( m->layout == NAME_horizontal )
    { *x += (index % rows) * (*w);
      *y += (index / rows) * (*h);
    } else
    { *x += (index / rows) * (*w);
      *y += (index % rows) * (*h);
    }
  }
}


static status
computeLabelMenu(Menu m)
{ int iox;				/* item_offset <- x */
  int ioy;				/* item_offset <- y */

  obtainClassVariablesObject(m);
  if ( isDefault(m->show_label) )
    assign(m, show_label, getClassVariableValueObject(m, NAME_showLabel));

  if ( m->show_label == ON )
  { int w, h;

    if ( isNil(m->label_area) )
      assign(m, label_area, newObject(ClassArea, 0));
    
    dia_label_size(m, &w, &h, NULL);
    if ( m->layout == NAME_horizontal )
      w += valInt(getExFont(m->label_font));
    setArea(m->label_area, DEFAULT, DEFAULT, toInt(w), toInt(h));
    
    if ( m->layout == NAME_vertical )
    { iox = 0;
      ioy = h;
    } else
    { iox = w;
      ioy = 0;
    }
  } else
  { assign(m, label_area, NIL);
    iox = ioy = 0;
  }
  
  if ( notDefault(m->label_width) &&
       m->layout == NAME_horizontal &&
       valInt(m->label_width) > iox )
    iox = valInt(m->label_width);

  if ( m->feedback == NAME_showSelectionOnly )
  { Any ci = getClassVariableValueObject(m, NAME_cycleIndicator);
    
    if ( (Name)ci == NAME_comboBox )
    { iox += 0;
    } else if ( instanceOfObject(ci, ClassElevation) )
    { iox += CYCLE_DROP_WIDTH + CYCLE_DROP_DISTANCE;
    } else /* if ( instanceOfObject(ci, ClassImage) ) */
    { Image i = ci;
      iox += valInt(i->size->w) + CYCLE_DROP_DISTANCE;
    }
  }

  assign(m->item_offset, x, toInt(iox));
  assign(m->item_offset, y, toInt(ioy));

  succeed;
}


static void
size_menu_item(Menu m, MenuItem mi, int *w, int *h)
{ if ( instanceOfObject(mi->label, ClassImage) )
  { Image image = (Image) mi->label;
  
    *w = valInt(image->size->w);
    *h = valInt(image->size->h);
  } else if ( isName(mi->label) )
  { FontObj f = getFontMenuItemMenu(m, mi);
    int fw = valInt(getExFont(f));

    str_size(&((CharArray)mi->label)->data, f, w, h);
    *w += fw;
  } else
    *w = *h = 0;
}


static status
computeItemsMenu(Menu m)
{ int w = 0, h = 0, iw, ih;
  int rm = 0, lm = 0;
  Cell cell;
  int border = valInt(m->border);
  int popup = (instanceOfObject(m, ClassPopup) ? 0 : 1);

  for_cell(cell, m->members)
  { MenuItem mi = cell->value;

    size_menu_item(m, mi, &iw, &ih);
    w = max(w, iw);
    h = max(h, ih);

    if ( notNil(mi->popup) && !popup++ )
    { int iw, ih;

      compute_popup_indicator(m, mi, &iw, &ih);
      rm = max(rm, iw+border);
    }
  }

  w += 2 * border;
  h += 2 * border;

  if ( m->feedback == NAME_showSelectionOnly )
  { Image ci = getClassVariableValueObject(m, NAME_cycleIndicator);

    if ( (Name)ci == NAME_comboBox )
      rm = ws_combo_box_width();
  } else
  { if ( notNil(m->on_image) || notNil(m->off_image) )
    { int cw, ch;

      if ( instanceOfObject(m->on_image, ClassImage) )
	lm = valInt(m->on_image->size->w);
      else if ( (Name)m->on_image == NAME_marked )
      { ws_checkbox_size(0, &cw, &ch);
	lm = cw;
      }
  
      if ( instanceOfObject(m->off_image, ClassImage) )
	lm = max(lm, valInt(m->off_image->size->w));
      else if ( (Name)m->off_image == NAME_marked )
      { ws_checkbox_size(0, &cw, &ch);
	lm = max(lm, cw);
      }
  
      lm += 5;				/* TBD: Parameter? */
    }
  }
  
  if ( isDefault(m->accelerator_font) )
    assign(m, accelerator_font,
	   getClassVariableValueObject(m, NAME_acceleratorFont));

  if ( notNil(m->accelerator_font) )
  { int am = 0;
    FontObj f = m->accelerator_font;
    
    for_cell(cell, m->members)
    { MenuItem mi = cell->value;
      int aw, ah;

      if ( notNil(mi->accelerator) )
      { str_size(&mi->accelerator->data, f, &aw, &ah);
	am = max(am, aw);
      }
    }

    rm += am + valInt(getExFont(f));
  }

  w += lm + rm;

  assign(m->item_size, w, toInt(w));
  assign(m->item_size, h, toInt(h));
  assign(m, right_offset, toInt(rm));
  assign(m, left_offset, toInt(lm));
  
  succeed;
}


static status
computeMenu(Menu m)
{ if ( notNil(m->request_compute) )
  { int x, y, w, h;
    int ix, iy, iw, ih;
    int aw, ah;

    if ( m->request_compute == NAME_assignAccelerators &&
	 hasSendMethodObject(m, NAME_assignAccelerators) )
      send(m, NAME_assignAccelerators, 0);

    if ( m->multiple_selection == OFF )
      ensureSingleSelectionMenu(m);

    computeLabelMenu(m);
    computeItemsMenu(m);

    if ( m->show_label == ON )
    { Area a = m->label_area;

      if ( m->layout == NAME_horizontal )
      { if ( valInt(m->item_size->h) > valInt(a->h) )
	  assign(a, h, m->item_size->h);
      }

      x = valInt(a->x); y = valInt(a->y);
      w = valInt(a->w); h = valInt(a->h);
    } else
      x = y = w = h = 0;
      
    ix = valInt(m->item_offset->x);
    iy = valInt(m->item_offset->y);

    if ( m->feedback == NAME_showSelectionOnly )
    { iw = valInt(m->item_size->w);
      ih = valInt(m->item_size->h);
    } else
    { int rows, cols;
      int pen = valInt(m->pen);

      rows_and_cols(m, &rows, &cols);
      if ( m->layout == NAME_horizontal )
      { iw = rows * (valInt(m->item_size->w) + x_gap(m)) + pen;
	ih = cols * (valInt(m->item_size->h) + y_gap(m)) + pen;
      } else
      { iw = cols * (valInt(m->item_size->w) + x_gap(m)) + pen;
	ih = rows * (valInt(m->item_size->h) + y_gap(m)) + pen;
      }
    }

    iw += 2*valInt(m->margin);

    aw = max(x+w, ix+iw);
    ah = max(y+h, iy+ih);

    CHANGING_GRAPHICAL(m,
	assign(m->area, w, toInt(aw));
	assign(m->area, h, toInt(ah));
	changedEntireImageGraphical(m));

    assign(m, request_compute, NIL);
  }

  succeed;
}


static Point
getReferenceMenu(Menu m)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(m)) )
  { int ry;

    ComputeGraphical(m);

    if ( m->show_label == ON )
    { int h, fh, ascent;

      h      = valInt(m->label_area->h);
      fh     = valInt(getHeightFont(m->label_font));
      ascent = valInt(getAscentFont(m->label_font));
      ry     = (h-fh)/2 + ascent;
    } else
    { MenuItem mi = getHeadChain(m->members);

      if ( mi && instanceOfObject(mi->label, ClassCharArray) )
      { FontObj f = getFontMenuItemMenu(m, mi);
	int vw, vh;
	int vy;
	int ih = valInt(m->item_size->h);

	str_size(&((Name) mi->label)->data, f, &vw, &vh);
	vy = (m->vertical_format == NAME_top    ? 0 :
	      m->vertical_format == NAME_center ? (ih-vh)/2 :
					          ih - vh);
        ry = vy + valInt(getAscentFont(f));
      } else
	ry = valInt(m->item_offset->y) + valInt(m->item_size->h);
    }

    ref = answerObject(ClassPoint, ZERO, toInt(ry), 0);
  }

  answer(ref);
}


		/********************************
		*           CHANGES		*
		********************************/

static status
ChangedItemMenu(Menu m, MenuItem mi)
{ int x, y, w, h;

  computeMenu(m);
  area_menu_item(m, mi, &x, &y, &w, &h);
  return changedImageGraphical(m, toInt(x), toInt(y), toInt(w), toInt(h));
}

		/********************************
		*            REDRAW		*
		********************************/

static void
draw_cycle_blob(int x, int y, Elevation z, int up)
{ int w = CYCLE_DROP_WIDTH;
  int h = CYCLE_DROP_HEIGHT;
  int tw = CYCLE_TRIANGLE_WIDTH;
  int th = CYCLE_TRIANGLE_HEIGHT;
  int tx = x + (w - tw)/2;
  int ty = y + (h - th)/2;

  r_3d_box(x, y, w, h, 0, z, up);
  r_3d_triangle(tx + tw/2, ty+th, tx, ty, tx+tw, ty, z, up, 0x3);
}


static int
item_mark_y(Menu m, int y, int h, int mh)
{ return (m->vertical_format == NAME_top    ? y :
	  m->vertical_format == NAME_center ? y + (h-mh)/2 :
					      y + (h-mh));
}


static status
compute_popup_indicator(Menu m, MenuItem mi, int *w, int *h)
{ if ( notNil(mi->popup) )
  { if ( notNil(m->popup_image) )
    { Image pi = m->popup_image;
      *w = valInt(pi->size->w);
      *h = valInt(pi->size->h);
    } else
    { *w = 8;
      *h = 7;
    }

    succeed;
  }

  *w = *h = 0;

  fail;
}


static void
draw_popup_indicator(Menu m, MenuItem mi, int x, int y, int w, int h, int b)
{ Elevation z;
  int iw, ih, ix, iy;

  if ( !instanceOfObject(m, ClassPopup) )
    return;

  compute_popup_indicator(m, mi, &iw, &ih);
  iy = (m->vertical_format == NAME_top    ? y :
	m->vertical_format == NAME_center ? y + (h-ih)/2 :
					    y + h - ih);
  ix = x+w-b-iw;

  if ( notNil(m->popup_image) )
  { r_image(m->popup_image, 0, 0, ix, iy, iw, ih, ON);
  } else if ( (z = getClassVariableValueObject(m, NAME_elevation)) )
  { r_3d_triangle(ix, iy+ih, ix, iy, ix+iw, iy+ih/2,
		  z, m->preview != mi, 0x3);
  }
}


static inline status
elevated_items(Menu m, Elevation z)
{ if ( instanceOfObject(z, ClassElevation) )
  { if ( m->kind == NAME_choice )
      succeed;

    if ( m->look == NAME_openLook )
    { if ( m->feedback == NAME_image )
	fail;
      succeed;
    }

    if ( m->look == NAME_motif )
      return instanceOfObject(m, ClassPopup);

    if ( m->look == NAME_win) 
      return (m->preview_feedback != NAME_colour &&
	      instanceOfObject(m, ClassPopup));
  }

  fail;
}


static status
RedrawMenuItem(Menu m, MenuItem mi, int x, int y, int w, int h, Elevation iz)
{ int b = valInt(m->border);
  int lm = valInt(m->left_offset);
  int rm = valInt(m->right_offset);
  Image leftmark = NIL;
  int pen = valInt(m->pen);
  int ix, iy, iw, ih;
  int radius = 0;
  Image fill = NIL;
  Any colour = mi->colour;
  Elevation z = iz;
  int lblflags = (mi->active == ON && m->active == ON ? 0 : LABEL_INACTIVE);
  int flags = 0;

  DEBUG(NAME_menu, Cprintf("Redraw %s at %d %d %d %d\n",
			   pp(mi->value), x, y, w, h));

  if ( mi->active == OFF )
  { Any c2 = getClassVariableValueObject(m, NAME_inactiveColour);

    if ( c2 && notNil(c2) )
      colour = c2;

    r_colour(colour);
  }

					/* Windows '95 popup */
  if ( m->preview_feedback == NAME_colour && m->preview == mi )
  { Any fg = getClassVariableValueObject(m, NAME_selectedForeground);
    Any bg = getClassVariableValueObject(m, NAME_selectedBackground);
    Elevation mz = getClassVariableValueObject(m, NAME_elevation);
    int bw = (mz && notNil(mz) ? valInt(mz->height) : 0);
    int bh = valInt(m->border);

    r_fill(x, y+bh/2, w-2*bw, h-bh, bg);
    colour = fg;
    r_colour(fg);
  }
  if ( mi->end_group == ON && m->look == NAME_win )
  { Elevation mz = getClassVariableValueObject(m, NAME_elevation);

    if ( m->layout == NAME_vertical )
      r_3d_line(x, y+h, x+w, y+h, mz, FALSE);
    else
      r_3d_line(x+w, y, x+w, y+h, mz, FALSE);
  }

  if ( mi->selected == ON && notNil(m->on_image) )
    leftmark = m->on_image;
  else if ( mi->selected == OFF && notNil(m->off_image) )
    leftmark = m->off_image;

  if ( elevated_items(m, z) )
  { int up = TRUE;

    if ( m->preview == mi )
    { z = getClassVariableValueObject(m, NAME_previewElevation);
    } else if ( mi->selected == ON )
      up = FALSE;

    if ( !ws_draw_button_face((DialogItem)m, x, y, w, h, up, FALSE, FALSE) )
      r_3d_box(x, y, w, h, 0, z, up);

    if ( mi->end_group == ON )
    { Elevation mz = getClassVariableValueObject(m, NAME_elevation);

      if ( m->layout == NAME_vertical )
	r_3d_line(x, y+h, x+w, y+h, mz, FALSE);
      else
	r_3d_line(x+w, y, x+w, y+h, mz, FALSE);
    }

    if ( notNil(mi->popup) )
      draw_popup_indicator(m, mi, x, y, w, h, b);
  } else
  { if ( (mi->selected == ON && m->feedback == NAME_box) )
      pen++;
    else if ( m->preview == mi )
    { if ( m->preview_feedback == NAME_box )
	pen++;
      else if ( m->preview_feedback == NAME_roundedBox )
      { radius = 10;
	pen++;
      } else if ( m->preview_feedback == NAME_invertedRoundedBox )
      { fill = BLACK_IMAGE;
	radius = 10;
      }
	
      DEBUG(NAME_menu, Cprintf("Feedback = %s, p = %d; r = %d, fill = %s\n",
			       pp(m->preview_feedback),
			       pen, radius, pp(fill)));
    }

    if ( mi->end_group == ON && m->look != NAME_win )
    { r_thickness(pen+1);
      r_dash(m->texture);
      if ( m->layout == NAME_vertical )
	r_line(x, y+h, x+w, y+h);
      else
	r_line(x+w, y, x+w, y+h);
    }

    if ( pen != 0 || notNil(fill) )
    { r_thickness(pen);
      r_dash(m->texture);
      r_box(x, y, w, h, radius, fill);
      if ( notNil(fill) )
	r_swap_background_and_foreground();
    }

    if ( notNil(mi->popup) )
      draw_popup_indicator(m, mi, x, y, w, h, b);
  }

  if ( mi->selected == ON )
    flags |= CHECKBOX_SELECTED;
  if ( mi->active == ON && m->active == ON )
    flags |= CHECKBOX_ACTIVE;
  if ( m->multiple_selection == ON )
    flags |= CHECKBOX_MULTIPLE;

  if ( ((mi->selected == ON  && (Name)m->on_image == NAME_marked ) ||
	(mi->selected == OFF && (Name)m->off_image == NAME_marked)) )
  { ws_draw_checkbox(x, y, w, h, b, flags);
  } else
  { if ( instanceOfObject(leftmark, ClassImage) )
    { int bw, bh, by;
      Elevation mz = getClassVariableValueObject(m, NAME_markElevation);
      
      bw = valInt(leftmark->size->w);
      bh = valInt(leftmark->size->h);
      by = item_mark_y(m, y, h, bh);
  
      if ( instanceOfObject(mz, ClassElevation) && mz->height != ZERO )
      { int h = valInt(mz->height);
	r_3d_box(x+b-h, by-h, bw+2*h, bh+2*h, 0, mz, FALSE);
      }
  
      r_image(leftmark, 0, 0, x+b, by, bw, bh, ON);
    } else if ( (Name) leftmark == NAME_marked )
    { if ( m->look == NAME_motif )
      { Elevation mz = getClassVariableValueObject(m, NAME_markElevation);
  
	if ( m->multiple_selection == ON )
	{ int dy = item_mark_y(m, y, h, MARK_BOX_SIZE);
  
	  r_3d_box(x+b, dy, MARK_BOX_SIZE, MARK_BOX_SIZE,
		   0, mz, mi->selected == OFF);
	} else
	{ int dy = item_mark_y(m, y, h, MARK_DIAMOND_SIZE);
  
	  r_3d_diamond(x+b, dy, MARK_DIAMOND_SIZE, MARK_DIAMOND_SIZE,
		       mz, mi->selected == OFF);
	}
      } else if ( m->look == NAME_win )
      { if ( m->multiple_selection == OFF )
	{ int d = MARK_CIRCLE_SIZE;
	  int zh = valInt(z->height);
	  int dy = item_mark_y(m, y, h, d);
	  int dx = x+b+lm - (MARK_CIRCLE_SIZE+5);
	  int mw = 3;			/* mark-width */
  
	  r_3d_ellipse(dx-zh, dy-zh, d+2*zh, d+2*zh, z, FALSE);
	  r_thickness(0);
	  r_ellipse(dx, dy, d, d, WHITE_COLOUR);
	  if ( mi->selected == ON )
	    r_fill(dx+(d-mw)/2, dy+(d-mw)/2, mw, mw, BLACK_COLOUR);
	}
      }
    }
  }

  if ( notNil(m->accelerator_font) && notNil(mi->accelerator) )
  { FontObj f = m->accelerator_font;
    int fw = valInt(getExFont(f));

    str_label(&mi->accelerator->data,
	      0,
	      f,
	      x, y, w-fw/2, h,
	      NAME_right, m->vertical_format,
	      lblflags);
  }

  ix = x + lm + b;
  iw = w - (lm + 2*b + rm);
  iy = y + b;
  ih = h - 2*b;

  if ( notDefault(colour) )
    r_colour(colour);

  if ( instanceOfObject(mi->label, ClassCharArray) )
  { FontObj f = getFontMenuItemMenu(m, mi);
    int fw = valInt(getExFont(f));

    str_label(&((Name) mi->label)->data,
	      accelerator_code(mi->accelerator),
	      f,
	      ix+fw/2, iy, iw-fw, ih,
	      m->format, m->vertical_format, 
	      lblflags);

  } else if ( instanceOfObject(mi->label, ClassImage) )
  { int bx, by, bw, bh;
    Image image = (Image) mi->label;

    bw = valInt(image->size->w);
    bh = valInt(image->size->h);
    bx = (m->format == NAME_left   ? ix :
	  m->format == NAME_center ? ix + (iw-bw)/2 :
				     ix + iw - bw);
    by = (m->vertical_format == NAME_top    ? iy :
	  m->vertical_format == NAME_center ? iy + (ih-bh)/2 :
					      iy + ih - bh);

    r_image(mi->label, 0, 0, bx, by, bw, bh, ON);
  }

  if ( notNil(fill) )
    r_swap_background_and_foreground();

  if ( (mi->selected == ON && m->feedback == NAME_invert) )
    r_complement(ix, iy, iw, ih);
  if ( (m->preview == mi && m->preview_feedback == NAME_invert) )
    r_complement(x, y, w, h);

  if ( notDefault(colour) )
    r_colour(DEFAULT);

  succeed;
}


static status
RedrawAreaMenu(Menu m, Area a)
{ int x, y, w, h;
  int bx, by, cx, cy, iw, ih;
  int gx = x_gap(m);
  int gy = y_gap(m);
  Elevation z  = getClassVariableValueObject(m, NAME_elevation);
  Elevation iz = getClassVariableValueObject(m, NAME_itemElevation);

  initialiseDeviceGraphical(m, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  
  if ( m->show_label == ON )
  { int flags = (m->active == ON ? 0 : LABEL_INACTIVE);
    int lw = (isDefault(m->label_width) ? valInt(m->label_area->w)
				        : valInt(m->label_width));
    if ( m->layout == NAME_horizontal )
      lw -= valInt(getExFont(m->label_font));

    RedrawLabelDialogItem(m,
			  accelerator_code(m->accelerator),
			  x + valInt(m->label_area->x),
			  y + valInt(m->label_area->y),
			  lw,
			  valInt(m->label_area->h),
			  m->label_format, m->vertical_format, flags);
  }
  
  bx = cx = x + valInt(m->item_offset->x);
  by = cy = y + valInt(m->item_offset->y);
  iw = valInt(m->item_size->w);
  ih = valInt(m->item_size->h);
  
  if ( m->feedback == NAME_showSelectionOnly )
  { MenuItem mi = getItemSelectionMenu(m);
    Any ci = getClassVariableValueObject(m, NAME_cycleIndicator);

    if ( (Name)ci == NAME_comboBox )
    { int flags = TEXTFIELD_COMBO;

      if ( mi && mi->active == ON && m->active == ON )
	flags |= TEXTFIELD_EDITABLE;

      ws_entry_field(cx, by, iw, ih, flags);
    } else if ( instanceOfObject(ci, ClassElevation) )
    { int bw = CYCLE_DROP_WIDTH;

      draw_cycle_blob(cx-(bw+CYCLE_DROP_DISTANCE), cy, ci, TRUE);
    } else /*if ( instanceOfObject(ci, ClassImage) )*/
    { Image i = ci;
      int bw = valInt(i->size->w);
      int bh = valInt(i->size->h);

      r_image(i, 0, 0, cx-(bw+CYCLE_DROP_DISTANCE), cy, bw, bh, ON);
    }

    if ( mi != FAIL )
      RedrawMenuItem(m, mi, cx, cy, iw, ih, iz);
  } else
  { int rows, cols;
    int n = 1;
    Cell cell;
    int ax, ay, aw, ah;

    ax = valInt(a->x); ay = valInt(a->y); 
    aw = valInt(a->w); ah = valInt(a->h); 
    ax += x - valInt(m->area->x);
    ay += y - valInt(m->area->y);
    rows_and_cols(m, &rows, &cols);
    
    if ( z && notNil(z) )
      r_3d_box(cx, cy, w-(cx-x), h-(cy-y), 0, z, TRUE);
    cx += valInt(m->margin);

    if ( m->look == NAME_motif ||
	 m->look == NAME_win ||
	 (m->look == NAME_openLook && instanceOfObject(iz, ClassElevation)) )
    { iw += gx; ih += gy;
      gx = gy = 0;
    } else if ( m->pen != ZERO )
    { iw += gx + 1; ih += gy + 1;
      gx = gy = -1;
    } 

    for_cell(cell, m->members)
    { MenuItem mi = cell->value;

      if ( OverlapArea(ax, ay, aw, ah, cx, cy, iw, ih) )
	RedrawMenuItem(m, mi, cx, cy, iw, ih, iz);

      if ( m->layout == NAME_vertical )
      { if ( rows == 1 || (n > 1 && n % rows == 0) )
	{ cy = by;
	  cx += iw+gx;
	} else
	{ cy += ih+gy;
	}
      } else
      { if ( rows == 1 || (n > 1 && n % rows == 0) )
	{ cx = bx;
	  cy += ih+gy;
	} else
	{ cx += iw+gx;
	}
      }
      n++;
    }
  }
    
  return RedrawAreaGraphical(m, a);
}


		/********************************
		*           COMPUTING		*
		********************************/

static FontObj
getFontMenuItemMenu(Menu m, MenuItem mi)
{ if ( notDefault(mi->font) )
    answer(mi->font);

  answer(m->value_font);
}


Int
getCenterYMenuItemMenu(Menu m, Any obj)
{ MenuItem mi;
  int x, y, w, h;

  if ( isDefault(obj) )
  { if ( (mi = getItemSelectionMenu(m)) == FAIL )
    { if ( emptyChain(m->members) != SUCCEED )
      	mi = getHeadChain(m->members);
      else
      	return ZERO;
    }
  } else
    if ( (mi = findMenuItemMenu(m, obj)) == FAIL )
      return ZERO;
  
  computeMenu(m);
  area_menu_item(m, mi, &x, &y, &w, &h);

  answer(toInt(y+h/2 + valInt(m->area->y)));
}


static Int
getLabelWidthMenu(Menu m)
{ ComputeGraphical(m);

  if ( m->show_label == ON )
    answer(m->label_area->w);
  else
    answer(ZERO);
}


static status
labelWidthMenu(Menu m, Int w)
{ return assignGraphical(m, NAME_labelWidth, w);
}

		/********************************
		*        EVENT HANDLING		*
		********************************/

static status
statusMenu(Menu m, Name stat)
{ assign(m, status, stat);

  succeed;
}


MenuItem
getItemFromEventMenu(Menu m, EventObj ev)
{ Int X, Y;
  int x, y;
  int index;
  int rows, cols;

  rows_and_cols(m, &rows, &cols);

  ComputeGraphical(m);
  get_xy_event(ev, m, ON, &X, &Y);
  x = valInt(X) - valInt(m->item_offset->x);
  y = valInt(Y) - valInt(m->item_offset->y);
  if ( x < 0 || y < 0 )
    fail;
  DEBUG(NAME_event, Cprintf("event at %d,%d\n", x, y));

  x /= valInt(m->item_size->w) + x_gap(m);
  y /= valInt(m->item_size->h) + y_gap(m);
  DEBUG(NAME_event, Cprintf("item at %d,%d; rows = %d\n", x, y, rows));
  
  if ( m->layout == NAME_horizontal )
    index = x + y * rows + 1;
  else
    index = y + x * rows + 1;

  return getNth1Chain(m->members, toInt(index));
}


static status
eventMenu(Menu m, EventObj ev)
{ if ( completerShownDialogItem(m) &&
       postEvent(ev, (Graphical) CompletionBrowser(), DEFAULT) )
    succeed;

  if ( eventDialogItem(m, ev) )
    succeed;

  if ( m->active == ON )
  { makeButtonGesture();

    return eventGesture(GESTURE_button, ev);
  }

  fail;
}


status
forwardMenu(Menu m, Code msg, EventObj ev)
{ MenuItem mi;

  TRY(mi = getItemSelectionMenu(m));

  if ( notDefault(mi->message) )
  { if ( notNil(mi->message) )
    { forwardReceiverCode(mi->message, m, mi->value, ev, 0);
      succeed;
    }
    succeed;
  }

  if ( notNil(m->message) && notDefault(m->message) )
  { Any val;

    if ( (val = get(m, NAME_selection, 0)) )
      forwardReceiverCode(m->message, m, val, ev, 0);
  }

  succeed;
}

  

static status
selectedCompletionMenu(Menu m, DictItem di)
{ selectionMenu(m, di->key);
  quitCompleterDialogItem(m);
  flushGraphical(m);
  if ( !send(m->device, NAME_modifiedItem, m, ON, 0) )
    forwardMenu(m, m->message, EVENT->value);

  succeed;
}


static status
keyboardQuitMenu(Menu m)
{ quitCompleterDialogItem(m);

  succeed;
}


static status
openComboBoxMenu(Menu m)
{ Browser c = CompletionBrowser();
  Cell cell;
  DictItem selection = NIL;

  send(c, NAME_clear, 0);
  for_cell(cell, m->members)
  { MenuItem mi = cell->value;
    DictItem di;

    if ( mi->active == ON )
    { send(c, NAME_append,
	   (di=newObject(ClassDictItem, mi->value, mi->label, 0)), 0);
      if ( mi->selected == ON )
	selection = di;
    }
  }

  if ( notNil(selection) )
    send(c, NAME_selection, selection, 0);

  selectCompletionDialogItem(m, NIL, NIL, ZERO);

  changedDialogItem(m);

  succeed;
}



static status
executeMenu(Menu m, EventObj ev)
{ MenuItem mi;

  if ( m->feedback == NAME_showSelectionOnly )
  { Any img = getClassVariableValueObject(m, NAME_cycleIndicator); /* TBD */

    if ( img == NAME_comboBox )
      return openComboBoxMenu(m);
    else
    { nextMenu(m);
      flushGraphical(m);
      if ( !send(m->device, NAME_modifiedItem, m, ON, 0) )
	forwardMenu(m, m->message, ev);
      succeed;
    }
  }

  if ( isDefault(ev) )
    ev = EVENT->value;			/* @event */
  TRY((mi = getItemFromEventMenu(m, ev)) && mi->active == ON);
    
  if ( m->multiple_selection == ON )
  { toggleMenu(m, mi);
    flushGraphical(m);
    send(m->device, NAME_modifiedItem, m, ON, 0);
    
    if ( notDefault(mi->message) )
    { if ( notNil(mi->message) )
	forwardReceiverCode(mi->message, m,
			    mi, mi->selected, ev, 0);
    } else if ( !modifiedMenu(m, ON) &&
		notNil(m->message) &&
		notDefault(m->message) )
    { forwardReceiverCode(m->message, m,
			  mi->value, mi->selected, ev, 0);
    }
  } else
  { selectionMenu(m, mi);
    flushGraphical(m);
    send(m->device, NAME_modifiedItem, m, ON, 0);

    if ( notDefault(mi->message) )
    { if ( notNil(mi->message) )
	forwardReceiverCode(mi->message, m,
			    mi->value, ev, 0);
    } else if ( !modifiedMenu(m, ON) )
      forwardMenu(m, m->message, ev);
  }

  succeed;
}


		/********************************
		*           SELECTION		*
		********************************/

static MenuItem
getItemSelectionMenu(Menu m)
{ if ( m->multiple_selection == OFF )
  { Cell cell;

    for_cell(cell, m->members)
    { MenuItem mi = cell->value;

      if ( mi->selected == ON )
	answer(mi);
    }
  }

  fail;
}


static Any
getSelectionMenu(Menu m)
{ if ( m->multiple_selection == OFF )
  { MenuItem mi;

    TRY( mi = getItemSelectionMenu(m) );
    assign(m, selection, mi->value);
  } else
  { Chain ch = answerObject(ClassChain, 0);
    Cell cell;

    for_cell(cell, m->members)
    { MenuItem mi = cell->value;

      if ( mi->selected == ON )
	appendChain(ch, mi->value);
    }
    assign(m, selection, ch);
  }

  answer(m->selection);
}


MenuItem
findMenuItemMenu(Menu m, Any spec)
{ if ( instanceOfObject(spec, ClassMenuItem) )
  { MenuItem mi = spec;

    if ( mi->menu == m )
      answer(mi);
  } else
  { Cell cell;

    for_cell(cell, m->members)
    { MenuItem mi = cell->value;

      if ( mi->value == spec )
	answer(mi);
    }

    for_cell(cell, m->members)
      if ( hasValueMenuItem(cell->value, spec) )
	answer(cell->value);
  }

  fail;
}


static int
index_item_menu(Menu m, Any spec)
{ if ( instanceOfObject(spec, ClassMenuItem) )
  { Cell cell; int n = 1;

    for_cell(cell, m->members)
    { if ( cell->value == spec )
        return n;
      n++;
    }
  } else
  { Cell cell;
    int n = 1;
    
    for_cell(cell, m->members)
    { if ( hasValueMenuItem(cell->value, spec) )
        return n;
      n++;
    }
  }    

  fail;
}


status
previewMenu(Menu m, MenuItem mi)
{ if ( !mi )
    mi = NIL;

  if ( notNil(m->preview) )
    ChangedItemMenu(m, m->preview);
  assign(m, preview, mi);
  if ( notNil(m->preview) )
    ChangedItemMenu(m, m->preview);

  succeed;
}


static status
clearSelectionMenu(Menu m)
{ return selectionMenu(m, FAIL);	/* not very nice! */
}


#define MAX_ITEMS 1000
#define IS_SET 1
#define NEEDS_SET 2

status
selectionMenu(Menu m, Any selection)
{ Cell cell;
  char is_set[MAX_ITEMS];
  int n;

  DEBUG(NAME_popup, Cprintf("selectionMenu(%s, %s)\n", pp(m), pp(selection)));

  is_set[0] = 0;
  n = 1;
  for_cell(cell, m->members)
  { MenuItem mi = cell->value;

    is_set[n] = (mi->selected == ON ? IS_SET : 0);
    n++;
  }

  if ( selection )
  { if ( instanceOfObject(selection, ClassChain) )
    { for_cell(cell, (Chain)selection)
	is_set[index_item_menu(m, cell->value)] |= NEEDS_SET;
    } else
      is_set[index_item_menu(m, selection)] |= NEEDS_SET;

    assign(m, selection, selection);
  }

  n = 1;
  for_cell(cell, m->members)
  { MenuItem mi = cell->value;

    if ( is_set[n] & NEEDS_SET )
    { if ( !(is_set[n] & IS_SET) )
      { assign(mi, selected, ON);
	ChangedItemMenu(m, mi);
      }
    } else
    { if ( is_set[n] & IS_SET )
      { assign(((MenuItem) cell->value), selected, OFF);
	ChangedItemMenu(m, mi);
      }
    }
    n++;
  }

  succeed;
}


static status
selectedMenu(Menu m, MenuItem mi, Bool val)
{ if ( mi->selected != val )
  { assign(mi, selected, val);
    ChangedItemMenu(m, mi);
  }
  
  succeed;
}


static Bool
getSelectedMenu(Menu m, MenuItem mi)
{ answer(mi->selected);
}


status
toggleMenu(Menu m, MenuItem mi)
{ CHANGING_GRAPHICAL(m,
	assign(mi, selected, mi->selected == ON ? OFF : ON);
	ChangedItemMenu(m, mi));
	 
  succeed;
}


static status
nextMenu(Menu m)
{ Cell cell;
  MenuItem current = NIL;
  MenuItem next = NIL;
  int skipping = TRUE;

  for_cell(cell, m->members)
  { MenuItem mi = cell->value;

    if ( skipping )
    { if ( mi->active == ON && isNil(next) )
        next = mi;
      if ( mi->selected == ON )
      { skipping = FALSE;
        current = mi;
      }
    } else if ( mi->active == ON )
    { next = mi;
      break;
    }
  }

  if ( current != next )
    selectionMenu(m, next);

  succeed;
}



		/********************************
		*            MEMBERS		*
		********************************/

static status
append_menu(Menu m, MenuItem mi, Name where)
{ if ( notNil(mi->menu) )
    return errorPce(mi, NAME_alreadyShown, mi, mi->menu);

  if ( equalName(where, NAME_head) )
    prependChain(m->members, mi);
  else
    appendChain(m->members, mi);

  assign(mi, menu, m);

  return requestComputeGraphical(m, NAME_assignAccelerators);
}


status
appendMenu(Menu m, Any mi)
{ return append_menu(m, mi, NAME_tail);
}


static status
prependMenu(Menu m, Any mi)
{ return append_menu(m, mi, NAME_head);
}


status
deleteMenu(Menu m, Any obj)
{ MenuItem mi;

  TRY( mi = findMenuItemMenu(m, obj) );

  assign(mi, menu, NIL);
  deleteChain(m->members, mi);
  return requestComputeGraphical(m, NAME_assignAccelerators);
}


static status
clearMenu(Menu m)
{ Cell cell;

  GcProtect(m,
	    { for_cell(cell, m->members)
	      { MenuItem mi = cell->value;
    
		assign(mi, menu, NIL);
	      }
	      clearChain(m->members);
	    });

  return requestComputeGraphical(m, DEFAULT);
}


static status
membersMenu(Menu m, Chain members)
{ Any val;

  send(m, NAME_clear, 0);
  for_chain(members, val, TRY(send(m, NAME_append, val, 0)));

  succeed;
}



static status
memberMenu(Menu m, Any obj)
{ TRY( findMenuItemMenu(m, obj) );
  
  succeed;
}


static MenuItem
getMemberMenu(Menu m, Any obj)
{ MenuItem mi;

  TRY( mi = findMenuItemMenu(m, obj) );
  
  answer(mi);
}


		/********************************
		*           ACTIVATE		*
		********************************/

status
updateMenu(Menu m, Any context)
{ Cell cell;
  int changed = FALSE;

  for_cell(cell, m->members)
  { MenuItem mi = cell->value;

    if ( notNil(mi->condition) )
    { Bool a = (forwardReceiverCode(mi->condition, mi, context, 0) ? ON : OFF);

      if ( a != mi->active )
      { changed = TRUE;
	qadSendv(mi, NAME_active, 1, (Any*) &a);
      }
    }
  }
      	
  if ( changed )
    CHANGING_GRAPHICAL(m, changedEntireImageGraphical(m));

  succeed;
}


static status
activeItemMenu(Menu m, Any obj, Bool val)
{ MenuItem mi;

  TRY( mi = findMenuItemMenu(m, obj) );

  CHANGING_GRAPHICAL(m,
	assign(mi, active, val);
	changedEntireImageGraphical(m));

  succeed;
}


static status
onMenu(Menu m, Any obj)
{ return activeItemMenu(m, obj, ON);
}


static status
offMenu(Menu m, Any obj)
{ return activeItemMenu(m, obj, OFF);
}


static Bool
getActiveItemMenu(Menu m, Any obj)
{ MenuItem mi;

  TRY( mi = findMenuItemMenu(m, obj) );
  
  answer(mi->active);
}


static status
isOnMenu(Menu m, Any obj)
{ if ( getActiveItemMenu(m, obj) == ON )
    succeed;

  fail;
}


static status
isOffMenu(Menu m, Any obj)
{ if ( getActiveItemMenu(m, obj) == OFF )
    succeed;

  fail;
}


static status
activeAllItemsMenu(Menu m, Bool val)
{ Cell cell;
  
  for_cell(cell, m->members)
  { MenuItem mi = cell->value;

    assign(mi, active, val);
  }
  
  CHANGING_GRAPHICAL(m,
	changedEntireImageGraphical(m));

  succeed;
}


static status
allOnMenu(Menu m)
{ return activeAllItemsMenu(m, ON);
}


static status
allOffMenu(Menu m)
{ return activeAllItemsMenu(m, OFF);
}

		/********************************
		*            KIND		*
		********************************/

static status
kindMenu(Menu m, Name kind)
{ if ( m->look == NAME_openLook )
  { if ( kind == NAME_choice || kind == NAME_toggle )
    { assign(m, on_image, NIL);
      assign(m, off_image, NIL);
      assign(m, feedback, NAME_box);
      assign(m, pen, ONE);
      assign(m, border, TWO);
      assign(m, multiple_selection, kind == NAME_toggle ? ON : OFF);

      assign(m, kind, kind);
      return requestComputeGraphical(m, DEFAULT);
    } else if ( kind == NAME_marked )
    { assign(m, on_image, MARK_IMAGE);
      assign(m, off_image, NOMARK_IMAGE);
      assign(m, feedback, NAME_image);
      assign(m, pen, ZERO);
      assign(m, border, TWO);
      assign(m, multiple_selection, OFF);
      assign(m, kind, kind);

      return requestComputeGraphical(m, DEFAULT);
    } else if ( kind == NAME_cycle )
    { assign(m, pen, ZERO);
    }
  } else if ( m->look == NAME_motif )
  { if ( kind == NAME_marked || kind == NAME_toggle )
    { assign(m, on_image, NAME_marked);
      assign(m, off_image, NAME_marked);
      assign(m, feedback, NAME_image);
      assign(m, pen, ZERO);
      assign(m, border, toInt(3));
      assign(m, multiple_selection, kind == NAME_toggle ? ON : OFF);

      assign(m, kind, kind);
      return requestComputeGraphical(m, DEFAULT);
    } else if ( kind == NAME_choice )
    { assign(m, on_image, NIL);
      assign(m, off_image, NIL);
      assign(m, feedback, NAME_box);
      assign(m, pen, ONE);
      assign(m, border, TWO);
      assign(m, multiple_selection, OFF);

      assign(m, kind, kind);
      return requestComputeGraphical(m, DEFAULT);
    } else if ( kind == NAME_cycle )
    { assign(m, pen, ZERO);
    }
  } else if ( m->look == NAME_win )
  { if ( kind == NAME_marked || kind == NAME_toggle )
    { if ( kind == NAME_marked )
      { assign(m, on_image, NAME_marked);
	assign(m, off_image, NAME_marked);
      } else
      { assign(m, on_image, NAME_marked);
	assign(m, off_image, NAME_marked);
      }
      assign(m, feedback, NAME_image);
      assign(m, pen, ZERO);
      assign(m, border, toInt(3));
      assign(m, multiple_selection, kind == NAME_toggle ? ON : OFF);

      assign(m, kind, kind);
      return requestComputeGraphical(m, DEFAULT);
    } else if ( kind == NAME_choice )
    { assign(m, on_image, NIL);
      assign(m, off_image, NIL);
      assign(m, feedback, NAME_box);
      assign(m, pen, ONE);
      assign(m, border, TWO);
      assign(m, multiple_selection, OFF);

      assign(m, kind, kind);
      return requestComputeGraphical(m, DEFAULT);
    } else if ( kind == NAME_cycle )
    { assign(m, border, toInt(4));
    }
  }

  if ( equalName(kind, NAME_cycle) )
  { assign(m, on_image, NIL);
    assign(m, off_image, NIL);
    assign(m, feedback, NAME_showSelectionOnly);
    assign(m, layout,   NAME_horizontal);
    assign(m, accelerator_font, NIL);
    multipleSelectionMenu(m, OFF);
    if ( m->look != NAME_win )
    { assign(m, popup, newObject(ClassPopup, 0));
      assign(m->popup, members, m->members);
      kindMenu((Menu) m->popup,  NAME_cyclePopup);
    }
  } else if ( equalName(kind, NAME_marked) )
  { assign(m, on_image, MARK_IMAGE);
    assign(m, off_image, NOMARK_IMAGE);
    assign(m, feedback, NAME_image);
    multipleSelectionMenu(m, OFF);
  } else if ( equalName(kind, NAME_choice) )
  { assign(m, on_image, NIL);
    assign(m, off_image, NIL);
    assign(m, feedback, NAME_invert);
    multipleSelectionMenu(m, OFF);
  } else if ( equalName(kind, NAME_toggle) )
  { assign(m, on_image, MARK_IMAGE);
    assign(m, off_image, NOMARK_IMAGE);
    assign(m, feedback, NAME_image);
    multipleSelectionMenu(m, ON);
  } else if ( equalName(kind, NAME_popup) )
  { if ( instanceOfObject(m, ClassPopup) )
    { defaultPopupImages((PopupObj)m);
    } else
    { assign(m, on_image, NIL);
      assign(m, off_image, NIL);
    }
    multipleSelectionMenu(m, OFF);
  } else if ( equalName(kind, NAME_cyclePopup) )
  { if ( m->look == NAME_win || m->look == NAME_motif )
      assign(m, on_image, NAME_marked);
    else
      assign(m, on_image, MARK_IMAGE);
    assign(m, off_image, NIL);
    multipleSelectionMenu(m, OFF);
  } else
    fail;

  assign(m, kind, kind);

  succeed;
}





		/********************************
		*          ATTRIBUTES		*
		********************************/


static status
ensureSingleSelectionMenu(Menu m)
{ MenuItem first = NIL;
  int sel = 0;
  Cell cell;

  for_cell(cell, m->members)
  { MenuItem mi = cell->value;
    
    if ( mi->active == ON && isNil(first) )
      first = mi;
    if ( mi->selected == ON )
    { if ( sel++ > 0 )
	selectedMenuItem(mi, OFF);
    }
  }

  if ( sel == 0 )
  { if ( notNil(first) )
      return selectionMenu(m, first);

    fail;
  }

  succeed;
}


static status
multipleSelectionMenu(Menu m, Bool val)
{ return assignGraphical(m, NAME_multipleSelection, val);
}


static status
layoutMenu(Menu m, Name or)
{ return assignGraphical(m, NAME_layout, or);
}


static status
columnsMenu(Menu m, Int n)
{ assignGraphical(m, NAME_columns, n);
  if ( m->feedback == NAME_showSelectionOnly && notNil(m->popup) )
    send(m->popup, NAME_columns, n, 0);

  succeed;
}


static status
valueFontMenu(Menu m, FontObj font)
{ return assignGraphical(m, NAME_valueFont, font);
}


static status
valueWidthMenu(Menu m, Int w)
{ if ( m->feedback != NAME_showSelectionOnly )
    assignGraphical(m, NAME_valueWidth, w);

  succeed;
}


static Int
getValueWidthMenu(Menu m)
{ if ( m->feedback != NAME_showSelectionOnly )
  { computeMenu(m);
    answer(m->item_size->w);
  }
   
  fail;
}


static status
showLabelMenu(Menu m, Bool val)
{ return assignGraphical(m, NAME_showLabel, val);
}


static status
offImageMenu(Menu m, Image image)
{ return assignGraphical(m, NAME_offImage, image);
}


static status
onImageMenu(Menu m, Image image)
{ return assignGraphical(m, NAME_onImage, image);
}


static status
popupImageMenu(Menu m, Image image)
{ return assignGraphical(m, NAME_popupImage, image);
}


static status
borderMenu(Menu m, Int b)
{ return assignGraphical(m, NAME_border, b);
}


static status
gapMenu(Menu m, Size gap)
{ return assignGraphical(m, NAME_gap, gap);
}


static status
formatMenu(Menu m, Name name)
{ return assignGraphical(m, NAME_format, name);
}


static status
verticalFormatMenu(Menu m, Name name)
{ return assignGraphical(m, NAME_verticalFormat, name);
}


static status
feedbackMenu(Menu m, Name feedback)
{ return assignGraphical(m, NAME_feedback, feedback);
}


static status
marginMenu(Menu m, Int margin)
{ return assignGraphical(m, NAME_margin, margin);
}


static status
sortMenu(Menu m, Code msg)
{ sortChain(m->members, msg, OFF);

  return requestComputeGraphical(m, NAME_assignAccelerators);
}



		/********************************
		*           VISUALISER		*
		********************************/

static Chain
getContainsMenu(Menu m)
{ answer(m->members);
}


		/********************************
		*         COMMUNICATION		*
		********************************/

static Bool
getModifiedMenu(Menu m)
{ if ( m->multiple_selection == OFF )
  { MenuItem mi = getItemSelectionMenu(m);

    answer((mi && m->selection == mi->value) ? OFF : ON);
  } else
  { Cell cell;
    char is_set[MAX_ITEMS];
    int n;

    if ( !instanceOfObject(m->selection, ClassChain) )
      answer(ON);

    n = 1;
    for_cell(cell, m->members)

      is_set[n++] = 0;
    
    for_cell(cell, (Chain) m->selection)
    { int index = index_item_menu(m, cell->value);

      is_set[index]++;
    }

    n = 1;
    for_cell(cell, m->members)
    { MenuItem mi = cell->value;

      if ( (is_set[n] && mi->selected == OFF) ||
	   (!is_set[n] && mi->selected == ON) )
	answer(ON);
      n++;
    }

    answer(OFF);
  }
}


static status
modifiedMenu(Menu m, Bool val)
{ if ( val == OFF )
    getSelectionMenu(m);
  else
    return modifiedDialogItem(m, ON);

  succeed;
}


static Any
getDefaultMenu(Menu m)
{ if ( notNil(m->default_value) )
    answer(checkType(m->default_value, TypeAny, m));

  fail;
}


static status
defaultMenu(Menu m, Any def)
{ if ( m->default_value != def )
  { assign(m, default_value, def);

    restoreMenu(m);
  }

  succeed;
}


static status
restoreMenu(Menu m)
{ Any val;

  TRY(val = getDefaultMenu(m));
  return send(m, NAME_selection, val, 0);
}


static status
applyMenu(Menu m, Bool always)
{ Any val;

  if ( instanceOfObject(m->message, ClassCode) &&
       (always == ON || getModifiedMenu(m) == ON) &&
       (val = get(m, NAME_selection, 0)) )
  { forwardReceiverCode(m->message, m, val, 0);
    succeed;
  }

  fail;
}

		 /*******************************
		 *	   ACCELERATORS		*
		 *******************************/

static status
assignAcceletatorsMenu(Menu m)
{ return assignAccelerators(m->members, NAME_, NAME_label);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_activeItem[] =
        { "item=member:menu_item", "active=bool" };
static char *T_selected[] =
        { "item=member:menu_item", "selected=bool" };
static char *T_initialise[] =
        { "name=[name]", "kind=[name]", "message=[code]*" };

/* Instance Variables */

static vardecl var_menu[] =
{ IV(NAME_selection, "values=any|chain", IV_NONE,
     NAME_selection, "Value(s) of currently selected menu_items"),
  IV(NAME_members, "items=chain", IV_GET,
     NAME_organisation, "Menu items (alternatives)"),
  IV(NAME_default, "value=any|chain|function*", IV_NONE,
     NAME_apply, "Default value"),
  SV(NAME_kind, "kind={cycle,marked,choice,toggle,popup,cycle_popup}", IV_GET|IV_STORE, kindMenu,
     NAME_appearance, "Kind of menu"),
  SV(NAME_preview, "item=menu_item*", IV_GET|IV_STORE, previewMenu,
     NAME_event, "Item in `preview' state"),
  IV(NAME_previewFeedback, "feedback={box,rounded_box,inverted_rounded_box,invert,colour}", IV_BOTH,
     NAME_appearance, "Feedback given to item in preview state"),
  SV(NAME_feedback, "feedback={box,invert,image,show_selection_only}", IV_GET|IV_STORE, feedbackMenu,
     NAME_appearance, "Type of feedback for selection"),
  SV(NAME_multipleSelection, "multiple=bool", IV_GET|IV_STORE, multipleSelectionMenu,
     NAME_selection, "If @on, more than one item may be selected"),
  SV(NAME_showLabel, "show=bool", IV_GET|IV_STORE, showLabelMenu,
     NAME_appearance, "Whether label is visible"),
  SV(NAME_valueFont, "font=font", IV_GET|IV_STORE, valueFontMenu,
     NAME_appearance, "Font for value"),
  SV(NAME_valueWidth, "width=0..", IV_GET|IV_STORE, valueWidthMenu,
     NAME_layout, "Minimum width for values"),
  SV(NAME_layout, "orientation={horizontal,vertical}", IV_GET|IV_STORE, layoutMenu,
     NAME_appearance, "Horizontal or vertical layout"),
  SV(NAME_columns, "number=1..", IV_GET|IV_STORE, columnsMenu,
     NAME_appearance, "Number of columns"),
  SV(NAME_format, "alignment={left,center,right}", IV_GET|IV_STORE, formatMenu,
     NAME_appearance, "Horizontal alignment of items in box"),
  SV(NAME_verticalFormat, "alignment={top,center,bottom}", IV_GET|IV_STORE, verticalFormatMenu,
     NAME_appearance, "Vertical alignment of items (and decorations) in box"),
  SV(NAME_gap, "size", IV_GET|IV_STORE, gapMenu,
     NAME_layout, "Gap between items"),
  SV(NAME_border, "width=0..", IV_GET|IV_STORE, borderMenu,
     NAME_appearance, "Width of border around item"),
  SV(NAME_onImage, "image=image|{marked}*", IV_GET|IV_STORE, onImageMenu,
     NAME_appearance, "Left mark if selected equals @on"),
  SV(NAME_offImage, "image=image|{marked}*", IV_GET|IV_STORE, offImageMenu,
     NAME_appearance, "Left mark if selected equals @off"),
  SV(NAME_popupImage, "image=image*", IV_GET|IV_STORE, popupImageMenu,
     NAME_appearance, "Right mark if popup not equal @nil"),
  IV(NAME_acceleratorFont, "font=font*", IV_GET,
     NAME_appearance, "When not @nil, font for accelerators"),
  SV(NAME_margin, "margin=0..", IV_GET|IV_STORE, marginMenu,
     NAME_appearance, "Extra margin at left and right side of values (for popup)"),
  IV(NAME_leftOffset, "offset=0..", IV_GET,
     NAME_update, "Offset of item in its box (left-side)"),
  IV(NAME_rightOffset, "offset=0..", IV_GET,
     NAME_update, "Offset of item in its box (right-side)"),
  IV(NAME_itemOffset, "offset=point", IV_GET,
     NAME_update, "Offset of the first item"),
  IV(NAME_itemSize, "size=size", IV_GET,
     NAME_update, "Size of item-box"),
  IV(NAME_labelArea, "area=area*", IV_GET,
     NAME_update, "Area for the label (if show_label equals @on)")
};

/* Send Methods */

static senddecl send_menu[] =
{ SM(NAME_compute, 0, NULL, computeMenu,
     DEFAULT, "Compute desired size"),
  SM(NAME_status, 1, "{inactive,active,preview,execute}", statusMenu,
     DEFAULT, "Status for event-processing"),
  SM(NAME_event, 1, "event", eventMenu,
     DEFAULT, "Process an event"),
  SM(NAME_initialise, 3, T_initialise, initialiseMenu,
     DEFAULT, "Create from label, kind and message"),
  SM(NAME_unlink, 0, NULL, unlinkMenu,
     DEFAULT, "Unlink from menu-items"),
  SM(NAME_assignAccelerators, 0, NULL, assignAcceletatorsMenu,
     NAME_accelerator, "Assign accelerators for the items"),
  SM(NAME_activeAllItems, 1, "active=bool", activeAllItemsMenu,
     NAME_active, "(De)activate all items in the menu"),
  SM(NAME_activeItem, 2, T_activeItem, activeItemMenu,
     NAME_active, "(De)activate item in menu"),
  SM(NAME_allOff, 0, NULL, allOffMenu,
     NAME_active, "Deactivate all menu_items"),
  SM(NAME_allOn, 0, NULL, allOnMenu,
     NAME_active, "Activate all menu_items"),
  SM(NAME_isOff, 1, "item=member:menu_item", isOffMenu,
     NAME_active, "Test if item is off"),
  SM(NAME_isOn, 1, "item=member:menu_item", isOnMenu,
     NAME_active, "Test if item is on"),
  SM(NAME_off, 1, "item=member:menu_item", offMenu,
     NAME_active, "Deactivate a menu_item or value"),
  SM(NAME_on, 1, "item=member:menu_item", onMenu,
     NAME_active, "Activate a menu_item or value"),
  SM(NAME_update, 1, "context=[any]", updateMenu,
     NAME_active, "Evaluate each item's condition"),
  SM(NAME_apply, 1, "always=[bool]", applyMenu,
     NAME_apply, "->execute if <-modified or @on"),
  SM(NAME_default, 1, "value=any|chain|function", defaultMenu,
     NAME_apply, "Set variable -default and ->selection"),
  SM(NAME_modified, 1, "modified=bool", modifiedMenu,
     NAME_apply, "Reset modified flag"),
  SM(NAME_restore, 0, NULL, restoreMenu,
     NAME_apply, "Set ->selection to <-default"),
  SM(NAME_execute, 1, "event=[event]", executeMenu,
     NAME_execute, "Change selection and forward message"),
  SM(NAME_append, 1, "item=menu_item", appendMenu,
     NAME_items, "Append a menu_item, value or popup"),
  SM(NAME_clear, 0, NULL, clearMenu,
     NAME_items, "Delete all menu_items"),
  SM(NAME_delete, 1, "item=member:menu_item", deleteMenu,
     NAME_items, "Delete a menu_item or value"),
  SM(NAME_member, 1, "item=member:menu_item", memberMenu,
     NAME_items, "Test if menu_item or value is member"),
  SM(NAME_members, 1, "chain", membersMenu,
     NAME_items, "->clear and ->append members of chain"),
  SM(NAME_prepend, 1, "item=menu_item", prependMenu,
     NAME_items, "Prepend a menu_item, value or popup"),
  SM(NAME_sort, 1, "[code]", sortMenu,
     NAME_items, "Sort members (see `chain ->sort')"),
  SM(NAME_labelWidth, 1, "width=[int]", labelWidthMenu,
     NAME_layout, "Set width in pixels used for label"),
  SM(NAME_ChangedItem, 1, "changed=menu_item", ChangedItemMenu,
     NAME_repaint, "Handle change of item"),
  SM(NAME_clearSelection, 0, NULL, clearSelectionMenu,
     NAME_selection, "Clear the selection"),
  SM(NAME_next, 0, NULL, nextMenu,
     NAME_selection, "Set selection to next (`cycle')"),
  SM(NAME_selected, 2, T_selected, selectedMenu,
     NAME_selection, "(De)select a single menu_item or value"),
  SM(NAME_selection, 1, "selection=member:menu_item|chain*", selectionMenu,
     NAME_selection, "Select menu_item or value (or chain)"),
  SM(NAME_selectedCompletion, 1, "dict_item", selectedCompletionMenu,
     NAME_internal, "Handle combo_box message"),
  SM(NAME_keyboardQuit, 0, NULL, keyboardQuitMenu,
     NAME_complete, "Remove completer")
};

/* Get Methods */

static getdecl get_menu[] =
{ GM(NAME_contains, 0, "items=chain", NULL, getContainsMenu,
     DEFAULT, "Chain with menu_items contained"),
  GM(NAME_reference, 0, "point", NULL, getReferenceMenu,
     DEFAULT, "Baseline of label"),
  GM(NAME_activeItem, 1, "active=bool", "item=member:menu_item", getActiveItemMenu,
     NAME_active, "Active value if indicated item"),
  GM(NAME_default, 0, "value=any|chain", NULL, getDefaultMenu,
     NAME_apply, "Current default value"),
  GM(NAME_modified, 0, "modified=bool", NULL, getModifiedMenu,
     NAME_apply, "If @on, menu has been modified"),
  GM(NAME_itemFromEvent, 1, "item=menu_item", "event=event", getItemFromEventMenu,
     NAME_event, "Find item on which event occurred"),
  GM(NAME_member, 1, "member=menu_item", "value=any", getMemberMenu,
     NAME_items, "Find menu_item with given value"),
  GM(NAME_labelWidth, 0, "width=int", NULL, getLabelWidthMenu,
     NAME_layout, "Minimum width for label in pixels"),
  GM(NAME_valueWidth, 0, "width=int", NULL, getValueWidthMenu,
     NAME_layout, "Minimum width for value in pixels"),
  GM(NAME_selected, 1, "selected=bool", "item=member:menu_item", getSelectedMenu,
     NAME_selection, "Find out if menu_item or value is selected"),
  GM(NAME_selection, 0, "values=any|chain*", NULL, getSelectionMenu,
     NAME_selection, "Get current selection (menu_item<-value or chain)")
};

/* Resources */

static classvardecl rc_menu[] =
{ RC(NAME_acceleratorFont, "font*", "@nil",
     "Show the accelerators"),
  RC(NAME_border, "int", "2",
     "Border around each item"),
  RC(NAME_cycleIndicator, "{combo_box}|image|elevation",
     UXWIN("when(@colour_display,  elevation(1), @ol_cycle_image)",
	   "combo_box"),
     "Indication of a ->kind: cycle menu"),
  RC(NAME_feedback, "name", "image",
     "Type of feedback for selection"),
  RC(NAME_format, "{left,center,right}", "left",
     "Adjust items {left,center,right} in their box"),
  RC(NAME_gap, "size", "size(0,0)",
     "Gap between items (XxY)"),
  RC(NAME_itemElevation, "elevation*",
     UXWIN("when(@colour_display, button, @nil)", "2"),
     "Elevation of items in the menu"),
  RC(NAME_markElevation, "elevation*",
     UXWIN("when(@colour_display, mark, @nil)",
	   "elevation(mark, 2, colour := white)"),
     "Elevation of marks"),
  RC(NAME_kind, "name", "marked",
     "Default menu kind"),
  RC(NAME_layout, "name", "horizontal",
     "Layout of the menu: {horizontal,vertical}"),
  RC(NAME_margin, "0..", "0",
     "Margin to the left and right"),
  RC(NAME_offImage, "{marked}|image*",
     UXWIN("@nomark_image", "marked"),
     "Marker for items not in selection"),
  RC(NAME_onImage, "{marked}|image*",
     UXWIN("@mark_image", "marked"),
     "Marker for items in selection"),
  RC(NAME_pen, "int", "when(@colour_display, 0, 1)",
     "Thickness of pen around items"),
  RC(NAME_popupImage, "image*", "@nil",
     "Marker for items with popup"),
  RC(NAME_previewElevation, "elevation*", "0",
     "Elevation of item in preview mode"),
  RC(NAME_previewFeedback, "name", "box",
     "Indication item is in preview state"),
  RC(NAME_showLabel, "bool", "@on",
     "Show the label"),
  RC(NAME_valueWidth, "int", "0",
     "Minimum width for popup menu"),
  RC(NAME_verticalFormat, "{top,center,bottom}", "center",
     "Adjust items {top,center,bottom} in their box"),
  RC(NAME_selectedForeground, "colour|pixmap",
     UXWIN("white", "win_highlighttext"),
     "Text colour for selected item"),
  RC(NAME_selectedBackground, "colour|pixmap",
     UXWIN("black", "win_highlight"),
     "Background colour for selected item"),
  RC(NAME_elevation, RC_REFINE,
     UXWIN("when(@colour_display, 0, @nil)", "@nil"), NULL),
  RC(NAME_valueFont, RC_REFINE, "normal", NULL)
};

/* Class Declaration */

static Name menu_termnames[] = { NAME_label, NAME_kind, NAME_message };

ClassDecl(menu_decls,
          var_menu, send_menu, get_menu, rc_menu,
          3, menu_termnames,
          "$Rev$");

status
makeClassMenu(Class class)
{ declareClass(class, &menu_decls);
  setRedrawFunctionClass(class, RedrawAreaMenu);

  succeed;
}
