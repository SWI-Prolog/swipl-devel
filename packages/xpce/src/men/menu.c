/*  $Id$
    Part of XPCE
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

#define CYCLE_DROP_WIDTH 14
#define CYCLE_DROP_HEIGHT 14
#define CYCLE_TRIANGLE_WIDTH 9
#define CYCLE_TRIANGLE_HEIGHT 8
#define CYCLE_DROP_DISTANCE 5

#define MARK_DIAMOND_SIZE 16
#define MARK_BOX_SIZE     13
#define MARK_CIRCLE_SIZE  8

status
initialiseMenu(Menu m, Name name, Name kind, Code msg)
{ createDialogItem(m, name);
 
  assign(m, message,		msg);
  assign(m, members,	        newObject(ClassChain, 0));
  assign(m, multiple_selection, OFF);

  assign(m, preview,		NIL);
  assign(m, preview_feedback,	DEFAULT);

  assign(m, kind,		kind);
  assign(m, feedback,		DEFAULT);
  assign(m, pen,		DEFAULT);
  assign(m, show_label,         DEFAULT); /* resource */
  assign(m, label_font,         NIL);	  /* resource if show_label != OFF */
  assign(m, label_width,        DEFAULT); /* dialog layout system */
  assign(m, value_font,         DEFAULT); /* resource */
  assign(m, value_width,        DEFAULT); /* minimum width of values */
  assign(m, accelerator_font,   DEFAULT);
  assign(m, layout,             DEFAULT);
  assign(m, columns,            ONE);
  assign(m, format,		DEFAULT);
  assign(m, vertical_format,	DEFAULT);
  assign(m, gap,		DEFAULT);
  assign(m, border,		DEFAULT);
  assign(m, margin,		DEFAULT);
  
  assign(m, on_image,	        DEFAULT); /* Image for selected == @on */
  assign(m, off_image,		DEFAULT); /* Image for selected == @off */
  assign(m, popup_image,	DEFAULT); /* Image for popup != @nil */

  assign(m, left_offset,	ZERO);
  assign(m, right_offset,	ZERO);
  assign(m, label_area,		NIL);
  assign(m, item_offset,	newObject(ClassPoint, 0));
  assign(m, item_size,		newObject(ClassSize, 0));
  obtainResourcesObject(m);

  kindMenu(m, kind);

  return requestComputeGraphical(m, DEFAULT);
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

  if ( isDefault(m->show_label) )
    assign(m, show_label, getResourceValueObject(m, NAME_showLabel));

  if ( m->show_label == ON )
  { int w, h;

    if ( isNil(m->label_area) )
      assign(m, label_area, newObject(ClassArea, 0));
    if ( isNil(m->label_font) )
      assign(m, label_font, getResourceValueObject(m, NAME_labelFont));
    
    str_size(&m->label->data, m->label_font, &w, &h);
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
  { Any ci = getResourceValueObject(m, NAME_cycleIndicator);
    
    if ( instanceOfObject(ci, ClassElevation) )
      iox += CYCLE_DROP_WIDTH + CYCLE_DROP_DISTANCE;
    else /* if ( instanceOfObject(ci, ClassImage) ) */
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


static int
named_image_width(Menu m)
{ if ( m->look == NAME_motif )
    return max(MARK_DIAMOND_SIZE, MARK_BOX_SIZE);
  else /* if ( m->look == NAME_win ) */
    return MARK_CIRCLE_SIZE + 2;
}


static status
computeItemsMenu(Menu m)
{ int w = 0, h = 0, iw, ih;
  int rm = 0, lm = 0;
  Cell cell;
  int border = valInt(m->border);
  int popup = 0;

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

  if ( notNil(m->on_image) || notNil(m->off_image) )
  { if ( instanceOfObject(m->on_image, ClassImage) )
      lm = valInt(m->on_image->size->w);
    else if ( notNil(m->on_image) )
      lm = named_image_width(m);

    if ( instanceOfObject(m->off_image, ClassImage) )
      lm = max(lm, valInt(m->off_image->size->w));
    else if ( notNil(m->off_image) )
      lm = max(lm, named_image_width(m));

    lm += 5;				/* TBD: Parameter? */
  }
  
  if ( isDefault(m->accelerator_font) )
    assign(m, accelerator_font,
	   getResourceValueObject(m, NAME_acceleratorFont));

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

    if ( m->multiple_selection == OFF )
      ensureSingleSelectionMenu(m);

    computeLabelMenu(m);
    computeItemsMenu(m);

    if ( m->show_label == ON )
    { Area a = m->label_area;

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
  { ComputeGraphical(m);

    if ( notNil(m->label_font) )
      ref = answerObject(ClassPoint, ZERO, getAscentFont(m->label_font), 0);
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

  compute_popup_indicator(m, mi, &iw, &ih);
  iy = (m->vertical_format == NAME_top    ? y :
	m->vertical_format == NAME_center ? y + (h-ih)/2 :
					    y + h - ih);
  ix = x+w-b-iw;

  if ( notNil(m->popup_image) )
  { r_image(m->popup_image, 0, 0, ix, iy, iw, ih, ON);
  } else if ( (z = getResourceValueObject(m, NAME_elevation)) )
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
      succeed;
    if ( (m->look == NAME_motif || m->look == NAME_win) &&
	 instanceOfObject(m, ClassPopup) )
      succeed;
  }

  fail;
}


static status
RedrawMenuItem(Menu m, MenuItem mi, int x, int y, int w, int h, Elevation z)
{ int b = valInt(m->border);
  int lm = valInt(m->left_offset);
  int rm = valInt(m->right_offset);
  Image leftmark = NIL;
  int pen = valInt(m->pen);
  int ix, iy, iw, ih;
  int radius = 0;
  Image fill = NIL;
  Any colour = mi->colour;

  DEBUG(NAME_menu, Cprintf("Redraw %s at %d %d %d %d\n",
			   pp(mi->value), x, y, w, h));

  if ( mi->active == OFF )
  { Any c2 = getResourceValueObject(m, NAME_inactiveColour);

    if ( c2 && notNil(c2) )
      colour = c2;

    r_colour(colour);
  }

  if ( mi->selected == ON && notNil(m->on_image) )
    leftmark = m->on_image;
  else if ( mi->selected == OFF && notNil(m->off_image) )
    leftmark = m->off_image;

  if ( elevated_items(m, z) )
  { int up = TRUE;

    if ( m->preview == mi )
    { z = getResourceValueObject(m, NAME_previewElevation);
    } else if ( mi->selected == ON )
      up = FALSE;

    r_3d_box(x, y, w, h, 0, z, up);

    if ( mi->end_group == ON )
    { Elevation mz = getResourceValueObject(m, NAME_elevation);

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

    if ( mi->end_group == ON )
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

  if ( instanceOfObject(leftmark, ClassImage) )
  { int bw, bh, by;
    
    bw = valInt(leftmark->size->w);
    bh = valInt(leftmark->size->h);
    by = item_mark_y(m, y, h, bh);

    if ( instanceOfObject(z, ClassElevation) )
    { int h = valInt(z->height);
      r_3d_box(x+b-h, by-h, bw+2*h, bh+2*h, 0, z, FALSE);
      r_fill(x+b, by, bw, bh, WHITE_COLOUR);
    }

    r_image(leftmark, 0, 0, x+b, by, bw, bh, ON);
  } else if ( (Name) leftmark == NAME_marked )
  { if ( m->look == NAME_motif )
    { if ( m->multiple_selection == ON )
      { int dy = item_mark_y(m, y, h, MARK_BOX_SIZE);

	r_3d_box(x+b, dy, MARK_BOX_SIZE, MARK_BOX_SIZE,
		 0, z, mi->selected == OFF);
      } else
      { int dy = item_mark_y(m, y, h, MARK_DIAMOND_SIZE);

	r_3d_diamond(x+b, dy, MARK_DIAMOND_SIZE, MARK_DIAMOND_SIZE,
		     z, mi->selected == OFF);
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

  if ( notNil(m->accelerator_font) && notNil(mi->accelerator) )
  { FontObj f = m->accelerator_font;
    int fw = valInt(getExFont(f));

    str_string(&mi->accelerator->data, f, x, y, w-fw/2, h,
	       NAME_right, m->vertical_format);
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
    str_string(&((Name) mi->label)->data, f,
	       ix+fw/2, iy, iw-fw, ih, m->format, m->vertical_format);
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
  Elevation z  = getResourceValueObject(m, NAME_elevation);
  Elevation iz = getResourceValueObject(m, NAME_itemElevation);

  initialiseDeviceGraphical(m, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  
  if ( m->show_label == ON )
  { int lw = (isDefault(m->label_width) ? valInt(m->label_area->w)
				        : valInt(m->label_width));
    if ( m->layout == NAME_horizontal )
      lw -= valInt(getExFont(m->label_font));

    str_string(&m->label->data, m->label_font,
	       x + valInt(m->label_area->x), y + valInt(m->label_area->y),
	       lw, valInt(m->label_area->h),
	       m->label_format, NAME_center);
  }
  
  bx = cx = x + valInt(m->item_offset->x);
  by = cy = y + valInt(m->item_offset->y);
  iw = valInt(m->item_size->w);
  ih = valInt(m->item_size->h);
  
  if ( m->feedback == NAME_showSelectionOnly )
  { MenuItem mi = getItemSelectionMenu(m);
    Any ci = getResourceValueObject(m, NAME_cycleIndicator);

    if ( instanceOfObject(ci, ClassElevation) )
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
{ computeLabelMenu(m);

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
{ if ( eventDialogItem(m, ev) )
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

  return forwardDialogItem((DialogItem) m, msg, ev);
}

  

static status
executeMenu(Menu m, EventObj ev)
{ MenuItem mi;

  if ( m->feedback == NAME_showSelectionOnly )
  { nextMenu(m);
    flushGraphical(m);
    if ( !send(m->device, NAME_modifiedItem, m, ON, 0) )
      forwardMenu(m, m->message, ev);
    succeed;
  }

  if ( isDefault(ev) )
    ev = EVENT->value;			/* @event */
  TRY((mi = getItemFromEventMenu(m, ev)) && mi->active == ON);
    
  if ( m->multiple_selection == ON )
  { toggleMenu(m, mi);
    flushGraphical(m);
    
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
{ if ( notNil(m->preview) )
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

  return requestComputeGraphical(m, DEFAULT);
}


static status
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
  return requestComputeGraphical(m, DEFAULT);
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
	assign(mi, active, a);
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
  { if ( kind == NAME_marked || kind == NAME_choice || kind == NAME_toggle )
    { assign(m, on_image, NIL);
      assign(m, off_image, NIL);
      assign(m, feedback, NAME_box);
      assign(m, pen, ONE);
      assign(m, border, TWO);
      assign(m, multiple_selection, kind == NAME_toggle ? ON : OFF);

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
    }	else if ( kind == NAME_choice )
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
      { assign(m, on_image, MS_MARK_IMAGE);
	assign(m, off_image, MS_NOMARK_IMAGE);
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
    }
  }

  if ( equalName(kind, NAME_cycle) )
  { assign(m, on_image, NIL);
    assign(m, off_image, NIL);
    assign(m, feedback, NAME_showSelectionOnly);
    assign(m, layout,   NAME_horizontal);
    assign(m, accelerator_font, NIL);
    multipleSelectionMenu(m, OFF);
    assign(m, popup,    newObject(ClassPopup, 0));
    assign(m->popup,    members, m->members);
    kindMenu((Menu) m->popup,  NAME_cyclePopup);
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
labelFontMenu(Menu m, FontObj font)
{ return assignGraphical(m, NAME_labelFont, font);
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
{ sortChain(m->members, msg);

  return requestComputeGraphical(m, DEFAULT);
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


status
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
  return selectionMenu(m, val);
}


static status
applyMenu(Menu m, Bool always)
{ Any val;

  if ( instanceOfObject(m->message, ClassCode) &&
       (always == ON || getModifiedMenu(m) == ON) &&
       (val = getSelectionMenu(m)) )
  { forwardReceiverCode(m->message, m, val, 0);
    succeed;
  }

  fail;
}


status
makeClassMenu(Class class)
{ sourceClass(class, makeClassMenu, __FILE__, "$Revision$");

  localClass(class, NAME_selection, NAME_selection,
	     "values=any|chain", NAME_none,
	     "Value(s) of currently selected menu_items");
  localClass(class, NAME_members, NAME_organisation,
	     "items=chain", NAME_get,
	     "Menu items (alternatives)");  
  localClass(class, NAME_default, NAME_apply,
	     "value=any|chain|function*", NAME_none,
	     "Default value");
  localClass(class, NAME_kind, NAME_appearance,
	     "kind={cycle,marked,choice,toggle,popup,cycle_popup}",
	     NAME_get,
	     "Kind of menu");
  localClass(class, NAME_preview, NAME_event, "item=menu_item*", NAME_get,
	     "Item in `preview' state");
  localClass(class, NAME_previewFeedback, NAME_appearance,
	     "feedback={box,rounded_box,inverted_rounded_box,invert}",
	     NAME_both,
	     "Feedback given to item in preview state");
  localClass(class, NAME_feedback, NAME_appearance,
	     "feedback={box,invert,image,show_selection_only}",
	     NAME_get,
	     "Type of feedback for selection");
  localClass(class, NAME_multipleSelection, NAME_selection,
	     "multiple=bool", NAME_get,
	     "If @on, more than one item may be selected");
  localClass(class, NAME_showLabel, NAME_appearance, "show=bool", NAME_get,
	     "Whether label is visible");  
  localClass(class, NAME_labelFont, NAME_appearance, "font=font*", NAME_get,
	     "Font for label");  
  localClass(class, NAME_valueFont, NAME_appearance, "font=font", NAME_get,
	     "Font for value");  
  localClass(class, NAME_valueWidth, NAME_layout, "width=0..", NAME_get,
	     "Minimum width for values");
  localClass(class, NAME_layout, NAME_appearance,
	     "orientation={horizontal,vertical}", NAME_get,
	     "Horizontal or vertical layout");  
  localClass(class, NAME_columns, NAME_appearance, "number=1..", NAME_get,
	     "Number of columns");
  localClass(class, NAME_format, NAME_appearance,
	     "alignment={left,center,right}", NAME_get,
	     "Horizontal alignment of items in box");
  localClass(class, NAME_verticalFormat, NAME_appearance,
	     "alignment={top,center,bottom}", NAME_get,
	     "Vertical alignment of items (and decorations) in box");
  localClass(class, NAME_gap, NAME_layout, "size", NAME_get,
	     "Gap between items");
  localClass(class, NAME_border, NAME_appearance, "width=0..", NAME_get,
	     "Width of border around item");
  localClass(class, NAME_onImage, NAME_appearance,
	     "image=image|{marked}*", NAME_get,
	     "Left mark if selected equals @on");  
  localClass(class, NAME_offImage, NAME_appearance,
	     "image=image|{marked}*", NAME_get,
	     "Left mark if selected equals @off");  
  localClass(class, NAME_popupImage, NAME_appearance,
	     "image=image*", NAME_get,
	     "Right mark if popup not equal @nil");  
  localClass(class, NAME_acceleratorFont, NAME_appearance,
	     "font=font*", NAME_get,
	     "When not @nil, font for accelerators");
  localClass(class, NAME_margin, NAME_appearance, "margin=0..", NAME_get,
	     "Extra margin at left and right side of values (for popup)");

  localClass(class, NAME_leftOffset, NAME_update, "offset=0..", NAME_get,
	     "Offset of item in its box (left-side)");
  localClass(class, NAME_rightOffset, NAME_update, "offset=0..", NAME_get,
	     "Offset of item in its box (right-side)");
  localClass(class, NAME_itemOffset, NAME_update, "offset=point", NAME_get,
	     "Offset of the first item");
  localClass(class, NAME_itemSize, NAME_update, "size=size", NAME_get,
	     "Size of item-box");
  localClass(class, NAME_labelArea, NAME_update, "area=area*", NAME_get,
	     "Area for the label (if show_label equals @on)");

  termClass(class, "menu", 3, NAME_label, NAME_kind, NAME_message);
  setRedrawFunctionClass(class, RedrawAreaMenu);

  storeMethod(class, NAME_status,	     statusMenu);
  storeMethod(class, NAME_kind,		     kindMenu);
  storeMethod(class, NAME_showLabel,	     showLabelMenu);
  storeMethod(class, NAME_labelFont,	     labelFontMenu);
  storeMethod(class, NAME_valueFont,	     valueFontMenu);
  storeMethod(class, NAME_valueWidth,        valueWidthMenu);
  storeMethod(class, NAME_columns,	     columnsMenu);
  storeMethod(class, NAME_offImage,	     offImageMenu);
  storeMethod(class, NAME_onImage,	     onImageMenu);
  storeMethod(class, NAME_popupImage,	     popupImageMenu);
  storeMethod(class, NAME_layout,	     layoutMenu);
  storeMethod(class, NAME_border,	     borderMenu);
  storeMethod(class, NAME_gap,               gapMenu);
  storeMethod(class, NAME_format,            formatMenu);
  storeMethod(class, NAME_verticalFormat,    verticalFormatMenu);
  storeMethod(class, NAME_multipleSelection, multipleSelectionMenu);
  storeMethod(class, NAME_preview,           previewMenu);
  storeMethod(class, NAME_feedback,	     feedbackMenu);
  storeMethod(class, NAME_margin,	     marginMenu);

  sendMethod(class, NAME_initialise, DEFAULT,
	     3, "name=[name]", "kind=[name]", "message=[code]*",
	     "Create from label, kind and message",
	     initialiseMenu);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink from menu-items",
	     unlinkMenu);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Process an event",
	     eventMenu);
  sendMethod(class, NAME_execute, NAME_execute, 1, "event=[event]",
	     "Change selection and forward message",
	     executeMenu);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Compute desired size",
	     computeMenu);
  sendMethod(class, NAME_ChangedItem, NAME_repaint, 1, "changed=menu_item",
	     "Handle change of item",
	     ChangedItemMenu);

  sendMethod(class, NAME_labelWidth, NAME_layout, 1, "width=[int]",
	     "Set width in pixels used for label",
	     labelWidthMenu);

  sendMethod(class, NAME_append, NAME_items, 1, "item=menu_item",
	     "Append a menu_item, value or popup",
	     appendMenu);
  sendMethod(class, NAME_prepend, NAME_items, 1, "item=menu_item",
	     "Prepend a menu_item, value or popup",
	     prependMenu);
  sendMethod(class, NAME_delete, NAME_items, 1, "item=member:menu_item",
	     "Delete a menu_item or value",
	     deleteMenu);
  sendMethod(class, NAME_clear, NAME_items, 0,
	     "Delete all menu_items",
	     clearMenu);
  sendMethod(class, NAME_sort, NAME_items, 1, "[code]",
	     "Sort members (see `chain ->sort')",
	     sortMenu);
  sendMethod(class, NAME_update, NAME_active, 1, "context=[any]",
	     "Evaluate each item's condition",
	     updateMenu);
  sendMethod(class, NAME_activeItem, NAME_active, 2,
	     "item=member:menu_item", "active=bool",
	     "(De)activate item in menu",
	     activeItemMenu);
  sendMethod(class, NAME_on, NAME_active, 1, "item=member:menu_item",
	     "Activate a menu_item or value",
	     onMenu);
  sendMethod(class, NAME_off, NAME_active, 1, "item=member:menu_item",
	     "Deactivate a menu_item or value",
	     offMenu);
  sendMethod(class, NAME_isOn, NAME_active, 1, "item=member:menu_item",
	     "Test if item is on",
	     isOnMenu);
  sendMethod(class, NAME_isOff, NAME_active, 1, "item=member:menu_item",
	     "Test if item is off",
	     isOffMenu);
  sendMethod(class, NAME_allOn, NAME_active, 0,
	     "Activate all menu_items",
	     allOnMenu);
  sendMethod(class, NAME_allOff, NAME_active, 0,
	     "Deactivate all menu_items",
	     allOffMenu);
  sendMethod(class, NAME_activeAllItems, NAME_active, 1, "active=bool",
	     "(De)activate all items in the menu",
	     activeAllItemsMenu);
  sendMethod(class, NAME_next, NAME_selection, 0,
	     "Set selection to next (`cycle')",
	     nextMenu);
  sendMethod(class, NAME_selection, NAME_selection, 1,
	     "selection=member:menu_item|chain*",
	     "Select menu_item or value (or chain)",
	     selectionMenu);
  sendMethod(class, NAME_clearSelection, NAME_selection, 0,
	     "Clear the selection",
	     clearSelectionMenu);
  sendMethod(class, NAME_selected, NAME_selection, 2,
	     "item=member:menu_item", "selected=bool",
	     "(De)select a single menu_item or value",
	     selectedMenu);
  sendMethod(class, NAME_member, NAME_items, 1, "item=member:menu_item",
	     "Test if menu_item or value is member",
	     memberMenu);
  sendMethod(class, NAME_members, NAME_items, 1, "chain",
	     "->clear and ->append members of chain",
	     membersMenu);
  sendMethod(class, NAME_restore, NAME_apply, 0,
	     "Set ->selection to <-default",
	     restoreMenu);
  sendMethod(class, NAME_default, NAME_apply, 1, "value=int|function",
	     "Set variable -default and ->selection",
	     defaultMenu);
  sendMethod(class, NAME_apply, NAME_apply, 1, "always=[bool]",
	     "->execute if <-modified or @on",
	     applyMenu);
  sendMethod(class, NAME_modified, NAME_apply, 1, "modified=bool",
	     "Reset modified flag",
	     modifiedMenu);

  getMethod(class, NAME_selection, NAME_selection, "values=any|chain*", 0,
	    "Get current selection (menu_item<-value or chain)",
	    getSelectionMenu);
  getMethod(class, NAME_selected, NAME_selection, "selected=bool", 1,
	    "item=member:menu_item",
	    "Find out if menu_item or value is selected",
	    getSelectedMenu);
  getMethod(class, NAME_labelWidth, NAME_layout, "width=int", 0,
	    "Minimum width for label in pixels",
	    getLabelWidthMenu);
  getMethod(class, NAME_valueWidth, NAME_layout, "width=int", 0,
	    "Minimum width for value in pixels",
	    getValueWidthMenu);
  getMethod(class, NAME_member, NAME_items, "member=menu_item", 1, "value=any",
	    "Find menu_item with given value",
	    getMemberMenu);
  getMethod(class, NAME_activeItem, NAME_active, "active=bool", 1,
	    "item=member:menu_item",
	    "Active value if indicated item",
	    getActiveItemMenu);
  getMethod(class, NAME_contains, DEFAULT, "items=chain", 0,
	    "Chain with menu_items contained",
	    getContainsMenu);
  getMethod(class, NAME_itemFromEvent, NAME_event, "item=menu_item", 1,
	    "event=event",
	    "Find item on which event occurred",
	    getItemFromEventMenu);
  getMethod(class, NAME_modified, NAME_apply, "modified=bool", 0,
	    "If @on, menu has been modified",
	    getModifiedMenu);
  getMethod(class, NAME_default, NAME_apply, "value=any|chain", 0,
	    "Current default value",
	    getDefaultMenu);
  getMethod(class, NAME_reference, DEFAULT, "point", 0,
	    "Baseline of label",
	    getReferenceMenu);


  attach_resource(class, "kind",       "name",    "marked",
		  "Default menu kind");
  attach_resource(class, "preview_feedback", "name", "box",
		  "Indication item is in preview state");
  attach_resource(class, "feedback",   "name",    "image",
		  "Type of feedback for selection");
  attach_resource(class, "pen",	       "int",	  "0",
		  "Thickness of pen around items");
  attach_resource(class, "show_label", "bool",	  "@on",
		  "Show the label");
  attach_resource(class, "accelerator_font", "font*", "@nil",
		  "Show the accelerators");
  attach_resource(class, "value_width", "int",     "0",
		  "Minimum width for popup menu");
  attach_resource(class, "layout",     "name",	  "horizontal",
		  "Layout of the menu: {horizontal,vertical}");
  attach_resource(class, "format",     "{left,center,right}",	  "left",
		  "Adjust items {left,center,right} in their box");
  attach_resource(class, "vertical_format", "{top,center,bottom}", "center",
		  "Adjust items {top,center,bottom} in their box");
  attach_resource(class, "gap",	       "size",    "size(0,0)",
		  "Gap between items (XxY)");
  attach_resource(class, "border",     "int",	  "0",
		  "Border around each item");
  attach_resource(class, "on_image",   "image|{marked}*", "@mark_image",
		  "Marker for items in selection");
  attach_resource(class, "off_image",  "image|{marked}*", "@nomark_image",
		  "Marker for items not in selection");
  attach_resource(class, "popup_image", "image*",  "@nil",
		  "Marker for items with popup");
  attach_resource(class, "cycle_indicator", "image|elevation", "@cycle_image",
		  "Indication of a ->kind: cycle menu");
  attach_resource(class, "margin", "0..",  "0",
		  "Margin to the left and right");
  attach_resource(class, "item_elevation", "elevation*",  "0",
		  "Elevation of items in the menu");
  attach_resource(class, "preview_elevation", "elevation*",  "0",
		  "Elevation of item in preview mode");

  succeed;
}
