/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

		 /*******************************
		 *	    (TYPE) UTIL		*
		 *******************************/

static inline Table
table_of_cell(TableCell cell)
{ Table tab = (Table)cell->layout_manager;

  if ( isNil(tab) )
    fail;
  
  answer(tab);
}

		 /*******************************
		 *	      CREATE		*
		 *******************************/

static status
initialiseTableCell(TableCell c, Graphical image)
{ initialiseLayoutInterface(c, image);

  assign(c, halign,       DEFAULT);
  assign(c, valign,       DEFAULT);
  assign(c, col_span,     ONE);
  assign(c, row_span,     ONE);
  assign(c, cell_padding, DEFAULT);
  assign(c, selected,     OFF);
  assign(c, background,	  DEFAULT);

  succeed;
}


static status
unlinkTableCell(TableCell cell)
{ Table tab;

  if ( (tab=table_of_cell(cell)) )
    qadSendv(tab, NAME_delete, 1, (Any *)&cell);
    
  return unlinkLayoutInterface(cell);
}


static LayoutInterface
getConvertTableCell(Any context, Graphical image)
{ answer(answerObject(ClassTableCell, image, 0));
}


		 /*******************************
		 *	     MODIFIED		*
		 *******************************/

static status
modifiedImageTableCell(TableCell c)
{ Table tab = table_of_cell(c);
  Device dev;

  if ( tab && notNil(dev=tab->device) )
  { table_cell_dimensions d;

    dims_table_cell(c, &d);
    changedImageGraphical(dev, toInt(d.x), toInt(d.y), toInt(d.w), toInt(d.h));
  }

  succeed;
}


		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static status
selectedTableCell(TableCell c, Bool selected)
{ if ( c->selected != selected )
  { assign(c, selected, selected);
    modifiedImageTableCell(c);
  }

  succeed;
}


static status
backgroundTableCell(TableCell c, Any bg)
{ if ( c->background != bg )
  { assign(c, background, bg);
    modifiedImageTableCell(c);
  }

  succeed;
}


static status
noteMarkTableCell(TableCell c, Image mark)
{ if ( c->note_mark != mark )
  { assign(c, note_mark, mark);
    modifiedImageTableCell(c);
  }

  succeed;
}


static status
halignTableCell(TableCell c, Name align)
{ if ( c->halign != align )
  { assign(c, halign, align);

					/* or placeImageTableCell()? */
    requestComputeLayoutManager(c->layout_manager, DEFAULT);
  }

  succeed;
}


static status
valignTableCell(TableCell c, Name align)
{ if ( c->valign != align )
  { assign(c, valign, align);

    requestComputeLayoutManager(c->layout_manager, DEFAULT);
  }

  succeed;
}


static status
cellPaddingTableCell(TableCell c, Size padding)
{ if ( !(c->cell_padding == padding ||
	 (classOfObject(c->cell_padding) == classOfObject(padding) &&
	  equalSize(c->cell_padding, padding))) )
  { assign(c, cell_padding, padding);

    requestComputeLayoutManager(c->layout_manager, DEFAULT);
  }

  succeed;
}


Name
getHalignTableCell(TableCell cell)
{ Table tab;

  if ( notDefault(cell->halign) )
    answer(cell->halign);

  if ( (tab=table_of_cell(cell)) && notNil(tab->columns) )
  { TableColumn col = getColumnTable(tab, cell->column, OFF);

    if ( col )
      answer(col->alignment);
  }

  answer(NAME_left);
}


Name
getValignTableCell(TableCell cell)
{ Table tab;

  if ( notDefault(cell->valign) )
    answer(cell->valign);

  if ( (tab=table_of_cell(cell)) && notNil(tab->rows) )
  { TableRow row = getRowTable(tab, cell->row, OFF);

    if ( row )
      answer(row->alignment);
  }

  answer(NAME_top);
}


static Table
getTableTableCell(TableCell cell)
{ answer(table_of_cell(cell));
}


static status
colSpanTableCell(TableCell cell, Int span)
{ if ( cell->col_span != span )
  { Table tab = table_of_cell(cell);
    
    if ( tab )
    { int x,y;
      int fx = valInt(cell->column);
      int w  = valInt(span);
      int ow = valInt(cell->col_span);
      int tx = max(w,ow)+fx;

      for(y=valInt(cell->row); y<valInt(cell->row)+valInt(cell->row_span); y++)
      { TableRow row = getRowTable(tab, toInt(y), ON);

	for(x=fx+1; x<tx; x++)
	{ Any e;

	  if ( x-fx < w )
	    e = cell;
	  else
	    e = NIL;
	  
	  cellTableRow(row, toInt(x), e);
	}
      }

      assign(cell, col_span, span);
      changedTable(tab);		/* changed line-pattern */
      requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
    } else
    { assign(cell, col_span, span);
    }
  }

  succeed;
}

static status
rowSpanTableCell(TableCell cell, Int span)
{ if ( cell->row_span != span )
  { Table tab = table_of_cell(cell);
    
    if ( tab )
    { int x,y;
      int fy = valInt(cell->row);
      int h  = valInt(span);
      int oh = valInt(cell->row_span);
      int ty = max(h,oh)+fy;

      for(y=fy+1; y<ty; y++)
      { TableRow row = getRowTable(tab, toInt(y), ON);

	for(x=valInt(cell->column); x<valInt(cell->column)+valInt(cell->col_span); x++)
	{ Any e;

	  if ( y-fy < h )
	    e = cell;
	  else
	    e = NIL;
	  
	  cellTableRow(row, toInt(x), e);
	}
      }

      assign(cell, row_span, span);
      changedTable(tab);		/* changed line-pattern */
      requestComputeLayoutManager((LayoutManager)tab, DEFAULT);
    } else
    { assign(cell, row_span, span);
    }
  }

  succeed;
}


		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

void
table_cell_padding(TableCell cell, int *pxptr, int* pyptr)
{ Size size;
  Table tab;

  if ( notDefault(cell->cell_padding) )
    size = cell->cell_padding;
  else if ( (tab=table_of_cell(cell)) )
    size = tab->cell_padding;
  else
    return;				/* should not happen */

  *pxptr = valInt(size->w);
  *pyptr = valInt(size->h);
}


void
dims_table_cell(TableCell cell, TableCellDimensions dims)
{ int w=0, h=0;
  TableRow row;
  TableColumn col;
  int cx, cy;
  int ncols = valInt(cell->col_span);
  int nrows = valInt(cell->row_span);
  int n;
  Table tab = table_of_cell(cell);
  int x = valInt(cell->column);
  int y = valInt(cell->row);

  table_cell_padding(cell, &dims->px, &dims->py);

  row = getRowTable(tab,    cell->row, ON); /* play safe */
  col = getColumnTable(tab, cell->column, ON);

  dims->x  = valInt(col->position);
  dims->y  = valInt(row->position);
  dims->rx = valInt(col->reference);
  dims->ry = valInt(row->reference);

  w        = valInt(col->width);
  h	   = valInt(row->width);

  if ( ncols > 1 || nrows > 1 )
  { int rowspace = valInt(tab->cell_spacing->h);
    int colspace = valInt(tab->cell_spacing->w);
					/* cells spanned */

    for(n=1, cx=x+1; n<ncols; n++, cx++)
      w += colspace+valInt(getColumnTable(tab, toInt(cx), ON)->width);
    for(n=1, cy=y+1; n<nrows; n++, cy++)
      h += rowspace+valInt(getRowTable(tab, toInt(cy), ON)->width);
  }

  dims->w = w;
  dims->h = h;
}


static Area
getAreaTableCell(TableCell c)
{ Table tab = table_of_cell(c);
  Device dev;

  if ( tab && notNil(dev=tab->device) )
  { table_cell_dimensions d;

    ComputeGraphical(dev);		/* make sure area is up-to-date */
    dims_table_cell(c, &d);
    answer(answerObject(ClassArea,
			toInt(d.x), toInt(d.y), toInt(d.w), toInt(d.h),
			0));
  }

  fail;
}


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

static Any
getIf(Any cell, Name method, Any def)	/* can be optimised further */
{ Any rval;

  if ( hasGetMethodObject(cell, method) &&
       (rval = getv(cell, method, 0, NULL)) )
    answer(rval);

  answer(def);
}


status
placeImageTableCell(TableCell cell)
{ Graphical gr = cell->image;
  Table tab = table_of_cell(cell);
  table_cell_dimensions d;
  int grx, gry;
  Any av[4];
  Point ref = NULL;
  Name halign = getHalignTableCell(cell);
  Name valign = getValignTableCell(cell);

  ComputeGraphical(gr);			/* make sure */
	  
  av[2] = DEFAULT;			/* width */
  av[3] = DEFAULT;			/* height */

  dims_table_cell(cell, &d);

  if ( halign == NAME_left )		/* determine X-placement */
    grx = d.x + d.px;
  else if ( halign == NAME_right )
    grx = d.x + d.w-d.px-valInt(gr->area->w);
  else if ( halign == NAME_center )
    grx = d.x + (d.w-valInt(gr->area->w)+1)/2;
  else if ( halign == NAME_stretch )
  { grx = d.x + d.px;
    av[2] = toInt(d.w-2*d.px);
  } else /* if ( halign == NAME_reference ) */
  { ref = getIf(gr, NAME_reference, NIL);

    if ( notNil(ref) )
      grx = d.x + d.rx - valInt(ref->x);
    else
      grx = d.x + d.px;
  }
  
  if ( valign == NAME_top )		/* determine Y-placement */
    gry = d.y + d.py;
  else if ( valign == NAME_bottom )
    gry = d.y + d.h-d.py-valInt(gr->area->h);
  else if ( valign == NAME_center )
    gry = d.y + (d.h-valInt(gr->area->h)+1)/2;
  else if ( valign == NAME_stretch )
  { gry = d.y + d.py;
    av[3] = toInt(d.h-2*d.py);
  } else /* if ( halign == NAME_reference ) */
  { if ( !ref )
      ref = getIf(gr, NAME_reference, NIL);

    if ( notNil(ref) )
      gry = d.y + d.ry - valInt(ref->y);
    else
      gry = gry = d.y + d.py;
  }
  
  av[0] = toInt(grx);
  av[1] = toInt(gry);

  qadSendv(gr, NAME_doSet, 4, av);
  if ( gr->device != tab->device )
    send(tab->device, NAME_display, gr, 0);

  succeed;
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

static vardecl var_table_cell[] =
{ IV(NAME_column, "int*", IV_GET,
     NAME_location, "X-location in table environment"),
  IV(NAME_row, "int*", IV_GET,
     NAME_location, "Y-location in table environment"),
  SV(NAME_halign, "[{left,center,right,reference,stretch}]", IV_NONE|IV_STORE,
     halignTableCell,
     NAME_layout, "Horizontal alignment of <-image in cell"),
  SV(NAME_valign, "[{top,center,bottom,reference,stretch}]", IV_NONE|IV_STORE,
     valignTableCell,
     NAME_layout, "Vertical alignment of <-image in cell"),
  SV(NAME_colSpan, "1..", IV_GET|IV_STORE, colSpanTableCell,
     NAME_layout, "Number of columns spanned"),
  SV(NAME_rowSpan, "1..", IV_GET|IV_STORE, rowSpanTableCell,
     NAME_layout, "Number of rows spanned"),
  SV(NAME_cellPadding, "[size]", IV_GET|IV_STORE, cellPaddingTableCell,
     NAME_layout, "Size around contents of the cell"),
  SV(NAME_selected, "bool", IV_GET|IV_STORE, selectedTableCell,
     NAME_selection, "Is cell selected?"),
  SV(NAME_background, "[colour|pixmap]", IV_GET|IV_STORE, backgroundTableCell,
     NAME_colour, "Backround colour for the cell"),
  SV(NAME_noteMark, "image*", IV_GET|IV_STORE, noteMarkTableCell,
     NAME_appearance, "Image painted in the top-right corner")
};
  
/* Send Methods */

static senddecl send_table_cell[] =
{ SM(NAME_initialise, 1, "graphical", initialiseTableCell,
     DEFAULT, "Initialise abstract instance"),
  SM(NAME_unlink, 0, NULL, unlinkTableCell,
     DEFAULT, "Remove <-image from device")
};

/* Get Methods */

static getdecl get_table_cell[] =
{ GM(NAME_convert, 1, "table_cell", "graphical",
     getConvertTableCell,
     DEFAULT, "Convert graphical object"),
  GM(NAME_halign, 0, "{left,center,right,reference,stretch}", NULL,
     getHalignTableCell,
     DEFAULT, "Current horizontal alignment"),
  GM(NAME_valign, 0, "{top,center,bottom,reference,stretch}", NULL,
     getValignTableCell,
     DEFAULT, "Current vertical alignment"),
  GM(NAME_table, 0, "table", NULL,
     getTableTableCell,
     DEFAULT, "The <-layout_manager if not @nil"),
  GM(NAME_area, 0, "area", NULL,
     getAreaTableCell,
     NAME_area, "Area in device coordinates")
};

/* Resources */

#define rc_table_cell NULL
/*
static classvardecl rc_table_cell[] =
{
};
*/

/* Class Declaration */

static Name table_cell_termnames[] = { NAME_image };

ClassDecl(table_cell_decls,
          var_table_cell,
	  send_table_cell,
	  get_table_cell,
	  rc_table_cell,
          1, table_cell_termnames,
          "$Rev$");

status
makeClassTableCell(Class class)
{ return declareClass(class, &table_cell_decls);
}

