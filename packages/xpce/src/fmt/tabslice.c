/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

status
initialiseTableSlice(TableSlice c)
{ initialiseVectorv((Vector)c, 0, NULL);

  assign(c, background, DEFAULT);
  assign(c, end_group,  OFF);
  assign(c, index,      ZERO);
  assign(c, width,      ZERO);
  assign(c, reference,  ZERO);
  assign(c, position,   ZERO);
  assign(c, fixed,      OFF);
/*assign(c, table,      NIL);
*/

  succeed;
}


static status
endGroupTableSlice(TableSlice slice, Bool end)
{ if ( slice->end_group != end )
  { assign(slice, end_group, end);
    if ( notNil(slice->table) )
      changedTable(slice->table);
  }

  succeed;
}


static status
widthTableSlice(TableSlice slice, Int width)
{ if ( notDefault(width) )
  { assign(slice, width, width);
    assign(slice, fixed, ON);
  } else
  { assign(slice, fixed, OFF);
  }

  if ( notNil(slice->table) )
    return requestComputeLayoutManager((LayoutManager)slice->table, DEFAULT);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

static vardecl var_table_slice[] =
{ IV(NAME_table, "table*", IV_GET,
     NAME_organisation, "Table I belong to"),
  IV(NAME_background, "[colour|pixmap]", IV_GET,
     NAME_colour, "Default background of the cells"),
  IV(NAME_alignment, "{top,bottom,left,right,center,reference,stretch}",
     IV_GET,
     NAME_layout, "Default alignment of cells"),
  SV(NAME_endGroup, "bool", IV_GET|IV_STORE, endGroupTableSlice,
     NAME_appearance, "Row/column ends a group (rules)"),
  IV(NAME_index, "int", IV_GET,
     NAME_position, "X/Y position for column/row"),
  IV(NAME_fixed, "bool", IV_BOTH,
     NAME_layout, "<-width and <-reference are fixed"),
  IV(NAME_width, "0..", IV_GET,
     NAME_layout, "Total width/height of the column/row"),
  IV(NAME_reference, "int", IV_GET,
     NAME_layout, "Location of the reference"),
  IV(NAME_position, "int", IV_GET,
     NAME_layout, "X/Y-offset of the column/row")
};
  
/* Send Methods */

static senddecl send_table_slice[] =
{ SM(NAME_initialise, 0, NULL, initialiseTableSlice,
     DEFAULT, "Initialise abstract instance"),
  SM(NAME_width, 1, "[int]", widthTableSlice,
     NAME_layout, "Set (fixed) width of the table slice")
};

/* Get Methods */

#define get_table_slice NULL
/*
static getdecl get_table_slice[] =
{ GM(NAME_convert, 1, "table_slice", "graphical",
     getConvertTableSlice,
     DEFAULT, "Convert graphical object")
};
*/

/* Resources */

#define rc_table_slice NULL
/*
static resourcedecl rc_table_slice[] =
{
};
*/

/* Class Declaration */

ClassDecl(table_slice_decls,
          var_table_slice,
	  send_table_slice,
	  get_table_slice,
	  rc_table_slice,
          0, NULL,
          "$Rev$");

status
makeClassTableSlice(Class class)
{ return declareClass(class, &table_slice_decls);
}


		 /*******************************
		 *	       COLUMN		*
		 *******************************/

static Any				/* TBD: merge */
getIf(Any cell, Name method, Any def)	/* can be optimised further */
{ Any rval;

  if ( hasGetMethodObject(cell, method) &&
       (rval = getv(cell, method, 0, NULL)) )
    answer(rval);

  answer(def);
}


static status
initialiseTableColumn(TableColumn col, Name halign)
{ initialiseTableSlice((TableSlice)col);

  if ( isDefault(halign) )
    halign = NAME_left;

  assign(col, alignment, halign);

  succeed;
}


static status
unlinkTableColumn(TableColumn col)
{ if ( notNil(col->table) && !isFreeingObj(col->table) )
    send(col->table, NAME_delete, col, 0);

  return unlinkVector((Vector)col);
}


static void
changedImageTableColumn(TableColumn col)
{ Table tab;
  Device dev;

  if ( notNil(tab = col->table) && notNil(dev=tab->device) )
    changedImageGraphical(dev, col->position, 0, col->width, tab->area->h);
}


static status
halignTableColumn(TableColumn col, Name halign)
{ assign(col, alignment, halign);

  succeed;
}


static Name
getHalignTableColumn(TableColumn col)
{ answer(col->alignment);
}


static status
backgroundTableColumn(TableColumn col, Any bg)
{ if ( col->background != bg )
  { assign(col, background, bg);
    changedImageTableColumn(col);
  }

  succeed;
}


TableCell
getCellTableColumn(TableColumn col, Int y)
{ Table tab = col->table;
  TableRow row = getElementVector(tab->rows, y);

  if ( row && notNil(row) )
    answer(getCellTableRow(row, col->index));

  fail;
}


static status
computeTableColumn(TableColumn col)
{ Table tab = col->table;
  int ymin = valInt(getLowIndexVector(tab->rows));
  int ymax = valInt(getHighIndexVector(tab->rows));
  int rows = 0;
  int l=0, r=0, w=0;
  int y;

  for(y=ymin; y<=ymax; y++)
  { TableCell cell = getCellTableColumn(col, toInt(y));

    if ( cell && cell->col_span == ONE && notNil(cell->image) )
    { Graphical gr = cell->image;
      int grw;
      int px, py;

      ComputeGraphical(gr);
      table_cell_padding(cell, &px, &py);
      grw = valInt(gr->area->w);

      if ( getHalignTableCell(cell) == NAME_reference )
      {	Point ref = getIf(gr, NAME_reference, FAIL);
	int rx = (ref ? valInt(ref->x) : 0);

	l = max(l, px+rx);
	r = max(r, px+grw-rx);
      } else
      { w = max(w, 2*px+grw);
      }
    }

    if ( cell )
      rows++;
  }
  
  w = max(w, l+r);
  assign(col, width, toInt(w));
  assign(col, reference, toInt(l));

  succeed;
}


/* Type declarations */

static char T_halign[]    = "{left,right,center,reference,stretch}";
static char T_defhalign[] = "[{left,right,center,reference,stretch}]";

/* Instance Variables */

static vardecl var_table_column[] =
{ IV(NAME_alignment, T_halign, IV_GET|IV_REDEFINE,
     NAME_layout, "Default alignment of cells")
};
  
/* Send Methods */

static senddecl send_table_column[] =
{ SM(NAME_initialise, 1, T_defhalign, initialiseTableColumn,
     DEFAULT, "Initialise table column"),
  SM(NAME_unlink, 0, NULL, unlinkTableColumn,
     DEFAULT, "Remove from <-table"),
  SM(NAME_background, 1, "[colour|pixmap]", backgroundTableColumn,
     NAME_colour, NULL),
  SM(NAME_halign, 1, T_halign, halignTableColumn,
     NAME_alignment, "Default horizontal alignment"),
  SM(NAME_compute, 0, NULL, computeTableColumn,
     NAME_layout, "Compute dimensions of the column")
};

/* Get Methods */

static getdecl get_table_column[] =
{ GM(NAME_halign, 0, T_halign, NULL,
     getHalignTableColumn,
     NAME_alignment, "Default horizontal alignment"),
  GM(NAME_cell, 1, "table_cell", "int",
     getCellTableColumn,
     NAME_contents, "Cell at indicated row")
};

/* Resources */

#define rc_table_column NULL
/*
static resourcedecl rc_table_column[] =
{
};
*/

/* Class Declaration */

ClassDecl(table_column_decls,
          var_table_column,
	  send_table_column,
	  get_table_column,
	  rc_table_column,
          0, NULL,
          "$Rev$");

status
makeClassTableColumn(Class class)
{ return declareClass(class, &table_column_decls);
}


		 /*******************************
		 *	       ROW		*
		 *******************************/

static status
initialiseTableRow(TableRow col, Name valign)
{ initialiseTableSlice((TableSlice)col);

  if ( isDefault(valign) )
    valign = NAME_top;

  assign(col, alignment, valign);

  succeed;
}


static status
unlinkTableRow(TableRow row)
{ if ( notNil(row->table) && !isFreeingObj(row->table) )
    send(row->table, NAME_delete, row, 0);

  return unlinkVector((Vector)row);
}


static status
valignTableRow(TableRow col, Name valign)
{ assign(col, alignment, valign);

  succeed;
}


static Name
getHalignTableRow(TableRow col)
{ answer(col->alignment);
}


static void
changedImageTableRow(TableRow row)
{ Table tab;
  Device dev;

  if ( notNil(tab = row->table) && notNil(dev=tab->device) )
    changedImageGraphical(dev, 0, row->position, tab->area->w, row->width);
}


static status
backgroundTableRow(TableRow row, Any bg)
{ if ( row->background != bg )
  { assign(row, background, bg);
    changedImageTableRow(row);
  }

  succeed;
}


TableCell
getCellTableRow(TableRow row, Int x)
{ TableCell cell;

  if ( (cell = getElementVector((Vector)row, x)) && notNil(cell) )
    answer(cell);

  fail;
}


status
cellTableRow(TableRow row, Int col, TableCell cell)
{ TableCell old;
  
  if ( (old=getCellTableRow(row, col)) )
  { if ( old != cell )
    { if ( notNil(cell) )
	freeObject(old);
    } else
      succeed;				/* no change */
  }
    
  return elementVector((Vector)row, col, cell);
}


status
indexTableRow(TableRow row, Int index)
{ for_vector_i(row, TableCell cell, i,
	       { if ( cell->row == row->index &&
		      cell->column == toInt(i) )
		   assign(cell, row, index);
	       });

  assign(row, index, index);

  succeed;
}


static status
computeTableRow(TableRow row)
{ int xmin = valInt(getLowIndexVector((Vector)row));
  int xmax = valInt(getHighIndexVector((Vector)row));
  int t=0, b=0, h=0;
  int x;

  for(x=xmin; x<=xmax; x++)
  { TableCell cell = getCellTableRow(row, toInt(x));

    if ( cell && cell->row_span == ONE && notNil(cell->image) )
    { Graphical gr = cell->image;
      int grh;
      int px, py;

      ComputeGraphical(gr);
      table_cell_padding(cell, &px, &py);
      grh = valInt(gr->area->h);

      if ( getValignTableCell(cell) == NAME_reference )
      { Point ref = getIf(gr, NAME_reference, FAIL);
	int ry = (ref ? valInt(ref->y) : 0);

	t = max(t, py+ry);
	b = max(b, py+grh-ry);
      } else
      { h = max(h, 2*py+grh);
      }
    }
  }
    
  h = max(h, t+b);
  assign(row, width,     toInt(h));
  assign(row, reference, toInt(t));

  succeed;
}


/* Type declarations */

static char T_valign[] = "{top,bottom,center,reference,stretch}";
static char T_defvalign[] = "[{top,bottom,center,reference,stretch}]";

/* Instance Variables */

static vardecl var_table_row[] =
{ IV(NAME_alignment, T_valign, IV_GET|IV_REDEFINE,
     NAME_layout, "Default alignment of cells")
};
  
/* Send Methods */

static senddecl send_table_row[] =
{ SM(NAME_initialise, 1, T_defvalign, initialiseTableRow,
     DEFAULT, "Initialise table column"),
  SM(NAME_unlink, 0, NULL, unlinkTableRow,
     DEFAULT, "Remove from <-table"),
  SM(NAME_background, 1, "[colour|pixmap]", backgroundTableRow,
     NAME_colour, NULL),
  SM(NAME_valign, 1, T_valign, valignTableRow,
     NAME_alignment, "Default vertical alignment"),
  SM(NAME_compute, 0, NULL, computeTableRow,
     NAME_layout, "Compute dimensions of the row"),
  SM(NAME_height, 1, "[int]", widthTableSlice,
     NAME_layout, "Set (fixed) height of the table row")
};

/* Get Methods */

static getdecl get_table_row[] =
{ GM(NAME_valign, 0, T_valign, NULL,
     getHalignTableRow,
     NAME_alignment, "Default horizontal alignment"),
  GM(NAME_cell, 1, "table_cell", "int",
     getCellTableRow,
     NAME_contents, "Cell at indicated column")
};

/* Resources */

#define rc_table_row NULL
/*
static resourcedecl rc_table_row[] =
{
};
*/

/* Class Declaration */

ClassDecl(table_row_decls,
          var_table_row,
	  send_table_row,
	  get_table_row,
	  rc_table_row,
          0, NULL,
          "$Rev$");

status
makeClassTableRow(Class class)
{ return declareClass(class, &table_row_decls);
}

